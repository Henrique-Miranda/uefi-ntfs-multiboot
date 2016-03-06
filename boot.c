/*
 * uefi-ntfs: UEFI/NTFS chain loader
 * Copyright © 2014-2016 Pete Batard <pete@akeo.ie>
 * With parts from GRUB © 2006-2015 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <efi.h>
#include <efilib.h>
#include <efistdarg.h>

#define FILE_INFO_SIZE  (512 * sizeof(CHAR16))
#define NUM_RETRIES     1
#define DELAY           3	// delay before retry, in seconds

EFI_GUID EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID = SIMPLE_FILE_SYSTEM_PROTOCOL;
EFI_HANDLE EfiImageHandle = NULL;
// NB: FreePool(NULL) is perfectly valid
#define SafeFree(p) do { FreePool(p); p = NULL;} while(0)

// Use 'rufus' in the driver path, so that we don't accidentally latch onto a user driver
#if defined(_M_X64) || defined(__x86_64__)
  static CHAR16* DriverPath = L"\\efi\\rufus\\ntfs_x64.efi";
#else
  static CHAR16* DriverPath = L"\\efi\\rufus\\ntfs_ia32.efi";
#endif
// We'll need to fix the casing as our target is a case sensitive file system and Microsoft
// indiscriminately seems to uses "EFI\Boot" or "efi\boot"
#if defined(_M_X64) || defined(__x86_64__)
  static CHAR16* LoaderPath = L"\\efi\\boot\\bootx64.efi";
#else
  static CHAR16* LoaderPath = L"\\efi\\boot\\bootia32.efi";
#endif
// Always good to know if we're actually running 32 or 64 bit
#if defined(_M_X64) || defined(__x86_64__)
  static CHAR16* Arch = L"64";
#else
  static CHAR16* Arch = L"32";
#endif

// Display a human readable error message
static VOID PrintStatusError(EFI_STATUS Status, const CHAR16 *Format, ...)
{
	CHAR16 StatusString[64];
	va_list ap;

	StatusToString(StatusString, Status);
	va_start(ap, Format);
	VPrint((CHAR16 *)Format, ap);
	va_end(ap);
	Print(L": [%d] %s\n", (Status & 0x7FFFFFFF), StatusString);
}

// Return the device path node right before the end node
static EFI_DEVICE_PATH* GetLastDevicePath(const EFI_DEVICE_PATH* dp)
{
	EFI_DEVICE_PATH *next, *p;

	if (IsDevicePathEnd(dp))
		return NULL;

	for (p = (EFI_DEVICE_PATH *) dp, next = NextDevicePathNode(p);
		!IsDevicePathEnd(next);
		p = next, next = NextDevicePathNode(next));

	return p;
}

// Get the parent device in an EFI_DEVICE_PATH
// Note: the returned device path is allocated and must be freed
static EFI_DEVICE_PATH* GetParentDevice(const EFI_DEVICE_PATH* DevicePath)
{
	EFI_DEVICE_PATH *dp, *ldp;

	dp = DuplicateDevicePath((EFI_DEVICE_PATH*)DevicePath);
	if (dp == NULL)
		return NULL;

	ldp = GetLastDevicePath(dp);
	if (ldp == NULL)
		return NULL;

	ldp->Type = END_DEVICE_PATH_TYPE;
	ldp->SubType = END_ENTIRE_DEVICE_PATH_SUBTYPE;

	SetDevicePathNodeLength(ldp, sizeof (*ldp));

	return dp;
}

// Compare device paths
static INTN CompareDevicePaths(const EFI_DEVICE_PATH *dp1, const EFI_DEVICE_PATH *dp2)
{
	if (dp1 == NULL || dp2 == NULL)
		return -1;

	while (1) {
		UINT8 type1, type2;
		UINT8 subtype1, subtype2;
		UINT16 len1, len2;
		INTN ret;

		type1 = DevicePathType(dp1);
		type2 = DevicePathType(dp2);

		if (type1 != type2)
			return (int) type2 - (int) type1;

		subtype1 = DevicePathSubType(dp1);
		subtype2 = DevicePathSubType(dp2);

		if (subtype1 != subtype2)
			return (int) subtype1 - (int) subtype2;

		len1 = DevicePathNodeLength(dp1);
		len2 = DevicePathNodeLength(dp2);
		if (len1 != len2)
			return (int) len1 - (int) len2;

		ret = CompareMem(dp1, dp2, len1);
		if (ret != 0)
			return ret;

		if (IsDevicePathEnd(dp1))
			break;

		dp1 = (EFI_DEVICE_PATH*) ((char *)dp1 + len1);
		dp2 = (EFI_DEVICE_PATH*) ((char *)dp2 + len2);
	}

	return 0;
}

// Some UEFI firmwares have a *BROKEN* Unicode collation implementation
// so we must provide our own version of StriCmp for ASCII comparison...
static CHAR16 _tolower(CHAR16 c)
{
	if(('A' <= c) && (c <= 'Z'))
		return 'a' + (c - 'A');
	return c;
}

static int _StriCmp(CONST CHAR16 *s1, CONST CHAR16 *s2)
{
	while ((*s1 != L'\0') && (_tolower(*s1) == _tolower(*s2)))
		s1++, s2++;
	return (int)(*s1 - *s2);
}

// Fix the case of a path by looking it up on the file system
static EFI_STATUS SetPathCase(EFI_FILE_HANDLE Root, CHAR16* Path)
{
	EFI_FILE_HANDLE FileHandle = NULL;
	EFI_FILE_INFO* FileInfo;
	UINTN i, Len;
	UINTN Size;
	EFI_STATUS Status;

	if ((Root == NULL) || (Path == NULL) || (Path[0] != L'\\'))
		return EFI_INVALID_PARAMETER;

	FileInfo = (EFI_FILE_INFO*)AllocatePool(FILE_INFO_SIZE);
	if (FileInfo == NULL)
		return EFI_OUT_OF_RESOURCES;

	Len = StrLen(Path);
	// Find the last backslash in the path
	for (i = Len-1; (i != 0) && (Path[i] != L'\\'); i--);

	if (i != 0) {
		Path[i] = 0;
		// Recursively fix the case
		Status = SetPathCase(Root, Path);
		if (EFI_ERROR(Status))
			goto out;
	}

	Status = Root->Open(Root, &FileHandle, (i==0)?L"\\":Path, EFI_FILE_MODE_READ, 0);
	if (EFI_ERROR(Status))
		goto out;

	do {
		Size = FILE_INFO_SIZE;
		Status = FileHandle->Read(FileHandle, &Size, (VOID*)FileInfo);
		if (EFI_ERROR(Status))
			goto out;
		if (_StriCmp(&Path[i+1], FileInfo->FileName) == 0) {
			StrCpy(&Path[i+1], FileInfo->FileName);
			Status = EFI_SUCCESS;
			goto out;
		}
		Status = EFI_NOT_FOUND;
	} while (FileInfo->FileName[0] != 0);

out:
	Path[i] = L'\\';
	if (FileHandle != NULL)
		FileHandle->Close(FileHandle);
	FreePool((VOID*)FileInfo);
	return Status;
}

// Application entrypoint
EFI_STATUS EfiMain(EFI_HANDLE hImage, EFI_SYSTEM_TABLE *pST)
{
    EFI_LOADED_IMAGE *pImage;
    EFI_STATUS nStatus;
    EFI_HANDLE hDriver, *hBlkDevs;
    UINTN nBlkDevs;
    EFI_DEVICE_PATH *pBootPart, *pBootDisk = NULL;

    InitializeLib(hImage, pST);
    Print(L"%H\n*** UEFI:NTFS multiboot ***");

    if (EFI_ERROR((nStatus = BS->OpenProtocol(hImage, &LoadedImageProtocol, &pImage, hImage, NULL, EFI_OPEN_PROTOCOL_GET_PROTOCOL)))) {
	Print(L"%E\nUnable to convert handle to interface: %r\n", nStatus);
	goto end;
    }

    pBootPart = DevicePathFromHandle(pImage->DeviceHandle);
    pBootDisk = GetParentDevice(pBootPart);

    CHAR16 *pszDev = DevicePathToStr(pBootDisk);
    Print(L"%N\nDisk: %s\n\n", pszDev);
    FreePool(pszDev);

    Print(L"%H\r[ WAIT ] Loading NTFS driver");
    EFI_DEVICE_PATH *pDrvPath = FileDevicePath(pImage->DeviceHandle, DriverPath);
    if (pDrvPath == NULL) {
	Print(L"%E\r[ FAIL ] Unable to construct path to NTFS driver\n");
	goto end;
    }
    nStatus = BS->LoadImage(FALSE, hImage, pDrvPath, NULL, 0, &hDriver);
    FreePool(pDrvPath);
    if (EFI_ERROR(nStatus)) {
	Print(L"%E\r[ FAIL ] Unable to load NTFS driver: %r\n", nStatus);
	goto end;
    }
    if (EFI_ERROR((nStatus = BS->StartImage(hDriver, NULL, NULL)))) {
	Print(L"%E\r[ FAIL ] Unable to start NTFS driver: %r\n", nStatus);
	goto end;
    }
    Print(L"%H\r[  OK  ] NTFS driver loaded and started\n");

    LINKED_LOADER_PATH_LIST_NODE *list = NULL;
    EFI_DEVICE_PATH *ldr;

    if (EFI_ERROR((nStatus = BS->LocateHandleBuffer(ByProtocol, &BlockIoProtocol, NULL, &nBlkDevs, &hBlkDevs)))) {
	Print(L"%E\r[ FAIL ] Unable to enumerate block devices: %r\n", nStatus);
	goto end;
    }
    for (UINTN i = 0; i < nBlkDevs; i++) {
	EFI_DEVICE_PATH *pDevice = DevicePathFromHandle(hBlkDevs[i]);
	pszDev = DevicePathToStr(pDevice);
	Print(L"%N\r[ INFO ] Probing %d devices... [%d] %s", nBlkDevs, i + 1, pszDev);
	FreePool(pszDev);

	if (CompareDevicePaths(pDevice, pBootPart) == 0) continue;
	if (CompareDevicePaths(pDevice, pBootDisk) == 0) continue;

	EFI_DEVICE_PATH *pDisk = GetParentDevice(pDevice);
	_Bool probe = CompareDevicePaths(pDisk, pBootDisk) == 0;
	FreePool(pDisk);
#if !defined(_DEBUG)
	if (!probe) continue;
#else
	UNREFERENCED(probe);
#endif

	EFI_BLOCK_IO *blkIo;
	if (EFI_ERROR((nStatus = BS->OpenProtocol(hBlkDevs[i], &BlockIoProtocol, &blkIo, hImage, NULL, EFI_OPEN_PROTOCOL_GET_PROTOCOL)))) {
	    Print(L"%E\n[ WARN ] Unable to open block device, skipping: %r\n", nStatus);
	    continue;
	}

	CHAR8 *buffer = AllocatePool(blkIo->Media->BlockSize);
	if (buffer == NULL) {
	    Print(L"%E\n[ WARN ] Unable to allocate buffer of size %d\n", blkIo->Media->BlockSize);
	    continue;
	}

	nStatus = blkIo->ReadBlocks(blkIo, blkIo->Media->MediaId, 0, blkIo->Media->BlockSize, buffer);
	_Bool isNTFS = CompareMem(&buffer[3], NTFSMagic, sizeof(NTFSMagic)) == 0;
	FreePool(buffer);
	if (EFI_ERROR(nStatus)) {
	    Print(L"%E\n[ WARN ] Unable to read block device, skipping: %r\n", nStatus);
	    continue;
	}
	if (!isNTFS) continue;

	Print(L"%H\n[ WAIT ] Attaching NTFS driver to device");
	EFI_FILE_IO_INTERFACE *fs;
	nStatus = BS->OpenProtocol(hBlkDevs[i], &FileSystemProtocol, NULL, hImage, NULL, EFI_OPEN_PROTOCOL_TEST_PROTOCOL);
	if (nStatus == EFI_UNSUPPORTED) {
	    nStatus = BS->ConnectController(hBlkDevs[i], NULL, NULL, TRUE);
	}
	for (UINTN j = 0; j < NUM_RETRIES; j++) {
	    if ((!EFI_ERROR((nStatus = BS->OpenProtocol(hBlkDevs[i], &FileSystemProtocol, &fs, hImage, NULL, EFI_OPEN_PROTOCOL_GET_PROTOCOL))))) {
		break;
	    }
	    Print(L".");
	    BS->Stall(DELAY * 1000000);
	}
	if(EFI_ERROR(nStatus)) {
	    Print(L"%E\r[ WARN ] Unable to attach NTFS driver, skipping: %r\n", nStatus);
	    continue;
	}
	Print(L"%H\r[  OK  ] NTFS driver attached to current device\n");

	Print(L"%H\r[ WAIT ] Locating EFI boot loader");
	EFI_FILE *fsRoot;
	if (EFI_ERROR((nStatus = fs->OpenVolume(fs, &fsRoot)))) {
	    Print(L"%E\r[ WARN ] Unable to open root directory, skipping: %r\n", nStatus);
	    continue;
	}
	CHAR16 *loader = StrDuplicate(LoaderPath);
	if (EFI_ERROR((nStatus = SetPathCase(fsRoot, loader)))) {
	    FreePool(loader);
	    Print(L"%E\r[ WARN ] Unable to locate EFI boot loader on this device: %r\n", nStatus);
	    continue;
	}
	Print(L"%H\r[  OK  ] EFI boot loader located at %s\n", loader);

	ldr = FileDevicePath(hBlkDevs[i], loader);
	ListAppend(&list, ldr);
	FreePool(loader);
    }

    UINTN nListEntries = 0, nBootEntry = 0, nPage = 0, nTotalPage = 0;
    EFI_INPUT_KEY key;
    _Bool interactive = FALSE;
    ListTraverse(&list, CountEntries, 0, 0, &nListEntries);

    switch (nListEntries) {
    case 0:
	Print(L"%E\n[ FAIL ] No bootable partition\n", nStatus);
	nStatus = EFI_NOT_FOUND;
	goto end;
    case 1:
	//break;
    default:
	nTotalPage = (nListEntries - 1) / PAGE_SIZE + 1;
	while (1) {
	    ST->ConOut->ClearScreen(ST->ConOut);
	    Print(L"%H*** UEFI:NTFS Multiboot ***\n");
	    CHAR16 *pszDev = DevicePathToStr(pBootDisk);
	    Print(L"%NDisk: %s\n\n%H", pszDev);
	    FreePool(pszDev);

	    ListTraverse(&list, DisplayEntries, nPage * PAGE_SIZE, PAGE_SIZE, NULL);

	    Print(L"%N\nPage %hd / %hd, %hd entries\n", nPage + 1, nTotalPage, nListEntries);
	    Print(L"%H\n[F1 - F8] [1 - 8] %N Boot corresponding entry");
	    Print(L"%H\n[PgUp]  [<]  [-]  %N Previous page");
	    Print(L"%H\n[PgDn]  [>]  [+]  %N Next page");

	    if (!interactive) {
		INTN nCountDown = AUTOBOOT_TIME;
		Print(L"%N\n\n");
		while (nCountDown >= 0) {
		    Print(L"\rWill automatically boot the first entry in %d seconds...", nCountDown);
		    if (WaitForSingleEvent(ST->ConIn->WaitForKey, 1000 * 1000 * 10) != EFI_TIMEOUT) {
			interactive = TRUE;
			break;
		    }
		    nCountDown--;
		}
		if (!interactive) {
		    goto boot;
		}
	    }
	    else {
		WaitForSingleEvent(ST->ConIn->WaitForKey, 0);
	    }
	    ST->ConIn->ReadKeyStroke(ST->ConIn, &key);
	    switch (key.UnicodeChar) {
	    case L'1':case L'2':case L'3':case L'4':case L'5':case L'6':case L'7':case L'8':
		nBootEntry = nPage * PAGE_SIZE + (key.UnicodeChar - L'1');
		goto boot;
	    case L'+':case L'=':case L'>':case L'.':
		if ((nPage + 1) != nTotalPage) {
		    nPage++;
		}
		break;
	    case L'-':case L'_': case L'<': case L',':
		if (nPage != 0) {
		    nPage--;
		}
		break;
	    default:
		switch (key.ScanCode) {
		case 0x09:
		    if (nPage != 0) {
			nPage--;
		    }
		    break;
		case 0x0a:
		    if ((nPage + 1) != nTotalPage) {
			nPage++;
		    }
		    break;
		case 0x0b:case 0x0c:case 0x0d:case 0x0e:case 0x0f:case 0x10:case 0x11:case 0x12:
		    nBootEntry = nPage * PAGE_SIZE + (key.ScanCode - 0x0b);
		    goto boot;
		}
	    }
	}
    }

boot:
    ldr = NULL;
    Print(L"%H");
    ST->ConOut->ClearScreen(ST->ConOut);
    ListTraverse(&list, ReadEntry, nBootEntry, 1, &ldr);
    ListTraverse(&list, DisplayEntries, nBootEntry, 1, NULL);
    if (ldr == NULL) {
	Print(L"%E\n[ FAIL ] No such boot entry\n", nStatus);
	nStatus = EFI_NOT_FOUND;
	goto end;
    }

    ldr = DuplicateDevicePath(ldr);
    ListTraverse(&list, DestroyEntries, 0, 0, NULL);
    ListDestroy(&list);

    EFI_HANDLE hChain;
    nStatus = BS->LoadImage(FALSE, hImage, ldr, NULL, 0, &hChain);
    FreePool(ldr);
    if (EFI_ERROR(nStatus)) {
	Print(L"%E\n[ FAIL ] Unable to load boot loader: %r\n", nStatus);
	goto end;
    }
    Print(L"%N");
    if (EFI_ERROR((nStatus = BS->StartImage(hChain, NULL, NULL)))) {
	Print(L"%E\n[ FAIL ] Unable to start boot loader: %r\n", nStatus);
	goto end;
    }

end:
    if (pBootDisk) FreePool(pBootDisk);
    if (EFI_ERROR(nStatus)) {
	Print(L"Press any key to exit\n");
	WaitForSingleEvent(ST->ConIn->WaitForKey, 0);
	ST->ConIn->ReadKeyStroke(ST->ConIn, &key);
    }
    return nStatus;
}
