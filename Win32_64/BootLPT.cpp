// BootLPT/86 - BOOTLPT.EXE for Windows, (c) J. Bogin
// Licensed under GPLv3

// Can be compiled with Visual Studio 2012 or similar
// compiler, for both x86 and x86_64 targets.

// Requires Inpout32 / InpOutx64 DLL from
// http://www.highrez.co.uk/downloads/inpout32/

#include <windows.h>
#include <stdio.h>

#ifdef _WIN64
#define DRIVER_DLL_NAME "inpoutx64.dll"
#else
#define DRIVER_DLL_NAME "inpout32.dll"
#endif

// Parallel port address (or emulated equivalent).
#define PARALLEL_PORT	0x378

// DLL exported functions typedef
typedef void  (CALLBACK* POUTP)(short, short);
typedef short (CALLBACK* PINP) (short);

// Pointers to functions
POUTP pOut = nullptr; //outp() equivalent
PINP pIn = nullptr;   //inp() equivalent

// Send a byte over the LapLink cable in two 4-bit nibbles.
void sendLPT(BYTE nData)
{
	// Function pointers validity already checked in main().

	// Write zeros (set receiver to busy)
	pOut(PARALLEL_PORT, 0);				// OUTPUT LPT D4 (BUSY, port nBaseAddr) is inverted
							// on the INPUT side (port nBaseAddr+1), where it appears as bit 7.

	// Wait until we are ready (BUSY bit 7 on the INPUT needs to be = 0)
	while (pIn(PARALLEL_PORT + 1) & 0x80) {}

	BYTE nDataNew = nData & 0x0F;
	pOut(PARALLEL_PORT, nDataNew);			// Send the low nibble out, give it a little time...
	pOut(PARALLEL_PORT, nDataNew | 0x10);		// Send them again, with data available flag (OR 0x10).

	// Wait for the other side to acknowledge the data (until bit 7 == 1).
	while ((pIn(PARALLEL_PORT + 1) & 0x80) == 0) {}

	pOut(PARALLEL_PORT, 0);				// OUTPUT 0 ==> the INPUT side (~BUSY) is busy again...

	// Wait until ready again...
	while (pIn(PARALLEL_PORT + 1) & 0x80) {}

	// Now send the high nibble out, then, after a delay, set data available flag.
	nDataNew = (nData >> 4) & 0x0F;
	pOut(PARALLEL_PORT, nDataNew);
	pOut(PARALLEL_PORT, nDataNew | 0x10);

	// Wait for the other side to acknowledge the data (until bit 7 == 1).
	while ((pIn(PARALLEL_PORT + 1) & 0x80) == 0) {}

	// Data acknowledged.
	pOut(PARALLEL_PORT, 0);
}

int main(int argc, char* argv[])
{
	printf("BootLPT/86 (c) J. Bogin\n");

	// Invalid argument count
	if (argc != 2)
	{
		printf("\nUsage: BOOTLPT.EXE DSKIMAGE.IMG"
		       "\nwhere DSKIMAGE.IMG is a boot floppy image (max 64K)\nQuitting...\n");

		return 0;
	}

	// Check whether the required DLL driver is present,
	DWORD dwFileAttributes = GetFileAttributes(DRIVER_DLL_NAME);
	
	// i.e. the file attributes are valid, and it's not a directory
	if ( (dwFileAttributes == INVALID_FILE_ATTRIBUTES) ||
	     (dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) )
	{
		printf("\nError: Required DLL %s not found.\nQuitting...\n", DRIVER_DLL_NAME);
		return 1;
	}

	// Load the driver
	HMODULE hDLL = LoadLibrary(DRIVER_DLL_NAME);
	if (!hDLL)
	{
		printf("\nError: Required DLL %s failed to load."
		        "\nTry re-running as administrator.\nQuitting...\n", DRIVER_DLL_NAME);
		return 1;
	}

	// Retrieve function addresses for Out32/Inp32 (the equivalent of outp, inp).
	pOut = (POUTP)GetProcAddress(hDLL, "Out32");
	pIn = (PINP)GetProcAddress(hDLL, "Inp32");
	if (!pOut || !pIn)
	{
		printf("\nError: Could not import required functions from the DLL.\nQuitting...\n");
		FreeLibrary(hDLL);
		return 1;
	}

	// Do a test read from the ports. Get the current day number from CMOS
	// and compare it with what we get from the system.
	SYSTEMTIME sTime = {0};	
	GetSystemTime(&sTime);		// stored in sTime.wDay
	
	// Register 7 from CMOS RTC (day of month)
	pOut(0x70, 7);

	// Check for a match
	if ((BYTE)sTime.wDay != pIn(0x71))
	{
		printf("\nError: Could not do a test read from the ports.\n"
		       "\nTry re-running as administrator.\nQuitting...\n");

		FreeLibrary(hDLL);
		return 1;
	}

	// Open the boot image in binary mode
	FILE* pFile = nullptr;
	fopen_s(&pFile, argv[1], "rb");
	if (!pFile)
	{
		printf("\nError: Cannot open \"%s\".\nQuitting...\n", argv[1]);
		FreeLibrary(hDLL);
		return 1;
	}

	// Get file size
	fseek(pFile, 0, SEEK_END);
	unsigned long nFileSize = ftell(pFile);
	fseek(pFile, 0, SEEK_SET);

	if (nFileSize < 1)
	{
		fclose(pFile);
		printf("\nError: Invalid file.\nQuitting...\n");
		FreeLibrary(hDLL);
		return 1;
	}

	if (nFileSize > 0xffff)
	{
		fclose(pFile);
		printf("\nError: Images bigger than 64K are not supported.\nQuitting...\n");
		FreeLibrary(hDLL);
		return 1;
	}

	// Send file size first
	printf("\nWaiting for connection...");

	SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);

	sendLPT((BYTE)nFileSize);						// Low order
	sendLPT((BYTE)((nFileSize & 0x0FF00) >> 8));	// High order

	// Transmit the file
	printf("\rSending %u bytes through LPT1...\n", nFileSize);
	for (unsigned long nIdx = 0; nIdx < nFileSize; nIdx++)
	{
		// Read one byte from file and transmit it
		BYTE nRead = 0;
		fread(&nRead, sizeof(BYTE), 1, pFile);
		sendLPT(nRead);
	}
	fclose(pFile);

	FreeLibrary(hDLL);
	printf("Done!\n");

	return 0;
}