// BootLPT/86 - BOOTLPT.EXE for DOS, (c) J. Bogin
// Licensed under GPLv3

// Compile with Turbo C++ or a similar DOS compiler

#define byte unsigned char
#define word unsigned int

#include <stdio.h>

// No need to include conio.h just because of outp/inp.
void _outp(word nAddr, byte nData)
{
	asm {
		mov al,nData
		mov dx,nAddr
		out dx,al
	}
}

byte _inp(word nAddr)
{
	byte nResult = 0;
	asm {
		mov dx,nAddr
		in al,dx
		mov nResult,al
	}
	return nResult;
	// return is not needed, but it'd throw a warning without it
}

// Send a byte over the LapLink cable in two 4-bit nibbles.
void sendLPT(word nBaseAddr, byte nData)
{
	// Write zeros (set receiver to busy)
	_outp(nBaseAddr, 0);			// OUTPUT LPT D4 (BUSY, port nBaseAddr) is inverted
						// on the INPUT side (port nBaseAddr+1), where it appears as bit 7.

	// Wait until we are ready (BUSY bit 7 on the INPUT needs to be = 0)
	while( _inp(nBaseAddr + 1) & 0x80 ) { }
	
	byte nDataNew = nData & 0x0F;	
	_outp(nBaseAddr, nDataNew);		// Send the low nibble out, give it a little time...
	_outp(nBaseAddr, nDataNew | 0x10);	// Send them again, with data available flag (OR 0x10).
	
	// Wait for the other side to acknowledge the data (until bit 7 == 1).
	while ( (_inp(nBaseAddr + 1) & 0x80) == 0 ) { }

	_outp(nBaseAddr, 0);			// OUTPUT 0 ==> the INPUT side (~BUSY) is busy again...

	// Wait until ready again...
	while( _inp(nBaseAddr + 1) & 0x80 ) { }

	// Now send the high nibble out, then, after a delay, set data available flag.
	nDataNew = (nData >> 4) & 0x0F;
	_outp(nBaseAddr, nDataNew);
	_outp(nBaseAddr, nDataNew | 0x10);

	// Wait for the other side to acknowledge the data (until bit 7 == 1).
	while ( (_inp(nBaseAddr + 1) & 0x80) == 0 ) { }

	// Data acknowledged.
	_outp(nBaseAddr, 0);
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

	// Open the boot image in binary mode
	FILE* pFile = fopen(argv[1], "rb");
	if (!pFile)
	{
		printf("\nError: Cannot open \"%s\".\nQuitting...\n", argv[1]);
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
		return 1;
	}

	if (nFileSize > 0xffff)
	{
		fclose(pFile);
		printf("\nError: Images bigger than 64K are not supported.\nQuitting...\n");
		return 1;
	}

	// Retrieve LPT1 port number (default: 0x378) from BIOS (0x0000:0x0408)
	word nLPTAddr = 0;
	asm {
		push ds
		xor ax,ax
		mov ds,ax
		mov si,0x408
		lodsw
		pop ds
		mov nLPTAddr,ax		
	}
	// Fallback
	if (!nLPTAddr)
	{
		nLPTAddr = 0x378;
	}

	// Send file size first
	printf("\nWaiting for connection...");
	sendLPT(nLPTAddr, (byte)nFileSize);			 //Low order
	sendLPT(nLPTAddr, (byte)((nFileSize & 0x0FF00) >> 8 ));  //High order

	// Transmit the file
	printf("\rSending %u bytes through LPT1...\n", nFileSize);
	for (unsigned long nIdx = 0; nIdx < nFileSize; nIdx++)
	{
		// Read one byte from file and transmit it
		byte nRead = 0;
		fread(&nRead, sizeof(byte), 1, pFile);
		sendLPT(nLPTAddr, nRead);
	}

	fclose(pFile);

	printf("Done!\n");

	return 0;
}