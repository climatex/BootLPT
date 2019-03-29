// Compile with any x86 C++ compiler

#include <stdio.h>

int main(int argc, char* argv[])
{	
	if (argc != 2)
	{
		printf("usage: PATCHER BOOTLPT.BIN\nwhere: [BOOTLPT.BIN] is the BootLPT/86 ROM image\n");
		return 0;
	}

	FILE* pFile = fopen(argv[1], "r+b");
	if (!pFile)
	{
		printf("error: cannot open %s\n", argv[1]);
		return -1;
	}

	unsigned char nChecksum = 0;
	int nOffset = 0;
	while (!feof(pFile))
	{
		bool bValid = true;
		unsigned char nByte = 0;

		fread(&nByte, sizeof(unsigned char), 1, pFile);		
		nChecksum += nByte;

		switch(nOffset)
		{
		case 0:
			bValid = (nByte == 0x55);
			break;
		case 1:
			bValid = (nByte == 0xAA);
			break;
		case 5:
			bValid = (nByte == 0);
		default:
			break;
		}

		if (!bValid)
		{
			if (nOffset < 2)
			{
				printf("error: %s is not a valid ROM image\n", argv[1]);
			}
			else
			{
				printf("error: %s already patched or not a BootLPT/86 ROM image\n", argv[1]);
			}
			fclose(pFile);
			return -1;
		}

		nOffset++;
	}

	if (nChecksum == 0)
	{
		printf("checksum of %s equal to 0, patch not required\n", argv[1]);
		fclose(pFile);
		return 0;
	}

	unsigned char nPatchValue = 0x100 - nChecksum;
	printf("8-bit checksum of %s is 0x%02x, patching the fifth byte to 0x%02x\n", argv[1], nChecksum, nPatchValue);

	fseek(pFile, 5, SEEK_SET);
	fwrite(&nPatchValue, sizeof(unsigned char), 1, pFile);
	fclose(pFile);

	printf("done!\n");
	return 0;
}


