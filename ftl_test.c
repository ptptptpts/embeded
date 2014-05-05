// Copyright 2012 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// LogblockFTL source file
//

#include "jasmine.h"
#include "ftl.h"

//----------------------------------
// macro
//----------------------------------

//----------------------------------
// Global Variables
//----------------------------------
UINT32 gnRandValue = 0x15881588;
UINT32 gpnSavedValues = (UINT32 *)FTL_TEST_ADDR;

extern UINT32 g_ftl_read_buf_id;
extern UINT32 g_ftl_write_buf_id;

//----------------------------------
// FTL internal function prototype
//----------------------------------
UINT32 _GetRandValue(void);
BOOL32 _TestCase00(void);
BOOL32 _TestCase01(void);
BOOL32 _TestCase02(void);
BOOL32 _TestCase03(void);
BOOL32 _TestCase04(void);
BOOL32 _TestCase05(void);
BOOL32 _TestCase06(void);
BOOL32 _TestCase07(void);

BOOL32 _TestCase08(void);

//----------------------------------
// FTL external function implementation
//----------------------------------
void ftl_test(void)
{
	_TestCase00();
	_TestCase01();
	_TestCase02();
	_TestCase03();
	_TestCase04();
	_TestCase05();
	_TestCase06();
	_TestCase07();
	_TestCase08();
}

//----------------------------------
// FTL internal function implementation
//----------------------------------

BOOL32 _TestCase08(void)
{
	UINT32 nRandVal = 0x1a2b3c4d;
	UINT32 nReadValue;

	uart_printf ("DBG> _TestCase08 Start");

	if (check_format_mark () == TRUE) {
		uart_printf ("TestCase08> Check format mark");
		ftl_read (0, SECTORS_PER_PAGE);
		flash_finish();
		if (0 == g_ftl_read_buf_id)
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
		}
		else
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
		}
		uart_printf ("TestCase08> Read from lpn 0 :: %x", nReadValue);
	}	
	else
	{
		uart_printf ("TestCase08> Format mark isn't checked");
	}

	_write_dram_32 ((UINT32)WR_BUF_PTR (g_ftl_write_buf_id), nRandVal);
	ftl_write(0, SECTORS_PER_PAGE);
	flash_finish();
	
	ftl_flush();
}

UINT32 _GetRandValue(void)
{
	gnRandValue = gnRandValue * 1103515245 + 12345;
	return gnRandValue;
}

BOOL32 _TestCase00(void)
{
	UINT32 nStartLPN = 0;
	UINT32 nLPN;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	BOOL32 bFail = FALSE;

	uart_printf("DBG> _TestCase00 Start");

	ftl_read(0, SECTORS_PER_PAGE);
	flash_finish();

	if (0 == g_ftl_read_buf_id)
	{
		nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
	}
	else
	{
		nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
	}

	if (0x0 != nReadValue)
	{
		uart_printf("ERR> Data Miscompare1 : nReadValue 0x%x", nReadValue);
		bFail = TRUE;
		goto T0;
	}

	nWriteValue = _GetRandValue();
	_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + 512, nWriteValue);	
	ftl_write(1, 1);

	ftl_read(0, 1);
	flash_finish();

	if (0 == g_ftl_read_buf_id)
	{
		nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
	}
	else
	{
		nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
	}

	if (0x0 != nReadValue)
	{
		uart_printf("ERR> Data Miscompare2 : nReadValue 0x%x", nReadValue);
		bFail = TRUE;
		goto T0;
	}

	ftl_read(2, 1);
	flash_finish();

	if (0 == g_ftl_read_buf_id)
	{
		nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + 1024);
	}
	else
	{
		nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + 1024);
	}

	if (0x0 != nReadValue)
	{
		uart_printf("ERR> Data Miscompare3 : nReadValue 0x%x", nReadValue);
		bFail = TRUE;
		goto T0;
	}

	ftl_read(1, 1);
	flash_finish();

	if (0 == g_ftl_read_buf_id)
	{
		nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + 512);
	}
	else
	{
		nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + 512);
	}

	if (nWriteValue != nReadValue)
	{
		uart_printf("ERR> Data Miscompare4 : nWriteValue 0x%x, nReadValue 0x%x", nWriteValue, nReadValue);
		bFail = TRUE;
		goto T0;
	}

T0:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase00 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase00 End");
	}
	
	return bFail;
}

BOOL32 _TestCase01(void)
{
	UINT32 nStartLPN = 0;
	//UINT32 nNumLPNs = 128*8;
	//UINT32 nNumLPNs = 128 * 3 * 8;
	UINT32 nNumLPNs = 10240; // 8 bank * 10 Block, Let NUM_LOG_BLOCKS be less than 10
	UINT32 nLPN;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	BOOL32 bFail = FALSE;

	uart_printf("DBG> _TestCase01 Start : nStartLPN 0x%x, nNumLPNs 0x%x", nStartLPN, nNumLPNs);

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		nWriteValue = _GetRandValue();
		_write_dram_32(WR_BUF_PTR(g_ftl_write_buf_id), nWriteValue);

		ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		flash_finish();

		if (0 == g_ftl_read_buf_id)
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
		}
		else
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
		}

		if (nWriteValue != nReadValue)
		{
			uart_printf("ERR> Data Miscompare : nLPN 0x%x, nWriteValue 0x%x, nReadValue 0x%x", nLPN, nWriteValue, nReadValue);
			bFail = TRUE;
			goto T1;
		}
	}

T1:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase01 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase01 End");
	}
	
	return bFail;
}

BOOL32 _TestCase02(void)
{
	UINT32 nStartLPN = 0;
	//UINT32 nNumLPNs = 128 * 8;
	//UINT32 nNumLPNs = 128 * 3 * 8;
	UINT32 nNumLPNs = 10240; // 8 bank * 10 Block, Let NUM_LOG_BLOCKS be less than 10
	UINT32 nLPN;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	BOOL32 bFail = FALSE;

	uart_printf("DBG> _TestCase02 Start : nStartLPN 0x%x, nNumLPNs 0x%x", nStartLPN, nNumLPNs);

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		nWriteValue = nLPN;
		_write_dram_32(WR_BUF_PTR(g_ftl_write_buf_id), nWriteValue);

		ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
	}

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		flash_finish();

		if (0 == g_ftl_read_buf_id)
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
		}
		else
		{
			nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
		}

		if (nLPN != nReadValue)
		{
			uart_printf("ERR> Data Miscompare : nLPN 0x%x, nWriteValue 0x%x, nReadValue 0x%x", nLPN, nLPN, nReadValue);
			bFail = TRUE;
			//goto T2;
		}
	}

T2:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase02 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase02 End");
	}
	
	return bFail;
}

BOOL32 _TestCase03(void)
{
	UINT32 nCount;
	UINT32 nIndex;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	BOOL32 bFail = FALSE;

	uart_printf("DBG> _TestCase03 Start");

	for (nCount = 0; nCount < 3; nCount++)
	{
		for (nIndex = 0; nIndex < PAGES_PER_VBLK; nIndex++)
		{
			nWriteValue = _GetRandValue();
			_write_dram_32(WR_BUF_PTR(g_ftl_write_buf_id), nWriteValue);

			ftl_write(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);
			ftl_read(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);
			flash_finish();

			if (0 == g_ftl_read_buf_id)
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
			}
			else
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
			}

			if (nWriteValue != nReadValue)
			{
				uart_printf("ERR> Data Miscompare : nLPN 0x%x, nWriteValue 0x%x, nReadValue 0x%x", nIndex, nWriteValue, nReadValue);
				bFail = TRUE;
				goto T3;
			}
		}
	}

T3:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase03 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase03 End");
	}
	
	return bFail;
}

BOOL32 _TestCase04(void)
{
	UINT32 nCount;
	UINT32 nStartLPN = 0;
	UINT32 nNumLPNs = 10240; // 8 bank * 10 Block, Let NUM_LOG_BLOCKS be less than 10
	UINT32 nLPN;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	UINT32 nReadLPN;
	BOOL32 bFail = FALSE;

	for (nLPN = 0; nLPN < nNumLPNs; nLPN++)
	{
		// 4Byte Random Value
		_write_dram_32((UINT32)gpnSavedValues + nLPN * 4, 0);
	}

	uart_printf("DBG> _TestCase04 Start : nStartLPN 0x%x, nNumLPNs 0x%x", nStartLPN, nNumLPNs);

	for (nCount = 0; nCount < 3; nCount++)
	{
		uart_printf("DBG> Count (%d/%d)", nCount + 1, 3);

		for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
		{
			nWriteValue = _GetRandValue();
			// 4Byte LPN + 4Byte Random Value
			_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id), nLPN);
			_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + 4, nWriteValue);

			// Save Random Value
			_write_dram_32((UINT32)gpnSavedValues + (nLPN - nStartLPN) * 4, nWriteValue);

			ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		}

		for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
		{
			ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
			flash_finish();

			if (0 == g_ftl_read_buf_id)
			{
				nReadLPN = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + 4);
			}
			else
			{
				nReadLPN = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + 4);
			}

			nWriteValue = _read_dram_32((UINT32)gpnSavedValues + (nLPN - nStartLPN) * 4);
			if ((nLPN != nReadLPN) || (nWriteValue != nReadValue))
			{
				uart_printf("ERR> Data Miscompare : nLPN 0x%x, nWriteValue 0x%x, nReadLPN 0x%x, nReadValue 0x%x", nLPN, nWriteValue, nReadLPN, nReadValue);
				bFail = TRUE;
				goto T4;
			}
		}
	}

T4:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase04 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase04 End");
	}
	
	return bFail;
}

BOOL32 _TestCase05(void)
{
	UINT32 nCount;
	UINT32 nStartLPN = 0;
	//UINT32 nNumLPNs = 128 * 2 * 8;
	UINT32 nNumLPNs = 10240; // 8 bank * 10 Block, Let NUM_LOG_BLOCKS be less than 10
	UINT32 nLPN;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	UINT32 nReadLPN;
	BOOL32 bFail = FALSE;

	for (nLPN = 0; nLPN < nNumLPNs; nLPN++)
	{
		// 4Byte Random Value
		_write_dram_32((UINT32)gpnSavedValues + nLPN * 4, 0);
	}

	uart_printf("DBG> _TestCase05 Start : nStartLPN 0x%x, nNumLPNs 0x%x", nStartLPN, nNumLPNs);

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		nWriteValue = _GetRandValue();
		// 4Byte LPN + 4Byte Random Value
		_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id), nLPN);
		_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + 4, nWriteValue);

		// Save Random Value
		_write_dram_32((UINT32)gpnSavedValues + (nLPN - nStartLPN) * 4, nWriteValue);

		ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
	}

	for (nCount = 0; nCount < 10; nCount++)
	//for (nCount = 0; nCount < nNumLPNs*3; nCount++)
	{
		// Random LPN
		nLPN = _GetRandValue() % nNumLPNs + nStartLPN;

		nWriteValue = _GetRandValue();
		// 4Byte LPN + 4Byte Random Value
		_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id), nLPN);
		_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + 4, nWriteValue);

		// Save Random Value
		_write_dram_32((UINT32)gpnSavedValues + (nLPN - nStartLPN) * 4, nWriteValue);

		ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
	}

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		flash_finish();

		if (0 == g_ftl_read_buf_id)
		{
			nReadLPN = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
			nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + 4);
		}
		else
		{
			nReadLPN = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
			nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + 4);
		}

		nWriteValue = _read_dram_32((UINT32)gpnSavedValues + (nLPN - nStartLPN) * 4);
		if ((nLPN != nReadLPN) || (nWriteValue != nReadValue))
		{
			uart_printf("ERR> Data Miscompare : nLPN 0x%x, nWriteValue 0x%x, nReadLPN 0x%x, nReadValue 0x%x", nLPN, nWriteValue, nReadLPN, nReadValue);
			bFail = TRUE;
			goto T5;
		}
	}

T5:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase05 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase05 End");
	}
	
	return bFail;
}

BOOL32 _TestCase06(void)
{
	UINT32 nCount;
	UINT32 nStartLPN = 0;
	UINT32 nNumLPNs = 10240; // 8 bank * 10 Block, Let NUM_LOG_BLOCKS be less than 10
	//UINT32 nNumLPNs = 1;
	UINT32 nLPN;
	UINT32 nSectorOffset;
	UINT32 nWriteValue;
	UINT32 nReadValue;
	UINT32 nReadLBA;
	UINT32 nStartLBA;
	UINT32 nSectorCount;
	BOOL32 bFail = FALSE;
	UINT32 nWriteBufAddr;

	for (nStartLBA = 0; nStartLBA < SECTORS_PER_PAGE * nNumLPNs; nStartLBA++)
	{
		// 4Byte Random Value
		//uart_printf ("DGB> _TestCase06 : nStartLBA %d / %d", nStartLBA, SECTORS_PER_PAGE * nNumLPNs);
		_write_dram_32((UINT32)gpnSavedValues + nStartLBA * 4, 0);
	}

	uart_printf("DBG> _TestCase06 Start : nStartLPN 0x%x, nNumLPNs 0x%x", nStartLPN, nNumLPNs);

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		nWriteValue = _GetRandValue();
		for (nSectorOffset = 0; nSectorOffset < SECTORS_PER_PAGE; nSectorOffset++)
		{
			// 4Byte LPN + 4Byte Random Value
			_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + (nSectorOffset * 512), (nLPN * SECTORS_PER_PAGE + nSectorOffset));
			_write_dram_32((UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + (nSectorOffset * 512) + 4, nWriteValue);

			// Save Random Value
			_write_dram_32((UINT32)gpnSavedValues + ((nLPN - nStartLPN) * SECTORS_PER_PAGE + nSectorOffset) * 4, nWriteValue);
		}

		ftl_write(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
	}

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		flash_finish();

		for (nSectorOffset = 0; nSectorOffset < SECTORS_PER_PAGE; nSectorOffset++)
		{
			if (0 == g_ftl_read_buf_id)
			{
				nReadLBA = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + (nSectorOffset * 512));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + (nSectorOffset * 512) + 4);
			}
			else
			{
				nReadLBA = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + (nSectorOffset * 512));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + (nSectorOffset * 512) + 4);
			}

			nWriteValue = _read_dram_32((UINT32)gpnSavedValues + ((nLPN - nStartLPN) * SECTORS_PER_PAGE + nSectorOffset) * 4);
			if (((nLPN * SECTORS_PER_PAGE + nSectorOffset) != nReadLBA) || (nWriteValue != nReadValue))
			{
				uart_printf("ERR> Data Miscompare (PreVerify) : nLBA 0x%x, nWriteValue 0x%x, nReadLBA 0x%x, nReadValue 0x%x", nLPN * SECTORS_PER_PAGE + nSectorOffset, nWriteValue, nReadLBA, nReadValue);
				bFail = TRUE;
				goto T6;
			}
		}
	}

	for (nCount = 0; nCount < 20; nCount++)
	{
		uart_printf("DBG> Count (%d/%d)", nCount + 1, 20);

		// Random Sector
		nSectorCount = _GetRandValue() % 8000 + 1;

		// Random LPN
		nStartLBA = _GetRandValue() % (nNumLPNs * SECTORS_PER_PAGE - nSectorCount + 1);

		nWriteBufAddr = (UINT32)WR_BUF_PTR(g_ftl_write_buf_id) + (nStartLBA % SECTORS_PER_PAGE) * 512;

		for (nSectorOffset = 0; nSectorOffset < nSectorCount; nSectorOffset++)
		{
			nWriteValue = _GetRandValue();
			// 4Byte LPN + 4Byte Random Value
			_write_dram_32(nWriteBufAddr, (nStartLBA + nSectorOffset));
			_write_dram_32(nWriteBufAddr + 4, nWriteValue);

			// Save Random Value
			_write_dram_32((UINT32)gpnSavedValues + (nStartLBA + nSectorOffset) * 4, nWriteValue);

			nWriteBufAddr += 512;
			if ((UINT32)WR_BUF_PTR(NUM_WR_BUFFERS) <= nWriteBufAddr)
			{
				nWriteBufAddr = (UINT32)WR_BUF_PTR(0);
			}
		}

		ftl_write(nStartLBA, nSectorCount);
		flash_finish();
	}

	for (nLPN = nStartLPN; nLPN < nStartLPN + nNumLPNs; nLPN++)
	{
		ftl_read(nLPN * SECTORS_PER_PAGE, SECTORS_PER_PAGE);
		flash_finish();

		for (nSectorOffset = 0; nSectorOffset < SECTORS_PER_PAGE; nSectorOffset++)
		{
			if (0 == g_ftl_read_buf_id)
			{
				nReadLBA = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + (nSectorOffset * 512));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(NUM_RD_BUFFERS - 1) + (nSectorOffset * 512) + 4);
			}
			else
			{
				nReadLBA = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + (nSectorOffset * 512));
				nReadValue = _read_dram_32((UINT32)RD_BUF_PTR(g_ftl_read_buf_id - 1) + (nSectorOffset * 512) + 4);
			}

			nWriteValue = _read_dram_32((UINT32)gpnSavedValues + ((nLPN - nStartLPN) * SECTORS_PER_PAGE + nSectorOffset) * 4);
			if (((nLPN * SECTORS_PER_PAGE + nSectorOffset) != nReadLBA) || (nWriteValue != nReadValue))
			{
				uart_printf("ERR> Data Miscompare : nLBA 0x%x, nWriteValue 0x%x, nReadLBA 0x%x, nReadValue 0x%x", nLPN * SECTORS_PER_PAGE + nSectorOffset, nWriteValue, nReadLBA, nReadValue);
				bFail = TRUE;
				goto T6;
			}
		}
	}

T6:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase06 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase06 End");
	}
	
	return bFail;
}

BOOL32 _TestCase07(void)
{
	UINT32 nCount;
	UINT32 nSubCount;
	UINT32 nIndex;
	UINT32 nReadValue;
	BOOL32 bFail = FALSE;
	
	uart_printf("DBG> _TestCase07 Start");

	for (nIndex = 0; nIndex < PAGES_PER_VBLK; nIndex++)
	{
		_write_dram_32(WR_BUF_PTR(g_ftl_write_buf_id), nIndex);

		ftl_write(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);
	}	

	for (nCount = 0; nCount < 3; nCount++)
	{
		uart_printf("DBG> Count (%d/%d)", nCount + 1, 3);
		for (nSubCount = 0; nSubCount < 100; nSubCount++)
		{
			nIndex = _GetRandValue() % PAGES_PER_BLK;
			_write_dram_32(WR_BUF_PTR(g_ftl_write_buf_id), nIndex);

			ftl_write(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);

			ftl_read(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);
			flash_finish();

			if (0 == g_ftl_read_buf_id)
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
			}
			else
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
			}

			if (nIndex != nReadValue)
			{
				uart_printf("ERR> Data Miscompare (Verify Imm) : nIndex 0x%x, nReadValue 0x%x", nIndex, nReadValue);
				bFail = TRUE;
				goto T7;
			}
		}

		UINT32 i = 0;
		for (nIndex = 0; nIndex < PAGES_PER_VBLK; nIndex++)
		{
			ftl_read(nIndex * SECTORS_PER_PAGE * NUM_BANKS, SECTORS_PER_PAGE);
			flash_finish();

			if (0 == g_ftl_read_buf_id)
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(NUM_RD_BUFFERS - 1));
			}
			else
			{
				nReadValue = _read_dram_32(RD_BUF_PTR(g_ftl_read_buf_id - 1));
			}

			if (nIndex != nReadValue)
			{
				uart_printf("ERR> Data Miscompare %d : nIndex 0x%x, nReadValue 0x%x", ++i, nIndex, nReadValue);
				bFail = TRUE;
				goto T7;
			}
		}
	}

T7:
	if (TRUE == bFail)
	{
		uart_printf("ERR> _TestCase07 Failed");
	}
	else
	{
		uart_printf("DBG> _TestCase07 End");
	}
	
	return bFail;
}