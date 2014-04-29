// Copyright 2011 INDILINX Co., Ltd.
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
// GreedyFTL header file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//

#ifndef FTL_H
#define FTL_H


/////////////////
// DRAM buffers
/////////////////

#define NUM_RW_BUFFERS		((DRAM_SIZE - DRAM_BYTES_OTHER) / BYTES_PER_PAGE - 1)
#define NUM_RD_BUFFERS		(((NUM_RW_BUFFERS / 8) + NUM_BANKS - 1) / NUM_BANKS * NUM_BANKS)
#define NUM_WR_BUFFERS		(NUM_RW_BUFFERS - NUM_RD_BUFFERS)
#define NUM_COPY_BUFFERS	NUM_BANKS_MAX
#define NUM_FTL_BUFFERS		NUM_BANKS
#define NUM_HIL_BUFFERS		1
#define NUM_TEMP_BUFFERS	1

#define NUM_LOG_BLKS		3

#define DRAM_BYTES_OTHER	((NUM_COPY_BUFFERS + NUM_FTL_BUFFERS + NUM_HIL_BUFFERS + NUM_TEMP_BUFFERS) * BYTES_PER_PAGE \
+ BAD_BLK_BMP_BYTES + DATA_BLK_MAP_BYTES + LOG_BLK_MAP_BYTES + VCOUNT_BYTES + EMPTY_BLK_BMP_BYTES)

#define WR_BUF_PTR(BUF_ID)	(WR_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define WR_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - WR_BUF_ADDR) / BYTES_PER_PAGE)
#define RD_BUF_PTR(BUF_ID)	(RD_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define RD_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - RD_BUF_ADDR) / BYTES_PER_PAGE)

#define _COPY_BUF(RBANK)	(COPY_BUF_ADDR + (RBANK) * BYTES_PER_PAGE)
#define COPY_BUF(BANK)		_COPY_BUF(REAL_BANK(BANK))
#define FTL_BUF(BANK)       (FTL_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

///////////////////////////////
// DRAM segmentation
///////////////////////////////

#define RD_BUF_ADDR			DRAM_BASE										// base address of SATA read buffers
#define RD_BUF_BYTES		(NUM_RD_BUFFERS * BYTES_PER_PAGE)

#define WR_BUF_ADDR			(RD_BUF_ADDR + RD_BUF_BYTES)					// base address of SATA write buffers
#define WR_BUF_BYTES		(NUM_WR_BUFFERS * BYTES_PER_PAGE)

#define COPY_BUF_ADDR		(WR_BUF_ADDR + WR_BUF_BYTES)					// base address of flash copy buffers
#define COPY_BUF_BYTES		(NUM_COPY_BUFFERS * BYTES_PER_PAGE)				// bank�� �� page�� Copy�� buffer�� ���� (bank�� �߰��� ��츦 ����� 32���� bank�� buffer�� ��� ����)

#define FTL_BUF_ADDR		(COPY_BUF_ADDR + COPY_BUF_BYTES)				// a buffer dedicated to FTL internal purpose
#define FTL_BUF_BYTES		(NUM_FTL_BUFFERS * BYTES_PER_PAGE)				// bank�� �� page�� FTL�� buffer�� ���� (���� �޷��ִ� bank��ŭ�� buffer�� ���)

#define HIL_BUF_ADDR		(FTL_BUF_ADDR + FTL_BUF_BYTES)					// a buffer dedicated to HIL internal purpose
#define HIL_BUF_BYTES		(NUM_HIL_BUFFERS * BYTES_PER_PAGE)

#define TEMP_BUF_ADDR		(HIL_BUF_ADDR + HIL_BUF_BYTES)					// general purpose buffer
#define TEMP_BUF_BYTES		(NUM_TEMP_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(TEMP_BUF_ADDR + TEMP_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

// logical address�� virtual address�� �����ϴ� table
// page valid bitmap, virtual block ���� �ּ�, �߰� ������ ����
// | .... .... | ~ | .... .... | .... .... | ~ | .... .... | .... .... |
// 0   virtual block number   4      valid page bitmap    20  option  21
// virtual block address	: logical block�� ��ġ�� virtual block�� ��ȣ ���� ( 0x00000000 : Invalid)
// valid page bitmap		: data block�� page�� valid �������� ��Ʈ�� ��� ('1' : valid)
// option					: bit ���� data block�� ���� ���
//							0 - data block�� exist ���� ('1' : exist)
//							1 - log block ��� ���� ('1' : log block ���)
//							2~7 - log block ��ȣ (0 ~ 63 max = 63)
#define DATA_BLK_MAP_ADDR	(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)			// �� block �ȿ� �ִ� �� page���� valid�� �� bit�� ǥ��
#define DATA_BLK_MAP_SIZE	(sizeof(UINT32) + (PAGES_PER_BLK / 8) + 1)
#define DATA_BLK_MAP_BYTES	(NUM_VBLKS * DATA_BLK_MAP_SIZE)		
#define DATA_BLK_VADDR		0
#define DATA_BLK_VPBMP		4
#define DATA_BLK_OP			20
#define DATA_BLK_OP_EX		0
#define DATA_BLK_OP_LB		1
#define DATA_BLK_OP_EX_MSK	0x80
#define DATA_BLK_OP_LB_MSK	0x40
#define DATA_BLK_OP_LBNUM	0x3F


// page mapping table, log block�� ����� data block �ּ� ����
// | .... .... | ~ | .... .... | .... .... | ~ | .... .... | .... .... | .... .... | .... .... | ~ | .... .... |
// 0   virtual block address   4    logical block number   8    bank   9  pagecnt  10      page number map     138
// virtual block address	: log block�� ��ġ�ϰ� �ִ� virtual block ��ȣ
// logical block number  	: log block�� �����ϰ� �ִ� logical block number�� ǥ��
// virtual block bank		: virtual block�� ��ġ�� bank ��ȣ
// page cnt					: log block���� ����� page ����
// page number map			: log block�� page�� block�� ���° page�� data�� ��� �ִ��� ��� (0~255, 244 : dirty, 255 : invalid)
#define LOG_BLK_MAP_ADDR	(DATA_BLK_MAP_ADDR + DATA_BLK_MAP_BYTES)		
#define LOG_BLK_MAP_SIZE	(PAGES_PER_BLK + 2 * sizeof (UINT32) + 2)
#define LOG_BLK_MAP_BYTES	(NUM_LOG_BLKS * LOG_BLK_MAP_SIZE)
#define LOG_BLK_VADDR		0
#define LOG_BLK_LADDR		4
#define LOG_BLK_BANK		8
#define LOG_BLK_PGCNT		9
#define LOG_BLK_PGMAP		10

#define VCOUNT_ADDR			(LOG_BLK_MAP_ADDR + LOG_BLK_MAP_BYTES)
#define VCOUNT_BYTES		((NUM_BANKS * VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR)

// empty ������ virtual block���� ǥ���ϴ� bitmap
// | .... .... | ~ | .... .... |
// 0      block bit map       
// block bit map			: block�� empty�������� bit������ ǥ�� ( '1' : empty )
#define EMPTY_BLK_BMP_ADDR	(VCOUNT_ADDR + VCOUNT_BYTES)
#define EMPTY_BLK_BMP_BYTES	(NUM_BANKS * VBLKS_PER_BANK / 8)
#define EMPTY_BLK_BND		0

#define BLKS_PER_BANK		VBLKS_PER_BANK


///////////////////////////////
// FTL public functions
///////////////////////////////

void ftl_open(void);
void ftl_read(UINT32 const lba, UINT32 const num_sectors);
void ftl_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_flush(void);
void ftl_isr(void);

#endif //FTL_H