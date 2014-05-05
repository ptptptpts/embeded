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

//#define __TEST_GC
//#define __TEST_WRT
//#define __TEST_RD
//#define __TEST_LB
//#define __TEST_LOGGING
//#define __TEST_PWRECV

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
+ BAD_BLK_BMP_BYTES + DATA_BLK_BYTES + LOG_BLK_BYTES + EMPTY_BLK_BYTES + FTL_TEST_BYTES)

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
#define COPY_BUF_BYTES		(NUM_COPY_BUFFERS * BYTES_PER_PAGE)				// bank당 한 page의 Copy용 buffer를 제공 (bank를 추가할 경우를 대비해 32개의 bank용 buffer를 모두 생성)

#define FTL_BUF_ADDR		(COPY_BUF_ADDR + COPY_BUF_BYTES)				// a buffer dedicated to FTL internal purpose
#define FTL_BUF_BYTES		(NUM_FTL_BUFFERS * BYTES_PER_PAGE)				// bank당 한 page의 FTL용 buffer를 제공 (현재 달려있는 bank만큼의 buffer만 사용)

#define HIL_BUF_ADDR		(FTL_BUF_ADDR + FTL_BUF_BYTES)					// a buffer dedicated to HIL internal purpose
#define HIL_BUF_BYTES		(NUM_HIL_BUFFERS * BYTES_PER_PAGE)

#define TEMP_BUF_ADDR		(HIL_BUF_ADDR + HIL_BUF_BYTES)					// general purpose buffer
#define TEMP_BUF_BYTES		(NUM_TEMP_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(TEMP_BUF_ADDR + TEMP_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

// logical address를 virtual address로 연결하는 table
// page valid bitmap, virtual block 시작 주소, 추가 데이터 저장
// | .... .... | ~ | .... .... | .... .... | ~ | .... .... | .... .... |
// 0   virtual block number   4      valid page bitmap    20  option  21
// virtual block address	: logical block이 위치한 virtual block의 번호 저장 ( 0x00000000 : Invalid)
// valid page bitmap		: data block의 page가 valid 상태인지 비트로 기록 ('1' : valid)
// option					: bit 별로 data block의 상태 기록
//							0 - data block의 exist 여부 ('1' : exist)
//							1 - log block 사용 여부 ('1' : log block 사용)
//							2~7 - log block 번호 (0 ~ 63 max = 63)
#define DATA_BLK_ADDR	(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)			// 한 block 안에 있는 한 page마다 valid를 한 bit로 표시
//#define DATA_BLK_SIZE	(sizeof(UINT32) + (PAGES_PER_BLK / 8) + 1)
#define DATA_BLK_SIZE		24
#define DATA_BLK_BYTES	(((NUM_VBLKS * DATA_BLK_SIZE) / 128 + 1) * 128)		
#define DATA_BLK_VADDR		0
#define DATA_BLK_VPBMP		4
#define DATA_BLK_OP			20
#define DATA_BLK_OP_EX		7
#define DATA_BLK_OP_LB		6
#define DATA_BLK_OP_EX_MSK	0x80
#define DATA_BLK_OP_LB_MSK	0x40
#define DATA_BLK_OP_LBNUM	0x3F


// page mapping table, log block에 연결된 data block 주소 저장
// | .... .... | ~ | .... .... | .... .... | ~ | .... .... | .... .... | .... .... | .... .... | .... .... | ~ | .... .... |
// 0   virtual block address   4    logical block number   8  pagecnt  9  validpg 10 merge st. 11   page number map     139
// virtual block address	: log block이 위치하고 있는 virtual block 번호
// logical block number  	: log block을 소유하고 있는 logical block number를 표시
// page cnt					: log block에서 사용한 page 갯수
// valid page				: log block에서 valid한 page의 갯수
// merge stat				: merge 상태를 나타낸다 (0 : swap, partial merge, 1 : full merge)
// page number map			: log block의 page가 block의 몇번째 page의 data를 담고 있는지 기록 (0~255, 244 : dirty, 255 : invalid)
#define LOG_BLK_ADDR	(DATA_BLK_ADDR + DATA_BLK_BYTES)		
//#define LOG_BLK_SIZE	(PAGES_PER_BLK + 2 * sizeof (UINT32) + 1)
#define LOG_BLK_SIZE		140
#define LOG_BLK_BYTES		(((NUM_LOG_BLKS * NUM_BANKS * LOG_BLK_SIZE) / 128 + 1) * 128)
#define LOG_BLK_VADDR		0
#define LOG_BLK_LADDR		4
#define LOG_BLK_PGCNT		8
#define LOG_BLK_VLDPG		9
#define LOG_BLK_MERGE		10
#define LOG_BLK_PGMAP		11

// empty 상태의 virtual block들을 표시하는 bitmap
// | .... .... | ~ | .... .... |
// 0      block bit map       
// block bit map			: block이 empty상태인지 bit단위로 표시 ( '1' : empty )
#define EMPTY_BLK_ADDR	(LOG_BLK_ADDR + LOG_BLK_BYTES)
#define EMPTY_BLK_BYTES	(((NUM_BANKS * VBLKS_PER_BANK / 8)/ 128 + 1) * 128)
#define EMPTY_BLK_BND		0

#define FTL_TEST_ADDR		(EMPTY_BLK_ADDR + EMPTY_BLK_BYTES)
#define FTL_TEST_BYTES		(4 * 1024 * 1024)

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

BOOL32 check_format_mark(void);

#endif //FTL_H
