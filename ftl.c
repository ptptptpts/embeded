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
// GreedyFTL source file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// - support POR
//  + fixed metadata area (Misc. block/Map block)
//  + logging entire FTL metadata when each ATA commands(idle/ready/standby) was issued
//

#include "jasmine.h"

//#define __TEST_WRT
//#define __TEST_RD

//----------------------------------
// macro
//----------------------------------
#define BYTES_PER_BLK		(BYTES_PER_PAGE * PAGES_PER_BLK)
#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK    ((DATA_BLK_BYTES / NUM_BANKS) / BYTES_PER_BLK + 1)
#define LOGBLKMAP_PER_BANK	((LOG_BLK_BYTES / NUM_BANKS) / BYTES_PER_BLK + 1)
#define EMPTYBLK_PER_BANK	((EMPTY_BLK_BYTES / NUM_BANKS) / BYTES_PER_BLK + 1)
#define META_BLKS_PER_BANK  (1 + 1 + MAPBLKS_PER_BANK + LOGBLKMAP_PER_BANK + EMPTYBLK_PER_BANK) // include block #0, misc block

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)

//----------------------------------
// metadata structure
//----------------------------------
typedef struct _ftl_statistics
{
	UINT32 gc_cnt;
	UINT32 page_wcount; // page write count
}ftl_statistics;

typedef struct _misc_metadata
{
	//UINT32 cur_write_vpn; // physical page for new write
	UINT32 cur_miscblk_vbn; // current write vpn for logging the misc. metadata
	UINT32 cur_mapblk_vbn[MAPBLKS_PER_BANK]; // current write vpn for logging the age mapping info.
	UINT32 cur_logblkmap_vbn[LOGBLKMAP_PER_BANK];
	UINT32 cur_emptyblk_vbn [EMPTYBLK_PER_BANK];
	UINT32 gc_vblock; // vblock number for garbage collection
	UINT32 log_blk_cnt;
	//UINT32 free_blk_cnt; // total number of free block count
	//UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC
}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;


//
// New Implement
//

//============================================================
static UINT32		log_blk_cnt[NUM_BANKS];
//============================================================

//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 : data block mapping table
// block #3 : log block mapping table
// block #4 : empty block mapping table
// block #5 : a free block for gc
// block #6~ : data blocks

//----------------------------------
// macro functions
//----------------------------------
#define is_full_all_blks(bank)  (g_misc_meta[bank].free_blk_cnt == 1)
#define inc_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt--)
#define dec_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt++)
#define inc_mapblk_vpn(bank, mapblk_lbn)    (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank)               (g_misc_meta[bank].cur_miscblk_vpn++)

#define get_logblk_cnt(bank)			(g_misc_meta[bank].log_blk_cnt)
#define inc_logblk_cnt(bank)			(g_misc_meta[bank].log_blk_cnt++)
#define dec_logblk_cnt(bank)			(g_misc_meta[bank].log_blk_cnt--)
#define set_logblk_cnt(bank, cnt)		(g_misc_meta[bank].log_blk_cnt = cnt)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn)             ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank)         (g_bad_blk_count[bank])
//#define get_cur_write_vpn(bank)       (g_misc_meta[bank].cur_write_vpn)
//#define set_new_write_vpn(bank, vpn)  (g_misc_meta[bank].cur_write_vpn = vpn)
#define get_gc_vblock(bank)           (g_misc_meta[bank].gc_vblock)
#define set_gc_vblock(bank, vblock)   (g_misc_meta[bank].gc_vblock = vblock)
//#define set_lpn(bank, page_num, lpn)  (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num] = lpn)
//#define get_lpn(bank, page_num)       (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num])
#define get_miscblk_vbn(bank)         (g_misc_meta[bank].cur_miscblk_vbn)
#define set_miscblk_vbn(bank, vbn)    (g_misc_meta[bank].cur_miscblk_vbn = vbn)

#define get_mapblk_vbn(bank, mapblk_lbn)      (g_misc_meta[bank].cur_mapblk_vbn[mapblk_lbn])
#define set_mapblk_vbn(bank, mapblk_lbn, vbn) (g_misc_meta[bank].cur_mapblk_vbn[mapblk_lbn] = vbn)

#define get_logblkmap_vbn(bank, mapblk_lbn)		(g_misc_meta[bank].cur_logblkmap_vbn[mapblk_lbn])
#define set_logblkmap_vbn(bank, mapblk_lbn, vbn)		(g_misc_meta[bank].cur_logblkmap_vbn[mapblk_lbn] = vbn)

#define get_emptyblk_vbn(bank, mapblk_lbn)		(g_misc_meta[bank].cur_emptyblk_vbn[mapblk_lbn])
#define set_emptyblk_vbn(bank, mapblk_lbn, vbn)		(g_misc_meta[bank].cur_emptyblk_vbn[mapblk_lbn] = vbn)

#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static void   sanity_check(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
//static void   set_vpn(UINT32 const lpn, UINT32 const vpn);
//static void   garbage_collection(UINT32 const bank);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static UINT32 get_vpn(UINT32 const lpn);
//static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank);


//=================================================================
static UINT32 get_dblock (UINT32 const lpn);
static BOOL8 is_exist_dblock (UINT32 const lpn);
static BOOL8 is_exist_dpage (UINT32 const lpn);
static BOOL8 is_valid_dblock (UINT32 const lpn);
static UINT32 assign_dblock (UINT32 const lpn);
static UINT32 get_dpage (UINT32 const lpn);
static UINT32 set_dpage (UINT32 const lpn);

static BOOL8 set_invalid_dpage (UINT32 const lpn);
static BOOL8 set_dirty_log_page (UINT32 const lpn);

static UINT32 set_log_blk (UINT32 const lpn);
static UINT32 get_log_blk (UINT32 const lpn);

static UINT32 get_log_page (UINT32 const lpn);
static UINT32 set_log_page (UINT32 const lpn);

static UINT32 get_logblk_page (UINT32 const lblk, UINT32 const lpn);

static UINT32 get_empty_blk (UINT32 const bank);
static BOOL8 set_using_blk (UINT32 const bank, UINT32 const vbn);
static BOOL8 set_empty_blk (UINT32 const bank, UINT32 const vbn);

static UINT32 garbage_collection (UINT32 const bank, UINT32 const logblk);
static UINT32 full_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn);
static UINT32 partial_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn);
static UINT32 get_victim_block (UINT32 const bank);

static void logging_misc_block (void);
static void logging_data_block (void);
static void logging_log_block (void);
static void logging_empty_block (void);

static void load_misc_block (void);
static void load_data_block (void);
static void load_log_block (void);
static void load_empty_block (void);
//=================================================================

static void sanity_check(void)
{
	UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
		+ HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + EMPTY_BLK_BYTES
		+ DATA_BLK_BYTES + LOG_BLK_BYTES + FTL_TEST_BYTES;

	if ((dram_requirement > DRAM_SIZE) || // DRAM metadata size check
		(sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
	{
		led_blink();
		while (1);
	}
}

static void build_bad_blk_list(void)
{
	UINT32 bank, num_entries, result, vblk_offset;
	scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

	mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

	disable_irq();

	flash_clear_irq();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
		SETREG(FCP_BANK, REAL_BANK(bank));
		SETREG(FCP_OPTION, FO_E);
		SETREG(FCP_DMA_ADDR, (UINT32) scan_list);
		SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
		SETREG(FCP_COL, 0);
		SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
		SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

		SETREG(FCP_ISSUE, NULL);
		while ((GETREG(WR_STAT) & 0x00000001) != 0);
		while (BSP_FSM(bank) != BANK_IDLE);

		num_entries = NULL;
		result = OK;

		if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
		{
			result = FAIL;
		}
		else
		{
			UINT32 i;

			num_entries = read_dram_16(&(scan_list->num_entries));

			if (num_entries > SCAN_LIST_ITEMS)
			{
				result = FAIL;
			}
			else
			{
				for (i = 0; i < num_entries; i++)
				{
					UINT16 entry = read_dram_16(scan_list->list + i);
					UINT16 pblk_offset = entry & 0x7FFF;

					if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
					{
						#if OPTION_REDUCED_CAPACITY == FALSE
						result = FAIL;
						#endif
					}
					else
					{
						write_dram_16(scan_list->list + i, pblk_offset);
					}
				}
			}
		}

		if (result == FAIL)
		{
			num_entries = 0;  // We cannot trust this scan list. Perhaps a software bug.
		}
		else
		{
			write_dram_16(&(scan_list->num_entries), 0);
		}

		g_bad_blk_count[bank] = 0;

		for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
		{
			BOOL32 bad = FALSE;

			#if OPTION_2_PLANE
			{
				UINT32 pblk_offset;

				pblk_offset = vblk_offset * NUM_PLANES;

				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}

				pblk_offset = vblk_offset * NUM_PLANES + 1;

				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#else
			{
				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#endif

			if (bad)
			{
				g_bad_blk_count[bank]++;
				set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
			}
		}
	}
}

void ftl_open(void)
{
	// debugging example 1 - use breakpoint statement!
	/* *(UINT32*)0xFFFFFFFE = 10; */

	/* UINT32 volatile g_break = 0; */
	/* while (g_break == 0); */
	
	led(0);
	sanity_check();
	//----------------------------------------
	// read scan lists from NAND flash
	// and build bitmap of bad blocks
	//----------------------------------------
	build_bad_blk_list();
	//----------------------------------------
	// If necessary, do low-level format
	// format() should be called after loading scan lists, because format() calls is_bad_block().
	//----------------------------------------
	if (check_format_mark() == FALSE) 
	//if (TRUE)
	{
		uart_print("do format");
		format();
		uart_print("end format");
	}
	// load FTL metadata
	else
	{
		load_metadata();
	}
	g_ftl_read_buf_id = 0;
	g_ftl_write_buf_id = 0;

	// This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
	flash_clear_irq();

	SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
	SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

	enable_irq();
}

// power off recovery
void ftl_flush(void)
{
#ifdef __TEST_LOGGING
	uart_printf ("ftl_flush :: logging metadata");
#endif
	logging_misc_block();
	logging_data_block ();
	logging_log_block ();
	logging_empty_block ();
#ifdef __TEST_LOGGING
	uart_printf ("ftl_flush :: logging complete");
#endif
}

// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
	ASSERT(lba + num_sectors <= NUM_LSECTORS);
	ASSERT(num_sectors > 0);

	ftl_write(lba, num_sectors);
}

void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
	UINT32 remain_sects, num_sectors_to_read;
	UINT32 lpn, sect_offset;
	UINT32 bank, vpn;
	
	lpn          = lba / SECTORS_PER_PAGE;	// byte address에서 logical page number를 계산
	sect_offset  = lba % SECTORS_PER_PAGE;	// logical sector offset을 계산
	remain_sects = num_sectors;	

	while (remain_sects != 0)	// nand flash에서 page별로 data를 read
	{
		if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)	// 한 page 초과 읽을 data가 남아있을 경우, offset에서부터 읽을 data가 한 page를 초과할 경우
		{
			num_sectors_to_read = remain_sects;
		}
		else // 한 page 이하 남아있을 경우
		{
			num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
		}

		// nand flash에서 bank와 page address를 받아온다
		bank = get_num_bank(lpn); // page striping
		vpn  = get_vpn(lpn);
		CHECK_VPAGE(vpn);

#ifdef __TEST_RD
		uart_printf ("ftl_read :: [lpn %d] [bank %d] [vpn %d]\n", lpn, bank, vpn);
#endif

		// 실제 page가 존재할 경우 nand flash에서 읽어온다
		if (vpn != NULL)
		{
			nand_page_ptread_to_host(bank,
									 vpn / PAGES_PER_BLK,
									 vpn % PAGES_PER_BLK,
									 sect_offset,
									 num_sectors_to_read);
		}

		// nand flash에 page가 존재하지 않을 경우 (쓰지 않은 page에서 read를 할 경우)
		// The host is requesting to read a logical page that has never been written to.
		else
		{
			UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

			#if OPTION_FTL_TEST == 0
			while (next_read_buf_id == GETREG(SATA_RBUF_PTR));	// wait if the read buffer is full (slow host)
			#endif

			// fix bug @ v.1.0.6
			// Send 0xFF...FF to host when the host request to read the sector that has never been written.
			// In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
			// However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
			// 쓰여진 적 없는 page의 경우 dram에 0000으로 임의의 data를 채운다
			mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset*BYTES_PER_SECTOR,
						 0x00000000, num_sectors_to_read*BYTES_PER_SECTOR);

			flash_finish();

			SETREG(BM_STACK_RDSET, next_read_buf_id);	// change bm_read_limit
			SETREG(BM_STACK_RESET, 0x02);				// change bm_read_limit

			g_ftl_read_buf_id = next_read_buf_id;
		}
		sect_offset   = 0;
		remain_sects -= num_sectors_to_read;
		lpn++;
	}
}

void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
	UINT32 remain_sects, num_sectors_to_write;
	UINT32 lpn, sect_offset;

	//uart_printf ("write start : %d %d\n", lba, num_sectors);

	lpn          = lba / SECTORS_PER_PAGE;
	sect_offset  = lba % SECTORS_PER_PAGE;
	remain_sects = num_sectors;

	while (remain_sects != 0)
	{
		if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
		{
			num_sectors_to_write = remain_sects;
		}
		else
		{
			num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
		}
		// single page write individually
		write_page(lpn, sect_offset, num_sectors_to_write);

		sect_offset   = 0;
		remain_sects -= num_sectors_to_write;
		lpn++;
	}

	//uart_printf ("write end : %d %d\n", lba, num_sectors);
}

static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
// 한 page 안에서 nand flash memory에 data를 write 한다
{	
	CHECK_LPAGE(lpn);
	ASSERT(sect_offset < SECTORS_PER_PAGE);
	ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

	UINT32 bank, old_vpn, new_vpn;
	UINT32 vblock, page_num, page_offset, column_cnt;

	bank        = get_num_bank(lpn); // page striping
	page_offset = sect_offset;	
	column_cnt  = num_sectors;
	
	old_vpn  = get_vpn(lpn);	// 이전에  write가 된 page 번호를 받는다

	CHECK_VPAGE (old_vpn);
	//CHECK_VPAGE (new_vpn);
	//ASSERT(old_vpn != new_vpn);

	g_ftl_statistics[bank].page_wcount++;

	// if old data already exist,
	if (old_vpn != NULL)
	{
		vblock   = old_vpn / PAGES_PER_BLK;	// block 번호 계산
		page_num = old_vpn % PAGES_PER_BLK;	// page 번호 계산

		//--------------------------------------------------------------------------------------
		// `Partial programming'
		// we could not determine whether the new data is loaded in the SATA write buffer.
		// Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
		// And then, program whole valid data
		//--------------------------------------------------------------------------------------
		if (num_sectors != SECTORS_PER_PAGE)	// 한 page 전체를 쓰지 않을 경우
		{
			// Performance optimization (but, not proved)
			// To reduce flash memory access, valid hole copy into SATA write buffer after reading whole page
			// Thus, in this case, we need just one full page read + one or two mem_copy
			if ((num_sectors <= 8) && (page_offset != 0))	// page의 맨 앞에서부터 쓰지 않을 경우 (page의 양 끝에 hole이 생김)
			{
				// nand에서 page를 읽어온 후 memory에 left hole과 right hole을 채워넣는다

				// one page async read
				nand_page_read(bank,
							   vblock,
							   page_num,
							   FTL_BUF(bank));
				// copy `left hole sectors' into SATA write buffer
				if (page_offset != 0)
				{
					mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
							 FTL_BUF(bank),
							 page_offset * BYTES_PER_SECTOR);
				}
				// copy `right hole sectors' into SATA write buffer
				if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
				{
					UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

					mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
							 FTL_BUF(bank) + rhole_base,
							 BYTES_PER_PAGE - rhole_base);
				}
			}

			// left/right hole async read operation (two partial page read)
			else
			{
				// left hole이나 right hole만 생겼을 경우 nand에서 직접 memory로 data를 읽어온다
				// read `left hole sectors'
				if (page_offset != 0)
				{
					nand_page_ptread(bank,
									 vblock,
									 page_num,
									 0,
									 page_offset,
									 WR_BUF_PTR(g_ftl_write_buf_id),
									 RETURN_ON_ISSUE);
				}
				// read `right hole sectors'
				if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
				{
					nand_page_ptread(bank,
									 vblock,
									 page_num,
									 page_offset + column_cnt,
									 SECTORS_PER_PAGE - (page_offset + column_cnt),
									 WR_BUF_PTR(g_ftl_write_buf_id),
									 RETURN_ON_ISSUE);
				}
			}
		}
		// page offset과 column count를 한 page에 맞게 align 시킨다
		// full page write
		page_offset = 0;
		column_cnt  = SECTORS_PER_PAGE;
	}
	// 새로 write하는 page인 경우
	else
	{
		// 쓰지 않는 sector는 0x00으로 초기화
		if (num_sectors != SECTORS_PER_PAGE)
		{
			// 한 page에서 partial programming을 할 경우
			if ((num_sectors <= 8) && (page_offset != 0))	
			{
				// copy `left hole sectors' into SATA write buffer
				if (page_offset != 0)
				{
					mem_set_dram (WR_BUF_PTR(g_ftl_write_buf_id),
								NULL, page_offset * BYTES_PER_SECTOR);
				}
				// copy `right hole sectors' into SATA write buffer
				if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
				{
					UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

					mem_set_dram (WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
								NULL, BYTES_PER_PAGE - rhole_base);
				}
			}

			// 두 페이지 이상에서 partial programming을 할 경우
			else
			{
				// left hole이나 right hole만 생겼을 경우 nand에서 직접 memory로 data를 읽어온다
				// read `left hole sectors'
				if (page_offset != 0)
				{
					mem_set_dram(WR_BUF_PTR (g_ftl_write_buf_id),
								NULL, page_offset * BYTES_PER_SECTOR);
				}
				// read `right hole sectors'
				if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
				{
					UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR % BYTES_PER_PAGE;

					mem_set_dram (WR_BUF_PTR (g_ftl_write_buf_id) + rhole_base,
								NULL, BYTES_PER_PAGE - rhole_base);
				}
			}
		}
		// page offset과 column count를 한 page에 맞게 align 시킨다
		// full page write
		page_offset = 0;
		column_cnt  = SECTORS_PER_PAGE;
	}
	

	// 새로 사용할 page의 block 번호와 page offset을 구한다
	new_vpn  = assign_new_write_vpn(lpn);	// 새로 write를 할 page 번호를 받는다
	vblock   = new_vpn / PAGES_PER_BLK;
	page_num = new_vpn % PAGES_PER_BLK;

#ifdef __TEST_WRT
	uart_printf ("write_page lpn : %d", lpn);
	uart_printf ("bank : %d, new vpn : %d, old vpn : %d", bank, new_vpn, old_vpn);
#endif

	// write new data (make sure that the new data is ready in the write buffer frame)
	// (c.f FO_B_SATA_W flag in flash.h)
	// nand flash memory에 데이터를 program 한다
	nand_page_ptprogram_from_host(bank,
								  vblock,
								  page_num,
								  page_offset,
								  column_cnt);

#ifdef __TEST_WRT
	uart_printf ("write_end :: bank : %d, vpn : %d, get_vpn : %d\n", bank, new_vpn, get_vpn(lpn));
#endif		
}

// get vpn from PAGE_MAP 
static UINT32 get_vpn(UINT32 const lpn)
{
	/*
	=======================================
	Need to implementation
	=======================================
	logical page가 data block에 있을 경우
		data block에서 page 주소 반환
	logical page가 log block에 있을 경우
		log block에서 page 주소 반환
	=======================================
	*/
	// 아직 data block이 연결되지 않은 곳
	UINT32 vbn;

	// data block이 없는 경우
	if (is_exist_dblock (lpn) == FALSE)
	{
		//uart_printf ("GET_VPN :: DATA BLOCK ISN'T EXIST");
		return (get_log_page (lpn));
		//return NULL;
	}

	// data block에 data가 존재하는 경우
	if (is_valid_dblock(lpn) == TRUE)
	{
		// data block에서 virtual page number 계산해서 반환
		return (get_dpage (lpn));
	}
	// data block에 data가 없는 경우
	else
	{
		// data block에서 invalid인 경우 log block에서 찾아서 반환 (없을경우 자동으로 NULL)
		return (get_log_page (lpn));
	}

	/*
	CHECK_LPAGE(lpn);
	return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
	*/
}


static UINT32 assign_new_write_vpn(UINT32 const lpn)
// nand flash에서 새로 write를 할 virtual page number를 받아온다
{
	//uart_printf ("assign new write vpn :: lpn %d", lpn);
	//data block이 있는 page인지 검사
	if (is_exist_dblock (lpn) == FALSE) 
	{
		//uart_printf ("data block isn't exist, assign new block");
		//data block이 없을 경우 새로 빈 block을 할당하고 page 주소 반환		
		//uart_printf ("first written block");
		assign_dblock (lpn);	// data block mapping		
	}

	// log block이 연결되었는지 검사
	if (get_log_blk (lpn) == 0xff)
	{
		// log block이 없을 경우 새로 연결
		set_log_blk (lpn);
	}

	// 기록된 적이 있을 경우 data block에 page가 valid 상태인지 검사
	if (is_valid_dblock (lpn) == TRUE) 
	{
		set_invalid_dpage (lpn);		
	}	
			
	// log block에서 page 반환
	return set_log_page (lpn);					
}

static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
	if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
	{
		return FALSE;
	}
	return TRUE;
}

static void format(void)
{
	UINT32 i32, j32;
	UINT32 bank, vblock;

	ASSERT(NUM_MISC_META_SECT > 0);

	uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

	uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
	uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
	uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);

	//----------------------------------------
	// initialize DRAM metadata
	//----------------------------------------

	// data block과 log block mapping 정보가 저장되는 공간 초기화
	
	mem_set_dram (DATA_BLK_ADDR, NULL, DATA_BLK_BYTES);
	mem_set_dram (LOG_BLK_ADDR, NULL, LOG_BLK_BYTES);
	
	for (j32 = 0; j32 < (NUM_LOG_BLKS * NUM_BANKS); j32++)
	{
		for (i32 = 0; i32 < PAGES_PER_BLK; i32++) 
		{
			write_dram_8 (LOG_BLK_ADDR + (j32 * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i32, 0xff);
		}
	}
	
	mem_set_dram(EMPTY_BLK_ADDR, 0xffffffff, EMPTY_BLK_BYTES);

	//----------------------------------------
	// erase all blocks except vblock #0
	//----------------------------------------
	for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (is_bad_block(bank, vblock) == FALSE)
			{
				nand_block_erase(bank, vblock);
			}
		}
	}
	//----------------------------------------
	// initialize SRAM metadata
	//----------------------------------------
	init_metadata_sram();

	// flush metadata to NAND
	//logging_pmap_table();
	//logging_misc_metadata();

	// initialize global variable
	for (i32 = 0; i32 < NUM_BANKS; i32++)
	{
		set_logblk_cnt (i32, 0);
		//log_blk_cnt[i32] = 0;
	}
	//==========================================================

	write_format_mark();
	led(1);
	uart_print("format complete");
}

static void init_metadata_sram(void)
{
	UINT32 bank;
	UINT32 vblock;
	UINT32 mapblk_lbn;

	uart_printf("init_metadata_sram :: MAPBLK %d LOGBLK %d EMPTYBLK %d", 
			MAPBLKS_PER_BANK, LOGBLKMAP_PER_BANK, EMPTYBLK_PER_BANK);

	//----------------------------------------
	// initialize misc. metadata
	//----------------------------------------
	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		set_using_blk (bank, 0);

		//g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
		//g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
		// NOTE: vblock #0,1 don't use for user space

		//----------------------------------------
		// assign misc. block
		//----------------------------------------
		// assumption: vblock #1 = fixed location.
		// Thus if vblock #1 is a bad block, it should be allocate another block.
		vblock = MISCBLK_VBN;
		while (is_bad_block (bank, vblock) == TRUE)
		{
			set_using_blk (bank, vblock);
			vblock++;
		}
		set_miscblk_vbn(bank, vblock);
		set_using_blk (bank, vblock);
		
		//----------------------------------------
		// assign map block
		//----------------------------------------
		mapblk_lbn = 0;
		while (mapblk_lbn < MAPBLKS_PER_BANK)
		{
			vblock++;
			ASSERT(vblock < VBLKS_PER_BANK);
			if (is_bad_block(bank, vblock) == FALSE)
			{
				set_mapblk_vbn(bank, mapblk_lbn, vblock);
				mapblk_lbn++;
			}
			set_using_blk (bank, vblock);			
		}

		//----------------------------------------
		// assign log block map block
		//----------------------------------------
		mapblk_lbn = 0;
		while (mapblk_lbn < LOGBLKMAP_PER_BANK)
		{
			vblock++;
			ASSERT (vblock < VBLKS_PER_BANK);
			if (is_bad_block (bank, vblock) == FALSE)
			{
				set_logblkmap_vbn (bank, mapblk_lbn, vblock);
				mapblk_lbn++;
			}
			set_using_blk (bank, vblock);
		}
		
		//----------------------------------------
		// assign empty block map block
		//----------------------------------------
		mapblk_lbn = 0;
		while (mapblk_lbn < EMPTYBLK_PER_BANK)
		{
			vblock++;
			ASSERT (vblock < VBLKS_PER_BANK);
			if (is_bad_block (bank, vblock) == FALSE)
			{
				set_emptyblk_vbn (bank, mapblk_lbn, vblock);
				mapblk_lbn++;
			}
			set_using_blk (bank, vblock);
		}

		//----------------------------------------
		// assign free block for gc
		//----------------------------------------
		do
		{
			vblock++;
			// NOTE: free block should not be secleted as a victim @ first GC
			// set free block
			set_gc_vblock(bank, vblock);
			set_using_blk (bank, vblock);

			ASSERT(vblock < VBLKS_PER_BANK);
		}while(is_bad_block(bank, vblock) == TRUE);

		// bad block을 사용하지 못하도록 표시
		do
		{
			vblock++;
			if (is_bad_block (bank, vblock) == TRUE)
			{
				set_using_blk (bank, vblock);
			}
		}while (vblock < BLKS_PER_BANK);
	}

	uart_printf ("init_metadata_sram :: end");
}

// load flushed FTL metadta
static void load_metadata(void)
{
#ifdef __TEST_PWRECV
	uart_printf ("load_metadata :: power off recovery");
#endif 
	load_misc_block ();
	load_data_block ();
	load_log_block ();
	load_empty_block ();
#ifdef __TEST_PWRECV
	uart_printf ("load_metadata :: loading metadata end");
#endif 
}

static void write_format_mark(void)
{
	// This function writes a format mark to a page at (bank #0, block #0).

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

	mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

	SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because we have waited for all the banks to become idle before returning from format().
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the write operation
	while (BSP_FSM(0) != BANK_IDLE);
}

BOOL32 check_format_mark(void)
{
	// This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
	UINT32 temp;

	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because scan list loading has been completed just before this function is called.
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the read operation
	while (BSP_FSM(0) != BANK_IDLE);

	// Now that the read operation is complete, we can check interrupt flags.
	temp = BSP_INTR(0) & FIRQ_ALL_FF;

	// clear interrupt flags
	CLR_BSP_INTR(0, 0xFF);

	if (temp != 0)
	{
		return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
	}
	else
	{
		return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
	}
}

// BSP interrupt service routine - incomplete
void ftl_isr(void)
{
	UINT32 bank;
	UINT32 bsp_intr_flag;

	uart_print("BSP interrupt occured...");
	// interrupt pending clear (ICU)
	SETREG(APB_INT_STS, INTR_FLASH);

	for (bank = 0; bank < NUM_BANKS; bank++) {
		while (BSP_FSM(bank) != BANK_IDLE);
		// get interrupt flag from BSP
		bsp_intr_flag = BSP_INTR(bank);

		if (bsp_intr_flag == 0) {
			continue;
		}
		UINT32 fc = GETREG(BSP_CMD(bank));
		// BSP clear
		CLR_BSP_INTR(bank, bsp_intr_flag);

		// interrupt handling
		if (bsp_intr_flag & FIRQ_DATA_CORRUPT) {
			uart_printf("BSP interrupt at bank: 0x%x", bank);
			uart_print("FIRQ_DATA_CORRUPT occured...");
		}
		if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L)) {
			uart_printf("BSP interrupt at bank: 0x%x", bank);
			if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG) {
				// program에서 생긴 bad block은 data를 다른 위치로 mapping 시켜준다
				uart_print("find runtime bad block when block program...");
				
			}
			else {
				// erase에서 생긴 bad block은 data를 보존할 필요가 없으므로 사용 못하도록 체크만 한다
				uart_printf("find runtime bad block when block erase...vblock #: %d", GETREG(BSP_ROW_H(bank)) / PAGES_PER_BLK);
				ASSERT(fc == FC_ERASE);
			}

			// bad block난 block은 bad block map에 표시

			// empty block bitmap에 using으로 표시해서 다시 사용할수 없도록 한다

		}
	}
}


// logical page가 들어간 data block의 virtual block number를 반환 - complete
static UINT32 get_dblock (UINT32 const lpn)
{
	UINT32 blk, lbpn, bank;
	
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;

	// 해당 logical page의 data block 번호를 반환 ( 0x00000000 : Invalid)
	return read_dram_32 (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);		
}


// logical page의 data block이 존재하는지 확인 - complete
static BOOL8 is_exist_dblock (UINT32 const lpn)
{
	UINT32 lbpn, bank;
	UINT32 blk;

	//uart_printf ("is exist dblock : %d", lpn);

	// DRAM에서 data block의 option 정보 read
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;

	//uart_printf ("data block option :: %x", read_dram_8 (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_OP));

	// block valid bit이 활성화 되있는지 검사
	//if (tst_bit_dram (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_EX)) {
	if (read_dram_32 (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_VADDR) != 0) {
		//uart_printf ("dblock is exist");
		return TRUE;
	}
	return FALSE;
}


// logical page가 data block이나 log block에 기록된 적이 있는지 확인 - complete
static BOOL8 is_exist_dpage (UINT32 const lpn)
{
	UINT32 blk, lbpn, bank, offset, bit_offset;

	// DRAM에서 data block의 page mapping 정보 read
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	offset = lbpn % PAGES_PER_BLK;
	bit_offset = offset % 8;

	// data block에서 valid 상태인지 검사
	if (tst_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset))
	{
		return TRUE;
	}
	else
	{
		// log block이 있는지 검사
		blk = get_log_blk (lpn);
		if (blk != 0xff) 
		{
			//log block에 page가 기록되어 있는지 검사
			if (get_logblk_page (blk, lpn) == NULL)
			{
				return FALSE;
			}
			else
			{
				return TRUE;
			}
		} 
		else
		{
			return FALSE;
		}
	}
}


// logical page가 data block에서 valid 상태인지 확인 - complete
static BOOL8 is_valid_dblock (UINT32 const lpn)
{
	UINT32 blk, offset, bitoffset;
	UINT32 bank, lbpn;

	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	offset = lbpn % PAGES_PER_BLK;
	bitoffset = offset % 8;

	// data block mapping data에서 해당 page의 page bitmap을 검사한다
	if (tst_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8),
					bitoffset))
	{
		return TRUE;
	}
	
	return FALSE;
}


// block을 할당 받지 않은 data block에 새로 block을 할당 - complete
static UINT32 assign_dblock (UINT32 const lpn)
{
	UINT32 vblk, blk, bank, lbpn, map_offset;
	UINT32 n;
	
	//uart_printf("assign new data block : %d", lpn);
	// data block용 새로운 block 할당
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	vblk = get_empty_blk(bank);

	//uart_printf("new data block number :%d", vblk);
	// data block mapping data 입력
	map_offset = blk * NUM_BANKS + bank;
	write_dram_32 (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR, vblk); // virtual block number 입력
	//write_dram_8 (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_OP, 0x80);	// option 입력
	set_bit_dram (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_OP, DATA_BLK_OP_EX);

	return vblk;
}


// logical page에 해당하는 virtual page의 주소를 읽어온다
static UINT32 get_dpage (UINT32 const lpn)
{
	UINT32 vbn;

	vbn = get_dblock (lpn);

	return ((vbn * PAGES_PER_BLK) + ((lpn / NUM_BANKS) % PAGES_PER_BLK));
}


// data block에서 logical page를 할당받는다 - complete
static UINT32 set_dpage (UINT32 const lpn)
{
	UINT32 blk, offset, vblk, bit_offset;
	UINT32 bank, lbpn;

	//uart_printf ("set_datablock_page : %d", lpn);

	// logical block number와 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	offset = lbpn % PAGES_PER_BLK;
	bit_offset = offset % 8;
	
	// virtual data block number를 읽어온다
	vblk = read_dram_32 (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);

	// data block의 valid page bitmap에 valid로 체크
	set_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

	//uart_printf ("set_dpage :: vpn %d", (vblk*PAGES_PER_BLK) + offset);

	// virtual page number 반환
	return ((vblk * PAGES_PER_BLK) + offset);
}


// data block에서 logical page를 invalid 시킨다 - complete
static BOOL8 set_invalid_dpage (UINT32 const lpn)
{
	UINT32 lbn, offset, bit_offset;
	UINT32 bank, lbpn;

	// block number와 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	lbn = lbpn / PAGES_PER_BLK;
	offset = lbpn % PAGES_PER_BLK;
	bit_offset = offset % 8;

	// 현재 상태와 상관없이 valid bit를 clear
	clr_bit_dram (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

	return TRUE;
}


// log block에서 logical page를 dirty invalid로 체크한다 - complete
static BOOL8 set_dirty_log_page (UINT32 const lpn)
{
	UINT32 lblk, i;
	UINT32 bank, lbpn, offset, map_offset;
	UINT8 d8;

	// page의 block 내 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;

	// log block 번호 검색
	lblk = get_log_blk (lpn);

	// log block의 page table에서 page가 존재하는지 검색
	map_offset = bank * NUM_LOG_BLKS + lblk;
	for (i=0; i < PAGES_PER_BLK; i++) 
	{
		// log block mapping table에서 page offset을 통해 logical page를 찾는다
		d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i);
		
		if (d8 == offset)
		{			
			// log block mapping table에서 dirty로 입력
			write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i, 0xFE);

			// valid page counter 감소
			d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
			write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG, d8-1);
			
			return TRUE;
		}
	}

	return FALSE;
}


// data block에 log block을 새로 연결한다 - complete
static UINT32 set_log_blk (UINT32 const lpn)
{
	UINT32 vblk, lbn, bank;
	UINT32 d, i, lbpn, offset;
	UINT8 d8;

	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	lbn = lbpn / PAGES_PER_BLK;

	// log block 갯수가 가득 찼으면 garbage collection을 한다
	//if (log_blk_cnt[bank] == NUM_LOG_BLKS) 
	if (get_logblk_cnt(bank) == NUM_LOG_BLKS)
	{
		garbage_collection (bank, NUM_LOG_BLKS);
	}
	vblk = get_empty_blk (bank);

	// log block mapping table에서 비어있는 자리 검색
	for (i=0; i < NUM_LOG_BLKS; i++) 
	{
		d = read_dram_32 (LOG_BLK_ADDR + (bank * NUM_LOG_BLKS + i) * LOG_BLK_SIZE + LOG_BLK_VADDR);

		// virtual block number가 0인 곳에 새 log block을 할당
		if (d == 0)
		{
			break;
		}
	}

	ASSERT (i != NUM_LOG_BLKS);

	// data block mapping table에 log block 정보 기록
	d8 = 0b11000000 + (UINT8)i;
	write_dram_8 (DATA_BLK_ADDR + (lbn * NUM_BANKS + bank) * DATA_BLK_SIZE + DATA_BLK_OP, d8);

#ifdef __TEST_LB
	uart_printf ("set_log_blk :: bank %d vbn %d logblk %d", bank, vblk, i);
#endif

	// log block mapping table에 mapping data 기록
	offset = bank * NUM_LOG_BLKS + i;
	write_dram_32 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_VADDR, vblk); // virtual block number 기록
	write_dram_32 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_LADDR, lbn); // logical block number 기록
	write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_PGCNT, 0); // page counter 0로 초기화
	write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_VLDPG, 0); // valid page counter 0으로 초기화
	write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_MERGE, 0); // merge status를 partial merge로 초기화
	for (d=0; d < PAGES_PER_BLK; d++)		// page number map을 invalid로 초기화
	{
		write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_PGMAP + d, 0xff);
	}

	// log block counter 증가
	inc_logblk_cnt (bank);
	//log_blk_cnt[bank]++;

	return vblk;	// 새로 할당 된 log block의 virtual block number를 반환
}


// data block에 연결된 log block의 번호를 받아온다 - complete
static UINT32 get_log_blk (UINT32 const lpn) 
{
	UINT32 lbn, bank, lbpn;
	UINT8 d8;

	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	lbn = lbpn / PAGES_PER_BLK;

	if (tst_bit_dram (DATA_BLK_ADDR + (DATA_BLK_SIZE * (lbn * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_LB))
	{
		// log block이 있을 경우 log block의 번호 반환
		d8 = read_dram_8 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_OP);
		d8 = d8 & DATA_BLK_OP_LBNUM;

		return (UINT32)d8;
	}
	else
	{
		// log block이 없을 경우 0xff 반환
		return 0xff;
	}
}


// logical page 번호를 받아 log block에서 해당하는 page를 찾아서 반환한다 - complete
static UINT32 get_log_page (UINT32 const lpn)
{
	UINT32 lblk;

	lblk = get_log_blk (lpn);
	if (lblk != 0xff)
	{
		return get_logblk_page (lblk, lpn);
	}
	else
	{
		//uart_printf("get_log_page :: ALERT return address is NULL");
		return NULL;
	}
}


// log block에 page를 할당한다 - complete
static UINT32 set_log_page (UINT32 const lpn)
{
	UINT32 offset, lbpn, bank, page_offset;
	UINT32 lblk, lpage, loffset, map_offset;
	UINT8 d8;
		
	// log block에서 다음에 쓸 page 위치를 받는다
	bank = get_num_bank(lpn);
	lblk = get_log_blk (lpn);	// data block에 연결된 log block의 번호를 받는다
	map_offset = bank * NUM_LOG_BLKS + lblk;
	d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT);
	loffset = (UINT32)d8;	// log block에 다음에 page를 쓸 위치

	// log block이 가득 찼으면 garbage collection을 한다
	if (loffset == PAGES_PER_BLK)
	{
		garbage_collection (bank, lblk);
		
		// data block에 page가 있을 경우 새로 log block 할당
		if (is_valid_dblock (lpn) == TRUE)
		{
			set_invalid_dpage (lpn);
			set_log_blk (lpn);
			return set_log_page(lpn);
		}
		else
		{
			return set_dpage(lpn);
		}
	}

	//log block에서 page를 invalid 시킨다
	set_dirty_log_page (lpn);

	// log block에서 page의 주소를 계산
	lpage = read_dram_32 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
	lpage = lpage * PAGES_PER_BLK + loffset;

	//uart_printf ("set_log_page :: bank %d lpn %d log page %d", bank, lpn, lpage);

	// log block mapping table에 logical page를 연결
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;
	write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + loffset, (UINT8)offset);
	
	// log block에 page를 기록할 위치를 한칸 뒤로 이동한다
	write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT, (UINT8)(loffset + 1));

	// valid page counter를 증가시킨다
	d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
	write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG, d8 + 1);

	// 이번에 쓴 page가 순서에 맞지 않을 경우 merge 상태를 full merge로 입력
	page_offset = (lpn / NUM_BANKS) % PAGES_PER_BLK;	// 이번에 쓰는 page의 block 내 offset 계산
	if (page_offset != loffset)	// 이번에 쓴 위치와 비교
	{
		//full merge로 표시
		write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_MERGE, 1);
	}

	return lpage;
}


// log block에 있는 logical page의 virtual address를 반환 - complete
static UINT32 get_logblk_page (UINT32 const lblk, UINT32 const lpn)
{
	UINT32 i, offset, blk, bank, lbpn;
	UINT8 d8, map_offset;
	
	// page의 block 내 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;

	// log block의 page table에서 page가 존재하는지 검색
	map_offset = bank * NUM_LOG_BLKS + lblk;
	for (i=0; i < PAGES_PER_BLK; i++) 
	{
		// log block mapping table에서 page offset을 통해 logical page를 찾는다
		d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i);
		
		if (d8 == offset)
		{
			// log block의 virtual block number read
			blk = read_dram_32 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);

			// page의 virtual page number 반환
			return ((blk * PAGES_PER_BLK) + i);
		}
	}

	// 존재하지 않을 경우 null 반환
	return NULL;
}


// 같은 bank에서 empty block 한개를 반환한다 - complete
static UINT32 get_empty_blk (UINT32 const bank)
{
	UINT32 blk, offset;
	UINT32 i, bytes, vbn;
		
	//uart_printf("get_empty_blk :: bank %d", bank);

	// empty blk bitmap에서 한 byte씩 검사
	offset = bank * BLKS_PER_BANK / 8;
	for (bytes = 0; bytes < (BLKS_PER_BANK / 8); bytes++)
	{
		// bank에 해당하는 bit를 검사
		for (i=0; i < 8; i++)
		{
			if (tst_bit_dram(EMPTY_BLK_ADDR + offset + bytes, 7-i))
			{
				// bit에 empty로 표시되었을 경우 vbn 반환
				vbn = bytes * 8 + i;
				// empty block bit map에 using으로 표시
				set_using_blk (bank, vbn);

				//uart_printf ("get_empty_blk :: vbn %d offset %d bit_offset %d", vbn, offset+bytes, 7-i);

				return vbn;
			}
		}
	}
	
	// 그래도 빈 block이 없으면 garbage collection 시도
	return garbage_collection(bank, NUM_LOG_BLKS);
}


// 해당 bank의 virtual block을 using 상태로 체크 - incomplete
static BOOL8 set_using_blk (UINT32 const bank, UINT32 const vbn)
{
	UINT32 offset, bit_offset;

	offset = (bank * BLKS_PER_BANK / 8) + (vbn / 8);
	bit_offset = 7-(vbn % 8);
	clr_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

	//uart_printf ("set_using_blk :: bank %d vbn %d offset %d, bit_offset %d", bank, vbn, offset, bit_offset);

	return TRUE;
}


static BOOL8 set_empty_blk (UINT32 const bank, UINT32 const vbn)
{
	UINT32 offset, bit_offset;

	offset = (bank * BLKS_PER_BANK / 8) + (vbn / 8);
	bit_offset = 7-(vbn % 8);
	set_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

	return TRUE;
}


// garbage collection - complete
static UINT32 garbage_collection (UINT32 const bank, UINT32 const logblk)
{
	UINT32 vblk, lbn, vbn, new_dbn, offset, bit_offset;
	UINT32 i32, is_fmerge, j32;
	UINT8 d8;

	// garbage collection을 할 victim block 선정
	if (logblk == NUM_LOG_BLKS) 
	{
		vblk = get_victim_block (bank);
	} 
	else
	{
		vblk = logblk;
	}
	lbn = read_dram_32 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + vblk) * LOG_BLK_SIZE) + LOG_BLK_LADDR);
	vbn = read_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);

#ifdef __TEST_GC
	uart_printf ("garbage_collection :: bank %d victim blk %d", bank, vblk);
#endif

	// log block에 page가 순서대로 위치하는지 검사
	is_fmerge = 0;
	for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
	{
		d8 = read_dram_8 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + vblk) * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i32);

		// log block에 기록된 적 없는 영역인 경우
		if (d8 == 0xff)
		{
			// 순서대로 기록되어 있고 빈 공간이 있으므로 partial merge
			break;
		}
		// log block에 page가 순서대로 들어있지 않은 상태
		else if (d8 != (UINT8)i32)
		{
			//uart_printf("lpn %d, logpn %d", i32, d8);
			is_fmerge = 1;
			//break;
		}
	}

	// partial merge
	if (is_fmerge == 0)
	{
		new_dbn = partial_merge(bank, vblk, lbn);
	}
	// full merge
	else
	{
		new_dbn = full_merge(bank, vblk, lbn);
	}

	// data block mapping table 갱신
	write_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR, new_dbn);
	write_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_OP, 0x80);

	// 이전에 사용한 data block을 empty block mapping table에 표시
	set_empty_blk (bank, vbn);

	// log block mapping table 초기화
	offset = bank * NUM_LOG_BLKS + vblk;
	write_dram_32 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_VADDR, 0x00000000);
	write_dram_32 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_LADDR, 0x00000000);
	write_dram_8 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT, 0x00);
	for (j32 = 0; j32 < PAGES_PER_BLK; j32++) 
	{
		write_dram_8 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + j32, 0xff);
	}

	// log block counter 감소
	dec_logblk_cnt  (bank);
	//log_blk_cnt[bank]--;

	// merge이전에 사용하던 data block이 empty 상태이므로 data block을 반환
	return vbn;
}


// partial merge - complete
static UINT32 partial_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
	UINT32 vbn, offset, bit_offset, lvbn;
	UINT32 dmap_offset, lmap_offset;
	UINT32 i32;
	UINT8 d8;

#ifdef __TEST_GC
	uart_printf ("partial_merge :: bank %d, vblk %d, lbn %d", bank, vblk, lbn);
#endif

	// garbage collection을 할 block의 virtual block number read
	dmap_offset = lbn * NUM_BANKS + bank;
	lmap_offset = bank * NUM_LOG_BLKS + vblk;
	vbn = read_dram_32 (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR);
	lvbn = read_dram_32 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
	
	for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
	{
		// log block에 있는 page 번호 검사
		d8 = read_dram_8 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i32);

		// log block에 정위치에 있는 경우 data block의 page bitmap에 표시
		if (d8 == (UINT8)i32)
		{
			set_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (i32 / 8), (i32 % 8));
		}
		// log block에 없는 page인 경우 data block에서 이동
		else if (d8 == 0xff)
		{
			// offset 계산
			offset = i32 / 8;
			bit_offset = i32 % 8;

			// data block에 page가 있는지 확인
			if (tst_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
			{
				// data block에 있는 내용을 log block으로 copy back
				nand_page_copyback (bank, vbn, i32, lvbn, i32);
			}
		}
	}
	// 정리가 끝난 data block은 erase
	nand_block_erase (bank, vbn);

#ifdef __TEST_GC
	uart_printf ("partial_merge :: end", bank, vblk, lbn);
#endif

	return lvbn;
}


// full merge - complete
static UINT32 full_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
	UINT32 i32, j32, offset, bit_offset;
	UINT32 nblk, vbn, lvbn, dmap_offset, lmap_offset;
	UINT8 d8;

	// 새로 만들 block 할당
	nblk = get_gc_vblock (bank);

	dmap_offset = lbn * NUM_BANKS + bank;
	lmap_offset = bank * NUM_LOG_BLKS + vblk;

#ifdef __TEST_GC
	uart_printf ("full_merge :: bank %d, log blk %d, lbn %d, newblk %d", bank, vblk, lbn, nblk);
#endif

	// 0번 page부터 순서대로 이동
	vbn = read_dram_32 (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR);
	lvbn = read_dram_32 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
	for (i32 = 0; i32 < PAGES_PER_BLK; i32++)
	{
		//data block에 있는 경우 data block에서 복사
		offset = i32 / 8;
		bit_offset = i32 % 8;

		// data block에 있는 page인 경우
		if (tst_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
		{
			// data block에 있는 내용을 새 block으로 copy back
#ifdef __TEST_GC
			uart_printf ("full_merge :: %d from data blk to new blk", i32);
#endif
			nand_page_copyback (bank, vbn, i32, nblk, i32);
		}
		// log block에 있는지 검사
		else
		{
			// log block mapping table 전체 검사
			for (j32 = 0; j32 < PAGES_PER_BLK; j32++)
			{
				d8 = read_dram_8 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + j32);

				// log block에 찾는 번호가 있을 경우
				if (d8 == i32)
				{
#ifdef __TEST_GC
					uart_printf ("full_merge :: page %3d is moved from log block %2d's %3d page to new block %2d", d8, lvbn, j32, nblk);
#endif
					nand_page_copyback (bank, lvbn, j32, nblk, i32);
					// data block bitmap에 표시
					set_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset);
					break;
				}	
			}
		}
	}

	// 정리가 끝난 data block과 log block을 erase
	nand_block_erase (bank, vbn);
	nand_block_erase (bank, lvbn);
	
	// 이전에 사용한 log block은 다음 full merge에 사용할 new block으로 설정
	set_gc_vblock (bank, lvbn);

	// 새로 완성된 block의 주소 반환
	return nblk;
}


// victim block을 선정한다 - sample
static UINT32 get_victim_block (UINT32 const bank)
{
	UINT32 i32, map_offset, sel_blk, sel_max;
	UINT8 d8;

	map_offset = NUM_LOG_BLKS * bank;

	// partial merge가 되는 block 중 valid page가 가장 많은 block 선택	
	sel_blk = -1;
	sel_max = 0;
	for (i32 = 0; i32 < NUM_LOG_BLKS; i32++)
	{
		// partial merge가 가능한 block
		if (read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32)* LOG_BLK_SIZE) + LOG_BLK_MERGE) == 0)
		{
			// 현재 최댓값과 비교
			d8 = read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32) * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
			if (d8 >= sel_max)
			{
				sel_blk = i32;
				sel_max = (UINT32)d8;
			}
		}
	}
	// partial merge가 가능한 block이 있으면 우선적으로 처리
	if (sel_blk != -1)
	{
		return sel_blk;
	}

	// full merge가 되는 block 중에 valid page가 가장 적은 block을 선택
	sel_max = PAGES_PER_BLK;
	for (i32 = 0; i32 < NUM_LOG_BLKS; i32++)
	{
		d8 = read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32) * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
		if (d8 < PAGES_PER_BLK)
		{
			sel_blk = i32;
			sel_max = d8;
		}
	}
	return sel_blk;
}

//sram 내용을 nand flash에 logging한다
static void logging_misc_block (void)
{
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 bank;

    flash_finish();

	// block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        nand_block_erase(bank, get_miscblk_vbn(bank));        
    }

    // srma에 있는 misc metadata를 nand로 저장
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
		mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

        nand_page_ptprogram(bank,
                            get_miscblk_vbn(bank),
                            0,
                            0,
                            NUM_MISC_META_SECT,
                            FTL_BUF(bank));
    }

    flash_finish();
}

static void logging_data_block (void)
{
	UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

	remain_bytes = DATA_BLK_BYTES / NUM_BANKS;

	// block에 쓰인 이전 log를 삭제
	for (bank = 0; bank < NUM_BANKS; bank++) 
	{
		for (i32 = 0; i32 < MAPBLKS_PER_BANK; i32++)
		{
			nand_block_erase (bank, get_mapblk_vbn (bank, i32));
		}
	}

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = DATA_BLK_ADDR;
	while (remain_bytes > 0) 
	{
		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_mapblk_vbn (bank, nblk);
			
			// data block mapping table을 ftl buffer로 이동
			mem_copy (FTL_BUF(bank), dblk_addr + bank * DATA_BLK_SIZE * BLKS_PER_BANK, write_bytes);

			// ftl buffer에서 nand flash로 이동
			nand_page_ptprogram (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank));
		}
		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

	flash_finish();
}

static void logging_log_block (void)
{
	UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

	// block에 쓰인 이전 log를 삭제
	for (bank = 0; bank < NUM_BANKS; bank++) 
	{
		for (i32 = 0; i32 < LOGBLKMAP_PER_BANK; i32++)
		{
			nand_block_erase (bank, get_logblkmap_vbn (bank, i32));
		}
	}

	remain_bytes = LOG_BLK_BYTES / NUM_BANKS;

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = LOG_BLK_ADDR;
	while (remain_bytes > 0) 
	{
		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_logblkmap_vbn (bank, nblk);
			
			// data block mapping table을 ftl buffer로 이동
			mem_copy (FTL_BUF(bank), dblk_addr + bank * LOG_BLK_SIZE * NUM_LOG_BLKS, write_bytes);

			// ftl buffer에서 nand flash로 이동
			nand_page_ptprogram (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank));
		}
		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

	flash_finish();
}

static void logging_empty_block (void)
{
	UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

	// block에 쓰인 이전 log를 삭제
	for (bank = 0; bank < NUM_BANKS; bank++) 
	{
		for (i32 = 0; i32 < EMPTYBLK_PER_BANK; i32++)
		{
			nand_block_erase (bank, get_emptyblk_vbn (bank, i32));
		}
	}

	remain_bytes = EMPTY_BLK_BYTES / NUM_BANKS;

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = EMPTY_BLK_ADDR;
	while (remain_bytes > 0) 
	{
		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_emptyblk_vbn (bank, nblk);
			
			// data block mapping table을 ftl buffer로 이동
			mem_copy (FTL_BUF(bank), dblk_addr + bank * (EMPTY_BLK_BYTES / NUM_BANKS), write_bytes);

			// ftl buffer에서 nand flash로 이동
			nand_page_ptprogram (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank));
		}
		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

	flash_finish();
}

//nand flash에서 sram의 전역변수 값들을 load한다
static void load_misc_block (void)
{
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;

    UINT32 load_flag = 0;
    UINT32 bank, blk;
    UINT32 load_cnt = 0;

#ifdef __TEST_PWRECV
	uart_printf ("load_misc_block :: start");
#endif 

	// misc metadata를 nand에서 읽어온다
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
		// 0번 block 이후에 가장 처음 나오는 valid block에 misc metadata가 저장
		blk = MISCBLK_VBN;
		while (is_bad_block (bank, blk) == TRUE)
		{
			blk++;
		}
		
#ifdef __TEST_PWRECV
		uart_printf ("load_misc_block :: bank %d load misc block from %d", bank, blk);
#endif 

        // misc. metadata read
		nand_page_ptread(bank,
                         MISCBLK_VBN,
                         0,
                         0,
                         NUM_MISC_META_SECT,
                         FTL_BUF(bank),
                         RETURN_ON_ISSUE);        
    }

	flash_finish();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));
	}

#ifdef __TEST_PWRECV
	uart_printf ("load_misc_block :: start");
#endif 
}

static void load_data_block (void)
{
	UINT32 bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

#ifdef __TEST_PWRECV
	uart_printf ("load_data_block :: start");
#endif 

	remain_bytes = DATA_BLK_BYTES / NUM_BANKS;

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = DATA_BLK_ADDR;
	while (remain_bytes > 0) 
	{
#ifdef __TEST_PWRECV
		uart_printf ("load_data_block :: remain bytes %d", remain_bytes);
#endif 

		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_mapblk_vbn (bank, nblk);
			
#ifdef __TEST_PWRECV
			uart_printf ("load_data_block :: bank %d load data block from %d %d th page", bank, vbn, vpn);
#endif 

			// nand flash에서 ftl buffer로 logging data 이동
			nand_page_ptread (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank), RETURN_ON_ISSUE);
		}

		flash_finish();

		for (bank = 0; bank < NUM_BANKS; bank++)
		{			
			// ftl buffer에서 data block mapping table로 logging data 이동
			mem_copy (dblk_addr + bank * DATA_BLK_SIZE * BLKS_PER_BANK, FTL_BUF(bank), write_bytes);
		}

		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

#ifdef __TEST_PWRECV
	uart_printf ("load_data_block :: end");
#endif 

}

static void load_log_block (void)
{
	UINT32 bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

#ifdef __TEST_PWRECV
	uart_printf ("load_log_block :: start");
#endif 

	remain_bytes = LOG_BLK_BYTES / NUM_BANKS;

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = LOG_BLK_ADDR;
	while (remain_bytes > 0) 
	{
#ifdef __TEST_PWRECV
		uart_printf ("load_log_block :: remain bytes %d", remain_bytes);
#endif 

		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_logblkmap_vbn (bank, nblk);
			
#ifdef __TEST_PWRECV
			uart_printf ("load_log_block :: bank %d load log block from %d %d th page", bank, vbn, vpn);
#endif 

			// nand flash에서 ftl buffer로 logging data 이동
			nand_page_ptread (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank), RETURN_ON_ISSUE);
		}
		flash_finish();
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			// ftl buffer에서 data block mapping table로 logging data 이동
			mem_copy (dblk_addr + bank * LOG_BLK_SIZE * NUM_LOG_BLKS, FTL_BUF(bank), write_bytes);
		
		}

		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

#ifdef __TEST_PWRECV
	uart_printf ("load_log_block :: end");
#endif 
}

static void load_empty_block (void)
{
	UINT32 bank, nblk, vbn, vpn, dblk_addr;
	UINT32 remain_bytes, write_bytes;

#ifdef __TEST_PWRECV
	uart_printf ("load_empty_block :: start");
#endif 

	remain_bytes = EMPTY_BLK_BYTES / NUM_BANKS;

	// 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
	nblk = 0;
	vpn = 0;
	dblk_addr = EMPTY_BLK_ADDR;
	while (remain_bytes > 0) 
	{
#ifdef __TEST_PWRECV
		uart_printf ("load_empty_block :: remain bytes %d", remain_bytes);
#endif 

		//한번에 한 page씩 nand로 back up
		if (remain_bytes > BYTES_PER_PAGE)
		{
			write_bytes = BYTES_PER_PAGE;
		}
		else
		{
			write_bytes = remain_bytes;
		}

		// 각 bank에 backup
		for (bank = 0; bank < NUM_BANKS; bank++) 
		{
			vbn = get_emptyblk_vbn (bank, nblk);
			
#ifdef __TEST_PWRECV
			uart_printf ("load_empty_block :: bank %d load empty block from %d %d th page", bank, vbn, vpn);
#endif 

			// nand flash에서 ftl buffer로 logging data 이동
			nand_page_ptread (bank, vbn, vpn, 0, write_bytes / BYTES_PER_SECTOR, FTL_BUF(bank), RETURN_ON_ISSUE);
		}
		flash_finish();
		for (bank = 0; bank < NUM_BANKS; bank++)
		{			
			// ftl buffer에서 data block mapping table로 logging data 이동
			mem_copy (dblk_addr + bank * (EMPTY_BLK_BYTES / NUM_BANKS), FTL_BUF(bank), write_bytes);
		}

		remain_bytes -= write_bytes;	
		dblk_addr += write_bytes;

		// backup block과 page counter 증가
		vpn++;
		if (vpn == PAGES_PER_BLK)
		{
			nblk++;
			vpn = 0;
		}
	}

#ifdef __TEST_PWRECV
	uart_printf ("load_empty_block :: end");
#endif 
}



