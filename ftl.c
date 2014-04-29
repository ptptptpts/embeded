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

//----------------------------------
// macro
//----------------------------------
#define BYTES_PER_BLK		(BYTES_PER_PAGE * PAGES_PER_BLK)
#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK    ((DATA_BLK_MAP_BYTES / NUM_BANKS) / BYTES_PER_BLK)
#define LOGBLKMAP_PER_BANK	(LOG_BLK_MAP_BYTES / BYTES_PER_BLK)
#define EMPTYBLK_PER_BANK	((EMPTY_BLK_BMP_BYTES / NUM_BANKS) / BYTES_PER_BLK)
#define META_BLKS_PER_BANK  (1 + 1 + MAPBLKS_PER_BANK + LOGBLKMAP_PER_BANK + EMPTYBLK_PER_BANK) // include block #0, misc block

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT     ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)

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
UINT32 recent_empty_blk;
UINT32 log_blk_cnt;
UINT32 full_merge_nblk;
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
static void   load_pmap_table(void);
static void   load_misc_metadata(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   logging_pmap_table(void);
static void   logging_misc_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
//static void   set_vpn(UINT32 const lpn, UINT32 const vpn);
//static void   garbage_collection(UINT32 const bank);
//static void   set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static BOOL32 check_format_mark(void);
//static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock);
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

static UINT32 garbage_collection (UINT32 const bank);
static UINT32 full_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn);
static UINT32 partial_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn);
static UINT32 get_victim_block (UINT32 const bank);
//=================================================================

static void sanity_check(void)
{
	UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
		+ HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + EMPTY_BLK_BMP_BYTES + VCOUNT_BYTES
		+ DATA_BLK_MAP_BYTES + LOG_BLK_MAP_BYTES;

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
//	if (check_format_mark() == FALSE) 
	if (TRUE)
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
	/*
	=======================================
	Need to implementation
	=======================================
	data block mapping table backup
	log block mapping table backup
	=======================================
	*/

	/* ptimer_start(); */
	logging_pmap_table();
	logging_misc_metadata();
	/* ptimer_stop_and_uart_print(); */
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
	
	new_vpn  = assign_new_write_vpn(lpn);	// 새로 write를 할 page 번호를 받는다
	old_vpn  = get_vpn(lpn);	// 이전에  write가 된 page 번호를 받는다

	CHECK_VPAGE (old_vpn);
	CHECK_VPAGE (new_vpn);
	ASSERT(old_vpn != new_vpn);

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
		// invalid old page (decrease vcount),  valid count를 감소시킨다
		//set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
	}
	

	// 새로 사용할 page의 block 번호와 page offset을 구한다
	vblock   = new_vpn / PAGES_PER_BLK;
	page_num = new_vpn % PAGES_PER_BLK;

	// write new data (make sure that the new data is ready in the write buffer frame)
	// (c.f FO_B_SATA_W flag in flash.h)
	// nand flash memory에 데이터를 program 한다
	nand_page_ptprogram_from_host(bank,
								  vblock,
								  page_num,
								  page_offset,
								  column_cnt);

	// log block이 가득 찼을 경우 garbage collection
	/*
	if (log_blk_cnt == NUM_LOG_BLKS)
	{
		garbage_collection(NUM_BANKS);
	}
	*/		
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

	if (is_exist_dblock (lpn) == FALSE)
		return NULL;

	// data block에서 valid인 경우
	if (is_valid_dblock(lpn) == TRUE)
	{
		// data block에서 virtual page number 계산해서 반환
		vbn = get_dblock (lpn);

		return ((vbn * PAGES_PER_BLK) + ((lpn / NUM_BANKS) % PAGES_PER_BLK));
	}
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
	//data block이 있는 page인지 검사
	if (is_exist_dblock (lpn) == FALSE) 
	{
		//data block이 없을 경우 새로 빈 block을 할당하고 page 주소 반환		
		assign_dblock (lpn);	// data block mapping
		
		return set_dpage(lpn);
	}
	else 
	{
		// data block이 있을 경우 처음 기록되는 page인지 검사
		if (is_exist_dpage (lpn) == FALSE)
		{
			// 처음 기록되는 page인 경우 data block에 할당 
			return set_dpage(lpn); 
		}
		else
		{
			// 기록된 적이 있을 경우 data block에 page가 valid 상태인지 검사
			if (is_valid_dblock (lpn) == TRUE) 
			{
				// data block에 있는 page를 invalid 시킨다
				set_invalid_dpage (lpn);
				// log block이 연결되었는지 검사
				if (get_log_blk (lpn) == 0xffffffff)
				{
					// log block이 없을 경우 새로 연결
					set_log_blk (lpn);
				}
			}
			else
			{
				//log block에서 page를 invalid 시킨다
				set_dirty_log_page (lpn);
			}
			
			// log block에서 page 반환
			return set_log_page (lpn);
		}			
	}
}

static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
	if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
	{
		return FALSE;
	}
	return TRUE;
}

// 수정 필요
static void format(void)
{
	UINT32 i32, j32;
	UINT32 bank, vblock, vcount_val;

	ASSERT(NUM_MISC_META_SECT > 0);
	ASSERT(NUM_VCOUNT_SECT > 0);

	uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

	uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
	uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
	uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);

	//----------------------------------------
	// initialize DRAM metadata
	//----------------------------------------

	// data block과 log block mapping 정보가 저장되는 공간 초기화
	mem_set_dram (DATA_BLK_MAP_ADDR, NULL, DATA_BLK_MAP_BYTES);
	mem_set_dram (LOG_BLK_MAP_ADDR, NULL, LOG_BLK_MAP_BYTES);
	for (j32 = 0; j32 < NUM_LOG_BLKS; j32++)
	{
		for (i32 = 0; i32 < NUM_VBLKS; i32 += 4) 
		{
			write_dram_32 (LOG_BLK_MAP_ADDR + (j32 * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + i32, 0xffffffff);
		}
	}
	mem_set_dram(VCOUNT_ADDR, NULL, VCOUNT_BYTES);
	mem_set_dram(EMPTY_BLK_BMP_ADDR, 0xffffffff, EMPTY_BLK_BMP_BYTES);

	//----------------------------------------
	// erase all blocks except vblock #0
	//----------------------------------------
	for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			vcount_val = VC_MAX;
			if (is_bad_block(bank, vblock) == FALSE)
			{
				nand_block_erase(bank, vblock);
				vcount_val = 0;
			}
			write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16),
						  vcount_val);
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
	recent_empty_blk = 0;
	log_blk_cnt = 0;
	full_merge_nblk = NUM_VBLKS - 1;
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

	//----------------------------------------
	// initialize misc. metadata
	//----------------------------------------
	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		//g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
		//g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
		// NOTE: vblock #0,1 don't use for user space
		//write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
		//write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);

		//----------------------------------------
		// assign misc. block
		//----------------------------------------
		// assumption: vblock #1 = fixed location.
		// Thus if vblock #1 is a bad block, it should be allocate another block.
		vblock = MISCBLK_VBN;
		while (is_bad_block (bank, vblock) == TRUE)
		{
			set_using_blk (bank, vblock);
		}
		set_miscblk_vbn(bank, vblock);
		
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
				//write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
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
			//write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
			// set free block
			set_gc_vblock(bank, vblock);
			set_using_blk (bank, vblock);

			ASSERT(vblock < VBLKS_PER_BANK);
		}while(is_bad_block(bank, vblock) == TRUE);
	}
}

// logging misc + vcount metadata
static void logging_misc_metadata(void)
{
	/*
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
	UINT32 vcount_addr     = VCOUNT_ADDR;
	UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR; // per bank
	UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES; // entire vcount data
	UINT32 bank;

	flash_finish();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		inc_miscblk_vpn(bank);

		// note: if misc. meta block is full, just erase old block & write offset #0
		if ((get_miscblk_vpn(bank) / PAGES_PER_BLK) != MISCBLK_VBN)
		{
			nand_block_erase(bank, MISCBLK_VBN);
			set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK); // vpn = 128
		}
		// copy misc. metadata to FTL buffer
		mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

		// copy vcount metadata to FTL buffer
		if (vcount_addr <= vcount_boundary)
		{
			mem_copy(FTL_BUF(bank) + misc_meta_bytes, vcount_addr, vcount_bytes);
			vcount_addr += vcount_bytes;
		}
	}
	// logging the misc. metadata to nand flash
	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		nand_page_ptprogram(bank,
							get_miscblk_vpn(bank) / PAGES_PER_BLK,
							get_miscblk_vpn(bank) % PAGES_PER_BLK,
							0,
							NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
							FTL_BUF(bank));
	}
	flash_finish();
	*/
}

// dram에 있는 mapping data를 nand flash로 backup
static void logging_pmap_table(void)
{	
	/*
	UINT32 pmap_addr  = PAGE_MAP_ADDR;
	UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
	UINT32 mapblk_vpn;
	UINT32 bank;
	UINT32 pmap_boundary = PAGE_MAP_ADDR + PAGE_MAP_BYTES;
	BOOL32 finished = FALSE;

	for (UINT32 mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
	{
		flash_finish();

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (finished)
			{
				break;
			}
			else if (pmap_addr >= pmap_boundary)
			{
				finished = TRUE;
				break;
			}
			else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				finished = TRUE;
				pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR ;
			}
			inc_mapblk_vpn(bank, mapblk_lbn);

			mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);

			// note: if there is no free page, then erase old map block first.
			if ((mapblk_vpn % PAGES_PER_BLK) == 0)
			{
				// erase full map block
				nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

				// next vpn of mapblk is offset #0
				set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);
				mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
			}
			// copy the page mapping table to FTL buffer
			mem_copy(FTL_BUF(bank), pmap_addr, pmap_bytes);

			// logging update page mapping table into map_block
			nand_page_ptprogram(bank,
								mapblk_vpn / PAGES_PER_BLK,
								mapblk_vpn % PAGES_PER_BLK,
								0,
								pmap_bytes / BYTES_PER_SECTOR,
								FTL_BUF(bank));
			pmap_addr += pmap_bytes;
		}
		if (finished)
		{
			break;
		}
	}
	*/
	flash_finish();
}

// load flushed FTL metadta
static void load_metadata(void)
{
	load_misc_metadata();
	load_pmap_table();
}

// misc + VCOUNT
static void load_misc_metadata(void)
{
	/*
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;
	UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR;
	UINT32 vcount_addr     = VCOUNT_ADDR;
	UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;

	UINT32 load_flag = 0;
	UINT32 bank, page_num;
	UINT32 load_cnt = 0;

	flash_finish();

	disable_irq();
	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	// scan valid metadata in descending order from last page offset
	for (page_num = PAGES_PER_BLK - 1; page_num != ((UINT32) -1); page_num--)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (load_flag & (0x1 << bank))
			{
				continue;
			}
			// read valid metadata from misc. metadata area
			nand_page_ptread(bank,
							 MISCBLK_VBN,
							 page_num,
							 0,
							 NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
							 FTL_BUF(bank),
							 RETURN_ON_ISSUE);
		}
		flash_finish();

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (!(load_flag & (0x1 << bank)) && !(BSP_INTR(bank) & FIRQ_ALL_FF))
			{
				load_flag = load_flag | (0x1 << bank);
				load_cnt++;
			}
			CLR_BSP_INTR(bank, 0xFF);
		}
	}
	ASSERT(load_cnt == NUM_BANKS);

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		// misc. metadata
		mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));

		// vcount metadata
		if (vcount_addr <= vcount_boundary)
		{
			mem_copy(vcount_addr, FTL_BUF(bank) + misc_meta_bytes, vcount_bytes);
			vcount_addr += vcount_bytes;

		}
	}
	enable_irq();
	*/
}

static void load_pmap_table(void)
{
	/*
	UINT32 pmap_addr = PAGE_MAP_ADDR;
	UINT32 temp_page_addr;
	UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
	UINT32 pmap_boundary = PAGE_MAP_ADDR + (NUM_LPAGES * sizeof(UINT32));
	UINT32 mapblk_lbn, bank;
	BOOL32 finished = FALSE;

	flash_finish();

	for (mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
	{
		temp_page_addr = pmap_addr; // backup page mapping addr

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (finished)
			{
				break;
			}
			else if (pmap_addr >= pmap_boundary)
			{
				finished = TRUE;
				break;
			}
			else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				finished = TRUE;
				pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
			}
			// read page mapping table from map_block
			nand_page_ptread(bank,
							 get_mapblk_vpn(bank, mapblk_lbn) / PAGES_PER_BLK,
							 get_mapblk_vpn(bank, mapblk_lbn) % PAGES_PER_BLK,
							 0,
							 pmap_bytes / BYTES_PER_SECTOR,
							 FTL_BUF(bank),
							 RETURN_ON_ISSUE);
			pmap_addr += pmap_bytes;
		}
		flash_finish();

		pmap_bytes = BYTES_PER_PAGE;
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (temp_page_addr >= pmap_boundary)
			{
				break;
			}
			else if (temp_page_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				pmap_bytes = (pmap_boundary - temp_page_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
			}
			// copy page mapping table to PMAP_ADDR from FTL buffer
			mem_copy(temp_page_addr, FTL_BUF(bank), pmap_bytes);

			temp_page_addr += pmap_bytes;
		}
		if (finished)
		{
			break;
		}
	}
	*/
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

static BOOL32 check_format_mark(void)
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
				uart_print("find runtime bad block when block program...");
			}
			else {
				uart_printf("find runtime bad block when block erase...vblock #: %d", GETREG(BSP_ROW_H(bank)) / PAGES_PER_BLK);
				ASSERT(fc == FC_ERASE);
			}
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
	return read_dram_32 (DATA_BLK_MAP_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR);		
}


// logical page의 data block이 존재하는지 확인 - complete
static BOOL8 is_exist_dblock (UINT32 const lpn)
{
	UINT32 lbpn, bank;
	UINT32 blk;

	// DRAM에서 data block의 option 정보 read
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;

	// block valid bit이 활성화 되있는지 검사
	if (tst_bit_dram (DATA_BLK_MAP_ADDR + (DATA_BLK_MAP_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_EX)) {
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
	if (tst_bit_dram (DATA_BLK_MAP_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset))
	{
		return TRUE;
	}
	else
	{
		// log block이 있는지 검사
		blk = get_log_blk (lpn);
		if (blk != NULL) 
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
	if (tst_bit_dram (DATA_BLK_MAP_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE)
		+ DATA_BLK_VPBMP + (offset / 8), bitoffset))
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
	
	// data block용 새로운 block 할당
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	vblk = get_empty_blk(bank);

	// data block mapping data 입력
	map_offset = blk * NUM_BANKS + bank;
	write_dram_32 (DATA_BLK_MAP_ADDR + (map_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR, vblk); // virtual block number 입력
	for (n=0; n < 16; n += 4) {			// valid page bitmap을 0으로 초기화
		write_dram_32 (DATA_BLK_MAP_ADDR + (map_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + n, 0x00000000);
	}
	write_dram_8 (DATA_BLK_MAP_ADDR + (map_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_OP, 0x80);	// option 입력

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

	// logical block number와 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	blk = lbpn / PAGES_PER_BLK;
	offset = lbpn % PAGES_PER_BLK;
	bit_offset = offset % 8;
	
	// virtual data block number를 읽어온다
	vblk = read_dram_32 (DATA_BLK_MAP_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR);

	// data block의 valid page bitmap에 valid로 체크
	set_bit_dram (DATA_BLK_MAP_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

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
	clr_bit_dram (DATA_BLK_MAP_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

	return TRUE;
}


// log block에서 logical page를 dirty invalid로 체크한다 - complete
static BOOL8 set_dirty_log_page (UINT32 const lpn)
{
	UINT32 lblk, i;
	UINT32 bank, lbpn, offset;
	UINT8 d8;

	// page의 block 내 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;

	// log block에서 page 위치 검색
	lblk = get_log_blk (lpn);

	// log block의 page table에서 page가 존재하는지 검색
	for (i=0; i < PAGES_PER_BLK; i++) 
	{
		// log block mapping table에서 page offset을 통해 logical page를 찾는다
		d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (LOG_BLK_MAP_SIZE * lblk) + LOG_BLK_PGMAP + i);
		
		if (d8 == offset)
		{
			// log block mapping table에서 dirty로 입력
			write_dram_8 (LOG_BLK_MAP_ADDR + (lblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + i, 0xFE);

			break;
		}
	}

	return TRUE;
}


// data block에 log block을 새로 연결한다 - complete
static UINT32 set_log_blk (UINT32 const lpn)
{
	UINT32 vblk, lbn, bank;
	UINT32 d, i, lbpn;
	UINT8 d8;

	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	lbn = lbpn / PAGES_PER_BLK;

	// log block 갯수가 가득 찼으면 garbage collection을 한다
	if (log_blk_cnt == NUM_LOG_BLKS) 
	{
		vblk = garbage_collection (NUM_BANKS);
	}
	else
	// log block을 더 쓸수 있을 경우 빈 block 하나를 받아온다
	{
		vblk = get_empty_blk (bank);
	}

	// log block mapping table에서 비어있는 자리 검색
	for (i=0; i < NUM_LOG_BLKS; i++) 
	{
		d = read_dram_32 (LOG_BLK_MAP_ADDR + i * LOG_BLK_MAP_SIZE);

		// virtual block number가 0인 곳에 새 log block을 할당
		if (d == 0)
		{
			break;
		}
	}

	ASSERT (i != NUM_LOG_BLKS);

	// data block mapping table에 log block 정보 기록
	d8 = 0b11000000 + (UINT8)i;
	write_dram_8 (DATA_BLK_MAP_ADDR + (lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE + DATA_BLK_OP, d);

	// log block mapping table에 mapping data 기록
	write_dram_32 (LOG_BLK_MAP_ADDR + i * LOG_BLK_MAP_SIZE + LOG_BLK_VADDR, vblk); // virtual block number 기록
	write_dram_32 (LOG_BLK_MAP_ADDR + i * LOG_BLK_MAP_SIZE + LOG_BLK_LADDR, lbn); // logical block number 기록
	write_dram_8 (LOG_BLK_MAP_ADDR + (i * LOG_BLK_MAP_SIZE) + LOG_BLK_BANK, (UINT8) bank); // bank number 기록
	write_dram_8 (LOG_BLK_MAP_ADDR + i * LOG_BLK_MAP_SIZE + LOG_BLK_PGCNT, 0); // page counter 0로 초기화
	for (d=0; d < PAGES_PER_BLK; d += 4)		// page number map을 invalid로 초기화
	{
		write_dram_32 (LOG_BLK_MAP_ADDR + i * LOG_BLK_MAP_SIZE + LOG_BLK_PGMAP + d, 0xffffffff);
	}

	// log block counter 증가
	log_blk_cnt++;

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

	if (tst_bit_dram (DATA_BLK_MAP_ADDR + (DATA_BLK_MAP_SIZE * (lbn * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_EX))
	{
		// log block이 있을 경우 log block의 번호 반환
		d8 = read_dram_8 (DATA_BLK_MAP_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_OP);
		d8 = d8 & DATA_BLK_OP_LBNUM;

		return (UINT32)d8;
	}
	else
	{
		// log block이 없을 경우 -1 반환
		return 0xffffffff;
	}
}


// logical page 번호를 받아 log block에서 해당하는 page를 찾아서 반환한다 - complete
static UINT32 get_log_page (UINT32 const lpn)
{
	UINT32 lblk;

	lblk = get_log_blk (lpn);
	if (lblk != NULL)
	{
		return get_logblk_page (lblk, lpn);
	}
	else
	{
		return NULL;
	}
}


// log block에 page를 할당한다 - complete
static UINT32 set_log_page (UINT32 const lpn)
{
	UINT32 offset, lbpn, bank;
	UINT32 lblk, lpage, loffset;
	UINT8 d8;
		
	// log block에서 다음에 쓸 page 위치를 받는다
	lblk = get_log_blk (lpn);	// data block에 연결된 log block의 번호를 받는다
	d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (lblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGCNT);
	loffset = (UINT32)d8;	// log block에 다음에 page를 쓸 위치

	// 이전에 log block에 기록된 page를 invalid로 설정
	set_dirty_log_page (lpn);

	// log block에서 page의 주소를 계산
	lpage = read_dram_32 (LOG_BLK_MAP_ADDR + (lblk * LOG_BLK_MAP_SIZE) + LOG_BLK_VADDR);
	lpage = lpage * PAGES_PER_BLK + loffset;

	// log block mapping table에 logical page를 연결
	bank = get_num_bank(lpn);
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;
	write_dram_8 (LOG_BLK_MAP_ADDR + (lblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + loffset, (UINT8)offset);

	// log block에 page를 기록할 위치를 한칸 뒤로 이동한다
	write_dram_8 (LOG_BLK_MAP_ADDR + (lblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGCNT, (UINT8)(loffset + 1));

	return lpage;
}


// log block에 있는 logical page의 virtual address를 반환 - complete
static UINT32 get_logblk_page (UINT32 const lblk, UINT32 const lpn)
{
	UINT32 i, offset, blk, bank, lbpn;
	UINT8 d8;
	
	// page의 block 내 offset 계산
	bank = get_num_bank (lpn);
	lbpn = lpn / NUM_BANKS;
	offset = lbpn % PAGES_PER_BLK;

	// log block의 page table에서 page가 존재하는지 검색
	for (i=0; i < PAGES_PER_BLK; i++) 
	{
		// log block mapping table에서 page offset을 통해 logical page를 찾는다
		d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (LOG_BLK_MAP_SIZE * lblk) + LOG_BLK_PGMAP + i);
		
		if (d8 == offset)
		{
			// log block의 virtual block number read
			blk = read_dram_32 (LOG_BLK_MAP_ADDR + (LOG_BLK_MAP_SIZE * lblk) + LOG_BLK_VADDR);

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
	UINT32 addr;
	UINT32 i, baddr;
	UINT8 d8, cmp;
		
	for (addr = 0; addr < EMPTY_BLK_BMP_BYTES; addr++)
	{
		d8 = read_dram_8 (EMPTY_BLK_BMP_ADDR + addr);

		// 해당하는 bank 검사
		i = bank;
		for (cmp = 0x80 >> bank; cmp != 0; cmp = cmp >> NUM_BANKS)
		{
			// 비어있는 block으로 체크되어있을 경우 빈 주소 반환
			if ((d8 & cmp) != 0) {
				// 사용하는 block으로 bitmap에 입력
				set_bit_dram (EMPTY_BLK_BMP_BYTES + addr, i);

				// virtual block number 반환
				baddr = addr * (8 / NUM_BANKS) + (i / NUM_BANKS);
				return baddr;
			}
			i += NUM_BANKS;
		}
	}
	
	// 그래도 빈 block이 없으면 garbage collection 시도
	return garbage_collection(bank);
}


// 해당 bank의 virtual block을 using 상태로 체크 - incomplete
static BOOL8 set_using_blk (UINT32 const bank, UINT32 const vbn)
{
	UINT32 offset, bit_offset;

	offset = (vbn * NUM_BANKS) / 8;
	bit_offset = vbn % (8 / NUM_BANKS);
	clr_bit_dram (EMPTY_BLK_BMP_ADDR + offset, bit_offset + bank);

	return TRUE;
}


// garbage collection - complete
static UINT32 garbage_collection (UINT32 const bank)
{
	UINT32 vblk, lbn, vbn, new_dbn, offset, bit_offset;
	UINT32 i32, is_fmerge, j32;
	UINT8 d8;

	// garbage collection을 할 victim block 선정
	vblk = get_victim_block (bank);
	lbn = read_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_LADDR);
	vbn = read_dram_32 (DATA_BLK_MAP_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR);

	// log block에 page가 순서대로 위치하는지 검사
	is_fmerge = 0;
	for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
	{
		d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + i32);

		// log block에 기록된 적 없는 영역인 경우
		if (d8 == 0xff)
		{
			// 순서대로 기록되어 있고 빈 공간이 있으므로 partial merge
			break;
		}
		// log block에 page가 순서대로 들어있지 않은 상태
		else if (d8 != i32)
		{
			is_fmerge = 1;
			break;
		}
	}

	// partial merge
	if (i32 == PAGES_PER_BLK)
	{
		new_dbn = partial_merge(bank, vblk, lbn);
	}
	// full merge
	else
	{
		new_dbn = full_merge(bank, vblk, lbn);
	}

	// data block mapping table 갱신
	write_dram_32 (DATA_BLK_MAP_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR, new_dbn);
	write_dram_32 (DATA_BLK_MAP_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_MAP_SIZE) + DATA_BLK_OP, 0x80);

	// 이전에 사용한 data block을 empty block mapping table에 표시
	offset = (vbn * NUM_BANKS) / 8; // vbn * NUM_BANKS = 전체 block에서 block number, /8 = 한 byte에 8 block 표시
	bit_offset = vbn % (8 / NUM_BANKS); // 8 / NUM_BANKS = 한 byte에 표시되는 block 수
	set_bit_dram (EMPTY_BLK_BMP_ADDR + offset, bit_offset + bank);

	// log block mapping table 초기화
	write_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_VADDR, 0x00000000);
	write_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_LADDR, 0x00000000);
	write_dram_8 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGCNT, 0x00);
	for (j32 = 0; j32 < PAGES_PER_BLK; j32 = j32 + 4) 
	{
		write_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + j32, 0xffffffff);
	}

	// log block counter 감소
	log_blk_cnt--;

	// merge이전에 사용하던 data block이 empty 상태이므로 data block을 반환
	return vbn;
}


// partial merge - complete
static UINT32 partial_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
	UINT32 vbn, offset, bit_offset, lvbn;
	UINT32 dmap_offset;
	UINT32 i32;
	UINT8 d8;

	// garbage collection을 할 block의 virtual block number read
	dmap_offset = lbn * NUM_BANKS + bank;
	vbn = read_dram_32 (DATA_BLK_MAP_ADDR + (dmap_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR);
	lvbn = read_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_VADDR);
	
	for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
	{
		// log block에 있는 page 번호 검사
		d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (i32 * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + i32);

		// log block에 정위치에 있는 경우 data block의 page bitmap에 표시
		if (d8 == i32)
		{
			set_bit_dram (DATA_BLK_MAP_ADDR + (dmap_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + (i32 / 8), i32 % 8);
		}
		// log block에 없는 page인 경우 data block에서 이동
		else if (d8 == 0xff)
		{
			// offset 계산
			offset = i32 / 8;
			bit_offset = i32 % 8;

			// data block에 page가 있는지 확인
			if (tst_bit_dram (DATA_BLK_MAP_ADDR + (dmap_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
			{
				// data block에 있는 내용을 log block으로 copy back
				nand_page_copyback (bank, vbn, i32, lvbn, i32);
			}
		}
	}
	// 정리가 끝난 data block은 erase
	nand_block_erase (bank, vbn);

	return lbn;
}


// full merge - complete
static UINT32 full_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
	UINT32 i32, offset, bit_offset;
	UINT32 nblk, vbn, dmap_offset;
	UINT8 d8;

	// 새로 만들 block 할당
	nblk = get_gc_vblock (bank);

	// data block에 있는 page를 이동
	dmap_offset = lbn * NUM_BANKS + bank;
	vbn = read_dram_32 (DATA_BLK_MAP_ADDR + (dmap_offset * DATA_BLK_MAP_SIZE) + DATA_BLK_VADDR);
	for (i32 = 0; i32 < PAGES_PER_BLK; i32++)
	{
		offset = i32 / 8;
		bit_offset = i32 % 8;

		// 유효한 page인 경우 새 block으로 복사
		if (tst_bit_dram (DATA_BLK_MAP_ADDR + (lbn * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
		{
			// data block에 있는 내용을 새 block으로 copy back
			nand_page_copyback (bank, vbn, i32, nblk, i32);
		}
	}
	// 정리가 끝난 data block은 erase
	nand_block_erase (bank, vbn);

	// log block에 있는 page를 이동
	vbn = read_dram_32 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_VADDR);
	for (i32 = 0; i32 < PAGES_PER_BLK; i32++)
	{
		d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (vblk * LOG_BLK_MAP_SIZE) + LOG_BLK_PGMAP + i32);

		// 유효한 page인 경우 새 block으로 복사
		if (d8 < PAGES_PER_BLK)
		{
			nand_page_copyback (bank, vbn, i32, nblk, d8);
		}

		// data block bitmap에 표시
		offset = d8 / 8;
		bit_offset = d8 % 8;
		set_bit_dram (DATA_BLK_MAP_ADDR + (lbn * DATA_BLK_MAP_SIZE) + DATA_BLK_VPBMP + offset, bit_offset);
	}	
	// 정리가 끝난 log block은 erase
	nand_block_erase (bank, vbn);

	// 이전에 사용한 log block은 다음 full merge에 사용할 new block으로 설정
	set_gc_vblock (bank, vbn);

	// 새로 완성된 block의 주소 반환
	return nblk;
}


// victim block을 선정한다 - sample
static UINT32 get_victim_block (UINT32 const bank)
{
	static UINT8 i = 0;
	UINT8 d8;

	if (bank == NUM_BANKS) 
	{
		i = (i + 1) % NUM_LOG_BLKS;
	}
	else
	{
		for (i=0; i < NUM_LOG_BLKS; i++)
		{
			d8 = read_dram_8 (LOG_BLK_MAP_ADDR + (i * LOG_BLK_MAP_SIZE) + LOG_BLK_BANK);
			if (d8 == bank)
			{
				break;
			}
		}
	}

	return (UINT32)i;
}

