/*
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	dld.c
*/

/* 
  There is a companion file rsym.c which is used to build
  a list of the external symbols in a COFF, ECOFF or A.OUT object file.
  These are loaded into ECL, and the linking is done directly inside ECL.
*/

#include "config.h"
#include "page.h"
#include "objff.h"
#include <sys/wait.h>
#include <limits.h>

/* bsearch is provided in the Standard C library		*/
#if !defined(__stdlib_h) && !defined(_STDLIB_H_) && !defined(__STDLIB_H__) && !defined(_STDLIB_H)
char *
bsearch(VOID *key, VOID *base, unsigned long nel, unsigned long offset,
	int (*compar)(const VOID *, const VOID *))
{
  int low, high, mid, n;
  low = 0;
  high = nel - 1;
  while (low <= high) {
    mid = (low + high) / 2;
    n = compar(key, (char *)((int)base + (offset * mid)));
    if (n < 0)
      high = mid - 1;
    else if (n > 0)
      low = mid + 1;
    else			/* found match */
      return ((char *)((int)base + (offset * mid))); }
  return (NULL);
}
#endif __stdlib_h

struct node {
  char *string;
  unsigned int address;
};

typedef struct node TABL[]; 

struct  string_address_table {
  TABL *ptable;
  unsigned int length;
  unsigned int alloc_length;
} c_table, combined_table;

#define PTABLE_EXTRA 20

/* ---------------------------------------------------------------------- */
#define TEMPSPACE 50000
static char tempspace[TEMPSPACE];
static char *lim;

char *temp_malloc(unsigned int n)
{  char *val;
   val = (char *)(((int)lim + 7) & ~07); /* was 3 on SUN3 */
   lim = val + n;
   if (lim < tempspace + TEMPSPACE)
     return val;
   else {
     lim = val;
     return malloc(n);
   }
 }

/* free up space which is not allocated in tempspace */

#define TEMP_FREE(x) \
if (((char *)x) > tempspace+TEMPSPACE || ((char *)x) < tempspace) free(x)

/* ---------------------------------------------------------------------- */

#ifdef apollo
int section_padding[5];
int section_vaddr[5];

int section_index(vaddr)
int vaddr;
{ int i;
  for (i = 0; i < 4; i++)
   if (vaddr < section_vaddr[i+1])
	return(i);
  return(4);
}
#endif apollo

static RELOC reloc_info;
static SYMENT *symbol_table;

int node_compare(const VOID *node1, const VOID *node2)
{ return(strcmp(((struct node *)node1)->string,
		((struct node *)node2)->string));
}

/* ---------------------------------------------------------------------- */

/*
 *----------------------------------------------------------------------
 *
 * dld --
 *     dynamically load a file into memory.
 *
 * Results:
 *	none.
 *
 * Side effects:
 *	codeblock: containing address where the code has been loaded, and
 *		   its size
 *
 *----------------------------------------------------------------------
 */
dld(char *faslfile, struct codeblock *Cblock)
{
	FILHDR fileheader;
#ifdef COFF
	AOUTHDR aouthdr;
#ifdef apollo
#define NSCNS 5
        SCNHDR section[NSCNS];
	int unwindsize, aptvsize;
	int section_start;
#else
#define NSCNS fileheader.f_nscns
        SCNHDR section[4];
#endif apollo
#ifdef ECOFF
	HDRR symheader;		/* header for symbol table */
#endif ECOFF
#endif COFF

#ifdef hpux
	int string_size = 0;
#endif hpux
	int textsize, datasize, bsssize, nsyms, extra_bss = 0;
	char *start_address, *segment_start;

	object memory, data;
	FILE *fp;
	int i, file_end;
	char *string_table;

	lim = tempspace; /* reset tmp malloc */
	fp = fopen(faslfile, "r");
	if (fp == NULL)
	  FEerror(";;; Could not open binary file.", 0);
	if (!fread((char *)&fileheader, sizeof(FILHDR), 1, fp))
	  FEerror(";;; Could not get the header.", 0);
	nsyms = NSYMS(fileheader);
#ifdef COFF
	/* read aouthdr */
	if (fileheader.f_opthdr)
	  fread(&aouthdr, fileheader.f_opthdr, 1, fp);
	fread(&section[0], sizeof(SCNHDR), 1, fp);
        textsize = section[0].s_size;
#ifdef apollo
	section_start = 0;
	section_vaddr[0] = section[0].s_vaddr;
	section_padding[0] = section[0].s_vaddr - section_start;
	/* APOLLO has additional sections */
	/*	.unwind	*/
	fread((char *)&section[1], sizeof(SCNHDR), 1, fp);
	unwindsize = section[1].s_size;
	section_start += section[0].s_size;
	section_vaddr[1] = section[1].s_vaddr;
	section_padding[1] = section[1].s_vaddr - section_start;
	/*	.data	*/
	fread((char *)&section[2], sizeof(SCNHDR), 1, fp);
	datasize = section[2].s_size; 
	section_start += section[1].s_size;
	section_vaddr[2] = section[2].s_vaddr;
	section_padding[2] = section[2].s_vaddr - section_start;
	/*	.aptv	*/
	fread((char *)&section[3], sizeof(SCNHDR), 1, fp);
	if (section[2].s_scnptr) {
	   aptvsize = section[3].s_size;
	   section_start += section[2].s_size;
	} else {
	   /* APTV is included in .data section */
	   aptvsize = section[3].s_size - datasize;
	}
	section_vaddr[3] = section[3].s_vaddr;
	section_padding[3] = section[3].s_vaddr - section_start;
	/*	.bss	*/
	fread((char *)&section[4], sizeof(SCNHDR), 1, fp);
	if (strcmp(section[4].s_name, ".bss") == 0)
	  bsssize = section[4].s_size; 
	else
	  bsssize = 0;
	section_start += section[3].s_size;
	section_vaddr[4] = section[4].s_vaddr;
	section_padding[4] = section[4].s_vaddr - section_start;
#else /* !apollo */
	/* Read data sections: .rdata (ECOFF), .data, .bss */
	datasize = 0; bsssize = 0;
	for (i = 1; i < NSCNS; i++) {
	  fread((char *)&section[i], sizeof(SCNHDR), 1, fp);
	  if (strcmp(section[i].s_name, ".bss") == 0)
	    bsssize = section[i].s_size; 
	  else
	    datasize = section[i].s_size; 
	}
#ifdef ECOFF
	datasize = aouthdr.dsize;
	bsssize = aouthdr.bsize;
#endif ECOFF
#endif apollo
#endif COFF
#ifdef AOUT
	textsize = fileheader.a_text;
	datasize = fileheader.a_data;
	bsssize = fileheader.a_bss;
#endif AOUT

	/* 
	  STEPS:
	  1) read in the symbol table from the file
	  2) read in the string table from the file
	  3) load text + data from the file
	  4) go through the symbol table, filling external entries
	  5) go through the relocation information for sections 0..2
	     relocating the text.
	*/

	/* STEP 1 -------- Read the symbol table */

#ifdef ECOFF
	/* ECOFF has a Symbolic Header at the beginning of the symbol table */
	fseek(fp, fileheader.f_symptr, 0);
	fread(&symheader, sizeof(HDRR), 1, fp);
	/* We care only of external symbols */
	nsyms = symheader.iextMax;
	fseek(fp, symheader.cbExtOffset, 0);
#else
	fseek(fp, (int)N_SYMOFF(fileheader), 0);
#endif ECOFF
	symbol_table = (SYMENT *)temp_malloc(sizeof(SYMENT)*nsyms);
	/*
	  Since for alignment reasons on some machines (e.g. i386 and sparc)
	  the sizeof(SYMENT) != SYMESZ, we must read one SYMENT at a time.
	  */
	for (i = 0;  i < nsyms;  i++) {
	  fread((char *)&symbol_table[i], SYMESZ, 1, fp);
#ifdef hpux
	  symbol_table[i].n_un.n_strx = string_size;
	  string_size += symbol_table[i].n_length + 1;
	  fseek(fp, (int)symbol_table[i].n_length, 1);
#endif hpux
	}

#ifdef ECOFF
	/* In ECOFF external symbols are at the end of file */
	file_end = ftell(fp);
#endif ECOFF

	/* STEP 2 -------- Read the string table */

#ifdef hpux
	{ char *p;
	  int slen, i, j;
	  string_table = p = temp_malloc((unsigned int)string_size);
	  fseek(fp, LESYM_OFFSET(fileheader), 0);
	  for (i = fileheader.a_lesyms, j = 0; i > 0; i = i - slen - SYMESZ) {
	    fseek(fp, SYMESZ, 1);
	    slen = symbol_table[j++].n_length;
	    fread(p, slen, 1, fp);
	    *((p)+slen) = '\0';
	    p += slen + 1;
	  }
	}
	fseek(fp, RDATA_OFFSET(fileheader) + fileheader.a_drsize, 0);
#else
#ifdef ECOFF
	/* Read External Strings */
	fseek(fp, symheader.cbSsExtOffset, 0);
	i = symheader.cbFdOffset - symheader.cbSsExtOffset;
	string_table = temp_malloc(i);
	fread(string_table, i, 1, fp);
#else /* COFF */
	/* The string table is located just after the symbols:
	   first word contains its size */
	if (!fread((char *)&i, sizeof(int), 1, fp))
	  FEerror(";;; The string table of this file is missing.", 0);
	fseek(fp, -sizeof(int), 1);
	string_table = temp_malloc(i);
	if (i != fread(string_table, 1, i, fp))
	  FEerror(";;; Could not read whole string table.", 0);
 	while ((i = getc(fp)) == 0) ; /* skip null characters */
	ungetc(i, fp);
#endif ECOFF
#endif hpux

#ifndef ECOFF
	file_end = ftell(fp);
#endif ECOFF

	if (!(c_table.ptable && *(c_table.ptable)))
	  siLbuild_symbol_table(0);
	if (!c_table.ptable)
	  FEerror(";;; Symbol table not loaded", 0);
	qsort((char*)(c_table.ptable), (int)(c_table.length),
	      sizeof(struct node), node_compare);

	/* figure out if there is more bss space needed */
	extra_bss = get_extra_bss(symbol_table, nsyms,
				  datasize+textsize+bsssize, string_table);
	
#ifdef apollo
	Cblock->cd_size = datasize+textsize+bsssize + extra_bss +
				unwindsize + ((aptvsize > 0) ? aptvsize : 0);
#else
	Cblock->cd_size = datasize+textsize+bsssize + extra_bss;
#endif apollo
	/*
	   We must align at 8 to ensure that doubles are properly aligned.
	   On SGI this helps work around an R4000 bug present in 2.1 and 2.2
	   silicon which appears when certain classes of branch instructions
	   are the last instruction on a page of virtual memory.
	 */
	Cblock->cd_start = 
	  (char *)((int)alloc_contblock(Cblock->cd_size + 7) + 7 & ~7);
	start_address = Cblock->cd_start;

	/* STEP 3 -------- Load text and data	*/

	if (fseek(fp, N_TXTOFF(fileheader), 0) < 0)
	  FEerror(";;; File seek error.", 0);
#ifdef apollo
	fread((char *)start_address, textsize + datasize +
					   unwindsize + aptvsize, 1, fp);
#else
	fread((char *)start_address, textsize + datasize, 1, fp);
#endif apollo

	/* STEP 4 -------- Put value into symbols  */

	link_symbols(nsyms, string_table, start_address);
	
	/* STEP 5 -------- Perform relocation	*/

	segment_start = start_address;
#ifdef COFF
	{ int j;
	  /* Apollo optimization.
	     Do .text last, so that we can bypass APTV indirection */
	  for (j = NSCNS-1; j >= 0; j--) {
	    if (section[j].s_nreloc == 0) continue;
	    fseek(fp, section[j].s_relptr, 0);
	    for (i = 0; i < section[j].s_nreloc; i++) {
	      fread(&reloc_info, RELSZ, 1, fp);
#ifdef apollo
	      relocate(segment_start + reloc_info.r_vaddr -
		       section_padding[section_index(reloc_info.r_vaddr)],
		       start_address);
#else
	      relocate(segment_start + reloc_info.r_vaddr, start_address);
#endif apollo
	    }
	  }
	}
#endif COFF
#ifdef AOUT
	{ int nrel = fileheader.a_trsize / RELSZ;
	  fseek(fp, N_TRELOCOFF(fileheader), 0);
	  for (i = 0; i < nrel; i++) {
	    fread((char *)&reloc_info, RELSZ, 1, fp);
	    relocate(segment_start + reloc_info.r_address, start_address);
	  }
#ifdef N_DRELOFF
	  fseek (fp, N_DRELOFF(fileheader), 0);
#endif N_DRELOFF
	  nrel = (fileheader.a_drsize / RELSZ);
	  segment_start += fileheader.a_text;
	  for (i=0; i < nrel; i++) {
	    fread((char *)&reloc_info, RELSZ, 1, fp);
	    relocate(segment_start + reloc_info.r_address, start_address);
	  }
	}
#endif AOUT

	/* end of relocation */
	fclose(fp);

	TEMP_FREE(string_table);
	TEMP_FREE(symbol_table);
}

get_extra_bss(SYMENT *symbol_table, int length, int start,
	      char *string_table)
{  int result = start;
   SYMENT *end, *sym;
   end = symbol_table + length;
   for (sym = symbol_table; sym < end; sym++) {
#ifdef COFF
#ifndef ECOFF
     char tem[SYMNMLEN +1];
#endif ECOFF
     if (0)
       /* what we really want is
	  if (sym->n_scnum == 0 && sym->n_sclass == C_EXT
	  && !(bsearch(..in ptable for this symbol)))
	  Since this won't allow loading in of a new external array
	  char foo[10]  not ok
	  static foo[10] ok.
	  for the moment we give undefined symbol warning..
	  Should really go through the symbols, recording the external addr
	  for ones found in ptable, and for the ones not in ptable
	  set some flag, and add up the extra_bss required.  Then
	  when you have the new memory chunk in hand,
	  you could make the pass setting the relative addresses.
	  for the ones you flagged last time.
	  */
#endif COFF
#ifdef AOUT
       if (sym->n_type == (N_EXT | N_UNDF))
#endif AOUT
	 /* external bss so not included in size of bss for file */
	 {  int val = SYM_VALUE(*sym);
	    struct node nod;
	    nod.string = SYM_NAME(sym);
	    if (val && c_table.ptable &&
		!bsearch((char *)(&nod), (char*)(c_table.ptable),
			 c_table.length, sizeof(struct node),
			 node_compare)) {
	      SYM_VALUE(*sym) = result;
	      result += val;}
	  }
     sym += NUM_AUX(sym); 
   }
   return (result - start);
 }
 

/* go through the symbol table:
   1. resolve references to external undefined symbols
   2. change addresses of other symbols to reflect the current cf_start */

link_symbols(unsigned int length, char *string_table,
	     unsigned long *start_address)
{  SYMENT *end, *sym;
   unsigned int typ;
   char *str;
#ifndef ECOFF
   char tem[SYMNMLEN +1];
   tem[SYMNMLEN] = 0;
#endif ECOFF
   end = symbol_table + length;

   for (sym = symbol_table; sym < end; sym++) {

     char *p;

     p = SYM_NAME(sym);
     typ = SYM_TYPE(sym);

#ifdef AOUT
#ifdef N_STAB
     if (N_STAB & sym->n_type) continue; /* skip: It is for dbx only */
#endif N_STAB
     typ = N_SECTION(sym);
#endif AOUT

     switch (typ) {

#ifdef COFF
     case C_STAT:
#ifdef apollo
       SYM_VALUE(*sym) = (int)start_address - section_padding[sym->n_scnum -1];
#else
       SYM_VALUE(*sym) = (int)start_address;
#endif apollo
       break;
#endif COFF

#ifdef AOUT
     case N_ABS: case N_TEXT: case N_DATA: case N_BSS:
       SYM_VALUE(*sym) = (int)start_address;
       break;
#endif AOUT

     case C_EXT:
       if (EXT_UNDEF(sym))
	 /* search the symbol_table for the address of symbol and
	    store it as value of sym */
	 set_symbol_address(sym, SYM_NAME(sym), start_address);
       else
#ifdef ECOFF
	 if (EXT_EXPORTED(sym))
	   SYM_VALUE(*sym) += (int)start_address;
       else
#endif ECOFF
	 {
	   /*  we should add the symbol name, so it would be accessible by
	       future loads (init_code should be an exception though. Beppe) */
	   printf("\nEXT_UNDEF %s", SYM_NAME(sym)); fflush(stdout); }
       break;
     default:
       break;
     }
     sym += NUM_AUX(sym);	/* skip auxiliary entries */
   }
 }

set_symbol_address(SYMENT *sym, char *string, char *start_address)
{  struct node *answ, nod;
   nod.string = string;
   answ = (struct node *)bsearch((char *)(&nod), (char *)c_table.ptable,
				 c_table.length, sizeof(struct node),
				 node_compare);
#ifdef COFF
   if (answ)
#ifdef ECOFF
     SYM_VALUE(*sym) = answ->address;
#else
     SYM_VALUE(*sym) = answ->address - SYM_VALUE(*sym);
#endif ECOFF
   else
     fprintf(stderr, "\n;;; Undefined symbol %s\n", string); fflush(stderr);
#else /* AOUT */
   if (answ)
     SYM_VALUE(*sym) = answ->address;
   else {
     char *name;

     name = malloc(1 + strlen(string));
     strcpy(name, string); 
     SYM_VALUE(*sym) = SYM_VALUE(*sym) + (unsigned int) start_address;
     add_symbol(name, SYM_VALUE(*sym)); /* changed string into name!!! tito */
     fprintf(stderr, "\n;;; Undefined symbol %s\n", string); fflush(stderr);
   }
#endif AOUT
}

/* ----------------------------------------------------------------------
 *	Relocation
 * ----------------------------------------------------------------------
 */

/*
  Indications on how to perform relocation is available
  in the tables reloc_howto_type in gnu/binutils/bfd (eg. coff-mips.c).
  Here is a sample entry:

  HOWTO (MIPS_R_REFHALF,	-- type
	 0,			-- rightshift
	 1,			-- size (0 = byte, 1 = short, 2 = long)
	 16,			-- bitsize
	 false,			-- pc_relative
	 0,			-- bitpos
	 complain_overflow_bitfield, -- complain_on_overflow
	 mips_generic_reloc,	-- special_function
	 "REFHALF",		-- name
	 true,			-- partial_inplace
	 0xffff,		-- src_mask
	 0xffff,		-- dst_mask
	 false),		-- pcrel_offset
*/

#ifdef COFF
relocate(char *where, char *start_address)
{  unsigned int new_value;

#ifdef __mips
   static unsigned long *refhi_where;	/* store loc for  */

   if (!reloc_info.r_extern)
     new_value = (int)start_address;
   else
#endif __mips
     new_value = SYM_VALUE(symbol_table[reloc_info.r_symndx]);

   switch (reloc_info.r_type) {

       case R_ABS: return;

#ifndef __mips

    case R_DIR32:
      { char tem[SYMNMLEN +1];
#ifdef apollo
	if (strcmp(SYM_NAME(&symbol_table[reloc_info.r_symndx]), ".aptv") == 0)
	  *(unsigned int *)where =
	   *(unsigned int *)(*(unsigned int *)where
				+ new_value
				+ 2);
	else
#endif apollo
	  *(unsigned int *)where += new_value;
      }
      break;

    case R_DIR32S: {
      char *p;
      unsigned int value;
 
      p = (char *)(&value);
      p[3] = where[0];
      p[2] = where[1];
      p[1] = where[2];
      p[0] = where[3];
      value += new_value;
      where[0] = p[3];
      where[1] = p[2];
      where[2] = p[1];
      where[3] = p[0];
      break;
    }
    case R_RELBYTE:
      *(unsigned char *)where += new_value;
      break;

    case R_RELWORD:
      *(unsigned short *)where += new_value;
      break;

    case R_RELLONG:
      *(unsigned int *)where += new_value;
      break;

    case R_PCRBYTE:
      *(unsigned char *)where += new_value - (unsigned int)start_address;
      break;

    case R_PCRWORD:
      *(unsigned short *)where += new_value - (unsigned int)start_address;
      break;

    case R_PCRLONG:
      *(unsigned int *)where += new_value - (unsigned int)start_address;
      break;

#else /* __mips */

    case R_REFHALF:
      *(unsigned short *)where += new_value;
      break;
    case R_REFWORD:
      *(unsigned int *)where += new_value;
      break;
    case R_JMPADDR:		/* 26-bit jump reference */
      *(long *)where = (*(long *)where & ~0x3ffffff) |
	((int)(new_value >> 2) + (*(long *)where & 0x3ffffff)) & 0x3ffffff ;
      break;
    case R_REFHI:
      /* need to add 1 if R_REFLO is negative */
      refhi_where = (unsigned long *)where;
      *(long *)where += new_value >> 16;
      break;
    case R_REFLO:
	 { unsigned long val;
	   unsigned long insn = *(long *)where;

	   /* Lower 16 bits of REFHI and REFLO are the offset to be added.
	      But the low order 16 bits of REFLO are treated as a signed value.
	      Therefore, a negative value in the low order bits requires an
	      adjustment in the high order bits.
	      */
	   val = ((*refhi_where & 0xffff) << 16) + (insn & 0xffff);
	   if (insn & 0x8000)
	     val -= 0x10000;	/* recover true value */
	   val += (new_value & 0xffff);
	   *refhi_where = (*refhi_where & ~0xffff) | (val >> 16);
	   if (val & 0x8000)
	     *refhi_where += 1;	/* carry into high order bits */
	   *(long *)where = (insn & ~0xffff) | (val & 0xffff);
	 break;
	 }

/*    case R_GPREL:		/* global pointer relative */

#endif __mips

    default:
      FEerror(";;; ~d: unsupported relocation type.", 1,
	      MAKE_FIXNUM(reloc_info.r_type));
    }
}
#endif COFF

#ifdef AOUT
#ifdef sparc

relocate(char *where, char *start_address)
{ unsigned int new_value;
  if (reloc_info.r_extern)
    new_value = SYM_VALUE(symbol_table[reloc_info.r_index]) + reloc_info.r_addend;
  else
    switch (reloc_info.r_index) {
    case N_DATA: case N_BSS: case N_TEXT:
      new_value = (int)start_address + reloc_info.r_addend;
      break;
    default:
      FEerror(";;; Relocation fault: index ~x.", 1,
	      MAKE_FIXNUM(reloc_info.r_index));
    };

  switch (reloc_info.r_type) {
  case RELOC_8:
    *(char *)where = new_value;
    break;
  case RELOC_16:
    *(short *)where = new_value;
    break;
  case RELOC_32:
    *(long *)where = new_value;
    break;
  case RELOC_DISP8:
    *(char *)where = new_value - (int)start_address;
    break;
  case RELOC_DISP16:
    *(short *)where = new_value - (int)start_address;
    break;
  case RELOC_DISP32:
    *(long *)where = new_value - (int)start_address;
    break;
  case RELOC_WDISP30:
    /* Note: where == start_address - reloc_info.addend */
    *(long *)where |= ((new_value - (int) start_address) >> 2);
    break;
  case RELOC_HI22:
    *(long *)where |= new_value >> 10;
    break;
  case RELOC_LO10:
    *(long *)where |= new_value & 0x3ff;
    break;
  default:
    FEerror(";;; ~d: unsupported relocation type.", 1,
	    MAKE_FIXNUM(reloc_info.r_type));
  }
}

#else /* not sparc */

relocate(char *where, char *start_address)
{ unsigned int new_value;
#ifdef hp9000s300
  switch(reloc_info.r_segment) {
  case RNOOP: return;
  case REXT: 
    new_value = SYM_VALUE(symbol_table[reloc_info.r_symbolnum]);
    break;
  case RDATA: 
  case RBSS: 
  case RTEXT: new_value = (int)start_address; break;
  default:
    printf(
    "\nRel_Info {r_segment: %x, r_symbolnum: %x, r_address: %d} -- Ignored", 
	   reloc_info.r_segment,
	   reloc_info.r_symbolnum,
	   reloc_info.r_address); 
    fflush(stdout);
    return;
  };
#else
  if (reloc_info.r_extern) {
    if (reloc_info.r_pcrel)
	new_value = SYM_VALUE(symbol_table[reloc_info.r_symbolnum])
	  - (int)start_address;
    else
      new_value = SYM_VALUE(symbol_table[reloc_info.r_symbolnum]);
  }
  else {
    switch (reloc_info.r_symbolnum) {
    case N_TEXT:
      /* when it is the result of a previous ld -r */
      if (reloc_info.r_pcrel) {
	new_value = 0;
	break;
      }
    case N_DATA: case N_BSS:
      new_value = (int)start_address;
      break;
    default:
      FEerror(";;; Relocation fault: index ~x.", 1,
	      MAKE_FIXNUM(reloc_info.r_symbolnum));
    }
  };
#endif hp9000s300

#ifdef vax
  if (!reloc_info.r_pcrel || reloc_info.r_extern)
#endif vax
    switch (reloc_info.r_length) {
    case 0:
      *(char *)where = new_value + *(char *) where;
      break;
    case 1:
      *(short *)where = new_value + *(short *) where;
      break;
    case 2:
      *(long *)where = new_value + *(long *) where;
      break;
    }
}
#endif sparc
#endif AOUT

/*
 * Use the add_symbol to add a c symbol, which you want to refer
 * to in subsequent loads.  The address used in subsequent loads,
 * will be the load address of the current symbol.
 * Such a symbol may only be added once, since subsequent references
 * will try to link to the old address.
 */

/* To do: Addition of one symbol to the ptable is very slow, because we 
   sort each time! */

add_symbol(char *string, int address)
{   struct node nod;
    if (!(c_table.ptable))
      return;			/* this is in a function before ptable is init */
    if ((c_table.alloc_length) - (c_table.length) > 1) { 
    BEGIN:
      nod.string=string;
      if (bsearch((char *)(&nod), (char *)c_table.ptable, c_table.length,
		  sizeof(struct node), node_compare))
	FEerror(";;; The string ~a is already in the ptable", 1,
		make_simple_string(string));
      (*(c_table.ptable) + c_table.length)->string = string;
      ((*c_table.ptable) + c_table.length)->address = address;
      c_table.length = c_table.length + 1;
      qsort((char*)c_table.ptable, (int)c_table.length, sizeof(struct node),
	    node_compare);
    }
    else {
      /* grow the ptable */
      TABL *new, *old_pt;
      c_table.alloc_length = c_table.length + 1 + PTABLE_EXTRA;
      new = (TABL *)malloc(sizeof(struct node) * c_table.alloc_length);
      old_pt = c_table.ptable;
      /* copy it */
      {  register int i ;
	 for (i=0; i < c_table.length; i++) {
	   (*new)[i].string = (*old_pt)[i].string;
	   (*new)[i].address = (*old_pt)[i].address;
	 }
       }
      c_table.ptable = new; 
      free((char *)old_pt);
      goto BEGIN;
    }
  }

#define SYM_ADDRESS(table,i) ((*(table).ptable))[i].address
#define SYM_STRING(table,i) ((*(table).ptable))[i].string

read_special_symbols(char *symfile)
{  FILE *symin;
   char *symbols;
   int i = 0, jj;
   struct lsymbol_table tab;
   if (!(symin = fopen(symfile, "r"))) {
     perror(symfile);
     exit(1);
   }
   if (!fread((char *)&tab, sizeof(tab), 1, symin))
     FEerror(";;; No header in symbol table file.", 0);
   symbols = malloc(tab.tot_leng);
   c_table.alloc_length = PTABLE_EXTRA + tab.n_symbols;
   c_table.ptable = (TABL *) malloc(sizeof(struct node) * c_table.alloc_length);
   if (!c_table.ptable) {
     perror("Could not allocate symbol table.");
     exit(1);
   }
   c_table.length = tab.n_symbols;
   while (i < tab.n_symbols) {
     fread((char *)&jj, sizeof(int), 1, symin);
     SYM_ADDRESS(c_table, i) = jj;
     SYM_STRING(c_table, i) = symbols;
 
     while (*(symbols++) = getc(symin)) 
       ;
     i++;
   }
 }

#define CFUN_LIM 10000

int maxpage;

#define CF_FLAG (1 << 31) 

cfuns_to_combined_table(unsigned int n)
     /* non zero n will ensure new table length */
{  int ii = 0;  
   register int i, j;
   register object x;
   register char *p,*cf_addr;
   register struct typemanager *tm;
   if (! (n || combined_table.ptable)) n = CFUN_LIM;
   if (n && combined_table.alloc_length < n) {
     combined_table.ptable = NULL;
     combined_table.ptable = (TABL *)malloc(n* sizeof(struct node));
     if (!combined_table.ptable)
       FEerror(";;; Unable to allocate", 0);
     combined_table.alloc_length = n;
   }

   for (i = 0;  i < maxpage;  i++) {
#ifdef CLOS
     if ((enum type)type_map[i] == tm_table[(short)t_gfun].tm_type)
       printf(";;; Generic function not dealt\n");
#endif CLOS
     if ((enum type)type_map[i] != tm_table[(short)t_cfun].tm_type)
       continue;
     tm = tm_of((enum type)type_map[i]);
     p = pagetochar(i);
     for (j = tm->tm_nppage; j > 0; --j, p += tm->tm_size) {
       x = (object)p;
#ifdef CLOS
       if (type_of(x) == t_gfun) printf(";;; Generic function not dealt\n");
#endif CLOS
       if (type_of(x) != t_cfun) continue;
       if ((x->d.m == FREE) || x->cf.cf_self == NULL)
	 continue;
       /* Sherlis looks also at the cdefn (the proclaimed call types). Beppe */
       cf_addr = (char * ) ((unsigned int)(x->cf.cf_self));	
	
       SYM_ADDRESS(combined_table,ii) = (unsigned int)cf_addr;
       SYM_STRING(combined_table,ii) = (char *)(CF_FLAG | (unsigned int)x) ;
       /*       (x->cf.cf_name ? x->cf.cf_name->s.st_self : NULL) ; */
       combined_table.length = ++ii;
       if (ii >= combined_table.alloc_length)
	 FEerror(";;; Need a larger combined_table", 0);
     }
   }
 }

#ifdef hpux
int
count_symbols(FILHDR *phdr, FILE *fp)
{  int nsyms,i;
   fseek(fp, (int)(LESYM_OFFSET(*phdr)), 0);
   for(i = phdr->a_lesyms, nsyms = 0; i > 0; nsyms++) {
     SYMENT tmp;
     fread((char *)&tmp, SYMESZ, 1, fp); i -= SYMESZ;
     fseek(fp, (int)tmp.n_length, 1); i -= tmp.n_length;
   }
   return (nsyms);
 }
#endif hpux


/* ----------------------------------------------------------------------
 * This could be made to work for all UNIX
 */

#ifdef unix

int
faslink(object faslfile, object ldargstring)
{
#ifdef AOUT
	FILHDR header, faslheader;
#define	textsize	header.a_text
#define	datasize	header.a_data
#define	bsssize		header.a_bss
#define	textstart	sizeof(header)
#endif AOUT
#ifdef COFF
	FILHDR header, faslheader;
	SCNHDR sectionheader;
	int textsize, datasize, bsssize;
	int textstart;
#endif COFF

	object temp_cfun, data, tempfile;
	struct codeblock Cblock;
	FILE *fp;
	char filename[MAXPATHLEN];
	char ldargstr[MAXPATHLEN];
	char tempfilename[32];
	char command[MAXPATHLEN * 2];
	char buf[BUFSIZ];
	int i;
#ifdef IBMRT
	struct nlist nl[2];
#endif IBMRT

	coerce_to_filename(ldargstring, ldargstr);
	coerce_to_filename(faslfile, filename);

	sprintf(tempfilename, "/tmp/fasltemp%d", getpid());

	sprintf(command,
		"ld -d -N -x -A %s -T %x %s %s -o %s",
		ecl_self,
		(int)data_end,
		filename,
		ldargstr,
		tempfilename);

	printf(";;; Linking %s\n", filename);

	if (system(command) != 0) {
	  unlink(tempfilename);
	  FEerror(";;; The linkage editor failed.", 0);
	}

	fp = fopen(tempfilename, "r");
	setbuf(fp, buf);
	fread(&header, sizeof(header), 1, fp);
#ifdef COFF
	fread(&sectionheader, sizeof(sectionheader), 1, fp);
	textsize = sectionheader.s_size;
	textstart = sectionheader.s_scnptr;
	fread(&sectionheader, sizeof(sectionheader), 1, fp);
	datasize = sectionheader.s_size;
	fread(&sectionheader, sizeof(sectionheader), 1, fp);
	if (strcmp(sectionheader.s_name, ".bss") == 0)
		bsssize = sectionheader.s_size;
	else
		bsssize = 0;
#endif COFF
	temp_cfun = alloc_object(t_cfun);
	temp_cfun->cf.cf_name = OBJNULL;
	temp_cfun->cf.cf_block = &Cblock;
	Cblock.cd_size = textsize + datasize + bsssize;
	Cblock.cd_start = alloc_contblock(Cblock.cd_size);
	Cblock.cd_data = OBJNULL;
	fclose(fp);

	faslfile = open_stream(faslfile, smm_input, Cnil, Kerror);
	fp = faslfile->sm.sm_fp;
	fread(&faslheader, sizeof(faslheader), 1, fp);
#ifdef AOUT
#ifdef hpux
 	fseek(fp, RDATA_OFFSET(faslheader) + faslheader.a_drsize, 0);
#else
 	fseek(fp,
	      faslheader.a_text+faslheader.a_data+
	      faslheader.a_syms+faslheader.a_trsize+faslheader.a_drsize,
	      1);
	fread(&i, sizeof(i), 1, fp);
	fseek(fp, i - sizeof(i), 1);
#endif hpux
#endif AOUT
#ifdef COFF
	fseek(fp,
	      faslheader.f_symptr + SYMESZ*faslheader.f_nsyms,
	      0);
	fread(&i, sizeof(i), 1, fp);
	fseek(fp, i - sizeof(i), 1);
	while ((i = getc(fp)) == 0)
		;
	ungetc(i, fp);
#endif COFF

	sprintf(command,
		"ld -d -N -x -A %s -T %x %s %s -o %s",
		ecl_self,
		Cblock.cd_start,
		filename,
		ldargstr,
		tempfilename);

	if (system(command) != 0) {
	        unlink(tempfilename);
		FEerror(";;; The linkage editor failed.", 0);
	      }

	printf(";;; Loading %s\n", filename);

	tempfile = make_simple_string(tempfilename);
	tempfile = open_stream(tempfile, smm_input, Cnil, Kerror);
	fp = tempfile->sm.sm_fp;

	if (fseek(fp, textstart, 0) < 0)
		error("file seek error");

	fread(Cblock.cd_start, textsize + datasize, 1, fp);

	close_stream(tempfile, TRUE);

#ifdef IBMRT
	nl[0].n_un.n_name = "_init_code";
	nl[1].n_un.n_name = "";
	nlist(tempfilename, nl);
#endif IBMRT

	unlink(tempfilename);

#ifdef IBMRT
	(*(int (*)())SYM_VALUE(nl[0]))
#else
	(*(int (*)())(Cblock.cd_start))
#endif IBMRT
		(Cblock.cd_size, faslfile);
	close_stream(faslfile, TRUE);

	return(0);
}
#endif unix
