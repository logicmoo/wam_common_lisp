/*
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*

  This program reads the external symbols from an object file and
  writs them out to a file together with their addresses.

  This information is used for dynamic linking of object files.

*/


#include <stdio.h>
#include <sys/file.h>
#include "machines.h"
#include "objff.h"


FILHDR fileheader; 
SYMENT *symbol_table;
char *string_table;
char *start_address;
int nsyms;

main(int argc, char *argv[])
{
  if (argc != 3) { perror("Usage: rsym file outfile"); fflush(stdout); exit(1);}
#ifdef MSDOS
#include <sys/fcntl.h>
  _fmode = O_BINARY;		/* default to binary file mode */
#endif MSDOS
  get_myself(argv[1]);
  output_externals(argv[1], argv[2]);
  exit(0);
}

get_myself(char *filename)
{
	unsigned int i, string_size;
	FILE *fp;
        
	fp = fopen(filename, "r");
	
	if (fp == NULL) {
		fprintf(stderr, "Can't open %s\n", filename);
		exit(1);
	}
	ftell(fp);
	fread((char *)&fileheader, sizeof(FILHDR), 1, fp);
	if (N_BADMAG(fileheader)) {
	  fprintf(stderr, "Bad magic %s",filename);
	  exit(1);
	}
	if (fseek(fp, (int)(N_SYMOFF(fileheader)), 0)) {
	  fprintf(stderr, "seek error");
	  exit(1);
	}
#ifdef ECOFF
	{ HDRR symheader;
	  fseek(fp, N_SYMOFF(fileheader), 0);
	  fread(&symheader, sizeof(HDRR), 1, fp);

	  /* Read External Strings */
	  fseek(fp, symheader.cbSsExtOffset, 0);
	  i = symheader.cbFdOffset - symheader.cbSsExtOffset;
	  string_table = (char *)malloc(i);
	  fread(string_table, i, 1, fp);

	  /* Read External Symbols */
	  nsyms = symheader.iextMax;
	  symbol_table = (SYMENT *)malloc(SYMESZ * nsyms);
	  fseek(fp, symheader.cbExtOffset, 0);
	  for (i = 0; i < nsyms; i++)
	    fread(&symbol_table[i], SYMESZ, 1, fp);
	}
#else /* !ECOFF */

	nsyms = NSYMS(fileheader);
	symbol_table = (SYMENT *)malloc(sizeof(SYMENT) * nsyms);
#ifndef hpux

	/* Read Symbol Table */
	for (i = 0;  i < nsyms;  i++)
	  /* sizeof(SYMENT) and SYMESZ are not always the same */
	  fread((char *)&symbol_table[i], SYMESZ, 1, fp);

	/* Read the String Table */
#ifdef N_STROFF
	fseek(fp, N_STROFF(fileheader), 0);
#endif N_STROFF	
	/* First word is size of table: */
	if (fread((char *)&string_size, sizeof(int), 1, fp) > 0) {
	  string_table = (char *)malloc(string_size);
	  fseek(fp, -sizeof(int), 1);
	  if (string_size != fread(string_table, 1, string_size, fp)) {
	    perror("rsym could not read bad string table") ;
	    exit(1);
	  }
	} else {
	  perror("Error: There is no string table \n");
	  exit(1);
	}
#else /* hpux */

	for (i = 0;  i < nsyms;  i++) {
	  fread((char *)&symbol_table[i], SYMESZ, 1, fp);
	  symbol_table[i].n_un.n_strx = string_size;
	  string_size += symbol_table[i].n_length + 1;
	  fseek(fp, symbol_table[i].n_length, 1);
	}

	/* Read the String Table */
	{  char *p; 
	   int slen;
	   p = string_table = malloc((unsigned int)string_size);
	   dprintf( string table leng = %d, string_size);
	   fseek(fp, (int)( LESYM_OFFSET(fileheader)), 0);
	   for (i = 0; i < nsyms; i++) {
	     fseek(fp, SYMESZ, 1);
	     slen = symbol_table[i].n_length;
	     fread(p, slen, 1, fp);
	     *((p)+slen) = '\0';
	     p += symbol_table[i].n_length + 1;
	   }
	 }
#endif hpux
#endif ECOFF

	fclose(fp);
}

output_externals(char *infile, char *outfile)
{ FILE *fp, *symout;
  char *name;
  struct lsymbol_table tab;
  SYMENT *p, *end;
#ifndef ECOFF
  char tem[SYMNMLEN+1];
  tem[SYMNMLEN] = 0;
#endif ECOFF

  tab.n_symbols = 0; tab.tot_leng = 0;
  symout = fopen(outfile,"w");
  if (!symout) { perror(outfile); exit(1);}
  fseek(symout, sizeof(struct lsymbol_table), 0);
  end = symbol_table + nsyms;
  for (p = symbol_table; p < end; p++) {
    /*
      Is the following check enough?
      */
    if (EXT_and_TEXT_BSS_DAT(p)) {
      name = SYM_NAME(p);
      tab.n_symbols++;
      fwrite((char *)&SYM_VALUE(*p), sizeof(int), 1, symout);
      while (tab.tot_leng++, *name)
	putc(*name++, symout); 
      putc('\0', symout);
      p = p + NUM_AUX(p);
    }
  }
#ifdef apollo
  { struct scnhdr aptvhdr;	
    struct reloc entry;
    int i, addr;
    /* go to the relocation entries for section APTV */
    fp = fopen(infile, "r");
    fseek(fp, FILHSZ + sizeof(struct aouthdr) + 2 * SCNHSZ, 0);
    fread(&aptvhdr, sizeof(struct scnhdr), 1, fp);
    fseek(fp, aptvhdr.s_relptr, 0);

    /* Each entry corresponds to an external library symbol.
       Such relocation entry refers to an element of the APTV vector.
       Elements in the APTV consist in 6 bytes (2 bytes of JMP instruction,
       4 bytes of JMP address).
       The relocation entry contains in r_vaddr the address of these 4 bytes.
       Therefore the relocation address for the symbol is exactly the
       address of the APTV element, e.g. the r_vaddr - 2.
       */

    for (i = 0; i < aptvhdr.s_nreloc; i++) {
      fread(&entry, sizeof(entry), 1 ,fp);
      p = &symbol_table[entry.r_symndx];
      name = SYM_NAME(p);
      tab.n_symbols++;
      addr = entry.r_vaddr - 2;
      fwrite(&addr, sizeof(int), 1, symout);
      while (tab.tot_leng++, *name)
	putc(*name++, symout);
      putc('\0', symout);
    }
    fclose(fp);
  }
#endif apollo
  fseek(symout, 0, 0);
  fwrite(&tab, sizeof(tab), 1, symout);
  fclose(symout);
}
