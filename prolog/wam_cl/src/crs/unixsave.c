/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	unixsave.c
*/

#include "config.h"
#include "objff.h"
#include <sys/file.h>

#ifdef COFF
#define a_text		tsize
#define a_data		dsize
#define a_bss		bsize
#endif


#ifdef apollo
#define SEGSIZ		65536
#endif

#ifdef IBMRT
#define	PAGSIZ		2048
#define	SEGSIZ		2048
#define	TXTRELOC	0
#endif

#ifdef __linux__
#define	PAGSIZ		1024
#define SEGSIZ		1024
#define TXTRELOC	0
#endif

#ifdef NEWS
#define	TXTRELOC	0
#endif

#ifdef SEQ
#define SEGSIZ		2048
#define TXTRELOC	0
#endif

#ifdef TAHOE
#define PAGSIZ          1024
#define SEGSIZ          1024
#define TXTRELOC        0
#endif

#ifdef VAX
#define	PAGSIZ		1024
#define	SEGSIZ		1024
#define	TXTRELOC	0
#endif

filecpy(FILE *to, FILE *from, register int n)
{
  char buffer[BUFSIZ];

  while (n > BUFSIZ) {
    fread(buffer, BUFSIZ, 1, from);
    fwrite(buffer, BUFSIZ, 1, to);
    n -= BUFSIZ;
  }
  if (n > 0) {
    fread(buffer, 1, n, from);
    fwrite(buffer, 1, n, to);
  }
}

#define ADJUST(field) if (field) field += diff

unexec(char *save_file, char *original_file,
       unsigned data_start, unsigned bss_start, unsigned entry_address)
{
#ifdef AOUT
	struct exec header;
#endif AOUT
#ifdef COFF
	FILHDR fileheader;
	AOUTHDR header;
	SCNHDR sectionheader;
	long diff, text_scnptr;
#endif COFF

	char *data_begin;
	int text_size, data_size, up_to_strings, str_size;
	FILE *original, *save, *standard_error;
	register int n;
	register char *p;
	char buf[BUFSIZ];

	extern VOID *sbrk();
	extern char stdin_buf[BUFSIZ], stdout_buf[BUFSIZ];

	printf("\nDumping image to file: %s\n", save_file); fflush(stdout);

#ifndef DEBUG
	_cleanup();
#endif DEBUG

	original = freopen(original_file, "r", stdin);
	if (stdin != original || original->_FILE != 0) {
		fprintf(stderr, "Can't open the original file.\n");
		exit(1);
	}
	setbuf(original, stdin_buf);

#ifndef apollo
	/* not unlinking the previous executable is a trick since I dont know
	   how to create a file whose type is COFF (rather then unstruct) */
	unlink(save_file);
#endif apollo

	n = open(save_file, O_CREAT | O_WRONLY, 0777);
#if defined(DEBUG) || defined(__linux__)
	save = fdopen(n, "w");
#else
	if ( n != 1 || (save = fdopen(n, "w")) != stdout) {
		fprintf(stderr, "Can't open the save file.\n");
		exit(1);
	}
	setbuf(save, stdout_buf);

	n = dup(n);
	if (n != 2 || (standard_error = fdopen(n, "w")) != stderr) {
		fprintf(stderr, "Can't open stderr.\n");
		exit(1);
	}
	setbuf(standard_error, NULL);
#endif DEBUG

/* ---------------------------------------------------------------------- */
#ifdef AOUT
/* ---------------------------------------------------------------------- */
	n = 3;	/* section count */
	fread(&header, sizeof(header), 1, original);

#if defined(VAX) || defined(NEWS) || defined(SEQ) || defined(TAHOE)
	data_begin
	= (char *)((TXTRELOC + header.a_text+(SEGSIZ-1)) & ~(SEGSIZ-1));
#endif
#if defined(sun) || defined(MSDOS) || defined(__linux__)
	data_begin = (char *)N_DATADDR(header);
#endif sun
#ifdef IBMRT
	data_begin = (char *)(TXTRELOC + header.a_text);
#endif IBMRT
#ifdef hp9000s300
        data_begin
        = (char *) ((header.a_magic.file_type == SHARE_MAGIC ||
		     header.a_magic.file_type == DEMAND_MAGIC) ?
		    EXEC_ALIGN(header.a_text) :
		    header.a_text);
#endif hp9000s300

	text_size = N_DATOFF(header) - N_TXTOFF(header);
	data_size = header.a_data;
/*
	up_to_strings = N_STROFF(header) - N_DATOFF(header) - data_size;
*/
	/* The file generated will have:
	   1. updated header;
	   2. same text section as original;
	   3. data section dumped from memory;
	   4. bss section empty;
	   5. syms, trel and drel sections as original;
	   6. strings as original.
	 */
	/* Update header before writing */
	header.a_data = data_end - data_begin;
#ifdef __linux__
	header.a_data += (PAGE_SIZE - header.a_data & (PAGE_SIZE-1));
#endif
	header.a_bss = 0;
	fwrite(&header, sizeof(header), 1, save);

#if defined(VAX) || defined(NEWS) || defined(TAHOE) || defined(IBMRT) || defined(__linux__)
	if (N_MAGIC(header) == ZMAGIC)
          filecpy(save, original, PAGSIZ - sizeof(header));
	filecpy(save, original, header.a_text);
#endif
#ifdef MSDOS
	filecpy(save, original, text_size);
#endif MSDOS
#ifdef sun
	filecpy(save, original, header.a_text - sizeof(header));
#endif sun
#ifdef SEQ
	filecpy(save, original, header.a_text - 
				N_ADDRADJ(header) - sizeof(header));
#endif SEQ
#ifdef hp9000s300
	if (header.a_magic.file_type == DEMAND_MAGIC) {
	  filecpy(save, original, EXEC_PAGESIZE - sizeof(header));
	  filecpy(save, original, EXEC_ALIGN(header.a_text));}
        else
	  filecpy(save, original, header.a_text);
#endif hp9000s300
#endif AOUT

/* ---------------------------------------------------------------------- */
#ifdef COFF
/* ---------------------------------------------------------------------- */
	n = 0;	/* section count */
	fread(&fileheader, sizeof(fileheader), 1, original);
	fread(&header, sizeof(header), 1, original);

	data_begin = (char *)header.data_start;
#ifdef apollo
	data_begin += FILHSZ + sizeof(header)
		      + fileheader.f_nscns * sizeof(struct scnhdr);
#endif apollo
	data_size = header.a_data;
#ifdef TEST
	header.a_data = data_end - data_begin;
#endif TEST
	diff = header.a_data - data_size;
	fileheader.f_symptr += diff;

	fwrite(&fileheader, sizeof(fileheader), 1, save);
#ifdef TEST

	header.a_bss = 0;
#ifdef apollo
	header.o_sri += diff;
	header.o_inlib += diff;
#endif apollo
#ifdef __mips
	header.bss_start = header.data_start + header.a_data;
	/* tsize includes headers which are also loaded into memory */
#endif __mips
#endif TEST
	fwrite(&header, sizeof(header), 1, save);

	/* .text */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	text_scnptr = sectionheader.s_scnptr;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

#ifdef apollo

	/* .unwind */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .aptv */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
#endif apollo

#ifdef ECOFF

	/* .init */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .data */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* .rdata */
#else
	/* .data */
#endif ECOFF
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	sectionheader.s_size += diff;
#ifdef apollo
	/* the APTV at the beginning of data section has already
	   been relocated at first initialization.
	   Avoid doing it again.
	 */
	sectionheader.s_nreloc = 0;
#endif apollo
	ADJUST(sectionheader.s_relptr);
	ADJUST(sectionheader.s_lnnoptr);
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);

	/* copy all remaining section headers */
	for (;  n < fileheader.f_nscns;  n++) {
	  fread(&sectionheader, sizeof(sectionheader), 1, original);
	  if (strcmp(sectionheader.s_name, ".bss") == 0) {
	    sectionheader.s_size = header.a_bss;
	    /* added for the SUN386, but harmless otherwise: */
	    ADJUST(sectionheader.s_paddr);
	    ADJUST(sectionheader.s_vaddr);
	  }
	  ADJUST(sectionheader.s_scnptr);
	  ADJUST(sectionheader.s_relptr);
	  ADJUST(sectionheader.s_lnnoptr);
	  fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	}

	/* copy the text section */
	filecpy(save, original, header.a_text);

#endif COFF

/* ---------------------------------------------------------------------- */

	/* write the new data section */

	for (n = header.a_data, p = data_begin; (n > BUFSIZ) ;
	     n -= BUFSIZ, p += BUFSIZ)
	  fwrite(p, BUFSIZ, 1, save);
	if (n > 0)
	  fwrite(p, 1, n, save);

	/*  skip data section of original file  */
	fseek(original, data_size, 1);

#ifdef hpux
	fseek(save, MODCAL_OFFSET(header), 0);
	header.a_data = data_size;
	fseek(original, MODCAL_OFFSET(header), 0);
#endif hpux

#ifdef ECOFF
	{  HDRR symhdr; 

	   /* copy up to Symbol Table */
	   filecpy(save, original, N_SYMOFF(fileheader) - ftell(original));
	   /* update Symbol Table Header */
	   fread(&symhdr, cbHDRR, 1, original);
	   ADJUST(symhdr.cbLineOffset);
	   ADJUST(symhdr.cbDnOffset);
	   ADJUST(symhdr.cbPdOffset);
	   ADJUST(symhdr.cbSymOffset);
	   ADJUST(symhdr.cbOptOffset);
	   ADJUST(symhdr.cbAuxOffset);
	   ADJUST(symhdr.cbSsOffset);
	   ADJUST(symhdr.cbSsExtOffset);
	   ADJUST(symhdr.cbFdOffset);
	   ADJUST(symhdr.cbRfdOffset);
	   ADJUST(symhdr.cbExtOffset);

	   fwrite(&symhdr, cbHDRR, 1, save);
	 }
#endif ECOFF

	/* Copy the rest */
	filecpy(save, original, file_len(original) - ftell(original));
/*	{  int in = _fileno(original), out = _fileno(save);
	   while ((n = read(in, buf, sizeof buf)) > 0)
	     write(out, buf, n);
	 }
*/
	fclose(original);
	fclose(save);
}
