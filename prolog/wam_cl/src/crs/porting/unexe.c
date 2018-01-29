int initflag = 0;

#include <stdio.h>
#include <sys/file.h>

int global_array[710]; /* 709 si, 710 no! */

#define ALLOCATE_INCREMENTALLY
#define COFF

#ifdef COFF
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#define a_text		tsize
#define a_data		dsize
#define a_bss		bsize
#endif

char *self, *core_end;
extern char *sbrk();
extern memory_save();
int gee;

main(argc, argv)
int argc; char ** argv;
{ char *core_init;
  if (!initflag) {
    self = argv[0];
    initflag = 1;
    gee = 1;
    printf("brk(0): %d\n", sbrk(0));
    printf("data begin: %d\n", &initflag);
    core_init = sbrk(2000);
    brk(core_init + 500);
    core_end = sbrk(0);
    printf ("core_init: %d, core_end: %d, sbrk: %d\n",
	    core_init, core_end, sbrk(0));
    memory_save(self,"/tmp/foo");
  } else {
    printf ("end: %d, sbrk(0): %d, sbrk(0): %d\n", core_end, sbrk(0), sbrk(0));
    printf ("gee: %d\n", gee);
  }
}


filecpy(to, from, n)
FILE *to, *from;
register int n;
{
	char buffer[BUFSIZ];

	for (;;)
		if (n > BUFSIZ) {
			fread(buffer, BUFSIZ, 1, from);
			fwrite(buffer, BUFSIZ, 1, to);
			n -= BUFSIZ;
		} else if (n > 0) {
			fread(buffer, 1, n, from);
			fwrite(buffer, 1, n, to);
			break;
		} else
			break;
}

memory_save(original_file, save_file)
char *original_file, *save_file;
{

#ifdef COFF
	int stsize;
	struct filehdr fileheader;
	struct aouthdr header;
	struct scnhdr sectionheader;
	long diff, text_scnptr;
#endif COFF
#ifdef E15
	struct exec header;
#endif

	char *data_begin, *data_end;
	int original_data;
	FILE *original, *save, *standard_error;
	register int n;
	register char *p;
	extern char *sbrk();
	char stdin_buf[BUFSIZ], stdout_buf[BUFSIZ];

	original = fopen(original_file, "r");
	if (original == NULL) {
		fprintf(stderr, "Can't open the original file.\n");
		exit(1);
	}
	setbuf(original, stdin_buf);
#ifndef apollo
	/* this is a trick since we dont know how to create a file whose
	   type is coff (rather then unstruct) */
	unlink(save_file);
#endif apollo
	n = open(save_file, O_CREAT|O_WRONLY, 0777);
	if ((save = fdopen(n, "w")) == NULL) {
		fprintf(stderr, "Can't open the save file.\n");
		exit(1);
	}
	setbuf(save, stdout_buf);
	n = dup(n);
	if ((standard_error = fdopen(n, "w")) == NULL) {
		fprintf(stderr, "Can't open stderr.\n");
		exit(1);
	}
	setbuf(standard_error, NULL);

#ifdef COFF
	n = 0;	/* section count */
	fread(&fileheader, sizeof(fileheader), 1, original);
	fread(&header, sizeof(header), 1, original);
	data_begin = (char *)header.data_start;
#ifdef apollo
	data_begin += FILHSZ + sizeof(header)
		      + fileheader.f_nscns * sizeof(struct scnhdr);
#endif apollo
	data_end = core_end;
	original_data = header.a_data;
	header.a_data = data_end - data_begin;
	diff = header.a_data - original_data;
#ifdef ALLOCATE_INCREMENTALLY
	header.a_bss = 0;
#else
	header.a_bss = sbrk(0) - core_end;
#endif
	fileheader.f_symptr += diff;
	fwrite(&fileheader, sizeof(fileheader), 1, save);
#ifdef apollo
	header.o_sri += diff;
	header.o_inlib += diff;
#endif apollo
	fwrite(&header, sizeof(header), 1, save);
	/* .text */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	text_scnptr = sectionheader.s_scnptr;
	if (sectionheader.s_relptr)
		sectionheader.s_relptr += diff;
	if (sectionheader.s_lnnoptr)
		sectionheader.s_lnnoptr += diff;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
#ifdef apollo
	/* .unwind */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	if (sectionheader.s_relptr)
		sectionheader.s_relptr += diff;
	if (sectionheader.s_lnnoptr)
		sectionheader.s_lnnoptr += diff;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	/* .aptv */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	if (sectionheader.s_relptr)
		sectionheader.s_relptr += diff;
	if (sectionheader.s_lnnoptr)
		sectionheader.s_lnnoptr += diff;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
#endif apollo
	/* .data */
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
	if (sectionheader.s_relptr)
		sectionheader.s_relptr += diff;
	if (sectionheader.s_lnnoptr)
		sectionheader.s_lnnoptr += diff;
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	/* .bss */
	fread(&sectionheader, sizeof(sectionheader), 1, original);
	n++;
	sectionheader.s_size = header.a_bss;
        if (sectionheader.s_scnptr)
                sectionheader.s_scnptr += diff;
	if (sectionheader.s_lnnoptr)
		sectionheader.s_lnnoptr += diff;
#ifdef SUN386
        if (sectionheader.s_paddr)
                sectionheader.s_paddr += diff;
        if (sectionheader.s_vaddr)
                sectionheader.s_vaddr += diff;
#endif SUN386
	fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	for (;  n < fileheader.f_nscns;  n++) {
		fread(&sectionheader, sizeof(sectionheader), 1, original);
		if (sectionheader.s_scnptr)
			sectionheader.s_scnptr += diff;
		if (sectionheader.s_relptr)
			sectionheader.s_relptr += diff;
		if (sectionheader.s_lnnoptr)
			sectionheader.s_lnnoptr += diff;
		fwrite(&sectionheader, sizeof(sectionheader), 1, save);
	}
	filecpy(save, original, header.a_text);
#endif COFF

#ifdef E15
	fread(&header, sizeof(header), 1, original);
	if (header.fmagic != NMAGIC)
		data_begin
		= (char *)(TXTRELOC+header.a_text);
	else
		data_begin
		= (char *)((TXTRELOC+header.a_text+(SEGSIZ-1)) & ~(SEGSIZ-1));
	data_end = core_end;
	original_data = header.a_data;
	header.a_data = data_end - data_begin;
#ifdef ALLOCATE_INCREMENTALLY
	header.a_bss = 0;
#else
	header.a_bss = sbrk(0) - core_end;
#endif
	fwrite(&header, sizeof(header), 1, save);
	filecpy(save, original, header.a_text);
#endif E15

	for (n = header.a_data, p = data_begin;  ;  n -= BUFSIZ, p += BUFSIZ)
		if (n > BUFSIZ)
			fwrite(p, BUFSIZ, 1, save);
		else if (n > 0) {
			fwrite(p, 1, n, save);
			break;
		} else
			break;

	fseek(original, original_data, 1);

#if defined(BSD) && !defined(COFF)
	filecpy(save, original, header.a_syms+header.a_trsize+header.a_drsize);
	fread(&stsize, sizeof(stsize), 1, original);
	fwrite(&stsize, sizeof(stsize), 1, save);
	filecpy(save, original, stsize - sizeof(stsize));
#endif

#ifdef COFF
	filecpy(save, original,
		fileheader.f_symptr - text_scnptr -
		header.a_text - header.a_data);
	filecpy(save, original, SYMESZ*fileheader.f_nsyms);
	fread(&stsize, sizeof(stsize), 1, original);
	fwrite(&stsize, sizeof(stsize), 1, save);
	filecpy(save, original, stsize - sizeof(stsize));
#endif

#ifdef E15
	filecpy(save, original, header.a_syms+header.a_trsize+header.a_drsize);
#endif

	fclose(original);
	fclose(save);

}

