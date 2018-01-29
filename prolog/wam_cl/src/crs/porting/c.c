
/* Dump ECOFF-MIPS file */

#include <stdio.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <reloc.h>
#include <storclass.h>

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

main(int argc, char *argv[])
{
	int i, n;
	FILE *fp, *out;

	FILHDR fileheader;
	AOUTHDR unix_header;
	char buf[512];

	fp = fopen(argv[1], "r");
	out = fopen(argv[2], "w");

	fread((char *)&fileheader, sizeof(FILHDR), 1, fp);
	fwrite((char *)&fileheader, sizeof(FILHDR), 1, out);
	printf("ptr: %d\n", ftell(fp));

	fread(&unix_header, sizeof(AOUTHDR), 1, fp);
	fwrite(&unix_header, sizeof(AOUTHDR), 1, out);
	printf("ptr: %d\n", ftell(fp));
/*
	for (i = 0; i < fileheader.f_nscns; i++) {
	  fread(&section[i], sizeof(SCNHDR), 1, fp);
	  fwrite(&section[i], sizeof(SCNHDR), 1, out);
	}
*/
	printf("symptr: %d, ptr: %d\n", fileheader.f_symptr, ftell(fp));
	filecpy(out, fp, fileheader.f_symptr - ftell(fp));
	while ((n = read(_fileno(fp), buf, sizeof buf)) > 0) {
	  write(_fileno(out), buf, n);
	  printf("in: %d, out: %d\n", ftell(fp), ftell(out));
	}
}
