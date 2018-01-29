#include <stdio.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <reloc.h>
#include <syms.h>

int a[100];

main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	struct scnhdr section;
	int tsize, dsize, bsize, text_start;
	char *text, *malloc();
	char command[128];

	fp = fopen(argv[1], "r");
	fseek(fp, sizeof(struct filehdr), 0);

	fread(&section, sizeof(struct scnhdr), 1, fp);
	tsize = section.s_size;
	text_start = section.s_scnptr;
	fread(&section, sizeof(struct scnhdr), 1, fp);
	dsize = section.s_size;
	fread(&section, sizeof(struct scnhdr), 1, fp);
	bsize - section.s_size;
	fclose(fp);

	printf("size: %d+%d+%d = %d\n", tsize, dsize, bsize,
		tsize + dsize + bsize);
	
	text = malloc(tsize + dsize + bsize);

	sprintf(command, "ild %s %d %s tmp", argv[0], (int)text, argv[1]);
	printf("%s\n", command);
	if (system(command) != 0)	{
		fprintf(stderr, "Can't relocate.\n");
		exit(1);
	}

	fp = fopen("tmp", "r");
	fseek(fp, text_start, 0);
	fread(text, 1, tsize + dsize, fp);
	fclose(fp);

	a[10] = 123;

	(*(int (*)())text)();
}

dummy()
{
	printf("What?\n");
}
