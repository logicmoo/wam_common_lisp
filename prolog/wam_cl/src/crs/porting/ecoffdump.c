
/* Dump ECOFF-MIPS file */

#include <stdio.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <reloc.h>
#include <storclass.h>

main(int argc, char *argv[])
{
	int i, j;
	char *s;
	FILE *fp;

	FILHDR fileheader;
	AOUTHDR unix_header;
        SCNHDR section[10];
	HDRR symheader;
	char *string_table;

	EXTR symbol;
	RELOC reloc;

	printf("sizeof(RELOC) = %d\n\n", sizeof(RELOC));
	printf("sizeof(EXTR) = %d\n\n", sizeof(EXTR));

	/* On MIPS the file headers are present also in the memory
	   at address 0x400000 (start of text) */

	fp = fopen(argv[1], "r");
	fread((char *)&fileheader, sizeof(FILHDR), 1, fp);

	printf("f_magic:        0x%x\n", fileheader.f_magic);
	printf("f_nscns:	%d\n", fileheader.f_nscns);
	printf("f_timdat:	%d\n", fileheader.f_timdat);
	printf("f_symptr:	%d\n", fileheader.f_symptr);
	printf("f_nsyms:	%d\n", fileheader.f_nsyms);
	printf("f_opthdr:	%d\n", fileheader.f_opthdr);
	printf("f_flags:	0x%x\n", fileheader.f_flags);

	if (fileheader.f_opthdr) {

	  fread(&unix_header, sizeof(AOUTHDR), 1, fp);

	  printf("\nmagic:		0%o\n", unix_header.magic);
	  printf("vstamp:		0%o\n", unix_header.vstamp);
	  printf("tsize:		%d\n", unix_header.tsize);
	  printf("dsize:		%d\n", unix_header.dsize);
	  printf("bsize:		%d\n", unix_header.bsize);
	  printf("entry:		%d\n", unix_header.entry);
	  printf("text_start:	%d\n", unix_header.text_start);
	  printf("data_start:	%d\n", unix_header.data_start);
#if __mips
	  printf("bss_start:	%d\n", unix_header.bss_start);
#endif
	}

	for (i = 0; i < fileheader.f_nscns; i++) {
	  fread(&section[i], sizeof(SCNHDR), 1, fp);
	  printf("\nsection[%d]:\n", i);
	  printf("s_name: %s\n", section[i].s_name);
	  printf("s_paddr:\t%d\n", section[i].s_paddr);
	  printf("s_vaddr:\t%d\n", section[i].s_vaddr);
	  printf("s_size:\t\t%d\n", section[i].s_size);
	  printf("s_scnptr:\t%d\n", section[i].s_scnptr);
	  printf("s_relptr:\t%d\n", section[i].s_relptr);
	  printf("s_lnnoptr:\t%d\n", section[i].s_lnnoptr);
	  printf("s_nreloc:\t%d\n", section[i].s_nreloc);
	  printf("s_nlnno:\t%d\n", section[i].s_nlnno);
	  printf("s_flags:\t0%o\n", section[i].s_flags);
	}

	/* Print reloc information */
	for (j = 0; j < fileheader.f_nscns; j++) {
	  if (section[j].s_nreloc == 0) continue;
	  fseek(fp, section[j].s_relptr, 0);
	  printf("\nreloc info for %s:\n", section[j].s_name);
	  printf("\tr_vaddr  r_symndx  r_type\n\n");
	  for (i = 0; i < section[j].s_nreloc; i++)	{
	    if (fread(&reloc, sizeof(reloc), 1, fp) != 1)
	      fprintf(stderr, "READ ERROR\n");
	    printf("%15x  %8d  ", reloc.r_vaddr, reloc.r_symndx);
	    switch (reloc.r_type)	{
	    case R_ABS:
	      printf("R_ABS\n");
	      break;
#ifdef __mips
	    case R_REFHALF:
	      printf("R_REFHALF\n");
	      break;
	    case R_REFWORD:
	      printf("R_REFWORD\n");
	      break;
	    case R_JMPADDR:	/* 26-bit jump reference */
	      printf("R_JMPADDR\n");
	      break;
	    case R_REFHI:
	      printf("R_REFHI\n");
	      break;
	    case R_REFLO:
	      printf("R_REFLO\n");
	      break;
	    case R_GPREL:
	      printf("R_GPREL\n");
	      break;
	    case R_LITERAL:
	      printf("R_LITERAL\n");
	      break;
	    case R_REL32:
	      printf("R_REL32\n");
	      break;
	    case R_REFHI_64:
	      printf("R_REFHI_64\n");
	      break;
	    case R_REFLO_64:
	      printf("R_REFLO_64\n");
	      break;
	    case R_REFWORD_64:
	      printf("R_REFWORD_64\n");
	      break;
	    case R_PC16:
	      printf("R_PC16\n");
	      break;
#ifdef __osf__
	    case R_RELHI:
	      printf("R_RELHI\n");
	      break;
	    case R_RELLO:
	      printf("R_RELLO\n");
	      break;
#endif				/* __osf__ */
	    case R_REFSHFT:
	      printf("R_REFSHFT\n");
	      break;
	    case R_REFHI_ADDEND:
	      printf("R_REFHI_ADDEND\n");
	      break;
#else
	    case R_DIR16:
	      printf("R_DIR16\n");
	      break;
	    case R_REL16:
	      printf("R_REL16\n");
	      break;
	    case R_IND16:
	      printf("R_IND16\n");
	      break;
	    case R_DIR24:
	      printf("R_DIR24\n");
	      break;
	    case R_REL24:
	      printf("R_REL24\n");
	      break;
	    case R_DIR32:
	      printf("R_DIR32\n");
	      break;
	    case R_OFF8:
	      printf("R_OFF8\n");
	      break;
	    case R_OFF16:
	      printf("R_OFF16\n");
	      break;
	    case R_SEG12:
	      printf("R_SEG12\n");
	      break;
	    case R_DIR32S:
	      printf("R_DIR32S\n");
	      break;
	    case R_AUX:
	      printf("R_AUX\n");
	      break;
	    case R_OPT16:
	      printf("R_OPT16\n");
	      break;
	    case R_IND24:
	      printf("R_IND24\n");
	      break;
	    case R_IND32:
	      printf("R_IND32\n");
	      break;
	    case R_RELBYTE:
	      printf("R_RELBYTE\n");
	      break;
	    case R_RELWORD:
	      printf("R_RELWORD\n");
	      break;
	    case R_RELLONG:
	      printf("R_RELLONG\n");
	      break;
	    case R_PCRBYTE:
	      printf("R_PCRBYTE\n");
	      break;
	    case R_PCRWORD:
	      printf("R_PCRWORD\n");
	      break;
	    case R_PCRLONG:
	      printf("R_PCRLONG\n");
	      break;
#endif mips

	    default:
	      printf("%d\n", reloc.r_type);

	    }
	  }
	}
	
	fseek(fp, fileheader.f_symptr, 0);
	fread(&symheader, sizeof(HDRR), 1, fp);
	printf("\nSymbolic header, magic number = 0x%x, vstamp = %d:\n",
	       symheader.magic, symheader.vstamp);
	printf("\tInfo\t\t\tOffset\tNumber\tBytes\n\n");
	printf("\t%s\t\t%d\t%d\t%d\n", "Line numbers",
	       symheader.cbLineOffset, symheader.ilineMax, symheader.cbLine);
#define SIZE(x, y)	((y) ? (x)-(y) : 0)
	printf("\t%s\t\t%d\t%d\t%d\n", "Dense numbers",
	       symheader.cbDnOffset, symheader.idnMax,
	       SIZE(symheader.cbPdOffset,symheader.cbDnOffset));
	printf("\t%s\t%d\t%d\t%d\n", "Procedure Tables",
	       symheader.cbPdOffset, symheader.ipdMax,
	       SIZE(symheader.cbSymOffset,symheader.cbPdOffset));
	printf("\t%s\t\t%d\t%d\t%d\n", "Local Symbols",
	       symheader.cbSymOffset, symheader.isymMax,
	       SIZE((symheader.cbOptOffset ?
		     symheader.cbOptOffset : symheader.cbAuxOffset),
		    symheader.cbSymOffset));
	printf("\t%s\t%d\t%d\t%d\n", "Optimization Symbols",
	       symheader.cbOptOffset, symheader.ioptMax,
	       SIZE(symheader.cbAuxOffset,symheader.cbOptOffset));
	printf("\t%s\t%d\t%d\t%d\n", "Auxiliary Symbols",
	       symheader.cbAuxOffset, symheader.iauxMax,
	       SIZE(symheader.cbSsOffset,symheader.cbAuxOffset));
	printf("\t%s\t\t%d\t%d\t%d\n", "Local Strings",
	       symheader.cbSsOffset, symheader.issMax,
	       SIZE(symheader.cbExtOffset,symheader.cbSsOffset));
	printf("\t%s\t%d\t%d\t%d\n", "External Strings",
	       symheader.cbSsExtOffset, symheader.issExtMax,
	       SIZE(symheader.cbFdOffset,symheader.cbSsExtOffset));
	printf("\t%s\t\t%d\t%d\t%d\n", "File Tables",
	       symheader.cbFdOffset, symheader.ifdMax,
	       SIZE(symheader.cbRfdOffset,symheader.cbFdOffset));
	printf("\t%s\t\t%d\t%d\t%d\n", "Relative Files",
	       symheader.cbRfdOffset, symheader.crfd,
	       SIZE(symheader.cbExtOffset,symheader.cbRfdOffset));
	printf("\t%s\t%d\t%d\t%d\n", "External Symbols",
	       symheader.cbExtOffset, symheader.iextMax,
	       sizeof(EXTR)*symheader.iextMax);

	/* Read External Strings */
	fseek(fp, symheader.cbSsExtOffset, 0);
	i = symheader.cbFdOffset - symheader.cbSsExtOffset;
	string_table = (char *)malloc(i);
	fread(string_table, i, 1, fp);

	printf("\nThere are %d external symbols, starting at %d\n\n",
	       symheader.iextMax, symheader.cbExtOffset);

	fseek(fp, symheader.cbExtOffset, 0);
	for (i = 0; i < symheader.iextMax; i++)	{
	  SYMR sym;
	  fread(&symbol, sizeof(symbol), 1, fp);
	  sym = symbol.asym;
	  printf("[%3d]: value %8x st%4d sc%4d index%6x	%s\n",
		 i, sym.value, sym.st, sym.sc, sym.index,
		 string_table + sym.iss);
	}
}
