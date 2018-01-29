
/* Dump COFF file */

#include <stdio.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <reloc.h>
#include <ldfcn.h>

#ifdef __sgi
#define ECOFF
#endif

#ifdef ECOFF
#define SYMENT SYMR
#define SYM_VALUE(sym)	(sym).value
#else
#define SYM_VALUE(sym)	(sym).n_value
#endif

main(int argc, char *argv[])
{
	int i, j;
	char *s;
	char *string_table;

	LDFILE *ldptr;
	struct aouthdr unix_header;
	SCNHDR secthead1;
	SCNHDR secthead2;
	SCNHDR secthead3;
	SCNHDR secthead4;
	SCNHDR sectheadn;
	SYMENT symbol;
	struct reloc reloc;

	printf("sizeof(struct reloc) = %d\n\n", sizeof(struct reloc));
	printf("sizeof(SYMENT) = %d\n\n", sizeof(SYMENT));

	ldptr = ldopen(argv[1], NULL);

	printf("f_magic:        %o\n", HEADER(ldptr).f_magic);
	printf("f_nscns:	%d\n", HEADER(ldptr).f_nscns);
	printf("f_timdat:	%d\n", HEADER(ldptr).f_timdat);
	printf("f_symptr:	%d\n", HEADER(ldptr).f_symptr);
	printf("f_nsyms:	%d\n", HEADER(ldptr).f_nsyms);
	printf("f_opthdr:	%d\n", HEADER(ldptr).f_opthdr);
	printf("f_flags:	%o\n", HEADER(ldptr).f_flags);

	if(HEADER(ldptr).f_opthdr) {

	  FSEEK(ldptr, sizeof(struct filehdr), 0);
	  FREAD((char *)&unix_header, sizeof(struct aouthdr), 1, ldptr);

#ifdef apollo
	  printf("\nvformat:		%o\n", unix_header.vformat);
#else
	  printf("\nmagic:		%o\n", unix_header.magic);
#endif /* apollo */
	  printf("vstamp:		%o\n", unix_header.vstamp);
	  printf("tsize:		%d\n", unix_header.tsize);
	  printf("dsize:		%d\n", unix_header.dsize);
	  printf("bsize:		%d\n", unix_header.bsize);
	  printf("entry:		%d\n", unix_header.entry);
	  printf("text_start:	%d\n", unix_header.text_start);
	  printf("data_start:	%d\n", unix_header.data_start);
#ifdef apollo
	  printf("o_sri:		%d\n", unix_header.o_sri);
	  printf("o_inlib:	%d\n", unix_header.o_inlib);
	  printf("vid[0]: %d, vid[1]: %d\n", unix_header.vid[0], unix_header.vid[1]);
#endif /* apollo */
	}

	ldshread(ldptr, 1, &secthead1);

	printf("\nsecthead1:\n");
	printf("s_name: %s\n", secthead1.s_name);
	printf("s_paddr:	%d\n", secthead1.s_paddr);
	printf("s_vaddr:	%d\n", secthead1.s_vaddr);
	printf("s_size:		%d\n", secthead1.s_size);
	printf("s_scnptr:	%d\n", secthead1.s_scnptr);
	printf("s_relptr:	%d\n", secthead1.s_relptr);
	printf("s_lnnoptr:	%d\n", secthead1.s_lnnoptr);
	printf("s_nreloc:	%d\n", secthead1.s_nreloc);
	printf("s_nlnno:	%d\n", secthead1.s_nlnno);
	printf("s_flags:	%o\n", secthead1.s_flags);

	ldshread(ldptr, 2, &secthead2);

	printf("\nsecthead2:\n");
	printf("s_name:	%s\n", secthead2.s_name);
	printf("s_paddr:	%d\n", secthead2.s_paddr);
	printf("s_vaddr:	%d\n", secthead2.s_vaddr);
	printf("s_size:		%d\n", secthead2.s_size);
	printf("s_scnptr:	%d\n", secthead2.s_scnptr);
	printf("s_relptr:	%d\n", secthead2.s_relptr);
	printf("s_lnnoptr:	%d\n", secthead2.s_lnnoptr);
	printf("s_nreloc:	%d\n", secthead2.s_nreloc);
	printf("s_nlnno:	%d\n", secthead2.s_nlnno);
	printf("s_flags:	%o\n", secthead2.s_flags);

	ldshread(ldptr, 3, &secthead3);

	printf("\nsecthead3:\n");
	printf("s_name:	%s\n", secthead3.s_name);
	printf("s_paddr:	%d\n", secthead3.s_paddr);
	printf("s_vaddr:	%d\n", secthead3.s_vaddr);
	printf("s_size:		%d\n", secthead3.s_size);
	printf("s_scnptr:	%d\n", secthead3.s_scnptr);
	printf("s_relptr:	%d\n", secthead3.s_relptr);
	printf("s_lnnoptr:	%d\n", secthead3.s_lnnoptr);
	printf("s_nreloc:	%d\n", secthead3.s_nreloc);
	printf("s_nlnno:	%d\n", secthead3.s_nlnno);
	printf("s_flags:	%o\n", secthead3.s_flags);

	ldshread(ldptr, 4, &secthead4);

	printf("\nsecthead4:\n");
	printf("s_name:	%s\n", secthead4.s_name);
	printf("s_paddr:	%d\n", secthead4.s_paddr);
	printf("s_vaddr:	%d\n", secthead4.s_vaddr);
	printf("s_size:		%d\n", secthead4.s_size);
	printf("s_scnptr:	%d\n", secthead4.s_scnptr);
	printf("s_relptr:	%d\n", secthead4.s_relptr);
	printf("s_lnnoptr:	%d\n", secthead4.s_lnnoptr);
	printf("s_nreloc:	%d\n", secthead4.s_nreloc);
	printf("s_nlnno:	%d\n", secthead4.s_nlnno);
	printf("s_flags:	%o\n", secthead4.s_flags);

#ifdef apollo
	for (i = 5; i <= HEADER(ldptr).f_nscns; i++) {
	ldshread(ldptr, i, &sectheadn);

	printf("\nsectheadn:\n");
	printf("s_name:	%s\n", sectheadn.s_name);
	printf("s_paddr:	%d\n", sectheadn.s_paddr);
	printf("s_vaddr:	%d\n", sectheadn.s_vaddr);
	printf("s_size:		%d\n", sectheadn.s_size);
	printf("s_scnptr:	%d\n", sectheadn.s_scnptr);
	printf("s_relptr:	%d\n", sectheadn.s_relptr);
	printf("s_lnnoptr:	%d\n", sectheadn.s_lnnoptr);
	printf("s_nreloc:	%d\n", sectheadn.s_nreloc);
	printf("s_nlnno:	%d\n", sectheadn.s_nlnno);
	printf("s_flags:	%o\n", sectheadn.s_flags);
		}
#endif /* apollo */

	printf("\nreloc info for 1:\n");
	ldrseek(ldptr, 1);
	for (i = 0; i < secthead1.s_nreloc; i++)	{
		j = fread(&reloc, 10, 1, IOPTR(ldptr));
		if (j != 1)
			fprintf(stderr, "READ ERROR\n");
		printf("\nr_vaddr		%d\n", reloc.r_vaddr);
		printf("r_symndx	%d\n", reloc.r_symndx);
		switch (reloc.r_type)	{
		case R_ABS:
			printf("r_type		R_ABS\n");
			break;
		case R_DIR32:
			printf("r_type		R_DIR32\n");
			break;
		case R_OFF16:
			printf("r_type		R_OFF16\n");
			break;
		case R_DIR32S:
			printf("r_type		R_DIR32S\n");
			break;
		case R_REL24:
			printf("r_type		R_REL24\n");
			break;
		case R_OPT16:
			printf("r_type		R_OPT16\n");
			break;
		case R_IND24:
			printf("r_type		R_IND24\n");
			break;
		case R_IND32:
			printf("r_type		R_IND32\n");
			break;
		default:
			fprintf(stderr, "Illegal r_type:	%o\n",
				reloc.r_type);
			break;
		}
	}

	printf("\nreloc info for 2:\n");
	ldrseek(ldptr, 2);
	for (i = 0; i < secthead2.s_nreloc; i++)	{
		fread(&reloc, sizeof(struct reloc), 1, IOPTR(ldptr));
		printf("\nr_vaddr		%d\n", reloc.r_vaddr);
		printf("r_symndx	%d\n", reloc.r_symndx);
		switch (reloc.r_type)	{
		case R_RELBYTE:
			printf("r_type		R_RELTYPE\n");
			break;
		case R_RELWORD:
			printf("r_type		R_RELWORD\n");
			break;
		case R_RELLONG:
			printf("r_type		R_RELLONG\n");
			break;
		case R_PCRBYTE:
			printf("r_type		R_PCRTYPE\n");
			break;
		case R_PCRWORD:
			printf("r_type		R_PCRWORD\n");
			break;
		case R_PCRLONG:
			printf("r_type		R_PCRLONG\n");
			break;
		default:
			fprintf(stderr, "Illegal r_type:	%d",
				reloc.r_type);
			break;
		}
	}

	/*
	  If the string table is not empty,
	  its length is stored after the symbol table,
	  This is not described in the manual, and may change in the future.
	  */
	FSEEK(ldptr, STROFFSET(ldptr), 0);
	if (FREAD((char *)&i, 4, 1, ldptr) > 0)	{
	  printf("i: %d\n", i);
	  string_table = (char *)malloc(i);
	  FREAD(string_table, 1, i - 4, ldptr);
	}

	printf("\nsymbols:\n");
	for (i = 0; i < HEADER(ldptr).f_nsyms; i++)	{
		ldtbread(ldptr, i, &symbol);
		printf("\nldgetname(%d):	%s\n",
			i, ldgetname(ldptr, &symbol));
#ifdef ECOFF
		printf("[%d] v %x st %d sc %d index %x %s\n",
		       i, symbol.value, symbol.st, symbol.sc, symbol.index,
		       string_table[symbol.iss]);
#else
		printf("n_value:	%o\n", SYM_VALUE(symbol));
		switch (symbol.n_scnum)	{
		case N_UNDEF:
			printf("n_scnum:	N_UNDEF\n");
			break;
		case N_ABS:
			printf("n_scnum:	N_ABS\n");
			break;
		case N_DEBUG:
			printf("n_scnum:	N_DEBUG\n");
			break;
		case N_TV:
			printf("n_scnum:	N_TV\n");
			break;
		case P_TV:
			printf("n_scnum:	P_TV\n");
			break;
		default:
			printf("n_scnum:	%d\n", symbol.n_scnum);
			break;
		}
		printf("n_type:		%d\n", symbol.n_type);
		printf("n_sclass:	%d\n", symbol.n_sclass);
		printf("n_numaux:	%d\n", symbol.n_numaux);
		i += symbol.n_numaux;
#endif /* ECOFF */
	}

      }
