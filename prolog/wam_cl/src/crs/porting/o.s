	.file	"o.c"
	.version	"RR-0.07-Beta"
.LL0:
        .data
        .comm   a,400
        .text
	.align	4
	.globl	extra_lenght_foo
extra_lenght_foo:
	jmp	.L28
.L27:
/     7		printf("Hello.\n");
        .text
	pushl	$.L30
	call	printf
	addl	$4,%esp
/     8		printf("a[10] = %d\n", a[10]);
        .text
	pushl	a+40
	pushl	$.L31
	call	printf
	addl	$8,%esp
/     9	}
.L26:
	leave
	ret
.L28:
	pushl	%ebp
	movl	%esp,%ebp
	jmp	.L27
/FUNCEND
        .data
.L30:

        .byte    0x48,0x65,0x6c,0x6c,0x6f,0x2e,0x0a,0x00
.L31:

        .byte    0x61,0x5b,0x31,0x30,0x5d,0x20,0x3d,0x20,0x25,0x64
        .byte    0x0a,0x00
