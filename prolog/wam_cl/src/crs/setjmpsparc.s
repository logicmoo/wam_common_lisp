.text
        .align 4
        .global __setjmp
        .proc   04
__setjmp:
	clr  [ %o0 ]
	st  %sp, [ %o0 + 4 ]
	add  8, %o7, %o1
	st  %o1, [ %o0 + 8 ]
	st  %fp, [ %o0 + 0xc ]
	st  %i7, [ %o0 + 0x10 ]
	retl 
	mov  %g0, %o0
        .align 4
        .global __longjmp
        .proc   04
__longjmp: ta  3
	ld  [ %o0 + 4 ], %o2
	ldd  [ %o2 ], %l0
	ldd  [ %o2 + 8 ], %l2
	ldd  [ %o2 + 0x10 ], %l4
	ldd  [ %o2 + 0x18 ], %l6
	ldd  [ %o2 + 0x20 ], %i0
	ldd  [ %o2 + 0x28 ], %i2
	ldd  [ %o2 + 0x30 ], %i4
	ld  [ %o0 + 0xc ], %fp
	mov  %o2, %sp
	ld  [ %o0 + 0x10 ], %i7
	ld  [ %o0 + 8 ], %o3
	tst  %o1
	bne  L1
	sub  %o3, 8, %o7
	mov  1, %o1
L1:     retl 
	mov  %o1, %o0
