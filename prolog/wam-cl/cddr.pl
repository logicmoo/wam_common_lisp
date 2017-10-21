/*******************************************************************
 *
 * A Lisp make_accessor(compiler, written in Prolog
 *
 * (lisp_library.pl)
 *
 * (make_accessor(c) Neil Smith, 2001
 *
 * This program provides some built-in funmake_accessor(ctionality for the 
 * Lisp make_accessor(compiler.  It requires that the file lisp_make_accessor(compiler.pl has 
 * already been sumake_accessor(cmake_accessor(cessfully make_accessor(compiled.
 *
 * Definitions in this file are given in the Lisp-like syntax 
 * read by this make_accessor(compiler.
 *
 *******************************************************************/

make_accessor(cadr).

make_accessor(cdar).

make_accessor(cddr).

make_accessor(caaar).

make_accessor(caadr).

make_accessor(cadar).

make_accessor(caddr).

make_accessor(cdaar).

make_accessor(cdadr).

make_accessor(cddar).

make_accessor(cdddr).

make_accessor(caaaar).

make_accessor(caaadr).

make_accessor(caadar).

make_accessor(caaddr).

make_accessor(cadaar).

make_accessor(cadadr).

make_accessor(caddar).

make_accessor(cadddr).

make_accessor(cdaaar).

make_accessor(cdaadr).

make_accessor(cdadar).

make_accessor(cdaddr).

make_accessor(cddaar).

make_accessor(cddadr).

make_accessor(cdddar).

make_accessor(cddddr).

/*
(caar x)        (car (car x))                    
(cadr x)        (car (cdr x))                    
(cdar x)        (cdr (car x))                    
(cddr x)        (cdr (cdr x))                    
(caaar x)       (car (car (car x)))              
(caadr x)       (car (car (cdr x)))              
(cadar x)       (car (cdr (car x)))              
(caddr x)       (car (cdr (cdr x)))              
(cdaar x)       (cdr (car (car x)))              
(cdadr x)       (cdr (car (cdr x)))              
(cddar x)       (cdr (cdr (car x)))              
(cdddr x)       (cdr (cdr (cdr x)))              
(caaaar x)      (car (car (car (car x))))        
(caaadr x)      (car (car (car (cdr x))))        
(caadar x)      (car (car (cdr (car x))))        
(caaddr x)      (car (car (cdr (cdr x))))        
(cadaar x)      (car (cdr (car (car x))))        
(cadadr x)      (car (cdr (car (cdr x))))        
(caddar x)      (car (cdr (cdr (car x))))        
(cadddr x)      (car (cdr (cdr (cdr x))))        
(cdaaar x)      (cdr (car (car (car x))))        
(cdaadr x)      (cdr (car (car (cdr x))))        
(cdadar x)      (cdr (car (cdr (car x))))        
(cdaddr x)      (cdr (car (cdr (cdr x))))        
(cddaar x)      (cdr (cdr (car (car x))))        
(cddadr x)      (cdr (cdr (car (cdr x))))        
(cdddar x)      (cdr (cdr (cdr (car x))))        
(cddddr x)      (cdr (cdr (cdr (cdr x))))  
*/
