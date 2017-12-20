
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "loop" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:41 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; CHANGES", 1, 1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:12 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; 19990429: Made this not a package. (erik)",
				     1,
				     13)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:58 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; 19990506: I notice this is quite buggy: (1) if the YUNTIL clause contains",
				     1,
				     59)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:136 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; the YFOR loop variable, it takes on the value NIL, (2) the YUNTIL clause",
				     1,
				     137)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:213 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; cannot be placed before the YFOR clause. (erik)",
				     1,
				     214)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:265 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Version 1.1 released.", 1, 266)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:291 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Tidied up test code, comments cleaned up a bit, provide yloop as feature",
				     1,
				     292)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:368 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;   and package, added version number, fixed ydo to be safer, included",
				     1,
				     369)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:441 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;   hints from Jeni Tennison.  5-16-94 -FER",
				     1,
				     442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:487 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; fixed restrictive yfor i from x to y where x and y were required to be",
				     1,
				     488)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:562 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;   numebrs at load time -fr 2/5/88",
				     1,
				     563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:600 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; made all clause keywords local to yloop package, and updated documentation",
				     1,
				     601)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:679 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;    to reflect this fact.  -fr 10/27/86",
				     1,
				     680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:722 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; end tests also checked in the beginning to catch the case of no iteration",
				     1,
				     723)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:800 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;     needed -fr 10/08/86", 1, 801)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; note that I optimized the code in for so that it didn't eval its arg's ",
				     1,
				     829)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:904 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;    twice -fr 7/19/86", 1, 905)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:929 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; multiple end tests accomidated by use of or on end-tests",
				     1,
				     930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:990 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;   -fr 9/10/86", 1, 991)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1008 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Code originially in  conger:>jim>yale-loop>yale-loop.lisp. ",
				     1,
				     1009)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1072 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Also available via anonymous ftp from ",
				     1,
				     1073)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;  ftp.cs.cmu.edu as ", 1, 1116)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1139 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;    /afs/cs/user/mkant/Public/Lisp/code/iter/loop/yloop/yloop.cl",
				     1,
				     1140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;  and unicorn.ccc.nottingham.ac.uk as",
				     1,
				     1208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1248 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;    pub/lpzfr/yloop.l", 1, 1249)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1273 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;          ", 1, 1274)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1287 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Questions or requests for later versions:",
				     1,
				     1288)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Jim Panagos (jpanagos@world.std.com) or ",
				     1,
				     1334)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Frank.Ritter@nottingham.ac.uk  (or Ritter@cs.cmu.edu) ",
				     1,
				     1379)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1437 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 1438)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1441 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; ", 1, 1442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1446 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; takes the following keywords:",
				     1,
				     1447)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1480 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      YLOOP", 1, 1481)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1495 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      INITIAL    (INITIAL (var1 val1) (var2 val2) ...)",
				     1,
				     1496)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1553 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      BEFORE     (BEFORE (to-do-1) (to-do-2) ...)",
				     1,
				     1554)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1606 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      AFTER      (AFTER  (to-do-1)(to-do-2) ...)",
				     1,
				     1607)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1658 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      YFOR       (YFOR var1 {IN ON FROM} var2 TO var3)",
				     1,
				     1659)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1716 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;                    IN gets cars of the list, ON gets the cdrs",
				     1,
				     1717)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1782 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      YDO        (YDO (to-do-1) (to-do-2) ...)",
				     1,
				     1783)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1832 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      YRESULT    returns the rest of the clause in an implicet progn, or nil",
				     1,
				     1833)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1912 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      NEXT       (NEXT   (var1 (+ var1 1)))",
				     1,
				     1913)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:1959 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      YWHILE     (YWHILE  {var1 (test)} )",
				     1,
				     1960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;      YUNTIL     (YUNTIL  {var1 (test)} )",
				     1,
				     2005)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2049 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      INCR", 1, 2050)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2063 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      DECR", 1, 2064)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2077 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      MAXIMIZE", 1, 2078)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2095 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      SUM", 1, 2096)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2108 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      YWHEN", 1, 2109)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2123 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;      LERROR", 1, 2124)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2139 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 2140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Yale loop macro written in Common Lisp based on the loop construct in ",
				     1,
				     2144)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2218 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; McDermont, Charniak and Riesbeck's AI programming book.",
				     1,
				     2219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2278 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 2279)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2282 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; DESIGN APPROACH", 1, 2283)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2302 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Each loop statement such as before or ywhile are macros themselves. Given",
				     1,
				     2303)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2380 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; their arguments they fill out a template of what should be stored in",
				     1,
				     2381)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2453 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; appropriate positions in the loop. The loop macro then fetches the",
				     1,
				     2454)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2524 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; collection of all templates and pieces them together the loop code (all ",
				     1,
				     2525)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2601 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; templates stored in the global *loop-alist*). The advantage of this ",
				     1,
				     2602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2674 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; approach is that the syntax of the loop is order independent.",
				     1,
				     2675)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2740 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";", 1, 2741)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2743 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; LOCAL LOOP VARIABLES", 1, 2744)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2768 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 2769)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2772 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Use INITIAL to define variables within the scope of the loop.",
				     1,
				     2773)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2838 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; E.g. (initial (foo 5) (bar 'baz)). This will be translated in ",
				     1,
				     2839)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2905 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; (let* ( (foo 5) (bar 'baz) ..) ..). Notice that ",
				     1,
				     2906)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:2958 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; bindings are done sequentially via let*",
				     1,
				     2959)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3002 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 3003)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3006 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; ITERATION DRIVING CLAUSES", 1, 3007)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3036 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 3037)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3040 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; The iteration driving clauses are those discussed the sections of",
				     1,
				     3041)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; numeric iteration", 1, 3111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3132 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; mapping of lists and the macros YWHILE and YUNTIL.  (YWHILE x) and (YUNTIL y)",
				     1,
				     3133)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3214 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; are translated to expand into (if (not x) nil (go loop)) and (if y nil ",
				     1,
				     3215)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3290 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; (go loop))", 1, 3291)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3305 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; ", 1, 3306)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3310 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; NUMERIC ITERATION", 1, 3311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3332 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; There are 2 ways to perform numeric iteration. The first is via the YFOR ",
				     1,
				     3333)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3410 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; statement:", 1, 3411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3425 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; e.g. (YFOR iteration-variable FROM begin-iteration TO  end-iteration STEP",
				     1,
				     3426)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3503 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;  inc-or-dec) [downward stepping can be implemented using negative steps]",
				     1,
				     3504)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3580 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; The second is via the (incr ..) and (decr ..) constructs. FROM and IN are ",
				     1,
				     3581)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3659 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; synonyms in this construct. If the .IN. type construct is desired ",
				     1,
				     3660)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3730 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; (see documentation), use IN not FROM. A step value is optional in both",
				     1,
				     3731)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3805 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; cases and defaults to 1.", 1, 3806)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3834 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 3835)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3838 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; MAPPING OF LISTS", 1, 3839)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3859 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Two constructs are provided for list mapping both accessible via the FOR",
				     1,
				     3860)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:3936 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; statement. The IN construct permits mapping over successive elements of ",
				     1,
				     3937)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4013 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; the list. Eg. (yfor a in '(1 2 3 4))",
				     1,
				     4014)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4054 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; The ON constuct is similar to in except that it maps over successive cdrs.",
				     1,
				     4055)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4134)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4137 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; Examples ", 1, 4138)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4151 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4152)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4155 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; (yloop (incr a .in. 0 to 10) (ydo (print a))) ;print 0..10",
				     1,
				     4156)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4218 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; (yloop (ydo (print 'a)))", 1, 4219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4247 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; ", 1, 4248)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4252 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; (yloop(initially a 0 b 5)(yfor x from 0 to 10)(ydo(print x))(yresult b))",
				     1,
				     4253)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;    will print 0..10 and return 5.",
				     1,
				     4330)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4367 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4368)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4371 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; ", 1, 4372)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4376 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4377)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4380 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; ADDING NEW LOOP MACROS", 1, 4381)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4407 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4408)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4411 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Code has been provided to add the user define his/her own loop macros. ",
				     1,
				     4412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4487 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; See explanation and code in the file.",
				     1,
				     4488)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4530)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4533 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; HINTS", 1, 4534)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4543 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4544)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4547 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; On Translation time syntax checking: as clauses are independent macros, ",
				     1,
				     4548)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4624 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; translation time syntax checking will be clumbersome. The  values in ",
				     1,
				     4625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4698 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; *loop-alist* will have to be used after that list is fully constituted.",
				     1,
				     4699)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4774 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4778 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; EXPORT CONTROL", 1, 4779)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4797 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 4798)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4801 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Note that all symbols that will be used in trio, or some other package, ",
				     1,
				     4802)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; have to be exported.", 1, 4879)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";  ;; fix 9-May-93 -FER  in DEFINE-AND-RENAME-LOOP-LOCALS NIL",
				     1,
				     4905)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:4967 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";  (if result (add-element-to-loop-alist (cons 'progn (list result)) 'result))",
				     1,
				     4968)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5047 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";", 1, 5048)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5050 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; Also need to fix yloop so that results get spliced in within a prog, list,",
				     1,
				     5051)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5128 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";  or values (don't know how result is being used if there are multiple ones,  ",
				     1,
				     5129)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5209 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defvar,
			    '*loop-alist*',
			    [],
			    '$STRING'("To contain translated loop information ")
			  ]).

% annotating U::*LOOP-ALIST* 
doc: doc_string(u_xx_loop_alist_xx,
	      pkg_user,
	      variable,
	      "To contain translated loop information ").

:- set_var(TLEnv3, defvar, u_xx_loop_alist_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5278 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'clear-loop-alist',
			    [],
			    ['#BQ', [setf, '*loop-alist*', []]]
			  ]).

% annotating U::CLEAR-LOOP-ALIST 
wl: lambda_def(defmacro,
	      u_clear_loop_alist,
	      f_u_clear_loop_alist,
	      [],
	      [progn, ['#BQ', [setf, u_xx_loop_alist_xx, []]]]).


% annotating U::CLEAR-LOOP-ALIST 
wl: arglist_info(u_clear_loop_alist,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CLEAR-LOOP-ALIST 
wl: init_args(exact_only, u_clear_loop_alist).


% annotating U::CLEAR-LOOP-ALIST 
f_u_clear_loop_alist(FnResult) :-
	TLEnv3=[],
	[setf, u_xx_loop_alist_xx, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_clear_loop_alist, classof, claz_macro),
   set_opv(u_clear_loop_alist, compile_as, kw_operator),
   set_opv(u_clear_loop_alist, function, f_u_clear_loop_alist),
   DefMacroResult=u_clear_loop_alist.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5337 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fetch-clauses',
			    ['clause-key'],
			    
			    [ '#BQ',
			      
			      [ car,
				
				[ rassoc,
				  [quote, ['#COMMA', 'clause-key']],
				  '*loop-alist*'
				]
			      ]
			    ]
			  ]).

% annotating U::FETCH-CLAUSES 
wl: lambda_def(defmacro,
	      u_fetch_clauses,
	      f_u_fetch_clauses,
	      [u_clause_key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ car,
		    
		    [ rassoc,
		      [quote, ['#COMMA', u_clause_key]],
		      u_xx_loop_alist_xx
		    ]
		  ]
		]
	      ]).


% annotating U::FETCH-CLAUSES 
wl: arglist_info(u_fetch_clauses,
		[u_clause_key],
		[Clause_key_Param],
		arginfo{ all:[u_clause_key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_clause_key],
			 opt:0,
			 req:[u_clause_key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FETCH-CLAUSES 
wl: init_args(exact_only, u_fetch_clauses).


% annotating U::FETCH-CLAUSES 
f_u_fetch_clauses(Clause_key_Param, FnResult) :-
	[car, [rassoc, [quote, Clause_key_Param], u_xx_loop_alist_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fetch_clauses, classof, claz_macro),
   set_opv(u_fetch_clauses, compile_as, kw_operator),
   set_opv(u_fetch_clauses, function, f_u_fetch_clauses),
   DefMacroResult=u_fetch_clauses.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5428 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'acons-setf',
			    [key, datum, list],
			    '$STRING'("Functions like acons accept changes list to the new value."),
			    
			    [ '#BQ',
			      
			      [ setf,
				['#COMMA', list],
				
				[ acons,
				  ['#COMMA', key],
				  ['#COMMA', datum],
				  ['#COMMA', list]
				]
			      ]
			    ]
			  ]).

% annotating U::ACONS-SETF 
doc: doc_string(u_acons_setf,
	      _118508,
	      function,
	      "Functions like acons accept changes list to the new value.").


% annotating U::ACONS-SETF 
wl: lambda_def(defmacro,
	      u_acons_setf,
	      f_u_acons_setf,
	      [key, u_datum, list],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ setf,
		    ['#COMMA', list],
		    [acons, ['#COMMA', key], ['#COMMA', u_datum], ['#COMMA', list]]
		  ]
		]
	      ]).


% annotating U::ACONS-SETF 
wl: arglist_info(u_acons_setf,
		[key, u_datum, list],
		[Key_Param, Datum_Param, List_Param],
		arginfo{ all:[key, u_datum, list],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[key, u_datum, list],
			 opt:0,
			 req:[key, u_datum, list],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACONS-SETF 
wl: init_args(exact_only, u_acons_setf).


% annotating U::ACONS-SETF 
f_u_acons_setf(Key_Param, Datum_Param, List_Param, FnResult) :-
	[setf, List_Param, [acons, Key_Param, Datum_Param, List_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_acons_setf, classof, claz_macro),
   set_opv(u_acons_setf, compile_as, kw_operator),
   set_opv(u_acons_setf, function, f_u_acons_setf),
   DefMacroResult=u_acons_setf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5575 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    before,
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				[cons, [quote, progn], [quote, ['#COMMA', body]]],
				[quote, before]
			      ]
			    ]
			  ]).

% annotating U::BEFORE 
wl: lambda_def(defmacro,
	      u_before,
	      f_u_before,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [cons, [quote, progn], [quote, ['#COMMA', u_body]]],
		    [quote, u_before]
		  ]
		]
	      ]).


% annotating U::BEFORE 
wl: arglist_info(u_before,
		[c38_rest, u_body],
		[u_body],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::BEFORE 
wl: init_args(rest_only, u_before).


% annotating U::BEFORE 
f_u_before(Whole, FnResult) :-
	append([], Body_Param, Whole),
	TLEnv3=[bv(u_body, Body_Param)],
	get_var(TLEnv3, u_body, Body_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Body_Get]], [quote, u_before]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_before, classof, claz_macro),
   set_opv(u_before, compile_as, kw_operator),
   set_opv(u_before, function, f_u_before),
   DefMacroResult=u_before.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5669 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    initial,
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ dolist,
				[clause, [quote, ['#COMMA', body]]],
				
				[ 'add-element-to-loop-alist',
				  clause,
				  [quote, initializations]
				]
			      ]
			    ]
			  ]).

% annotating U::INITIAL 
wl: lambda_def(defmacro,
	      u_initial,
	      f_u_initial,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ dolist,
		    [u_clause, [quote, ['#COMMA', u_body]]],
		    
		    [ u_add_element_to_loop_alist,
		      u_clause,
		      [quote, u_initializations]
		    ]
		  ]
		]
	      ]).


% annotating U::INITIAL 
wl: arglist_info(u_initial,
		[c38_rest, u_body],
		[u_body],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::INITIAL 
wl: init_args(rest_only, u_initial).


% annotating U::INITIAL 
f_u_initial(Whole, FnResult) :-
	append([], Body_Param, Whole),
	TLEnv3=[bv(u_body, Body_Param)],
	get_var(TLEnv3, u_body, Body_Get),
	[dolist, [u_clause, [quote, Body_Get]], [u_add_element_to_loop_alist, u_clause, [quote, u_initializations]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_initial, classof, claz_macro),
   set_opv(u_initial, compile_as, kw_operator),
   set_opv(u_initial, function, f_u_initial),
   DefMacroResult=u_initial.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5787 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    next,
			    ['&rest', clauses],
			    '$STRING'("Next clauses must be of the form (var exp). Eg (next (i (+ i 1)))."),
			    
			    [ '#BQ',
			      
			      [ let,
				[['assignment-list', []]],
				
				[ dolist,
				  [clause, [quote, ['#COMMA', clauses]]],
				  
				  [ setf,
				    'assignment-list',
				    
				    [ cons,
				      [cons, [quote, setf], clause],
				      'assignment-list'
				    ]
				  ]
				],
				
				[ 'add-element-to-loop-alist',
				  [cons, [quote, progn], 'assignment-list'],
				  [quote, next]
				]
			      ]
			    ]
			  ]).

% annotating U::NEXT 
doc: doc_string(u_next,
	      _117786,
	      function,
	      "Next clauses must be of the form (var exp). Eg (next (i (+ i 1))).").


% annotating U::NEXT 
wl: lambda_def(defmacro,
	      u_next,
	      f_u_next,
	      [c38_rest, u_clauses],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_assignment_list, []]],
		    
		    [ dolist,
		      [u_clause, [quote, ['#COMMA', u_clauses]]],
		      
		      [ setf,
			u_assignment_list,
			[cons, [cons, [quote, setf], u_clause], u_assignment_list]
		      ]
		    ],
		    
		    [ u_add_element_to_loop_alist,
		      [cons, [quote, progn], u_assignment_list],
		      [quote, u_next]
		    ]
		  ]
		]
	      ]).


% annotating U::NEXT 
wl: arglist_info(u_next,
		[c38_rest, u_clauses],
		[u_clauses],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clauses],
			 opt:0,
			 req:0,
			 rest:[u_clauses],
			 sublists:0,
			 whole:0
		       }).


% annotating U::NEXT 
wl: init_args(rest_only, u_next).


% annotating U::NEXT 
f_u_next(Whole, FnResult) :-
	append([], Clauses_Param, Whole),
	TLEnv3=[bv(u_clauses, Clauses_Param)],
	get_var(TLEnv3, u_clauses, Clauses_Get),
	[let, [[u_assignment_list, []]], [dolist, [u_clause, [quote, Clauses_Get]], [setf, u_assignment_list, [cons, [cons, [quote, setf], u_clause], u_assignment_list]]], [u_add_element_to_loop_alist, [cons, [quote, progn], u_assignment_list], [quote, u_next]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_next, classof, claz_macro),
   set_opv(u_next, compile_as, kw_operator),
   set_opv(u_next, function, f_u_next),
   DefMacroResult=u_next.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6109 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    yresult,
			    ['&rest', clauses],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				
				[ cons,
				  [quote, progn],
				  [quote, ['#COMMA', clauses]]
				],
				[quote, result]
			      ]
			    ]
			  ]).

% annotating U::YRESULT 
wl: lambda_def(defmacro,
	      u_yresult,
	      f_u_yresult,
	      [c38_rest, u_clauses],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]],
		    [quote, u_result]
		  ]
		]
	      ]).


% annotating U::YRESULT 
wl: arglist_info(u_yresult,
		[c38_rest, u_clauses],
		[u_clauses],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clauses],
			 opt:0,
			 req:0,
			 rest:[u_clauses],
			 sublists:0,
			 whole:0
		       }).


% annotating U::YRESULT 
wl: init_args(rest_only, u_yresult).


% annotating U::YRESULT 
f_u_yresult(Whole, FnResult) :-
	append([], Clauses_Param, Whole),
	TLEnv3=[bv(u_clauses, Clauses_Param)],
	get_var(TLEnv3, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, u_result]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yresult, classof, claz_macro),
   set_opv(u_yresult, compile_as, kw_operator),
   set_opv(u_yresult, function, f_u_yresult),
   DefMacroResult=u_yresult.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6208 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    ydo,
			    ['&rest', clauses],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				
				[ cons,
				  [quote, progn],
				  [quote, ['#COMMA', clauses]]
				],
				[quote, do]
			      ]
			    ]
			  ]).

% annotating U::YDO 
wl: lambda_def(defmacro,
	      u_ydo,
	      f_u_ydo,
	      [c38_rest, u_clauses],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]],
		    [quote, do]
		  ]
		]
	      ]).


% annotating U::YDO 
wl: arglist_info(u_ydo,
		[c38_rest, u_clauses],
		[u_clauses],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clauses],
			 opt:0,
			 req:0,
			 rest:[u_clauses],
			 sublists:0,
			 whole:0
		       }).


% annotating U::YDO 
wl: init_args(rest_only, u_ydo).


% annotating U::YDO 
f_u_ydo(Whole, FnResult) :-
	append([], Clauses_Param, Whole),
	TLEnv3=[bv(u_clauses, Clauses_Param)],
	get_var(TLEnv3, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, do]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ydo, classof, claz_macro),
   set_opv(u_ydo, compile_as, kw_operator),
   set_opv(u_ydo, function, f_u_ydo),
   DefMacroResult=u_ydo.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6299 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    ywhile,
			    [expression],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				
				[ list,
				  [quote, not],
				  [quote, ['#COMMA', expression]]
				],
				[quote, 'end-test']
			      ]
			    ]
			  ]).

% annotating U::YWHILE 
wl: lambda_def(defmacro,
	      u_ywhile,
	      f_u_ywhile,
	      [u_expression],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [list, [quote, not], [quote, ['#COMMA', u_expression]]],
		    [quote, u_end_test]
		  ]
		]
	      ]).


% annotating U::YWHILE 
wl: arglist_info(u_ywhile,
		[u_expression],
		[Expression_Param],
		arginfo{ all:[u_expression],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_expression],
			 opt:0,
			 req:[u_expression],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::YWHILE 
wl: init_args(exact_only, u_ywhile).


% annotating U::YWHILE 
f_u_ywhile(Expression_Param, FnResult) :-
	[u_add_element_to_loop_alist, [list, [quote, not], [quote, Expression_Param]], [quote, u_end_test]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ywhile, classof, claz_macro),
   set_opv(u_ywhile, compile_as, kw_operator),
   set_opv(u_ywhile, function, f_u_ywhile),
   DefMacroResult=u_ywhile.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6397 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    yuntil,
			    [expression],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				[quote, ['#COMMA', expression]],
				[quote, 'end-test']
			      ]
			    ]
			  ]).

% annotating U::YUNTIL 
wl: lambda_def(defmacro,
	      u_yuntil,
	      f_u_yuntil,
	      [u_expression],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [quote, ['#COMMA', u_expression]],
		    [quote, u_end_test]
		  ]
		]
	      ]).


% annotating U::YUNTIL 
wl: arglist_info(u_yuntil,
		[u_expression],
		[Expression_Param],
		arginfo{ all:[u_expression],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_expression],
			 opt:0,
			 req:[u_expression],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::YUNTIL 
wl: init_args(exact_only, u_yuntil).


% annotating U::YUNTIL 
f_u_yuntil(Expression_Param, FnResult) :-
	[u_add_element_to_loop_alist, [quote, Expression_Param], [quote, u_end_test]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yuntil, classof, claz_macro),
   set_opv(u_yuntil, compile_as, kw_operator),
   set_opv(u_yuntil, function, f_u_yuntil),
   DefMacroResult=u_yuntil.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6484 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    lerror,
			    ['format-string', '&rest', 'format-args'],
			    
			    [ '#BQ',
			      
			      [ error,
				['#COMMA', 'format-string'],
				['#BQ-COMMA-ELIPSE', 'format-args']
			      ]
			    ]
			  ]).

% annotating U::LERROR 
wl: lambda_def(defmacro,
	      u_lerror,
	      f_u_lerror,
	      [u_format_string, c38_rest, u_format_args],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ error,
		    ['#COMMA', u_format_string],
		    ['#BQ-COMMA-ELIPSE', u_format_args]
		  ]
		]
	      ]).


% annotating U::LERROR 
wl: arglist_info(u_lerror,
		[u_format_string, c38_rest, u_format_args],
		[u_format_string, u_format_args],
		arginfo{ all:[u_format_string],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_format_string, u_format_args],
			 opt:0,
			 req:[u_format_string],
			 rest:[u_format_args],
			 sublists:0,
			 whole:0
		       }).


% annotating U::LERROR 
wl: init_args(1, u_lerror).


% annotating U::LERROR 
f_u_lerror(Format_string_Param, Format_args_Param, FnResult) :-
	TLEnv3=[bv(u_format_string, Format_string_Param), bv(u_format_args, Format_args_Param)],
	get_var(TLEnv3, u_format_args, Format_args_Get),
	[error, Format_string_Param|Format_args_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_lerror, classof, claz_macro),
   set_opv(u_lerror, compile_as, kw_operator),
   set_opv(u_lerror, function, f_u_lerror),
   DefMacroResult=u_lerror.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6577 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defvar,
			    '*stepping-variable*',
			    [],
			    '$STRING'("Dummy variable to nest macros without compiler barf.")
			  ]).

% annotating U::*STEPPING-VARIABLE* 
doc: doc_string(u_xx_stepping_variable_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(TLEnv3, defvar, u_xx_stepping_variable_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defvar,
			    '*what-to-do*',
			    [],
			    '$STRING'("Dummy variable to nest macros without compiler barf.")
			  ]).

% annotating U::*WHAT-TO-DO* 
doc: doc_string(u_xx_what_to_do_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(TLEnv3, defvar, u_xx_what_to_do_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6752 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defvar,
			    '*llist*',
			    [],
			    '$STRING'("Dummy variable to nest macros without compiler barf.")
			  ]).

% annotating U::*LLIST* 
doc: doc_string(u_xx_llist_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(TLEnv3, defvar, u_xx_llist_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6832 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    yfor,
			    [variable, 'what-to-do', '&rest', llist],
			    
			    [ let,
			      
			      [ ['iteration-variable', [gensym]],
				['iteration-expression', [gensym]],
				'stepping-variable'
			      ],
			      
			      [ '#BQ',
				
				[ let,
				  
				  [ [['#COMMA', 'iteration-variable'], []],
				    [['#COMMA', 'iteration-expression'], []]
				  ],
				  
				  [ '#COMMA',
				    
				    [ 'record-in-loop-alist',
				      
				      [ '#BQ',
					
					[ ['#COMMA', variable],
					  ['#COMMA', 'iteration-variable']
					]
				      ],
				      [quote, 'iteration-variable']
				    ]
				  ],
				  
				  [ '#COMMA',
				    
				    [ case,
				      [intern, ['symbol-name', 'what-to-do']],
				      
				      [ in,
					
					[ 'record-in-loop-alist',
					  
					  [ '#BQ',
					    
					    [ endp,
					      
					      [ '#COMMA',
						'iteration-expression'
					      ]
					    ]
					  ],
					  [quote, 'end-test']
					],
					
					[ 'add-elements-to-clause',
					  [quote, next],
					  
					  [ '#BQ',
					    
					    [ setf,
					      ['#COMMA', 'iteration-variable'],
					      
					      [ car,
						
						[ '#COMMA',
						  'iteration-expression'
						]
					      ]
					    ]
					  ],
					  
					  [ '#BQ',
					    
					    [ setf,
					      
					      [ '#COMMA',
						'iteration-expression'
					      ],
					      
					      [ cdr,
						
						[ '#COMMA',
						  'iteration-expression'
						]
					      ]
					    ]
					  ]
					],
					
					[ 'add-elements-to-clause',
					  [quote, initializations],
					  
					  [ '#BQ',
					    
					    [ ['#COMMA', 'iteration-variable'],
					      
					      [ car,
						
						[ '#COMMA',
						  'iteration-expression'
						]
					      ]
					    ]
					  ],
					  
					  [ '#BQ',
					    
					    [ 
					      [ '#COMMA',
						'iteration-expression'
					      ],
					      ['#BQ-COMMA-ELIPSE', llist]
					    ]
					  ]
					]
				      ],
				      
				      [ on,
					
					[ 'record-in-loop-alist',
					  'iteration-expression',
					  [quote, 'iteration-control-variable']
					],
					
					[ 'record-in-loop-alist',
					  
					  [ '#BQ',
					    
					    [ endp,
					      
					      [ '#COMMA',
						'iteration-expression'
					      ]
					    ]
					  ],
					  [quote, 'end-test']
					],
					
					[ 'add-elements-to-clause',
					  [quote, next],
					  
					  [ '#BQ',
					    
					    [ setf,
					      ['#COMMA', 'iteration-variable'],
					      
					      [ '#COMMA',
						'iteration-expression'
					      ]
					    ]
					  ],
					  
					  [ '#BQ',
					    
					    [ setf,
					      
					      [ '#COMMA',
						'iteration-expression'
					      ],
					      
					      [ cdr,
						
						[ '#COMMA',
						  'iteration-expression'
						]
					      ]
					    ]
					  ]
					],
					
					[ 'add-elements-to-clause',
					  [quote, initializations],
					  
					  [ '#BQ',
					    
					    [ ['#COMMA', 'iteration-variable'],
					      
					      [ car,
						
						[ '#COMMA',
						  'iteration-expression'
						]
					      ]
					    ]
					  ],
					  
					  [ '#BQ',
					    
					    [ 
					      [ '#COMMA',
						'iteration-expression'
					      ],
					      ['#BQ-COMMA-ELIPSE', llist]
					    ]
					  ]
					]
				      ],
				      
				      [ from,
					
					[ if,
					  [null, [fifth, llist]],
					  [setf, 'stepping-variable', 1],
					  
					  [ setf,
					    'stepping-variable',
					    [fifth, llist]
					  ]
					],
					
					[ cond,
					  
					  [ [>, [length, llist], 5],
					    
					    [ lerror,
					      '$STRING'("YL:Too many clauses in (yfor ~a ~a ..)"),
					      variable,
					      'what-to-do'
					    ]
					  ],
					  
					  [ 
					    [ and,
					      [minusp, 'stepping-variable'],
					      [<=, [first, llist], [third, llist]]
					    ],
					    
					    [ lerror,
					      '$STRING'("YL:Cannot decrement from ~a to ~a"),
					      [first, llist],
					      [third, llist]
					    ]
					  ],
					  
					  [ t,
					    
					    [ 'add-element-to-loop-alist',
					      
					      [ '#BQ',
						
						[ 
						  [ '#COMMA',
						    'iteration-variable'
						  ],
						  ['#COMMA', [first, llist]]
						]
					      ],
					      [quote, initializations]
					    ],
					    
					    [ 'add-element-to-loop-alist',
					      
					      [ '#BQ',
						
						[ setf,
						  
						  [ '#COMMA',
						    'iteration-variable'
						  ],
						  
						  [ (+),
						    
						    [ '#COMMA',
						      'iteration-variable'
						    ],
						    
						    [ '#COMMA',
						      'stepping-variable'
						    ]
						  ]
						]
					      ],
					      [quote, next]
					    ],
					    
					    [ if,
					      [minusp, 'stepping-variable'],
					      
					      [ 'add-element-to-loop-alist',
						
						[ '#BQ',
						  
						  [ (<),
						    
						    [ '#COMMA',
						      'iteration-variable'
						    ],
						    ['#COMMA', [third, llist]]
						  ]
						],
						[quote, 'end-test']
					      ],
					      
					      [ 'add-element-to-loop-alist',
						
						[ '#BQ',
						  
						  [ (>),
						    
						    [ '#COMMA',
						      'iteration-variable'
						    ],
						    ['#COMMA', [third, llist]]
						  ]
						],
						[quote, 'end-test']
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    t
			  ]).

% annotating U::YFOR 
wl: lambda_def(defmacro,
	      u_yfor,
	      f_u_yfor,
	      [variable, u_what_to_do, c38_rest, u_llist],
	      
	      [ progn,
		
		[ let,
		  
		  [ [u_iteration_variable, [gensym]],
		    [u_iteration_expression, [gensym]],
		    u_stepping_variable
		  ],
		  
		  [ '#BQ',
		    
		    [ let,
		      
		      [ [['#COMMA', u_iteration_variable], []],
			[['#COMMA', u_iteration_expression], []]
		      ],
		      
		      [ '#COMMA',
			
			[ u_record_in_loop_alist,
			  
			  [ '#BQ',
			    
			    [ ['#COMMA', variable],
			      ['#COMMA', u_iteration_variable]
			    ]
			  ],
			  [quote, u_iteration_variable]
			]
		      ],
		      
		      [ '#COMMA',
			
			[ case,
			  [intern, [symbol_name, u_what_to_do]],
			  
			  [ u_in,
			    
			    [ u_record_in_loop_alist,
			      ['#BQ', [endp, ['#COMMA', u_iteration_expression]]],
			      [quote, u_end_test]
			    ],
			    
			    [ u_add_elements_to_clause,
			      [quote, u_next],
			      
			      [ '#BQ',
				
				[ setf,
				  ['#COMMA', u_iteration_variable],
				  [car, ['#COMMA', u_iteration_expression]]
				]
			      ],
			      
			      [ '#BQ',
				
				[ setf,
				  ['#COMMA', u_iteration_expression],
				  [cdr, ['#COMMA', u_iteration_expression]]
				]
			      ]
			    ],
			    
			    [ u_add_elements_to_clause,
			      [quote, u_initializations],
			      
			      [ '#BQ',
				
				[ ['#COMMA', u_iteration_variable],
				  [car, ['#COMMA', u_iteration_expression]]
				]
			      ],
			      
			      [ '#BQ',
				
				[ ['#COMMA', u_iteration_expression],
				  ['#BQ-COMMA-ELIPSE', u_llist]
				]
			      ]
			    ]
			  ],
			  
			  [ u_on,
			    
			    [ u_record_in_loop_alist,
			      u_iteration_expression,
			      [quote, u_iteration_control_variable]
			    ],
			    
			    [ u_record_in_loop_alist,
			      ['#BQ', [endp, ['#COMMA', u_iteration_expression]]],
			      [quote, u_end_test]
			    ],
			    
			    [ u_add_elements_to_clause,
			      [quote, u_next],
			      
			      [ '#BQ',
				
				[ setf,
				  ['#COMMA', u_iteration_variable],
				  ['#COMMA', u_iteration_expression]
				]
			      ],
			      
			      [ '#BQ',
				
				[ setf,
				  ['#COMMA', u_iteration_expression],
				  [cdr, ['#COMMA', u_iteration_expression]]
				]
			      ]
			    ],
			    
			    [ u_add_elements_to_clause,
			      [quote, u_initializations],
			      
			      [ '#BQ',
				
				[ ['#COMMA', u_iteration_variable],
				  [car, ['#COMMA', u_iteration_expression]]
				]
			      ],
			      
			      [ '#BQ',
				
				[ ['#COMMA', u_iteration_expression],
				  ['#BQ-COMMA-ELIPSE', u_llist]
				]
			      ]
			    ]
			  ],
			  
			  [ u_from,
			    
			    [ if,
			      [null, [fifth, u_llist]],
			      [setf, u_stepping_variable, 1],
			      [setf, u_stepping_variable, [fifth, u_llist]]
			    ],
			    
			    [ cond,
			      
			      [ [>, [length, u_llist], 5],
				
				[ u_lerror,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('Y'),
					     #\('L'),
					     #\(:),
					     #\('T'),
					     #\(o),
					     #\(o),
					     #\(' '),
					     #\(m),
					     #\(a),
					     #\(n),
					     #\(y),
					     #\(' '),
					     #\(c),
					     #\(l),
					     #\(a),
					     #\(u),
					     #\(s),
					     #\(e),
					     #\(s),
					     #\(' '),
					     #\(i),
					     #\(n),
					     #\(' '),
					     #\('('),
					     #\(y),
					     #\(f),
					     #\(o),
					     #\(r),
					     #\(' '),
					     #\(~),
					     #\(a),
					     #\(' '),
					     #\(~),
					     #\(a),
					     #\(' '),
					     #\('.'),
					     #\('.'),
					     #\(')')
					   ]),
				  variable,
				  u_what_to_do
				]
			      ],
			      
			      [ 
				[ and,
				  [minusp, u_stepping_variable],
				  [<=, [first, u_llist], [third, u_llist]]
				],
				
				[ u_lerror,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('Y'),
					     #\('L'),
					     #\(:),
					     #\('C'),
					     #\(a),
					     #\(n),
					     #\(n),
					     #\(o),
					     #\(t),
					     #\(' '),
					     #\(d),
					     #\(e),
					     #\(c),
					     #\(r),
					     #\(e),
					     #\(m),
					     #\(e),
					     #\(n),
					     #\(t),
					     #\(' '),
					     #\(f),
					     #\(r),
					     #\(o),
					     #\(m),
					     #\(' '),
					     #\(~),
					     #\(a),
					     #\(' '),
					     #\(t),
					     #\(o),
					     #\(' '),
					     #\(~),
					     #\(a)
					   ]),
				  [first, u_llist],
				  [third, u_llist]
				]
			      ],
			      
			      [ t,
				
				[ u_add_element_to_loop_alist,
				  
				  [ '#BQ',
				    
				    [ ['#COMMA', u_iteration_variable],
				      ['#COMMA', [first, u_llist]]
				    ]
				  ],
				  [quote, u_initializations]
				],
				
				[ u_add_element_to_loop_alist,
				  
				  [ '#BQ',
				    
				    [ setf,
				      ['#COMMA', u_iteration_variable],
				      
				      [ (+),
					['#COMMA', u_iteration_variable],
					['#COMMA', u_stepping_variable]
				      ]
				    ]
				  ],
				  [quote, u_next]
				],
				
				[ if,
				  [minusp, u_stepping_variable],
				  
				  [ u_add_element_to_loop_alist,
				    
				    [ '#BQ',
				      
				      [ (<),
					['#COMMA', u_iteration_variable],
					['#COMMA', [third, u_llist]]
				      ]
				    ],
				    [quote, u_end_test]
				  ],
				  
				  [ u_add_element_to_loop_alist,
				    
				    [ '#BQ',
				      
				      [ (>),
					['#COMMA', u_iteration_variable],
					['#COMMA', [third, u_llist]]
				      ]
				    ],
				    [quote, u_end_test]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		],
		t
	      ]).


% annotating U::YFOR 
wl: arglist_info(u_yfor,
		[variable, u_what_to_do, c38_rest, u_llist],
		[variable, u_what_to_do, u_llist],
		arginfo{ all:[variable, u_what_to_do],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[variable, u_what_to_do, u_llist],
			 opt:0,
			 req:[variable, u_what_to_do],
			 rest:[u_llist],
			 sublists:0,
			 whole:0
		       }).


% annotating U::YFOR 
wl: init_args(2, u_yfor).


% annotating U::YFOR 
f_u_yfor(Variable_Param, What_to_do_Param, Llist_Param, FnResult) :-
	TLEnv3=[bv(variable, Variable_Param), bv(u_what_to_do, What_to_do_Param), bv(u_llist, Llist_Param)],
	cl_gensym(Iteration_variable_Init),
	cl_gensym(Iteration_expression_Init),
	Setf_Env=[[bv(u_iteration_variable, Iteration_variable_Init), bv(u_iteration_expression, Iteration_expression_Init), bv(u_stepping_variable, [])]|TLEnv3],
	get_var(Setf_Env, u_iteration_expression, Iteration_expression_Get),
	get_var(Setf_Env, u_iteration_variable, Iteration_variable_Get25),
	f_u_record_in_loop_alist([Variable_Param, Iteration_variable_Get25],
				 u_iteration_variable,
				 Iteration_variable),
	cl_symbol_name(What_to_do_Param, Intern_Param),
	cl_intern(Intern_Param, avar(PredArg1Result, att(preserved_var, t, []))),
	(   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), u_in)
	->  get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get31),
	    f_u_record_in_loop_alist([endp, Iteration_expression_Get31],
				     u_end_test,
				     End_test),
	    get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get33),
	    get_var(Setf_Env, u_iteration_variable, Iteration_variable_Get32),
	    f_u_add_elements_to_clause(u_next,
				       
				       [ 
					 [ setf,
					   Iteration_variable_Get32,
					   [car, Iteration_expression_Get33]
					 ],
					 
					 [ setf,
					   Iteration_expression_Get33,
					   [cdr, Iteration_expression_Get33]
					 ]
				       ],
				       To_clause_Ret),
	    get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get37),
	    get_var(Setf_Env, u_iteration_variable, Iteration_variable_Get36),
	    get_var(Setf_Env, u_llist, Llist_Get),
	    f_u_add_elements_to_clause(u_initializations,
				       
				       [ 
					 [ Iteration_variable_Get36,
					   [car, Iteration_expression_Get37]
					 ],
					 
					 [ Iteration_expression_Get37
					 | Llist_Get
					 ]
				       ],
				       TrueResult99),
	    ElseResult92=TrueResult99
	;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), u_on)
	->  get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get42),
	    f_u_record_in_loop_alist(Iteration_expression_Get42,
				     u_iteration_control_variable,
				     Iteration_control_variable),
	    get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get43),
	    f_u_record_in_loop_alist([endp, Iteration_expression_Get43],
				     u_end_test,
				     End_test106),
	    get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get45),
	    get_var(Setf_Env, u_iteration_variable, Iteration_variable_Get44),
	    f_u_add_elements_to_clause(u_next,
				       
				       [ 
					 [ setf,
					   Iteration_variable_Get44,
					   Iteration_expression_Get45
					 ],
					 
					 [ setf,
					   Iteration_expression_Get45,
					   [cdr, Iteration_expression_Get45]
					 ]
				       ],
				       To_clause_Ret111),
	    get_var(Setf_Env,
		    u_iteration_expression,
		    Iteration_expression_Get49),
	    get_var(Setf_Env, u_iteration_variable, Iteration_variable_Get48),
	    get_var(Setf_Env, u_llist, Llist_Get51),
	    f_u_add_elements_to_clause(u_initializations,
				       
				       [ 
					 [ Iteration_variable_Get48,
					   [car, Iteration_expression_Get49]
					 ],
					 
					 [ Iteration_expression_Get49
					 | Llist_Get51
					 ]
				       ],
				       TrueResult97),
	    ElseResult92=TrueResult97
	;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), u_from)
	->  get_var(Setf_Env, u_llist, Llist_Get56),
	    cl_fifth(Llist_Get56, IFTEST54),
	    (   IFTEST54==[]
	    ->  set_place(Setf_Env,
			  setf,
			  [value, u_stepping_variable],
			  [1],
			  Setf_R),
		_126372=Setf_R
	    ;   get_var(Setf_Env, u_llist, Llist_Get59),
		cl_fifth(Llist_Get59, Fifth_Ret),
		set_place(Setf_Env,
			  setf,
			  [value, u_stepping_variable],
			  [Fifth_Ret],
			  Setf_R60),
		_126372=Setf_R60
	    ),
	    get_var(Setf_Env, u_llist, Llist_Get64),
	    cl_length(Llist_Get64, PredArg1Result66),
	    (   PredArg1Result66>5
	    ->  f_u_lerror('$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('Y'),
				      #\('L'),
				      #\(:),
				      #\('T'),
				      #\(o),
				      #\(o),
				      #\(' '),
				      #\(m),
				      #\(a),
				      #\(n),
				      #\(y),
				      #\(' '),
				      #\(c),
				      #\(l),
				      #\(a),
				      #\(u),
				      #\(s),
				      #\(e),
				      #\(s),
				      #\(' '),
				      #\(i),
				      #\(n),
				      #\(' '),
				      #\('('),
				      #\(y),
				      #\(f),
				      #\(o),
				      #\(r),
				      #\(' '),
				      #\(~),
				      #\(a),
				      #\(' '),
				      #\(~),
				      #\(a),
				      #\(' '),
				      #\('.'),
				      #\('.'),
				      #\(')')
				    ]),
			   [variable, u_what_to_do],
			   TrueResult93),
		ElseResult92=TrueResult93
	    ;   get_var(Setf_Env, u_stepping_variable, Stepping_variable_Get),
		(   mth:is_minusp(Stepping_variable_Get)
		->  get_var(Setf_Env, u_llist, Llist_Get73),
		    cl_car(Llist_Get73, Car_Ret),
		    get_var(Setf_Env, u_llist, Llist_Get74),
		    cl_third(Llist_Get74, Third_Ret),
		    <=(Car_Ret, Third_Ret, TrueResult75),
		    IFTEST67=TrueResult75
		;   IFTEST67=[]
		),
		(   IFTEST67\==[]
		->  f_u_lerror('$ARRAY'([*],
					claz_base_character,
					
					[ #\('Y'),
					  #\('L'),
					  #\(:),
					  #\('C'),
					  #\(a),
					  #\(n),
					  #\(n),
					  #\(o),
					  #\(t),
					  #\(' '),
					  #\(d),
					  #\(e),
					  #\(c),
					  #\(r),
					  #\(e),
					  #\(m),
					  #\(e),
					  #\(n),
					  #\(t),
					  #\(' '),
					  #\(f),
					  #\(r),
					  #\(o),
					  #\(m),
					  #\(' '),
					  #\(~),
					  #\(a),
					  #\(' '),
					  #\(t),
					  #\(o),
					  #\(' '),
					  #\(~),
					  #\(a)
					]),
			       [[first, u_llist], [third, u_llist]],
			       TrueResult91),
		    ElseResult92=TrueResult91
		;   get_var(Setf_Env,
			    u_iteration_variable,
			    Iteration_variable_Get76),
		    get_var(Setf_Env, u_llist, Llist_Get77),
		    cl_car(Llist_Get77, Car_Ret115),
		    f_u_add_element_to_loop_alist(
						  [ Iteration_variable_Get76,
						    Car_Ret115
						  ],
						  u_initializations,
						  Initializations),
		    get_var(Setf_Env,
			    u_iteration_variable,
			    Iteration_variable_Get78),
		    get_var(Setf_Env,
			    u_stepping_variable,
			    Stepping_variable_Get80),
		    f_u_add_element_to_loop_alist(
						  [ setf,
						    Iteration_variable_Get78,
						    
						    [ (+),
						      Iteration_variable_Get78,
						      Stepping_variable_Get80
						    ]
						  ],
						  u_next,
						  Next),
		    get_var(Setf_Env,
			    u_stepping_variable,
			    Stepping_variable_Get82),
		    (   mth:is_minusp(Stepping_variable_Get82)
		    ->  get_var(Setf_Env,
				u_iteration_variable,
				Iteration_variable_Get85),
			get_var(Setf_Env, u_llist, Llist_Get86),
			cl_third(Llist_Get86, Third_Ret116),
			f_u_add_element_to_loop_alist(
						      [ (<),
							Iteration_variable_Get85,
							Third_Ret116
						      ],
						      u_end_test,
						      TrueResult89),
			ElseResult92=TrueResult89
		    ;   get_var(Setf_Env,
				u_iteration_variable,
				Iteration_variable_Get87),
			get_var(Setf_Env, u_llist, Llist_Get88),
			cl_third(Llist_Get88, Third_Ret117),
			f_u_add_element_to_loop_alist(
						      [ (>),
							Iteration_variable_Get87,
							Third_Ret117
						      ],
						      u_end_test,
						      ElseResult90),
			ElseResult92=ElseResult90
		    )
		)
	    )
	;   ElseResult92=[]
	),
	LetResult=[let, [[Iteration_variable_Get25, []], [Iteration_expression_Get, []]], Iteration_variable, ElseResult92],
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yfor, classof, claz_macro),
   set_opv(u_yfor, compile_as, kw_operator),
   set_opv(u_yfor, function, f_u_yfor),
   DefMacroResult=u_yfor.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6832 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("(car ,@llist))", 60, 7774)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6832 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" note that since you are in a let*, don't eval the expression twice, use",
				     13,
				     8360)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6832 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the variable that it will be bound to",
				     13,
				     8446)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:9875 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'with-incr-or-decr-checking',
			    ['&body', body],
			    '$STRING'("Very specialized code to fit in the incr and decr macros."),
			    
			    [ '#BQ',
			      
			      [ progn,
				
				[ cond,
				  [[null, args], [setf, final, t], [setf, step, 1]],
				  
				  [ [numberp, [first, args]],
				    
				    [ lerror,
				      '$STRING'("Syntax error in incr: expected a yloop keyword after ~a"),
				      init
				    ]
				  ],
				  
				  [ [not, [numberp, [second, args]]],
				    
				    [ lerror,
				      '$STRING'("Syntax error in incr: ~a not a number"),
				      [second, args]
				    ]
				  ],
				  
				  [ t,
				    [setf, final, [second, args]],
				    
				    [ if,
				      [null, [fourth, args]],
				      [setf, step, 1],
				      [setf, step, [fourth, args]]
				    ]
				  ]
				],
				['#BQ-COMMA-ELIPSE', body]
			      ]
			    ]
			  ]).

% annotating U::WITH-INCR-OR-DECR-CHECKING 
doc: doc_string(u_with_incr_or_decr_checking,
	      _119454,
	      function,
	      "Very specialized code to fit in the incr and decr macros.").


% annotating U::WITH-INCR-OR-DECR-CHECKING 
wl: lambda_def(defmacro,
	      u_with_incr_or_decr_checking,
	      f_u_with_incr_or_decr_checking,
	      [c38_body, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    
		    [ cond,
		      [[null, args], [setf, u_final, t], [setf, step, 1]],
		      
		      [ [numberp, [first, args]],
			
			[ u_lerror,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('S'),
				     #\(y),
				     #\(n),
				     #\(t),
				     #\(a),
				     #\(x),
				     #\(' '),
				     #\(e),
				     #\(r),
				     #\(r),
				     #\(o),
				     #\(r),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(c),
				     #\(r),
				     #\(:),
				     #\(' '),
				     #\(e),
				     #\(x),
				     #\(p),
				     #\(e),
				     #\(c),
				     #\(t),
				     #\(e),
				     #\(d),
				     #\(' '),
				     #\(a),
				     #\(' '),
				     #\(y),
				     #\(l),
				     #\(o),
				     #\(o),
				     #\(p),
				     #\(' '),
				     #\(k),
				     #\(e),
				     #\(y),
				     #\(w),
				     #\(o),
				     #\(r),
				     #\(d),
				     #\(' '),
				     #\(a),
				     #\(f),
				     #\(t),
				     #\(e),
				     #\(r),
				     #\(' '),
				     #\(~),
				     #\(a)
				   ]),
			  u_init
			]
		      ],
		      
		      [ [not, [numberp, [second, args]]],
			
			[ u_lerror,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('S'),
				     #\(y),
				     #\(n),
				     #\(t),
				     #\(a),
				     #\(x),
				     #\(' '),
				     #\(e),
				     #\(r),
				     #\(r),
				     #\(o),
				     #\(r),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(c),
				     #\(r),
				     #\(:),
				     #\(' '),
				     #\(~),
				     #\(a),
				     #\(' '),
				     #\(n),
				     #\(o),
				     #\(t),
				     #\(' '),
				     #\(a),
				     #\(' '),
				     #\(n),
				     #\(u),
				     #\(m),
				     #\(b),
				     #\(e),
				     #\(r)
				   ]),
			  [second, args]
			]
		      ],
		      
		      [ t,
			[setf, u_final, [second, args]],
			
			[ if,
			  [null, [fourth, args]],
			  [setf, step, 1],
			  [setf, step, [fourth, args]]
			]
		      ]
		    ],
		    ['#BQ-COMMA-ELIPSE', u_body]
		  ]
		]
	      ]).


% annotating U::WITH-INCR-OR-DECR-CHECKING 
wl: arglist_info(u_with_incr_or_decr_checking,
		[c38_body, u_body],
		[u_body],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:[u_body],
			 complex:[body],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::WITH-INCR-OR-DECR-CHECKING 
wl: init_args(rest_only, u_with_incr_or_decr_checking).


% annotating U::WITH-INCR-OR-DECR-CHECKING 
f_u_with_incr_or_decr_checking(Whole, FnResult) :-
	append([], RestNKeys, Whole),
	TLEnv3=[bv(u_body, Body_Param)],
	as_body(u_body, Body_Param, RestNKeys),
	get_var(TLEnv3, u_body, Body_Get),
	[progn, [cond, [[null, args], [setf, u_final, t], [setf, step, 1]], [[numberp, [first, args]], [u_lerror, '$ARRAY'([*], claz_base_character, [#\('S'), #\(y), #\(n), #\(t), #\(a), #\(x), #\(' '), #\(e), #\(r), #\(r), #\(o), #\(r), #\(' '), #\(i), #\(n), #\(' '), #\(i), #\(n), #\(c), #\(r), #\(:), #\(' '), #\(e), #\(x), #\(p), #\(e), #\(c), #\(t), #\(e), #\(d), #\(' '), #\(a), #\(' '), #\(y), #\(l), #\(o), #\(o), #\(p), #\(' '), #\(k), #\(e), #\(y), #\(w), #\(o), #\(r), #\(d), #\(' '), #\(a), #\(f), #\(t), #\(e), #\(r), #\(' '), #\(~), #\(a)]), u_init]], [[not, [numberp, [second, args]]], [u_lerror, '$ARRAY'([*], claz_base_character, [#\('S'), #\(y), #\(n), #\(t), #\(a), #\(x), #\(' '), #\(e), #\(r), #\(r), #\(o), #\(r), #\(' '), #\(i), #\(n), #\(' '), #\(i), #\(n), #\(c), #\(r), #\(:), #\(' '), #\(~), #\(a), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(a), #\(' '), #\(n), #\(u), #\(m), #\(b), #\(e), #\(r)]), [second, args]]], [t, [setf, u_final, [second, args]], [if, [null, [fourth, args]], [setf, step, 1], [setf, step, [fourth, args]]]]]|Body_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_incr_or_decr_checking, classof, claz_macro),
   set_opv(u_with_incr_or_decr_checking, compile_as, kw_operator),
   set_opv(u_with_incr_or_decr_checking,
	   function,
	   f_u_with_incr_or_decr_checking),
   DefMacroResult=u_with_incr_or_decr_checking.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:10447 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    incr,
			    [variable, from, init, '&rest', args],
			    
			    [ 'let*',
			      [final, step, ['iteration-variable', [gensym]]],
			      
			      [ '#BQ',
				
				[ let,
				  [[['#COMMA', 'iteration-variable'], []]],
				  
				  [ '#COMMA',
				    
				    [ 'record-in-loop-alist',
				      
				      [ '#BQ',
					
					[ ['#COMMA', variable],
					  ['#COMMA', 'iteration-variable']
					]
				      ],
				      [quote, 'iteration-variable']
				    ]
				  ],
				  
				  [ '#COMMA',
				    
				    [ 'with-incr-or-decr-checking',
				      
				      [ 'add-element-to-loop-alist',
					
					[ '#BQ',
					  
					  [ setf,
					    ['#COMMA', 'iteration-variable'],
					    
					    [ (+),
					      ['#COMMA', 'iteration-variable'],
					      ['#COMMA', step]
					    ]
					  ]
					],
					[quote, next]
				      ],
				      
				      [ case,
					[intern, ['symbol-name', from]],
					
					[ '.in.',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (>),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ '.in',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (=),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ 'in.',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (>),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', ['1+', init]]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ in,
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (=),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', ['1+', init]]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ otherwise,
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (>),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    t
			  ]).

% annotating U::INCR 
wl: lambda_def(defmacro,
	      u_incr,
	      f_u_incr,
	      [variable, u_from, u_init, c38_rest, args],
	      
	      [ progn,
		
		[ let_xx,
		  [u_final, step, [u_iteration_variable, [gensym]]],
		  
		  [ '#BQ',
		    
		    [ let,
		      [[['#COMMA', u_iteration_variable], []]],
		      
		      [ '#COMMA',
			
			[ u_record_in_loop_alist,
			  
			  [ '#BQ',
			    
			    [ ['#COMMA', variable],
			      ['#COMMA', u_iteration_variable]
			    ]
			  ],
			  [quote, u_iteration_variable]
			]
		      ],
		      
		      [ '#COMMA',
			
			[ u_with_incr_or_decr_checking,
			  
			  [ u_add_element_to_loop_alist,
			    
			    [ '#BQ',
			      
			      [ setf,
				['#COMMA', u_iteration_variable],
				
				[ (+),
				  ['#COMMA', u_iteration_variable],
				  ['#COMMA', step]
				]
			      ]
			    ],
			    [quote, u_next]
			  ],
			  
			  [ case,
			    [intern, [symbol_name, u_from]],
			    
			    [ u_c46_in_c46,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (>),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_c46_in,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (=),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_in_c46,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (>),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', ['1+', u_init]]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_in,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (=),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', ['1+', u_init]]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ otherwise,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (>),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		],
		t
	      ]).


% annotating U::INCR 
wl: arglist_info(u_incr,
		[variable, u_from, u_init, c38_rest, args],
		[variable, u_from, u_init, args],
		arginfo{ all:[variable, u_from, u_init],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[variable, u_from, u_init, args],
			 opt:0,
			 req:[variable, u_from, u_init],
			 rest:[args],
			 sublists:0,
			 whole:0
		       }).


% annotating U::INCR 
wl: init_args(3, u_incr).


% annotating U::INCR 
f_u_incr(Variable_Param, From_Param, Init_Param, Args_Param, FnResult) :-
	TLEnv3=[bv(variable, Variable_Param), bv(u_from, From_Param), bv(u_init, Init_Param), bv(args, Args_Param)],
	LEnv=[[bv(u_final, [])]|TLEnv3],
	LEnv21=[[bv(step, [])]|LEnv],
	cl_gensym(Iteration_variable_Init),
	Env=[[bv(u_iteration_variable, Iteration_variable_Init)]|LEnv21],
	get_var(Env, u_iteration_variable, Iteration_variable_Get29),
	f_u_record_in_loop_alist([Variable_Param, Iteration_variable_Get29],
				 u_iteration_variable,
				 Iteration_variable),
	f_u_with_incr_or_decr_checking(
				       [ 
					 [ u_add_element_to_loop_alist,
					   
					   [ '#BQ',
					     
					     [ setf,
					       ['#COMMA', u_iteration_variable],
					       
					       [ (+),
						 
						 [ '#COMMA',
						   u_iteration_variable
						 ],
						 ['#COMMA', step]
					       ]
					     ]
					   ],
					   [quote, u_next]
					 ],
					 
					 [ case,
					   [intern, [symbol_name, u_from]],
					   
					   [ u_c46_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_c46_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1+', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1+', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ otherwise,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ]
					 ]
				       ],
				       Decr_checking_Ret),
	LetResult=[let, [[Iteration_variable_Get29, []]], Iteration_variable, Decr_checking_Ret],
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_incr, classof, claz_macro),
   set_opv(u_incr, compile_as, kw_operator),
   set_opv(u_incr, function, f_u_incr),
   DefMacroResult=u_incr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:11986 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    decr,
			    [variable, from, init, '&rest', args],
			    
			    [ let,
			      [final, step, ['iteration-variable', [gensym]]],
			      
			      [ '#BQ',
				
				[ let,
				  [[['#COMMA', 'iteration-variable'], []]],
				  
				  [ '#COMMA',
				    
				    [ 'record-in-loop-alist',
				      
				      [ '#BQ',
					
					[ ['#COMMA', variable],
					  ['#COMMA', 'iteration-variable']
					]
				      ],
				      [quote, 'iteration-variable']
				    ]
				  ],
				  
				  [ '#COMMA',
				    
				    [ 'with-incr-or-decr-checking',
				      
				      [ when,
					[<=, init, final],
					
					[ lerror,
					  '$STRING'("Cannot decrement from ~a downto ~a. Check the order of your arguments"),
					  init,
					  final
					]
				      ],
				      
				      [ 'add-element-to-loop-alist',
					
					[ '#BQ',
					  
					  [ setf,
					    ['#COMMA', 'iteration-variable'],
					    
					    [ (-),
					      ['#COMMA', 'iteration-variable'],
					      ['#COMMA', step]
					    ]
					  ]
					],
					[quote, next]
				      ],
				      
				      [ case,
					[intern, ['symbol-name', from]],
					
					[ '.in.',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (<),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ '.in',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (=),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ 'in.',
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (<),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', ['1-', init]]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ in,
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (=),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', ['1-', init]]
					      ]
					    ],
					    [quote, initializations]
					  ]
					],
					
					[ otherwise,
					  
					  [ 'record-in-loop-alist',
					    
					    [ '#BQ',
					      
					      [ (>),
						
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', final]
					      ]
					    ],
					    [quote, 'end-test']
					  ],
					  
					  [ 'add-element-to-loop-alist',
					    
					    [ '#BQ',
					      
					      [ 
						[ '#COMMA',
						  'iteration-variable'
						],
						['#COMMA', init]
					      ]
					    ],
					    [quote, initializations]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    t
			  ]).

% annotating U::DECR 
wl: lambda_def(defmacro,
	      u_decr,
	      f_u_decr,
	      [variable, u_from, u_init, c38_rest, args],
	      
	      [ progn,
		
		[ let,
		  [u_final, step, [u_iteration_variable, [gensym]]],
		  
		  [ '#BQ',
		    
		    [ let,
		      [[['#COMMA', u_iteration_variable], []]],
		      
		      [ '#COMMA',
			
			[ u_record_in_loop_alist,
			  
			  [ '#BQ',
			    
			    [ ['#COMMA', variable],
			      ['#COMMA', u_iteration_variable]
			    ]
			  ],
			  [quote, u_iteration_variable]
			]
		      ],
		      
		      [ '#COMMA',
			
			[ u_with_incr_or_decr_checking,
			  
			  [ when,
			    [<=, u_init, u_final],
			    
			    [ u_lerror,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('C'),
					 #\(a),
					 #\(n),
					 #\(n),
					 #\(o),
					 #\(t),
					 #\(' '),
					 #\(d),
					 #\(e),
					 #\(c),
					 #\(r),
					 #\(e),
					 #\(m),
					 #\(e),
					 #\(n),
					 #\(t),
					 #\(' '),
					 #\(f),
					 #\(r),
					 #\(o),
					 #\(m),
					 #\(' '),
					 #\(~),
					 #\(a),
					 #\(' '),
					 #\(d),
					 #\(o),
					 #\(w),
					 #\(n),
					 #\(t),
					 #\(o),
					 #\(' '),
					 #\(~),
					 #\(a),
					 #\('.'),
					 #\(' '),
					 #\('C'),
					 #\(h),
					 #\(e),
					 #\(c),
					 #\(k),
					 #\(' '),
					 #\(t),
					 #\(h),
					 #\(e),
					 #\(' '),
					 #\(o),
					 #\(r),
					 #\(d),
					 #\(e),
					 #\(r),
					 #\(' '),
					 #\(o),
					 #\(f),
					 #\(' '),
					 #\(y),
					 #\(o),
					 #\(u),
					 #\(r),
					 #\(' '),
					 #\(a),
					 #\(r),
					 #\(g),
					 #\(u),
					 #\(m),
					 #\(e),
					 #\(n),
					 #\(t),
					 #\(s)
				       ]),
			      u_init,
			      u_final
			    ]
			  ],
			  
			  [ u_add_element_to_loop_alist,
			    
			    [ '#BQ',
			      
			      [ setf,
				['#COMMA', u_iteration_variable],
				
				[ (-),
				  ['#COMMA', u_iteration_variable],
				  ['#COMMA', step]
				]
			      ]
			    ],
			    [quote, u_next]
			  ],
			  
			  [ case,
			    [intern, [symbol_name, u_from]],
			    
			    [ u_c46_in_c46,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (<),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_c46_in,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (=),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_in_c46,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (<),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', ['1-', u_init]]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ u_in,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (=),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', ['1-', u_init]]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ],
			    
			    [ otherwise,
			      
			      [ u_record_in_loop_alist,
				
				[ '#BQ',
				  
				  [ (>),
				    ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_final]
				  ]
				],
				[quote, u_end_test]
			      ],
			      
			      [ u_add_element_to_loop_alist,
				
				[ '#BQ',
				  
				  [ ['#COMMA', u_iteration_variable],
				    ['#COMMA', u_init]
				  ]
				],
				[quote, u_initializations]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		],
		t
	      ]).


% annotating U::DECR 
wl: arglist_info(u_decr,
		[variable, u_from, u_init, c38_rest, args],
		[variable, u_from, u_init, args],
		arginfo{ all:[variable, u_from, u_init],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[variable, u_from, u_init, args],
			 opt:0,
			 req:[variable, u_from, u_init],
			 rest:[args],
			 sublists:0,
			 whole:0
		       }).


% annotating U::DECR 
wl: init_args(3, u_decr).


% annotating U::DECR 
f_u_decr(Variable_Param, From_Param, Init_Param, Args_Param, FnResult) :-
	TLEnv3=[bv(variable, Variable_Param), bv(u_from, From_Param), bv(u_init, Init_Param), bv(args, Args_Param)],
	cl_gensym(Iteration_variable_Init),
	LEnv=[[bv(u_final, []), bv(step, []), bv(u_iteration_variable, Iteration_variable_Init)]|TLEnv3],
	get_var(LEnv, u_iteration_variable, Iteration_variable_Get25),
	f_u_record_in_loop_alist([Variable_Param, Iteration_variable_Get25],
				 u_iteration_variable,
				 Iteration_variable),
	f_u_with_incr_or_decr_checking(
				       [ 
					 [ when,
					   [<=, u_init, u_final],
					   
					   [ u_lerror,
					     '$ARRAY'([*],
						      claz_base_character,
						      
						      [ #\('C'),
							#\(a),
							#\(n),
							#\(n),
							#\(o),
							#\(t),
							#\(' '),
							#\(d),
							#\(e),
							#\(c),
							#\(r),
							#\(e),
							#\(m),
							#\(e),
							#\(n),
							#\(t),
							#\(' '),
							#\(f),
							#\(r),
							#\(o),
							#\(m),
							#\(' '),
							#\(~),
							#\(a),
							#\(' '),
							#\(d),
							#\(o),
							#\(w),
							#\(n),
							#\(t),
							#\(o),
							#\(' '),
							#\(~),
							#\(a),
							#\('.'),
							#\(' '),
							#\('C'),
							#\(h),
							#\(e),
							#\(c),
							#\(k),
							#\(' '),
							#\(t),
							#\(h),
							#\(e),
							#\(' '),
							#\(o),
							#\(r),
							#\(d),
							#\(e),
							#\(r),
							#\(' '),
							#\(o),
							#\(f),
							#\(' '),
							#\(y),
							#\(o),
							#\(u),
							#\(r),
							#\(' '),
							#\(a),
							#\(r),
							#\(g),
							#\(u),
							#\(m),
							#\(e),
							#\(n),
							#\(t),
							#\(s)
						      ]),
					     u_init,
					     u_final
					   ]
					 ],
					 
					 [ u_add_element_to_loop_alist,
					   
					   [ '#BQ',
					     
					     [ setf,
					       ['#COMMA', u_iteration_variable],
					       
					       [ (-),
						 
						 [ '#COMMA',
						   u_iteration_variable
						 ],
						 ['#COMMA', step]
					       ]
					     ]
					   ],
					   [quote, u_next]
					 ],
					 
					 [ case,
					   [intern, [symbol_name, u_from]],
					   
					   [ u_c46_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (<),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_c46_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (<),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1-', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1-', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ otherwise,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ]
					 ]
				       ],
				       Decr_checking_Ret),
	LetResult=[let, [[Iteration_variable_Get25, []]], Iteration_variable, Decr_checking_Ret],
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_decr, classof, claz_macro),
   set_opv(u_decr, compile_as, kw_operator),
   set_opv(u_decr, function, f_u_decr),
   DefMacroResult=u_decr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13433 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    after,
			    ['&rest', clauses],
			    
			    [ '#BQ',
			      
			      [ 'add-element-to-loop-alist',
				
				[ cons,
				  [quote, progn],
				  [quote, ['#COMMA', clauses]]
				],
				[quote, after]
			      ]
			    ]
			  ]).

% annotating U::AFTER 
wl: lambda_def(defmacro,
	      u_after,
	      f_u_after,
	      [c38_rest, u_clauses],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_add_element_to_loop_alist,
		    [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]],
		    [quote, u_after]
		  ]
		]
	      ]).


% annotating U::AFTER 
wl: arglist_info(u_after,
		[c38_rest, u_clauses],
		[u_clauses],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clauses],
			 opt:0,
			 req:0,
			 rest:[u_clauses],
			 sublists:0,
			 whole:0
		       }).


% annotating U::AFTER 
wl: init_args(rest_only, u_after).


% annotating U::AFTER 
f_u_after(Whole, FnResult) :-
	append([], Clauses_Param, Whole),
	TLEnv3=[bv(u_clauses, Clauses_Param)],
	get_var(TLEnv3, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, u_after]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_after, classof, claz_macro),
   set_opv(u_after, compile_as, kw_operator),
   set_opv(u_after, function, f_u_after),
   DefMacroResult=u_after.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'fetch-new-iteration-variable',
			    [],
			    
			    [ second,
			      [car, ['fetch-clauses', 'iteration-variable']]
			    ]
			  ]).

% annotating U::FETCH-NEW-ITERATION-VARIABLE 
wl: lambda_def(defun,
	      u_fetch_new_iteration_variable,
	      f_u_fetch_new_iteration_variable,
	      [],
	      [[second, [car, [u_fetch_clauses, u_iteration_variable]]]]).


% annotating U::FETCH-NEW-ITERATION-VARIABLE 
wl: arglist_info(u_fetch_new_iteration_variable,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FETCH-NEW-ITERATION-VARIABLE 
wl: init_args(exact_only, u_fetch_new_iteration_variable).


% annotating U::FETCH-NEW-ITERATION-VARIABLE 
f_u_fetch_new_iteration_variable(FnResult) :-
	Env=[],
	f_u_fetch_clauses(u_iteration_variable, Car_Param),
	cl_car(Car_Param, Second_Param),
	cl_second(Second_Param, Second_Ret),
	Second_Ret=FnResult.
:- set_opv(f_u_fetch_new_iteration_variable, classof, claz_function),
   set_opv(u_fetch_new_iteration_variable, compile_as, kw_function),
   set_opv(u_fetch_new_iteration_variable,
	   function,
	   f_u_fetch_new_iteration_variable),
   DefunResult=u_fetch_new_iteration_variable.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13622 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'fetch-old-iteration-variable',
			    [],
			    
			    [ first,
			      [car, ['fetch-clauses', 'iteration-variable']]
			    ]
			  ]).

% annotating U::FETCH-OLD-ITERATION-VARIABLE 
wl: lambda_def(defun,
	      u_fetch_old_iteration_variable,
	      f_u_fetch_old_iteration_variable,
	      [],
	      [[first, [car, [u_fetch_clauses, u_iteration_variable]]]]).


% annotating U::FETCH-OLD-ITERATION-VARIABLE 
wl: arglist_info(u_fetch_old_iteration_variable,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FETCH-OLD-ITERATION-VARIABLE 
wl: init_args(exact_only, u_fetch_old_iteration_variable).


% annotating U::FETCH-OLD-ITERATION-VARIABLE 
f_u_fetch_old_iteration_variable(FnResult) :-
	Env=[],
	f_u_fetch_clauses(u_iteration_variable, Car_Param),
	cl_car(Car_Param, Car_Param13),
	cl_car(Car_Param13, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_fetch_old_iteration_variable, classof, claz_function),
   set_opv(u_fetch_old_iteration_variable, compile_as, kw_function),
   set_opv(u_fetch_old_iteration_variable,
	   function,
	   f_u_fetch_old_iteration_variable),
   DefunResult=u_fetch_old_iteration_variable.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13714 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'record-in-loop-alist',
			    [element, key],
			    '$STRING'("Adds new assoc pairs in *loop-alist*."),
			    ['acons-setf', [list, element], key, '*loop-alist*']
			  ]).

% annotating U::RECORD-IN-LOOP-ALIST 
doc: doc_string(u_record_in_loop_alist,
	      _338324,
	      function,
	      "Adds new assoc pairs in *loop-alist*.").


% annotating U::RECORD-IN-LOOP-ALIST 
wl: lambda_def(defun,
	      u_record_in_loop_alist,
	      f_u_record_in_loop_alist,
	      [u_element, key],
	      [[u_acons_setf, [list, u_element], key, u_xx_loop_alist_xx]]).


% annotating U::RECORD-IN-LOOP-ALIST 
wl: arglist_info(u_record_in_loop_alist,
		[u_element, key],
		[Element_Param, Key_Param],
		arginfo{ all:[u_element, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_element, key],
			 opt:0,
			 req:[u_element, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RECORD-IN-LOOP-ALIST 
wl: init_args(exact_only, u_record_in_loop_alist).


% annotating U::RECORD-IN-LOOP-ALIST 
f_u_record_in_loop_alist(Element_Param, Key_Param, FnResult) :-
	Env=[bv(u_element, Element_Param), bv(key, Key_Param)],
	f_u_acons_setf([list, u_element],
		       key,
		       u_xx_loop_alist_xx,
		       Xx_loop_alist_xx),
	Xx_loop_alist_xx=FnResult.
:- set_opv(f_u_record_in_loop_alist, classof, claz_function),
   set_opv(u_record_in_loop_alist, compile_as, kw_function),
   set_opv(u_record_in_loop_alist, function, f_u_record_in_loop_alist),
   DefunResult=u_record_in_loop_alist.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13847 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-element-to-loop-alist',
			    [element, 'clause-key'],
			    '$STRING'("Adds elements to a particular assoc sublist."),
			    
			    [ cond,
			      
			      [ [null, [rassoc, 'clause-key', '*loop-alist*']],
				['record-in-loop-alist', element, 'clause-key']
			      ],
			      
			      [ t,
				
				[ rplaca,
				  [rassoc, 'clause-key', '*loop-alist*'],
				  
				  [ cons,
				    element,
				    [car, [rassoc, 'clause-key', '*loop-alist*']]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::ADD-ELEMENT-TO-LOOP-ALIST 
doc: doc_string(u_add_element_to_loop_alist,
	      _117356,
	      function,
	      "Adds elements to a particular assoc sublist.").


% annotating U::ADD-ELEMENT-TO-LOOP-ALIST 
wl: lambda_def(defun,
	      u_add_element_to_loop_alist,
	      f_u_add_element_to_loop_alist,
	      [u_element, u_clause_key],
	      
	      [ 
		[ cond,
		  
		  [ [null, [rassoc, u_clause_key, u_xx_loop_alist_xx]],
		    [u_record_in_loop_alist, u_element, u_clause_key]
		  ],
		  
		  [ t,
		    
		    [ rplaca,
		      [rassoc, u_clause_key, u_xx_loop_alist_xx],
		      
		      [ cons,
			u_element,
			[car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::ADD-ELEMENT-TO-LOOP-ALIST 
wl: arglist_info(u_add_element_to_loop_alist,
		[u_element, u_clause_key],
		[Element_Param, Clause_key_Param],
		arginfo{ all:[u_element, u_clause_key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_element, u_clause_key],
			 opt:0,
			 req:[u_element, u_clause_key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-ELEMENT-TO-LOOP-ALIST 
wl: init_args(exact_only, u_add_element_to_loop_alist).


% annotating U::ADD-ELEMENT-TO-LOOP-ALIST 
f_u_add_element_to_loop_alist(Element_Param, Clause_key_Param, FnResult) :-
	Env=[bv(u_element, Element_Param), bv(u_clause_key, Clause_key_Param)],
	get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get, [], IFTEST),
	(   IFTEST==[]
	->  f_u_record_in_loop_alist(Element_Param,
				     Clause_key_Param,
				     TrueResult),
	    FnResult=TrueResult
	;   get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get21),
	    cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get21, [], Rplaca_Param),
	    get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get24),
	    cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get24, [], Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    _117900=[Element_Param|Car_Ret],
	    cl_rplaca(Rplaca_Param, _117900, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_element_to_loop_alist, classof, claz_function),
   set_opv(u_add_element_to_loop_alist, compile_as, kw_function),
   set_opv(u_add_element_to_loop_alist, function, f_u_add_element_to_loop_alist),
   DefunResult=u_add_element_to_loop_alist.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14173 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-elements-to-end-of-clause',
			    ['clause-key', '&rest', elements],
			    
			    [ dolist,
			      [element, elements],
			      
			      [ 'add-element-to-end-of-loop-alist',
				element,
				'clause-key'
			      ]
			    ]
			  ]).

% annotating U::ADD-ELEMENTS-TO-END-OF-CLAUSE 
wl: lambda_def(defun,
	      u_add_elements_to_end_of_clause,
	      f_u_add_elements_to_end_of_clause,
	      [u_clause_key, c38_rest, u_elements],
	      
	      [ 
		[ dolist,
		  [u_element, u_elements],
		  [u_add_element_to_end_of_loop_alist, u_element, u_clause_key]
		]
	      ]).


% annotating U::ADD-ELEMENTS-TO-END-OF-CLAUSE 
wl: arglist_info(u_add_elements_to_end_of_clause,
		[u_clause_key, c38_rest, u_elements],
		[u_clause_key, u_elements],
		arginfo{ all:[u_clause_key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clause_key, u_elements],
			 opt:0,
			 req:[u_clause_key],
			 rest:[u_elements],
			 sublists:0,
			 whole:0
		       }).


% annotating U::ADD-ELEMENTS-TO-END-OF-CLAUSE 
wl: init_args(1, u_add_elements_to_end_of_clause).


% annotating U::ADD-ELEMENTS-TO-END-OF-CLAUSE 
f_u_add_elements_to_end_of_clause(Clause_key_Param, Elements_Param, FnResult) :-
	Env=[bv(u_clause_key, Clause_key_Param), bv(u_elements, Elements_Param)],
	get_var(Env, u_elements, Elements_Get),
	BV=bv(u_element, Ele),
	Env=[BV|Env],
	forall(member(Ele, Elements_Get),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env, u_element, Element_Get),
		 f_u_add_element_to_end_of_loop_alist(Element_Get,
						      Clause_key_Param,
						      Loop_alist_Ret)
	       )),
	Loop_alist_Ret=FnResult.
:- set_opv(f_u_add_elements_to_end_of_clause, classof, claz_function),
   set_opv(u_add_elements_to_end_of_clause, compile_as, kw_function),
   set_opv(u_add_elements_to_end_of_clause,
	   function,
	   f_u_add_elements_to_end_of_clause),
   DefunResult=u_add_elements_to_end_of_clause.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14324 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-elements-to-clause',
			    ['clause-key', '&rest', elements],
			    
			    [ dolist,
			      [element, elements],
			      
			      [ 'add-element-to-loop-alist',
				element,
				'clause-key'
			      ]
			    ]
			  ]).

% annotating U::ADD-ELEMENTS-TO-CLAUSE 
wl: lambda_def(defun,
	      u_add_elements_to_clause,
	      f_u_add_elements_to_clause,
	      [u_clause_key, c38_rest, u_elements],
	      
	      [ 
		[ dolist,
		  [u_element, u_elements],
		  [u_add_element_to_loop_alist, u_element, u_clause_key]
		]
	      ]).


% annotating U::ADD-ELEMENTS-TO-CLAUSE 
wl: arglist_info(u_add_elements_to_clause,
		[u_clause_key, c38_rest, u_elements],
		[u_clause_key, u_elements],
		arginfo{ all:[u_clause_key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clause_key, u_elements],
			 opt:0,
			 req:[u_clause_key],
			 rest:[u_elements],
			 sublists:0,
			 whole:0
		       }).


% annotating U::ADD-ELEMENTS-TO-CLAUSE 
wl: init_args(1, u_add_elements_to_clause).


% annotating U::ADD-ELEMENTS-TO-CLAUSE 
f_u_add_elements_to_clause(Clause_key_Param, Elements_Param, FnResult) :-
	Env=[bv(u_clause_key, Clause_key_Param), bv(u_elements, Elements_Param)],
	get_var(Env, u_elements, Elements_Get),
	BV=bv(u_element, Ele),
	Env=[BV|Env],
	forall(member(Ele, Elements_Get),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env, u_element, Element_Get),
		 f_u_add_element_to_loop_alist(Element_Get,
					       Clause_key_Param,
					       Loop_alist_Ret)
	       )),
	Loop_alist_Ret=FnResult.
:- set_opv(f_u_add_elements_to_clause, classof, claz_function),
   set_opv(u_add_elements_to_clause, compile_as, kw_function),
   set_opv(u_add_elements_to_clause, function, f_u_add_elements_to_clause),
   DefunResult=u_add_elements_to_clause.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'substitute-iteration-variable',
			    [list],
			    '$STRING'("Substitutes iteration variables with those given by gensym."),
			    
			    [ 'let*',
			      
			      [ 
				[ 'saved-iteration-variable-clause',
				  
				  [ rassoc,
				    [quote, 'iteration-variable'],
				    '*loop-alist*'
				  ]
				],
				
				[ 'new-iteration-variable-symbol',
				  ['fetch-new-iteration-variable']
				],
				
				[ 'old-iteration-variable-symbol',
				  ['fetch-old-iteration-variable']
				],
				
				[ 'secured-list',
				  
				  [ remove,
				    
				    [ rassoc,
				      [quote, 'iteration-variable'],
				      '*loop-alist*'
				    ],
				    list
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ 
				  [ null,
				    
				    [ or,
				      'new-iteration-variable-symbol',
				      'old-iteration-variable-symbol'
				    ]
				  ],
				  
				  [ lerror,
				    '$STRING'("No iteration variable defined")
				  ]
				],
				
				[ t,
				  
				  [ cons,
				    'saved-iteration-variable-clause',
				    
				    [ subst,
				      'new-iteration-variable-symbol',
				      'old-iteration-variable-symbol',
				      'secured-list'
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::SUBSTITUTE-ITERATION-VARIABLE 
doc: doc_string(u_substitute_iteration_variable,
	      _118346,
	      function,
	      "Substitutes iteration variables with those given by gensym.").


% annotating U::SUBSTITUTE-ITERATION-VARIABLE 
wl: lambda_def(defun,
	      u_substitute_iteration_variable,
	      f_u_substitute_iteration_variable,
	      [list],
	      
	      [ 
		[ let_xx,
		  
		  [ 
		    [ u_saved_iteration_variable_clause,
		      [rassoc, [quote, u_iteration_variable], u_xx_loop_alist_xx]
		    ],
		    
		    [ u_new_iteration_variable_symbol,
		      [u_fetch_new_iteration_variable]
		    ],
		    
		    [ u_old_iteration_variable_symbol,
		      [u_fetch_old_iteration_variable]
		    ],
		    
		    [ u_secured_list,
		      
		      [ remove,
			
			[ rassoc,
			  [quote, u_iteration_variable],
			  u_xx_loop_alist_xx
			],
			list
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ 
		      [ null,
			
			[ or,
			  u_new_iteration_variable_symbol,
			  u_old_iteration_variable_symbol
			]
		      ],
		      
		      [ u_lerror,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('N'),
				   #\(o),
				   #\(' '),
				   #\(i),
				   #\(t),
				   #\(e),
				   #\(r),
				   #\(a),
				   #\(t),
				   #\(i),
				   #\(o),
				   #\(n),
				   #\(' '),
				   #\(v),
				   #\(a),
				   #\(r),
				   #\(i),
				   #\(a),
				   #\(b),
				   #\(l),
				   #\(e),
				   #\(' '),
				   #\(d),
				   #\(e),
				   #\(f),
				   #\(i),
				   #\(n),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ t,
		      
		      [ cons,
			u_saved_iteration_variable_clause,
			
			[ subst,
			  u_new_iteration_variable_symbol,
			  u_old_iteration_variable_symbol,
			  u_secured_list
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::SUBSTITUTE-ITERATION-VARIABLE 
wl: arglist_info(u_substitute_iteration_variable,
		[list],
		[List_Param],
		arginfo{ all:[list],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[list],
			 opt:0,
			 req:[list],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBSTITUTE-ITERATION-VARIABLE 
wl: init_args(exact_only, u_substitute_iteration_variable).


% annotating U::SUBSTITUTE-ITERATION-VARIABLE 
f_u_substitute_iteration_variable(List_Param, FnResult) :-
	Env=[bv(list, List_Param)],
	get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(u_iteration_variable,
		  Xx_loop_alist_xx_Get,
		  [],
		  Saved_iteration_variable_clause_Init),
	LEnv=[[bv(u_saved_iteration_variable_clause, Saved_iteration_variable_clause_Init)]|Env],
	f_u_fetch_new_iteration_variable(New_iteration_variable_symbol_Init),
	LEnv16=[[bv(u_new_iteration_variable_symbol, New_iteration_variable_symbol_Init)]|LEnv],
	f_u_fetch_old_iteration_variable(Old_iteration_variable_symbol_Init),
	Env=[[bv(u_old_iteration_variable_symbol, Old_iteration_variable_symbol_Init)]|LEnv16],
	get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get24),
	cl_rassoc(u_iteration_variable, Xx_loop_alist_xx_Get24, [], Remove_Param),
	cl_remove(Remove_Param, List_Param, Secured_list_Init),
	Env=[[bv(u_secured_list, Secured_list_Init)]|Env],
	(   get_var(Env,
		    u_new_iteration_variable_symbol,
		    New_iteration_variable_symbol_Get),
	    New_iteration_variable_symbol_Get\==[],
	    IFTEST=New_iteration_variable_symbol_Get
	->  true
	;   get_var(Env,
		    u_old_iteration_variable_symbol,
		    Old_iteration_variable_symbol_Get),
	    IFTEST=Old_iteration_variable_symbol_Get
	),
	(   IFTEST==[]
	->  f_u_lerror('$ARRAY'([*],
				claz_base_character,
				
				[ #\('N'),
				  #\(o),
				  #\(' '),
				  #\(i),
				  #\(t),
				  #\(e),
				  #\(r),
				  #\(a),
				  #\(t),
				  #\(i),
				  #\(o),
				  #\(n),
				  #\(' '),
				  #\(v),
				  #\(a),
				  #\(r),
				  #\(i),
				  #\(a),
				  #\(b),
				  #\(l),
				  #\(e),
				  #\(' '),
				  #\(d),
				  #\(e),
				  #\(f),
				  #\(i),
				  #\(n),
				  #\(e),
				  #\(d)
				]),
		       [],
		       TrueResult),
	    FnResult=TrueResult
	;   get_var(Env,
		    u_new_iteration_variable_symbol,
		    New_iteration_variable_symbol_Get35),
	    get_var(Env,
		    u_old_iteration_variable_symbol,
		    Old_iteration_variable_symbol_Get36),
	    get_var(Env,
		    u_saved_iteration_variable_clause,
		    Saved_iteration_variable_clause_Get),
	    get_var(Env, u_secured_list, Secured_list_Get),
	    cl_subst(New_iteration_variable_symbol_Get35,
		     Old_iteration_variable_symbol_Get36,
		     Secured_list_Get,
		     Subst_Ret),
	    FnResult=[Saved_iteration_variable_clause_Get|Subst_Ret]
	).
:- set_opv(f_u_substitute_iteration_variable, classof, claz_function),
   set_opv(u_substitute_iteration_variable, compile_as, kw_function),
   set_opv(u_substitute_iteration_variable,
	   function,
	   f_u_substitute_iteration_variable),
   DefunResult=u_substitute_iteration_variable.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; Worring about the effect of (subst..) on ((#) . iteration-variable) ",
				     9,
				     14598)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; the hard way  (sublis ..) may work better",
				     9,
				     14678)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; should not be required -fr", 51, 15267)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'iteration-variable-exists-p',
			    [],
			    ['fetch-clauses', 'iteration-variable']
			  ]).

% annotating U::ITERATION-VARIABLE-EXISTS-P 
wl: lambda_def(defun,
	      u_iteration_variable_exists_p,
	      f_u_iteration_variable_exists_p,
	      [],
	      [[u_fetch_clauses, u_iteration_variable]]).


% annotating U::ITERATION-VARIABLE-EXISTS-P 
wl: arglist_info(u_iteration_variable_exists_p,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ITERATION-VARIABLE-EXISTS-P 
wl: init_args(exact_only, u_iteration_variable_exists_p).


% annotating U::ITERATION-VARIABLE-EXISTS-P 
f_u_iteration_variable_exists_p(FnResult) :-
	Env=[],
	f_u_fetch_clauses(u_iteration_variable, Fetch_clauses_Ret),
	Fetch_clauses_Ret=FnResult.
:- set_opv(f_u_iteration_variable_exists_p, classof, claz_function),
   set_opv(u_iteration_variable_exists_p, compile_as, kw_function),
   set_opv(u_iteration_variable_exists_p,
	   function,
	   f_u_iteration_variable_exists_p),
   DefunResult=u_iteration_variable_exists_p.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    yloop,
			    ['&rest', clauses],
			    [setf, '*loop-alist*', []],
			    [mapcar, [quote, eval], clauses],
			    
			    [ when,
			      ['iteration-variable-exists-p'],
			      
			      [ setf,
				'*loop-alist*',
				
				[ 'substitute-iteration-variable',
				  '*loop-alist*'
				]
			      ]
			    ],
			    
			    [ let,
			      
			      [ [dos, ['fetch-clauses', do]],
				[afters, ['fetch-clauses', after]],
				['end-tests', ['fetch-clauses', 'end-test']],
				[bindings, ['fetch-clauses', initializations]],
				[result, ['fetch-clauses', result]],
				[nexts, ['fetch-clauses', next]],
				[befores, ['fetch-clauses', before]],
				['middle-stuff', ['fetch-clauses', middle]],
				['front-stuff', ['fetch-clauses', front]],
				['end-stuff', ['fetch-clauses', end]],
				['block-label', [gensym]]
			      ],
			      [setf, '*loop-alist*', []],
			      
			      [ '#BQ',
				
				[ 'unwind-protect',
				  
				  [ block,
				    ['#COMMA', 'block-label'],
				    
				    [ 'let*',
				      [['#BQ-COMMA-ELIPSE', bindings]],
				      ['#BQ-COMMA-ELIPSE', befores],
				      ['#BQ-COMMA-ELIPSE', 'front-stuff'],
				      
				      [ if,
					[or, ['#BQ-COMMA-ELIPSE', 'end-tests']],
					
					[ 'return-from',
					  ['#COMMA', 'block-label'],
					  
					  [ '#BQ-COMMA-ELIPSE',
					    [or, result, [quote, [[]]]]
					  ]
					]
				      ],
				      
				      [ tagbody,
					loop,
					['#BQ-COMMA-ELIPSE', dos],
					['#BQ-COMMA-ELIPSE', 'middle-stuff'],
					['#BQ-COMMA-ELIPSE', nexts],
					
					[ if,
					  
					  [ or,
					    ['#BQ-COMMA-ELIPSE', 'end-tests']
					  ],
					  [],
					  [go, loop]
					]
				      ],
				      ['#BQ-COMMA-ELIPSE', afters],
				      ['#BQ-COMMA-ELIPSE', 'end-stuff'],
				      
				      [ 'return-from',
					['#COMMA', 'block-label'],
					
					[ '#BQ-COMMA-ELIPSE',
					  [or, result, [quote, [[]]]]
					]
				      ]
				    ]
				  ],
				  ['#COMMA', ['clear-loop-alist']]
				]
			      ]
			    ]
			  ]).

% annotating U::YLOOP 
wl: lambda_def(defmacro,
	      u_yloop,
	      f_u_yloop,
	      [c38_rest, u_clauses],
	      
	      [ progn,
		[setf, u_xx_loop_alist_xx, []],
		[mapcar, [quote, eval], u_clauses],
		
		[ when,
		  [u_iteration_variable_exists_p],
		  
		  [ setf,
		    u_xx_loop_alist_xx,
		    [u_substitute_iteration_variable, u_xx_loop_alist_xx]
		  ]
		],
		
		[ let,
		  
		  [ [u_dos, [u_fetch_clauses, do]],
		    [u_afters, [u_fetch_clauses, u_after]],
		    [u_end_tests, [u_fetch_clauses, u_end_test]],
		    [bindings, [u_fetch_clauses, u_initializations]],
		    [u_result, [u_fetch_clauses, u_result]],
		    [u_nexts, [u_fetch_clauses, u_next]],
		    [u_befores, [u_fetch_clauses, u_before]],
		    [u_middle_stuff, [u_fetch_clauses, u_middle]],
		    [u_front_stuff, [u_fetch_clauses, u_front]],
		    [u_end_stuff, [u_fetch_clauses, end]],
		    [u_block_label, [gensym]]
		  ],
		  [setf, u_xx_loop_alist_xx, []],
		  
		  [ '#BQ',
		    
		    [ unwind_protect,
		      
		      [ block,
			['#COMMA', u_block_label],
			
			[ let_xx,
			  [['#BQ-COMMA-ELIPSE', bindings]],
			  ['#BQ-COMMA-ELIPSE', u_befores],
			  ['#BQ-COMMA-ELIPSE', u_front_stuff],
			  
			  [ if,
			    [or, ['#BQ-COMMA-ELIPSE', u_end_tests]],
			    
			    [ return_from,
			      ['#COMMA', u_block_label],
			      ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]
			    ]
			  ],
			  
			  [ tagbody,
			    loop,
			    ['#BQ-COMMA-ELIPSE', u_dos],
			    ['#BQ-COMMA-ELIPSE', u_middle_stuff],
			    ['#BQ-COMMA-ELIPSE', u_nexts],
			    
			    [ if,
			      [or, ['#BQ-COMMA-ELIPSE', u_end_tests]],
			      [],
			      [go, loop]
			    ]
			  ],
			  ['#BQ-COMMA-ELIPSE', u_afters],
			  ['#BQ-COMMA-ELIPSE', u_end_stuff],
			  
			  [ return_from,
			    ['#COMMA', u_block_label],
			    ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]
			  ]
			]
		      ],
		      ['#COMMA', [u_clear_loop_alist]]
		    ]
		  ]
		]
	      ]).


% annotating U::YLOOP 
wl: arglist_info(u_yloop,
		[c38_rest, u_clauses],
		[u_clauses],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_clauses],
			 opt:0,
			 req:0,
			 rest:[u_clauses],
			 sublists:0,
			 whole:0
		       }).


% annotating U::YLOOP 
wl: init_args(rest_only, u_yloop).


% annotating U::YLOOP 
f_u_yloop(Whole, FnResult) :-
	append([], Clauses_Param, Whole),
	Env=[bv(u_clauses, Clauses_Param)],
	set_place(Env, setf, [value, u_xx_loop_alist_xx], [[]], Setf_R),
	get_var(Env, u_clauses, Clauses_Get),
	cl_mapcar(eval, [Clauses_Get], Mapcar_Ret),
	f_u_iteration_variable_exists_p(IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	    f_u_substitute_iteration_variable(Xx_loop_alist_xx_Get,
					      Iteration_variable_Ret),
	    set_place(Env,
		      setf,
		      [value, u_xx_loop_alist_xx],
		      [Iteration_variable_Ret],
		      Setf_R19),
	    _119386=Setf_R19
	;   _119386=[]
	),
	f_u_fetch_clauses(do, Dos_Init),
	f_u_fetch_clauses(u_after, Afters_Init),
	f_u_fetch_clauses(u_end_test, End_tests_Init),
	f_u_fetch_clauses(u_initializations, Bindings_Init),
	f_u_fetch_clauses(u_result, Result_Init),
	f_u_fetch_clauses(u_next, Nexts_Init),
	f_u_fetch_clauses(u_before, Befores_Init),
	f_u_fetch_clauses(u_middle, Middle_stuff_Init),
	f_u_fetch_clauses(u_front, Front_stuff_Init),
	f_u_fetch_clauses(end, End_stuff_Init),
	cl_gensym(Block_label_Init),
	Env=[[bv(u_dos, Dos_Init), bv(u_afters, Afters_Init), bv(u_end_tests, End_tests_Init), bv(bindings, Bindings_Init), bv(u_result, Result_Init), bv(u_nexts, Nexts_Init), bv(u_befores, Befores_Init), bv(u_middle_stuff, Middle_stuff_Init), bv(u_front_stuff, Front_stuff_Init), bv(u_end_stuff, End_stuff_Init), bv(u_block_label, Block_label_Init)]|Env],
	set_place(Env, setf, [value, u_xx_loop_alist_xx], [[]], Setf_R34),
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_befores, Befores_Get),
	get_var(Env, u_block_label, Block_label_Get42),
	get_var(Env, u_end_tests, End_tests_Get),
	get_var(Env, u_front_stuff, Front_stuff_Get),
	(   get_var(Env, u_result, Result_Get),
	    Result_Get\==[],
	    CDR65=Result_Get
	->  true
	;   CDR65=[[]]
	),
	get_var(Env, u_dos, Dos_Get),
	get_var(Env, u_end_tests, End_tests_Get48),
	get_var(Env, u_middle_stuff, Middle_stuff_Get),
	get_var(Env, u_nexts, Nexts_Get),
	bq_append(Nexts_Get,
		  [[if, [or|End_tests_Get48], [], [go, loop]]],
		  Bq_append_Ret),
	bq_append(Middle_stuff_Get, Bq_append_Ret, Bq_append_Ret59),
	bq_append([loop|Dos_Get], Bq_append_Ret59, Bq_append_Ret60),
	get_var(Env, u_afters, Afters_Get),
	get_var(Env, u_block_label, Block_label_Get51),
	get_var(Env, u_end_stuff, End_stuff_Get),
	(   get_var(Env, u_result, Result_Get52),
	    Result_Get52\==[],
	    CDR=Result_Get52
	->  true
	;   CDR=[[]]
	),
	bq_append(End_stuff_Get,
		  [[return_from, Block_label_Get51|CDR]],
		  Bq_append_Ret61),
	bq_append([[tagbody|Bq_append_Ret60]|Afters_Get],
		  Bq_append_Ret61,
		  Bq_append_Ret63),
	bq_append(Front_stuff_Get,
		  
		  [ 
		    [ if,
		      [or|End_tests_Get],
		      [return_from, Block_label_Get42|CDR65]
		    ]
		  | Bq_append_Ret63
		  ],
		  Bq_append_Ret64),
	bq_append([Bindings_Get|Befores_Get], Bq_append_Ret64, Bq_append_Ret66),
	f_u_clear_loop_alist(Loop_alist_Ret),
	LetResult=[unwind_protect, [block, Block_label_Get42, [let_xx|Bq_append_Ret66]], Loop_alist_Ret],
	LetResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yloop, classof, claz_macro),
   set_opv(u_yloop, compile_as, kw_operator),
   set_opv(u_yloop, function, f_u_yloop),
   DefMacroResult=u_yloop.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; you have an iteration variuable to subsitute",
				     5,
				     15672)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; if there are multiple end-test's, accomidate them",
				     5,
				     16255)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; with an or wrapped around the end-test    ",
				     5,
				     16312)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("if you have nothing to do, jump -fr",
				     14,
				     16488)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; return results or nil              ",
				     14,
				     16809)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:16948 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    maximize,
			    [expression],
			    
			    [ 'add-element-to-end-of-loop-alist',
			      
			      [ '#BQ',
				['maximum-variable', ['#COMMA', expression]]
			      ],
			      [quote, initializations]
			    ],
			    
			    [ 'add-element-to-loop-alist',
			      
			      [ '#BQ',
				
				[ if,
				  [>, ['#COMMA', expression], 'maximum-variable'],
				  
				  [ setf,
				    'maximum-variable',
				    ['#COMMA', expression]
				  ]
				]
			      ],
			      [quote, 'middle-stuff']
			    ],
			    [result, 'maximum-variable'],
			    t
			  ]).

% annotating U::MAXIMIZE 
wl: lambda_def(defmacro,
	      u_maximize,
	      f_u_maximize,
	      [u_expression],
	      
	      [ progn,
		
		[ u_add_element_to_end_of_loop_alist,
		  ['#BQ', [u_maximum_variable, ['#COMMA', u_expression]]],
		  [quote, u_initializations]
		],
		
		[ u_add_element_to_loop_alist,
		  
		  [ '#BQ',
		    
		    [ if,
		      [>, ['#COMMA', u_expression], u_maximum_variable],
		      [setf, u_maximum_variable, ['#COMMA', u_expression]]
		    ]
		  ],
		  [quote, u_middle_stuff]
		],
		[u_result, u_maximum_variable],
		t
	      ]).


% annotating U::MAXIMIZE 
wl: arglist_info(u_maximize,
		[u_expression],
		[Expression_Param],
		arginfo{ all:[u_expression],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_expression],
			 opt:0,
			 req:[u_expression],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAXIMIZE 
wl: init_args(exact_only, u_maximize).


% annotating U::MAXIMIZE 
f_u_maximize(Expression_Param, FnResult) :-
	TLEnv3=[bv(u_expression, Expression_Param)],
	f_u_add_element_to_end_of_loop_alist(
					     [ u_maximum_variable,
					       Expression_Param
					     ],
					     u_initializations,
					     Initializations),
	f_u_add_element_to_loop_alist(
				      [ if,
					
					[ (>),
					  Expression_Param,
					  u_maximum_variable
					],
					
					[ setf,
					  u_maximum_variable,
					  Expression_Param
					]
				      ],
				      u_middle_stuff,
				      Middle_stuff),
	get_var(TLEnv3, u_maximum_variable, Maximum_variable_Get),
	f_u_result(Maximum_variable_Get, Result_Ret),
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_maximize, classof, claz_macro),
   set_opv(u_maximize, compile_as, kw_operator),
   set_opv(u_maximize, function, f_u_maximize),
   DefMacroResult=u_maximize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-element-to-end-of-loop-alist',
			    [element, 'clause-key'],
			    '$STRING'("Adds elements to a particular assoc sublist."),
			    
			    [ cond,
			      
			      [ [null, [rassoc, 'clause-key', '*loop-alist*']],
				['record-in-loop-alist', element, 'clause-key']
			      ],
			      
			      [ t,
				
				[ rplaca,
				  [rassoc, 'clause-key', '*loop-alist*'],
				  
				  [ reverse,
				    
				    [ cons,
				      element,
				      
				      [ car,
					[rassoc, 'clause-key', '*loop-alist*']
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST 
doc: doc_string(u_add_element_to_end_of_loop_alist,
	      _117404,
	      function,
	      "Adds elements to a particular assoc sublist.").


% annotating U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST 
wl: lambda_def(defun,
	      u_add_element_to_end_of_loop_alist,
	      f_u_add_element_to_end_of_loop_alist,
	      [u_element, u_clause_key],
	      
	      [ 
		[ cond,
		  
		  [ [null, [rassoc, u_clause_key, u_xx_loop_alist_xx]],
		    [u_record_in_loop_alist, u_element, u_clause_key]
		  ],
		  
		  [ t,
		    
		    [ rplaca,
		      [rassoc, u_clause_key, u_xx_loop_alist_xx],
		      
		      [ reverse,
			
			[ cons,
			  u_element,
			  [car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST 
wl: arglist_info(u_add_element_to_end_of_loop_alist,
		[u_element, u_clause_key],
		[Element_Param, Clause_key_Param],
		arginfo{ all:[u_element, u_clause_key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_element, u_clause_key],
			 opt:0,
			 req:[u_element, u_clause_key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST 
wl: init_args(exact_only, u_add_element_to_end_of_loop_alist).


% annotating U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST 
f_u_add_element_to_end_of_loop_alist(Element_Param, Clause_key_Param, FnResult) :-
	Env=[bv(u_element, Element_Param), bv(u_clause_key, Clause_key_Param)],
	get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get, [], IFTEST),
	(   IFTEST==[]
	->  f_u_record_in_loop_alist(Element_Param,
				     Clause_key_Param,
				     TrueResult),
	    FnResult=TrueResult
	;   get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get21),
	    cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get21, [], Rplaca_Param),
	    get_var(Env, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get24),
	    cl_rassoc(Clause_key_Param, Xx_loop_alist_xx_Get24, [], Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    Reverse_Param=[Element_Param|Car_Ret],
	    cl_reverse(Reverse_Param, Reverse_Ret),
	    cl_rplaca(Rplaca_Param, Reverse_Ret, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_element_to_end_of_loop_alist, classof, claz_function),
   set_opv(u_add_element_to_end_of_loop_alist, compile_as, kw_function),
   set_opv(u_add_element_to_end_of_loop_alist,
	   function,
	   f_u_add_element_to_end_of_loop_alist),
   DefunResult=u_add_element_to_end_of_loop_alist.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17648 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'define-and-rename-loop-locals',
			    ['where-to-add', 'arg-list', result, body],
			    
			    [ when,
			      'arg-list',
			      
			      [ dolist,
				[clause, 'arg-list'],
				
				[ 'let*',
				  [[var, []], ['new-var', [gensym]]],
				  
				  [ if,
				    [listp, clause],
				    [setq, var, [car, clause]],
				    [setq, var, clause]
				  ],
				  
				  [ setf,
				    'arg-list',
				    [subst, 'new-var', var, 'arg-list']
				  ],
				  [setf, body, [subst, 'new-var', var, body]],
				  [setf, result, [subst, 'new-var', var, result]]
				]
			      ]
			    ],
			    
			    [ if,
			      result,
			      
			      [ 'add-element-to-loop-alist',
				[cons, [quote, progn], [list, result]],
				[quote, result]
			      ]
			    ],
			    ['add-element-to-loop-alist', body, 'where-to-add'],
			    
			    [ when,
			      'arg-list',
			      
			      [ dolist,
				['new-var', 'arg-list'],
				
				[ 'add-element-to-loop-alist',
				  'new-var',
				  [quote, initializations]
				]
			      ]
			    ]
			  ]).

% annotating U::DEFINE-AND-RENAME-LOOP-LOCALS 
wl: lambda_def(defun,
	      u_define_and_rename_loop_locals,
	      f_u_define_and_rename_loop_locals,
	      [u_where_to_add, u_arg_list, u_result, u_body],
	      
	      [ 
		[ when,
		  u_arg_list,
		  
		  [ dolist,
		    [u_clause, u_arg_list],
		    
		    [ let_xx,
		      [[u_var, []], [u_new_var, [gensym]]],
		      
		      [ if,
			[listp, u_clause],
			[setq, u_var, [car, u_clause]],
			[setq, u_var, u_clause]
		      ],
		      [setf, u_arg_list, [subst, u_new_var, u_var, u_arg_list]],
		      [setf, u_body, [subst, u_new_var, u_var, u_body]],
		      [setf, u_result, [subst, u_new_var, u_var, u_result]]
		    ]
		  ]
		],
		
		[ if,
		  u_result,
		  
		  [ u_add_element_to_loop_alist,
		    [cons, [quote, progn], [list, u_result]],
		    [quote, u_result]
		  ]
		],
		[u_add_element_to_loop_alist, u_body, u_where_to_add],
		
		[ when,
		  u_arg_list,
		  
		  [ dolist,
		    [u_new_var, u_arg_list],
		    
		    [ u_add_element_to_loop_alist,
		      u_new_var,
		      [quote, u_initializations]
		    ]
		  ]
		]
	      ]).


% annotating U::DEFINE-AND-RENAME-LOOP-LOCALS 
wl: arglist_info(u_define_and_rename_loop_locals,
		[u_where_to_add, u_arg_list, u_result, u_body],
		[Where_to_add_Param, Arg_list_Param, Result_Param, Body_Param],
		arginfo{ all:[u_where_to_add, u_arg_list, u_result, u_body],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_where_to_add, u_arg_list, u_result, u_body],
			 opt:0,
			 req:[u_where_to_add, u_arg_list, u_result, u_body],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DEFINE-AND-RENAME-LOOP-LOCALS 
wl: init_args(exact_only, u_define_and_rename_loop_locals).


% annotating U::DEFINE-AND-RENAME-LOOP-LOCALS 
f_u_define_and_rename_loop_locals(Where_to_add_Param, Arg_list_Param, Result_Param, Body_Param, FnResult) :-
	Env=[bv(u_where_to_add, Where_to_add_Param), bv(u_arg_list, Arg_list_Param), bv(u_result, Result_Param), bv(u_body, Body_Param)],
	get_var(Env, u_arg_list, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_arg_list, Arg_list_Get21),
	    BV=bv(u_clause, Ele),
	    Env2=[BV|Env],
	    forall(member(Ele, Arg_list_Get21),
		   ( nb_setarg(2, BV, Ele),
		     LEnv=[[bv(u_var, [])]|Env2],
		     cl_gensym(New_var_Init),
		     Setf_Env=[[bv(u_new_var, New_var_Init)]|LEnv],
		     get_var(Setf_Env, u_clause, Clause_Get),
		     (   is_listp(Clause_Get)
		     ->  get_var(Setf_Env, u_clause, Clause_Get32),
			 cl_car(Clause_Get32, TrueResult),
			 set_var(Setf_Env, u_var, TrueResult),
			 _118988=TrueResult
		     ;   get_var(Setf_Env, u_clause, Clause_Get33),
			 set_var(Setf_Env, u_var, Clause_Get33),
			 _118988=Clause_Get33
		     ),
		     get_var(Setf_Env, u_arg_list, Arg_list_Get38),
		     get_var(Setf_Env, u_new_var, New_var_Get),
		     get_var(Setf_Env, u_var, Var_Get),
		     cl_subst(New_var_Get, Var_Get, Arg_list_Get38, Subst_Ret),
		     set_place(Setf_Env,
			       setf,
			       [value, u_arg_list],
			       [Subst_Ret],
			       Setf_R),
		     get_var(Setf_Env, u_body, Body_Get),
		     get_var(Setf_Env, u_new_var, New_var_Get41),
		     get_var(Setf_Env, u_var, Var_Get42),
		     cl_subst(New_var_Get41, Var_Get42, Body_Get, Subst_Ret77),
		     set_place(Setf_Env,
			       setf,
			       [value, u_body],
			       [Subst_Ret77],
			       Setf_R44),
		     get_var(Setf_Env, u_new_var, New_var_Get45),
		     get_var(Setf_Env, u_result, Result_Get),
		     get_var(Setf_Env, u_var, Var_Get46),
		     cl_subst(New_var_Get45, Var_Get46, Result_Get, Subst_Ret78),
		     set_place(Setf_Env,
			       setf,
			       [value, u_result],
			       [Subst_Ret78],
			       Setf_R48),
		     LetResult=Setf_R48
		   )),
	    _118796=LetResult
	;   _118796=[]
	),
	get_var(Env, u_result, IFTEST55),
	(   IFTEST55\==[]
	->  get_var(Env, u_result, Result_Get58),
	    CDR=[Result_Get58],
	    Loop_alist_Param=[progn|CDR],
	    f_u_add_element_to_loop_alist(Loop_alist_Param,
					  u_result,
					  TrueResult59),
	    _119798=TrueResult59
	;   _119798=[]
	),
	get_var(Env, u_body, Body_Get60),
	f_u_add_element_to_loop_alist(Body_Get60,
				      Where_to_add_Param,
				      Loop_alist_Ret),
	get_var(Env, u_arg_list, IFTEST62),
	(   IFTEST62\==[]
	->  get_var(Env, u_arg_list, Arg_list_Get65),
	    BV68=bv(u_new_var, Ele70),
	    Env67=[BV68|Env],
	    forall(member(Ele70, Arg_list_Get65),
		   ( nb_setarg(2, BV68, Ele70),
		     get_var(Env67, u_new_var, New_var_Get66),
		     f_u_add_element_to_loop_alist(New_var_Get66,
						   u_initializations,
						   TrueResult72)
		   )),
	    FnResult=TrueResult72
	;   FnResult=[]
	).
:- set_opv(f_u_define_and_rename_loop_locals, classof, claz_function),
   set_opv(u_define_and_rename_loop_locals, compile_as, kw_function),
   set_opv(u_define_and_rename_loop_locals,
	   function,
	   f_u_define_and_rename_loop_locals),
   DefunResult=u_define_and_rename_loop_locals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17648 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; nsubst doesnt work on body as it isn't quite represented ",
				     13,
				     17921)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17648 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; as a list on the function stack",
				     13,
				     17994)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'add-to-loop-macro',
			    ['where-to-add', 'arg-list', result, '&body', body],
			    
			    [ '#BQ',
			      
			      [ 'define-and-rename-loop-locals',
				[quote, ['#COMMA', 'where-to-add']],
				[quote, ['#COMMA', 'arg-list']],
				[quote, ['#COMMA', result]],
				[quote, ['#BQ-COMMA-ELIPSE', body]]
			      ]
			    ]
			  ]).

% annotating U::ADD-TO-LOOP-MACRO 
wl: lambda_def(defmacro,
	      u_add_to_loop_macro,
	      f_u_add_to_loop_macro,
	      [u_where_to_add, u_arg_list, u_result, c38_body, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_define_and_rename_loop_locals,
		    [quote, ['#COMMA', u_where_to_add]],
		    [quote, ['#COMMA', u_arg_list]],
		    [quote, ['#COMMA', u_result]],
		    [quote, ['#BQ-COMMA-ELIPSE', u_body]]
		  ]
		]
	      ]).


% annotating U::ADD-TO-LOOP-MACRO 
wl: arglist_info(u_add_to_loop_macro,
		[u_where_to_add, u_arg_list, u_result, c38_body, u_body],
		[u_where_to_add, u_arg_list, u_result, u_body],
		arginfo{ all:[u_where_to_add, u_arg_list, u_result],
			 allow_other_keys:0,
			 aux:0,
			 body:[u_body],
			 complex:[body],
			 env:0,
			 key:0,
			 names:[u_where_to_add, u_arg_list, u_result, u_body],
			 opt:0,
			 req:[u_where_to_add, u_arg_list, u_result],
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::ADD-TO-LOOP-MACRO 
wl: init_args(3, u_add_to_loop_macro).


% annotating U::ADD-TO-LOOP-MACRO 
f_u_add_to_loop_macro(Where_to_add_Param, Arg_list_Param, Result_Param, RestNKeys, FnResult) :-
	TLEnv3=[bv(u_where_to_add, Where_to_add_Param), bv(u_arg_list, Arg_list_Param), bv(u_result, Result_Param), bv(u_body, Body_Param)],
	as_body(u_body, Body_Param, RestNKeys),
	get_var(TLEnv3, u_body, Body_Get),
	[u_define_and_rename_loop_locals, [quote, Where_to_add_Param], [quote, Arg_list_Param], [quote, Result_Param], [quote|Body_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_add_to_loop_macro, classof, claz_macro),
   set_opv(u_add_to_loop_macro, compile_as, kw_operator),
   set_opv(u_add_to_loop_macro, function, f_u_add_to_loop_macro),
   DefMacroResult=u_add_to_loop_macro.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;     Examples of \"programmer\" defined loop macro functions. They are to",
				     1,
				     18570)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; function as their zetalisp loop counterparts. To define a yloop macro",
				     1,
				     18645)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; you must invoke the macro ADD-TO-LOOP-MACRO. This macro: 1)substitutes",
				     1,
				     18719)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; symbols (via gensym) so as to avoid symbol conflicts within the loop",
				     1,
				     18794)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; in the future; 2) provides requested local loop variables that will be",
				     1,
				     18867)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; within the lexical scope of the repeating statements (i.e. the loop),",
				     1,
				     18942)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; and ; 3)places the new code in the requested part of the",
				     1,
				     19016)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; loop. (Specifically the yloop macro is conceptually separated into 3",
				     1,
				     19077)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; parts: the FRONT, the MIDDLE and the end. Code that is in the FRONT of",
				     1,
				     19150)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; the yloop macro is executed after local bindings are made but before",
				     1,
				     19225)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; the executions of the statements to be repeated. Code that is in the",
				     1,
				     19298)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; MIDDLE of the yloop macro is executed after the FRONT code has been",
				     1,
				     19371)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; executed and is executed repeatedly until some termination condition",
				     1,
				     19443)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; is met. Code in the END of the yloop macro is executed after the loop",
				     1,
				     19516)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; terminates normally.)  The first argument to ADD-TO-LOOP-MACRO is to",
				     1,
				     19590)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; indicate where to place the new code.  It is to be one of FRONT MIDDLE",
				     1,
				     19663)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; END. The second argument is a list of desired local yloop",
				     1,
				     19738)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; variables. The syntax is to be the same as the car of let statements",
				     1,
				     19800)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; as that list will actually be placed at the position of the first",
				     1,
				     19873)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; argument in the let statement. The third argument is the variable",
				     1,
				     19943)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; which will given to the (return ) statement of the loop so that its",
				     1,
				     20013)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; value will be returned on normal termination of the loop. And the",
				     1,
				     20085)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; final arguments are to be the body of new macro to be inserted in the",
				     1,
				     20155)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; loop .", 1, 20229)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; ", 1, 20240)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Hint When you want something returned, declare a new local loop",
				     1,
				     20245)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; variable, declare it as that which will be returned and set your",
				     1,
				     20313)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; answer to it.", 1, 20382)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defmacro sum (expression)", 1, 20401)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  `(add-to-loop-macro middle ((sum 0)) sum",
				     1,
				     20429)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    (setq sum (+ sum ,expression))))",
				     1,
				     20473)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defmacro ywhen (test &body clauses-to-execute)",
				     1,
				     20512)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  `(add-to-loop-macro middle nil nil",
				     1,
				     20561)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (when ,test ,@clauses-to-execute)))",
				     1,
				     20599)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:20658 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-form',
			    [sequence, 'form-to-find'],
			    
			    [ cond,
			      [[atom, sequence], []],
			      [[null, sequence], []],
			      [[equal, [car, sequence], 'form-to-find'], sequence],
			      
			      [ t,
				
				[ 'list-without-nils',
				  ['find-form', [car, sequence], 'form-to-find'],
				  ['find-form', [cdr, sequence], 'form-to-find']
				]
			      ]
			    ]
			  ]).

% annotating U::FIND-FORM 
wl: lambda_def(defun,
	      u_find_form,
	      f_u_find_form,
	      [sequence, u_form_to_find],
	      
	      [ 
		[ cond,
		  [[atom, sequence], []],
		  [[null, sequence], []],
		  [[equal, [car, sequence], u_form_to_find], sequence],
		  
		  [ t,
		    
		    [ u_list_without_nils,
		      [u_find_form, [car, sequence], u_form_to_find],
		      [u_find_form, [cdr, sequence], u_form_to_find]
		    ]
		  ]
		]
	      ]).


% annotating U::FIND-FORM 
wl: arglist_info(u_find_form,
		[sequence, u_form_to_find],
		[Sequence_Get30, Form_to_find_Param],
		arginfo{ all:[sequence, u_form_to_find],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sequence, u_form_to_find],
			 opt:0,
			 req:[sequence, u_form_to_find],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FIND-FORM 
wl: init_args(exact_only, u_find_form).


% annotating U::FIND-FORM 
f_u_find_form(Sequence_Get30, Form_to_find_Param, ElseResult34) :-
	(   Sequence_Get30\=[CAR|CDR]
	->  ElseResult34=[]
	;   Sequence_Get30==[]
	->  ElseResult34=[]
	;   cl_car(Sequence_Get30, PredArg1Result),
	    (   is_equal(PredArg1Result, Form_to_find_Param)
	    ->  ElseResult34=Sequence_Get30
	    ;   cl_car(Sequence_Get30, Find_form_Param),
		f_u_find_form(Find_form_Param,
			      Form_to_find_Param,
			      Without_nils_Param),
		cl_cdr(Sequence_Get30, Find_form_Param39),
		f_u_find_form(Find_form_Param39,
			      Form_to_find_Param,
			      Find_form_Ret),
		f_u_list_without_nils(Without_nils_Param,
				      Find_form_Ret,
				      ElseResult),
		ElseResult34=ElseResult
	    )
	).
:- set_opv(f_u_find_form, classof, claz_function),
   set_opv(u_find_form, compile_as, kw_function),
   set_opv(u_find_form, function, f_u_find_form),
   DefunResult=u_find_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:20939 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'substitute-loop-return',
			    [label, 'lisp-expressions'],
			    
			    [ dolist,
			      
			      [ 'subst-clause',
				
				[ 'find-form',
				  'lisp-expressions',
				  [quote, 'loop-return']
				]
			      ],
			      
			      [ nsublis,
				
				[ '#BQ',
				  
				  [ ['#COMMA', 'subst-clause'],
				    
				    [ 'return-from',
				      ['#COMMA', label],
				      [cdr, 'subst-clause']
				    ]
				  ]
				],
				'lisp-expressions'
			      ]
			    ]
			  ]).

% annotating U::SUBSTITUTE-LOOP-RETURN 
wl: lambda_def(defun,
	      u_substitute_loop_return,
	      f_u_substitute_loop_return,
	      [u_label, u_lisp_expressions],
	      
	      [ 
		[ dolist,
		  
		  [ u_subst_clause,
		    [u_find_form, u_lisp_expressions, [quote, u_loop_return]]
		  ],
		  
		  [ nsublis,
		    
		    [ '#BQ',
		      
		      [ ['#COMMA', u_subst_clause],
			[return_from, ['#COMMA', u_label], [cdr, u_subst_clause]]
		      ]
		    ],
		    u_lisp_expressions
		  ]
		]
	      ]).


% annotating U::SUBSTITUTE-LOOP-RETURN 
wl: arglist_info(u_substitute_loop_return,
		[u_label, u_lisp_expressions],
		[Label_Param, Lisp_expressions_Param],
		arginfo{ all:[u_label, u_lisp_expressions],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_label, u_lisp_expressions],
			 opt:0,
			 req:[u_label, u_lisp_expressions],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBSTITUTE-LOOP-RETURN 
wl: init_args(exact_only, u_substitute_loop_return).


% annotating U::SUBSTITUTE-LOOP-RETURN 
f_u_substitute_loop_return(Label_Param, Lisp_expressions_Param, FnResult) :-
	Env=[bv(u_label, Label_Param), bv(u_lisp_expressions, Lisp_expressions_Param)],
	f_u_find_form(Lisp_expressions_Param, u_loop_return, List),
	BV=bv(u_subst_clause, Ele),
	Env=[BV|Env],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env, u_subst_clause, Subst_clause_Get),
		 cl_nsublis(
			    [ Subst_clause_Get,
			      [return_from, Label_Param, [cdr, u_subst_clause]]
			    ],
			    Lisp_expressions_Param,
			    Nsublis_Ret)
	       )),
	Nsublis_Ret=FnResult.
:- set_opv(f_u_substitute_loop_return, classof, claz_function),
   set_opv(u_substitute_loop_return, compile_as, kw_function),
   set_opv(u_substitute_loop_return, function, f_u_substitute_loop_return),
   DefunResult=u_substitute_loop_return.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:21158 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'list-without-nils',
			    [a, b],
			    
			    [ cond,
			      [[or, [null, a], [null, b]], [append, a, b]],
			      [t, [list, a, b]]
			    ]
			  ]).

% annotating U::LIST-WITHOUT-NILS 
wl: lambda_def(defun,
	      u_list_without_nils,
	      f_u_list_without_nils,
	      [u_a, u_b],
	      
	      [ 
		[ cond,
		  [[or, [null, u_a], [null, u_b]], [append, u_a, u_b]],
		  [t, [list, u_a, u_b]]
		]
	      ]).


% annotating U::LIST-WITHOUT-NILS 
wl: arglist_info(u_list_without_nils,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::LIST-WITHOUT-NILS 
wl: init_args(exact_only, u_list_without_nils).


% annotating U::LIST-WITHOUT-NILS 
f_u_list_without_nils(A_Param, B_Param, FnResult) :-
	(   cl_null(A_Param, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   cl_null(B_Param, Null_Ret),
	    IFTEST=Null_Ret
	),
	(   IFTEST\==[]
	->  cl_append(A_Param, B_Param, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[A_Param, B_Param]
	).
:- set_opv(f_u_list_without_nils, classof, claz_function),
   set_opv(u_list_without_nils, compile_as, kw_function),
   set_opv(u_list_without_nils, function, f_u_list_without_nils),
   DefunResult=u_list_without_nils.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:21158 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; *EOF*", 1, 21262)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:21158 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; *EOF*", 0, 21263)).
:- true.


% Total time: 7.305 seconds

