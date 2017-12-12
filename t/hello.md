
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "hello.lisp" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
% Start time: Mon Dec 11 12:48:38 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user, [print, [cons, '$STRING'("hello"), '*ARGS*']]).
:- get_var(TLEnv, ext_xx_args_xx, Ext_xx_args_xx_Get),
   Print_Param=['$ARRAY'([*], claz_base_character, "hello")|Ext_xx_args_xx_Get],
   cl_print(Print_Param, _Ignored).


% Total time: 0.02 seconds

