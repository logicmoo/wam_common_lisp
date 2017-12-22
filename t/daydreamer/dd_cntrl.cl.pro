#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "dd_cntrl" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
%; Start time: Fri Dec 22 03:34:36 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
;(break)
*/
/*
*/
/*
 Daydreamer
*/
/*
 Version 3.5
*/
/*
*/
/*
 Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
*/
/*
 All Rights Reserved.
*/
/*
*/
/*
  9/25/86: Removed flavors
*/
/*
 19990504: ported to Common Lisp
*/
/*
*/
/*
*******************************************************************************
*/
/*
*/
/*
 Top-level functions
*/
/*
*/
/*
(setq *starting-state* 'daydreaming)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:379 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*starting-state*',[quote,daydreaming]])
:- set_var(TLEnv3, setq, u_xx_starting_state_xx, u_daydreaming).
/*
(defun daydreamer ()
  (ndbg-reset)
  (if *typeset?*
      (format *gate-dbg* "\\begin{flushleft}"(defun daydreamer ()\n  (ndbg-reset)\n  (if *typeset?*\n      (format *gate-dbg* \"\\\\begin{flushleft}~%\"))\n  (ndbg-roman-nl *gate-dbg* rule *dd-version*)\n  (setq *state* 'suspended)\n  (daydreamer-initialize)\n  (set-state *starting-state*)\n  ; Get off the ground by running inferences which will activate\n  ; some top-level goals.\n  (run-inferences *reality-lookahead* nil *me-belief-path*)\n  ; Run the top-level emotion-directed control loop.\n  (daydreamer-control0)\n  (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER terminates\")\n  (if *typeset?*\n      (format *gate-dbg* \"\\\\end{flushleft}~%\"))\n  t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,daydreamer,[],['ndbg-reset'],[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\begin{flushleft}~%")]],['ndbg-roman-nl','*gate-dbg*',rule,'*dd-version*'],[setq,'*state*',[quote,suspended]],['daydreamer-initialize'],['set-state','*starting-state*'],['run-inferences','*reality-lookahead*',[],'*me-belief-path*'],['daydreamer-control0'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER terminates")],[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\end{flushleft}~%")]],t])
/*
% macroexpand:-[u_ndbg_roman_nl,u_xx_gate_dbg_xx,u_rule,u_xx_dd_version_xx].
*/
/*
% into:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,u_xx_dd_version_xx],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% macroexpand:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,u_xx_dd_version_xx],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% into:-[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[progn,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,u_xx_dd_version_xx],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]],[]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_61474),cl_assoc(u_rule,_61474,[],_61272).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_63740),cl_assoc(u_rule,_63740,[],_72306).
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_63740),cl_assoc(u_rule,_63740,[],_72306),cl_member(u_all,_72306,_61850),f_u_t_or_nil(_61850,_61618).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_62456,u_x,_62940),get_var(_62456,u_xx_ndbg_items_xx,_64406),cl_member(_62940,_64406,_74050),f_u_t_or_nil(_74050,_81088).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_173866),cl_assoc(u_rule,_173866,[],_182454).
*/
/*
% code:-_61866=closure([_62270|_57100],_62164,[u_x],(get_var(_62270,u_x,_61882),get_var(_62270,u_xx_ndbg_items_xx,_62004),cl_member(_61882,_62004,_62136),f_u_t_or_nil(_62136,_62164))),get_var(_57100,u_xx_ndbg_interests_xx,_62338),cl_assoc(u_rule,_62338,[],_62398),cl_cdr(_62398,_62306),cl_some(_61866,_62306,_64488),f_u_t_or_nil(_64488,_81318).
*/
/*
% macroexpand:-[u_ndbg,u_xx_gate_dbg_xx,u_rule,u_xx_dd_version_xx].
*/
/*
% into:-[progn,[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[cond,[[u_eq_c63,u_xx_gate_dbg_xx,t],[format,[u_standard_output],u_xx_dd_version_xx],t],[u_xx_gate_dbg_xx,[format,u_xx_gate_dbg_xx,u_xx_dd_version_xx],t],[u_else,[]]],[]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_65996),cl_assoc(u_rule,_65996,[],_74590).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_65130),cl_assoc(u_rule,_65130,[],_66282).
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_65130),cl_assoc(u_rule,_65130,[],_66282),cl_member(u_all,_66282,_87378),f_u_t_or_nil(_87378,_94508).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_67260,u_x,_66614),get_var(_67260,u_xx_ndbg_items_xx,_68800),cl_member(_66614,_68800,_76912),f_u_t_or_nil(_76912,_83930).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_176526),cl_assoc(u_rule,_176526,[],_185132).
*/
/*
% code:-_65152=closure([_64958|_57100],_65060,[u_x],(get_var(_64958,u_x,_64914),get_var(_64958,u_xx_ndbg_items_xx,_64988),cl_member(_64914,_64988,_65032),f_u_t_or_nil(_65032,_65060))),get_var(_57100,u_xx_ndbg_interests_xx,_65236),cl_assoc(u_rule,_65236,[],_65314),cl_cdr(_65314,_81952),cl_some(_65152,_81952,_67404),f_u_t_or_nil(_67404,_84234).
*/
/*
% macroexpand:-[u_eq_c63,u_xx_gate_dbg_xx,t].
*/
/*
% into:-[eql,u_xx_gate_dbg_xx,t].
*/
/*
% code:-get_var(_57100,u_xx_gate_dbg_xx,_179464),cl_eql(_179464,t,_150616).
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_61894),cl_assoc(u_rule,_61894,[],_61850),(_61850\==[]->(get_var(_57100,u_xx_ndbg_interests_xx,_61966),cl_assoc(u_rule,_61966,[],_61992),cl_member(u_all,_61992,_62018),f_u_t_or_nil(_62018,_62044),_62044\==[],_62488=_62044->true;_62262=closure([_62126|_57100],_62222,[u_x],(get_var(_62126,u_x,_62084),get_var(_62126,u_xx_ndbg_items_xx,_62154),cl_member(_62084,_62154,_62196),f_u_t_or_nil(_62196,_62222))),get_var(_57100,u_xx_ndbg_interests_xx,_62308),cl_assoc(u_rule,_62308,[],_62370),cl_cdr(_62370,_62422),cl_some(_62262,_62422,_62396),f_u_t_or_nil(_62396,_62448),_62488=_62448),_61806=_62488;_61806=[]),(_61806\==[]->get_var(_57100,u_xx_gate_dbg_xx,_62564),cl_eql(_62564,t,_62518),(_62518\==[]->f_u_standard_output(_62612),get_var(_57100,u_xx_dd_version_xx,_62614),cl_format([_62612,_62614],_62610),_63214=t;get_var(_57100,u_xx_gate_dbg_xx,_62748),(_62748\==[]->get_var(_57100,u_xx_dd_version_xx,_62924),get_var(_57100,u_xx_gate_dbg_xx,_62860),cl_format([_62860,_62924],_78008),_63186=t;get_var(_57100,u_else,_62972),(_62972\==[]->_63158=[];_63128=[],_63158=_63128),_63186=_63158),_63214=_63186),_61740=_63214;_61740=[]).
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_59818),cl_assoc(u_rule,_59818,[],_59788),(_59788\==[]->(get_var(_57100,u_xx_ndbg_interests_xx,_59952),cl_assoc(u_rule,_59952,[],_59988),cl_member(u_all,_59988,_59920),f_u_t_or_nil(_59920,_60568),_60568\==[],_60612=_60568->true;_60046=closure([_60356|_57100],_60256,[u_x],(get_var(_60356,u_x,_60062),get_var(_60356,u_xx_ndbg_items_xx,_60130),cl_member(_60062,_60130,_60230),f_u_t_or_nil(_60230,_60256))),get_var(_57100,u_xx_ndbg_interests_xx,_60422),cl_assoc(u_rule,_60422,[],_60458),cl_cdr(_60458,_60390),cl_some(_60046,_60390,_60504),f_u_t_or_nil(_60504,_60542),_60612=_60542),_59738=_60612;_59738=[]),(_59738\==[]->get_var(_57100,u_xx_gate_dbg_xx,_60662),f_u_begin_roman_font(_60662,_60644),get_var(_57100,u_xx_ndbg_interests_xx,_60782),cl_assoc(u_rule,_60782,[],_60738),(_60738\==[]->(get_var(_57100,u_xx_ndbg_interests_xx,_60850),cl_assoc(u_rule,_60850,[],_60876),cl_member(u_all,_60876,_60902),f_u_t_or_nil(_60902,_60928),_60928\==[],_61364=_60928->true;_61138=closure([_61006|_57100],_61098,[u_x],(get_var(_61006,u_x,_60968),get_var(_61006,u_xx_ndbg_items_xx,_61034),cl_member(_60968,_61034,_61072),f_u_t_or_nil(_61072,_61098))),get_var(_57100,u_xx_ndbg_interests_xx,_61184),cl_assoc(u_rule,_61184,[],_61246),cl_cdr(_61246,_61298),cl_some(_61138,_61298,_61272),f_u_t_or_nil(_61272,_61324),_61364=_61324),_60696=_61364;_60696=[]),(_60696\==[]->get_var(_57100,u_xx_gate_dbg_xx,_61438),cl_eql(_61438,t,_61394),(_61394\==[]->f_u_standard_output(_61464),get_var(_57100,u_xx_dd_version_xx,_61466),cl_format([_61464,_61466],_61462),_61910=t;get_var(_57100,u_xx_gate_dbg_xx,_61540),(_61540\==[]->get_var(_57100,u_xx_dd_version_xx,_61642),get_var(_57100,u_xx_gate_dbg_xx,_61614),cl_format([_61614,_61642],_61960),_61884=t;get_var(_57100,u_else,_61684),(_61684\==[]->_61858=[];_61830=[],_61858=_61830),_61884=_61858),_61910=_61884),_60678=_61910;_60678=[]),get_var(_57100,u_xx_gate_dbg_xx,_61990),f_u_end_font(_61990,_61972),get_var(_57100,u_xx_gate_dbg_xx,_62018),f_u_do_newline(_62018,_62502),_59672=_62502;_59672=[]).
*/
/*
% code:-get_var(_57100,u_xx_ndbg_interests_xx,_59664),cl_assoc(u_rule,_59664,[],_59634),(_59634\==[]->(get_var(_57100,u_xx_ndbg_interests_xx,_59798),cl_assoc(u_rule,_59798,[],_59834),cl_member(u_all,_59834,_59766),f_u_t_or_nil(_59766,_60414),_60414\==[],_60458=_60414->true;_59892=closure([_60202|_57100],_60102,[u_x],(get_var(_60202,u_x,_59908),get_var(_60202,u_xx_ndbg_items_xx,_59976),cl_member(_59908,_59976,_60076),f_u_t_or_nil(_60076,_60102))),get_var(_57100,u_xx_ndbg_interests_xx,_60268),cl_assoc(u_rule,_60268,[],_60304),cl_cdr(_60304,_60236),cl_some(_59892,_60236,_60350),f_u_t_or_nil(_60350,_60388),_60458=_60388),_59584=_60458;_59584=[]),(_59584\==[]->get_var(_57100,u_xx_gate_dbg_xx,_60508),f_u_begin_roman_font(_60508,_60490),get_var(_57100,u_xx_ndbg_interests_xx,_60628),cl_assoc(u_rule,_60628,[],_60584),(_60584\==[]->(get_var(_57100,u_xx_ndbg_interests_xx,_60696),cl_assoc(u_rule,_60696,[],_60722),cl_member(u_all,_60722,_60748),f_u_t_or_nil(_60748,_60774),_60774\==[],_61210=_60774->true;_60984=closure([_60852|_57100],_60944,[u_x],(get_var(_60852,u_x,_60814),get_var(_60852,u_xx_ndbg_items_xx,_60880),cl_member(_60814,_60880,_60918),f_u_t_or_nil(_60918,_60944))),get_var(_57100,u_xx_ndbg_interests_xx,_61030),cl_assoc(u_rule,_61030,[],_61092),cl_cdr(_61092,_61144),cl_some(_60984,_61144,_61118),f_u_t_or_nil(_61118,_61170),_61210=_61170),_60542=_61210;_60542=[]),(_60542\==[]->get_var(_57100,u_xx_gate_dbg_xx,_61284),cl_eql(_61284,t,_61240),(_61240\==[]->f_u_standard_output(_61310),get_var(_57100,u_xx_dd_version_xx,_61312),cl_format([_61310,_61312],_61308),_61756=t;get_var(_57100,u_xx_gate_dbg_xx,_61386),(_61386\==[]->get_var(_57100,u_xx_dd_version_xx,_61488),get_var(_57100,u_xx_gate_dbg_xx,_61460),cl_format([_61460,_61488],_61806),_61730=t;get_var(_57100,u_else,_61530),(_61530\==[]->_61704=[];_61676=[],_61704=_61676),_61730=_61704),_61756=_61730),_60524=_61756;_60524=[]),get_var(_57100,u_xx_gate_dbg_xx,_61836),f_u_end_font(_61836,_61818),get_var(_57100,u_xx_gate_dbg_xx,_61864),f_u_do_newline(_61864,_62348),_59516=_62348;_59516=[]).
*/
/*
% macroexpand:-[u_ndbg_roman_nl,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")].
*/
/*
% into:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% macroexpand:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% into:-[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[progn,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]],[]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_67840),cl_assoc(u_rule,_67840,[],_76438).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_199896),cl_assoc(u_rule,_199896,[],_208500).
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_66596),cl_assoc(u_rule,_66596,[],_66686),cl_member(u_all,_66686,_68182),f_u_t_or_nil(_68182,_75312).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_69222,u_x,_68554),get_var(_69222,u_xx_ndbg_items_xx,_70762),cl_member(_68554,_70762,_78896),f_u_t_or_nil(_78896,_85914).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_178510),cl_assoc(u_rule,_178510,[],_187120).
*/
/*
% code:-_67106=closure([_66906|_63096],_67014,[u_x],(get_var(_66906,u_x,_66856),get_var(_66906,u_xx_ndbg_items_xx,_66936),cl_member(_66856,_66936,_66986),f_u_t_or_nil(_66986,_67014))),get_var(_63096,u_xx_ndbg_interests_xx,_67190),cl_assoc(u_rule,_67190,[],_67286),cl_cdr(_67286,_83914),cl_some(_67106,_83914,_69376),f_u_t_or_nil(_69376,_86196).
*/
/*
% macroexpand:-[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")].
*/
/*
% into:-[progn,[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[cond,[[u_eq_c63,u_xx_gate_dbg_xx,t],[format,[u_standard_output],'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],t],[u_xx_gate_dbg_xx,[format,u_xx_gate_dbg_xx,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],t],[u_else,[]]],[]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_70946),cl_assoc(u_rule,_70946,[],_79562).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_203020),cl_assoc(u_rule,_203020,[],_211642).
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_69640),cl_assoc(u_rule,_69640,[],_69748),cl_member(u_all,_69748,_71244),f_u_t_or_nil(_71244,_78374).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_72290,u_x,_71616),get_var(_72290,u_xx_ndbg_items_xx,_73830),cl_member(_71616,_73830,_81970),f_u_t_or_nil(_81970,_88988).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_181584),cl_assoc(u_rule,_181584,[],_190212).
*/
/*
% code:-_70180=closure([_69974|_63096],_70088,[u_x],(get_var(_69974,u_x,_69918),get_var(_69974,u_xx_ndbg_items_xx,_70004),cl_member(_69918,_70004,_70060),f_u_t_or_nil(_70060,_70088))),get_var(_63096,u_xx_ndbg_interests_xx,_70264),cl_assoc(u_rule,_70264,[],_70378),cl_cdr(_70378,_72230),cl_some(_70180,_72230,_71148),f_u_t_or_nil(_71148,_74512).
*/
/*
% macroexpand:-[u_eq_c63,u_xx_gate_dbg_xx,t].
*/
/*
% into:-[eql,u_xx_gate_dbg_xx,t].
*/
/*
% code:-get_var(_63096,u_xx_gate_dbg_xx,_169682),cl_eql(_169682,t,_172192).
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_66826),cl_assoc(u_rule,_66826,[],_66782),(_66782\==[]->(get_var(_63096,u_xx_ndbg_interests_xx,_66898),cl_assoc(u_rule,_66898,[],_66924),cl_member(u_all,_66924,_66950),f_u_t_or_nil(_66950,_66976),_66976\==[],_67480=_66976->true;_67218=closure([_67070|_63096],_67178,[u_x],(get_var(_67070,u_x,_67016),get_var(_67070,u_xx_ndbg_items_xx,_67098),cl_member(_67016,_67098,_67152),f_u_t_or_nil(_67152,_67178))),get_var(_63096,u_xx_ndbg_interests_xx,_67264),cl_assoc(u_rule,_67264,[],_67362),cl_cdr(_67362,_67414),cl_some(_67218,_67414,_67388),f_u_t_or_nil(_67388,_67440),_67480=_67440),_66738=_67480;_66738=[]),(_66738\==[]->get_var(_63096,u_xx_gate_dbg_xx,_67556),cl_eql(_67556,t,_67510),(_67510\==[]->f_u_standard_output(_74128),cl_format([_74128,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_75662),_68108=t;get_var(_63096,u_xx_gate_dbg_xx,_67680),(_67680\==[]->get_var(_63096,u_xx_gate_dbg_xx,_67824),cl_format([_67824,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_81868),_68080=t;get_var(_63096,u_else,_67926),(_67926\==[]->_68052=[];_68024=[],_68052=_68024),_68080=_68052),_68108=_68080),_66672=_68108;_66672=[]).
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_64760),cl_assoc(u_rule,_64760,[],_64706),(_64706\==[]->(get_var(_63096,u_xx_ndbg_interests_xx,_64862),cl_assoc(u_rule,_64862,[],_64898),cl_member(u_all,_64898,_64938),f_u_t_or_nil(_64938,_64964),_64964\==[],_65488=_64964->true;_65216=closure([_65044|_63096],_65128,[u_x],(get_var(_65044,u_x,_65014),get_var(_65044,u_xx_ndbg_items_xx,_65072),cl_member(_65014,_65072,_65102),f_u_t_or_nil(_65102,_65128))),get_var(_63096,u_xx_ndbg_interests_xx,_65296),cl_assoc(u_rule,_65296,[],_65332),cl_cdr(_65332,_65404),cl_some(_65216,_65404,_65378),f_u_t_or_nil(_65378,_65430),_65488=_65430),_64656=_65488;_64656=[]),(_64656\==[]->get_var(_63096,u_xx_gate_dbg_xx,_65538),f_u_begin_roman_font(_65538,_73010),get_var(_63096,u_xx_ndbg_interests_xx,_65658),cl_assoc(u_rule,_65658,[],_65614),(_65614\==[]->(get_var(_63096,u_xx_ndbg_interests_xx,_65722),cl_assoc(u_rule,_65722,[],_65748),cl_member(u_all,_65748,_65774),f_u_t_or_nil(_65774,_65800),_65800\==[],_66296=_65800->true;_66034=closure([_65890|_63096],_65994,[u_x],(get_var(_65890,u_x,_65840),get_var(_65890,u_xx_ndbg_items_xx,_65918),cl_member(_65840,_65918,_65968),f_u_t_or_nil(_65968,_65994))),get_var(_63096,u_xx_ndbg_interests_xx,_66080),cl_assoc(u_rule,_66080,[],_66178),cl_cdr(_66178,_66230),cl_some(_66034,_66230,_66204),f_u_t_or_nil(_66204,_66256),_66296=_66256),_65572=_66296;_65572=[]),(_65572\==[]->get_var(_63096,u_xx_gate_dbg_xx,_66370),cl_eql(_66370,t,_66326),(_66326\==[]->f_u_standard_output(_66728),cl_format([_66728,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_66754),_66702=t;get_var(_63096,u_xx_gate_dbg_xx,_66432),(_66432\==[]->get_var(_63096,u_xx_gate_dbg_xx,_66506),cl_format([_66506,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_66780),_66676=t;get_var(_63096,u_else,_66536),(_66536\==[]->_66650=[];_66624=[],_66650=_66624),_66676=_66650),_66702=_66676),_65554=_66702;_65554=[]),get_var(_63096,u_xx_gate_dbg_xx,_66810),f_u_end_font(_66810,_73038),get_var(_63096,u_xx_gate_dbg_xx,_66838),f_u_do_newline(_66838,_67790),_64590=_67790;_64590=[]).
*/
/*
% code:-get_var(_63096,u_xx_ndbg_interests_xx,_64606),cl_assoc(u_rule,_64606,[],_64552),(_64552\==[]->(get_var(_63096,u_xx_ndbg_interests_xx,_64708),cl_assoc(u_rule,_64708,[],_64744),cl_member(u_all,_64744,_64784),f_u_t_or_nil(_64784,_64810),_64810\==[],_65334=_64810->true;_65062=closure([_64890|_63096],_64974,[u_x],(get_var(_64890,u_x,_64860),get_var(_64890,u_xx_ndbg_items_xx,_64918),cl_member(_64860,_64918,_64948),f_u_t_or_nil(_64948,_64974))),get_var(_63096,u_xx_ndbg_interests_xx,_65142),cl_assoc(u_rule,_65142,[],_65178),cl_cdr(_65178,_65250),cl_some(_65062,_65250,_65224),f_u_t_or_nil(_65224,_65276),_65334=_65276),_64502=_65334;_64502=[]),(_64502\==[]->get_var(_63096,u_xx_gate_dbg_xx,_65384),f_u_begin_roman_font(_65384,_67674),get_var(_63096,u_xx_ndbg_interests_xx,_65504),cl_assoc(u_rule,_65504,[],_65460),(_65460\==[]->(get_var(_63096,u_xx_ndbg_interests_xx,_65568),cl_assoc(u_rule,_65568,[],_65594),cl_member(u_all,_65594,_65620),f_u_t_or_nil(_65620,_65646),_65646\==[],_66142=_65646->true;_65880=closure([_65736|_63096],_65840,[u_x],(get_var(_65736,u_x,_65686),get_var(_65736,u_xx_ndbg_items_xx,_65764),cl_member(_65686,_65764,_65814),f_u_t_or_nil(_65814,_65840))),get_var(_63096,u_xx_ndbg_interests_xx,_65926),cl_assoc(u_rule,_65926,[],_66024),cl_cdr(_66024,_66076),cl_some(_65880,_66076,_66050),f_u_t_or_nil(_66050,_66102),_66142=_66102),_65418=_66142;_65418=[]),(_65418\==[]->get_var(_63096,u_xx_gate_dbg_xx,_66216),cl_eql(_66216,t,_66172),(_66172\==[]->f_u_standard_output(_66574),cl_format([_66574,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_66600),_66548=t;get_var(_63096,u_xx_gate_dbg_xx,_66278),(_66278\==[]->get_var(_63096,u_xx_gate_dbg_xx,_66352),cl_format([_66352,'$ARRAY'([*],claz_base_character,"DAYDREAMER terminates")],_66626),_66522=t;get_var(_63096,u_else,_66382),(_66382\==[]->_66496=[];_66470=[],_66496=_66470),_66522=_66496),_66548=_66522),_65400=_66548;_65400=[]),get_var(_63096,u_xx_gate_dbg_xx,_66656),f_u_end_font(_66656,_67702),get_var(_63096,u_xx_gate_dbg_xx,_66684),f_u_do_newline(_66684,_67636),_64434=_67636;_64434=[]).
*/
wl:lambda_def(defun, u_daydreamer, f_u_daydreamer, [], [[u_ndbg_reset], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\begin{flushleft}~%")]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, u_xx_dd_version_xx], [setq, u_xx_state_xx, [quote, u_suspended]], [u_daydreamer_initialize], [u_set_state, u_xx_starting_state_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_daydreamer_control0], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER terminates")], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")]], t]).
wl:arglist_info(u_daydreamer, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_daydreamer).

/*

### Compiled:  `U::DAYDREAMER` 
*/
f_u_daydreamer(FnResult) :-
	Env=[],
	f_u_ndbg_reset(Ndbg_reset_Ret),
	get_var(Env, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 "\\begin{flushleft}~%")
		      ],
		      TrueResult),
	    _54530=TrueResult
	;   _54530=[]
	),
	get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	cl_assoc(u_rule, Xx_ndbg_interests_xx_Get, [], IFTEST17),
	(   IFTEST17\==[]
	->  (   get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get21),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get21, [], Assoc_Ret22),
		cl_member(u_all, Assoc_Ret22, Or_nil_Param),
		f_u_t_or_nil(Or_nil_Param, Or_nil_Ret),
		Or_nil_Ret\==[],
		IFTEST15=Or_nil_Ret
	    ->  true
	    ;   Lambda=closure([_GEnv|Env], Or_nil_Ret29, [u_x],  (get_var(_GEnv, u_x, X_Get), get_var(_GEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get), cl_member(X_Get, Xx_ndbg_items_xx_Get, Or_nil_Param28), f_u_t_or_nil(Or_nil_Param28, Or_nil_Ret29))),
		get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get33),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get33, [], Assoc_Ret34),
		cl_cdr(Assoc_Ret34, Cdr_Ret),
		cl_some(Lambda, Cdr_Ret, Or_nil_Param35),
		f_u_t_or_nil(Or_nil_Param35, Or_nil_Ret37),
		IFTEST15=Or_nil_Ret37
	    )
	;   IFTEST15=[]
	),
	(   IFTEST15\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get40),
	    f_u_begin_roman_font(Xx_gate_dbg_xx_Get40, Roman_font_Ret),
	    get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get45),
	    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get45, [], IFTEST43),
	    (   IFTEST43\==[]
	    ->  (   get_var(Env,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get47),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get47, [], Assoc_Ret48),
		    cl_member(u_all, Assoc_Ret48, Or_nil_Param49),
		    f_u_t_or_nil(Or_nil_Param49, Or_nil_Ret50),
		    Or_nil_Ret50\==[],
		    IFTEST41=Or_nil_Ret50
		->  true
		;   Lambda57=closure([_GEnv52|Env], Or_nil_Ret55, [u_x],  (get_var(_GEnv52, u_x, X_Get51), get_var(_GEnv52, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get53), cl_member(X_Get51, Xx_ndbg_items_xx_Get53, Or_nil_Param54), f_u_t_or_nil(Or_nil_Param54, Or_nil_Ret55))),
		    get_var(Env,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get59),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get59, [], Assoc_Ret60),
		    cl_cdr(Assoc_Ret60, Cdr_Ret62),
		    cl_some(Lambda57, Cdr_Ret62, Or_nil_Param61),
		    f_u_t_or_nil(Or_nil_Param61, Or_nil_Ret63),
		    IFTEST41=Or_nil_Ret63
		)
	    ;   IFTEST41=[]
	    ),
	    (   IFTEST41\==[]
	    ->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get68),
		cl_eql(Xx_gate_dbg_xx_Get68, t, IFTEST66),
		(   IFTEST66\==[]
		->  f_u_standard_output(Standard_output_Ret),
		    get_var(Env, u_xx_dd_version_xx, Xx_dd_version_xx_Get),
		    cl_format([Standard_output_Ret, Xx_dd_version_xx_Get],
			      Format_Ret),
		    ElseResult80=t
		;   get_var(Env, u_xx_gate_dbg_xx, IFTEST71),
		    (   IFTEST71\==[]
		    ->  get_var(Env, u_xx_dd_version_xx, Xx_dd_version_xx_Get75),
			get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get74),
			cl_format(
				  [ Xx_gate_dbg_xx_Get74,
				    Xx_dd_version_xx_Get75
				  ],
				  Format_Ret85),
			ElseResult80=t
		    ;   get_var(Env, u_else, IFTEST76),
			(   IFTEST76\==[]
			->  ElseResult80=[]
			;   ElseResult80=[]
			)
		    )
		)
	    ;   ElseResult80=[]
	    ),
	    get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get86),
	    f_u_end_font(Xx_gate_dbg_xx_Get86, End_font_Ret),
	    get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get87),
	    f_u_do_newline(Xx_gate_dbg_xx_Get87, TrueResult88),
	    _54710=TrueResult88
	;   _54710=[]
	),
	set_var(Env, setq, u_xx_state_xx, u_suspended),
	f_u_daydreamer_initialize(Daydreamer_initialize_Ret),
	get_var(Env, u_xx_starting_state_xx, Xx_starting_state_xx_Get),
	f_u_set_state(Xx_starting_state_xx_Get, Set_state_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get,
			   [],
			   Xx_me_belief_path_xx_Get,
			   Run_inferences_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get99),
	cl_assoc(u_rule, Xx_ndbg_interests_xx_Get99, [], IFTEST97),
	(   IFTEST97\==[]
	->  (   get_var(Env,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get101),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get101, [], Assoc_Ret102),
		cl_member(u_all, Assoc_Ret102, Or_nil_Param103),
		f_u_t_or_nil(Or_nil_Param103, Or_nil_Ret104),
		Or_nil_Ret104\==[],
		IFTEST95=Or_nil_Ret104
	    ->  true
	    ;   Lambda111=closure([_GEnv106|Env], Or_nil_Ret109, [u_x],  (get_var(_GEnv106, u_x, X_Get105), get_var(_GEnv106, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get107), cl_member(X_Get105, Xx_ndbg_items_xx_Get107, Or_nil_Param108), f_u_t_or_nil(Or_nil_Param108, Or_nil_Ret109))),
		get_var(Env,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get113),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get113, [], Assoc_Ret114),
		cl_cdr(Assoc_Ret114, Cdr_Ret116),
		cl_some(Lambda111, Cdr_Ret116, Or_nil_Param115),
		f_u_t_or_nil(Or_nil_Param115, Or_nil_Ret117),
		IFTEST95=Or_nil_Ret117
	    )
	;   IFTEST95=[]
	),
	(   IFTEST95\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get120),
	    f_u_begin_roman_font(Xx_gate_dbg_xx_Get120, Roman_font_Ret167),
	    get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get125),
	    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get125, [], IFTEST123),
	    (   IFTEST123\==[]
	    ->  (   get_var(Env,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get127),
		    cl_assoc(u_rule,
			     Xx_ndbg_interests_xx_Get127,
			     [],
			     Assoc_Ret128),
		    cl_member(u_all, Assoc_Ret128, Or_nil_Param129),
		    f_u_t_or_nil(Or_nil_Param129, Or_nil_Ret130),
		    Or_nil_Ret130\==[],
		    IFTEST121=Or_nil_Ret130
		->  true
		;   Lambda137=closure([_GEnv132|Env], Or_nil_Ret135, [u_x],  (get_var(_GEnv132, u_x, X_Get131), get_var(_GEnv132, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get133), cl_member(X_Get131, Xx_ndbg_items_xx_Get133, Or_nil_Param134), f_u_t_or_nil(Or_nil_Param134, Or_nil_Ret135))),
		    get_var(Env,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get139),
		    cl_assoc(u_rule,
			     Xx_ndbg_interests_xx_Get139,
			     [],
			     Assoc_Ret140),
		    cl_cdr(Assoc_Ret140, Cdr_Ret142),
		    cl_some(Lambda137, Cdr_Ret142, Or_nil_Param141),
		    f_u_t_or_nil(Or_nil_Param141, Or_nil_Ret143),
		    IFTEST121=Or_nil_Ret143
		)
	    ;   IFTEST121=[]
	    ),
	    (   IFTEST121\==[]
	    ->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get148),
		cl_eql(Xx_gate_dbg_xx_Get148, t, IFTEST146),
		(   IFTEST146\==[]
		->  f_u_standard_output(Standard_output_Ret161),
		    cl_format(
			      [ Standard_output_Ret161,
				'$ARRAY'([*],
					 claz_base_character,
					 "DAYDREAMER terminates")
			      ],
			      Format_Ret162),
		    ElseResult158=t
		;   get_var(Env, u_xx_gate_dbg_xx, IFTEST150),
		    (   IFTEST150\==[]
		    ->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get153),
			cl_format(
				  [ Xx_gate_dbg_xx_Get153,
				    '$ARRAY'([*],
					     claz_base_character,
					     "DAYDREAMER terminates")
				  ],
				  Format_Ret163),
			ElseResult158=t
		    ;   get_var(Env, u_else, IFTEST154),
			(   IFTEST154\==[]
			->  ElseResult158=[]
			;   ElseResult158=[]
			)
		    )
		)
	    ;   ElseResult158=[]
	    ),
	    get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get164),
	    f_u_end_font(Xx_gate_dbg_xx_Get164, End_font_Ret168),
	    get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get165),
	    f_u_do_newline(Xx_gate_dbg_xx_Get165, TrueResult166),
	    _57062=TrueResult166
	;   _57062=[]
	),
	get_var(Env, u_xx_typeset_c63_xx, IFTEST169),
	(   IFTEST169\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get172),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get172,
			'$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")
		      ],
		      TrueResult173),
	    _59096=TrueResult173
	;   _59096=[]
	),
	t=FnResult.
:- set_opv(f_u_daydreamer, classof, claz_function),
   set_opv(u_daydreamer, compile_as, kw_function),
   set_opv(u_daydreamer, function, f_u_daydreamer),
   DefunResult=u_daydreamer.
/*
:- side_effect(assert_lsp(u_daydreamer,
			  wl:lambda_def(defun, u_daydreamer, f_u_daydreamer, [], [[u_ndbg_reset], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\begin{flushleft}~%")]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, u_xx_dd_version_xx], [setq, u_xx_state_xx, [quote, u_suspended]], [u_daydreamer_initialize], [u_set_state, u_xx_starting_state_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_daydreamer_control0], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER terminates")], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")]], t]))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer,
			  wl:arglist_info(u_daydreamer, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer, wl:init_args(exact_only, u_daydreamer))).
*/
/*
 Get off the ground by running inferences which will activate
*/
/*
 some top-level goals.
*/
/*
 Run the top-level emotion-directed control loop.
*/
/*
(defun dd-continue ()
  (daydreamer-control0))

; This will pick up from the first subgoal in the set of subgoals of
; which goal is a part. We need to find the first context in which there
; is a plan for goal and back up one.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'dd-continue',[],['daydreamer-control0']])
wl:lambda_def(defun, u_dd_continue, f_u_dd_continue, [], [[u_daydreamer_control0]]).
wl:arglist_info(u_dd_continue, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_dd_continue).

/*

### Compiled:  `U::DD-CONTINUE` 
*/
f_u_dd_continue(FnResult) :-
	Env=[],
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_dd_continue, classof, claz_function),
   set_opv(u_dd_continue, compile_as, kw_function),
   set_opv(u_dd_continue, function, f_u_dd_continue),
   DefunResult=u_dd_continue.
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:lambda_def(defun, u_dd_continue, f_u_dd_continue, [], [[u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:arglist_info(u_dd_continue, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:init_args(exact_only, u_dd_continue))).
*/
/*
 This will pick up from the first subgoal in the set of subgoals of
*/
/*
 which goal is a part. We need to find the first context in which there
*/
/*
 is a plan for goal and back up one.
*/
/*
(defun pickup (goal)
  (let ((context (ob$get goal 'activation-context))
        (top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER "(defun pickup (goal)\n  (let ((context (ob$get goal 'activation-context))\n        (top-level-goal (ob$get goal 'top-level-goal)))\n       (ob$set context 'children nil)\n       (set-next-context top-level-goal context)\n       (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER ~A pickup\" goal)\n       (daydreamer-control0)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1238 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,pickup,[goal],[let,[[context,['ob$get',goal,[quote,'activation-context']]],['top-level-goal',['ob$get',goal,[quote,'top-level-goal']]]],['ob$set',context,[quote,children],[]],['set-next-context','top-level-goal',context],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER ~A pickup"),goal],['daydreamer-control0']]])
/*
% macroexpand:-[u_ndbg_roman_nl,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal].
*/
/*
% into:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% macroexpand:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% into:-[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[progn,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]],[]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_61284),cl_assoc(u_rule,_61284,[],_61082).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_63590),cl_assoc(u_rule,_63590,[],_72172).
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_63590),cl_assoc(u_rule,_63590,[],_72172),cl_member(u_all,_72172,_61700),f_u_t_or_nil(_61700,_61468).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_62306,u_x,_62790),get_var(_62306,u_xx_ndbg_items_xx,_64352),cl_member(_62790,_64352,_73972),f_u_t_or_nil(_73972,_81010).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_173788),cl_assoc(u_rule,_173788,[],_182392).
*/
/*
% code:-_61716=closure([_62130|_58248],_62024,[u_x],(get_var(_62130,u_x,_61732),get_var(_62130,u_xx_ndbg_items_xx,_61874),cl_member(_61732,_61874,_61996),f_u_t_or_nil(_61996,_62024))),get_var(_58248,u_xx_ndbg_interests_xx,_62198),cl_assoc(u_rule,_62198,[],_62258),cl_cdr(_62258,_62166),cl_some(_61716,_62166,_64348),f_u_t_or_nil(_64348,_81178).
*/
/*
% macroexpand:-[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal].
*/
/*
% into:-[progn,[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[cond,[[u_eq_c63,u_xx_gate_dbg_xx,t],[format,[u_standard_output],'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],t],[u_xx_gate_dbg_xx,[format,u_xx_gate_dbg_xx,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],t],[u_else,[]]],[]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_65984),cl_assoc(u_rule,_65984,[],_74594).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_65118),cl_assoc(u_rule,_65118,[],_66270).
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_65118),cl_assoc(u_rule,_65118,[],_66270),cl_member(u_all,_66270,_87366),f_u_t_or_nil(_87366,_94496).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_67264,u_x,_66602),get_var(_67264,u_xx_ndbg_items_xx,_68804),cl_member(_66602,_68804,_76932),f_u_t_or_nil(_76932,_83950).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_176546),cl_assoc(u_rule,_176546,[],_185168).
*/
/*
% code:-_65140=closure([_64946|_58248],_65048,[u_x],(get_var(_64946,u_x,_64902),get_var(_64946,u_xx_ndbg_items_xx,_64976),cl_member(_64902,_64976,_65020),f_u_t_or_nil(_65020,_65048))),get_var(_58248,u_xx_ndbg_interests_xx,_65224),cl_assoc(u_rule,_65224,[],_65302),cl_cdr(_65302,_81940),cl_some(_65140,_81940,_67392),f_u_t_or_nil(_67392,_84222).
*/
/*
% macroexpand:-[u_eq_c63,u_xx_gate_dbg_xx,t].
*/
/*
% into:-[eql,u_xx_gate_dbg_xx,t].
*/
/*
% code:-get_var(_58248,u_xx_gate_dbg_xx,_179488),cl_eql(_179488,t,_150640).
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_61846),cl_assoc(u_rule,_61846,[],_61802),(_61802\==[]->(get_var(_58248,u_xx_ndbg_interests_xx,_61918),cl_assoc(u_rule,_61918,[],_61944),cl_member(u_all,_61944,_61970),f_u_t_or_nil(_61970,_61996),_61996\==[],_62440=_61996->true;_62214=closure([_62078|_58248],_62174,[u_x],(get_var(_62078,u_x,_62036),get_var(_62078,u_xx_ndbg_items_xx,_62106),cl_member(_62036,_62106,_62148),f_u_t_or_nil(_62148,_62174))),get_var(_58248,u_xx_ndbg_interests_xx,_62260),cl_assoc(u_rule,_62260,[],_62322),cl_cdr(_62322,_62374),cl_some(_62214,_62374,_62348),f_u_t_or_nil(_62348,_62400),_62440=_62400),_61758=_62440;_61758=[]),(_61758\==[]->get_var(_58248,u_xx_gate_dbg_xx,_62516),cl_eql(_62516,t,_62470),(_62470\==[]->f_u_standard_output(_62558),cl_format([_62558,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_62556),_63092=t;get_var(_58248,u_xx_gate_dbg_xx,_62640),(_62640\==[]->get_var(_58248,u_xx_gate_dbg_xx,_62746),cl_format([_62746,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_76910),_63064=t;get_var(_58248,u_else,_62870),(_62870\==[]->_63036=[];_63006=[],_63036=_63006),_63064=_63036),_63092=_63064),_61692=_63092;_61692=[]).
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_59662),cl_assoc(u_rule,_59662,[],_59632),(_59632\==[]->(get_var(_58248,u_xx_ndbg_interests_xx,_59796),cl_assoc(u_rule,_59796,[],_59832),cl_member(u_all,_59832,_59764),f_u_t_or_nil(_59764,_60482),_60482\==[],_59758=_60482->true;_59890=closure([_60252|_58248],_60152,[u_x],(get_var(_60252,u_x,_59906),get_var(_60252,u_xx_ndbg_items_xx,_60010),cl_member(_59906,_60010,_60126),f_u_t_or_nil(_60126,_60152))),get_var(_58248,u_xx_ndbg_interests_xx,_60318),cl_assoc(u_rule,_60318,[],_60372),cl_cdr(_60372,_60286),cl_some(_59890,_60286,_60418),f_u_t_or_nil(_60418,_60456),_59758=_60456),_59598=_59758;_59598=[]),(_59598\==[]->get_var(_58248,u_xx_gate_dbg_xx,_60546),f_u_begin_roman_font(_60546,_60544),get_var(_58248,u_xx_ndbg_interests_xx,_60790),cl_assoc(u_rule,_60790,[],_60746),(_60746\==[]->(get_var(_58248,u_xx_ndbg_interests_xx,_60862),cl_assoc(u_rule,_60862,[],_60888),cl_member(u_all,_60888,_60914),f_u_t_or_nil(_60914,_60940),_60940\==[],_61384=_60940->true;_61158=closure([_61022|_58248],_61118,[u_x],(get_var(_61022,u_x,_60980),get_var(_61022,u_xx_ndbg_items_xx,_61050),cl_member(_60980,_61050,_61092),f_u_t_or_nil(_61092,_61118))),get_var(_58248,u_xx_ndbg_interests_xx,_61204),cl_assoc(u_rule,_61204,[],_61266),cl_cdr(_61266,_61318),cl_some(_61158,_61318,_61292),f_u_t_or_nil(_61292,_61344),_61384=_61344),_60702=_61384;_60702=[]),(_60702\==[]->get_var(_58248,u_xx_gate_dbg_xx,_61458),cl_eql(_61458,t,_61414),(_61414\==[]->f_u_standard_output(_61488),cl_format([_61488,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_61486),_61934=t;get_var(_58248,u_xx_gate_dbg_xx,_61542),(_61542\==[]->get_var(_58248,u_xx_gate_dbg_xx,_61616),cl_format([_61616,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_61984),_61908=t;get_var(_58248,u_else,_61724),(_61724\==[]->_61882=[];_61854=[],_61882=_61854),_61908=_61882),_61934=_61908),_60684=_61934;_60684=[]),get_var(_58248,u_xx_gate_dbg_xx,_62014),f_u_end_font(_62014,_61996),get_var(_58248,u_xx_gate_dbg_xx,_62078),f_u_do_newline(_62078,_62986),_59518=_62986;_59518=[]).
*/
/*
% code:-get_var(_58248,u_xx_ndbg_interests_xx,_59502),cl_assoc(u_rule,_59502,[],_59472),(_59472\==[]->(get_var(_58248,u_xx_ndbg_interests_xx,_59636),cl_assoc(u_rule,_59636,[],_59672),cl_member(u_all,_59672,_59604),f_u_t_or_nil(_59604,_60222),_60222\==[],_59598=_60222->true;_59730=closure([_60010|_58248],_59910,[u_x],(get_var(_60010,u_x,_59746),get_var(_60010,u_xx_ndbg_items_xx,_59814),cl_member(_59746,_59814,_59884),f_u_t_or_nil(_59884,_59910))),get_var(_58248,u_xx_ndbg_interests_xx,_60076),cl_assoc(u_rule,_60076,[],_60112),cl_cdr(_60112,_60044),cl_some(_59730,_60044,_60158),f_u_t_or_nil(_60158,_60196),_59598=_60196),_59438=_59598;_59438=[]),(_59438\==[]->get_var(_58248,u_xx_gate_dbg_xx,_60286),f_u_begin_roman_font(_60286,_60284),get_var(_58248,u_xx_ndbg_interests_xx,_60496),cl_assoc(u_rule,_60496,[],_60452),(_60452\==[]->(get_var(_58248,u_xx_ndbg_interests_xx,_60564),cl_assoc(u_rule,_60564,[],_60590),cl_member(u_all,_60590,_60616),f_u_t_or_nil(_60616,_60642),_60642\==[],_61078=_60642->true;_60852=closure([_60720|_58248],_60812,[u_x],(get_var(_60720,u_x,_60682),get_var(_60720,u_xx_ndbg_items_xx,_60748),cl_member(_60682,_60748,_60786),f_u_t_or_nil(_60786,_60812))),get_var(_58248,u_xx_ndbg_interests_xx,_60898),cl_assoc(u_rule,_60898,[],_60960),cl_cdr(_60960,_61012),cl_some(_60852,_61012,_60986),f_u_t_or_nil(_60986,_61038),_61078=_61038),_60410=_61078;_60410=[]),(_60410\==[]->get_var(_58248,u_xx_gate_dbg_xx,_61152),cl_eql(_61152,t,_61108),(_61108\==[]->f_u_standard_output(_61178),cl_format([_61178,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_61176),_61596=t;get_var(_58248,u_xx_gate_dbg_xx,_61232),(_61232\==[]->get_var(_58248,u_xx_gate_dbg_xx,_61306),cl_format([_61306,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56936],_61646),_61570=t;get_var(_58248,u_else,_61390),(_61390\==[]->_61544=[];_61516=[],_61544=_61516),_61570=_61544),_61596=_61570),_60392=_61596;_60392=[]),get_var(_58248,u_xx_gate_dbg_xx,_61676),f_u_end_font(_61676,_61658),get_var(_58248,u_xx_gate_dbg_xx,_61704),f_u_do_newline(_61704,_62606),_59356=_62606;_59356=[]).
*/
wl:lambda_def(defun, u_pickup, f_u_pickup, [u_goal], [[let, [[u_context, [u_ob_c36_get, u_goal, [quote, u_activation_context]]], [u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]).
wl:arglist_info(u_pickup, [u_goal], [Goal_Param], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_pickup).

/*

### Compiled:  `U::PICKUP` 
*/
f_u_pickup(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c36_get(Goal_Param, u_activation_context, Context_Init),
	f_u_ob_c36_get(Goal_Param, u_top_level_goal, Top_level_goal_Init),
	LEnv=[[bv(u_context, Context_Init), bv(u_top_level_goal, Top_level_goal_Init)]|Env],
	get_var(LEnv, u_context, Context_Get),
	f_u_ob_c36_set(Context_Get, u_children, [], C36_set_Ret),
	get_var(LEnv, u_context, Context_Get20),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Get20,
			     Next_context_Ret),
	get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	cl_assoc(u_rule, Xx_ndbg_interests_xx_Get, [], IFTEST23),
	(   IFTEST23\==[]
	->  (   get_var(LEnv,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get27),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get27, [], Assoc_Ret28),
		cl_member(u_all, Assoc_Ret28, Or_nil_Param),
		f_u_t_or_nil(Or_nil_Param, Or_nil_Ret),
		Or_nil_Ret\==[],
		IFTEST=Or_nil_Ret
	    ->  true
	    ;   Lambda=closure([_GEnv|LEnv], Or_nil_Ret35, [u_x],  (get_var(_GEnv, u_x, X_Get), get_var(_GEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get), cl_member(X_Get, Xx_ndbg_items_xx_Get, Or_nil_Param34), f_u_t_or_nil(Or_nil_Param34, Or_nil_Ret35))),
		get_var(LEnv,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get39),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get39, [], Assoc_Ret40),
		cl_cdr(Assoc_Ret40, Cdr_Ret),
		cl_some(Lambda, Cdr_Ret, Or_nil_Param41),
		f_u_t_or_nil(Or_nil_Param41, Or_nil_Ret43),
		IFTEST=Or_nil_Ret43
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    f_u_begin_roman_font(Xx_gate_dbg_xx_Get, Roman_font_Ret),
	    get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get51),
	    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get51, [], IFTEST49),
	    (   IFTEST49\==[]
	    ->  (   get_var(LEnv,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get53),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get53, [], Assoc_Ret54),
		    cl_member(u_all, Assoc_Ret54, Or_nil_Param55),
		    f_u_t_or_nil(Or_nil_Param55, Or_nil_Ret56),
		    Or_nil_Ret56\==[],
		    IFTEST47=Or_nil_Ret56
		->  true
		;   Lambda63=closure([_GEnv58|LEnv], Or_nil_Ret61, [u_x],  (get_var(_GEnv58, u_x, X_Get57), get_var(_GEnv58, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get59), cl_member(X_Get57, Xx_ndbg_items_xx_Get59, Or_nil_Param60), f_u_t_or_nil(Or_nil_Param60, Or_nil_Ret61))),
		    get_var(LEnv,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get65),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get65, [], Assoc_Ret66),
		    cl_cdr(Assoc_Ret66, Cdr_Ret68),
		    cl_some(Lambda63, Cdr_Ret68, Or_nil_Param67),
		    f_u_t_or_nil(Or_nil_Param67, Or_nil_Ret69),
		    IFTEST47=Or_nil_Ret69
		)
	    ;   IFTEST47=[]
	    ),
	    (   IFTEST47\==[]
	    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get74),
		cl_eql(Xx_gate_dbg_xx_Get74, t, IFTEST72),
		(   IFTEST72\==[]
		->  f_u_standard_output(Standard_output_Ret),
		    cl_format(
			      [ Standard_output_Ret,
				'$ARRAY'([*],
					 claz_base_character,
					 "DAYDREAMER ~A pickup"),
				Goal_Param
			      ],
			      Format_Ret),
		    ElseResult86=t
		;   get_var(LEnv, u_xx_gate_dbg_xx, IFTEST77),
		    (   IFTEST77\==[]
		    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get80),
			cl_format(
				  [ Xx_gate_dbg_xx_Get80,
				    '$ARRAY'([*],
					     claz_base_character,
					     "DAYDREAMER ~A pickup"),
				    Goal_Param
				  ],
				  Format_Ret91),
			ElseResult86=t
		    ;   get_var(LEnv, u_else, IFTEST82),
			(   IFTEST82\==[]
			->  ElseResult86=[]
			;   ElseResult86=[]
			)
		    )
		)
	    ;   ElseResult86=[]
	    ),
	    get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get92),
	    f_u_end_font(Xx_gate_dbg_xx_Get92, End_font_Ret),
	    get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get93),
	    f_u_do_newline(Xx_gate_dbg_xx_Get93, TrueResult94),
	    _55132=TrueResult94
	;   _55132=[]
	),
	f_u_daydreamer_control0(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_pickup, classof, claz_function),
   set_opv(u_pickup, compile_as, kw_function),
   set_opv(u_pickup, function, f_u_pickup),
   DefunResult=u_pickup.
/*
:- side_effect(assert_lsp(u_pickup,
			  wl:lambda_def(defun, u_pickup, f_u_pickup, [u_goal], [[let, [[u_context, [u_ob_c36_get, u_goal, [quote, u_activation_context]]], [u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]))).
*/
/*
:- side_effect(assert_lsp(u_pickup,
			  wl:arglist_info(u_pickup, [u_goal], [Goal_Param], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pickup, wl:init_args(exact_only, u_pickup))).
*/
/*
(defun pickup-in (goal context)
  (let ((top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER "(defun pickup-in (goal context)\n  (let ((top-level-goal (ob$get goal 'top-level-goal)))\n       (ob$set context 'children nil)\n       (set-next-context top-level-goal context)\n       (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER ~A pickup\" goal)\n       (daydreamer-control0)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1553 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'pickup-in',[goal,context],[let,[['top-level-goal',['ob$get',goal,[quote,'top-level-goal']]]],['ob$set',context,[quote,children],[]],['set-next-context','top-level-goal',context],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER ~A pickup"),goal],['daydreamer-control0']]])
/*
% macroexpand:-[u_ndbg_roman_nl,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal].
*/
/*
% into:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% macroexpand:-[u_if_interested_in,u_rule,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]].
*/
/*
% into:-[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[progn,[u_begin_roman_font,u_xx_gate_dbg_xx],[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],[u_end_font,u_xx_gate_dbg_xx],[u_do_newline,u_xx_gate_dbg_xx]],[]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58098,u_xx_ndbg_interests_xx,_60984),cl_assoc(u_rule,_60984,[],_60782).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_63192),cl_assoc(u_rule,_63192,[],_71774).
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_63192),cl_assoc(u_rule,_63192,[],_71774),cl_member(u_all,_71774,_61302),f_u_t_or_nil(_61302,_61070).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_61908,u_x,_62392),get_var(_61908,u_xx_ndbg_items_xx,_63954),cl_member(_62392,_63954,_73574),f_u_t_or_nil(_73574,_80612).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_173390),cl_assoc(u_rule,_173390,[],_181994).
*/
/*
% code:-_61318=closure([_61732|_58014],_61626,[u_x],(get_var(_61732,u_x,_61334),get_var(_61732,u_xx_ndbg_items_xx,_61476),cl_member(_61334,_61476,_61598),f_u_t_or_nil(_61598,_61626))),get_var(_58014,u_xx_ndbg_interests_xx,_61800),cl_assoc(u_rule,_61800,[],_61860),cl_cdr(_61860,_61768),cl_some(_61318,_61768,_63950),f_u_t_or_nil(_63950,_80780).
*/
/*
% macroexpand:-[u_ndbg,u_xx_gate_dbg_xx,u_rule,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal].
*/
/*
% into:-[progn,[if,[and,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx],[or,[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]],[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]]],[cond,[[u_eq_c63,u_xx_gate_dbg_xx,t],[format,[u_standard_output],'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],t],[u_xx_gate_dbg_xx,[format,u_xx_gate_dbg_xx,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),u_goal],t],[u_else,[]]],[]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_65586),cl_assoc(u_rule,_65586,[],_74196).
*/
/*
% macroexpand:-[u_memq_c63,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]].
*/
/*
% into:-[u_t_or_nil,[member,[quote,u_all],[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_64720),cl_assoc(u_rule,_64720,[],_65872).
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_64720),cl_assoc(u_rule,_64720,[],_65872),cl_member(u_all,_65872,_86968),f_u_t_or_nil(_86968,_94098).
*/
/*
% macroexpand:-[u_any_c63,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]].
*/
/*
% into:-[u_t_or_nil,[some,[lambda,[u_x],[u_memq_c63,u_x,u_xx_ndbg_items_xx]],[cdr,[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx]]]].
*/
/*
% macroexpand:-[u_memq_c63,u_x,u_xx_ndbg_items_xx].
*/
/*
% into:-[u_t_or_nil,[member,u_x,u_xx_ndbg_items_xx]].
*/
/*
% code:-get_var(_66866,u_x,_66204),get_var(_66866,u_xx_ndbg_items_xx,_68406),cl_member(_66204,_68406,_76534),f_u_t_or_nil(_76534,_83552).
*/
/*
% macroexpand:-[ext_assq,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% into:-[assoc,[quote,u_rule],u_xx_ndbg_interests_xx].
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_176148),cl_assoc(u_rule,_176148,[],_184770).
*/
/*
% code:-_64742=closure([_64548|_58014],_64650,[u_x],(get_var(_64548,u_x,_64504),get_var(_64548,u_xx_ndbg_items_xx,_64578),cl_member(_64504,_64578,_64622),f_u_t_or_nil(_64622,_64650))),get_var(_58014,u_xx_ndbg_interests_xx,_64826),cl_assoc(u_rule,_64826,[],_64904),cl_cdr(_64904,_81542),cl_some(_64742,_81542,_66994),f_u_t_or_nil(_66994,_83824).
*/
/*
% macroexpand:-[u_eq_c63,u_xx_gate_dbg_xx,t].
*/
/*
% into:-[eql,u_xx_gate_dbg_xx,t].
*/
/*
% code:-get_var(_58014,u_xx_gate_dbg_xx,_179090),cl_eql(_179090,t,_150242).
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_61448),cl_assoc(u_rule,_61448,[],_61404),(_61404\==[]->(get_var(_58014,u_xx_ndbg_interests_xx,_61520),cl_assoc(u_rule,_61520,[],_61546),cl_member(u_all,_61546,_61572),f_u_t_or_nil(_61572,_61598),_61598\==[],_62042=_61598->true;_61816=closure([_61680|_58014],_61776,[u_x],(get_var(_61680,u_x,_61638),get_var(_61680,u_xx_ndbg_items_xx,_61708),cl_member(_61638,_61708,_61750),f_u_t_or_nil(_61750,_61776))),get_var(_58014,u_xx_ndbg_interests_xx,_61862),cl_assoc(u_rule,_61862,[],_61924),cl_cdr(_61924,_61976),cl_some(_61816,_61976,_61950),f_u_t_or_nil(_61950,_62002),_62042=_62002),_61360=_62042;_61360=[]),(_61360\==[]->get_var(_58014,u_xx_gate_dbg_xx,_62118),cl_eql(_62118,t,_62072),(_62072\==[]->f_u_standard_output(_62160),cl_format([_62160,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_62158),_62682=t;get_var(_58014,u_xx_gate_dbg_xx,_62236),(_62236\==[]->get_var(_58014,u_xx_gate_dbg_xx,_62342),cl_format([_62342,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_76500),_62654=t;get_var(_58014,u_else,_62460),(_62460\==[]->_62626=[];_62596=[],_62626=_62596),_62654=_62626),_62682=_62654),_61294=_62682;_61294=[]).
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_59264),cl_assoc(u_rule,_59264,[],_59234),(_59234\==[]->(get_var(_58014,u_xx_ndbg_interests_xx,_59398),cl_assoc(u_rule,_59398,[],_59434),cl_member(u_all,_59434,_59366),f_u_t_or_nil(_59366,_60084),_60084\==[],_59360=_60084->true;_59492=closure([_59854|_58014],_59754,[u_x],(get_var(_59854,u_x,_59508),get_var(_59854,u_xx_ndbg_items_xx,_59612),cl_member(_59508,_59612,_59728),f_u_t_or_nil(_59728,_59754))),get_var(_58014,u_xx_ndbg_interests_xx,_59920),cl_assoc(u_rule,_59920,[],_59974),cl_cdr(_59974,_59888),cl_some(_59492,_59888,_60020),f_u_t_or_nil(_60020,_60058),_59360=_60058),_59200=_59360;_59200=[]),(_59200\==[]->get_var(_58014,u_xx_gate_dbg_xx,_60148),f_u_begin_roman_font(_60148,_60146),get_var(_58014,u_xx_ndbg_interests_xx,_60392),cl_assoc(u_rule,_60392,[],_60348),(_60348\==[]->(get_var(_58014,u_xx_ndbg_interests_xx,_60464),cl_assoc(u_rule,_60464,[],_60490),cl_member(u_all,_60490,_60516),f_u_t_or_nil(_60516,_60542),_60542\==[],_60986=_60542->true;_60760=closure([_60624|_58014],_60720,[u_x],(get_var(_60624,u_x,_60582),get_var(_60624,u_xx_ndbg_items_xx,_60652),cl_member(_60582,_60652,_60694),f_u_t_or_nil(_60694,_60720))),get_var(_58014,u_xx_ndbg_interests_xx,_60806),cl_assoc(u_rule,_60806,[],_60868),cl_cdr(_60868,_60920),cl_some(_60760,_60920,_60894),f_u_t_or_nil(_60894,_60946),_60986=_60946),_60304=_60986;_60304=[]),(_60304\==[]->get_var(_58014,u_xx_gate_dbg_xx,_61060),cl_eql(_61060,t,_61016),(_61016\==[]->f_u_standard_output(_61090),cl_format([_61090,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_61088),_61530=t;get_var(_58014,u_xx_gate_dbg_xx,_61144),(_61144\==[]->get_var(_58014,u_xx_gate_dbg_xx,_61218),cl_format([_61218,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_61580),_61504=t;get_var(_58014,u_else,_61320),(_61320\==[]->_61478=[];_61450=[],_61478=_61450),_61504=_61478),_61530=_61504),_60286=_61530;_60286=[]),get_var(_58014,u_xx_gate_dbg_xx,_61610),f_u_end_font(_61610,_61592),get_var(_58014,u_xx_gate_dbg_xx,_61674),f_u_do_newline(_61674,_62582),_59120=_62582;_59120=[]).
*/
/*
% code:-get_var(_58014,u_xx_ndbg_interests_xx,_59104),cl_assoc(u_rule,_59104,[],_59074),(_59074\==[]->(get_var(_58014,u_xx_ndbg_interests_xx,_59238),cl_assoc(u_rule,_59238,[],_59274),cl_member(u_all,_59274,_59206),f_u_t_or_nil(_59206,_59824),_59824\==[],_59200=_59824->true;_59332=closure([_59612|_58014],_59512,[u_x],(get_var(_59612,u_x,_59348),get_var(_59612,u_xx_ndbg_items_xx,_59416),cl_member(_59348,_59416,_59486),f_u_t_or_nil(_59486,_59512))),get_var(_58014,u_xx_ndbg_interests_xx,_59678),cl_assoc(u_rule,_59678,[],_59714),cl_cdr(_59714,_59646),cl_some(_59332,_59646,_59760),f_u_t_or_nil(_59760,_59798),_59200=_59798),_59040=_59200;_59040=[]),(_59040\==[]->get_var(_58014,u_xx_gate_dbg_xx,_59888),f_u_begin_roman_font(_59888,_59886),get_var(_58014,u_xx_ndbg_interests_xx,_60098),cl_assoc(u_rule,_60098,[],_60054),(_60054\==[]->(get_var(_58014,u_xx_ndbg_interests_xx,_60166),cl_assoc(u_rule,_60166,[],_60192),cl_member(u_all,_60192,_60218),f_u_t_or_nil(_60218,_60244),_60244\==[],_60680=_60244->true;_60454=closure([_60322|_58014],_60414,[u_x],(get_var(_60322,u_x,_60284),get_var(_60322,u_xx_ndbg_items_xx,_60350),cl_member(_60284,_60350,_60388),f_u_t_or_nil(_60388,_60414))),get_var(_58014,u_xx_ndbg_interests_xx,_60500),cl_assoc(u_rule,_60500,[],_60562),cl_cdr(_60562,_60614),cl_some(_60454,_60614,_60588),f_u_t_or_nil(_60588,_60640),_60680=_60640),_60012=_60680;_60012=[]),(_60012\==[]->get_var(_58014,u_xx_gate_dbg_xx,_60754),cl_eql(_60754,t,_60710),(_60710\==[]->f_u_standard_output(_60780),cl_format([_60780,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_60778),_61192=t;get_var(_58014,u_xx_gate_dbg_xx,_60834),(_60834\==[]->get_var(_58014,u_xx_gate_dbg_xx,_60908),cl_format([_60908,'$ARRAY'([*],claz_base_character,"DAYDREAMER ~A pickup"),_56852],_61242),_61166=t;get_var(_58014,u_else,_60986),(_60986\==[]->_61140=[];_61112=[],_61140=_61112),_61166=_61140),_61192=_61166),_59994=_61192;_59994=[]),get_var(_58014,u_xx_gate_dbg_xx,_61272),f_u_end_font(_61272,_61254),get_var(_58014,u_xx_gate_dbg_xx,_61300),f_u_do_newline(_61300,_62202),_58958=_62202;_58958=[]).
*/
wl:lambda_def(defun, u_pickup_in, f_u_pickup_in, [u_goal, u_context], [[let, [[u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]).
wl:arglist_info(u_pickup_in, [u_goal, u_context], [Goal_Param, Context_Param], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_pickup_in).

/*

### Compiled:  `U::PICKUP-IN` 
*/
f_u_pickup_in(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Goal_Param, u_top_level_goal, Top_level_goal_Init),
	LEnv=[[bv(u_top_level_goal, Top_level_goal_Init)]|Env],
	f_u_ob_c36_set(Context_Param, u_children, [], C36_set_Ret),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Param,
			     Next_context_Ret),
	get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	cl_assoc(u_rule, Xx_ndbg_interests_xx_Get, [], IFTEST23),
	(   IFTEST23\==[]
	->  (   get_var(LEnv,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get27),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get27, [], Assoc_Ret28),
		cl_member(u_all, Assoc_Ret28, Or_nil_Param),
		f_u_t_or_nil(Or_nil_Param, Or_nil_Ret),
		Or_nil_Ret\==[],
		IFTEST=Or_nil_Ret
	    ->  true
	    ;   Lambda=closure([_GEnv|LEnv], Or_nil_Ret35, [u_x],  (get_var(_GEnv, u_x, X_Get), get_var(_GEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get), cl_member(X_Get, Xx_ndbg_items_xx_Get, Or_nil_Param34), f_u_t_or_nil(Or_nil_Param34, Or_nil_Ret35))),
		get_var(LEnv,
			u_xx_ndbg_interests_xx,
			Xx_ndbg_interests_xx_Get39),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get39, [], Assoc_Ret40),
		cl_cdr(Assoc_Ret40, Cdr_Ret),
		cl_some(Lambda, Cdr_Ret, Or_nil_Param41),
		f_u_t_or_nil(Or_nil_Param41, Or_nil_Ret43),
		IFTEST=Or_nil_Ret43
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    f_u_begin_roman_font(Xx_gate_dbg_xx_Get, Roman_font_Ret),
	    get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get51),
	    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get51, [], IFTEST49),
	    (   IFTEST49\==[]
	    ->  (   get_var(LEnv,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get53),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get53, [], Assoc_Ret54),
		    cl_member(u_all, Assoc_Ret54, Or_nil_Param55),
		    f_u_t_or_nil(Or_nil_Param55, Or_nil_Ret56),
		    Or_nil_Ret56\==[],
		    IFTEST47=Or_nil_Ret56
		->  true
		;   Lambda63=closure([_GEnv58|LEnv], Or_nil_Ret61, [u_x],  (get_var(_GEnv58, u_x, X_Get57), get_var(_GEnv58, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get59), cl_member(X_Get57, Xx_ndbg_items_xx_Get59, Or_nil_Param60), f_u_t_or_nil(Or_nil_Param60, Or_nil_Ret61))),
		    get_var(LEnv,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get65),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get65, [], Assoc_Ret66),
		    cl_cdr(Assoc_Ret66, Cdr_Ret68),
		    cl_some(Lambda63, Cdr_Ret68, Or_nil_Param67),
		    f_u_t_or_nil(Or_nil_Param67, Or_nil_Ret69),
		    IFTEST47=Or_nil_Ret69
		)
	    ;   IFTEST47=[]
	    ),
	    (   IFTEST47\==[]
	    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get74),
		cl_eql(Xx_gate_dbg_xx_Get74, t, IFTEST72),
		(   IFTEST72\==[]
		->  f_u_standard_output(Standard_output_Ret),
		    cl_format(
			      [ Standard_output_Ret,
				'$ARRAY'([*],
					 claz_base_character,
					 "DAYDREAMER ~A pickup"),
				Goal_Param
			      ],
			      Format_Ret),
		    ElseResult86=t
		;   get_var(LEnv, u_xx_gate_dbg_xx, IFTEST77),
		    (   IFTEST77\==[]
		    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get80),
			cl_format(
				  [ Xx_gate_dbg_xx_Get80,
				    '$ARRAY'([*],
					     claz_base_character,
					     "DAYDREAMER ~A pickup"),
				    Goal_Param
				  ],
				  Format_Ret91),
			ElseResult86=t
		    ;   get_var(LEnv, u_else, IFTEST82),
			(   IFTEST82\==[]
			->  ElseResult86=[]
			;   ElseResult86=[]
			)
		    )
		)
	    ;   ElseResult86=[]
	    ),
	    get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get92),
	    f_u_end_font(Xx_gate_dbg_xx_Get92, End_font_Ret),
	    get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get93),
	    f_u_do_newline(Xx_gate_dbg_xx_Get93, TrueResult94),
	    _55134=TrueResult94
	;   _55134=[]
	),
	f_u_daydreamer_control0(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_pickup_in, classof, claz_function),
   set_opv(u_pickup_in, compile_as, kw_function),
   set_opv(u_pickup_in, function, f_u_pickup_in),
   DefunResult=u_pickup_in.
/*
:- side_effect(assert_lsp(u_pickup_in,
			  wl:lambda_def(defun, u_pickup_in, f_u_pickup_in, [u_goal, u_context], [[let, [[u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]))).
*/
/*
:- side_effect(assert_lsp(u_pickup_in,
			  wl:arglist_info(u_pickup_in, [u_goal, u_context], [Goal_Param, Context_Param], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pickup_in, wl:init_args(exact_only, u_pickup_in))).
*/
/*
(defun resume (goal)
  (change-tlg-status goal 'runable)
  (daydreamer-control0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1827 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,resume,[goal],['change-tlg-status',goal,[quote,runable]],['daydreamer-control0']])
wl:lambda_def(defun, u_resume, f_u_resume, [u_goal], [[u_change_tlg_status, u_goal, [quote, u_runable]], [u_daydreamer_control0]]).
wl:arglist_info(u_resume, [u_goal], [Goal_Param], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_resume).

/*

### Compiled:  `U::RESUME` 
*/
f_u_resume(Goal_Param, FnResult) :-
	f_u_change_tlg_status(Goal_Param, u_runable, Runable),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume, classof, claz_function),
   set_opv(u_resume, compile_as, kw_function),
   set_opv(u_resume, function, f_u_resume),
   DefunResult=u_resume.
/*
:- side_effect(assert_lsp(u_resume,
			  wl:lambda_def(defun, u_resume, f_u_resume, [u_goal], [[u_change_tlg_status, u_goal, [quote, u_runable]], [u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_resume,
			  wl:arglist_info(u_resume, [u_goal], [Goal_Param], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_resume, wl:init_args(exact_only, u_resume))).
*/
/*
(defun resume-infs ()
  (yloop (yfor fact in *needs*)
        (ydo (cx$touch-fact *reality-lookahead* fact)))
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  (set-state 'performance)
  (daydreamer-control0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1910 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'resume-infs',[],[yloop,[yfor,fact,in,'*needs*'],[ydo,['cx$touch-fact','*reality-lookahead*',fact]]],['run-inferences','*reality-lookahead*',[],'*me-belief-path*'],['set-state',[quote,performance]],['daydreamer-control0']])throw(100,u_fact)
