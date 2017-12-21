
(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1124804,plevel:initialise_error(_1124804)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1127152,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1129412,[expand(false)]),_1129390,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1130112,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1131518,system:'$exception_in_directive'(_1131518)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1132206,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1134214,[if(not_loaded)]),_1134192,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1134830,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1135454,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1136068,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1138404,system:'$exception_in_directive'(_1138404)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1140548,system:'$initialization_error'(_1140548,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1147204,wamcl:gripe_problem(uncaught(_1147204),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1147856)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1147856)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1148132)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1148420)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1148668))).   %  eightball.pl:66: 
(75)*[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1148928))).   %  eightball.pl:104: 
(76)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1149174)),_1149150,user:gripe_problem(uncaught(_1149150),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1149174))),!,fail))).   %  init.pl:371: 
(77)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1149390)).   %  callp.pl:111: 
(78)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1149608)).   %  callp.pl:114: 
(79)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1149818))).   %  eightball.pl:153: 
(80)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1150008)).   %  eightball.pl:63: 
(81)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1150186)).   %  eightball.pl:66: 
(82)*[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1150362)).   %  eightball.pl:104: 
(83)[system] system:catch(user:pl_load('sanity-test.lisp',[],_1150524),error(instantiation_error,context(system:file_name_extension/3,_1150536)),user:gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1150536))),(rtrace(user:pl_load('sanity-test.lisp',[],_1150524)),!,fail))).   %  init.pl:371: 
(84)[user] '8ball':gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1150670))),(rtrace(user:pl_load('sanity-test.lisp',[],_1150706)),!,fail)).   %  eightball.pl:109: 
(85)[user] '8ball':always_catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1150808))),(rtrace(user:pl_load('sanity-test.lisp',[],_1150844)),!,fail))).   %  eightball.pl:93: 
(86)[system] system:catch(user:catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1150948))),(rtrace(user:pl_load('sanity-test.lisp',[],_1150984)),!,fail)),'$aborted',notrace),_1150910,user:(lmsg(always_uncaught(_1150910)),notrace,!,fail)).   %  init.pl:371: 
(87)$[system] system:catch(user:gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151050))),(rtrace(user:pl_load('sanity-test.lisp',[],_1151086)),!,fail)),'$aborted',user:notrace).   %  init.pl:371: 
(88)[user] '8ball':gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151134))),(rtrace(user:pl_load('sanity-test.lisp',[],_1151170)),!,fail)).   %  eightball.pl:110: 
(89)$[user] system:notrace((lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151220))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151256)),!,fail)),dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151220))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151256)),!,fail)))).  no(clause) 
(90)$[system] system:'$c_call_prolog'.  no(clause) 
(91)[system] system:(user:lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151372))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151408)),!,fail)),user:(dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151372))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151408)),!,fail)))).   %  init.pl:211: 
(92)$[system] system:(user:lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151412))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151448)),!,fail)),user:(dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1151412))):-rtrace(user:pl_load('sanity-test.lisp',[],_1151448)),!,fail)))).  no(clause) 
(93)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(94)$[user] first:zotrace((prolog_current_frame(1528),b_setval('$dump_frame',1528),dumpST1)).   %  first.pl:432: 
(95)$[user] system:(prolog_current_frame(1528),b_setval('$dump_frame',1528),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1182314,plevel:initialise_error(_1182314)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1184590,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1186778,[expand(false)]),_1186756,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1187454,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1188812,system:'$exception_in_directive'(_1188812)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1189476,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1191412,[if(not_loaded)]),_1191390,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1192004,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1192604,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1193194,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1195434,system:'$exception_in_directive'(_1195434)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1197482,system:'$initialization_error'(_1197482,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1203754,wamcl:gripe_problem(uncaught(_1203754),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1204358)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1204358)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1204610)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1204874)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1205098))).   %  eightball.pl:66: 
(75)*[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1205334))).   %  eightball.pl:104: 
(76)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1205556)),_1205532,user:gripe_problem(uncaught(_1205532),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1205556))),!,fail))).   %  init.pl:371: 
(77)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1205748)).   %  callp.pl:111: 
(78)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1205942)).   %  callp.pl:114: 
(79)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1206128))).   %  eightball.pl:153: 
(80)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1206294)).   %  eightball.pl:63: 
(81)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1206448)).   %  eightball.pl:66: 
(82)*[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1206600)).   %  eightball.pl:104: 
(83)[system] system:catch(user:pl_load('sanity-test.lisp',[],_1206738),error(instantiation_error,context(system:file_name_extension/3,_1206750)),user:gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1206750))),(rtrace(user:pl_load('sanity-test.lisp',[],_1206738)),!,fail))).   %  init.pl:371: 
(84)[user] '8ball':gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1206860))),(rtrace(user:pl_load('sanity-test.lisp',[],_1206896)),!,fail)).   %  eightball.pl:109: 
(85)[user] '8ball':always_catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1206974))),(rtrace(user:pl_load('sanity-test.lisp',[],_1207010)),!,fail))).   %  eightball.pl:93: 
(86)[system] system:catch(user:catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1207090))),(rtrace(user:pl_load('sanity-test.lisp',[],_1207126)),!,fail)),'$aborted',notrace),_1207052,user:(lmsg(always_uncaught(_1207052)),notrace,!,fail)).   %  init.pl:371: 
(87)$[system] system:catch(user:gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1207168))),(rtrace(user:pl_load('sanity-test.lisp',[],_1207204)),!,fail)),'$aborted',user:notrace).   %  init.pl:371: 
(88)[user] '8ball':gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1207228))),(rtrace(user:pl_load('sanity-test.lisp',[],_1207264)),!,fail)).   %  eightball.pl:110: 
(89)*[user] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(90)[user] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(91)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(92)$[user] first:zotrace((prolog_current_frame(1459),b_setval('$dump_frame',1459),dumpST1)).   %  first.pl:432: 
(93)$[user] system:(prolog_current_frame(1459),b_setval('$dump_frame',1459),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1238368,plevel:initialise_error(_1238368)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1240644,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1242832,[expand(false)]),_1242810,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1243508,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1244866,system:'$exception_in_directive'(_1244866)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1245530,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1247466,[if(not_loaded)]),_1247444,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1248058,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1248658,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1249248,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1251488,system:'$exception_in_directive'(_1251488)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1253536,system:'$initialization_error'(_1253536,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1259808,wamcl:gripe_problem(uncaught(_1259808),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1260412)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1260412)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1260664)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1260928)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1261152))).   %  eightball.pl:66: 
(75)*[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1261388))).   %  eightball.pl:104: 
(76)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1261610)),_1261586,user:gripe_problem(uncaught(_1261586),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1261610))),!,fail))).   %  init.pl:371: 
(77)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1261802)).   %  callp.pl:111: 
(78)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1261996)).   %  callp.pl:114: 
(79)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1262182))).   %  eightball.pl:153: 
(80)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1262348)).   %  eightball.pl:63: 
(81)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1262502)).   %  eightball.pl:66: 
(82)[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1262654)).   %  eightball.pl:104: 
(83)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1262800)))).   %  eightball.pl:109: 
(84)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1262926))))).   %  eightball.pl:93: 
(85)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263054)))),'$aborted',notrace),_1263008,user:(lmsg(always_uncaught(_1263008)),notrace,!,fail)).   %  init.pl:371: 
(86)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263144)))),'$aborted',user:notrace).   %  init.pl:371: 
(87)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263216)))).   %  eightball.pl:110: 
(88)$[user] system:notrace((lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263290))))),dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263290))))))).  no(clause) 
(89)$[system] system:'$c_call_prolog'.  no(clause) 
(90)[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263418))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263418))))))).   %  init.pl:211: 
(91)$[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263446))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1263446))))))).  no(clause) 
(93)$[user] first:zotrace((prolog_current_frame(1499),b_setval('$dump_frame',1499),dumpST1)).   %  first.pl:432: 
(99)[_check] '_check':loop_check_term_frame(user:dumpST9,user:dumpST9,1,1489,user:dumpST0).   %  loop_check.pl:203: 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1293448,plevel:initialise_error(_1293448)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1295688,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1297840,[expand(false)]),_1297818,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1298504,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1299838,system:'$exception_in_directive'(_1299838)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1300490,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1302390,[if(not_loaded)]),_1302368,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1302970,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1303558,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1304136,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1306328,system:'$exception_in_directive'(_1306328)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1308328,system:'$initialization_error'(_1308328,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1314408,wamcl:gripe_problem(uncaught(_1314408),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1314988)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1314988)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1315228)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1315480)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1315692))).   %  eightball.pl:66: 
(75)*[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1315916))).   %  eightball.pl:104: 
(76)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1316126)),_1316102,user:gripe_problem(uncaught(_1316102),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1316126))),!,fail))).   %  init.pl:371: 
(77)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1316306)).   %  callp.pl:111: 
(78)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1316488)).   %  callp.pl:114: 
(79)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1316662))).   %  eightball.pl:153: 
(80)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1316816)).   %  eightball.pl:63: 
(81)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1316958)).   %  eightball.pl:66: 
(82)[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1317098)).   %  eightball.pl:104: 
(83)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1317232)))).   %  eightball.pl:109: 
(84)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1317346))))).   %  eightball.pl:93: 
(85)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1317462)))),'$aborted',notrace),_1317416,user:(lmsg(always_uncaught(_1317416)),notrace,!,fail)).   %  init.pl:371: 
(86)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1317540)))),'$aborted',user:notrace).   %  init.pl:371: 
(87)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1317600)))).   %  eightball.pl:110: 
(88)*[user] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(89)[user] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(90)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(91)$[user] first:zotrace((prolog_current_frame(1439),b_setval('$dump_frame',1439),dumpST1)).   %  first.pl:432: 
(92)$[user] system:(prolog_current_frame(1439),b_setval('$dump_frame',1439),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1343006,plevel:initialise_error(_1343006)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1345030,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1346966,[expand(false)]),_1346944,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1347558,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1348748,system:'$exception_in_directive'(_1348748)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1349328,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1351012,[if(not_loaded)]),_1350990,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1351520,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1352036,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1352542,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1354446,system:'$exception_in_directive'(_1354446)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1356158,system:'$initialization_error'(_1356158,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1361086,wamcl:gripe_problem(uncaught(_1361086),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1361522)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1361522)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1361690)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1361870)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1362010))).   %  eightball.pl:66: 
(75)[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362162))).   %  eightball.pl:104: 
(76)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362308))))).   %  eightball.pl:109: 
(77)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362434)))))).   %  eightball.pl:93: 
(78)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362562))))),'$aborted',notrace),_1362508,user:(lmsg(always_uncaught(_1362508)),notrace,!,fail)).   %  init.pl:371: 
(79)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362652))))),'$aborted',user:notrace).   %  init.pl:371: 
(80)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362724))))).   %  eightball.pl:110: 
(81)$[user] system:notrace((lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362798)))))),dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362798)))))))).  no(clause) 
(82)$[system] system:'$c_call_prolog'.  no(clause) 
(83)[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362926)))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362926)))))))).   %  init.pl:211: 
(84)$[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362954)))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1362954)))))))).  no(clause) 
(86)$[user] first:zotrace((prolog_current_frame(1221),b_setval('$dump_frame',1221),dumpST1)).   %  first.pl:432: 
(92)[_check] '_check':loop_check_term_frame(user:dumpST9,user:dumpST9,1,1211,user:dumpST0).   %  loop_check.pl:203: 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1387306,plevel:initialise_error(_1387306)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1389294,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1391194,[expand(false)]),_1391172,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1391774,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1392940,system:'$exception_in_directive'(_1392940)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1393508,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1395156,[if(not_loaded)]),_1395134,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1395652,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1396156,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1396650,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1398506,system:'$exception_in_directive'(_1398506)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1400170,system:'$initialization_error'(_1400170,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)*[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[system] system:catch(user:clear_op_buffer,_1404906,wamcl:gripe_problem(uncaught(_1404906),(rtrace(user:clear_op_buffer),!,fail))).   %  init.pl:371: 
(70)[user] in1t:clear_op_buffer.   %  init.pl:164: 
(71)*[$apply] '$apply':forall(user:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1405318)),true,<clause>(0x4beadd0)),user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1405318)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(72)$[user] system:user:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1405474)))),erase(<clause>(0x4beadd0))).  no(clause) 
(73)*[$apply] '$apply':forall(user:true,user:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1405642)))).   %  apply.pl:51: 
(74)[user] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1405770))).   %  eightball.pl:66: 
(75)[user] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1405910))).   %  eightball.pl:104: 
(76)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1406044))))).   %  eightball.pl:109: 
(77)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1406158)))))).   %  eightball.pl:93: 
(78)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1406274))))),'$aborted',notrace),_1406220,user:(lmsg(always_uncaught(_1406220)),notrace,!,fail)).   %  init.pl:371: 
(79)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1406352))))),'$aborted',user:notrace).   %  init.pl:371: 
(80)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1406412))))).   %  eightball.pl:110: 
(81)*[user] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(82)[user] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(83)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(84)$[user] first:zotrace((prolog_current_frame(1161),b_setval('$dump_frame',1161),dumpST1)).   %  first.pl:432: 
(85)$[user] system:(prolog_current_frame(1161),b_setval('$dump_frame',1161),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1426672,plevel:initialise_error(_1426672)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1428444,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1430128,[expand(false)]),_1430106,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1430636,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1431658,system:'$exception_in_directive'(_1431658)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1432154,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1433586,[if(not_loaded)]),_1433564,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1434010,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1434442,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1434864,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1436432,system:'$exception_in_directive'(_1436432)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1437808,system:'$initialization_error'(_1437808,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[wamcl] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))).   %  eightball.pl:109: 
(70)[wamcl] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer)))).   %  eightball.pl:93: 
(71)[system] system:catch(wamcl:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))),'$aborted',notrace),_1441624,wamcl:(lmsg(always_uncaught(_1441624)),notrace,!,fail)).   %  init.pl:371: 
(72)$[system] system:catch(wamcl:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))),'$aborted',wamcl:notrace).   %  init.pl:371: 
(73)[wamcl] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))).   %  eightball.pl:110: 
(74)$[wamcl] system:notrace((lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))),dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))))).  no(clause) 
(75)$[system] system:'$c_call_prolog'.  no(clause) 
(76)[system] system:(wamcl:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))),wamcl:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))))).   %  init.pl:211: 
(77)$[system] system:(wamcl:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))),wamcl:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:clear_op_buffer)))))).  no(clause) 
(79)$[wamcl] first:zotrace((prolog_current_frame(1050),b_setval('$dump_frame',1050),dumpST1)).   %  first.pl:432: 
(80)$[wamcl] system:(prolog_current_frame(1050),b_setval('$dump_frame',1050),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1461368,plevel:initialise_error(_1461368)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)*$[plevel] plevel:load_associated_files(['./wam_repl.pl']).   %  toplevel.pl:451: 
(11)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',<clause>(0x24753f0),[expand(false)])),system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]),_1463104,system:'$mt_end_load'(<clause>(0x24753f0))).   %  init.pl:443: 
(12)$[system] system:'$mt_do_load'(<clause>(0x24753f0),'./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,[expand(false)]).   %  init.pl:2068: 
(15)*$[system] system:'$do_load_file_2'('./wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',user,compiled,[expand(false)]).   %  init.pl:2124: 
(19)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1464752,[expand(false)]),_1464730,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(20)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/wam_repl.pl',_1465248,[expand(false)]).   %  init.pl:2507: 
(25)*$[system] system:'$execute_directive_3'(ensure_loaded(library(wamcl))).   %  init.pl:3034: 
(26)$[system] system:catch(user:ensure_loaded(library(wamcl)),_1466246,system:'$exception_in_directive'(_1466246)).   %  init.pl:371: 
(34)[system] system:scccu(system:with_mutex('$load_file','$mt_start_load'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',<clause>(0x2482ae0),[if(not_loaded)])),system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]),_1466730,system:'$mt_end_load'(<clause>(0x2482ae0))).   %  init.pl:443: 
(35)$[system] system:'$mt_do_load'(<clause>(0x2482ae0),library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,[if(not_loaded)]).   %  init.pl:2068: 
(38)*$[system] system:'$do_load_file_2'(library(wamcl),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',user,compiled,[if(not_loaded)]).   %  init.pl:2124: 
(42)$[system] system:scccu(system:true,system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1468126,[if(not_loaded)]),_1468104,system:'$end_consult'(lexstate(202,swi),user)).   %  init.pl:443: 
(43)*$[system] system:'$load_file'('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',_1468538,[if(not_loaded)]).   %  init.pl:2507: 
(44)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1468958,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:2573: 
(45)$[system] system:'$compile_term'((:-initialization(do_wamcl_inits,now)),_1469368,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl',-).   %  init.pl:2583: 
(46)$[system] system:'$execute_directive'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3005: 
(47)$[system] system:'$execute_directive_2'(initialization(do_wamcl_inits,now),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl').   %  init.pl:3031: 
(48)*$[system] system:'$execute_directive_3'(initialization(do_wamcl_inits,now)).   %  init.pl:3034: 
(49)$[system] system:catch(wamcl:initialization(do_wamcl_inits,now),_1470888,system:'$exception_in_directive'(_1470888)).   %  init.pl:371: 
(50)$[system] system:initialization(wamcl:do_wamcl_inits,now).   %  init.pl:481: 
(51)$[system] system:'$initialization'(now,wamcl:do_wamcl_inits,'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wamcl.pl','mcl.pl':5).   %  init.pl:492: 
(52)*$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits,'mcl.pl':5).   %  init.pl:563: 
(53)$[system] system:catch(system:'$run_init_goal'(wamcl:do_wamcl_inits),_1472216,system:'$initialization_error'(_1472216,wamcl:do_wamcl_inits,'mcl.pl':5)).   %  init.pl:371: 
(54)$[system] system:'$run_init_goal'(wamcl:do_wamcl_inits).   %  init.pl:572: 
(55)[wamcl] in1t:do_wamcl_inits.   %  init.pl:156: 
(56)[wamcl] in1t:primordial_init.   %  init.pl:140: 
(57)[wamcl] '8ball':always((ensure_env,set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(58)[wamcl] '8ball':always((set_opv(xx_package_xx,value,pkg_sys),set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(59)[wamcl] '8ball':always((set_prolog_flag(lisp_autointern,true),current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(60)[wamcl] '8ball':always((current_prolog_flag(os_argv,[swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(61)[wamcl] '8ball':always((handle_all_os_program_args([swipl,'./wam_repl.pl','--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(62)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(63)[wamcl] '8ball':always((set_prolog_flag(wamcl_init_level,2),current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(64)[wamcl] '8ball':always((current_prolog_flag(argv,['--load','sanity-test.lisp','--markdown','--quit']),handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(65)[wamcl] '8ball':always((handle_all_program_args(['--load','sanity-test.lisp','--markdown','--quit']),clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(66)[wamcl] '8ball':always((clear_op_buffer,set_prolog_flag(wamcl_init_level,3))).   %  eightball.pl:58: 
(67)[wamcl] '8ball':always(clear_op_buffer).   %  eightball.pl:66: 
(68)[wamcl] '8ball':nonquietly_must_or_rtrace(user:clear_op_buffer).   %  eightball.pl:104: 
(69)[wamcl] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))).   %  eightball.pl:109: 
(70)[wamcl] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer)))).   %  eightball.pl:93: 
(71)[system] system:catch(wamcl:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))),'$aborted',notrace),_1475816,wamcl:(lmsg(always_uncaught(_1475816)),notrace,!,fail)).   %  init.pl:371: 
(72)$[system] system:catch(wamcl:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))),'$aborted',wamcl:notrace).   %  init.pl:371: 
(73)[wamcl] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:clear_op_buffer))).   %  eightball.pl:110: 
(74)*[wamcl] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(75)[wamcl] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(76)[wamcl] dumpst:dumpST.   %  dumpst.pl:128: 
(77)$[wamcl] first:zotrace((prolog_current_frame(990),b_setval('$dump_frame',990),dumpST1)).   %  first.pl:432: 
(78)$[wamcl] system:(prolog_current_frame(990),b_setval('$dump_frame',990),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1486018,plevel:initialise_error(_1486018)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1487442,plevel:true).   %  init.pl:371: 
(7)[cl] cl:lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1488710)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1488710)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1488974)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1489250)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1489486))).   %  eightball.pl:66: 
(14)*[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1489734))).   %  eightball.pl:104: 
(15)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1489968)),_1489944,cl:gripe_problem(uncaught(_1489944),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1489968))),!,fail))).   %  init.pl:371: 
(16)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1490172)).   %  callp.pl:111: 
(17)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1490378)).   %  callp.pl:114: 
(18)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1490576))).   %  eightball.pl:153: 
(19)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1490754)).   %  eightball.pl:63: 
(20)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1490920)).   %  eightball.pl:66: 
(21)*[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1491084)).   %  eightball.pl:104: 
(22)[system] system:catch(user:pl_load('sanity-test.lisp',[],_1491234),error(instantiation_error,context(system:file_name_extension/3,_1491246)),user:gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491246))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491234)),!,fail))).   %  init.pl:371: 
(23)[user] '8ball':gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491368))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491404)),!,fail)).   %  eightball.pl:109: 
(24)[user] '8ball':always_catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491494))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491530)),!,fail))).   %  eightball.pl:93: 
(25)[system] system:catch(user:catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491622))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491658)),!,fail)),'$aborted',notrace),_1491584,user:(lmsg(always_uncaught(_1491584)),notrace,!,fail)).   %  init.pl:371: 
(26)$[system] system:catch(user:gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491712))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491748)),!,fail)),'$aborted',user:notrace).   %  init.pl:371: 
(27)[user] '8ball':gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491784))),(rtrace(user:pl_load('sanity-test.lisp',[],_1491820)),!,fail)).   %  eightball.pl:110: 
(28)$[user] system:notrace((lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491858))):-rtrace(user:pl_load('sanity-test.lisp',[],_1491894)),!,fail)),dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491858))):-rtrace(user:pl_load('sanity-test.lisp',[],_1491894)),!,fail)))).  no(clause) 
(29)$[system] system:'$c_call_prolog'.  no(clause) 
(30)[system] system:(user:lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491986))):-rtrace(user:pl_load('sanity-test.lisp',[],_1492022)),!,fail)),user:(dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1491986))):-rtrace(user:pl_load('sanity-test.lisp',[],_1492022)),!,fail)))).   %  init.pl:211: 
(31)$[system] system:(user:lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1492014))):-rtrace(user:pl_load('sanity-test.lisp',[],_1492050)),!,fail)),user:(dumpST,lmsg((uncaught(error(instantiation_error,context(system:file_name_extension/3,_1492014))):-rtrace(user:pl_load('sanity-test.lisp',[],_1492050)),!,fail)))).  no(clause) 
(33)$[user] first:zotrace((prolog_current_frame(789),b_setval('$dump_frame',789),dumpST1)).   %  first.pl:432: 
(39)[_check] '_check':loop_check_term_frame(user:dumpST9,user:dumpST9,1,779,user:dumpST0).   %  loop_check.pl:203: 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1500676,plevel:initialise_error(_1500676)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1502052,plevel:true).   %  init.pl:371: 
(7)[cl] cl:lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1503272)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1503272)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1503524)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1503788)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1504012))).   %  eightball.pl:66: 
(14)*[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1504248))).   %  eightball.pl:104: 
(15)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1504470)),_1504446,cl:gripe_problem(uncaught(_1504446),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1504470))),!,fail))).   %  init.pl:371: 
(16)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1504662)).   %  callp.pl:111: 
(17)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1504856)).   %  callp.pl:114: 
(18)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1505042))).   %  eightball.pl:153: 
(19)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1505208)).   %  eightball.pl:63: 
(20)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1505362)).   %  eightball.pl:66: 
(21)*[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1505514)).   %  eightball.pl:104: 
(22)[system] system:catch(user:pl_load('sanity-test.lisp',[],_1505652),error(instantiation_error,context(system:file_name_extension/3,_1505664)),user:gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1505664))),(rtrace(user:pl_load('sanity-test.lisp',[],_1505652)),!,fail))).   %  init.pl:371: 
(23)[user] '8ball':gripe_problem(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1505774))),(rtrace(user:pl_load('sanity-test.lisp',[],_1505810)),!,fail)).   %  eightball.pl:109: 
(24)[user] '8ball':always_catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1505888))),(rtrace(user:pl_load('sanity-test.lisp',[],_1505924)),!,fail))).   %  eightball.pl:93: 
(25)[system] system:catch(user:catch(gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1506004))),(rtrace(user:pl_load('sanity-test.lisp',[],_1506040)),!,fail)),'$aborted',notrace),_1505966,user:(lmsg(always_uncaught(_1505966)),notrace,!,fail)).   %  init.pl:371: 
(26)$[system] system:catch(user:gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1506082))),(rtrace(user:pl_load('sanity-test.lisp',[],_1506118)),!,fail)),'$aborted',user:notrace).   %  init.pl:371: 
(27)[user] '8ball':gripe_problem0(uncaught(error(instantiation_error,context(system:file_name_extension/3,_1506142))),(rtrace(user:pl_load('sanity-test.lisp',[],_1506178)),!,fail)).   %  eightball.pl:110: 
(28)*[user] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(29)[user] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(30)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(31)$[user] first:zotrace((prolog_current_frame(729),b_setval('$dump_frame',729),dumpST1)).   %  first.pl:432: 
(32)$[user] system:(prolog_current_frame(729),b_setval('$dump_frame',729),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1515080,plevel:initialise_error(_1515080)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1516456,plevel:true).   %  init.pl:371: 
(7)[cl] cl:lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1517676)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1517676)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1517928)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1518192)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1518416))).   %  eightball.pl:66: 
(14)*[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1518652))).   %  eightball.pl:104: 
(15)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1518874)),_1518850,cl:gripe_problem(uncaught(_1518850),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1518874))),!,fail))).   %  init.pl:371: 
(16)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1519066)).   %  callp.pl:111: 
(17)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1519260)).   %  callp.pl:114: 
(18)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1519446))).   %  eightball.pl:153: 
(19)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1519612)).   %  eightball.pl:63: 
(20)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1519766)).   %  eightball.pl:66: 
(21)[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1519918)).   %  eightball.pl:104: 
(22)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520064)))).   %  eightball.pl:109: 
(23)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520190))))).   %  eightball.pl:93: 
(24)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520318)))),'$aborted',notrace),_1520272,user:(lmsg(always_uncaught(_1520272)),notrace,!,fail)).   %  init.pl:371: 
(25)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520408)))),'$aborted',user:notrace).   %  init.pl:371: 
(26)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520480)))).   %  eightball.pl:110: 
(27)$[user] system:notrace((lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520554))))),dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520554))))))).  no(clause) 
(28)$[system] system:'$c_call_prolog'.  no(clause) 
(29)[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520682))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520682))))))).   %  init.pl:211: 
(30)$[system] system:(user:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520710))))),user:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1520710))))))).  no(clause) 
(32)$[user] first:zotrace((prolog_current_frame(769),b_setval('$dump_frame',769),dumpST1)).   %  first.pl:432: 
(38)[_check] '_check':loop_check_term_frame(user:dumpST9,user:dumpST9,1,759,user:dumpST0).   %  loop_check.pl:203: 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1528918,plevel:initialise_error(_1528918)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1530246,plevel:true).   %  init.pl:371: 
(7)[cl] cl:lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1531418)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1531418)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1531658)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1531910)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1532122))).   %  eightball.pl:66: 
(14)*[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1532346))).   %  eightball.pl:104: 
(15)[system] system:catch(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1532556)),_1532532,cl:gripe_problem(uncaught(_1532532),(rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1532556))),!,fail))).   %  init.pl:371: 
(16)[user] callp:do_interned_eval(pl_load('sanity-test.lisp',[],_1532736)).   %  callp.pl:111: 
(17)[user] callp:call_interned_eval(pl_load('sanity-test.lisp',[],_1532918)).   %  callp.pl:114: 
(18)[user] '8ball':show_call_trace(always(pl_load('sanity-test.lisp',[],_1533092))).   %  eightball.pl:153: 
(19)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1533246)).   %  eightball.pl:63: 
(20)[user] '8ball':always(pl_load('sanity-test.lisp',[],_1533388)).   %  eightball.pl:66: 
(21)[user] '8ball':nonquietly_must_or_rtrace(user:pl_load('sanity-test.lisp',[],_1533528)).   %  eightball.pl:104: 
(22)[user] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1533662)))).   %  eightball.pl:109: 
(23)[user] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1533776))))).   %  eightball.pl:93: 
(24)[system] system:catch(user:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1533892)))),'$aborted',notrace),_1533846,user:(lmsg(always_uncaught(_1533846)),notrace,!,fail)).   %  init.pl:371: 
(25)$[system] system:catch(user:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1533970)))),'$aborted',user:notrace).   %  init.pl:371: 
(26)[user] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:pl_load('sanity-test.lisp',[],_1534030)))).   %  eightball.pl:110: 
(27)*[user] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(28)[user] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(29)[user] dumpst:dumpST.   %  dumpst.pl:128: 
(30)$[user] first:zotrace((prolog_current_frame(709),b_setval('$dump_frame',709),dumpST1)).   %  first.pl:432: 
(31)$[user] system:(prolog_current_frame(709),b_setval('$dump_frame',709),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1540090,plevel:initialise_error(_1540090)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1541130,plevel:true).   %  init.pl:371: 
(7)[cl] lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1542014)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1542014)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1542182)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1542362)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1542502))).   %  eightball.pl:66: 
(14)[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1542654))).   %  eightball.pl:104: 
(15)[cl] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1542800))))).   %  eightball.pl:109: 
(16)[cl] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1542926)))))).   %  eightball.pl:93: 
(17)[system] system:catch(cl:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543054))))),'$aborted',notrace),_1543000,cl:(lmsg(always_uncaught(_1543000)),notrace,!,fail)).   %  init.pl:371: 
(18)$[system] system:catch(cl:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543144))))),'$aborted',cl:notrace).   %  init.pl:371: 
(19)[cl] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543216))))).   %  eightball.pl:110: 
(20)$[cl] system:notrace((lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543290)))))),dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543290)))))))).  no(clause) 
(21)$[system] system:'$c_call_prolog'.  no(clause) 
(22)[system] system:(cl:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543418)))))),cl:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543418)))))))).   %  init.pl:211: 
(23)$[system] system:(cl:lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543446)))))),cl:(dumpST,lmsg((fail_must_or_rtrace_failed:-rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1543446)))))))).  no(clause) 
(25)$[cl] first:zotrace((prolog_current_frame(491),b_setval('$dump_frame',491),dumpST1)).   %  first.pl:432: 
(26)$[cl] system:(prolog_current_frame(491),b_setval('$dump_frame',491),dumpST1).  no(clause) 

(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_1548860,plevel:initialise_error(_1548860)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':247)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_1549852,plevel:true).   %  init.pl:371: 
(7)[cl] lisp.   %  repl.pl:22: 
(8)[cl] in1t:do_wamcl_inits.   %  init.pl:156: 
(9)[cl] in1t:clear_op_buffer.   %  init.pl:164: 
(10)*[$apply] '$apply':forall(cl:clause(wl:interned_eval(pl_load('sanity-test.lisp',[],_1550688)),true,<clause>(0x4beadd0)),cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1550688)))),erase(<clause>(0x4beadd0)))).   %  apply.pl:51: 
(11)$[cl] system:cl:(forall(true,always(do_interned_eval(pl_load('sanity-test.lisp',[],_1550844)))),erase(<clause>(0x4beadd0))).  no(clause) 
(12)*[$apply] '$apply':forall(cl:true,cl:always(do_interned_eval(pl_load('sanity-test.lisp',[],_1551012)))).   %  apply.pl:51: 
(13)[cl] '8ball':always(do_interned_eval(pl_load('sanity-test.lisp',[],_1551140))).   %  eightball.pl:66: 
(14)[cl] '8ball':nonquietly_must_or_rtrace(user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551280))).   %  eightball.pl:104: 
(15)[cl] '8ball':gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551414))))).   %  eightball.pl:109: 
(16)[cl] '8ball':always_catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551528)))))).   %  eightball.pl:93: 
(17)[system] system:catch(cl:catch(gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551644))))),'$aborted',notrace),_1551590,cl:(lmsg(always_uncaught(_1551590)),notrace,!,fail)).   %  init.pl:371: 
(18)$[system] system:catch(cl:gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551722))))),'$aborted',cl:notrace).   %  init.pl:371: 
(19)[cl] '8ball':gripe_problem0(fail_must_or_rtrace_failed,rtrace((slow_trace,user:do_interned_eval(pl_load('sanity-test.lisp',[],_1551782))))).   %  eightball.pl:110: 
(20)*[cl] '8ball':lisp_dump_break.   %  eightball.pl:44: 
(21)[cl] '8ball':both_outputs(dumpST).   %  eightball.pl:126: 
(22)[cl] dumpst:dumpST.   %  dumpst.pl:128: 
(23)$[cl] first:zotrace((prolog_current_frame(431),b_setval('$dump_frame',431),dumpST1)).   %  first.pl:432: 
(24)$[cl] system:(prolog_current_frame(431),b_setval('$dump_frame',431),dumpST1).  no(clause) 
