
% Daydreammer uses Roger Schanks conceptual dependancy and KR in a special way differnt than Schanks crowd had tried 

:- module(logicmoo_dd,[]).
/** <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
   clif syntax to be recocogized via our CycL/KIF handlers 
 
 Logicmoo Project: A LarKC Server written in Prolog
 Maintainer: Douglas Miles
 Dec 13, 2035

 ?- ensure_loaded(library(logicmoo_clif)).

:- set_prolog_flag(verbose_autoload,true).
*/

:- set_prolog_flag(os_argv,[swipl, '-f', '/dev/null','--nonet','--unsafe','--']).

%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,break).

:- user:use_module(library(logicmoo_util_common)).
:- use_module(library(pfc)).
% :- use_module(library(logicmoo_user)).
:- statistics.
:- use_module(library(sexpr_reader)).




:- set_fileAssertMt(baseKB).

:- set_defaultAssertMt(baseKB).

:- set_prolog_flag_until_eof(retry_undefine,false).


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = library,
   during_boot((( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true)),!.

:- '$set_source_module'(baseKB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP DAYDREAMER KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- set_prolog_flag(do_renames,term_expansion).

%:- during_boot(set_prolog_flag(do_renames,restore)).

dd_ain2(documentation(_, xtChineseLanguage,_)).
dd_ain2(CycLOut):-
    delay_rule_eval(CycLOut,dd_rule,NewAsserts),
    dmsg(NewAsserts),
    ain(NewAsserts).

loadDD1(File):- \+ exists_file(File),!,wdmsg(no_such_file(File)),!.
loadDD1(File):- arg(_,v('gate_instan777.cl','gate_instan66.cl','dd_reversal777.cl'),File),!,dmsg(loadDD2(File)),!.
loadDD1(File):-loadDD2(File).

loadDD2(File):- dmsg(loadDD1(File)),with_lisp_translation_cached(File,=,nop).

skip_dd:- app_argv('--nodd'),!.
skip_dd:- app_argv(List), \+ member('--ldd',List), \+ member('--snark',List), \+ member('--all',List),!.


%ldd:- skip_dd,!.
ldd:- expand_file_name('*.cl',Files),maplist(loadDD1,Files).

%loadDD2:- skip_dd,!.
loadDD2:- expand_file_name('*.tmp',Files),maplist(ensure_loaded,Files).
   !.

loadDD3:- skip_dd,!.
loadDD3:- 
   % ensure_loaded(baseKB:library('logicmoo/common_logic/common_logic_dd.pfc')),
   !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE DAYDREAMER KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- after_boot(ldd).

%:- after_boot(loadDD2).

%:- after_boot(loadDD3).


:- fixup_exports.




end_of_file.


:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.
user:file_search_path(jar, ('.')).


%:- shell('touch jpl.jar').
%:- ignore(shell('killall -9 xterm')).

po(O):- format(user_error,'~N % LO: ~w ;; ~w~n',[O.toString,O.getClass.getSimpleName]),!.
po(O):- format(user_error,'~N % PO: ~q ~n',[O]),!.

dwq(Q):- format(user_error,'~N % DWQ: ~q~n',[Q]).

:- meta_predicate dwq_call(0).
dwq_call(Q):- Q *-> dwq(success:Q); (dwq(failed:Q),!,fail).

/*  -*- Prolog -*-

    SWI-Prolog personalization file
*/

mp:- sys, meta_predicate(system: write(7)), meta_predicate(system: writeq(7)), meta_predicate(system:is(7,7)),meta_predicate(system: =:=(7,7)),usys.

:- load_files(library(prolog_stack)).
prolog_stack:stack_guard(none).
:- use_module(library(base32)).

:- set_prolog_flag(access_level,system).
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- set_prolog_flag(generate_debug_info, true).
% :- set_prolog_flag(logicmoo_include,fail).


:- ensure_loaded(udt).

cl_eval(L):- cl_eval(L, O),po(O).
cl_read_lisp(L):- cl_read_lisp(L, O),po(O).
cl_eval_string(L):- cl_eval_string(L, O),po(O).
% cl_eval(L,O):-cl_lisp_eval(L,O).

call_ctrl(Head):- call_ctrl(Head,_).
call_ctrl(Head,O):-oo_deref(Head,HeadE),dwq_call(call_crtl(call_ctrl,[{HeadE}],O)).
call_crtl(Name,Args,O):-jpl_call('org.logicmoo.system.BeanShellCntrl',Name,Args,O).


/*
:- use_module(library(prolog_history)).
% :- '$toplevel':'$clean_history'.
% :- '$toplevel':setup_history.

 ps -ef | grep  pts\/1 | grep -v grep | awk '{print "sudo kill -9", $2}' | h
*/

:- debug.
:- nodebug(_).
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
% user:file_search_path(pack,'/devel/LogicmooDeveloperFramework/PrologMUD/pack' ).

:- if(exists_directory('D:/workspace')).
:- asserta(user:file_search_path(pack,'D:/workspace')).
:- asserta(user:file_search_path(library,'D:/workspace/logicmoo_base/prolog')).
:- asserta(user:file_search_path(runtime,'D:/workspace/runtime')).
:- else.
:- asserta(user:file_search_path(pack,'/mnt/dddd/workspace')).
:- asserta(user:file_search_path(library,'/mnt/dddd/workspace/logicmoo_base/prolog')).
:- asserta(user:file_search_path(runtime,'/mnt/dddd/workspace/runtime')).
:- endif.

:- attach_packs.


:-if(\+ exists_source(library(gui_tracer))).
:-asserta(guitracer).
:-asserta(noguitracer).
:-endif.

sys :- set_prolog_flag(access_level,system).
usys :- set_prolog_flag(access_level,user).


lmv:-lm,call(call,use_listing_vars).
lm :- use_module(system:library(logicmoo_utils)).
lmb :- use_module(library(pfc)).
lmu :- ensure_loaded(library(logicmoo_user)).

lmp:-
   attach_packs,pack_list_installed.

lmcd:- user:file_search_path(runtime,DIR),cd(DIR).

ss:-lmcd,ensure_loaded(init_mud_server).

lm_repl :-lmcd,system:ensure_loaded(logicmoo_repl),qsave_program(lm_repl).

lms:-lmp,lmcd,call(call,swicli).

:- lmcd.
:- lmp.


%:- user:use_module(library(lists)).
%:- user:use_module(library(make)).





        /*******************************
         *     PLDOC SETUP
        *******************************/

% :- set_prolog_flag(access_level,system).
% :- set_prolog_flag(gc,false).

%:- user:use_module(library(lists)).

mw:- use_module(library(prolog_modewalk)).

:- dynamic(user:type_modes/3).


nopt :- (initialization((set_prolog_flag(optimise,false),
         set_prolog_flag(last_call_optimisation,false))),
  use_module(library(pce)),
  use_module(library(prolog_xref)),
  use_module(library(pldoc/doc_access)),
  assert((doc_access:can_edit(X):-nonvar(X))),
  assert((doc_access:allow_from(X):-nonvar(X)))).


sf:- forall(source_file(SF), call(call,load_all(SF))).

ds:-
   doc_server(3020,[allow(ip(_,_,_,_)),can_edit(true),workers(8)]),
   use_module(library(pldoc/doc_library)),
   call(call,doc_load_library),
   use_module(library(pldoc/doc_access)),
   assert((doc_access:can_edit(X):-nonvar(X))),
   assert((doc_access:allow_from(X):-nonvar(X))).


% current_module(M),catch(M:'$pldoc'(F//A, Det,O,I),_,fail),format('~q.~n',[M:'$pldoc'(F//A, Det,I)]),fail.
% current_module(M),catch(M:'$mode'(Head, Det),_,fail),format('~q.~n',[M:'$mode'(Head, Det)]),fail.


gm:-
   current_module(M),catch(M:'$pldoc'(F/A, Loc,Text,_),_,fail),
   functor(Head,F,A),
   \+ catch(M:'$mode'(Head,_Det1),_,fail),
   format('~q.~n',[M:'$pldoc'(F/A, Loc,Text)]),
   format('~q.~n',[M:'$mode'(Head, _Det2)]).

sm:-
   current_module(M),
   catch(M:'$mode'(Head, Det),_,fail),
   \+ user:type_modes(M,Head,Det),
   asserta(user:type_modes(M,Head,Det)),fail.

sm:-
   current_module(M),catch(M:'$pldoc'(F/A, _,_,Sum),_,fail), functor(Head,F,A),
   \+ user:type_modes(M,Head,_Det),\+ user:type_modes(M,_:Head,_Det2),
   format('~q.~n',[parse_modes(M:Head, Sum)]),fail.


sm:-
   current_module(M),catch(M:'$pldoc'(F//Am2, _,_,Sum),_,fail), A is Am2+2, functor(Head,F,A),
   \+ user:type_modes(M,Head,_Det),\+ user:type_modes(M,_:Head,_Det2),
   format('~q.~n',[parse_modes(M:Head, Sum)]),fail.

% :- ds.

      /*******************************
       *     XREF SETUP
      *******************************/


:- multifile(check:predicate_name/2).
:- multifile prolog_xref:xref_source_file/3.
:- use_module(library(check)).
check:predicate_name(A, D):- prolog_clause:predicate_name(A, D).
%:- gxref.
%:- check.
:- set_prolog_flag(access_level,user).



:- if(current_predicate(prolog_ide/1)).
:- prolog_ide(debug_monitor).
:- endif.

%:- interactor.


%

:- set_prolog_flag(access_level,user).




% :- set_prolog_flag(scce,pure).
% :- set_prolog_flag(scce,scce_orig).
:- set_prolog_flag(scce,setup_call_cleanup).


        /*******************************
         ****     JPL SETUP        *****
        *******************************/

clean_bad_chars(PATH,Path10):-name(PATH,Chars),delete(Chars,0,Chars0),delete(Chars0,1,Chars1),delete(Chars1,10,Chars10),name(Path10,Chars10).

clean_path:-
  getenv('PATH',PATH),clean_bad_chars(PATH,Path10),setenv('PATH',Path10).

:- clean_path.

:- if( \+ getenv('CLASSPATH',_)).
:- if(current_prolog_flag(shared_object_extension,'so')).
:-  setenv('CLASSPATH', '/usr/share/ant/lib/ant.jar:/mnt/dddd/workspace/phase02-jrtl/AppdapterGUI/bin-eclipse:/mnt/dddd/workspace/phase02-jrtl/CycJava/bin-eclipse:/mnt/dddd/workspace/phase02-jrtl/platform/bin-eclipse:/mnt/dddd/workspace/phase02-jrtl/platform-deps/bin-eclipse:/mnt/dddd/workspace/phase02-jrtl/platform/dist/abcl-contrib.jar:/cp/ZorillaAdaptor.jar:/cp/zoni.jar:/cp/xstream-1.1.1-patched.jar:/cp/xpp3-1.1.3.4d_b4_min.jar:/cp/xmlsec.jar:/cp/xmlParserAPIs.jar:/cp/xml-apis.jar:/cp/xml-apis-1.4.01.jar:/cp/xml4j.jar:/cp/xercesImpl.jar:/cp/xbean-2.1.0.jar:/cp/xalan.jar:/cp/wstx-asl-3.2.9.jar:/cp/wss4j.jar:/cp/wss4j-1.5.0-itinnov-2.jar:/cp/wsrf_tools.jar:/cp/wsrf_provider_jce.jar:/cp/wsrf_mds_usefulrp_schema_stubs.jar:/cp/wsrf_mds_index_stubs.jar:/cp/wsrf_core_stubs.jar:/cp/wsrf_core.jar:/cp/wsdl4j.jar:/cp/wms3.1.jar:/cp/unicode_panel.jar:/cp/ublsupport.jar:/cp/trilead-ssh2-build213-RK.jar:/cp/tcljava-1.2.6.jar:/cp/syntax.jar:/cp/swingx-1.6.jar:/cp/stax-api-1.0.1.jar:/cp/SshTrileadAdaptor.jar:/cp/srm.jar:/cp/smartsockets-1.51.jar:/cp/slf4j-log4j12-1.7.5.jar:/cp/slf4j-api-1.7.5.jar:/cp/SgeAdaptor.jar:/cp/SftpTrileadAdaptor.jar:/cp/SftpGanymedAdaptor.jar:/cp/SftpAdaptor.jar:/cp/servlet.jar:/cp/servlet-api-3.0.20100224.jar:/cp/services_IndexServiceProxyService_stubs.jar:/cp/services_AdvertServiceEntry_stubs.jar:/cp/scala-reflect-2.10.2.jar:/cp/scala-library-2.10.2.jar:/cp/scala-compiler-2.10.2.jar:/cp/saaj.jar:/cp/runnersFramework-2.0.jar:/cp/rhino-1.7.7.1.jar:/cp/reflections-0.9.8.jar:/cp/puretls.jar:/cp/procyon-core-0.5.33-enigma.jar:/cp/procyon-compilertools-0.5.33-enigma.jar:/cp/postgresql-9.1-901.jdbc4.jar:/cp/pddl4j.jar:/cp/owlsyntax.jar:/cp/owlim-big-3.1.a7.jar:/cp/owlapi-osgidistribution-4.1.4.jar:/cp/owlapi-distribution-4.1.4.jar:/cp/orphanNodesAlg.jar:/cp/org.osgi.core-4.2.0.jar:/cp/org.osgi.core-1.4.0.jar:/cp/org.osgi.compendium-4.2.0.jar:/cp/org.openl.lib.poi.dev-5.9.4.1.jar:/cp/org.appdapter.lib.remote-1.2.4-20160803.212630-10.jar:/cp/org.appdapter.lib.core-1.1.6-SNAPSHOT.jar:/cp/org.appdapter.lib.bind.jena-1.2.3.jar:/cp/org.appdapter.bundle.xload-1.2.4-20160803.212610-8.jar:/cp/org.appdapter.bundle.fileconv-1.1.6-SNAPSHOT.jar:/cp/org.appdapter.bundle.debug-1.1.6-SNAPSHOT.jar:/cp/org.appdapter.bundle.core-1.1.6-SNAPSHOT.jar:/cp/org.apache.servicemix.specs.activation-api-1.1-1.8.0.jar:/cp/org.apache.servicemix.bundles.xmlresolver-1.2_3.jar:/cp/org.apache.servicemix.bundles.xerces-2.11.0_1.jar:/cp/org.apache.servicemix.bundles.lucene-3.0.3_2.jar:/cp/org.apache.servicemix.bundles.junit-4.7_3.jar:/cp/org.apache.servicemix.bundles.javax.mail-1.4.1_4.jar:/cp/ordi-trree-adapter-3.1.a6.jar:/cp/ordi-model-0.5.jar:/cp/opensaml.jar:/cp/openrdf-sesame-2.2.4-onejar.jar:/cp/openrdf-rio-ntriples-2.0.1.jar:/cp/openrdf-rio-api-2.0.1.jar:/cp/openrdf-model-2.0.1.jar:/cp/omii-security-utils-1.1.jar:/cp/naming-resources.jar:/cp/naming-java.jar:/cp/naming-factory.jar:/cp/naming-common.jar:/cp/mysql-connector-java-5.1.6-bin.jar:/cp/monetdb-1.7-jdbc.jar:/cp/miglayout-swing-4.2.jar:/cp/miglayout-core-4.2.jar:/cp/MercuryAdaptor.jar:/cp/mail.jar:/cp/looks.jar:/cp/log4jdbc3-1.1alpha2.jar:/cp/log4j-1.2.12.jar:/cp/LocalAdaptor.jar:/cp/lb.jar:/cp/kshared-2.0.jar:/cp/KoalaAdaptor.jar:/cp/kazuki.jar:/cp/kaon2.plain.jar:/cp/kaon2.jar:/cp/junit-4.10.jar:/cp/jsr305-1.3.9.jar:/cp/jsp-api-2.1-glassfish-2.1.v20100127.jar:/cp/jsp-2.1-glassfish-2.1.v20100127.jar:/cp/json.jar:/cp/jide-oss-3.5.5.jar:/cp/jgss.jar:/cp/jgraph.jar:/cp/JGoWeb.jar:/cp/JGoSVG.jar:/cp/JGoLayout.jar:/cp/JGo.jar:/cp/JGoInstruments.jar:/cp/jfreechart-1.0.13.jar:/cp/jfreechart-1.0.0.jar:/cp/JFlex.jar:/cp/jetty-xml-8.0.4.v20111024.jar:/cp/jetty-webapp-8.0.4.v20111024.jar:/cp/jetty-util-8.0.4.v20111024.jar:/cp/jetty-servlet-8.0.4.v20111024.jar:/cp/jetty-server-8.0.4.v20111024.jar:/cp/jetty-security-8.0.4.v20111024.jar:/cp/jetty-io-8.0.4.v20111024.jar:/cp/jetty-http-8.0.4.v20111024.jar:/cp/jetty-continuation-8.0.4.v20111024.jar:/cp/jep-2.4.1.jar:/cp/jena-dig.jar:/cp/jdom.jar:/cp/jcommon-1.0.16.jar:/cp/jce-jdk13-131.jar:/cp/jcalendar.jar:/cp/jaxrpc.jar:/cp/jaxen-1.1.1.jar:/cp/java_websocket.jar:/cp/javassist-3.12.1.GA.jar:/cp/java-json.jar:/cp/javaff_lrta_bounded_children_heap-0.0.1-SNAPSHOT.jar:/cp/jakarta-slide-webdavlib-2.0.jar:/cp/jakarta-regexp-1.2.jar:/cp/jacl-1.2.6.jar:/cp/jaas.jar:/cp/j2ssh-core-0.2.9.jar:/cp/j2ssh-common-0.2.2.jar:/cp/itinnov-grid-utils-omii1.jar:/cp/itinnov-grid-types-omii1.jar:/cp/itinnov-gridservit-0.3.0.jar:/cp/itinnov-grid-service-utils-omii1.jar:/cp/itinnov-grid-service-types-omii1.jar:/cp/itinnov-grid-comms-omii1.1.jar:/cp/itinnov-grid-client-swing-omii1.jar:/cp/itinnov-grid-client-staterepos-omii1.1.jar:/cp/itinnov-grid-client-helpers-omii1.jar:/cp/itinnov-grid-client-echo-omii1.jar:/cp/itinnov-grid-client-cli-omii1.1.jar:/cp/iri.jar:/cp/icu4j_3.4.4.jar:/cp/httpcore-osgi-4.3.jar:/cp/httpclient-osgi-4.3.1.jar:/cp/hivemind-lib-1.1.1.jar:/cp/hivemind-jmx-1.1.1.jar:/cp/hivemind-1.1.1.jar:/cp/hamcrest-core-1.1.jar:/cp/h2-1.3.157.jar:/cp/guava-12.0-sources.jar:/cp/guava-12.0.jar:/cp/gson-2.3.1.jar:/cp/gridsam-schema-2.0.1.jar:/cp/gridsam-core-2.0.1.jar:/cp/gridsam-client.jar:/cp/GridsamAdaptor.jar:/cp/graphlayout-1.2.1.jar:/cp/gram-utils.jar:/cp/gram-stubs.jar:/cp/gram-client.jar:/cp/globus_wsrf_rft_stubs.jar:/cp/globus_wsrf_rft.jar:/cp/globus_wsrf_rft_client.jar:/cp/globus_wsrf_rendezvous_stubs.jar:/cp/globus_wsrf_rendezvous_service.jar:/cp/globus_wsrf_mds_aggregator_stubs.jar:/cp/globus_delegation_stubs.jar:/cp/globus_delegation_service.jar:/cp/GlobusAdaptor.jar:/cp/glite-security-util-java.jar:/cp/glite-security-trustmanager.jar:/cp/glite-security-delegation-java.jar:/cp/glite-jdl-api-java.jar:/cp/GliteAdaptor.jar:/cp/GenericAdaptor.jar:/cp/GAT-engine.jar:/cp/GAT-API.jar:/cp/ganymed-ssh2-build211beta4.jar:/cp/forms-1.3.0.jar:/cp/ext.bundle.xml.xerces-1.1.6-SNAPSHOT.jar:/cp/ext.bundle.xml.dom4j_161-1.1.6-SNAPSHOT.jar:/cp/ext.bundle.osgi.common-1.1.6-SNAPSHOT.jar:/cp/ext.bundle.openconverters-1.1.6-SNAPSHOT.jar:/cp/ext.bundle.math.jscience_50SNAP-1.0.9.jar:/cp/ext.bundle.jena_all_2_10_1-1.1.6-SNAPSHOT.jar:/cp/ext.bundle.apache_httpclient-1.1.6-SNAPSHOT.jar:/cp/ekitspell.jar:/cp/edtftpj-1.5.2.jar:/cp/ecj-3.5.1.jar:/cp/drmaa.jar:/cp/dom4j-1.6.1.jar:/cp/docking-frames-core.jar:/cp/docking-frames-common.jar:/cp/cycSparqlEndpoint.jar:/cp/customSysParam.jar:/cp/cryptix.jar:/cp/cryptix-asn1.jar:/cp/cryptix32.jar:/cp/concurrent.jar:/cp/com.springsource.org.apache.log4j-1.2.16.jar:/cp/com.springsource.com.ibm.icu-3.4.5.jar:/cp/commons-vfs2-2.1.jar:/cp/commons-vfs-1.0.jar:/cp/commons-math-3.0-SNAPSHOT.jar:/cp/commons-logging-1.1.3.jar:/cp/commons-lang3-3.1.jar:/cp/commons-lang-2.4.jar:/cp/commons-httpclient-3.1.jar:/cp/commons-discovery-0.4.jar:/cp/commons-digester.jar:/cp/commons-collections-3.0.jar:/cp/commons-codec-1.6.jar:/cp/commons-cli-1.0.jar:/cp/commons-beanutils.jar:/cp/commonj.jar:/cp/CommandlineSshAdaptor.jar:/cp/cog-util-0.91.jar:/cp/cog-url.jar:/cp/cog-trap-1.0.jar:/cp/cog-setup-0.91.jar:/cp/cog-resources-1.0.jar:/cp/cog-provider-webdav-1.0.jar:/cp/cog-provider-ssh-2.1.jar:/cp/cog-provider-local-2.0.jar:/cp/cog-provider-gt4_0_0-2.3.jar:/cp/cog-provider-gt3_2_1-2.0.jar:/cp/cog-provider-gt2ft-1.0.jar:/cp/cog-provider-gt2-2.2.jar:/cp/cog-provider-condor-2.0.jar:/cp/cog-provider-clref-gt4_0_0.jar:/cp/cog-provider-clref-gt3_2_1.jar:/cp/cog-karajan-0.33.jar:/cp/cog-jobmanager.jar:/cp/cog-jglobus.jar:/cp/cog-gridshell-1.0.jar:/cp/cog-gridfaces-1.0.jar:/cp/cog-grapheditor-0.47.jar:/cp/cog-certrequest-1.0.jar:/cp/cog-certmanagement-1.0.jar:/cp/cog-axis.jar:/cp/cog-abstraction-examples-2.1.jar:/cp/cog-abstraction-common-2.1.jar:/cp/castor-1.1.1-xml.jar:/cp/castor-0.9.6.jar:/cp/bsf-utils-3.1.jar:/cp/bsf.jar:/cp/bsf-api-3.1.jar:/cp/bsf-all-3.1.jar:/cp/BrowserLauncher2-1_3.jar:/cp/bcprov-jdk15-133.jar:/cp/backport-util-concurrent.jar:/cp/axis-jaxrpc-1.4.jar:/cp/axis-1.4.jar:/cp/arq-extra.jar:/cp/antlr-2.7.5.jar:/cp/ant-1.6.5.jar:/cp/addressing-1.0.jar:/cp/activation.jar:/cp/2p.jar:/usr/lib/jvm/java-8-oracle/lib/tools.jar').
:- else.
:-  setenv('CLASSPATH', 'd:\\workspace\\phase02-jrtl\\AppdapterGUI\\bin-eclipse;d:\\workspace\\phase02-jrtl\\CycJava\\bin-eclipse;d:\\workspace\\phase02-jrtl\\platform\\bin-eclipse;d:\\workspace\\phase02-jrtl\\platform-deps\\bin-eclipse;d:\\workspace\\phase02-jrtl\\platform\\dist\\abcl-contrib.jar;g:\\cp\\ZorillaAdaptor.jar;g:\\cp\\zoni.jar;g:\\cp\\xstream-1.1.1-patched.jar;g:\\cp\\xpp3-1.1.3.4d_b4_min.jar;g:\\cp\\xmlsec.jar;g:\\cp\\xmlParserAPIs.jar;g:\\cp\\xml-apis.jar;g:\\cp\\xml-apis-1.4.01.jar;g:\\cp\\xml4j.jar;g:\\cp\\xercesImpl.jar;g:\\cp\\xbean-2.1.0.jar;g:\\cp\\xalan.jar;g:\\cp\\wstx-asl-3.2.9.jar;g:\\cp\\wss4j.jar;g:\\cp\\wss4j-1.5.0-itinnov-2.jar;g:\\cp\\wsrf_tools.jar;g:\\cp\\wsrf_provider_jce.jar;g:\\cp\\wsrf_mds_usefulrp_schema_stubs.jar;g:\\cp\\wsrf_mds_index_stubs.jar;g:\\cp\\wsrf_core_stubs.jar;g:\\cp\\wsrf_core.jar;g:\\cp\\wsdl4j.jar;g:\\cp\\wms3.1.jar;g:\\cp\\unicode_panel.jar;g:\\cp\\ublsupport.jar;g:\\cp\\trilead-ssh2-build213-RK.jar;g:\\cp\\tcljava-1.2.6.jar;g:\\cp\\syntax.jar;g:\\cp\\swingx-1.6.jar;g:\\cp\\stax-api-1.0.1.jar;g:\\cp\\SshTrileadAdaptor.jar;g:\\cp\\srm.jar;g:\\cp\\smartsockets-1.51.jar;g:\\cp\\slf4j-log4j12-1.7.5.jar;g:\\cp\\slf4j-api-1.7.5.jar;g:\\cp\\SgeAdaptor.jar;g:\\cp\\SftpTrileadAdaptor.jar;g:\\cp\\SftpGanymedAdaptor.jar;g:\\cp\\SftpAdaptor.jar;g:\\cp\\servlet.jar;g:\\cp\\servlet-api-3.0.20100224.jar;g:\\cp\\services_IndexServiceProxyService_stubs.jar;g:\\cp\\services_AdvertServiceEntry_stubs.jar;g:\\cp\\scala-reflect-2.10.2.jar;g:\\cp\\scala-library-2.10.2.jar;g:\\cp\\scala-compiler-2.10.2.jar;g:\\cp\\saaj.jar;g:\\cp\\runnersFramework-2.0.jar;g:\\cp\\rhino-1.7.7.1.jar;g:\\cp\\reflections-0.9.8.jar;g:\\cp\\puretls.jar;g:\\cp\\procyon-core-0.5.33-enigma.jar;g:\\cp\\procyon-compilertools-0.5.33-enigma.jar;g:\\cp\\postgresql-9.1-901.jdbc4.jar;g:\\cp\\pddl4j.jar;g:\\cp\\owlsyntax.jar;g:\\cp\\owlim-big-3.1.a7.jar;g:\\cp\\owlapi-osgidistribution-4.1.4.jar;g:\\cp\\owlapi-distribution-4.1.4.jar;g:\\cp\\orphanNodesAlg.jar;g:\\cp\\org.osgi.core-4.2.0.jar;g:\\cp\\org.osgi.core-1.4.0.jar;g:\\cp\\org.osgi.compendium-4.2.0.jar;g:\\cp\\org.openl.lib.poi.dev-5.9.4.1.jar;g:\\cp\\org.appdapter.lib.remote-1.2.4-20160803.212630-10.jar;g:\\cp\\org.appdapter.lib.core-1.1.6-SNAPSHOT.jar;g:\\cp\\org.appdapter.lib.bind.jena-1.2.3.jar;g:\\cp\\org.appdapter.bundle.xload-1.2.4-20160803.212610-8.jar;g:\\cp\\org.appdapter.bundle.fileconv-1.1.6-SNAPSHOT.jar;g:\\cp\\org.appdapter.bundle.debug-1.1.6-SNAPSHOT.jar;g:\\cp\\org.appdapter.bundle.core-1.1.6-SNAPSHOT.jar;g:\\cp\\org.apache.servicemix.specs.activation-api-1.1-1.8.0.jar;g:\\cp\\org.apache.servicemix.bundles.xmlresolver-1.2_3.jar;g:\\cp\\org.apache.servicemix.bundles.xerces-2.11.0_1.jar;g:\\cp\\org.apache.servicemix.bundles.lucene-3.0.3_2.jar;g:\\cp\\org.apache.servicemix.bundles.junit-4.7_3.jar;g:\\cp\\org.apache.servicemix.bundles.javax.mail-1.4.1_4.jar;g:\\cp\\ordi-trree-adapter-3.1.a6.jar;g:\\cp\\ordi-model-0.5.jar;g:\\cp\\opensaml.jar;g:\\cp\\openrdf-sesame-2.2.4-onejar.jar;g:\\cp\\openrdf-rio-ntriples-2.0.1.jar;g:\\cp\\openrdf-rio-api-2.0.1.jar;g:\\cp\\openrdf-model-2.0.1.jar;g:\\cp\\omii-security-utils-1.1.jar;g:\\cp\\naming-resources.jar;g:\\cp\\naming-java.jar;g:\\cp\\naming-factory.jar;g:\\cp\\naming-common.jar;g:\\cp\\mysql-connector-java-5.1.6-bin.jar;g:\\cp\\monetdb-1.7-jdbc.jar;g:\\cp\\miglayout-swing-4.2.jar;g:\\cp\\miglayout-core-4.2.jar;g:\\cp\\MercuryAdaptor.jar;g:\\cp\\mail.jar;g:\\cp\\looks.jar;g:\\cp\\log4jdbc3-1.1alpha2.jar;g:\\cp\\log4j-1.2.12.jar;g:\\cp\\LocalAdaptor.jar;g:\\cp\\lb.jar;g:\\cp\\kshared-2.0.jar;g:\\cp\\KoalaAdaptor.jar;g:\\cp\\kazuki.jar;g:\\cp\\kaon2.plain.jar;g:\\cp\\kaon2.jar;g:\\cp\\junit-4.10.jar;g:\\cp\\jsr305-1.3.9.jar;g:\\cp\\jsp-api-2.1-glassfish-2.1.v20100127.jar;g:\\cp\\jsp-2.1-glassfish-2.1.v20100127.jar;g:\\cp\\json.jar;g:\\cp\\jide-oss-3.5.5.jar;g:\\cp\\jgss.jar;g:\\cp\\jgraph.jar;g:\\cp\\JGoWeb.jar;g:\\cp\\JGoSVG.jar;g:\\cp\\JGoLayout.jar;g:\\cp\\JGo.jar;g:\\cp\\JGoInstruments.jar;g:\\cp\\jfreechart-1.0.13.jar;g:\\cp\\jfreechart-1.0.0.jar;g:\\cp\\JFlex.jar;g:\\cp\\jetty-xml-8.0.4.v20111024.jar;g:\\cp\\jetty-webapp-8.0.4.v20111024.jar;g:\\cp\\jetty-util-8.0.4.v20111024.jar;g:\\cp\\jetty-servlet-8.0.4.v20111024.jar;g:\\cp\\jetty-server-8.0.4.v20111024.jar;g:\\cp\\jetty-security-8.0.4.v20111024.jar;g:\\cp\\jetty-io-8.0.4.v20111024.jar;g:\\cp\\jetty-http-8.0.4.v20111024.jar;g:\\cp\\jetty-continuation-8.0.4.v20111024.jar;g:\\cp\\jep-2.4.1.jar;g:\\cp\\jena-dig.jar;g:\\cp\\jdom.jar;g:\\cp\\jcommon-1.0.16.jar;g:\\cp\\jce-jdk13-131.jar;g:\\cp\\jcalendar.jar;g:\\cp\\jaxrpc.jar;g:\\cp\\jaxen-1.1.1.jar;g:\\cp\\java_websocket.jar;g:\\cp\\javassist-3.12.1.GA.jar;g:\\cp\\java-json.jar;g:\\cp\\javaff_lrta_bounded_children_heap-0.0.1-SNAPSHOT.jar;g:\\cp\\jakarta-slide-webdavlib-2.0.jar;g:\\cp\\jakarta-regexp-1.2.jar;g:\\cp\\jacl-1.2.6.jar;g:\\cp\\jaas.jar;g:\\cp\\j2ssh-core-0.2.9.jar;g:\\cp\\j2ssh-common-0.2.2.jar;g:\\cp\\itinnov-grid-utils-omii1.jar;g:\\cp\\itinnov-grid-types-omii1.jar;g:\\cp\\itinnov-gridservit-0.3.0.jar;g:\\cp\\itinnov-grid-service-utils-omii1.jar;g:\\cp\\itinnov-grid-service-types-omii1.jar;g:\\cp\\itinnov-grid-comms-omii1.1.jar;g:\\cp\\itinnov-grid-client-swing-omii1.jar;g:\\cp\\itinnov-grid-client-staterepos-omii1.1.jar;g:\\cp\\itinnov-grid-client-helpers-omii1.jar;g:\\cp\\itinnov-grid-client-echo-omii1.jar;g:\\cp\\itinnov-grid-client-cli-omii1.1.jar;g:\\cp\\iri.jar;g:\\cp\\icu4j_3.4.4.jar;g:\\cp\\httpcore-osgi-4.3.jar;g:\\cp\\httpclient-osgi-4.3.1.jar;g:\\cp\\hivemind-lib-1.1.1.jar;g:\\cp\\hivemind-jmx-1.1.1.jar;g:\\cp\\hivemind-1.1.1.jar;g:\\cp\\hamcrest-core-1.1.jar;g:\\cp\\h2-1.3.157.jar;g:\\cp\\guava-12.0-sources.jar;g:\\cp\\guava-12.0.jar;g:\\cp\\gson-2.3.1.jar;g:\\cp\\gridsam-schema-2.0.1.jar;g:\\cp\\gridsam-core-2.0.1.jar;g:\\cp\\gridsam-client.jar;g:\\cp\\GridsamAdaptor.jar;g:\\cp\\graphlayout-1.2.1.jar;g:\\cp\\gram-utils.jar;g:\\cp\\gram-stubs.jar;g:\\cp\\gram-client.jar;g:\\cp\\globus_wsrf_rft_stubs.jar;g:\\cp\\globus_wsrf_rft.jar;g:\\cp\\globus_wsrf_rft_client.jar;g:\\cp\\globus_wsrf_rendezvous_stubs.jar;g:\\cp\\globus_wsrf_rendezvous_service.jar;g:\\cp\\globus_wsrf_mds_aggregator_stubs.jar;g:\\cp\\globus_delegation_stubs.jar;g:\\cp\\globus_delegation_service.jar;g:\\cp\\GlobusAdaptor.jar;g:\\cp\\glite-security-util-java.jar;g:\\cp\\glite-security-trustmanager.jar;g:\\cp\\glite-security-delegation-java.jar;g:\\cp\\glite-jdl-api-java.jar;g:\\cp\\GliteAdaptor.jar;g:\\cp\\GenericAdaptor.jar;g:\\cp\\GAT-engine.jar;g:\\cp\\GAT-API.jar;g:\\cp\\ganymed-ssh2-build211beta4.jar;g:\\cp\\forms-1.3.0.jar;g:\\cp\\ext.bundle.xml.xerces-1.1.6-SNAPSHOT.jar;g:\\cp\\ext.bundle.xml.dom4j_161-1.1.6-SNAPSHOT.jar;g:\\cp\\ext.bundle.osgi.common-1.1.6-SNAPSHOT.jar;g:\\cp\\ext.bundle.openconverters-1.1.6-SNAPSHOT.jar;g:\\cp\\ext.bundle.math.jscience_50SNAP-1.0.9.jar;g:\\cp\\ext.bundle.jena_all_2_10_1-1.1.6-SNAPSHOT.jar;g:\\cp\\ext.bundle.apache_httpclient-1.1.6-SNAPSHOT.jar;g:\\cp\\ekitspell.jar;g:\\cp\\edtftpj-1.5.2.jar;g:\\cp\\ecj-3.5.1.jar;g:\\cp\\drmaa.jar;g:\\cp\\dom4j-1.6.1.jar;g:\\cp\\docking-frames-core.jar;g:\\cp\\docking-frames-common.jar;g:\\cp\\cycSparqlEndpoint.jar;g:\\cp\\customSysParam.jar;g:\\cp\\cryptix.jar;g:\\cp\\cryptix-asn1.jar;g:\\cp\\cryptix32.jar;g:\\cp\\concurrent.jar;g:\\cp\\com.springsource.org.apache.log4j-1.2.16.jar;g:\\cp\\com.springsource.com.ibm.icu-3.4.5.jar;g:\\cp\\commons-vfs2-2.1.jar;g:\\cp\\commons-vfs-1.0.jar;g:\\cp\\commons-math-3.0-SNAPSHOT.jar;g:\\cp\\commons-logging-1.1.3.jar;g:\\cp\\commons-lang3-3.1.jar;g:\\cp\\commons-lang-2.4.jar;g:\\cp\\commons-httpclient-3.1.jar;g:\\cp\\commons-discovery-0.4.jar;g:\\cp\\commons-digester.jar;g:\\cp\\commons-collections-3.0.jar;g:\\cp\\commons-codec-1.6.jar;g:\\cp\\commons-cli-1.0.jar;g:\\cp\\commons-beanutils.jar;g:\\cp\\commonj.jar;g:\\cp\\CommandlineSshAdaptor.jar;g:\\cp\\cog-util-0.91.jar;g:\\cp\\cog-url.jar;g:\\cp\\cog-trap-1.0.jar;g:\\cp\\cog-setup-0.91.jar;g:\\cp\\cog-resources-1.0.jar;g:\\cp\\cog-provider-webdav-1.0.jar;g:\\cp\\cog-provider-ssh-2.1.jar;g:\\cp\\cog-provider-local-2.0.jar;g:\\cp\\cog-provider-gt4_0_0-2.3.jar;g:\\cp\\cog-provider-gt3_2_1-2.0.jar;g:\\cp\\cog-provider-gt2ft-1.0.jar;g:\\cp\\cog-provider-gt2-2.2.jar;g:\\cp\\cog-provider-condor-2.0.jar;g:\\cp\\cog-provider-clref-gt4_0_0.jar;g:\\cp\\cog-provider-clref-gt3_2_1.jar;g:\\cp\\cog-karajan-0.33.jar;g:\\cp\\cog-jobmanager.jar;g:\\cp\\cog-jglobus.jar;g:\\cp\\cog-gridshell-1.0.jar;g:\\cp\\cog-gridfaces-1.0.jar;g:\\cp\\cog-grapheditor-0.47.jar;g:\\cp\\cog-certrequest-1.0.jar;g:\\cp\\cog-certmanagement-1.0.jar;g:\\cp\\cog-axis.jar;g:\\cp\\cog-abstraction-examples-2.1.jar;g:\\cp\\cog-abstraction-common-2.1.jar;g:\\cp\\castor-1.1.1-xml.jar;g:\\cp\\castor-0.9.6.jar;g:\\cp\\bsf-utils-3.1.jar;g:\\cp\\bsf.jar;g:\\cp\\bsf-api-3.1.jar;g:\\cp\\bsf-all-3.1.jar;g:\\cp\\BrowserLauncher2-1_3.jar;g:\\cp\\bcprov-jdk15-133.jar;g:\\cp\\backport-util-concurrent.jar;g:\\cp\\axis-jaxrpc-1.4.jar;g:\\cp\\axis-1.4.jar;g:\\cp\\arq-extra.jar;g:\\cp\\antlr-2.7.5.jar;g:\\cp\\ant-1.6.5.jar;g:\\cp\\addressing-1.0.jar;g:\\cp\\activation.jar;g:\\cp\\2p.jar').
:- endif.
:- endif.

append_to_classpath(WildCard):-
  expand_file_name(WildCard,O),
  maplist(prolog_to_os_filename,O,Fs),

  (       current_prolog_flag(windows, true)
    ->      Separator = ';'
    ;       Separator = ':'
    ),
   atomic_list_concat([''|Fs],Separator,Extra),
   getenv('CLASSPATH',WAS),
   atom_concat(WAS,Extra,NEW),
   setenv('CLASSPATH',NEW),!.

% :- append_to_classpath('G:/cp/*.jar').

% :- call(call,swi_cli:jpl_start_dbg(5005)).

:- dynamic(jdwp_available/0).
:- if(false).
jdwp_available.
:- if( \+ jdwp_available).
:- catch((  tcp_connect('10.0.0.95':5005, StreamPair, []),close(StreamPair),asserta(jdwp_available)),_,true).
:- endif.
:- endif.

:- if(jdwp_available).
jvm_opts(
    ['-agentlib:jdwp=transport=dt_socket,suspend=y,address=10.0.0.95:5005,server=n','-XX:PermSize=512m',
     '-XX:MaxPermSize=4g','-Xmx26g','-Djava.util.Arrays.useLegacyMergeSort=true','-Dsun.java2d.d3d=false' % ,'-Dfile.encoding=Cp1252',
     %,'-Xbootclasspath:C:\\pf\\java\\jdk\\jre\\lib\\resources.jar;C:\\pf\\java\\jdk\\jre\\lib\\rt.jar;C:\\pf\\java\\jdk\\jre\\lib\\jsse.jar;C:\\pf\\java\\jdk\\jre\\lib\\jce.jar;C:\\pf\\java\\jdk\\jre\\lib\\charsets.jar;C:\\pf\\java\\jdk\\jre\\lib\\jfr.jar;C:\\pf\\java\\jdk\\lib\\tools.jar;C:\\pf\\java\\jdk\\lib\\sa-jdi.jar'
     ]).
:-else.
jvm_opts(
    [ '-agentlib:jdwp=transport=dt_socket,suspend=n,address=5005,server=y',
      '-XX:PermSize=512m',
     '-XX:MaxPermSize=4g','-Xmx26g','-Djava.util.Arrays.useLegacyMergeSort=true','-Dsun.java2d.d3d=false' % ,'-Dfile.encoding=Cp1252',
     %,'-Xbootclasspath:C:\\pf\\java\\jdk\\jre\\lib\\resources.jar;C:\\pf\\java\\jdk\\jre\\lib\\rt.jar;C:\\pf\\java\\jdk\\jre\\lib\\jsse.jar;C:\\pf\\java\\jdk\\jre\\lib\\jce.jar;C:\\pf\\java\\jdk\\jre\\lib\\charsets.jar;C:\\pf\\java\\jdk\\jre\\lib\\jfr.jar;C:\\pf\\java\\jdk\\lib\\tools.jar;C:\\pf\\java\\jdk\\lib\\sa-jdi.jar'
     ]).
:-endif.



:- multifile oo_started/0.
:- dynamic(oo_started/0).

jpl:- oo_started,!.
jpl:-
  asserta((oo_started)),
  ensure_loaded(library(jpl)),
  getenv('PATH',PATH),getenv('JAVA_HOME',JH),current_prolog_flag(home,HOME),atomic_list_concat([JH,HOME],';',NEWPATH),
  setenv('PATH',NEWPATH),
  use_module(library(jpl)),
  setenv('PATH',PATH),
  jvm_opts(Opts),
  jpl_set_default_jvm_opts(Opts),
  getenv('CLASSPATH',WAS),format('CLASSPATH=~q~n',[WAS]),
  jpl:setup_jvm,
  jpl_classname_to_class('java.lang.Class', _CC).


% :- jpl.
% :- set_prolog_flag(verbose_load, silent).





     /*******************************
      ****     SWICLI SETUP      *****
     *******************************/

swicli:- oo_started,!.
swicli:- lmp,ensure_loaded(library(swicli)),
   cli_type_to_classname('java.lang.Class', _CC),
   cli_ensure_classpath.

swc:-
   cli_type_to_classname('org.slf4j.LoggerFactory',Found1),
   writeln([found,Found1]),
   cli_type_to_classname('org.logicmoo.system.BeanShellCntrl',Found),
   writeln([found,Found]),
   jpl_call('org.logicmoo.system.BeanShellCntrl',start_from_prolog_ikvm,[],_O).




:- if(not(current_module(prolog_server))).
:- if(not(current_module(jpl))).
%:- swicli.
:- endif.
:- endif.

:- if(not(current_module(swicli))).
:- jpl.
:- endif.


uabcl:- jpl_call('org.armedbear.lisp.Main',main,[[]],_Out).
jabcl:- jpl_call('org.armedbear.lisp.Main',main,[[]],_Out).

% :- swc.
% :- jpl.
% :- set_prolog_flag(verbose_load, silent).

startDmiles:- dwq_call(jpl_call('org.logicmoo.system.BeanShellCntrl',start_from_prolog,[],_O)).

:- thread_create(startDmiles,_,[detached(true),debug(false),alias(startDmiles)]).
:- set_prolog_flag(access_level,system).                         
:- debug.

do_rn:- ensure_loaded(rs), forall(rn(B,A),do_rn(B,A)).
p_rn:- ensure_loaded(rs), forall(rn(B,A),p_rn(B,A)).

p_rn(B,A):- atom_string(B,BS),atom_string(A,AS),format('(safely-rename-or-merge ~q ~q).~n',[BS,AS]).
do_rn(B,A):- atom_string(B,BS),atom_string(A,AS),cl_eval(['safely-rename-or-merge',BS,AS], _O),!.


test_e:- cl_eval('*package*').
test_e:- cl_eval([+,1,2], O),po(O).
test_e:- cl_eval_string("(+ 1 2)", O),po(O.toString).
test_e:- cl_read_lisp("(+ 1 2)", O), po(O.cdr.toString).

test_x:- jpl:jpl_class_to_methods('org.logicmoo.system.BeanShellCntrl',_C).



end_of_file.


cl_eval(['cyc:safely-rename-or-merge',"Rebelliousness","vtRebelliousnessFeeling"]).
cl_eval(['cyc-rename-fast',"Rebelliousness","vtRebelliousnessFeeling"]).

do_rn('Rebelliousness', vtRebelliousnessFeeling).

bsh % org.jpl7.Query.oneSolution("thread_self(ID)");

Target exception: org.jpl7.JPLException: unknown term type=8

(gdb)
(gdb)
(gdb) help all


import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

Command class: aliases

ni -- Step one instruction
rc -- Continue program being debugged but run it in reverse
rni -- Step backward one instruction
rsi -- Step backward exactly one instruction
si -- Step one instruction exactly
stepping -- Specify single-stepping behavior at a tracepoint
tp -- Set a tracepoint at specified location
tty -- Set terminal for future runs of program being debugged
where -- Print backtrace of all stack frames
ws -- Specify single-stepping behavior at a tracepoint

Command class: breakpoints

awatch -- Set a watchpoint for an expression
break -- Set breakpoint at specified location
break-range -- Set a breakpoint for an address range
catch -- Set catchpoints to catch events
catch assert -- Catch failed Ada assertions
catch catch -- Catch an exception
catch exception -- Catch Ada exceptions
catch exec -- Catch calls to exec
catch fork -- Catch calls to fork
catch load -- Catch loads of shared libraries
catch rethrow -- Catch an exception
catch signal -- Catch signals by their names and/or numbers
catch syscall -- Catch system calls by their names and/or numbers
catch throw -- Catch an exception
catch unload -- Catch unloads of shared libraries
catch vfork -- Catch calls to vfork
clear -- Clear breakpoint at specified location
commands -- Set commands to be executed when a breakpoint is hit
condition -- Specify breakpoint number N to break only if COND is true
delete -- Delete some breakpoints or auto-display expressions
delete bookmark -- Delete a bookmark from the bookmark list
delete breakpoints -- Delete some breakpoints or auto-display expressions
delete checkpoint -- Delete a checkpoint (experimental)
delete display -- Cancel some expressions to be displayed when program stops
delete mem -- Delete memory region
delete tracepoints -- Delete specified tracepoints
delete tvariable -- Delete one or more trace state variables
---Type <return> to continue, or q <return> to quit---
disable -- Disable some breakpoints
disable breakpoints -- Disable some breakpoints
disable display -- Disable some expressions to be displayed when program stops
disable frame-filter -- GDB command to disable the specified frame-filter
disable mem -- Disable memory region
disable pretty-printer -- GDB command to disable the specified pretty-printer
disable probes -- Disable probes
disable tracepoints -- Disable specified tracepoints
disable type-printer -- GDB command to disable the specified type-printer
disable unwinder -- GDB command to disable the specified unwinder
disable xmethod -- GDB command to disable a specified (group of) xmethod(s)
dprintf -- Set a dynamic printf at specified location
enable -- Enable some breakpoints
enable breakpoints -- Enable some breakpoints
enable breakpoints count -- Enable breakpoints for COUNT hits
enable breakpoints delete -- Enable breakpoints and delete when hit
enable breakpoints once -- Enable breakpoints for one hit
enable count -- Enable breakpoints for COUNT hits
enable delete -- Enable breakpoints and delete when hit
enable display -- Enable some expressions to be displayed when program stops
enable frame-filter -- GDB command to disable the specified frame-filter
enable mem -- Enable memory region
enable once -- Enable breakpoints for one hit
enable pretty-printer -- GDB command to enable the specified pretty-printer
enable probes -- Enable probes
enable tracepoints -- Enable specified tracepoints
enable type-printer -- GDB command to enable the specified type printer
enable unwinder -- GDB command to enable unwinders
enable xmethod -- GDB command to enable a specified (group of) xmethod(s)
ftrace -- Set a fast tracepoint at specified location
hbreak -- Set a hardware assisted breakpoint
ignore -- Set ignore-count of breakpoint number N to COUNT
rbreak -- Set a breakpoint for all functions matching REGEXP
rwatch -- Set a read watchpoint for an expression
save -- Save breakpoint definitions as a script
save breakpoints -- Save current breakpoint definitions as a script
save gdb-index -- Save a gdb-index file
save tracepoints -- Save current tracepoint definitions as a script
skip -- Ignore a function while stepping
skip delete -- Delete skip entries
skip disable -- Disable skip entries
skip enable -- Enable skip entries
skip file -- Ignore a file while stepping
---Type <return> to continue, or q <return> to quit---
skip function -- Ignore a function while stepping
strace -- Set a static tracepoint at location or marker
tbreak -- Set a temporary breakpoint
tcatch -- Set temporary catchpoints to catch events
tcatch assert -- Catch failed Ada assertions
tcatch catch -- Catch an exception
tcatch exception -- Catch Ada exceptions
tcatch exec -- Catch calls to exec
tcatch fork -- Catch calls to fork
tcatch load -- Catch loads of shared libraries
tcatch rethrow -- Catch an exception
tcatch signal -- Catch signals by their names and/or numbers
tcatch syscall -- Catch system calls by their names and/or numbers
tcatch throw -- Catch an exception
tcatch unload -- Catch unloads of shared libraries
tcatch vfork -- Catch calls to vfork
thbreak -- Set a temporary hardware assisted breakpoint
trace -- Set a tracepoint at specified location
watch -- Set a watchpoint for an expression

Command class: data

agent-printf -- Agent-printf "printf format string"
append -- Append target code/data to a local file
append binary -- Append target code/data to a raw binary file
append binary memory -- Append contents of memory to a raw binary file
append binary value -- Append the value of an expression to a raw binary file
append memory -- Append contents of memory to a raw binary file
append value -- Append the value of an expression to a raw binary file
call -- Call a function in the program
disassemble -- Disassemble a specified section of memory
display -- Print value of expression EXP each time the program stops
dump -- Dump target code/data to a local file
dump binary -- Write target code/data to a raw binary file
dump binary memory -- Write contents of memory to a raw binary file
dump binary value -- Write the value of an expression to a raw binary file
dump ihex -- Write target code/data to an intel hex file
dump ihex memory -- Write contents of memory to an ihex file
dump ihex value -- Write the value of an expression to an ihex file
dump memory -- Write contents of memory to a raw binary file
dump srec -- Write target code/data to an srec file
dump srec memory -- Write contents of memory to an srec file
dump srec value -- Write the value of an expression to an srec file
---Type <return> to continue, or q <return> to quit---
dump tekhex -- Write target code/data to a tekhex file
dump tekhex memory -- Write contents of memory to a tekhex file
dump tekhex value -- Write the value of an expression to a tekhex file
dump value -- Write the value of an expression to a raw binary file
dump verilog -- Write target code/data to a verilog hex file
dump verilog memory -- Write contents of memory to a verilog hex file
dump verilog value -- Write the value of an expression to a verilog hex file
explore -- Explore a value or a type valid in the current context
explore type -- Explore a type or the type of an expression valid in the current
explore value -- Explore value of an expression valid in the current context
find -- Search memory for a sequence of bytes
init-if-undefined -- Initialize a convenience variable if necessary
mem -- Define attributes for memory region or reset memory region handling to
output -- Like "print" but don't put in value history and don't print newline
print -- Print value of expression EXP
print-object -- Ask an Objective-C object to print itself
printf -- Printf "printf format string"
ptype -- Print definition of type TYPE
restore -- Restore the contents of FILE to target memory
set -- Evaluate expression EXP and assign result to variable VAR
set ada -- Prefix command for changing Ada-specfic settings
set ada print-signatures -- Enable or disable the output of formal and return types for functions in the overloads selection menu
set ada trust-PAD-over-XVS -- Enable or disable an optimization trusting PAD types over XVS types
set agent -- Set debugger's willingness to use agent as a helper
set annotate -- Set annotation_level
set architecture -- Set architecture of target
set args -- Set argument list to give program being debugged when it is started
set auto-connect-native-target -- Set whether GDB may automatically connect to the native target
set auto-load -- Auto-loading specific settings
set auto-load gdb-scripts -- Enable or disable auto-loading of canned sequences of commands scripts
set auto-load libthread-db -- Enable or disable auto-loading of inferior specific libthread_db
set auto-load local-gdbinit -- Enable or disable auto-loading of .gdbinit script in current directory
set auto-load python-scripts -- Set the debugger's behaviour regarding auto-loaded Python scripts
set auto-load safe-path -- Set the list of files and directories that are safe for auto-loading
set auto-load scripts-directory -- Set the list of directories from which to load auto-loaded scripts
set auto-load-scripts -- Set the debugger's behaviour regarding auto-loaded Python scripts
set auto-solib-add -- Set autoloading of shared library symbols
set backtrace -- Set backtrace specific variables
set backtrace limit -- Set an upper bound on the number of backtrace levels
set backtrace past-entry -- Set whether backtraces should continue past the entry point of a program
set backtrace past-main -- Set whether backtraces should continue past "main"
set basenames-may-differ -- Set whether a source file may have multiple base names
set breakpoint -- Breakpoint specific settings
---Type <return> to continue, or q <return> to quit---
set breakpoint always-inserted -- Set mode for inserting breakpoints
set breakpoint auto-hw -- Set automatic usage of hardware breakpoints
set breakpoint condition-evaluation -- Set mode of breakpoint condition evaluation
set breakpoint pending -- Set debugger's behavior regarding pending breakpoints
set can-use-hw-watchpoints -- Set debugger's willingness to use watchpoint hardware
set case-sensitive -- Set case sensitivity in name search
set charset -- Set the host and target character sets
set check -- Set the status of the type/range checker
set check range -- Set range checking
set check type -- Set strict type checking
set circular-trace-buffer -- Set target's use of circular trace buffer
set code-cache -- Set cache use for code segment access
set coerce-float-to-double -- Set coercion of floats to doubles when calling functions
set compile-args -- Set compile command GCC command-line arguments
set complaints -- Set max number of complaints about incorrect symbols
set confirm -- Set whether to confirm potentially dangerous operations
set cp-abi -- Set the ABI used for inspecting C++ objects
set data-directory -- Set GDB's data directory
set dcache -- Use this command to set number of lines in dcache and line-size
set dcache line-size -- Set dcache line size in bytes (must be power of 2)
set dcache size -- Set number of dcache lines
set debug -- Generic command for setting gdb debugging flags
set debug arch -- Set architecture debugging
set debug auto-load -- Set auto-load verifications debugging
set debug bfd-cache -- Set bfd cache debugging
set debug check-physname -- Set cross-checking of "physname" code against demangler
set debug coff-pe-read -- Set coff PE read debugging
set debug compile -- Set compile command debugging
set debug displaced -- Set displaced stepping debugging
set debug dwarf-die -- Set debugging of the DWARF DIE reader
set debug dwarf-line -- Set debugging of the dwarf line reader
set debug dwarf-read -- Set debugging of the DWARF reader
set debug entry-values -- Set entry values and tail call frames debugging
set debug expression -- Set expression debugging
set debug frame -- Set frame debugging
set debug infrun -- Set inferior debugging
set debug jit -- Set JIT debugging
set debug libthread-db -- Set libthread-db debugging
set debug lin-lwp -- Set debugging of GNU/Linux lwp module
set debug linux-namespaces -- Set debugging of GNU/Linux namespaces module
set debug notification -- Set debugging of async remote notification
set debug observer -- Set observer debugging
set debug overload -- Set debugging of C++ overloading
---Type <return> to continue, or q <return> to quit---
set debug parser -- Set parser debugging
set debug py-unwind -- Set Python unwinder debugging
set debug record -- Set debugging of record/replay feature
set debug remote -- Set debugging of remote protocol
set debug serial -- Set serial debugging
set debug stap-expression -- Set SystemTap expression debugging
set debug symbol-lookup -- Set debugging of symbol lookup
set debug symfile -- Set debugging of the symfile functions
set debug symtab-create -- Set debugging of symbol table creation
set debug target -- Set target debugging
set debug timestamp -- Set timestamping of debugging messages
set debug varobj -- Set varobj debugging
set debug xml -- Set XML parser debugging
set debug-file-directory -- Set the directories where separate debug symbols are searched for
set default-collect -- Set the list of expressions to collect by default
set demangle-style -- Set the current C++ demangling style
set detach-on-fork -- Set whether gdb will detach the child of a fork
set directories -- Set the search path for finding source files
set disable-randomization -- Set disabling of debuggee's virtual address space randomization
set disassemble-next-line -- Set whether to disassemble next source line or insn when execution stops
set disassembly-flavor -- Set the disassembly flavor
set disconnected-dprintf -- Set whether dprintf continues after GDB disconnects
set disconnected-tracing -- Set whether tracing continues after GDB disconnects
set displaced-stepping -- Set debugger's willingness to use displaced stepping
set dprintf-channel -- Set the channel to use for dynamic printf
set dprintf-function -- Set the function to use for dynamic printf
set dprintf-style -- Set the style of usage for dynamic printf
set editing -- Set editing of command lines as they are typed
set endian -- Set endianness of target
set environment -- Set environment variable value to give the program
set exec-direction -- Set direction of execution
set exec-done-display -- Set notification of completion for asynchronous execution commands
set exec-wrapper -- Set a wrapper for running programs
set extended-prompt -- Set the extended prompt
set extension-language -- Set mapping between filename extension and source language
set filename-display -- Set how to display filenames
set follow-exec-mode -- Set debugger response to a program call of exec
set follow-fork-mode -- Set debugger response to a program call of fork or vfork
set frame-filter -- Prefix command for 'set' frame-filter related operations
set frame-filter priority -- GDB command to set the priority of the specified frame-filter
set gnutarget -- Set the current BFD target
set guile -- Prefix command for Guile preference settings
set guile print-stack -- Set mode for Guile exception printing on error
---Type <return> to continue, or q <return> to quit---
set height -- Set number of lines in a page for GDB output pagination
set history -- Generic command for setting command history parameters
set history expansion -- Set history expansion on command input
set history filename -- Set the filename in which to record the command history
set history remove-duplicates -- Set how far back in history to look for and remove duplicate entries
set history save -- Set saving of the history record on exit
set history size -- Set the size of the command history
set host-charset -- Set the host character set
set inferior-tty -- Set terminal for future runs of program being debugged
set input-radix -- Set default input radix for entering numbers
set interactive-mode -- Set whether GDB's standard input is a terminal
set language -- Set the current source language
set libthread-db-search-path -- Set search path for libthread_db
set listsize -- Set number of source lines gdb will list by default
set logging -- Set logging options
set logging file -- Set the current logfile
set logging off -- Disable logging
set logging on -- Enable logging
set logging overwrite -- Set whether logging overwrites or appends to the log file
set logging redirect -- Set the logging output mode
set max-completions -- Set maximum number of completion candidates
set max-user-call-depth -- Set the max call depth for non-python/scheme user-defined commands
set max-value-size -- Set maximum sized value gdb will load from the inferior
set may-insert-breakpoints -- Set permission to insert breakpoints in the target
set may-insert-fast-tracepoints -- Set permission to insert fast tracepoints in the target
set may-insert-tracepoints -- Set permission to insert tracepoints in the target
set may-interrupt -- Set permission to interrupt or signal the target
set may-write-memory -- Set permission to write into target memory
set may-write-registers -- Set permission to write into registers
set mem -- Memory regions settings
set mem inaccessible-by-default -- Set handling of unknown memory regions
set mi-async -- Set whether MI asynchronous mode is enabled
set mpx -- Set Intel Memory Protection Extensions specific variables
set mpx bound -- Set the memory bounds for a given array/pointer storage in the bound table
set multiple-symbols -- Set the debugger behavior when more than one symbol are possible matches
set non-stop -- Set whether gdb controls the inferior in non-stop mode
set observer -- Set whether gdb controls the inferior in observer mode
set opaque-type-resolution -- Set resolution of opaque struct/class/union types (if set before loading symbols)
set osabi -- Set OS ABI of target
set output-radix -- Set default output radix for printing of values
set overload-resolution -- Set overload resolution in evaluating C++ functions
set pagination -- Set state of GDB output pagination
set print -- Generic command for setting how things print
---Type <return> to continue, or q <return> to quit---
set print address -- Set printing of addresses
set print array -- Set pretty formatting of arrays
set print array-indexes -- Set printing of array indexes
set print asm-demangle -- Set demangling of C++/ObjC names in disassembly listings
set print demangle -- Set demangling of encoded C++/ObjC names when displaying symbols
set print elements -- Set limit on string chars or array elements to print
set print entry-values -- Set printing of function arguments at function entry
set print frame-arguments -- Set printing of non-scalar frame arguments
set print inferior-events -- Set printing of inferior events (e.g.
set print max-symbolic-offset -- Set the largest offset that will be printed in <symbol+1234> form
set print null-stop -- Set printing of char arrays to stop at first null char
set print object -- Set printing of object's derived type based on vtable info
set print pascal_static-members -- Set printing of pascal static members
set print pretty -- Set pretty formatting of structures
set print raw -- Generic command for setting what things to print in "raw" mode
set print raw frame-arguments -- Set whether to print frame arguments in raw form
set print repeats -- Set threshold for repeated print elements
set print sevenbit-strings -- Set printing of 8-bit characters in strings as \nnn
set print static-members -- Set printing of C++ static members
set print symbol -- Set printing of symbol names when printing pointers
set print symbol-filename -- Set printing of source filename and line number with <symbol>
set print symbol-loading -- Set printing of symbol loading messages
set print thread-events -- Set printing of thread events (such as thread start and exit)
set print type -- Generic command for setting how types print
show print type methods -- Set printing of methods defined in classes
show print type typedefs -- Set printing of typedefs defined in classes
set print union -- Set printing of unions interior to structures
set print vtbl -- Set printing of C++ virtual function tables
set prompt -- Set gdb's prompt
set python -- Prefix command for python preference settings
set python print-stack -- Set mode for Python stack dump on error
set radix -- Set default input and output number radices
set range-stepping -- Enable or disable range stepping
set record -- Set record options
set record btrace -- Set record options
set record btrace bts -- Set record btrace bts options
set record btrace bts buffer-size -- Set the record/replay bts buffer size
set record btrace pt -- Set record btrace pt options
set record btrace pt buffer-size -- Set the record/replay pt buffer size
set record btrace replay-memory-access -- Set what memory accesses are allowed during replay
set record full -- Set record options
set record full insn-number-max -- Set record/replay buffer limit
set record full memory-query -- Set whether query if PREC cannot record memory change of next instruction
---Type <return> to continue, or q <return> to quit---
set record full stop-at-limit -- Set whether record/replay stops when record/replay buffer becomes full
set record function-call-history-size -- Set number of function to print in "record function-call-history"
set record instruction-history-size -- Set number of instructions to print in "record instruction-history"
set remote -- Remote protocol specific variables
set remote P-packet -- Set use of remote protocol `P' (set-register) packet
set remote TracepointSource-packet -- Set use of remote protocol `TracepointSource' (TracepointSource) packet
set remote X-packet -- Set use of remote protocol `X' (binary-download) packet
set remote Z-packet -- Set use of remote protocol `Z' packets
set remote access-watchpoint-packet -- Set use of remote protocol `Z4' (access-watchpoint) packet
set remote agent-packet -- Set use of remote protocol `QAgent' (agent) packet
set remote allow-packet -- Set use of remote protocol `QAllow' (allow) packet
set remote attach-packet -- Set use of remote protocol `vAttach' (attach) packet
set remote binary-download-packet -- Set use of remote protocol `X' (binary-download) packet
set remote breakpoint-commands-packet -- Set use of remote protocol `BreakpointCommands' (breakpoint-commands) packet
set remote btrace-conf-bts-size-packet -- Set use of remote protocol `Qbtrace-conf:bts:size' (btrace-conf-bts-size) packet
set remote btrace-conf-pt-size-packet -- Set use of remote protocol `Qbtrace-conf:pt:size' (btrace-conf-pt-size) packet
set remote catch-syscalls-packet -- Set use of remote protocol `QCatchSyscalls' (catch-syscalls) packet
set remote conditional-breakpoints-packet -- Set use of remote protocol `ConditionalBreakpoints' (conditional-breakpoints) packet
set remote conditional-tracepoints-packet -- Set use of remote protocol `ConditionalTracepoints' (conditional-tracepoints) packet
set remote ctrl-c-packet -- Set use of remote protocol `vCtrlC' (ctrl-c) packet
set remote disable-btrace-packet -- Set use of remote protocol `Qbtrace:off' (disable-btrace) packet
set remote disable-randomization-packet -- Set use of remote protocol `QDisableRandomization' (disable-randomization) packet
set remote enable-btrace-bts-packet -- Set use of remote protocol `Qbtrace:bts' (enable-btrace-bts) packet
set remote enable-btrace-pt-packet -- Set use of remote protocol `Qbtrace:pt' (enable-btrace-pt) packet
set remote exec-event-feature-packet -- Set use of remote protocol `exec-event-feature' (exec-event-feature) packet
set remote exec-file -- Set the remote pathname for "run"
set remote fast-tracepoints-packet -- Set use of remote protocol `FastTracepoints' (fast-tracepoints) packet
set remote fetch-register-packet -- Set use of remote protocol `p' (fetch-register) packet
set remote fork-event-feature-packet -- Set use of remote protocol `fork-event-feature' (fork-event-feature) packet
set remote get-thread-information-block-address-packet -- Set use of remote protocol `qGetTIBAddr' (get-thread-information-block-address) packet
set remote get-thread-local-storage-address-packet -- Set use of remote protocol `qGetTLSAddr' (get-thread-local-storage-address) packet
set remote hardware-breakpoint-limit -- Set the maximum number of target hardware breakpoints
set remote hardware-breakpoint-packet -- Set use of remote protocol `Z1' (hardware-breakpoint) packet
set remote hardware-watchpoint-length-limit -- Set the maximum length (in bytes) of a target hardware watchpoint
set remote hardware-watchpoint-limit -- Set the maximum number of target hardware watchpoints
set remote hostio-close-packet -- Set use of remote protocol `vFile:close' (hostio-close) packet
set remote hostio-fstat-packet -- Set use of remote protocol `vFile:fstat' (hostio-fstat) packet
set remote hostio-open-packet -- Set use of remote protocol `vFile:open' (hostio-open) packet
set remote hostio-pread-packet -- Set use of remote protocol `vFile:pread' (hostio-pread) packet
set remote hostio-pwrite-packet -- Set use of remote protocol `vFile:pwrite' (hostio-pwrite) packet
set remote hostio-readlink-packet -- Set use of remote protocol `vFile:readlink' (hostio-readlink) packet
set remote hostio-setfs-packet -- Set use of remote protocol `vFile:setfs' (hostio-setfs) packet
set remote hostio-unlink-packet -- Set use of remote protocol `vFile:unlink' (hostio-unlink) packet
---Type <return> to continue, or q <return> to quit---
set remote hwbreak-feature-packet -- Set use of remote protocol `hwbreak-feature' (hwbreak-feature) packet
set remote install-in-trace-packet -- Set use of remote protocol `InstallInTrace' (install-in-trace) packet
set remote interrupt-on-connect -- Set whether interrupt-sequence is sent to remote target when gdb connects to
set remote interrupt-sequence -- Set interrupt sequence to remote target
set remote kill-packet -- Set use of remote protocol `vKill' (kill) packet
set remote library-info-packet -- Set use of remote protocol `qXfer:libraries:read' (library-info) packet
set remote library-info-svr4-packet -- Set use of remote protocol `qXfer:libraries-svr4:read' (library-info-svr4) packet
set remote memory-map-packet -- Set use of remote protocol `qXfer:memory-map:read' (memory-map) packet
set remote memory-read-packet-size -- Set the maximum number of bytes per memory-read packet
set remote memory-write-packet-size -- Set the maximum number of bytes per memory-write packet
set remote multiprocess-feature-packet -- Set use of remote protocol `multiprocess-feature' (multiprocess-feature) packet
set remote no-resumed-stop-reply-packet -- Set use of remote protocol `N stop reply' (no-resumed-stop-reply) packet
set remote noack-packet -- Set use of remote protocol `QStartNoAckMode' (noack) packet
set remote osdata-packet -- Set use of remote protocol `qXfer:osdata:read' (osdata) packet
set remote p-packet -- Set use of remote protocol `p' (fetch-register) packet
set remote pass-signals-packet -- Set use of remote protocol `QPassSignals' (pass-signals) packet
set remote pid-to-exec-file-packet -- Set use of remote protocol `qXfer:exec-file:read' (pid-to-exec-file) packet
set remote program-signals-packet -- Set use of remote protocol `QProgramSignals' (program-signals) packet
set remote query-attached-packet -- Set use of remote protocol `qAttached' (query-attached) packet
set remote read-aux-vector-packet -- Set use of remote protocol `qXfer:auxv:read' (read-aux-vector) packet
set remote read-btrace-conf-packet -- Set use of remote protocol `qXfer:btrace-conf' (read-btrace-conf) packet
set remote read-btrace-packet -- Set use of remote protocol `qXfer:btrace' (read-btrace) packet
set remote read-fdpic-loadmap-packet -- Set use of remote protocol `qXfer:fdpic:read' (read-fdpic-loadmap) packet
set remote read-sdata-object-packet -- Set use of remote protocol `qXfer:statictrace:read' (read-sdata-object) packet
set remote read-siginfo-object-packet -- Set use of remote protocol `qXfer:siginfo:read' (read-siginfo-object) packet
set remote read-spu-object-packet -- Set use of remote protocol `qXfer:spu:read' (read-spu-object) packet
set remote read-watchpoint-packet -- Set use of remote protocol `Z3' (read-watchpoint) packet
set remote reverse-continue-packet -- Set use of remote protocol `bc' (reverse-continue) packet
set remote reverse-step-packet -- Set use of remote protocol `bs' (reverse-step) packet
set remote run-packet -- Set use of remote protocol `vRun' (run) packet
set remote search-memory-packet -- Set use of remote protocol `qSearch:memory' (search-memory) packet
set remote set-register-packet -- Set use of remote protocol `P' (set-register) packet
set remote software-breakpoint-packet -- Set use of remote protocol `Z0' (software-breakpoint) packet
set remote static-tracepoints-packet -- Set use of remote protocol `StaticTracepoints' (static-tracepoints) packet
set remote supported-packets-packet -- Set use of remote protocol `qSupported' (supported-packets) packet
set remote swbreak-feature-packet -- Set use of remote protocol `swbreak-feature' (swbreak-feature) packet
set remote symbol-lookup-packet -- Set use of remote protocol `qSymbol' (symbol-lookup) packet
set remote system-call-allowed -- Set if the host system(3) call is allowed for the target
set remote target-features-packet -- Set use of remote protocol `qXfer:features:read' (target-features) packet
set remote thread-events-packet -- Set use of remote protocol `QThreadEvents' (thread-events) packet
set remote threads-packet -- Set use of remote protocol `qXfer:threads:read' (threads) packet
set remote trace-buffer-size-packet -- Set use of remote protocol `QTBuffer:size' (trace-buffer-size) packet
set remote trace-status-packet -- Set use of remote protocol `qTStatus' (trace-status) packet
---Type <return> to continue, or q <return> to quit---
set remote traceframe-info-packet -- Set use of remote protocol `qXfer:traceframe-info:read' (traceframe-info) packet
set remote unwind-info-block-packet -- Set use of remote protocol `qXfer:uib:read' (unwind-info-block) packet
set remote verbose-resume-packet -- Set use of remote protocol `vCont' (verbose-resume) packet
set remote verbose-resume-supported-packet -- Set use of remote protocol `vContSupported' (verbose-resume-supported) packet
set remote vfork-event-feature-packet -- Set use of remote protocol `vfork-event-feature' (vfork-event-feature) packet
set remote write-siginfo-object-packet -- Set use of remote protocol `qXfer:siginfo:write' (write-siginfo-object) packet
set remote write-spu-object-packet -- Set use of remote protocol `qXfer:spu:write' (write-spu-object) packet
set remote write-watchpoint-packet -- Set use of remote protocol `Z2' (write-watchpoint) packet
set remoteaddresssize -- Set the maximum size of the address (in bits) in a memory packet
set remotebreak -- Set whether to send break if interrupted
set remotecache -- Set cache use for remote targets
set remoteflow -- Set use of hardware flow control for remote serial I/O
set remotelogbase -- Set numerical base for remote session logging
set remotelogfile -- Set filename for remote session recording
set remotetimeout -- Set timeout limit to wait for target to respond
set remotewritesize -- Set the maximum number of bytes per memory write packet (deprecated)
set schedule-multiple -- Set mode for resuming threads of all processes
set scheduler-locking -- Set mode for locking scheduler during execution
set script-extension -- Set mode for script filename extension recognition
set serial -- Set default serial/parallel port configuration
set serial baud -- Set baud rate for remote serial I/O
set serial parity -- Set parity for remote serial I/O
set solib-absolute-prefix -- Set an alternate system root
set solib-search-path -- Set the search path for loading non-absolute shared library symbol files
set stack-cache -- Set cache use for stack access
set startup-with-shell -- Set use of shell to start subprocesses
set step-mode -- Set mode of the step operation
set stop-on-solib-events -- Set stopping for shared library events
set struct-convention -- Set the convention for returning small structs
set substitute-path -- Usage: set substitute-path FROM TO
set sysroot -- Set an alternate system root
set target-async -- Set whether MI asynchronous mode is enabled
set target-charset -- Set the target character set
set target-file-system-kind -- Set assumed file system kind for target reported file names
set target-wide-charset -- Set the target wide character set
set tcp -- TCP protocol specific variables
set tcp auto-retry -- Set auto-retry on socket connect
set tcp connect-timeout -- Set timeout limit in seconds for socket connection
set tdesc -- Set target description specific variables
set tdesc filename -- Set the file to read for an XML target description
set trace-buffer-size -- Set requested size of trace buffer
set trace-commands -- Set tracing of GDB CLI commands
set trace-notes -- Set notes string to use for current and future trace runs
---Type <return> to continue, or q <return> to quit---
set trace-stop-notes -- Set notes string to use for future tstop commands
set trace-user -- Set the user name to use for current and future trace runs
set trust-readonly-sections -- Set mode for reading from readonly sections
set tui -- TUI configuration variables
set tui active-border-mode -- Set the attribute mode to use for the active TUI window border
set tui border-kind -- Set the kind of border for TUI windows
set tui border-mode -- Set the attribute mode to use for the TUI window borders
set unwind-on-terminating-exception -- Set unwinding of stack if std::terminate is called while in call dummy
set unwindonsignal -- Set unwinding of stack if a signal is received while in a call dummy
set use-coredump-filter -- Set whether gcore should consider /proc/PID/coredump_filter
set use-deprecated-index-sections -- Set whether to use deprecated gdb_index sections
set variable -- Evaluate expression EXP and assign result to variable VAR
set verbose -- Set verbosity
set watchdog -- Set watchdog timer
set width -- Set number of characters where GDB should wrap lines of its output
set write -- Set writing into executable and core files
undisplay -- Cancel some expressions to be displayed when program stops
whatis -- Print data type of expression EXP
x -- Examine memory: x/FMT ADDRESS

Command class: files

add-symbol-file -- Load symbols from FILE
add-symbol-file-from-memory -- Load the symbols out of memory from a dynamically loaded object file
cd -- Set working directory to DIR for debugger and program being debugged
core-file -- Use FILE as core dump for examining memory and registers
directory -- Add directory DIR to beginning of search path for source files
edit -- Edit specified file or function
exec-file -- Use FILE as program for getting contents of pure memory
file -- Use FILE as program to be debugged
forward-search -- Search for regular expression (see regex(3)) from last line listed
generate-core-file -- Save a core file with the current state of the debugged process
list -- List specified function or line
load -- Dynamically load FILE into the running program
nosharedlibrary -- Unload all shared object library symbols
path -- Add directory DIR(s) to beginning of search path for object files
pwd -- Print working directory
remote -- Manipulate files on the remote system
remote delete -- Delete a remote file
remote get -- Copy a remote file to the local system
remote put -- Copy a local file to the remote system
remove-symbol-file -- Remove a symbol file added via the add-symbol-file command
reverse-search -- Search backward for regular expression (see regex(3)) from last line listed
---Type <return> to continue, or q <return> to quit---
search -- Search for regular expression (see regex(3)) from last line listed
section -- Change the base address of section SECTION of the exec file to ADDR
sharedlibrary -- Load shared object library symbols for files matching REGEXP
symbol-file -- Load symbol table from executable file FILE

Command class: internals

flushregs -- Force gdb to flush its register cache (maintainer command)
maintenance -- Commands for use by GDB maintainers
maintenance agent -- Translate an expression into remote agent bytecode for tracing
maintenance agent-eval -- Translate an expression into remote agent bytecode for evaluation
maintenance agent-printf -- Translate an expression into remote agent bytecode for evaluation and display the bytecodes
maintenance btrace -- Branch tracing maintenance commands
maintenance btrace clear -- Clears the branch tracing data
maintenance btrace clear-packet-history -- Clears the branch tracing packet history
maintenance btrace packet-history -- Print the raw branch tracing data
maintenance check-psymtabs -- Check consistency of currently expanded psymtabs versus symtabs
maintenance check-symtabs -- Check consistency of currently expanded symtabs
maintenance cplus -- C++ maintenance commands
maintenance cplus first_component -- Print the first class/namespace component of NAME
maintenance cplus namespace -- Deprecated placeholder for removed functionality
maintenance demangle -- This command has been moved to "demangle"
maintenance demangler-warning -- Give GDB a demangler warning
maintenance deprecate -- Deprecate a command
maintenance dump-me -- Get fatal error; make debugger dump its core
maintenance expand-symtabs -- Expand symbol tables
maintenance flush-symbol-cache -- Flush the symbol cache for each program space
maintenance info -- Commands for showing internal info about the program being debugged
maintenance info bfds -- List the BFDs that are currently open
maintenance info breakpoints -- Status of all breakpoints
maintenance info btrace -- Info about branch tracing data
maintenance info program-spaces -- Info about currently known program spaces
maintenance info psymtabs -- List the partial symbol tables for all object files
maintenance info sections -- List the BFD sections of the exec and core files
maintenance info symtabs -- List the full symbol tables for all object files
maintenance internal-error -- Give GDB an internal error
maintenance internal-warning -- Give GDB an internal warning
maintenance packet -- Send an arbitrary packet to a remote target
maintenance print -- Maintenance command for printing GDB internal state
maintenance print architecture -- Print the internal architecture configuration
maintenance print c-tdesc -- Print the current target description as a C source file
maintenance print cooked-registers -- Print the internal register configuration including cooked values
maintenance print dummy-frames -- Print the contents of the internal dummy-frame stack
---Type <return> to continue, or q <return> to quit---
maintenance print msymbols -- Print dump of current minimal symbol definitions
maintenance print objfiles -- Print dump of current object file definitions
maintenance print psymbols -- Print dump of current partial symbol definitions
maintenance print raw-registers -- Print the internal register configuration including raw values
maintenance print reggroups -- Print the internal register group names
maintenance print register-groups -- Print the internal register configuration including each register's group
maintenance print registers -- Print the internal register configuration
maintenance print remote-registers -- Print the internal register configuration including each register's
maintenance print statistics -- Print statistics about internal gdb state
maintenance print symbol-cache -- Dump the symbol cache for each program space
maintenance print symbol-cache-statistics -- Print symbol cache statistics for each program space
maintenance print symbols -- Print dump of current symbol definitions
maintenance print target-stack -- Print the name of each layer of the internal target stack
maintenance print type -- Print a type chain for a given symbol
maintenance print user-registers -- List the names of the current user registers
maintenance set -- Set GDB internal variables used by the GDB maintainer
maintenance set ada -- Set Ada maintenance-related variables
maintenance set ada ignore-descriptive-types -- Set whether descriptive types generated by GNAT should be ignored
maintenance set bfd-sharing -- Set whether gdb will share bfds that appear to be the same file
maintenance set btrace -- Set branch tracing specific variables
maintenance set btrace pt -- Set Intel Processor Trace specific variables
maintenance set btrace pt skip-pad -- Set whether PAD packets should be skipped in the btrace packet history
maintenance set catch-demangler-crashes -- Set whether to attempt to catch demangler crashes
maintenance set demangler-warning -- Configure what GDB does when demangler-warning is detected
maintenance set demangler-warning quit -- Set whether GDB should quit when an demangler-warning is detected
maintenance set dwarf -- Set DWARF specific variables
maintenance set dwarf always-disassemble -- Set whether `info address' always disassembles DWARF expressions
maintenance set dwarf max-cache-age -- Set the upper bound on the age of cached DWARF compilation units
maintenance set internal-error -- Configure what GDB does when internal-error is detected
maintenance set internal-error corefile -- Set whether GDB should create a core file of GDB when internal-error is detected
maintenance set internal-error quit -- Set whether GDB should quit when an internal-error is detected
maintenance set internal-warning -- Configure what GDB does when internal-warning is detected
maintenance set internal-warning corefile -- Set whether GDB should create a core file of GDB when internal-warning is detected
maintenance set internal-warning quit -- Set whether GDB should quit when an internal-warning is detected
maintenance set per-command -- Per-command statistics settings
set per-command space -- Set whether to display per-command space usage
set per-command symtab -- Set whether to display per-command symtab statistics
set per-command time -- Set whether to display per-command execution time
maintenance set profile -- Set internal profiling
maintenance set show-debug-regs -- Set whether to show variables that mirror the x86 debug registers
maintenance set symbol-cache-size -- Set the size of the symbol cache
maintenance set target-async -- Set whether gdb controls the inferior in asynchronous mode
maintenance set target-non-stop -- Set whether gdb always controls the inferior in non-stop mode
---Type <return> to continue, or q <return> to quit---
maintenance show -- Show GDB internal variables used by the GDB maintainer
maintenance show ada -- Show Ada maintenance-related variables
maintenance show ada ignore-descriptive-types -- Show whether descriptive types generated by GNAT should be ignored
maintenance show bfd-sharing -- Show whether gdb will share bfds that appear to be the same file
maintenance show btrace -- Show branch tracing specific variables
maintenance show btrace pt -- Show Intel Processor Trace specific variables
maintenance show btrace pt skip-pad -- Show whether PAD packets should be skipped in the btrace packet history
maintenance show catch-demangler-crashes -- Show whether to attempt to catch demangler crashes
maintenance show demangler-warning -- Show what GDB does when demangler-warning is detected
maintenance show demangler-warning quit -- Show whether GDB will quit when an demangler-warning is detected
maintenance show dwarf -- Show DWARF specific variables
maintenance show dwarf always-disassemble -- Show whether `info address' always disassembles DWARF expressions
maintenance show dwarf max-cache-age -- Show the upper bound on the age of cached DWARF compilation units
maintenance show internal-error -- Show what GDB does when internal-error is detected
maintenance show internal-error corefile -- Show whether GDB will create a core file of GDB when internal-error is detected
maintenance show internal-error quit -- Show whether GDB will quit when an internal-error is detected
maintenance show internal-warning -- Show what GDB does when internal-warning is detected
maintenance show internal-warning corefile -- Show whether GDB will create a core file of GDB when internal-warning is detected
maintenance show internal-warning quit -- Show whether GDB will quit when an internal-warning is detected
maintenance show per-command -- Show per-command statistics settings
show per-command space -- Show whether to display per-command space usage
show per-command symtab -- Show whether to display per-command symtab statistics
show per-command time -- Show whether to display per-command execution time
maintenance show profile -- Show internal profiling
maintenance show show-debug-regs -- Show whether to show variables that mirror the x86 debug registers
maintenance show symbol-cache-size -- Show the size of the symbol cache
maintenance show target-async -- Show whether gdb controls the inferior in asynchronous mode
maintenance show target-non-stop -- Show whether gdb always controls the inferior in non-stop mode
maintenance space -- Set the display of space usage
maintenance time -- Set the display of time usage
maintenance translate-address -- Translate a section name and address to a symbol
maintenance undeprecate -- Undeprecate a command

Command class: obscure

checkpoint -- Fork a duplicate process (experimental)
compare-sections -- Compare section data on target to the exec file
compile -- Command to compile source code and inject it into the inferior
compile code -- Compile
compile file -- Evaluate a file containing source code
compile print -- Evaluate EXPR by using the compiler and print result
complete -- List the completions for the rest of the line as a command
expression -- Command to compile source code and inject it into the inferior
---Type <return> to continue, or q <return> to quit---
compile code -- Compile
compile file -- Evaluate a file containing source code
compile print -- Evaluate EXPR by using the compiler and print result
guile -- Evaluate a Guile expression
guile-repl -- Start a Guile interactive prompt
monitor -- Send a command to the remote monitor (remote targets only)
python -- Evaluate a Python command
python-interactive -- Start an interactive Python prompt
record -- Start recording
record btrace -- Start branch trace recording
record btrace bts -- Start branch trace recording in Branch Trace Store (BTS) format
record btrace pt -- Start branch trace recording in Intel Processor Trace format
record delete -- Delete the rest of execution log and start recording it anew
record full -- Start full execution recording
record full restore -- Restore the execution log from a file
record function-call-history -- Prints the execution history at function granularity
record goto -- Restore the program to its state at instruction number N
record goto begin -- Go to the beginning of the execution log
record goto end -- Go to the end of the execution log
record instruction-history -- Print disassembled instructions stored in the execution log
record save -- Save the execution log to a file
record stop -- Stop the record/replay target
restart -- Restart <n>: restore program context from a checkpoint
stop -- There is no `stop' command

Command class: running

advance -- Continue the program up to the given location (same form as args for break command)
attach -- Attach to a process or file outside of GDB
continue -- Continue program being debugged
detach -- Detach a process or file previously attached
detach checkpoint -- Detach from a checkpoint (experimental)
detach inferiors -- Detach from inferior ID (or list of IDS)
disconnect -- Disconnect from a target
finish -- Execute until selected stack frame returns
handle -- Specify how to handle signals
inferior -- Use this command to switch between inferiors
interrupt -- Interrupt the execution of the debugged program
jump -- Continue program being debugged at specified line or address
kill -- Kill execution of program being debugged
kill inferiors -- Kill inferior ID (or list of IDs)
next -- Step program
nexti -- Step one instruction
---Type <return> to continue, or q <return> to quit---
queue-signal -- Queue a signal to be delivered to the current thread when it is resumed
reverse-continue -- Continue program being debugged but run it in reverse
reverse-finish -- Execute backward until just before selected stack frame is called
reverse-next -- Step program backward
reverse-nexti -- Step backward one instruction
reverse-step -- Step program backward until it reaches the beginning of another source line
reverse-stepi -- Step backward exactly one instruction
run -- Start debugged program
signal -- Continue program with the specified signal
start -- Run the debugged program until the beginning of the main procedure
step -- Step program until it reaches a different source line
stepi -- Step one instruction exactly
target -- Connect to a target machine or process
target core -- Use a core file as a target
target ctf -- Use a CTF directory as a target
target exec -- Use an executable file as a target
target extended-remote -- Use a remote computer via a serial line
target native -- Native process (started by the "run" command)
target record -- Log program while executing and replay execution from log
target record-btrace -- Collect control-flow trace and provide the execution history
target record-core -- Log program while executing and replay execution from log
target record-full -- Log program while executing and replay execution from log
target remote -- Use a remote computer via a serial line
target tfile -- Use a trace file as a target
task -- Use this command to switch between Ada tasks
thread -- Use this command to switch between threads
thread apply -- Apply a command to a list of threads
thread apply all -- Apply a command to all threads
thread find -- Find threads that match a regular expression
thread name -- Set the current thread's name
until -- Execute until the program reaches a source line greater than the current

Command class: stack

backtrace -- Print backtrace of all stack frames
bt -- Print backtrace of all stack frames
down -- Select and print stack frame called by this one
frame -- Select and print a stack frame
return -- Make selected stack frame return to its caller
select-frame -- Select a stack frame without printing anything
up -- Select and print stack frame that called this one

Command class: status
---Type <return> to continue, or q <return> to quit---

info -- Generic command for showing things about the program being debugged
info address -- Describe where symbol SYM is stored
info all-registers -- List of all registers and their contents
info args -- Argument variables of current stack frame
info auto-load -- Print current status of auto-loaded files
info auto-load gdb-scripts -- Print the list of automatically loaded sequences of commands
info auto-load libthread-db -- Print the list of loaded inferior specific libthread_db
info auto-load local-gdbinit -- Print whether current directory .gdbinit file has been loaded
info auto-load python-scripts -- Print the list of automatically loaded Python scripts
info auto-load-scripts -- Print the list of automatically loaded Python scripts
info auxv -- Display the inferior's auxiliary vector
info bookmarks -- Status of user-settable bookmarks
info breakpoints -- Status of specified breakpoints (all user-settable breakpoints if no argument)
info checkpoints -- IDs of currently known checkpoints
info classes -- All Objective-C classes
info common -- Print out the values contained in a Fortran COMMON block
info copying -- Conditions for redistributing copies of GDB
info dcache -- Print information on the dcache performance
info display -- Expressions to display when program stops
info exceptions -- List all Ada exception names
info extensions -- All filename extensions associated with a source language
info files -- Names of targets and files being debugged
info float -- Print the status of the floating point unit
info frame -- All about selected stack frame
info frame-filter -- List all registered Python frame-filters
info functions -- All function names
info guile -- Prefix command for Guile info displays
info handle -- What debugger does when program gets various signals
info inferiors -- IDs of specified inferiors (all inferiors if no argument)
info line -- Core addresses of the code for a source line
info locals -- Local variables of current stack frame
info macro -- Show the definition of MACRO
info macros -- Show the definitions of all macros at LINESPEC
info mem -- Memory region attributes
info os -- Show OS data ARG
info pretty-printer -- GDB command to list all registered pretty-printers
info probes -- Show available static probes
info probes all -- Show information about all type of probes
info probes dtrace -- Show information about DTrace static probes
info probes stap -- Show information about SystemTap static probes
info proc -- Show /proc process information about any running process
info proc all -- List all available /proc info
---Type <return> to continue, or q <return> to quit---
info proc cmdline -- List command line arguments of the process
info proc cwd -- List current working directory of the process
info proc exe -- List absolute filename for executable of the process
info proc mappings -- List of mapped memory regions
info proc stat -- List process info from /proc/PID/stat
info proc status -- List process info from /proc/PID/status
info program -- Execution status of the program
info record -- Info record options
info registers -- List of integer registers and their contents
info scope -- List the variables local to a scope
info selectors -- All Objective-C selectors
info set -- Show all GDB settings
info sharedlibrary -- Status of loaded shared object libraries
info signals -- What debugger does when program gets various signals
info skip -- Display the status of skips
info source -- Information about the current source file
info sources -- Source files in the program
info stack -- Backtrace of the stack
info static-tracepoint-markers -- List target static tracepoints markers
info symbol -- Describe what symbol is at location ADDR
info target -- Names of targets and files being debugged
info tasks -- Provide information about all known Ada tasks
info terminal -- Print inferior's saved terminal status
info threads -- Display currently known threads
info tracepoints -- Status of specified tracepoints (all tracepoints if no argument)
info tvariables -- Status of trace state variables and their values
info type-printers -- GDB command to list all registered type-printers
info types -- All type names
info unwinder -- GDB command to list unwinders
info variables -- All global and static variable names
info vector -- Print the status of the vector unit
info vtbl -- Show the virtual function table for a C++ object
info warranty -- Various kinds of warranty you do not have
info watchpoints -- Status of specified watchpoints (all watchpoints if no argument)
info win -- List of all displayed windows
info xmethod -- GDB command to list registered xmethod matchers
macro -- Prefix for commands dealing with C preprocessor macros
macro define -- Define a new C/C++ preprocessor macro
macro expand -- Fully expand any C/C++ preprocessor macro invocations in EXPRESSION
macro expand-once -- Expand C/C++ preprocessor macro invocations appearing directly in EXPRESSION
macro list -- List all the macros defined using the `macro define' command
macro undef -- Remove the definition of the C/C++ preprocessor macro with the given name
show -- Generic command for showing things about the debugger
---Type <return> to continue, or q <return> to quit---
show ada -- Generic command for showing Ada-specific settings
show ada print-signatures -- Show whether the output of formal and return types for functions in the overloads selection menu is activated
show ada trust-PAD-over-XVS -- Show whether an optimization trusting PAD types over XVS types is activated
show agent -- Show debugger's willingness to use agent as a helper
show annotate -- Show annotation_level
show architecture -- Show architecture of target
show args -- Show argument list to give program being debugged when it is started
show auto-connect-native-target -- Show whether GDB may automatically connect to the native target
show auto-load -- Show auto-loading specific settings
show auto-load gdb-scripts -- Show whether auto-loading of canned sequences of commands scripts is enabled
show auto-load libthread-db -- Show whether auto-loading inferior specific libthread_db is enabled
show auto-load local-gdbinit -- Show whether auto-loading .gdbinit script in current directory is enabled
show auto-load python-scripts -- Show the debugger's behaviour regarding auto-loaded Python scripts
show auto-load safe-path -- Show the list of files and directories that are safe for auto-loading
show auto-load scripts-directory -- Show the list of directories from which to load auto-loaded scripts
show auto-load-scripts -- Show the debugger's behaviour regarding auto-loaded Python scripts
show auto-solib-add -- Show autoloading of shared library symbols
show backtrace -- Show backtrace specific variables
show backtrace limit -- Show the upper bound on the number of backtrace levels
show backtrace past-entry -- Show whether backtraces should continue past the entry point of a program
show backtrace past-main -- Show whether backtraces should continue past "main"
show basenames-may-differ -- Show whether a source file may have multiple base names
show breakpoint -- Breakpoint specific settings
show breakpoint always-inserted -- Show mode for inserting breakpoints
show breakpoint auto-hw -- Show automatic usage of hardware breakpoints
show breakpoint condition-evaluation -- Show mode of breakpoint condition evaluation
show breakpoint pending -- Show debugger's behavior regarding pending breakpoints
show can-use-hw-watchpoints -- Show debugger's willingness to use watchpoint hardware
show case-sensitive -- Show case sensitivity in name search
show charset -- Show the host and target character sets
show check -- Show the status of the type/range checker
show check range -- Show range checking
show check type -- Show strict type checking
show circular-trace-buffer -- Show target's use of circular trace buffer
show code-cache -- Show cache use for code segment access
show coerce-float-to-double -- Show coercion of floats to doubles when calling functions
show commands -- Show the history of commands you typed
show compile-args -- Show compile command GCC command-line arguments
show complaints -- Show max number of complaints about incorrect symbols
show configuration -- Show how GDB was configured at build time
show confirm -- Show whether to confirm potentially dangerous operations
show convenience -- Debugger convenience ("$foo") variables and functions
show copying -- Conditions for redistributing copies of GDB
---Type <return> to continue, or q <return> to quit---
show cp-abi -- Show the ABI used for inspecting C++ objects
show data-directory -- Show GDB's data directory
show dcache -- Show dcachesettings
show dcache line-size -- Show dcache line size
show dcache size -- Show number of dcache lines
show debug -- Generic command for showing gdb debugging flags
show debug arch -- Show architecture debugging
show debug auto-load -- Show auto-load verifications debugging
show debug bfd-cache -- Show bfd cache debugging
show debug check-physname -- Show cross-checking of "physname" code against demangler
show debug coff-pe-read -- Show coff PE read debugging
show debug compile -- Show compile command debugging
show debug displaced -- Show displaced stepping debugging
show debug dwarf-die -- Show debugging of the DWARF DIE reader
show debug dwarf-line -- Show debugging of the dwarf line reader
show debug dwarf-read -- Show debugging of the DWARF reader
show debug entry-values -- Show entry values and tail call frames debugging
show debug expression -- Show expression debugging
show debug frame -- Show frame debugging
show debug infrun -- Show inferior debugging
show debug jit -- Show JIT debugging
show debug libthread-db -- Show libthread-db debugging
show debug lin-lwp -- Show debugging of GNU/Linux lwp module
show debug linux-namespaces -- Show debugging of GNU/Linux namespaces module
show debug notification -- Show debugging of async remote notification
show debug observer -- Show observer debugging
show debug overload -- Show debugging of C++ overloading
show debug parser -- Show parser debugging
show debug py-unwind -- Show Python unwinder debugging
show debug record -- Show debugging of record/replay feature
show debug remote -- Show debugging of remote protocol
show debug serial -- Show serial debugging
show debug stap-expression -- Show SystemTap expression debugging
show debug symbol-lookup -- Show debugging of symbol lookup
show debug symfile -- Show debugging of the symfile functions
show debug symtab-create -- Show debugging of symbol table creation
show debug target -- Show target debugging
show debug timestamp -- Show timestamping of debugging messages
show debug varobj -- Show varobj debugging
show debug xml -- Show XML parser debugging
show debug-file-directory -- Show the directories where separate debug symbols are searched for
show default-collect -- Show the list of expressions to collect by default
show demangle-style -- Show the current C++ demangling style
---Type <return> to continue, or q <return> to quit---
show detach-on-fork -- Show whether gdb will detach the child of a fork
show directories -- Show the search path for finding source files
show disable-randomization -- Show disabling of debuggee's virtual address space randomization
show disassemble-next-line -- Show whether to disassemble next source line or insn when execution stops
show disassembly-flavor -- Show the disassembly flavor
show disconnected-dprintf -- Show whether dprintf continues after GDB disconnects
show disconnected-tracing -- Show whether tracing continues after GDB disconnects
show displaced-stepping -- Show debugger's willingness to use displaced stepping
show dprintf-channel -- Show the channel to use for dynamic printf
show dprintf-function -- Show the function to use for dynamic printf
show dprintf-style -- Show the style of usage for dynamic printf
show editing -- Show editing of command lines as they are typed
show endian -- Show endianness of target
show environment -- The environment to give the program
show exec-direction -- Show direction of execution (forward/reverse)
show exec-done-display -- Show notification of completion for asynchronous execution commands
show exec-wrapper -- Show the wrapper for running programs
show extended-prompt -- Show the extended prompt
show extension-language -- Show mapping between filename extension and source language
show filename-display -- Show how to display filenames
show follow-exec-mode -- Show debugger response to a program call of exec
show follow-fork-mode -- Show debugger response to a program call of fork or vfork
show frame-filter -- Prefix command for 'show' frame-filter related operations
show frame-filter priority -- GDB command to show the priority of the specified frame-filter
show gnutarget -- Show the current BFD target
show guile -- Prefix command for Guile preference settings
show guile print-stack -- Show the mode of Guile exception printing on error
show height -- Show number of lines in a page for GDB output pagination
show history -- Generic command for showing command history parameters
show history expansion -- Show history expansion on command input
show history filename -- Show the filename in which to record the command history
show history remove-duplicates -- Show how far back in history to look for and remove duplicate entries
show history save -- Show saving of the history record on exit
show history size -- Show the size of the command history
show host-charset -- Show the host character set
show inferior-tty -- Show terminal for future runs of program being debugged
show input-radix -- Show default input radix for entering numbers
show interactive-mode -- Show whether GDB's standard input is a terminal
show language -- Show the current source language
show libthread-db-search-path -- Show the current search path or libthread_db
show listsize -- Show number of source lines gdb will list by default
show logging -- Show logging options
show logging file -- Show the current logfile
---Type <return> to continue, or q <return> to quit---
show logging overwrite -- Show whether logging overwrites or appends to the log file
show logging redirect -- Show the logging output mode
show max-completions -- Show maximum number of completion candidates
show max-user-call-depth -- Show the max call depth for non-python/scheme user-defined commands
show max-value-size -- Show maximum sized value gdb will load from the inferior
show may-insert-breakpoints -- Show permission to insert breakpoints in the target
show may-insert-fast-tracepoints -- Show permission to insert fast tracepoints in the target
show may-insert-tracepoints -- Show permission to insert tracepoints in the target
show may-interrupt -- Show permission to interrupt or signal the target
show may-write-memory -- Show permission to write into target memory
show may-write-registers -- Show permission to write into registers
show mem -- Memory regions settings
show mem  inaccessible-by-default -- Show handling of unknown memory regions
show mi-async -- Show whether MI asynchronous mode is enabled
show mpx -- Show Intel Memory Protection Extensions specific variables
show mpx bound -- Show the memory bounds for a given array/pointer storage in the bound table
show multiple-symbols -- Show how the debugger handles ambiguities in expressions
show non-stop -- Show whether gdb controls the inferior in non-stop mode
show observer -- Show whether gdb controls the inferior in observer mode
show opaque-type-resolution -- Show resolution of opaque struct/class/union types (if set before loading symbols)
show osabi -- Show OS ABI of target
show output-radix -- Show default output radix for printing of values
show overload-resolution -- Show overload resolution in evaluating C++ functions
show pagination -- Show state of GDB output pagination
show paths -- Current search path for finding object files
show print -- Generic command for showing print settings
show print address -- Show printing of addresses
show print array -- Show pretty formatting of arrays
show print array-indexes -- Show printing of array indexes
show print asm-demangle -- Show demangling of C++/ObjC names in disassembly listings
show print demangle -- Show demangling of encoded C++/ObjC names when displaying symbols
show print elements -- Show limit on string chars or array elements to print
show print entry-values -- Show printing of function arguments at function entry
show print frame-arguments -- Show printing of non-scalar frame arguments
show print inferior-events -- Show printing of inferior events (e.g.
show print max-symbolic-offset -- Show the largest offset that will be printed in <symbol+1234> form
show print null-stop -- Show printing of char arrays to stop at first null char
show print object -- Show printing of object's derived type based on vtable info
show print pascal_static-members -- Show printing of pascal static members
show print pretty -- Show pretty formatting of structures
show print raw -- Generic command for showing "print raw" settings
show print raw frame-arguments -- Show whether to print frame arguments in raw form
show print repeats -- Show threshold for repeated print elements
---Type <return> to continue, or q <return> to quit---
show print sevenbit-strings -- Show printing of 8-bit characters in strings as \nnn
show print static-members -- Show printing of C++ static members
show print symbol -- Show printing of symbol names when printing pointers
show print symbol-filename -- Show printing of source filename and line number with <symbol>
show print symbol-loading -- Show printing of symbol loading messages
show print thread-events -- Show printing of thread events (such as thread start and exit)
show print type -- Generic command for showing type-printing settings
show print type methods -- Show printing of methods defined in classes
show print type typedefs -- Show printing of typedefs defined in classes
show print union -- Show printing of unions interior to structures
show print vtbl -- Show printing of C++ virtual function tables
show prompt -- Show gdb's prompt
show python -- Prefix command for python preference settings
show python print-stack -- Show the mode of Python stack printing on error
show radix -- Show the default input and output number radices
show range-stepping -- Show whether target-assisted range stepping is enabled
show record -- Show record options
show record btrace -- Show record options
show record btrace bts -- Show record btrace bts options
show record btrace bts buffer-size -- Show the record/replay bts buffer size
show record btrace pt -- Show record btrace pt options
show record btrace pt buffer-size -- Show the record/replay pt buffer size
show record btrace replay-memory-access -- Show what memory accesses are allowed during replay
show record full -- Show record options
show record full insn-number-max -- Show record/replay buffer limit
show record full memory-query -- Show whether query if PREC cannot record memory change of next instruction
show record full stop-at-limit -- Show whether record/replay stops when record/replay buffer becomes full
show record function-call-history-size -- Show number of functions to print in "record function-call-history"
show record instruction-history-size -- Show number of instructions to print in "record instruction-history"
show remote -- Remote protocol specific variables
show remote P-packet -- Show current use of remote protocol `P' (set-register) packet
show remote TracepointSource-packet -- Show current use of remote protocol `TracepointSource' (TracepointSource) packet
show remote X-packet -- Show current use of remote protocol `X' (binary-download) packet
show remote Z-packet -- Show use of remote protocol `Z' packets
show remote access-watchpoint-packet -- Show current use of remote protocol `Z4' (access-watchpoint) packet
show remote agent-packet -- Show current use of remote protocol `QAgent' (agent) packet
show remote allow-packet -- Show current use of remote protocol `QAllow' (allow) packet
show remote attach-packet -- Show current use of remote protocol `vAttach' (attach) packet
show remote binary-download-packet -- Show current use of remote protocol `X' (binary-download) packet
show remote breakpoint-commands-packet -- Show current use of remote protocol `BreakpointCommands' (breakpoint-commands) packet
show remote btrace-conf-bts-size-packet -- Show current use of remote protocol `Qbtrace-conf:bts:size' (btrace-conf-bts-size) packet
show remote btrace-conf-pt-size-packet -- Show current use of remote protocol `Qbtrace-conf:pt:size' (btrace-conf-pt-size) packet
show remote catch-syscalls-packet -- Show current use of remote protocol `QCatchSyscalls' (catch-syscalls) packet
---Type <return> to continue, or q <return> to quit---
show remote conditional-breakpoints-packet -- Show current use of remote protocol `ConditionalBreakpoints' (conditional-breakpoints) packet
show remote conditional-tracepoints-packet -- Show current use of remote protocol `ConditionalTracepoints' (conditional-tracepoints) packet
show remote ctrl-c-packet -- Show current use of remote protocol `vCtrlC' (ctrl-c) packet
show remote disable-btrace-packet -- Show current use of remote protocol `Qbtrace:off' (disable-btrace) packet
show remote disable-randomization-packet -- Show current use of remote protocol `QDisableRandomization' (disable-randomization) packet
show remote enable-btrace-bts-packet -- Show current use of remote protocol `Qbtrace:bts' (enable-btrace-bts) packet
show remote enable-btrace-pt-packet -- Show current use of remote protocol `Qbtrace:pt' (enable-btrace-pt) packet
show remote exec-event-feature-packet -- Show current use of remote protocol `exec-event-feature' (exec-event-feature) packet
show remote exec-file -- Show the remote pathname for "run"
show remote fast-tracepoints-packet -- Show current use of remote protocol `FastTracepoints' (fast-tracepoints) packet
show remote fetch-register-packet -- Show current use of remote protocol `p' (fetch-register) packet
show remote fork-event-feature-packet -- Show current use of remote protocol `fork-event-feature' (fork-event-feature) packet
show remote get-thread-information-block-address-packet -- Show current use of remote protocol `qGetTIBAddr' (get-thread-information-block-address) packet
show remote get-thread-local-storage-address-packet -- Show current use of remote protocol `qGetTLSAddr' (get-thread-local-storage-address) packet
show remote hardware-breakpoint-limit -- Show the maximum number of target hardware breakpoints
show remote hardware-breakpoint-packet -- Show current use of remote protocol `Z1' (hardware-breakpoint) packet
show remote hardware-watchpoint-length-limit -- Show the maximum length (in bytes) of a target hardware watchpoint
show remote hardware-watchpoint-limit -- Show the maximum number of target hardware watchpoints
show remote hostio-close-packet -- Show current use of remote protocol `vFile:close' (hostio-close) packet
show remote hostio-fstat-packet -- Show current use of remote protocol `vFile:fstat' (hostio-fstat) packet
show remote hostio-open-packet -- Show current use of remote protocol `vFile:open' (hostio-open) packet
show remote hostio-pread-packet -- Show current use of remote protocol `vFile:pread' (hostio-pread) packet
show remote hostio-pwrite-packet -- Show current use of remote protocol `vFile:pwrite' (hostio-pwrite) packet
show remote hostio-readlink-packet -- Show current use of remote protocol `vFile:readlink' (hostio-readlink) packet
show remote hostio-setfs-packet -- Show current use of remote protocol `vFile:setfs' (hostio-setfs) packet
show remote hostio-unlink-packet -- Show current use of remote protocol `vFile:unlink' (hostio-unlink) packet
show remote hwbreak-feature-packet -- Show current use of remote protocol `hwbreak-feature' (hwbreak-feature) packet
show remote install-in-trace-packet -- Show current use of remote protocol `InstallInTrace' (install-in-trace) packet
show remote interrupt-on-connect --             Show whether interrupt-sequence is sent to remote target when gdb connects to
show remote interrupt-sequence -- Show interrupt sequence to remote target
show remote kill-packet -- Show current use of remote protocol `vKill' (kill) packet
show remote library-info-packet -- Show current use of remote protocol `qXfer:libraries:read' (library-info) packet
show remote library-info-svr4-packet -- Show current use of remote protocol `qXfer:libraries-svr4:read' (library-info-svr4) packet
show remote memory-map-packet -- Show current use of remote protocol `qXfer:memory-map:read' (memory-map) packet
show remote memory-read-packet-size -- Show the maximum number of bytes per memory-read packet
show remote memory-write-packet-size -- Show the maximum number of bytes per memory-write packet
show remote multiprocess-feature-packet -- Show current use of remote protocol `multiprocess-feature' (multiprocess-feature) packet
show remote no-resumed-stop-reply-packet -- Show current use of remote protocol `N stop reply' (no-resumed-stop-reply) packet
show remote noack-packet -- Show current use of remote protocol `QStartNoAckMode' (noack) packet
show remote osdata-packet -- Show current use of remote protocol `qXfer:osdata:read' (osdata) packet
show remote p-packet -- Show current use of remote protocol `p' (fetch-register) packet
---Type <return> to continue, or q <return> to quit---
show remote pass-signals-packet -- Show current use of remote protocol `QPassSignals' (pass-signals) packet
show remote pid-to-exec-file-packet -- Show current use of remote protocol `qXfer:exec-file:read' (pid-to-exec-file) packet
show remote program-signals-packet -- Show current use of remote protocol `QProgramSignals' (program-signals) packet
show remote query-attached-packet -- Show current use of remote protocol `qAttached' (query-attached) packet
show remote read-aux-vector-packet -- Show current use of remote protocol `qXfer:auxv:read' (read-aux-vector) packet
show remote read-btrace-conf-packet -- Show current use of remote protocol `qXfer:btrace-conf' (read-btrace-conf) packet
show remote read-btrace-packet -- Show current use of remote protocol `qXfer:btrace' (read-btrace) packet
show remote read-fdpic-loadmap-packet -- Show current use of remote protocol `qXfer:fdpic:read' (read-fdpic-loadmap) packet
show remote read-sdata-object-packet -- Show current use of remote protocol `qXfer:statictrace:read' (read-sdata-object) packet
show remote read-siginfo-object-packet -- Show current use of remote protocol `qXfer:siginfo:read' (read-siginfo-object) packet
show remote read-spu-object-packet -- Show current use of remote protocol `qXfer:spu:read' (read-spu-object) packet
show remote read-watchpoint-packet -- Show current use of remote protocol `Z3' (read-watchpoint) packet
show remote reverse-continue-packet -- Show current use of remote protocol `bc' (reverse-continue) packet
show remote reverse-step-packet -- Show current use of remote protocol `bs' (reverse-step) packet
show remote run-packet -- Show current use of remote protocol `vRun' (run) packet
show remote search-memory-packet -- Show current use of remote protocol `qSearch:memory' (search-memory) packet
show remote set-register-packet -- Show current use of remote protocol `P' (set-register) packet
show remote software-breakpoint-packet -- Show current use of remote protocol `Z0' (software-breakpoint) packet
show remote static-tracepoints-packet -- Show current use of remote protocol `StaticTracepoints' (static-tracepoints) packet
show remote supported-packets-packet -- Show current use of remote protocol `qSupported' (supported-packets) packet
show remote swbreak-feature-packet -- Show current use of remote protocol `swbreak-feature' (swbreak-feature) packet
show remote symbol-lookup-packet -- Show current use of remote protocol `qSymbol' (symbol-lookup) packet
show remote system-call-allowed -- Show if the host system(3) call is allowed for the target
show remote target-features-packet -- Show current use of remote protocol `qXfer:features:read' (target-features) packet
show remote thread-events-packet -- Show current use of remote protocol `QThreadEvents' (thread-events) packet
show remote threads-packet -- Show current use of remote protocol `qXfer:threads:read' (threads) packet
show remote trace-buffer-size-packet -- Show current use of remote protocol `QTBuffer:size' (trace-buffer-size) packet
show remote trace-status-packet -- Show current use of remote protocol `qTStatus' (trace-status) packet
show remote traceframe-info-packet -- Show current use of remote protocol `qXfer:traceframe-info:read' (traceframe-info) packet
show remote unwind-info-block-packet -- Show current use of remote protocol `qXfer:uib:read' (unwind-info-block) packet
show remote verbose-resume-packet -- Show current use of remote protocol `vCont' (verbose-resume) packet
show remote verbose-resume-supported-packet -- Show current use of remote protocol `vContSupported' (verbose-resume-supported) packet
show remote vfork-event-feature-packet -- Show current use of remote protocol `vfork-event-feature' (vfork-event-feature) packet
show remote write-siginfo-object-packet -- Show current use of remote protocol `qXfer:siginfo:write' (write-siginfo-object) packet
show remote write-spu-object-packet -- Show current use of remote protocol `qXfer:spu:write' (write-spu-object) packet
show remote write-watchpoint-packet -- Show current use of remote protocol `Z2' (write-watchpoint) packet
show remoteaddresssize -- Show the maximum size of the address (in bits) in a memory packet
show remotebreak -- Show whether to send break if interrupted
show remotecache -- Show cache use for remote targets
show remoteflow -- Show use of hardware flow control for remote serial I/O
show remotelogbase -- Show numerical base for remote session logging
show remotelogfile -- Show filename for remote session recording
show remotetimeout -- Show timeout limit to wait for target to respond
---Type <return> to continue, or q <return> to quit---
show remotewritesize -- Show the maximum number of bytes per memory write packet (deprecated)
show schedule-multiple -- Show mode for resuming threads of all processes
show scheduler-locking -- Show mode for locking scheduler during execution
show script-extension -- Show mode for script filename extension recognition
show serial -- Show default serial/parallel port configuration
show serial baud -- Show baud rate for remote serial I/O
show serial parity -- Show parity for remote serial I/O
show solib-absolute-prefix -- Show the current system root
show solib-search-path -- Show the search path for loading non-absolute shared library symbol files
show stack-cache -- Show cache use for stack access
show startup-with-shell -- Show use of shell to start subprocesses
show step-mode -- Show mode of the step operation
show stop-on-solib-events -- Show stopping for shared library events
show struct-convention -- Show the convention for returning small structs
show substitute-path -- Usage: show substitute-path [FROM]
show sysroot -- Show the current system root
show target-async -- Show whether MI asynchronous mode is enabled
show target-charset -- Show the target character set
show target-file-system-kind -- Show assumed file system kind for target reported file names
show target-wide-charset -- Show the target wide character set
show tcp -- TCP protocol specific variables
show tcp auto-retry -- Show auto-retry on socket connect
show tcp connect-timeout -- Show timeout limit in seconds for socket connection
show tdesc -- Show target description specific variables
show tdesc filename -- Show the file to read for an XML target description
show trace-buffer-size -- Show requested size of trace buffer
show trace-commands -- Show state of GDB CLI command tracing
show trace-notes -- Show the notes string to use for current and future trace runs
show trace-stop-notes -- Show the notes string to use for future tstop commands
show trace-user -- Show the user name to use for current and future trace runs
show trust-readonly-sections -- Show mode for reading from readonly sections
show tui -- TUI configuration variables
show tui active-border-mode -- Show the attribute mode to use for the active TUI window border
show tui border-kind -- Show the kind of border for TUI windows
show tui border-mode -- Show the attribute mode to use for the TUI window borders
show unwind-on-terminating-exception -- Show unwinding of stack if std::terminate() is called while in a call dummy
show unwindonsignal -- Show unwinding of stack if a signal is received while in a call dummy
show use-coredump-filter -- Show whether gcore should consider /proc/PID/coredump_filter
show use-deprecated-index-sections -- Show whether to use deprecated gdb_index sections
show user -- Show definitions of non-python/scheme user defined commands
show values -- Elements of value history around item number IDX (or last ten)
show verbose -- Show verbosity
show version -- Show what version of GDB this is
---Type <return> to continue, or q <return> to quit---
show warranty -- Various kinds of warranty you do not have
show watchdog -- Show watchdog timer
show width -- Show number of characters where GDB should wrap lines of its output
show write -- Show writing into executable and core files

Command class: support

! -- Execute the rest of the line as a shell command
add-auto-load-safe-path -- Add entries to the list of directories from which it is safe to auto-load files
add-auto-load-scripts-directory -- Add entries to the list of directories from which to load auto-loaded scripts
alias -- Define a new command that is an alias of an existing command
apropos -- Search for commands matching a REGEXP
define -- Define a new command name
demangle -- Demangle a mangled name
document -- Document a user-defined command
dont-repeat -- Don't repeat this command
down-silently -- Same as the `down' command
echo -- Print a constant string
help -- Print list of commands
if -- Execute nested commands once IF the conditional expression is non zero
interpreter-exec -- Execute a command in an interpreter
make -- Run the ``make'' program using the rest of the line as arguments
overlay -- Commands for debugging overlays
overlay auto -- Enable automatic overlay debugging
overlay list-overlays -- List mappings of overlay sections
overlay load-target -- Read the overlay mapping state from the target
overlay manual -- Enable overlay debugging
overlay map-overlay -- Assert that an overlay section is mapped
overlay off -- Disable overlay debugging
overlay unmap-overlay -- Assert that an overlay section is unmapped
quit -- Exit gdb
shell -- Execute the rest of the line as a shell command
source -- Read commands from a file named FILE
up-silently -- Same as the `up' command
while -- Execute nested commands WHILE the conditional expression is non zero

Command class: tracepoints

actions -- Specify the actions to be taken at a tracepoint
collect -- Specify one or more data items to be collected at a tracepoint
end -- Ends a list of commands or actions
passcount -- Set the passcount for a tracepoint
save-tracepoints -- Save current tracepoint definitions as a script
---Type <return> to continue, or q <return> to quit---
tdump -- Print everything collected at the current tracepoint
teval -- Specify one or more expressions to be evaluated at a tracepoint
tfind -- Select a trace frame;
tfind end -- De-select any trace frame and resume 'live' debugging
tfind line -- Select a trace frame by source line
tfind none -- De-select any trace frame and resume 'live' debugging
tfind outside -- Select a trace frame whose PC is outside the given range (exclusive)
tfind pc -- Select a trace frame by PC
tfind range -- Select a trace frame whose PC is in the given range (inclusive)
tfind start -- Select the first trace frame in the trace buffer
tfind tracepoint -- Select a trace frame by tracepoint number
tsave -- Save the trace data to a file
tstart -- Start trace data collection
tstatus -- Display the status of the current trace data collection
tstop -- Stop trace data collection
tvariable -- Define a trace state variable
while-stepping -- Specify single-stepping behavior at a tracepoint

Command class: user-defined

boot -- User-defined
ef -- User-defined
public -- User-defined
qpl -- User-defined
test -- User-defined

Unclassified commands

add-inferior -- Add a new inferior
clone-inferior -- Clone inferior ID
eval -- Convert "printf format string"
function -- Placeholder command for showing help on convenience functions
function _any_caller_is -- Check all calling function's names
function _any_caller_matches -- Compare all calling function's names with a regexp
function _caller_is -- Check the calling function's name
function _caller_matches -- Compare the calling function's name with a regexp
function _isvoid -- Check whether an expression is void
function _memeq -- $_memeq - compare bytes of memory
function _regex -- $_regex - check if a string matches a regular expression
function _streq -- $_streq - check string equality
function _strlen -- $_strlen - compute string length
jit-reader-load -- Load FILE as debug info reader and unwinder for JIT compiled code
jit-reader-unload -- Unload the currently loaded JIT debug info reader
---Type <return> to continue, or q <return> to quit---
remove-inferiors -- Remove inferior ID (or list of IDs)
unset -- Complement to certain "set" commands
unset environment -- Cancel environment variable VAR for the program
unset exec-wrapper -- Disable use of an execution wrapper
unset substitute-path -- Usage: unset substitute-path [FROM]
unset tdesc -- Unset target description specific variables
unset tdesc filename -- Unset the file to read for an XML target description
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)
(gdb)


