/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- if((prolog_load_context(source,F),prolog_load_context(file,F))).
:- else.
%module(_,Y):- maplist(export,Y).
:- endif.
:- module(dumpst,[
          getPFA/3,getPFA1/3,getPFA2/3,get_m_opt/4,fdmsg/1,fdmsg1/1,
          neg1_numbervars/3,clauseST/2,
          dtrace/0,dbreak/0,
          dtrace/1,dtrace/2,
          dumptrace/1,dumptrace/2,dumptrace0/1,dumptrace1/1,
          dumptrace_ret/1,
          drain_framelist/1,
          drain_framelist_ele/1,
          printable_variable_name/2,
          v_name1/2,
          v_name2/2,
          dump_st/0,
          with_source_module/1,
          to_wmsg/2,
          fmsg_rout/1,
          simplify_goal_printed/2,
          dumpST/0,dumpST/1,dumpST1/0,
          dumpST0/0,dumpST0/1,dumpST0/2,
          dumpST9/0,dumpST9/1,dumpST9/2,dumpST_now/2,printFrame/3,frame_to_fmsg/4

   ]).

:-  meta_predicate dumptrace_ret(?),
  neg1_numbervars(?, ?, 0),
  with_source_module(0),
  dumptrace_ret(0),
  dumptrace0(0),
  dumptrace1(0),
  dumptrace(0),
  dtrace(*,0).

:- set_module(class(library)).
% % % OFF :- system:use_module(library(apply)).
% % % OFF :- system:use_module(library(logicmoo/util_strings)).
% % % OFF :- system:use_module(library(logicmoo_utils_all)).
% % % OFF :- system:use_module((ucatch)).
% % % OFF :- system:use_module(library(logicmoo/no_loops)).
% % % OFF :- system:use_module((rtrace)).
% % % OFF :- system:use_module(library(must_sanity)).

:- use_module(library(backcomp)).
:- use_module(library(debug)).
:- use_module(library(occurs)).
:- use_module(library(check)).
:- use_module(library(edinburgh)).
:- use_module(library(prolog_stack)).
:- use_module(library(make)).



:- use_module(library(logicmoo_startup)).
:- use_module(library(logicmoo_common)).
%:- use_module(library(debuggery/first)).
:- use_module(library(logicmoo/util_strings)).
%:- use_module(library(debuggery/dmsg)).
:- use_module(library(debuggery/rtrace)).
%:- use_module(library(debuggery/bugger)).
%:- use_module(library(debuggery/dumpst)).
%:- use_module(library(debuggery/ucatch)).
:- use_module(library(debuggery/frames)).


:- set_prolog_flag(backtrace_depth,      200).
:- set_prolog_flag(backtrace_goal_depth, 20).
:- set_prolog_flag(backtrace_show_lines, true).

:- module_transparent
          getPFA/3,getPFA1/3,getPFA2/3,get_m_opt/4,fdmsg/1,fdmsg1/1,
          neg1_numbervars/3,clauseST/2,
          % dtrace/0,
          dtrace/1,dtrace/2,
          dumptrace/1,dumptrace/2,
          dumptrace_ret/1,
          dump_st/0,
          dumpST/0,dumpST/1,
          dumpST0/0,dumpST0/1,dumpST0/2,
          dumpST9/0,dumpST9/1,dumpST9/2.



%:- ensure_loaded(library(debug)).
% % % OFF :- system:use_module((dmsg)).% WAS OFF  :- system:use_module(library(logicmoo_util_strings)).


%=

%% dump_st is semidet.
%
% Dump Stack Trace.
%
dump_st:- prolog_current_frame(Current),dumpST0(Current,100),!.


%=

%% dumpST0 is semidet.
%
% Dump S True Stucture Primary Helper.
%
dumpST0:- prolog_current_frame(Current),
  dbreak,(tracing->zotrace((CU=dtrace,notrace));CU=true),dumpST0(Current,800),!,CU.

%=

%% dumpST0( ?Opts) is semidet.
%
% Dump S True Stucture Primary Helper.
%
dumpST0(Opts):- prolog_current_frame(Current), once(nb_current('$dump_frame',Frame);Frame=Current),dumpST0(Frame,Opts).

%=

%% dumpST0( ?Frame, ?MaxDepth) is semidet.
%
% Dump S True Stucture Primary Helper.
%
:- thread_local(tlbugger:ifHideTrace/0).
dumpST0(_,_):- tlbugger:ifHideTrace,!.
dumpST0(Frame,MaxDepth):-  prolog_current_frame(Current),(number(Frame);get_dump_frame(Current,Frame)),!,
   ignore(MaxDepth=5000),Term = dumpST(MaxDepth),!,
   ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(13)]),
    format(user_error, '% dumpST ~p', [Term]), nl(user_error),
    attach_console,dtrace,
    dbreak,

    print_prolog_backtrace(user_error, Trace,[subgoal_positions(true)]), nl(user_error), fail)),!.



% dumpstack_arguments.

%=

%% dumpST is semidet.
%
% Dump S True Stucture.
%

%:- use_module(library(sandbox)).
%sandbox:safe_primitive(dumpST/0).

dumpST:- ((prolog_current_frame(Current),get_dump_frame(Current,Frame),
  no_bfly(zotrace(with_all_dmsg((b_setval('$dump_frame',Frame),dumpST1)))))).

%no_bfly(Goal):- current_predicate(in_bfly/2)->in_bfly(f,Goal);call(Goal).

:- thread_local(tlbugger:no_slow_io/0).
:- multifile(tlbugger:no_slow_io/0).

%=

%% dumpST1 is semidet.
%
% Dump S True Stucture Secondary Helper.
%
dumpST1:- current_prolog_flag(dmsg_level,never),!.
dumpST1:- tlbugger:no_slow_io,!,dumpST0,!.
dumpST1:- tlbugger:ifHideTrace,!.
dumpST1:- show_current_source_location,!,loop_check_early(dumpST9,dumpST0).

%=

%% dumpST( ?Depth) is semidet.
%
% Dump S True Stucture.
%
dumpST(Depth):-
   prolog_current_frame(Current),
   no_bfly((zotrace((b_setval('$dump_frame',Current))),
   loop_check_early(dumpST9(Depth),dumpST0(Depth)))).


%=

%% get_m_opt( ?Opts, ?Max_depth, ?D100, ?RetVal) is semidet.
%
% Get Module Opt.
%
get_m_opt(Opts,Max_depth,D100,RetVal):- univ_safe_2(E,[Max_depth,V]),(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).



%=

%% dumpST9 is semidet.
%
% Dump S T9.
%
dumpST9:- prolog_current_frame(Current),get_dump_frame(Current,Frame),dumpST9(Frame,5000).

%=

%% dumpST9( ?Depth) is semidet.
%
% Dump S T9.
%
dumpST9(Depth):-prolog_current_frame(Current),get_dump_frame(Current,Frame),dumpST9(Frame,Depth).


get_dump_frame(Current,Frame):- (nb_current('$dump_frame',Frame),number(Frame))->true;Frame=Current.

%=

%% dumpST9( ?Frame, :TermMaxDepth) is semidet.
%
% Dump S T9.
%
dumpST9(_,_):- tlbugger:ifHideTrace,!.
dumpST9(Frame,MaxDepth):- integer(MaxDepth),!,dumpST_now(Frame,[max_depth(MaxDepth),numbervars(true),show([level,has_alternatives,hidden,context_module,goal,clause])]).
dumpST9(Frame,From-MaxDepth):- integer(MaxDepth),!,dumpST_now(Frame,[skip_depth(From),max_depth(MaxDepth),numbervars(true),show([level,has_alternatives,hidden,context_module,goal,clause])]).
dumpST9(Frame,List):- is_list(List),dumpST_now(Frame,[show([level,has_alternatives,hidden,context_module,goal,clause])|List]).



%=

%% drain_framelist( ?Opts) is semidet.
%
% Drain Framelist.
%
drain_framelist(Opts):- repeat, \+ drain_framelist_ele(Opts).


%=

%% drain_framelist_ele( ?Opts) is semidet.
%
% Drain Framelist Ele.
%
drain_framelist_ele(Opts):-
    nb_getval('$current_stack_frame_list',[N-Frame|Next]),
    nb_setval('$current_stack_frame_list',Next),!,
    printFrame(N,Frame,Opts),!.




%=

%% dumpST_now( ?FrameIn, ?Opts) is semidet.
%
% Dump S True Stucture Now.
%
dumpST_now(FrameInto,Opts):-
   prolog_current_frame(Current),
  (number(FrameInto) -> FrameIn=FrameInto ; FrameIn=Current),
   nb_setval('$hide_rest_frames',false),
   b_setval('$current_stack_frame_depth',0),
   b_setval('$current_stack_frame_list',[]),
   get_m_opt(Opts,max_depth,100,MD),
   b_setval('$current_stack_frame_handle',FrameIn),
  (repeat,
     nb_getval('$current_stack_frame_depth',N),
     nb_getval('$current_stack_frame_handle',Frame),
    ((pushFrame(N,Frame,Opts),MD>N)->
     ((getPFA2(Frame,parent,ParentFrame)->
       (nb_setval('$current_stack_frame_handle',ParentFrame),
       NN is N +1,nb_setval('$current_stack_frame_depth',NN),fail); !));
     (!))),
   drain_framelist(Opts),!.



%% pushFrame( ?N, ?Frame, ?Opts) is semidet.
%
% Push Frame.
%
pushFrame(N,Frame,_Opts):- nb_getval('$current_stack_frame_list',Current),nb_setval('$current_stack_frame_list',[N-Frame|Current]).


%=

%% printFrame( ?N, ?Frame, ?Opts) is semidet.
%
% Print Frame.
%
printFrame(_,_,_):- nb_current('$hide_rest_frames',true),!.
printFrame(N,Frame,Opts):-
  ignore(((frame_to_fmsg(N,Frame,Opts,Out)),must(fmsg_rout(Out)))),!.


%=

%% frame_to_fmsg( ?N, ?Frame, ?Opts, ?N) is semidet.
%
% Frame Converted To Functor Message.
%
frame_to_fmsg(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!,fail.
%  dumpST9(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,skip_depth,100,SD),N=<SD,!.
frame_to_fmsg(_,Frame,Opts,[fr(Goal)]):- get_m_opt(Opts,show,goal,Ctrl),getPFA(Frame,Ctrl,Goal),!.
frame_to_fmsg(N,Frame,Opts,[nf(no(Ctrl),N,Frame,Opts)]):- get_m_opt(Opts,show,goal,Ctrl),!.
frame_to_fmsg(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).




%=

%% fmsg_rout( :TermRROut) is semidet.
%
% Functor Message Rout.
%
fmsg_rout([]):-!.
fmsg_rout([fr(E)|_]):- member(goal=GG,E),end_dump(GG),!,ignore(fdmsg(fr(E))),!.
fmsg_rout([fr(E)|_]):- member(goal=GG,E),end_dump(GG),!,ignore(fdmsg(fr(E))),!.
fmsg_rout([E|RROut]):- ignore(fdmsg(E)),!,fmsg_rout(RROut).
fmsg_rout(RROut):- show_call(why,forall(member(E,RROut),fdmsg(E))),!.


%=

%% neg1_numbervars( ?Out, ?Start, :GoalROut) is semidet.
%
% Negated Secondary Helper Numbervars.
%
neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,false,Out):-!.
neg1_numbervars(Out,true,ROut):-copy_term(Out,ROut),!,snumbervars(ROut,777,_).
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),!,snumbervars(ROut,Start,_).
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),safe_numbervars(ROut).

if_defined_mesg_color(G,C):- current_predicate(mesg_color/2),mesg_color(G,C).

%=

%% fdmsg1( ?G) is semidet.
%
% Fdmsg Secondary Helper.
%
fdmsg1(txt(S)):-'format'(S,[]),!.
fdmsg1(level=L):-'format'('(~q)',[L]),!.
fdmsg1(context_module=G):- simplify_m(G,M),!,if_defined_mesg_color(G,Ctrl),ansicall(Ctrl,format('[~w]',[M])),!.
fdmsg1(has_alternatives=G):- (G==(false)->true;'format'('<*>',[])),!.
fdmsg1(hidden=G):- (G==(false)->true;'format'('$',[])),!.
fdmsg1(goal=G):- do_fdmsg1(G).
fdmsg1(clause=[F,L]):- directory_file_path(_,FF,F),'format'('  %  ~w:~w: ',[FF,L]),!.
fdmsg1(clause=[F,L]):- fresh_line,'format'('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-'format'(' /*DYN*/ ',[]),!.
fdmsg1(G):- if_defined_mesg_color(G,Ctrl),ansicall(Ctrl,fmt_gg(G)),!.
fdmsg1(M):-dmsg(failed_fdmsg1(M)).

do_fdmsg1(G):-
  simplify_goal_printed(G,GG),!,
  (GG\==G->write('#');true),
  do_fdmsg2(GG),!.

term_contains_ansi_b(S,N):- compound(S), !, arg(_,S,E),term_contains_ansi_b(E,N),!.
term_contains_ansi_b(S,S):- string(S),!,sub_string(S,_,_,_,'\x1B').
term_contains_ansi_b(S,S):- atom(S),!,sub_string(S,_,_,_,'\x1B').

%do_fdmsg2(GG):- term_contains_ansi_b(GG,_),pt(GG),!.
do_fdmsg2(GG):- term_contains_ansi_b(GG,_),write(GG),!.
%do_fdmsg2(GG):- term_contains_ansi_b(GG,N),write(N),fail.
do_fdmsg2(GG):-
  term_variables(GG,_Vars),
  copy_term_nat(GG,GGG), =(GG,GGG),
  numbervars(GGG,0,_,[attvar(skip)]),
  if_defined_mesg_color(GGG,Ctrl),ansicall(Ctrl,fmt_gg(GGG)),!.

fmt_gg(GGG):- term_contains_ansi_b(GGG,_),!,write(' '),write(GGG),write('. ').
fmt_gg(GGG):- format(' ~q. ',[GGG]).

%=

%% simplify_m( ?G, ?M) is semidet.
%
% Simplify Module.
%

% simplify_m(G,M):-atom(G),sub_atom(G,_,6,0,M),!.
simplify_m(G,G).

%=

%% fdmsg( ?M) is semidet.
%
% Fdmsg.
%
fdmsg(fr(List)):-is_list(List),!,must((fresh_line,ignore(forall(member(E,List),fdmsg1(E))),nl)).
fdmsg(M):- logicmoo_util_catch:ddmsg(failed_fdmsg(M)).

:- thread_local(tlbugger:plain_attvars/0).

:-export(simplify_goal_printed/2).


printable_variable_name(Var, Name) :- nonvar(Name),!,must(printable_variable_name(Var, NameO)),!,Name=NameO.
printable_variable_name(Var, Name) :- nonvar(Var),Var='$VAR'(Named), (nonvar(Named)-> Name=Named ; format(atom(Name),"~w_",[Var])).
printable_variable_name(Var, Name) :- nonvar(Var),format(atom(Name),"(_~q_)",[Var]).
printable_variable_name(Var,Name):- (get_attr(Var, vn, Name1);
  get_attr(Var, varnames, Name1)),
 (var_property(Var,name(Name2))->
   (Name1==Name2-> atom_concat(Name1,'_VN',Name) ; Name=(Name1:Name2));
    (atom(Name1)->atom_concat('?',Name1,Name);
   format(atom(Name),"'$VaR'(~q)",[Var]))),!.
printable_variable_name(Var,Name):- v_name1(Var,Name),!.
printable_variable_name(Var,Name):- v_name2(Var,Name),!. % ,atom_concat(Name1,'_TL',Name).

v_name1(Var,Name):- var_property(Var,name(Name)),!.
v_name1(Var,Name):- get_varname_list(Vs),member(Name=V,Vs),atomic(Name),V==Var,!.
v_name1(Var,Name):- nb_current('$old_variable_names', Vs),member(Name=V,Vs),atomic(Name),V==Var,!.
v_name2(Var,Name):- get_varname_list(Vs),format(atom(Name),'~W',[Var, [variable_names(Vs)]]).


%attrs_to_list(att(sk,_,ATTRS),[sk|List]):-!,attrs_to_list(ATTRS,List).
attrs_to_list(att(vn,_,ATTRS),List):-!,attrs_to_list(ATTRS,List).
attrs_to_list(att(M,V,ATTRS),[M=VV|List]):- locally(tlbugger:plain_attvars,simplify_goal_printed(V,VV)),!,attrs_to_list(ATTRS,List).
attrs_to_list([],[]).
attrs_to_list(_ATTRS,[]).

%% simplify_goal_printed( :TermVar, :TermVar) is semidet.
%
% Simplify Goal Printed.
%

% public_file_link(M:G,MG):-

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

simplify_var_printed(Var,'aVar'('$VAR'(Name))):- tlbugger:plain_attvars,must(printable_variable_name(Var,Name)),!.
simplify_var_printed(Var,'$VAR'(Name)):-  get_attrs(Var,att(vn, _, [])),printable_variable_name(Var, Name),!.
simplify_var_printed(Var,'aVar'('$VAR'(Name))):- tlbugger:plain_attvars,must(printable_variable_name(Var,Name)),!.
simplify_var_printed(Var,'aVar'(Dict)):- get_attrs(Var,ATTRS),must(printable_variable_name(Var,Name)),attrs_to_list(ATTRS,List),
                         dict_create(Dict,'$VAR'(Name),List).
simplify_var_printed(Var,'$VAR'(Name)):- is_ftVar(Var),!,printable_variable_name(Var, Name).

simplify_goal_printed(Var,Printed):- nonvar(Printed),!,simplify_goal_printed(Var,UnPrinted),ignore(Printed=UnPrinted),!.
% simplify_goal_printed(Var,Name):-is_ftVar(Var), \+ current_prolog_flag(variable_names_bad,true), simplify_var_printed(Var,Name),!.
simplify_goal_printed(Var,VarO):- var(Var),!,VarO=Var.
simplify_goal_printed(Var,VarO):- is_ftVar(Var),!,VarO=Var.
simplify_goal_printed(Var,Name):-cyclic_term(Var),!,w_o_c(Name=Var).
simplify_goal_printed(setup_call_catcher_cleanup,sccc).
% simplify_goal_printed(existence_error(X,Y),existence_error(X,Y)):-nl,writeq(existence_error(X,Y)),nl,fail.
simplify_goal_printed(setup_call_cleanup,scc).
%simplify_goal_printed(existence_error,'existence_error_XXXXXXXXX__\e[0m\e[1;34m%-6s\e[m\'This is text\e[0mRED__existence_error_existence_error').
simplify_goal_printed(each_call_cleanup,ecc).
simplify_goal_printed(call_cleanup,cc).
simplify_goal_printed([Var|_],'$'):-compound(Var),Var = (VT = _ ), (attvar(VT);var(VT);VT = var_tracker(_); VT = fbound(_)),!.
%  %simplify_goal_printed(M:G,MG):-atom(M),number(G),exists_file_safe(M),public_file_link(M:G,MG),!.
%  %simplify_goal_printed(M,MG):-atom(M),exists_file_safe(M), public_file_link(M,MG),!.
simplify_goal_printed(M:G,MS:GS):-atom(M), simplify_m(M,MS),!,simplify_goal_printed(G,GS).
simplify_goal_printed(M:I,M:O):-!, simplify_goal_printed(I,O).
%simplify_goal_printed(M:I,O):- atom(M),(M==user;M==system),!,simplify_goal_printed(I,O).
%simplify_goal_printed(M:I,O):- atom(M),!,simplify_goal_printed(I,O).
%simplify_goal_printed(catch(I,V,_),O):- var(V),!,simplify_goal_printed(I,O).
simplify_goal_printed(always(I),O):- !,simplify_goal_printed(I,O).
simplify_goal_printed(must_det_lm(M,G),GS):-!,simplify_goal_printed(M:must_det_l(G),GS).
%simplify_goal_printed('<meta-call>'(G),GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(call(G),GS):-!,simplify_goal_printed(G,GS).
simplify_goal_printed(M:G,MS:GS):-atom(M), simplify_m(M,MS),!,simplify_goal_printed(G,GS).
simplify_goal_printed(dinterp(_,_,I,_),O):- !,simplify_goal_printed(I,O).
simplify_goal_printed(call_term_expansion(_,A,_,B,_),O):- !, simplify_goal_printed(call_term_expansion_5('...',A,'...',B,'...'),O).
%simplify_goal_printed(A,'/.../'(Dir,SA)):- atom(A),atom_concat('/',_,A),directory_file_path(DirL,SA,A),directory_file_path(_,Dir,DirL),!.
%simplify_goal_printed(A,'...'(SA)):- atom(A),concat_atom([_,SA1|SA2],'logicmoo_',A),!,(SA2==[]->SA=SA1;SA=SA2).
simplify_goal_printed(GOAL=A,AS):- goal==GOAL,!,simplify_goal_printed(A,AS).
simplify_goal_printed(Var,Var):- \+ compound(Var),!.
simplify_goal_printed(P,O):- compound(P), \+ is_dict(P),compound_name_arguments(P,F,[I]),
  atom_contains(F,must),!,simplify_goal_printed(I,O).
simplify_goal_printed(term_position(_,_,_,_,_),'$..term_position/4..$').
%simplify_goal_printed(user:G,GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(system:G,GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(catchv(G,_,_),GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(catch(G,_,_),GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(skolem(V,N,_F),GS):-!,simplify_goal_printed(skeq(V,N,'..'),GS).
simplify_goal_printed(I,O):- once(dumpst_hook:simple_rewrite(I,O)), I \== O,!.

simplify_goal_printed(List,O):- current_prolog_flag(dmsg_len,Three),
  is_list(List),length(List,L),L>Three,
   append([A,B,C],[F|_],List),F \='...'(_), !,
  simplify_goal_printed([A,B,C,'....'(L>Three)],O).


simplify_goal_printed([E|OList],O):- fail,  \+ is_list(OList),
   append(List,Open,OList),var(Open),!,
    current_prolog_flag(dmsg_len,Three),
   is_list(List),length(List,L),L>Three,
    append([A,B,C],[F|_],[E|List]),F \='...'(_), !,
   simplify_goal_printed([A,B,C,'...'(_)],O).


simplify_goal_printed([F|A],[FS|AS]):- !,simplify_goal_printed(F,FS),simplify_goal_printed(A,AS).
simplify_goal_printed(G,GS):- univ_safe_2(G,[F|A]),maplist(simplify_goal_printed,A,AA),univ_safe_2(GS,[F|AA]).


:-create_prolog_flag(dmsg_len,99,[keep(true)]).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray




%=

%% getPFA( ?Frame, ?Ctrl, ?Goal) is semidet.
%
% Get Pred Functor A.
%
getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Ctrl,Goal):-getPFA1(Frame,Ctrl,Goal).


%=

%% getPFA1( ?Frame, ?Txt, ?Txt) is semidet.
%
% Get Pred Functor A Secondary Helper.
%
getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Ctrl,Ctrl=Goal):-getPFA2(Frame,Ctrl,Goal),!.
getPFA1(_,Ctrl,no(Ctrl)).


%=

%% getPFA2( ?Frame, ?Ctrl, ?Goal) is semidet.
%
% Get Pred Functor A Extended Helper.
%
getPFA2(Frame,Ctrl,Goal):- catchv((prolog_frame_attribute(Frame,Ctrl,Goal)),E,Goal=[error(Ctrl,E)]),!.


%=

%% clauseST( ?ClRef, :TermGoal) is semidet.
%
% Clause S True Stucture.
%
clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   snumbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).


:- thread_local(tlbugger:ifCanTrace/0).


%=

%% end_dump( :TermGG) is semidet.
%
% End Dump.
%
end_dump(true):-!,fail.
end_dump(_:GG):-!,end_dump(GG).
end_dump(GG):-compound(GG),functor(GG,F,_),atom_concat(dump,_,F),nb_setval('$hide_rest_frames',true).

% =====================
% dtrace/0/1/2
% =====================

%:- redefine_system_predicate(system:dtrace()).
dtrace:- wdmsg("DUMP_TRACE/0"), (thread_self_main->(dumpST,rtrace);(dumpST(30),abort)).
%=

%% dtrace is semidet.
%
% (debug) Trace.
%
%:- redefine_system_predicate(system:dbreak()).

:- thread_local(t_l:no_dbreak/0).
%dbreak:- wdmsg("DUMP_BREAK/0"), !, break, throw(abort).
dbreak:- wdmsg("DUMP_BREAK/0"), !, throw(abort).
dbreak:- wdmsg("DUMP_BREAK/0"),((ignore(on_x_fail(dumpST)), break,wdmsg("DUMP_BREAK/0"))),!,
  (t_l:no_dbreak -> wdmsg("NO__________________DUMP_BREAK/0") ;
      (thread_self_main->(dumpST,dtrace(system:break),break);true)).

:- thread_local(tlbugger:has_auto_trace/1).
:-meta_predicate(dtrace(0)).

%=

%% dtrace( :GoalG) is semidet.
%
% (debug) Trace.
%

dtrace(G):- zotrace((tlbugger:has_auto_trace(C),wdmsg(has_auto_trace(C,G)))),!,call(C,G).
dtrace(G):- strip_module(G,_,dbreak),\+ thread_self_main,!.
% dtrace(G):- zotrace((tracing,notrace)),!,wdmsg(tracing_dtrace(G)),
%   scce_orig(notrace,restore_trace((leash(+all),dumptrace_or_cont(G))),trace).

dtrace(G):- zotrace((once(((G=dmsg(GG);G=_:dmsg(GG);G=GG),nonvar(GG))),wdmsg(GG)))->true;
 catch(dumptrace1(G),E, handle_dumptrace_signal(G,E)),fail. %always fails
%dtrace(G):- \+ tlbugger:ifCanTrace,!,quietly((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,dumpST.
%dtrace(G):- \+ tlbugger:ifCanTrace,!,quietly((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,dumpST.
dtrace(G):-
    catch(dumptrace1(G),E,handle_dumptrace_signal(G,E)).

handle_dumptrace_signal(G,E):-arg(_,v(continue,abort),E),!,wdmsg(continuing(E,G)),notrace,nodebug.
handle_dumptrace_signal(_,E):-throw(E).
%:- export(dumptrace_or_cont/1).
%dumptrace_or_cont(G):- catch(dumptrace(G),E,handle_dumptrace_signal(G,E)).



% :-meta_predicate(dtrace(+,?)).

%=

%% dtrace( +MSG, ?G) is semidet.
%
% (debug) Trace.
%
dtrace(MSG,G):-wdmsg(MSG),dtrace(G).


%=

%% to_wmsg( :TermG, :TermWG) is semidet.
%
% Converted To Wmsg.
%
to_wmsg(G,WG):- \+ compound(G),!,WG=G.
to_wmsg(M:G,M:WG):-atom(M), to_wmsg(G,WG).
to_wmsg(dmsg(G),WG):-!, to_wmsg(G,WG).
to_wmsg(wdmsg(G),WG):-!, to_wmsg(G,WG).
to_wmsg(G,WG):- (G=WG).


with_source_module(G):-
  '$current_source_module'(M),
  '$current_typein_module'(WM),
  scce_orig('$set_typein_module'(M),G,'$set_typein_module'(WM)).



% =====================
% dumptrace/1/2
% =====================
% :-meta_predicate(dumptrace(?)).

%=

%% dumptrace( ?G) is semidet.
%
% Dump Trace.
%
dumptrace(G):- non_user_console,!,dumpST_error(non_user_console+dumptrace(G)),abort,fail.
dumptrace(G):-
  locally(set_prolog_flag(gui_tracer, false),
   locally(set_prolog_flag(gui, false),
    locally(set_prolog_flag(runtime_debug,0),
     dumptrace0(G)))).

dumptrace0(G):- zotrace((tracing,notrace,wdmsg(tracing_dumptrace(G)))),!, catch(((dumptrace0(G) *-> dtrace ; (dtrace,fail))),_,true).
dumptrace0(G):-dumptrace1(G).
dumptrace1(G):-
  catch(attach_console,_,true),
    repeat,
    (tracing -> (!,fail) ; true),
    to_wmsg(G,WG),
    fmt(in_dumptrace(G)),
    wdmsg(WG),
    (get_single_char(C)->with_all_dmsg(dumptrace(G,C));throw(cant_get_single_char(!))).

:-meta_predicate(dumptrace(0,+)).

ggtrace:-
  leash(-all),
  visible(+all),
  % debug,
  maybe_leash(+exception).

%=

%% dumptrace( :GoalG, +C) is semidet.
%
% Dump Trace.
%
dumptrace(_,0'h):- listing(dumptrace/2),!,fail.
dumptrace(_,0'g):-!,dumpST,!,fail.
dumptrace(_,0'G):-!,zotrace(dumpST0(500000)),!,fail.
dumptrace(_,0'D):-!,prolog_stack:backtrace(8000),!,fail.
dumptrace(_,0'd):-!,prolog_stack:backtrace(800),!,fail.

dumptrace(G,0'l):-!,
  restore_trace(( zotrace(ggtrace),G)),!,notrace.
%dumptrace(G,0's):-!,quietly(ggtrace),!,(quietly(G)*->true;true).
dumptrace(G,0'S):-!, wdmsg(skipping(G)),!.
dumptrace(_,0'c):-!, throw(continue).
%dumptrace(G,0'i):-!,quietly(ggtrace),!,ignore(G).
dumptrace(_,0'b):-!,debug,break,!,fail.
dumptrace(_,0'a):-!,abort,!,fail.
% dumptrace(_,0'x):-!,must(lex),!,fail.
dumptrace(_,0'e):-!,halt(1),!.
dumptrace(_,0'm):-!,make,fail.
dumptrace(G,0'L):-!,use_module(library(xlisting)),call(call,xlisting,G),!,fail.
dumptrace(G,0'l):-!,visible(+all),show_and_do(rtrace(G)).
% dumptrace(G,0'c):-!, show_and_do((G))*->true;true.
dumptrace(G,0'r):-!, stop_rtrace,notrace,nortrace,srtrace,(rtrace((trace,G,notrace))),!,fail.
dumptrace(G,0'f):-!, notrace,(ftrace((G,notrace))),!,fail.
dumptrace(G,0't):-!,visible(+all),leash(+all),trace,!,G.
dumptrace(G,10):-!,dumptrace_ret(G).
dumptrace(G,13):-!,dumptrace_ret(G).
dumptrace(G,Code):- number(Code),char_code(Char,Code),!,dumptrace(G,Char).
dumptrace(_G,'p'):- in_cmt(if_defined(pp_DB,fail)),!,fail.


dumptrace(_,C):-fmt(unused_keypress(C)),!,fail.
% )))))))))))))) %  '

%=

%% dumptrace_ret( ?G) is semidet.
%
% Dump Trace Ret.
%
dumptrace_ret(G):- zotrace((leash(+all),visible(+all),visible(+unify),trace)),G.

% % % OFF :- system:use_module(library(logicmoo_utils_all)).

% % % OFF :- system:use_module(library(logicmoo_startup)).
:- fixup_exports.


end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.


%% hook_message_hook is semidet.
%
% Hook Message Hook.
%
% hook_message_hook
hook_message_hook:-
 asserta((

%  current_predicate(logicmoo_bugger_loaded/0)

user:message_hook(Term, Kind, Lines):- current_prolog_flag(runtime_message_hook, true),
 quietly((
 loop_check((ignore((
 tlbugger:rtracing,
 \+ \+
 catch(((
 rtrace,
 (Kind= warning;Kind= error),
 Term\=syntax_error(_),
 backtrace(40), \+ baseKB:no_buggery, \+ tlbugger:no_buggery_tl,
 stop_rtrace,1trace,
  dmsg(message_hook(Term, Kind, Lines)),quietly(dumpST(10)),dmsg(message_hook(Term, Kind, Lines)),
   !,fail,
   (sleep(1.0),read_pending_codes(user_input, Chars, []), format(error_error, '~s', [Chars]),flush_output(error_error),!,Chars=[C],
                dtrace(true,C),!),

   fail)),_,true))),fail)))))).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
% :- load_files(library(prolog_stack), [silent(true)]).
%prolog_stack:stack_guard(none).

% :-hook_message_hook.

%user:prolog_exception_hook(A,B,C,D):- fail,
%   once(copy_term(A,AA)),catchv(( once(bugger_prolog_exception_hook(AA,B,C,D))),_,fail),fail.

