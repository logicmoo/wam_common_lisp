/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * 8ball.pl 
 *
 * Douglas'' Notes:
 *
 * 8BALL is used to predict when failure and errors may occur
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module('os7r33M', []).

:- meta_predicate in_comment(0).
:- meta_predicate both_outputs(0).
:- meta_predicate split_user_output_to_file(*,0).
:- meta_predicate in_md2(*,0).
:- meta_predicate in_md(*,0).
:- meta_predicate on_first_write_old_goal(*,0,*,*).
:- meta_predicate show_call_trace(0).
:- meta_predicate dnotrace(0).
:- meta_predicate ansicall_maybe(*,0).


:- include('./header').

:- use_module(library(predicate_streams)).

wl:interned_eval("(defparameter sys:*markdown* cl:t)").



both_outputs(G):-
  (is_user_output -> G ; (with_output_to(user_output,G),G)).


colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,format(Msg,Args)).
%colormsg1(Msg):- writeq(Msg),nl,nl,!. %dnotrace(colormsg11(Msg)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,in_md(prolog,fmt9(Msg))).

%ansicall_maybe(_Ctrl,Cmd):- !,nl,nl,portray_clause_w_vars(Cmd),nl,nl,(Cmd),break.
ansicall_maybe(_Ctrl,Cmd):- current_output(O), \+ stream_property(O,tty(true)),!,call(Cmd).
ansicall_maybe(Ctrl,Cmd):- always(make_pretty(Cmd,Cmd0)),!,call(ansicall(Ctrl,Cmd0)).

show_call_trace(G):- G *-> dbginfo(:- side_effect(G)); ((dbginfo(:- failure(show_call_trace(G)))),!,fail).

% http://htmlpreview.github.io/?https://github.com/bartaz/impress.js/blob/master/index.html#/step-3

is_must_show_msg(C):-compound(C),compound_name_arity(C,N,_),!,is_must_show_msg0(N). is_must_show_msg0(warn). is_must_show_msg0(error). is_must_show_msg0(failed).


fmt_lispcode(Cmt):- is_comment(Cmt,Txt),clean_codes(Txt,Clean),atom_string(Clean,Str),
   (atom_contains(Str,'\n')->format('~N/*~n~w~N*/~n',[Str]), format('~N% ~w~n',[Str])).
fmt_lispcode(Txt):- set_md_lang('common-lisp'),cmpout(comment(Txt)),set_md_lang(prolog).

% Lisp Informational Message (depends on verbosity level)

dbginfo(X):- dnotrace(ignore((is_verbose;is_must_show_msg(X))->userout(X))).

%dnotrace(G):- !, notrace(G).
dnotrace(G):- call(G),!.

userout(X):- dnotrace(userout0(X)).
% User Message (intended to be seen)
userout0(flat(X)):- !,write_flat(X).
userout0(X):- simplify_goal_printed(X,XX),!,in_md(cl,dnotrace(dbmsg(comment(XX)))).

write_flat(X):- !, dnotrace((make_pretty(X,X0),writeq(X0),writeln('.'))),!.

% Compiler output when writitng files
cmpout(X):- in_md(prolog,dnotrace(dbmsg(X))).


dbmsg(_):- notrace(current_prolog_flag(dmsg_level,never)),!.
dbmsg(X):- make_pretty(X,X0),both_outputs(dbmsg0(X0)).
dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str)).
dbmsg0([A|B]):- !,dbmsg0(A),dbmsg0(B).
dbmsg0(comment(X)):-!, in_comment(dbmsg0(X)).
dbmsg0(N=V):- !, in_comment(dbmsg0(N:-V)).
dbmsg0(:- A):- dbmsg1((:-),A),!.
dbmsg0(A):- dbmsg1(call,A),!.
% dbmsg0(StringL):- to_prolog_string_if_needed(StringL,String),!,dbmsg0(String).
dbmsg0(:- X):- colormsg1(:- X),!.
dbmsg0(X):- in_comment(colormsg1(:- was_info(X))),!.

dbmsg1(_,!).
dbmsg1(_,true).
dbmsg1(_Mode,((H:-B))):-!,in_cmt(write_flat(H:-B)).
dbmsg1(Mode,((A,B))):-  is_assert_op(A,Where,AA), !,dbmsg_assert(Where, AA),dbmsg2(Mode, B).
dbmsg1(Mode,((B,A))):-  is_assert_op(A,Where,AA), !,dbmsg2(Mode, B),dbmsg_assert(Where, AA).
dbmsg1(Mode,((B,A,C))):-  is_assert_op(A,Where,AA), !,dbmsg2(Mode, B),dbmsg_assert(Where, AA),dbmsg2(Mode, C).
dbmsg1(_,A):- is_assert_op(A,Where,AA),!,dbmsg_assert(Where,AA).

dbmsg2(call, B):- dbmsg0(B).
dbmsg2((:-), B):- dbmsg0(:-B).

pre_annote_type(Head,Type):- compound(Head),!,functor(Head,F,_),pre_annote_type(F,Type).
pre_annote_type(Head,Type):- \+ atom(Head),!,Type = "### Compiled Value:".
pre_annote_type(Head,Type):- atom_concat('f_',_,Head),!,Type = "### Compiled Function:".
pre_annote_type(Head,Type):- atom_concat('mf_',_,Head),!,Type = "### Compiled Macro Function:".
pre_annote_type(Head,Type):- atom_concat('sf_',_,Head),!,Type = "### Compiled Macro Operator:".
pre_annote_type(_Head,Type):- Type = "### Compiled:".
  

dbmsg_assert(Where,(A,B)):- !,dbmsg_assert(Where,A),dbmsg_assert(Where,B).
dbmsg_assert(Where,user:(HBody)):- !,dbmsg_assert(Where,(HBody)).
dbmsg_assert(Where, (user:H) :- Body):- !,dbmsg_assert(Where,(H :- Body)),!.
dbmsg_assert(Where,(Head:-Body)):- Body==true,!, dbmsg_assert(Where,(Head)).
dbmsg_assert(Where,(Head:-Body)):- set_md_lang([]),!, pre_annote_type(Head,Type),always((in_comment(pre_annotation(Type,Where)),
   del_attrs_of((Head:-Body),rwstate),
   set_md_lang(prolog),in_md(prolog,colormsg1(Head:-Body)),assert_lsp(Head:-Body))).
dbmsg_assert(Where,Head):- strip_module(Head,_,P),functor(P,F,_),arg(_,v(arglist_info,lambda_def),F),!, 
   always((nop(pre_annotation("#### annotating...",Where)),!,in_md(prolog,fmt9(Head)),!,assert_lsp(Head))).
dbmsg_assert(Where,Head):- !, always((nop(pre_annotation("#### annotating...",Where)),!, fmt99(Head), assert_lsp(Head))),!.


pre_annotation(What,Where):- where_where(Where,Where1),colormsg1("\n~w `~w` ",[What,Where1]).

where_where(Where,Atom):- locally_let(sym('cl:*package*')=pkg_kw,with_output_to(atom(Atom),f_prin1(Where,_))),!.
where_where(Where,Where).

:- thread_local(t_l:in_print_cmt/0).
  
is_verbose :- !.
is_verbose :- \+ set_prolog_flag(lisp_verbose,0); \+ current_prolog_flag(debug,false).
is_user_output:- current_output(O),!,
  (is_predicate_stream(O)-> true ; (stream_property(O,alias(user_output))-> true ; stream_property(O,alias(user_error)))).
is_markdown:- is_user_output, true.

in_comment(X):- (t_l:in_print_cmt;is_user_output),!,call(X).
in_comment(X):- locally(t_l:in_print_cmt, setup_call_cleanup(format('~N/*~n',[]),in_md(cl,X),format('~N*/~n',[]))).

:- thread_local(t_l:inside_lang/1).

current_md_lang(Was):- t_l:inside_lang(Was)->true;Was=[].


in_md(_Lang,G):- !,call(G).

in_md2(_Lang,G):- (( \+ is_markdown ) ; ( \+ is_user_output )),!,call(G).
% in_md(Lang,G):- set_md_lang(Lang),!,call(G).
% in_md(Lang,G):- current_md_lang(Was),retractall(t_l:inside_lang(Was)),assert(t_l:inside_lang(Lang)),setup_call_cleanup(format('~N```~n```~w~n',[Lang]),call(G),format('~N```~n```~w~n',[Was])).
in_md2(Lang,G):- current_md_lang(Was),Was==Lang,!,call(G).
in_md2(Lang,G):- current_md_lang(Was),!,
      locally(t_l:inside_lang(Lang),
      (
       ((setup_call_cleanup(format('~N```~n```~w~n',[Lang]),
          call(G),
            set_md_lang(Was)))))).

in_md2(Lang,G):- current_md_lang(Was),
       Cleanup = call(true),
       current_output(Old),
       setup_call_cleanup( 
        set_on_first_write((set_md_lang(Lang),nb_setarg(1,Cleanup,nop(set_md_lang(Was))))),
          (call(G),set_real_user_output(Old)),
            (set_real_user_output(Old),Cleanup)).



%set_md_lang([]).
set_md_lang(Lang):- current_md_lang(Was),
 (Was==Lang -> true;
   (retractall(t_l:inside_lang(Was)),assert(t_l:inside_lang(Lang)),
   (is_markdown -> 
    ((Was \== [] -> format('~N```~n',[]) ; true),
     (Lang \== [] ->  format('~N```~w~n',[Lang]) ; true)); true))).



split_user_output_to_file(File,G):- 
  stream_property(Old,alias(user_output)),
  setup_call_cleanup(open(File,write,FOut),
  setup_call_cleanup(new_predicate_output_stream(split_user_output_to_file(Old,FOut),Out),
  setup_call_cleanup(set_prolog_IO(user_input,Out,user_error),call(G),
          close(Out)),
          set_real_user_output(Old)),
          close(FOut)).

split_user_output_to_file(Old,FOut,Data):-
  write(Old,Data),flush_output(Old),
  write(FOut,Data),flush_output(FOut).
   
set_real_user_output(Out):- set_prolog_IO(user_input,Out,user_error).

set_on_first_write(Goal):- 
((current_output(Old), 
  new_predicate_output_stream(on_first_write_old_goal(Old,Goal),Out),
  set_real_user_output(Out))).

on_first_write_old_goal(Old,Goal,_,Data):- 
  (no_data(Data) -> true ; (set_real_user_output(Old),call(Goal),write(Old,Data),flush_output(Old))). 
no_data(''). no_data(' ').  no_data('\n').

:- asserta(t_l:inside_lang(prolog)).
:- fixup_exports.

