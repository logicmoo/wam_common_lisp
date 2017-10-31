/*******************************************************************
 *
 * A Lisp interpreter, written in Prolog
 *
 * (lisp_interpreter.pl)
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * This file: implements a small amount of EVAL for bootstrapping the compiler
 *  
 *
 * Neil''s Notes:
 *
 * (c) Neil Smith, 2001
 *
 * This program is a small interpreter for Lisp.  It was written
 * in LPA Prolog v3.6, running under Windows.  It should be fairly
 * easy to convert it to other Prologs.
 *
 * It supports real Lisp syntax, excessive brackets and all.  You don't 
 * need to terminate input with a full stop.  It also understands 
 * 'x for (quote x) and #'func for (function func)
 *
 * Variables are lexically scoped, except where defined as special.
 *
 * read_words code from "The Craft of Prolog", R.A.O'Keefe
 * lisp evaluator from "Lisp" (3rd ed), Winston & Horn.
 *
 *******************************************************************/

:- ensure_loaded(lpa_to_swi).

:-style_check.

:- dynamic 
	lisp_global_bindings/1,
	named_lambda/2,
        macro_lambda/3.

:- initialization((lisp,prolog),main).

:- meta_predicate(timel(:)).
timel(M:X):- prolog_statistics:time(M:X).

str_to_expression(Str, Expression):- lisp_add_history(Str),parse_sexpr_untyped(string(Str), Expression),!.
str_to_expression(Str, Expression):- with_input_from_string(Str,read_and_parse(Expression)),!.

print_eval_string(Str):-
  str_to_expression(Str, Expression),
   dmsg(print_eval_string(Expression)),
   eval(Expression, Result),!,
   dmsg(print_Result(Result)),
     writeExpression(Expression),
     !.


eval_string(Str):-
  str_to_expression(Str, Expression),
   timel(eval(Expression, Result)),
        writeExpression(Result),
     !.

trace_eval_string(Str):-
  str_to_expression(Str, Expression),
   redo_call_cleanup(trace,eval(Expression, Result),notrace),
     writeExpression(Result),
     !.

rtrace_eval_string(Str):-
  str_to_expression(Str, Expression),
   rtrace(eval(Expression, Result)),
     writeExpression(Result),
     !.

:- meta_predicate(with_input_from_string(+,:)).
with_input_from_string(Str,Goal):-
 open_string(Str,In),
 with_input_from_stream(In,Goal).


:- meta_predicate(with_input_from_stream(+,:)).
with_input_from_stream(In,Goal):- 
   each_call_cleanup(see(In),Goal,seen).




prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).
lisp_global_bindings([]).

lisp:- write('
__        ___    __  __        ____ _
\\ \\      / / \\  |  \\/  |      / ___| |
 \\ \\ /\\ / / _ \\ | |\\/| |_____| |   | |
  \\ V  V / ___ \\| |  | |_____| |___| |___
   \\_/\\_/_/   \\_\\_|  |_|      \\____|_____|
'),nl,
	write('Common Lisp, written in Prolog'),nl,
	prompt(Old, '> '),
	prompts(Old1, Old2),
	prompts('> ', '> '),
	% tidy_database,
	repeat,
   	once(read_eval_print(Result)),
   	Result == quit,!,
   	prompt(_, Old),
   	prompts(Old1, Old2),!.


tidy_database:-
	retract(lisp_global_bindings(_)),
	assert(lisp_global_bindings([])),
	retractall(lambda(_, _)).


read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
	read_and_parse(Expression),
        lisp_add_history(Expression),
        catch((
        eval(Expression,Result),
	writeExpression(Result)),E,dmsg(uncaught(E))),
	!.

 
lisp_add_history(end_of_file):-!.
lisp_add_history(_):- prolog_load_context(reload,true),!.
lisp_add_history(_):- prolog_load_context(reloading,true),!.
lisp_add_history(Expression):- atom(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):- string(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):-
        with_output_to(string(S),writeExpression(Expression)),
        (string_upper(S,S)->string_lower(S,Store);Store=S),
        prolog:history(user_input, add(Store)).

:- set_prolog_flag(lisp_compile,true).
eval_compiled(SExpression,Result):-
    locally(set_prolog_flag(lisp_compile,true),eval_int(SExpression,Result)).

% basic EVAL statements for built-in procedures
eval_int(Var,  R):- var(Var),!, R=Var.
eval_int(SExpression,Result):- 
  as_sexp(SExpression,Expression),
  eval(Expression,Result).

eval(SExpression,Result):-eval_repl(SExpression,Result).
eval(Expression, Result):-
   current_prolog_flag(lisp_compile,true),!,
   dbmsg(lisp_compile(Expression)),
   lisp_compile(Result,Expression,Code),
   dbmsg(Code),
   call(Code),!.
eval(Expression, Result):-
   lisp_global_bindings(Bindings),
   eval(Expression, Bindings, Result).


macro_expand([],[]):-!.
macro_expand([#, '''', X|Xs], [[function, MX]|MXs
 ]):-
	!,             
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand(['''', X|Xs], [[quote, MX]|MXs]):-
	!,
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand([X|Xs], [MX|MXs]):-
	!,
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand(X, X):-
	atomic(X),
	!.                        

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).
is_self_evaluationing_object(X):- var(X),!.
is_self_evaluationing_object(X):- atomic(X),!,(number(X);string(X);(blob(X,T),T\==text);X=t;X=[]),!.
is_self_evaluationing_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.


eval_repl(nil,  []):-!.
eval_repl(Atom, R):- atom(Atom),atom_concat(_,'.',Atom),notrace(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).
eval_repl([quote, X], X):-!.
eval_repl([debug,A], t):- debug(lisp(A)).
eval_repl([nodebug,A], t):- nodebug(lisp(A)).
eval_repl([X], R):- eval_repl_atom( X, R),!.
eval_repl( X , R):- eval_repl_atom( X, R),!.

eval_repl_atom(end_of_file, quit):-!.
eval_repl_atom(quit, quit):-!.
eval_repl_atom(make, t):- !, make.
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(debug, t):- debug(lisp(_)).
eval_repl_atom(nodebug, t):- nodebug(lisp(_)).
eval_repl_atom(show, t):- 
  listing([named_lambda/2,
        macro_lambda/3,
        lisp_global_bindings/1]).


eval(Expression, Bindings, Result):- 
   debug(lisp(eval),'~N~p -> ',[eval3(Expression, Bindings)]),
   eval3(Expression, Bindings, Result),
   debug(lisp(eval),' -> ~p ~n ',[result(Result)]),!.

eval3([defvar, Name], _, Name):-
	!,
	retract(lisp_global_bindings(GlobalBindings)),
	assert(lisp_global_bindings([bv(Name, [])|GlobalBindings])),
	!.
eval3([setq, Name, Value], Bindings, EvalValue):-
	!,
	lisp_global_bindings(GlobalBindings),
	append(Pre, [bv(Name, _)|Post], GlobalBindings),
	eval(Value, Bindings, EvalValue),
	retract(lisp_global_bindings(GlobalBindings)),
	append(Pre, [bv(Name, EvalValue)|Post], GlobalBindings1),
	assert(lisp_global_bindings(GlobalBindings1)),
	!.
eval3([defmacro, Name, FormalParms | Body], _, Name):-
	!,
        retractall(macro_lambda(Name,_,_)),
	assert(macro_lambda(Name, FormalParms, Body)),
	!.
eval3([defun, Name, FormalParms, Body], _, Name):-
	!,        
        retractall(named_lambda(Name, [lambda, _, _])),
	assert(named_lambda(Name, [lambda, FormalParms, Body])),
	!.

eval3(['$BQ',One], Bindings, Out):-!, expand_commas(Bindings,One,Out).
eval3([('`'),One], Bindings, Out):-!, expand_commas(Bindings,One,Out).

eval3([apply|Arguments], Bindings, Result):-
	!,
	evalL(Arguments, Bindings, [Function, ActualParams]),
	apply_f(Bindings,Function, ActualParams, Result),
	!.


eval3([function, [lambda,  FormalParams, Body]], Bindings, 
		[closure, FormalParams, Body, Bindings]):-!.

eval3(['eval*',Arguments], Bindings, Result):-!,
   eval(Arguments, Bindings, MResult),
   eval(MResult,Result).
eval3(['eval*'|Arguments], Bindings, OutR):-!,
   evalL(Arguments, Bindings, MResults),
   evalL(MResults, [none], Results),
   once(last(Results,OutR);OutR=[]).

eval3([Procedure|Arguments], Bindings, Result):-  lisp_operator(Procedure),!,
  apply_f(Bindings,Procedure, Arguments, Result),
  !.

eval3([Procedure|Arguments], Bindings, Result):-
  macro_lambda(Procedure, FormalParams, LambdaExpression),
  bind_variables(FormalParams, Arguments, Bindings, NewBindings),
  eval(['eval*'|LambdaExpression], NewBindings, Result).

eval3([Procedure|Arguments], Bindings, Result):-
	evalL(Arguments, Bindings, EvalArguments),
	apply_f(Bindings,Procedure, EvalArguments, Result),
	!.
eval3(X, Bindings, Val):-
	zotrace((atom(X),
      (member(bv(X, Val), Bindings)
	;	(lisp_global_bindings(GlobalBindings),
		 member(bv(X, Val), GlobalBindings)))
	)),!.
eval3(X, _, X):- zotrace(is_self_evaluationing_object(X)),!.
eval3(X, Bindings, Y):- \+ is_list(X),compound(X),!,X=..XL,eval3(XL, Bindings, Y).
eval3(X, _, []):- (debugging(lisp(eval))->dumpST;true),
	write('SYSTEM::READ-EVAL-PRINT: variable '),write(X),write(' has no value'),nl,!.

lisp_operator(if).

expand_commas(_,One,Out):- \+ compound(One),!,One=Out.
expand_commas(Bindings,['$COMMA',One],Out):- !, eval(One,Bindings,Out).
expand_commas(Bindings,['$BQ',One],Out):- !, expand_commas(Bindings,One,Mid), (One==Mid ->  Out=['$BQ',Mid] ; Out=Mid),!.
expand_commas(Bindings,'$COMMA'(One),Out):- !, eval(One,Bindings,Out).
expand_commas(Bindings,One,Out):- is_list(One),!,maplist(expand_commas(Bindings),One,Out).
expand_commas(Bindings,One,Out):-
  compound_name_arguments(One,F,Args),
  maplist(expand_commas(Bindings),Args,ArgsOut),
  Out=..[F|ArgsOut],!.

evalL([], _, []):-!.
evalL([H|T], Bindings, [EvalH|EvalT]):-
	eval(H, Bindings, EvalH),
	evalL(T, Bindings, EvalT),
	!.

pf_car(A,Out):- \+ is_list(A),type_error(list,A,car(A),pf_car(A,Out)).
pf_car([A|_],A).
pf_car(_,[]).

apply_f(_Binds,function, [A],[function,A]).
apply_f(_Binds,car, LIST, Result):-!,(LIST=[[Result|_]]->true;Result=[]).
apply_f(_Binds,cdr, LIST, Result):-!,(LIST=[[_|Result]]->true;Result=[]).
apply_f(_Binds,list, Args, Args):-!.
apply_f(_Binds,cons, [Arg1, Arg2], [Arg1|Arg2]):-!.
apply_f(_Binds,eq, [Arg1, Arg2], Result):-
	(Arg1 = Arg2 -> Result = Arg1 
		      ; Result = []),
	!.
apply_f(Bindings,if_wrong, [Test, Success, Failure], Result):-
	eval(Test,Bindings, TestResult),
	eval(Success,Bindings, EvalSuccess),
	eval(Failure,Bindings, EvalFailure),
	(TestResult = [] -> Result = EvalFailure
			  ; Result = EvalSuccess),
	!.
apply_f(Bindings,if, [Test, Success, Failure], Result):-  !,
	eval(Test, Bindings, TestResult),
	(TestResult \== [] -> eval(Success, Bindings, Result)
			  ; eval(Failure, Bindings, Result)),
	!.
apply_f(Binds,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams,Binds, Bindings),!,
	eval(Body, Bindings, Result),
	!.
apply_f(_Binds,[closure, FormalParams, Body, Bindings0], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams, Bindings0, Bindings),
	eval(Body, Bindings, Result),
	!.

apply_f(_Binds,ProcedureName, ActualParams, Result):-
	macro_lambda(ProcedureName,FormalParams, LambdaExpression),!,
	bind_variables(FormalParams, ActualParams, Bindings),
        eval(LambdaExpression, Bindings, Result),
	!.
apply_f(Bindings,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_f(Bindings,LambdaExpression, Args, Result),
	!.

apply_f(_,=,[X,Y],R):-!, X \= Y -> R=[] ; R=t.
apply_f(_,-,[X,Y],R):-!, R is X - Y.
apply_f(_,'1+',[X],R):-!, R is X + 1.
apply_f(_,+,[X,Y],R):-!, R is X + Y.
apply_f(_,F,ARGS,R):- atom(F),append(ARGS,[R],RARGS),length(RARGS,A),current_predicate(F/A),!,apply(F,RARGS),!.
apply_f(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dmsg(CALL->E),!,fail))->R=t;R=[]).
apply_f(Binds,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  Cannot apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('Binds'=Binds),nl,
        
	!.
	
new_cl_fixnum(X,R):-
  create_struct(cl_fixnum,[X],R),!.

create_struct(Type,R):-create_struct(Type,[],R),!.
create_struct(TypeARGS,R):- compound_name_arguments(TypeARGS,Type,ARGS), create_struct(Type,ARGS,R),!.
create_struct(Type,ARGS,R):-
   data_record(Type,PARGS),
   parse_data_record_args3(PARGS,ARGS,KVs),
   %append(KVs,['type-info'-data_record(Type,PARGS),extended2-'$mutable'([],_)],Make),
   Make = KVs,
   dict_create(R,Type,Make).

create_struct1(Type,[Value],Value):- data_record(Type,[_]),!.
create_struct1(Type,ARGS,R):-create_struct(Type,ARGS,R),!.
create_struct1(_Type,Value,Value).

parse_data_record_args3(PARGS,[],KVs):- make_defaults(PARGS,KVs),!.
parse_data_record_args3([m(ro, integer, Name)], [X], [Name-X]).
parse_data_record_args3(PARGS,ARGS,KVs):-
  must_det_l(( apply:partition(=(m(ro,_,_)), PARGS,ReadOnly,ReadWrite),
   parse_data_record_args5(ReadOnly,ARGS,Part1,ExtraRO,ExtraArgs),
   parse_data_record_args5(ReadWrite,ExtraArgs,Part2,ExtraRW,ExtraExtraArgs),
   make_defaults(ExtraRO,Part3),
   make_defaults(ExtraRW,Part4),
   make_defaults(ExtraRW,Part4),
   append(Part1,Part2,Part12),
   append(Part3,Part4,Part34),   
   (ExtraExtraArgs == [] ->  append(Part12,Part34,KVs) ; 
   append(Part12,[extended1-ExtraExtraArgs|Part34],KVs)))).
parse_data_record_args5(REST,[],[],REST,[]).
parse_data_record_args5([],REST,[],[],REST).
parse_data_record_args5([m(_,Type,Name)|PARGS],[X|ARGS],[Name-Value|KVs],O1,O2):-
   create_struct1(Type,X,Value),!,
   parse_data_record_args5(PARGS,ARGS,KVs,O1,O2).

name_value_default(m(_,array_of(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,prolog_array_list(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,Type,Name),Name-Def):-value_default(Type,Def).
name_value_default(m(_,Type,Name),Name-mut(@null,Type)).
name_value_default(N-V,N-V).

value_default(prolog_concurrent_hash_map(K,V),mut([],map(K,V))).
value_default(prolog_hash_map(K,V),mut([],map(K,V))).
value_default(cl_list,[]).
value_default(integer,0).
value_default(cl_object,mut([],cl_object)).

%value_default(cl_simple_string, @(null)).
%value_default(cl_string, @(null)).
%value_default(prolog_array_list(_),[]).
%value_default(array_of(_),[]).

make_defaults([],[]).
make_defaults([Default|From],[Value|Values]):- name_value_default(Default,Value),
  make_defaults(From,Values).

data_record(Name,[Name],[],[InitArg]):- 
  data_record(Name,[InitArg]),
  m_arg3(InitArg,Name).

data_record(Name,InitArgs,Extras,Args):-
  data_record(Name,Args),Args\=[_],
  must_det_l((
  apply:partition(=(m(ro,_,_)), Args,ReadOnly,ReadWrite),
  maplist(m_arg3,ReadOnly,InitArgs),
  maplist(m_arg3,ReadWrite,Extras))).

m_arg3(m(_,_,Name),Name):-!.

:- ensure_loaded(wam_cl_structs).


bind_variables(Formal, Actual, Bindings):-
	bind_variables(Formal, Actual, [], Bindings).

bind_variables([], [], Bindings, Bindings).
bind_variables([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_variables(FormalParams, ActualParams, 
		[bv(FormalParam, ActualParam)|Bindings0], Bindings).




:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse_i(Expr):- current_input(In), parse_sexpr_untyped(In, Expr).
:- endif.

% read and parse a line of Lisp
read_and_parse1(Expression):-
	read_words(TokenL),
	(	sexpr1(Expression, TokenL, [])
	;
		( write('ERROR!  Could not parse `'),
		  writeTokenL(TokenL),
		  write('`'),nl,
		  Expression = [] )
	),
	!.



% read a line of supposed Lisp code

read_words(Words):-
	get0(C),
	read_words(C, Words).

read_words(C, []):-
	ends_line(C),	
	!.
read_words(C, Words):-
	whitespace(C),
	!,
	read_words(Words).
read_words(C, [Word|Words]):-
	punctuation(C),
	!,
	name(Word, [C]),
	read_words(Words).
read_words(C, [Word|Words]):-
	other(C),
	!,
	read_rest_of_word(Chars, LeftOver),
	name(UCWord, [C|Chars]),
	( atom(UCWord) -> lwrupr(Word, UCWord)
		;	
		Word = UCWord),
	read_words(LeftOver, Words).


read_rest_of_word(Chars, LeftOver):-
	get0(C),
	read_rest_of_word(C, Chars, LeftOver).


read_rest_of_word(C, [], C):-
	\+ other(C),
	!.
read_rest_of_word(C, [C|Chars], LeftOver):-
	other(C),
	!,
	read_rest_of_word(Chars, LeftOver).



ends_line(10).
ends_line(13).
ends_line(0).
ends_line(-1).


whitespace(9).
whitespace(32).


punctuation(0'.).
punctuation(0'!).
punctuation(0'").
punctuation(0',).
punctuation(0'').
punctuation(0':).
punctuation(0';).
punctuation(0'?).
punctuation(0'().
punctuation(0')).
punctuation(0'[).
punctuation(0']).
% punctuation(0'#).


other(Char):-
	integer(Char),
	Char >= 0,
	Char =< 127,
	\+ ends_line(Char),
	\+ whitespace(Char),
	\+ punctuation(Char).


% Grammar rules for parsing Lisp s-expressions.
% Given a list of tokens, lisplist does all the nesting of lists

sexpr1(X) --> {is_ftVar(X),(get_var_name(X,N)->format(atom(NN),'~w',[N]);format(atom(NN),'~w',[X]))},!,[NN].
sexpr1(Str)--> {string(Str)},!,[Str].
sexpr1([function, Expression]) --> [#, ''''], !, sexpr1(Expression).
sexpr1([quote, Expression]) --> [''''], !, sexpr1(Expression).
sexpr1(['$BQ',X])--> ['`'],sexpr1(X).
sexpr1(Xs) --> {is_list(Xs)},!,['('], lisplist(Xs), !.
sexpr1([X|Y]) --> sexpr1(X), ['.'], sexpr1(Y), [')'], !.
sexpr1('$COMMA'(X)) --> [','],sexpr1(X).
sexpr1(X) --> [X].


lisplist([]) --> [')'], !.
lisplist([X|Xs]) --> sexpr1(X), lisplist(Xs), !.



% writeExpression/1 displays a lisp expression

writeExpression(quit):-
	!,
	write('Terminating WAM-CL'),nl.
writeExpression(Expression):-
	sexpr1(Expression, TokenL, []),
%	write('  '),
	writeTokenL(TokenL),
	nl.


writeTokenL([]).
writeTokenL(['(', ')'|TokenL]):-
	!,
	write('NIL '),
	writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
	atom(Token),
	!,
	% lwrupr(Token, UCToken),
        =(Token, UCToken),
	write(UCToken),
	write(' '),
	writeTokenL(TokenL).
writeTokenL([UCToken|TokenL]):-
	string(UCToken),
   write(' '),
   writeq(UCToken),	
   write(' '),
	writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
	number(Token),
	!,
	write(Token),
	write(' '),
	writeTokenL(TokenL).


run666(S):-'format'('~n~s~n',[S]),run666(S,V),writeq(V).

parsing(Program, Forms0):- sformat(S,'(\n~s\n)\n',[Program]),str_to_expression(S,Forms0).
run666(Program, Values) :-
    quietly(parsing(Program, Forms)),
    maplist(see_and_do(eval),Forms,Values),
    last(Values,Last),
    writeExpression(Last).

see_and_do(Pred2, I,O):-
  dmsg(seeingFormala(I)),
  call(Pred2,I,O),
  dmsg(result(O)).

:- ensure_loaded(('lisp_compiler')).
:- set_prolog_flag(double_quotes,string).

% if_script_file_time666(_):-!.
if_script_file_time666(X):- prolog_statistics:time(user:X).

% Append:
test(0) :- if_script_file_time666(run666("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
test(1) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
test(2) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
test(3):- if_script_file_time666(run666("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
test(4):- if_script_file_time666(run666("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].
 

unused_ :- writeln('
| ?- lisp.
Welcome to WAM-CL!
This is a miniscule Lisp interpreter, written in Prolog
> (cons 1 nil)
( 1 ) 
> (defun my_second (lst) (car (cdr lst)))
MY_SECOND 
> (my_second \'(a b c))
B 
> (defun fib (n) (if (> n 1) (+ (fib (- n 1)) (fib (- n 2)))1))

> quit
Terminating WAM-CL
yes
'
).


/*
:- eval_string("(defun append (x y) (if x (cons (car x) (append (cdr x) y))  y))").
:- eval_string("(defmacro foo (a) `,a)").
:- eval_string("(defmacro fooq (a) `',a)").
:- eval_string("(fooq b)").
:- eval_string("(nodebug)").
:- eval_string("(debug)").
*/
:- lisp_add_history("lisp.").
 





