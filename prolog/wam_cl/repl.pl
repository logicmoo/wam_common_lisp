/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(cl, []).
:- set_module(class(library)).
:- include('header.pro').

:- use_module(sreader).

:- dynamic 
	user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4.
:- multifile 
	user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4.

:- initialization((lisp,prolog),main).


print_eval_string(Str):-
   str_to_expression(Str, Expression),
   dmsg(:- print_eval_string(Expression)),
   eval_at_repl(Expression, Result),!,
   write_results(Result),
   !.

eval_string(Str):-
   str_to_expression(Str, Expression),
   eval_at_repl(Expression, Result),!,
   write_results(Result),!.


trace_eval_string(Str):-
  str_to_expression(Str, Expression),
   redo_call_cleanup(trace,eval(Expression, Result),notrace),
     write_results(Result),
     !.

rtrace_eval_string(Str):-
  str_to_expression(Str, Expression),
   rtrace(eval(Expression, Result)),
     write_results(Result),
     !.

:- meta_predicate(with_input_from_string(+,:)).
with_input_from_string(Str,Goal):-
 open_string(Str,In),
 with_input_from_stream(In,Goal).


:- meta_predicate(with_input_from_stream(+,:)).
with_input_from_stream(In,Goal):- 
   each_call_cleanup(see(In),Goal,seen).



prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).
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
	%tidy_database,
        el_new(ENV),nb_setval('$env_toplevel',ENV),
	repeat,
        catch(read_eval_print(Result),'$aborted',fail),
   	notrace(Result == end_of_file),!,
   	prompt(_, Old),
   	prompts(Old1, Old2),!.


tidy_database:-
	nb_delete('$env_current'),
        env_current(_Env),
	retractall(lambda(_, _)).

show_uncaught_or_fail((A,B)):-!,show_uncaught_or_fail(A),show_uncaught_or_fail(B).
show_uncaught_or_fail(G):- notrace(flush_all_output_safe),
  (catch(G,E,notrace((wdmsg(uncaught(E)),!,fail)))*->true;notrace((wdmsg(failed(G)),!,fail))).

set_prompt_from_package:-
  quietly((ignore((symbol_value(_ReplEnv, xx_package_xx, Package),
        short_package_or_hash(Package,Name0),
        (Name0=="U"->Name="CL-USER";Name=Name0),
        always(nonvar(Name)),
        atom_concat(Name,'> ',Prompt),
        prompt(_,Prompt))))).

read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
        set_prompt_from_package,

        quietly(show_uncaught_or_fail(read_no_parse(Expression))),!,
        quietly(show_uncaught_or_fail(lisp_add_history(Expression))),!,
        nb_linkval('$mv_return',[Result]),
        show_uncaught_or_fail(eval_at_repl(Expression,Result)),!,
        quietly(show_uncaught_or_fail(write_results(Result))),!.
	

write_results(Result):- 
 writeExpression(Result),
 ignore((nb_current('$mv_return',[Result|Push]),writeExtraValues(Push))),
 nl.

writeExtraValues(X):- maplist(writeExpressionEV,X),!.
writeExpressionEV(X):- writeln(' ;'),writeExpression(X),flush_all_output_safe.

/*
:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse_i(Expr):- current_input(In), parse_sexpr_untyped_read(In, Expr).
:- endif.
*/

% read and parse a line of Lisp
/*
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
*/
 
lisp_add_history(end_of_file):-!.
lisp_add_history(_):- prolog_load_context(reload,true),!.
lisp_add_history(_):- prolog_load_context(reloading,true),!.
lisp_add_history(Expression):- atom(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):- string(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):- is_stringp(Expression),!,
       to_prolog_string(Expression,ExpressionS),
        prolog:history(user_input, add(ExpressionS)).
lisp_add_history(Expression):-
        with_output_to(string(S),writeExpression(Expression)),
        (string_upper(S,S)->string_lower(S,Store);Store=S),
        prolog:history(user_input, add(Store)).

:- set_prolog_flag(lisp_no_compile,false).

% basic EVAL statements for built-in procedures
eval_at_repl(Var,  R):- notrace(var(Var)),!, R=Var.
eval_at_repl(Expression, Result):- quietly(eval_repl_hooks(Expression,Result)),!.
eval_at_repl(Expression,Result):- notrace(tracing), !, trace,eval_at_repl_tracing(Expression,Result).
eval_at_repl(Expression,Result):-
  notrace(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  notrace(dbmsg_cmt(:- lisp_compiled_eval(LExpression))),
  notrace(debug_var('ReplEnv',Env)),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  notrace(dbmsg_cmt(repl_exec:-Code)),
  (notrace(tracing)-> Code ; 
   timel('EXEC',always_catch(ignore(always(maybe_ltrace(call(user:Code))))))),!.

eval_at_repl_tracing(Expression,Result):-
  quietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  writeq((reader_intern_symbols(SExpression,LExpression))),nl,
  notrace(debug_var('ReplEnv',Env)),
  %notrace(cls),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   dbmsg(:- lisp_compiled_eval(LExpression)),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
  lisp_compile(Env,Result,LExpression,Code),
  % notrace(cls),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace(dbmsg(:-Code)),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
  Code.

eval(Expression, Result):- env_current(Env), eval(Expression, Env, Result).

eval(Expression, Env, Result):-
   always_catch(maybe_ltrace(lisp_compile(Env,Result,Expression,Code))), 
   always_catch(ignore(always(maybe_ltrace(call(user:Code))))),!.


/*:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
:- endif.
*/
read_and_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped_read(In, Expr).
read_no_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped(In, Expr).

flush_all_output_safe:- forall(stream_property(S,mode(write)),notrace(catch(flush_output(S),_,true))).

parse_sexpr_untyped_read(In, Expr):- 
  parse_sexpr_untyped(In,ExprS),!,
  as_sexp(ExprS,ExprS1),!,reader_intern_symbols(ExprS1,Expr).


eval_repl_hooks(V,_):-var(V),!,fail.
eval_repl_hooks(nil,  []):-!.
eval_repl_hooks(Atom, R):- atom(Atom),atom_concat_or_rtrace(_,'.',Atom),notrace(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((user:call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).
eval_repl_hooks([debug,A], t):- !,debug(lisp(A)).
eval_repl_hooks([nodebug,A], t):- !, nodebug(lisp(A)).
eval_repl_hooks([UC|A], R):- atom(UC),downcase_atom(UC,DC),DC\==UC,!,eval_repl_hooks([DC|A], R).
eval_repl_hooks([X], R):-!, eval_repl_atom( X, R),!.
eval_repl_hooks( X , R):- eval_repl_atom( X, R),!.

maybe_ltrace(G):- current_prolog_flag(lisp_trace,true)->rtrace(G);always(G).

eval_repl_atom(V,_):-var(V),!,fail.
eval_repl_atom(end_of_file, end_of_file):-!.
eval_repl_atom(quit, end_of_file):-!.
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(ltrace, t):- set_prolog_flag(lisp_trace,true),debug(lisp(trace)),debug,debugging.
eval_repl_atom(noltrace, t):- set_prolog_flag(lisp_trace,false),nodebug(lisp(trace)),nodebug,debugging.

eval_repl_atom(debug, t):- debug(lisp(_)),debug,debugging.
eval_repl_atom(nodebug, t):- nodebug(lisp(_)),nodebug,debugging.

eval_repl_atom(make, O):- !, always((make, cl_compile_file_mask(pack('wam_commmon_lisp/prolog/wam_cl/lisp/'),keys([]),O))).

eval_repl_atom(UC, R):- atom(UC),downcase_atom(UC,DC),DC\==UC,eval_repl_atom(DC, R).


eval_repl_atom(show, t):- 
  listing([user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4]).

lw:- cl_load("wam-cl-params",_).
%:- cddd.
% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
%lisp_add_history("prolog.")
:- initialization((lisp,prolog),main).
%:- lisp_add_history("lisp.").

:- set_prolog_flag(verbose_autoload,false).

:- ignore((((((clause(arithmetic:eval(P,_,_),_),nonvar(P)),(functor(P,F,A),show_call_trace(define_cl_math(F,A)))))),fail)).

:- fixup_exports.

%:- cddd.

end_of_file.





 X = "
(defmacro yfor  (variable what-to-do &rest llist)
  (let ((iteration-variable (gensym))
          (iteration-expression (gensym))
          stepping-variable)
    `(let ((,iteration-variable nil)
           (,iteration-expression nil) )
       ,(record-in-loop-alist `(,variable ,iteration-variable) 'iteration-variable)
       #|
        #+ignore
           (format t \"~% yfor variable is: ~a ~% and it is ~a to in\"
                       (intern (symbol-name what-to-do))
                (eq 'in 
                 (intern (symbol-name what-to-do))))
                  |#
       ,(case (intern (symbol-name what-to-do))
          (in
            (record-in-loop-alist `(endp ,iteration-expression) 'end-test
             )
            (add-elements-to-clause 'next
            `(setf ,iteration-variable (car ,iteration-expression))
            `(setf ,iteration-expression (cdr ,iteration-expression)))
            (add-elements-to-clause 'initializations
                                    `(,iteration-variable  ;(car ,@llist))
                                                           (car ,iteration-expression))
                                    `(,iteration-expression  ,@llist))
            )   
          (on
            (record-in-loop-alist iteration-expression 'iteration-control-variable
             )
            (record-in-loop-alist `(endp ,iteration-expression) 'end-test
             )
            (add-elements-to-clause 'next
                             `(setf ,iteration-variable ,iteration-expression)
                             `(setf ,iteration-expression (cdr ,iteration-expression)))
            ; note that since you are in a let*, don't eval the expression twice, use
            ; the variable that it will be bound to
            (add-elements-to-clause 'initializations
                             `(,iteration-variable  (car ,iteration-expression))
                            `(,iteration-expression ,@llist)))
          (from     
            (if (null (fifth llist)) (setf stepping-variable 1)
                  (setf stepping-variable (fifth llist)))
            (cond
              ((> (length llist) 5) 
                (lerror \"YL:Too many clauses in (yfor ~a ~a ..)\" variable
                                       what-to-do))
              ((and (minusp stepping-variable)(<= (first llist) (third llist)))
               (lerror \"YL:Cannot decrement from ~a to ~a\" 
                        (first llist) (third llist)))
              (t 
               (add-element-to-loop-alist `(,iteration-variable ,(first llist))
                                          'initializations
                )
               (add-element-to-loop-alist `(setf ,iteration-variable
                                            (+ ,iteration-variable ,stepping-variable)) 'next
                )
               (if (minusp stepping-variable )
                   (add-element-to-loop-alist `(< ,iteration-variable ,(third llist))
                                              'end-test 
                    )
                   (add-element-to-loop-alist `(> ,iteration-variable ,(third llist))
                                              'end-test
                    )))))
       ))) t)

       ",
    parse_sexpr(X,Out),
    wdmsg(parse_sexpr(X,Out)).
    

