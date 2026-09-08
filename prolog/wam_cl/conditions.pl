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
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(errs, []).



:- include('./header').


wl:declared_as(f_error,inline(error)).
wl:init_args(0,error).
f_error(Args,Res):- f_format(t,"~a",Args,Res),throw(f_error(Args,Res)).

mf_ignore_errors([ignore_errors|Forms],_Env,
                 [sys_ignore_errors|Forms]).

mf_handler_bind([handler_bind,[]|Forms],_Env,
                [progn|Forms]).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [sys_ignore_errors|Forms],_PreviousResult,
    catch((nb_linkval('$mv_return',[Result]),Body),
          Error,wamcl_ignore_error(Error,Result))):-
  must_compile_body(Ctx,Env,Result,[progn|Forms],Body).

wamcl_ignore_error(Error,_Result):-
  wamcl_control_transfer(Error),!,
  throw(Error).
wamcl_ignore_error(Error,Result):-
  f_values_list([[],Error],Result).

wamcl_control_transfer(block_exit(_,_)).
wamcl_control_transfer(goto(_,_)).
wamcl_control_transfer(goto(_,_,_)).
wamcl_control_transfer(goto(_,_,_,_)).
wamcl_control_transfer(lisp_throw(_,_)).
wamcl_control_transfer('$aborted').

% Connection to LPA's built-in error handler

'?ERROR?'(Error, Form):-
	lisp_error_description(_, Error, Description),
	!,
	write('LISP ERROR  '),
	write(Description),
	write(Form),
	nl.
'?ERROR?'(Error, Goal):-
	wl:error_hook(Error, Goal).

lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'SetQ: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'Form1: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').



:- fixup_exports.

      
end_of_file.