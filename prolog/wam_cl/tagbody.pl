/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(tagbody,[]).

:- set_module(class(library)).

:- include('header.pro').


%:- dynamic(compile_body_h/5).
%:- multifile(compile_body_h/5).
:- discontiguous(compile_body_h/5).



tst:is_local_test(tagbody_let3,
 [let, [b],
     [tagbody,
         setq(b,2),go(tag2),
         setq(a,1), % never seen
         (tag1),setq(b,3),go(tag3),
         (tag2),setq(a,4),go(tag1),
         (tag3),print('1+'(plus(a,b)))
     ],
  b],3).


tst:is_local_test(tagbody7_prints_8,
  [tagbody,
      setq(b,2),go(tag2),
      setq(a,1), % never seen
      (tag1),setq(b,3),go(tag3),
      (tag2),setq(a,4),go(tag1),
      (tag3),print('1+'(plus(a,b)))
   ],[]). % prints 8



tst:is_local_test(tagbody6,
  [tagbody,
   setq(b,2),[go,tag2],setq(a,1),
   (tag1),setq(a,4),prolog_call([a,b],plus(a,b,C)),prolog_call(writeln(C)),
   (tag2),setq(a,4),[go,tag1]],
    []). % prints 6


shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_body_h(Ctx,Env,Result,InstrS,Code),!.


%label_atoms(Instr,[label,Tag]):- is_label(Instr,Tag),!.
label_atoms(Tag,[label,Tag]):-atomic(Tag),!.
label_atoms(Instr,Instr).

compile_body_h(_Ctx,_Env,Result, nop(X),  nop(X)):- !, debug_var("_NopResult",Result).
%compile_body_h(_Ctx,_Env,Result,[label, Tag], push_label(Tag) ):- debug_var("_LABELRES",Result).
% @IDEA we might use labels later 
compile_body_h(_Ctx,_Env,Result,[label, Tag], push_label(Tag) ):- debug_var("_LABELRES",Result).

compile_body_h(Ctx,Env,[],[tagbody| InstrS], Code):- 
  compile_as_tagbody(Ctx,Env,InstrS,Code).

% tagbody_go(Label,Env)
compile_body_h(_Ctx,Env,Result,[go, Tag], tagbody_go(Tag,Env) ):- debug_var("_GORES",Result),debug_var("GoEnv",Env).
compile_body_h(Ctx,Env,Result,go(Tag), Body ):- !,compile_body_h(Ctx,Env,Result,[go, Tag],Body).
compile_body_h(_Ctx,_Env,PrevResult,[u_prolog_trace],trace).

tst:is_local_test(tagbody1,[tagbody,setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b))],[]).
compile_as_tagbody(Ctx,Env,InstrS,Code):-
 must_det_l((
   get_go_points(InstrS,Gos),
   get_tags(Env,InstrS,Gos,Addrs), 
   check_missing_gos(Gos),   
   compile_addrs(Ctx,Env,Result,Addrs),
   compile_tagbody_forms(Ctx,Env,Result,InstrS,CInstrS),
   copy_term(Addrs,Addrs2),
   Code = call_addr_block(Env,CInstrS,Addrs2,Result))).


call_addr_block(EnvCatch,Start,Addrs,Result):- 
   catch(Start,
      tagbody_go(Label,EnvCatchE),
      go_addr_block(EnvCatchE,Label,EnvCatch,Addrs,Result)).

% @TODO find out how to get rid of copy_term/2
go_addr_block(EnvCatchE,Label,EnvCatch,Addrs,Result):- 
   member(addr(Label,_,NewEnv,NewCode),Addrs),!,
   copy_term(NewEnv:NewCode,NewEnvCopy:NewCodeCopy),
   NewEnvCopy = EnvCatchE,
   call_addr_block(EnvCatch,NewCodeCopy,Addrs,Result).	 
   
go_addr_block(EnvCatchE,Label,_EnvCatch,Addrs,_Result):- 
   wdmsg(addrs:-Addrs),
   debug(lisp(tagbody_go),'Bubbling ~q',[tagbody_go(Label,EnvCatchE)]),
   notrace(throw(tagbody_go(Label,EnvCatchE))).

tagbody_go(Label,Env):- notrace(throw(tagbody_go(Label,Env))).
push_label(_).


is_reflow([OP|ARGS],Label):- is_reflow(OP,ARGS,Label).
is_reflow(OPARGS,Label):- OPARGS=..[OP|ARGS],is_reflow(OP,ARGS,Label).
is_reflow('go',[Label|_],Label).
is_reflow('cl_go',[Label|_],Label).
is_reflow('tagbody_go',[Label|_],Label).
is_reflow('gosub',[Label|_],Label).
is_reflow('return',_,[]).
is_reflow(OP,[Label|_],Label):- same_symbol('return-from',OP).
is_reflow('throw',[Label|_],Label).

is_label(Atom,Atom):- atomic(Atom),!,Atom\==[].
is_label([OP|ARGS],Label):- is_label(OP,ARGS,Label).
is_label(OPARGS,Label):- OPARGS=..[OP|ARGS],is_label(OP,ARGS,Label).
is_label('enter',[Label|_],Label).
is_label('exit',[Label|_],Label).
is_label('label',[Label|_],Label).

is_branched([Op|_]):- fail,member(Op,[if,or,and,progn]).


get_go_points([FlowInst|InstrS],[addr(Label,'$used','$missing','$missing')|Addrs]):- is_reflow(FlowInst,Label),!,
  get_go_points(InstrS,Addrs).
get_go_points([],[]).
get_go_points([I|InstrS],Addrs):-% #branching call
  is_branched(I),get_go_points(I,IAddrs),
  get_go_points(InstrS,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_go_points([_|InstrS],Addrs):-
  get_go_points(InstrS,Addrs).

get_tags(Env,[Label|InstrS],Gos,[GAddrs|Addrs]):- atomic(Label),
  member(GAddrs,Gos),GAddrs=addr(Label,_Used,_Env_,_Missing), !,
   setarg(3,GAddrs,Env),
   setarg(4,GAddrs,InstrS),
  get_tags(Env,InstrS,Gos,Addrs).
get_tags(Env,[Label|InstrS],Gos,[GAddrs|Addrs]):-
  member(GAddrs,Gos),GAddrs=addr(Label,_Used,_Env_,_Missing),
   setarg(3,GAddrs,Env),
   setarg(4,GAddrs,InstrS),
  get_tags(Env,InstrS,Gos,Addrs).
get_tags(Env,[TagInstr|InstrS],Gos,[GAddrs|Addrs]):- is_label(TagInstr,Label),
   GAddrs = addr(Label,'$unused','$env',InstrS),
    setarg(3,GAddrs,Env),
    setarg(4,GAddrs,InstrS),
  get_tags(Env,InstrS,[GAddrs|Gos],Addrs).
get_tags(_Env,[],_,[]).
get_tags(Env,[I|InstrS],Gos,Addrs):- % #branching call
  is_branched(I),get_tags(Env,I,Gos,IAddrs),
  get_tags(Env,InstrS,Gos,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_tags(Env,[_|InstrS],Gos,Addrs):-
  get_tags(Env,InstrS,Gos,Addrs).

% @todo
check_missing_gos(_).


compile_addrs(Ctx,Env,Result,[A|Addrs]):-
  compile_addr1(Ctx,Env,Result,A),
  compile_addrs(Ctx,Env,Result,Addrs).
compile_addrs(_Ctx,_Env,_Result,_).

compile_addr1(Ctx,_Env,Result,A):- A= addr(_Tag,_Unused,_E,InstrS),   
   must_or_rtrace(compile_tagbody_forms(Ctx,NewEnv,Result,InstrS,Code)),
   setarg(3,A,NewEnv),
   setarg(4,A,Code),!.
compile_addr1(_Ctx,_Env,_Result,_):- !.


compile_tagbody_forms(Ctx,Env,Result,[enter(_)|InstrS],BInstrS):- !,compile_tagbody_forms(Ctx,Env,Result,InstrS,BInstrS).
compile_tagbody_forms(Ctx,Env,Result,InstrS,BInstrS):-
   maplist(label_atoms,InstrS,TInstrS),
   trim_tagbody(TInstrS,CInstrS),
   compile_forms(Ctx,Env,Result,CInstrS,BInstrS).

trim_tagbody(InstrS,TInstrS):- append(Left,[R|_],InstrS),is_reflow(R,_),!,append(Left,[R],TInstrS).
trim_tagbody(InstrS,InstrS).


% asserta((fifteen(Val_Thru23):-!, []=[[]], LETENV=[[bv(val, [[]|_832])]], 
%   call_addr_block(Env,(symbol_setq(val, 1, _1398), tagbody_go(Result,'point-a')), [addr('point-c', '$used',  (push_label('point-c'), sym_arg_val_env(val, Val_In, Val_Thru, LETENV), incf(Val_Thru, 4, Incf_Ret), tagbody_go(Result,'point-b'))), addr('point-a', '$used',  (push_label('point-a'), push_label('point-d-unused'), sym_arg_val_env(val, Val_In12, Val_Thru13, LETENV), incf(Val_Thru13, 2, Incf_Ret14), tagbody_go(Result,'point-c'))), addr('point-d-unused', '$unused',  (push_label('point-d-unused'), sym_arg_val_env(val, Val_In17, Val_Thru18, LETENV), incf(Val_Thru18, 2, Incf_Ret19), tagbody_go(Result,'point-c'))), addr('point-b', '$used', ['point-b', [incf, val, 8]])], _GORES15), sym_arg_val_env(val, Val_In22, Val_Thru23, LETENV)))


tst:is_local_test(let_simple,
"(defun let_simple ()
  (let (val)
    val))
    ",[]).

tst:is_local_test(let_simple1,
"(defun let_simple1 ()
  (let ((val 1))
    val))
    ",1).

tst:is_local_test(let_tagbody,
"(defun fifteen ()
  (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
     point-u ;; unused
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val))
    ",15).


tst:is_local_test(`
 (tagbody ))
`,[]).

tst:is_local_test(`
 (tagbody 1 (print "hi" ))
`,[]).

% should loop
tst:is_local_test(`
 (tagbody 1 (print "hi" ) (go 1))
`,[]).


:- fixup_exports.

end_of_file.


%:- forall(clause(block_tagbody_test(_N),B),B).
`

 (let (val)
    (tagbody
      (setq val 1)
      (print "(setq val 1)")
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (print "(incf val 04)")
      (go point-b)
      (incf val 32)
     point-a
      (incf val 02)
      (print "(incf val 02)")
      (go point-c)
      (incf val 64)
     point-b
      (print "(incf val 08)")
      (prolog-trace)
      (incf val 08)
      (print val))
    val)
=>  15
 (defun f1 (flag)
   (let ((n 1))
     (tagbody 
       (setq n (f2 flag #'(lambda () (go out))))
      out
       (prin1 n))))
=>  F1
 (defun f2 (flag escape)
   (if flag (funcall escape) 2))
=>  F2
 (f1 nil)
>>  2
=>  NIL
 (f1 t)
>>  1
 ; =>  NIL


`



