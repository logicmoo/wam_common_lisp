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

:- discontiguous(compile_body_h/5).

shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_body_h(Ctx,Env,Result,InstrS,Code),!.



% @IDEA we might use labels later 
%compile_body_h(_Ctx,_Env,Result, nop(X),  nop(X)):- !, debug_var("_NopResult",Result).
compile_body_h(_Ctx,_Env,Result,[label,ID, Tag], push_label(ID,Tag) ):- debug_var("_LABELRES",Result).
%tagbody_go(Label,Env):- notrace(throw(tagbody_go(Label,Env))).
tagbody_go(_TB,_Tag,Pred,Env):- call(Pred,Env).


compute_new_proc(TB,Label,Pred):-atomic_list_concat([TB,Label],'_',Pred).

push_label(_,_).

compile_body_h(Ctx,Env,[],[tagbody| InstrS], Code):- 
  gensym(tagbody_,TB),
  compile_as_tagbody(Ctx,Env,TB,[TB|InstrS],Assertions),
  maplist(add_context_code(Ctx),Assertions),
  Code =.. [TB,Env].

% tagbody_go(Label,Env)
compile_body_h(_Ctx,Env,Result,[go,TB, Tag], tagbody_go(TB,Tag,Pred,Env) ):- compute_new_proc(TB,Label,Pred), debug_var("_GORES",Result),debug_var("GoEnv",Env).
compile_body_h(_Ctx,_Env,PrevResult,[u_prolog_trace],trace).

compile_as_tagbody(Ctx,Env,TB,InstrS,Assertions):-
 must_det_l((
   externalize_go_points(Env,TB,InstrS,Gos),
   get_tags(TB,Env,InstrS,Gos,Addrs),
   check_missing_gos(Gos),   
   compile_addrs(TB,Ctx,Env,Result,Addrs),
   maplist(addresses_to_proceedures(TB),Addrs,Assertions).

 /*
 addr(Pred,u_point_c,'$used',
   Incf_Env, 
    (place_op(Incf_Env, incf, u_val, [4], _2854), 
    cl_print("(incf val 04)", _2866), 
    tagbody_go(u_point_b, Incf_Env))).

tagBodyId_66_u_point_c(Incf_Env) :-
    (place_op(Incf_Env, incf, u_val, [4], _2854), 
    cl_print("(incf val 04)", _2866), 
    tagbody_go(u_point_b, Incf_Env))).

 */
addresses_to_proceedures(TB,Addr,Assert):-
 Addr = addr(Pred,Label,MaybeUsed,
   Incf_Env, 
    (place_op(Incf_Env, incf, u_val, [4], _2854), 
    cl_print("(incf val 04)", _2866), 
    tagbody_go(u_point_b, Incf_Env))).




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


externalize_go_points(Env,TB,[FlowInst|InstrS],[addr(Pred,Label,'$used',Env,'$missing')|Addrs]):- 
  is_reflow(FlowInst,Label),!,
  compute_new_proc(TB,Label,Pred),
  externalize_go_points(Env,TB,InstrS,Addrs).
externalize_go_points(Env,TB,[],[]).
externalize_go_points(Env,TB,[I|InstrS],Addrs):-% #branching call
  is_branched(I),externalize_go_points(Env,TB,I,IAddrs),
  externalize_go_points(Env,TB,InstrS,NAddrs),
  append(IAddrs,NAddrs,Addrs).
externalize_go_points(Env,TB,[_|InstrS],Addrs):-
  externalize_go_points(Env,TB,InstrS,Addrs).

get_tags(TB,Env,[Label|InstrS],Gos,[GAddrs|Addrs]):- atomic(Label),
  compute_new_proc(TB,Label,Pred),
  member(GAddrs,Gos),GAddrs=addr(Pred,Label,_Used,_Env_,_Missing), !,
   setarg(3,GAddrs,Env),
   setarg(4,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,Gos,Addrs).
get_tags(TB,Env,[Label|InstrS],Gos,[GAddrs|Addrs]):-
  member(GAddrs,Gos),GAddrs=addr(Pred,Label,_Used,_Env_,_Missing),
  compute_new_proc(TB,Label,Pred),
   setarg(3,GAddrs,Env),
   setarg(4,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,Gos,Addrs).
get_tags(TB,Env,[TagInstr|InstrS],Gos,[GAddrs|Addrs]):- is_label(TagInstr,Label),
   compute_new_proc(TB,Label,Pred),
   GAddrs = addr(Pred,Label,'$unused','$env',InstrS),
    setarg(3,GAddrs,Env),
    setarg(4,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,[GAddrs|Gos],Addrs).
get_tags(TB,_Env,[],_,[]).
get_tags(TB,Env,[I|InstrS],Gos,Addrs):- % #branching call
  is_branched(I),get_tags(TB,Env,I,Gos,IAddrs),
  get_tags(TB,Env,InstrS,Gos,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_tags(TB,Env,[_|InstrS],Gos,Addrs):-
  get_tags(TB,Env,InstrS,Gos,Addrs).

% @todo
check_missing_gos(_).


compile_addrs(TB,Ctx,Env,Result,[A|Addrs]):-
  compile_addr1(TB,Ctx,Env,Result,A),
  compile_addrs(TB,Ctx,Env,Result,Addrs).
compile_addrs(TB,_Ctx,_Env,_Result,_).

compile_addr1(TB,Ctx,_Env,Result,A):- A= addr(Pred,_Tag,_Unused,_E,InstrS),   
   must_or_rtrace(compile_tagbody_forms(TB,Ctx,NewEnv,Result,InstrS,Code)),
   setarg(3,A,NewEnv),
   setarg(4,A,Code),!.
compile_addr1(_TB,_Ctx,_Env,_Result,_):- !.




compile_tagbody_forms(TB,Ctx,Env,Result,[enter(_)|InstrS],BInstrS):- !,compile_tagbody_forms(TB,Ctx,Env,Result,InstrS,BInstrS).
compile_tagbody_forms(TB,Ctx,Env,Result,InstrS,BInstrS):-
   maplist(label_atoms(TB),InstrS,TInstrS),
   trim_tagbody(TInstrS,CInstrS),
   compile_forms(Ctx,Env,Result,CInstrS,BInstrS).

%label_atoms(Instr,[label,Tag]):- is_label(Instr,Tag),!.
label_atoms(TB,Tag,[label,Tag]):-atomic(Tag),!.
label_atoms(TB,[go,Label],[go,TB,Label]):-atomic(Tag),!.
label_atoms(TB,Instr,Instr).

trim_tagbody(InstrS,TInstrS):- append(Left,[R|_],InstrS),is_reflow(R,_),!,append(Left,[R],TInstrS).
trim_tagbody(InstrS,InstrS).












% asserta((fifteen(Val_Thru23):-!, []=[[]], LETENV=[[bv(val, [[]|_832])]], 
%   call_addr_block(Env,(symbol_setq(val, 1, _1398), tagbody_go(Result,'point-a')), [addr(Pred,'point-c', '$used',  (push_label('point-c'), sym_arg_val_env(val, Val_In, Val_Thru, LETENV), incf(Val_Thru, 4, Incf_Ret), tagbody_go(Result,'point-b'))), addr(Pred,'point-a', '$used',  (push_label('point-a'), push_label('point-d-unused'), sym_arg_val_env(val, Val_In12, Val_Thru13, LETENV), incf(Val_Thru13, 2, Incf_Ret14), tagbody_go(Result,'point-c'))), addr(Pred,'point-d-unused', '$unused',  (push_label('point-d-unused'), sym_arg_val_env(val, Val_In17, Val_Thru18, LETENV), incf(Val_Thru18, 2, Incf_Ret19), tagbody_go(Result,'point-c'))), addr(Pred,'point-b', '$used', ['point-b', [incf, val, 8]])], _GORES15), sym_arg_val_env(val, Val_In22, Val_Thru23, LETENV)))

tst:is_local_test(tagbody1,[tagbody,setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b))],[]).

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



