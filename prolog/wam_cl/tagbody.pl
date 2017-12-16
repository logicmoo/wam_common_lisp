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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(tagbody,[]).

:- set_module(class(library)).

:- include('header').

:- discontiguous(compile_body_go_tagbody/5).

shared_lisp_compiler:plugin_expand_progbody(Ctx,Env,Result,InstrS,_PreviousResult,Code):- 
  compile_body_go_tagbody(Ctx,Env,Result,InstrS,Code),!.


push_label(_,_,_).
% @IDEA we might use labels later 
compile_body_go_tagbody(_Ctx,_Env,Result,[label,Label,ID|Rest], PUSH ):-!, debug_var("_LABELRES",Result),
  nop(PUSH = push_label(Label,ID,Rest)),PUSH=true.
compile_body_go_tagbody(_Ctx,_Env,Result,[label,Label|Rest], PUSH ):-!, debug_var("_LABELRES",Result),
  nop(PUSH = push_label(Label,_,Rest)),PUSH=true.


tagbody_go(_TB,_Label,Pred,Env):- call(Pred,Env).
% GO TAG
compile_body_go_tagbody(_Ctx,Env,Result,[go,Label,TB,Pred],  Code ):- create_jump(TB,Label,Pred,Env,Code),!, debug_var("_GoThree",Result).
compile_body_go_tagbody(_Ctx,Env,Result,[go,Label,TB|_],  Code ):- create_jump(TB,Label,_Pred,Env,Code),!, debug_var("_GoTwo",Result).
compile_body_go_tagbody(_Ctx,Env,Result,[go,Label,TB], Code):- compute_new_address(TB,Label,Pred), debug_var("_GORES",Result),debug_var("GoEnv",Env),create_jump(TB,Label,Pred,Env,Code).
compile_body_go_tagbody(_Ctx,Env,Result,[go,Label], Code):- local_override(tagbody_scope,TB),compute_new_address(TB,Label,Pred), debug_var("_GORES",Result),debug_var("GoEnv",Env),create_jump(TB,Label,Pred,Env,Code).


add_context_code(_Ctx,Assertion):- !,dbmsg_real(:-assert_lsp(Assertion)).
/*
add_context_code(_Ctx,Assertion):- assert_lsp(Assertion),dbmsg(:-assert_lsp(Assertion)),!.
add_context_code(Ctx,Assertion):- 
  always((
  (nb_current_value(Ctx,symbol,Symbol);(toplevel=Symbol)),
  user:assert_lsp(Symbol,Assertion),dbmsg_real(:-assert_lsp(Symbol,Assertion)))),!.*/

compile_body_go_tagbody(Ctx,Env,[],[tagbody,Symbol| InstrSAll], Code):- 
  atom(Symbol),
  append(InstrS,[[go,Symbol]],InstrSAll),  
  \+ contains_var(Symbol,InstrS),
  \+ contains_var(go,InstrS),
  gensym(addr_tagbody_,TB),gensym(addr_enter_,Label),
  compile_tagbody(Ctx,Env,TB,[[go,Label],Label|InstrS],Clauses),
  compute_new_address(TB,Label,Pred),
  debug_var("TBEnv",Env),
  create_jump(TB,Label,Pred,Env,Code),
  maplist(add_context_code(Ctx),Clauses).

% TAGBODY
compile_body_go_tagbody(Ctx,Env,[],[tagbody| InstrS], Code):- 
  gensym(addr_tagbody_,TB),gensym(addr_enter_,Label),
  compile_tagbody(Ctx,Env,TB,[[go,Label],Label|InstrS],Clauses),
  compute_new_address(TB,Label,Pred),
  debug_var("TBEnv",Env),
  create_jump(TB,Label,Pred,Env,Code),
  maplist(add_context_code(Ctx),Clauses).
 
compile_tagbody(Ctx,Env,TB,InstrS,Clauses):-
 locally(local_override(tagbody_scope,TB),
 must_det_l((
   get_go_points(TB,InstrS,Gos),
   get_tags(TB,Env,InstrS,Gos,Addrs),  % check_missing_gos(Gos), 
   compile_addrs(TB,Ctx,Env,_Result,Addrs),
   % copy_term
   =(Addrs,Addrs2),   
   must_maplist(compile_addresses(TB),Addrs2,Clauses)))).
  % Code = call_addr_block(Env,CInstrS,Addrs2,Result))).


create_jump(TB,Label,_UPred,Env,COUT):- compute_new_address(TB,Label,Pred),simplify_call(call(Pred,Env),COUT),!.
create_jump(_TB,_Label,Pred,Env,COUT):- simplify_call(call(Pred,Env),COUT),!.
create_jump(TB,Label,Pred,Env,call(Pred,Env)):- compute_new_address(TB,Label,Pred).

simplify_call(call(Pred,Env),COUT):- atom(Pred),!, COUT=..[Pred,Env].
simplify_call(COUT,COUT).

compute_new_address(_, Label,Pred):- atom_concat_or_rtrace('block_exit_',_,Label),!,Pred=Label.
compute_new_address(TB,Label,Pred):- always(atomic_list_concat([TB,Label],'_',Pred)).


 /*
 ADDRESS TERM LOOK LIKE

%         addr(addr_tagbody_1_u_point_b,
%              u_point_b,
%              '$used',
%              _32692,
%
%              [ [print, "(incf val 08)"],
%                [u_prolog_trace],
%                [incf, u_val, 8],
%                [print, u_val],
%                [go, u_point_a]
%              ])

 COMPILES TO

 addr_tagbody_1_u_point_b(ENV) :-
       cl_print("(incf val 08)", _Print_Ret10),
       trace,
       set_place(ENV, incf, u_val, [8], _Incf_Ret11),
       get_var(ENV, u_val, U_Val_Res),
       cl_print(U_Val_Res, U_Val_Res),
       addr_tagbody_1_u_point_a(ENV).

 */
compile_addresses(_TB,Addr,(Head:-Body)):- 
 Addr = addr(Pred,_Label,_MaybeUsed,Incf_Env,Code),
 var(Incf_Env),( Env = Incf_Env),!,
 Head=..[Pred,Env],
 (Body = (Code)).

compile_addresses(_TB,Addr,(Head:-Body)):-
 Addr = addr(Pred,_Label,_MaybeUsed,Incf_Env,Code),
 Head=..[Pred,Env],
 (Body = (( Env = Incf_Env),Code)).



is_reflow([OP|ARGS],Label):- is_reflow(OP,ARGS,Label).
is_reflow(OPARGS,Label):- OPARGS=..[OP|ARGS],is_reflow(OP,ARGS,Label).
is_reflow('go',[Label|_],Label).
is_reflow('cl_go',[Label|_],Label).
is_reflow('tagbody_go',[Label|_],Label).
is_reflow('gosub',[Label|_],Label).
is_reflow('return',_,[]).
is_reflow(OP,[Label|_],Label):- same_symbol(OP,'return-from').
is_reflow('throw',[Label|_],Label).


is_label(Atom,Atom):- atomic(Atom),!,Atom\==[].
is_label([OP|ARGS],Label):- is_label(OP,ARGS,Label).
is_label(OPARGS,Label):- OPARGS=..[OP|ARGS],is_label(OP,ARGS,Label).
is_label('enter',[Label|_],Label).
is_label('exit',[Label|_],Label).
is_label('label',[Label|_],Label).

is_branched([Op|_]):- fail,member(Op,[if,or,and,progn]).


get_go_points(TB,[FlowInst|InstrS],[addr(Pred,Label,'$used','$missing','$missing')|Addrs]):- 
  is_reflow(FlowInst,Label),!,
  compute_new_address(TB,Label,Pred),
  get_go_points(TB,InstrS,Addrs).
get_go_points(_TB,[],[]).
get_go_points(TB,[I|InstrS],Addrs):-% #branching call
  is_branched(I),get_go_points(TB,I,IAddrs),
  get_go_points(TB,InstrS,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_go_points(TB,[_|InstrS],Addrs):-
  get_go_points(TB,InstrS,Addrs).

get_tags(TB,Env,[Label|InstrS],Gos,[GAddrs|Addrs]):- atomic(Label),
  member(GAddrs,Gos),GAddrs=addr(_Pred,Label,_Used,_Env_,_Missing), !,
   compute_new_address(TB,Label,Pred),
   setarg(1,GAddrs,Pred),
   setarg(4,GAddrs,Env),
   setarg(5,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,Gos,Addrs).
get_tags(TB,Env,[Label|InstrS],Gos,[GAddrs|Addrs]):-
  member(GAddrs,Gos),GAddrs=addr(_Pred,Label,_Used,_Env_,_Missing),
   compute_new_address(TB,Label,Pred),
   setarg(1,GAddrs,Pred),
   setarg(4,GAddrs,Env),
   setarg(5,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,Gos,Addrs).
get_tags(TB,Env,[TagInstr|InstrS],Gos,[GAddrs|Addrs]):- is_label(TagInstr,Label),
   GAddrs = addr(_Pred,Label,'$unused','$env',InstrS),
    compute_new_address(TB,Label,Pred),
    setarg(1,GAddrs,Pred),
    setarg(4,GAddrs,Env),
    setarg(5,GAddrs,InstrS),
  get_tags(TB,Env,InstrS,[GAddrs|Gos],Addrs).
get_tags(_TB,_Env,[],_,[]).
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
compile_addrs(_TB,_Ctx,_Env,_Result,_).

compile_addr1(TB,Ctx,_Env,Result,A):- A= addr(_Pred,Label,_Unused,_E,InstrS),   
   always(compile_tagbodys(TB,Ctx,NewEnv,Result,InstrS,Code)),
   compute_new_address(TB,Label,Pred),
   setarg(1,A,Pred),
   setarg(4,A,NewEnv),
   setarg(5,A,Code),!.
compile_addr1(_TB,_Ctx,_Env,_Result,_):- !.


compile_tagbodys(TB,Ctx,Env,Result,[enter(_)|InstrS],BInstrS):- !,
  compile_tagbodys(TB,Ctx,Env,Result,InstrS,BInstrS).
compile_tagbodys(TB,Ctx,Env,Result,InstrS,BInstrS):-
   maplist(label_atoms(TB),InstrS,TInstrS),
   trim_tagbody(TInstrS,CInstrS),
   compile_forms(Ctx,Env,Result,CInstrS,BInstrS).

label_atoms(Instr,[label,Label]):- is_label(Instr,Label),!.
label_atoms(TB,Label,[label,Label,TB]):-atomic(Label),!.
%label_atoms(TB,[go,Label],[go,Label,TB]):-atomic(Label),!.
% label_atoms(TB,[return_from|Label],[return_from|Label]):-!.
label_atoms(_TB,Instr,Instr).

trim_tagbody(InstrS,TInstrS):- append(Left,[R|_],InstrS),is_reflow(R,_),!,append(Left,[R],TInstrS).
trim_tagbody(InstrS,InstrS).



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
 (tagbody )
`,[]).

tst:is_local_test(`
 (tagbody 1 (print "hi" ))
`,[]).

% should loop
tst:is_local_test(`
 (tagbody 1 (print "hi" ) (go 1))
`,[]).

% (compile (tagbody 1 (print "hi" ) (go 1)))

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
      ;; (prolog-trace)
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









USER> 
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


#|

:- lisp_compile( pkg_user,
                [ let,
                  [u_val],

                  [ tagbody,
                    [setq, u_val, 1],
                    [go, u_point_a],
                    [incf, u_val, 16],
                    u_point_c,
                    [incf, u_val, 4],
                    [go, u_point_b],
                    [incf, u_val, 32],
                    u_point_a,
                    u_point_u,
                    [incf, u_val, 2],
                    [go, u_point_c],
                    [incf, u_val, 64],
                    u_point_b,
                    [incf, u_val, 8]
                  ],
                  u_val
                ]).


 addr_tagbody_1_addr_enter_1(ENV) :-
       set_var(ENV, setq, u_val, 1),
       addr_tagbody_1_u_point_a(ENV).

 addr_tagbody_1_u_point_c(ENV) :-
       set_place(ENV, incf, u_val, [4], _Incf_R),
       addr_tagbody_1_u_point_b(ENV).

 addr_tagbody_1_u_point_a(ENV) :-
       push_label(u_point_u, addr_tagbody_1, []),
       set_place(ENV, incf, u_val, [2], _Incf_R5),
       addr_tagbody_1_u_point_c(ENV).

 addr_tagbody_1_u_point_u(ENV) :-
       set_place(ENV, incf, u_val, [2], _Incf_R8),
       addr_tagbody_1_u_point_c(ENV).

 addr_tagbody_1_u_point_b(ENV) :-
       set_place(ENV, incf, u_val, [8], _Incf_R3).


 :- TOPEnv=[[bv(u_val, [[]|_1330])]|toplevel],
   addr_tagbody_1_addr_enter_1(TOPEnv),
   get_var(TOPEnv, u_val, U_val_Get).

|#
15
>

`
