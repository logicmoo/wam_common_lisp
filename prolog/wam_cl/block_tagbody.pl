:- module(block_tagbody,
  [call_block_interp/2,
   call_block_compiled/2]).


:- dynamic(shared_lisp_compiler:plugin_expand_function_body/5).
:- multifile(shared_lisp_compiler:plugin_expand_function_body/5).
:- discontiguous(shared_lisp_compiler:plugin_expand_function_body/5).


/*
[tagbody, 
   Named0 = (c0,o0,d0,e0),
   Named1 = code1,
   named2 = (co2,de2) ):-

     


 (tagbody
   (setq val 2)
   (go lp)
   (incf val 3)
   lp (incf val 4)) 
=> 6
     */

:-user:ensure_loaded('lisp-interpreter').

% :- thread_local(t_l:btb/2).
:- discontiguous(t_l:btb/2).

/*

Interpretor...

*/

call_block_interp(Name,Value):- 
  btba(Name,InstrS,Addrs),
  call_instructions_pc(0,InstrS,Addrs,Value).

btba(Name,InstrS,Addrs):-
   t_l:btb(Name,InstrS),
   must_or_rtrace(get_addrs(0,InstrS,Addrs)).

% TODO - dont bother recording adresses until after the first 'GO'/1
get_addrs(_,[],[]):-!.
get_addrs(N,[TagInstr|InstrS],[addr(Label,N)|Addrs]):- is_label(TagInstr,Label),!,
  N1 is N + 1,
  get_addrs(N1,InstrS,Addrs).
get_addrs(N,[_|InstrS],Addrs):- 
  N1 is N + 1,
  get_addrs(N1,InstrS,Addrs).
  

call_instructions_pc(PC,InstrS,Addrs,Value):- 
   nth0(PC,InstrS,I)->call_i_pc(I,PC,InstrS,Addrs,Value);true.

% #:LABEL allows rearrangments and address changes
call_i_pc(TagInstr,PC,InstrS,Addrs,Value):- is_label(TagInstr,Label),!,
   PC2 is PC + 1,
   call_instructions_pc(PC2,InstrS,[addr(Label,PC)|Addrs],Value).
% #:GO 
call_i_pc([go,Label],_PC,InstrS,Addrs,Value):-!,
   must_or_rtrace(member(addr(Label,Where),Addrs)),
   call_instructions_pc(Where,InstrS,Addrs,Value).
% #:RETURN
call_i_pc('return-from'(_,Value),_PC,_Instr,_Addrs,Value):-!.
% #normal call
call_i_pc(I,PC,InstrS,Addrs,Value):-!,
   call(I),
   PC2 is PC + 1,
   call_instructions_pc(PC2,InstrS,Addrs,Value).

/* testing */

t_l:btb(block1,[setq(B,2),[go,tag1],setq(A,1),(tag1),setq(A,4),plus(A,B,C),writeln(C)]).
t_l:btb(block2,[setq(B,2),[go,tag2],setq(A,1),(tag1),setq(A,4),plus(A,B,C),writeln(C),'return-from'(block2,C),(tag2),setq(A,4),[go,tag1]]).


/*

Compiler...

TODO: This might be rewritten to not use a numbers as addresses
Instead simply grab the List''s reference at some numerical points

 (let (val)
    (tagbody
[      (setq val 1)
*      (go point-a)]
      (incf val 16)
[     point-c
      (incf val 04)
*      (go point-b)]
      (incf val 32)
[    point-a
      (incf val 02)
*      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val)
=>  15


    Tagbody = 
     (setq(val,1)
      call_then_return(Point_A),
      incf(val,16),
      Point_C =
      (incf(val,04),
       call(Point_B),
       incf(val,32)),
     Point_A =
      (incf(val,02)
      call_then_return(Point_C),
      incf(val,64))
     Point_B =
      incf(val,08),
      nb_current(var,Value),
      'return-from'([],Value,Result)),
    catch(TagBody,'return-from'(_,Value),true).


*/

:-user:ensure_loaded(lisp_compiler).


call_block_compiled(Name,Value):- 
  must_or_rtrace(compile_btba(Name,Code)),
  catch(Code,'return-from'(_,Value),true).

compile_btba(Name,Code):-
   t_l:btb(Name,InstrS),
   compile_tagbody([]:[],[],_Result,InstrS,Code).

compile_tagbody_forms(Ctx,Env,Result,[begin(_)|InstrS],BInstrS):- !,compile_tagbody_forms(Ctx,Env,Result,InstrS,BInstrS).
compile_tagbody_forms(Ctx,Env,Result,InstrS,BInstrS):-
   maplist(label_atoms(),InstrS,TInstrS),
   trim_tagbody(TInstrS,CInstrS),
   compile_forms(Ctx,Env,Result,CInstrS,BInstrS).

trim_tagbody(InstrS,TInstrS):- append(Left,[R|_],InstrS),is_reflow(R,_),!,append(Left,[R],TInstrS).
trim_tagbody(InstrS,InstrS).

%label_atoms(Instr,[label,Tag]):- is_label(Instr,Tag),!.
label_atoms(Tag,[label,Tag]):-atomic(Tag),!.
label_atoms(Instr,Instr).

shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_tagbody_hook(Ctx,Env,Result,InstrS,Code),!.

compile_tagbody_hook(_Ctx,_Env,Result, nop(X),  nop(X)):- !, debug_var("_NopResult",Result).
compile_tagbody_hook(_Ctx,_Env,Result,[label, Tag], push_label(Tag) ):- debug_var("_LABELRES",Result).
compile_tagbody_hook(_Ctx,_Env,Result,end( Tag), push_label(end( Tag)) ):- debug_var("_GORES",Result).
compile_tagbody_hook(_Ctx,_Env,Result,begin( Tag), push_label(begin( Tag)) ):- debug_var("_GORES",Result).
compile_tagbody_hook(Ctx,Env,Result,[tagbody| InstrS], Code):- debug_var("_TBResult",Result),!,
  compile_tagbody(Ctx,Env,Result,InstrS,Code).

% goto(Label,Value,Env)
compile_tagbody_hook(_Ctx,Env,Result,[go, Tag], goto(Tag,[],Env) ):- debug_var("_GORES",Result),debug_var("GoEnv",Env).
compile_tagbody_hook(Ctx,Env,GoResult,['return-from',Tag,ValueForm], (ValueBody, goto(exit(Tag),ValueResult,Env)) ):- 
  compile_body(Ctx,Env,ValueResult,ValueForm, ValueBody),
  debug_var("_GORES",GoResult),
  debug_var("RetVal",ValueResult).

compile_tagbody_hook(Ctx,Env,Result,['return',Value],Body):-!,compile_tagbody_hook(Ctx,Env,Result,['return-from',[],Value],Body).
compile_tagbody_hook(Ctx,Env,Result,return(Value), Body ):- !,compile_tagbody_hook(Ctx,Env,Result,[return, Value],Body).
compile_tagbody_hook(Ctx,Env,Result,go(Tag), Body ):- !,compile_tagbody_hook(Ctx,Env,Result,[go, Tag],Body).


compile_tagbody(Ctx,Env,Result,InstrS,Code):- compile_tagbody_real(Ctx,Env,Result,InstrS,Code),!.
/*compile_tagbody(Ctx,Env,Result,InstrS,Code):- Tag= tagbody,          &optional
 append([[go,begin(Tag)],begin(Tag)|InstrS],[[go,end(Tag)],end(Tag)],WInstrS),
 compile_tagbody_real(Ctx,Env,Result,WInstrS,Code).
*/


shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,[block,Tag|InstrS], Code):- 
  compile_block(Ctx,Env,Result,Tag,InstrS,Code),!.

compile_block(Ctx,Env,Result,Tag,InstrS,Code):-
 append([[go,begin(Tag)],begin(Tag)|InstrS],[[go,end(Tag)],end(Tag)],WInstrS),
 compile_tagbody_real(Ctx,Env,Result,WInstrS,Code).

block_tagbody_test(0.0):- 
 local_test_2(
"(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one))", 4).

block_tagbody_test(0.1):- 
 local_test_2(
"(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))     
      ((= 3 temp-two) temp-one))",  3).

shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,[do,LoopVars,[EndTest|ResultForms]|TagBody], Code):- 
   loop_vars_to_let_n_step(LoopVars,LetVars,[],PSetQStepCode),
   gensym(dosym,Tag),
   must_compile_body(Ctx,Env,Result,
     [block,[],
       [let,LetVars,
          [tagbody,
            [label,Tag],
            [if,
              EndTest,
               ['return-from',[],[progn|ResultForms]],
               [progn,[progn|TagBody],[psetq|PSetQStepCode]]
            ],
            go(Tag)]
       ]
     ],  Code).


 
 loop_vars_to_let_n_step([],[],InOut,InOut).
loop_vars_to_let_n_step([Decl|LoopVars],[Norm|LetVars],In,Out):-
  must_or_rtrace(loop_1var_n_step(Decl,Norm,More)),
  append(In,More,Mid),
  loop_vars_to_let_n_step(LoopVars,LetVars,Mid,Out).

% loop_1var_n_step([bind, Variable, Form],[bind, Variable, Form],[]).
loop_1var_n_step([Variable, Form, Step],[bind, Variable, Form],[Variable,Step]).
loop_1var_n_step([Variable, Form],[bind, Variable, Form],[]).
loop_1var_n_step(Variable,[bind, Variable, []],[]).


compile_tagbody_real(Ctx,Env,Result,InstrS,Code):-
   must_or_rtrace(get_go_points(InstrS,Gos)),
   must_or_rtrace(get_tags(InstrS,Gos,Addrs)), 
   check_missing_gos(Gos),   
   compile_addrs(Ctx,Env,Result,Addrs),
   compile_tagbody_forms(Ctx,Env,Result,InstrS,CInstrS),
   copy_term(CInstrS,Copy),
   Code = call_addr_block(Env,Copy,Addrs,Result).


call_addr_block(EnvIn,Start,Addrs,Result):-
  catch(Start,
      goto(Result,Label,Env),
           (must_or_rtrace(member(addr(Label,_,Where),Addrs)),
           copy_term(Where,Copy),
           call_addr_block([Env|EnvIn],Copy,Addrs,Result))).


goto(Label,Value,Env):-throw(goto(Label,Value,Env)).
push_label(_).
%'return-from'(Label,Result):-throw(goto(Result,exit(Label),Env)).
%return(G):- 'return-from'([],G).
call_then_return(G):- G,return([]).
setq(Var,X):-Var=X.
=(N1,N2,Result):- t_or_nil(=(N1,N2),Result). 

'1+'(N,R):- R is N + 1.
'1-'(N,R):- R is N - 1.

t_or_nil(G,Result):- G->Result=t;Result=[].

/*
call_addr_block(Env,[],'return-from'([]),_Result):-!.
% #:ATOM or #:LABEL allows rearrangments and address changes
call_addr_block(Env,[(TagInstr)|InstrS],Addrs,Result):- is_label(TagInstr,Label),!,
   call_addr_block(Env,InstrS,[addr(Label,'$late',InstrS)|Addrs],Result).
% #:GO 
call_addr_block(Env,[goto(Result,Label)|_],Addrs,Result):-!,
   must_or_rtrace(member(addr(Label,_,Where),Addrs)),
   copy_term(Where,Copy),
   call_addr_block(Env,Copy,Addrs,Result).
% #normal call
call_addr_block(Env,[I|InstrS],Addrs,Result):- call(I),
  call_addr_block(Env,InstrS,Addrs,Result).
*/

is_reflow([OP|ARGS],Label):- is_reflow(OP,ARGS,Label).
is_reflow(OPARGS,Label):- OPARGS=..[OP|ARGS],is_reflow(OP,ARGS,Label).
is_reflow('go',[Label|_],Label).
is_reflow('goto',[Label|_],Label).
is_reflow('gosub',[Label|_],Label).
is_reflow('return',_,[]).
is_reflow('return-from',[Label|_],Label).
is_reflow('throw',[Label|_],Label).

is_label(Atom,Atom):- atomic(Atom),!,Atom\==[].
is_label([OP|ARGS],Label):- is_label(OP,ARGS,Label).
is_label(OPARGS,Label):- OPARGS=..[OP|ARGS],is_label(OP,ARGS,Label).
is_label('begin',[Label|_],Label).
is_label('end',[Label|_],Label).
is_label('label',[Label|_],Label).

is_branched([Op|_]):- fail,member(Op,[if,or,and,progn]).



get_go_points([FlowInst|InstrS],[addr(Label,'$used','$missing')|Addrs]):- is_reflow(FlowInst,Label),!,
  get_go_points(InstrS,Addrs).
get_go_points([],[]).
get_go_points([I|InstrS],Addrs):-% #branching call
  is_branched(I),get_go_points(I,IAddrs),
  get_go_points(InstrS,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_go_points([_|InstrS],Addrs):-
  get_go_points(InstrS,Addrs).

get_tags([Label|InstrS],Gos,[GAddrs|Addrs]):- atomic(Label),
  member(GAddrs,Gos),GAddrs=addr(Label,_Used,_Missing), !,
  setarg(3,GAddrs,InstrS),
  get_tags(InstrS,Gos,Addrs).
get_tags([Label|InstrS],Gos,[GAddrs|Addrs]):-
  member(GAddrs,Gos),GAddrs=addr(Label,_Used,_Missing),
  setarg(3,GAddrs,[Label|InstrS]),
  get_tags(InstrS,Gos,Addrs).
get_tags([TagInstr|InstrS],Gos,[GAddrs|Addrs]):- is_label(TagInstr,Label),
  GAddrs = addr(Label,'$unused',InstrS),
  get_tags(InstrS,[GAddrs|Gos],Addrs).
get_tags([],_,[]).
get_tags([I|InstrS],Gos,Addrs):- % #branching call
  is_branched(I),get_tags(I,Gos,IAddrs),
  get_tags(InstrS,Gos,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_tags([_|InstrS],Gos,Addrs):-
  get_tags(InstrS,Gos,Addrs).

check_missing_gos(_).

% asserta((fifteen(Val_Thru23):-!, []=[[]], LETENV=[[bv(val, [[]|_832])]], 
%   call_addr_block(Env,(symbol_setq(val, 1, _1398), goto(Result,'point-a')), [addr('point-c', '$used',  (push_label('point-c'), sym_arg_val_env(val, Val_In, Val_Thru, LETENV), incf(Val_Thru, 4, Incf_Ret), goto(Result,'point-b'))), addr('point-a', '$used',  (push_label('point-a'), push_label('point-d-unused'), sym_arg_val_env(val, Val_In12, Val_Thru13, LETENV), incf(Val_Thru13, 2, Incf_Ret14), goto(Result,'point-c'))), addr('point-d-unused', '$unused',  (push_label('point-d-unused'), sym_arg_val_env(val, Val_In17, Val_Thru18, LETENV), incf(Val_Thru18, 2, Incf_Ret19), goto(Result,'point-c'))), addr('point-b', '$used', ['point-b', [incf, val, 8]])], _GORES15), sym_arg_val_env(val, Val_In22, Val_Thru23, LETENV)))


compile_addrs(Ctx,Env,Result,[A|Addrs]):-
  compile_addr1(Ctx,Env,Result,A),
  compile_addrs(Ctx,Env,Result,Addrs).
compile_addrs(_Ctx,_Env,_Result,_).

compile_addr1(Ctx,Env,Result,A):- A= addr(_Tag,_Unused,InstrS),   
   must_or_rtrace(compile_tagbody_forms(Ctx,Env,Result,InstrS,CInstrS)),
   setarg(3,A,CInstrS),!.
compile_addr1(_Ctx,_Env,_Result,_):- !.





t_l:btb(block3,[setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b)),'return-from'(block3,plus(a,b))]).


make_cont(G,Cont):-
  	reset(((   
          shift(mc(G))
	     ->  G
	     ;   true
	     )), mc(G), Cont).

reset_in_cond4(R):-
  make_cont((format(atom(R), 'Hello ~w', [X]);format(atom(R), 'Bye ~w', [X])),Cont),
   X = world,
   call(Cont).

loop_cont:-
   make_cont(writeln([x=X,y=Y]),Cont1),
   make_cont(once(number(Y)->X is Y+1;X=1),CalcX),
   make_cont(once(number(X)->Y is X+1;Y=1),CalcY),
   \+ \+ call(CalcY),
   call(CalcX),
   call(Cont1).


local_test_1(SExpression):- 
  as_sexp(SExpression,Expression),
  dbmsg(lisp_compile(Expression)),
  must_or_rtrace(lisp_compile(Result,Expression,Code)),
  dbmsg(Code),
  must_or_rtrace(call(Code)),
  dbmsg(result(Result)).

local_test_2(SExpression,Result):- 
  as_sexp(SExpression,Expression),
  dbmsg(lisp_compiled_eval(Expression)),
  must_or_rtrace(lisp_compile(Expression,Code)),
  dbmsg(Code),
  nop((must_or_rtrace(call(Code)),dbmsg(result(Result)))).

:- fixup_exports.

:- local_test_1(
"(defun let_simple ()
  (let (val)
    val))
    ").

:- local_test_1(
"(defun let_simple1 ()
  (let ((val 1))
    val))
    ").

   
:- local_test_1(
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
    ").

 % [let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],
   % 'point-a',[incf,val,2],[go,'point-c'],[incf,val,64], 
   % 'point-b',[incf,val,8]],val]


:- 
 local_test_2("(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one))",4).

:- 
 local_test_2("(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))     
      ((= 3 temp-two) temp-one))",  3).

:- forall(clause(block_tagbody_test(_N),B),B).

/*
   call_addr_block(Env,
     (symbol_setq(val, 1, _1398), goto(Result,'point-a')),
       [addr('point-c', '$used',  (sym_arg_val_env(val, Val_In, Val_Thru, LETENV), incf(Val_Thru, 4, Incf_Ret), goto(Result,'point-b'))), 
        addr('point-a', '$used',  (push_label('point-d-unused'), sym_arg_val_env(val, Val_In12, Val_Thru13, LETENV), incf(Val_Thru13, 2, Incf_Ret14), goto(Result,'point-c'))), 
        addr('point-d-unused', '$unused',  (sym_arg_val_env(val, Val_In17, Val_Thru18, LETENV), incf(Val_Thru18, 2, Incf_Ret19), goto(Result,'point-c'))), 
        addr('point-b', '$used', ['point-b', [incf, val, 8]])], _GORES15),
   sym_arg_val_env(val, Val_In22, Val_Thru23, LETENV)

      */

