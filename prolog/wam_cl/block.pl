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
:- module(block,[]).

:- set_module(class(library)).

:- include('header.pro').


tst:is_local_test(do(0.0),
"(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
      ((> (- temp-one temp-two) 5) temp-one))", 4).

tst:is_local_test(do(0.1),
"(do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))     
      ((= 3 temp-two) temp-one))",  3).


 
loop_vars_to_let_n_step([],[],InOut,InOut).
loop_vars_to_let_n_step([Decl|LoopVars],[Norm|LetVars],In,Out):-
  must_or_rtrace(loop_1var_n_step(Decl,Norm,More)),
  append(In,More,Mid),
  loop_vars_to_let_n_step(LoopVars,LetVars,Mid,Out).

% loop_1var_n_step([bind, Variable, Form],[bind, Variable, Form],[]).
loop_1var_n_step([Variable, Form, Step],[bind, Variable, Form],[Variable,Step]).
loop_1var_n_step([Variable, Form],[bind, Variable, Form],[]).
loop_1var_n_step(Variable,[bind, Variable, []],[]).



compile_prolog_call(_Ctx,Env,ResultOut,[],Body,BodyOut):-subst(Body,'$out',ResultOut,BodyMid),subst(BodyMid,'$env',Env,BodyOut).
compile_prolog_call(Ctx,Env,ResultOut,[R|Resolve],Body,(Code,BodyResolved)):-
  subst(Body,R,Result,BodyMid),
  must_compile_body(Ctx,Env,Result,R,Code),
  compile_prolog_call(Ctx,Env,ResultOut,Resolve,BodyMid,BodyResolved).



/*
TODO fix prolog_call

tst:is_local_test(block2,[block,block2,[tagbody,setq(b,2),[go,tag2],setq(a,1),(tag1),
                     prolog_call([a,b],plus(a,b,C)),prolog_call(writeln(C)),
                     'return-from'(block2,c),(tag2),setq(a,4),[go,tag1]]],6).
*/

shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_body_block(Ctx,Env,Result,InstrS,Code),!.


compile_body_block(_Ctx,_Env,_Result,call(Body), call(Body) ):-!.
compile_body_block(Ctx,Env,ResultOut,prolog_call(Body), call(BodyResolved) ):-
   compile_prolog_call(Ctx,Env,[],ResultOut,Body,BodyResolved),!.
compile_body_block(Ctx,Env,ResultOut,prolog_call(Resolve,Body), call(BodyResolved) ):-
   compile_prolog_call(Ctx,Env,Resolve,ResultOut,Body,BodyResolved),!.

compile_body_block(_Ctx,_Env,Result,exit( Tag), push_label(exit( Tag)) ):- debug_var("_GORES",Result).
compile_body_block(_Ctx,_Env,Result,enter( Tag), push_label(enter( Tag)) ):- debug_var("_GORES",Result).

compile_body_block(Ctx,Env,Result,[do,LoopVars,[EndTest|ResultForms]|TagBody], Code):- 
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

compile_body_block(Ctx,Env,GoResult,['return-from',Tag,ValueForm], (ValueBody, goto(exit(Tag),ValueResult,Env)) ):- 
  compile_body(Ctx,Env,ValueResult,ValueForm, ValueBody),
  debug_var("_GORES",GoResult),
  debug_var("RetVal",ValueResult).

compile_body_block(Ctx,Env,Result,['return',Value],Body):-!,compile_body_block(Ctx,Env,Result,['return-from',[],Value],Body).
compile_body_block(Ctx,Env,Result,return(Value), Body ):- !,compile_body_block(Ctx,Env,Result,[return, Value],Body).

compile_body_block(Ctx,Env,Result,[block,Tag|InstrS], Code):- 
  compile_block(Ctx,Env,Result,Tag,InstrS,Code),!.

compile_block(Ctx,Env,Result,Tag,InstrS,Code):-
 append([[go,enter(Tag)],enter(Tag)|InstrS],[[go,exit(Tag)],exit(Tag)],WInstrS),
 compile_as_tagbody(Ctx,Env,Result,WInstrS,Code).


tst:is_local_test(block3,
   [block,block3,setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b)),'return-from'(block3,plus(a,b))],6).

:- fixup_exports.

