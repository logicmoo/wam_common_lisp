/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:

 * Need ot decide if BLOCK exits will use throw/1 or if they will be able to use tagbody GOs 
   (which are implemented as calls to predicate closures )

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


shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_body_block(Ctx,Env,Result,InstrS,Code),!.

%compile_body_block(_Ctx,_Env,Result,exit( Tag), push_label(exit( Tag)) ):- debug_var("_GORES",Result).
%compile_body_block(_Ctx,_Env,Result,enter( Tag), push_label(enter( Tag)) ):- debug_var("_GORES",Result).
compile_body_block(Ctx,Env,Result,[do,LoopVars,[EndTest|ResultForms]|TagBody], Code):- 
   loop_vars_to_let_n_step(LoopVars,LetVars,[],PSetQStepCode),
   gensym(dosym,DoTag),
   must_compile_body(Ctx,Env,Result,
    [block,[],       
      [let,LetVars,
          [tagbody,
            [label,DoTag],
            [if,
              EndTest,
               [return_from,[],[progn|ResultForms]],
               [progn,[progn|TagBody],[psetq|PSetQStepCode]]
            ],
            [go,DoTag]]
       ]
     ],  Code).

compile_body_block(Ctx,Env,GoResult,[return_from,Tag,ValueForm], (ValueBody, goto(exit(Tag),ValueResult,Env)) ):- 
  compile_body(Ctx,Env,ValueResult,ValueForm, ValueBody),
  debug_var("_GORES",GoResult),
  debug_var("RetVal",ValueResult).

compile_body_block(Ctx,Env,Result,['return',Value],Body):-!,compile_body_block(Ctx,Env,Result,[return_from,[],Value],Body).
compile_body_block(Ctx,Env,Result,return(Value), Body ):- !,compile_body_block(Ctx,Env,Result,[return, Value],Body).

compile_body_block(Ctx,Env,Result,[block,BlockTag|InstrS], Code):- must(is_symbolp(BlockTag)), 
  must_compile_block(Ctx,Env,Result,BlockTag,InstrS,Code),!.

compile_block(Ctx,Env,Result,Tag,InstrS,Code):-
 append([[go,[enter,Tag]],[label,[enter,Tag]]|InstrS],[[go,[exit,Tag]],[label,[exit,Tag]]],WInstrS),
 must_compile_body(Ctx,Env,Result,WInstrS,Code),!.


tst:is_local_test(block3,
   [block,block3,setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b)),return_from(block3,plus(a,b))],6).

:- fixup_exports.

