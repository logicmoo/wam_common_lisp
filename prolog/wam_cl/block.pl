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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
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
   gensym_in_labels(dosym,DoTag),
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


compile_body_block(Ctx,Env,Result,RETURN_FROM,Body):- 
  p_or_s(RETURN_FROM,'return',Value),!,
  compile_body_block(Ctx,Env,Result,[return_from,[]|Value],Body).
compile_body_block(Ctx,Env,GoResult,[return_from,Tag],Code):- !,
   compile_body_block_in_tb(Ctx,Env,GoResult,[return_from,Tag,[]],Code).

%@TODO make this work all the time uncommented
%compile_body_block(Ctx,Env,Result,RETURN_FROM,Body):- 
% compile_body_block_in_tb(Ctx,Env,Result,RETURN_FROM,Body).

compile_body_block(Ctx,Env,Result,RETURN_FROM,Body):- 
 compile_body_block_in_throw(Ctx,Env,Result,RETURN_FROM,Body).


compile_body_block_in_throw(Ctx,Env,ValueResult,[RETURN_FROM,Tag,ValueForm],(ValueFormCode, Code) ):- 
   same_symbol(RETURN_FROM,'return-from'), 
   debug_var('RetResult',ValueResult),
   must_compile_body(Ctx,Env,ValueResult,ValueForm,ValueFormCode),
   %suffixed_atom_concat(block_ret_,Tag,Var),
   %suffixed_atom_concat(block_exit_,Tag,ExitTag),
  % compile_body(Ctx,Env,GoResult,[progn,[setq,Var,ValueResult],[go,ExitTag]], Code ).
   debug_var('BlockExitEnv',Env),
   Code = (throw(block_exit(Tag,ValueResult))).

compile_body_block_in_throw(Ctx,Env,Result,[block,BlockTag|InstrS], 
  catch((Code,ResultExit=Result),block_exit(BlockTag,Result),true)):-  % must(is_symbolp(BlockTag)),
      (must_compile_body(Ctx,Env,ResultExit,[progn|InstrS],Code)).

/*
compile_body_block_in_throw(Ctx,Env,GoResult,[RETURN_FROM,Tag,ValueForm],(ValueFormCode, Code) ):- 
   same_symbol(RETURN_FROM,'return-from'), 
   debug_var('GoBlockResult',GoResult),
   must_compile_body(Ctx,Env,ValueResult,ValueForm,ValueFormCode),
   %suffixed_atom_concat(block_ret_,Tag,Var),
   suffixed_atom_concat(block_exit_,Tag,ExitTag),
  % compile_body(Ctx,Env,GoResult,[progn,[setq,Var,ValueResult],[go,ExitTag]], Code ).
   debug_var('BlockExitEnv',Env),
   Code = (throw(block_exit(ExitTag,ValueResult))).

compile_body_block_in_throw(Ctx,Env,Result,[block,BlockTag|InstrS], 
  catch((Code,ResultExit=Result),block_exit(ExitTag,Result),true)):-  must(is_symbolp(BlockTag)),
   suffix_by_context(BlockTag,Tag),
   gensym('_labels',Suffix),
   must_or_rtrace(within_labels_context(Suffix,
   ((%suffixed_atom_concat(block_ret_,Tag,Var),
     suffixed_atom_concat(block_exit_,Tag,ExitTag), 
      (must_compile_body(Ctx,Env,ResultExit,[progn|InstrS],Code)))))),!.
*/



compile_body_block_in_tb(Ctx,Env,GoResult,[RETURN_FROM,Tag,ValueForm],(ValueFormCode, Code) ):- 
   same_symbol(RETURN_FROM,'return-from'), 
   debug_var('GoBlockResult',GoResult),
   must_compile_body(Ctx,Env,ValueResult,ValueForm,ValueFormCode),
   suffixed_atom_concat(block_ret_,Tag,Var),
   suffixed_atom_concat(block_exit_,Tag,ExitTag),
  % compile_body(Ctx,Env,GoResult,[progn,[setq,Var,ValueResult],[go,ExitTag]], Code ).
   debug_var('BlockExitEnv',Env),
   Code = (set_symbol_value(Env,Var,ValueResult),must_or_rtrace(ExitTag,Env),clean_escape(_)).


compile_body_block_in_tb(Ctx,Env,Result,[block,BlockTag|InstrS], Code):- must(is_symbolp(BlockTag)),
  suffix_by_context(BlockTag,Tag),
  gensym('_labels',Suffix),
  must_or_rtrace(within_labels_context(Suffix,
  ((suffixed_atom_concat(block_ret_,Tag,Var),
    suffixed_atom_concat(block_exit_,Tag,ExitTag), 
    BLOCK = 
      [let,[Var],[tagbody,[setq,Var,[progn|InstrS]],ExitTag],Var], 
     (must_compile_body(Ctx,Env,Result,BLOCK,Code)))))),!.






tst:is_local_test(block3,
   [block,block3,setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b)),return_from(block3,plus(a,b))],6).

:- fixup_exports.

