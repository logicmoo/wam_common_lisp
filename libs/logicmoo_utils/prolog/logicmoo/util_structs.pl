/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: logicmoo@gmail.com ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(logicmoo_util_structs,
          [ % attr_portray_hook/2,
            % attr_unify_hook/2,
            by_name_datatype/2,
            compile_argtypes/3,
            compile_struct_slots/6,
            datatype_to_init/2,
            decl_argtypes/1,
            decl_struct/1,
            ensure_instance/2,
            ensure_instance/3,
            ensure_struct/2,
            ensure_struct/3,
            extract_struct_parameter/4,
            extract_struct_parameter/5,
            hooked_gvar_get/2,
            hooked_gvar_put/2,
            if_changed_struct/3,
            key_match/2,
            member_arg_convert/5,
            %us:member_datatype/2,
            merge_values/3,
            nb_set_pairlist/3,
            nb_set_pairlist0/3,
            nb_set_s2list/4,
            nb_set_s2list0/4,
            nb_setarg_ex/3,
            new_struct/2,
            prop_get/1,
            prop_get/3,
            prop_get_map/3,
            prop_get_nvlist/2,
            prop_get_try/4,
            prop_merge/3,
            prop_put_extra_extra/2,
            prop_set/1,
            prop_set/3,
            prop_set_dict_real/4,
            prop_set_map/3,
            prop_set_nvlist/2,
            prop_set_try/4,
            record_onto_var/3,
            record_var_names/1,
            record_var_names/2,
            record_var_type/2,
            sisctus_key/2,
            struct_sclass/2,
            t2ot/2,
            t2ot_0/2,
            term_to_ord_term/2,
            to_datatype/3
          ]).
/** <module> Utility LOGICMOO_UTIL_STRUCTS
This module allows use of C++ like structures in prolog. 
- @author Douglas R. Miles
- @license LGPL
*/

:- meta_predicate
        sisctus_key(:, -).
:- module_transparent
        by_name_datatype/2,
        compile_argtypes/3,
        compile_struct_slots/6,
        datatype_to_init/2,
        decl_argtypes/1,
        decl_struct/1,
        ensure_instance/2,
        ensure_instance/3,
        ensure_struct/2,
        ensure_struct/3,
        extract_struct_parameter/4,
        extract_struct_parameter/5,
        hooked_gvar_get/2,
        hooked_gvar_put/2,
        if_changed_struct/3,
        key_match/2,
        member_arg_convert/5,
        %us:member_datatype/2,
        merge_values/3,
        %module_local_init /2,
        nb_set_pairlist/3,
        nb_set_pairlist0/3,
        nb_set_s2list/4,
        nb_set_s2list0/4,
        nb_setarg_ex/3,
        new_struct/2,
        prop_get/1,
        prop_get/3,
        prop_get_map/3,
        prop_get_nvlist/2,
        prop_get_try/4,
        prop_merge/3,
        prop_put_extra_extra/2,
        prop_set/1,
        prop_set/3,
        prop_set_dict_real/4,
        prop_set_map/3,
        prop_set_nvlist/2,
        prop_set_try/4,
        record_onto_var/3,
        record_var_names/1,
        record_var_names/2,
        record_var_type/2,
        struct_sclass/2,
        t2ot/2,
        t2ot_0/2,
        term_to_ord_term/2,
        to_datatype/3.

% :- dynamic module_local_init/2.

:- if(current_module(logicmoo_utils)).
:- public((
  prop_get/1,prop_get/3,prop_set/1,prop_set/3,prop_merge/3,
  prop_set_nvlist/2,
  decl_argtypes/1,
  decl_struct/1,
  %us:struct_decl/1,
  if_changed_struct/3,
  prop_get_nvlist/2,
  term_to_ord_term/2,
  new_struct/2,
  ensure_struct/2,ensure_struct/3,
  ensure_instance/2,ensure_instance/3)).
:- else.

:- endif.
:- set_module(class(library)).



:- ensure_loaded(library(record)).
:- ensure_loaded(library(rbtrees)).
:- ensure_loaded(library(ordsets)).

/*
default_point2d/1,
is _point2d/1,
make_point2d/2,
make_point2d/3,
nb_set_x_of_point2d/2,
nb_set_y_of_point2d/2,
point2d_data/3,
point2d_x/2,
point2d_y/2,
set_point2d_field/3,
set_point2d_fields/3,
set_point2d_fields/4,
set_x_of_point2d/2,
set_x_of_point2d/3,
set_y_of_point2d/2,
set_y_of_point2d/3,
*/
:- record(point2d(x:integer=0, y:integer=0)).
     /*
        default_point2d(Point),
        point2d_x(Point, X),
        set_x_of_point2d(10, Point, Point1),

        make_point2d([y(20)], YPoint),
   */

:- use_module(library(assoc)).
:- module_transparent(import_dynamic/1).
import_dynamic(M:F/A):- 
  multifile(M:F/A),
  dynamic(M:F/A),
  M:export(M:F/A),
  system:import(M:F/A),
  import(M:F/A).

:- import_dynamic(us:member_datatype/3).
:- import_dynamic(us:member_init/3).
:- import_dynamic(us:member_loc/3).
:- import_dynamic(us:struct_datatype/2).
:- import_dynamic(us:struct_decl/1).
:- import_dynamic(us:struct_names/2).
:- import_dynamic(us:struct_prototype/2).

% :- us:struct_datatype(_,_) -> true; true.


%= 	 	 

%% record_onto_var( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Record Onto Variable.
%
record_onto_var(AttribName,AV,Value):-
 ignore((
 atom(Value),var(AV), 
 ((get_attr(AV,lp,Dict),nonvar(Dict))-> true; (new_struct(map,Dict),put_attr(AV,lp,Dict))),
 (prop_get(AttribName,Dict,_)->true;prop_set(AttribName,Dict,AV)))).


%= 	 	 

%% record_var_names( :TermARG1) is semidet.
%
% Record Variable Names.
%
record_var_names(V):- \+ compound(V),!.
record_var_names(N=V):-!,record_var_names(V,N).
record_var_names(List):-is_list(List),!,maplist(record_var_names,List).
record_var_names(Comp):-functor(Comp,_,A),arg(A,Comp,E),!,record_var_names(E).


%= 	 	 

%% record_var_names( ?VALUE1, ?VALUE2) is semidet.
%
% Record Variable Names.
%
record_var_names(ATTVAR,Value):-record_onto_var(vn,ATTVAR,Value).



%= 	 	 

%% record_var_type( ?VALUE1, ?VALUE2) is semidet.
%
% Record Variable Type.
%
record_var_type(ATTVAR,Type):-record_onto_var(type,ATTVAR,Type).


/*

prop_get(Name,mutable(Dict),Value):-!,nonvar(Dict),prop_get(Name,Dict,Value).


?- 
  prop_get(uses_domain, problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
   [on(b,a),on(c,b)],[],[],[],extraprops{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
     length:[],metric:[],object:[block([a,b,c])],
      problem_filename:'/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
      problem_name:'blocks-3-0',requires:[],uses_domain:blocks}),X).



  ?-

   Y = problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],[on(b,a),
     on(c,b)],[],[],[],extraprops{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),
     ontable(b),ontable(c)],length:[],metric:[],object:[block([a,b,c])],problem_filename:
     '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
     problem_name:'blocks-3-0',requires:[],uses_domain:blocks}).

   ?- prop_get(init, $Y , O).

   ?- prop_merge(init, $Y , suey(y)).

      ?- prop_get(init, $Y , O).


   ?- prop_set(init, $Y , suey(y)).


*/
:-meta_predicate(sisctus_key(:,-)).

%= 	 	 

%% sisctus_key( ?CALL1, -IN2) is semidet.
%
% Sisctus Key.
%
sisctus_key(Module:Key, Atom) :- atom(Module), !,atomic(Key),atomic_list_concat([Module, Key], :, Atom).


%= 	 	 

%% prop_get( ?VALUE1) is semidet.
%
% Prop Get.
%
prop_get(Call):- Call=..[P,A,B],prop_get(P,A,B).


%= 	 	 

%% prop_get( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Prop Get.
%
prop_get(Name,Dict,Value):- (var(Name);var(Dict)),!,trace_or_throw(var_prop_get(Name,Dict,Value)).
prop_get(Name,Dict,Value):- nonvar(Value),!,must(prop_get(Name,Dict,ValueVar)),!,Value=ValueVar.
prop_get(_,     Dict, _ ):- ( \+ \+ Dict=[] ),!, fail.
prop_get(Name, Struct,  Value):- prop_get_try(Name, Struct,  Value, _),!.
prop_get(Name, Struct,  Value):- Name \= extraprops, prop_get(extraprops, Struct,  Extra),
                                              prop_get_try(Name, Extra,  Value, _),!.
prop_get(Name,_Struct, Value):-hooked_gvar_get(Name,  Value).


%= 	 	 

%% hooked_gvar_get( ?VALUE1, ?VALUE2) is semidet.
%
% Gvar Get.
%
% hooked_gvar_get(Name,  Value):- sisctus_key(Name,N),nb_current(N,ValueV),!,Value=ValueV.
hooked_gvar_get(Name,  Value):- nb_current(Name,Value),!.
hooked_gvar_get(_Name,  []):-!.


%= 	 	 

%% hooked_gvar_put( ?VALUE1, ?VALUE2) is semidet.
%
% Gvar Put.
%
% hooked_gvar_put(Name,  Value):- sisctus_key(Name,N),nb_current(N,_),!,nb_setval(N,Value).
hooked_gvar_put(Name,  Value):- nb_setval(Name,Value).




%= 	 	 

%% key_match( ?VALUE1, ?VALUE2) is semidet.
%
% Key Match.
%
key_match(Name,N):-atom(N), (Name=N -> true ; atom_concat(':',Name,N)).


%= 	 	 

%% prop_get_try( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Prop Get Try.
%
prop_get_try(_,     Dict, _  ,_ ):- ( \+ \+ Dict=[] ),!, fail.
prop_get_try(Name, bb,   Value, gvar(Name,Value)):- !, must(hooked_gvar_get(Name,Value)).
prop_get_try(_   , Atomic,  _  , _  ):- atomic(Atomic),!,fail.
prop_get_try(Name, Dict,   Value, Ref):- prop_get_map(Name, Dict, Value),!,must(Dict=Ref).
prop_get_try(Name, STRUCT,  Value, Ref):- STRUCT = mutable(Struct), !, prop_get_try(Name, Struct,  Value, Ref).


%= 	 	 

%% prop_get_map( ?VALUE1, :TermARG2, ?VALUE3) is semidet.
%
% Prop Get Map.
%
prop_get_map(_,    Dict,       _ ):- ( \+ \+ Dict=[] ),!, fail.
prop_get_map(Name, Struct,  Value):- is_list(Struct),memberchk(Name=Value,Struct).
prop_get_map(Name, Dict,    Value):- is_dict(Dict),!,get_dict(Name,Dict,Value).
prop_get_map(Name, Dict,    Value):- is_rbtree_t4(Dict),!,nb_rb_get_node(Dict,Name,Value).
prop_get_map(Name, Dict,    Value):- is_assoc(Dict),!,get_assoc(Dict,Name,Value).

prop_get_map(Name, Struct,  Value):- Name==sclass, compound(Struct),functor(Struct,Value,_),!.

prop_get_map(sclass, sterm(Type,_), Type).
prop_get_map(Name, sterm(_,LIST), Value):- append(_,[N,V|_],LIST),key_match(Name,N),!,V=Value.

prop_get_map(Indx, Struct,  Value):- integer(Indx),!, arg(Indx,Struct,Value).

prop_get_map(Name, Struct,  Value):- us:member_loc(StructName,Name,N), functor(Struct,StructName,_),!,
      must((integer(N) -> arg(N,Struct,Value); prop_get_map(Name, Struct,  Value))).




%= 	 	 

%% prop_put_extra_extra( ?VALUE1, ?VALUE2) is semidet.
%
% Prop Put Extra Extra.
%
prop_put_extra_extra(Struct,More):- must_det_l((prop_get(extraprops,Struct,Extras),prop_set(extraprops,Extras,More))).
  



%= 	 	 

%% prop_set( ?VALUE1) is semidet.
%
% Prop Set.
%
prop_set(Call):- Call=..[P,A,B],prop_set(P,A,B).



%= 	 	 

%% prop_set( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Prop Set.
%
prop_set(Name,Dict,Value):- (var(Name);var(Dict)),!,trace_or_throw(var_prop_set(Name,Dict,Value)).
prop_set(_,     Dict, _ ):- ( \+ \+ Dict=[] ),!, fail.
prop_set(Name,Dict,Value):- 
 member_arg_convert(Dict,Name,_,Value,NewValue) -> 
    must(((prop_set_try(Name,Dict,NewValue, NewDict ),NewDict==Dict)));
    must(((prop_set_try(Name,Dict,Value, NewDict ),NewDict==Dict))).


%= 	 	 

%% prop_set_try( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Prop Set Try.
%
prop_set_try(Name,Dict,Value,_):- (var(Name);var(Dict);var(Value)),!,trace_or_throw(var_prop_set(Name,Dict,Value)).
prop_set_try(_,    Dict,   _, _):- ( \+ \+ Dict=[] ),!, fail.
prop_set_try([Name],Dict,Value,NewDict):-!, prop_set_try(Name,Dict,Value, NewDict).
prop_set_try(Name, bb,   Value, _):- !, hooked_gvar_put(Name,Value).
prop_set_try(_,    Struct,   _, _):- ( \+ compound(Struct)),!,fail.
prop_set_try([Name,Last],Dict,Value,WasNewDict):- prop_get(Name,Dict,SubDict),prop_set_try(Last,SubDict,Value,NewSubDict),NewSubDict\==SubDict,prop_set_try(Name,Dict,NewSubDict,WasNewDict),!.
prop_set_try([Name|More],Dict,Value,WasNewDict):-prop_get(Name,Dict,SubDict),prop_set_try(More,SubDict,Value,NewDict),NewDict\==SubDict,prop_set_try(Name,Dict,NewDict,WasNewDict).

prop_set_try( Name,Dict,Value, Dict):- Name = sclass, functor(Dict,F,_), F==Value,!.
prop_set_try( Name,Dict,Value, Dict):-  prop_set_map(Name,Dict,Value),!.
prop_set_try( Name,Dict,Value, NewDict) :- is_dict(Dict),!,prop_set_dict_real(Name,Dict,Value,NewDict).



%= 	 	 

%% prop_set_map( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Prop Set Map.
%
prop_set_map(Name,Dict,Value):- (var(Name);var(Dict);var(Value)),!,trace_or_throw(var_prop_set_map(Name,Dict,Value)).
prop_set_map(sclass, STERM, Type):- STERM=sterm(Type,_), nb_setarg_ex(1,STERM,Type).
prop_set_map(Name, STERM, Value):- STERM=sterm(_,List),
  nb_set_s2list(Name,List,Value,NewList),(List\==NewList -> nb_setarg_ex(2,STERM,NewList) ; true).

prop_set_map(Name,HDict,Value):- is_list(HDict), memberchk(sclass=_,HDict),!,nb_set_pairlist(Name,HDict,Value).

prop_set_map(Name,HDict,Value):- compound(HDict), HDict = mutable(Dict),
   (Dict == [] -> nb_setarg(1,HDict,[Name=Value]) ;
    must_det_l((prop_set_try(Name,Dict,Value,NewDict),(Dict == NewDict -> true ; 
     (must(nonvar(NewDict)),nb_setarg_ex(1,HDict,NewDict)))))).

prop_set_map(Name,Dict,Value):- is_rbtree_t4(Dict),!,nb_rb_insert(Name,Dict,Value).
prop_set_map(Name,List,Value):- is_list(List), !, nb_set_pairlist(Name,List,Value).
prop_set_map(Index,Dict,Value):- integer(Index),!, nb_setarg_ex(Index,Dict,Value).
prop_set_map(Name,Dict,Value):- functor(Dict,StructName,_),
   (us:member_loc(StructName,Name,N) -> nb_setarg_ex(N,Dict,Value);
     must_det_l((prop_get(extraprops,Dict,Extra),nonvar(Extra),prop_set(Name,Extra,Value)))).





%= 	 	 

%% prop_set_dict_real( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE2) is semidet.
%
% Prop Set Dict Real.
%
prop_set_dict_real(Name,Dict,Value, Dict):-  get_dict(Name,Dict,Old),!, (Value==Old -> (!); ((nb_set_dict(Name,Dict,Value)))),!.
prop_set_dict_real(Name,Dict,Value, NewDict):- put_dict(Name,Dict,Value,NewDict).


%= 	 	 

%% nb_set_pairlist( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Non Backtackable Set Pairlist.
%
nb_set_pairlist(Name,List,Value):- must(List=[_|_]), must(nb_set_pairlist0(Name,List,Value)).

%= 	 	 

%% nb_set_pairlist0( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Non Backtackable Set Pairlist Primary Helper.
%
nb_set_pairlist0(Name,List,Value):- 
     List = [PName=_|T],!,
          ((PName==Name -> nb_setarg_ex(1,List,Name=Value) ;
           T == [] -> nb_setarg_ex(2,List,[Name=Value]) ;
           nb_set_pairlist0(Name,T,Value))).


%= 	 	 

%% nb_set_s2list( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Non Backtackable Set S2list.
%
nb_set_s2list(Name,List,Value,NewList):- must(List=[_|_]), must(nb_set_s2list0(Name,List,Value,NewList)).


%= 	 	 

%% nb_set_s2list0( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE2) is semidet.
%
% Non Backtackable Set S2list Primary Helper.
%
nb_set_s2list0(Name,[],Value,[Name,Value]).
nb_set_s2list0(Name,LIST,Value,LIST):- append(_,[N|REST],LIST),key_match(Name,N),nb_setarg_ex(1,REST,Value),!.
nb_set_s2list0(Name,LIST,Value,LIST):- LIST = [_|A2REST],A2REST=[_|REST],nb_setarg_ex(2,A2REST,[Name,Value|REST]).

nb_set_s2list0(Name,LIST,Value,NEWLIST):- 
   ((append(_,[N|REST],LIST),key_match(Name,N)) -> (nb_setarg_ex(1,REST,Value),NEWLIST=LIST);
   append(LIST,[Name,Value],NEWLIST)).



%= 	 	 

%% prop_merge( :TermARG1, ?VALUE2, ?VALUE3) is semidet.
%
% Prop Merge.
%
prop_merge([Name],Struct,Value):-!, prop_merge(Name,Struct,Value).
prop_merge([Name|More],Struct,Value):-!, prop_get(Name,Struct,Ref),prop_merge(More,Ref,Value).
prop_merge(Name,Struct,ValueIn):- term_to_ord_term(ValueIn,Value),
   (prop_get(Name,Struct,Old) -> merge_values(Old,Value,New) ;  Value=New),
   prop_set(Name,Struct,New),!.


% term_to_ord_term(+Term, -OrdTerm)
%
%   Go throught the term and look for sets, return the same term
%   with all sets become ordered.
%


%= 	 	 

%% term_to_ord_term( ?VALUE1, ?VALUE2) is semidet.
%
% Term Converted To Ord Term.
%
term_to_ord_term(Term, OrdTerm):-t2ot(Term, OrdTerm).


%= 	 	 

%% t2ot( ?VALUE1, ?VALUE1) is semidet.
%
% T2ot.
%
t2ot(A, A):- \+ compound(A), !.
t2ot(vv(T), T):-!.
t2ot(T, OTO):-t2ot_0(T, OT),(T==OT->OTO=T;OTO=OT).



%= 	 	 

%% t2ot_0( ?VALUE1, ?VALUE2) is semidet.
%
% t2ot  Primary Helper.
%
t2ot_0([H|T], R):-
    t2ot(H, OH),
    t2ot(T, OT),
    ord_add_element(OT, OH, R),
    !.
%    write(OH), write(OT), write('   '), write(R), nl.

t2ot_0(T, OT):-
    T =.. [F,P],
    !,
    t2ot(P, OP),
    OT =..[F,OP].
t2ot_0(T, OT):-
    T =.. [F,P|Ps],
    NT=.. [F|Ps],
    t2ot(P, OP),
    t2ot(NT, ONT),
    ONT =.. [_|OPs],
    OT =.. [F,OP|OPs],
    !. 



%= 	 	 

%% merge_values( ?VALUE1, ?VALUE3, ?VALUE3) is semidet.
%
% Merge Values.
%
merge_values(Var,Value,Value):-var(Var),!.
merge_values([], Value,Value).
merge_values(Old,Value,Value):-Old==Value,!.
merge_values(Old,Value,New):-is_list(Old),!,(is_list(Value)->ord_union(Old,Value,New);ord_add_element(Old,Value,New)).
merge_values(Old,Value,[Value,Old]).


%= 	 	 

%% nb_setarg_ex( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Non Backtackable Setarg Ex.
%
nb_setarg_ex(Name,Struct,New):-(var(Name);var(Struct);var(New)),!,trace_or_throw(var_prop_set_map(Name,Struct,New)).
nb_setarg_ex(Name,Struct,New):-arg(Name,Struct,Old),nb_setarg(Name,Struct,New),ignore(Old=New).


%= 	 	 

%% us:member_datatype( ?VALUE1, ?VALUE2) is semidet.
%
% Member Datatype.
%
us:member_datatype(prototype,compound).


%= 	 	 

%% by_name_datatype( ?VALUE1, ?VALUE2) is semidet.
%
% By Name Datatype.
%
by_name_datatype(init, sorted).
by_name_datatype(goal, sorted).
by_name_datatype(assign_effect, sorted).
by_name_datatype(effects, sorted).
by_name_datatype(negativ_effect, sorted).
by_name_datatype(positiv_effect, sorted).
by_name_datatype(preconditions, sorted).


:- functor(t{a:t},A,_),asserta(dict_functor(A)).


% member_arg_convert(+StructName,+Name,?N,+Value,-NewValue).


%= 	 	 

%% struct_sclass( ?VALUE1, ?VALUE2) is semidet.
%
% Struct Sclass.
%
struct_sclass(sterm(SC,_),SC).
struct_sclass(mutable(Struct),SC):-!,struct_sclass(Struct,SC).
struct_sclass([sclass=SC|_],SC).
struct_sclass([],any).
struct_sclass(Struct,SC):-prop_get(sclass,Struct,SC).
struct_sclass(Struct,SC):-functor(Struct,F,_),(dict_functor(F)->prop_get(sclass,Struct,SC);SC=F).


%= 	 	 

%% member_arg_convert( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4, ?VALUE4) is semidet.
%
% Member Argument Convert.
%
member_arg_convert( _,Name,_,Value,Value):-var(Name),!.
member_arg_convert(_,Name,_N,Value,NewValue):-by_name_datatype(Name,Type),!,to_datatype(Type,Value,NewValue).
member_arg_convert(_,varnames,_N,Value,Value):-!.
member_arg_convert(Struct,Name,N,Value,NewValue):- \+ \+ (Struct=[] ),!,member_arg_convert(any,Name,N,Value,NewValue).
member_arg_convert(Struct,Name,N,Value,NewValue):- \+atom(Struct),!, struct_sclass(Struct,SC),!,member_arg_convert(SC,Name,N,Value,NewValue).
member_arg_convert(uppercase_string,charAt(_),_,Char,Converted):-to_upper(Char,Converted).
member_arg_convert(StructName,Name,_N,Value,NewValue):-us:member_datatype(StructName,Name,Type),to_datatype(Type,Value,NewValue).
member_arg_convert(StructName,_Name,N,Value,NewValue):-us:struct_datatype(StructName,ArgTypes),arg(N,ArgTypes,Datatype),to_datatype(Datatype,Value,NewValue).
member_arg_convert(_Type,Datatype,_,Value,NewValue):-to_datatype(Datatype,Value,NewValue).
member_arg_convert(_Type,_Named,_,UnConverted,UnConverted).



%= 	 	 

%% if_changed_struct( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% If Changed.
%
if_changed_struct(Value,NewValue,NewValueO):- must((NewValue=@=Value -> NewValueO=Value ; NewValueO=NewValue)).
%if_changed_struct(Ex,I,O):- call(Ex,I,O)-> I\==O.


%= 	 	 

%% to_datatype( ?Type, ?Value, ?Value) is semidet.
%
% Converted To Datatype.
%
to_datatype(=,Value,Value).
to_datatype(sorted,Value,NewValueO):-term_to_ord_term(Value,NewValue),!,if_changed_struct(Value,NewValue,NewValueO).
to_datatype(_Type,Value,Value).



%= 	 	 

%% decl_struct( ?VALUE1) is semidet.
%
% Declare Struct.
%
decl_struct(StructDecl):- 
  must_det_l((
    compile_argtypes(StructDecl,1,StructPrototype),    
    functor(StructPrototype,StructName,_),
    show_call(ain(us:struct_prototype(StructName,StructPrototype))))),!.


%= 	 	 

%% decl_argtypes( ?VALUE1) is semidet.
%
% Declare Argument Types.
%
decl_argtypes(StructDecl):- 
  compile_argtypes(StructDecl,"NotSlotted",_),!.


%= 	 	 

%% compile_argtypes( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Compile Argument Types.
%
compile_argtypes(StructDecl,Loc,StructPrototype):- 
 must_det_l((
    functor(StructDecl,StructName,_),
    StructDecl=..[StructName|PARGS],
    compile_struct_slots(StructName,Loc,PARGS,PArgNames,PArgTypes,InitArgs),
    StructPrototype=..[StructName|InitArgs],
  (number(Loc) -> 
    ((
      ArgNames=..[StructName|PArgNames],ain(us:struct_names(StructName,ArgNames)),
      Datatypes=..[StructName|PArgTypes],ain(us:struct_datatypes(StructName,Datatypes))));
    true))).
    



%= 	 	 

%% compile_struct_slots( ?VALUE1, ?VALUE2, :TermARG3, :TermARG4, :TermARG5, :TermARG6) is semidet.
%
% Compile Struct Slots.
%
compile_struct_slots(_,_,[],[],[],[]).
compile_struct_slots(StructType,Loc,[Param|ARGS],[Name|ArgNames],[Datatype|Datatypes],[Init|InitTypes]):-
   extract_struct_parameter(=,Param,Name,Datatype,Init),
   (number(Loc)->(ain(us:member_loc(StructType,Name,Loc)), Loc2 is Loc + 1);Loc2=Loc),
   ain(us:member_datatype(StructType,Name,Datatype)),
   (nonvar(Init)-> ain(us:member_init(StructType,Name,Datatype));true),   
   compile_struct_slots(StructType,Loc2,ARGS,ArgNames,Datatypes,InitTypes).



%= 	 	 

%% extract_struct_parameter( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4, ?VALUE5) is semidet.
%
% Extract Struct Parameter.
%
extract_struct_parameter(_Def,Name:Datatype,Name,Datatype,Init):-!,
  datatype_to_init(Datatype,Init).

extract_struct_parameter(Def,Decl=Init,Name,Datatype,Init):-!,
  extract_struct_parameter(Def,Decl,Name,Datatype).

extract_struct_parameter(Def,Decl,Name,Datatype,Init):-
   extract_struct_parameter(Def,Decl,Name,Datatype),!,
   datatype_to_init(Datatype,Init).


%= 	 	 

%% extract_struct_parameter( ?VALUE1, ?VALUE2, ?VALUE2, ?VALUE1) is semidet.
%
% Extract Struct Parameter.
%
extract_struct_parameter(_Def,Decl,Name,Type):-Decl=..[K1,K2,Name],!,Type=..[K1,K2].
extract_struct_parameter(_Def,Decl,Name,Type):-Decl=..[Type,Name],!.
extract_struct_parameter(Def,Name,Name,Def).
   

%= 	 	 

% module_local_init(_UserModule,SystemModule) is semidet.
%
% Hook To [module_local_init/2] For Module Logicmoo_util_structs.
% Module Local Init.
%
% @TODO module_local_init(_UserModule,SystemModule):- ain(SystemModule:'==>'(us:struct_decl(StructDecl),decl_struct(StructDecl))).



%= 	 	 

%% ensure_instance( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Instance.
%
ensure_instance(Type,Struct):-ensure_struct(Type,Struct).

%= 	 	 

%% ensure_instance( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Ensure Instance.
%
ensure_instance(Type,List,Struct):-ensure_struct(Type,List,Struct).


%= 	 	 

%% ensure_struct( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Struct.
%
ensure_struct(Type,Struct):- nonvar(Struct)->prop_set(sclass,Struct,Type);new_struct(Type,Struct).

%= 	 	 

%% ensure_struct( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Ensure Struct.
%
ensure_struct(Type,List,Struct):- must_det_l((ensure_instance(Type,Struct),prop_set_nvlist(Struct,List))).


%= 	 	 

%% prop_set_nvlist( ?VALUE1, :TermARG2) is semidet.
%
% Prop Set Nvlist.
%
prop_set_nvlist(Struct,[N=V|More]):-must_det_l((prop_set(N,Struct,V),( More==[]->true;prop_set_nvlist(Struct,More)))).

%= 	 	 

%% prop_get_nvlist( ?VALUE1, :TermARG2) is semidet.
%
% Prop Get Nvlist.
%
prop_get_nvlist(Struct,[N=V|More]):-must_det_l((ignore(show_failure(why,prop_get(N,Struct,V))),( More==[]->true;prop_get_nvlist(Struct,More)))).


%= 	 	 

%% new_struct( ?VALUE1, ?VALUE2) is semidet.
%
% New Struct.
%
new_struct(Type,Struct):- var(Type),!,trace_or_throw(var_new_struct(Type,Struct)).
new_struct(Type,Struct):- us:struct_prototype(Type,Struct),!,us:struct_prototype(Type,Struct).
new_struct(Type,Struct):- us:struct_datatype(Type,DType),!,new_struct(DType,Struct).
new_struct(Type,mutable([sclass=Type])):-!.
new_struct(Type,[sclass=Type]):-!.




%= 	 	 

%% datatype_to_init( ?VALUE1, ?VALUE2) is semidet.
%
% Datatype Converted To Init.
%
datatype_to_init(dict, Dict):- Dict = mutable([]). % sclass=dict
datatype_to_init(rb,   NewArg):-rb_new(NewArg),!.
datatype_to_init(assoc,NewArg):-empty_assoc(NewArg),!.
datatype_to_init(actions,[]).
datatype_to_init(sorted,[]).
datatype_to_init(_,_).

/*
new_struct(map, mutable(O)):- dict_create(O,Name,[]),!.
%new_struct(Type,Struct):- rb_new(O),rb_insert_new(O,sclass,Type,Struct),!.
%new_struct(Type,Struct):- rb_insert_new(_O,sclass,Type,Struct),!.
new_struct(Name,mutable(O)):- dict_create(O,Name,[]),!.
*/
:- fixup_exports.
