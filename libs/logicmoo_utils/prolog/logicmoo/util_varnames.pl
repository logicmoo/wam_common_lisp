/*  Logicmoo Debug Tools
% ===================================================================
% File 'util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: logicmoo@gmail.com ;
% Version: 'util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/util_varnames.pl
:- module(util_varnames,
          [ ain00/1,
            contains_ftVar/1,
            count_members_eq/3,
            all_different_vars/1,
            all_different_vals/1,
            all_different_vals/2,
            all_disjoint_in_sets/3,
            del_each_attr/2,
            get_random_headvars/1,
            member_open/2,
            get_random_headvars/4,
            get_1head_arg_var/5,
            term_slots/2,
            never_bound/1,
            dif_matrix/2,
            term_singleslots/2,
          term_singleslots/3,
          term_singleslots/5,         
            set_varname/2,
            set_varname/3,
            write_functor/2,
            atom_subst_frak_0/4,
            arg_varname/3,
            renumbervars/3,
            b_implode_varnames/1,
            b_implode_varnames0/1,
            bad_varnamez/1,
            call_not_not/1,
            call_return_tf/2,
            check_varnames/1,
            clause_ref_file/2,
            clause_ref_vars/2,
            contains_badvarnames/1,
            contains_dvar/1,
            contains_singletons/1,
            del_attr_type/2,
            copy_term_and_varnames/2,
            mpred_numbervars_with_names/1,
            ensure_vars_labled/2,
            ensure_vars_labled_r/2,
            fix_varcase_name/2,
            get_clause_vars/1,
            get_clause_vars/2,
            get_clause_vars_copy/2,
            get_clause_vars_hb_int/2,
            imploded_copyvars/2,
            locate_clause_ref/5,
            not_member_eq_local/2,
            listing_vars_file/0,
            make_subterm_path/3,
            maybe_record_scanned_file/0,

            without_varname_scan/1,

            no_varnaming/1,
            no_vars_needed/1,
            
   print_numbervars_maybe/1,
   print_numbervars/1,
   scan_for_varnames0/0,
   que_read_source_file_vars/1,
            read_source_file_vars/1,
            read_source_file_vars_1/1,
            read_source_vars/2,
            replace_in_string_frak_0/4,
            save_clause_vars/2,
            save_clause_vars/3,
            save_clause_vars/6,
            save_file_source_vars/3,
            save_to_clause_ref/3,
            scan_for_varnames/0,
            dcall_if_verbose/1,
            dcall_when/3,
            snumbervars4/4,
            snumbervars5/4,
            subterm_path/3,
            term_expansion_save_vars/1,
            try_get_body_vars/1,
            try_get_head_vars/1,
            try_get_inner_vars/1,
            set_varname/2,
            try_save_vars/1,
            
            v_dif_rest/2,
            vmust/1,
            init_varname_stores/1,
            maybe_scan_for_varnames/0

          ]).


/** <module> Utility LOGICMOO UTIL VARNAMES / Name Prolog variables (debugging)
This module holds utilities to access and change the names of prolog variables. 
- @author Douglas R. Miles
- @license LGPL
*/

% % % OFF :- system:use_module(library(must_sanity)).
% % % OFF :- system:use_module(library(logicmoo/misc_terms)).

:- multifile
        prolog:make_hook/2.
:- meta_predicate
       ain00(*),
        call_not_not(0),
        call_return_tf(0, ?),
        contains_singletons(*),
        get_clause_vars(:),
        get_clause_vars(:, ?),
        no_varnaming(0),
        without_varname_scan(0),
        dcall_when(2, ?, ?),
        snumbervars4(*, ?, ?, ?),
        snumbervars5(*, ?, ?, ?),
        try_save_vars(*),
        all_different_vals(2,*),
        all_disjoint_in_sets(2,*,*),
        init_varname_stores(?),
        vmust(0).

:- module_transparent((
            all_different_vals/1,
            all_different_vals/2,
            set_varname/2,
            set_varname/3,
            atom_subst_frak_0/4,
            b_implode_varnames/1,
            b_implode_varnames0/1,
            bad_varnamez/1,
            call_not_not/1,
            call_return_tf/2,
            check_varnames/1,
            clause_ref_file/2,
            clause_ref_vars/2,
            contains_badvarnames/1,
            contains_singletons/1,
            del_attr_type/2,
            ensure_vars_labled/2,
            ensure_vars_labled_r/2,
            fix_varcase_name/2,
            get_clause_vars/1,
            get_clause_vars/2,
            get_clause_vars_copy/2,
            get_clause_vars_hb_int/2,
            imploded_copyvars/2,
            locate_clause_ref/5,  
            
            listing_vars_file/0,
            make_subterm_path/3,
            maybe_record_scanned_file/0,
            no_varnaming/1,
            without_varname_scan/1,
            no_vars_needed/1,
            
   print_numbervars_maybe/1,
   print_numbervars/1,
            read_source_file_vars/1,
            read_source_file_vars_1/1,
            read_source_vars/2,
            replace_in_string_frak_0/4,
            save_clause_vars/2,
            save_clause_vars/3,
            save_clause_vars/6,
            save_file_source_vars/3,
            save_to_clause_ref/3,
            scan_for_varnames/0,
            dcall_if_verbose/1,
            dcall_when/3,
            snumbervars4/4,
            snumbervars5/4,
            subterm_path/3,
            term_expansion_save_vars/1,
            try_get_body_vars/1,
            try_get_head_vars/1,
            try_get_inner_vars/1,
            set_varname/2,
            try_save_vars/1,
            
            v_dif_rest/2,
            % name_variable/2, variable_name/2,
            vmust/1,
            init_varname_stores/1
 )).
:- meta_predicate dcall_if_verbose(0).

% % % OFF :- system:reexport((lockable_vars)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).
% % % % OFF :- system:use_module(library(dictoo_lib)).
:- system:use_module(library(logicmoo/each_call)).
:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).


:- use_module(library(backcomp)).
:- use_module(library(debug)).
:- use_module(library(occurs)).
:- use_module(library(check)).
%:- use_module(library(edinburgh)).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).
:- use_module(library(make)).


% :- use_module(library(gui_tracer)).
:- use_module(library(system)).
:- use_module(library(socket)).
:- use_module(library(readutil)).
:- abolish(system:time/1).
:- use_module(library(statistics)).
%:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
%:- use_module(library(editline)).
:- use_module(library(listing)).


%% member_open( ?ARG1, :TermARG2) is det.
%
% Member Open.
%
member_open(C, [B|A]) :-  (nonvar(B),B=C) ; (nonvar(A),member_open(C, A)).

%= 	 	 

%% source_variables_lwv( ?AllS) is det.
%
% Source Variables Lwv.
%
:-export(sourceable_variables_lwv/1).

sourceable_variables_lwv(AllVs):-
  ((prolog_load_context(variable_names,Vs1),is_list(Vs1));Vs1=[]),
  ((nb_current('$varable_names',Vs2),is_list(Vs2));Vs2=[]),
  ((execute_goal_vs(Vs3))),
  ((nb_current('$old_variable_names',Vs4),is_list(Vs4));Vs4=[]),
   append([Vs3,Vs2,Vs1,Vs4],All),!,list_to_set(All,AllVs),!.

source_variables_lwv(Msg,AllVs):-
   sourceable_variables_lwv(Vs1),
   grab_vn_varnames(Msg,Vs2),
   append([Vs2,Vs1],All),!,list_to_set(All,AllVs),!.
   % set_varname_list( AllS).
source_variables_lwv(_Msg,[]):-!.


:- export(execute_goal_vs/1).
execute_goal_vs(Vs):- execute_goal_vs0(Vs),!.
execute_goal_vs([]).
execute_goal_vs0(Vs):- notrace(catch(parent_goal('$toplevel':'$execute_goal2'(_,Vs,_)),_,fail)).
execute_goal_vs0(Vs):- notrace(catch(parent_goal('$toplevel':'$execute_goal2'(_,Vs)),_,fail)).

%:- public ((attr_unify_hook/2,attr_portray_hook/2)).
% :- public portray_attvar/1.
% :- export(portray_attvar/1).
%:- export(attr_unify_hook/2).
%:- export(attr_portray_hook/2).
%:- export(attribute_goals/3).

:- thread_local(t_l:varname_lock/1).
when_var_locked(What):- t_l:varname_lock(What),!.

:- thread_local(t_l:no_kif_var_coroutines/1).

%% attr_unify_hook( ?X, ?Other) is semidet.
%
% Hook To [dom:attr_unify_hook/2] For Module Logicmoo_varnames.
% Attr Unify Hook.
%
vn:attr_unify_hook(X,Y):- ignore(quietly(must(vn_auh_0(X,Y)))).

vn_auh_0(_, Var):- cyclic_term(Var),!,fail.
vn_auh_0(Var,_):- cyclic_term(Var),!,fail.
vn_auh_0(Name1, Var):- when_var_locked(What),!,(unify_name_based(Name1, Var)->true;call(What,Var)).
vn_auh_0(Name1, Var):- var(Var),unify_name_based(Name1, Var),!.
vn_auh_0(_Form, _OtherValue):-!.



unify_name_based(Var1, Var2):- \+ atom(Var1),variable_name_or_ref(Var1,Name),!,unify_name_based(Name, Var2).
unify_name_based(_Form, _OtherValue):- local_override(no_kif_var_coroutines,G),!,call(G).
unify_name_based(Name1, Var):-  get_var_name(Var,Name2),!,Name1=Name2,!.
unify_name_based(Name1, Var):- get_attr(Var, vn, Name2),!,combine_names(Name1,Name2,Name),(Name2==Name->true;put_attr(Var,vn,Name)).
unify_name_based(Name1, Var):- var(Var),!,put_attr(Var, vn, Name1).
unify_name_based(_, Var):- nonvar(Var),!.
%unify_name_based(_, Var):- cyclic_term(Var),!,fail.
%unify_name_based(_, Var):- cyclic_term(Var),!.
%unify_name_based(_, Var):- cyclic_break(Var),!,fail.

combine_names(Name1,Name2,Name1):-Name1==Name2,!.
combine_names(Name1,Name2,Name):-
 ((atom_concat(_,Name1,Name2);atom_concat(Name1,_,Name2)) -> Name=Name2 ; (
   ((atom_concat(Name2,_,Name1);atom_concat(_,Name2,Name1)) -> Name=Name1 ; (
   (combine_names_dash(Name1,Name2,Name)))))).

combine_names_dash(Name1,Name2,Name):- number_name(Name2),!,Name=Name1.
combine_names_dash(Name1,Name2,Name):- number_name(Name1),!,Name=Name2.
combine_names_dash(Name1,Name2,Name):- word_name(Name2),!,atomic_list_concat([Name2,'_',Name1],Name),!.
combine_names_dash(Name1,Name2,Name):- atomic_list_concat([Name1,'_',Name2],Name),!.

number_name(Name):- name(Name,[95,Number|_]),char_type(Number,digit).
word_name(Name):- name(Name,[A,_|_]),char_type(A,upper).


/*

%=

%% attr_portray_hook( ?Value, ?Var) is semidet.
%
% Attr Portray Hook.
%
 % vn:attr_portray_hook(Name, _) :- write('???'), write(Name),!.

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
user:portray(Sk):- get_attr(Sk, vn, Name), get_attrs(Sk,att(vn,Name,[])),write(Name),!,write('{}').


%% portray_attvar( ?Var) is semidet.
%
% Hook To [portray_attvar/1] For Module Logicmoo_varnames.
% Portray Attribute Variable.
%
portray_attvar(Var) :-
	write('{<'),
        ((get_attr(Var,vn, VarName))->true;sformat(VarName,'~q',[Var])),
	get_attrs(Var, Attr),
	catch(writeq('??'(VarName,Attr)),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').
*/



:- set_module(class(library)).

% % % OFF :- system:use_module(library(when)).

:- prolog_clause:multifile((
	unify_goal/5,			% +Read, +Decomp, +M, +Pos, -Pos
	unify_clause_hook/5,
	make_varnames_hook/5,
	open_source/2)).			% +Input, -Stream

:- predicate_options(prolog_clause:clause_info/5, 5,
		     [ variable_names(-list)
		     ]).

% % % OFF :- system:use_module(library(prolog_source)).
% % % OFF :- system:use_module(library(prolog_clause)). % read_term_at_line/6


:- meta_predicate vmust(0).

%=

%% vmust( :GoalG) is semidet.
%
% Vmust.
%
vmust(G):-!,must(G).
vmust(G):-!,call(G).


%=

%% dcall_when( :PRED2P, ?In, ?Out) is semidet.
%
% Dirrectly Call When.
%
dcall_when(P,In,Out):- must(call(P,In,Out)),ignore((In\=@=Out,dmsg((dcall_when(P) :- (In,Out))))).

:- create_prolog_flag(source_variables, false, [type(boolean),keep(true)]).
:- thread_local(t_l:dont_varname/0).
:- thread_local(t_l:dont_varname_te/0).
:- thread_local(t_l:try_varname_clause_next/1).


% :- use_module('./redo_locally').
%=

%% no_varnaming( :GoalG) is semidet.
%
% No Varnaming.
%

no_varnaming(G):-locally(t_l:dont_varname,G).


%=

%% all_different_vals(+P2, ?Term) is semidet.
%
% All Different Vals.
%
all_different_vals(Term):-all_different_vals(dif_matrix,Term).


%% all_different_vars( ?A) is semidet.
%
% All Different Variables.
%
all_different_vars(_):- t_l:dont_varname,!.
all_different_vars(A):-(quietly((all_disjoint_in_sets(dif_matrix,A,A)))),!.
all_different_vars(A):-must(quietly((all_disjoint_in_sets(dif_matrix,A,A)))),!.
all_different_vars(A):-all_different_vals(v_dif_rest,A),!.



%% all_different_vals(:PRED2, +Term) is semidet.
%
% All Different Vals.
%
all_different_vals(Pred,Term):-
 must(quietly(( (is_list(Term)-> Slots = Term ; term_slots(Term,Slots)),!,
                                 all_disjoint_in_sets(Pred,Slots,Slots)))).

%% all_different_vals(:PRED2, +SET1, +SET2) is semidet.
%
% All Different Vals.
%
all_disjoint_in_sets(_,[],_):-!.
all_disjoint_in_sets(_,[_],_):-!.
all_disjoint_in_sets(P,[V|Vs],SET):-delete_eq(SET,V,REST),!,call(P,V,REST),all_disjoint_in_sets(P,Vs,SET).

/*
delete_eq([],Item,[]):-!,dmsg(warn(delete_eq([],Item,[]))).
delete_eq([L|List],Item,List):-Item==L,!.
delete_eq([L|List],Item,[L|ListO]):-delete_eq(List,Item,ListO),!.
*/

%% v_dif_rest( ?V, ?REST) is semidet.
%
% V not member_eq Rest.
%
v_dif_rest(V,REST):- when('?='(V,_),not_member_eq_local(V,REST)).

%% not_member_eq_local( ?E, ?REST) is semidet.
%
% Not Member Using (==/2) (or =@=/2) ).
%
not_member_eq_local(_,[]):-!.
not_member_eq_local(E,REST):- \+ identical_memberchk(E,REST).

%% dif_matrix( ?V, ?REST) is semidet.
%
% V dif Rest of
%
dif_matrix(List,V):- is_list(List),!,list_to_set(List,SET), maplist(dif_matrix(V),SET).
dif_matrix(V,List):- is_list(List),!,list_to_set(List,SET), maplist(dif_matrix_hopfully(V),SET).
dif_matrix(A,B):- dif_matrix_hopfully(A,B).
dif_matrix_hopfully(A,B):-A==B,!.
dif_matrix_hopfully(A,B):- dif:dif(A,B),!.


%=

%% make_subterm_path( ?Sub, ?Term, ?PathO) is semidet.
%
% Make Subterm Path.
%
make_subterm_path(Sub,Term,PathO):-vmust(subterm_path(Sub,Term,Path)),!,PathO=Path.


%=

%% subterm_path( ?Sub, ?Term, :TermARG3) is semidet.
%
% Subterm Path.
%
subterm_path(Sub,Term,[]):-Sub==Term,!.
subterm_path(Sub,Term,[arg(N)|Path]):-compound(Term),!,arg(N,Term,TermE),subterm_path(Sub,TermE,Path),!.


%=

%% get_clause_vars( ?CV) is semidet.
%
% Get Clause Variables.
%
get_clause_vars(CV):- quietly(get_clause_vars_nontraced(CV)).
:- export(get_clause_vars_nontraced/1).

%=

%% get_clause_vars_nontraced( :TermMHB) is semidet.
%
% Get Clause Variables Nontraced.
%
get_clause_vars_nontraced(_):- t_l:dont_varname,!.
get_clause_vars_nontraced(V):- var(V),!.
get_clause_vars_nontraced('$VAR'(_)):- !.
get_clause_vars_nontraced(_:V):- var(V),!,ignore((get_varname_list_local(Vs),member(N=V0,Vs),V0==V,set_varname(write_functor,N,V))).
get_clause_vars_nontraced(MHB):- term_variables(MHB,Vs),must(get_clause_vars(MHB,Vs)).

:- '$set_predicate_attribute'(get_clause_vars(_), trace, 1).
:- '$set_predicate_attribute'(get_clause_vars(_), hide_childs, 1).


%=

%% del_attr_type( ?Type, ?Var) is semidet.
%
% Remove/erase Attr Type.
%
del_attr_type(Type,Var):-ignore(del_attr(Var,Type)).


%=

%% get_clause_vars( ?MHB, ?Vs) is semidet.
%
% Get Clause Variables.
%
get_clause_vars(_,[]):-!.
get_clause_vars(MHB,[V|Vs]):- all_different_vars([V|Vs]),vmust((get_clause_vars_copy(MHB,WVARS),!,
   vmust(MHB=WVARS),unlock_vars(MHB),nop(sanity(check_varnames(MHB))))),!,
   maplist(del_attr_type(vl),[V|Vs]).
get_clause_vars(MHB,Vs):- vmust((get_clause_vars_copy(MHB,WVARS),!,vmust(MHB=WVARS),unlock_vars(MHB),must(check_varnames(Vs)))),!.
get_clause_vars(_,_):- !.



%=

%% get_clause_vars_copy( ?HB, ?HB) is semidet.
%
% Get Clause Variables Copy.
%
get_clause_vars_copy(HB,HB):- ground(HB),!.
get_clause_vars_copy(HH,HH):- sub_term(S,HH),compound(S),S='$VAR'(_),!. % already labled
get_clause_vars_copy(H0,MHB):- copy_term(H0,MHB),!.
get_clause_vars_copy(H0,MHB):-
    source_variables_lv(AllS),
    must((copy_term(H0+AllS,MHB+CAllS),
    term_slots(MHB,Slots),
    all_different_vars(Slots),
    % lock_vars(Slots),
    as_clause_no_m( MHB,  H, B),
    must_maplist(set_varname(write_functor),CAllS),
    get_clause_vars_hb_int(H,B))),!.

get_clause_vars_copy(H0,MHB):-
    must((copy_term_and_varnames(H0,MHB),lock_vars(MHB),as_clause_no_m( MHB,  H, B),
    get_clause_vars_hb_int(H,B))),!.




%=

%% get_clause_vars_hb_int( ?H, ?B) is semidet.
%
% Get Clause Variables Head+body Int.
%
get_clause_vars_hb_int(H,B):- varname_cache:varname_info(H,B,Vs,_),must_maplist(set_varname(write_functor),Vs),!.
get_clause_vars_hb_int(H,B):- call_return_tf(try_get_body_vars(B),_TF1),call_return_tf(try_get_head_vars(H),_TF2),!.



%=

%% atom_subst_frak_0( ?A, ?F, ?R, ?K) is semidet.
%
% atom subst frak  Primary Helper.
%
atom_subst_frak_0(A,F,R,K):-replace_in_string_frak_0(F,R,A,K),!.

%=

%% replace_in_string_frak_0( ?F, ?R, ?A, ?K) is semidet.
%
% replace in string frak  Primary Helper.
%
replace_in_string_frak_0(F,R,A,K):-atom(A),!,atom_string(A,S),replace_in_string_frak_0(F,R,S,C),atom_string(K,C).
replace_in_string_frak_0(SepChars,Repl,A,C):- atomics_to_string(B,SepChars,A),atomics_to_string(B,Repl,C).



%=

%% fix_varcase_name( ?N, ?VN) is semidet.
%
% Fix Varcase Name.
%
fix_varcase_name(N,VN):-atom_subst_frak_0(N,'-','_',O),atom_subst_frak_0(O,'?','_',VN).


%=

%% no_vars_needed( ?H) is semidet.
%
% No Variables Needed.
%
no_vars_needed(H):- is_ftVar(H),!.
no_vars_needed(H):- (t_l:dont_varname; ( ground(H) ; \+ compound(H))) ,!.
no_vars_needed(A:H):-atom(A),!,no_vars_needed(H).
no_vars_needed('$VAR'(_)):-!.
no_vars_needed(H):- compound(H),H=varname_info(_,_,_,_),!.

%=

%% try_get_inner_vars( ?H) is semidet.
%
% Try Get Inner Variables.
%
try_get_inner_vars(H):- once((functor(H,_,N),arg(N,H,List),member(vars(Vs),List))),is_list(Vs),term_variables(H,VL),must_maplist(set_varname(write_functor),Vs,VL).



%=

%% term_slots( ?Term, ?Slots) is semidet.
%
% Hook To [mpred_type_wff:term_slots/2] For Module Logicmoo_varnames.
% Term Slots.
%
term_slots(Term,Slots):-term_singleslots(Term, [],NS, [],S),append(NS,S,Slots).


%=

%% term_singleslots( ?A, ?Vs) is semidet.
%
% Hook To [mpred_type_wff:term_singleslots/2] For Module Logicmoo_varnames.
% Term Singletons.
%
term_singleslots(A,Vs):- quietly(term_singleslots(A,[],_,[],Vs)).
%= %= :- was_export(term_singleslots/3).

%=

%% term_singleslots( ?Term, ?NonSingle, ?Singles) is semidet.
%
% Term Singletons.
%
term_singleslots(Term,NonSingle,Singles):- quietly(term_singleslots(Term,[],NonSingle,[],Singles)).
%= %= :- was_export(term_singleslots/5).

%=

%% term_singleslots( :TermFml, +NonSinglesIn, -NonSinglesOut, +SinglesIn, -SinglesOut) is semidet.
%
% Term Singletons.
%
term_singleslots(Fml, NS,NS, S,S):- atomic(Fml),!.
term_singleslots(Fml, NS,NS, S,S):- identical_memberchk(Fml,NS),!.
term_singleslots(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_memberchk(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singleslots(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singleslots([H|T],NS,NSO,S,NSV):- !, term_singleslots(H,NS,NSM,S,M),term_singleslots(T,NSM,NSO,M,NSV).
term_singleslots(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_|T],!, term_singleslots(T, NS,NSO, S,NSV).


%=

%% call_return_tf( :Goal, ?TF) is semidet.
%
% Call Return True/false.
%
call_return_tf(Call,TF):- ((Call-> TF = t ; TF = nil)).



%=

%% try_get_varname_cache( ?H) is semidet.
%
% Try Get Varname Cache.
%
try_get_varname_cache(H):- no_vars_needed(H),!.
try_get_varname_cache(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_varname_cache(H):- varname_cache:varname_info(_,H,Vs,_),maplist(set_varname(write_functor),Vs),!.

%=

%% try_get_head_vars( ?H) is semidet.
%
% Try Get Head Variables.
%
try_get_head_vars(H):- no_vars_needed(H),!.
try_get_head_vars(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_head_vars(H):- try_get_inner_vars(H).
try_get_head_vars(H):- varname_cache:varname_info(_,H,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_head_vars(B):- must(get_random_headvars(B)),!.



%=

%% try_varname_infos( ?H) is semidet.
%
% Try Varname Infos.
%
try_varname_infos(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.


%=

%% get_random_headvars( ?H) is semidet.
%
% Get Random Headvars.
%
get_random_headvars(H):- \+ compound(H),!.
get_random_headvars(H):- functor(H,F,A),get_random_headvars(H,F,A,A).


%=

%% get_random_headvars( ?H, ?F, ?A, ?N) is semidet.
%
% Get Random Headvars.
%
get_random_headvars(_H,_F,A,N):- (N < 1 ; N>A),!.
get_random_headvars( H, F,A,N):- arg(N,H,HA),ignore(get_1head_arg_var(H,F,N,A,HA)),N2 is N-1,get_random_headvars(H,F,A,N2).


%=

%% get_1head_arg_var( ?H, ?F, ?N, ?A, ?HA) is semidet.
%
% Get 1head Argument Variable.
%
get_1head_arg_var(_H,F,N,A,HA):-
 functor(HH,F,A),
 (nonvar(HA)->!;
 (arg(N,HH,COMP),
  (( arg_varname(HH,N,Name),HA='$VAR'(Name));(try_get_varname_cache(HH),sub_ft_var(HA,COMP))))).


%=

%% sub_ft_var( ?HA, ?COMP) is semidet.
%
% Sub Format Type Variable.
%
sub_ft_var(HA,COMP):-
  (is_ftVar(COMP)-> HA=COMP; (compound(COMP),arg(_,COMP,FTVAR),sub_ft_var(HA,FTVAR))).


%% arg_varname( +P, ?N, ?Name) is semidet.
%
% Argument variable name.
%
arg_varname(P,N,Name):- arg(N,P,Now),varname_cache:varname_info(P,_,List,_),var(Now),member(Nam=V,List),Now==V,must(Nam=Name).


%=

%% try_get_body_vars( :TermH) is semidet.
%
% Try Get Body Variables.
%
try_get_body_vars(H):- no_vars_needed(H),!.
try_get_body_vars(H):- varname_cache:varname_info(_,H,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_body_vars(H):- try_get_inner_vars(H),!.
try_get_body_vars(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_body_vars((A,B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars((A;B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars(C):- C=..[_,L],maplist(try_get_body_vars,L).
try_get_body_vars(B):- must(get_random_headvars(B)),!.
try_get_body_vars(_).

:- multifile(varname_cache:varname_info/4).
:- dynamic(varname_cache:varname_info/4).

:- meta_predicate renumbervars(*,*,*).

:- meta_predicate set_varname(+,*,*).
:- meta_predicate set_varname(+,*).
% set_varname(How,B):-var(B),!.

%=

%% set_varname( +How, ?B) is semidet.
%
% Set Varname.
%
:- meta_predicate set_varname(:,*,*).
:- meta_predicate set_varname(:,*).

set_varname(How,B):-var(B),writeq(set_varname(How,B)),nl,dtrace,trace_or_throw(var_assign_varname_vars(How,B)).
set_varname(How,N=V):-must(set_varname(How,N,V)),!.

%set_varname(How,N,V):-var(V),var(N),!,V=N.

%=

%% set_varname( +How, ?N, ?V) is semidet.
%
% Set Varname.
%
set_varname(How,N,V):- (var(How);var(N)),trace_or_throw(var_var_set_varname(How,N,V)).
set_varname(_,_,NV):-nonvar(NV),ignore((NV='$VAR'(N),must(number(N);atom(N)))).
set_varname(_,_,V):-atom(V),!.
set_varname(How,'$VAR'(Name),V):- !, set_varname(How,Name,V).
set_varname(_:[How],N,V):- !, set_varname(How,N,V).
set_varname(_:[How|List],N,V):- !, set_varname(How,N,V),set_varname(List,N,V).
set_varname(How,N,V):- number(N),!,format(atom(VN),'~w',[N]),set_varname(How,VN,V).
set_varname(How,N,V):- atom(N),atom_concat('"?',LS,N),atom_concat(NN,'"',LS),fix_varcase_name(NN,VN),!,set_varname(How,VN,V).
set_varname(_M:write_functor,N,V):- !,ignore('$VAR'(N)=V),!.
set_varname(_M:write_attribute,N,V):-!,put_attr(V,vn,N).
set_varname(_M:put_attr,N,V):-!,put_attr(V,vn,N).
set_varname(Nb_setval,N,V):- get_varname_list_local(Vs),!,register_var(N,Vs,V,NewVs),call(call,Nb_setval,'$variable_names',NewVs).
set_varname(Nb_setval,N,V):- call(call,Nb_setval,'$variable_names',[N=V]).
%set_varname(Nb_setval,N,V):- must(call(call,Nb_setval,N,V)).
%set_varname(_How,_,_).



write_functor(N=V):-write_functor(N,V).

%=

%% write_functor( ?N, ?V) is semidet.
%
% Write Functor.
%
write_functor(N,V):-var(V),!,put_attr(V,vn,N).
write_functor(N,V):-ignore('$VAR'(N)=V),!.

:- export(save_clause_vars/2).
:-module_transparent(save_clause_vars/2).

%=

%% save_clause_vars( ?MHB, ?Vs) is semidet.
%
% Save Clause Variables.
%
% save_clause_vars(_,[]):-!.
save_clause_vars(MHB,Vs):- ignore(maybe_record_scanned_file),ignore(current_why(Why)),
  ignore((var(Why),loading_file(Why))),!,save_clause_vars(MHB,Vs,Why).

% ?- clause(pui_help:prolog_help_topic(A),B,ClauseRef), prolog_clause:clause_info(ClauseRef, File, TermPos, NameOffset, Options).


:- export(save_clause_vars/3).

%=

%% save_clause_vars( ?MHB, ?Vs, :TermWhy) is semidet.
%
% Save Clause Variables.
%
save_clause_vars(_, [],_):-!.
save_clause_vars(MHB,Vs,Why:_):-atom(Why),!,save_clause_vars(MHB,Vs,Why).
save_clause_vars(MHB,Vs,Why):-  ( \+ \+ (logicmoo_util_term_listing:as_clause_w_m(MHB, M, H, B, MB),save_clause_vars(M,H,MB,B,Vs,Why))).



%=

%% locate_clause_ref( ?M, ?H, ?MB, ?B, ?ClauseRef) is semidet.
%
% Locate Clause Ref.
%
locate_clause_ref(M,H,_MB,_B,_ClauseRef):- ( \+ (predicate_property(M:H,number_of_clauses(_)))),( \+ (predicate_property(_:H,number_of_clauses(_)))),!,fail.
locate_clause_ref(M,H,MB,B,ClauseRef):-clause_asserted(M:H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_asserted(H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_asserted(_:H,MB:B,ClauseRef).
locate_clause_ref(M,H,_MB,B,ClauseRef):-clause_asserted(M:H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_asserted(H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_asserted(_:H,B,ClauseRef).


%=

%% clause_ref_vars( ?ClauseRef, ?Was) is semidet.
%
% Clause Ref Variables.
%
clause_ref_vars(ClauseRef,Was):-prolog_clause:clause_info(ClauseRef, _File, _TermPos, _NameOffset, [variable_names(Was)]).

%=

%% clause_ref_file( ?ClauseRef, ?File) is semidet.
%
% Clause Ref File.
%
clause_ref_file(ClauseRef,File):-prolog_clause:clause_info(ClauseRef, File, _TermPos, _NameOffset, []).

:- export(save_to_clause_ref/3).

%=

%% save_to_clause_ref( ?ClauseRef, ?Vs, ?Why) is semidet.
%
% Save Converted To Clause Ref.
%
save_to_clause_ref(ClauseRef,Vs,Why):- ain00(names(ClauseRef,Vs)),ain00(names_why(ClauseRef,Why)),!.

:- export(save_clause_vars/6).

%=

%% save_clause_vars( ?M, ?H, ?MB, ?B, ?Vs, ?Why) is semidet.
%
% Save Clause Variables.
%
save_clause_vars(M,H,MB,B,Vs,Why:_):-atom(Why),!,save_clause_vars(M,H,MB,B,Vs,Why).
save_clause_vars(M,H,MB,B,Vs,Why):- fail, locate_clause_ref(M,H,MB,B,ClauseRef),clause_ref_vars(ClauseRef,Was),
   ((Was=Vs) -> fail ; save_to_clause_ref(ClauseRef,Vs,Why)),!.
save_clause_vars(_M,H,_MB,B,Vs,Why):- 
  ain00(varname_cache:varname_info(H,B,Vs,Why)).


%=

%% ain00( ?A) is semidet.
%
% Assert If New Primary Helper Primary Helper.
%
ain00(A):- clause_asserted(A),!.
ain00(A):- assertz(A).


%% contains_ftVar( +Term) is semidet.
%
% Contains Format Type Variable.
%
contains_ftVar(Term):- sub_term(Sub,Term),compound(Sub),Sub='$VAR'(_).



%=

%% ensure_vars_labled_r( ?I, ?O) is semidet.
%
% Ensure Variables Labled R.
%
ensure_vars_labled_r(I,I):-!.
ensure_vars_labled_r(I,O):-
  once((((get_varname_list_local(Vs),Vs\==[])),
   copy_term(I:Vs,O:OVs),
    must_maplist(write_functor,OVs))),
   (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):-
     once((get_clause_vars_copy(I,O),unlock_vars(O))),
     (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- copy_term_and_varnames(I,O),I\=@=O.

:- export(copy_term_and_varnames/2).

%=

%% copy_term_and_varnames( ?Term, ?Named) is semidet.
%
% Copy Term And Varnames.
%
copy_term_and_varnames(Term,Named):- quietly((unnumbervars(Term,UNV),copy_term(UNV,Named))),!.
copy_term_and_varnames(Term,Named):- quietly((ignore((source_variables_lv(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(set_varname([write_functor,b_setarg]),CAllS))).


%=

%% renumbervars( ?How, ?Term, ?Named) is semidet.
%
% Renumbervars.
%
renumbervars(How,Term,Named):-
   quietly((
   must_det((source_variables_lv(AllS);AllS=[])),
   copy_term(Term+AllS,Named+CAllS),
   maplist(set_varname(How),CAllS))).





%=

%% source_variables_lv( ?AllS) is semidet.
%
% Source Variables Lv.
%
source_variables_lv(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (get_varname_list_local(Vs2);Vs2=[]),
  %    execute_goal_vs(Vs3),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  set_varname_list( AllS).



%=

%% contain_numbervars( ?Term) is semidet.
%
% Contain Numbervars.
%
contain_numbervars(Term):- sub_term(Sub,Term),compound(Sub),Sub='$VAR'(_),!.




%=

%% contains_singletons( ?Term) is semidet.
%
% Contains Singletons.
%
contains_singletons(Term):-contains_singletons(Term,N),N>0.


%=

%% contains_singletons( ?Term, ?N) is semidet.
%
% Contains Singletons.
%
contains_singletons(Term,N):-
  % sanity(\+contain_numbervars(Term)),
     \+ ground(Term),
     copy_term_nat(Term,Nat),
     term_variables(Nat,Vs),
     numbervars(Nat,0,_,[attvar(bind),singletons(true)]),!,
     count_members_eq('$VAR'('_'),Vs,N).



%% count_members_eq(+Find, +TermE, -N) is det.
%
% Count Members Using (==/2) (or =@=/2) ).
%
count_members_eq(_,[],0):-!.
count_members_eq(Find,[E|List],N):-
  count_members_eq(Find,List,NN),!,  (E == Find -> N is NN+1 ; NN=N).



%% call_not_not(:Goal) is semidet.
%
% Call \+ \+ Goal
%
call_not_not(Goal):- \+ \+ Goal.


%=

%% contains_badvarnames( ?Term) is semidet.
%
% Contains Badvarnames.
%
contains_badvarnames(Term):-  quietly((sub_term(SubV,Term),compound(SubV),SubV='$VAR'(Sub),bad_varnamez(Sub))),!.

contains_dvar(Term):-quietly((sub_term(SubV,Term),compound(SubV),SubV='$VAR'(_),!)).

%=

%% bad_varnamez( ?Sub) is semidet.
%
% Bad Varnamez.
%
bad_varnamez(Sub):- atom(Sub),!,sub_string(Sub,_,_,_,'.').
bad_varnamez(Sub):- var(Sub),!.
bad_varnamez(Sub):- integer(Sub),!, (Sub < 0 ; Sub > 991000).
bad_varnamez(Sub):- number(Sub).
bad_varnamez(Sub):- string(Sub),!.
bad_varnamez(Sub):- format(atom(A),'~w',['$VAR'(Sub)]),sub_string(A,_,_,_,'$').


%=

%% mpred_numbervars_with_names( ?Term) is semidet.
%
% Managed Predicate Numbervars Using Names.
%
mpred_numbervars_with_names(Term):- term_variables(Term,Vars),mpred_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.


%=

%% mpred_name_variables( :TermVar) is semidet.
%
% Managed Predicate Name Variables.
%
mpred_name_variables([]).
mpred_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   mpred_name_variables(Vars).


%=========================================
% unnumbervars
%=========================================


%=

%% b_implode_varnames( ?VALUE1) is semidet.
%
% ?- b_implode_varnames(Vs),display(Vs).
%
% '$VAR'('Vs')
%
b_implode_varnames(T):- imploded_copyvars(T,TT),T=TT.

%=

%% b_implode_varnames0( :TermN) is semidet.
%
% Backtackable Implode Varnames Primary Helper.
%

b_implode_varnames0([N=V|Vs]):- sanity(is_list(Vs)),ignore((nonvar(N),V='$VAR'(N);V=N)),b_implode_varnames0(Vs),!.
b_implode_varnames0(_).


%=

%% imploded_copyvars( ?C, ?CT) is semidet.
%
% Imploded Copyvars.
%
imploded_copyvars(C,CT):-vmust((source_variables_l(Vs),copy_term(C-Vs,CT-VVs,_),b_implode_varnames0(VVs))),!.


%% source_variables( ?Vs) is semidet.
%
% Source Variables.
%
source_variables(Vs):- 
 (prolog_load_context(variable_names,Vs2);Vs2=[]),
 execute_goal_vs(Vs1),
 append(Vs1,Vs2,Vs3),list_to_set(Vs3,Vs),
 (Vs\==Vs2 -> b_setval('$variable_names',Vs) ; true).

source_variables0(Vs):- 
 (prolog_load_context(variable_names,Vs2);Vs2=[]),
 execute_goal_vs(Vs1),
 append(Vs1,Vs2,Vs3),list_to_set(Vs3,Vs).


% snumbervars(Term,Functor,Start,End,List):-vmust(( vmust(var(End);number(End)),snumbervars4(Term,Start,End,[functor_name(Functor)|List]))),check_varnames(Term).



%=

%% check_varnames( :TermVs) is semidet.
%
% Check Varnames.
%
check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),!,check_varnames(Vs).
check_varnames(Term):- contains_badvarnames(Term),!,dumpST0,dtrace,nortrace,dtrace,!,dtrace(contains_badvarnames(Term)).
check_varnames(_).

:-meta_predicate(snumbervars4(0,?,?,?)).

%=

%% snumbervars4( :GoalTerm, ?Start, ?End, ?List) is semidet.
%
% Snumbervars4.
%
snumbervars4(Term,Start,End,List):-  \+ member(attvar(_),List),!,snumbervars5(Term,Start,End,[attvar(skip)|List]).
snumbervars4(Term,Start,End,List):- snumbervars5(Term,Start,End,List).

:-meta_predicate(snumbervars5(0,?,?,?)).

%=

%% snumbervars5( :GoalTerm, ?Start, ?End, ?List) is semidet.
%
% Snumbervars5.
%
snumbervars5(Term,Start,End,List):-must_det_l((integer(Start),is_list(List), numbervars(Term,Start,End,List),check_varnames(Term))).

:- export(try_save_vars/1).

%=

%% try_save_vars( ?HB) is semidet.
%
% Try Save Variables.
%
try_save_vars(_):- t_l:dont_varname,!.
try_save_vars(HB):-ignore((get_varname_list_local(Vs),Vs\==[],save_clause_vars(HB,Vs))),!.

:- export(maybe_scan_for_varnames/0).

%=

%% maybe_scan_for_varnames is semidet.
%
% Maybe Scan Source Files For Varnames.
%
maybe_scan_for_varnames:- current_prolog_flag(source_variables, true)->scan_for_varnames;true.

:- export(scan_for_varnames/0).

:- dynamic(thglobal:scanning_for_varnames_already/0).

%=

%% scan_for_varnames is semidet.
%
% Scan For Varnames.
%
scan_for_varnames:- thglobal:scanning_for_varnames_already,!.
scan_for_varnames:- %swc, 
 setup_call_cleanup(
   asserta(thglobal:scanning_for_varnames_already),
   scan_for_varnames0,
   retractall(thglobal:scanning_for_varnames_already)).

scan_for_varnames0:-
 set_prolog_flag(source_variables, true),
 ensure_loaded(library(make)),
 doall((make:modified_file(F),retractall(varname_cache:varname_info_file(F)))),
 doall((filematch(swi('boot/*.pl'),F),que_read_source_file_vars(F))),
 doall((source_file(F),que_read_source_file_vars(F))),!,
 ignore((
   ( \+ \+ varname_cache:queued_read_source_file_vars(_)),
   dmsg("Begining grovel for vars..."),
   gripe_time(1.0,doall((retract(varname_cache:queued_read_source_file_vars(F)),
     read_source_file_vars(F)))))).

que_read_source_file_vars(F):-varname_cache:varname_info_file(F),!.
que_read_source_file_vars(F):-ain00(varname_cache:queued_read_source_file_vars(F)).

%=

%% dcall_if_verbose( :GoalG) is semidet.
%
% Dirrectly Call If While Being Descriptive.
%
dcall_if_verbose(G):-!, quietly(G).
dcall_if_verbose(G):-show_call(why,G).

%  list_undefined([module_class([user,system,library,test,temporary,development])]).
:- dynamic(varname_cache:varname_info_file/1).
:- dynamic(varname_cache:queued_read_source_file_vars/1).

%=

%% read_source_file_vars( ?F) is semidet.
%
% Read Source File Variables.
%
read_source_file_vars(_):- ( \+ current_prolog_flag(source_variables, true)),!.
read_source_file_vars(F):- \+ ((atom(F),exists_file(F))),!, forall(filematch(F,E),read_source_file_vars(E)).
read_source_file_vars(F):- clause_asserted(varname_cache:varname_info_file(F)),!.
read_source_file_vars(F):- asserta(varname_cache:varname_info_file(F),Ref), catch((read_source_file_vars_1(F)),E,(dmsg(E),erase(Ref))).


%=

%% save_file_source_vars( ?F, ?T, ?Vs) is semidet.
%
% Save File Source Variables.
%
save_file_source_vars(_F,end_of_file,_Vs):-!.
save_file_source_vars(_F,_T,[]):-!.
save_file_source_vars(F,T,Vs):- put_variable_names(Vs),!,locally(t_l:current_why_source(F),save_clause_vars(T,Vs)),!.



%=

%% read_source_vars( ?File, ?In) is semidet.
%
% Read Source Variables.
%

read_source_vars(File,In):-
   Module = module(_),
	repeat,
	  catch(prolog_read_source_term(In, Term, Expanded, [ variable_names(Vs), syntax_errors(error) , term_position(TermPos)  ]),
		E,(nop((dmsg(E))),fail)),
          arg(1,Module,M),
          (Term = module(MM,_) -> (nb_setarg(1,Module,MM),fail);
          ((stream_position_data(line_count, TermPos, LineNo),

	  ignore(save_file_source_vars(mfl4(Vs,M,File,LineNo),Term,Vs)),
	  (   is_list(Expanded)
	  ->  member(T, Expanded)
	  ;   T = Expanded
	  ),
	(   T == end_of_file
	->  ! ;
           ( T\==Term, save_file_source_vars(mfl4(Vs,M,File,LineNo),T,Vs),
	    fail))))).


% new method

%=

%% read_source_file_vars_1( ?File) is semidet.
%
% read source file Variables  Secondary Helper.
%
read_source_file_vars_1('/usr/lib/swi-prolog/library/sandbox.pl'):-!.
read_source_file_vars_1(File):-
   once(current_prolog_flag(xref, Was);Was=false),
   locally(set_prolog_flag(xref, true),
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    read_source_vars(File,In),
	    (prolog_close_source(In),
              current_prolog_flag(xref, Was)))),!.





:- export(ensure_vars_labled/2).


mfree:attr_unify_hook(This,That):-get_attr(That,eq,Thats),Thats==This,!.
mfree:attr_unify_hook(This,That):-get_attrs(That,Thats),get_attrs(This,Value),Thats==Value,!.
never_bound(V):- var(V),!,put_attr(V,eq,_).
never_bound(_).

star:attr_unify_hook(This,That):-This==That.
star:attr_unify_hook(_,That):-compound(That),That='$VAR'(Atom),nonvar(Atom).
only_stars(V):- var(V),!,put_attr(V,star,V).
only_stars(_).

del_each_attr(M,V):-del_attr(V,M).

%=

%% ensure_vars_labled( ?I, ?O) is semidet.
%
% Ensure Variables Labled.
%
ensure_vars_labled(I,O):-nonvar(O),!,must(ensure_vars_labled(I,M)),!,M=O.
ensure_vars_labled(I,I):- (t_l:dont_varname;no_vars_needed(I)),!.
% ensure_vars_labled(I,I):- term_variables(I,Vs),maplist(never_bound,Vs),!.
% ensure_vars_labled(I,I):- !.
ensure_vars_labled(I,OO):- acyclic_term(O),term_variables(I,Vs),all_different_vals(Vs),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,OO):- vmust(acyclic_term(I)),term_variables(I,Vs),all_different_vars(Vs),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,I).


:- multifile(user:portray/1).

%=

%% portray( :TermObj) is semidet.
%
% Hook To [user:portray/1] For Module Logicmoo_varnames.
% Portray.
%
user:portray(A) :- fail, \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), %JUNIT \+ tracing,
  catch(quietly(((compound(A);var(A)), current_prolog_flag(source_variables, true), set_prolog_flag(source_variables, false),
    call_cleanup((((user:portray(A) -> ! ; print_numbervars_maybe(A)))),set_prolog_flag(source_variables, true)))),E,(writeq(E),nl,fail)).


%=

%% print_numbervars( ?H) is semidet.
%
% Print Numbervars.
%
print_numbervars(H):- (tlbugger:no_slow_io; tracing),!, writeq(H),!.
print_numbervars(H):- must( \+ \+ ((on_x_log_fail(print_numbervars_maybe(H));on_x_log_fail(print_numbervars_1(H))))),!.



%=

%% print_numbervars_maybe( ?H) is semidet.
%
% Print Numbervars Maybe.
%
print_numbervars_maybe(H):-(compound(H);var(H)), copy_term(H,HC), \+ \+ ((get_clause_vars(H), HC\=@=H, print_numbervars_1(H))),!.


%=

%% print_numbervars_1( ?H) is semidet.
%
% print numbervars  Secondary Helper.
%
print_numbervars_1(H):- loop_check(print_numbervars_2(H),format('~N~q.~n',[H])).

% print_numbervars_2(H):- dtrace,baseKB:portray_one_line_hook(H),!.

%=

%% print_numbervars_2( ?H) is semidet.
%
% print numbervars  Extended Helper.
%
print_numbervars_2(H):- current_output(S),prolog_listing:portray_clause(S,H,[portrayed(true),singletons(false)]),!.
print_numbervars_2(H):- write_term(H,[portrayed(false)]),nl,!.


:- thread_local(t_l:disable_px/0).


%=

%% term_expansion_save_vars( ?HB) is semidet.
%
% Term Expansion Save Variables.
%
term_expansion_save_vars(HB):- \+ ground(HB),  \+ t_l:dont_varname_te,\+ t_l:dont_varname, % \+ current_prolog_flag(xref, true),
   current_predicate(listing_vars_file/0), current_prolog_flag(source_variables,true),
   source_context_module(M),init_varname_stores(M),
   logicmoo_util_with_assertions:locally([t_l:dont_varname_te,t_l:disable_px],try_save_vars(M:HB)),!,fail.


without_varname_scan(Goal):-
  locally(current_prolog_flag(source_variables,false),
   locally([ - t_l:dont_varname_te,- t_l:dont_varname],Goal)).


%=

%% maybe_record_scanned_file is semidet.
%
% Maybe Record Scanned File.
%
maybe_record_scanned_file:-ignore((  source_location(F,_), \+ varname_cache:varname_info_file(F), asserta(varname_cache:varname_info_file(F)))).


%=

%% init_varname_stores( ?M) is semidet.
%
% Init Varname Storages.
%
init_varname_stores(_):- !.
% init_varname_stores(M):- M:ensure_loaded(logicmoo_util_with_assertions), M:ensure_loaded(util_varnames).


:- initialization(maybe_scan_for_varnames).
% :- maybe_scan_for_varnames.


%=

%% listing_vars_file is semidet.
%
% Logicmoo Util Varnames File.
%
listing_vars_file.


%=

%% make_hook( ?VALUE1, ?Files) is semidet.
%
% Hook To [prolog:make_hook/2] For Module Logicmoo_varnames.
% Make Hook.
%
%prolog:make_hook(before, Files):-forall(member(File,Files),retractall(varname_cache:varname_info_file(File))).
% prolog:make_hook(after, Files):- forall(member(File,Files),show_call(why,ain00(varname_cache:varname_info_file(File)))).

%=

:- system:use_module(library(debuggery/bugger)).

:- fixup_exports.

%% term_expansion( :TermFDecl, ?Clause) is semidet.
%
% Hook To [user:term_expansion/2] For Module Logicmoo_varnames.
% Term Expansion.
%
% % % % OFF :- system:use_module(library(logicmoo_utils_all)).
system:term_expansion((H:-B),_):- current_prolog_flag(source_variables,true),term_expansion_save_vars((H:-B)),fail.

