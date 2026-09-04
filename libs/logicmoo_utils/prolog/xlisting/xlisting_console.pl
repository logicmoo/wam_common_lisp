/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/xlisting.pl
:- module(xlisting_console,
          [ blob_count/2,
          use_listing_vars/0,
          use_listing_vars/1,
          use_xlisting/0,
          use_xlisting/1,
          get_print_mode/1,               
          xlisting_1/1,
           is_listing_hidden/1,
            bad_pred/1,
            blob_info/3,            
            bookeepingPredicateXRef/1,
            buggery_ok/0,
            %prolog_listing_portray_clause/3,
            catch_each/3,
            clause_ref/2,
            cur_predicate/2,
            current_atom_or_blob/2,
            get_matcher_code/4,
            get_search_ref/2,
            get_search_ref0/2,
            get_search_ref_tl/2,
            is_listing_hidden_00/2,
            is_listing_hidden/1,
            m_clause/4,
            m_clause0/4,
            m_clause_no_missing/4,
            make_headkey/2,
            make_search_key/2,
            make_searchable/1,
            make_searchable_index/1,
            make_searchable_list_ref/2,
            make_searchable_ref/2,
            maybe_separate/2,
            maybe_separate_0/2,
            mmake/0,
            mp/3,            
            mpred_match_listing/1,
            mpred_match_listing_skip_pi/3,
            mstatistics/0,
            new_atoms/2,
            nonvar_search/1,
            ok_show/1,
            plisting/1,
            portray_hb/2,
            portray_hbr/3,
            portray_phbr/4,
            portray_one_line/2,
            pp_listing/1,
            predicateUsesCall/1,
            printAll/1,
            printAll/2,
            print_clause_properties/2,
            print_record_properties/2,
            process_unify_in_thread/2,
            real_list_undefined/1,
            remove_undef_search/0,
            save_atoms/0,
            save_search_ref/2,
            save_search_ref_0/2,
            save_search_ref_recorded/2,
            save_search_ref_tl/2,
            scansrc_list_undefined/1,
            search_refs_use_recorded/0,
            searchable_of_clause/2,
            searchable_of_clause_0/2,
            searchable_of_clause_1/2,
            searchable_of_clause_1/3,
            searchable_terms/1,
            searchable_terms_tl/1,
            sourceTextPredicate/1,
            sourceTextPredicateSource/1,
            synth_clause_for_l2/5,
            synth_clause_for_large/6,
            synth_clause_ref/5,
            synth_in_listing/1,
            term_matches_hb/3,
         
            term_matches_unify/3,
            unify_in_thread/2,
            unify_in_thread_tl/2,
            unify_listing/1,
            unify_listing/3,
            unify_listing_header/1,
            unify_listing_header/3,
            unmake_search_key/2,
            update_changed_files/0,
            update_changed_files0/0,
            update_changed_files1/0,
            xlisting/1,
            xlisting/0,
            xlisting_inner/3
    ]).

/** <module> Utility LOGICMOO XLISTING CONSOLE
Cross references predicates from the command line. 

- @author Douglas R. Miles
- @license LGPL 
*/

:- dynamic((xlisting:'$exported_op'/3)).

:- multifile     
        baseKB:shared_hide_data/1,
        synth_clause_for/5.
:- meta_predicate maybe_separate(*,0).
:- meta_predicate maybe_separate_0(*,0).
:- meta_predicate
        mpred_match_listing_skip_pi(+,+, +),
        printAll(0),
        printAll(0, ?),
        unify_in_thread(+, 0),
        unify_in_thread_tl(?, 0),
        unify_listing(:),
        unify_listing(:, ?, ?),
        unify_listing_header(:),
        unify_listing_header(:, ?, ?),
        xlisting_inner(3, +, +).

:- public(is_listing_hidden/1).
:- export(is_listing_hidden/1).

:- module_transparent
        blob_count/2,
        bad_pred/1,
        blob_info/3,
        bookeepingPredicateXRef/1,
        buggery_ok/0,
        clause_ref/2,
        cur_predicate/2,
        current_atom_or_blob/2,
        get_matcher_code/4,
        get_search_ref/2,
        get_search_ref0/2,
        get_search_ref_tl/2,        
        is_listing_hidden_00/2,
        is_listing_hidden/1,
        m_clause/4,
        m_clause0/4,
        m_clause_no_missing/4,
        make_headkey/2,
        make_search_key/2,
        make_searchable/1,
        make_searchable_index/1,
        make_searchable_list_ref/2,
        make_searchable_ref/2,
        maybe_separate/2,
        maybe_separate_0/2,
        mmake/0,
        mp/3,
        mpred_match_listing/1,        
        mstatistics/0,
        new_atoms/2,
        nonvar_search/1,
        ok_show/1,
        plisting/1,
        plisting_0/1,
        portray_hb/2,
        portray_hbr/3,
        portray_phbr/4,
        portray_one_line/2,
        pp_listing/1,
        predicateUsesCall/1,
        print_clause_properties/2,
        print_record_properties/2,
        process_unify_in_thread/2,
        real_list_undefined/1,
        remove_undef_search/0,
        save_atoms/0,
        save_search_ref/2,
        save_search_ref_0/2,
        save_search_ref_recorded/2,
        save_search_ref_tl/2,
        scansrc_list_undefined/1,
        search_refs_use_recorded/0,
        searchable_of_clause/2,
        searchable_of_clause_0/2,
        searchable_of_clause_1/2,
        searchable_of_clause_1/3,
        searchable_terms/1,
        searchable_terms_tl/1,
        sourceTextPredicate/1,
        sourceTextPredicateSource/1,
        synth_clause_for/5,
        synth_clause_for_l2/5,
        synth_clause_for_large/6,
        synth_clause_ref/5,
        synth_in_listing/1,
        term_matches_hb/3,       
        term_matches_unify/3,
        unmake_search_key/2,
        update_changed_files/0,
        update_changed_files0/0,
        update_changed_files1/0,
        xlisting/1.
:- dynamic
        search_refs_use_recorded/0.

:- set_module(class(library)).


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
:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
:- autoload(library(listing),[portray_clause/3,listing/1]).
:- autoload(library(http/http_open),[http_open/3]).
:- autoload(library(http/http_parameters),[http_parameters/2]).
:- autoload(library(http/http_stream),[is_cgi_stream/1,cgi_property/2]).
:- autoload(library(http/http_wrapper),[http_peer/2]).

%:- use_module(library(editline)).
%:- prolog_listing:use_module(library(listing)).


% % % OFF :- system:use_module(library(hook_database)).
% % % OFF :- system:use_module(library(logicmoo/no_repeats)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).
% % % OFF :- system:use_module(library(logicmoo/redo_locally)).
% % % OFF :- system:use_module(library(logicmoo/virtualize_source)).
% % % OFF :- system:use_module(library(logicmoo/attvar_serializer)).


:- thread_local(t_l:print_mode/1).

:- export(mstatistics/0).

:- prolog_load_context(directory,Dir),asserta(baseKB:mpred_loader_dir(Dir)).

%= 	 	 

%% mstatistics is semidet.
%
% Mstatistics.
%
mstatistics:-
  garbage_collect,
  garbage_collect_atoms,
  statistics,
  statistics(stack,Om),O is Om/1000000,
  statistics(clauses,C),
  statistics(memory,[Tm,_]),T is Tm/1000000,
  statistics(atoms,[A,Mm,0]),AM is Mm/1000000,
  OdC is O/C,
  OdA is (A/1000)*39,
  PerAtom is (Mm/A),
  blob_count(L,NA),
  save_atoms,
  fmt((stack/clauses/mem/new + O/C/T-Tm/NA = c(OdC)/a(OdA-AM-PerAtom))),!,
  ((number(NA),NA<1000)->fmt(L);true).


%= 	 	 

%% current_atom_or_blob( ?X, ?VALUE2) is semidet.
%
% Current Atom Or Blob.
%
current_atom_or_blob(X,atom):-current_atom(X).
current_atom_or_blob(X,blob(T)):-current_blob(X,T).
current_atom_or_blob(X,functor_safe(Y)):-current_functor(X,Y).
current_atom_or_blob(X,key):-current_key(X).
current_atom_or_blob(X,flag):-current_key(X).


%= 	 	 

%% blob_info( ?A, :TermARG2, :TermA) is semidet.
%
% Blob Info.
%
blob_info(A,atom,blob(A,text)).
blob_info(A,blob(text),blob(A,text)).
blob_info(A,functor_safe(Y),functor_safe(A,Y)).
blob_info(A,key,key(A,Y)):-findall(V,recorded(A,V),Y).
blob_info(A,flag,flag(A,Y)):-flag(A,Y,Y).
blob_info(A,blob(clause),blob(A,T,Y,H,B)):-T=clause, findall(V,clause_property(A,V),Y),(m_clause(_,H,B,A)->true;H=dead).
blob_info(A,blob((record)),blob(A,T,Y)):-T=(record),with_output_to(string(Y),print_record_properties(A, current_output)).
% blob_info(A,blob(T),blob(A,T,Y)):-with_output_to(string(Y),prolog_term_view:emit_term(A, [])).
blob_info(A,blob(T),blob(A,T)).

:- export(tlbugger:saved_current_atom/2).
:- dynamic(tlbugger:saved_current_atom/2).
:- export(new_atoms/2).

%= 	 	 

%% new_atoms( ?X, ?Type) is semidet.
%
% New Atoms.
%
new_atoms(X,Type):-current_atom_or_blob(X,Type),\+(tlbugger:saved_current_atom(X,Type)).
:- export(save_atoms/0).

%= 	 	 

%% save_atoms is semidet.
%
% Save Atoms.
%
save_atoms:-forall(new_atoms(X,Type),assert(tlbugger:saved_current_atom(X,Type))).
:- export(blob_count/2).

%= 	 	 

%% blob_count( ?L, ?NA) is semidet.
%
% Backtackable For Internal Interface.
%
blob_count(L,NA):-findall(W,(new_atoms(X,Type),once(blob_info(X,Type,W))),LL),list_to_set(LL,L),length(L,NA).

%= 	 	 

%% print_record_properties( ?Record, ?Out) is semidet.
%
% Print Record Properties.
%
print_record_properties(Record, Out) :-
	format(Out, 'Record reference ~w~n', [Record]),
	(   recorded(Key, Value, Record)
	->  format(Out, ' Key:   ~p~n', [Key]),
	    format(Out, ' Value: ~p~n', [Value])
	;   format(Out, ' <erased>~n', [])
	).


%= 	 	 

%% print_clause_properties( ?REF, ?Out) is semidet.
%
% Print Clause Properties.
%
print_clause_properties(REF, Out) :-
	format(Out, 'Clause reference ~w~n', [REF]),
	(   m_clause(_,Head, Body, REF)
	->  nl(Out),
	    portray_clause(Out, (Head:-Body))
	;   format(Out, '\t<erased>~n', [])
	).


%= 	 	 

%% make_searchable_index( ?PI) is semidet.
%
% Make Searchable Index.
%
make_searchable_index(PI):- forall(to_matcher_pi(PI,H),forall(clause(H,B,Ref),make_searchable_ref((H:-B),Ref))).

to_matcher_pi(I,O):- strip_module(I,M,P),to_matcher_pi(M,P,O).
to_matcher_pi(M,I,O):-var(I),!,trace_or_throw(var_to_matcher_pi(M,I,O)).
to_matcher_pi(_,M:PI, M:Head) :- !,
	to_matcher_pi(M,PI, Head).
to_matcher_pi(M,Name, M:Head) :- atom(Name), !, current_predicate(M:Name/Arity),functor(Head, Name, Arity).
to_matcher_pi(M,Name/Arity, Head) :- var(Arity),!, current_predicate(M:Name/Arity),
	functor(Head, Name, Arity).
to_matcher_pi(M,Name//DCGArity, M:Term) :- 
        var(DCGArity),!,
        current_predicate(Name/Arity), integer(Arity), Arity>=2,
	functor(Term, Name, Arity).
to_matcher_pi(M,Name//DCGArity, M:Term) :- 
        between(0,40,DCGArity),!,
        current_predicate(Name/Arity),
	plus(DCGArity,2,Arity),
	functor(Term, Name, Arity).
to_matcher_pi(M,Head, M:Head).




%= 	 	 

%% m_clause( ?M, ?H, ?B, ?R) is semidet.
%
% Module Clause.
%
m_clause(M,H,B,R):- catch(m_clause0(M,H,B,R),E,R=missing(M,H,B,E)).

%= 	 	 

%% m_clause_no_missing( ?M, ?H, ?B, ?R) is semidet.
%
% Module Clause No Missing.
%
m_clause_no_missing(M,H,B,R):- catch(m_clause0(M,H,B,R),_,fail).


%= 	 	 

%% m_clause0( ?M, ?H, ?B, ?R) is semidet.
%
% Module Clause Primary Helper.
%
m_clause0(M,H,B,R):- atom(M),!, clause(M:H,B,R)*->true;M:clause(H,B,R).
m_clause0(_,H,B,R):- clause(H,B,R).
                              
:- thread_local(t_l:tl_hide_data/1).


%= 	 	 

%% clause_ref( ?HB, ?Ref) is semidet.
%
% Clause Ref.
%
clause_ref(HB,Ref):-as_clause_w_m( HB, M, H, B),m_clause_no_missing(M,H,B,Ref).


%= 	 	 

%% make_searchable( ?HB) is semidet.
%
% Make Searchable.
%
make_searchable(HB):- must(clause_ref(HB,Ref)),make_searchable_ref(HB,Ref).

%= 	 	 

%% make_searchable_ref( ?HB, ?Ref) is semidet.
%
% Make Searchable Ref.
%
make_searchable_ref(HB,Ref):-searchable_of_clause(HB,List),make_searchable_list_ref(List,Ref).

%= 	 	 

%% make_searchable_list_ref( ?List, ?Ref) is semidet.
%
% Make Searchable List Ref.
%
make_searchable_list_ref(List,Ref):- maplist(save_search_ref(Ref),List).


%= 	 	 

%% searchable_of_clause( :TermH, ?List) is semidet.
%
% Searchable Of Clause.
%
searchable_of_clause(C,[]):-var(C),!.
searchable_of_clause(M:C,[module(M),M|List]):-atom(M),!,searchable_of_clause(C,List).
searchable_of_clause(':-'(H,B),List):-B==true,!,searchable_of_clause(H,List).
searchable_of_clause(':-'(H,B),List):-!,searchable_of_clause_1(H,[B],List).
searchable_of_clause(H,List):-searchable_of_clause_0(H,List).


%= 	 	 

%% searchable_of_clause_0( ?C, :TermARG2) is semidet.
%
% searchable of clause  Primary Helper.
%
searchable_of_clause_0(C,[]):-var(C),!.
searchable_of_clause_0(A,[A]):-atomic(A),!.
searchable_of_clause_0(M:C,[module(M),M|List]):-atom(M),!,searchable_of_clause_0(C,List).
searchable_of_clause_0([H|T],[F|List]):- L=[H|T],!,functor(L,F,_),searchable_of_clause_1(H,T,List).
searchable_of_clause_0(C,[funct(F,A),F|List]):- C=..[F,H|T],functor(C,F,A),searchable_of_clause_1(H,T,List).


%= 	 	 

%% searchable_of_clause_1( :TermH, ?List) is semidet.
%
% searchable of clause  Secondary Helper.
%
searchable_of_clause_1([],[]).
searchable_of_clause_1([H|T],List):-searchable_of_clause_1(H,T,List).


%= 	 	 

%% searchable_of_clause_1( ?H, ?T, ?List) is semidet.
%
% searchable of clause  Secondary Helper.
%
searchable_of_clause_1(H,T,[H|List]):-atomic(H),searchable_of_clause_1(T,List).
searchable_of_clause_1(H,T,List):-searchable_of_clause_0(H,List1),searchable_of_clause_1(T,List2),append(List1,List2,List).

:- dynamic(search_refs_use_recorded/0).
	 


% load statistics to keep ifprolog from overriding time/1 ?
%:- abolish(system:time/1).
%:- abolish(time/1).
%:- abolish(ifprolog:time,1).
%:- use_module(library(statistics),[time/1]).
	 	 

%% searchable_terms( ?T) is semidet.
%
% Searchable Terms.
%
searchable_terms(T):-search_refs_use_recorded,!,current_key(Key),unmake_search_key(Key,T).
searchable_terms(T):-unify_in_thread(main,searchable_terms_tl(T)).

%% search_refs_use_recorded is semidet.
%
% Search Refs Use Recorded.
%
search_refs_use_recorded.

searchable_terms_tl(T):- nb_current(GName, _),(atomic_list_concat([_,Name],(':'), GName)-> T=Name ; T = GName).



%= 	 	 

%% make_search_key( ?E, ?Atomic) is semidet.
%
% Make Search Key.
%
make_search_key(E,Atomic):-E=..L,atomic_list_concat(['$search'|L],'%',Atomic).

%= 	 	 

%% unmake_search_key( ?Atomic, ?E) is semidet.
%
% Unmake Search Key.
%
unmake_search_key(Atomic,E):-atomic_list_concat(['$search'|L],'%',Atomic)->E=..L;(E=Atomic).


%= 	 	 

%% get_search_ref( ?E, ?Results) is semidet.
%
% Get Search Ref.
%
get_search_ref(E,Results):-make_search_key(E,Atomic),get_search_ref0(Atomic,Results).


%= 	 	 

%% get_search_ref0( ?Atomic, ?Results) is semidet.
%
% Get Search Ref Primary Helper.
%
get_search_ref0(Atomic,Results):- search_refs_use_recorded,!,findall(R,recorded(Atomic,R),Results).
get_search_ref0(Atomic,Results):- unify_in_thread(main,once(get_search_ref_tl(Atomic,Results))).



%= 	 	 

%% unify_in_thread( +Thread, :Goal) is semidet.
%
% Unify In Thread.
%
unify_in_thread(Thread,once(Goal)):- !,
      message_queue_create(Id),
       call_cleanup((thread_signal(Thread,unify_in_thread_tl(Id,Goal)),thread_get_message(Id,Message),process_unify_in_thread(Message,Goal),!),
         message_queue_destroy(Id)).
unify_in_thread(Thread,Goal):- 
      message_queue_create(Id),
       call_cleanup((thread_signal(Thread,unify_in_thread_tl(Id,Goal)),thread_get_message(Id,Message),process_unify_in_thread(Message,Goal)),
         message_queue_destroy(Id)).


%= 	 	 

%% process_unify_in_thread( ?Message, ?Goal) is semidet.
%
% Process Unify In Thread.
%
process_unify_in_thread(thrown(Message),_Goal):-!,throw(Message).
process_unify_in_thread(failed(_Message),_Goal):-!,fail.
process_unify_in_thread(result(Goal0),Goal1):-!,Goal0=Goal1.
process_unify_in_thread(Message,Goal):-throw(unknown_process_unify_in_thread(Message,Goal)).


%= 	 	 

%% unify_in_thread_tl( ?Id, :Goal) is semidet.
%
% Unify In Thread Thread Local.
%
unify_in_thread_tl(Id,Goal):- catch((forall(Goal,thread_send_message(Id,result(Goal))),thread_send_message(Id,failed(Goal))), E,thread_send_message(Id,thrown(E))).


%= 	 	 

%% get_search_ref_tl( ?Atomic, ?Refs) is semidet.
%
% Get Search Ref Thread Local.
%
get_search_ref_tl(Atomic,Refs):- nb_current(Atomic,Refs)->true;Refs=[].


%= 	 	 

%% save_search_ref( ?Ref, ?E) is semidet.
%
% Save Search Ref.
%
save_search_ref(Ref,E):-make_search_key(E,Atomic),save_search_ref_0(Ref,Atomic).


%= 	 	 

%% save_search_ref_0( ?Ref, ?Atomic) is semidet.
%
% save search ref  Primary Helper.
%
save_search_ref_0(Ref,Atomic):- search_refs_use_recorded,!, save_search_ref_recorded(Ref,Atomic).
save_search_ref_0(Ref,Atomic):- thread_signal(main,save_search_ref_tl(Ref,Atomic)).


%= 	 	 

%% save_search_ref_recorded( ?Ref, ?Atomic) is semidet.
%
% Save Search Ref Recorded.
%
save_search_ref_recorded(Ref,Atomic):-recorded(Atomic,Ref),!.
save_search_ref_recorded(Ref,Atomic):-recordz(Atomic,Ref).


%= 	 	 

%% save_search_ref_tl( ?Ref, ?Atomic) is semidet.
%
% Save Search Ref Thread Local.
%
save_search_ref_tl(Ref,Atomic):-nb_current(Atomic,Refs),(member(Ref,Refs)->true;nb_setval(Atomic,[Ref|Refs])).
save_search_ref_tl(Ref,Atomic):-nb_setval(Atomic,[Ref]).

% that is    CL=beliefs(we,loves(joe,turkey)), asserta(C,Ref),forall(find_each_atom(CL,A),(assert_if_new(idexed_atom(A)),asserta(atom_index(A,Ref)))).



:- multifile baseKB:shared_hide_data/1.
:- dynamic(baseKB:shared_hide_data/1).
% :- kb_shared(baseKB:shared_hide_data/1).



%= 	 	 

%% shared_hide_data( ?VALUE1) is semidet.
%
% Shared Hide Data.
%
baseKB:shared_hide_data(lmcache:varname_info/4):- !,is_listing_hidden(metaInfo).
baseKB:shared_hide_data(lmcache:_):- nop(is_listing_hidden(metaInfo)).
baseKB:shared_hide_data(wid):- is_listing_hidden(metaInfo).
baseKB:shared_hide_data( Head:- True):- nonvar(Head), True==true, baseKB:shared_hide_data( Head ).


%= 	 	 

%% is_listing_hidden( ?P) is semidet.
%
% Listing Filter.
%
is_listing_hidden(MP):- \+ \+ nquietly((strip_module(MP,M,P),is_listing_hidden_00(M,P))).

:- export(is_listing_hidden/1).
:- baseKB:import(is_listing_hidden/1).

nquietly(G):- call(G).
%nquietly(G):- quietly(G).
%= 	 	 

%% is_listing_hidden_00(_, :TermP) is semidet.
%
% Hide Data Primary Helper.
%

% is_listing_hidden_00(_,_):- !, fail.
is_listing_hidden_00(_,M:P):- atom(M),is_listing_hidden_00(M,P).
is_listing_hidden_00(_,P):-var(P),!,fail.
is_listing_hidden_00(_,~(_)):-!,fail.
is_listing_hidden_00(_,metaInfo):- is_listing_hidden(showAll),!,fail.
is_listing_hidden_00(_,P):-t_l:tl_hide_data(P),!.
is_listing_hidden_00(M,P):-baseKB:shared_hide_data(M:P),!.
is_listing_hidden_00(M,F/A):- atom(F),integer(A), compound_name_arity(P,F,A), predicate_property(M:P,number_of_clauses(N)),N>100,is_listing_hidden(largePreds).
is_listing_hidden_00(_,_/_):-!,fail.
is_listing_hidden_00(M,P):- is_meta_info_pred(M:P),!,is_listing_hidden(metaInfo).
is_listing_hidden_00(M,P):- compound(P),functor(P,F,A), (is_listing_hidden_00(M,F/A);is_listing_hidden_00(M,F)).
is_listing_hidden_00(_,'$spft'):- is_listing_hidden(metaInfo),!.
% %%% is_listing_hidden_00(_,P):- predicate_property(P,number_of_clauses(N)),N > 50000,\+ is_listing_hidden(showAll), \+ is_listing_hidden(showHUGE),!.

is_meta_info_pred(Var):- var(Var),!,fail.
is_meta_info_pred(_:P):- !, is_meta_info_pred(P).
is_meta_info_pred(rnc).
is_meta_info_pred(_):- fail.

:- meta_predicate unify_listing(:).

%= 	 	 

%% unify_listing( ?Cntxt) is semidet.
%
% Unify Listing.
%
unify_listing(Cntxt:Pred):-functor_safe(Pred,F,A),unify_listing(Cntxt:Pred,F,A),!.


:- meta_predicate unify_listing_header(:).

%= 	 	 

%% unify_listing_header( ?Pred) is semidet.
%
% Unify Listing Header.
%
unify_listing_header(M:Pred):-!,functor_safe(Pred,F,A),unify_listing_header(M:Pred,F,A),!.
unify_listing_header(Pred):-functor_safe(Pred,F,A),unify_listing_header(Pred,F,A),!.

:- meta_predicate unify_listing_header(:,?,?).

%= 	 	 

%% unify_listing_header( ?CntxtPred, ?F, ?A) is semidet.
%
% Unify Listing Header.
%
unify_listing_header(M:_,F,A):- (format('~n% -=<[ ~q ]>=-~n%~n%',[M:F/A])),fail.
unify_listing_header(MP,_F,_A):- forall((system:predicate_property(MP,PP),\+ unify_listing_header_item(PP)),format(' ~w',[PP])),format('~n%~n~n',[]),fail.
unify_listing_header(M:_P,F,A):- module_property(M,exports(List)),member(F/A,List),format(':- ~q:export(~q).~n',[M,M:F/A]),fail.
unify_listing_header(M:P,F,A):- predicate_property(system:P,imported_from(M)),format(':- system:import(~q).~n',[M:F/A]),fail.
unify_listing_header(M:P,F,A):- predicate_property(M:P,transparent),format(':- ~q:module_transparent(~q).~n',[M,F/A]),fail.
unify_listing_header(M:P,_,_):- predicate_property(M:P,meta_predicate(P)),format(':- ~q:meta_predicate(~q).~n',[M,P]),fail.
unify_listing_header(M:P,F,A):- unify_listing_header_item(Dynamic), 
  predicate_property(M:P,Dynamic),format(':- ~q:~q(~q).~n',[M,Dynamic,F/A]),fail.
unify_listing_header(_M_FileMatch,_F,_A):- format('~n',[]).

unify_listing_header_item(Dynamic):- arg(_,v(public,dynamic, multifile,discontiguous,volatile,thread_local,nodebug),Dynamic).
:- meta_predicate unify_listing(:,?,?).

%= 	 	 

%% unify_listing( ?Cntxt, ?F, ?A) is semidet.
%
% Unify Listing.
%
unify_listing(Cntxt:Pred,F,A):- unify_listing_header(Cntxt:Pred,F,A), Cntxt:printAll(Pred).


%= 	 	 

%% printAll( :Goal) is semidet.
%
% Print All.
%
printAll(Call):-printAll(Call,Call).

%= 	 	 

%% printAll( :Goal, ?Print) is semidet.
%
% Print All.
%
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),portray_clause_w_vars(Print)),fail.
printAll(_Call,Print):- flag(printAll,PA,0),format('~n /* found ~q for ~q. ~n */ ~n',[PA,Print]).


/*
contains_term_unifiable(SearchThis,Find):-Find=SearchThis,!.
contains_term_unifiable(SearchThis,Find):-compound(SearchThis),functor_safe(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term_unifiable(Arg,Find)).
*/


%                         library(http/http_host) compiled into http_host 0.01 sec, 28 clauses


:- thread_local(t_l:no_xlisting/1).
:- thread_local(t_l:in_prolog_listing/1).

% terms listing and varnames
:- export(xlisting/1).
:- module_transparent(xlisting/1).

xlisting:- xlisting([]).

:- create_prolog_flag(retry_undefined,default,[type(term),keep(true)]).

%= 	 	 
:- thread_local(etmp:last_s_l/2).

%% xlisting( ?Match) is semidet.
%
% Xlisting.
%
xlisting(Match):-
  retractall(etmp:last_s_l(_,_)), 
  retractall(lmcache:completely_expanded(_,_)),
  retractall(t_l:no_xlisting(_)),
  xlisting_0(Match).

xlisting_0(Match):- \+ \+ t_l:no_xlisting(Match),!.
xlisting_0([]):- '$current_source_module'(M),!,listing(M:_),'$current_typein_module'(TM),(TM==M->true;listing(TM:_)),!.
xlisting_0(Match):- is_list(Match),!,must_maplist(xlisting_0,Match),!.
xlisting_0(M:P):- atom(M),'$current_source_module'(W), 
   locally(before_after('$set_source_module'(M),
                    '$set_source_module'(W)),
          xlisting_0a(P)),!.
xlisting_0(Match):- 
   xlisting_0a(Match).


xlisting_0a(Match):- 
 % maybe_scan_for_varnames,
 locally(t_l:no_xlisting(Match),
  locally(set_prolog_flag(verbose_autoload,false),
   locally(set_prolog_flag(retry_undefined, none),
    locally(set_prolog_flag(verbose_load,false), 
     xlisting_1(Match))))).


xlisting_1(Match):- t_l:in_prolog_listing(Match),!,findall(PI,to_mpi_matcher(Match,PI),SkipPI),!,
  mpred_match_listing_skip_pi(portray_hbr,Match,[_:varname_info(_,_,_,_)|SkipPI]),!.

xlisting_1(f(Match)):- !,xlisting_inner(portray_hbr,Match,[_:varname_info(_,_,_,_)]),!.

xlisting_1(Match):- mpred_match_listing_skip_pi(portray_hbr,Match,[]),!. % ,locally(t_l:no_xlisting(Match),plisting(Match)),!.

% baseKB:xlisting(G):-xlisting:xlisting(G).
% listing with varnames
:- export(plisting/1).
:- module_transparent(plisting/1).

%= 	 	 

%% plisting( ?Match) is semidet.
%
% Plisting.
%
plisting(Match):- locally(t_l:no_xlisting(Match),xlisting:plisting_0(Match)).

%= 	 	 

%% plisting_0( ?Match) is semidet.
%
% plisting  Primary Helper.
%
plisting_0(Match):- findall(G,to_mpi_matcher(Match,G),Gs),
  forall(member(H,Gs),
    ignore((synth_clause_for(H,B,R,_SIZE,SYNTH),SYNTH,
     once(portray_phbr(portray_hbr,H,B,R)),fail))).


:- export(mpred_match_listing/1).

%= 	 	 

%% mpred_match_listing( ?Match) is semidet.
%
% Managed Predicate Match Listing.
%
mpred_match_listing(Match):- mpred_match_listing_skip_pi(portray_hbr,Match,[]).
:- export(mpred_match_listing_skip_pi/3).
:- meta_predicate(mpred_match_listing_skip_pi(+,+,+)).           

%= 	 	 

%% mpred_match_listing_skip_pi(+How, +Match, +SkipPI) is semidet.
%
% Managed Predicate Match Listing Skip Predicate Indicator.
%
mpred_match_listing_skip_pi(How,Match,SkipPI):- 
  locally(t_l:no_xlisting(Match),
 (
  % format('~N/* mpred_matches(~q) => ~n',[Match]),
   xlisting_inner(How,Match,SkipPI),
  % format(' <= mpred_matches(~q) */ ~n',[Match]).
  !)).



%= 	 	 

%% get_matcher_code( ?Match, ?H, ?B, ?MATCHER) is semidet.
%
% Get Matcher Code.
%
get_matcher_code(Match,H,B,MATCHER):-  atom(Match),!, MATCHER= notrace(term_matches_unify(99,Match,((H:-B)))).
get_matcher_code(Match,H,B,MATCHER):-  MATCHER = notrace(term_matches_term(Match,(H:-B))).

:- meta_predicate xlisting_inner(3,+,+).

%= 	 	 

trans_mask(cyc,kb0988).
trans_mask(wn,wnframes).
trans_mask(sys,'$syspreds').

mymatch_excludes_module(MM,M,MM):- nonvar(M),!.
mymatch_excludes_module(L+all,M2,O):- !,del_attr(M2,freeze),mymatch_excludes_module(L,M2,O).
mymatch_excludes_module(L+M1,M2,O):- trans_mask(M1,M), !,mymatch_excludes_module(L+M,M2,O).
mymatch_excludes_module(L-M1,M2,O):- trans_mask(M1,M), !,mymatch_excludes_module(L-M,M2,O).

mymatch_excludes_module(L-M1,M2,L2-M1):- !,mymatch_excludes_module(L,M2,L2),freeze(M2,M1\==M2),!.
mymatch_excludes_module(L+M1,M2,L2+M1):- !, mymatch_excludes_module(L,M2,L2),
  ignore((frozen(M2,Goals),subst(Goals,M1\==M2,true,NewGoals),del_attr(M2,freeze),freeze(M2,NewGoals))),!.  

mymatch_excludes_module(MM,M2,MM):- dif(kb0988,M2),dif(wnframes,M2),dif(tmp,M2),dif('$syspreds',M2).

pre_init_matcher(_Match,M:_H,_B):-   
  current_module(M).


%% xlisting_inner( :PRED3Pred, +Match, +SkipPI) is semidet.
%
% Xlisting Inner.
%
xlisting_inner(_,portray_phbr(PW,Match),SkipPI):-!,
  xlisting_inner(portray_phbr(PW),Match,SkipPI).
 
xlisting_inner(Printer,Match0,SkipPI):-  
 must_det_l((
   ignore(mymatch_excludes_module(Match0,M,Match)),
   H=M:_,
   get_matcher_code(Match,H,B,MATCHER),!,   
   PRINT = must(ignore((once(call(Printer,H,B,Ref))))),   
   PREDZ = ( (pre_init_matcher(Match,H,B),
              synth_clause_for(H,B,Ref,Size,SYNTH)), \+member(H,SkipPI), \+is_listing_hidden(H)),
   forall(PREDZ,
     must(( 
      (is_listing_hidden(wholePreds),integer(Size),Size<100)
        -> 
          ( \+ \+ ((SYNTH,MATCHER)) -> (forall(SYNTH,PRINT)) ; true) 
         ; 

        ((forall(SYNTH,(MATCHER->PRINT;true))))))))),!.
 
      

:- multifile user:prolog_list_goal/1.

%= 	 	 

%% prolog_list_goal( :TermGoal) is semidet.
%
% Hook To [user:prolog_list_goal/1] For Module Logicmoo_util_term_listing.
% Prolog List Goal.
%
user:prolog_list_goal(Goal):- nquietly(xlisting(Goal)). % writeq(hello(prolog_list_goal(Goal))),nl.


% :- dynamic(buggery_ok/0).
:- export(buggery_ok/0).
:- thread_local(tlbugger:no_buggery_tl/0).
:- dynamic(baseKB:no_buggery/0).


%= 	 	 

%% buggery_ok is semidet.
%
% Buggery Ok.
%
buggery_ok :- \+ compiling, current_predicate(_:logicmoo_bugger_loaded/0), \+ baseKB:no_buggery, \+ tlbugger:no_buggery_tl,!.


:- multifile((synth_clause_for/5)).
:- export((synth_clause_for/5)).

% bookeepingPredicate(M:G):- member(M:F/A,[M:'$exported_op'/3]),current_module(M),functor(G,F,A),once(predicate_property(M:G,_)).

%= 	 	 

%% bookeepingPredicateXRef( :TermARG1) is semidet.
%
% Bookeeping Predicate X Ref.
%
bookeepingPredicateXRef(user:file_search_path(_,_)).
%bookeepingPredicateXRef_never(wordnet:wn_s(_,_,_ ,_,_,_)).
%bookeepingPredicateXRef(_:G):-member(F/A,[xref_defined/3,xref_called/3,xref_exported/2]),functor(G,F,A).

%= 	 	 

%% predicateUsesCall( :TermARG1) is semidet.
%
% Predicate Uses Call.
%
predicateUsesCall(_:G):-
  member(F/A,[module_property/2,predicate_property/2,pengine_property/2,current_pengine_application/1,source_file_property/2,
            source_file/2,current_prolog_flag/2,current_op/3]),
  functor(G,F,A).


%= 	 	 

%% sourceTextPredicate( ?M) is semidet.
%
% Source Text Predicate.
%

sourceTextPredicate(M:G):- fail, M=el_assertions, source_file((M:el_holds(_,_,_,_,_,_,_)),F) -> source_file(M:G,F).

%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_implies,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_implies_t,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_t,A),current_predicate(_,el_assertions:G).
sourceTextPredicate(_):-fail.


%= 	 	 

%% sourceTextPredicateSource( ?VALUE1) is semidet.
%
% Source Text Predicate Source.
%
sourceTextPredicateSource(_):-fail.

:- thread_local(t_l:large_predicates/2).


%= 	 	 

%% plisting_1 is semidet.
%
% plisting  Secondary Helper.
%
plisting_1:-plisting('$spft'(_,_,_,_)).


%= 	 	 

%% synth_clause_for(MyMatch, ?G, ?B, :GoalRef, :PRED222Size, ?SYNTH) is semidet.
%
% Synth Clause For.
%
:- multifile(xlisting_config:xlisting_always/1).
:- dynamic(xlisting_config:xlisting_always/1).  

synth_clause_for(G,true,0,222, SYNTH):- 
   G=M:H, xlisting_config:xlisting_always(G),
   SYNTH = m_clause(M,H,_B,_Ref).
   %SYNTH = on_x_fail(G).

synth_clause_for(G,true,0,244,SYNTH):-  bookeepingPredicateXRef(G), 
  nquietly(( \+ is_listing_hidden(metaInfo))), SYNTH=on_x_fail(G).

synth_clause_for(G,B,Ref,Size,SYNTH):- cur_predicate(_,G), (((nquietly(( \+ bookeepingPredicateXRef(G), \+ sourceTextPredicate(G), 
                                                                \+ is_listing_hidden(G))))), 
                                                                SYNTH = (synth_clause_ref(G,B,Ref,Size,SYNTH2),SYNTH2)).
synth_clause_for(G,true,0,223, SYNTH):-  sourceTextPredicate(G), \+ is_listing_hidden(G), SYNTH = on_x_fail(G).
synth_clause_for(G,  B, Ref,Size, SYNTH):- !, 
  gripe_time(10,synth_clause_for_l2(G,B,Ref,Size,SYNTH)).
 

%= 	 	 

%% synth_clause_for_l2( :TermM, ?B, ?Ref, ?Size, ?SYNTH) is semidet.
%
% Synth Clause For (list Version) Extended Helper.
%
synth_clause_for_l2(M:H,B,Ref,Size,SYNTH):- 
  findall((Size- (M:H)),retract(t_l:large_predicates(M:H,Size)),KeyList),keysort(KeyList,KeySorted),!,
  synth_clause_for_large(M:H,B,Ref,KeySorted,Size,SYNTH).


%= 	 	 

%% synth_clause_for_large( ?M, ?B, ?Ref, :TermKeySorted, :GoalSize, ?M) is semidet.
%
% Synth Clause For Large.
%
synth_clause_for_large(_,_,_,[   ],0,(!,fail)):-!.
synth_clause_for_large(_,_,_,[_|_],0,(!,fail)):- is_listing_hidden(skipLarge),!.
synth_clause_for_large(M:H,B,Ref,KeySorted,Size,m_clause(M,H,B,Ref)):-   
    %  format('~N% Synthesizing the larger preds now ~q .~n',[KeySorted]),!,
      member( (Size- (M:H)) , KeySorted) *-> \+ is_listing_hidden(M:H).

:- export((synth_clause_ref/5)).

%= 	 	 

%% synth_clause_ref( :TermARG1, ?B, ?Ref, ?Size, ?CALL) is semidet.
%
% Synth Clause Ref.
%
synth_clause_ref(_:no_xlisting(_),_B,_Ref, _Size, _CALL):-!,fail.
synth_clause_ref(_:in_prolog_listing(_),_B,_Ref, _Size, _CALL):-!,fail.
synth_clause_ref(_:varname_info(_,_,_,_),_B,_Ref,_Size, _CALL):- \+ is_listing_hidden(showAll),!,fail.

synth_clause_ref(M:H,'$info'(B),Ref, 250, SYNTH):- \+ is_listing_hidden(metaInfo), 
  SYNTH= (findall(PP,predicate_property(M:H,PP),PPL),Ref=0,
  %CPPL=..['$'|PPL],
  CPPL=PPL,
  B=M:('$predicate_property'(H,CPPL))).
synth_clause_ref(MHG,B,Ref, 213, SYNTH):- predicateUsesCall(MHG),synth_in_listing(MHG), !, 
  SYNTH= (on_x_fail(MHG),Ref=0,B='$info'(predicateUsedCall)).

synth_clause_ref(M:H,B,Ref, 200, SYNTH):-     
    xlisting_config:xlisting_always(M:H),!, SYNTH= m_clause(M,H,B,Ref).

synth_clause_ref(M:H,B,Ref, Size, SYNTH):- 
    predicate_property(M:H,number_of_clauses(Size)),!, 
    SYNTH= m_clause(M,H,B,Ref).


synth_clause_ref(M:H,B,Ref, Size, SYNTH):- 
    predicate_property(M:H,number_of_clauses(Size)),synth_in_listing(M:H),!, 
    xlisting_config:xlisting_always(M:H), SYNTH= m_clause(M,H,B,Ref).

synth_clause_ref(M:H,B,Ref, Size, SYNTH):- 
    predicate_property(M:H,number_of_clauses(Size)),synth_in_listing(M:H),!, 
    SYNTH= m_clause(M,H,B,Ref).

/*
synth_clause_ref(M:H,B,Ref,Size, SYNTH):- predicate_property(M:H,number_of_clauses(Size)),
  Size > 500000,  !,  is_listing_hidden(showHUGE), SYNTH = m_clause(M,H,B,Ref),synth_in_listing(M:H).
synth_clause_ref(M:H,B,Ref,Size, SYNTH):- predicate_property(M:H,number_of_clauses(Size)),synth_in_listing(M:H),
  (Size > 5000 ->  ( \+ is_listing_hidden(skipLarge), asserta(t_l:large_predicates(M:H,Size)),fail) ; SYNTH = m_clause(M,H,B,Ref)).

*/


%= 	 	 

%% synth_in_listing( ?MH) is semidet.
%
% Synth In Listing.
%
synth_in_listing(MH):- ( \+ is_listing_hidden(MH), \+ sourceTextPredicateSource(MH) ),!.

:- export((term_matches_hb/3)).




%= 	 	 

%% term_matches_hb( ?HO, ?H, ?B) is semidet.
%
% Term Matches Head+body.
%
term_matches_term(HO,HB):-term_matches_hb(999,HO,HB),!.

%= 	 	 

%% term_matches_hb( ?VALUE1, :TermVar, ?VALUE3, ?VALUE4) is semidet.
%
% Term Matches Head+body.
%
term_matches_hb(_,Var,_):-var(Var),!.

term_matches_hb(_,[],_):-!.
term_matches_hb(D,_,_):- integer(D), D<0,!,fail.

term_matches_hb(D,noinfo,H):- !, \+ term_matches_hb(D,unify('$info'(_)),H).

term_matches_hb(D,head(P),HB):-!,expand_to_hb(HB,H,_),strip_module(H,_,H0), !, term_matches_hb(D,P,H0).
term_matches_hb(D,body(P),HB):-!,expand_to_hb(HB,_,B), term_matches_hb(D,P,B).
term_matches_hb(_,unify(N,HO),HB):- !,term_matches_unify(N,HO,HB).
term_matches_hb(D,unify(HO),HB):- !,term_matches_unify(D,HO,HB).
term_matches_hb(D,(F1+FS),HB):-!,term_matches_hb(D,(F1),HB), \+ \+(term_matches_hb(D,FS,HB)).
term_matches_hb(D,(F1-FS),HB):-!,term_matches_hb(D,(F1),HB), \+(term_matches_hb(D,FS,HB)).
term_matches_hb(D,(F1,FS),HB):-!,term_matches_hb(D,F1,HB),term_matches_hb(D,FS,HB).
term_matches_hb(D,(F1;FS),HB):-!, (term_matches_hb(D,F1,HB);term_matches_hb(D,FS,HB)).
term_matches_hb(D,[F1],HB):-!,term_matches_hb(D,F1,HB),!.
term_matches_hb(D,[F1|FS],HB):-!,term_matches_hb(D,(F1;FS),HB).
term_matches_hb(D,-(C),HB):-nonvar(C),!,\+(term_matches_hb(D,C,HB)).
term_matches_hb(D,not(C),HB):-nonvar(C),!,\+(term_matches_hb(D,C,HB)).
term_matches_hb(D,+(C),HB):-nonvar(C),!, term_matches_hb(D,C,HB).
term_matches_hb(D,M:HO,HB):-!,term_matches_hb(D,(module(M),HO),HB).
%term_matches_hb(_,cistring(HO),HB):- nonvar(HO),any_to_atom(HO,HS),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
term_matches_hb(_,depth(Depth,HO),HB):- term_matches_hb(Depth,HO,HB).
term_matches_hb(D,F/A,HB):-atom(F),var(A),!,term_matches_hb(D,functor(F),HB).
term_matches_hb(D,F/A,HB):-var(F),integer(A),!,term_matches_hb(D,arity(A),HB).
term_matches_hb(D,F/A,HB):-atom(F),integer(A),!,functor(P,F,A),!,expand_to_hb(HB,H,B), (term_matches_unify(D,P,H);term_matches_unify(D,P,B)).
term_matches_hb(D,F/A,HB):-var(F),var(A),!,fail,term_matches_hb(D,call1(compound),HB).
% term_matches_hb(D,P,HB):- (term_matches_unify(D,P,H);term_matches_unify(D,P,B)).
term_matches_hb(_,contains(HO),HB):- !, my_wildcard_match(HO,HB,false).
term_matches_hb(_,match(HO),HB):- !, my_wildcard_match(HO,HB,true).
term_matches_hb(D,HO,HB):- expand_to_hb(HB,H,B),
 (term_matches_unify(D,HO,H); 
   (B\==H,B\==true,term_matches_unify(D,HO,B))).


% ?- xlisting((head(depth(0,'$pt'/3)),same(tBird(A)))).

:- export(term_matches_unify/3).

my_wildcard_match(HO,HB,TF):- 
  nonvar(HO),any_to_string(HO,HS),!, with_output_to(string(H1B1),write_canonical((HB))),!,
  my_wildcard_match_1(HS,H1B1,TF).

my_wildcard_match_1(HS,H1B1,false):- !, (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
my_wildcard_match_1(HS,H1B1,TF):- wildcard_match(HS,H1B1,[case_sensitive(TF)]),!.
%= 	 	 

term_matches_module(M,F):- atom(F),!,current_predicate(M:F/A),
 compound_name_arity(HB,F,A), \+ predicate_property(M:HB, imported_from(_)).
term_matches_module(M,HB):- compound(HB),!,compound_name_arity(HB,F,A), 
  \+ predicate_property(M:HB, imported_from(_)), current_predicate(M:F/A).

%% term_matches_unify( :GoalR, ?V, ?V) is semidet.
%
% Term Matches Unify.
%
term_matches_unify(_,M,V):- var(M),!, var(V),!.
term_matches_unify(_,call1(HO),V):- callable(HO),call(HO,V), !.
term_matches_unify(_,M,V):- var(V),!,M==var,fail.
term_matches_unify(_,same(HO),V):-HO=@=V,!.
term_matches_unify(_,V1,V2):- V1=@=V2,!.
%(sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
term_matches_unify(_,A,Str):- string(Str),atom(A),!,atom_string(A,Str).
term_matches_unify(_,unify(HO),V):- nonvar(HO),!, \+ HO \= V, !.
term_matches_unify(_,module(M),HB):- term_matches_module(M,HB).
term_matches_unify(_,functor(F),H):- atom(H),!,H==F.
term_matches_unify(_,functor(F),H):- !, compound_name_arity(H,F,_).
term_matches_unify(_,arity(R),H):- atom(H),!,xin_range(0,R).
term_matches_unify(_,arity(R),H):- !, compound(H), compound_name_arity(H,_,A), xin_range(A,R).
term_matches_unify(_,_,V):- \+ compound(V),!,fail.
%term_matches_unify(D,F/A,HB):- atom(F),integer(A),!, compound_name_arity(P,F,A), term_matches_unify(D,P,HB).
%term_matches_unify(R,M,O):- compound(O), sub_term(I,O), nonvar(I), term_matches_unify(R,M,I),!.
term_matches_unify(0,_,_):- !,fail.
term_matches_unify(R,HO,V):- RR is R -1, compound_name_arguments(V,F,ARGS),member(E,[F|ARGS]),term_matches_unify(RR,HO,E),!.

xin_range(A,R):- number(R),!,A == R.
xin_range(A,between(L,H)):- !, between(L,H,A).
xin_range(A,R):- R=..[F|List],C=..[F,A|List],call(C),!.

%= 	 	 

%% nonvar_search( :TermV) is semidet.
%
% Nonvar Search.
%
nonvar_search(V):-var(V),!,fail.
nonvar_search(M:_/A):-nonvar(M),!,nonvar(A).
nonvar_search(M:P):-nonvar(M),nonvar(P).
nonvar_search(F/A):-!,nonvar(F),nonvar(A).
%nonvar_search(F):-atom(F).
%nonvar_search(P):-compound(F).

:- dynamic(cur_predicates/1).
:- export((cur_predicate)/2).


%= 	 	 

%% cur_predicate( :TermM, :TermM) is semidet.
%
% Cur Predicate.
%
cur_predicate(M:F/A,M:P):-atomic(M),compound(P),!,ignore(functor(P,F,A)).
cur_predicate(M:F/A,M:P):- current_module(M),
   current_predicate(M:F/A),functor(P,F,A),\+ predicate_property(M:P,imported_from(_)).

  /*
cur_predicate(M:F/A,MP):-atom(M),var(F),!,cur_predicate(M,F/A,MP).
cur_predicate(MFA,M:P):-atom(M),var(P),!,cur_predicate(M,MFA,P).
cur_predicate(M:FA,MP):-atom(M),!,cur_predicate(M,FA,MP).
cur_predicate(MFA,MP):-cur_predicate(_,MFA,MP).
cur_predicate(M,MFA,MP):-nonvar(M),
 M:call(cur_predicate(MFA,MP)).

cur_predicate(SM,MFA,MP):-
   (

 %nonvar_search(MFA) -> SEARCH=MFA ; 
 %nonvar_search(MP) -> SEARCH=MP ;
    
  nonvar(MFA) ->    (current_predicate(MFA),SEARCH=MFA);
    
  nonvar(MP) ->     (current_predicate(_,MP),SEARCH=MP);
   
   nonvar(SM) ->  (SM:current_predicate(SF/SA),SEARCH=M:SF/SA);
                      
    current_predicate(SEARCH)),
  no_repeats(M:F/A,(match_predicates(SEARCH,MFAs),
   
  member(M:F/A,MFAs))),
   
   must_det_l((
  once(two_mfa(MFA, M,F,A)),

       functor(P,F,A),
  once(to_mp(MP,M,P)))).
   ignore(SM=M).


% match_mfa(MFA1,MFA2):-must_det_l((to_mfa(MFA1,M1,F1,A1),to_mfa(MFA2,M2,F2,A2))),A1=A2,F1=F2,!,ignore(M1=M2).


two_mfa(M1:F/A,M2,F,A):-!,ignore(M1=M2).
two_mfa(M1:F,M2,F,_):-atom(F),!,ignore(M1=M2).
two_mfa(M1:FA,M2,F,A):- nonvar(FA),!, get_functor(FA,F,A),ignore(M1=M2).

two_mfa(F,_,F,_):- atom(F),!.
two_mfa(F/A,_,F,A):- !.
to_mp(M1:P,M2,P):-ignore(M1=M2).
to_mp(P,_,P):-nonvar(P),!.
  */

:- export(ok_show/1).

%= 	 	 

%% ok_show( ?P) is semidet.
%
% Ok Show.
%
ok_show(F/A):-!,functor(P,F,A),ok_show(P),!.
ok_show(P):-not(bad_pred(P)).



% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
:- export(scansrc_list_undefined/1).

%= 	 	 

%% scansrc_list_undefined( ?A) is semidet.
%
% Scansrc List Undefined.
%
%scansrc_list_undefined(_):-!.
scansrc_list_undefined(A):- real_list_undefined(A).


:- thread_local check:undef/2.
:- dynamic check:undef/2.
:- volatile check:undef/2.

:- export(real_list_undefined/1).

%= 	 	 

%% real_list_undefined( ?A) is semidet.
%
% Real List Undefined.
%


:- public check:collect_undef/1.

real_collect_undef(Grouped) :-
    findall(PI-From,
            retract(check:undef(PI, From)),
            Pairs),
    keysort(Pairs, Sorted),
    check:group_pairs_by_key(Sorted, Grouped).


:- export(real_list_undefined/1).
real_list_undefined(Options) :-
    merge_options(Options, [module_class([user])], WalkOptions),
    call_cleanup(prolog_walk_code(
                                  [ undefined(trace),
                                    on_trace(check:found_undef)
                                  | WalkOptions
                                  ]),
                 real_collect_undef(Grouped)),
    (   Grouped==[]
    ->  true
    ;   print_message(warning, check(undefined_procedures, Grouped))
    ).

real_list_undefined :-
    real_list_undefined([]).
full_list_undefined:- real_list_undefined([module_class([user,library,system])]).
/*
836 ?- real_list_undefined([module_class([user,library,system])]).
Warning: The predicates below are not defined. If these are defined
Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: win_get_user_preferred_ui_languages/2, which is referenced by
Warning:        /usr/lib/swi-prolog/boot/messages.pl:1842:4: 1-st clause of os_user_lang/1
Warning: '$urgent_exception'/3, which is referenced by
Warning:        /usr/lib/swi-prolog/boot/syspred.pl:1580:12: 2-nd clause of run_undo/3
Warning: win_registry_get_value/3, which is referenced by
Warning:        /usr/lib/swi-prolog/boot/toplevel.pl:479:10: 1-st clause of '$set_prolog_file_extension'/0
Warning: bugger:fif/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/debuggery/dmsg.pl:1764:27: 6-th clause of bugger:mUST_det_ll/1
Warning: bugger:noRTrace/0, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/debuggery/dmsg.pl:1777:59: 1-st clause of bugger:mUST_det_ll_failed/1
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/debuggery/dmsg.pl:1781:21: 2-nd clause of bugger:rRTrace/1
Warning: bugger:parent_goal/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/debuggery/ucatch.pl:1025:38: 1-st clause of bugger:uexecute_goal_vs0/1
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/debuggery/ucatch.pl:1026:38: 2-nd clause of bugger:uexecute_goal_vs0/1
Warning: cp_menu:sort_menu_popups/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web.pl:300:36: 2-nd clause of xlisting_web:do_sort_menu_popups/2
Warning: dictoo_lib:nb_rb_insert/4, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/dictoo/prolog/dictoo_lib.pl:225:49: 5-th clause of dictoo_lib:oo_put_dict/4
Warning:        /home/norights/.local/share/swi-prolog/pack/dictoo/prolog/dictoo_lib.pl:162:41: 5-th clause of dictoo_lib:oo_set/3
Warning: in_pce_thread_sync/1, which is referenced by
Warning:        /usr/lib/swi-prolog/xpce/prolog/lib/gui_tracer.pl:80:4: 3-th clause of guitracer/0
Warning:        /usr/lib/swi-prolog/xpce/prolog/lib/gui_tracer.pl:139:8: 1-st clause of trace_goal_2/1
Warning: gvs:is_oo_hooked/4, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/dictoo/prolog/dictoo_lib.pl:506:68: 2-nd clause of dictoo:is_dot_hook/4
Warning: start_emacs/0, which is referenced by
Warning:        /usr/lib/swi-prolog/library/doc_http.pl:242:4: 1-st clause of prepare_editor/0
Warning: pretty_clauses:bfly_write_s/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/butterfly_console.pl:323:69: 2-nd clause of pretty_clauses:bfly_write_h/1
Warning: pretty_clauses:is_user_output/0, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/portray_vars.pl:414:19: 2-nd clause of pretty_clauses:make_pretty/2
Warning: pretty_clauses:print_tree_unit/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/pretty_clauses.pl:561:4: 1-st clause of pretty_clauses:print_tree_loop/2
Warning: pretty_clauses:shrink_naut_vars/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/portray_vars.pl:414:36: 2-nd clause of pretty_clauses:make_pretty/2
Warning: in_pce_thread/1, which is referenced by
Warning:        /usr/lib/swi-prolog/library/edit.pl:302:4: 2-nd clause of do_edit_source/1
Warning: swish_config:json_config/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web_server.pl:24: 1-st clause of xlisting_web_server:swish_reply_config_root/0
Warning: swish_config:reply_json/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web_server.pl:24: 1-st clause of xlisting_web_server:swish_reply_config_root/0
Warning: swish_config:swish_reply_config/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web_server.pl:21:19: 1-st clause of xlisting_web_server:swish_reply_config_root/1
Warning: never_move/2, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/predicate_inheritance.pl:437: 3-th clause of do_inherit_above/2
Warning: window_title/2, which is referenced by
Warning:        /usr/lib/swi-prolog/boot/toplevel.pl:441:11: 1-st clause of set_window_title/1
Warning: make_clpfd_var/1, which is referenced by
Warning:        /usr/lib/swi-prolog/library/clp/clpfd.pl:7855:8: 1-st clause of exception/3
Warning: util_varnames:parent_goal/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/util_varnames.pl:266:37: 1-st clause of util_varnames:execute_goal_vs0/1
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/util_varnames.pl:267:37: 2-nd clause of util_varnames:execute_goal_vs0/1
Warning: util_varnames:swc/0, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo/util_varnames.pl:1222:20: 2-nd clause of util_varnames:scan_for_varnames/0
Warning: xlisting_web:b/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web.pl:3010:7: 1-st clause of xlisting_web:a_ok/1
Warning: xlisting_web:dsmg/1, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web.pl:1218:56: 5-th clause of xlisting_web:write_atom_link/3
Warning: xlisting_web:guitracer/0, which is referenced by
Warning:        /home/norights/.local/share/swi-prolog/pack/logicmoo_utils/prolog/xlisting/xlisting_web.pl:2130:31: 1-st clause of xlisting_web:do_guitracer/0
*/

:- export(mmake/0).
:- system:import(mmake).
%= 	 	 

%% mmake is semidet.
%
% Mmake.
%
% 

% mmake:- lmcache:thread_main(user,Main), \+ thread_self(Main), !.
% mmake:- lmcache:thread_main(user,Main),!,thread_signal(Main,catch(((ignore(update_changed_files), ignore(if_defined(mpred_update_changed_files,true)))),_,true)).
mmake:- thread_signal(main,catch(((ignore(update_changed_files), ignore(if_defined(mpred_update_changed_files,true)))),_,true)).


:- export(update_changed_files/0).

%= 	 	 

%% update_changed_files is semidet.
%
% Update Changed Files.
%
update_changed_files:- thread_self(main),!,update_changed_files1.
update_changed_files:- !.
update_changed_files:- !,thread_signal(main,update_changed_files0).

%= 	 	 

%% update_changed_files0 is semidet.
%
% Update Changed Files Primary Helper.
%
update_changed_files0 :- get_main_error_stream(Err),!,with_output_to(Err,update_changed_files1).

%= 	 	 

%% update_changed_files1 is semidet.
%
% Update Changed Files Secondary Helper.
%
update_changed_files1 :- 
 current_prolog_flag(verbose_load,VL),
 locally(set_prolog_flag(verbose_load,VL),
   with_no_dmsg(make:((        
        '$update_library_index',
    findall(File, make:modified_file(File), Reload0),
    list_to_set(Reload0, Reload),
    (   prolog:make_hook(before, Reload)
    ->  true
    ;   true
    ),
    print_message(silent, make(reload(Reload))),
    maplist(reload_file, Reload),
    print_message(silent, make(done(Reload))),
    (   prolog:make_hook(after, Reload)
    ->  true
    ;   nop(list_undefined),
        nop(list_void_declarations)
    ))))).
    

:- export(remove_undef_search/0).
% check:list_undefined:-real_list_undefined([]).

%= 	 	 

%% remove_undef_search is semidet.
%
% Remove Undef Search.
%

% remove_undef_search:- !.
remove_undef_search:- 
 once((
 '@'(use_module(library(check)),'user'),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assertz((check:list_undefined(A):- \+ thread_self_main ,!, ignore(A=[]))),
 %assert((check:list_undefined(A):- dmsg(check:list_undefined(A)),!)),
 assertz((check:list_undefined(A):- check:reload_library_index,  update_changed_files, call(thread_self_main),!, ignore(A=[]))),
 assertz((check:list_undefined(A):- ignore(A=[]),scansrc_list_undefined(A),!)),
 assertz((check:list_undefined(_))),
 redefine_system_predicate(check:list_void_declarations),
 abolish(check:list_void_declarations/0),
 asserta(check:list_void_declarations))),
 wdmsg('removed check:list_undefined and list_void_declarations use:  ?- real_list_undefined([]),real_list_void_declarations. ').

% :- remove_undef_search.
/*
:- multifile(check:list_undefined/1).
:- dynamic(check:list_undefined/1).
:- system:use_module(library(make)), system:use_module(library(check)), 
   redefine_system_predicate(check:list_undefined/1).
:- asserta((check:list_undefined(Stuff):- Stuff==[], dmsg(list_undefined(Stuff)),!)).
*/
:- export(real_list_void_declarations/0).
real_list_void_declarations :-
 check:(
    P=_:_,
    (   predicate_property(P, undefined),
        (   '$get_predicate_attribute'(P, meta_predicate, Pattern),
            print_message(warning,
                          check(void_declaration(P,
                                                 (meta_predicate Pattern))))
        ;   void_attribute(Attr),
            '$get_predicate_attribute'(P, Attr, 1),
            print_message(warning, check(void_declaration(P, Attr)))
        ),
        fail
    ;   predicate_property(P, discontiguous),
        \+ ( predicate_property(P, number_of_clauses(N)),
             N>0
           ),
        print_message(warning, check(void_declaration(P, discontiguous))),
        fail
    ;   true
    )).


%= 	 	 

%% mp( ?M, ?P, ?MP) is semidet.
%
% Module Goal.
%
mp(M,P,MP):-atom(M),!,(MP=M:P ; MP=P).
mp(_,P,MP):-MP=P.


:- export(bad_pred/1).

%= 	 	 

%% bad_pred( :TermM) is semidet.
%
% Bad Predicate.
%
bad_pred(M:P):-!,atom(M),bad_pred(P). 
%bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,mpred_op/_,mpred_op00/_,mpred_op0/_,mpred_op_loop/_,do_expand_args_l/3),F/A).
%bad_pred(P):-predicate_property(P,autoloaded(_)).
%bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
%bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
%bad_pred(P):-predicate_property(P,foreign).

:- export(portray_phbr/4).
:- export(portray_hbr/3).
:- export(portray_hb/2).

%= 	 	 

%% portray_phbr(PW, :TermH, :TermB, ?R) is semidet.
%
% Portray Hbr.
%
portray_hbr(H,B,R):-
 portray_phbr(print_tree_stop,H,B,R),!.

% print_tree_stop(H):- functor_color(H,C),ansicall(current_output,C,print_tree_stop_1(H)).
print_tree_stop(H):- print_tree_stop_1(H),!.

print_tree_stop_1(H):- \+ in_pp(ansi), !, print_tree_with_final(H,'.').
print_tree_stop_1(H):-  pprint_ecp(yellow,H).

portray_phbr(_PW,M: P, M:pp(PPL),_):- (atom(P);compound(P)),format('~N~n'),
       in_cmt(print(P=PPL)),!,format('~N~n').

portray_phbr(PW,M:P,'$info'(M:'$predicate_property'(P,Props)),_):- (atom(P);compound(P)),
       % functor(P,F,A), NEWH = pp(M:F/A,Props),
       pprint_ecp_cmt([hfg(black)],portray_hb1(PW,'$predicate_property'(P),Props)),!.

portray_phbr(PW,H,B,Ref):- var(Ref), clause_u_here(H,B,Ref), nonvar(Ref),!, portray_phbr(PW,H,B,Ref),!.
portray_phbr(PW,H,B,in_cmt(NV)):- in_cmt(portray_phbr(PW,H,B,NV)),!.

portray_phbr(_,H,B,Ref) :- nonvar(Ref),
    catch(clause_property(Ref,module(M)),_,fail),
    once((on_x_fail(((prolog_listing_list_clause((M:(H:-B)),Ref,_,[source(true)])))); 
          (on_x_fail((prolog_listing_list_clause(_,Ref,_,[source(true)])))))).          

portray_phbr(PW,H,B,Ref):- 
 portray_refinfo(Ref),portray_hb1(PW,H,B).

clause_u_here(H,B,Ref):- catch(call(call,clause_u(H,B,Ref)),_,clause(H,B,Ref)).

portray_refinfo(R):- (var(R) ; R == 0),!.
portray_refinfo(R):- \+ catch(clause_property(R,module(_)),_,fail), in_cmt(format('Ref: ~p',[R])),!.
portray_refinfo(R):- clause_property(R,erased), in_cmt(format('Warn: ~p is erased!',[R])),fail.
portray_refinfo(R):- source_file_info(R,Info), ignore((Info\=unk:_, in_cmt(format('Fileinfo: ~w',[Info])),!,format('~N',[]))).
portray_refinfo(R):- in_cmt(format('Ref: ~q',[R])),!,format('~N',[]).

source_file_info(R,F:L):- (clause_property(R,line_count(L));L= (-1)), (clause_property(R,file(F));clause_property(R,source(F));F=unk).
%= 	 	 

%% portray_hb( ?H, ?B) is semidet.
%
% Portray Head+body.
%

portray_hb(H,B):- portray_hb1(print_tree,H,B),!.
portray_hb(H,B):- portray_hb1(print_ecp(white),H,B).


portray_hb1(PW,H,B):- B==true, !, portray_one_line(PW,H),format('~N').
portray_hb1(PW,H,B):-  portray_one_line(PW,(H:-B)), format('~N').



:- export(portray_one_line/2).
:- thread_local(baseKB:portray_one_line_hook/1).

:- meta_predicate(catch_each(:,-,-)).

catch_each(M:G,E,Or):-
  LE = ex((failed_catch_each(G))),
  catchv((
  ((must(clause(M:G,B,Ref)), once(clause_property(Ref,module(Module));Module=M),
  (Module:catchv(M:B,E,(nb_setarg(1,LE,E),fail))))->!;(LE = ex(W),throw(W)))),E,Or).


%% portray_one_line(PW, ?H) is semidet.
%
% Portray One Line.
%
portray_one_line(_,H):- nquietly((tlbugger:no_slow_io,!, writeq(H),write('.'),nl)),!.
portray_one_line(PW,H):-  nquietly((catch_each(portray_one_line0(PW,H),_,(writeq(H),write('.'),nl)))),!.

portray_one_line0(_,H):- baseKB:portray_one_line_hook(H),!.
portray_one_line0(_,H):- maybe_separate(H,(format('~N~n'))),fail.
portray_one_line0(PW,H):- \+ \+ ((logicmoo_varnames:get_clause_vars(H), portray_one_line1(PW,H))),!.
portray_one_line0(PW,H):- portray_one_line1(PW,H),!.

portray_one_line1(PW,H):- ignore(mort(xlisting_console:catch(portray_one_line2(PW,H),_,fail))),!.

%portray_one_line2(_PW,H):- write_term(H,[portray(true),nl(false)]),!,write('.'),nl.
portray_one_line2( PW,H):- on_x_fail(call(PW,H)),!.
portray_one_line2(_PW,H):- print(H),!,write('.'),nl.
%portray_one_line2(_PW,H):- on_x_fail(user:portray(H)),!,write('.'),nl.
portray_one_line2(_PW,H):- on_x_fail(pprint_ecp(green,H)),!.
portray_one_line2(_PW,H):- writeq(H),!,write('.'),nl.
portray_one_line2(_PW,H):- display(H),!,write('.'),nl.


:- thread_local t_l:last_portray_key/1.

%= 	 	 

%% maybe_separate( ?H, :GoalHow) is semidet.
%
% Maybe Separate.
%
maybe_separate(H,How):-make_headkey(H,HK),maybe_separate_0(HK,How).

%= 	 	 

%% maybe_separate_0( ?NK, :GoalHow) is semidet.
%
% maybe separate  Primary Helper.
%
maybe_separate_0(NK,How):- \+ t_l:last_portray_key(_), asserta(t_l:last_portray_key(NK)),!,How.
maybe_separate_0(NK,How):- retract(t_l:last_portray_key(OK)),!,
   ( ( \+ OK=NK) ->  How ; true),!,asserta(t_l:last_portray_key(NK)),!.

maybe_separate_0(NK,How):- 
  (t_l:last_portray_key(LK) -> 
         ((LK = NK) -> true ; (How,retractall(t_l:last_portray_key(LK)),asserta(t_l:last_portray_key(NK)),How));
         asserta(t_l:last_portray_key(NK))),!.


%= 	 	 

%% make_headkey( :TermH, :TermNK) is semidet.
%
% Make Headkey.
%
make_headkey(V,var):-var(V),!.
make_headkey((H:-TRUE),NK):-nonvar(TRUE),!,make_headkey(H,NK).
make_headkey(M:F/_, M:F):-atom(F).
make_headkey(F  /_, _:F):-atom(F).
make_headkey(M:H,M:NK):-nonvar(M),make_headkey(H,NK),!.
make_headkey(H,NK):-compound(H),functor(H,NK,_).
make_headkey(H,NK):-copy_term_nat(H,NK),numbervars(NK, 0, _,[ singletons(true)]).





%= 	 	 

%% pp_listing( ?Pred) is semidet.
%
% Pretty Print Listing.
%
pp_listing(Pred):- functor_safe(Pred,F,A),functor_safe(FA,F,A),findall(NV,predicate_property(FA,NV),LIST),dmsg((pp(Pred):-LIST)),nl,listing(FA).






%= 	 	 

%% use_xlisting is semidet.
%
% Use Xlisting.
%
use_xlisting:- use_xlisting(true).

%= 	 	 

%% use_xlisting( ?TF) is semidet.
%
% Use Xlisting.
%
use_xlisting(TF):-var(TF),!,(current_prolog_flag(xlisting,TF)->true;TF=false).
use_xlisting(TF):-set_prolog_flag(xlisting,TF).


%= 	 	 

%% use_listing_vars is semidet.
%
% Use Listing Variables.
%
use_listing_vars:- use_listing_vars(true),scan_for_varnames.

%= 	 	 

%% use_listing_vars( ?TF) is semidet.
%
% Use Listing Variables.
%
use_listing_vars(TF):-var(TF),!,(current_prolog_flag(util_varnames,TF)->true;TF=false).
use_listing_vars(TF):-set_prolog_flag(util_varnames,TF).


:- thread_local t_l:in_prolog_locate_clauses/1.

:- multifile(baseKB:hook_mpred_listing/1).
:- dynamic(baseKB:hook_mpred_listing/1).



:- export(get_print_mode/1).
get_print_mode(PM):- nonvar(PM),!,get_print_mode(PMR),!,PM==PMR.
get_print_mode(PM):- t_l:print_mode(PM),!.
get_print_mode(html):- on_x_log_fail(this_http_current_request(_)),!.
get_print_mode(bfly):- getenv('COLORTERM',butterfly),!.
get_print_mode(text).


:- use_module(library(http/http_wrapper)). % ([is_cgi_stream/1,cgi_property/2]).
this_http_current_request(Request) :-
    current_output(CGI),
    http_stream:((is_cgi_stream(CGI), 
    cgi_property(CGI, request(Request)))).


:- fixup_exports.

