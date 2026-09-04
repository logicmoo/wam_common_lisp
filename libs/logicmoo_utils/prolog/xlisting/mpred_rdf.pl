/* <module> mpred_mpred_t
% Provides a prolog dabase in these predicates...
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(mpred_rdf, [mpred_rdf/3,atom_to_qname/2,rdf_object/1,rdf_assert_hook/1,expire_rdf_caches/0 ]).



:- kb_shared(baseKB:expire_one_rdf_cache/0).


:- kb_shared(rdf_db:rdf_open_hook/3).

expire_rdf_caches :- forall(clause(expire_one_rdf_cache,Body),must(Body)).

:- kb_shared(mpred_online:semweb_startup/0).

mpred_online:semweb_startup:- retractall((tlbugger:show_must_go_on(_))).
mpred_online:semweb_startup:- expire_rdf_caches.

/* <module> MUD STORE

Installed to the ClioPatria SeRQL and SPARQL server

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does MUD-DB entailment.

@tbd	Check the completeness
*/

% % % OFF :- system:use_module(library(semweb/rdf_db),except([rdf/3])).
% % % OFF :- system:use_module(library(semweb/rdf_persistency)).
% % % % OFF :- system:use_module(rdfql(rdfql_runtime)).	% runtime tests
% % % OFF :- system:use_module(library(nb_set)).
% % % OFF :- system:use_module(library(semweb/rdfs)).
% % % OFF :- system:use_module(library(semweb/turtle)).
% % % OFF :- system:use_module(library(semweb/rdf_edit)).
% % % OFF :- system:use_module(library(url)).
% % % OFF :- system:use_module(library(http/http_open)).
% % % OFF :- system:use_module(library(http/http_ssl_plugin)).

ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :- !.

:- listing(rdf_db:rdf_open_hook/3).
rdf_db:rdf_open_hook(_, _, _):- expire_rdf_caches, fail.

%% n3_parse(+URL)
%
% Parse an OWL file and load it into the local RDF database.
% 
% Resolves owl:imports and supports both the common URL formats
% (file paths, file:// or http://) and the package:// URLs used
% in ROS to reference files with respect to the surrounding ROS
% package.
%
% @param URL Local or global file path or URL of the forms file://, http:// or package://
%
:- export(n3_parse/1).

graph_src(URL,Source):-rdf_current_prefix(Source,URL),!.
graph_src(URL,Source):-atom_concat(URL,'#',URIH),rdf_current_prefix(Source,URIH),!.
graph_src(URL,URL).

n3_parse(URL) :- graph_src(URL,Source),n3_parse(URL,[graph(Source),base_uri(URL),register_namespaces(true)]).
n3_parse(URL,Options) :-
  n3_parse_1(URL,[URL],Options).
  

n3_parse_1(URL,Imported,Options) :- catch(n3_parse_2(URL,Imported,Options),E,dmsg(E:n3_parse_1(URL,Imported,Options))).
:- kb_shared(lmcache:owl_file_loaded/1).
:- dynamic(lmcache:owl_file_loaded/1).
:- export(lmcache:owl_file_loaded/1).

n3_parse_2(URL,Imported,Options) :-

  ((sub_string(URL,0,4,_,'http'), !,
    http_open(URL,RDF_Stream,[ cert_verify_hook(ssl_verify)
				]),
    rdf_load(RDF_Stream,[blank_nodes(noshare)|Options]),
    close(RDF_Stream)),
    assert(lmcache:owl_file_loaded(URL))
    ;
   (sub_string(URL,0,7,_,'package'), !,

    % retrieve part after package://
    sub_atom(URL, 10, _, 0, Path),
    atomic_list_concat(PathList, '/', Path),

    % determine package name and resolve path
    selectchk(Pkg, PathList, LocalPath),
    rospack_package_path(Pkg, PkgPath),

    % build global path and load OWL file
    atomic_list_concat([PkgPath|LocalPath], '/',  GlobalPath),

    rdf_load(GlobalPath,[blank_nodes(noshare)|Options]),
    assert(lmcache:owl_file_loaded(URL))
    ) ; (
    rdf_load(URL,[blank_nodes(noshare)|Options])),
    assert(lmcache:owl_file_loaded(URL))
  ),
  (   rdf_db:rdf(_,'http://www.w3.org/2002/07/owl#imports',Import_URL),
      not( lmcache:owl_file_loaded(Import_URL)),!,
      n3_parse_1(Import_URL,[Import_URL|Imported],Options)
    ; true).

mpred_online:semweb_startup:- n3_parse('http://omeo.googlecode.com/svn/trunk/build/ro-subset.owl').
mpred_online:semweb_startup:- n3_parse('http://knowrob.org/baseKB/roboearth.owl').
mpred_online:semweb_startup:- n3_parse('http://ias.cs.tum.edu/baseKB/knowrob.owl').
mpred_online:semweb_startup:- n3_parse('http://raw.github.com/knowrob/knowrob/master/knowrob_omics/rdf/locations.rdf').
mpred_online:semweb_startup:- n3_parse('http://raw.github.com/knowrob/knowrob/master/knowrob_omics/rdf/roboearth.rdf').

% :- rdf_attach_library((.)).
% :-  with_no_mpred_expansions(use_module(cliopatria(cliopatria))).



/*[rdf_register_prefix/2,rdf_register_prefix/3,rdf_current_ns/2,rdf/4,rdf_assert/4,
                                      rdf_current_prefix/2,rdf_global_mpred_object/2,rdf_resource/1,rdf_current_predicate/1,
                                      rdf_has/3,rdf_graph_property/2,rdf_equal/2,rdf_literal_value/2,rdf_reachable/3,rdf_set_graph/2,rdf_subject/1]).
                                      */
:- rdf_register_prefix(agents_owl, 'http://onto.ui.sav.sk/agents.owl#',[force(true)]).
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#',[force(true)]).
:- rdf_register_prefix(skosxl,  'http://www.w3.org/2008/05/skos-xl#',[force(true)]).
:- rdf_register_prefix(knowrob_objects, 'http://ias.cs.tum.edu/baseKB/knowrob_objects.owl#',[force(true)]).
:- rdf_register_prefix(knowrob, 'http://ias.cs.tum.edu/baseKB/knowrob.owl#',[force(true)]).
:- rdf_register_prefix(mud,'http://logicmoo.org/onto/mud.owl#',[force(true)]).
:- rdf_register_ns(mud,'http://logicmoo.org/downloads/mud.ttl#',[force(true)]).


:- public(rdf/3).
rdf(S,P,O):- show_call(why,mpred_rdf(S,P,O)).

:- op(1150, fx, (rdf_meta)).   :- rdf_meta
	rdf(o,o,o),
        rdf_assert_p2q(o,r,o),
        rdf_x(o,r,o,o),
        rdf_assert_x(o,r,o,o),
	mpred_rdf:individual_of(r,r).



rdf_graph_ns(DB,DB):-var(DB),!.
rdf_graph_ns(DB:_,O):-!,rdf_graph_ns(DB,O).
rdf_graph_ns(DB,DB).

is_url(S):-not(atom(S)),!,fail.
is_url(S):-atom_chars(S,Chars),(memberchk(':',Chars);memberchk('/',Chars);memberchk('#',Chars)),!.
is_url(S):-atom_chars(S,['h'|Chars]),memberchk('/',Chars),!.

:- dynamic(lmcache:rdf_qname_url_created/3).

% allow rdf_current_resource(knowrob:'Food')
:- export(rdf_current_resource/1).
rdf_current_resource(O):- compound(O),!,O=NS:A, rdf_global_mpred_object(O,URL),(O\=@=URL->rdf_current_resource(URL);rdf_current_qname(NS,A)).
rdf_current_resource(O):- (rdf_resource(O);rdf_current_predicate(O);lmcache:rdf_qname_url_created(_,_,O)). % aalready included rdf_subject(O).

:- dynamic(lmcache:rdf_current_qname_cached/2).

:- export(rdf_current_qname/2).
rdf_current_qname(NS,A):-p2q(_,NS,A).
rdf_current_qname(NS,A):-lmcache:rdf_qname_url_created(NS,A,_).
rdf_current_qname(NS,A):-cache_all_qnames , lmcache:rdf_current_qname_cached(NS,A).


:- export(add_p2q_alias/3).
add_p2q_alias(P,NS,N):-asserta(lmcache:p2q_alias(P,NS,N)),rdf_assert_p2q(P,NS,N).

:- export(rdf_assert_p2q/3).
% rdf_assert_p2q(P,NS,N) :- must(rdf_assert_x(NS:N, mud:txtAtomName, literal(type(xsd:string, P)))).
rdf_assert_p2q(PA,NS,N) :- atomic(PA),!,text_to_string(PA,P),must(rdf_assert_x(NS:N, mud:txtAtomName, literal(P))).
rdf_assert_p2q(PA,NS,N) :- term_string(PA,P),must(rdf_assert_x(NS:N, mud:txtCompoundName, literal(P))).


:- dynamic(lmcache:all_qnames_cached/0).
cache_all_qnames:- lmcache:all_qnames_cached -> check_each_ns ; 
   ( asserta(lmcache:all_qnames_cached), forall(rdf_current_prefix(NS,_),forall(rdf_resource(X),forall(rdf_global_id(NS:A,X),asserta_if_new(lmcache:rdf_current_qname_cached(NS,A)))))).

check_each_ns :- !.
check_each_ns :- rdf_current_prefix(NS,_),not(lmcache:rdf_current_qname_cached(NS,_)),retractall(lmcache:all_qnames_cached),cache_all_qnames.

baseKB:expire_one_rdf_cache :- dmsg(color(red,expire_one_rdf_cache)).
baseKB:expire_one_rdf_cache :- retractall(lmcache:all_qnames_cached).

:- baseKB:expire_one_rdf_cache.

:- export(atom_to_qname/2).

atom_to_qname(URL,Q:NAME):-is_url(URL),!,must(url_to_qname(URL,Q,NAME)),!.
atom_to_qname(P,NS:A):-p2q(P,NS,A),!.
atom_to_qname(MUD:O,OO):-MUD==mud,nonvar(O),!,atom_to_qname(O,OO).
atom_to_qname(A,_):-not(atom(A)),!,fail.
atom_to_qname(URL,NS:Name):-concat_atom_safe([NS,Name],':',URL),!,must(rdf_current_prefix(NS,_)).
atom_to_qname(TProlog,URI):-must(atom_to_qname_search(TProlog,URI)),!.

atom_to_qname_search(P,QNAME):-www_form_encode(P,PO),!,atom_to_qname_search_0(PO,QN),!,must(QNAME=QN).
atom_to_qname_search_0(P,QNAME):-atom_to_qname_search_1(P,QNAME), (QNAME=(NS:NAME) -> add_p2q_alias(P,NS,NAME) ; dmsg(todo(atom_to_qname_search(P,QNAME)))).
atom_to_qname_search_1(TProlog,QNAME):- atom_prefix_other(TProlog,Prefix,COL), (prefix_other_qname(Prefix,COL,QNAME) *-> true ; rdf_create_qname(mud,TProlog,QNAME)).
atom_to_qname_search_1(TProlog,QNAME):-(prefix_other_qname('',TProlog,QNAME) *-> true ; rdf_create_qname(mud,TProlog,QNAME)).

check_qname(NS,NAME,OUT):-atom(NS),atom(NAME),check_qname0(NS,NAME,OUTO),!,OUTO=OUT.
check_qname0(_, fmt,MF):-!,MF= mud:fmt.
check_qname0(NS, Name,NS:Name):-rdf_current_qname(NS,Name),!.
check_qname0(NS,OAtom,NS:Name):-toCamelcase(OAtom,PCAtom),toPropercase(PCAtom,Name),!,rdf_current_qname(NS,Name),!.
check_qname0(NS,OAtom,NS:Name):-toPropercase(OAtom,Name),rdf_current_qname(NS,Name),!.
%check_qname(NS,OAtom,NS:Name):-toLowercase(OAtom,Name),rdf_current_qname(NS,Name),!.
%check_qname(NS,OAtom,NS:Name):-toLowercase(OAtom,LAtom),toPropercase(LAtom,Name),rdf_current_qname(NS,Name),!.
%check_qname(NS,OAtom,NS:Name):-toCamelcase(OAtom,Name),rdf_current_qname(NS,Name),!.


standard_search_pattern(ft,[xsd,prolog],mud).
standard_search_pattern(tt,[t],mud).
standard_search_pattern(vt,[t],mud).
standard_search_pattern(t,[knowrob],mud).
standard_search_pattern(i,[knowrob],mud).

standard_search_pattern(T,[rdfs,rdf,owl,knowrob,prolog,e,_]):- (var(T) ; T == ''),!.
standard_search_pattern(T,[Knowrob,prolog,rdfs,rdf,owl,e,_]):- standard_search_pattern(T,Knowrob,_Mud),!.

rdf_global_mpred_object(NS:N,URL):-lmcache:rdf_qname_url_created(NS,N,URL).
rdf_global_mpred_object(L,O):-rdf_global_object(L,O).

rdf_create_qname(NS,Name,URL):-rdf_global_mpred_object(NS:Name,URL),rdf_current_resource(URL),!.
rdf_create_qname(NS,Name,URL):-must(rdf_current_ns(NS,PREFIX)),atom_concat(PREFIX,Name,URL),nop(must(is_url(URL))),asserta(lmcache:rdf_qname_url_created(NS,Name,URL)).
   %text_to_string(Name,Label),show_call(why,rdf_assert(URL,rdfs:label,literal(type(xsd:string, Label)))).



% search all NS
prefix_other_qname(NS,Name,QNAME):-var(NS),!,rdf_current_prefix(NS,_),check_qname(NS,Name,QNAME),!.
% create In NS
prefix_other_qname(create(NS),Name,QNAME):-!,must_det_l([rdf_create_qname(NS,Name,_URI),check_qname(NS,Name,QNAME)]).
prefix_other_qname(List,COL,URI):-is_list(List),!,member(NS,List),prefix_other_qname(NS,COL,URI).
prefix_other_qname(NS,Name,QNAME):-rdf_current_prefix(NS,_),!,check_qname(NS,Name,QNAME).
prefix_other_qname(Prefix,COL,URI):-standard_search_pattern(Prefix,List),!,((member(NS,List),prefix_other_qname(NS,COL,URI))),!.

url_to_qname(URL,NS,Name):-rdf_global_mpred_object(NS:Name,URL).
url_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#first',rdf,first).
url_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',rdf,type).
url_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#',rdf,'<>').
url_to_qname(URL,Px,Name):-concat_atom([NS,Name],'#',URL),(atom_concat(NS,'#',NSH);NS=NSH),rdf_current_prefix(Px,NSH),!.



self_eval_object(X):-var(X);integer(X);string(X);number(X).

any_to_prolog(_,Var,V):-var(Var),!,copy_term(Var,V).
any_to_prolog(_,Sx,node(S)):-atom(Sx),atom_concat('__bnode',S,Sx),!.
any_to_prolog(_,X,X):-self_eval_object(X),!.
any_to_prolog(DB,Sx,Prolog):-p2q(Prolog,DB,Sx).
any_to_prolog(DB,URL,Prolog):-lmcache:rdf_qname_url_created(NS,A,URL),!,any_to_prolog(DB,NS:A,Prolog).
any_to_prolog(_,literal(Sx),S):-!,must(rdf_literal_value_safe(literal(Sx),S)).
any_to_prolog(_,NS:Name,Prolog):-qname_to_prolog(NS,Name,Prolog),!.
any_to_prolog(NS,Name,Prolog):-qname_to_prolog(NS,Name,Prolog),!.
any_to_prolog(_,Mud:A,O):-Mud==mud,!,must(atom_to_qname(A,O)).
any_to_prolog(Mud,A,O):-Mud==mud,!,must(atom_to_qname(A,O)).
any_to_prolog(_,Sx,S):-is_url(Sx),url_to_qname(Sx,NS,Name),!,qname_to_prolog(NS,Name,S0),!,must(S=S0).
any_to_prolog(_,Sx,S):-atom_to_qname(Sx,S0),!,must(S=S0).
any_to_prolog(DB,Sx,S):-rdfs_list_to_prolog_list(Sx,LL),!,maplist(any_to_prolog(DB),LL,S).
any_to_prolog(_,literal(type('http://www.w3.org/2001/XMLSchema#string', Name)),String):-string_to_atom(String,Name),!.
any_to_prolog(_,literal(type(_, Name)),Term):-catch(term_to_atom(Term,Name),_,fail),!.
any_to_prolog(_,literal(type(_, String)),Term):-catch(term_string(Term,String),_,fail),!.
any_to_prolog(_,Sx,S):-compound(Sx),!,must(rdf_literal_value_safe(Sx,S)).
any_to_prolog(DB,Sx,NS:SI):-atom(Sx),rdf_graph_ns(DB,NS),!,must(SI=Sx).
any_to_prolog(_,Ss,Sx):-copy_term(Ss,Sx).


:- dynamic(lmcache:p2q_alias/3).

p2q(~,mud,not).
p2q(~,mud,not).
p2q(-,mud,not).
p2q(naf,mud,not).
p2q((\+),mud,not).

% for atom_to_qname
p2q(P,N,A):-lmcache:p2q_alias(P,N,A).
p2q(isa,rdf,type).
p2q(tFood,knowrob,'Food').
p2q(arity, mud,arity).
p2q(arity, mud,arity).
p2q(tCol, rdfs,'Class').
p2q(tCol,owl,'Class').
p2q(tItem,knowrob,'HumanScaleObject').
p2q(tSpatialThing,knowrob,'SpatialThing').
p2q(genls,rdfs,subClassOf).
p2q(tRegion,knowrob,'FixedStructure').
p2q(tAgent,knowrob,'Agent-Generic').
p2q(ftInt,xsd,integer).
p2q(string,xsd,string).
p2q(ftString,xsd,string).
p2q(a,rdf,type).
p2q(ftInt,xsd,integer).
p2q([],rdf,nil).

% qname_to_prolog(NS,Name,Atom):-NS==mud,!,Name=Atom.
qname_to_prolog(NS,Name,Atom):-p2q(Atom,NS,Name).
qname_to_prolog(NS,Name,Atom):-qname_to_prolog_1(NS,Name,Atom),not(p2q(Atom,NS,Name)),add_p2q_alias(Atom,NS,Name).
qname_to_prolog_1(NS,Name,Atom):-rdf_current_qname(NS,Name),toPropercase(Name,AtomU),!,atom_concat(NS,AtomU,Atom),!.
qname_to_prolog_1(NS,Name,Atom):-toPropercase(Name,AtomU),!,atom_concat(NS,AtomU,Atom),!.

%any_to_prolog(P,O):-atom_to_qname(P,C),P\=C,!,(any_to_prolog(O,C);O=C).


:- dynamic(lmcache:rdf_alias/3).
cache_rdf_alias(_,URL,URL):- is_url(URL),!.
cache_rdf_alias(DB,From,To):- sanity(ground(lmcache:rdf_alias(DB,From,To))),lmcache:rdf_alias(DB,From,To),!.
cache_rdf_alias(DB,NS:From,To):- DB==NS,!,asserta(lmcache:rdf_alias(DB,NS:From,To)),!.
cache_rdf_alias(DB,R:From,To):- dmsg(trace_or_throw(cache_rdf_alias(DB:R:From,To))),asserta(lmcache:rdf_alias(DB,R:From,To)),!.
cache_rdf_alias(DB,From,To):- dmsg(lmcache:rdf_alias(DB,From,To)),asserta(lmcache:rdf_alias(DB,From,To)),!.
list_rdf_alias:-listing(lmcache:rdf_alias).


any_to_rdf(From,To):-sanity(ground(From)),any_to_rdf(mud,From,To),!,sanity(ground(To)),!.

any_to_rdf(_,Var,V):-var(Var),!,must(copy_term(Var,V)),!.
%any_to_rdf(DB,DB:From,To):-!,any_to_rdf(DB,From,To).
%any_to_rdf(_,DB:From,To):-!,any_to_rdf(DB,From,To).

:- export(any_to_rdf/3).
any_to_rdf(_,U,U):-is_url(U),!.
any_to_rdf(_,A,Sx):-var(A),format(atom(S),'~w',[(A)]),atom_concat('__bnode',S,Sx),!.
any_to_rdf(DB,User:B,URL):-not(rdf_current_prefix(User,_)),!,any_to_rdf(DB,prefix_concat(User,B),URL),!.
any_to_rdf(DB,A:B,URL):-is_ftVar(A),!,any_to_rdf(DB,prefix_concat(A,B),URL),!.
any_to_rdf(DB,baseKB:B,URL):-!,any_to_rdf(DB,prefix_concat(user,B),URL),!.
any_to_rdf(DB,A / B,URL):-any_to_rdf(DB,f_a(A,B),URL),!.
any_to_rdf(_,'$VAR'('_'),Sx):-format(atom(S),'~w',[_]),atom_concat('__bnode',S,Sx),!.
any_to_rdf(_,'$VAR'(A),Sx):-format(atom(S),'~w',['$VAR'(A)]),atom_concat('__bnode',S,Sx),!.
any_to_rdf(DB,S,URL):- ground(S),lmcache:rdf_alias(DB,S,URL),!.
any_to_rdf(_,NS:N,URL):-rdf_current_qname(NS,N),rdf_global_mpred_object(NS:N,URL),!.
any_to_rdf(DB,From,URL):-p2q(From,DB,N),!,rdf_global_mpred_object(DB:N,URL),!.
any_to_rdf(_,mud:From,URL):-p2q(From,NS,N),!,rdf_global_mpred_object(NS:N,URL),!.
any_to_rdf(mud,From,URL):-p2q(From,NS,N),!,rdf_global_mpred_object(NS:N,URL),!.
any_to_rdf(_,From,URL):-p2q(From,NS,N),!,rdf_global_mpred_object(NS:N,URL),!.
any_to_rdf(_,S,URL):- lmcache:rdf_alias(_,S,URL),!.
any_to_rdf(_, [], Nil) :- rdf_equal(rdf:nil, Nil),!.
any_to_rdf(_,node(S),Sx):-atom_concat('__bnode',S,Sx),!.
any_to_rdf(_,Text,Sx):- is_list(Text),catch(text_to_string(Text,String),_,fail),!,any_to_rdf(String,Sx),!.
any_to_rdf(DB,U,O):- nonvar(U),self_eval_object(U),rdf_to_lit(U,M),!,any_to_rdf(DB,M,O),!.
any_to_rdf(DB,S,URL):- any_to_rdf_0(DB,S,URL),cache_rdf_alias(DB,S,URL),!.
any_to_rdf(DB,S,URL):- any_to_rdf_1(DB,S,URL),!.
any_to_rdf(DB,S,URL):- any_to_rdf_2(DB,S,URL),cache_rdf_alias(DB,S,URL),!.

any_to_rdf_0(DB,Var,V):- \+ (ground(Var)),trace_or_throw(nonground(any_to_rdf(DB,Var,V))).
any_to_rdf_0(DB,Var,V):- sanity(nonvar(DB)), \+ (ground(DB)),trace_or_throw(nonground(any_to_rdf(DB,Var,V))).
any_to_rdf_0(DB,[H|T],List):- nonvar(T), is_list([H|T]),maplist(any_to_rdf(DB),[H|T],HT),!,rdfs_assert_list(HT, List, DB).
any_to_rdf_0(DB,[H|T],List):- 
        any_to_rdf(DB,H,HH),any_to_rdf(DB,T,TT),!,
        rdf_bnode(List),
	rdf_assert(List, rdf:rest, TT, DB),
	rdf_assert(List, rdf:first, HH, DB),
	rdf_assert(List, rdf:type, rdf:'List', DB).

any_to_rdf_0(_,NS:R,URL):-sanity(ground(NS:R)),must(rdf_global_mpred_object(NS:R,URL)),!.
any_to_rdf_0(DB,S,Sxx):-atom(S),atom_to_qname(S,Sx),Sx\=S,!,any_to_rdf(DB,Sx,Sxx).
any_to_rdf_0(DB,S,URL):-atom(S),rdf_graph_ns(DB,NS),rdf_global_mpred_object(NS:S,URL),!.

any_to_rdf_1(_,literal(W),O):-nonvar(W),!,must(rdf_global_object(literal(W),O)),!,must(compound(O)).
any_to_rdf_1(_,S,Sx):-catch((rdf_global_mpred_object(S,Sx),not(compound(Sx))),_,fail),!.
% any_to_rdf_1(DB,literal(type(xsd:string,String)),G):- text_to_string(String,SString),rdf_global_object(literal(type('http://www.w3.org/2001/XMLSchema#string',SString)),O).
% any_to_rdf_1(mud,literal(S),literal(S)):-!.

any_to_rdf_2(DB,C,List):-compound(C),C=..[H|T],must(nonvar(DB)),!,must(( maplist(any_to_rdf(DB),
   [mud:aCompFn,H|T],HT),!,rdfs_assert_list(HT, List, DB))).

any_to_rdf_2(DB,C,List):-compound(C),C=..[H|T],must(nonvar(DB)),!,must(( maplist(any_to_rdf(DB),
   [mud:aCompFn,H|T],HT),!,rdfs_assert_list(HT, List, DB))).



rdf_to_lit(U,literal(type(xsd:integer, S))):-integer(U),!,atom_string(U,S).
rdf_to_lit(U,literal(type(xsd:double, S))):-number(U),!,atom_string(U,S).
rdf_to_lit(U,literal(type(xsd:string, U))):-string(U),!.


rdf_literal_value_safe(Sx,S):-rdf_literal_value(Sx,S),!.

onLoadTTL([],_File):-!.
onLoadTTL(List,DB):-is_list(List),!,forall(member(X,List),onLoadTTL(X,DB)).
onLoadTTL(X,DB):-format('~q.~n',[X-DB]),fail.
onLoadTTL(rdf(S,P,O),DB):- must(rdf_assert_x(S,P,O,DB)).

rdf_to_graph(DB,Go):-var(DB),!,rdf_to_graph(user,Go),!.
rdf_to_graph(Gx,DB):-rdf_current_ns(DB,Gx),!.
rdf_to_graph(Gx,Gx):-rdf_current_ns(Gx,_),!.
rdf_to_graph(DB,Gx):-rdf_create_graph(DB),rdf_graph_property(DB,source(S)),(rdf_register_prefix(DB, S,[force(true)]),Gx=DB),!.
rdf_to_graph(DB,DB):- atom(DB),atomic_list_concat(['source://',DB,'#'],S),
   rdf_register_prefix(DB,S),
   ignore(rdf_set_graph(DB,source(S))),!.


rdf_from_graph(DB,DB).

/*
:- mode fully_det rdf_to_prolog_io(+,+,+,-).
rdf_to_prolog_io(DB,o,S,Sx):-any_to_prolog(DB,S,Sx).
rdf_to_prolog_io(_ ,i,S,S):-ground(S).
rdf_to_prolog_io(DB,i,S,Sx):-any_to_prolog(DB,S,Sx).
*/

rdf_to_prolog_io(DB,o,S,Sx):-!,must(any_to_prolog(DB,S,Sx)),!.
rdf_to_prolog_io(_,i,S,Sx):-ground(S),!,must(S=Sx).
rdf_to_prolog_io(DB,i,S,Sx):-must(quietly(any_to_prolog(DB,S,Sx))),!.

to_rdf_io(_,S,Sx,o):-var(S),!,must(copy_term(S,Sx)).
to_rdf_io(DB,S,Sx,i):-quietly(must(any_to_rdf(DB,S,Sx))).

bootstrap_ttl:- rdf_process_turtle('bootstrap.ttl',onLoadTTL,[prefixes(X)]),forall(member(NS-Prefix,X),rdf_register_prefix(NS,Prefix)).

enforce_never_p(_,_,[]):-!.
enforce_never_p(_,P,List):-member(L,List),rdf_equal(P,L),!,fail.
enforce_never_p(_,_,_).


   
current_g(knowrob).

rdf_object(NS:C):-!,ground(rdf_object(NS:C)).
rdf_object(L):-is_list(L),!.
rdf_object(C):-atomic(C).
rdf_object(O):-ground(O).

:- dynamic(baseKB:using_rdf_mpred_hook).

% :- kb_shared(baseKB:decl_database_hook).
%OLD baseKB:decl_database_hook(change(assert,_A_or_Z),DBI):- copy_term(DBI,DB), baseKB:using_rdf_mpred_hook,unnumbervars_with_names(DB),rdf_assert_hook(DB),!.

:- thread_local(t_l:rdf_asserting/2).

rdf_assert_ignored(DB):-t_l:rdf_asserting(_,DB),!.
rdf_assert_ignored(':-'(_)).
rdf_assert_ignored(G):-not(compound(atom(G))),!.
rdf_assert_ignored(_):-flag(rdf_assert_hook_max,W,W),W>4000,!.
rdf_assert_ignored('$spft'(_,_,_,_)).
rdf_assert_ignored(support2(_,_,_)).
rdf_assert_ignored(support3(_,_,_)).
rdf_assert_ignored(isa(tCol,tCol)).
%rdf_assert_ignored(isa(_,_)).
%rdf_assert_ignored(genls(_,_)).
rdf_assert_ignored((_:-INFOC)):-is_meta_info(INFOC),!.
rdf_assert_ignored(svo(_,prologDynamic,_)).
rdf_assert_ignored(mpred_isa(_,_)).
rdf_assert_ignored(_:mpred_isa(_,_)).
rdf_assert_ignored(mpred_isa(_,arity(1))).
rdf_assert_ignored(mpred_isa(_,meta_argtypes(_))).
%rdf_assert_ignored(DB):-functor(DB,F,_),member(F,[ruleBackward,mudTermAnglify,'<=>']).
rdf_assert_ignored(DB):-functor(DB,_,1).
rdf_assert_ignored(G):-pfcTypeFull(G,Type),!,(Type==trigger;Type==support).
% rdf_assert_ignored(DB):-   \+ (ground(DB)). 


cyc_to_rdf(mpred_isa(P,PST),svo(F,StubType,S)):- PST=..[StubType,S],rdf_object(S),rdf_to_pred(P,F).
cyc_to_rdf(argIsa(P,1,D),domain(P,D)).
cyc_to_rdf(isa(apathFn(A,Dir),T),isa([apathFn,A,Dir],T)).
cyc_to_rdf(pathName(A,Dir,String),mudNamed([apathFn,A,Dir],String)).
% cyc_to_rdf(relationMostInstance(PAB, Type, V),type_default(A,[P,isThis,V])):-PAB=[P,A,_].
cyc_to_rdf(argIsa(P,2,D),range(P,D)):-arity(P,2).

:- flag(rdf_assert_hook_max,_,0).
:- export(rdf_assert_hook/1).
rdf_assert_hook(CYC):- \+ (ground(CYC)),!,copy_term(CYC,O),unnumbervars_with_names(O),!,rdf_assert_hook(O),!.
rdf_assert_hook(PSO):-rdf_assert_ignored(PSO),!.
rdf_assert_hook((A,B)):-!,rdf_assert_hook(A),rdf_assert_hook(B).
rdf_assert_hook(CYC):-into_mpred_form(CYC,DIF),CYC\=@=DIF,!,must(rdf_assert_hook(DIF)),!.
rdf_assert_hook(CYC):-once(cyc_to_rdf(CYC,RDF)),CYC\=@=RDF,!,must(call(rdf_assert_hook(RDF))),!.
rdf_assert_hook(isa(I,C)):-must((rdf_assert_hook0(isa(I,C)))),!.
rdf_assert_hook(PSO):-flag(rdf_assert_hook_max,W,W+1),must(show_call(why,rdf_assert_hook0(PSO))),!.
rdf_assert_hook(PSO):-dmsg(once(skipped(rdf_assert_hook(PSO)))).


%rdf_assert_hook0(mudLabelTypeProps(A,Food,Props)):-nonvar(A),atom_string(A,S),!,must((rdf_assert_hook(typeProps(Food,[label(S)|Props])))),!.
rdf_assert_hook0(typeProps(Food,Props)):-is_list(Props),!,forall(member(P,Props),must(rdf_assert_hook(typeProps(Food,P)))),!.
rdf_assert_hook0(typeProps(Food,Prop)):-Prop=..[P|ARGS],must(rdf_assert_hook(type_default(Food,[P,isThis|ARGS]))),!.
rdf_assert_hook0(genls(C,P)):-!,rdf_object(C),rdf_object(P),rdf_assert_x(C,rdfs:subClassOf,P),!.
rdf_assert_hook0(mudDescription(C,P)):-!,rdf_object(C),rdf_object(P),rdf_assert_x(C,rdfs:comment,P),!.
rdf_assert_hook0((H:-TRUE)):-is_src_true(TRUE),!,rdf_assert_hook0(H),!.
rdf_assert_hook0((H:-B)):-rdf_assert_hook0(neck(H,B)),!.
rdf_assert_hook0('<-'(H  , B)):-rdf_assert_hook0(backchain(H,B)),!.
rdf_assert_hook0('=..'(H  , B)):-rdf_assert_hook0(univ_p_l(H,B)),!.
rdf_assert_hook0('=>'(L , R)):-rdf_assert_hook0(forwardchain(L,R)),!.
rdf_assert_hook0(isa(Prop,tPred)):- rdf_to_pred(Prop,P),!,rdf_object(P),rdf_assert_x(P,rdf:type,owl:'Property'),!.
rdf_assert_hook0(isa(Prop,prologSingleValued)):- functor(Prop,P,_),!,rdf_object(P),rdf_assert_x(P,rdf:type,owl:'FunctionalProperty'),!.
rdf_assert_hook0(arity(W1,N)):-rdf_to_pred(W1,W),N>1,rdf_assert_x(W,rdf:type,owl:'Property'),!.
rdf_assert_hook0(isa(W,tCol)):-!,rdf_object(W),rdf_assert_x(W,rdf:type,owl:'Class'),!.
rdf_assert_hook0(isa(C,P)):-!,rdf_object(C),rdf_object(P),P\=tCol,rdf_assert_x(C,rdf:type,P),!.
rdf_assert_hook0(svo(S,P,O)):-!,must(rdf_assert_x(S,P,O)),!.
rdf_assert_hook0(PSO):-PSO=..[P,S,O],!,rdf_assert_x(S,P,O),!.
rdf_assert_hook0(PSO):-PSO=..[P,S|O],!,rdf_assert_x(S,P,O),!.

rdf_to_pred(W,W):-var(W),!.
rdf_to_pred(W,F):-get_functor(W,F),!.

to_rdf_ignore(DB,S,Sxx):-atom(S),atom_to_qname(S,Sx),!,any_to_rdf(DB,Sx,Sxx).
to_rdf_ignore(DB,A,B):-any_to_rdf(DB,A,BB),ignore(B=BB).



mpred_t_rdf(Sc,rdf:type,CC):- /*o_to_p(CC,Oc),*/clause(t(Oc,Sc),true),any_to_rdf(Oc,CC),!.
mpred_t_rdf(Sc,Pc,Oc):-t(Pc,Sc,Oc),!.

:- export(rdf_assert_x/3).
rdf_assert_x(S,P,O):- rdf_assert_x(S,P,O,mud).
:- export(rdf_assert_x/4).
% rdf_assert_x(S,P,O,DB):-Q=rdf_x(S,P,O,DB), \+ (ground(Q)),!,Q.
rdf_assert_x(S,P,O,DB):-
  logOnFailure((
    must(((rdf_to_graph(DB,Gx),!,any_to_rdf(Gx,S,Sx),!,any_to_rdf(Gx,P,Px),!,any_to_rdf(Gx,O,Ox),!))),
      logOnFailure(((must(call(rdf_assert(Sx,Px,Ox,Gx)))))))),!.

:- export(add_spog/4).
add_spog(S,P,O,DB):- must(spog_to_prolog(S,P,O,DB,REQ)),!,
  (REQ==end_of_file-> true ; locally(t_l:rdf_asserting(DB,REQ),add(REQ))),!.

:- export(spog_to_prolog/5).
spog_to_prolog(Sx,Px,Ox,Gx,svo(S,P,O)):- quietly(NeverP=[rdf:rest,rdf:first]),
    must_det_l([rdf_from_graph(Gx,DB),rdf_to_prolog_io(DB,o,Px,P)]),!,
    enforce_never_p(DB,P,NeverP),
    must_det_l([rdf_to_prolog_io(DB,o,Sx,S),rdf_to_prolog_io(DB,o,Ox,O)]).
spog_to_prolog(_Sx,_Px,_Ox,_Gx,end_of_file).


:- export(rdf_x/3).
rdf_x(S,P,O):- rdf_x(S,P,O,_MUD).
:- export(rdf_x/4).
rdf_x(S,P,O,DB):-
  (nonvar(DB)->true;rdf_graph(Gx)),
  quietly(once((to_rdf_io(user,DB,Gx,_Gio),to_rdf_io(Gx,S,Sx,Sio),to_rdf_io(Gx,P,Px,Pio),to_rdf_io(Gx,O,Ox,Oio)))),
                quietly(((nonvar(P)->NeverP=[];NeverP=[rdf:rest,rdf:first,rdf:type]))),
                rdf_db:rdf(Sx,Px,Ox,Gx),
                rdf_to_prolog_io(DB,Pio,Px,P),
                enforce_never_p(DB,P,NeverP),
                once((rdf_from_graph(Gx,DB),rdf_to_prolog_io(DB,Sio,Sx,S),rdf_to_prolog_io(DB,Oio,Ox,O))).

mpred_rdf(S,P,O):-
  current_g(DB),
  maplist(any_to_prolog(DB),[S,P,O],[Sc,Pc,Oc]),
  mpred_t_rdf(Sc,Pc,Oc),
  maplist(to_rdf_ignore(DB),[Sc,Pc,Oc],[S,P,O]).

% http://localhost:3020/cliopatria/browse/list_triples?graph=file:///t:/devel/cliopatria/rdf/base/rdfs.rdfs
% http://localhost:3020/cliopatria/browse/list_triples_with_object?r=http://www.w3.org/1999/02/22-rdf-syntax-ns%23Property
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=rdfs
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=owl

mpred_rdf(skosxl:prefLabel,   rdf:type, owl:'ObjectProperty').
mpred_rdf(skosxl:altLabel,    rdf:type, owl:'ObjectProperty').
mpred_rdf(skosxl:hiddenLabel, rdf:type, owl:'ObjectProperty').

mpred_rdf('http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty','http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
mpred_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
mpred_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/2000/01/rdf-schema#label',literal(someValuesFrom)).
mpred_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/2000/01/rdf-schema#domain','http://www.w3.org/2002/07/owl#Restriction').
mpred_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Class').
mpred_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/2000/01/rdf-schema#label',literal('FunctionalProperty')).
mpred_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
mpred_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
mpred_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#comment',literal('The object of an RDF statement.')).
mpred_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#domain','http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement').
mpred_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#isDefinedBy','http://www.w3.org/1999/02/22-rdf-syntax-ns#').
mpred_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#label',literal(lang(en,object))).


mpred_rdf(S, P, O):-!, rdf_db:rdf(S, P, O).

mpred_rdf(S, P, O):-dmsg(mpred_rdf(S, P, O)),!,fail.

mpred_rdf(literal(L), _, _) :-		% should move to compiler
	nonvar(L), !, fail.
mpred_rdf(_, literal(L), _) :-		% idem
	nonvar(L), !, fail.
mpred_rdf(S, P, O) :-
	var(P), !,
	(   rdf_db:rdf(S,P,O)
	;   rdf_db:rdf(P, rdf:type, rdf:'Property'),
	    rdf_db:rdf(S, P, O),
	    \+ rdf_db:rdf(S,P,O)
	).
mpred_rdf(S, P, C) :-
	rdf_reachable(rdf:type, rdfs:subPropertyOf, P), !,
	individual_of(S, C).
mpred_rdf(S, P, O) :-					% transitive predicates
	rdf_reachable(rdfs:subClassOf, rdfs:subPropertyOf, P), !,
	(   (nonvar(S) ; nonvar(O))
	->  rdfs_subclass_of(S, O)		% generate from given start
	;   individual_of(S, rdfs:'Class'),	% generated unbounded (slow!)
	    rdfs_subclass_of(S, O)
	).
mpred_rdf(S, rdfs:subPropertyOf, O) :- !,
	(   nonvar(S)
	->  individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   nonvar(O)
	->  individual_of(O, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	).
mpred_rdf(S, serql:directSubClassOf, O) :- !, rdf_has(S, rdfs:subClassOf, O).
mpred_rdf(S, serql:directType, O) :- !, rdf_has(S, rdf:type, O).
mpred_rdf(S, serql:directSubPropertyOf, O) :- !, rdf_has(S, rdfs:subPropertyOf, O).
mpred_rdf(S, P, O) :- rdf_has(S, P, O).


%%	individual_of(?Resource, ?Class)
individual_of(Resource, Class):- mpred_individual_of(Resource, Class).


mpred_individual_of(Resource, Class) :-
	nonvar(Resource), !,
	(   Resource = literal(_)
	->  rdfs_subclass_of(Class, rdfs:'Literal')
	;   mpred_rdf_has_type(Resource, MyClass),
	    rdfs_subclass_of(MyClass, Class)
	;   rdf_equal(Class, rdfs:'Resource')
	).
mpred_individual_of(Resource, Class) :-
	nonvar(Class), !,
	(   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(Resource)
	;   rdfs_subclass_of(SubClass, Class),
	    mpred_rdf_has_type(Resource, SubClass)
	).
mpred_individual_of(Resource, Class) :-
	rdf_subject(Resource),
	individual_of(Resource, Class).


%%	mpred_rdf_has_type(+Resource, -Class) is nondet.
%%	mpred_rdf_has_type(-Resource, +Class) is nondet.
%
%	Perform RDFS entailment rules to enumerate the types of Resource
%	or generate all resources entailed  by   the  given  class.

mpred_rdf_has_type(Resource, Class) :-
	empty_nb_set(Set),
	(   atom(Resource)
	->  (   rdf_has(Resource, rdf:type, Class)
	    ;	rdf_db:rdf(Resource, P, _),
		rdf_has(P, rdfs:domain, Class)
	    ;	rdf_db:rdf(_, P, Resource),
		rdf_has(P, rdfs:range, Class)
	    ),
	    add_nb_set(Class, Set, New),
	    New == true
	;   atom(Class)
	->  (	rdf_has(Resource, rdf:type, Class)
	    ;	rdf_has(P, rdfs:domain, Class),
		rdf_has(Resource, P, _)
	    ;	rdf_has(P, rdfs:range, Class),
		rdf_has(_, P, Resource)
	    ),
	    add_nb_set(Resource, Set, New),
	    New == true
	;   throw(error(instantiation_error, _))
	).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- kb_shared
	cliopatria:entailment/2.

cliopatria:entailment(mpred_rdf, mpred_rdf).


:- export(sync_to_rdf/0).
:- export(sync_from_rdf/0).

sync_from_rdf:-dmsg(todo(code_sync_from_rdf)),!.
sync_from_rdf:-forall(rdf_db:rdf(S,P,O,DB),add_spog(S,P,O,DB)).

% sync_to_rdf:-dmsg(todo(code_sync_to_rdf)),!.
sync_to_rdf:-!.
sync_to_rdf:-
   forall(p2q(P,NS,N),must(rdf_assert_p2q(P,NS,N))),  
   forall(mpred_isa(P,O),rdf_assert_hook(mpred_isa(P,O))),
   forall(t(C,I),rdf_assert_hook(isa(I,C))),
   forall(is_known_trew(B),rdf_assert_hook(B)),
   (baseKB:using_rdf_mpred_hook -> true ; (asserta(baseKB:using_rdf_mpred_hook),forall(prologHybridFact(G),rdf_assert_hook(G)))),
   asserta_if_new(baseKB:call_OnEachLoad(sync_to_rdf)),
   asserta_if_new(baseKB:call_OnEachLoad(sync_from_rdf)),
   !.



mpred_online:semweb_startup:- must(sync_from_rdf).
mpred_online:semweb_startup:- must(sync_to_rdf).


:- kb_shared(baseKB:call_OnEachLoad/1).
mpred_online:semweb_startup:- asserta_if_new(baseKB:call_OnEachLoad(sync_to_rdf)).
mpred_online:semweb_startup:- asserta_if_new(baseKB:call_OnEachLoad(sync_from_rdf)).

