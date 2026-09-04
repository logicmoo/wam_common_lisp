% :-module(xlisting_web,[ensure_sigma/0,search4term/0]).
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- module(xlisting_web,
          [ action_menu_applied/3,
            action_menu_applied_here/3,
            %action_menu_item/2,
            add_form_script/0,
            %register_logicmoo_browser/0,
            add_to_env_here/1,
            call_for_terms/1,
            classify_alpha_tail/1,
            classify_name/2,
            classify_other_tail/1,
            current_form_var/1,
            current_line_position/1,
            current_line_position/2,
            url_decode_term/2,
            url_decode_term/3,
            do_guitracer/0,
            edit1term/0,
            output_telnet_console/1,
            edit1term/1,
            in_pre/0,
            with_pre/1,
            with_tag/2,
            find_cl_ref/2,
            find_ref/2,
            fmtimg/3,
            'functor spec'/4,
            functor_to_color/2,
            functor_to_color/4,

            atom_to_term_safe/3,
            
            get_http_current_request/1,
            get_http_session/1,
            get_nv_session/3,
            get_param_req/2,
            get_param_sess/2,
            get_param_sess/3,
            get_request_vars/1,
            handler_logicmoo_cyclone_call/1,
            head_functor_sort/3,
            must_run/1,
            human_language/1,
            i2tml_hbr/3,
            if_html/2,
            % write_html/1,
            show_map_legend/0,
            indent_nbsp/1,
            indent_nbsp/2,
            indent_nl/0,
            is_cgi_stream/0,
            is_context/2,
            is_goog_bot/0,
            list_w_clauses/4,
            list_w_magic/2,
            list_w_magic/3,
            list_w_magic/4,
            logic_lang_name/2,
            xlisting_html/1,
            make_quotable/2,
            make_session/1,
            maybe_paren/5,
            maybe_space/2,
            member_open/2,
            merge_key_vals/3,
            name_the_var/5,
            nl_same_pos/0,
            numberlist_at/2,
            object_sub_page/4,
            %param_default_value/2,
            param_matches/2,
            parameter_names/2,
            %partOfSpeech/2,
            portable_display/1,
            portable_listing/0,
            portable_listing/1,
            portable_print/1,
            portable_write/1,
            portable_writeq/1,
            pp_i2tml/1,
            pp_i2tml_now/1,
            pp_i2tml_save_seen/1,
            pp_i2tml_saved_done/1,
            pp_i2tml_v/1,
            pp_item_html/2,
            pp_item_html_if_in_range/2,
            pp_item_html_now/2,
            pp_now/0,
            print_request/1,
            web_prover_name/2,
            
            reply_object_sub_page/1,
            reset_assertion_display/0,
            return_to_pos/1,
            rok_portray_clause/1,
            save_in_session/1,
            save_in_session/2,
            save_in_session/3,
            save_request_in_session/1,
            intern_request_data/1,
            search4term/0,
            search_filter_name_comment/3,
            section_close/1,
            section_open/1,
            sensical_term/1,
            %send_tokens/1,
            session_checkbox/3,
            session_checked/1,
            set_line_pos/1,
            set_line_pos/2,
            show_clause_ref/1,
            show_clause_ref_now/1,
            show_edit_term/2,
               show_http_session/0,
            show_iframe/1,
            show_iframe/3,
            show_pcall_footer/0,
            show_search_filters/1,
            suppliment_cp_menu/0,
            term_to_pretty_string/3,
            this_listing/1,
            test_tmw/0,
            tovl/3,
            url_decode_term/2,
            url_decode_term/3,
            url_encode/2,
            url_encode_term/4,
            with_search_filters/1,
            with_search_filters0/1,
            write_VAR/4,
            write_args/5,
            write_as_url_encoded/2,
            write_atom/4,
            write_atom_link/1,
            write_atom_link/2,
            write_atom_link/3,
            write_begin_html/2,
            write_end_html/0,
            write_oper/5,
            write_out/5,
            write_oout_cmpd/7,
            write_tail/2,
            write_plain_atom/2,
            write_variable/1,
          
          xlisting_web_file/0
            /*
            http:location/3,
            http_dispatch:handler/4,
            http_log:log_stream/2,
            http_session:session_data/2,
            http_session:urandom_handle/1,
            system:'$init_goal'/3,
            user:file_search_path/2
            */
          ]).

:- set_module(class(library)).
/** <module> xlisting_web
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
/*
:- system:use_module(library(hook_database)).
:- system:use_module(library(logicmoo/no_repeats)).
:- system:use_module(library(logicmoo/each_call)).
:- system:use_module(library(logicmoo/locally_redo)).
:- system:use_module(library(logicmoo/virtualize_source)).% WAS OFF  :- system:use_module(library(no_repeats)).
*/
:- system:use_module(library(logicmoo/attvar_serializer)).

:- dynamic user:library_directory/1.
:- multifile user:library_directory/1.
hide_xpce_library_directory:- 
  user:library_directory(X),
  atom(X),
  atom_concat(_,'xpce/prolog/lib/',X),!,
  retract((user:library_directory(X))),
  assert((user:library_directory(X):- \+ current_prolog_flag(hide_xpce_library_directory,true))).
hide_xpce_library_directory.
%:- hide_xpce_library_directory.

%:- hide_xpce_library_directory.
:- set_prolog_flag(hide_xpce_library_directory,false).

%:- ensure_loaded(library(logicmoo_swilib)).
:- system:use_module(library(http/thread_httpd)).
:- system:use_module(thread_httpd:library(http/http_dispatch)).
:- system:use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_dispatch))
:- system:use_module(swi(library/http/html_head)).
:- system:use_module(library(http/http_path)).
:- system:use_module(library(http/http_log)).
:- system:use_module(library(http/http_client)).
:- system:use_module(library(http/http_server_files)).
:- system:use_module(library(http/http_parameters)).

:- system:use_module(library(uri)).
:- system:use_module(library(http/http_openid)).
:- system:use_module(library(http/http_host)).
:- use_module(library(http/html_write)).
:- system:use_module(library(http/http_error)).


:- system:use_module(library(predicate_streams)).
%:- system:use_module(library(logicmoo/with_no_x)).
:- system:use_module(library(logicmoo/each_call)).
%:- system:use_module(library(logicmoo/butterfly_console)).

:- thread_local(t_l:no_cycstrings/0).
:- asserta(t_l:no_cycstrings).
:- autoload(library(sgml),[xml_quote_cdata/3,xml_quote_attribute/3]).

/*
:- include(library('pfc2.0'/'mpred_header.pi')).
:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- 
 user:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).
*/
%:- endif.

:- thread_local(t_l:print_mode/1).

:- multifile((cp_menu:extra_cp_menu//0)).
:- dynamic((cp_menu:extra_cp_menu//0)).
:- export(cp_menu:extra_cp_menu//0).

cp_menu:extra_cp_menu -->  
   { \+ (( httpd_wrapper:http_current_request(Request),member(request_uri(URI),Request), atom_contains(URI,pldoc))) },!,
   pldoc_search:doc_links([],[]).
cp_menu:extra_cp_menu --> [].


:- if(exists_source(cliopatria('applications/help/load'))).
:- system:use_module(cliopatria('applications/help/load')).
% Load ClioPatria itself.  Better keep this line.
:- system:use_module(cliopatria(cliopatria)).
:- else.
cp_menu:cp_menu(X,X).
%cp_menu:cp_menu.
:- endif.



:- multifile(cp_menu:(cp_menu/2)).
:- dynamic(cp_menu:(cp_menu/2)).
:- multifile(cp_menu:(current_menu_item/2)).
:- dynamic(cp_menu:(current_menu_item/2)).

% handler_with_output_to 
suppliment_cp_menu:- 
 asserta((    
  cp_menu:cp_menu(A, B) :- 
   cp_menu:
   (!,
    findall(Key-Item,
            current_menu_item(Key, Item),
            Pairs0),
    sort(Pairs0, Pairs),
    group_pairs_by_key(Pairs, ByKey),
    xlisting_web:do_sort_menu_popups(ByKey, Menu),
    C=A,
    html_requires(css('menu.css'), C, D),
    html(ul(id(nav), \menu(Menu)), D, B1),
    html(\ xlisting_web:extra_cp_menu,B1,B)))).

:- suppliment_cp_menu.

:- multifile
	cp_menu:menu_item/2,
	cp_menu:menu_popup_order/2.
:- dynamic
	cp_menu:menu_item/2,
	cp_menu:menu_popup_order/2.


:- retractall(cp_menu:menu_item(_, 'XListing Web')).
%:- asserta(cp_menu:menu_item(500=swish/swish, 'Swish')).
:- asserta(cp_menu:menu_item('/swish/lm_xref/',	'XListing')).
%:- asserta(cp_menu:menu_item(700=places/swish/lm_xref, 'XListing Web')).

do_sort_menu_popups(List, Sorted) :-
	map_list_to_pairs(our_popup_order, List, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, Sorted).

our_popup_order(Key-Members, Order-(Key-Members)) :-
	(   cp_menu:menu_popup_order(Key, Order)
	->  true
	;   Order = 550			% between application and help
	).

:- dynamic(baseKB:param_default_value/2).
:- kb_global(baseKB:param_default_value/2).
%:- kb_global(baseKB:mtExact/1).

:- meta_predicate 
        edit1term(*),
        handler_logicmoo_cyclone_call(+),
        must_run(*),must_run_html(*),must_run(*),must_run0(*),must_run0(*),
        %output_html(//),        
        if_html(?, 0),
        return_to_pos(0),
        with_search_filters(0),
        with_search_filters0(0).
:- (multifile http:location/3, http_dispatch:handler/4, http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1, baseKB:shared_hide_data/1, system:'$init_goal'/3, user:file_search_path/2).
:- (module_transparent edit1term/1, must_run/1, if_html/2, return_to_pos/1, show_edit_term/2, with_search_filters/1).
:- (volatile http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1).
:- export((current_form_var0/1, find_http_session/1,  is_context0/1, escape_quoting/2, pp_i2tml_0/1, pp_i2tml_1/1, show_edit_term/2, show_select1/2, show_select2/3)).
:- multifile((lmcache:last_item_offered/1, http:location/3, http_dispatch:handler/4, http_session:session_data/2, http_session:urandom_handle/1,
   foobar/1, lmcache:last_http_request/1, lmcache:last_item_offered/1, system:'$init_goal'/3, user:file_search_path/2)).


:- thread_initialization(nb_setval(pldoc_options,[ prefer(manual) ])).

:- meta_predicate must_run(0).
:- meta_predicate must_run0(0).
:- meta_predicate must_run0(0).
:- meta_predicate with_search_filters(0).
:- meta_predicate return_to_pos(0).
:- meta_predicate edit1term(+).


:- multifile(http_session:session_data/2).
:- volatile(http_session:session_data/2).

:- multifile(system:'$loading_file'/3).
:- volatile(system:'$loading_file'/3).

:- multifile(http_session:urandom_handle/1).
:- volatile(http_session:urandom_handle/1).

:- multifile(http_log:log_stream/2).
:- volatile(http_log:log_stream/2).



:- if( \+ exists_source(library(logicmoo_utils_all))).
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = mpred_online,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
% [Required] Load the Logicmoo Library Utils
:- endif.
 


% :- portray_text(false).  % or Enable portray of strings


:- thread_local(t_l:omit_full_stop/0).


:- meta_predicate handler_logicmoo_cyclone_call(+).



:- kb_global(baseKB:param_default_value/2).
:- kb_global(xlisting_web:combo_default_value/3).

/*
:- if((current_predicate(is_pfc_file/0),is_pfc_file)).
:- expects_dialect(pfc).
:- set_fileAssertMt(xlisting_web).
singleValueInArg(baseKB:param_default_value,2).
singleValueInArg(combo_default_value,3).
baseKB:param_default_value(Pred,Arity,_Value)==> {kb_shared(Pred/Arity)}.
search_filter_name_comment(N,_,D)==>baseKB:param_default_value(N,D).
% % % combo_default_value(N,_,V) ==> baseKB:param_default_value(N,V)
% % % combo_default_value(Pred,Arity,_Value) ==> {kb_shared(Pred/Arity)}.
% :- ensure_loaded('xlisting_web.pfc').
*/
%:- else.
:- style_check(-discontiguous).
%:- endif.






combo_default_value(human_language,1,'EnglishLanguage').

%% baseKB:param_default_value( ?ARG1, ?ARG2) is det.
%
% Param Default Value.
%
baseKB:param_default_value(N,V):- combo_default_value(N,_,V).

%:- brea.


%% human_language( ?ARG1) is det.
%
% Human Language.
%
%:- kb_global(baseKB:human_language/1).
human_language("AlbanianLanguage").
human_language("ArabicLanguage").
human_language("BasqueLanguage").
human_language("CatalanLanguage").
human_language("ChineseLanguage").
human_language("DanishLanguage").
human_language("EnglishLanguage"). 
human_language("FarsiLanguage").
human_language("FinnishLanguage").
human_language("FrenchLanguage").
human_language("GalicianLanguage").
human_language("GermanLanguage").
human_language("HebrewLanguage").
human_language("IndonesianLanguage").
human_language("ItalianLanguage").
human_language("JapaneseLanguage").
human_language("MalayLanguage").
human_language("NorwegianBokmalLanguage").
human_language("NorwegianNorskLanguage").
human_language("PolishLanguage").
human_language("PortugueseLanguage").
human_language("SpanishLanguage").
human_language("ThaiLanguage").
human_language("de").


baseKB:param_default_value(request_uri,'/swish/lmxref/').
baseKB:param_default_value(olang,'CLIF').
baseKB:param_default_value(fa,'tHumanHead').

:- forall(
  member(N=V,[
     cmd=edit1term,
     'prover'='proverPTTP',
     'apply'='fa',
     'term'='',
     action_below=query,
     'action_above'='query',
     'context'='BaseKB',
     'flang'='CLIF','fa'='tHumanHead','xref'='Overlap','POS'='N',
     'humanLang'='EnglishLanguage','olang'='CLIF','sExprs'='0','webDebug'='1',
     'displayStart'='0','displayMax'='100000']),
  xlisting_web:ain(baseKB:param_default_value(N,V))).


combo_default_value(logic_lang_name,2,'CLIF').
%% logic_lang_name( ?ARG1, ?ARG2) is det.
%
% Logic Language Name.
%
logic_lang_name('E2C',"Logicmoo English (E2C)").
logic_lang_name('CLIF',"Common Logic (CLIF)").
logic_lang_name('CycL',"CycL").
logic_lang_name('Prolog',"Prolog").
logic_lang_name('CGIF',"CG-Logic (CGIF)").
logic_lang_name('SUO-KIF',"SUO-KIF").
logic_lang_name('TPTP',"TPTP (fof/cnf)").
logic_lang_name('OWL',"OWL").



combo_default_value(web_prover_name,2,'proverPTTP').
%% web_prover_name( ?ARG1, ?ARG2) is det.
%
% Prover Name.
%

%:- kb_global(web_prover_name/2).
web_prover_name(proverCyc,"CycL (LogicMOO)").
web_prover_name(proverPFC,"PFC").
web_prover_name(proverPTTP,"PTTP (LogicMOO)").
web_prover_name(proverDOLCE,"DOLCE (LogicMOO)").



/*
combo_default_value(partOfSpeech,2,'N').
%% partOfSpeech( ?ARG1, ?ARG2) is det.
%
% Part Of Speech.
%
:- kb_shared(partOfSpeech/2).
partOfSpeech("N","Noun").
partOfSpeech("V","Verb").
partOfSpeech("J","Adjective").
partOfSpeech("Z","Adverb").
*/

%% search_filter_name_comment( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Search Filter Name Comment.
%

%:- xlisting_web:kb_global(search_filter_name_comment/3).

%:- xlisting_web:dynamic(xlisting_web:search_filter_name_comment/3).
%:- baseKB:import(xlisting_web:search_filter_name_comment/3).
search_filter_name_comment(hideMeta,'Hide Meta/BookKeeping','1').
search_filter_name_comment(hideSystem,'Skip System','0').
search_filter_name_comment(hideTriggers,'Hide Triggers','1').
search_filter_name_comment(skipLarge,'No Large','0').
search_filter_name_comment(showHyperlink,'Hyperlink','1').
search_filter_name_comment(showFilenames,'Filenames','1').
search_filter_name_comment(showHUGE,'showHUGE','1').
search_filter_name_comment(wholePreds,'Whole Preds','1').
search_filter_name_comment(skipVarnames,'Skip Varnames','0').
search_filter_name_comment(hideClauseInfo,'Skip ClauseInfo','0').
search_filter_name_comment(hideXRef,'Skip XREF','1').
search_filter_name_comment(showAll,'Show All','0').
  

%:- add_import_module(baseKB, xlisting_web,end).


:- forall(search_filter_name_comment(N,_,D),ain(baseKB:param_default_value(N,D))).

combo_default_value(is_context,2,'BaseKB').

:- kb_global(xlisting_web:xaction_menu_item/2).

combo_default_value(xaction_menu_item,2,'query').

%arg2Isa(xaction_menu_item,xtPrologString).

%% xaction_menu_item( ?ARG1, ?ARG2) is det.
%
% Action Menu Item.
%

xaction_menu_item('Find',"Find $item").
xaction_menu_item('Forward',"Forward Direction").
xaction_menu_item('Backward',"Backward Direction").
xaction_menu_item('query',"Query $item").
xaction_menu_item('repropagate',"Repropagate $item (ReAssert)").
xaction_menu_item('remove',"Remove $item(Unassert)").   
xaction_menu_item('Code',"Assume Theorem (Disable $item)").
xaction_menu_item('prologSingleValued',"Make $item Single Valued").
xaction_menu_item('prologBuiltin',"Impl $item in Prolog").
xaction_menu_item('prologPTTP',"Impl $item in PTTP").
xaction_menu_item('prologDRA',"Impl $item in DRA").
xaction_menu_item('prologPfc',"Impl $item in PFC").
xaction_menu_item('Monotonic',"Treat $item Monotonic").
xaction_menu_item('NonMonotonic',"Treat $item NonMonotonic").   

% % %:- expects_dialect(swi).


%% print_request( :TermARG1) is det.
%
% Print Request.
%
print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format('<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).


%:- xlisting_web:listing(print_request/1).



%% escape_quoting( ?ARG1, ?ARG2) is det.
%
% make quotable  Primary Helper.
%
escape_quoting(SUnq0,SObj):-
  any_to_string(SUnq0,SUnq),
  atom_subst(SUnq,'\\','\\\\',SObj0),atom_subst(SObj0,'\n','\\n',SObj1),atom_subst(SObj1,'"','\\\"',SObj).



%% make_quotable( ?ARG1, ?ARG2) is det.
%
% Make Quotable.
%
make_quotable(String,SObj):-string(String),format(string(SUnq),'\"~s\"',[String]),into_attribute(SUnq,SObj),!.
make_quotable(String,SObj):-atomic(String),format(string(SUnq),'~w',[String]),into_attribute(SUnq,SObj),!.
make_quotable(String,SObj):-format(string(SUnq),'~q',[String]),into_attribute(SUnq,SObj),!.

% 

% :- set_yes_debug.

:- export(save_in_session/1).



%% save_in_session( :TermARG1) is det.
%
% Save In Session.
%
save_in_session(NV):- \+ compound(NV),!.
save_in_session(NV):- is_list(NV),!,must_maplist(save_in_session,NV),!.
save_in_session(NV):- NV=..[N,V],!,save_in_session(N,V),!.
save_in_session(N=V):- save_in_session(N,V),!.
save_in_session(NV):- dmsg(not_save_in_session(NV)),!.

:- export(save_in_session/2).



%% save_in_session( ?ARG1, ?ARG2) is det.
%
% Save In Session.
%

save_in_session(Search,List):-is_list(List),member(Search,[search,cookie]),once(save_in_session(List)),fail.
save_in_session(Unsaved,_):- member(Unsaved,[session_data,request_uri,search,pool,path,input,cmd,session]),!.
save_in_session(_,V):- sub_term(Sub,V),nonvar(Sub),is_stream(Sub),!.
save_in_session(N,V):- get_http_session(S), save_in_session(S, N,V),!.

% save_in_session(S,N,V):- \+ baseKB:param_default_value(N,_),!.



%% save_in_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Save In Session.
%
save_in_session(S,N,V):- atom(N), NV=..[N,V],functor(NVR,N,1),
   retractall(http_session:session_data(S,NVR)),
   asserta(http_session:session_data(S,NV)),!.
save_in_session(S,N,V):- dmsg(not_save_in_session(S,N,V)),!.



skip_if_ansi(Goal):- is_html_mode,!,call(Goal).
skip_if_ansi(_).

%% show_http_session is det.
%
% Show Http Session.
%
show_http_session:- get_http_session(S),with_pre(listing(http_session:session_data(S,_NV))).

in_pre:- in_tag(pre).
in_tag(Tag):- nb_current('$html_tags',[Tag|_]),!.
in_tag(pp(PP)):- in_pp(PP),!.
:- meta_predicate(with_pre(0)).
with_pre(G):- with_tag('pre',G).

:- export(is_taglike/1).
is_taglike(H):- atom(H), atom_length(H,N), N>0.

:- meta_predicate(with_tag(+,0)).
with_tag(Tag,GL):- is_list(GL),!,maplist(with_tag(Tag),GL).
with_tag(Tag,G):- must_det_ll(call(xlisting_web:is_taglike,Tag)),
  once(nb_current('$html_tags',Was);Was=[]),
    locally(nb_setval('$html_tags',[Tag|Was]), 
    sccs((write('<'),write(Tag),write('>')),once(G),
     (write('</'),write(Tag),write('>')))).

:- meta_predicate(with_tag_class(+,+,0)).
with_tag_class(Tag,Class,Goal):- with_tag_ats(Tag,class(Class),Goal).
:- meta_predicate(with_tag_style(+,+,0)).
with_tag_style(Tag,Style,Goal):- with_tag_ats(Tag,style(Style),Goal).
:- meta_predicate(with_tag_props(+,+,0)).
with_tag_props(Tag,Props,Goal):- with_tag_ats(Tag,Props,Goal).
:- meta_predicate(with_tag_ats(+,+,0)).
with_tag_ats(Tag,Props,GL):- is_list(GL),!,maplist(with_tag_ats(Tag,Props),GL).
with_tag_ats(Tag,Props,G):- 
  must_det_ll(call(xlisting_web:is_taglike,Tag)),
  once(nb_current('$html_tags',Was);Was=[]),
    locally(nb_setval('$html_tags',[Tag|Was]), 
    sccs(
     skip_if_ansi(((write('<'),write(Tag),print_tag_ats(Props),write('>')))),
     once(G),
     skip_if_ansi(((write('</'),write(Tag),write('>')))))).

print_tag_ele(List):- is_list(List),!,maplist(print_tag_ele,List).
print_tag_ele(A:V):- nonvar(A),!, format(' ~w: ~w;',[A:V]).
print_tag_ele(AV):- format(' ~w',[AV]).

print_tag_ats(AV):- (AV==[];var(AV)),!.
print_tag_ats(List):- is_list(List),!,print_tag_ats_l(List).
print_tag_ats(CLASS):- atom(CLASS),!,print_att_val(class,CLASS).
print_tag_ats(AV):- \+ compound(AV),!,format(' ~w',[AV]).

print_tag_ats({AV}):- conjuncts_to_list(AV,List), !, print_tag_ats(List).
print_tag_ats((A,V)):- conjuncts_to_list((A,V),List), !, print_tag_ats(List).
print_tag_ats(call(AV)):- !, call(AV).
print_tag_ats(AV):- compound_name_arguments(AV,A,[V]),!,print_att_val(A,V).
print_tag_ats(AV):- compound_name_arguments(AV,_,[A,V]),!,print_att_val(A,V).
print_tag_ats(AV):- format(' ~w',[AV]).


print_tag_ats_l([H|T]):- !,print_tag_ats(H),print_tag_ats(T).
print_tag_ats_l(AV):- my_partition(has_functor(':'),AV,Styles,Rest),Styles\==[],!,format(' style="@~"',[print_tag_ele(Styles)]),print_tag_ats(Rest).
print_tag_ats_l(AV):- my_partition(p1_call(has_functor('class');atom),AV,Classes,Rest),Classes\==[],!,format(' class="@~"',[print_tag_ele(Classes)]),print_tag_ats(Rest).

print_att_val(A,V):- into_attribute(V,VV), format(' ~w="~w"',[A,VV]).




%% make_session( ?ARG1) is det.
%
% Make Session.
%
make_session(S):- ignore((catch((is_cgi_stream->http_session:http_open_session(S,[renew(false)]);true),_,true))).



:- export(get_http_session/1).
:- system:import(get_http_session/1).
%% get_http_session( ?ARG1) is det.
%
% Get Http Session.
%
get_http_session(S):- catch(find_http_session(S),_,fail),nonvar(S),!, make_session(S).
get_http_session(S):- catch((pengine:pengine_user(S)),_,fail),!.
get_http_session(ID):- thread_self(ID).

on_xx_log_fail(G):- notrace((catch(G,E,((dmsg(E:G)),fail)))).

:- export(find_http_session/1).
:- system:import(find_http_session/1).
%% find_http_session( ?ARG1) is det.
%
% Get Http Session Primary Helper.
%
find_http_session(S):- on_xx_log_fail((get_http_current_request(R),member(session(S),R))),nonvar(S),!.
find_http_session(S):- on_xx_log_fail((http_session:http_in_session(S))),nonvar(S),!.
find_http_session(S):- on_xx_log_fail((get_http_current_request(R),member(cookie(Cookies),R),member(swipl_session=S,Cookies))),nonvar(S),!.
find_http_session(S):- on_xx_log_fail(make_session(S)),nonvar(S),!.




%% is_cgi_stream is det.
%
% If Is A Cgi Stream.
%
is_cgi_stream:-current_output(X),http_stream:is_cgi_stream(X).
%is_cgi_stream:-!.



%% reset_assertion_display is det.
%
% Reset Assertion Display.
%
reset_assertion_display:-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   ignore((resave_edit_filters)),
   retractall(shown_subtype(_)),
   retractall(xlw:shown_clause(_)).

resave_edit_filters:-
   save_in_session('sExprs','0'),save_in_session('webDebug','0'),
   get_http_current_request(B),save_request_in_session(B),!.



%% get_param_sess( ?ARG1, ?ARG2) is det.
%
% Get Param Sess.
%
get_param_sess(N,V):- once(baseKB:param_default_value(N,D);D=''),!,get_param_sess(N,V,D),!.

:- dynamic(lmcache:last_http_request/1).
:- volatile(lmcache:last_http_request/1).
:- dynamic(lmcache:last_item_offered/1).
:- volatile(lmcache:last_item_offered/1).

%% lmcache:last_item_offered( ?ARG1) is det.
%
% Last Item Offered.
%
:- asserta(lmcache:last_item_offered(unknown)).


%% get_http_current_request( ?ARG1) is det.
%
% Get Http Current Request.
%
get_http_current_request(B):- httpd_wrapper:http_current_request(B), !,ignore((retractall(lmcache:last_http_request(_)),asserta(lmcache:last_http_request(B)))).
get_http_current_request(B):- lmcache:last_http_request(B),!.
get_http_current_request(R):- thread_self(main),fake_req(R),!.
get_http_current_request([]).

fake_req(R):- 
 (R =
  [session('f505-37db-4b08-2150.gitlab'),protocol(http),peer(ip(10,0,0,122)),
   pool(client(TID,user:http_dispatch,IOS,IOS)),
   input(IOS), method(get),

   request_uri('/swish/lm_xref/?cmd=is_detatched_thread'),path('/swish/lm_xref/'),search([cmd=is_detatched_thread]),

   http_version(1-1),host('127.0.0.1'),port(3020),
   connection('keep-alive'),pragma('no-cache'),cache_control('no-cache'),upgrade_insecure_requests('1'),
   user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.5112.102 Safari/537.36'),
   accept([media(text/html,[],1.0,[]),media(application/'xhtml+xml',[],1.0,[]),media(image/avif,[],1.0,[]),
   media(image/webp,[],1.0,[]),media(image/apng,[],1.0,[]),media(application/'signed-exchange',[v=b3],0.9,[]),
   media(application/xml,[],0.9,[]),media(_4092/_4094,[],0.8,[])]),accept_encoding('gzip, deflate'),
   accept_language('en-US,en;q=0.9'),cookie([swipl_session='1ea7-7eda-8461-ce27.gitlab'])]),

   current_input(IS),current_output(OS),stream_pair(IOS,IS,OS),
   thread_self(TID).





%% get_param_sess( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Param Sess.
%
get_param_sess(N,V,D):- nonvar(V),!,get_param_sess(N,VV,D),!,param_matches(V,VV).
get_param_sess(L,V,D):- get_nv_session(L,V,D).




%% get_param_req( ?ARG1, ?ARG2) is det.
%
% Get Param Req.
%
get_param_req(L,V):- httpd_wrapper:http_current_request(Request), 
  member(search(List),Request),member(L=V,List),!.
get_param_req(L,V):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V,[optional(true),default(Foo)]],
  get_http_current_request(B),
   http_parameters:http_parameters(B,[CALL2])->
       V \== Foo,!.


get_param_req(L,V,_):- get_param_req(L,V),!.
get_param_req(_,V,V).
% get_param_sess(L,V,V):- (is_list(L)-> member(N,L) ; N=L), save_in_session(N=V),!.




%% get_nv_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Nv Session.
%
get_nv_session(L,V,_):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V], get_any_from_sess(_,CALL2).     
get_nv_session(_,V,V):-!.

get_any_from_sess(F,CALL2):- get_http_session(F),http_session:session_data(F, CALL2).
%get_any_from_sess(_,CALL2):- http_session:session_data(_, CALL2),!.

has_search_filter(Request):-  member(search(Search),Request), search_filter_name_comment(N,_,_), \+ \+ member(N=_,Search),!.
clear_search_filter_in_session:- forall(search_filter_name_comment(N,_,_),save_in_session(N,'0')).
clear_http_session:- ignore((get_http_session(S),retractall(http_session:session_data(S,_NVR)))).


%% save_request_in_session( ?ARG1) is det.
%
% Save Request In Session.
%
save_request_in_session(Request):- 
  on_xf_ignore_flush(save_in_session(t_request,t(Request))),
        on_xf_ignore_flush(member(method(post), Request) -> (http_read_data(Request, Data, []),save_in_session(Data));true),
     on_xf_ignore_flush(has_search_filter(Request) -> clear_search_filter_in_session ; true),
        on_xf_ignore_flush(save_in_session(Request)).
        % http_session:http_session_id(F),forall(http_session:session_data(F,D),wdmsg(D)).

nop_format(G):- nop(format(G)).


:- dynamic(lmcache:current_ioet/4).
:- volatile(lmcache:current_ioet/4).

:- create_prolog_flag(retry_undefined,default,[type(term),keep(true)]).

write_expandable(true,Goal):- !, inline_html_format(['<pre>',call(Goal),'</pre>']).
write_expandable(Showing,Goal):- write_expandable3(Showing,Goal,Goal).

write_expandable3(Showing,Title,Goal):- 
 on_xf_ignore_flush(ensure_colapable_styles), 
 (Showing -> PX='128'; PX='600'),
 (Showing -> Exp=''; Exp='collapsed-c'),
  inline_html_format([
   '<pre><button type="button" class="collapsible">',Title,' (click to un/expand)</button>',
   '<div class="',write(Exp),'" style="max-height: ',PX,'px"><pre>',
   call(Goal),'</pre></div></pre>']).


%% write_begin_html( ?ARG1, Call ) is det.
%
% Write Begin HTML.
%
% <link rel="SHORTCUT ICON" href="/swish/lm_xref/pixmapx/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">
write_begin_html(Title,InHead):-
  inline_html_format([print_xlisting_head(Title,InHead),
   '<body id="body" class="yui-skin-sam cliopatria">',
   %call(ensure_swish_app_html),
   '<div style="display: none;">',  
  (get_param_req(lean,'1') -> write("</div>") ; (write("</div>"), do_cp_menu, format('<br/>'))),  
  % ensure_collapsable_script,
   call(ensure_collapsable_styles)]).

print_xlisting_head(Title,InHead):-
 inline_html_format(['<html><head><title>LOGICMOO XListing - ',write(Title),'</title>',
 '<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<meta name="description" content="Prolog XListing for Logicmoo Code">
	<meta name="author" content="logicmoo@gmail.com">
	<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
	<link rel="stylesheet" type="text/css" href="/swish/css/menu.css"> 
  <link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">  
	<script src="https://unpkg.com/gojs@2.2.15/release/go.js"></script>
  <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
	<script type="text/javascript">window.name="lm_xref"; </script>
	<!-- <script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script>  -->
	<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
	<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>
	<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
	<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
	<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/icon?family=Material+Icons">
  <script type="text/javascript" src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
	<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
  <script type="text/javascript" src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
	<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
  <script type="text/javascript" src="/swish/js/cliopatria.js"></script>
	<link rel="stylesheet" type="text/css" href="/swish/css/butterfly_term.css">
  <script type="text/javascript" href="/swish/js/butterfly_term.js"></script>
	<link rel="stylesheet" type="text/css" href="/swish/css/term.css">',
  call(InHead),
	'</head>']).

do_cp_menu:- \+ current_predicate(cp_menu/2),!.
do_cp_menu:-
  output_html(div([id('cp-menu'), class(menu)],  \ cp_menu)).

offer_testcases :- forall(no_repeats(X,xlisting_whook:offer_testcase(X)),write_cmd_link(X)).

:- export(write_cmd_link/1).
write_cmd_link(menu(Info,Goal)):- nonvar(Info),!, write_cmd_link(Info,Goal).
write_cmd_link(Goal):- sformat(S,'~q',['?-'(Goal)]),write_cmd_link(S,Goal).

:- export(write_cmd_link/2).
write_cmd_link(Info,Goal):- nonvar(Goal),with_output_to(string(S),writeq(Goal)),
  toplevel_pp(PP),
  %in_pp(PP),
  www_form_encode(S,A), 
   sformat(SO,'<a href="/swish/lm_xref/?mouse_iframer_div=~w&cmd=~w" target="lm_xref">~w</a>~n ',[PP,A,Info]),!,
   our_pengine_output(SO).

:- dynamic(xlisting_whook:offer_testcase/1).
:- multifile(xlisting_whook:offer_testcase/1).
xlisting_whook:offer_testcase(G):- nonvar(G), asserta_new(xlisting_whook:offer_testcase(G)),!.
xlisting_whook:offer_testcase(run_pipeline('Every man likes at least 3 things.')).
xlisting_whook:offer_testcase(ls).
xlisting_whook:offer_testcase(xlisting_html(ls)).
%xlisting_whook:offer_testcase(Body):- clause(baseKB:feature_test,Body).
%xlisting_whook:offer_testcase(Body):- clause(baseKB:sanity_test,Body).
xlisting_whook:offer_testcase(embed_test('/','100%','50%')).
xlisting_whook:offer_testcase(test_rok).
xlisting_whook:offer_testcase(test_pp).
xlisting_whook:offer_testcase(X):- var(X), % source_file(xlisting_web:show_menu_types,F),!,
  source_file(xlisting_web:X,_F),ground(X).
:- export(xlisting_whook:offer_testcase/1).


%% handler_logicmoo_cyclone_call( +Request) is det.
%
% Handler Logicmoo Cyclone.
handler_logicmoo_cyclone3(A):- handler_logicmoo_cyclone_call(A).
handler_logicmoo_cyclone2(A):- handler_logicmoo_cyclone_call(A).
handler_logicmoo_cyclone1(A):- handler_logicmoo_cyclone_call(A).
handler_logicmoo_cyclone0(A):- handler_logicmoo_cyclone_call(A).

handler_logicmoo_cyclone_call(_):- quietly(is_goog_bot),!,
  ((format('Content-type: text/html~n~n',[]),
  format('<!DOCTYPE html><html><head></head><body><pre></pre></body></html>~n~n',[]),
  flush_output_safe)),!.

handler_logicmoo_cyclone_call(Request):-     
  make_here,
  with_no_x_http(handler_logicmoo_cyclone(Request)).

on_xf_ignore_flush(G):- flush_output_safe,notrace(ignore(catch(G,_,true))),flush_output_safe.

system:xweto(G):- call(G).
%system:xweto(G):- weto(G).


system:xnotrace(G):- call(G).

%handler_logicmoo_cyclone(Request):- !, arcproc_left(Request),!.
handler_logicmoo_cyclone(Request):- 
  (html_write:html_current_option(content_type(D))-> true ; D=  'text/html'),
  format('Content-type: ~w~n~n', [D]),!,
  %format('<!DOCTYPE html>',[]),flush_output_safe,
  %no_xdbg_flags,
  with_http(
    must_run_html(handler_logicmoo_cyclone000(Request))
     -> true ; handler_logicmoo_cyclone000(Request)),!.

handler_logicmoo_cyclone000(Request):-
  ignore(intern_request_data(Request)),
  handler_logicmoo_cyclone111.

mUST_CALL(G):- G*->true;writeln(failed(G)).
%mUST_CALL(G):- mUST_det_ll(G).

intern_request_data(Request):-
  mUST_CALL((
  ignore(find_http_session(_)), 
  set_prolog_flag(retry_undefined, none),
  current_input(In),current_output(Out),
  once(stream_property(Err,file_no(2));current_error_stream(Err)),
  thread_self(ID),!,
  retractall(lmcache:current_ioet(_,_,_,ID)),
  asserta(lmcache:current_ioet(In,Out,Err,ID)),
  save_request_in_session(Request))),!.

:- dynamic(mu_tmp:asserted_queued_cmd/2).

make_happen(Prolog):- 
  thread_self(Self),
  CODE = mu_tmp:asserted_queued_cmd(Prolog,Self),
  ( CODE -> wdmsg(waiting_on(CODE)) ; 
   (asserta(CODE), 
     ( \+ CODE -> wdmsg(something_doing(CODE)) ; 
      ( sleep(0.2), 
        ( \+ CODE -> wdmsg(something_now_doing(CODE)) ;  (retractall(CODE), now_really(Prolog))))))). 

now_really(Prolog):- 
  %nop(call(CODE)),%wdmsg(thread_signal(main,CODE)),!,  
  CODE = ignore((catch((    
     wdmsg(doing(call(user:Prolog))),
    (user:locally(nb_setval('$maybe_abort',t),call(user:Prolog)),menu)),
     E,wdmsg(Prolog=E)))),
  %thread_signal(main,xlisting_web:maybe_abort),
  thread_signal(main,CODE),!.

maybe_abort:- \+ nb_current('$maybe_abort',t),!.
maybe_abort:- wdmsg(aborting(main)),throw('$aborted').

handler_logicmoo_cyclone111:- get_param_req(mouse_iframer_div,PP),PP=='bfly',get_param_req(cmd,Call),url_decode_term(Call,Prolog),
  make_happen(Prolog),!.

%handler_logicmoo_cyclone111:- current_predicate(handler_logicmoo_arc/0),ignore(with_http(handler_logicmoo_arc)),!.

handler_logicmoo_cyclone111:- 
 get_webproc(WebProc),
 ignore(WebProc=ls),
 maplist(on_xf_ignore_flush,[
  write_begin_html(WebProc,true),

  ((ignore( \+ ((  
    get_param_req(cmd,Call),url_decode_term(Call,Prolog),
    current_predicate(_,Prolog),
    dmsg(cmd=Prolog),
    ignore((nonvar(Prolog),asserta_new(xlisting_whook:offer_testcase(Prolog)))), 
    xweto(write_expandable(true,Prolog))))))),
  %write_expandable(true,(menu)),
   write_expandable(false,(offer_testcases,show_http_session)),
   maybe_do_wp(WebProc),
   ensure_collapsable_script,write_end_html]), !.

 maybe_do_wp(WebProc):- 
    ((ignore(  \+ (( callable(WebProc), must_run_html(WebProc)))))),
    ((get_param_req(lean,'1') -> true ;
     ((
     ((ignore( \+ (( WebProc\== edit1term, edit1term))))),
     ((ignore( \+ (( WebProc\== search4term, search4term))))))))),!.

get_param_req_or_session(N,V):- get_param_req(N,M),!,url_decode_term(M,V).
get_param_req_or_session(N,V):- get_param_sess(N,M),!,url_decode_term(M,V).

get_webproc(Call):- nonvar(Call),get_webproc(SCall),!,Call==SCall.
get_webproc(CallD):- get_param_req_or_session(cmd,Call),url_decode_term(Call,CallD),!.
get_webproc(PageName):- get_param_req(path,PATH),directory_file_path(_,PageName,PATH),current_predicate(PageName/0),!.

%% write_end_html is det.
%
% Write End HTML.
%
write_end_html:- flush_output_safe,
  % add_context_menu,
  format('</body></html>~n~n',[]),flush_output_safe,!.





/*
dasm:print_clause_plain(Term) :-
        current_prolog_flag(color_term, Prolog_flag_Ret),
        make_pretty(Term, Make_pretty_Ret),
        sccs(set_prolog_flag(color_term, false),
                           ( nl,
                             lcolormsg1(Make_pretty_Ret)
                           ),
                           set_prolog_flag(color_term, Prolog_flag_Ret)).
*/

%test_rok_cyclone(X):- print_html_term_tree(X),fail.
test_rok_cyclone(X):- handler_logicmoo_cyclone_call(X).

test_rok:- test_rok(tHumanHead).
test_rok(W) :- 
  In = user_input,
  Out = user_output,
  into_attribute_q(W,TextBoxObj),
  format(atom(S4T),'/swish/lm_xref/?fa=~w',[TextBoxObj]),
  test_rok_cyclone([path_info('/'), protocol(http), peer(ip(127, 0, 0, 1)), 
     pool(client('httpd@3020', http_dispatch, In, Out)),
     input(In), method(get), request_uri(S4T),
     path('/swish/lm_xref/'), search([fa=W]), 
     http_version(1-1), host('127.0.0.1'), port(3020), cache_control('max-age=0'), 
     upgrade_insecure_requests('1'), user_agent('Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3393.4 Safari/537.36'),
     accept([media(text/html, [], 1.0, []), media(application/'xhtml+xml', [], 1.0, []), 
     media(image/webp, [], 1.0, []), media(image/apng, [], 1.0, []), media(application/xml, [], 0.9, []), 
     media(_9672/_9674, [], 0.8, [])]), accept_encoding('gzip, deflate'), accept_language('en-US,en;q=0.9'), 
     cookie(['PHPSESSID'=u265i7e611jval7odhrs316n07, '_ga'='GA1.2.854971883.1519291037', 
     session='eyJjc3JmX3Rva2VuIjoiMGU3MzE1ZWUxMjVkZTNlZDNlZDg3ZDgyNWQ5ZmZiNjMxNjE4ODdjZiJ9.DYDY5A.so4fbyaXlbCXtzExefb_aYRjJ6g', 
     io='DjFUY0jh0SbK64uLAAAM', lo_session_in='1', '_jsuid'='984133034', 
     '__lotl'='https%3A%2F%2Flogicmoo.org%2Fdocs%2FA%2520Fuzzy%2520Belief-Desire-Intention%2520Model%2520for%2520Agent-Based%2520Image%2520Analysis%2520_%2520IntechOpen.html', 
     euCookie='1', swipl_session='cc4e-bdf6-b3ff-9ffc.gitlab']), x_forwarded_for('10.0.0.122'), x_forwarded_host('logicmoo.org'),
      x_forwarded_server('127.0.1.1'), connection('Keep-Alive')]),!.

add_context_menu:-!.

:- include(xlisting_web_cm).

%:- listing(ensure_collapsable_script/0).

% logicmoo_html_needs_debug.


write_script(X):-
  format(X).

%% add_form_script is det.
%
% Add Form Script.
%
% add_form_script:-!.
add_form_script:- 
  must_run_html((write_script("\n<script>
function add_form_script() {

if(!(window.added_form_script)) {

window.added_form_script = true;

$('form').submit(function() {
  $(this).find('input[type=checkbox]').each(function (i, el) {
    if(!el.checked) {
      var hidden_el = $(el).clone();
      hidden_el[0].checked = true;
      hidden_el[0].value = '0';
      hidden_el[0].type = 'hidden';
      hidden_el.insertAfter($(el));
    }    
  })
 // alert($(this));
});

}

var handled = false;"),

write("

function callback(e) {
    var e = window.e || e;

    var targ = e.target;
    if (targ.tagName !== 'A')
        return;
    if(!handled) {     
      handled = true;
     // alert('hi ' +  targ.target);
      if (targ.target !== '') {
       return;
      }
      e.preventDefault();
      e.stopPropagation();
      $('form').action = targ.href;
      document.getElementById('fa').value = targ.innerText;
     // alert('hi ' +  targ.innerText);
      $('form').submit();
    } else {
      handled = false;           
    }
}

if (document.addEventListener)
    document.addEventListener('click', callback, false);
else
    document.attachEvent('onclick', callback);

if (document.addEventListener)
    document.addEventListener('load', add_form_script, false);
else
    document.attachEvent('load', add_form_script);

}

</script>
"))).




%% show_pcall_footer is det.
%
% Show Pcall Footer.
%
show_pcall_footer:- format('<hr><a href="/swish/lm_xref/">LogicMOO/PrologMUD</a>',[]),!.






% :- (thread_property(ID,status(running)),ID=reloader30) -> true; thread_create(((repeat,sleep(30),mmake,fail)),_,[alias(reloader30),detached(true)]).
% ===================================================
% Pretty Print Formula
% ===================================================

in_webui:- (in_pp(html);in_pp(bfly)),!.

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(_Options, Pos, Term):- % in_webui,
  if_defined(rok_linkable(Term),fail),
  with_no_hrefs(t,
   (nop(prefix_spaces(Pos)),
    write_atom_link(Term))),!.


%% write_atom_link( ?ARG1) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/1).
write_atom_link(A):- write_atom_link(A,A),!.

rok_linkable(A):- atom(A),!,A\==[].
rok_linkable(A):- string(A),!.

%% write_atom_link( ?ARG1, ?ARG2) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/2).
write_atom_link(L,N):- write_atom_link(atom(W),L,N),format('~w',[W]),!.

% pred_href(Name/Arity, Module, HREF) :-



%% write_atom_link( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/3).
write_atom_link(W,A/_,N):-atom(A),!,write_atom_link(W,A,N).
write_atom_link(W,C,N):- sanity(nonvar(W)),compound(C),get_functor(C,F,A),!,write_atom_link(W,F/A,N).
%write_atom_link(W,_,N):- thread_self_main,!,write_plain_atom(W,N),!.
write_atom_link(W,_,N):- \+ is_html_mode, write_plain_atom(W,N),!.
write_atom_link(W,_,N):- nb_current('$no_hrefs',t), !, format(W,'<code>~w</code>',[N]),!.
write_atom_link(W,A,N):-  
 catch((into_attribute_q(A,TextBoxObj),
   format(W,'<a href="?fa=~w">~q</a>',[TextBoxObj,N])),E,(dmsg(E),write_plain_atom(W,N))).


%% write_plain_atom( :TermARG1, ?ARG2) is det.
%
% Write Term Converted To Atom One.
%
write_plain_atom(S,Term):-format(S,'~q',[Term]).

/*


%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

:- public
	portable_display/1,
	portable_listing/0,
	portable_listing/1,
	portable_print/1,
	portable_write/1,
	portable_writeq/1,
	rok_portray_clause/1.

:- meta_predicate
	classify_name(+, -),
	classify_alpha_tail(+),
	classify_other_tail(+),
	'functor spec'(+, -, -, -),
	list_w_clauses(+, +, +, +),
	list_w_magic(+, +),
	list_w_magic(+, +, +),
	list_w_magic(+, +, +, +),
	maybe_paren(+, +, +, +, -),
	maybe_space(+, +),
	rok_portray_clause(+),
	put_string(+),
	put_string(+, +),
	write_args(+, +, +, +, +),
	write_atom(+, +, +, -),
	write_oper(+, +, +, +, -),
	write_out(+, +, +, +, -),
	write_out(+, +, +, +, +, +, -),
	write_tail(+, +),
	write_VAR(+, +, +, -),
	write_variable(?).
*/
     

/*  WARNING!
    This file was written to assist portability and to help people
    get a decent set of output routines off the ground fast.  It is
    not particularly efficient.  Information about atom names and
    properties should be precomputed and fetched as directly as
    possible, and strings should not be created as lists!

    The four output routines differ in the following respects:
    [a] display doesn't use operator information or handle {X} or
	[H|T] specially.  The others do.
    [b] print calls portray/1 to give the user a chance to do
	something different.  The others don't.
    [c] writeq puts quotes around atoms that can't be read back.
	The others don't.
    Since they have such a lot in common, we just pass around a
    single Style argument to say what to do.

    In a Prolog which supports strings;
	write(<string>) should just write the text of the string, this so
	that write("Beware bandersnatch") can be used.  The other output
	commands should quote the string.

    listing(Preds) is supposed to write the predicates out so that they
    can be read back in exactly as they are now, provided the operator
    declarations haven't changed.  So it has to use writeq.  $VAR(X)
    will write the atom X without quotes, this so that you can write
    out a clause in a readable way by binding each input variable to
    its name.
*/





%% portable_display( ?ARG1) is det.
%
% Portable Display.
%
portable_display(Term) :-
	write_out(Term, display, 1200, punct, _).





%% portable_print( ?ARG1) is det.
%
% Portable Print.
%
portable_print(Term) :-
   write_out(Term, print, 1200, punct, _).





%% portable_write( ?ARG1) is det.
%
% Portable Write.
%
portable_write(Term) :-
   write_out(Term, write, 1200, punct, _).





%% portable_writeq( ?ARG1) is det.
%
% Portable Writeq.
%

%portable_writeq(Term) :- max_depth_goal(0,'$rok_print_tree',local_print_tree(Term)),!.
portable_writeq(Term) :- rok_writeq(Term).

rok_writeq(Term) :-
       write_out(Term, writeq, 1200, punct, _).



%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.




%% maybe_paren( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Maybe Paren.
%
maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.




%% maybe_space( ?ARG1, ?ARG2) is det.
%
% Maybe Space.
%
maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).




%   write_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.




%% write_variable( ?ARG1) is det.
%
% Write Variable.
%
write_variable(V) :- get_var_name(V,Name),!,write(Name).
write_variable(V) :- writeq(V).

%portray_or_print(Term):- var(Term),!,write_variable(Term).
portray_or_print(Term):- rok_linkable(Term),!,is_html_mode,write_atom_link(Term).
%portray_or_print(Term):- catch(user:portray(Term),_,fail),!.
%portray_or_print(Term):- catch(print(Term),_,fail),!.

%%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.


write_out(Term, Style, _Prio, _Ci, _Co):- !, call(Style,Term).
write_out(Term, Style, Prio, Ci, Co):-
 write_oout(Term, Style, Prio, Ci, Co).

%% write_oout(Term, Style, Prio, Ci, Co) is det.
%
% Write Out.
%
write_oout(Term, _, _, Ci, alpha) :-
	var(Term),
	!,
	maybe_space(Ci, alpha),
	write_variable(Term).
write_oout('$VAR'(N), Style, _, Ci, Co) :- !,
	write_VAR(N, Style, Ci, Co).
write_oout(N, _, _, Ci, alpha) :-
	integer(N),
	(   N < 0, maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	name(N, String),
	put_string(String).
write_oout(Term, _, _, _Ci, _Co) :-
	string(Term),
	!,
	write_atom_link(Term, Term).

write_oout(Term, print, _,  _, alpha) :-
	% DMILES HSOULD BE print/1
        loop_check(portray_or_print(Term),writeq(Term)),
       % loop_check(print(Term),writeq(Term)),
        % print(Term),
	!.
write_oout(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	current_op(P, _, Atom),
	P > Prio,
	!,
	put(40),
	(   Style = writeq, write_atom(Atom, Style, punct, _)
	;   (name(Atom, String), put_string(String))
	),  !,
	put(41).
write_oout(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	write_atom(Atom, Style, Ci, Co).
write_oout(Term, display, _, Ci, punct) :- !,
	functor(Term, Fsymbol, Arity),
	write_atom(Fsymbol, display, Ci, _),
	write_args(0, Arity, Term, 40, display).
write_oout({Term}, Style, _, _, punct) :- !,
	put(123),
	write_oout(Term, Style, 1200, punct, _),
	put(125).
write_oout([Head|Tail], Style, _, _, punct) :- !,
	put(91),
	write_oout(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_oout((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	write_oout(A, Style, 999, C1, _),
	put(44),
	write_oout(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).

write_oout(List, _Style, _Prio, Ci, _Co) :- Ci\==punct, is_list(List), !, call(print,List).

%write_oout(Term, _Style, _Prio, _Ci, _Co) :- local_print_tree(Term),!.

write_oout(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	write_oout_cmpd(N, F, Term, Style, Prio, Ci, Co).


local_print_tree(Term):- max_depth_goal(0,'$rok_print_tree',with_output_to(string(S), mUST_CALL(print(Term)))), !, write(S).

%% write_oout( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6, ?ARG7) is det.
%
% Write Out.
%
write_oout_cmpd(1, F, Term, Style, Prio, Ci, Co) :- compound(Term),
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	write_atom(F, Style, C1, C2),
	arg(1, Term, A),
	write_oout(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_oout_cmpd(1, F, Term, Style, Prio, Ci, Co) :- compound(Term),
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_oout(A, Style, P, C1, C2),
	write_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_oout_cmpd(2, F, Term, Style, Prio, Ci, Co) :- compound(Term),
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_oout(A, Style, P, C1, C2),
	write_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	write_oout(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
write_oout_cmpd(N, F, Term, Style, _Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).





%% write_oper( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Oper.
%
write_oper(Op, Prio, Style, Ci, Co) :-
  integer(Prio),
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, _Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).




%% write_VAR( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Var.
%
write_VAR(A, _Style, _Ci, _Co) :- atom(A), !,write(A).
write_VAR(N, writeq, _Ci, alpha):- writeq('$VAR'(N)),!.
write_VAR(X, Style, Ci, punct) :-
	write_atom('$VAR', Style, Ci, _),
	write_args(0, 1, '$VAR'(X), 40, Style).





%% write_atom( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Atom.
%
write_atom(('!'), _, _, punct) :- !,
	put(33).
write_atom((';'), _, _, punct) :- !,
	put(59).
write_atom([], _, _, punct) :- !,
	put(91), put(93).
write_atom({}, _, _, punct) :- !,
	put(123), put(125).
write_atom(A, write, _Ci, _Co):- !,write(A),!.
write_atom(A, _Style, _Ci, _Co):- write_atom_link(A,A),!.
write_atom(Atom, Style, Ci, Co) :-
	name(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = writeq, Co = quote,
	    maybe_space(Ci, Co),
	    (put(39), put_string(String, 39),put(39))
	;   Co = alpha,
	    put_string(String)
	),  !.



%txt_to_codes(Text,Codes):- text_to_string(Text,Str),name(Str,Codes).

%% put_string( ?ARG1) is det.
%
%   writes a list of character codes.
%
put_string(B):- txt_to_codes(B,C),put_string0(C).



%% put_string0( :TermARG1) is det.
%
% Put String Primary Helper.
%
put_string0([]).
put_string0([H|T]) :-
	put(H),
	put_string0(T).


%%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.
put_string(A,B):- is_html_mode,!, write_atom_link(A,B).
put_string(A,B):- put_string2(A,B).


put_string2(A,B):- txt_to_codes(A,C),to_ascii_code(B,BC),put_string0(C,BC).
to_ascii_code(B,BC):- (number(B)->BC=B;name(B,[BC|_])).

% :-rtrace.



%% put_string0( :TermARG1, ?ARG2) is det.
%
% Put String Primary Helper.
%
put_string0([], _) :- !. % put(Q).
put_string0([Q|T], Q) :- !,
	put(Q), put(Q),
	put_string0(T, Q).
put_string0([H|T], Q) :-
	put(H),
	put_string0(T, Q).


%%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in write_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, andf just looked up.  This has to
%   be as fast as you can make it.
classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).




%% classify_alpha_tail( :TermARG1) is det.
%
% Classify Alpha Tail.
%
classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).




%% classify_other_tail( :TermARG1) is det.
%
% Classify Other Tail.
%
classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .




%% write_args( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Arguments.
%
write_args(N, N, _, _, _) :- !,
	put(41).
write_args(I, N, Term, C, Style) :- compound(Term),
	put(C),
	J is I+1,
	arg(J, Term, A),
	write_oout(A, Style, 999, punct, _),
	write_args(J, N, Term, 44, Style).



%   write_tail(Tail, Style)
%   writes the tail of a list of a given style.




%% write_tail( :TermARG1, ?ARG2) is det.
%
% Write Tail.
%
write_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put(124),
	write_variable(Var),
	put(93).
write_tail([], _) :- !,			%  ]
	put(93).
write_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put(44),
	write_oout(Head, Style, 999, punct, _),
        
	write_tail(Tail, Style).
write_tail(Other, Style) :-		%  |junk]
	put(124),
	write_oout(Other, Style, 999, punct, _),
	put(93).


/*  The listing/0 and listing/1 commands are based on the Dec-10
    commands, but the format they generate is based on the "pp" command.
    The idea of rok_portray_clause/1 came from PDP-11 Prolog.

    BUG: the arguments of goals are not separated by comma-space but by
    just comma.  This should be fixed, but I haven't the time right not.
    Run the output through COMMA.EM if you really care.

    An irritating fact is that we can't guess reliably which clauses
    were grammar rules, so we can't print them out in grammar rule form.

    We need a proper pretty-printer that takes the line width into
    acount, but it really isn't all that feasible in Dec-10 Prolog.
    Perhaps we could use some ideas from NIL?
*/




%% portable_listing is det.
%
% Portable Listing.
%
portable_listing :-
	current_predicate(_, M:Pred),
        \+ predicate_property(M:Pred,imported_from(_)),
        predicate_property(M:Pred,number_of_clauses(_)),        
	nl,
	forall(clause(M:Pred, Body),rok_portray_clause((M:Pred:-Body))),
        fail.
portable_listing.


%   listing(PredSpecs)

%   Takes a predicate specifier F/N, a partial specifier F, or a
%   list of such things, and lists each current_predicate Pred
%   matching one of these specifications.




%% portable_listing( :TermARG1) is det.
%
% Portable Listing.
%
portable_listing(V) :-
	var(V), !.       % ignore variables
portable_listing([]) :- !.
portable_listing([X|Rest]) :- !,
	portable_listing(X),
	portable_listing(Rest).
portable_listing(X) :-
	'functor spec'(X, Name, Low, High),
	current_predicate(Name, Pred),
	functor(Pred, _, N),
	N >= Low, N =< High,
	nl, 
	clause(Pred, Body),
	rok_portray_clause((Pred:-Body)),
	fail.
portable_listing(_).




%% functor spec( ?ARG1, ?ARG2, :GoalARG3, :PRED255ARG4) is det.
%
% Functor Spec.
%
'functor spec'(Name/Low-High, Name, Low, High) :- !.
'functor spec'(Name/Arity, Name, Arity, Arity) :- !.
'functor spec'(Name, Name, 0, 255).




%% rok_portray_clause( :TermARG1) is det.
%
% Rok Portray Clause.
%

% rok_portray_clause(Var):- !, with_pp(html,print_tree(Var)).

rok_portray_clause(Var):- var(Var),!,write_variable(Var).
rok_portray_clause(Var):- rok_portray_clause0(Var), !.

rok_portray_clause0(Var):- var(Var),!,write_variable(Var).
rok_portray_clause0(I):- catch(get_clause_vars_for_print_here(I,O),_,I=O),must_run(rok_portray_clause1(O)),!.

rok_portray_clause1( :-(Command)) :- 
	(   Command = public(Body), Key = (public)
	;   Command = mode(Body),   Key = (mode)
	;   Command = type(Body),   Key = (type)
	;   Command = pred(Body),   Key = (pred)
	;   Command = Body,	    Key = ''
	),  !,
	nl,
	% nu mbervars(Body, 0, _),
	\+ \+ list_w_clauses(Body, Key, 2, 8),!.
rok_portray_clause1(M:(Pred:-Body)) :- !,
     must_run((
	% nu mbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(M:Pred),
	\+ \+ list_w_clauses(Body, 0, 2, 8))), !.
rok_portray_clause1((Pred:-Body)) :- !,
     must_run((
	% nu mbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(Pred),
	\+ \+ list_w_clauses(Body, 0, 2, 8))), !.
rok_portray_clause1(M:(Pred)) :- 
	call(call,rok_portray_clause1((M:Pred:-true))).
rok_portray_clause1((Pred)) :- !,
	call(call,rok_portray_clause1((Pred:-true))).


%% list clauses( :TermARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Clauses.
%
list_w_clauses((A,B), L, R, D) :- !,
	list_w_clauses(A, L, 1, D), !,
	list_w_clauses(B, 1, R, D).
list_w_clauses(true, _L, 2, _D) :- !,
	put(0'. %'
        ), nl.
        
list_w_clauses((A;B), L, R, D) :- !,
	list_w_magic(fail, L, D),
	list_w_magic((A;B), 0, 2, D),
	list_w_magic(R, '.
'
).

list_w_clauses((A->B), L, R, D) :- !,
	list_w_clauses(A, L, 5, D), !,
	list_w_clauses(B, 5, R, D).
list_w_clauses(Goal, L, R, D) :-
	list_w_magic(Goal, L, D),
	portable_writeq(Goal),
	list_w_magic(R, '.
'
).




%% list magic( ?ARG1, :PRED5ARG2, ?ARG3) is det.
%
% List Magic.
%
list_w_magic(!,    0, _D) :- !,
	write(' :- ').
list_w_magic(!,    1, _D) :- !,
	write(',  ').
list_w_magic(_Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
list_w_magic(_Goal, 1, D) :- !,
	put(','),
	nl, tab(D).
list_w_magic(_Goal, 3, _D) :- !,
	write('(   ').
list_w_magic(_Goal, 4, _D) :- !,
	write(';   ').
list_w_magic(_Goal, 5, D) :- !,
	write(' ->'),
	nl, tab(D).
list_w_magic(_Goal, Key, D) :-
	atom(Key),
	write(':- '), write(Key),
	nl, tab(D).





%% list magic( ?ARG1, ?ARG2) is det.
%
% List Magic.
%
list_w_magic(2, C) :- !, write(C).
list_w_magic(_, _).





%% list magic( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Magic.
%
list_w_magic((A;B), L, R, D) :- !,
	list_w_magic(A, L, 1, D), !,
	list_w_magic(B, 1, R, D).
list_w_magic(Conj,  L, R, D) :-
	E is D+8,
	M is L+3,
	list_w_clauses(Conj, M, 1, E),
	nl, tab(D),
	list_w_magic(R, ')').


/*	Test code for rok_portray_clause.
	If it works, test_portray_clause(File) should write out the
	contents of File in a more or less readable fashion.

test_portray_clause(File) :-
	see(File),
	repeat,
	    read(Clause, Vars),
	    (   Clause = end_of_file
	    ;   test_bind(Vars), rok_portray_clause(Clause), fail
	    ),
	!,
	seen.

test_bind([]) :- !.
test_bind([X='$VAR'(X)|L]) :-
	test_bind(L).
:- public test_portray_clause/1.
*/






% is_html_mode:- \+ in_pp(ansi), \+ toplevel_pp(bfly), !.
is_html_mode:- get_print_mode(html), \+ in_pp(ansi).


%% sanity_test_000 is det.
%
% Optional Sanity Checking test  Primary Helper Primary Helper Primary Helper.
%

bok((pkif :-

        [ implies,

          [ isa(F, tPred),
            isa(A, ftInt),
            poss(KB, pos([arity(F, A)])),
            poss(KB, arity(F, A))
          ],
          =>,

          [ all([F]),

            [ implies,
              [isa(F, tPred), ex([A]), isa(A, ftInt), poss(KB, arity(F, A))],
              =>,
              [ex([A]), [isa(A, ftInt), arity(F, A)]]
            ]
          ]
        ])).




%make_here:- ttyflush,notrace(ignore(xweto(wo_messages((make_here0,ttyflush))))).
make_here:- !.
make_here:-  with_no_x_http(make_here0).
make_here0:- with_output_to(string(_),xweto(ignore(make))),!.
make_here0:- with_output_to(string(_),make),!.

x123:- make_here,x123(test_rok(tHumanHead)).
x124:- make_here,
 x123((print_tree(
     ((call_for_terms(
                  do_search -> xlisting_html(Html)),
  do_search -> xlisting_html(Html)),
  ignore(((do_search -> xlisting_html(Html));write(no_search)))),[nl(false)]))).
x125:- bok(G),x123((print_tree(G))).
x126:- x123(x127).
x127:- bok(G),find_and_call((rok_portray_clause((G)))).

x129:- make_here,locally_tl(print_mode(html),test_rok(end_of_file)).
x123(G):- make_here, locally_tl(print_mode(html), with_pp(html,G)).

%% param_matches( ?ARG1, ?ARG2) is det.
%
% Param Matches.
%
param_matches(A,B):-A=B,!.
param_matches(VV,V):-atomic(VV),atomic(V),string_to_atom(VV,VVA),string_to_atom(V,VA),downcase_atom(VVA,VD),downcase_atom(VA,VD).
param_matches(A,B):-A=B,!.




%% show_select2( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Select Extended Helper.
%
:- meta_predicate(show_select2(+,:,+)).
show_select2(Name,Pred,Options):- must_run_html(show_select22(Name,Pred,Options)).
show_select22(Name,Pred,Options):-  
    append_termlist(Pred,[ID,Value],Call),
    must_run(baseKB:param_default_value(Name,D); baseKB:param_default_value(Pred,D)),!,
    get_param_sess(Name,UValue,D),
    must_run_html((format('<select name="~w">',[Name]),
    forall(no_repeats(Call),
       (((member(atom_subst(Item,ItemName),Options) -> (any_to_string(Value,ValueS),atom_subst(ValueS,Item,ItemName,NValue)); NValue=Value),
        (((param_matches(UValue,ID);param_matches(UValue,NValue)) -> format('<option value="~w" selected="yes">~w</option>',[ID,NValue]);
                   format('<option value="~w">~w</option>',[ID,Value])))))),
    format('</select>',[]))),!.



%% show_select1( ?ARG1, ?ARG2) is det.
%
% Show Select Secondary Helper.
%
show_select1(Name,Pred):- 
  must_run_html(show_select11(Name,Pred)).

show_select11(Name,Pred):-
 Call=..[Pred,Value],
 ( baseKB:param_default_value(Name,D); baseKB:param_default_value(Pred,D)),!,
 format('<select name="~w">',[Name]),
 forall(Call,
    (get_param_sess(Name,Value,D)->format('<option value="~w" selected="yes">~w</option>',[Value,Value]);
                format('<option value="~w">~w</option>',[Value,Value]))),
 format('</select>',[]),!.



% :- ensure_loaded(library(logicmoo/util/logicmoo_util_varnames)).

% :- use_listing_vars.



%% search4term is det.
%
% Search4term.
%

search4term:- toplevel_pp(bfly), !, show_search4term. 
search4term:- 
 must_run((show_search4term,
  (do_search 
   -> call_for_terms(ignore(( get_search_term(Obj), xlisting_html(Obj))))
   ; write(no_search)))),!.

show_search4term:-  get_search_term(Obj), show_search_form(Obj).

    
show_search_form(Obj):- toplevel_pp(bfly), !, show_search_form(Obj,lm_xref).
show_search_form(Obj):- show_search_form(Obj,lm_xref).

show_search_form(Obj,Where):- 
  inline_html_format(['<form action="/swish/lm_xref/" target="',q(Where),'">', %call(add_form_script),     
     'Find: <input id="fa" type="text" name="fa" value="',q(Obj),'">&nbsp;',
     'Apply: ', action_menu_applied('action_below','Checked or Clicked',"&nbsp;below&nbsp;"),'&nbsp;\n',
    show_search_filters('&nbsp;&nbsp;'),         
    '</form>']).

do_search:- \+ get_webproc(edit1term), !.
do_search:- get_param_req(action_below,query), !.
do_search:- get_param_req(fa,_), !.
do_search:- thread_self(main),!.

term_to_find(H,Word):- empty_str(H),!,Word=_.
term_to_find(H,Word):- var(H),!,Word=_.
term_to_find(H:-_,Word):- !, term_to_find(H,Word).
term_to_find(H, H):- \+ compound(H).
term_to_find(H, F/A ):- compound(H), compound_name_arity(H,F,A).

get_search_term(Obj):- 
   get_param_sess(term,Term,"tHumanHead"),
   get_param_sess(fa,SObj,Term),
   url_decode_term(SObj,Obj).


%ensure_guitracer_x:-!.
ensure_guitracer_x:- %break,
  current_prolog_flag(xpce,Was1),
  current_prolog_flag(gui,Was2),
  absolute_file_name(swi(xpce/prolog/lib),X), 
  set_prolog_flag(xpce,false),
  set_prolog_flag(gui,false),
  asserta(user:library_directory(X),CLRef), 
  user:use_module(library(pce_prolog_xref)),
  user:use_module(library(emacs_extend)),
  user:use_module(library(trace/gui)),
  user:use_module(library(pce)),
  user:use_module(library(gui_tracer)),
  %reload_library_index,
  erase(CLRef),
  set_prolog_flag(xpce,Was1),
  set_prolog_flag(gui,Was2).


%% do_guitracer is det.
%
% Do Guitracer.
%
do_guitracer:- ensure_guitracer_x, call(call,guitracer),dtrace.

output_telnet_console(Port):- HttpPort is Port +100,
  sformat(HTML,'<iframe id="port~w" src="https://logicmoo.org:~w/" height="600" width="100%">loading...</iframe>',[HttpPort,HttpPort]),
  write_html(HTML).
output_telnet_console2(Port):- HttpPort is Port +100,
  sformat(HTML,'<iframe id="port~w" src="https://logicmoo.org:~w/" height="80%" width="100%">loading...</iframe>',[HttpPort,HttpPort]),
  write_html(HTML).






inline_html_format(G):- must_run_html(ilhf(G)).

ilhf(X):- var(X),!,format(' ~p ',[X]),!,flush_output_safe.
ilhf(X):- (string(X);is_codelist(X);is_charlist(X)),!,format('~s',[X]),flush_output_safe.
ilhf(X):- \+ callable(X), !, format(' ~p ',[X]),!,flush_output_safe.
ilhf([]):-!.
ilhf([H|T]):- is_grid([H|T]),print_grid([H|T]),!.
ilhf([H]):- !, ilhf(H).
ilhf([H|T]):- !, ilhf(H),ilhf(T).
ilhf((H,T)):- !, ilhf(H),ilhf(T).
ilhf(X):- atom(X), \+ current_predicate(_,X), !, catch(format(X),_,write(X)), flush_output_safe.
ilhf(call(X)):- !, mUST_CALL(X),flush_output_safe.
ilhf(ignore(X)):- !, mUST_CALL(ignore(X)),flush_output_safe.
ilhf(q(X)):-!, into_attribute(X,Y),write(Y), flush_output_safe.
ilhf(w(X)):-!, write(X),flush_output_safe.
ilhf(h(X)):-!, with_http(ilhf(X)), flush_output_safe.
%ilhf(h(X)):-!, write_bfly_html(X), flush_output_safe.
ilhf(X):- ( current_predicate(_,X)->ignore(mUST_CALL(X));print_tree(X)),flush_output_safe.



get_session_term(Term,Vs):- 
   get_param_sess(term,String,""),
   url_decode_term(String,Term,Vs),
   add_to_env_here(Vs),
   %ignore((term_to_find(Term,Word), nonvar(Word), save_in_session(fa,Word))),
   !.

%% show_pre_call( :GoalARG1) is det.
%
% show_pre_call.
%
show_pre_call(Goal):- 
  inline_html_format(['<pre>',xweto(on_x_debug(Goal)),'</pre>']).

do_post_edit_term(Term,Vs):- get_param_req('ASK','ASK'),!,
  show_pre_call(forall(Term,pp_item_html('Answer',':-'(Vs,Term)))),!.
  
do_post_edit_term(Term,Vs):- get_param_req('TELL','TELL'),!,
  show_pre_call(forall(ain(Term),pp_item_html('Assert',':-'(Vs,Term)))),!.
  
do_post_edit_term(Term,Vs):- get_param_req('RETRACT','RETRACT'),!,
   show_pre_call(forall(mpred_withdraw(Term),pp_item_html('Retract',':-'(Vs,Term)))),!.
  
do_post_edit_term(_Term,_Vs):- !.


%% edit1term is det.
%
% Edit One term.
%
edit1term:-  get_session_term(Term,Vs), !, show_edit_term(Term,Vs), do_post_edit_term(Term,Vs).

edit1term(String):- url_decode_term(String,Term,Vs), show_edit_term(Term,Vs).

%% show_edit_term(  ?Term, ?Vs) is det.
%
% Show Edit Term.
%
show_edit_term(Term,Vs):- show_edit_term_c(Term,Vs).
show_edit_term_c(Term,Vs):-
 inline_html_format([ 
'<form action="?cmd=edit1term"> <table width="90%" cellspacing="0" cellpadding="0" height="121" id="table4">',
'<tbody><tr><td align="left" valign="top" width="36"><img src="/swish/lm_xref/pixmapx/sigmaSymbol-gray.gif"></td><td></td><td align="left" valign="top" width="711" rowspan="2"><img src="/swish/lm_xref/pixmapx/logoText-gray.gif">&nbsp;&nbsp;Prover:&nbsp;',
 show_select2(prover, web_prover_name,[]),
'<table cellspacing="0" cellpadding="0" id="table5" width="658" height="97"><tbody><tr><td align="left"><b>Fml:</b></td><td align="left" valign="top" colspan="4">',
'<textarea style="white-space: pre;overflow: auto;font-size: 7pt;font-weight: bold;font-family: Verdana, Arial, Helvetica, sans-serif;border: 1px solid black;margin: 0px;width: 600px;height: 131px;" wrap="off" rows="20" cols="70" name="term"textarea style="white-space: pre;overflow: auto;font-size: 7pt;font-weight: bold;font-family: Verdana, Arial, Helvetica, sans-serif;border: 1px solid black;margin: 0px;width: 600px;height: 131px;" wrap="off" rows="20" cols="70" name="term">',print_pretty_string(Term,Vs),'</textarea></td><td align="left" valign="top" height="68"><label>',
action_menu_applied('action_above',"Item",""),
'&nbsp;&nbsp;&nbsp;<input type="submit" value="Now" name="Apply"></label><br><b>Microthory</b><br>',
show_select2('context',is_context,[]),
'<br><input type="submit" value="ASK" name="ASK"><input type="submit" value="TELL" name="TELL"><input type="submit" value="RETRACT" name="RETRACT"> <br>',
'<b>Input Language</b><br>',show_select2(flang,logic_lang_name,[]),
'</td></tr><tr><td></td><td></td><td></td><td height="3"></td></tr><tr><td align="left" valign="top" width="144">&nbsp;</td><td align="left" valign="top" height="26" width="139"></td></tr></tbody></table></td><td valign="bottom" width="9" rowspan="2"></td><td height="121" rowspan="2" width="163"><span class="navlinks"><b>[&nbsp;<a href="../">Home</a>&nbsp;|&nbsp;<a href="?Graph=true">Grap2h</a>]</b></span>',
'<p><b>Response&nbsp;Language&nbsp;<br></b>',show_select2(olang,logic_lang_name,[]),'</p><p>',
show_select1('humanLang',human_language),
'<br>',session_checkbox(webDebug,'Debugging','&nbsp;'),session_checkbox(sExprs,'S-Exprs','&nbsp;'),
 '</p></td><td height="121" rowspan="2" width="188"></td></tr><tr><td width="4">&nbsp;</td></tr></tbody></table>']).


%% show_iframe( ?ARG1) is det.
%% show_iframe( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Iframe.
%
show_iframe(URL):- must_run_html(format('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="search4term?fa= ~w"></iframe>',[URL])).

show_iframe(URL,Name,Value):- sformat(FullURL,'~w?~w=~w',[URL,Name,Value]),embed_test(FullURL,'100%','40%'),!.
%show_iframe(URL,Name,Value):- format('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="~w?~w= ~w"></iframe>',[URL,Name,Value]).


embed_test(URL,Width,Height):- 
  format( '<div width="~w" height="~w" ><object data="~w"  width="100%" height="~w" type="text/html"><embed src="~w" width="100%" height="100%" onerror="alert(`URL invalid ~w !!`);"/></object></div>',
                [     Width,     Height,              URL,                      Height,                        URL,        URL                  ]).

%slow_frame(Goal):- !,call(Goal).
slow_frame(Goal):- slow_frame('300',Goal).
slow_frame(_,Goal):- thread_self(main), \+ toplevel_pp(bfly),!, call(Goal).
slow_frame(Height,Goal):- 
   gensym("into",Where), url_encode(Goal,GoalE),  
   sformat(FullURL,'/swish/lm_xref/slowcode/?into=~w&goal=~w',[Where,GoalE]),
   inline_html_format(['<div style="white-space: pre; font-size: 7pt; font-weight: bold; font-family: Verdana, Arial, Helvetica, sans-serif;border: 2px solid black; ',
       'overflow: auto; overflow-y: visible; min-height: 40%; height: ',Height,'; max-height: 40%;" cols="100%" title="',
       q(goal_attach_to(Where,Goal)),'" id="',q(Where),'">working..</div>',
       embed_test(FullURL,'0%','0%')]).

slow_iframe(Goal):- slow_iframe('300',Goal).
slow_iframe(_,Goal):- thread_self(main), \+ toplevel_pp(bfly),!, call(Goal).
slow_iframe(Height,Goal):- url_encode(Goal,GoalE),  
   sformat(FullURL,'/swish/lm_xref/slowcode/?lean=1&goal=~w',[GoalE]),
   embed_test(FullURL,'100%',Height).
   

handler_logicmoo_slowcode(Request):- 
 mUST_CALL(save_request_in_session(Request)),!,
  format('Content-type: text/html~n~n'),
  must_run_html(handler_logicmoo_slowcode_m(Request)).

handler_logicmoo_slowcode_m(Request):-
  locally_tl(print_mode(html),
     handler_logicmoo_slowcode_call(Request)),!.

:- thread_local(t_l:where_to/1). 


handler_logicmoo_slowcode_call(Request):- 
  select(search(List),Request,Request0), nonvar(List),
  select(into=Where,List,List0),!,
  must_run((
  format('<html><head>'),
  format('<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script></head><body><pre>'),
  locally_tl(where_to(Where),handler_logicmoo_slowcode_call([search(List0)|Request0])),  
  goal_attach_to(Where,write('<hr/>Complete!')),  
  format('</pre></body></html>'))).


handler_logicmoo_slowcode_call(Request):- 
  member(search(List),Request),
  member(goal=String,List), 
  url_decode_term(String,Goal),!,    
  must_run_html(((xweto(call(Goal))))),!.

handler_logicmoo_slowcode_call(Request):- 
 must_run((
  must_run_html((print_term_to_html_page(handler_logicmoo_slowcode(Request)))))).
   
goal_attach_to(Where,Goal):- 
  with_output_to(string(String),Goal),
  %write(String),
  format('~n<script type="text/JavaScript" language="JavaScript">$(\'#~w\', window.parent.document).append($(`~w`)); </script>',
   [Where,String]),!.
  




%% show_search_filters( ?ARG1) is det.
%
% Show Search Filters.
%
show_search_filters(BR):- 
   must_run_html((
   forall(no_repeats(N=C,search_filter_name_comment(N,C,_)),
            session_checkbox(N,C,BR)))).




%% parameter_names( ?ARG1, ?ARG2) is det.
%
% Parameter Names.
%
parameter_names(List,N):-is_list(List),!,member(E,List),parameter_names(E,N).
parameter_names(V,_):- var(V),!,fail.
parameter_names(N=_,N):-!,atom(N).
parameter_names(C,N):-compound(C),functor(C,N,1).




%% current_form_var( ?ARG1) is det.
%
% Current Form Variable.
%
current_form_var(N):-no_repeats((current_form_var0(N))),atom(N),\+ arg(_,v(peer,idle,ip,session),N).



%% current_form_var0( ?ARG1) is det.
%
% Current Form Variable Primary Helper.
%
current_form_var0(N):- baseKB:param_default_value(N,_).
%current_form_var0(N):- get_http_current_request(B),member(search(Parameters),B),parameter_names(Parameters,N).
%current_form_var0(N):- http_current_session(_, Parameters),parameter_names(Parameters,N).




%% is_goog_bot is det.
%
% If Is A Goog Bot.
%
is_goog_bot:- get_http_current_request(B),member(user_agent(UA),B),!,atom_contains(UA,'Googlebot').
 

%% pp_now is det.
%
% Pretty Print Now.
%
pp_now.




%% this_listing( :TermARG1) is det.
%
% This Listing.
%
this_listing(M:F/A):-functor(H,F,A),predicate_property(M:H,number_of_causes(_)),!, forall(clause(M:H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-functor(H,F,A),predicate_property(H,number_of_causes(_)),!, forall(clause(H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-listing(M:F/A),!.
this_listing(MFA):-listing(MFA).

:- thread_local(sortme_buffer/2).


% i2tml_save(Obj,H):- \+ is_list(H),cyc:pterm_to_sterm(H,S),H\=@=S,!,i2tml_save(Obj,S).




%% pp_i2tml_saved_done( ?ARG1) is det.
%
% Pretty Print I2tml Saved Done.
%
pp_i2tml_saved_done(_Obj):-pp_now,!,flush_output_safe.
pp_i2tml_saved_done(Obj):-
  findall(H,retract(sortme_buffer(Obj,H)),List),predsort(head_functor_sort,List,Set),
  forall(member(S,Set),pp_i2tml(S)),!.




%% find_cl_ref( :TermARG1, ?ARG2) is det.
%
% Find Clause Ref.
%
find_cl_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_cl_ref(clause(_,_,Ref),Ref):-!.
find_cl_ref(clause(H,B),Ref):- clause(H,B,Ref),!.
find_cl_ref((H:-B),Ref):-!, clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!.
find_cl_ref(H,Ref):- clause(H,true,Ref),clause(HH,true,Ref),H=@=HH,!.



:- dynamic('$si$':'$was_imported_kb_content$'/2).


%% find_ref( :TermARG1, ?ARG2) is det.
%
% Find Ref.
%
find_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_ref(H,Ref):- find_cl_ref(H,Ref),!.
find_ref(This,Ref):- call(call,'$si$':'$was_imported_kb_content$'(A,CALL)),
   arg(1,CALL,This),clause('$si$':'$was_imported_kb_content$'(A,CALL),true,Ref),!.
find_ref(M:This,Ref):- atom(M),!,find_ref(This,Ref).




%% head_functor_sort( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Head Functor Sort.
%
head_functor_sort(Result,H1,H2):- (var(H1);var(H2)),compare(Result,H1,H2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,A1),get_functor(H2,F2,A2))),F1==F2,A1>0,A2>0,arg(1,H1,E1),arg(1,H2,E2),compare(Result,E1,E2),Result \== (=),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,_),get_functor(H2,F2,_))),F1\==F2,compare(Result,F1,F2),Result \== (=),!.
head_functor_sort(Result,H1,H2):-compare(Result,H1,H2),!.




%% i2tml_hbr( ?ARG1, ?ARG2, ?ARG3) is det.
%
% I2tml Hbr.
%
i2tml_hbr(H,B,Ref):- nonvar(Ref),!,pp_i2tml_save_seen(clause(H,B,Ref)).
i2tml_hbr(H,B,Ref):- nonvar(H),var(Ref),clause(H,B,Ref),!,pp_i2tml_save_seen(clause(H,B,Ref)).
i2tml_hbr(H,B,_):- B==true,!, pp_i2tml_save_seen(H).
i2tml_hbr(H,B,_):- pp_i2tml_save_seen((H:-B)).




%% pp_i2tml_save_seen( ?ARG1) is det.
%
% Pretty Print I2tml Save Seen.
%
pp_i2tml_save_seen(HB):- pp_now, !,pp_i2tml(HB),!.
pp_i2tml_save_seen(HB):- assertz_if_new(sortme_buffer(_Obj,HB)),!.


:- thread_local(t_l:pp_i2tml_hook/1).

:- thread_local(t_l:tl_hide_data/1).
   
:- thread_local(shown_subtype/1).
:- thread_local(xlw:shown_clause/1).
:- meta_predicate if_html(*,0).






%% section_open( ?ARG1) is det.
%
% Section Open.
%
section_open(Type):-  once(shown_subtype(Type)->true;((is_html_mode->format('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n~n',[Type]))),asserta(shown_subtype(Type)))),!.



%% section_close( ?ARG1) is det.
%
% Section Close.
%
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(is_html_mode->format('</font>\n</pre><hr/><pre>',[]);draw_line));true.


%% pp_item_html( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML.
%
pp_item_html(_Type,H):-var(H),!,write_variable(H).
pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):- xlw:shown_clause(H),!.
pp_item_html(_,P):- is_hidden_pred(P),!.
pp_item_html(Type,H):- \+ is_html_mode, pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,5000,X),pp_item_html_now(Type,H))).

is_hidden_pred(M:P):-!, (is_listing_hidden(M); is_hidden_pred(P)).
is_hidden_pred(P):- (is_listing_hidden(P); (compound(P),functor(P,F,A),(is_listing_hidden((F/A));is_listing_hidden((F))))),!.





%% pp_item_html_now( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML Now.
%
pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(xlw:shown_clause(H)),!.





%% pp_item_html_if_in_range( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML If In Range.
%
pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_i2tml(H),!.

:- thread_local(t_l:last_show_clause_ref/1).
:- thread_local(t_l:current_clause_ref/1).





%% show_clause_ref( ?ARG1) is det.
%
% Show Clause Ref.
%
show_clause_ref(Ref):- Ref == none,!.
show_clause_ref(Ref):- t_l:last_show_clause_ref(Ref),!.
show_clause_ref(Ref):- 
  retractall(t_l:last_show_clause_ref(_)),
  asserta(t_l:last_show_clause_ref(Ref)),
  on_x_debug(show_clause_ref_now(Ref)),!.




%% show_clause_ref_now( :GoalARG1) is det.
%
% Show Clause Ref Now.
%
show_clause_ref_now(_Ref):- is_listing_hidden(hideClauseRef),!.
show_clause_ref_now(V):-var(V),!.
show_clause_ref_now(0):-!.
show_clause_ref_now(none):-!.
show_clause_ref_now(Ref):- is_listing_hidden(showFilenames), \+ clause_property(Ref,predicate(_)),format('~N~p~N',[clref(Ref)]),!.
% write_html(div(class(src_formats),a(href(EditLink), edit)])).
show_clause_ref_now(Ref):- is_listing_hidden(showFilenames),clause_property(Ref,file(File)),ignore(clause_property(Ref,line_count(Line))),
  ignore(clause_property(Ref,module(Module))),
    format('<a href="/swish/filesystem/~w#L~w">@file:~w:~w</a>(~w)~N',[File,Line,File,Line,Module]),
    fail. 
show_clause_ref_now(Ref):- clause_property(Ref,erased),
  ignore(clause_property(Ref,module(Module))),
    format('erased(~w) (~w)~N',[Ref,Module]),!.
show_clause_ref_now(Ref):- nop(writeq(Ref)).

xbformat(X,Y):- format(X,Y).
xbformat(X):- format(X).

%% pp_i2tml( :TermARG1) is det.
%
% Pretty Print I2tml.
%

pp_i2tml(Done):- 
  t_l:where_to(Where),!,
  goal_attach_to(Where,pp_i2tml1(Done)).
pp_i2tml(Done):- pp_i2tml1(Done).

pp_i2tml1(Done):- once(pp_i2tml0(Done)).

pp_i2tml0(Done):-Done==done,!,format('<p>Complete</p>',[]),!.
pp_i2tml0(T):-var(T),!,format('~w~n',[T]),!.
pp_i2tml0(T):-string(T),!,format('"~w"~n',[T]).
pp_i2tml0(clause(H,B,Ref)):- !, locally_tl(current_clause_ref(Ref),pp_i2tml_v((H:-B))).
pp_i2tml0(HB):- find_ref(HB,Ref),!, locally_tl(current_clause_ref(Ref),pp_i2tml_v(HB)).
pp_i2tml0(HB):- locally_tl(current_clause_ref(none),pp_i2tml_v((HB))).




%% numberlist_at( ?ARG1, :TermARG2) is det.
%
% Numberlist When.
%
numberlist_at(_,[]).
numberlist_at(_,[N|More]):- number(N),!,N2 is N+1,numberlist_at(N2,More),!.
numberlist_at(Was,[N|More]):-var(N),  N is Was+1, N2 is N+1,  numberlist_at(N2,More),!.
numberlist_at(Was,[_|More]):- N2 is Was+2, numberlist_at(N2,More),!.




%get_clause_vars_for_print_here(HB,HB2):- catch(get_clause_vars_for_print(HB,HB2),_,fail),!.
get_clause_vars_for_print_here(HB,HB2):- sccs(lock_vars(HB),pretty_numbervars(HB,HB2),unlock_vars(HB)),!.
get_clause_vars_for_print_here(HB,HB2):- HB=HB2.

%% pp_i2tml_v( ?ARG1) is det.
%
% Pretty Print I2tml V.
%
pp_i2tml_v(HB):- must_run_html(catch(( \+ \+ ((get_clause_vars_for_print_here(HB,HB2),pp_i2tml_0(HB2)))),_,true)),!.




%% pp_i2tml_0( :TermARG1) is det.
%
% Pretty Print i2tml  Primary Helper.
%
pp_i2tml_0(Var):-var(Var),!,write_variable(Var).
pp_i2tml_0(USER:HB):-USER==user,!,pp_i2tml_0(HB),!.
pp_i2tml_0((H :- B)):-B==true,!,pp_i2tml_0((H)),!.
pp_i2tml_0(((USER:H) :- B)):-USER==user,!,pp_i2tml_0((H:-B)),!.
pp_i2tml_0((H:-B)):-B==true, !, pp_i2tml_0(H).

pp_i2tml_0(P):- is_listing_hidden(P),!.
pp_i2tml_0(was_chain_rule(H)):- pp_i2tml_0(H).
pp_i2tml_0(M:(H)):-M==user, pp_i2tml_0(H).
pp_i2tml_0(is_edited_clause(H,B,A)):- pp_i2tml_0(proplst([(clause)=H,before=B,after=A])).
pp_i2tml_0(is_disabled_clause(H)):- pp_i2tml_0((disabled)=H).


% pp_i2tml_0(FET):-fully_expand(change(assert,html_gen),FET,NEWFET),FET\=@=NEWFET,!,pp_i2tml_0(NEWFET).
%pp_i2tml_0('$spft'(MZ,P,F,T)):- !, pp_i2tml_0('$spft'(MZ,P,F,T)).
%pp_i2tml_0('$spft'(MZ,P,F,T)):-!, locally_tl(current_why_source(T),pp_i2tml_0('$spft'(MZ,P,F,T))).

pp_i2tml_0('$spft'(MZ,P,U,U)):- nonvar(U),!, pp_i2tml_1(MZ:P:-asserted_by(U)).
pp_i2tml_0('$spft'(MZ,P,F,T)):- atom(F),atom(T),!, pp_i2tml_1(MZ:P:-asserted_in(F:T)).
pp_i2tml_0('$spft'(MZ,P,F,T)):- atom(T),!,  pp_i2tml_1(((MZ:P):-  T:'t-deduced',F)). 
pp_i2tml_0('$spft'(MZ,P,F,T)):- atom(F),!,  pp_i2tml_1(((MZ:P):-  F:'f-deduced',T)). 
pp_i2tml_0('$spft'(MZ,P,F,T)):- !, pp_i2tml_1((MZ:P:- ( 'deduced-from'=F,  (rule_why = T)))).
pp_i2tml_0('$nt'(Trigger,Test,Body)) :- !, pp_i2tml_1(proplst(['n-trigger'=Trigger , format=Test  ,  (body = (Body))])).
pp_i2tml_0('$pt'(_MZ,Trigger,Body)):-      pp_i2tml_1(proplst(['p-trigger'=Trigger , ( body = Body)])).
pp_i2tml_0('$bt'(Trigger,Body)):-      pp_i2tml_1(proplst(['b-trigger'=Trigger ,  ( body = Body)])).

pp_i2tml_0(proplst([N=V|Val])):- is_list(Val),!, pp_i2tml_1(N:-([clause=V|Val])).
pp_i2tml_0(proplst(Val)):-!, pp_i2tml_1(:-(proplst(Val))).


pp_i2tml_0(M:H):- M==user,!,pp_i2tml_1(H).
pp_i2tml_0((M:H:-B)):- M==user,!,pp_i2tml_1((H:-B)).
pp_i2tml_0(HB):-pp_i2tml_1(HB).




%% if_html( ?ARG1, :GoalARG2) is det.
%
% If HTML.
%
if_html(F,A):-is_html_mode,!,format(F,[A]).
if_html(_,A):-A.



%% pp_i2tml_1( ?ARG1) is det.
%
% Pretty Print i2tml  Secondary Helper.
%
pp_i2tml_1(H):-
 must_run_html((
 once(((lmcache:last_item_offered(Was);Was=foobar),get_functor(Was,F1,_A1),get_functor(H,F2,_A2),
   retractall(lmcache:last_item_offered(_Waz)),
   (H\=lmcache:last_item_offered(_) -> asserta(lmcache:last_item_offered(H)) ; true),
    ((F1 \== F2 -> if_html('~N~@<hr/>',true);true)))))),fail.

pp_i2tml_1(_H):- t_l:current_clause_ref(Ref),
   must_run_html((if_html('<font size="1">~@</font>',ignore(show_clause_ref(Ref))))),fail.

pp_i2tml_1(H):- is_html_mode, 
  must_run_html((
  functor_to_color(H,FC),
  term_varnames(H,Vs,_), 
  url_encode_term(H,Vs,URL),
  term_to_pretty_string(H,Vs,Title), into_attribute(Title,TitleQ),
  ignore(fmtimg(FC,Title,URL)),
  format('<input type="checkbox" name="assertion[]" value="~w" title="~w">',[URL,TitleQ]))),
  fail.

pp_i2tml_1(H):- locally_tl(print_mode(html), \+ \+ must_run_html(pp_i2tml_now(H))).




%% pp_i2tml_now( ?ARG1) is det.
%
% Pretty Print I2tml Now.
%
pp_i2tml_now(C):- on_x_fail(t_l:pp_i2tml_hook(C)),!.
pp_i2tml_now(C):- if_html('<font size="3">~@</font>~N',locally_pp_i2tml_now(C)).


locally_pp_i2tml_now(C):-on_x_fail(locally_tl(print_mode(html),ensure_pp(print_tree(C)))),!.
locally_pp_i2tml_now(C):-on_x_fail(rok_portray_clause(C)),!.
locally_pp_i2tml_now(C):-on_x_fail(print_tree(C)),!.
locally_pp_i2tml_now(C):-on_x_fail(portray_clause(C)),!.
locally_pp_i2tml_now(C):-on_x_fail(writeq(C)),!.
%% functor_to_color( ?ARG1, ?ARG2) is det.
%
% Functor Converted To Color.
%

functor_to_color(wid(_,_,G),C):-!,functor_to_color(G,C).
functor_to_color(G,C):-compound(G),functor(G,F,A),functor_to_color(G,F,A,C),!.
functor_to_color(_G,green):-!.





%% functor_to_color( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Functor Converted To Color.
%
functor_to_color(_G,isa,_,bug_btn_s).

functor_to_color(_G,genls,1,'plus-green').
functor_to_color(_G,arity,_,'white').
functor_to_color(_G,argIsa,_,'white').
functor_to_color(_G,argGenls,_,'white').

functor_to_color(_,_,1,yellow).

functor_to_color(G:-_,_,_,C):-nonvar(G),!,functor_to_color(G,C).



functor_to_color(_,(<==>),_,'plus-purple').
functor_to_color(_,(<-),_,purple).
functor_to_color(_,(<=),_,'cyc-right-triangle-violet').
functor_to_color(_,(==>),_,'cyc-right-triangle-violet').
functor_to_color(_,(:-),_,red_diam).


functor_to_color(_,-,_,red).
functor_to_color(_,not,_,red).
functor_to_color(_,~,_,red).
functor_to_color(_,~,_,red).

functor_to_color(_,(if),_,cy_menu).
functor_to_color(_,(iff),_,cyan).
functor_to_color(_,(all),_,cyan).
functor_to_color(_,(exists),_,blue).

functor_to_color(_,(mudEquals),_,pink).
functor_to_color(_,(skolem),_,pink).
functor_to_color(_,(wid),_,green_yellow).

functor_to_color(G,_,_,'lightgrey'):-predicate_property(G,foreign).
functor_to_color(G,_,_,'cyc-logo-3-t'):-predicate_property(G,built_in).




%% session_checked( ?ARG1) is det.
%
% Session Checked.
%
session_checked(Name):- (get_param_sess(Name,V);baseKB:param_default_value(Name,V)),!,V\=='0',V\==0,V\=="0".




%% session_checkbox( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Session Checkbox.
%
session_checkbox(Name,Caption,BR):-session_checkbox(Name,Caption,BR,session_checked(Name)).

session_checkbox(Name,Caption,BR,Call):-
 (Call-> CHECKED='CHECKED';CHECKED=''),
 format('<input type="checkbox" name="~w" value="1" ~w />~w~w',[Name,CHECKED,Caption,BR]).
 % format('<font size="-3"><label><input type="checkbox" name="~w" value="1" ~w/>~w</label></font>~w',[Name,CHECKED,Caption,BR]).




%% action_menu_applied( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Action Menu Applied.
%
action_menu_applied(MenuName,ItemName,Where):-
  inline_html_format(['<label>',show_select2(MenuName,xaction_menu_item,[atom_subst('$item',ItemName)]),
      '&nbsp;',Where,'&nbsp;&nbsp;<input type="submit" value="Now" name="Apply"></label>']).

action_menu_applied_here(MenuName,ItemName,Where):-
  inline_html_format(['<label>',show_select2(MenuName,xaction_menu_item,[atom_subst('$item',ItemName)]),
      '&nbsp;',Where,'&nbsp;&nbsp;<button type="button" value="Now" name="Apply"/></label>']).

%% is_context( ?ARG1, ?ARG2) is det.
%
% If Is A Context.
%
is_context(MT,MT):-no_repeats(is_context0(MT)).



%% is_context0( ?ARG1) is det.
%
% If Is A Context Primary Helper.
%
is_context0(MT):- if_defined(exactlyAssertedEL_first(isa, MT, 'tMicrotheory',_,_),fail).
is_context0(MT):- if_defined(isa(MT,'tMicrotheory'),fail).
is_context0('BaseKB').                           






%% get_request_vars( ?ARG1) is det.
%
% Get Request Variables.
%
get_request_vars(Format):- ignore(Exclude=[term,fa,session_data,cmd,user_agent,referer,session,request_uri,accept]),
   findall(N=V,(current_form_var(N),\+ member(N,Exclude),once(get_param_sess(N,V))),NVs),
   forall(member(N=V,NVs),format(Format,[N,V])).


%% must_run( :GoalARG1) is det.
%
% Hmust (list Version).
%

% must_run(G):- fast_and_mean, !, call(G).

%must_run(Goal)
must_run_html(Goal):- 
 wots(S,(toplevel_pp(bfly)-> bfly_html_goal(must_run(Goal)); must_run(Goal))),
 our_pengine_output(S).

must_run(List):- is_list(List),!,maplist(must_run0,List).
must_run(Goal):- must_run0(Goal).

must_run0(Goal):- tracing,!, 
  ((Goal) *-> true ; wdmsg(assertion_failed(fail, Goal))),
  flush_output_safe.
must_run0((G1,G2)):- !, call_cleanup(must_run0(G1),must_run0(G2)),!.
%must_run0(Goal):- ignore(catch(no_undefined_preds(Goal),_,true)),!.
must_run0(Goal):- flush_output_safe, 
  (catch(mUST_CALL(no_undefined_preds(mUST_CALL(Goal))),E,(wdmsg(E),www_dumpST,wdmsg(E=Goal),fail)) -> true ; 
    wdmsg(nop(assertion_failed(fail, Goal)))),
  flush_output_safe.

no_undefined_preds(G):- locally(set_prolog_flag(unknown,fail),G),!.
no_undefined_preds(G):- call(G).


:- multifile(prolog_debug:assertion/1).
:- abolish(prolog_debug:assertion/1).
:- asserta(prolog_debug:assertion(_)).

%% call_for_terms( ?ARG1) is det.
%
% Call For Terms.
%
call_for_terms(Call):- 
   must_run_html((       
      with_pre(ignore((locally_tl(print_mode(html),with_search_filters(catch(ignore(with_no_x_http(Call)),E,dmsg(E))))))),
        show_pcall_footer)),!.

:- thread_local(t_l:tl_hide_data/1).



%% with_search_filters( :GoalARG1) is det.
%
% Using Search Filters.
%


with_search_filters(C):-
  retractall(t_l:tl_hide_data(_)),
  with_search_filters0(C),!.
with_search_filters0(C):-
   search_filter_name_comment(FILTER,_,_),
   session_checked(FILTER), 
   \+ t_l:tl_hide_data(FILTER),!,
    locally_tl(tl_hide_data(FILTER),with_search_filters0(C)).
with_search_filters0(C):- must_run_html(C).


call_with_time_limit_notrace(_,Goal):- (thread_self(main);pengines:pengine_self(_)), !, with_no_x_http(Goal).
%call_with_time_limit_notrace(Time,Goal):- call_with_time_limit(Time,Goal).
call_with_time_limit_notrace(_,Goal):- with_no_x_http(Goal).



%% xlisting_html( ?ARG1) is det.
%
% Make Page Pretext Obj.
%
xlisting_html(Obj):- empty_str(Obj),!,write(done),pp_i2tml_saved_done(Obj).
xlisting_html(Obj):- slow_frame(xlisting_html_c(Obj)).

xlisting_html_c(Obj):- xlisting_html_c(0,Obj).

xlisting_html_c(_,Obj):- empty_str(Obj),!,write(done),pp_i2tml_saved_done(Obj).
xlisting_html_c(0,Obj):- !, xlisting_inner(i2tml_hbr_trace,Obj,[]).
xlisting_html_c(T,Obj):- 
  catch(
    call_with_time_limit_notrace(T,xlisting_html_c(0,Obj)),
     TimeOut,
     format('~q.',timeout_xlisting_html_obj(Obj,TimeOut))),
     pp_i2tml_saved_done(Obj),
     write('complete...\n').


fast_and_mean:- true.

try_or_rtrace(G):- tracing,!,dmsg(try(G)),call(G).
try_or_rtrace(G):- fast_and_mean, !, with_no_x_http(G).
try_or_rtrace(G):- catch(G,E,(E==time_limit_exceeded->throw(time_limit_exceeded);(ignore((dmsg(G=E),www_dumpST,dmsg(G=E),thread_self(main),rtrace(G),www_dumpST,dmsg(G=E),break))))).

www_dumpST:- !.
www_dumpST:- with_output_to(user_error,dumpST),write_expandable(false,(write('<pre>'),dumpST,write('</pre>'))).
% :- prolog_xref:assert_default_options(register_called(all)).

%i2tml_hbr_trace(H,B,R):- rtrace(i2tml_hbr(H,B,R)).
i2tml_hbr_trace(H,B,R):- try_or_rtrace(i2tml_hbr(H,B,R)).


%% reply_object_sub_page( ?ARG1) is det.
%
% Reply Object Sub Page.
%
reply_object_sub_page(Obj) :- phrase(object_sub_page(Obj, []), HTML), html_write:print_html(HTML),!.


%%  object_sub_page(+ Obj, + Options)// is det.
%
% -->.
%
object_sub_page(Obj, Options) -->
	{ pldoc_process:doc_comment(Obj, File:_Line, _Summary, _Comment)
	}, !,
	(   { \+ ( pldoc_process:doc_comment(Obj, File2:_, _, _),
		   File2 \== File )
	    }
	->  html([ \object_synopsis(Obj, []),
		   \objects([Obj], Options)
		 ])
	;   html([
		   \objects([Obj], [synopsis(true)|Options])
		 ])
	).













%% return_to_pos( :GoalARG1) is det.
%
% Return Converted To Pos.
%
return_to_pos(Call):- sccs(current_line_position(LP),Call,set_line_pos(LP)),!.



%% nl_same_pos is det.
%
% Nl Same Pos.
%
nl_same_pos:-return_to_pos(nl).






%% set_line_pos( ?ARG1) is det.
%
% Set Line Pos.
%
set_line_pos(LP):-current_output(Out),set_line_pos(Out,LP).



%% set_line_pos( ?ARG1, ?ARG2) is det.
%
% Set Line Pos.
%
set_line_pos(_,_):-!.
set_line_pos(Out,LP):- 
  current_line_position(Out,CLP), 
  (CLP==LP->! ;((CLP>LP->nl(Out);put_code(Out,32)),!,set_line_pos(Out,LP))).




%% current_line_position( ?ARG1) is det.
%
% Current Line Position.
%
current_line_position(LP):-current_output(Out),current_line_position(Out,LP).

:- kb_shared(baseKB:wid/3).
:- asserta((baseKB:wid(_,_,_):-fail)).

%% current_line_position( ?ARG1, ?ARG2) is det.
%
% Current Line Position.
%
current_line_position(Out,LP):-stream_property(Out,position( Y)),stream_position_data(line_position,Y,LP),!.

a_ok(_LP):-writeln([1,2,3,4]).

%% test_tmw is det.
%
% Tmw.
%
test_tmw:- locally_tl(print_mode(html),test_tmw2).

test_tmw2:- 
 rok_portray_clause(a_ok(LP)), 
 rok_portray_clause((a_ok(LP):-writeln([1,2,3,4]))),
 print((a_ok(_LP):-writeln([1,2,3,4]))),
  nl,nl,
    wid_kif(KIF),
 KIF='=>'(_,_),nl,nl,
 print(KIF),
 listing(print_request/1),!.


wid_kif(KIF):- call_u(wid(_,_,KIF)),!.
wid_kif(_).


cccc:- ignore(retract((swish_html_output:make_safe_html(Module:HTML0,
                                 M,
                                 Module:HTML) :-
    !,
    (   Module==M
    ->  true
    ;   permission_error(cross_module_call, M, Module:HTML)
    ),
    make_safe_html(HTML0, M, HTML)))).


%html(Spec, A, B):- swish_html_output:html(Spec, A, B), !.
%html(Spec, A, B):- html_write:html(Spec, A, B).
  






test_url_encode(AA):-
  mUST_CALL((url_encode(AA,X),url_decode_term(X,A),A==AA)),fail.

test_url_encode:- current_output(A),test_url_encode(A).
test_url_encode:- clause(a_ok(_),_,R),test_url_encode(R).
test_url_encode:- test_url_encode([]).
test_url_encode:- test_url_encode('').
test_url_encode:- test_url_encode("").
test_url_encode:- test_url_encode("%20").
test_url_encode:- test_url_encode('%20').
test_url_encode:- test_url_encode('\'').
test_url_encode:- test_url_encode('""').
test_url_encode:- !.



%% url_encode_term( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Url Encode Term.
%

cant_encode(B):- compound(B),!, arg(_,B,E),cant_encode(E).
cant_encode(B):- (var(B);atom(B);string(B);number(B);B==[]),!,fail.
cant_encode(_).







%% name_the_var( ?ARG1, ?ARG2, :TermARG3, :TermARG4, :TermARG5) is det.
%
% Name The Variable.
%
name_the_var(_Num,Vs,[],Vs,[]).

name_the_var(Num,Vs,[VIn|More],VsOut,[N=V|Added]):- member_open(N=V,Vs),VIn==V,!,name_the_var(Num,Vs,More,VsOut,Added).
% name_the_var(Num,Vs,[VIn|More],VsOut,[N=VIn|Added]):- \+ is_list(Vs), append(Vs,[N=VIn],NewVs),!, name_the_var(Num,NewVs,More,VsOut,Added).
name_the_var(Num,Vs,[VIn|More],[N=VIn|VsOut],[N=VIn|Added]):- Num2 is Num +1, NV = '$VAR'(Num),
  with_output_to(atom(N),write_term(NV,[portrayed(true),quoted,priority(9)])),
  name_the_var(Num2,Vs,More,VsOut,Added).



%% url_encode( ?ARG1, ?ARG2) is det.
%
% Url Encode.
%
url_encode(B,A):- atom(B),!,url_iri(A,B).
url_encode(B,A):- term_varnames(B,Vars,Un),url_encode_term(B,Vars,Un,O),!,O=A.
/*
url_encode(B,A):- \+ atom(B),!,term_variables(B,Vars),url_encode_term(B,Vars,O),O=A.
url_encode(B,A):- atom_concat('\n',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,'\n',B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(' ',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,' ',B),!,url_encode(BT,A).
*/

url_encode_term(B,Vs,O):- url_encode_term(B,Vs,_Un,O).

url_encode_term(B,Vs,_Un,O):- cant_encode(B),!, gensym('cant_encode_',Sym), recorda(Sym,f(B:Vs)),!,O=Sym.

url_encode_term(B,[],_Un,O):- !, term_to_atom('f'(B:[]),BB),!,url_iri(O,BB).
url_encode_term(InTerm,_VsIn,_Un,URL):- fail, with_output_to(atom(IRI),portray_clause('f'((InTerm:_)))),
  url_iri(URL,IRI),nb_linkval(URL,InTerm),!.
  %url_encode_term(B,[],[],O):- !, sformat(BB,'~q',[B]),!,url_iri(O,BB).
%url_encode_term(B,[],[],O):- !, term_to_atom(B,BB),!,url_iri(O,BB).
url_encode_term(B,Vs,_Un,O):- !, term_to_atom('f'(B:Vs),BB),!,url_iri(O,BB).


url_encode_term(InTerm,VsIn,_Un,URL):-
  get_varname_list(Prev),
  name_the_var(40,Prev,VsIn,_NewVs,Added),
  % (NewVs\==Prev ->  show_call(why,put_variable_names(NewVs)) ; true),
  with_output_to(atom(IRI),write_term('f'(InTerm:Added),[quoted(true),variable_names(Added),quoted,priority(9)])),
  url_iri(URL,IRI),!.

%% url_decode_term( ?ARG1, ?ARG2) is det.
%
% Url Decode.
%
%url_decode_term(Sym,T):- \+ atom(Sym),Sym=T.
%url_decode_term(Sym,T):- atom_concat('#%24%28',_,Sym) , url_decode_term(Sym,T,_),!,T=T.

url_decode_term0(Sym,Term):- url_decode_term(Sym,Term,Vs), (sensical_term(Term); Vs\==[]), !,add_to_env_here(Vs).
url_decode_term0(Sym,Sym).

url_decode_term(S,O):- url_decode_term0(S,M), any_to_atom_term(M,[],O,_Vs),!.

un_inner_quote(AtomIn,AtomOut):- atom(AtomIn),atom_concat('\'',L,AtomIn),atom_concat(AtomOut,'\'',L),!.
%any_to_atom_term(Term,Vs,Term,Vs):-!.
any_to_atom_term(Atom,_,Term,Vs):- fail, un_inner_quote(Atom,R), atom_to_term_safe(R,Term,Vs),!.
any_to_atom_term(Atom,_,Term,Vs):- atom_to_term_safe(Atom,Term,Vs),(sensical_term(Term); Vs\==[]),!.
any_to_atom_term(Term,Vs,Term,Vs).

atom_to_term_safe(Key,Prolog,Vs):- atom(Key),notrace(catch(atom_to_term(Key,Prolog,Vs),_,fail)), sensical_term(Prolog).

%% sensical_term( ?ARG1) is det.
%
% Sensical Nonvar.
%
sensical_term(O):-nonvar(O), O\==[], O\=='', O \= (_ - _), O\==end_of_file.


%% url_decode_term( ?ARG1, ?ARG2) is det.
%
% Url Decode Term.
%
url_decode_term(Sym,T,Vs):- var(Sym),!, T=Sym, term_varnames(T,Vs,_),!.
url_decode_term(Sym,T,Vs):- empty_str(Sym),!, T=Sym, Vs=[].

url_decode_term(Sym,T,Vs):- url_decode_term0(Sym,TMid,VsMid), 
  any_to_atom_term(TMid,VsMid,T,Vs).

url_decode_term0(Sym,T,Vs):- atom(Sym) -> url_decode_atom(Sym,T,Vs) ; url_decode_nonatom(Sym,T,Vs).

url_decode_nonatom(Sym,T,Vs):- compound(Sym), decode_f1(Sym,T,Vs), !.
url_decode_nonatom(Sym,T,Vs):- blob(Sym,clause),!, clause(H,B,Sym), !, T = (H:-B), term_varnames(T,Vs,_),!.
url_decode_nonatom(Sym,T,Vs):- blob(Sym,record),!, recorded(_,R,Sym), !, url_decode_term(R,T,Vs).
url_decode_nonatom(Sym,T,Vs):- T=Sym, term_varnames(T,Vs,_),!.

url_decode_atom(Sym,T,Vs):- Sym=='~w', !, T="", Vs = [].
url_decode_atom(Sym,T,Vs):- recorded(Sym,f(M),R),!, ignore(catch(erase(R),_,fail)), decode_f1(f(M),T,Vs).
url_decode_atom(Sym,T,Vs):- nb_current(Sym,T), ignore(catch(nb_delete(Sym),_,fail)),!, term_varnames(T,Vs,_),!.
url_decode_atom(Sym,T,Vs):- url_iri(Sym,Decode), Sym\==Decode, url_decode_atom(Decode,T,Vs), !.
url_decode_atom(Sym,T,Vs):- rtfa_decode(Sym,T,Vs), !.
url_decode_atom(Sym,T,Vs):- T=Sym, Vs=[].

decode_f1('f'(T:Vs),T,Vs):- is_list(Vs), !.
decode_f1('f'(M:F1),M:T,Vs):- nonvar(F1), !, decode_f1('f'(F1),T,Vs).
 
%sorted_term_variables(Set,Vs):- term_variables(Set,VsU),sort(VsU,Vs).
%same_varsets(Set1,Set2):- sorted_term_variables(Set1,Vs1),sorted_term_variables(Set2,Vs2),Vs1==Vs2.
rtfa_decode(Sym,T,Vs):- notrace(catch(atom_to_term(Sym,f(H:F1),_),_,fail)),!,nonvar(F1),decode_f1(f(H:F1),T,Vs),!.  
rtfa_decode(Sym,T,Vs):- atom_to_term_safe(Sym,T,Vs),!.

add_to_env_here(A):- \+ compound(A).
add_to_env_here([H|T]):-
  add_to_env_here(H),
  add_to_env_here(T).
add_to_env_here(N=V):- get_var_by_name(N,VV),VV=V.
add_to_env_here(N=V):- debug_var(N,V).
%add_to_env_here(N=V):- add_var_to_env(N,V).


/*
url_decode_term(A,T,Vs2):-
    url_iri(A,B),
    rtfa_decode(B,T,Vs2,Vs3),
    ignore(Vs2=[]),ignore(Vs2=Vs3),
    merge_key_vals(B,Vs2,Merge),
    get_varname_list(Env),
    merge_key_vals(Env,Merge,New),
    put_variable_names(New),!.
*/




%% tovl( :TermARG1, :TermARG2, :TermARG3) is det.
%
% Tovl.
%
tovl([],[],[]).
tovl([K|KL],[V|VL],[K=V|KVL]) :- tovl(KL, VL, KVL).




%% merge_key_vals( :TermARG1, ?ARG2, ?ARG3) is det.
%
% Merge Key Vals.
%
merge_key_vals(Prev,Pairs,NewSave):-var(Prev),!,NewSave=Pairs.
merge_key_vals([],Pairs,NewSave):-!,NewSave=Pairs.
merge_key_vals([K=V1|Prev],Pairs,NewSave):-
   member_open(K=V2,Pairs),
   V1==V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   member_open(K2=V2,Pairs),
   K1==K2, V1=V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   merge_key_vals(Prev,[K1=V1|Pairs],NewSave).





% x(Z+B)

%   b_setval(URL,InTerm).




%% write_as_url_encoded( ?ARG1, ?ARG2) is det.
%
% Write Converted To Url Encoded.
%
write_as_url_encoded(_Arg, D):- rok_linkable(D), write_atom_link(D).
write_as_url_encoded(_Arg, D):- url_encode(D,U),!,write(U).
:- format_predicate('u',write_as_url_encoded(_Arg,_Time)).




%% term_to_pretty_string( ?ARG1, ?Vs, ?ARG2) is det.
%
% Term Converted To Pretty String.
%
term_to_pretty_string(H,Vs,HS):- with_output_to(string(HS),print_pretty_string(H,Vs)).

print_pretty_string(H,_):- \+ compound(H),!,write(H).
print_pretty_string(H,Vs):- into_textarea(print_tree(H,[variable_names(Vs),right_margin(40)])).

:- export(into_textarea/1).
:- meta_predicate(into_textarea(0)).
into_textarea(G):- with_pp(plain,G).

:- export(with_http/1).
:- meta_predicate(with_http(0)).
with_http(Goal):- !, call(Goal).
with_http(Goal):- with_pp(http,with_no_x_http(Goal)).

%with_no_x_http(Goal):- make_sessionwith_no_xdbg(Goal),!.
with_no_x_http(Goal):- call(Goal).

%% fmtimg( ?ARG1, ?ARG2) is det.
%
% Fmtimg.
%
fmtimg(N,Title,TermE):- is_html_mode,!,
 into_attribute(Title,TitleQ),
 format('~N<a href="?cmd=edit1term&term=~w" target="lm_xref"><img src="/swish/lm_xref/pixmapx/~w.gif" alt="~w" title="~w"><a>',[TermE,N,TitleQ,TitleQ]).
fmtimg(_,_,_).





%% indent_nbsp( ?ARG1) is det.
%
% Indent Nbsp.
%
indent_nbsp(X):-is_html_mode,forall(between(0,X,_),format('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format(' ')),!.




%% indent_nl is det.
%
% Indent Nl.
%
indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).





%% indent_nbsp( :PRED1ARG1, ?ARG2) is det.
%
% Indent Nbsp.
%
indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.






%% shared_hide_data( :PRED4ARG1) is det.
%
% Hook To [logicmoo_util_term_listing:shared_hide_data/1] For Module Mpred_www.
% Shared Hide Data.
%

:- xlisting_web:import(xlisting:is_listing_hidden/1).

shared_hide_data_sp(Var):- is_ftVar(Var),!,fail.
shared_hide_data_sp(_:F/A):- !,shared_hide_data_sp(F/A).
shared_hide_data_sp('$si$':'$was_imported_kb_content$'/2):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp('$spft'/4):- !,is_listing_hidden(hideTriggers).
shared_hide_data_sp('$nt'/3):- !,is_listing_hidden(hideTriggers).
shared_hide_data_sp('$pt'/3):- !, is_listing_hidden(hideTriggers).
shared_hide_data_sp('$bt'/2):- !, is_listing_hidden(hideTriggers).
shared_hide_data_sp((_:-
 
        second_order(_,G19865),
        (   _G19865 = (G19867,!,G19871) ->
                call(G19867),  !,
                call(G19871)
        ;   CALL
        ))):- CALL=@=call(G19865).


shared_hide_data_sp(saved_request/_):- !.
shared_hide_data_sp(session_data/_):- !.
shared_hide_data_sp(mpred_prop/3):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp(last_item_offered/1):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp(P0):- strip_module(P0,_,P), compound(P),functor(P,F,A),F\== (/) , !,shared_hide_data_sp(F/A).
shared_hide_data_sp((Pred)) :-  fail, rok_portray_clause((Pred:-true)).


:- multifile baseKB:shared_hide_data/1.
:- kb_global(baseKB:shared_hide_data/1).
baseKB:shared_hide_data(MFA):- nonvar(MFA), shared_hide_data_sp(MFA).



%:- nb_setval(defaultAssertMt,xlisting_web).



xlisting_web_file.

          
%:- baseKB:import(xlisting_web:web_prover_name/2).

write_non_pre(G):- must_run_html(sccs(write('\n</pre>'),once(G),write('<pre>\n'))).
write_pre(G):- must_run_html(sccs(write('<pre>'),once(G),write('</pre>'))).



rok_style(_):- fail.




:- discontiguous rok_portray/1. 
:- export(rok_portray/1).

/*

% '$messages':rok_portray(X):-fail,loop_check(rok_portray(X)).
rok_portray(A) :- var(A),!,fail,writeq(A).
rok_portray(A) :-
        atom(A),
        sub_atom(A, 0, _, _, 'http://'), !,
        (   rok_style(B)
        ->  true
        ;   B=prefix:id
        ),
        portray_url(B, A).
rok_portray(A) :-
        atom(A),
        sub_atom(A, 0, _, _, 'https://'), !,
        (   rok_style(B)
        ->  true
        ;   B=prefix:id
        ),
        portray_url(B, A).
rok_portray(A) :-
        atom(A),
        atom_concat('__file://', B, A),
        sub_atom(B, D, _, C, #),
        sub_atom(B, _, C, 0, G),
        sub_atom(B, 0, D, _, E),
        file_base_name(E, F),
        format('__~w#~w', [F, G]).
*/
rok_portray(A) :- rok_linkable(A),!,write_atom_link(A,A).
rok_portray(A) :- \+compound(A),fail.
rok_portray(P):- return_to_pos(rok_portray_clause(P)),!.

max_depth_goal(Max,Var,Goal):- 
 flag(Var,N,N), N =< Max, 
  sccs(flag(Var,N,N+1),(Goal),flag(Var,_,N)).

:- fixup_exports.


:- multifile(user:portray/1).
:- dynamic(user:portray/1).
%user:portray(X):- notrace((X\==[], \+ toplevel_pp(bfly), get_print_mode(html), ground(X))), max_depth_goal(0,'$rok_portray',rok_portray(X)).


%:- noguitracer.
% WANT 

:- set_prolog_flag(hide_xpce_library_directory,false).
:- retract(t_l:no_cycstrings).

%:- add_import_module(baseKB,xlisting_web,end).

%:- during_net_boot(register_logicmoo_browser).
%:- set_fileAssertMt(baseKB).

% :- use_module(xlisting_web_server).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).


/*

Do you say* then that LMs taken to the extreme fundamentally cannot achieve AGI?
[11:41 AM] Utilop: You say that logicmoo outperforms LMs in QA. We recently did a QA system for open-ended customer question, achieving a near 100 % accuracy. How would the system - without humans having to tell it anything about these concepts - answer questions like, "Can I use Jabra Elite 75t earbuds when swimming?" (we did not tell it)
[11:48 AM] dmiles: It depends on whether or not a LM can update it's short and long term information .. that is if it can learn to correct for its mistakes in a short interaction
[11:49 AM] dmiles: lets say that some consumer reports end up invalidating the use of those earbuds underwater.. if the model is designed that it will change it's answer
[11:49 AM] dmiles: (if it can be designed to)
[11:50 AM] dmiles: (and if not, if there is an inkling of what roadmap would be to add that ability)
[11:54 AM] dmiles: in "a short interaction" is probably the key stumbling block for language models?
[11:58 AM] dmiles: With the  cyc/logicmoo way .. when we have access to the same information as the language model did when training, answering such questions would not be a problem
[12:01 PM] dmiles: identifying and changing that information is much easier to when it is wrong (language model obfuscates the storage a bit too much i think for the system to update itself)
[12:05 PM] dmiles: note, the cyc way, even if it does beat LMs at Q&A, still doesnt go towards what they would need for AGI
[12:09 PM] dmiles: what is needed for AGI is a type of process that models a handful of concurrent informationally in-compatible hill-climbs (simplistic even) and then maps a single non-simplistic hill-climb process and a system to keep cohesion between that one and the set.
[12:11 PM] dmiles: What Cyc and LMs do is models the hills but without modeling the climbing process
[12:13 PM] dmiles: The LM stores multiple final results of climbs and hopes the next usecase will be a search to find the right climbed hill.. the best-case is it will have hopefully blended a climb together
[12:16 PM] dmiles: Cyc's problem (the gold standard of GOFAI and mostly Logicmoo's) is that sometimes blending is so rigid .. the blending falls short
[12:21 PM] dmiles: to avoid Cyc's rigidness, Logicmoo try's to reverse engineer enough climbed hills that the process can be recreated each time and hopefully a different set of rules get made
[12:23 PM] dmiles: comes down to how a small change can be and still be important to compete with all the knowledge each system had previously acquired
[12:25 PM] dmiles: In LMs we try to "prime" the model for instance finding the right customer data to be part of the query and hope the LM will find a matching hill-climb that "completes" the customer's thought.. next we try to continue.. hoping the system and the customer is on the same path.  Right? (that is how one of the products I worked on professionally uses LMs .. the problem was the language model will never be big enough to really stay on a customer's path.   The next problem is if the customer nuances the path in too subtle of ways the system wont pick up on that.  Especially a problem due the LM was never designed to "act on it" even if it subtle or not)
[12:36 PM] dmiles: So actually I am admitting to the that fact that neither approach , my method (a copy of Cyc's) of RTE/Q&A or LM's will actualyl get us anywhere towards the process of AGI which is  a type of process that models a handful of concurrent informationally incompatible hill-climbs (simplistic even) and then maps a single non-simplistic hill-climb process and a system to keep cohesion between that one and the set.  since both avoided the qualia of the climbing process.     Though since logicmoo realizes this early, the process of reproduction of that qualia is what logicmoo hopefully can scale on..  Interestingly I do find that LM does help fix the scaling in that it can produce content that otherwise would never been obvious.. for instance I can convert a "scene" to english and have the LM help adorn the scene in ways that logicmoo didnt have the logical reasoning to have guessed
[12:45 PM] dmiles: an example of that: imagine a SHRDLU like scene/scenario where the user gives it a story problem of blocks and pyramids on a table and starts to ask it to change the objects and ask it questions..  the LM can be queried to harvest out new sort of unrealized physics that the KB could have been missing..  even though the LM could never help actually simulate or answer questions about the state it was in
[12:50 PM] dmiles: The underlying problem was that customers needed a program that will help simulate life scenarios with them .. and LMs just couldn't update on micro-levels at the rapid rate in which is needed.. Worse yet, an AGI needs even a more rapid updating system than a customer would for it's internal imaginary states in which it build ideas from
[12:50 PM] dmiles: Also, on the commercial product that uses LMs (that i am helping with) we would (to upgrade the system) have to still run all of its input and output thru the Cyc system to vet its wild hairs (protect the customers by helping the system not meander so far off the customer's path).. So far we used Cyc add to the customers "prime"-ing we send to the LM process.  Although it has vastly improved the LM (by magnitudes by helping the LM hone the context),  it still is not enough for the simulation the customer needs.  You find that when the LM is missing content.. nothing you do in priming will ever be enough to help
*/
