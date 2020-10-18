:- if(\+ current_module(sxpr_reader)).
:- module(s3xpr,[
  codelist_to_forms/2,
  svar_fixvarname/2,
  with_kifvars/1,
  current_input_to_forms/2,
  input_to_forms/2,
  input_to_forms/3,
  input_to_forms_debug/1,
  input_to_forms_debug/2,
  sexpr_sterm_to_pterm_list/3,
  sexpr//1,
  fixvars/4,
  txt_to_codes/2,
  with_lisp_translation/2,
  to_untyped/2,
  ok_var_name/1,
  svar_fixvarname/2,
  sexpr_sterm_to_pterm/2,
  lisp_read/2,
  phrase_from_stream_nd/2,
  write_trans/4,
  parse_sexpr/2]).

:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo_common)).
:- use_module(library(backcomp)).
:- use_module(library(rbtrees)).

%:- meta_predicate always_b(//,?,?).
%:- meta_predicate bx(0).
:- meta_predicate call_proc(1,?).
:- meta_predicate dcg_and2(//,//,?,?).
:- meta_predicate dcg_each_call_cleanup(0,//,0,?,?).
:- meta_predicate dcg_not(//,?,?).
:- meta_predicate dcg_phrase(//,?,?).
:- meta_predicate dcg_xor(//,//,?,?).
%:- meta_predicate expr_with_text(*,2,*,*,*).

:- meta_predicate remove_optional_char(//,?,?).

:- meta_predicate sexpr_vector0(*,//,?,?).
:- meta_predicate with_all_rest_info(1).
:- meta_predicate with_lisp_translation_stream(*,1).
:- meta_predicate write_trans(+,*,2,?).


def_is_characterp(CH):- current_predicate(is_characterp/1),!,call(call,is_characterp,CH).
def_is_characterp_def('#\\'(_)).

def_to_prolog_string(I,O):- current_predicate(to_prolog_string/2),!,call(call,to_prolog_string,I,O).
def_to_prolog_string(I,O):- any_to_string(I,O).


def_compile_all(I,O):- current_predicate(compile_all/2),!,call(call,compile_all,I,O).
def_compile_all(I,O):- wdmsg(undefined_compile_all(I)),I=O.


%:- meta_predicate(always(0)).
%always(G):- must(G).

:- use_module(library(logicmoo/filestreams)).
%:- use_module(library(bugger)).

:- if(exists_file('./header')).
% :- include('./header').
:- endif.
%:- use_module(eightball).

:- thread_local(t_l:sreader_options/2).
kif_ok:- t_l:sreader_options(logicmoo_read_kif,true).


:- meta_predicate((with_lisp_translation(+,1),input_to_forms_debug(+,:))).
:- meta_predicate sexpr_vector(*,//,
 ?,?).


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- thread_local(t_l:s_reader_info/1).

:- meta_predicate(quietly_sreader(0)).
quietly_sreader(G):- !, call(G).
quietly_sreader(G):- quietly(G).

%% with_lisp_translation( +FileOrStream, :Pred1) is det.
%
% With File or Stream read all S-expressions submitting each to Pred1
%
with_lisp_translation(In,Pred1):-
   is_stream(In),!,with_lisp_translation_stream(In,Pred1).
with_lisp_translation(Other,Pred1):- 
   setup_call_cleanup(l_open_input(Other,In),
     with_lisp_translation_stream(In,Pred1),
     ignore(notrace_catch_fail(close(In)))),!.

with_lisp_translation_stream(In,Pred1):- 
   repeat,
      once(lisp_read(In,O)),
      (O== end_of_file 
         -> (with_all_rest_info(Pred1),!) ; 
         (((once((zalwayz(call_proc(Pred1,O))))),fail))).

call_proc(Pred1,O):- call(Pred1,O),!,with_all_rest_info(Pred1),!.

with_all_rest_info(Pred1):- 
 forall(clause(t_l:s_reader_info(O2),_,Ref),
  (zalwayz(once(call(Pred1,O2))),erase(Ref))),!.

parse_sexpr_untyped(I,O):- quietly_sreader((parse_sexpr(I,M))),!,quietly_sreader((to_untyped(M,O))),!.

read_pending_whitespace(In):- repeat, peek_char(In,Code),
   (( \+ char_type(Code,space), \+ char_type(Code,white))-> ! ; (get_char(In,_),fail)).


make_tmpfile_name(Name,Temp):- 
  atomic_list_concat(List1,'/',Name),atomic_list_concat(List1,'_',Temp1),
  atomic_list_concat(List2,'.',Temp1),atomic_list_concat(List2,'_',Temp2),
  atomic_list_concat(List3,'\\',Temp2),atomic_list_concat(List3,'_',Temp3),
  atom_concat_or_rtrace(Temp3,'.tmp',Temp),!.
  



:- meta_predicate(with_lisp_translation_cached(:,2,1)).
:- meta_predicate(maybe_cache_lisp_translation(+,+,2)).

with_lisp_translation_cached(M:LFile,WithPart2,WithPart1):- 
   absolute_file_name(LFile,File),
   make_tmpfile_name(LFile,Temp),
   maybe_cache_lisp_translation(File,Temp,WithPart2),!,
   finish_lisp_translation_cached(M,File,Temp,WithPart1).

finish_lisp_translation_cached(M,File,Temp,WithPart1):-
   multifile(M:lisp_trans/2),
   dynamic(M:lisp_trans/2),
   file_base_name(File,BaseName),
   M:load_files([Temp],[qcompile(auto)]),
   forall(M:lisp_trans(Part2,BaseName:Line),
   once((b_setval('$lisp_translation_line',Line),
         zalwayz(M:call(WithPart1,Part2))))).
  
maybe_cache_lisp_translation(File,Temp,_):- \+ file_needs_rebuilt(Temp,File),!.
maybe_cache_lisp_translation(File,Temp,WithPart2):- 
 file_base_name(File,BaseName),
 setup_call_cleanup(open(Temp,write,Outs),
  must_det((format(Outs,'~N~q.~n',[:- multifile(lisp_trans/2)]),
            format(Outs,'~N~q.~n',[:- dynamic(lisp_trans/2)]),
            format(Outs,'~N~q.~n',[:- style_check(-singleton)]),
            format(Outs,'~N~q.~n',[lisp_trans(translated(File,Temp,BaseName),BaseName:( -1))]),            
            with_lisp_translation(File,write_trans(Outs,BaseName,WithPart2)),
            format(Outs,'~N~q.~n',[end_of_file]))),
  ((ignore(notrace_catch_fail(flush_output(Outs),_,true)),ignore(notrace_catch_fail(close(Outs),_,true))))),!.
  

write_trans(Outs,File,WithPart2,Lisp):-
   zalwayz((call(WithPart2,Lisp,Part),
   nb_current('$lisp_translation_line',Line),
   format(Outs,'~N~q.~n',[lisp_trans(Part,File:Line)]))),!.

/* alternate method*/
phrase_from_stream_partial(Grammar, In):-
  phrase_from_stream((Grammar,!,lazy_forgotten(In)), In).

lazy_forgotten(In,UnUsed,UnUsed):- 
  (is_list(UnUsed)-> true ; append(UnUsed,[],UnUsed)),
  length(UnUsed,PlzUnread),
  seek(In, -PlzUnread, current, _).


% :- use_module(library(yall)).
% :- rtrace.
% tstl(I):- with_lisp_translation(I,([O]>>(writeq(O),nl))).
tstl(I):- with_kifvars(with_lisp_translation(I,writeqnl)).

with_kifvars(Goal):- 
  locally(t_l:sreader_options(logicmoo_read_kif,true),Goal).



:- thread_local(t_l:fake_buffer_codes/2).


%% parse_sexpr( :TermS, -Expr) is det.
%
% Parse S-expression.
%

parse_sexpr(S, Expr) :- parse_meta_term(file_sexpr_with_comments, S, Expr).

%% parse_sexpr_ascii( +Codes, -Expr) is det.
%
% Parse S-expression Codes.
%
parse_sexpr_ascii(S, Expr) :- parse_meta_ascii(file_sexpr_with_comments, S,Expr).


parse_sexpr_ascii_as_list(Text, Expr) :- txt_to_codes(Text,DCodes),
 clean_fromt_ws(DCodes,Codes),!,append([`(`,Codes,`)`],NCodes),!, 
 phrase(sexpr_rest(Expr), NCodes, []).


%% parse_sexpr_string( +Codes, -Expr) is det.
%
% Parse S-expression That maybe sees string from Codes.
%
parse_sexpr_string(S,Expr):- 
 locally_setval('$maybe_string',t,parse_sexpr(string(S), Expr)).

%% parse_sexpr_stream( +Stream, -Expr) is det.
%
% Parse S-expression from a Stream
%
parse_sexpr_stream(S,Expr):- parse_meta_stream(file_sexpr_with_comments,S,Expr).

:- export('//'(file_sexpr,1)).
:- export('//'(sexpr,1)).

% for offline use of this lisp reader
intern_and_eval(UTC,V):- current_predicate(lisp_compiled_eval/2),!,
  call(call,(reader_intern_symbols(UTC,M),!,lisp_compiled_eval(M,V))).
intern_and_eval(UTC,'$intern_and_eval'(UTC)).

% Use DCG for parser.


%file_sexpr_with_comments(O) --> [], {clause(t_l:s_reader_info(O),_,Ref),erase(Ref)},!.
file_sexpr_with_comments(end_of_file) --> file_eof,!.
file_sexpr_with_comments(O) --> one_blank,!,file_sexpr_with_comments(O),!.  % WANT? 
file_sexpr_with_comments(C)                 --> dcg_peek(`#|`),!,zalwayz(comment_expr(C)),swhite,!.
file_sexpr_with_comments(C)                 --> dcg_peek(`;`),!, zalwayz(comment_expr(C)),swhite,!.
file_sexpr_with_comments(Out,S,E):- \+ t_l:sreader_options(with_text,true),!,phrase(file_sexpr(Out),S,E),!.
file_sexpr_with_comments(Out,S,E):- expr_with_text(Out,file_sexpr(O),O,S,E),!.

% in Cyc there was a fitness heuristic that every time an logical axiom had a generated a unique consequent it was considered to have utility as it would expand the breadth of a search .. the problem often was those consequents would feed a another axiom's antecedant where that 
:- asserta((system:'$and'(X,Y):- (X,Y))).

%expr_with_text(Out,DCG,O,S,E):- 
%   call(DCG,S,E) -> append(S,Some,E) -> get_sexpr_with_comments(O,Some,Out,S,E),!.

get_sexpr_with_comments(O,_,O,_,_):- compound(O),functor(O,'$COMMENT',_),!.
get_sexpr_with_comments(O,Txt,with_text(O,Str),S,_E):-append(Txt,_,S),!,text_to_string(Txt,Str).
%file_sexpr_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.


file_sexpr(end_of_file) --> file_eof,!.
% WANT? 
file_sexpr(O) --> sblank,!,file_sexpr(O).
% file_sexpr(planStepLPG(Name,Expr,Value)) --> swhite,sym_or_num(Name),`:`,swhite, sexpr(Expr),swhite, `[`,sym_or_num(Value),`]`,swhite.  %   0.0003:   (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
% file_sexpr(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
% file_sexpr(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
file_sexpr(Expr) --> sexpr(Expr),!.
% file_sexpr(Expr,H,T):- lisp_dump_break,rtrace(phrase(file_sexpr(Expr), H,T)).
/*
file_sexpr(Expr) --> {fail},
   sexpr_lazy_list_character_count(Location,Stream),
  {break,
   seek(Stream,Location,bof,_),   
   read_clause(Stream,Expr,[cycles(true),double_quotes(string),variable_names(Vars)]),
   implode_threse_vars(Vars)},!.

file_sexpr(Expr) --> sexpr(Expr),!.

file_sexpr(end_of_file) --> [].
*/
% file_sexpr('$ERROR'(S_EOF)) --> read_until_eof_e(Unitl_EOF),!,{sformat(S_EOF,'~s',[Unitl_EOF])}.
% read_until_eof_e(Unitl_EOF,S,E):- append(S,E,Unitl_EOF),break,is_list(Unitl_EOF),!.

%read_dispatch(E,[Disp,Char|In],Out):- read_dispatch_char([Disp,Char],E,In,Out). 
read_dispatch(E,[DispatCH|In],Out):- read_dispatch_char([DispatCH],E,In,Out). 

read_dispatch_char(DispatCH,Form,In,Out):- sread_dyn:plugin_read_dispatch_char(DispatCH,Form,In,Out),!.
% read_dispatch_char(`@`,Form,In,Out):- phrase(sexpr(Form), In, Out),!.

read_dispatch_error(Form,In,Out):- trace, dumpST,trace_or_throw((read_dispatch_error(Form,In,Out))).



                                                                  
:- multifile(sread_dyn:plugin_read_dispatch_char/4).
:- dynamic(sread_dyn:plugin_read_dispatch_char/4).

:- use_module(library(dcg/basics)).

% #x Hex
sread_dyn:plugin_read_dispatch_char([DispatCH],Form,In,Out):-
  member(DispatCH,`Xx`),(phrase((`-`,dcg_basics:xinteger(FormP)), In, Out)),!,Form is -FormP.

sread_dyn:plugin_read_dispatch_char([DispatCH],Form,In,Out):-
  member(DispatCH,`Xx`),!,zalwayz(phrase(dcg_basics:xinteger(Form), In, Out)),!.

% #B Binary
sread_dyn:plugin_read_dispatch_char([DispatCH],Form,In,Out):-
  member(DispatCH,`Bb`),!,phrase(signed_radix_2(2,Form), In, Out),!.

% #O Octal
sread_dyn:plugin_read_dispatch_char([DispatCH],Form,In,Out):-
  member(DispatCH,`Oo`),!,phrase(signed_radix_2(8,Form), In, Out),!.

signed_radix_2(W,V)--> signed_radix_2_noext(W,Number),extend_radix(W,Number,V).

signed_radix_2_noext(W,Number) --> `-`,!,unsigned_radix_2(W,NumberP),{Number is - NumberP },!.
signed_radix_2_noext(W,Number) --> `+`,!,unsigned_radix_2(W,Number).
signed_radix_2_noext(W,Number) --> unsigned_radix_2(W,Number).

unsigned_radix_2(W,Number) --> radix_digits(W,Xs),!,{mkvar_w(Xs,W,Number)},!.


radix(Radix)-->`#`,integer(Radix),ci(`r`).
radix(16)-->`#`,ci(`X`).
radix(8)-->`#`,ci(`O`).
radix(2)-->`#`,ci(`B`).

signed_radix_number(V)--> radix(Radix),!,signed_radix_2(Radix,V).
unsigned_radix_number(V)--> radix(Radix),!,unsigned_radix_2(Radix,V).

extend_radix(Radix,Number0,'$RATIO'(Number0,Number1)) --> `/`,unsigned_radix_2(Radix,Number1).
%extend_radix(Radix,Number0,'/'(NumberB,Number1)) --> `.`,radix_number(Radix,Number1),{NumberB is (Number0*Number1)+1},!.
%extend_radix(Radix,Number0,'/'(NumberB,NumberR)) --> `.`,radix_number(Radix,Number1),{NumberR is Number1 * Radix, NumberB is (Number0*Number1)+1},!.
extend_radix(_Radix,Number,Number) --> [].

radix_digits(OF,[X|Xs]) --> xdigit(X),{X<OF},!,radix_digits(OF,Xs).
radix_digits(OF,[X|Xs]) --> alpha_to_lower(C),{X is C - 87,X<OF},!,radix_digits(OF,Xs).
radix_digits(_,[]) --> [].



mkvar_w([W0|Weights], Base, Val) :-
  mkvar_w(Weights, Base, W0, Val).
  
mkvar_w([], _, W, W).
mkvar_w([H|T], Base, W0, W) :-
  W1 is W0*Base+(H),
  mkvar_w(T, Base, W1, W).


ci([])--> !, [].
ci([U|Xs]) --> {to_lower(U,X)},!,alpha_to_lower(X),ci(Xs).
  

remove_optional_char(S)--> S,!.
remove_optional_char(_)-->[].

implode_threse_vars([N='$VAR'(N)|Vars]):-!, implode_threse_vars(Vars).
implode_threse_vars([]).

ugly_sexpr_cont('$OBJ'([S|V]))                 --> rsymbol_maybe(``,S), sexpr_vector(V,`>`),swhite,!.
ugly_sexpr_cont('$OBJ'(V))                 -->  sexpr_vector(V,`>`),swhite,!.
ugly_sexpr_cont('$OBJ'(V))                 -->  sexpr_vector(V,`>`),swhite,!.
ugly_sexpr_cont('$OBJ'(V))                 -->  read_string_until_pairs(VS,`>`), swhite,{parse_sexpr_ascii_as_list(VS,V)},!.
ugly_sexpr_cont('$OBJ'(sugly,S))                 -->  read_string_until(S,`>`), swhite,!.

%%  sexpr(L)// is det.
% 

%sexpr(L)                   --> sblank,!,sexpr(L),!.
%sexpr(_) --> `)`,!,{trace,break,throw_reader_error(": an object cannot start with #\\)")}.
sexpr(X,H,T):- zalwayz(sexpr0(X),H,M),zalwayz(swhite,M,T), if_debugging(sreader,(wdmsg(sexpr(X)))),!.
%sexpr(X,H,T):- zalwayz(sexpr0(X,H,T)),!,swhite.

sexpr0(L)                      --> sblank,!,sexpr(L),!.
sexpr0(L)                      --> `(`, !, swhite, zalwayz(sexpr_list(L)),!, swhite.
sexpr0((Expr))                 --> {fail}, `{`, !, read_string_until(S,`}.`),!, swhite,
  {read_term_from_codes(S,Expr,[cycles(true),module(baseKB),double_quotes(string),variable_names(Vars)]),implode_threse_vars(Vars)}.

sexpr0(['#'(quote),E])             --> `'`, !, sexpr(E).
sexpr0(['#'(backquote),E])         --> ````, !, sexpr(E).
sexpr0(['#BQ-COMMA-ELIPSE',E])     --> `,@`, !, sexpr(E).
sexpr0(['#COMMA',E])               --> `,`, !, sexpr(E).
sexpr0('$OBJ'(claz_bracket_vector,V))                 --> `[`, sexpr_vector(V,`]`),!, swhite.
sexpr0('#'(A))              --> `|`, !, read_string_until(S,`|`), swhite,{quietly_sreader(((atom_string(A,S))))}.

% maybe this is KIF
sexpr0('?'(E))              --> {kif_ok}, `?`, dcg_peek(([C],{sym_char(C)})),!, rsymbol(``,E), swhite.
% @TODO if KIF sexpr('#'(E))              --> `&%`, !, rsymbol(`#$`,E), swhite.

sexpr0('$STRING'(S))             --> s_string(S),!.

/******** BEGIN HASH ************/

sexpr0('#\\'(35))                 --> `#\\#`,!, swhite.
sexpr0(E)                      --> `#`,read_dispatch(E),!.

%sexpr('#\\'(C))                 --> `#\\`,ci(`u`),!,remove_optional_char(`+`),dcg_basics:xinteger(C),!.
%sexpr('#\\'(C))                 --> `#\\`,dcg_basics:digit(S0), swhite,!,{atom_codes(C,[S0])}.
sexpr0('#\\'(32))                 --> `#\\ `,!.
sexpr0('#\\'(C))                 --> `#\\`,!,zalwayz(rsymbol(``,C)), swhite.

%sexpr(['#-',K,Out]) --> `#-`,!,sexpr(C),swhite,expr_with_text(Out,sexpr(O),O),!,{as_keyword(C,K)}.
%sexpr(['#+',K,Out]) --> `#+`,!,sexpr(C),swhite,expr_with_text(Out,sexpr(O),O),!,{as_keyword(C,K)}.

sexpr0(['#-',K,O]) --> `#-`,!,sexpr(C),swhite,sexpr(O),!,{as_keyword(C,K)},!.
sexpr0(['#+',K,O]) --> `#+`,!,sexpr(C),swhite,sexpr(O),!,{as_keyword(C,K)},!.

sexpr0(P) --> `#`,ci(`p`),!,zalwayz((sexpr(C),{f_pathname(C,P)})),!.
sexpr0('$S'(C)) -->                  (`#`, ci(`s`),`(`),!,zalwayz(sexpr_list(C)),swhite,!.
%sexpr('$COMPLEX'(R,I)) --> `#`,ci(`c`),`(`,!,  lnumber(R),lnumber(I),`)`.
sexpr0('$COMPLEX'(R,I)) -->         (`#`, ci(`c`),`(`),!,zalwayz(sexpr_list([R,I])),swhite,!.
sexpr0('$OBJ'(claz_bitvector,C)) --> `#*`,radix_digits(2,C),swhite,!.

sexpr0(function(E))                 --> `#\'`, sexpr(E), !. %, swhite.
sexpr0('$OBJ'(claz_vector,V))                 --> `#(`, !, zalwayz(sexpr_vector(V,`)`)),!, swhite,!.

sexpr0(Number) --> `#`,integer(Radix),ci(`r`),!,zalwayz((signed_radix_2(Radix,Number0),extend_radix(Radix,Number0,Number))),!.
sexpr0('$ARRAY'(Dims,V)) --> `#`,integer(Dims),ci(`a`),!,sexpr(V).
sexpr0(V)                    --> `#.`, !,sexpr(C),{to_untyped(C,UTC),!,intern_and_eval(UTC,V)},!.
sexpr0('#'(E))              --> `#:`, !,zalwayz(rsymbol(`#:`,E)), swhite.

sexpr0(OBJ)--> `#<`,!,zalwayz(ugly_sexpr_cont(OBJ)),!.

% @TODO if CYC sexpr('#'(E))              --> `#$`, !, rsymbol(`#$`,E), swhite.
% @TODO if scheme sexpr('#'(t))                 --> `#t`, !, swhite.
% @TODO if schemesexpr('#'(f))                 --> `#f`, !, swhite.

% sexpr(E)                      --> `#`,read_dispatch_error(E).

/*********END HASH ***********/

sexpr0(E)                      --> !,zalwayz(sym_or_num(E)), swhite,!.

sym_or_num(('+1-')) --> `+1-`,!,swhite.
sym_or_num(('-1+')) --> `-1+`,!,swhite.


sym_or_num(('-1-')) --> `-1-`,swhite,!.
sym_or_num(('-1+')) --> `-1+`,swhite,!.
sym_or_num(('+1+')) --> `+1-`,swhite,!.
sym_or_num('$COMPLEX'(L)) --> `#C(`,!, swhite, sexpr_list(L), swhite.
%sym_or_num((E)) --> unsigned_number(S),{number_string(E,S)}.
sym_or_num(('1+')) --> `1+`,swhite,!.
sym_or_num(('1-')) --> `1-`,swhite,!.
%sym_or_num((E)) --> unsigned_number(S),{number_string(E,S)}.
sym_or_num(('#+')) --> `#+`,swhite,!.
sym_or_num(('#-')) --> `#-`,swhite,!.
sym_or_num(('-#+')) --> `-#+`,swhite,!.
sym_or_num((E)) --> lnumber(E),swhite,!.
sym_or_num(E) --> rsymbol_maybe(``,E),!.
%sym_or_num('#'(E)) --> [C],{atom_codes(E,[C])}.

sym_or_num(E) --> dcg_xor(rsymbol(``,E),lnumber(E)),!.
% sym_or_num('#'(E)) --> [C],{atom_codes(E,[C])}.


dcg_xor(DCG1,DCG2,S,E):- copy_term(DCG1,DCG1C),phrase(DCG1C,S,E),!,
  (phrase(DCG2,S,[])->true;zalwayz(DCG1C=DCG1)),!.
dcg_xor(_,DCG2,S,E):- phrase(DCG2,S,E),!.
%sblank --> [C], {var(C)},!.

% sblank --> comment_expr(S,I,CP),!,{assert(t_l:s_reader_info('$COMMENT'(S,I,CP)))},!,swhite.
sblank --> comment_expr(CMT),!,{assert(t_l:s_reader_info(CMT))},!,swhite.
sblank --> [C], {nonvar(C),charvar(C),!,bx(C =< 32)},!,swhite.

sblank_line --> eoln,!.
sblank_line --> [C],{bx(C =< 32)},!, sblank_line.

s_string(Text)                 --> `"`, !, zalwayz(s_string_cont(Text)),!.
s_string_cont("")             --> `"`,!, swhite.
s_string_cont(Txt)                 --> sexpr_string(S), swhite,{text_to_string_safe(S,Txt)}.


swhite --> sblank,!.
swhite --> [].


sexpr_lazy_list_character_count(Location, Stream, Here, Here) :-
	sexpr_lazy_list_character_count(Here, Location, Stream).

sexpr_lazy_list_character_count(Here, CharNo, Stream) :-
	'$skip_list'(Skipped, Here, Tail),
	(   attvar(Tail)
	->  frozen(Tail,
		   pure_input:read_to_input_stream(Stream, _PrevPos, Pos, _List)),
	    stream_position_data(char_count, Pos, EndRecordCharNo),
	    CharNo is EndRecordCharNo - Skipped
	;   Tail == []
	->  CharNo = end_of_file-Skipped
	;   type_error(lazy_list, Here)
	).



comment_expr('$COMMENT'(Expr,I,CP)) --> comment_expr_3(Expr,I,CP),!.

comment_expr_3(T,N,CharPOS) --> `#|`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`|#`)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_3(T,N,CharPOS) -->  `;`,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
  {text_to_string_safe(S,T)},!.


sexprs([H|T]) --> sexpr(H), !, sexprs(T).
sexprs([]) --> [].


:- export('//'(sexpr_list,1)).
 

peek_symbol_breaker_or_number --> dcg_peek([C]),{\+ sym_char(C),\+ char_type(C,digit)}.
peek_symbol_breaker --> dcg_peek([C]),{\+ sym_char(C)}.
peek_symbol_breaker --> one_blank.

sexpr_list(X) --> one_blank,!,sexpr_list(X).
sexpr_list([]) --> `)`, !.
%sexpr_list(_) --> `.`, [C], {\+ sym_char(C)}, {fail}.
sexpr_list([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr),!.

sexpr_rest([]) --> `)`, !.
sexpr_rest(E) --> `.`, [C], {\+ sym_char(C)}, !, sexpr(E,C), !, `)`.
sexpr_rest(E) --> {kif_ok}, `@`, rsymbol(`?`,E), `)`.
sexpr_rest([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr),!.

sexpr_vector(O,End) --> zalwayz(sexpr_vector0(IO,End)),!,{zalwayz(O=IO)}.

sexpr_vector0(X) --> one_blank,!,sexpr_vector0(X).
sexpr_vector0([],End) --> End, !.
sexpr_vector0([First|Rest],End) --> sexpr(First), !, sexpr_vector0(Rest,End).

sexpr_string(X)--> read_string_until(X,`"`).
%sexpr_string([C|S],End) --> `\\`,!, zalwayz(escaped_char(C)),!, sexpr_string(S,End).
%sexpr_string([],End) --> End, !.
% sexpr_string([32|S]) --> [C],{eoln(C)}, sexpr_string(S).
%sexpr_string([C|S],End) --> [C],!,sexpr_string(S,End).

rsymbol(Chars,E) --> [C], {sym_char(C)},!, sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.
rsymbol_cont(Prepend,E) --> sym_continue(S), {append(Prepend,S,AChars),string_to_atom(AChars,E)},!.

rsymbol_maybe(Prepend,ES) --> rsymbol(Prepend,E),{maybe_string(E,ES)},!.

maybe_string(E,ES):- nb_current('$maybe_string',t),!,text_to_string_safe(E,ES),!.
maybe_string(E,E).

sym_continue([H|T]) --> [H], {sym_char(H)},!, sym_continue(T).
sym_continue([]) --> peek_symbol_breaker,!.
sym_continue([]) --> [].

string_vector([First|Rest]) --> sexpr(First), !, string_vector(Rest),!.
string_vector([]) --> [], !.

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

lnumber(_)--> [C],{code_type(C,alpha)},!,{fail}.
lnumber(N)-->  lnumber0(N),!. % (peek_symbol_breaker;[]).

oneof_ci(OneOf,[C])--> {member(C,OneOf)},ci([C]). 
dcg_and2(DCG1,DCG2,S,E) :- dcg_phrase(DCG1,S,E),!,dcg_phrase(DCG2,S,E),!.
dcg_each_call_cleanup(Setup,DCG,Cleanup,S,E) :- each_call_cleanup(Setup,dcg_phrase(DCG,S,E),Cleanup).
dcg_phrase(\+ DCG1,S,E):- !, \+ phrase(DCG1,S,E).
dcg_phrase(DCG1,S,E):- phrase(DCG1,S,E),!.

dcg_not(DCG1,S,E) :- \+ dcg_phrase(DCG1,S,E).

enumber(N)--> lnumber(L),!,{to_untyped(L,N)},!.

/*
Format  Minimum Precision  Minimum Exponent Size  
Short   13 bits            5 bits                 
Single  24 bits            8 bits                 
Double  50 bits            8 bits                 
Long    50 bits            8 bits   
*/

float_e_type(`E`,claz_single_float).
float_e_type(`f`,claz_single_float).
float_e_type(`d`,claz_double_float).
float_e_type(`L`,claz_long_float).
float_e_type(`s`,claz_short_float).

lnumber_exp('$EXP'(N,T,E))-->snumber_no_exp(N),!,oneof_ci(`EsfdL`,TC),dcg_basics:integer(E),{exp:float_e_type(TC,T)},!.
lnumber_exp('$EXP'(N,T,E))-->dcg_basics:integer(N),!,oneof_ci(`EsfdL`,TC),dcg_basics:integer(E),!,{float_e_type(TC,T)},!.


lnumber0(N) --> lnumber_exp(N),!.
lnumber0('$RATIO'(N,D)) --> sint(N),`/`,uint(D),!.
lnumber0(N) --> snumber_no_exp(N),!.
%lnumber0(N) --> dcg_basics:number(N),!.


snumber_no_exp(N)--> `-`,!,unumber_no_exp(S),{N is -S},!.
snumber_no_exp(N)--> `+`,!,unumber_no_exp(N),!.
snumber_no_exp(N)--> unumber_no_exp(N),!.
%snumber_no_exp(N)-->  sint(N),!.


sint(N) --> signed_radix_number(N),!.
sint(N)--> `-`,!,uint(S),{N is -S},!.                          
sint(N)--> `+`,!,uint(N),!.
sint(N)--> uint(N),!.

natural_int(_) --> dcg_not(dcg_basics:digit(_)),!,{fail}.
natural_int(N) --> dcg_basics:integer(N),!.

digits_dot_digits --> natural_int(_),!,`.`,!,natural_int(_),!.

unumber_no_exp(N) --> dcg_and2(digits_dot_digits,dcg_basics:float(N)),!.
unumber_no_exp(N) --> `.`,!,dcg_basics:digit(S0),!,dcg_basics:digits(S),{(notrace_catch_fail(number_codes(N,[48,46,S0|S])))},!.
unumber_no_exp(N)--> natural_int(E),`.`,natural_int(S),{(notrace_catch_fail(number_codes(ND,[48,46|S]))),N is ND + E},!.
unumber_no_exp(N) --> natural_int(N),!,remove_optional_char(`.`),!.

uint(N) --> unsigned_radix_number(N),!.
uint(N) --> natural_int(N),!,remove_optional_char(`.`),!.


% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


%= 	 	 

%% sexpr( ?E, ?C, ?X, ?Z) is det.
%
% S-Expression.
%
sexpr(E,C,X,Z) :- swhite([C|X],Y), sexpr(E,Y,Z),!.

% dquote semicolon parens  hash qquote  comma backquote

%= 	 	 

%% sym_char( ?C) is det.
%
% Sym Char.  (not ";()#',` 
% )
%

sym_char(C):- bx(C =<  32),!,fail.
%sym_char(44). % allow comma in middle of symbol
sym_char(C):- memberchk(C,`";()#'```),!,fail.  % maybe 44 ? comma
%sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!%`),!,fail.
sym_char(_):- !.

sym_char_start(C):- C\==44,C\==59,sym_char(C).



:- thread_initialization(nb_setval('$maybe_string',[])).

:- thread_local(t_l:s2p/1).
:- thread_local(t_l:each_file_term/1).



%= 	 	 

%% to_unbackquote( ?I, ?O) is det.
%
% Converted To Unbackquote.
%
to_unbackquote(I,O):-to_untyped(I,O),!.

:- export(to_untyped/2).


%atom_or_string(X):- (atom(X);string(X)),!.
as_keyword(C,K):- atom(C),!,(atom_concat_or_rtrace(':',_,C)->K=C;atom_concat_or_rtrace(':',C,K)),!.
as_keyword(C,C):- \+compound(C),!.
as_keyword([A|B],[AK|BK]):- !, as_keyword(A,AK),as_keyword(B,BK),!.
as_keyword(C,C).


%% to_untyped( :TermVar, :TermName) is det.
%
% Converted To Untyped.
%
to_untyped(S,S):- var(S),!.
to_untyped(S,S):- is_dict(S),!.
to_untyped([],[]):-!.
to_untyped('#-'(C,I),'#-'(K,O)):- as_keyword(C,K),!,to_untyped(I,O),!.
to_untyped('#+'(C,I),'#+'(K,O)):- as_keyword(C,K),!,to_untyped(I,O),!.
to_untyped('?'(S),_):- S=='??',!.
% to_untyped('?'(S),'$VAR'('_')):- S=='??',!.
% to_untyped(VAR,NameU):-atom(VAR),atom_concat_or_rtrace('#$',NameU,VAR),!.
to_untyped(VAR,NameU):-atom(VAR),(atom_concat_or_rtrace(N,'.',VAR)->true;N=VAR),(notrace_catch_fail(atom_number(N,NameU))),!.
%to_untyped(S,s(L)):- string(S),atom_contains(S,' '),atomic_list_concat(['(',S,')'],O),parse_sexpr_string(O,L),!.
to_untyped(S,S):- string(S),!.
to_untyped(S,S):- number(S),!.
%to_untyped(S,O):- atom(S),notrace_catch_fail(atom_number(S,O)),!.
to_untyped(Var,'$VAR'(Name)):-svar(Var,Name),!.
to_untyped(Atom,Atom):- \+ compound(Atom),!.
to_untyped('@'(Var),'$VAR'(Name)):-svar_fixvarname(Var,Name),!.
to_untyped('#'(S),O):- !, (nonvar(S)->to_untyped(S,O) ; O='#'(S)).
to_untyped('$CHAR'(S),C):-!,to_untyped('#\\'(S),C),!.
to_untyped('#\\'(S),C):-to_char(S,C),!.
to_untyped('#\\'(S),'#\\'(S)):-!.
to_untyped('$OBJ'([FUN, F]),O):- atom(FUN),!,to_untyped('$OBJ'(FUN, F),O).
to_untyped('$OBJ'([FUN| F]),O):- atom(FUN),!,to_untyped('$OBJ'(FUN, F),O).
to_untyped('$OBJ'(S),'$OBJ'(O)):-to_untyped(S,O),!.
to_untyped('$OBJ'(Ungly,S),'$OBJ'(Type,O)):- text_to_string_safe(Ungly,Str),string_to_atom(Str,Type),to_untyped(S,O),!.
to_untyped('$OBJ'(Ungly,S),'$OBJ'(Ungly,O)):-to_untyped(S,O),!.
to_untyped('$OBJ'(Ungly,S),O):-to_untyped(S,SO),!,O=..[Ungly,SO].
to_untyped('$COMPLEX'(N0,D0),N):- to_untyped(D0,D), notrace_catch_fail(( 0 =:= D)),to_untyped(N0,N).
to_untyped('$RATIO'(N0,D0),V):- to_untyped(N0,N),to_untyped(D0,D), notrace_catch_fail(( 0 is N mod D, V is N div D)).
to_untyped('$NUMBER'(S),O):-nonvar(S),to_number(S,O),to_untyped(S,O),!.
to_untyped('$NUMBER'(S),'$NUMBER'(claz_short_float,S)):- float(S),!.
to_untyped('$NUMBER'(S),'$NUMBER'(claz_bignum,S)).
to_untyped('$EXP'(I,'E',E),N):- (notrace_catch_fail(N is 0.0 + ((I * 10^E)))),!.
to_untyped('$EXP'(I,claz_single_float,E),N):- (notrace_catch_fail(N is 0.0 + ((I * 10^E)))),!.
to_untyped('$EXP'(I,T,E),'$NUMBER'(T,N)):- (notrace_catch_fail(N is (I * 10^E))),!.
to_untyped('$EXP'(I,T,E),'$EXP'(I,T,E)):-!.

to_untyped(with_text(I,_Txt),O):-to_untyped(I,O),!.
to_untyped(with_text(I,Txt),with_text(O,Txt)):-to_untyped(I,O),!.

% to_untyped([[]],[]):-!.
to_untyped('$STR'(Expr),Forms):- (text_to_string_safe(Expr,Forms);to_untyped(Expr,Forms)),!.
to_untyped('$STRING'(Expr),'$STRING'(Forms)):- (text_to_string_safe(Expr,Forms);to_untyped(Expr,Forms)),!.
to_untyped(['#'(Backquote),Rest],Out):- Backquote == backquote, !,to_untyped(['#'('#BQ'),Rest],Out).
to_untyped(['#'(S)|Rest],OOut):- nonvar(S), is_list(Rest),must_maplist(to_untyped,[S|Rest],[F|Mid]), 
          ((atom(F),t_l:s2p(F))-> Out=..[F|Mid];Out=[F|Mid]),
          to_untyped(Out,OOut).
to_untyped(ExprI,ExprO):- ExprI=..[F|Expr],atom_concat_or_rtrace('$',_,F),must_maplist(to_untyped,Expr,TT),ExprO=..[F|TT].

% to_untyped([H|T],Forms):-is_list([H|T]),zalwayz(text_to_string_safe([H|T],Forms);maplist(to_untyped,[H|T],Forms)).
to_untyped([H|T],[HH|TT]):-!,zalwayz((to_untyped(H,HH),to_untyped(T,TT))).
to_untyped(ExprI,ExprO):- zalwayz(ExprI=..Expr),
  must_maplist(to_untyped,Expr,[HH|TT]),(atom(HH)-> ExprO=..[HH|TT] ; ExprO=[HH|TT]).
% to_untyped(Expr,Forms):-def_compile_all(Expr,Forms),!.

to_number(S,S):-number(S),!.
to_number(S,N):- text_to_string_safe(S,Str),number_string(N,Str),!.


to_char(S,'#\\'(S)):- var(S),!.
to_char('#'(S),C):- !, to_char(S,C).
to_char('#\\'(S),C):- !, to_char(S,C).
to_char(S,C):- atom(S),atom_concat('^',SS,S),upcase_atom(SS,SU),atom_codes(SU,[N64]),N is N64-64,N>=0,!,to_char(N,C).
to_char(S,C):- atom(S),atom_codes(S,[N]),!,to_char(N,C).
to_char(N,C):- text_to_string_safe(N,Str),name_to_charcode(Str,Code),to_char(Code,C),!.
%to_char(N,'#\\'(S)):- to_number(N,NC),!,char_code_to_char(NC,S),!.
to_char(N,'#\\'(S)):- integer(N),!,char_code_to_char(N,S),!.
to_char(N,'#\\'(N)).


char_code_int(Char,Code):- notrace_catch_fail(char_code(Char,Code)),!.
char_code_int(Char,Code):- notrace_catch_fail(atom_codes(Char,[Code])),!.
char_code_int(Char,Code):- atom(Char),name_to_charcode(Char,Code),!.
char_code_int(Char,Code):- var(Char),!,wdmsg(char_code_int(Char,Code)),break.
char_code_int(Char,Code):- wdmsg(char_code_int(Char,Code)),break.

char_code_to_char(N,S):- atom(N),atom_codes(N,[_]),!,S=N.
char_code_to_char(N,S):- atom(N),!,S=N.
%char_code_to_char(N,S):- code_type(N,graph),atom_codes(S,[N]),atom(S),!.
%char_code_to_char(N,O):- \+ integer(N),char_type(N,_),!,N=O.
%char_code_to_char(32,' '):-!.
%char_code_to_char(N,N):- \+ code_type(N,graph),!.
%char_code_to_char(N,N):- code_type(N,white),!.
char_code_to_char(N,S):-  notrace_catch_fail(atom_codes(S,[N])),!.



name_to_charcode(Str,Code):-find_from_name(Str,Code),!.
name_to_charcode(Str,Code):-text_upper(Str,StrU),find_from_name2(StrU,Code).
name_to_charcode(Str,Code):-string_codes(Str,[S,H1,H2,H3,H4|HEX]),memberchk(S,`Uu`),char_type(H4,xdigit(_)),
   notrace_catch_fail(read_from_codes([48, 120,H1,H2,H3,H4|HEX],Code)).
name_to_charcode(Str,Code):-string_codes(Str,[S,H1|BASE10]),memberchk(S,`nd`),char_type(H1,digit),
   notrace_catch_fail(read_from_codes([H1|BASE10],Code)).

find_from_name(Str,Code):-string_codes(Str,Chars),lisp_code_name_extra(Code,Chars).
find_from_name(Str,Code):-lisp_code_name(Code,Str).
find_from_name(Str,Code):-string_chars(Str,Chars),lisp_code_name(Code,Chars).

make_lisp_character(I,O):-notrace(to_char(I,O)).

f_code_char(CH,CC):- zalwayz(to_char(CH,CC)),!.
f_name_char(Name,CC):- zalwayz((def_to_prolog_string(Name,CH),name_to_charcode(CH,Code),to_char(Code,CC))).
f_char_name(CH,CC):- zalwayz(def_is_characterp(CH)),zalwayz(code_to_name(CH,CC)).
f_char_int(CH,CC):-  zalwayz(def_is_characterp(CH)),zalwayz('#\\'(C)=CH),(integer(C)->CC=C;char_code_int(C,CC)).
f_char_code(CH,CC):- f_char_int(CH,CC).

to_prolog_char('#\\'(X),O):-!,to_prolog_char(X,O).
to_prolog_char(Code,Char):- number(Code),!,zalwayz(char_code_int(Char,Code)),!.
%to_prolog_char(S,S):- atom(S),char_type(S,_),!.
to_prolog_char(Atom,Char):- name(Atom,[C|Odes]),!,
  ((Odes==[] -> char_code_int(Char,C); 
  zalwayz((text_to_string(Atom,String),name_to_charcode(String,Code),char_code_int(Char,Code))))).

code_to_name(Char,Str):- number(Char),Char=Code,!,zalwayz((code_to_name0(Code,Name),!,text_to_string(Name,Str))).
code_to_name(Char,Str):- zalwayz((to_prolog_char(Char,PC),char_code_int(PC,Code),code_to_name0(Code,Name),!,text_to_string(Name,Str))).

code_to_name0(Code,Name):-lisp_code_name_extra(Code,Name).
code_to_name0(Code,Name):-lisp_code_name(Code,Name).
code_to_name0(Code,Name):- Code<32, Ascii is Code+64,atom_codes(Name,[94,Ascii]).
code_to_name0(Code,Name):- code_type(Code,graph),!,atom_codes(Name,[Code]).


find_from_name2(Str,Code):-find_from_name(Str,Code).
find_from_name2(Str,Code):-lisp_code_name(Code,Chars),text_upper(Chars,Str).
find_from_name2(Str,Code):-lisp_code_name_extra(Code,Chars),text_upper(Chars,Str).

text_upper(T,U):-text_to_string_safe(T,S),string_upper(S,U).

lisp_code_name_extra(0,`Null`).
lisp_code_name_extra(1,`Soh`).
lisp_code_name_extra(2,`^B`).
lisp_code_name_extra(7,`Bell`).
lisp_code_name_extra(7,`bell`).
lisp_code_name_extra(8,`BCKSPC`).
lisp_code_name_extra(10,`Newline`).
lisp_code_name_extra(10,`LF`).
lisp_code_name_extra(10,`Linefeed`).
lisp_code_name_extra(11,`Vt`).
lisp_code_name_extra(27,`Escape`).
lisp_code_name_extra(27,`Esc`).
lisp_code_name_extra(32,`Space`).
lisp_code_name_extra(28,`fs`).
lisp_code_name_extra(13,`Ret`).


% @TODO undo this temp speedup
:- set_prolog_flag(all_lisp_char_names,false).
:- use_module('chars.data').
/*

(with-open-file (strm "lisp_code_names.pl" :direction :output :if-exists :supersede :if-does-not-exist :create)
 (format  strm ":- module(lisp_code_names,[lisp_code_name/2]).~%:- set_prolog_flag(double_quotes,chars).~%~%")
 (loop for i from 0 to 655360 do (let ((cname (char-name (code-char i))) (uname4 (format ()  "U~4,'0X" i)) (uname8 (format ()  "U~8,'0X" i)))
  (unless (equal cname uname4) (unless (equal cname uname8)  (format  strm "lisp_code_name(~A,~S).~%" i  cname ))))))
*/
    	 

%% remove_incompletes( :TermN, :TermCBefore) is det.
%
% Remove Incompletes.
%
remove_incompletes([],[]).
remove_incompletes([N=_|Before],CBefore):-var(N),!,
 remove_incompletes(Before,CBefore).
remove_incompletes([NV|Before],[NV|CBefore]):-
 remove_incompletes(Before,CBefore).

:- export(extract_lvars/3).

%= 	 	 

%% extract_lvars( ?A, ?B, ?After) is det.
%
% Extract Lvars.
%
extract_lvars(A,B,After):-
     (get_varname_list(Before)->true;Before=[]),
     remove_incompletes(Before,CBefore),!,
     copy_lvars(A,CBefore,B,After),!.

% copy_lvars( VAR,Vars,VAR,Vars):- var(VAR),!.

%= 	 	 

%% copy_lvars( :TermVAR, ?Vars, :TermNV, ?NVars) is det.
%                        
% Copy Lvars.
%
copy_lvars(Term,Vars,Out,VarsO):- Term ==[],!,zalwayz((Out=Term,VarsO=Vars)).
copy_lvars( VAR,Vars,Out,VarsO):- var(VAR),!,zalwayz((Out=VAR,VarsO=Vars)).
copy_lvars([H|T],Vars,[NH|NT],VarsO):- !, copy_lvars(H,Vars,NH,SVars),!, copy_lvars(T,SVars,NT,VarsO).
copy_lvars('?'(Inner),Vars,Out,VarsO):- !, copy_lvars(Inner,Vars,NInner,VarsO), zalwayz((atom(NInner) -> atom_concat_or_rtrace('?',NInner,Out) ; Out = '?'(NInner))),!.
copy_lvars( VAR,Vars,Out,VarsO):- svar(VAR,Name)->zalwayz(atom(Name)),!,zalwayz(register_var(Name=Out,Vars,VarsO)).
copy_lvars( VAR,Vars,Out,VarsO):- \+ compound(VAR),!,zalwayz((Out=VAR,VarsO=Vars)).
copy_lvars(Term,Vars,NTerm,VarsO):-    
    Term=..[F|Args],    % decompose term
    (svar(F,_)-> copy_lvars( [F|Args],Vars,NTerm,VarsO);
    % construct copy term
       (copy_lvars(Args,Vars,NArgs,VarsO), NTerm=..[F|NArgs])),!.  


%= 	 	 

%% svar( ?Var, ?NameU) is det.
%
% If this is a KIF var, convert to a name for prolog
%
svar(SVAR,UP):- nonvar(UP),!,trace_or_throw(nonvar_svar(SVAR,UP)).
svar(Var,Name):- var(Var),!,zalwayz(svar_fixvarname(Var,Name)).
svar('$VAR'(Var),Name):-number(Var),Var > -1, !, zalwayz(format(atom(Name),'~w',['$VAR'(Var)])),!.
svar('$VAR'(Name),VarName):-!,zalwayz(svar_fixvarname(Name,VarName)).
svar(_,_):- \+ kif_ok,!,fail.
svar([],_):-!,fail.
svar('#'(Name),NameU):-!,svar(Name,NameU),!.
svar('?'(Name),NameU):-svar_fixvarname(Name,NameU),!.
svar('@'(Name),NameU):-svar_fixvarname(Name,NameU),!.
% svar(VAR,Name):-atom(VAR),atom_concat_or_rtrace('_',_,VAR),svar_fixvarname(VAR,Name),!.
svar(VAR,Name):-atom(VAR),atom_concat_or_rtrace('@',A,VAR),non_empty_atom(A),svar_fixvarname(VAR,Name),!.
svar(VAR,Name):-atom(VAR),atom_concat_or_rtrace('?',A,VAR),non_empty_atom(A),svar_fixvarname(VAR,Name),!.


:- export(svar_fixvarname/2).

%= 	 	 

%% svar_fixvarname( ?SVARIN, ?UP) is det.
%
% Svar Fixvarname.
%

svar_fixvarname(SVAR,UP):- nonvar(UP),!,trace_or_throw(nonvar_svar_fixvarname(SVAR,UP)).
svar_fixvarname(SVAR,UP):- svar_fixname(SVAR,UP),!.
svar_fixvarname(SVAR,UP):- trace_or_throw(svar_fixname(SVAR,UP)).

svar_fixname(Var,NameO):-var(Var),variable_name_or_ref(Var,Name),sanity(nonvar(Name)),!,svar_fixvarname(Name,NameO).
svar_fixname('$VAR'(Name),UP):- !,svar_fixvarname(Name,UP).
svar_fixname('@'(Name),UP):- !,svar_fixvarname(Name,UP).
svar_fixname('?'(Name),UP):- !,svar_fixvarname(Name,UP).
svar_fixname('block'(Name),UP):- !,svar_fixvarname(Name,UP).
svar_fixname(SVAR,SVARO):- ok_var_name(SVAR),!,SVARO=SVAR.
svar_fixname('??','_'):-!.
svar_fixname(QA,AU):-atom_concat_or_rtrace('??',A,QA),non_empty_atom(A),!,svar_fixvarname(A,AO),atom_concat_or_rtrace('_',AO,AU).
svar_fixname(QA,AO):-atom_concat_or_rtrace('?',A,QA),non_empty_atom(A),!,svar_fixvarname(A,AO).
svar_fixname(QA,AO):-atom_concat_or_rtrace('@',A,QA),non_empty_atom(A),!,svar_fixvarname(A,AO).
svar_fixname(NameU,NameU):-atom_concat_or_rtrace('_',Name,NameU),non_empty_atom(Name),atom_number(Name,_),!.
svar_fixname(NameU,NameUO):-atom_concat_or_rtrace('_',Name,NameU),non_empty_atom(Name), \+ atom_number(Name,_),!,svar_fixvarname(Name,NameO),atom_concat_or_rtrace('_',NameO,NameUO).
svar_fixname(I,O):-  
 zalwayz((
  fix_varcase(I,M0),
  atom_subst(M0,'@','_AT_',M1),
  atom_subst(M1,'?','_Q_',M2),
  atom_subst(M2,':','_C_',M3),
  atom_subst(M3,'-','_',O),
  ok_var_name(O))),!.

%= 	 	 

%% fix_varcase( ?I, ?O) is det.
%                                                               
% Fix Varcase.
%
fix_varcase(Word,Word):- atom_concat_or_rtrace('_',_,Word),!.
fix_varcase(Word,WordC):- !, atom_codes(Word,[F|R]),to_upper(F,U),atom_codes(WordC,[U|R]).
% the cut above stops the rest 
fix_varcase(Word,Word):-upcase_atom(Word,UC),UC=Word,!.
fix_varcase(Word,WordC):-downcase_atom(Word,UC),UC=Word,!,atom_codes(Word,[F|R]),to_upper(F,U),atom_codes(WordC,[U|R]).
fix_varcase(Word,Word). % mixed case

:- export(ok_varname_or_int/1).

%% ok_varname_or_int( ?Name) is det.
%
% Ok Varname.
%
ok_varname_or_int(Name):- atom(Name),!,ok_var_name(Name).
ok_varname_or_int(Name):- number(Name).

%% ok_var_name( ?Name) is det.
%
% Ok Varname.
%
ok_var_name(Name):- 
  quietly_sreader(( atom(Name),atom_codes(Name,[C|_List]),char_type(C,prolog_var_start),
      read_term_from_atom(Name,Term,[syntax_errors(fail),variable_names(Vs)]),!,var(Term),Vs=[RName=RVAR],!,RVAR==Term,RName==Name)).

%:- export(ok_codes_in_varname/1).
%ok_codes_in_varname([]).
%ok_codes_in_varname([C|List]):-!,ok_in_varname(C),ok_codes_in_varname(List).

%:- export(ok_in_varname/1).
%ok_in_varname(C):-sym_char(C),\+member(C,`!@#$%^&*?()`).



%= 	 	 

%% atom_upper( ?A, ?U) is det.
%
% Atom Upper.
%
atom_upper(A,U):-string_upper(A,S),quietly_sreader(((atom_string(U,S)))).


%= 	 	 

%% lisp_read_from_input( ?Forms) is det.
%
% Lisp Read Converted From Input.
%
lisp_read_from_input(Forms):-lisp_read(current_input,Forms),!.

readCycL(Forms):-lisp_read(current_input,Forms).

%% lisp_read_from_stream( ?I, ?Forms) is det.
%
% Lisp Read Converted To Simple Form.
%
lisp_read_from_stream(Input,Forms):- 
   lisp_read(Input,Forms).
     	 

%% lisp_read( ?I, ?Forms) is det.
%
% Lisp Read Converted To Simple Form.
%
lisp_read(Input,Forms):- 
    lisp_read_typed(Input, Forms0),!,
    quietly_sreader((zalwayz(to_untyped(Forms0,Forms)))).



%% lisp_read_typed( ?I, -Expr) is det.
%
% Lisp Read, Expression models DCG
%
lisp_read_typed(In,Expr):- track_stream(In,parse_sexpr(In,Expr)),!.
/*
lisp_read_typed(In,Expr):- fail, % old_stream_read
 (read_line_to_codes(current_input,AsciiCodes),
      (AsciiCodes==[]-> (at_end_of_stream(In) -> (Expr=end_of_file); lisp_read_typed(In,Expr)); 
        once(zalwayz(parse_sexpr(AsciiCodes,Expr);lisp_read_typed(In,Expr));read_term_from_codes(AsciiCodes,Expr,[])))).
*/


%= 	 	 

%% lowcase( :TermC1, :TermC2) is det.
%
% Lowcase.
%
lowcase([],[]).
lowcase([C1|T1],[C2|T2]) :- lowercase(C1,C2), lowcase(T1,T2).


%= 	 	 

%% lowercase( ?C1, ?C2) is det.
%
% Lowercase.
%
lowercase(C1,C2) :- C1 >= 65, C1 =< 90, !, C2 is C1+32.
lowercase(C,C).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



%= 	 	 

%% codelist_to_forms( ?AsciiCodesList, ?FormsOut) is det.
%
% Codelist Converted To Forms.
%
codelist_to_forms(AsciiCodesList,FormsOut):-
    parse_sexpr(AsciiCodesList, Forms0),!,   
    zalwayz(def_compile_all(Forms0, FormsOut)),!.


/*

:- export(baseKB:rff/0).

baseKB:rff:-baseKB:rff(dbginfo(n(first)),dbginfo(n(retry)),dbginfo(n(success)),dbginfo(n(failure))).

:- export(baseKB:rff/4).
baseKB:rff(OnFirst,OnRetry,OnSuccess,OnFailure) :- CU = was(never,first),
  call_cleanup((
    process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),
    (nb_setarg(1,CU,first));((nb_setarg(1,CU,second)),!,fail)),
    (nb_setarg(2,CU,second),process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),dbginfo(cleanup(CU)))),
  once((
    process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),
    CU \= was(second, _))).

:- export(process_rff/5).
process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure):-
   dbginfo(next(CU)),
   once(((CU==was(first,first)->OnFirst;true),
   (CU==was(second,first)->OnRetry;true),
   (CU==was(second,second)->OnFailure;true),
   (CU==was(first,second)-e>OnSuccess;true))).


*/


/*
:- prolog_load_context(directory,Dir),
   DirFor = plarkc,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
*/

% [Required] Load the Logicmoo Library Utils
% = % :- ensure_loaded(logicmoo(logicmoo_utils)).

% % :- ensure_loaded(logicmoo(plarkc/mpred_cyc_api)).


:- export(fixvars/4).

%= 	 	 

%% fixvars( ?P, ?VALUE2, :TermARG3, ?P) is det.
%
% Fixvars.
%
fixvars(P,_,[],P):-!.
fixvars(P,N,[V|VARS],PO):-  
     quietly_sreader((atom_string(Name,V))),
     svar_fixvarname(Name,NB),Var = '$VAR'(NB),
     subst(P,'$VAR'(N),Var,PM0),
     subst(PM0,'$VAR'(Name),Var,PM),
   %  (get_varname_list(Vs)->true;Vs=[]),
  %   append(Vs,[Name=Var],NVs),
  %   nput_variable_names( NVs),
     N2 is N + 1, fixvars(PM,N2,VARS,PO).




non_empty_atom(A1):- atom(A1),atom_length(A1,AL),!,AL>0.

:- meta_predicate(sexpr_sterm_to_pterm(+,?,?)).
:- meta_predicate(sexpr_sterm_to_pterm_list(+,?,?)).

is_relation_sexpr('=>').
is_relation_sexpr('<=>').
is_relation_sexpr('==>').
is_relation_sexpr('<==>').
is_relation_sexpr('not').
is_relation_sexpr(typeGenls).

is_va_relation('or').
is_va_relation('and').
%= 	 	 


is_exact_symbol(N,_):- \+ atom(N),!,fail.
is_exact_symbol(N,P):- nonvar(P),!,is_exact_symbol(N,PP),zalwayz(P=PP).
is_exact_symbol(':-',':-').
is_exact_symbol('?-','?-').
is_exact_symbol('??',_).

%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

maybe_var(S,Name,'$VAR'(Name)):- S=='?',atom(Name),!.

%% sexpr_sterm_to_pterm(?VAR, ?V) is det.
%
% S-expression Sterm Converted To Pterm.
%
sexpr_sterm_to_pterm(S,P):- sexpr_sterm_to_pterm(0,S,P).


sexpr_sterm_to_pterm_pre_list(_,STERM,STERM):- \+ compound(STERM), !.
sexpr_sterm_to_pterm_pre_list(_,STERM,STERM):- \+ is_list(STERM), !.
% sexpr_sterm_to_pterm_pre_list(_,[S|STERM],[S|STERM]):- STERM == [], !.
sexpr_sterm_to_pterm_pre_list(TD,[S0|STERM0],[S|STERM]):- 
 (is_list(S0)->sexpr_sterm_to_pterm(TD,S0,S);sexpr_sterm_to_pterm_pre_list(TD,S0,S)),
 sexpr_sterm_to_pterm_pre_list(TD,STERM0,STERM).

sexpr_sterm_to_pterm(_TD,VAR,VAR):-is_ftVar(VAR),!.
sexpr_sterm_to_pterm(_TD,S,P):- is_exact_symbol(S,P),!.
sexpr_sterm_to_pterm(_TD,'#'(S),P):- is_exact_symbol(S,P),!.
sexpr_sterm_to_pterm(_TD,VAR,'$VAR'(Name)):- atom(VAR),svar(VAR,Name),!.

% sexpr_sterm_to_pterm(TD,List,PTERM):- append(Left,[S,Name|TERM],List),maybe_var(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_sterm_to_pterm(TD,NewList,PTERM).
% sexpr_sterm_to_pterm(TD,[S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_sterm_to_pterm_list(TD,[S|TERM],PTERM),!.
%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- \+ atom(S),sexpr_sterm_to_pterm_list(TD,[S|TERM],PTERM),!.
/*
sexpr_sterm_to_pterm(TD,[S,Vars|TERM],PTERM):- nonvar(S),
   call_if_defined(common_logic_snark:is_quantifier(S)),
   zalwayz((sexpr_sterm_to_pterm_list(TD,TERM,PLIST),
   PTERM=..[S,Vars|PLIST])),!.
*/

sexpr_sterm_to_pterm(TD,[S|STERM0],PTERM):- var(S),  TD1 is TD + 1, sexpr_sterm_to_pterm_pre_list(TD1,STERM0,STERM), sexpr_sterm_to_pterm_list(TD1,STERM,PLIST),s_univ(TD,PTERM,[S|PLIST]),!.
sexpr_sterm_to_pterm(_,[S,STERM0],PTERM):- is_quoter(S),sexpr_sterm_to_pterm_pre_list(0,STERM0,STERM), !,PTERM=..[S,STERM],!.
sexpr_sterm_to_pterm(_,[S|STERM0],PTERM):- is_quoter(S),sexpr_sterm_to_pterm_pre_list(0,STERM0,STERM), !,PTERM=..[S,STERM],!.
sexpr_sterm_to_pterm(TD,[S|STERM0],PTERM):- sexpr_sterm_to_pterm_pre_list(TD,STERM0,STERM), is_list(STERM),
  next_args_are_lists_unless_string(S,NonList),
  length(LEFT,NonList),append(LEFT,[List|RIGHT],STERM),is_list(List),
  TD1 is TD+1,
  sexpr_sterm_to_pterm_list(TD1,LEFT,PLEFTLIST),  
  sexpr_sterm_to_pterm_list(0,RIGHT,PRIGHTLIST),
  append(PLEFTLIST,[List|PRIGHTLIST],PLIST),
  s_univ(TD,PTERM,[S|PLIST]),!.

sexpr_sterm_to_pterm(TD,STERM0,PTERM):- TD1 is TD+1,sexpr_sterm_to_pterm_pre_list(TD,STERM0,STERM), 
  is_list(STERM),!, sexpr_sterm_to_pterm_list(TD1,STERM,PLIST),s_univ(TD,PTERM,PLIST),!.
sexpr_sterm_to_pterm(_TD,VAR,VAR).

is_quoter('#BQ').
is_quoter('#COMMA').
is_quoter('quote').

next_args_are_lists_unless_string(defmacro,1).
next_args_are_lists_unless_string(defun,1).
next_args_are_lists_unless_string(let,0).
next_args_are_lists_unless_string('let*',0).

%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- (number(S);  (atom(S),fail,atom_concat_or_rtrace(_,'Fn',S))),sexpr_sterm_to_pterm_list(TD,[S|TERM],PTERM),!.            
%sexpr_sterm_to_pterm(TD,[S],O):- is_ftVar(S),sexpr_sterm_to_pterm(TD,S,Y),!,s_univ(TD,O,[Y]),!.
%sexpr_sterm_to_pterm(TD,[S],O):- nonvar(S),sexpr_sterm_to_pterm(TD,S,Y),!,s_univ(TD,O,[Y]),!.
%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- is_ftVar(S), sexpr_sterm_to_pterm_list(TD,TERM,PLIST),s_univ(TD,PTERM,[t,S|PLIST]),!.
%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- \+ atom(S), sexpr_sterm_to_pterm_list(TD,TERM,PLIST),s_univ(TD,PTERM,[t,S|PLIST]),!.
%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- S==and,!,zalwayz((maplist(sexpr_sterm_to_pterm,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
% sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- is_va_relation(S),!,zalwayz((maplist(sexpr_sterm_to_pterm,TERM,PLIST),list_to_conjuncts(S,PLIST,PTERM))).
%sexpr_sterm_to_pterm(TD,[S|TERM],PTERM):- is_relation_sexpr(S),zalwayz((sexpr_sterm_to_pterm_list(TD,TERM,PLIST),PTERM=..[S|PLIST])),!.
%sexpr_sterm_to_pterm(TD,STERM,PTERM):- STERM=..[S|TERM],sexpr_sterm_to_pterm_list(TD,TERM,PLIST),s_univ(TD,PTERM,[S|PLIST]),!.


s_univ(1,S,S):-!.
s_univ(_TD,P,[F|ARGS]):- atom(F),is_list(ARGS),length(ARGS,A),l_arity(F,A),P=..[F|ARGS].
s_univ(0,P,[F|ARGS]):- atom(F),is_list(ARGS),P=..[F|ARGS].
s_univ(_TD,P,[F|ARGS]):- atom(F),is_list(ARGS),P=..[F|ARGS].
s_univ(_TD,P,S):-P=S.

l_arity(F,A):- clause_b(arity(F,A)).
l_arity(function,1).
l_arity(quote,1).
l_arity('#BQ',1).
l_arity(F,A):-current_predicate(F/A).
l_arity(_,1).

%% sexpr_sterm_to_pterm_list(TD, ?VAR, ?VAR) is det.
%
% S-expression Converted To Pterm List.
%

sexpr_sterm_to_pterm_list(TD,TERM,PTERMO):- is_list(TERM),append(BEFORE,[VAR],TERM),atom(VAR),
  atom_concat_or_rtrace('@',RVAR,VAR),non_empty_atom(RVAR),svar_fixvarname(RVAR,V),!,append(BEFORE,'$VAR'(V),PTERM),
  sexpr_sterm_to_pterm_list0(TD,PTERM,PTERMO).
sexpr_sterm_to_pterm_list(TD,TERM,PTERM):- sexpr_sterm_to_pterm_list0(TD,TERM,PTERM).

sexpr_sterm_to_pterm_list0(_,VAR,VAR):-is_ftVar(VAR),!.
sexpr_sterm_to_pterm_list0(_,[],[]):-!.
sexpr_sterm_to_pterm_list0(TD,[S|STERM],[P|PTERM]):-sexpr_sterm_to_pterm(TD,S,P),sexpr_sterm_to_pterm_list0(TD,STERM,PTERM),!.
sexpr_sterm_to_pterm_list0(_,VAR,VAR).


/*===================================================================
% input_to_forms/3 does less consistancy checking then conv_to_sterm

Always a S-Expression: 'WFFOut' placing variables in 'VARSOut'

|?-input_to_forms(`(isa a b)`,Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- input_to_forms(`(isa a (b))`,Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-input_to_forms(`(list a b )`,Clause,Vars)
Clause = [list,a,b]
Vars = _h70

?- input_to_forms_debug("(=> (isa ?NUMBER ImaginaryNumber) (exists (?REAL) (and (isa ?REAL RealNumber) (equal ?NUMBER (MultiplicationFn ?REAL (SquareRootFn -1))))))").

?- input_to_forms_debug("(=> (isa ?PROCESS DualObjectProcess) (exists (?OBJ1 ?OBJ2) (and (patient ?PROCESS ?OBJ1) (patient ?PROCESS ?OBJ2) (not (equal ?OBJ1 ?OBJ2)))))").


| ?- input_to_forms(`(genlMt A ?B)`,Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- input_to_forms(`
 (goals Iran  (not   (exists   (?CITIZEN)   
  (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN)))))`
 ).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]

| ?- input_to_forms_debug(`
(queryTemplate-Reln QuestionTemplate definitionalDisplaySentence 
       (NLPatternList 
           (NLPattern-Exact "can you") 
           (RequireOne 
               (NLPattern-Word Acquaint-TheWord Verb) 
               (NLPattern-Word Tell-TheWord Verb)) 
           (RequireOne 
               (NLPattern-Exact "me with") 
               (NLPattern-Exact "me what")) 
           (OptionalOne 
               (WordSequence "the term") "a" "an") 
           (NLPattern-Template NPTemplate :THING) 
           (OptionalOne "is" ) 
           (OptionalOne TemplateQuestionMarkMarker)) 
       (definitionalDisplaySentence :THING ?SENTENCE)) `
).

| ?- input_to_forms_debug(`
 (#$STemplate #$bioForProposal-short 
  (#$NLPatternList (#$NLPattern-Template #$NPTemplate :ARG1) 
   (#$NLPattern-Exact "short bio for use in proposals" ) (#$NLPattern-Word #$Be-TheWord #$Verb) 
      (#$NLPattern-Exact "") (#$NLPattern-Template #$NPTemplate :ARG2)) (#$bioForProposal-short :ARG1 :ARG2))`
 ).
 
input_to_forms_debug("(=> (disjointDecomposition ?CLASS @ROW) (forall (?ITEM1 ?ITEM2) (=> (and (inList ?ITEM1 (ListFn @ROW)) (inList ?ITEM2 (ListFn @ROW)) (not (equal ?ITEM1 ?ITEM2))) (disjoint ?ITEM1 ?ITEM2))))").


input_to_forms_debug(
`
 (#$STemplate #$bioForProposal-short 
  (#$NLPatternList (#$NLPattern-Template #$NPTemplate :ARG1) 
   (#$NLPattern-Exact "short bio for use in proposals" ) (#$NLPattern-Word #$Be-TheWord #$Verb) 
      (#$NLPattern-Exact "") (#$NLPattern-Template #$NPTemplate :ARG2)) (#$bioForProposal-short :ARG1 :ARG2)) `
 ).

% txt_to_codes("(documentation Predicate EnglishLanguage \"A &%Predicate is a sentence-forming &%Relation. Each tuple in the &%Relation is a finite, ordered sequence of objects. The fact that a particular tuple is an element of a &%Predicate is denoted by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the objects so related. In the case of &%BinaryPredicates, the fact can be read as `arg_1 is *predicate* arg_2' or `a *predicate* of arg_1 is arg_2'.\")",X).
input_to_forms_debug("(documentation Predicate EnglishLanguage \"A &%Predicate is a sentence-forming &%Relation. Each tuple in the &%Relation is a finite, ordered sequence of objects. The fact that a particular tuple is an element of a &%Predicate is denoted by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the objects so related. In the case of &%BinaryPredicates, the fact can be read as `arg_1 is *predicate* arg_2' or `a *predicate* of arg_1 is arg_2'.\")",X,Y).

// ==================================================================== */
:- export(current_input_to_forms/2).
 	 

%% input_to_forms( ?FormsOut, ?Vars) is det.
%
% Input Converted To Forms.
%
current_input_to_forms(FormsOut,Vars):- 
    current_input(In),
    input_to_forms(In, FormsOut,Vars).

show_wff_debug(Wff,Vs):- nonvar(Wff),Wff=(H=B),!,show_wff_debug((H:-B),Vs).
show_wff_debug(Wff,Vs):- fmt("\n"),
  must_or_rtrace(portray_clause_w_vars(Wff,Vs,[])),!.  

% input_to_forms_debug(String):- sumo_to_pdkb(String,Wff),dbginfo(Wff),!.
input_to_forms_debug(String):-
  input_to_forms_debug(String,['=']).

input_to_forms_debug(String,M:Decoders):-
  setup_call_cleanup(
    fmt("% ========================\n"),
    (get_varnames(Was), show_wff_debug(input=String,Was),
     input_to_forms(String,Wff,Vs),
     b_setval('$variable_names',Vs),
     show_wff_debug(to_forms=Wff,Vs),
     do_decoders(Wff,Vs,M,Decoders),!,
     ignore((nonvar(Vs),Vs\==[], show_wff_debug(vars=Vs,Vs)))),
    fmt("\n% ========================\n")).

do_decoders(_,_,_,[]):-!.
do_decoders(Wff,Vs,M,[Decoder|Decoders]):- !,  
  ((M:call(Decoder,Wff,WffO), ignore((Wff \== WffO , show_wff_debug((M:Decoder:-WffO),Vs))))
  -> do_decoders(WffO,Vs,M,Decoders)
  ; 
  (fmt(decoder_failed(M:Decoder)),
   do_decoders(Wff,Vs,M,Decoders))). 
do_decoders(Wff,Vs,M,Decoder):- do_decoders(Wff,Vs,M,[Decoder]).  
  
:- export(input_to_forms/2).
%% input_to_forms( ?In, ?FormsOut) is det.
%
% Get Input Converted To Forms.
%
input_to_forms(Codes,FormsOut):- 
  input_to_forms(Codes,FormsOut,Vars) ->
  add_variable_names(Vars).	 	 

:- export(input_to_forms/3).

%% input_to_forms( ?In, ?FormsOut, ?Vars) is det.
%
% Get Input Converted To Forms.
%
input_to_forms(Codes,FormsOut,Vars):- 
  push_varnames(_) ->
  quietly_sreader((input_to_forms0(Codes,FormsOut,Vars))).
  
is_variable_names_safe(Vars):- var(Vars),!.
is_variable_names_safe([N=V|Vars]):- !,
   is_name_variable_safe(N,V) -> 
   is_variable_names_safe(Vars).
is_variable_names_safe([]).

is_name_variable_safe(N,V):- 
  ok_var_name(N)-> var(V).
     

get_varnames(Was):- nb_current('$variable_names',Was)->true;Was=[].

push_varnames(New):-  
  (nonvar(New)-> b_setval('$variable_names',New)
    ; (get_varnames(Was), Was = New, b_setval('$variable_names',Was))).

add_variable_names(Vars):- var(Vars),!.
add_variable_names(N=V):- !, ignore(set_varname_s(N,V)).
add_variable_names([NV|Vars]):- add_variable_names(NV),!, add_variable_names(Vars).
add_variable_names([]).

set_varname_s(N,V):- get_varnames(Was), set_varname4(Was,N,V,New),b_setval('$variable_names',New).

set_varname4(Was,N,V,New):- member(NV,Was),NV=(NN=VV), NN==N,!, (V=VV->true;setarg(2,NV,V)), New = Was.
set_varname4(Was,N,V,New):- member(NV,Was),NV=(NN=VV), VV==V,!, (N=NN->true;setarg(1,NV,N)), New = Was.
set_varname4(Was,N,V,[N=V|Was]).


set_variable_names_safe(Vars):-
  is_variable_names_safe(Vars)->
  b_setval('$variable_names',Vars); true.

input_to_forms0(Codes,FormsOut,Vars):- 
    % is_openable(Codes),!,
    parse_sexpr(Codes, Forms0),!,
    once((to_untyped(Forms0, Forms1),
      extract_lvars(Forms1,FormsOut,Vars))).

input_to_forms0(Forms,FormsOut,Vars):-
    (to_untyped(Forms, Forms1) ->
    extract_lvars(Forms1,FormsOut,Vars)-> true),!.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lisprolog -- Interpreter for a simple Lisp. Written in Prolog.
    Written Nov. 26th, 2006 by Markus Triska (triska@gmx.at).
    Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:- style_check(-singleton).
%:- style_check(-discontiguous).
% :- style_check(-atom).
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


tstl:- tstl('./ontologyportal_sumo/Merge.kif'),
         tstl('./ontologyportal_sumo/Translations/relations-en.txt'),
         tstl('./ontologyportal_sumo/english_format.kif'),
         tstl('./ontologyportal_sumo/domainEnglishFormat.kif'),
         tstl('./ontologyportal_sumo/Mid-level-ontology.kif'),
         !.

writeqnl(O):-writeq(O),nl.



:- fixup_exports.
:- endif.

