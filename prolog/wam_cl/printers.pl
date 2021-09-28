/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(print, []).

:- include('./header').


% Grammar rules for printing Lisp s-expressions.
% Given a list of tokens, lisplist does all the nesting of lists

trim_full_stop(SPClosure,TSPClosure):-atom_concat_or_rtrace(SPClosureN,'\n',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,TSPClosure):-atom_concat_or_rtrace(SPClosureN,' ',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,SPClosureN):-atom_concat_or_rtrace(SPClosureN,'.',SPClosure).
trim_full_stop(SPClosure,SPClosure).

lisp_chars_to_pl_string(Str,Str):- string(Str),!.
lisp_chars_to_pl_string(Str,SS):- \+ is_list(Str),!,always((atom_chars(Str,Codes),text_to_string(Codes,SS))).
lisp_chars_to_pl_string(List,SS):- notrace(catch(((must_maplist(to_prolog_char,List,Codes),!,text_to_string(Codes,SS))),_,fail)).

shrink_lisp_strings(Str,PStr):- \+ compound(Str),!,Str=PStr.
%shrink_lisp_strings(Str,PStr):- is_stringp(Str),!,to_prolog_string(Str,PStr).
shrink_lisp_strings([A|Str],PStr):- is_list(Str),maplist(is_characterp_lisp,[A|Str]),lisp_chars_to_pl_string([A|Str],PStr),!.
shrink_lisp_strings(C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(shrink_lisp_strings,C1O,C2O),
  compound_name_arguments(C2,F,C2O),!. %%$C2=..[F|C2O].
shrink_lisp_strings(Str,Str).


lisp_qchar(X) --> {atom(X),atom_length(X,1),char_type(X,graph),format(atom(NN),'#\\~w',[X])},!,[NN].
lisp_qchar(X) --> {atom(X),atom_length(X,1),atom_codes(X,[Code])},!,sexpr1('#\\'(Code)).
lisp_qchar(X) --> {\+ number(X),!,format(atom(NN),'#\\~w ',[X])},!,[NN].
lisp_qchar(X) --> {number(X),notrace_catch_fail((code_type(X,graph),format(atom(NN),'#\\~s',[[X]])))},!,[NN].
lisp_qchar(X) --> {number(X),code_to_name(X,N),format(atom(NN),'#\\~w ',[N])},!,[NN].
lisp_qchar(X) --> {number(X),format(atom(NN),'#\\U~|~`0t~16r~8+',[X])},!,[NN].
lisp_qchar(X) --> {format(atom(NN),'#\\~w ',[X])},!,[NN].


is_characterp_lisp(X):- compound(X),X='#\\'(_). %,is_characterp(X).

sexpr1(X) --> {is_ftVar(X),(get_var_name(X,N)->format(atom(NN),'~w',[N]);format(atom(NN),'~w',[X]))},!,[NN].
sexpr1(PN) --> {is_pathnamep(PN)},['#P'],{f_namestring(PN,NS)},sexpr1(NS).
sexpr1(Str)--> {is_stringp(Str),to_prolog_string(Str,PStr)},!,[PStr].
sexpr1([]) --> !, ['(',')'].
sexpr1(X)--> {atom(X)},!,[X].
sexpr1(X)--> {\+compound(X),format(atom(NN),'~q',[X])},!,[NN].
sexpr1(['quote', Expression]) --> [''''], !, sexpr1(Expression).
sexpr1(['#COMMA',X]) --> [','],sexpr1(X).
sexpr1(['#BQ',X])--> ['`'],!,sexpr1(X).
sexpr1(['#BQ-COMMA-ELIPSE',X])--> [',@'],sexpr1(X).
sexpr1('#\\'(X))--> lisp_qchar(X).
sexpr1([X|Y]) --> ['('],  sexpr1(X), lisplist(Y,')').
sexpr1([function,Expression]) --> ['#'''], !, sexpr1(Expression).
sexpr1(function(Expression)) --> ['#'''], !, sexpr1(Expression).
sexpr1('$STRING'(X)) --> {format(atom(NN),'~q',[X])},!,[NN].
sexpr1('$COMPLEX'(R,I)) --> ['#C('],sexpr1(R),sexpr1(I),[')'].
sexpr1('$RATIO'(R,I)) --> [''],sexpr1(R),['/'],sexpr1(I),[''].
sexpr1('$NUMBER'(claz_double_float,V)) --> {format_number(O,'d',V)},[O].
sexpr1('$NUMBER'(claz_single_float,V)) --> {format_number(O,'e',V)},[O].
sexpr1('$NUMBER'(claz_long_float,V)) --> {format_number(O,'L',V)},[O]. % SBCL = claz_double_float
sexpr1('$NUMBER'(claz_short_float,V)) --> {format_number(O,'s',V)},[O]. % SBCL = claz_single_float
sexpr1('$NUMBER'(T,V)) --> {format_number(O,T,V)},[O].
sexpr1('$S'(X)) --> ['#S'],sexpr1(X).

sexpr1('$OBJ'('#\\',(X))) --> sexpr1('#\\'(X)).
sexpr1(PClosure) --> {compound(PClosure),functor(PClosure,closure,_),with_output_to(atom(SPClosure),fmt9(PClosure)),trim_full_stop(SPClosure,TSPClosure)}, ['{',TSPClosure,'}.'], !.
sexpr1(Dict) --> {is_dict(Dict,T),Dict=..[_,_|Rest]},!, ['#<'],sexpr1(T),lisplist(Rest,'>').
sexpr1('$OBJ'([T,X])) --> sexpr1('$OBJ'(T,X)).
sexpr1('$OBJ'([T|X])) --> sexpr1('$OBJ'(T,X)).
sexpr1('$OBJ'(T,X)) --> {T==claz_prolog,with_output_to(atom(SPClosure),fmt9(X)),trim_full_stop(SPClosure,TSPClosure)}, 
  ['{',TSPClosure,'}.'], !.
sexpr1('$OBJ'(T,X)) --> {T==claz_function},['#\''],sexpr1(X).
sexpr1('$OBJ'(T,X)) --> {T==claz_vector},['#('],lisplist(X,')').
sexpr1('$OBJ'(T,X)) --> ['#S('],{is_list(X),is_structure_classp(T),claz_to_symbol(T,TP)},sexpr1(TP),lisplist(X,')').
sexpr1('$OBJ'(T,X)) --> ['#S'],{is_list(X),is_structure_classp(T),claz_to_symbol(T,TP)},sexpr1(TP),sexpr1(X).
sexpr1('$OBJ'(claz_package,X)) -->  !,sexpr1(X).
sexpr1('$OBJ'(T,X)) --> ['#<'],{claz_to_symbol(T,TP)},!,sexpr1(TP),sexpr1(X),['>'].
sexpr1('$OBJ'(T,X)) --> ['#<'],!,sexpr1(T),sexpr1(X),['>'].
sexpr1(X) --> {compound_name_arguments(X,F,ARGS)}, ['#<'],[F],lisplist(ARGS,'>').

format_number(O,T,V):-format(atom(S),'~w',[V]),atomic_list_concat([L|RS],'e',S),
   ((RS=[_]) -> atomic_list_concat([L|RS],T,O) ; atomic_list_concat([L,0],T,O)).


lisplist([],EQ) --> [EQ], !.
lisplist([X|Xs],EQ) --> sexpr1(X), !, lisplist(Xs,EQ).
lisplist(X,EQ) --> ['.'], sexpr1(X), [EQ].

wl:init_args(2,format).
f_format(Stream,Fmt,ArgS,Result):-
   (Stream==[]->
      (with_output_to(string(String),lisp_format(Fmt,ArgS)),to_lisp_string(String,Result)) ;
   (with_stream_opt([Stream],lisp_format(Fmt,ArgS)),Result=t)).

lisp_format(Fmt,ArgS):- writeq(lisp_format(Fmt,ArgS)).

% TODO actually redirect stdout
with_stream_opt(_StreamL,Goal):- always(Goal).

f_prin1(X,R):- f_prin1(X,[],R).
f_print(X,R):- f_print(X,[],R).
f_princ(X,R):- f_princ(X,[],R).

f_prin1(X,StreamL,X):- with_stream_opt(StreamL,quietly((copy_term(X,Y),writeExpression(Y)))).
f_princ(X,StreamL,X):- is_stringp(X),!,with_stream_opt(StreamL,(to_prolog_string(X,S),write(S))).
f_princ(X,StreamL,X):- copy_term(X,Y),with_stream_opt(StreamL,(writeExpression(Y),nl)).
f_print(X,StreamL,X):- quietly((f_prin1(X,StreamL,X))),nl.
f_terpri(StreamL,t):-  with_stream_opt(StreamL,nl).
f_write_line(X,StreamL,Y):- with_stream_opt(StreamL,(f_princ(X,StreamL,Y),nl)).

% writeExpression/1 displays a lisp expression
writeExpression(X):- notrace(writeExpression0(X)).
writeExpression0(X):- is_ftVar(X),(get_var_name(X,N)->format('~w~w)',[N,X]);format('~w',[X])),!.
writeExpression0([]):- write('NIL').
% writeExpression(quit):- !, write('Terminating WAM-CL'),nl.
writeExpression0('$COMMENT'([])):- 	writeln(';'),!.
writeExpression0('$COMMENT'(S)):- 	write(';'),writeln(S),!.
writeExpression0('$COMMENT'('#'(S))):- 	write('#|'),write(S),writeln('|#').
writeExpression0('$COMMENT'(S,_,_)):- 	write('#|'),write(S),writeln('|#').
writeExpression0(Expression):-
	sexpr1(Expression, TokenL, []), !, %	write('  '),
	writeTokenL(TokenL).

no_right_padding(')'):-!,fail.
no_right_padding('#'):-!,fail.
no_right_padding('#''').
no_right_padding('#C(').
no_right_padding('`').
no_right_padding('#<').
no_right_padding('#P').
no_right_padding('#\'').
no_right_padding('''').
no_right_padding('#S').
no_right_padding(',').
no_right_padding('(').
no_right_padding(',@').

no_right_padding(S):- atom_concat_or_rtrace(_,'(',S).
no_right_padding(S):- atom_concat_or_rtrace('#',_,S).
%no_right_padding(')').            
no_right_padding(X):-need_right_padding(X),!,fail.
no_right_padding(Atom):- \+ atom(Atom),!,fail.
no_right_padding(Atom):- \+ atom_length(Atom,1),!,fail.
% no_right_padding(Atom):- upcase_atom(Atom,Atom).
need_right_padding('.').

no_left_padding(S):- atom(S), atom_concat_or_rtrace(')',_,S).

maybe_write_space([]):-!.
maybe_write_space([S|_]):- no_left_padding(S),!.
maybe_write_space(_):- write(' ').


writeTokenL([]).
writeTokenL([''|Rest]):- !,writeTokenL3(Rest).
writeTokenL(['{',Code,'}.'|TokenL]):- write({Code}),write('.'), writeTokenL(TokenL).
writeTokenL(['(', ')'|TokenL]):- !,
	write('NIL '),
	writeTokenL(TokenL).
writeTokenL([NRP|TokenL]):- no_right_padding(NRP),!, write(NRP), writeTokenL(TokenL).
writeTokenL([')', '('|TokenL]):- !, writeTokenL([')('|TokenL]).
writeTokenL(['('|TokenL]):- write(' ('), writeTokenL(TokenL).
writeTokenL([')'|TokenL]):- write(') '), writeTokenL(TokenL).
writeTokenL([Token|TokenL]):- writeTokenL2([Token|TokenL]).

writeTokenL2([Token|TokenL]):- atomic(Token), \+ atom(Token),!,
   writeq(Token),
   maybe_write_space(TokenL),
   writeTokenL(TokenL).
writeTokenL2([Token|TokenL]):-
   atom(Token),
   write_atom_obj(Token),!,
   maybe_write_space(TokenL),
   writeTokenL(TokenL).
writeTokenL2([Token|TokenL]):-
   writeq(Token),
   maybe_write_space(TokenL),
   writeTokenL(TokenL).

writeTokenL3([]):-!.
writeTokenL3([Token|TokenL]):- atomic(Token), \+ atom(Token),!,
   writeq(Token),
   writeTokenL3(TokenL).
writeTokenL3([Token|TokenL]):-
   atom(Token),
   write_atom_obj(Token),!,
   writeTokenL3(TokenL).
writeTokenL3([Token|TokenL]):-
   writeq(Token),
   writeTokenL3(TokenL).



write_atom_obj(Atom):- atom(Atom),atomic_list_concat([Type,Named],'_znst_',Atom),
   atomic_concat('claz_',Type,Kind),!,
   write('#<'),write(Kind),write(' '),write(Named),write('>').
write_atom_obj(Symbol):-  print_symbol(Symbol),!.
write_atom_obj(Package):- pl_package_name(Package,Name),!,write('#<PACKAGE '),write(Name),write('>').







wl:lambda_def(defun, write, f_write, [u_object, c38_key, array, u_base, case, u_circle, u_escape, gensym, length, u_level, u_lines, u_miser_width, pprint_dispatch, u_pretty, u_radix, u_readably, u_right_margin, stream], []).
%wl:arglist_info(write, f_write, [u_object, c38_key, array, u_base, case, u_circle, u_escape, gensym, length, u_level, u_lines, u_miser_width, pprint_dispatch, u_pretty, u_radix, u_readably, u_right_margin, stream], arginfo{all:[u_object], allow_other_keys:0, aux:0, body:0, complex:[key], env:0, key:[array, u_base, case, u_circle, u_escape, gensym, length, u_level, u_lines, u_miser_width, pprint_dispatch, u_pretty, u_radix, u_readably, u_right_margin, stream], names:[u_object, array, u_base, case, u_circle, u_escape, gensym, length, u_level, u_lines, u_miser_width, pprint_dispatch, u_pretty, u_radix, u_readably, u_right_margin, stream], opt:0, outer:ReplEnv, req:[u_object], rest:0, sublists:0, whole:0}).
wl:init_args(1, f_write).
f_write(Object_In, RestNKeys, FnResult) :-
        _59226=[bv(u_object, Object_In), bv(array, Array_In), bv(u_base, Base_In), bv(case, Case_In), bv(u_circle, Circle_In), bv(u_escape, Escape_In), bv(gensym, Gensym_In), bv(length, Length_In), bv(u_level, Level_In), bv(u_lines, Lines_In), bv(u_miser_width, Miser_width_In), bv(pprint_dispatch, Pprint_dispatch_In), bv(u_pretty, Pretty_In), bv(u_radix, Radix_In), bv(u_readably, Readably_In), bv(u_right_margin, Right_margin_In), bv(stream, Stream_In)],
        get_kw(ReplEnv,
               RestNKeys,
               kw_array,
               array,
               Array_In,
               []=Array_In,
               Array_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_base,
               u_base,
               Base_In,
               []=Base_In,
               Base_Present),
        get_kw(ReplEnv, RestNKeys, kw_case, case, Case_In, []=Case_In, Case_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_circle,
               u_circle,
               Circle_In,
               []=Circle_In,
               Circle_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_escape,
               u_escape,
               Escape_In,
               []=Escape_In,
               Escape_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_gensym,
               gensym,
               Gensym_In,
               []=Gensym_In,
               Gensym_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_length,
               length,
               Length_In,
               []=Length_In,
               Length_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_level,
               u_level,
               Level_In,
               []=Level_In,
               Level_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_lines,
               u_lines,
               Lines_In,
               []=Lines_In,
               Lines_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_miser_width,
               u_miser_width,
               Miser_width_In,
               []=Miser_width_In,
               Miser_width_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_pprint_dispatch,
               pprint_dispatch,
               Pprint_dispatch_In,
               []=Pprint_dispatch_In,
               Pprint_dispatch_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_pretty,
               u_pretty,
               Pretty_In,
               []=Pretty_In,
               Pretty_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_radix,
               u_radix,
               Radix_In,
               []=Radix_In,
               Radix_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_readably,
               u_readably,
               Readably_In,
               []=Readably_In,
               Readably_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_right_margin,
               u_right_margin,
               Right_margin_In,
               []=Right_margin_In,
               Right_margin_Present),
        get_kw(ReplEnv,
               RestNKeys,
               kw_stream,
               stream,
               Stream_In,
               []=Stream_In,
               Stream_Present),
        catch(( 
                with_stream_opt([Stream_In],fmt(write_f(Object_In, RestNKeys))),
                Object_In=FnResult
              ), block_exit(write, FnResult),
              true),
        _= [Array_Present,Base_Present,Case_Present,Circle_Present,Escape_Present,Gensym_Present,Length_Present,Level_Present,Lines_Present,Miser_width_Present,Pprint_dispatch_Present,Pretty_Present,Radix_Present,Readably_Present,Right_margin_Present,Stream_Present],
        Object_In=FnResult.
        
% :- set_opv(write, symbol_function, f_write).



:- fixup_exports.


