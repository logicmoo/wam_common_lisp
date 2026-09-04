:- module(xml_reader,[fileToLineInfoElements/3]).
/** <module> Utility LOGICMOO XML READER
Allows you to read xml files from prolog. 

- @author Douglas R. Miles
- @license LGPL 
*/
:- use_module(library(sgml)).

atrace:-trace.
useCateID:- fail.
:- dynamic(aimlCateSig/1).
:- dynamic(aimlCate/13).

prolog_mostly_ground(Out):-ground(Out),!.
prolog_mostly_ground(Out):-var(Out),!,atrace.
prolog_mostly_ground([H|_Out]):-!,prolog_must(prolog_mostly_ground1(H)),!.
prolog_mostly_ground(Out):- ((arg(_N,Out,Arg),prolog_must(prolog_mostly_ground1(Arg)),fail));true.
prolog_mostly_ground1(Out):-prolog_must(nonvar(Out)).
 
aimlCateSig(_).


%% get_sgml_parser_defs(PARSER_DEFAULTS,PARSER_CALLBACKS) /*shorttag(false),*/
get_sgml_parser_defs(PARSER_DEFAULTS,PARSER_CALLBACKS):- 
  current_prolog_flag(sgml_parser_defaults,PARSER_DEFAULTS),
  current_prolog_flag(sgml_parser_callbacks,PARSER_CALLBACKS),!.

get_sgml_parser_defs(
  [defaults(false), space(remove),/*number(integer),*/ qualify_attributes(false),
         %call(decl, on_decl),
         %call(pi, on_pi),call(xmlns, on_xmlns),call(urlns, xmlns),
         %%call(error,xml_error),
         dialect(xml)
         ],
         [max_errors(0),call(begin, on_begin),call(end, on_end)]).



% % ?- string_to_structure('<?xml version="1.0" encoding="ISO-8859-1"?>\n<aiml><p>hi</p></aiml>',X).
% % ?- string_to_structure('<category><pattern>_ PLANETS</pattern></category>',X).


on_end('aiml', _) :- !,
        ignore(retract(in_aiml_tag(_))).

on_begin('aiml', Attribs, _) :- !,
        asserta(in_aiml_tag(Attribs)).


on_begin(Tag, Attr, Parser) :- skipOver(not(inLineNum)),
        get_sgml_parser(Parser,context(Context)), Context=[Tag,aiml|_],
        skipOver(debugFmt(on_begin(Tag, Attr, Context))),
        skipOver(retract(in_aiml_tag(AimlAttr))),
       % skipOver(get_sgml_parser_defs(PARSER_DEFAULTS, PARSER_CALLBACKS)),
        get_sgml_parser(Parser,line(Line)),
        get_sgml_parser(Parser,charpos(Offset)),
        get_sgml_parser(Parser,file(File)),
        global_pathname(File,Pathname),
      %  get_sgml_parser(Parser,source(Stream)),
        skipOver(asserta(inLineNum)),
%        load_structure(Stream,Content,[line(Line)|PARSER_DEFAULTS]),!,
 %      skipOver( sgml_parse(Parser,[ document(Content),parse(input)])),
        NEW = t_l:lineInfoElem(Pathname,Line:Offset, Context, element(Tag, Attr, no_content_yet)),
        %%debugFmt(NEW),
        skipOver(ignore(retract(inLineNum))),
        skipOver(asserta(in_aiml_tag(AimlAttr))),
        assertz(NEW),!.

on_begin(_Tag, _Attr, _Parser) :-!. %%get_sgml_parser(Parser,context(Context)),!. %%,debugFmt(on_begin_Context(Tag, Attr, Context)).

%%on_begin_ctx(TAG, URL, Parser, Context) :-!, debugFmt(on_begin_ctx(URL, TAG, Parser,Context)),!.
on_begin_ctx(_TAG, _URL, _Parser, _Context) :- !. %%, debugFmt(on_begin_ctx(URL, TAG, Parser,Context)),!.



:- thread_local
        xmlns/3.

on_xmlns(rdf, URL, _Parser) :- !,debugFmt(on_xmlns(URL, rdf)),asserta(xmlns(URL, rdf, _)).
on_xmlns(TAG, URL, _Parser) :- sub_atom(URL, _, _, _, 'rdf-syntax'), !,
        debugFmt('rdf-syntax'(URL, TAG)),
        immediateCall(_Ctx,asserta(xmlns(URL, rdf, _))).
on_xmlns(TAG, URL, _Parser) :- debugFmt(on_xmlns(URL, TAG)).

on_decl(URL, _Parser) :- debugFmt(on_decl(URL)).
on_pi(URL, _Parser) :- debugFmt(on_pi(URL)).


xml_error(TAG, URL, Parser) :- !, debugFmt(xml_error(URL, TAG, Parser)).
% ============================================
% Loading content
% ============================================

load_aiml_structure_lineno(Attributes,Ctx,L):-must_maplist(load_inner_aiml_lineno(Attributes,Ctx),L),!.

:-thread_local(t_l:lineInfoElem/4).

load_inner_aiml_lineno(Attributes,Ctx,element(Tag,Attribs,ContentIn)):-
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   load_aiml_structure(Ctx,element(Tag,RightAttribs,ContentIn)),!.

/*
%% offset
load_inner_aiml_lineno(Attributes,Ctx,element(Tag,Attribs,ContentIn)):-
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   prolog_must(attributeValue(Ctx,RightAttribs,[srcfile,srcdir],File,'$error')),
   MATCH = t_l:lineInfoElem(File,Line:Offset, Context, element(Tag, Attribs, no_content_yet)),
   ignore(MATCH),                                            
   Context=[_Tag0,aiml|_More],
   ignore(Line = nonfile),
   ignore(Offset = nonfile),
   NewAttribs  = [srcfile=File,lineno=Line:Offset|RightAttribs],
   ignore(retract(MATCH)),
   load_aiml_structure(Ctx,element(Tag,NewAttribs,ContentIn)),!.
*/
   /*

   load_inner_aiml_lineno(Attributes,Ctx,element(Tag,Attribs,ContentIn)):-
   prolog_must(current_value(Ctx,srcfile,File)),
   retract((t_l:lineInfoElem(File0,Line0:Offset0,graph, element(_Tag0, _Attr0, _Content0)))),
   prolog_must(call(OLD)),

   MATCH = t_l:lineInfoElem(File,Line:Offset,Context, element(Tag, Attribs, _ContentIn)),!,
   prolog_must((call(MATCH),!,not(not((Line:Offset)==(Line0:Offset0))),retract(OLD),
   load_aiml_structure(Ctx,element(Tag,[srcinfo=File0:Line0-Offset0|Attribs],ContentIn)),
        NEW = t_l:lineInfoElem(File,Line:Offset,Attributes, element(Tag, Attribs, ContentIn)),
        assertz(NEW))),!.

   */





tls :- string_to_structure('<aiml><p>hi</p></aiml>',X),debugFmt(X).
tls2 :- string_to_structure('<?xml version="1.0" encoding="ISO-8859-1"?>\n<aiml><p>hi</p></aiml>\n\n',X),debugFmt(X).

string_to_structure(String,XMLSTRUCTURESIN):- fail, sformat(Strin0,'<pre>~s</pre>',[String]),string_to_structure0(Strin0,XMLSTRUCTURES),!,  
   prolog_must([element(pre,[],XMLSTRUCTURESIN)]=XMLSTRUCTURES).
   
string_to_structure(String,XMLSTRUCTURES):- string_to_structure0(String,XMLSTRUCTURES),!.
string_to_structure0(String,XMLSTRUCTURES):- 
     %%get_sgml_parser_defs(PARSER_DEFAULTS,_PARSER_CALLBACKS),
     PARSER_DEFAULTS = [defaults(false), space(remove),/*number(integer),*/ qualify_attributes(false),dialect(xml)],
     string_to_structure0(String,PARSER_DEFAULTS,XMLSTRUCTURES),!.

string_to_structure(String,PARSER_DEFAULTS0,XMLSTRUCTURES):-string_to_structure0(String,PARSER_DEFAULTS0,XMLSTRUCTURES).

string_to_structure0(String,PARSER_DEFAULTS0,XMLSTRUCTURESIN):-
        setup_call_cleanup(((string_to_stream(String,In),new_sgml_parser(Parser, []))),
          prolog_must((                     
           atom_length(String,Len),
           append(PARSER_DEFAULTS0,[],PARSER_DEFAULTS),
           must_maplist(set_sgml_parser(Parser),PARSER_DEFAULTS),
           string_parse_structure(Len, Parser, user:PARSER_DEFAULTS, XMLSTRUCTURES, In)
           )),
       (free_sgml_parser(Parser),close(In))),!,prolog_must(XMLSTRUCTURESIN=XMLSTRUCTURES).

string_parse_structure(Len,Parser, M:Options, Document, In) :-
	quietly((catch(call(call,string_parse_structure_opts_547(Parser),In,M,Options,Options2),_,string_parse_structure_opts(Parser,In,M,Options,Options2)))),
        % quietly((string_parse_structure_opts(Parser,In,M,Options,Options2))),
	sgml:sgml_parse(Parser,
		   [ document(Document),
		     source(In),
                     parse(input),
                     content_length(Len)
		   | Options2
		   ]).

/*
string_parse_structure_opts_547(Parser, _In, _M, Options,Options2):-
	sgml:set_parser_options(Parser, Options, Options1),
	Options2=Options1.
*/

string_parse_structure_opts(Parser,In,M,Options,Options2):-
	sgml:set_parser_options(Options, Parser, In, Options1),
        sgml:parser_meta_options(Options1, M, Options2).



fileToLineInfoElements(Ctx,File,XMLSTRUCTURES):-
    atom_concat(File,'.term',Elis),
     ((fail,file_newer(Elis,File)) ->  
      termFileContents(Ctx,Elis,XMLSTRUCTURES) ;
       fileToLineInfoElements0(Ctx,File,XMLSTRUCTURES)).


termFileContents(_Ctx,File,termFileContents(File)):-!. %%,atrace.
termFileContents(_Ctx,File,element(aiml,[],XMLSTRUCTURES)):- %% another way to fileToLineInfoElements
   setup_call_cleanup((open(File, read, In, [])), 
      findall(Elem,((repeat,read(In,Elem),((Elem\=end_of_file)->true;!))),XMLSTRUCTURES), close(In)),!.




% gather line numbers
fileToLineInfoElements0(Ctx,F0,XMLSTRUCTURES):-
   global_pathname(F0,File),
       retractall(t_l:lineInfoElem(File,_,_,_)),
        setup_call_cleanup((open(File, read, In, [type(binary)]),new_sgml_parser(Parser, [])),

          prolog_must((           
           get_sgml_parser_defs(PARSER_DEFAULTS,PARSER_CALLBACKS),
           must_maplist(set_sgml_parser(Parser),[file(File)|PARSER_DEFAULTS]),
           %% todo offset(Offset)
           sgml_parse(Parser,[source(In)|PARSER_CALLBACKS]))),

        (free_sgml_parser(Parser),close(In))),!,


        fileToLineInfoElements2(Ctx,File,XMLSTRUCTURES).


% gather line contents
fileToLineInfoElements2(Ctx,File,XMLSTRUCTURES):-!,
  get_sgml_parser_defs(PARSER_DEFAULTS,_PARSER_CALLBACKS),
  setup_call_cleanup(open(File, read, In, [type(binary)]),(load_structure(In,Whole, [file(File)|PARSER_DEFAULTS]),!,
   load_inner_aiml_w_lineno(File,[],[],[],Ctx,Whole,XMLSTRUCTURES)),close(In)),!.

load_inner_aiml_w_lineno(_SrcFile,_OuterTag,_Parent,_Attributes,_Ctx,Atom,Atom):-(atomic(Atom);var(Atom)),!.
load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,Ctx,[H|T],LL):-!,
      must_maplist(load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,Ctx),[H|T],LL),!.

% % offset
load_inner_aiml_w_lineno(SrcFile,[OuterTag|PREV],Parent,Attributes,Ctx,element(Tag,Attribs,ContentIn),element(Tag,NewAttribs,ContentOut)):-
   Context=[Tag,OuterTag|_],
   MATCH = t_l:lineInfoElem(SrcFile,Line:Offset, Context, element(Tag, Attribs, no_content_yet)),
   MATCH,!,
   ignore(Line = nonfile),
   ignore(Offset = nonfile),
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   % % Src = element(Tag,Attribs,ContentIn),
   Src = nosrc,
   appendAttributes(Ctx,[srcfile=SrcFile:Line-Offset,srcinfo=Src],RightAttribs,NewAttribs),
   ignore(retract(MATCH)),
   (member(Tag,[aiml,topic]) ->  NextAttribs = NewAttribs ; NextAttribs = []),
   must_maplist(load_inner_aiml_w_lineno(SrcFile,[Tag,OuterTag|PREV],Parent,NextAttribs,Ctx),ContentIn,ContentOut),!.

load_inner_aiml_w_lineno(SrcFile,MORE,Parent,Attributes,Ctx,element(Tag,Attribs,ContentIn),element(Tag,RightAttribs,ContentOut)):-
   appendAttributes(Ctx,Attributes,Attribs,RightAttribs),
   load_inner_aiml_w_lineno(SrcFile,[Tag|MORE],Parent,[],Ctx,ContentIn,ContentOut),!.

load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,_Ctx,L,L):-
   aiml_error(load_inner_aiml_w_lineno(SrcFile,OuterTag,Parent,Attributes,L)).


addAttribsToXML(Attribs,element(Tag,Pre,Content),element(Tag,Post,Content)):-appendAttributes(_Ctx,Pre,Attribs,Post),!.
addAttribsToXML(Attribs,[H|T],OUT):-must_maplist(addAttribsToXML(Attribs),[H|T],OUT),!.
addAttribsToXML(Attribs,OUT,OUT):-!,debugFmt(addAttribsToXML(Attribs,OUT,OUT)),!.


appendAttributes(_Ctx,L,R,AA):-hotrace((mergeAppend0(L,R,A),list_to_set_safe(A,AA))),!.
mergeAppend0(L,R,R):-var(L),!,var(R),!.
mergeAppend0(L,R,A):-var(R),append(L,R,A),!.
mergeAppend0(L,R,A):-var(L),append(L,R,A),!.
mergeAppend0(L,[R|RR],A):-eqmember(R,L),mergeAppend0(L,RR,A).
mergeAppend0([L|LL],R,A):-eqmember(L,R),mergeAppend0(LL,R,A).
mergeAppend0(L,R,A):-append(L,R,A).

eqmember(E,List):-copy_term_numvars(E:List,E0:List0),member(E0,List0).

list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


lastMember(E,List):-hotrace(lastMember0(E,List)).

lastMember0(_E,List):-var(List),!,fail.
lastMember0(E,[H|List]):-lastMember0(E,List);E=H.

lastMember(E,List,Rest):-hotrace(lastMember0(E,List,Rest)).

lastMember0(E,List,Rest):-lastMember0(E,List),!,delete_safe(List,E,Rest),!.
lastMember0(E,List,Rest):-lastMember0(EE,List),!,lastMember0(E,EE,Rest),!,atrace. %%delete_safe(List,EE,Rest),!.

delete_safe(List,_E,Rest):-var(List),!,Rest=List.
delete_safe(List,E,Rest):-is_list(List),!,delete(List,E,Rest).
delete_safe([H|List],E,Rest):- H==E,!,delete_safe(List,E,Rest).
delete_safe([H|List],E,[H|Rest]):-delete_safe(List,E,Rest).


getKeyValue(FullList,N=V):-lastMember(N=V,FullList),!.
%%addKeyValue(FullList,N=V):-nonvar(N),!,append(_Closed,[N=V|_],FullList),!.
addKeyValue(FullList,NV):- prolog_must((not(ground(FullList)),nonvar(NV))),append(_Closed,[NV|_],FullList),!.


lastMember2(E,List):-to_open_list(_,Closed,_Open,List),reverse(Closed,Rev),member(E,Rev).

%lastMember(End,List) :- append(_,[End|_],List).


to_open_list(FullList,Closed,Open,FullList) :- append(Closed,Open,FullList),var(Open),!.
to_open_list(Closed,Closed,Open,FullList) :- append(Closed,Open,FullList),!.


copy_term_numvars(OLD,NEW):-copy_term(OLD,NEW),numbervars(NEW,0,_).


error_catch(C,E,F):-E=error(E1,E2),!,catch(C,error(E1,E2),F).
error_catch(C,E,F):-nonvar(E),!,catch(C,E,F).
error_catch(C,E,F):-catch(C,E,(needs_rethrown(E),F)).
needs_rethrown(E):- functor(aiml_goto,E,_),!,throw(E).
needs_rethrown(E):- functor(aiml_novalue,E,_),!,throw(E).
needs_rethrown(_).

hotrace(G):- quietly(G).


:-thread_local(in_aiml_tag/1).
:-thread_local(inLineNum/0).

skipOver(_).

aiml_error(G):- wdmsg(aiml_error(G)),!.

immediateCall(_,G):- trace,call(G).

:- use_module(library(memfile)).

%% Use a memory-file. The resulting handling is closed using close/1. 
string_to_stream(String,InStream):- string(String),string_to_atom(String,Atom),!,string_to_stream(Atom,InStream).
string_to_stream(Atom,InStream):- atom_to_memory_file(Atom, Handle),open_memory_file(Handle,read,InStream).


% \n\n\n
load_aiml_structure(Ctx,O):-atomic(O),!,debugFmt(load_aiml_structure(Ctx,O)),!.


:- fixup_exports.
