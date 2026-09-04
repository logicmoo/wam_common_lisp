/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_filestreams.pl
:- module(logicmoo_util_filestreams,
          [ copy_stream/2,
            file_to_stream/2,
            file_to_stream_ssl_verify/5,
            ensure_loaded_with/2,
            when_file_output/1,
            l_open_input/2,
            l_open_input0/2,
            l_open_input1/2,
      %      make_socket/3,
         reread_vars/3,
         ensure_translated_with/3,
      %      negotiate_http_connect/2,
       %     ssl_failed/3,
       %     ssl_protocol_hook/4,
            text_to_stream/2,
            l_open_output/2,
            is_openable/1,
            with_stream_pos/2
          ]).

:- meta_predicate(ensure_loaded_with(:,3)).
% % % OFF :- system:use_module(library(logicmoo_startup)).
% % % OFF :- system:use_module((filesystem)).

:- multifile
        thread_httpd:accept_hook/2,
        thread_httpd:make_socket_hook/3,
        thread_httpd:open_client_hook/5,
        http:open_options/2,
        package_path/2.
:- meta_predicate((
        with_stream_pos(+, 0),
        when_file_output(0),
        ensure_translated_with(:,0,?))).
:- module_transparent
        copy_stream/2,
        file_to_stream/2,
        file_to_stream_ssl_verify/5,
        ensure_translated_with/3,
        l_open_input/2,
        l_open_input0/2,
        l_open_input1/2,
     %   make_socket/3,
     %   negotiate_http_connect/2,
        package_path/2,
     %   ssl_failed/3,
     %   ssl_protocol_hook/4,
        text_to_stream/2.

:- set_module(class(library)).

protected_op(_,',').
with_operators(Ops,Goal):- setup_call_cleanup(push_operators(Ops,Undo),Goal,pop_operators(Undo)).
with_no_operators(Goal):- setof(op(0,Y,Z),X^(current_op(X,Y,Z),\+ protected_op(Y,Z)),Zero), with_operators(Zero,Goal).
with_only_operators(Ops,Goal):-  with_no_operators(with_operators(Ops,Goal)).


% :- use_module(library(gui_tracer)).
:- use_module(library(system)).
:- use_module(library(socket)).
:- use_module(library(readutil)).
:- abolish(system:time/1).
:- use_module(library(statistics)).
:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(gensym)).
:- use_module(library(when)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
%:- use_module(library(editline)).
:- use_module(library(listing)).

:- meta_predicate each_single(2,*).
:- meta_predicate translate_file_stream(3,*,*,*,*).
:- meta_predicate translate_file0(3,*,*,*,*),translate_file(3,*,*).
:- meta_predicate reread_vars(*,2,*).


l_open_output(File,Out):- \+ atom(File), absolute_file_name(File,Name),!,open(Name,write,Out,[alias(Name)]).
l_open_output(File,Out):- is_stream(File),!,Out=File.
l_open_output(File,Out):- stream_property(Out,alias(File)),!.
l_open_output(File,Out):- stream_property(Out,file_name(File)),!.
l_open_output(File,Out):- absolute_file_name(File,Name),!,open(Name,write,Out,[alias(Name)]).

translate_file(With,Module:InF,OutF):-
  translate_file0(With,Module,InF,OutF,[]).

translate_file0(With,Module,InF,OutF,Options):-
  setup_call_cleanup(l_open_input(InF,In),
  setup_call_cleanup(l_open_output(OutF,Out),
                     ((must_be(stream,In),must_be(stream,Out),
                     translate_file_stream(With,Module,In,Out,Options))),
  ( \+ is_stream(OutF)->close(Out);true)),
   ( \+ is_stream(InF)->close(In);true)).

translate_file_stream(With,Module,In,Out,Options):-  
  Key = '$translate_file_steam',
  % Info = translate_file_stream(With,Module,In,Out,Expanded,Options),
  % b_setval(Key,Info),
  % nb_linkval(Key,Info),
  ignore((once(call(With,Key,Info,Out1)),once(write_translation(Out,Out1)))),  
  repeat,
   % trans_read_source_term(Module,In,Wff,Expanded,[variable_names(Vs)|Options]),
   trans_read_source_term(Module,In,Wff,_Expanded,[variable_names(Vs)|Options]),
   once(call(With,Wff-Vs,Info,WffO)),
   once(write_translation(Out,WffO)),
   (end_of_file == Wff; end_of_file == WffO),!.


trans_read_source_term(M, In, Term, Term, Options):- !,
   read_clause(In, Term, [ module(M)| Options ]).

:- ensure_loaded(library(prolog_source)).
trans_read_source_term(M, In, Term, Expanded, Options) :-
    prolog_source:maplist(prolog_source:read_clause_option, Options),
    !,
 prolog_source:(
    select_option(subterm_positions(TermPos), Options,
                  RestOptions, TermPos),
    read_clause(In, Term,
                [ subterm_positions(TermPos)
                | RestOptions
                ]),
    logicmoo_util_filestreams:expand_unlikely(Term, TermPos, In, Expanded),
    update_state(Term, Expanded, M)).
trans_read_source_term(M, In, Term, Expanded, Options) :-
 prolog_source:(
    select_option(syntax_errors(SE), Options, RestOptions0, dec10),
    select_option(subterm_positions(TermPos), RestOptions0,
                  RestOptions, TermPos),
    (   style_check(?(singleton))
    ->  FinalOptions = [ singletons(warning) | RestOptions ]
    ;   FinalOptions = RestOptions
    ),
    read_term(In, Term,
              [ module(M),
                syntax_errors(SE),
                subterm_positions(TermPos)
              | FinalOptions
              ]),
    logicmoo_util_filestreams:expand_unlikely(Term, TermPos, In, Expanded),
    update_state(Term, Expanded, M)).

expand_unlikely(Term, TermPos, In, Expanded):-
  b_setval('$term', Term),
  prolog_source:expand(Term, TermPos, In, Expanded),
  b_setval('$term', []).

write_translation(Out,Wff):- must_be(nonvar,Wff), is_list(Wff),!,maplist(write_translation(Out),Wff).
write_translation(Out,end_of_file):-!,flush_output(Out),!.
write_translation(Out,flush_output):-!,flush_output(Out),!.
write_translation(_Ut,call(Goal)):-!,call(Goal).
write_translation(Out,Wff-Vs):- !, must(is_list(Vs)), 
  wto(Out,Wff,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)]).
write_translation(Out,Wff):- nb_current('$variable_names',Vs),
  wto(Out,Wff,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)]).

:-thread_initialization(nb_setval('$ra5_often',1)).

:- export(when_file_output/1).
when_file_output(G):- (current_output(X),stream_property(X,file_name(_)))->call(G);true.
%when_file_output(G):- (current_output(X),stream_property(X,alias(user_output)))->true;call(G).

wto(Out,Wff,Opts):- 
 write_term(Out,Wff,Opts),!,
   when_file_output(
   ignore((b_getval('$ra5_often',Often),
   once((((nb_current('$has_var',t);nb_current('$has_quote',t);Often=1;(flag('$ett',X,X+1),0 is X rem Often))))),
   write_term(user_output,Wff,Opts)))).

file_newer(NamePl,Name):- exists_file(NamePl),exists_file(Name), 
  time_file(NamePl,T1),time_file(Name,T2),!,T1>T2.

file_needs_rebuilt(NamePl,Name):- \+ file_newer(NamePl,Name).
file_needs_rebuilt(NamePl,Name):- size_file(NamePl,S1), (S1< 1000 ;
 (size_file(Name,S2), Thresh is S2 * 0.50,!,S1<Thresh)).

ensure_loaded_with(ModuleFile,With):-   
   strip_module(ModuleFile,Module,File),
      absolute_file_name(File,Name),
      ensure_translated_with(ModuleFile,With,NamePl),
      locally(set_prolog_flag(do_renames,never),
      time(Module:load_files([NamePl],[derived_from(Name),if(not_loaded),redefine_module(false),qcompile(auto)]))).

ensure_translated_with(ModuleFile,With,NamePl):-   
   strip_module(ModuleFile,Module,File),
      absolute_file_name(File,Name),
      ignore((var(NamePl),
      file_name_extension(Base,Ext,Name),atomic_list_concat([Base,'-trans.',Ext],NamePl))),
      (file_needs_rebuilt(NamePl,Name)->
        (dmsg(start(translate_file(With,Module:Name,NamePl))),
        translate_file(With,Module:Name,NamePl),
        dmsg(complete(translate_file(With,Module:Name,NamePl))));
        dmsg(unneeded(translate_file(With,Module:Name,NamePl)))).


:- export(with_stream_pos/2).
:- meta_predicate(with_stream_pos(+,0)).

:- export(reread_vars/3).

reread_vars(P-VsIn,SVC,Wff-VsO):-
   wt(string(S),P,VsIn),
   catch(read_term_from_atom(S,Wff,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),
         E,trace_or_throw(error(E,P-VsIn))),
   must(\+ \+ Wff=P),
   maplist(each_single(SVC),Singles),
   subtract_eq(Vs,Singles,VsO).

each_single(CB,N=V):-call(CB,N,V).

%rt(string(In),WffO,VsO):-!,catch(read_term_from_atom(In,Wff,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),E,(dmsg(E),dtrace,fail)),correct_singletons(Wff,WffO,Vs,Singles,VsO).
%rt(In,WffO,VsO):- catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),E,(dmsg(E),dtrace,fail)),correct_singletons(Wff,WffO,Vs,Singles,VsO).
wt(string(O),P,Vs):- !, with_output_to(string(O), write_term(P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)])).
wt(O,P,Vs):- write_term(O,P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)]).
      

%% with_stream_pos( +In, :Goal) is semidet.
%
% If Goal fails or exceptions then the Stream Postion is reset.
%
with_stream_pos(In,Call):-
    must((stream_property(In, position(InitalPos)),
    PS = position(InitalPos))),
    (Call *-> 
       (stream_property(In,position(NewPos)),nb_setarg(1,PS,NewPos)) ; 
       ((arg(1,PS,Pos),set_stream_position_safe(In, Pos),!,fail))).

set_stream_position_safe(In,Pos):- catch(set_stream_position(In,Pos),
   error(permission_error(reposition, stream, In),Cxt),dmsg(warn(error(permission_error(reposition, stream, In),Cxt)))).

:- export(l_open_input/2).
:- export(l_open_input0/2).
:- export(l_open_input1/2).

%= 	 	 

%% l_open_input( ?InS, ?In) is semidet.
%
% (list Version) Open Input.
%
l_open_input(InS,In):-once(must(l_open_input0(InS,In))).


%= 	 	 

%% l_open_input0( ?In, ?InS) is semidet.
%
% (list Version) Open Input Primary Helper.
%
l_open_input0(In,InS):-l_open_input1(In,InS),!.
l_open_input0(InS,In):-string(InS),!,open_string(InS,In).
l_open_input0(Filename,In) :- \+ is_list(Filename),nonvar(Filename),filematch(Filename,File), file_open_read(File,In).
l_open_input0(InS,In):-!,open_string(InS,In).




is_openable(In):-ground(In),is_openable1(In),!.
is_openable1(In):-is_stream(In).
is_openable1(In):-string(In).
is_openable1(In):-exists_source(In).
is_openable1(In):-catch(text_to_string(In,Out),_,fail),!,In\==Out.
is_openable1(In):- compound(In),!,functor(In,F,1),arg(_,v(file,atom,string,alias,codes,chars),F).


%= 	 	 

%% l_open_input1( :TermInS, ?In) is semidet.
%
% (list Version) Open Input Secondary Helper.
%
l_open_input1([V|_],_):-var(V),V=zzzzzzzzzzzzz,!,throw(error(l_open_input/2,'Arguments are not sufficiently instantiated (l_open_input)')).
l_open_input1(InS,In):-is_stream(InS),!,In=InS.
l_open_input1(file(Filename),In) :- filematch(Filename,File), file_open_read(File,In).
l_open_input1(alias(Name),In) :- stream_property(In,alias(Name)),!.
l_open_input1(alias(Filename),In) :-  catch(see(Filename),_,fail),current_input(In).
l_open_input1(string(string(InS)),In):-!,dmsg_text_to_string_safe(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In).
l_open_input1(string(InS),In):-!,open_string(InS,In).
l_open_input1(atom(InS),In):-!,open_string(InS,In).
l_open_input1(codes(InS),In):-!,open_string(InS,In).
l_open_input1(chars(InS),In):-!,open_string(InS,In).


file_open_read(File,In):-catch(see(File),_,fail),current_input(In).
file_open_read(File,In):-catch(open(File,read,In,[]),_,fail),!,see(In),current_input(In).
file_open_read(File,In):-open(File,read,In,[]),!,see(In),current_input(In).

% % % OFF :- system:use_module(library(url)).

/*
% % % OFF :- system:use_module(library(http/http_ssl_plugin)).
*/

% :- module(http_ssl_plugin, []).
% % % OFF 
:- if(exists_source(library(ssl))).
% % TODO :- system:use_module(library(ssl),[]).
:- endif.

% % % OFF :- system:use_module(library(socket),[]).
% % % OFF :- system:use_module(library(debug),[]).
% % % OFF :- system:use_module(library(option),[]).
% % TODO  :- system:use_module(library(http/thread_httpd),[]).
% % % OFF 
% % TODO  :- system:use_module(library(http/http_header)).

/* Part of LogicMOO Base SSL plugin for HTTP libraries

This  module  can  be   loaded    next   to   library(thread_httpd)  and
library(http_open) to provide secure HTTP   (HTTPS)  services and client
access.

An example secure server using self-signed  certificates can be found in
the <plbase>/doc/packages/examples/ssl/https.pl, where <plbase>   is the
SWI-Prolog installation directory.
*/

:- multifile
	thread_httpd:make_socket_hook/3,
	thread_httpd:accept_hook/2,
	thread_httpd:open_client_hook/5,
        http:http_protocol_hook/5,
	http:open_options/2,
	http:http_connection_over_proxy/6.


		 /*******************************
		 *	    SERVER HOOKS	*
		 *******************************/
/*
%%	thread_httpd:make_socket_hook(?Port, :OptionsIn, -OptionsOut)
%%								is semidet.
%
%	Hook into http_server/2 to create an   SSL  server if the option
%	ssl(SSLOptions) is provided.
%
%	@see thread_httpd:accept_hook/2 handles the corresponding accept
%
%
% Hook To [thread_httpd:make_socket_hook/3] For Module Logicmoo_util_filestreams.
% Make Socket Hook.
%
thread_httpd:make_socket_hook(Port, Options0, Options) :- thread_httpd_make_socket_hook(Port, Options0, Options).

thread_httpd_make_socket_hook(Port, M:Options0, Options) :-
	memberchk(ssl(SSLOptions), Options0), !,
	make_socket(Port, Socket, Options0),
	ssl_context(server,
                    SSL,
                    M:[ port(Port),
                        close_parent(true)
                      | SSLOptions
                      ]),
	atom_concat('httpsd', Port, Queue),
	Options = [ queue(Queue),
                    tcp_socket(Socket),
		    ssl_instance(SSL)
		  | Options0
		  ].


%= 	 	 

%% make_socket( ?Port, ?Socket, ?Options) is semidet.
%
% Hook To [thread_httpd:make_socket/3] For Module Logicmoo_util_filestreams.
% Make Socket.
%
make_socket(_Port, Socket, Options) :-
	option(tcp_socket(Socket), Options), !.
make_socket(Port, Socket, _Options) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5).


%%	thread_httpd:accept_hook(:Goal, +Options) is semidet.
%
%	Implement the accept for HTTPS connections.
% 
% Hook To [thread_httpd:accept_hook/2] For Module Logicmoo_util_filestreams.
% Accept Hook.
%
thread_httpd:accept_hook(Goal, Options) :-
	memberchk(ssl_instance(SSL), Options), !,
	memberchk(queue(Queue), Options),
        memberchk(tcp_socket(Socket), Options),
        tcp_accept(Socket, Client, Peer),
	debug(http(connection), 'New HTTPS connection from ~p', [Peer]),
	thread_httpd:http_enough_workers(Queue, accept, Peer),
	thread_send_message(Queue, ssl_client(SSL, Client, Goal, Peer)).


%= 	 	 

%% thread_httpd:open_client_hook( :TermSSL, ?Goal, ?In, ?Out, ?Peer) is semidet.
%
% Hook To [thread_httpd:open_client_hook/5] For Module Logicmoo_util_filestreams.
% Open Client Hook.
%
thread_httpd:open_client_hook(ssl_client(SSL, Client, Goal, Peer),
			      Goal, In, Out,
			      [peer(Peer), protocol(https)]) :-
        tcp_open_socket(Client, Read, Write),
	catch(ssl_negotiate(SSL, Read, Write, In, Out),
	      E,
	      ssl_failed(Read, Write, E)).


%= 	 	 

%% ssl_failed( ?Read, ?Write, ?E) is semidet.
%
% Ssl Failed.
%
ssl_failed(Read, Write, E) :-
	close(Write, [force(true)]),
	close(Read,  [force(true)]),
	throw(E).

*/
  /*
		 /*******************************
		 *	   CLIENT HOOKS		*
		 *******************************/

%	http:http_protocol_hook(+Scheme, +Parts, +PlainStreamPair,
%				-StreamPair, +Options) is semidet.
%
%	Hook for http_open/3 to connect  to   an  HTTPS (SSL-based HTTP)
%	server.   This   plugin   also   passes   the   default   option
%	`cacert_file(system(root_certificates))` to ssl_context/3.
%
% Hook To [http:http_protocol_hook/5] For Module Logicmoo_util_filestreams.
%
http:http_protocol_hook(https, Parts, PlainStreamPair, StreamPair, Options):-
	ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options).


%= 	 	 

%% ssl_protocol_hook( ?Parts, ?PlainStreamPair, ?StreamPair, ?Options) is semidet.
%
% Ssl Protocol Hook.
%
ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options) :-
        memberchk(host(Host), Parts),
        option(port(Port), Parts, 443),
	ssl_context(client, SSL, [ host(Host),
                                   port(Port),
                                   close_parent(true)
				 | Options
				 ]),
        stream_pair(PlainStreamPair, PlainIn, PlainOut),
        catch(ssl_negotiate(SSL, PlainIn, PlainOut, In, Out),
              Exception,
              ( ssl_exit(SSL, PlainIn, PlainOut, In, Out), throw(Exception)) ),
        stream_pair(StreamPair, In, Out).

ssl_exit(SSL, _PlainIn, _PlainOut, In, Out):- safely_try_close(Out),safely_try_close(In),safely_try_close(SSL).
*/
safely_try_close(Out):- ignore(catch(close(Out),_,true)).
/*
%	http:open_options(Parts, Options) is nondet.
%  
%  Hook To [http:open_options/2] For Module Logicmoo_util_filestreams.
%
%	Implementation of the multifile hook http:open_options/2 used by
%	library(http/http_open). By default, we use   the system trusted
%	root certificate database for validating an SSL certificate.
http:open_options(Parts, Options) :-
	memberchk(scheme(https), Parts),
	Options = [cacert_file(system(root_certificates))].

%	http:http_connection_over_proxy(+Proxy, +Parts, +HostPort, -StreamPair, +OptionsIn, -OptionsOut)
%
%	Facilitate an HTTPS connection via a   proxy using HTTP CONNECT.
%	Note that most proxies will only  support this for connecting on
%	port 443
%
%  Hook To [http:http_connection_over_proxy/6] For Module Logicmoo_util_filestreams.
http:http_connection_over_proxy(proxy(ProxyHost, ProxyPort), Parts,
				Host:Port, StreamPair, Options, Options) :-
        memberchk(scheme(https), Parts), !,
        tcp_connect(ProxyHost:ProxyPort, StreamPair, [bypass_proxy(true)]),
        catch(negotiate_http_connect(StreamPair, Host:Port),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).

% % % OFF :- system:use_module(library(http/http_open),[]).


%= 	 	 

%% negotiate_http_connect( ?StreamPair, ?Address) is semidet.
%
% Negotiate Http Connect.
%
negotiate_http_connect(StreamPair, Address):-
        format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
        flush_output(StreamPair),
        http_read_reply_header(StreamPair, Header),
        memberchk(status(_, Status, Message), Header),
        (   Status == ok
	->  true
        ;   throw(error(proxy_rejection(Message), _))
        ).
*/

:- multifile(package_path/2).

%= 	 	 

%% package_path( ?Pkg, ?PkgPath) is semidet.
%
% Package Path.
%
package_path(Pkg,PkgPath):-expand_file_search_path(pack(Pkg),PkgPathN),exists_directory(PkgPathN),normalize_path(PkgPathN,PkgPath).
package_path(Pkg,PkgPath):-atom(Pkg),T=..[Pkg,'.'],expand_file_search_path(T,PkgPathN),exists_directory(PkgPathN),normalize_path(PkgPathN,PkgPath).


%= 	 	 

%% file_to_stream_ssl_verify( ?SSL, ?ProblemCert, ?AllCerts, ?FirstCert, ?Error) is semidet.
%
% File Converted To Stream Ssl Verify.
%
file_to_stream_ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :- !.


:- export(text_to_stream/2).

%= 	 	 

%% text_to_stream( ?Text, ?Stream) is semidet.
%
% Text Converted To Stream.
%
text_to_stream(Text,Stream):-text_to_string(Text,String),string_codes(String,Codes),open_codes_stream(Codes,Stream).
:- export(file_to_stream/2).

%= 	 	 

%% file_to_stream( :TermStreamIn, ?Stream) is semidet.
%
% File Converted To Stream.
%
file_to_stream((StreamIn),Stream):-is_stream(StreamIn),!,copy_stream(StreamIn,Stream).
file_to_stream(stream(StreamIn),Stream):-copy_stream(StreamIn,Stream).
file_to_stream('$socket'(Sock),Stream):-tcp_open_socket('$socket'(Sock),StreamIn),copy_stream(StreamIn,Stream).
file_to_stream(term(Text),Stream):-term_to_atom(Text,String),string_codes(String,Codes),open_codes_stream(Codes,Stream).
file_to_stream(text(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(codes(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(chars(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(atom(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(string(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(alias(Text),Stream):-stream_property(Stream,alias(Text)).
file_to_stream(file(Spec),Stream):-file_to_stream(match(Spec),Stream).
file_to_stream(exfile(File),Stream):- size_file(File,Size),Max is 2^20*64, 
 (Size<Max->
     (read_file_to_codes(File,Codes,[expand(true)]),open_codes_stream(Codes,Stream));
      open(File,read,Stream,[])).

file_to_stream(match(Spec),Stream):-!,filematch(Spec,File),exists_file(File),!,file_to_stream(exfile(File),Stream).
file_to_stream(package(Pkg,LocalPath),Stream) :-!,
   package_path(Pkg,PkgPath),
   % build global path
   atomic_list_concat([PkgPath|LocalPath], '/',  GlobalPath),file_to_stream(GlobalPath,Stream).
file_to_stream(Spec,Stream):-compound(Spec),!,file_to_stream(match(Spec),Stream).
file_to_stream(URL,Stream):-sub_string(URL,_,_,_,":/"),sub_string(URL,0,4,_,'http'), !, if_defined(http_open:http_open(URL,HTTP_Stream,[ cert_verify_hook(file_to_stream_ssl_verify)]),fail),copy_stream(HTTP_Stream,Stream),!.
file_to_stream(URL,Stream):-atom_concat('file://', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-atom_concat('file:', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-on_x_fail(atomic_list_concat(['package://',Pkg,'/', Path], URL)),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(URL,Stream):-on_x_fail(atomic_list_concat([Pkg,'://',Path],URL)),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(Spec,Stream):-file_to_stream(match(Spec),Stream).

:- export(copy_stream/2).

%= 	 	 

%% copy_stream( ?HTTP_Stream, ?Stream) is semidet.
%
% Copy Stream.
%
copy_stream(HTTP_Stream,Stream):-read_stream_to_codes(HTTP_Stream,Codes),catch(close(HTTP_Stream),_,true),open_codes_stream(Codes,Stream).

:- fixup_exports.
