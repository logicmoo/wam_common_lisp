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
:- module(socksrv, []).



:- include('./header').

:- use_module(library(socket)).

%%	lspsrv_server(?Port, +Options)
%
%	Create a TCP/IP based server  on  the   given  Port,  so you can
%	telnet into Prolog and run an  interactive session. This library
%	is intended to provide access for   debugging  and management of
%	embedded servers.
%
%	Currently defined options are:
%
%		* allow(IP)
%		Allow access from IP, a term of the format ip(A,B,C,D).
%		Multiple of such terms can exist and access is granted
%		if the peer IP address unifies to one of them.  If no
%		allow option is provided access is only granted from
%		ip(127,0,0,1) (localhost).
%
%	For example:
%
%		==
%		?- lspsrv_server(4000, []).
%
%		% telnet localhost 4000
%		Welcome to the SWI-Prolog server on thread 3
%
%		1 ?-
%		==
%
%	@bug As the connection does not involve a terminal, command history
%	and completion are not provided. Neither are interrupts
%	(Control-C).  To terminate the Prolog shell one must enter the
%	command "end_of_file."

start_lspsrv(Call,Port,Description):- PortNum is Port,
  must(lspsrv_server(PortNum, [allow(_),call(Call),description(Description)])),!.

lspsrv_server(Port, Options):-  
 \+ member(alias(_),Options),
 option(call(Call),Options,lsp_telnet),
 atomic_list_concat([Call,'_',Port],Alias),!, 
 lspsrv_server(Port, [alias(Alias)|Options]).

lspsrv_server(_Port, Options) :- member(alias(Alias),Options),thread_property(Base, status(running)),Base==Alias,!.

lspsrv_server(Port, Options) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    option(alias(Alias),Options,lspsrv_server),
    option(description(Desc),Options,Alias),
    dmsg(Port=Desc),
    thread_create(lsp_server_loop(ServerSocket, Options), _,
                  [ alias(Alias),detached(true)]),!.

lsp_server_loop(ServerSocket, Options):-
    ignore(catch(lsp_server_loop_1(ServerSocket, Options),E,writeln(user_error, lsp_server_loop_1(ServerSocket, Options,E)))),
       lsp_server_loop(ServerSocket, Options).


resolve_host_sksrv(Peer,Host):- catch(tcp_host_to_address(Host, Peer),_,fail),!.
resolve_host_sksrv(Peer,Host):- atom(Peer),Peer=Host,!.
resolve_host_sksrv(Peer,Host):- compound(Peer),catch((Peer=..PeerL,atomic_list_concat(PeerL,'.',Host)),_,fail),!.
resolve_host_sksrv(Peer,Host):- term_to_atom(Peer,Host),!.



lsp_server_loop_1(ServerSocket, Options) :-
  always((
    tcp_accept(ServerSocket, ClientSock, Peer),
    tcp_open_socket(ClientSock, In, Out),
    set_stream(In, close_on_abort(false)),
    set_stream(Out, close_on_abort(false)),    
    resolve_host_sksrv(Peer,Host),
    gensym('_',PostFix),
    option(alias(ServerAlias),Options,lspsrv_server),!,
    atomic_list_concat(['client_',Host,PostFix, '@', ServerAlias], Alias),
    catch(thread_create(
              call_service_lsp_client(Host, Alias, ClientSock, In, Out, Peer, Options),
              _, [ alias(Alias),detached(true)]),
          error(permission_error(create, thread, Alias), _),
          fail))).
   




call_service_lsp_client(Host, Alias, ClientSock, In, Out, Peer, Options):-
  call(call,service_lsp_client(Host, Alias, ClientSock, In, Out, Peer, Options)).

service_lsp_client(Host,Alias,ClientSock,In,Out,Peer,Options) :-
    option(allow(PeerAllow),Options,ip(127,0,0,1))-> PeerAllow=Peer,
    !,
    thread_self(Id),
    set_prolog_flag(tty_control, true),
    set_prolog_IO(In, Out, Out),    
    set_stream(In, tty(true)),
    % TODO figure out how to get immedate results
    % set_stream(In, buffer_size(1)),
    set_stream(user_output, tty(true)),
    set_stream(user_error, tty(true)),
    set_thread_error_stream(Id,user_error),
    current_prolog_flag(encoding, Enc),
    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),

    call(retractall,thread_util:has_console(Id, _, _, _)),
    thread_at_exit(call(retractall,thread_util:has_console(Id, _, _, _))),
    call(asserta,thread_util:has_console(Id, In, Out, Out)),

    option(call(Call), Options, prolog),
    format(main_error,'~N~n~q~n~n',[service_lsp_client_call(Call,Id,Alias,ClientSock,In,Out,Host,Peer,Options)]),
    %format(user_error, '',[Call,Id]),
    call_cleanup(Call,
                 ( close(In),
                   close(Out),
                   thread_detach(Id))).

service_lsp_client(Host,Alias,ClientSock,In,Out,Peer,Options):-
    thread_self(Id),option(call(Call), Options, prolog),
    format(main_error,'~N~n~q~n~n',[rejecting(Call,Id,Alias,ClientSock,In,Out,Host,Peer,Options)]),    
    format(Out, 'Bye!!~n', []),
    close(In),
    close(Out),
    thread_detach(Id).


:- fixup_exports.
                                          
