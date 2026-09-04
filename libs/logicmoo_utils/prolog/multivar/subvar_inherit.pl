/*  Part of SWI-Prolog
    Author:        Douglas R. Miles, Jan Wielemaker
    E-mail:        logicmoo@gmail.com, jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org http://www.logicmoo.org
    Copyright (C): 2015, University of Amsterdam
                                    VU University Amsterdam
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(subvar_inherit,[subvar_inherit/1]).

subvar_inherit(Var,Which):-term_variables(Var,Vars),maplist(subvar_inherit1(Which),Vars).
subvar_inherit(Var):-term_variables(Var,Vars),maplist(subvar_inherit1,Vars).

subvar_inherit1(Var):- (get_attr(Var,subvar_inherit,v(Var,_Already))->true;put_attr(Var,subvar_inherit,v(Var,_NewList))).
subvar_inherit1(Which,Var):- 
  (get_attr(Var,subvar_inherit,v(Var,Already))->true;Already=[]),
   update_list(Which,Already,NewList),put_attr(Var,subvar_inherit,v(Var,NewList)).

subvar_inherit:attr_unify_hook(v(Var,NewList),Value):- term_variables(Value,Vars),maplist(subvar_copy_to(Var,NewList),Vars).


invert_pn_0(+,-).
invert_pn_0(-,+).

subvar_copy_to(Var,NewList,Value):- var(NewList),get_attrs(Var,Atts),copy_atts_to(Atts,Value).
subvar_copy_to(_,att(M,V,Atts),Value):- copy_atts_to(att(M,V,Atts),Value).
subvar_copy_to(Var,[New|List],Value):- !, subvar_copy1_to(Var,[New|List],Value).

subvar_copy1_to(Var,[M|List],Value):- !,get_attr(Var,M,V),put_attr(Value,M,V),subvar_copy1_to(Var,List,Value).
subvar_copy1_to(_,_,_).

copy_atts_to(att(M,V,Atts),Value):-!,put_attr(Value,M,V),copy_atts_to(Atts,Value).
copy_atts_to(_,_).

update_list(At,In,Out):- InOut=ul(v(M,In)),update_list(+,InOut,M,At),InOut=ul(v(M,Out)).

update_list(PN,Var,M,At):-var(At),!,throw(error(instantiation_error, M:update_list(Var,PN:At))).
%update_list(PN,Var,user,Atts):-!, update_list(PN,Var,tst,Atts).
update_list(PN,Var,M, X+Y):-!, update_list(PN,Var,M, X),update_list(PN,Var,M,+Y).
update_list(PN,Var,M, X-Y):-!, update_list(PN,Var,M, X),update_list(PN,Var,M,-Y).
update_list(PN,Var,M, +X+Y):-!, update_list(PN,Var,M, +X),update_list(PN,Var,M,+Y).
update_list(PN,Var,M, +X-Y):-!, update_list(PN,Var,M, +X),update_list(PN,Var,M,-Y).
update_list(PN,Var,M, List):- is_list(List),!,maplist(update_list(PN,Var,M),List).
update_list(_, Var,M,  +At):-!, update_list(+,Var,M,At).
update_list(PN,Var,M,  -At):- invert_pn_0(PN,NP),!,update_list(NP,Var,M,At).
%update_list(PN,Var,_,(M:At)):- \+ meta_handler_name(M), !,update_list(PN,Var,M,At).
%update_list(PN,Var,M, Meta):- \+ \+ clause(M:meta_hook(Meta,_,_),_), !, forall(M:meta_hook(Meta,P,A),update_list(PN,Var,M,P=A)).
% =(+a,b) -->   +(A=B).
update_list(PN,Var,M, Pair):- compound(Pair),Pair=..[P,Arg1,Arg2],listep(P),compound(Arg1),call((Arg1=..List,append(Head,[Last],List),At=..[P,Last,Arg2],append(Head,[At],ListNew),Try=..ListNew,!,update_list(PN,Var,M, Try))).
% update_list(PN,Var,_, Hook):-  handler_fbs(+ Hook,Number), Number>0, !,PNHook=..[PN,Hook], update_dlist(Var, PNHook).

update_list(PN,Var,M,Pair):- !,
  list_to_lst(Pair,Tmpl),
 % update_hooks(PN,Var,M,Tmpl),
   % list_exist(PN,Tmpl),
   exec_list_update(PN,Var,M,Tmpl).



exec_list_update(-,Var,M,Tmpl):-
   (get_lstr(Var,M,Cur)->
     (delete(Cur,Tmpl,Upd),update_lstr(Var,M,Upd)) ;
    true).

exec_list_update(+,Var,M,At):-
   (get_lstr(Var,M,Cur) ->
    (functor(At,Tmpl,_),
     delete(Cur,Tmpl,Mid), % ord_del_element wont work here because -a(_) stops short of finding a(1).
     ord_add_element(Mid,At,Upd),
     update_lstr(Var,M,Upd));
    update_lstr(Var,M,[At])).

get_lstr(InVar,M,Upd):-arg(1,InVar,v(M,Upd)).
update_lstr(InVar,M,Upd):- setarg(1,InVar,v(M,Upd)).

listep('=').
listep(':').
listep('-').

list_to_lst(Var,Var):-var(Var),!.
list_to_lst(N-V,Tmpl):-!,list_to_lst(N=V,Tmpl).
list_to_lst(N:V,Tmpl):-!,list_to_lst(N=V,Tmpl).
list_to_lst(N=V,Tmpl):-!,assertion(atom(N)),!,Tmpl=..[N,V].
list_to_lst(F/A,Tmpl):-!,assertion((atom(F),integer(A))),functor(Tmpl,F,A).
list_to_lst(Tmpl,Tmpl).

