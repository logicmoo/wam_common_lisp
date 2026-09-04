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

:- module(vprox,
 []).


match_existing_var(Var,Var0):- attvar(Var)->Var==Var0;(var(Var)->Var=Var0;Var=@=Var0).
is_hprox_vprox(Var,Proxy):- is_hprox(Var,Proxy),!,is_vprox(Proxy,Var).


is_vprox(Proxy):- get_attr(Proxy,vprox,vprox4(Proxy,_Serial,_Var,_)).
is_vprox(Proxy,Var):- get_attr(Proxy,vprox,vprox4(Proxy,Var0,_Ser,_)),!,match_existing_var(Var,Var0).

vprox(Proxy,Var):- is_vprox(Proxy,Var),!.
vprox(Proxy,Var):- is_hprox_vprox(Var,Proxy),!.
vprox(Proxy,Var):- gensym(vprox_,Serial),vprox(Proxy,Var,Serial,[vprox(Proxy,Var,Serial)]).

vprox(Proxy,Var,Serial):-hprox(Var,Proxy,Serial,[vprox(Proxy,Var,Serial)]).

% vprox(Proxy,Var,Serial,PrevGoals):-  get_attrs(Var,Atts),!, put_attrs(Proxy,att(vprox,vprox4(Proxy,Var,Serial,Atts),Atts)).
vprox(Proxy,Var,Serial,PrevGoals):- (var(Serial)->gensym(vprox_,Serial);true), hprox(Var,Proxy,Serial,PrevGoals).

vprox:attr_unify_hook(vprox4(_Proxy,Var,_Serial,_Atts),Value):- Var==Value,!.
vprox:attr_unify_hook(_InfoI,Value):- get_attr(Value,hprox,_),!.
vprox:attr_unify_hook(_InfoI,Value):- get_attr(Value,vprox,_),!.
% ,throw((var:Var==value:Value , vprox:attr_unify_hook(vprox4(Proxy,Var,Serial,Atts),Value))).
vprox:attr_unify_hook(vprox4(Proxy,Var,Serial,Atts),Value):- nonvar(Value),!,throw((nonvar(Value) , vprox:attr_unify_hook(vprox4(Proxy,Var,Serial,Atts),Value))).
vprox:attr_unify_hook(vprox4(_Proxy,_Serial,_Var,_Atts),Value):- var(Value),!.
vprox:attribute_goals(Proxy)-->{get_attr(Proxy,vprox,vprox4(OrigProxy,Var,Serial,_))},({\+ is_hprox(Var,OrigProxy)}->[vprox(Proxy,Var,Serial)];[]).

is_hprox(Var):- get_attr(Var,hprox,_).
is_hprox(Var,Proxy):- get_attr(Var,hprox,hprox(_VarI,ProxyI,_PrevGoals)),match_existing_var(Proxy,ProxyI).

hprox(Var):- hprox(Var,_Proxy,[hprox(Var)]). 
hprox(Var,Proxy):- is_hprox_vprox(Var,Proxy),!.
hprox(Var,Proxy):- hprox(Var,Proxy,[hprox(Var,Proxy)]). 
hprox(Var,Proxy,PrevGoals):- gensym(hprox_,Serial),hprox(Var,Proxy,Serial,PrevGoals).

hprox(Var,Proxy,Serial,PrevGoals):-  
   put_attr(Var,hprox,hprox(Var,Proxy,PrevGoals)),
   dif(Var,Serial),
   get_attrs(Var,Atts),
   put_attrs(Proxy,att(vprox,vprox4(Proxy,Var,Serial,Atts),Atts)).

hprox:attribute_goals(Var)-->{get_attr(Var,hprox,hprox(OrigVar,_Proxy,Goals))},({OrigVar==Var}->Goals;[]).
hprox:attr_unify_hook(hprox(_VarI,ProxyI,_GoalsI),Value):- (Value==ProxyI),!.
hprox:attr_unify_hook(_InfoI,Value):- get_attr(Value,hprox,_),!.
hprox:attr_unify_hook(_InfoI,Value):- get_attr(Value,vprox,_),!.
hprox:attr_unify_hook(hprox(_Var,Proxy,_Goals),Value):-
  get_attrs(Proxy,Atts),
  '$attvar':call_all_attr_uhooks(Atts, Value).
/*

hprox:attr_unify_hook(InfoI,Value):- get_attr(Value,hprox,InfoO),
  InfoI=hprox(VarI,ProxyI,GoalsI),InfoI=hprox(VarI,ProxyI,GoalsI),
  InfoO=hprox(VarO,ProxyO,GoalsO),InfoO=hprox(VarO,ProxyO,GoalsO),
   get_attrs(ProxyI,AttsI),
   '$attvar':call_all_attr_uhooks(AttsI, Value),
   get_attrs(ProxyO,AttsO),
   '$attvar':call_all_attr_uhooks(AttsO, ProxyI),!.

hprox:attr_unify_hook(InfoI,Value):- get_attr(Value,hprox,InfoO),
  InfoI=hprox(VarI,ProxyI,GoalsI),InfoI=hprox(VarI,ProxyI,GoalsI),
  InfoO=hprox(VarO,ProxyO,GoalsO),InfoO=hprox(VarO,ProxyO,GoalsO),
  copy_term(ProxyI,ProxyI,ProxyGoalsI),subst(ProxyGoalsI,VarI,ProxyI,CorrectedGoalsI),
  copy_term(ProxyO,ProxyO,ProxyGoalsO),subst(ProxyGoalsO,VarO,ProxyO,CorrectedGoalsO),
  maplist(call,CorrectedGoalsI),
  maplist(call,CorrectedGoalsO),
  append(GoalsI,GoalsO,Goals),
  list_to_set(Goals,GoalsSet),
  setarg(3,InfoI,GoalsSet),
  setarg(3,InfoO,GoalsSet).
*/

% ?- hprox(X,PX),hprox(Y,PY),X=Y.


:- fixup_exports.

