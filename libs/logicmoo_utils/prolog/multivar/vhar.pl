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

:- module(vhar,
 []).



is_vhar(Var,Attrs):-attvar(Var),get_attr(Var,vhar,Attrs).
vhar(Var):- vhar(Var,_Proxy,_Out,[vhar(Var)]). 
vhar(Var,Out):- vhar(Var,_Proxy,Out,[vhar(Var,Out)]). 
vhar(Var,Proxy,Out):-vhar(Var,Proxy,Out,[vhar(Var,Proxy,Out)]).
vhar(Var,Proxy,Out,PrevGoals):-  
  (var(Out)->Out=the(Var);setarg(1,Out,Var)),put_attr(Var,vhar,vhar(Var,Proxy,Out,PrevGoals)),
   gensym(vhar_,Serial),
   vprox(Proxy,Serial,Var).


vhar:attribute_goals(Var)-->{get_attr(Var,vhar,vhar(OrigVar,_Proxy,_Out,Goals))},({OrigVar==Var}->Goals;[]).

vhar:attr_unify_hook(vhar(_VarI,ProxyI,OutI,_GoalsI),Value):- (Value==ProxyI;Value==OutI),!.
vhar:attr_unify_hook(InfoI,Value):- is_vhar(Value,InfoO),!,
  InfoI=vhar(VarI,ProxyI,OutI,GoalsI),InfoI=vhar(VarI,ProxyI,OutI,GoalsI),
  InfoO=vhar(VarO,ProxyO,OutO,GoalsO),InfoO=vhar(VarO,ProxyO,OutO,GoalsO),
  copy_term(ProxyI,ProxyI,ProxyGoalsI),subst(ProxyGoalsI,VarI,ProxyI,CorrectedGoalsI),
  copy_term(ProxyO,ProxyO,ProxyGoalsO),subst(ProxyGoalsO,VarO,ProxyO,CorrectedGoalsO), 
  trace,maplist(call,CorrectedGoalsI),
  trace,maplist(call,CorrectedGoalsO),
  append(GoalsI,GoalsO,Goals),
  list_to_set(Goals,GoalsSet),
  setarg(4,InfoI,GoalsSet),
  setarg(4,InfoO,GoalsSet).

vhar:attr_unify_hook(InfoI,Value):- is_vhar(Value,InfoO),!,
  InfoI=vhar(VarI,ProxyI,OutI,GoalsI),InfoI=vhar(VarI,ProxyI,OutI,GoalsI),
  InfoO=vhar(VarO,ProxyO,OutO,GoalsO),InfoO=vhar(VarO,ProxyO,OutO,GoalsO),
  copy_term(Value,VarI,ValueGoals),
  %get_attrs(ProxyO,Atts),
  %put_attrs(Value,Atts),
  %put_attrs(ProxyI,Atts),
  subst(ValueGoals,VarI,VarO,ValueGoals2),
  maplist(call,ValueGoals2),
  append(GoalsI,GoalsO,Goals),
  setarg(4,InfoI,Goals),
  setarg(4,InfoO,Goals).

vhar:attr_unify_hook(vhar(_Var,Proxy,Out,Goals),_Value):- vhar(Proxy,New,Goals),setarg(1,Out,New).

:- fixup_exports.

