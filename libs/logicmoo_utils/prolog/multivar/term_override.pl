
/*

 a(A,B|Rest) = a(x,y)    


 A=x,B=y,Rest='@'().



 a(A,B|Rest) = a(x,y,z,1)

 A=x,B=y,Rest='@'(z,1).



 a(A,B|Rest) = a(x,y,z|Rest2)

 A=x,B=y,Rest='@'(z|Rest2),Rest2 = _.

*/

extendable_compound(PT,Ex):- 
   put_attr(Ex,'$VAR$',Ex), % make extensable
   put_attr(Ex,partial_term,ec(Ex,PT,_More)).

partial_term:attr_unify_hook(ec(Ex,PT,More),Value):-
  partial_univ(Value,ValueL),
  partial_univ(Partial,PartialL),
  ValueL=PartialL.

partial_univ(Partial,PartialL):-
   Partial=..PartialM,append(PartialM,_,Partial
new_partial_dict(Dict):- 
   extendable_compound(_{},Dict).


