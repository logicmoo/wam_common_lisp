:- module(block_tagbody,
  [call_block_interp/2,
   call_block_compiled/2]).


:- dynamic(shared_lisp_compiler:plugin_expand_function_body/5).
:- multifile(shared_lisp_compiler:plugin_expand_function_body/5).
:- discontiguous(shared_lisp_compiler:plugin_expand_function_body/5).


/*
[tagbody, 
   Named0 = (c0,o0,d0,e0),
   Named1 = code1,
   named2 = (co2,de2) ):-

     


 (tagbody
   (setq val 2)
   (go lp)
   (incf val 3)
   lp (incf val 4)) 
=> 6
     */

:-user:ensure_loaded('lisp-interpreter').

% :- thread_local(t_l:btb/2).
:- discontiguous(t_l:btb/2).

/*

Interpretor...

*/

call_block_interp(Name,Value):- 
  btba(Name,Instr,Addrs),
  call_instructions_pc(0,Instr,Addrs,Value).

btba(Name,Instr,Addrs):-
   t_l:btb(Name,Instr),
   must(get_addrs(0,Instr,Addrs)).

% TODO - dont bother recording adresses until after the first 'GO'/1
get_addrs(_,[],[]):-!.
get_addrs(N,[TagInstr|Instr],[addr(Tag,N)|Addrs]):- is_tag_name(TagInstr,Tag),!,
  N1 is N + 1,
  get_addrs(N1,Instr,Addrs).
get_addrs(N,[_|Instr],Addrs):- 
  N1 is N + 1,
  get_addrs(N1,Instr,Addrs).
  

call_instructions_pc(PC,Instr,Addrs,Value):- 
   nth0(PC,Instr,I)->call_i_pc(I,PC,Instr,Addrs,Value);true.

% #:LABEL allows rearrangments and address changes
call_i_pc(TagInstr,PC,Instr,Addrs,Value):- is_tag_name(TagInstr,Tag),!,
   PC2 is PC + 1,
   call_instructions_pc(PC2,Instr,[addr(Tag,PC)|Addrs],Value).
% #:GO 
call_i_pc([go,Tag],_PC,Instr,Addrs,Value):-!,
   must(member(addr(Tag,Where),Addrs)),
   call_instructions_pc(Where,Instr,Addrs,Value).
% #:RETURN
call_i_pc('return-from'(_,Value),_PC,_Instr,_Addrs,Value):-!.
% #normal call
call_i_pc(I,PC,Instr,Addrs,Value):-!,
   call(I),
   PC2 is PC + 1,
   call_instructions_pc(PC2,Instr,Addrs,Value).

/* testing */

t_l:btb(block1,[setq(B,2),[go,tag1],setq(A,1),(tag1),setq(A,4),plus(A,B,C),writeln(C)]).
t_l:btb(block2,[setq(B,2),[go,tag2],setq(A,1),(tag1),setq(A,4),plus(A,B,C),writeln(C),'return-from'(block2,C),(tag2),setq(A,4),[go,tag1]]).


/*

Compiler...

TODO: This might be rewritten to not use a numbers as addresses
Instead simply grab the List''s reference at some numerical points

 (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val)
=>  15


    Tagbody = 
     (setq(val,1)
      call_then_return(Point_A),
      incf(val,16),
      Point_C =
      (incf(val,04),
       call(Point_B),
       incf(val,32)),
     Point_A =
      (incf(val,02)
      call_then_return(Point_C),
      incf(val,64))
     Point_B =
      incf(val,08),
      nb_current(var,Value),
      'return-from'(Value)),
    catch(TagBody,'return-from'(_,Value),true).


*/

:-user:ensure_loaded(lisp_compiler).

'return-from'(G):-throw('return-from'([],G)).
'return-from'(Tag,G):-throw('return-from'(Tag,G)).
call_then_return(G):- G,'return-from'([]).
setq(Var,X):-Var=X.

call_block_compiled(Name,Value):- 
  must(compile_btba(Name,Code)),
  catch(Code,'return-from'(_,Value),true).

compile_btba(Name,Code):-
   t_l:btb(Name,Instr),
   must(get_entries(Instr,Addrs)),
   must(put_entries(Instr,Addrs,Code)).

get_entries([],[]).
get_entries([TagInstr|Instr],[addr(Tag,Instr)|Addrs]):- is_tag_name(TagInstr,Tag),
  get_entries(Instr,Addrs).
% #branching call
get_entries([I|Instr],Addrs):-
  is_branched(I),get_entries(I,IAddrs),
  get_entries(Instr,NAddrs),
  append(IAddrs,NAddrs,Addrs).
get_entries([_|Instr],Addrs):-
  get_entries(Instr,Addrs).


is_tag_name(Atom,Atom):- atomic(Atom).
is_branched([Op|_]):- member(Op,[if,or,and,progn]).

put_entries([],_,'return-from'([])).
% #:ATOM or #:LABEL allows rearrangments and address changes
put_entries([(TagInstr)|Instr],Addrs,Code):- is_tag_name(TagInstr,Tag),!,
   put_entries(Instr,[addr(Tag,Instr)|Addrs],Code).
% #:GO 
put_entries([[go,Tag]|_],Addrs,(call(Where),'return-from'([]))):-!,
   must(member(addr(Tag,Where),Addrs)).
% #:RETURN-FROM
put_entries(['return-from'(Value)|_],_Addrs,'return-from'([],Value,_)):-!.
put_entries(['return-from'(Name,Value)|_],_Addrs,'return-from'(Name,Value,_)):-!.
% #branching call
put_entries([I|Instr],Addrs,(ICode,Code)):- !,
  is_branched(I),put_entries(I,Addrs,ICode),
  put_entries(Instr,Addrs,Code).
% #normal call
put_entries([I|Instr],Addrs,(I,Code)):-!,
  put_entries(Instr,Addrs,Code).

t_l:btb(block3,[setq(b,2),[go,tag1],setq(a,1),(tag1),setq(a,4),print(plus(a,b)),'return-from'(block3,plus(a,b))]).

:- lisp_read(
"(defun fifteen ()
  (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val))
    ",O),
   dmsg(O),
   complie_lisp(O).

 % [let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],'point-a',[incf,val,2],[go,'point-c'],[incf,val,64],'point-b',[incf,val,8]],val]

make_cont(G,Cont):-
  	reset(((   
          shift(mc(G))
	     ->  G
	     ;   true
	     )), mc(G), Cont).

reset_in_cond4(R):-
  make_cont((format(atom(R), 'Hello ~w', [X]);format(atom(R), 'Bye ~w', [X])),Cont),
   X = world,
   call(Cont).

loop_cont:-
   make_cont(writeln([x=X,y=Y]),Cont1),
   make_cont(once(number(Y)->X is Y+1;X=1),CalcX),
   make_cont(once(number(X)->Y is X+1;Y=1),CalcY),
   \+ \+ call(CalcY),
   call(CalcX),
   call(Cont1).
   

:- fixup_exports.

