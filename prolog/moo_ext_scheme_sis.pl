% -----------------------------------------------------------------------------
%
%                               S I S   version 0.1
%
%                    (Straightforward Implementation of Scheme)
%
% This program is a compiler for the Scheme language which generates
% native code (for MC68000).  Quintus prolog has been used to develop the
% program.
%
% Sample use (on a SUN):
%
%  $ prolog
%  | ?- restore('sc.bin').         (load the compiler's image)
%  | ?- ex.
%  Input file name (.scm) : test
%  Input file is "test.scm"
%  Output file is "test.s"
%  | ?- halt.
%  $ asm test
%  $ test
%
% -----------------------------------------------------------------------------
%
% Revision history:
%
% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
%
% Version 0.0 (Dec 15, 1987).
%
%   - 'rest' arguments are not implemented (e.g. (lambda (a b . c) c)) and
%     a maximum of 126 arguments can be passed to a procedure.
%   - backquote notation is not supported
%   - floating point numbers and bignums are not implemented
%   - first class continuations are not implemented (all is ready for them
%     though; the stack could be copied by call-with-current-continuation
%     and restored by a call to the continuation)
%   - there is no garbage-collector and heap-overflow is not checked
%   - list constants will cause the assembly to abort (this is a restriction
%     caused by the SUN's assembler not the Scheme compiler) however, you
%     can use 'cons' to build a list at execution time
%   - symbols are not interned (i.e. symbol constants with the same name at
%     two different places are not eq?)
%   - only a small number of procedures are implemented, and they do not
%     check the type or number of their arguments.  The following procedures
%     are implemented:
%
%       not, eq?, pair? cons, car, cdr, set-car!, set-cdr!, null?,
%       =, <, >, +, -, *, /, -1+, force, write, newline
%
%       new procedures can be added by putting their names in the
%       'initial_global_env' list (in the compiler) and by adding their
%       definition in the 'header.s' file.
%
% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
%
% Version 0.1 (Jan 26, 1988).
%
%   - 'rest' arguments are now permitted.
%   - backquote notation is implemented.
%   - list constants now work correctly.
%   - symbols are now interned.
%   - primitive procedures now check the type and number of their arguments.
%   - it is now possible to open-code certain procedure calls; this is
%     done by typing 'integrate(all).' before compiling (note: open-coded
%     procedures do no error checking).
%   - source-code can be printed for procedures written with 'write' by
%     typing 'debug(on).'.
%   - some new procedures have been added:
%
%      append, length, list, vector, list->vector, memq, assq, symbol?,
%      vector?, string?, procedure?, number?, char?
%
% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
%
% Version 0.2 (Feb 3, 1988).
%
%   - procedure calls can be traced by typing 'trace(on).'.
%
% -----------------------------------------------------------------------------



% -----------------------------------------------------------------------------

% Toplevel of compiler.

ex :-
    query_io_files(I,O),
    parse(I,Program),
    compile(Program,Code),
    write_code(Code,O).

query_io_files(I,O) :-
    write_term('Input file name (.scm) : '),
    read_line(X),
    append(X,".scm",Y),
    name(I,Y),
    append(X,".s",Z),
    name(O,Z),
    write_term('Input file is "'), write_term(I), write_term('"'), newline,
    write_term('Output file is "'), write_term(O), write_term('"'), newline.
% -----------------------------------------------------------------------------

% Basic file I/O and utilities.

open_input(Filename) :- see(Filename).
read_char(Ch) :- get0(C), read_char2(C,Ch).
read_char2(-1,eof) :- !.
read_char2(C,C).
close_input :- seen.

read_line(L) :- read_char(C), read_line(C,L).
read_line(C,[]) :- eoln(C), !.
read_line(C1,[C1|L]) :- read_char(C2), read_line(C2,L).

open_output(Filename) :- tell(Filename).
write_char(Ch) :- put(Ch).
write_term(X) :- write(X).
newline :- nl.
close_output :- told.

eoln(10).

append([],L,L).
append([E|X],Y,[E|Z]) :- append(X,Y,Z).

reverse(L1,L2) :- reverse_aux(L1,[],L2).
reverse_aux([],L,L).
reverse_aux([X|Y],L1,L2) :- reverse_aux(Y,[X|L1],L2).

symbol([]) :- !, fail.
symbol(X) :- atom(X).

% -----------------------------------------------------------------------------

% Compiler option management.

option_on(X) :- retract(options(Y)), !, union(X,Y,Z), asserta(options(Z)).
option_off(X) :- retract(options(Y)), !, difference(Y,X,Z), asserta(options(Z)).

option(X) :- options(O), memb(X,O).

:- dynamic options/1.

options([]).

integrate(all) :- !, option_on([int(car),int(cdr),int('+'),int('-'),int('*'),
                                int('/'),int('-1+')]).
integrate(none) :- !, option_off([int(car),int(cdr),int('+'),int('-'),int('*'),
                                  int('/'),int('-1+')]).
integrate(X) :- option_on([int(X)]).

debug(on) :- option_on([debug]).

debug(off) :- option_off([debug]).

trace(on) :- option_on([trace]).

trace(off) :- option_off([trace]).

% -----------------------------------------------------------------------------

% Parser.

parse(I,Program) :-
    write_term('1) Reading input...'), newline,
      open_input(I), get_source(Source), !, close_input,
    write_term('   ...done'), newline,
    write_term('2) Parsing...'), newline,
      white(Source,Start), sexprs(Program,Start,[]), !,
    write_term('   ...done'), newline.

get_source(S) :- read_char(C), get_source(C,S).
get_source(eof,[]) :- !.
get_source(C,[C|S]) :- get_source(S).

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

% Use DCG for parser.

blank --> [C], {C =< 32}, white.
blank --> ";", comment, white.

white --> blank.
white --> [].

comment --> [C], {eoln(C)}, !.
comment --> [C], comment.

sexprs([H|T]) --> sexpr(H), !, sexprs(T).
sexprs([]) --> [].

sexpr(L)                      --> "(", !, white, sexpr_list(L), white.
sexpr(vec(V))                 --> "#(", !, sexpr_vector(V), white.
sexpr(boo(t))                 --> "#t", !, white.
sexpr(boo(f))                 --> "#f", !, white.
sexpr(chr(N))                 --> "#\", [C], !, {N is C}, white.
sexpr(str(S))                 --> """", !, sexpr_string(S), white.
sexpr([quote,E])              --> "'", !, white, sexpr(E).
sexpr([quasiquote,E])         --> "`", !, white, sexpr(E).
sexpr(['unquote-splicing',E]) --> ",@", !, white, sexpr(E).
sexpr([unquote,E])            --> ",", !, white, sexpr(E).
sexpr(E)                      --> sym_or_num(E), white.

sexpr_list([]) --> ")", !.
sexpr_list(_) --> ".", [C], {\+ sym_char(C)}, !, fail.
sexpr_list([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_rest([]) --> ")", !.
sexpr_rest(E) --> ".", [C], {\+ sym_char(C)}, !, sexpr(E,C), !, ")".
sexpr_rest([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_vector([]) --> ")", !.
sexpr_vector([First|Rest]) --> sexpr(First), !, sexpr_vector(Rest).

sexpr_string([]) --> """", !.
sexpr_string([C|S]) --> chr(C), sexpr_string(S).

chr(92) --> "\\", !.
chr(34) --> "\""", !.
chr(N)  --> [C], {C >= 32, N is C}.

sym_or_num(E) --> [C], {sym_char(C)}, sym_string(S), {string_to_atom([C|S],E)}.

sym_string([H|T]) --> [H], {sym_char(H)}, sym_string(T).
sym_string([]) --> [].

number(N) --> unsigned_number(N).
number(N) --> "-", unsigned_number(M), {N is -M}.
number(N) --> "+", unsigned_number(N).

unsigned_number(N) --> digit(X), unsigned_number(X,N).
unsigned_number(N,M) --> digit(X), {Y is N*10+X}, unsigned_number(Y,M).
unsigned_number(N,N) --> [].

digit(N) --> [C], {C >= 48, C =<57, N is C-48}.

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

sexpr(E,C,X,Z) :- white([C|X],Y), sexpr(E,Y,Z).

sym_char(C) :- C > 32, \+ memb(C,";()#""',`").

string_to_atom(S,N) :- number(N,S,[]), !.
string_to_atom(S,I) :- lowcase(S,L), name(I,L).

lowcase([],[]).
lowcase([C1|T1],[C2|T2]) :- lowercase(C1,C2), lowcase(T1,T2).

lowercase(C1,C2) :- C1 >= 65, C1 =< 90, !, C2 is C1+32.
lowercase(C,C).
% -----------------------------------------------------------------------------

% Compilation.

compile(Program,Code) :-
    write_term('3) Compilation...'), newline,
      compile_list(Program,Expr), !,
    write_term('   ...done'), newline,
    write_term('4) Virtual machine code generation...'), newline,
      gen_program(Expr,Code), !,
    write_term('   ...done'), newline.

compile_list([],cst(U)) :- !, undefined(U).
compile_list([Expr],C) :- !, compile_expr(Expr,C).
compile_list([Expr|Tail],app([pro(['#'],none,[],[],Rest,[]),C])) :-
    compile_expr(Expr,C),
    compile_list(Tail,Rest).

compile_expr([define|Def],set(Var,C)) :- !,
    definition(Def,[Var,Expr]),
    write_term('   compiling '), write_term(Var), newline,
    compile_expression(Expr,C).
compile_expr(Expr,C) :-
    write_term('   compiling <expression>'), newline,
    compile_expression(Expr,C).

compile_expression(E,C) :-
    expand(E,X),    % expand macros and convert to prolog structures
    alpha(X,Y),     % rename variables and convert assignments
    closurize(Y,C). % find out which lambda-expression are actually going to
                    % be 'true' closures (i.e. they have closed variables)

% To add new predefined procedures, add their names to this list:

initial_global_env([
  '#trace', '#make-promise', '#memv', '#cons', '#list', '#append', '#list->vector',
  not, 'eq?', 'pair?', cons, append, length, car, cdr, 'set-car!', 'set-cdr!',
  'null?', '=', '<', '>', '+', '-', '*', '/', '-1+', force, write, newline,
  list, vector, 'list->vector', memq, assq, 'symbol?', 'vector?', 'string?',
  'procedure?', 'number?', 'char?'
]).

% -----------------------------------------------------------------------------

% Code output to file.

write_code(Code,O) :-
    write_term('5) Writing M68000 machine code...'), newline,
      open_output(O),
      emit_objects(Code,[],_), !,
      close_output,
    write_term('   ...done'), newline.

% Emit a sequence of Scheme objects (i.e. symbols, lists, vectors, strings
% and procedure definitions) to the output file.  The complication is
% that writing an object might require writing others (e.g. for vectors
% the elements have to be written also (recursively)) and that a given
% symbol should only be output once.

emit_objects([],Syms,Syms).
emit_objects([Object|Tail],Syms1,Syms3) :-
    emit_object(Object,Vals1,Syms1,Syms2),
    append(Vals1,Tail,Vals2),
    emit_objects(Vals2,Syms2,Syms3).

emit_object(obj(Label,S),Vals,Syms1,Syms2) :- symbol(S), !,
    name(S,L),
    conv_obj(str(L),String,[],Vals,Syms1,Syms2),
    write_term(symbol_object(Label,String)), newline.
emit_object(obj(Label,[Car|Cdr]),Vals2,Syms1,Syms3) :-
    conv_obj(Car,Car_val,[],Vals1,Syms1,Syms2),
    conv_obj(Cdr,Cdr_val,Vals1,Vals2,Syms2,Syms3),
    write_term(pair_object(Label,Car_val,Cdr_val)), newline.
emit_object(obj(Label,vec(L)),Vals,Syms1,Syms2) :-
    length(L,Length),
    write_term(vector_object(Label,Length)), newline,
    emit_object_list(L,[],Vals,Syms1,Syms2).
emit_object(obj(Label,str(L)),[],Syms,Syms) :-
    length(L,Length),
    write_term(string_object), write_char(40),
    write_term(Label), comma, emit_string(L), write_char(41), newline.
emit_object(obj(Label,pro(L,Source)),Vals,Syms1,Syms2) :-
    write_term(procedure_object_begin(Label)), newline,
    emit_instructions(L,[],Const1),
    write_term(procedure_object_constants), newline,
    genlabel(Source_label),
    include_source(Source,Include),
    append(Const1,[const(Source_label,Include)],Const2),
    emit_constants(Const2,[],Vals,Syms1,Syms2),
    write_term(procedure_object_end), newline.

emit_string(S) :- write_char(96), write_char(34),
                  emit_string2(S),
                  write_char(34), write_char(39).

emit_string2([]).
emit_string2([C|L]) :- (C<32;C=34;C=92;C>=127), !, write_char(92),
                       N1 is 48+(C // 64), write_char(N1),
                       N2 is 48+((C // 8) mod 8), write_char(N2),
                       N3 is 48+(C mod 8), write_char(N3),
                       emit_string2(L).
emit_string2([C|L]) :- write_char(C), emit_string2(L).

include_source(Source,Source) :- option(debug), !.
include_source(Source,[]).

emit_instructions([],Const,Const).
emit_instructions([Instr|Tail],Const1,Const3) :-
    emit(Instr,Const1,Const2),
    emit_instructions(Tail,Const2,Const3).

emit_constants([],Vals,Vals,Syms,Syms).
emit_constants([const(Label,Object)|Tail],Vals1,Vals3,Syms1,Syms3) :-
    emit(label(Label)),
    conv_obj(Object,Value,Vals1,Vals2,Syms1,Syms2),
    opcode('.long'), label(Value), newline,
    emit_constants(Tail,Vals2,Vals3,Syms2,Syms3).

emit_object_list([],Vals,Vals,Syms,Syms).
emit_object_list([Object|Tail],Vals1,Vals3,Syms1,Syms3) :-
    conv_obj(Object,Value,Vals1,Vals2,Syms1,Syms2),
    opcode('.long'), label(Value), newline,
    emit_object_list(Tail,Vals2,Vals3,Syms2,Syms3).

conv_obj(Object,Value,Vals,Vals,Syms,Syms) :- non_gc(Object,Value), !.
conv_obj(Object,Value,Vals,Vals,Syms,Syms) :- memb(sym(Object,Value),Syms), !.
conv_obj(Object,Value,Vals,[obj(Value,Object)|Vals],Syms1,Syms2) :-
    genlabel(Value),
    intern_symbol(Object,Value,Syms1,Syms2).

intern_symbol(Object,Value,Syms,[sym(Object,Value)|Syms]) :- symbol(Object), !.
intern_symbol(Object,Value,Syms,Syms).

% -----------------------------------------------------------------------------

% Virtual machine implementation for MC68000.

% Each virtual instruction generated is expanded into MC68000 code.  The
% virtual instructions are:

% label(Label)                - declare a label
% dealloc(Depth)              - deallocate words from the stack
% return(Depth)               - deallocate and return to caller
% enter(Type,Nb_args,Kind)    - enter a procedure of Nb_args parameters (Type
%                               is the type of procedure (either 'closure' or
%                               'plain') and Kind specifies if the last
%                               parameter is a rest parameter)
% jump_glo(Disp,Nb_args)      - jump to a global procedure
% jump(Src,Nb_args)           - jump to a procedure (general form)
% sub_procedure(Label)        - declare a sub-procedure label
% push_continuation(Label)    - push a sub-procedure label (ie. a return addr.)
% branch_if_false(Src,Label)  - branch to label if 'Src' is false
% branch_always(Label)        - branch to label
% move(Src,Dest)              - move 'Src' to 'Dest'
% set_glo(Disp,Src)           - set a global variable
% set_clo(Depth,Disp,Src)     - set a closed variable
% set_loc(Disp,Src)           - set a local variable
% box_clo(Depth,Disp,Dest)    - put a closed variable in a cell
% box_loc(Disp,Dest)          - put a local variable in a cell
% get_clo(Depth,Disp,Dest)    - fetch the value of a mutable closed variable
% get_loc(Disp,Dest)          - fetch the value of a mutable local variable
% ref_glo(Disp,Dest)          - fetch the value of a global variable
% ref_clo(Depth,Disp,Dest)    - fetch the value of a closed variable
% ref_loc(Disp,Dest)          - fetch the value of a local variable
% cst(Val,Dest)               - move a constant value to 'Dest'
% make_closure(Body,Nb_closed,Dest) - make a closure with a given 'Body'
% close_loc(Disp)             - add a local variable to a closure
% close_clo(Disp)             - add a closed variable to a closure
% open_code(Proc,Nb_args)     - open code the procedure 'Proc' taking
%                               'Nb_args' arguments

integrable(car,1).
emit(open_code(car,1)) :-
    opcode(movl), dregister(1),  comma, aregister(0), newline,
    opcode(movl), indirect(0,0), comma, dregister(1), newline.

integrable(cdr,1).
emit(open_code(cdr,1)) :-
    opcode(movl), dregister(1), comma, aregister(0), newline,
    opcode(movl), autodecr(0),  comma, dregister(1), newline.

integrable('+',2).
emit(open_code('+',2)) :-
    opcode(addl), dregister(2), comma, dregister(1), newline.

integrable('-',1).
emit(open_code('-',1)) :-
    opcode(negl), dregister(1), newline.

integrable('-',2).
emit(open_code('-',2)) :-
    opcode(subl), dregister(2), comma, dregister(1), newline.

integrable('*',2).
emit(open_code('*',2)) :-
    opcode(asrl), immediate(3), comma, dregister(1), newline,
    opcode(muls), dregister(2), comma, dregister(1), newline.

integrable('/',2).
emit(open_code('/',2)) :-
    opcode(divs), dregister(2), comma, dregister(1), newline,
    opcode(extl), dregister(1), newline,
    opcode(asll), immediate(3), comma, dregister(1), newline.

integrable('-1+',1).
emit(open_code('-1+',1)) :-
    opcode(subql), immediate(8), comma, dregister(1), newline.

emit(cst(Object,Dest),Const,Const) :- non_gc(Object,Value), !,
    emit_non_gc(Value,Dest).
emit(cst(Object,Dest),Const,[const(Label,Object)|Const]) :- !,
    genlabel(Label),
    opcode(movl), label(Label), comma, destination(Dest), newline.

emit(make_closure(Proc,Nb_closed,Dest),Const,[const(Label,Proc)|Const]) :-
    genlabel(Label),
    Tag is Nb_closed+1-8192,
    opcode(movw), immediate(Tag),   comma, autoincr(5), newline,
    opcode(movl), aregister(5),     comma, destination(Dest), newline,
    opcode(movw), immediate(20153), comma, autoincr(5), newline,
    opcode(movl), label(Label),     comma, autoincr(5), newline.

emit(Instr,Const,Const) :- emit(Instr).

emit(ref_loc(Disp,Dest)) :-
    Byte_disp is Disp*4,
    opcode(movl), indirect(7,Byte_disp), comma, destination(Dest), newline.

emit(ref_clo(Depth,Disp,Dest)) :-
    Byte_depth is Depth*4-4,
    Byte_disp  is Disp*4+6,
    opcode(movl), indirect(7,Byte_depth), comma, aregister(0), newline,
    opcode(movl), indirect(0,Byte_disp),  comma, destination(Dest), newline.

emit(ref_glo(Disp,Dest)) :-
    Byte_disp is Disp*6+6,
    opcode(movl), indirect(6,Byte_disp), comma, destination(Dest), newline.

emit(get_loc(Disp,Dest)) :-
    emit(ref_loc(Disp,-1)),
    opcode(movl), indirect(0,0), comma, destination(Dest), newline.

emit(get_clo(Depth,Disp,Dest)) :-
    emit(ref_clo(Depth,Disp,-1)),
    opcode(movl), indirect(0,0), comma, destination(Dest), newline.

emit(box_loc(Disp,Dest)) :-
    emit(ref_loc(Disp,0)),
    opcode(movl), dregister(0), comma, autodecr(4), newline,
    opcode(movl), aregister(4), comma, destination(Dest), newline,
    opcode(movl), dregister(0), comma, autodecr(4), newline.

emit(box_clo(Depth,Disp,Dest)) :-
    emit(ref_clo(Depth,Disp,0)),
    opcode(movl), dregister(0), comma, autodecr(4), newline,
    opcode(movl), aregister(4), comma, destination(Dest), newline,
    opcode(movl), dregister(0), comma, autodecr(4), newline.

emit(set_loc(Disp,Src)) :-
    emit(ref_loc(Disp,-1)),
    opcode(movl), source(Src), comma, indirect(0,0), newline.

emit(set_clo(Depth,Disp,Src)) :-
    emit(ref_clo(Depth,Disp,-1)),
    opcode(movl), source(Src), comma, indirect(0,0), newline.

emit(set_glo(Disp,Src)) :-
    Byte_disp1 is Disp*6+4,
    Byte_disp2 is Disp*6+6,
    opcode(movw), immediate(20115), comma, indirect(6,Byte_disp1), newline,
    opcode(movl), source(Src),      comma, indirect(6,Byte_disp2), newline.

emit(move(X,X)) :- !.
emit(move(Src,Dest)) :-
    opcode(movl), destination(Src), comma, destination(Dest), newline.

emit(branch_always(Label)) :-
    opcode(bra), label(Label), newline.

emit(branch_if_false(Src,Label)) :-
    opcode(addql), immediate(3), comma, source(Src), newline,
    opcode(bcs),   label(Label), newline.

emit(push_continuation(Label)) :- !,
    opcode(pea), label(Label), newline.

emit(close_loc(Disp)) :-
    Byte_disp is Disp*4,
    opcode(movl), indirect(7,Byte_disp), comma, autoincr(5), newline.

emit(close_clo(Depth,Disp)) :-
    Byte_depth is Depth*4-4,
    Byte_disp  is Disp*4+6,
    opcode(movl), indirect(7,Byte_depth), comma, aregister(0), newline,
    opcode(movl), indirect(0,Byte_disp),  comma, autoincr(5), newline.

emit(sub_procedure(Label)) :-
    write_term(sub_procedure(Label)), newline.

emit(jump(Src,Nb_args)) :-
    nb_arg_code(Nb_args,Code),
    genlabel(Error),
    opcode(btst),  source(Src),     comma, dregister(7), newline,
    opcode(beqs),  label(Error), newline,
    opcode(movl),  source(Src),     comma, aregister(0), newline,
    opcode(tstw),  indirect(0,-2), newline,
    opcode(bpls),  label(Error), newline,
    opcode(moveq), immediate(Code), comma, dregister(0), newline,
    opcode(jmp),   indirect(0,0), newline,
    emit(label(Error)),
    opcode(jmp),   indirect(6,-510), newline.
    
emit(jump_glo(Disp,Nb_args)) :-
    nb_arg_code(Nb_args,Code),
    Byte_disp is Disp*6+4,
    opcode(moveq), immediate(Code), comma, dregister(0), newline,
    opcode(jmp),   indirect(6,Byte_disp), newline.

emit(return(Depth)) :-
    emit(dealloc(Depth),Const,Const),
    opcode(rts), newline.

emit(dealloc(0)) :- !.
emit(dealloc(Depth)) :- Depth =< 2, !,
    Byte_depth is Depth*4,
    opcode(addql), immediate(Byte_depth), comma, aregister(7), newline.
emit(dealloc(Depth)) :-
    Byte_depth is Depth*4,
    opcode(addw), immediate(Byte_depth), comma, aregister(7), newline.

emit(label(Label)) :-
    label(Label), write_char(58), newline.

emit(enter(Type,Nb_args,rest)) :-
    genlabel(Label),
    rest_enter(Type,Handler),
    opcode(movw), immediate(Nb_args), comma, aregister(0), newline,
    opcode(lea),  label(Label),       comma, aregister(1), newline,
    opcode(jmp),  indirect(6,Handler), newline,
    emit(sub_procedure(Label)).
emit(enter(plain,Nb_args,none)) :-
    genlabel(Error),
    genlabel(Continue),
    emit_arg_check(Nb_args,Error),
    opcode(cmpl), indirect(6,0), comma, aregister(7), newline,
    opcode(bhis), label(Continue), newline,
    emit(label(Error)),
    opcode(jmp),  indirect(6,-522), newline,
    emit(label(Continue)),
    emit_push_args(Nb_args).
emit(enter(closure,Nb_args,none)) :-
    genlabel(Error),
    genlabel(Continue),
    emit_arg_check(Nb_args,Error),
    opcode(cmpl),  indirect(6,0), comma, aregister(7), newline,
    opcode(bhis),  label(Continue), newline,
    emit(label(Error)),
    opcode(jmp),   indirect(6,-516), newline,
    emit(label(Continue)),
    opcode(subql), immediate(6), comma, indirect(7,0), newline,
    emit_push_args(Nb_args).

rest_enter(plain,-534).
rest_enter(closure,-528).
    
emit_arg_check(1,Label) :- !,
    opcode(bpls), label(Label), newline.
emit_arg_check(2,Label) :- !,
    opcode(bnes), label(Label), newline.
emit_arg_check(N,Label) :- N < 8, !,
    M is N+1,
    opcode(subqw), immediate(M), comma, dregister(0), newline,
    opcode(bnes),  label(Label), newline.
emit_arg_check(N,Label) :-
    M is N+1,
    opcode(subw), immediate(M), comma, dregister(0), newline,
    opcode(bnes), label(Label), newline.

emit_push_args(0) :- !.
emit_push_args(1) :- !,
    opcode(movl), dregister(1), comma, destination(push), newline.
emit_push_args(2) :- !,
    opcode(movl), dregister(2), comma, destination(push), newline,
    opcode(movl), dregister(1), comma, destination(push), newline.
emit_push_args(3) :- !,
    opcode(moveml), immediate(28672), comma, destination(push), newline.
emit_push_args(4) :- !,
    opcode(moveml), immediate(30720), comma, destination(push), newline.
emit_push_args(N) :- !,
    emit_push_arg(N),
    emit_push_args(4).

emit_push_arg(4) :- !.
emit_push_arg(N) :-
    Dist is -4*N, M is N-1,
    opcode(movl), indirect(6,Dist), comma, destination(push), newline,
    emit_push_arg(M).
    
nb_arg_code(1,-1) :- !.
nb_arg_code(2,0) :- !.
nb_arg_code(N,M) :- M is N+1.

non_gc(Val,Value) :- integer(Val), Value is Val*8.
non_gc([],-1).
non_gc(boo(f),-3).
non_gc(boo(t),-5).
non_gc(Val,-7) :- undefined(Val).
non_gc(chr(N),Value) :- Value is N*2-131071.

data_reg(N) :- integer(N), 1 =< N, N =< 4.

emit_non_gc(Value,Dest) :- -128=<Value, Value<128, \+ data_reg(Dest), !,
    opcode(moveq), immediate(Value), comma, dregister(0), newline,
    emit(move(0,Dest)).
emit_non_gc(Value,Dest) :- -128=<Value, Value<128, Dest>=0, !,
    opcode(moveq), immediate(Value), comma, destination(Dest), newline.
emit_non_gc(Value,Dest) :-
    opcode(movl), immediate(Value), comma, destination(Dest), newline.

label(Label) :-
    write_term(Label).

opcode(Op) :-  write_char(9), write_term(Op), write_char(9).

comma :- write_char(44).

immediate(N) :- write_char(35), write_term(N).

aregister(N) :- write_char(97), write_term(N).

dregister(N) :- write_char(100), write_term(N).

indirect(Areg,0) :- !,
    aregister(Areg),
    write_char(64).
indirect(Areg,Disp) :-
    aregister(Areg),
    write_char(64),
    write_char(40),
    write_term(Disp),
    write_char(41).

autoincr(N) :- indirect(N,0), write_char(43).

autodecr(N) :- indirect(N,0), write_char(45).

destination(push) :- !, autodecr(7).
destination(pop) :- !, autoincr(7).
destination(top) :- !, indirect(7,0).
destination(N) :- N < 0, !, M is -1-N, aregister(M).
destination(N) :- N > 4, !, Byte_disp is -4*N, indirect(6,Byte_disp).
destination(N) :- dregister(N).

source(push) :- !, destination(top).
source(pop) :- !, destination(top).
source(X) :- destination(X).

% -----------------------------------------------------------------------------

% Symbol generation routines.

genvar(V) :- gensym('#',V).

genlabel(V) :- gensym(l,V).

gensym(Name,S) :-
    gennum(N),
    name(Name,S1),
    name(N,S2),
    append(S1,S2,S3),
    name(S,S3), !.

gennum(N) :-
  last_num(LN),
  N is LN+1,
  retract(last_num(LN)), !,
  asserta(last_num(N)).

:- dynamic last_num/1.

last_num(0).

% -----------------------------------------------------------------------------

% Mutable variable analysis.

% Compute the set of all variables which are assigned in a given expression.

mut_vars(Expr,L) :- mut_vars(Expr,[],L).
mut_vars(cst(C),Env,[]).
mut_vars(ref(V),Env,[]).
mut_vars(set(V,E),Env,S) :- free_var(V,Env,X), mut_vars(E,Y), union(X,Y,S).
mut_vars(tst(X,Y,Z),Env,S) :- mut_list([X,Y,Z],Env,S).
mut_vars(pro(P,K,B,_),Env,S) :- append(P,Env,X), mut_vars(B,X,S).
mut_vars(app(L),Env,S) :- mut_list(L,Env,S).

mut_list([],Env,[]).
mut_list([E|Tail],Env,S) :-
    mut_vars(E,Env,X),
    mut_list(Tail,Env,Y),
    union(X,Y,S).

mut_bindings([],_,[]).
mut_bindings([V|Tail],Vals,S) :-
    memb(val(V,Val),Vals),
    mut_vars(Val,X),
    mut_bindings(Tail,Vals,Y),
    union(X,Y,S).

% -----------------------------------------------------------------------------

% Free variable analysis.

% Compute the set of all free variables in a given expression.

free_vars(Expr,L) :- free_vars(Expr,[],L).
free_vars(cst(C),Env,[]).
free_vars(ref(V),Env,S) :- free_var(V,Env,S).
free_vars(get(V),Env,S) :- free_var(V,Env,S).
free_vars(box(V),Env,S) :- free_var(V,Env,S).
free_vars(set(V,E),Env,S) :- free_var(V,Env,X), free_vars(E,Y), union(X,Y,S).
free_vars(tst(X,Y,Z),Env,S) :- free_list([X,Y,Z],Env,S).
free_vars(pro(P,K,B,_),Env,S) :- append(P,Env,X), free_vars(B,X,S).
free_vars(app(L),Env,S) :- free_list(L,Env,S).

free_list([],Env,[]).
free_list([E|Tail],Env,S) :-
    free_vars(E,Env,X),
    free_list(Tail,Env,Y),
    union(X,Y,S).

free_var(V,Env,[]) :- memb(V,Env), !.
free_var(V,Env,[V]).

% -----------------------------------------------------------------------------

% Normalization of expressions.

% The input is an S-expression that follows Scheme's syntax for expressions.
% The resulting expression will only contain the following structures:
%
%   cst(C)        a constant of value 'C'
%   ref(V)        a reference to variable 'V'
%   set(V,X)      an assignment of expression 'X' to variable 'V'
%   tst(X,Y,Z)    a conditionnal expression (X=test,Y=consequent,Z=alternative)
%   app(L)        an application (first expr in 'L'=procedure, rest=arguments)
%   pro(P,K,B,S)  a procedure (lambda-expression) having 'P' as formal
%                 parameters (i.e. the list of all parameters in order),
%                 the expression 'B' as body and 'S' as source-code ('K' is
%                 'rest' if there is a rest parameter and 'none' otherwise)
%
% Most of the conversion is done through macro expansion and is fairly
% straightforward.  The hardest expressions to convert are mutually or
% self-recursive expressions (such as 'letrec's, 'define's, etc...).
% They are converted first by doing a topological sort on the sub-expressions
% according to the variable dependencies between them.  The equivalent of a
% cascade of 'let's is generated for the expressions which are not really
% recursive.  When they really are recursive (i.e. a cycle has been discovered
% while doing the topological sort), a method involving a kind of 'Y' operator
% is used.  I will not describe it in detail but here is an example that gives
% a flavor of what is done:
%
% (letrec ((fact (lambda (x) (if (< x 2) 1 (* x (fact (- x 1))))))) (fact 5))
%
% is converted into the equivalent of:
%
% (let ((fact (lambda (fact)
%               (lambda (x)
%                 (let ((fact (fact fact)))
%                   (if (< x 2) 1 (* x (fact (- x 1)))))))))
%   (let ((fact (fact fact)))
%     (fact 5)))
%
% There is an added complication when the recursive expressions are bound
% to mutable variables.  In this case, an allocate/assign/use form must
% be generated.  For example,
%
% (letrec ((loop (lambda () (loop)))) (set! loop read))
%
% is transformed into the equivalent of:
%
% (let ((loop 'undefined))
%   (set! loop (lambda () (loop)))
%   (set! loop read))

expand([H|T],X) :- !, expnd([H|T],X).
expand(V,ref(V)) :- symbol(V), !.
expand(C,cst(C)).

expnd([quote,X],cst(X)) :- !.
expnd(['set!',V,E],set(V,X)) :- !, expand(E,X).
expnd([if,X,Y],Z) :- undefined(U), !, expand([if,X,Y,U],Z).
expnd([if,X,Y,Z],tst(A,B,C)) :- !, expand(X,A), expand(Y,B), expand(Z,C).
expnd([lambda,Parms|X],pro(P,K,B,[lambda,Parms|X])) :- !,
    parameters(Parms,P,K), body(X,B).
expnd([letrec,Bindings|Exprs],Y) :- !, body(Exprs,X), letrec(Bindings,X,Y).
expnd([begin|Tail],Y) :- !, expnd_list(Tail,X), begin(X,Y).
expnd(X,Z) :- macro(X,Y), !, expand(Y,Z).
expnd(X,Z) :- expnd_list(X,Y), add_trace(X,Y,Z).

add_trace(X,Y,app([pro(Temps,none,Z,[])|Y])) :- option(trace), !,
    make_temps(Y,Temps),
    ref_list(Temps,Refs),
    begin([app([ref('#trace'),app([ref('#list')|Refs])]),app(Refs)],Z).
add_trace(X,Y,app(Y)).

make_temps([],[]).
make_temps([E|Tail1],[V|Tail2]) :- genvar(V), make_temps(Tail1,Tail2).

parameters(Param_pattern,Params,Kind) :- params(Param_pattern,Kind,[],Params).

params([],none,P,P) :- !.
params([V|Tail],R,P1,P3) :- !, param_add(V,P1,P2), params(Tail,R,P2,P3).
params(V,rest,P1,P2) :- param_add(V,P1,P2).

param_add(V,_,_) :- \+ symbol(V), !,
    error("Variable name must be a symbol").
param_add(V,P,_) :- memb(V,P), !,
    error("Duplicate variable name in binding list").
param_add(V,P1,P2) :- append(P1,[V],P2).

expnd_list([],[]).
expnd_list([X|Tail1],[Y|Tail2]) :- expand(X,Y), expnd_list(Tail1,Tail2).

begin([E],E).
begin([E|Tail],app([pro([V],none,X,[]),E])) :- begin(Tail,X), genvar(V).

body(Exprs,Z) :-
    local_defs(Exprs,Defs,Body),
    expnd_list(Body,X),
    begin(X,Y),
    letrec(Defs,Y,Z).

local_defs([[define|Def1]|Tail1],[Def2|Tail2],B) :- !,
    definition(Def1,Def2),
    local_defs(Tail1,Tail2,B).
local_defs(B,[],B).

definition([[Variable|Formals]|Body],[Variable,[lambda,Formals|Body]]) :- !.
definition([Variable,Expression],[Variable,Expression]) :- !.
definition([Variable],[Variable,U]) :- undefined(U).

letrec(Bindings,Body,X) :-
    split(Bindings,Vars,Vals),
    dependency_graph(Vals,Vars,Dep),
    topological_sort(Dep,Binding_order),
    bind_in_order(Binding_order,Body,Vals,X).

split([],[],[]).
split([[Var,Val]|Tail1],Vars2,[val(Var,X)|Tail2]) :-
    expand(Val,X),
    split(Tail1,Vars1,Tail2),
    union([Var],Vars1,Vars2).

dependency_graph([],_,[]).
dependency_graph([val(Var,Val)|Tail1],Vars,[node(Var,Dep,_)|Tail2]) :-
    free_vars(Val,L),
    intersection(Vars,L,Dep),
    dependency_graph(Tail1,Vars,Tail2).

bind_in_order([],Body,Vals,Body).
bind_in_order([Bindings|Tail],Body,Vals,X) :-
    bind_in_order(Tail,Body,Vals,New_body),
    bind_level(Bindings,New_body,Vals,X).

bind_level(V,Body,Vals,app([pro([V],none,Body,[]),Val])) :- symbol(V), !,
    memb(val(V,Val),Vals).
bind_level(L,Body,Vals,X) :- lambdas(L,Vals), !,
    mut_bindings(L,Vals,Mut1),
    mut_vars(Body,Mut2),
    union(Mut1,Mut2,Mut3),
    intersection(Mut3,L,Mut),
    difference(L,Mut,Non_mut),
    bind_cyclic(Mut,Non_mut,Body,Vals,X).
bind_level(_,_,_,_) :-
    error("untransformable cyclical definition").

lambdas([],_).
lambdas([V|Tail],Vals) :- memb(val(V,pro(_,_,_,_)),Vals), lambdas(Tail,Vals).

bind_cyclic([],Non_mut,Body,Vals,X) :- !,
    bind_non_mut(Non_mut,Body,Vals,X).
bind_cyclic(Mut,Non_mut,Body,Vals,app([pro(Mut,none,Z,[])|Undefs])) :- !,
    bind_mut(Mut,Vals,Undefs,Assignments),
    append(Assignments,[Body],X),
    begin(X,Y),
    bind_non_mut(Non_mut,Y,Vals,Z).

bind_mut([],_,[],[]).
bind_mut([V|Tail1],Vals,[U|Tail2],[set(V,Val)|Tail3]) :-
    undefined(U),
    memb(val(V,Val),Vals),
    bind_mut(Tail1,Vals,Tail2,Tail3).

bind_non_mut([],Body,_,Body) :- !.
bind_non_mut(L,Body,Vals,
             app([pro(L,none,app([pro(L,none,Body,[])|V1]),[])|V2])) :-
    fix_procs1(L,L,V1),
    fix_procs2(L,L,Vals,V1,V2).

fix_procs1(L,[],[]).
fix_procs1(L,[V|Tail1],[app(X)|Tail2]) :-
    ref_list([V|L],X),
    fix_procs1(L,Tail1,Tail2).

fix_procs2(L,[],_,_,[]).
fix_procs2(L,[V|T1],Vals,V1,
           [pro(L,none,pro(X,Y,app([pro(L,none,Z,[])|V1]),S),[])|T2]) :-
    memb(val(V,pro(X,Y,Z,S)),Vals),
    fix_procs2(L,T1,Vals,V1,T2).

ref_list([],[]).
ref_list([V|Tail1],[ref(V)|Tail2]) :- ref_list(Tail1,Tail2).

undefined(spc(undef)).
% -----------------------------------------------------------------------------

% Macro definitions.

%   (quasiquote A)  -->

macro([quasiquote,X],Y) :- template(X,1,Y).

template(X,0,X) :- !.
template([unquote,X],1,X) :- !.
template(['unquote-splicing'],1,_) :- !,
    error("Misplaced 'unquote-splicing' special form").
template([quasiquote,X],N,Y) :- !, M is N+1, list_template([quasiquote,X],M,Y).
template([unquote,X],N,Y) :- !, M is N-1, list_template([unquote,X],M,Y).
template([Car|Cdr],N,Y) :- list_template([Car|Cdr],N,Y).
template(vec(L),N,Y) :- vector_template(L,N,X), vectorize_form(X,Y).
template(X,N,[quote,X]).

list_template([['unquote-splicing',X]],1,X) :- !.
list_template([['unquote-splicing',X]|Cdr],1,Y) :- !,
    template(Cdr,1,A),
    append_forms(X,A,Y).
list_template([Car|Cdr],N,Y) :-
    template(Car,N,A),
    template(Cdr,N,B),
    cons_forms(A,B,Y).

vector_template([['unquote-splicing',X]],1,X) :- !.
vector_template([['unquote-splicing',X]|Cdr],1,Y) :- !,
    vector_template(Cdr,1,A),
    append_forms(X,A,Y).
vector_template([],N,[]) :- !.
vector_template([Car|Cdr],N,Y) :-
    template(Car,N,A),
    vector_template(Cdr,N,B),
    cons_forms(A,B,Y).

append_forms([quote,X],[quote,Y],[quote,Z]) :- !, append(X,Y,Z).
append_forms(X,Y,['#append',X,Y]).

cons_forms([quote,X],[quote,Y],[quote,[X|Y]]) :- !.
cons_forms(X,Y,['#cons',X,Y]).

vectorize_form([quote,X],[quote,vec(X)]) :- !.
vectorize_form(X,['#list->vector',X]).

%   (unquote A)  -->  error

macro([unquote,X],_) :-
    error("Misplaced 'unquote' special form").

%   (unquote-splicing A)  -->  error

macro(['unquote-splicing',X],_) :-
    error("Misplaced 'unquote-splicing' special form").

%   (let ((a A)...) B C...)  -->  ((lambda (a...) B C...) A...)
% and
%   (let name ((a A)...) B C...)  -->
%   ((letrec ((name (lambda (a...) B C...))) name) A...)

macro([let,Name,Bindings|Body],
      [[letrec,[[Name,[lambda,Vars|Body]]],Name]|Exprs]) :- symbol(Name), !,
    let_bindings(Bindings,Vars,Exprs).
macro([let,Bindings|Body],[[lambda,Vars|Body]|Exprs]) :-
    let_bindings(Bindings,Vars,Exprs).

let_bindings([],[],[]).
let_bindings([[V,E]|X],[V|Y],[E|Z]) :- let_bindings(X,Y,Z).

%   (let* () A B...)  -->  (let () A B...)
% and
%   (let* ((a A)) B C...)  -->  (let ((a A)) B C...)
% and
%   (let* ((a A) (b B) (c C)...) D E...)  -->
%   (let ((a A)) (let* ((b B) (c C)...) D E...)

macro(['let*',[]|Body],[let,[]|Body]) :- !.
macro(['let*',[[V,E]]|Body],[let,[[V,E]]|Body]) :- !.
macro(['let*',[[V,E]|Tail]|Body],[let,[[V,E]],['let*',Tail|Body]]).

%   (and A)  -->  A
% and
%   (and A B C...)  -->  (let ((@ A)) (if @ (and B C...) @))

macro([and,E],E) :- !.
macro([and,E|Tail],[let,[[V,E]],[if,V,[and|Tail],V]]) :- genvar(V).

%   (or A)  -->  A
% and
%   (or A B C...)  -->  (let ((@ A)) (if @ @ (or B C...)))

macro([or,E],E) :- !.
macro([or,E|Tail],[let,[[V,E]],[if,V,V,[or|Tail]]]) :- genvar(V).

%   (cond)  -->  ?
% and
%   (cond (else A B...))  -->  (begin A B...)
% and
%   (cond (A) (B C...)...)  -->  (or A (cond (B C...)...))
% and
%   (cond (A B C...) (D E...)...)  -->  (if A (begin B C...) (cond (D E...)...))
% and
%   (cond (A => B) (C D...)...)  -->  (let ((@ A)) (if @ (B @)
%                                                    (cond (C D...)...)))

macro([cond],U) :- !, undefined(U).
macro([cond,[else|Tail]],[begin|Tail]) :- !.
macro([cond,[E]|Tail],[or,E,[cond|Tail]]) :- !.
macro([cond,[E,'=>',P]|Tail],[let,[[V,E]],[if,V,[P,V],[cond|Tail]]]) :- !,
    genvar(V).
macro([cond,[E|Tail1]|Tail2],[if,E,[begin|Tail1],[cond|Tail2]]).

%   (case A ((x...) B C...)...)  -->
%   (let ((@ A)) (cond ((memv @ '(x...)) B C...)...))

macro([case,Key|Clauses],[let,[[V,Key]],[cond|X]]) :-
    genvar(V),
    cases(V,Clauses,X).

cases(V,[],[]) :- !.
cases(V,[[else|Tail]],[[else|Tail]]) :- !.
cases(V,[[Set|Tail1]|Tail2],[[['#memv',V,[quote,Set]]|Tail1]|X]) :-
    cases(V,Tail2,X).

%   (define ...)  -->  error

macro([define|_],_) :- error("Misplaced 'define' special form").

%   (delay A)  -->  (make-promise (lambda () A))

macro([delay,E],['#make-promise',[lambda,[],E]]).

%   (do ((a A B)...) (C D...) E F...)  -->
%   (let @ ((a A)...) (if C (begin ? D...) (let () E F... (@ a/B...))))

macro([do,Bindings,[Test|Result]|Body],
      [let,Loop,Inits,[if,Test,[begin,U|Result],[let,[]|New_body]]]) :-
    genvar(Loop),
    undefined(U),
    do_bindings(Bindings,Inits,Steps),
    append(Body,[[Loop|Steps]],New_body).

do_bindings([],[],[]).
do_bindings([[V,I]|X],[[V,I]|Y],[V|Z]) :- do_bindings(X,Y,Z).
do_bindings([[V,I,S]|X],[[V,I]|Y],[S|Z]) :- do_bindings(X,Y,Z).

% -----------------------------------------------------------------------------

% Alpha conversion (renaming of variables) and assignment conversion.

% This phase renames all of the variables local to the expression (to eliminate
% aliasing problems) and adds 'boxes' (cells) to handle assignment to local
% variables.  For each mutable variable (i.e. a local variable that is assigned
% somewhere in the expression), create a box containing the value of the
% variable.  References to mutable variables is done by dereferencing the box
% it is associated to.  For example, the normal form of this expression:
%
% (lambda (x y) (set! x (- x y)) x)
%
% is transformed into the equivalent of:
%
% (lambda (x y) (let ((x (box x))) (set-box! x (- (get x) y)) (get x)))

alpha(Expr,C) :- alpha(Expr,C,[]).

alpha(cst(C),cst(C),Env).
alpha(ref(V),get(T),Env) :- memb(var(V,T,mut),Env), !.
alpha(ref(V),ref(T),Env) :- memb(var(V,T,non_mut),Env), !.
alpha(ref(V),ref(V),Env).
alpha(set(V,E),set(T,C),Env) :- memb(var(V,T,mut),Env), !, alpha(E,C,Env).
alpha(set(V,E),set(V,C),Env) :- alpha(E,C,Env).
alpha(tst(X,Y,Z),tst(A,B,C),Env) :- alpha_list([X,Y,Z],[A,B,C],Env).
alpha(app(L),app(C),Env) :- alpha_list(L,C,Env).
alpha(pro(Params1,Kind,Body1,S),pro(Params2,Kind,Body3,S),Env) :-
    mut_vars(Body1,Mutable),
    rename(Mutable,Params1,Params2,Bindings,Box1,Box2),
    append(Bindings,Env,New_env),
    alpha(Body1,Body2,New_env),
    alpha_pro(Box1,Box2,Body2,Body3).

alpha_pro([],_,Body,Body) :- !.
alpha_pro(Box1,Box2,Body,app([pro(Box2,none,Body,[])|X])) :- boxes(Box1,X).

boxes([],[]).
boxes([V|Tail1],[box(V)|Tail2]) :- boxes(Tail1,Tail2).

rename(Mut,[],[],[],[],[]).
rename(Mut,[V|T1],[X|T2],[var(V,Y,mut)|T3],[X|T4],[Y|T5]) :- memb(V,Mut), !,
    genvar(X), genvar(Y), rename(Mut,T1,T2,T3,T4,T5).
rename(Mut,[V|T1],[X|T2],[var(V,X,non_mut)|T3],T4,T5) :-
    genvar(X), rename(Mut,T1,T2,T3,T4,T5).

alpha_list([],[],Env).
alpha_list([E|T1],[C|T2],Env) :- alpha(E,C,Env), alpha_list(T1,T2,Env).

% -----------------------------------------------------------------------------

% Closure analysis.

% For every procedure definition (i.e. lambda-expression) in the given
% expression, compute the set of closed variables of the procedure
% and the set of parameters which are referenced in the procedure
% and augment the procedure definition by these sets.  A closed variable
% is a variable that is declared in a lambda-expression and used
% in a sub-lambda-expression.  For example, the expression:
%
% (lambda (x y z) (map (lambda (n) (+ y n)) x))
%
% would be augmented to this:
%
% (lambda (x y z) [] [x y] (map (lambda (n) [y] [n] (+ y n)) x))
%
% because,
% 'y' is a closed variable,
% 'map' and '+' are global variables,
% 'x', 'z' and 'n' are non-closed local variables.

closurize(Expr,C) :- closurize(Expr,C,[]).

closurize(cst(C),cst(C),Env).
closurize(ref(V),ref(V),Env).
closurize(get(V),get(V),Env).
closurize(box(V),box(V),Env).
closurize(set(V,E),set(V,C),Env) :-
    closurize(E,C,Env).
closurize(tst(X,Y,Z),tst(A,B,C),Env) :-
    closurize_list([X,Y,Z],[A,B,C],Env).
closurize(app(L),app(C),Env) :-
    closurize_list(L,C,Env).
closurize(pro(Params,Kind,Body1,S),pro(Params,Kind,Closed,Used,Body2,S),Env) :-
    free_vars(Body1,Free),
    intersection(Free,Env,Closed),
    make_set(Params,Vars),
    intersection(Free,Vars,Used),
    union(Vars,Env,New_env),
    closurize(Body1,Body2,New_env).

closurize_list([],[],Env).
closurize_list([E|Tail1],[C|Tail2],Env) :-
    closurize(E,C,Env),
    closurize_list(Tail1,Tail2,Env).

% -----------------------------------------------------------------------------

% Code generation for the virtual machine.

gen_program(Expr,[obj(entry,pro(Main_code,[]))]) :-
    initial_global_env(G1),
    gen(Expr,t,env([],[],G1),env([],[],G2),Main_code,[]).

gen_procedure(Params,Kind,Closed,Body,Source,pro(Code1,Source),G1,G2) :-
    gen_proc_entry(Params,Kind,Closed,Locals,Code1,Code2),
    gen(Body,t,env(Locals,Closed,G1),env(Locals,Closed,G2),Code2,[]).

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

% Use DCG for code generation.

gen_proc_entry(Params1,Kind,[],Params2) --> !,
    {length(Params1,Nb_args), reverse(Params1,Params2)},
    [enter(plain,Nb_args,Kind)].
gen_proc_entry(Params1,Kind,Closed,[temp|Params2]) -->
    {length(Params1,Nb_args), reverse(Params1,Params2)},
    [enter(closure,Nb_args,Kind)].

% gen(Expr,Dest,Env1,Env2)
%
% Generate intermediate code for expression Expr given the run-time environment
% Env1.  Env2 is the environment after having executed Expr.  Dest specifies
% where to put the result: 'd' means discard the result, 't' means the
% expression is in tail position, 'push' means push the result on the run-time
% stack otherwise Dest is a virtual register number.

gen(cst(C),d,E1,E1) --> !, [].
gen(cst(C),t,E1,E2) --> !, gen(cst(C),1,E1,E2), gen_return(E2).
gen(cst(C),D,E1,E2) --> !, {fix(D,E1,E2)}, [cst(C,D)].

gen(ref(V),d,E1,E1) --> !, [].
gen(ref(V),t,E1,E2) --> !, gen(ref(V),1,E1,E2), gen_return(E2).
gen(ref(V),D,E1,E2) --> {loc(V,E1,X), fix(D,E1,E2)}, !, [ref_loc(X,D)].
gen(ref(V),D,E1,E2) --> {clo(V,E1,X,Y), fix(D,E1,E2)}, !, [ref_clo(X,Y,D)].
gen(ref(V),D,E1,E3) --> {glo(V,E1,E2,X), fix(D,E2,E3)}, !, [ref_glo(X,D)].

gen(get(V),d,E1,E1) --> !, [].
gen(get(V),t,E1,E2) --> !, gen(get(V),1,E1,E2), gen_return(E2).
gen(get(V),D,E1,E2) --> {loc(V,E1,X), fix(D,E1,E2)}, !, [get_loc(X,D)].
gen(get(V),D,E1,E2) --> {clo(V,E1,X,Y), fix(D,E1,E2)}, !, [get_clo(X,Y,D)].

gen(box(V),d,E1,E1) --> !, [].
gen(box(V),t,E1,E2) --> !, gen(box(V),1,E1,E2), gen_return(E2).
gen(box(V),D,E1,E2) --> {loc(V,E1,X), fix(D,E1,E2)}, !, [box_loc(X,D)].
gen(box(V),D,E1,E2) --> {clo(V,E1,X,Y), fix(D,E1,E2)}, !, [box_clo(X,Y,D)].

gen(set(V,E),d,E1,E2) --> !, gen(set(V,E),1,E1,E2).
gen(set(V,E),t,E1,E2) --> !, gen(set(V,E),1,E1,E2), gen_return(E2).
gen(set(V,E),D,E1,E3) --> !, gen(E,D,E1,E2), gen_set(V,D,E2,E3).

gen_set(V,S,E1,E1) --> {loc(V,E1,X)}, !, [set_loc(X,S)].
gen_set(V,S,E1,E1) --> {clo(V,E1,X,Y)}, !, [set_clo(X,Y,S)].
gen_set(V,S,E1,E2) --> {glo(V,E1,E2,X)}, !, [set_glo(X,S)].

gen(tst(X,Y,Z),t,E1,E5) --> !,
    {genlabel(Label1)},
    gen(X,1,E1,E2),
    [branch_if_false(1,Label1)],
    gen(Y,t,E2,E3),
    [label(Label1)],
    {join_env(E2,E3,E4)},
    gen(Z,t,E4,E5).
gen(tst(X,Y,Z),D,E1,E5) -->
    {genlabel(Label1), genlabel(Label2)},
    gen(X,1,E1,E2),
    [branch_if_false(1,Label1)],
    gen(Y,D,E2,E3),
    [branch_always(Label2)],
    [label(Label1)],
    {join_env(E2,E3,E4)},
    gen(Z,D,E4,E5),
    [label(Label2)].

gen(app([pro(P,none,C,U,B,_)|A]),D,E1,E2) --> !, gen_app_pro(P,C,U,B,A,D,E1,E2).
gen(app(L),d,E1,E2) --> !, gen(app(L),1,E1,E2).
gen(app([ref(V)|Args]),t,E1,E2) -->
    {length(Args,Nb_args), option(int(V)), integrable(V,Nb_args)}, !,
    gen_app_args(Args,1,E1,E2),
    [open_code(V,Nb_args)],
    gen_return(E2).
gen(app([ref(V)|Args]),t,E1,E3) --> {glo(V,E1,E2,X)}, !,
    gen_app_args(Args,1,E2,E3),
    {length(Args,Nb_args), depth(E3,Depth)},
    [dealloc(Depth)],
    [jump_glo(X,Nb_args)].
gen(app([Proc|Args]),t,E1,E2) --> !,
    gen_app_args([Proc|Args],0,E1,E2),
    {depth(E2,Depth), length(Args,Nb_args)},
    [dealloc(Depth)],
    [jump(0,Nb_args)].
gen(app([ref(V)|Args]),D,E1,E4) -->
    {length(Args,Nb_args), option(int(V)), integrable(V,Nb_args)}, !,
    gen_app_args(Args,1,E1,E2),
    [open_code(V,Nb_args)],
    [move(1,D)],
    {join_env(E1,E2,E3), fix(D,E3,E4)}.
gen(app([ref(V)|Args]),D,E1,E5) --> {glo(V,E1,E2,X)}, !,
    gen_app_args(Args,1,E2,E3),
    {genlabel(Label), length(Args,Nb_args)},
    [push_continuation(Label)],
    [jump_glo(X,Nb_args)],
    [sub_procedure(Label)],
    [move(1,D)],
    {join_env(E1,E3,E4), fix(D,E4,E5)}.
gen(app([Proc|Args]),D,E1,E4) --> !,
    gen_app_args([Proc|Args],0,E1,E2),
    {genlabel(Label), length(Args,Nb_args)},
    [push_continuation(Label)],
    [jump(0,Nb_args)],
    [sub_procedure(Label)],
    [move(1,D)],
    {join_env(E1,E2,E3), fix(D,E3,E4)}.

gen_app_args(L,N,E1,E3) -->
    {split_args(L,N,Non_trivial,Trivial)},
    gen_non_trivial_args(Non_trivial,E1,E2),
    gen_trivial_args(Trivial,E2,E3).

gen_non_trivial_args([],E1,E1) --> !, [].
gen_non_trivial_args([arg(Dest,Arg)],E1,E2) --> !,
    gen(Arg,1,E1,E2),
    [move(1,Dest)].
gen_non_trivial_args([arg(Dest,Arg)|Tail],E1,E4) --> !,
    gen(Arg,push,E1,E2),
    gen_non_trivial_args(Tail,E2,E3),
    [move(pop,Dest)],
    {fix(pop,E3,E4)}.

gen_trivial_args([],E1,E1) --> [].
gen_trivial_args([arg(Dest,Arg)|Tail],E1,E3) -->
    gen(Arg,Dest,E1,E2),
    gen_trivial_args(Tail,E2,E3).

gen_app_pro(P,C,U,B,A,D,E1,E5) -->
    gen_alloc_args(P,U,A,N,E1,E2),
    gen_body(B,D,N,E2,E3),
    {join_env(E1,E3,E4), fix(D,E4,E5)}.

gen_alloc_args([],U,[],0,E1,E1) --> !, [].
gen_alloc_args([V|Tail1],U,[A|Tail2],N,E1,E4) --> {memb(V,U)}, !,
    gen(A,push,E1,E2),
    {rename_temp(V,E2,E3)},
    gen_alloc_args(Tail1,U,Tail2,M,E3,E4),
    {N is M+1}.
gen_alloc_args([V|Tail1],U,[A|Tail2],N,E1,E3) --> !,
    gen(A,d,E1,E2),
    gen_alloc_args(Tail1,U,Tail2,N,E2,E3).

gen_body(B,D,0,E1,E2) --> !, gen(B,D,E1,E2).
gen_body(B,t,N,E1,E2) --> !, gen(B,t,E1,E2).
gen_body(B,push,N,E1,E2) --> !,
    gen(B,1,E1,E2),
    [dealloc(N)],
    [move(1,push)].
gen_body(B,D,N,E1,E2) --> !,
    gen(B,D,E1,E2),
    [dealloc(N)].

gen(pro(Params,Kind,Closed,Used,Body,Source),d,E1,E1) --> !, [].
gen(pro(Params,Kind,Closed,Used,Body,Source),t,E1,E2) --> !,
    gen(pro(Params,Kind,Closed,Used,Body,Source),1,E1,E2),
    gen_return(E2).
gen(pro(Params,Kind,[],Used,Body,Source),D,env(L,C,G1),E) --> !,
    {gen_procedure(Params,Kind,[],Body,Source,Proc,G1,G2),
     fix(D,env(L,C,G2),E)},
    [cst(Proc,D)].
gen(pro(Params,Kind,Closed,Used,Body,Source),D,env(L,C,G1),E) --> !,
    {gen_procedure(Params,Kind,Closed,Body,Source,Proc,G1,G2),
     fix(D,env(L,C,G2),E), length(Closed,Nb_closed)},
    [make_closure(Proc,Nb_closed,D)],
    gen_close_var(Closed,E).

gen_close_var([],Env) --> [].
gen_close_var([V|Tail],Env) --> {loc(V,Env,X)}, !,
    [close_loc(X)],
    gen_close_var(Tail,Env).
gen_close_var([V|Tail],Env) --> {clo(V,Env,X,Y)}, !,
    [close_clo(X,Y)],
    gen_close_var(Tail,Env).

gen_return(Env) -->
    {depth(Env,Depth)},
    [return(Depth)].

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

% Utilities for code generation.

join_env(env(L1,C,G1),env(L2,C,G2),env(L1,C,G2)).

depth(env(L,C,G),Depth) :- length(L,Depth).

loc(V,env(L,C,G),Y) :- length(L,Depth), position(V,L,X), Y is Depth-X-1.

clo(V,env(L,C,G),Depth,X) :- length(L,Depth), position(V,C,X).

glo(V,env(L,C,G),env(L,C,G),X) :-
    \+ memb(V,L),
    \+ memb(V,C),
    position(V,G,X), !.
glo(V,env(L,C,G1),env(L,C,G2),X) :-
    \+ memb(V,L),
    \+ memb(V,C),
    append(G1,[V],G2),
    position(V,G2,X).

position(X,L,P) :- position(X,L,P,0).
position(X,[X|Tail],N,N) :- !.
position(X,[_|Tail],P,N) :- M is N+1, position(X,Tail,P,M).

fix(push,env(L1,C,G),env(L2,C,G)) :- !, append(L1,[temp],L2).
fix(pop,env(L1,C,G),env(L2,C,G)) :- !, append(L2,[_],L1).
fix(Dest,Env,Env).

rename_temp(V,env(L1,C,G),env(L3,C,G)) :-
    append(L2,[temp],L1),
    append(L2,[V],L3).

split_args([],N,[],[]).
split_args([Arg|Tail1],N,Tail2,[arg(N,Arg)|Tail3]) :- trivial(Arg), !,
    M is N+1,
    split_args(Tail1,M,Tail2,Tail3).
split_args([Arg|Tail1],N,Tail2,Tail3) :-
    M is N+1,
    split_args(Tail1,M,Tail4,Tail3),
    append(Tail4,[arg(N,Arg)],Tail2).

trivial(cst(_)).
trivial(ref(_)).
trivial(get(_)).
trivial(box(_)).
% -----------------------------------------------------------------------------

% Set manipulation.

memb(X,[X|T]) :- !.
memb(X,[Y|T]) :- memb(X,T).

remove(E,[],[]) :- !.
remove(E,[E|T],T) :- !.
remove(E,[X|T],[X|S]) :- remove(E,T,S).

make_set([],[]).
make_set([X|Y],Z) :- make_set(Y,S), union([X],S,Z).

union([],S,S) :- !.
union(S,[],S) :- !.
union([E|T1],[E|T2],[E|T3]) :- !, union(T1,T2,T3).
union([E1|T1],[E2|T2],[E1|T3]) :- E1 @< E2, !, union(T1,[E2|T2],T3).
union([E1|T1],[E2|T2],[E2|T3]) :- E1 @> E2, !, union([E1|T1],T2,T3).

intersection([],S,[]) :- !.
intersection(S,[],[]) :- !.
intersection([E|T1],[E|T2],[E|T3]) :- !, intersection(T1,T2,T3).
intersection([E1|T1],[E2|T2],T3) :- E1 @< E2, !, intersection(T1,[E2|T2],T3).
intersection([E1|T1],[E2|T2],T3) :- E1 @> E2, !, intersection([E1|T1],T2,T3).

difference([],S,[]) :- !.
difference(S,[],S) :- !.
difference([E|T1],[E|T2],T3) :- !, difference(T1,T2,T3).
difference([E1|T1],[E2|T2],[E1|T3]) :- E1 @< E2, !, difference(T1,[E2|T2],T3).
difference([E1|T1],[E2|T2],T3) :- E1 @> E2, !, difference([E1|T1],T2,T3).

% -----------------------------------------------------------------------------

% Graph manipulation.

% A graph is a set of nodes of the form: node(Name,Set_of_neighbors,Info)

% Transitive closure of a graph.

transitive_closure(G1,G2) :-
    add_neighbors(G1,X),
    transitive_closure(G1,X,G2), !.
transitive_closure(X,X,X).
transitive_closure(_,G1,G2) :- transitive_closure(G1,G2).

add_neighbors(G1,G2) :- add_neighbors(G1,G1,G2).
add_neighbors(G,[],[]).
add_neighbors(G,[node(X,N1,Info)|Tail1],[node(X,N2,Info)|Tail2]) :-
    union_of_neighbors(G,N1,N1,N2),
    add_neighbors(G,Tail1,Tail2).

union_of_neighbors(G,[],N,N).
union_of_neighbors(G,[X|Tail],N1,N3) :-
    memb(node(X,N,_),G),
    union(N,N1,N2),
    union_of_neighbors(G,Tail,N2,N3).

% Topological sorting (modified to handle cycles).

topological_sort(G1,L) :- transitive_closure(G1,G2), topo_sort(G2,L).

topo_sort([],[]).
topo_sort(G1,[X|Tail]) :- memb(node(X,[],_),G1), !,
    remove_node(X,G1,G2),
    topo_sort(G2,Tail).
topo_sort(G1,[N|Tail]) :-
    topo_sort_find_cycle(G1,G1,N),
    remove_nodes(N,G1,G2),
    topo_sort(G2,Tail).

topo_sort_find_cycle(G,[node(X,N,_)|_],N) :- memb(X,N), cyclic(G,N,N), !.
topo_sort_find_cycle(G,[_|Tail],N) :- topo_sort_find_cycle(G,Tail,N).

cyclic(G,N,[]).
cyclic(G,N,[X|Tail]) :- memb(node(X,N,_),G), cyclic(G,N,Tail).

remove_nodes([],G,G).
remove_nodes([X|Tail],G1,G3) :- remove_node(X,G1,G2), remove_nodes(Tail,G2,G3).

remove_node(X,[],[]) :- !.
remove_node(X,[node(X,_,_)|Tail1],Tail2) :- !, remove_node(X,Tail1,Tail2).
remove_node(X,[node(Y,N1,Info)|Tail1],[node(Y,N2,Info)|Tail2]) :-
    remove(X,N1,N2),
    remove_node(X,Tail1,Tail2).


end_of_file.

                        SIS Compiler documentation

                (Straightforward Implementation of Scheme)

                    (for SUN version 0.1, jan 26 1988)


                              By Marc Feeley

---------------------------------------------------------------------------


1. Using the compiler

The compiler comes with the following files:

  doc           this file
  sc            the compiler itself written in Quintus Prolog
  header.s      an assembly file containing the run-time system
  asm           command file to assemble the code generated by the compiler
  queens.scm    a couple of test files in scheme
  fib.scm
  tak.scm
  sort.scm

To use the compiler, write the scheme program you want to compile into a
file; the file must have the extension '.scm' (for the sake of brevity
let's call it 'source.scm').  Then start your Prolog and load 'sc'.  Type
the query 'ex.' to start the compiler.  It will then ask for the name of
the file to compile; type the file's name without the extension (ie.
source).  A trace of the compilation phases should appear.  The file
'source.s' will be generated which contains an MC68000 assembly language
program.  Exit Prolog and type 'asm source' this will assemble the
compiler's output and generate the file 'source', the executable image of
the program.  To execute, type 'source'.

It is possible to ask for the open-coding of certain procedure calls
(presently only car, cdr, +, -, *, /, -1+).  This can be done by
typing 'integrate(all).' before typing 'ex.'.  This speeds up execution
by a small amount.  However, open-coded procedures do not check the
type of their arguments or result, so be careful.  To remove open-coding
type 'integrate(none).'.  By typing the query 'debug(on).', the compiler
will generate code that will enable 'write' to print the source of the
procedures instead of just '#<procedure>'.  Type 'debug(off).' to
generate the usual code.

Although the compiler adheres fairly closely to R3RS there are certain
restrictions.  The most important of which are the lack of a GC and
the small number of primitive procedures implemented.  However, all
special forms are implemented.  You should read the comments in 'sc' for
more information.  Here is a list of the primitives which are implemented:

    not, eq?, pair?, cons, append, length, car, cdr, set-car!, set-cdr!,
    null?, =, <, >, +, -, *, /, -1+, force, write, newline, list, vector,
    list->vector, memq, assq, symbol?, vector?, string?, procedure?, number?,
    char?


In Quintus Prolog you can pre-compile 'sc' to save time on the following
uses of the compiler.  Simply type:

    compile(sc).

when you are in Quintus (this gives some warning messages) and then type:

    save('sc.bin').

On subsequent uses of the compiler, startup Quintus and then type the query:

    restore('sc.bin').

The compiler itself is written in a fairly portable fashion.  The main
system dependent code is for I/O.  Interface procedures have been written
for I/O so only these have to be modified.  The compiler uses 'retract'
and 'assert' statements (to generate symbols and so forth), which means
that some of the clauses have been declared 'dynamic'.  On other Prologs
the 'dynamic' declarations might have to be removed.


2. How does it work?

Compilation is a 3 step process:

  1 - Parse input (ie. generate parse tree for program)
  2 - Compile program (ie. generate intermediate code)
  3 - Generate assembly code (ie. transform interm. code to M68000 code)


2.1 Parsing

The first step consists in reading all the characters of the source file
into a list.  This is not very efficient but it simplifies parsing.
The list is parsed using a DCG grammar description of Scheme.  A parse
tree of the program is generated by the parser.  A simple list
representation similar to Scheme's own representation for S-expressions
is used.  For example, the expression

    (DEFINE (weird x) (list x "ABC" 123 #t #(1 2 3)))

is represented by the Prolog structure:

    [define,[weird,x],[list,x,str([65,66,67]),123,boo(t),vec([1,2,3])]]

Note: uppercase characters are automatically transformed to lowercase.


2.2 Compiling to intermediate code

This step takes the parse tree of the program and generates the
corresponding intermediate code (ie. pseudocode).  Before the intermediate
code can actually be generated, the expressions must first be put into
a normal form.  This simplifies case analysis in the code generator.
The normalization of the expressions is done in 3 steps.

    1 - Macro expansion (to take care of derived special forms)
    2 - Alphatization (ie. renaming of the variables) and
        assignment conversion
    3 - closure analysis

Most of macro expansion is straightforward.  It's simplicity stems from
Prolog's ease of doing pattern matching and case analysis.  The hardest
case to handle is mutually or self-recursive expressions (such as
'letrec's, 'define's, etc.).  I will explain this case using an example.
Suppose the expression to compile is:

    (letrec ((a (lambda (x) (b (+ x (c 1)))))           ; def1
             (b (lambda (x) (if (d x 0) (a x) (c x))))  ; def2
             (c (lambda (x) (- x)))                     ; def3
             (d (lambda (x y) (< x y))))                ; def4
      (a (c -30)))                                      ; body

The compiler will start by constructing the variable dependency graph
for the (local) variables in this expression.  Thus for the example:

     a <-----> b                   ( x --> y  means x depends on y )
       \     / |
        \   /  |
         V V   V
          c    d

It then does a topological sort on the graph to find out in which order
the variables should be bound.  The topological sort also discovers
the cycles in the dependencies; these must be handled in a special manner.
Cyclical dependencies conversion involves a kind of 'Y' operator.  For
the example we would obtain:

    (let ((c (lambda (x) (- x))))
      (let ((d (lambda (x y) (< x y))))
        (let ((a (lambda (a b)
                   (lambda (x)
                     (let ((a (a a b))
                           (b (b a b)))
                       (b (+ x (c 1)))))))
              (b (lambda (a b)
                   (lambda (x)
                     (let ((a (a a b))
                           (b (b a b)))
                       (if (d x 0) (a x) (c x)))))))
          (let ((a (a a b))
                (b (b a b)))
            (a (c -30))))))

Think about it, it works...  A more 'natural' conversion would be to
generate an allocate/assign/use form (as explained in the R3RS) but
our solution is entirely functional and through data-flow analysis the
compiler could recover the original expression's semantics (presently
the compiler is not that intelligent but it might come in the future).

After this phase, only the basic special form of expressions are left
in the program, ie.

    - constant reference       eg.  123
    - variable reference       eg.  x
    - assignement              eg.  (set! x 456)
    - conditional expression   eg.  (if a b c)
    - application              eg.  (list a b c)
    - lambda-expression        eg.  (lambda (b) b)

Alphatization consists of renaming the local variables to prevent
aliasing problems.  In this step, assignments are also converted into a
'functional' equivalent by introducing cells which hold the values
of mutable variables.  This simplifies the handling of closures and
continuations since after this conversion, the value of any (non-global)
variable always stays the same after it has been bound.  Only
data structures are mutable.

The last normalization step is closure analysis.  It consists in anotating
each lambda-expression with the set of its closed variables (ie. the
free, non-global variables which are referenced in its body).  Space for
these variables will be allocated in the closures generated for this
lambda-expression.

Once the program is normalized, it is passed to a DCG grammar in order to
generate the intermediate code.  Code generation is case driven and
is basically a post-order traversal of the normalized parse tree.
However, several special cases are recognized for which efficient code
can be generated.  This includes code for:

    - the application of a procedure bound to a global variable
    - the body of lambda-expressions with no closed variables
    - the application of a lambda-expression (ie. a 'let')
    - a 'let' variable which is not used in the body
    - ordering of arguments to evaluate trivial arguments last
      (ie. arguments which need only a small number of resources)


2.3 Assembly code generation

This step scans the intermediate code instruction stream and generates
the appropriate MC68000 code for them.  The intermediate code is machine
independent where as the assembly code generated is MC68000 and 'run-time
structure' dependent.  In theory, only this part has to be modified to
generate code for another machine.

In addition, this step is responsible for the 'dumping' of Scheme objects
to the code file.  These are most often the constants contained in the
source program.  For example, if the following expression were compiled:

    (member 'b '(a b c))

the symbols a, b and c and the list (a b c) would be 'dumped' to the code
file (and the symbol b would be dumped only once).


3. Benchmarks

We have run the compiler on a few benchmark programs.  No special declarations
were used and the programs were run on a SUN3.  Here are the results in secs:

                           Our comp.   MIT Scheme (compiled)

tak                          1.3
sort                         1.7
queens (without the trace)   1.7
fib (10 times (fib 20))      4.1


4. Run-time structure

A quick look at the benchmarks shows that the code generated is fairly
efficient.  The system's efficiency is mainly due to a cleverly designed
run-time architecture.  It has been designed in a way that makes frequent
operations perform quickly.  Some of the most frequent operations in Scheme
are:

    - checking type of an object
    - calling a procedure (very frequently the procedure comes from a
      global variable and has no closed variables)

In our system, objects are represented by tagged 32 bit pointers.  The
tag resides in the 3 least significant bits of the pointer.  The coding
is as follows:

    least 3 bits = 000   object is a 29 bit integer (ie. a fixnum)

    least 3 bits = 100   object is a pair whose address is given by the
                         pointer.  the address is where the car is and
                         the cdr is just BEFORE this address.

    least 2 bits =  10   object is a memory allocated object (other than
                         a pair) whose address is given by the pointer.
                         the word that precedes this address gives the
                         type of the object.

    least bit    =   1   object is either a special value ( (), #f, #t ),
                         a character or a 31 bit floating point number
                         the value 111..111 represents () and 111..101
                         represents #f.

With this coding integers can be added or substracted directly without
first having to extract the type (multiplication and division require
an additional 3 bit shift).

Pairs can also be manipulated easily.  Indirect addressing can be used to
access the car of the pair and predecrement addressing to access the cdr.

Procedures (which are memory allocated objects) can be called simply by
jumping to the pointer.  This is possible because all procedures
(including true closures) start with machine code.  True closures (ie.
procedures with closed variables) are represented like this:

         <- 16bits ->
         +----------+----------+----------+----------+----------+----
    ---> |JSR opcode|          |          |          |          | ...
         +----------+----------+----------+----------+----------+----
                     \_________ _________/ \_________ _________/
                               |                     |
                     Pointer to closure's      First closed       ...
                             body            variable's value

Thus the caller does not have to distinguish simple procedures from true
closures.  Environment related processing is done automatically (and if
needed) by the callee.  This implies that there is no overhead when
calling procedures with no closed variables (the most frequent case).

Testing for 'falsity' is also efficient.  Remember, in Scheme both () and
#f represent false.  In some implementations they are the same object
(which means you can't distinguish () from #f).  In our system we wanted
them to be different.  The representation is such that () and #f are the
2 highest object values whose least significant bit is 1.  By adding 3
and checking the carry flag one can determine if the object was () or #f
(ie. the carry is on) or if it was something else (ie. the carry flag is
off).

In order to have efficient type checks we have 3 dedicated data registers
that always contain the following masks:

    d5 = 00000001000000010000000100000001 for checking fixnums
    d6 = 00010000000100000001000000010000 for checking pairs
    d7 = 01000100010001000100010001000100 for checking mem. alloc. obj.

To test if a given data register contains a given type a single 'btst'
(bit test) instruction is required.  For example, branching to a label
if d1 contains a pair can be performed with the following code:

        btst    d1,d6      (use last 5 bits of d1 to index d6)
        bne     label      (branch if bit tested was 1)

A very common type check is making sure that only procedure objects are
called.  With our representation this involves 7 MC68000 inctructions.
It would be prohibitive to perform this check every time a procedure
is called.  In our system, a special trick is used for the frequent
case of calling procedures bound to global variables.  Global variables
are allocated in a table (one of the address register points to this
table).  Each global variable occupies 6 bytes; 4 bytes for the variable's
value and 2 bytes (just before the value) that encode the type of the
value.  These 2 bytes contain the opcode 'jmp' (long jump) if the value
is a procedure and a 'trap' opcode if it isn't a procedure of if the type
is not yet known.  When a procedure bound to a global variable needs to
be called a jump to the variable's opcode is done.  Most of the time
the value of the variable will be a procedure and it will be jumped to.
If this is not the case the trap handler is called; it will determine
if the value at the return address is a procedure or not.  If it is,
the opcode will be changed to 'jmp' and a jump to the procedure will
be performed.  If it is not, an error handler is called.  The price
one pays for this scheme is that assignments to global variables must
also change the variable's opcode back to a 'trap' opcode.  Since
assignments are infrequent in Scheme programs this is not a high price
to pay.

The procedure calling convention used in our system are as follows:

    - the first 4 arguments are passed in registers (d1, d2, d3 and d4)
      and the rest in fixed memory locations.
    - the register d0 is loaded with an argument count code just
      before jumping to the procedure.

The procedure called must check that it has been called with the correct
number of arguments.  To make this check efficient we have a special coding
for the argument count:

    number of arguments:  0   1   2   3   4   5   ...
    argument count code:  1  -1   0   4   5   6   ...

When the argument count code is loaded into d0 the flags are automatically
set in accordance to the code loaded.  For 1 argument the N (negative) flag is
set, for 2 arguments the Z (zero) flag is set.  Thus a single branch-on-
condition instruction is required for the check when 1 or 2 arguments are
expected (these are frequent cases).


5. Future plans

In its present state, the compiler is mainly useful for learning about
Scheme or Lisp compilation techniques.  Real applications can not be
developed due to the lack of a GC, debugger, support routines, etc.
In addition, since the compiler is written in Prolog the system is far
from being interactive.

In the future (and if time permits), we plan on doing some of the following:

    - rewritting the compiler in Scheme and cleaning up the code
    - adding a GC and a full bag of primitive procedures
    - turning the system into an interactive one (with read-eval-print loop)
    - adding some 'intelligence' to the compiler.

All comments regarding the SIS compiler are welcome (especially bug reports
and suggestions).


6. Acknowledgements

The compiler is the outgrowth of a course project for the 'Logic Programming'
course taught by Tim Hickey at Brandeis University.  We would like to thank
Tim for his wonderful course and for suggesting this project.



HEADER============


| -----------------------------------------------------------------------------
|
| This is the 'header.s' file which is appended to the front of the
| assembly program output by the compiler.  It contains run-time routines
| for the scheme system and M4 macro definitions which are needed to
| expand the macros in the compiler's output.

| -----------------------------------------------------------------------------
|
| Representation of objects:
|
| An object is represented using a 32 bit value.  When more than 32 bits
| are needed to represent an object, the 32 bits are actually a pointer
| to the object in memory.
|
|                                  28     28
| Fixnums (integers in the range -2   .. 2  -1):
|
|     XXXXXXXXXXXXXXXXXXXXXXXXXXXXX000
|     \------ integer value ------/
|
| Floating point numbers and other constants:
|
|     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX1
|     \-- encoding of f.p. number --/
|              or constant
|
| Pair (i.e. cons cell):
|                                           16 bits
|                                          +-------+           |
|                                          |       | \         |
|                                          |- - - -|  > cdr    | high mem.
|                                          |       | /         |
|                                          +-------+           |
|     XXXXXXXXXXXXXXXXXXXXXXXXXXXXX100 --> |       | \         V
|                                          |- - - -|  > car
|                                          |       | /
|                                          +-------+
|
| Hunk (i.e. object in mem. other than pair, e.g. vectors, strings, proc.):
|
|                                           16 bits
|                                          +-------+     |
|                                          |field1 |     |
|                                          |- - - -|     | high mem.
|     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX10 --> |field2 |     |
|                                          +-------+\    |
|                                          |       | |   V
|                                  dword 1 |- - - -| |
|                                          |       | |
|                                          +-------+ |
|                                     .    |   .   | |
|                                     .    |   .   | length (# of double words)
|                                     .    |   .   | |
|                                          +-------+ |
|                                          |       | |
|                                  dword N |- - - -| |
|                                          |       | |
|                                          +-------+/
|
| meaning of field1 and field2:
|
| field1            field2               type of object
|
| 0TTTTTTTXXXXXXXX  XXXXXXXXXXXXXXXX     TT..TT = type, GC information is
|                                           in a table indexed by TT..TT,
|                                           XX.XX = auxiliary information
| 1000000000000000                       forwarded hunk (dword 1 = forw addr.)
| 10DDDDDDDDDDDDDD  first word of proc.  sub-procedure (DD..DD distance in # of
|                                           quad-bytes from parent procedure)
| 110NNNNNNNNNNNNN  first word of proc.  prim proc. (dword M..dword N =
|                                           GC information)
| 111NNNNNNNNNNNNN  JSR opcode           closure (dword 1 = pointer to body,
|                                           dword 2 ..dword N = closed values)

|------------------------------------------------------------------------------
|
| Scheme constants:
|

Cst_nil		= 0xFFFFFFFF	| -1
Cst_false	= 0xFFFFFFFD	| -3
Cst_true	= 0xFFFFFFFB	| -5
Cst_undefined	= 0xFFFFFFF9	| -7
Forwarded_pair	= 0xFFFFFFF7	| -9, if car of pair is this value, pair has
				|   already been forwarded to new space


|------------------------------------------------------------------------------
|
| Constants:
|

Fix_type_set	= 0x01010101	| value ends with 000
FP_type_set	= 0xAAAAAAAA	| value ends with 1
Pair_type_set	= 0x10101010	| value ends with 100
Hunk_type_set	= 0x44444444	| value ends with 10
Forwarded_hunk	= 0x8000	| if field1 of hunk is this value, the hunk
				|   has already been copied to new space

Vector_type	= 0x40		| type field for vectors
Symbol_type	= 0x41		| type field for symbols
String_type	= 0x60		| type field for strings

Sub_proc_type	= 0x8000	| type field for sub-procedures
Primitive_type	= 0xC000	| type field for primitives
Closure_type	= 0xE000	| type field for closures


|------------------------------------------------------------------------------
|
| Macro definitions:
|

define(concatenate,`$1$2$3$4$5$6$7$8$9')
define(the_number,`0')
define(new_number,`define(`the_number',incr(the_number))the_number')
define(new_symbol,`define(`the_symbol',concatenate($1,new_number))the_symbol')

define(change_area,
`define(`current_area',$1)
	$1')

define(begin_main,
`	.lcomm	_reg_at_entry,4*16
	change_area(.text)
	.globl	_entry
_entry:
	moveml	d0-d7/a0-a7,_reg_at_entry
	movl	sp,d0
	andl	#0xfffffffc,d0
	movl	d0,sp')

define(end_main,
`	moveml	_reg_at_entry,d0-d7/a0-a7
	rts')

define(save_regs,
`	moveml	d0-d7/a0-a6,sp@-')

define(restore_regs,
`	moveml	sp@+,d0-d7/a0-a6')

define(print_str,
`	.data2
new_symbol(Str):	.asciz	$1
	current_area
save_regs
	pea	the_symbol
	jbsr	_printf
	addqw	#0x4,sp
restore_regs')

define(print_int,
`	.data2
new_symbol(Str):	.asciz	$1
	current_area
save_regs
	movl	$2,sp@-
	pea	the_symbol
	jbsr	_printf
	addqw	#0x8,sp
restore_regs')

define(print_chr,
`	.data2
new_symbol(Str):	.asciz	$1
	current_area
save_regs
	movb	$2,d0
	extbl	d0
	movl	d0,sp@-
	pea	the_symbol
	jbsr	_printf
	addqw	#0x8,sp
restore_regs')

define(define_primitive,
`define(`the_procedure_name',$1)define(`the_procedure',scm_$1)
	.align	4
	.word	(end_of_procedure_scm_$1-scm_$1)/4+Primitive_type
scm_$1:')

define(end_primitive,
`	.align	4
concatenate(start_of_constants_,the_procedure):
	.long	new_symbol(Sym)
	.long	(concatenate(end_of_procedure_,the_procedure)-concatenate(start_of_constants_,the_procedure))/4-1
concatenate(end_of_procedure_,the_procedure):
symbol_object(the_symbol,new_symbol(Str))
string_object(the_symbol,"the_procedure_name")')

define(procedure_object_begin,
`define(`the_procedure',$1)
	.align	4
	.word	(end_of_procedure_$1-$1)/4+Primitive_type
$1:')

define(procedure_object_constants,
`	.align	4
concatenate(start_of_constants_,the_procedure):')

define(procedure_object_end,
`	.long	(concatenate(end_of_procedure_,the_procedure)-concatenate(start_of_constants_,the_procedure))/4-1
concatenate(end_of_procedure_,the_procedure):')

define(sub_procedure,
`	.align	4
	.word	($1-the_procedure)/4+Sub_proc_type
$1:')

define(pair_object,
`	.data
	.long	$3
$1:	.long	$2
	current_area')

define(vector_object,
`	.align	4
	.word	Vector_type*256
$1:	.word	$2')

define(symbol_object,
`	.align	4
	.word	Symbol_type*256
$1:	.word	1
	.long	$2')

define(string_object,
`	.align	4
	.word	String_type*256
$1:	.word	end_of_$1-$1-2
	.ascii	`$2'
end_of_$1:')

| -----------------------------------------------------------------------------
|
| Interface for SUN:
|

	change_area(.text)

	.globl	_main
_main:
	link	a6,#0
	jbsr	_entry
	unlk	a6
	rts

begin_main

	lea	data_adr,a0	| make sure pairs are on octuple address

	.data
	.skip	4	| 'magical' offset to bring 'data' section to an
data_adr:		| octuple address (may be different on other systems)
	current_area

	movl	a0,d0
	andl	#7,d0
	beq	pairs_are_ok

print_str("Can not execute; pairs are not at an address multiple of 8\012")
print_str("Change magical offset in header file\012")

	bra	exit

pairs_are_ok:

	movl	#Hunk_type_set,d7	| mask to test tags of hunks
	movl	#Pair_type_set,d6	| mask to test tags of pairs
	movl	#Fix_type_set,d5	| mask to test tags of fixnums

	lea	globals,a6	| setup pointer to global area

	lea	heap_bottom,a5	| setup pointer to allocate hunks
	movl	a5,d0
	andl	#0xFFFFFFF8,d0	| make sure address ends with 000
	movl	d0,a5

	lea	heap_top,a4	| setup pointer to allocate pairs
	movl	a4,d0
	andl	#0xFFFFFFF8,d0	| make sure address ends with 000
	movl	d0,a4

	lea	handler,a3	| error handler

	.lcomm	stack_bottom,41000
	.lcomm	stack_top,0

	lea	stack_bottom,a0	| set stack limit
	movl	a0,a6@

	lea	stack_top,a7	| set stack pointer

	jmp	eval_print	| eval program and print result

exit:

end_main

|------------------------------------------------------------------------------
|
| low-level run-time routines:

rest_enter_2:
	subql	#6,a7@		| fix environment pointer for closures
rest_enter_1:
	cmpl	a6@,a7		| check for stack overflow
	bhis	rest_enter_ok
	bra	error_1
rest_enter_ok:

	movl	a6,a2		| transfer arguments from registers to
	movl	d1,a2@-		| argument vector
	movl	d2,a2@-
	movl	d3,a2@-
	movl	d4,a2@-

	tstw	d0		| get number of actual parameters in d0
	bgts	not_special_code
	addqw	#3,d0
not_special_code:

	movw	d0,d1		| save number of actual parameters

	subw	a0,d0		| are there enough actual parameters?
	bpls	enough_args

print_str("error: too few arguments to an n-ary procedure\012")
	jmp	dump

enough_args:

	subqw	#1,d1		| compute location of last argument in
	extl	d1		| argument vector
	asll	#2,d1
	movl	a6,a2
	subl	d1,a2

	moveq	#Cst_nil,d1	| start with the empty list
	subqw	#1,d0		| just enough actual parameters?
	bmis	push_rest_arg

construct_list_loop:

	movl	a2@+,a4@-
	movl	a4,a0
	movl	d1,a4@-
	movl	a0,d1

	dbra	d0,construct_list_loop

push_rest_arg:

	movl	d1,a7@-
	bras	rest_enter_push

rest_enter_loop:
	movl	a2@+,a7@-
rest_enter_push:
	cmpl	a2,a6
	bnes	rest_enter_loop

	jmp	a1@		| return to procedure

handler:
	movl	a7@+,a0		| get pointer to value of global variable
	movw	d0,a7@-
	movl	a0@,d0
	btst	d0,d7		| is it a procedure?
	beqs	error_3
	movl	d0,a1
	tstw	a1@(-2)
	bpls	error_3

	movw	#20217,a0@(-2)	| change trap to a long 'jmp'
	movw	a7@+,d0		| jump to it...
	jmp	a1@

error_1:
	cmpl	a6@,a7
	bhis	wrong_nb_args

stack_overflow:

print_str("error: stack overflow\012")
	jmp	dump

wrong_nb_args:

print_str("error: wrong number of arguments to a user procedure\012")
	jmp	dump

error_2:
	subql	#6,a7@
	cmpl	a6@,a7
	bhis	wrong_nb_args
	bras	stack_overflow

error_3:
print_str("error: application of a non-procedure object\012")
	jmp	dump

error_4:

print_str("error: wrong number of arguments to a primitive procedure\012")
	jmp	dump

| -----------------------------------------------------------------------------
|
| Heap:

	change_area(.bss)

	.skip	8

heap_bottom:

	.skip	8*400000	| reserve space for heap

heap_top:

| -----------------------------------------------------------------------------
|
| Globals:

	change_area(.data1)

	.word	20217
	.long	rest_enter_1	| entering a procedure with a 'rest' parameter
	.word	20217
	.long	rest_enter_2	| entering a closure with a 'rest' parameter
	.word	20217
	.long	error_1		| wrong number of arg or stack overflow
	.word	20217
	.long	error_2		| wrong number of arg or stack overflow (for a
				| closure)
	.word	20217
	.long	error_3		| calling a non-procedure

	.skip	126*4		| space for 126 arguments

globals:
	.long	0
	.word	20217
	.long	scm_trace
	.word	20217
	.long	scm_make_promise
	.word	20217
	.long	scm_memv
	.word	20217
	.long	scm_cons
	.word	20217
	.long	scm_list
	.word	20217
	.long	scm_append
	.word	20217
	.long	scm_list_vector
	.word	20217
	.long	scm_not
	.word	20217
	.long	scm_eq
	.word	20217
	.long	scm_pair
	.word	20217
	.long	scm_cons
	.word	20217
	.long	scm_append
	.word	20217
	.long	scm_length
	.word	20217
	.long	scm_car
	.word	20217
	.long	scm_cdr
	.word	20217
	.long	scm_set_car
	.word	20217
	.long	scm_set_cdr
	.word	20217
	.long	scm_null
	.word	20217
	.long	scm_num_eq
	.word	20217
	.long	scm_num_lt
	.word	20217
	.long	scm_num_gt
	.word	20217
	.long	scm_plus
	.word	20217
	.long	scm_minus
	.word	20217
	.long	scm_times
	.word	20217
	.long	scm_quotient
	.word	20217
	.long	scm_minus_1
	.word	20217
	.long	scm_force
	.word	20217
	.long	scm_write
	.word	20217
	.long	scm_newline
	.word	20217
	.long	scm_list
	.word	20217
	.long	scm_vect
	.word	20217
	.long	scm_list_vector
	.word	20217
	.long	scm_memv
	.word	20217
	.long	scm_assq
	.word	20217
	.long	scm_symbol
	.word	20217
	.long	scm_vector
	.word	20217
	.long	scm_string
	.word	20217
	.long	scm_procedure
	.word	20217
	.long	scm_number
	.word	20217
	.long	scm_char

	| space for 128 other global variables (initially undefined)

	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7
	.word	20115,-1,-7,20115,-1,-7,20115,-1,-7,20115,-1,-7

| -----------------------------------------------------------------------------
|
| Primitive procedures:

define_primitive(trace)
print_str("\012Trace: calling ")
	tstw	d0
	jmp	scm_write
end_primitive

define_primitive(append)
	bnes	scm_append2

	movl	d2,a1

	btst	d1,d6		| is arg1 a pair?
	beqs	scm_append1

	movl	d1,a0
	movl	a0@,a4@-
	movl	a4,a1
	movl	d2,a4@-

scm_append_loop:
	movl	a0@-,d1
	btst	d1,d6		| is the tail a pair?
	beqs	scm_append1

	movl	d1,a0
	movl	a0@,a4@-
	movl	a4,a4@(4)
	movl	d2,a4@-

	bras	scm_append_loop

scm_append1:
	cmpl	#Cst_nil,d1
	bnes	scm_append3

	movl	a1,d1
	rts
scm_append2:
	jmp	error_4		| for now signal an error (should be n-ary)
scm_append3:
print_str("error: improper list passed to 'append'\012")
	jmp	dump
end_primitive

define_primitive(length)
	bpls	scm_length2
	moveq	#0,d2
scm_length_loop:
	btst	d1,d6		| is arg1 a pair?
	beqs	scm_length1

	movl	d1,a0
	movl	a0@-,d1
	addql	#8,d2

	bras	scm_length_loop

scm_length1:
	cmpl	#Cst_nil,d1
	bnes	scm_length3

	movl	d2,d1
	rts
scm_length2:
	jmp	error_4
scm_length3:
print_str("error: improper list passed to 'length'\012")
	jmp	dump
end_primitive

define_primitive(not)
	bpls	scm_not2
	addql	#3,d1
	bcss	scm_not1
	moveq	#Cst_false,d1
	rts
scm_not1:
	moveq	#Cst_true,d1
	rts
scm_not2:
	jmp	error_4
end_primitive

define_primitive(list_vector)
	bpls	scm_list_vector3
	movl	d1,sp@-
	pea	scm_list_vector1
	moveq	#-1,d0
	jmp	scm_length
sub_procedure(scm_list_vector1)
	movl	d1,d2
	asrl	#3,d2
	movw	#Vector_type*256,a5@+
	movl	a5,d1
	movw	d2,a5@+
	movl	sp@+,d2
scm_list_vector_loop:
	btst	d2,d6
	beqs	scm_list_vector2

	movl	d2,a0
	movl	a0@,a5@+
	movl	a0@-,d2

	bras	scm_list_vector_loop

scm_list_vector2:
	rts
scm_list_vector3:
	jmp	error_4
end_primitive

define_primitive(eq)
	bnes	scm_eq2
	cmpl	d2,d1
	beqs	scm_eq1
	moveq	#Cst_false,d1
	rts
scm_eq1:
	moveq	#Cst_true,d1
	rts
scm_eq2:
	jmp	error_4
end_primitive

define_primitive(pair)
	bpls	scm_pair2
	btst	d1,d6
	bnes	scm_pair1
	moveq	#Cst_false,d1
	rts
scm_pair1:
	moveq	#Cst_true,d1
	rts
scm_pair2:
	jmp	error_4
end_primitive

define_primitive(cons)
	bnes	scm_cons1
	movl	d1,a4@-
	movl	a4,d1
	movl	d2,a4@-
	rts
scm_cons1:
	jmp	error_4
end_primitive

define_primitive(car)
	bpls	scm_car2
	btst	d1,d6
	beqs	scm_car1
	movl	d1,a0
	movl	a0@,d1
	rts
scm_car1:
print_str("error: non pair argument to 'car': ")
	moveq	#-1,d0
	jbsr	scm_write
print_str("\012")
	jmp	dump
scm_car2:
	jmp	error_4
end_primitive

define_primitive(cdr)
	bpls	scm_cdr2
	btst	d1,d6
	beqs	scm_cdr1
	movl	d1,a0
	movl	a0@-,d1
	rts
scm_cdr1:
print_str("error: non pair argument to 'cdr': ")
	moveq	#-1,d0
	jbsr	scm_write
print_str("\012")
	jmp	dump
scm_cdr2:
	jmp	error_4
end_primitive

define_primitive(set_car)
	bnes	scm_set_car2
	btst	d1,d6
	beqs	scm_set_car1
	movl	d1,a0
	movl	d2,a0@
	rts
scm_set_car1:
print_str("error: non pair argument to 'set-car': ")
	moveq	#-1,d0
	jbsr	scm_write
print_str("\012")
	jmp	dump
scm_set_car2:
	jmp	error_4
end_primitive

define_primitive(set_cdr)
	bnes	scm_set_cdr2
	btst	d1,d6
	beqs	scm_set_cdr1
	movl	d1,a0
	movl	d2,a0@-
	rts
scm_set_cdr1:
print_str("error: non pair argument to 'set-cdr': ")
	moveq	#-1,d0
	jbsr	scm_write
print_str("\012")
	jmp	dump
scm_set_cdr2:
	jmp	error_4
end_primitive

define_primitive(null)
	bpls	scm_null2
	addql	#1,d1
	beqs	scm_null1
	moveq	#Cst_false,d1
	rts
scm_null1:
	moveq	#Cst_true,d1
	rts
scm_null2:
	jmp	error_4
end_primitive

define_primitive(num_eq)
	bnes	scm_num_eq3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_num_eq2
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_num_eq2
	cmpl	d2,d1
	beqs	scm_num_eq1
	moveq	#Cst_false,d1
	rts
scm_num_eq1:
	moveq	#Cst_true,d1
	rts
scm_num_eq2:
print_str("error: non fixnum argument(s) to '='\012")
	jmp	dump
scm_num_eq3:
	jmp	error_4
end_primitive

define_primitive(num_lt)
	bnes	scm_num_lt3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_num_lt2
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_num_lt2
	cmpl	d2,d1
	blts	scm_num_lt1
	moveq	#Cst_false,d1
	rts
scm_num_lt1:
	moveq	#Cst_true,d1
	rts
scm_num_lt2:
print_str("error: non fixnum argument(s) to '<'\012")
	jmp	dump
scm_num_lt3:
	jmp	error_4
end_primitive

define_primitive(num_gt)
	bnes	scm_num_gt3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_num_gt2
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_num_gt2
	cmpl	d2,d1
	bgts	scm_num_gt1
	moveq	#Cst_false,d1
	rts
scm_num_gt1:
	moveq	#Cst_true,d1
	rts
scm_num_gt2:
print_str("error: non fixnum argument(s) to '>'\012")
	jmp	dump
scm_num_gt3:
	jmp	error_4
end_primitive

define_primitive(plus)
	bnes	scm_plus2
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_plus5
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_plus5
	addl	d2,d1
	bvss	scm_plus1
	rts
scm_plus1:
print_str("error: overflow on '+'\012")	| for now signal an error, eventually
	jmp	dump			| should convert result to bignum
scm_plus2:
	bpls	scm_plus3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_plus5
	rts
scm_plus3:
	subql	#1,d0
	bnes	scm_plus4
	moveq	#0,d0
	rts
scm_plus4:
	addql	#1,d0
	jmp	error_4		| for now signal an error (should be n-ary)
scm_plus5:
print_str("error: non fixnum argument(s) to '+'\012")
	jmp	dump
end_primitive

define_primitive(minus)
	bnes	scm_minus2
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_minus4
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_minus4
	subl	d2,d1
	bvss	scm_minus1
	rts
scm_minus1:
print_str("error: overflow on '-'\012")	| for now signal an error, eventually
	jmp	dump			| should convert result to bignum
scm_minus2:
	bpls	scm_minus3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_minus4
	negl	d1
	bvss	scm_minus1
	rts
scm_minus3:
	jmp	error_4		| for now signal an error (should be n-ary)
scm_minus4:
print_str("error: non fixnum argument(s) to '-'\012")
	jmp	dump
end_primitive

define_primitive(times)
	bnes	scm_times2
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_times5
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_times5
	asrl	#3,d1
	muls	d2,d1
	bvss	scm_times1
	rts
scm_times1:
print_str("error: overflow on '*'\012")	| for now signal an error, eventually
	jmp	dump			| should convert result to bignum
scm_times2:
	bpls	scm_times3
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_times5
	rts
scm_times3:
	subql	#1,d0
	bnes	scm_times4
	moveq	#8,d0
	rts
scm_times4:
	addql	#1,d0
	jmp	error_4		| for now signal an error (should be n-ary)
scm_times5:
print_str("error: non fixnum argument(s) to '*'\012")
	jmp	dump
end_primitive

define_primitive(quotient)
	bnes	scm_quotient1
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_quotient2
	btst	d2,d5		| is arg2 a fixnum?
	beqs	scm_quotient2
	divs	d2,d1
	extl	d1
	asll	#3,d1
	rts
scm_quotient1:
	jmp	error_4		| for now signal an error (should be n-ary)
scm_quotient2:
print_str("error: non fixnum argument(s) to '/'\012")
	jmp	dump
end_primitive

define_primitive(minus_1)
	bpls	scm_minus_11
	btst	d1,d5		| is arg1 a fixnum?
	beqs	scm_minus_12
	subql	#8,d1
	rts
scm_minus_11:
	jmp	error_4
scm_minus_12:
print_str("error: non fixnum argument to '-1+'\012")
	jmp	dump
end_primitive

define_primitive(force)
	bpls	scm_force2
	btst	d1,d7
	beqs	scm_force1
	movl	d1,a0
	tstw	a0@(-2)
	bpls	scm_force1
	moveq	#1,d0
	jmp	a0@
scm_force1:
	jmp	a6@(-510)
scm_force2:
	jmp	error_4
end_primitive

define_primitive(write)
	bpls	scm_write1
	btst	d1,d5	| is it a fixnum?
	beqs	not_fixnum

	movl	d1,d2
	asrl	#3,d2
print_int("%d",d2)
	moveq	#Cst_undefined,d1
	rts

scm_write1:
	jmp	error_4

not_fixnum:
	btst	#0,d1
	beqs	not_char2
	swap	d1
	cmpw	#-2,d1
	bnes	not_char1

	swap	d1
	asrw	#1,d1

print_chr("#\134%c",d1)
	moveq	#Cst_undefined,d1
	rts

not_char1:
	swap	d1
not_char2:
	btst	d1,d6	| is it a pair?
	beq	not_pair

print_str("\050")

next_pair:
	movl	d1,a0
	movl	a0@,d1
	movl	a0@-,a7@-
	moveq	#-1,d0
	jbsr	scm_write

	movl	a7@+,d1
	cmpl	#Cst_nil,d1
	bnes	not_end

print_str("\051")
	moveq	#Cst_undefined,d1
	rts

not_end:
	btst	d1,d6	| is it a pair?
	bnes	pair_again

print_str(" . ")

	moveq	#-1,d0
	jbsr	scm_write

print_str("\051")
	moveq	#Cst_undefined,d1
	rts

pair_again:
print_str(" ")
	bra	next_pair

not_pair:
	cmpl	#Cst_nil,d1
	bnes	not_nil

print_str("\050\051")
	moveq	#Cst_undefined,d1
	rts

not_nil:
	cmpl	#Cst_false,d1
	bnes	not_false

print_str("#f")
	moveq	#Cst_undefined,d1
	rts

not_false:
	cmpl	#Cst_true,d1
	bnes	not_true

print_str("#t")
	moveq	#Cst_undefined,d1
	rts

not_true:
	cmpl	#Cst_undefined,d1
	bnes	not_undef

print_str("#<undefined>")
	moveq	#Cst_undefined,d1
	rts

not_undef:
	btst	d1,d7
	beq	unknown

	movl	d1,a0
	movw	a0@(-2),d0

	bpl	other_hunks

	cmpw	#Closure_type,d0
	bhi	write_closure

	cmpw	#Primitive_type,d0
	bhi	write_primitive

print_str("#<return address into ")

	subw	#Sub_proc_type,d0
	extl	d0
	asll	#2,d0

	subl	d0,a0
	movl	a0,d1
	moveq	#-1,d0
	jbsr	scm_write

print_str(">")

	moveq	#Cst_undefined,d1
	rts

write_primitive:

print_str("#<procedure")

	subw	#Primitive_type,d0
	extl	d0
	asll	#2,d0

	addl	d0,a0
	subql	#6,a0
	movl	a0@,d1
	cmpl	#Cst_nil,d1
	beqs	write_primitive1

print_str(" ")

	moveq	#-1,d0
	jbsr	scm_write

write_primitive1:

print_str(">")

	moveq	#Cst_undefined,d1
	rts

write_closure:

	addql	#2,a0
	movl	a0@,d1
	moveq	#-1,d0
	jbsr	scm_write

	moveq	#Cst_undefined,d1
	rts

other_hunks:
	asrw	#8,d0

	cmpb	#Vector_type,d0
	bne	other_1

print_str("#\050")

	subql	#2,a0
	movl	a0@+,d0
	andl	#0xFFFFFF,d0
	beqs	vector_end

write_vector_loop:

	movl	a0@+,d1
	movl	d0,a7@-
	movl	a0,a7@-
	moveq	#-1,d0
	jbsr	scm_write
	movl	a7@+,a0
	movl	a7@+,d0

	subql	#1,d0
	beqs	vector_end

print_str(" ")
	bra	write_vector_loop

vector_end:
print_str("\051")
	moveq	#Cst_undefined,d1
	rts

other_1:
	cmpb	#String_type,d0
	bne	other_2

print_str("\042")

	subql	#2,a0
	movl	a0@+,d0
	andl	#0xFFFFFF,d0
	beqs	string_end

write_string_loop:

	movb	a0@+,d1

	cmpb	#34,d1
	beqs	write_escape
	cmpb	#92,d1
	bnes	write_string_char

write_escape:
print_chr("%c",#92)

write_string_char:
print_chr("%c",d1)

	subql	#1,d0
	bnes	write_string_loop

string_end:
print_str("\042")
	moveq	#Cst_undefined,d1
	rts

other_2:
	cmpb	#Symbol_type,d0
	bne	other_3

	movl	a0@(2),a0

	subql	#2,a0
	movl	a0@+,d0
	andl	#0xFFFFFF,d0
	beqs	symbol_end

write_symbol_loop:

	movb	a0@+,d1

print_chr("%c",d1)

	subql	#1,d0
	bnes	write_symbol_loop

symbol_end:
	moveq	#Cst_undefined,d1
	rts

other_3:

unknown:
print_str("#<unknown>")
	moveq	#Cst_undefined,d1
	rts
end_primitive

define_primitive(newline)
	subqw	#1,d0
	bne	scm_newline1
print_str("\012")
	moveq	#Cst_undefined,d1
	rts
scm_newline1:
	addqw	#1,d0
	jmp	error_4
end_primitive

define_primitive(assq)
	bnes	scm_assq4
scm_assq1:
	btst	d2,d6
	beqs	scm_assq2
	movl	d2,a0
	movl	a0@,d3
	movl	a0@-,d2
	btst	d3,d6
	beqs	scm_assq3
	movl	d3,a1
	cmpl	a1@,d1		| use eq? for now (should use eqv?)
	bnes	scm_assq1
	movl	d3,d1
	rts	
scm_assq2:
	moveq	#Cst_false,d1
	rts	
scm_assq3:
print_str("error: illformed a-list to 'assq'\012")
	jmp	dump
scm_assq4:
	jmp	error_4
end_primitive

define_primitive(symbol)
	bpls	scm_symbol2
	btst	d1,d7
	beqs	scm_symbol1
	movl	d1,a0
	cmpb	#Symbol_type,a0@(-2)
	bnes	scm_symbol1
	moveq	#Cst_true,d1
	rts
scm_symbol1:
	moveq	#Cst_false,d1
	rts
scm_symbol2:
	jmp	error_4
end_primitive

define_primitive(vector)
	bpls	scm_vector2
	btst	d1,d7
	beqs	scm_vector1
	movl	d1,a0
	cmpb	#Vector_type,a0@(-2)
	bnes	scm_vector1
	moveq	#Cst_true,d1
	rts
scm_vector1:
	moveq	#Cst_false,d1
	rts
scm_vector2:
	jmp	error_4
end_primitive

define_primitive(string)
	bpls	scm_string2
	btst	d1,d7
	beqs	scm_string1
	movl	d1,a0
	cmpb	#String_type,a0@(-2)
	bnes	scm_string1
	moveq	#Cst_true,d1
	rts
scm_string1:
	moveq	#Cst_false,d1
	rts
scm_string2:
	jmp	error_4
end_primitive

define_primitive(procedure)
	bpls	scm_procedure2
	btst	d1,d7
	beqs	scm_procedure1
	movl	d1,a0
	tstw	a0@(-2)
	bpls	scm_procedure1
	moveq	#Cst_true,d1
	rts
scm_procedure1:
	moveq	#Cst_false,d1
	rts
scm_procedure2:
	jmp	error_4
end_primitive

define_primitive(number)
	bpls	scm_number2
	btst	d1,d5
	beqs	scm_number1
	moveq	#Cst_true,d1
	rts
scm_number1:
	moveq	#Cst_false,d1
	rts
scm_number2:
	jmp	error_4
end_primitive

define_primitive(char)
	bpls	scm_char2
	btst	#0,d1
	beqs	scm_char1
	swap	d1
	cmpw	#-2,d1
	bnes	scm_char1
	moveq	#Cst_true,d1
	rts
scm_char1:
	moveq	#Cst_false,d1
	rts
scm_char2:
	jmp	error_4
end_primitive

| -----------------------------------------------------------------------------
|
| Run-time procedures (written in Scheme):


procedure_object_begin(eval_print)

	pea	eval_print1
	moveq	#1,d0
	jmp	entry		| enter program
sub_procedure(eval_print1)

print_str("Result: ")

	pea	eval_print2
	moveq	#-1,d0
	jmp	scm_write
sub_procedure(eval_print2)

print_str("\012")

	jmp	exit
procedure_object_constants
	.long	Cst_nil
procedure_object_end

procedure_object_begin(dump)

print_str("\012Stack dump:\012")

dump1:
	cmpl	#stack_top,a7
	bccs	dump2

print_str("> ")
	movl	sp@+,d1
	moveq	#-1,d0
	jbsr	scm_write

print_str("\012")

	bras	dump1

dump2:
	jmp	exit
procedure_object_constants
procedure_object_end

define_primitive(make_promise)

| (define (make-promise thunk)
|   (let ((computed #f))
|     (lambda ()
|       (if computed thunk
|         (begin
|           (set! thunk (thunk))
|           (set! computed #t)
|           thunk)))))

	bpls	scm_make_promise2
	cmpl	a6@,a7
	bhis	scm_make_promise3
scm_make_promise2:
	jmp	a6@(-522)
scm_make_promise3:
	movl	d1,a7@-
	movl	a7@,d0
	movl	d0,a4@-
	movl	a4,a7@-
	movl	d0,a4@-
	moveq	#Cst_false,d0
	movl	d0,a7@-
	movl	a7@,d0
	movl	d0,a4@-
	movl	a4,a7@-
	movl	d0,a4@-
	movw	#-8189,a5@+
	movl	a5,d1
	movw	#20153,a5@+
	lea	scm_make_promise4,a0
	movl	a0,a5@+
	movl	a7@(8),a5@+
	movl	a7@,a5@+
	addw	#16,a7
	rts	
sub_procedure(scm_make_promise4)
	subql	#1,d0
	bnes	scm_make_promise5
	cmpl	a6@,a7
	bhis	scm_make_promise6
scm_make_promise5:
	jmp	a6@(-516)
scm_make_promise6:
	subql	#6,a7@
	movl	a7@,a0
	movl	a0@(10),a0
	movl	a0@,d1
	addql	#3,d1
	bcs	scm_make_promise7
	movl	a7@,a0
	movl	a0@(6),a0
	movl	a0@,d1
	addql	#4,a7
	rts	
scm_make_promise7:
	movl	a7@,a0
	movl	a0@(6),a0
	movl	a0@,d0
	pea	scm_make_promise9
	btst	d0,d7
	beqs	scm_make_promise8
	movl	d0,a0
	tstw	a0@(-2)
	bpls	scm_make_promise8
	moveq	#1,d0
	jmp	a0@
scm_make_promise8:
	jmp	a6@(-510)
sub_procedure(scm_make_promise9)
	movl	a7@,a0
	movl	a0@(6),a0
	movl	d1,a0@
	moveq	#Cst_true,d1
	movl	a7@,a0
	movl	a0@(10),a0
	movl	d1,a0@
	movl	a7@,a0
	movl	a0@(6),a0
	movl	a0@,d1
	addql	#4,a7
	rts	
end_primitive

define_primitive(memv)

| (define (memv obj list)
|  (if (pair? list)
|    (if (eqv? (car list) obj)
|      list
|      (memv obj (cdr list)))
|    #f))

	bnes	scm_memv1
	cmpl	a6@,a7
	bhis	scm_memv3
scm_memv1:
	jmp	a6@(-522)
scm_memv2:
	movl	a0@-,d2
scm_memv3:
	btst	d2,d6
	beqs	scm_memv4
	movl	d2,a0
	cmpl	a0@,d1		| use eq? for now (should use eqv?)
	bnes	scm_memv2
	movl	d2,d1
	rts	
scm_memv4:
	moveq	#Cst_false,d1
	rts	
end_primitive

define_primitive(list)

| (define (list . l) l)

	movw	#1,a0
	lea	scm_list1,a1
	jmp	a6@(-534)
sub_procedure(scm_list1)
	movl	a7@,d1
	addql	#4,a7
	rts	
end_primitive

define_primitive(vect)

| (define (vector . l) (list->vector l))

	movw	#1,a0
	lea	scm_vect1,a1
	jmp	a6@(-534)
sub_procedure(scm_vect1)
	movl	a7@+,d1
	moveq	#-1,d0
	jmp	scm_list_vector
end_primitive

| -----------------------------------------------------------------------------
|
| The program:

===========================================


cat header.s $1.s > asm.tmp1
sed -e s/#/@sharp@/ asm.tmp1 | m4 | sed -e s/@sharp@/#/ > asm.tmp2
as -o asm.o asm.tmp2
cc asm.o -o $1
rm asm.o
