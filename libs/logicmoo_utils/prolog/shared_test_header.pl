% This file is mostly all inside if/endifs so it doesnt interfere with `module/2`
:- if((prolog_load_context(source,File),prolog_load_context(file,File));current_prolog_flag(xref,true)).
:- else.
:- if((
   keep_going,
   redefine_system_predicate(system:break/0),
   abolish(system:break,0),
   assert(system:break :- (dumpST, sleep(1))))).
:- endif.
:- endif.
