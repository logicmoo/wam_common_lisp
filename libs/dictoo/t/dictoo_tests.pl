:- module(example, [test/1]).

:- if(exists_source(library(must_trace))).
:- use_module(library(must_trace)).
:- endif.

:- discontiguous example:test/1.

:- use_module(library(dictoo)).
:- use_module(library(dictoo_declarations)).

:- if(exists_source(library(jpl))).
:- use_module(library(jpl)).
test(0.0):- jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', $cursor.value ).

test(0.01):- writeln($cursor.value ).

test(0.1):- $cursor.value == 7.

test(0.11):- ($cursor.value = 5).

test(0.2):- $cursor.value == 5.

test(0.201):- $cursor.value = 7.

test(0.21):- $my_array1.clear(), 
  jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array1.value),
  writeln($my_array1.value).

test(0.22):- $my_array2.clear(), 
  (jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array2.set())),
  writeln($my_array2.value).

test(0.23):- $my_array.clear(), 
  (jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array.set())),
  writeln($my_array.value).

test(0.24):- writeln($my_array.value).

test(0.3):- writeln($my_array.value.3 = then).

test(0.4):- writeln(3-5 = $my_array.value.(3-5)).

test(0.5):- writeln(length = $my_array.value.length).
:- endif.

:- user: $foo.set(v3{ x: -1.0 , y:0.0, z:1.0}).

:- use_module(library(dictoo_declarations)).


:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).



/*
:- set_prolog_flag(access_level,system).
:- use_module(library(yall)).
:- user:use_module(library(gvar_syntax)).
:- user:use_module(library(qvars)).
:- user:use_module(library(dictoo)).
:- use_module(library(hook_database)).
:- use_module(library(must_trace)).
*/



$current_file.value = X :- prolog_load_context(file,X).

modfoo1: $ foo1.value = X :- bar1 = X.

% :- rtrace,trace.
$modfoo2:foo2.value = X :- bar2 = X.
:- notrace.

$flags.Name = X :- current_prolog_flag(Name,X).

:- writeln($current_file.value).

:- listing(dot_cfg:dictoo_decl/8).

:-  nb_setval('$mud_prefs',_{realname:dug}).

% :- meta_predicate(ain(0)).

:- dynamic(call_after_logon/1).
:- meta_predicate(call_after_logon(0)).


get_xy($a,$b):- $a.set(ab),$b.set()=ab.

test(0):- $test.set("this is a test").

test(1):- writeln($test.current()).
test(2):- dict_create(Dict,foo,[key_test-value_success]) , X = Dict.key_test, writeln(X).
% :- debug(gvar(syntax)).
test(3):- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).
test(4):- get_xy(X,Y), X.unify()=Y.unify().
:- nodebug(gvar(syntax)).
test(5):- \+ source_location(_,_)-> true; ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).
test(6):- X.set(6), X = $six, writeln($six.value()).



all_tests:- debug(dictoo(_)),forall(clause(test(X),Body),(dmsg(test(X)),must(Body))).

:- nodebug(dictoo(_)).


:- user:use_module(library(dictoo)).
%:- debug(dictoo(goal_expand)).
%:- debug(dictoo(decl)).

foo :- ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).


:- freeze(Prefs,throw(nonvar(Prefs))), % rtrace,trace,
   expand_goal((asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname)))))),OO),
  nl,writeq(:- OO),nl.

% :- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).

:- listing(foo).
:- listing(test(_)).


:- fixup_exports.

:- 
  set_prolog_flag(toplevel_goal_expansion,true).
% :- set_prolog_flag(toplevel_goal_expansion,false).

end_of_file.



















:- expand_goal(ain(system_autoexec:(==>(mpred_unload_option(never, $current_file.value)))),OO).
:- expand_goal(mpred_unload_option(never, $current_file.value),OO).

:- expand_term(foo :- ain(system_autoexec:(==> (==>mpred_unload_option(never, $current_file.value)))),OO).

% :- debug(dictoo(decl)).

