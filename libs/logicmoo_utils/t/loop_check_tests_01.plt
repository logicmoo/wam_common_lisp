
%:- include(sanity_tests).

:- include(library(logicmoo_test_header)).


:- begin_tests(loop_check_tests).

:- use_module(library(loop_check)).

loop_inf0 :- loop_check(loop_inf0).

loop_inf1 :- loop_check(loop_inf2,true).
loop_inf1 :- loop_inf0.

loop_inf2 :- loop_inf1.

loop_inf3 :- loop_inf1.
loop_inf3.


loop_inf4(X) :- loop_check(loop_inf4(X),X=1).
loop_inf4(2).


test(loop_inf0):- must(\+ loop_inf0).

test(loop_inf1):- must( loop_inf1),!.


test(loop_inf2):- must( loop_inf2),!.

test(loop_inf3):- must(   loop_inf3).

test(loop_inf4):- must((   loop_inf4(X),!,X=1)).

test(loop_inf4a):- must((   loop_inf4(X),X=2,!)).

%:- break.

:- end_tests(loop_check_tests).

% :- run_tests.



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master//var/lib/jenkins/workspace/logicmoo_workspace/packs_sys/logicmoo_utils/t/loop_check_tests_01.plt 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.utils.utils.t/LOOP_CHECK_TESTS_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ALOOP_CHECK_TESTS_01 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/ 

