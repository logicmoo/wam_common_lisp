#!/usr/bin/env swipl
:- use_module(library(logicmoo_test)).
%:- include(library(logicmoo_test_header)).
:- begin_tests(plunit_01_te).
test(te_plunit_01):- true.
:- end_tests(plunit_01_te).
:- at_halt(run_junit_tests).
%:- run_tests_and_halt.


% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master//var/lib/jenkins/workspace/logicmoo_workspace/packs_sys/logicmoo_utils/t/te_plunit_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.utils.utils.t/TE_PLUNIT_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ATE_PLUNIT_01 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/ 

