
:- include(sanity_tests).


:- dynamic(local_pred_0/0).
test(local_pred_0):- locally(local_pred_0,local_pred_0), \+ local_pred_0.

test(local_gvar):- locally(b_setval(local_gvar,set),nb_current(local_gvar,set)), \+ nb_current(local_gvar,set).


test(local_pred_0):- locally(local_pred_0,local_pred_0), \+ local_pred_0.

