/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(hashts, []).

:- set_module(class(library)).

:- include('header').

f_hash_table_p(HT,RetVal):- t_or_nil((f_class_of(HT,Claz),Claz==claz_hash_table),RetVal).

f_hash_table_test(HT,RetVal):-get_opv(HT,test,RetVal).

(wl:init_args(0,make_hash_table)).
f_make_hash_table(Keys,HT):- 
  create_object(claz_hash_table,Keys,HT),
  ((get_opv(HT,u_data,Tree),Tree\==[])->true;(rb_new(Tree),set_opv(HT,u_data,'$OBJ'(clz_rb_tree,Tree)))).

wl:setf_inverse(gethash,sys_puthash).
get_table(HT,Tree,Data):- get_opv(HT,u_data,Data),arg(2,Data,Tree).

ht_match_key_value(Tree,Test,Key,Name,Value):-
  rb_in(Name,Value,Tree),
  ht_match(Test,Key,Name).

(wl:init_args(x,gethash)).
f_gethash(Key,HT,RetVal):- f_gethash(Key,HT,[],RetVal).
f_gethash(Key,HT,Default,RetVal):- 
  get_table(HT,Tree,_),ht_test_fn(HT,TestFn),
  (ht_match_key_value(Tree,TestFn,Key,_Name,Value)->f_values_list([Value,t],RetVal);f_values_list([Default,[]],RetVal)).

ht_test_fn(HT,TestFn):- get_opv(HT,test,Test),!,as_lisp_binary_fn(Test,TestFn).

as_lisp_binary_fn(Test,TestFn):- as_funcallable(Test,Test,TestFn).

f_sys_puthash(Key,HT,RetVal,RetVal):-
   get_table(HT,Tree,_),ht_test_fn(HT,TestFn),!,
   (ht_match_key_value(Tree,TestFn,Key,Name,_OldValue)-> 
      (nb_rb_get_node(Tree,Name,Node),nb_rb_set_node_value(Node,RetVal));
      nb_rb_insert(Tree,Key,RetVal)).
   
f_remhash(Key,HT,RetVal):-  
  get_table(HT,Tree,BV),ht_test_fn(HT,TestFn),!,
  t_or_nil(( ht_match_key_value(Tree,TestFn,Key,Name,_OldValue),rb_delete(Tree,Name,NT),nb_setarg(2,BV,NT)),RetVal).

f_clrhash(HT,RetVal):-  RetVal=t,
  rb_new(Tree),set_opv(HT,u_data,'$OBJ'(clz_rb_tree,Tree)).

f_maphash(Fn,HT,RetVal):-  RetVal=[],
  get_table(HT,Tree,_),as_lisp_binary_fn(Fn,MapFn),
  forall(rb_in(Name,Value,Tree),ignore(ht_match(MapFn,Name,Value))).


f_sys_maphash_iter(Function, Hash_table, FnResult) :-
        global_env(ReplEnv5),
        _Env10=[bv(function, Function), bv(hash_table, Hash_table)|ReplEnv5],
        f_with_hash_table_iterator([u_next_entry, hash_table],

                                    [ loop,

                                      [ multiple_value_bind,
                                        [u_more, key, u_value],
                                        [u_next_entry],
                                        [unless, u_more, [return, []]],
                                        [funcall, function, key, u_value]
                                      ]
                                    ],
                                    Table_iterator_Ret),
        Table_iterator_Ret=FnResult.


/*
(defmacro with-hash-table-iterator ((macroname hashtable) &body body)
  (unless (symbolp macroname)
    (error (TEXT "~S: macro name should be a symbol, not ~S")
           'with-hash-table-iterator macroname))
  (let ((var (gensym "WHTI-")))
    `(LET ((,var (SYS::HASH-TABLE-ITERATOR ,hashtable)))
       (MACROLET ((,macroname () '(SYS::HASH-TABLE-ITERATE ,var) ))
         ,@body))))

### Compiled:  `CL:WITH-HASH-TABLE-ITERATOR`
*/
sf_with_hash_table_iterator([Name, Hash_table], RestNKeys, FnResult) :-
        global_env(ReplEnv),
        CDR20=[bv(u_body, Body), bv(sys_name, Name), bv(hash_table, Hash_table)|ReplEnv],
        as_body(u_body, Body, RestNKeys),
        f_gensym(Iter_Init),
        LEnv=[bv(u_iter, Iter_Init)|CDR20],
        get_var(LEnv, hash_table, Hash_table_Get),
        get_var(LEnv, sys_name, Name_Get),
        get_var(LEnv, u_body, Body_Get),
        get_var(LEnv, u_iter, Iter_Get13),
        MFResult = [let, [[Iter_Get13, [u_hash_table_iterator_function, Hash_table_Get]]], 
         [macrolet, [[Name_Get, [], [quote, [funcall, Iter_Get13]]]]
               |Body_Get]],
        f_eval(MFResult, FnResult).


f_hash_table_count(HT,RetVal):-get_table(HT,Tree,_),rb_size(Tree,RetVal).

ht_match(Test,Key,Matcher):- (call(Test,Key,Matcher,R)->R=t).

/*
(defun hash-table-iterator-function (hash-table)
  (let ((entries (hash-table-entries hash-table)))
    #'(lambda () (let ((entry (car entries)))
                   (setq entries (cdr entries))
                   (if entry
                       (values t (car entry) (cdr entry))
                       nil)))))

*/
f_sys_hash_table_iterator_function(Hash_table, FnResult) :-
        nop(global_env(ReplEnv)),
        Env26=[bv(hash_table, Hash_table)|ReplEnv],
        get_var(Env26, hash_table, Hash_table_Get),
        f_sys_hash_table_entries(Hash_table_Get, Entries_Init),
        _LEnv=[bv(sys_entries, Entries_Init)|Env26],
        FnResult =
         closure(kw_function,LEnv22, LetResult10, [],  
          (get_var(LEnv22, sys_entries, Entries_Get), 
           f_car(Entries_Get, Entry_Init), LEnv11=[bv(sys_entry, Entry_Init)|LEnv22], 
           get_var(LEnv11, sys_entries, Entries_Get15), 
           f_cdr(Entries_Get15, Entries), set_var(LEnv11, sys_entries, Entries), 
           get_var(LEnv11, sys_entry, IFTEST), 
           (IFTEST\==[]-> 
             get_var(LEnv11, sys_entry, Entry_Get19), f_car(Entry_Get19, Car_Ret), 
             get_var(LEnv11, sys_entry, Entry_Get20), f_cdr(Entry_Get20, Cdr_Ret), 
             nb_setval('$mv_return', [t, Car_Ret, Cdr_Ret]), LetResult10=t;
                LetResult10=[]))).

:- fixup_exports.

end_of_file.

