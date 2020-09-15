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
:- module(hash7s, []).

:- set_module(class(library)).

:- include('./header').

f_hash_table_p(HT,RetVal):- t_or_nil((f_class_of(HT,Claz),Claz==claz_hash_table),RetVal).

f_hash_table_test(HT,RetVal):-get_opv(HT,hash_table_test,RetVal).

(wl:init_args(0,make_hash_table)).
f_make_hash_table(Keys,HT):- 
  create_object(claz_hash_table,Keys,HT),
  ((get_opv(HT,sys_hash_table_data,Tree),Tree\==[])->true;(rb_new(Tree),set_opv(HT,sys_hash_table_data,'$OBJ'(clz_rb_tree,Tree)))).

wl:setf_inverse(gethash,sys_puthash).
get_table(HT,Tree,Data):- get_opv(HT,sys_hash_table_data,Data),arg(2,Data,Tree).

ht_match_key_value(Tree,Test,Key,Name,Value):-
  rb_in(Name,Value,Tree),
  ht_match(Test,Key,Name).

(wl:init_args(x,gethash)).
f_gethash(Key,HT,RetVal):- f_gethash(Key,HT,[],RetVal).
f_gethash(Key,HT,Default,RetVal):- 
  get_table(HT,Tree,_),ht_test_fn(HT,TestFn),
  (ht_match_key_value(Tree,TestFn,Key,_Name,Value)->f_values_list([Value,t],RetVal);f_values_list([Default,[]],RetVal)).

ht_test_fn(HT,TestFn):- get_opv(HT,hash_table_test,Test),!,as_lisp_binary_fn(Test,TestFn).

as_lisp_binary_fn(Test,TestFn):- as_funcallable(Test,Test,TestFn).

f_sys_puthash(Key,HT,RetVal,RetVal):-
  set_prolog_flag(wamcl_gvars,true),
   get_table(HT,Tree,_),ht_test_fn(HT,TestFn),!,
   (ht_match_key_value(Tree,TestFn,Key,Name,_OldValue)-> 
      (nb_rb_get_node(Tree,Name,Node),nb_rb_set_node_value(Node,RetVal));
      nb_rb_insert(Tree,Key,RetVal)).
   
f_remhash(Key,HT,RetVal):-  
  get_table(HT,Tree,BV),ht_test_fn(HT,TestFn),!,
  t_or_nil(( ht_match_key_value(Tree,TestFn,Key,Name,_OldValue),rb_delete(Tree,Name,NT),nb_setarg(2,BV,NT)),RetVal).

f_clrhash(HT,RetVal):-  RetVal=t,
  set_prolog_flag(wamcl_gvars,true),
  rb_new(Tree),set_opv(HT,sys_hash_table_data,'$OBJ'(claz_sys_rb_tree,Tree)).

f_maphash(Fn,HT,RetVal):-  RetVal=[],
  get_table(HT,Tree,_),as_lisp_binary_fn(Fn,MapFn),
  forall(rb_in(Name,Value,Tree),ignore(ht_match(MapFn,Name,Value))).


f_sys_maphash_iter(Function, Hash_table, FnResult) :-
        global_env(ReplEnv5),
        Env10=[bv(function, Function), bv(hash_table, Hash_table)|ReplEnv5],
        sf_with_hash_table_iterator(Env10,[u_next_entry, hash_table],

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
sf_with_hash_table_iterator(SubEnv, [Macroname_In, Hashtable_In|CDR], RestNKeys, FResult) :-
        mf_with_hash_table_iterator(
                                    [ with_hash_table_iterator,
                                      [Macroname_In, Hashtable_In|CDR]
                                    | RestNKeys
                                    ],
                                    SubEnv,
                                    MFResult),
        f_sys_env_eval(SubEnv, MFResult, FResult).

mf_with_hash_table_iterator([with_hash_table_iterator, [Macroname_In, Hashtable_In|_CDR]|RestNKeys], _SubEnv, MFResult) :-
        nop(defmacro),
        GEnv=[bv(u_body, Body_In), bv(u_macroname, Macroname_In), bv(u_hashtable, Hashtable_In)],
        as_body(u_body, Body_In, RestNKeys),
        catch(( ( get_var(GEnv, u_macroname, Macroname_Get),
                  (   is_symbolp(Macroname_Get)
                  ->  _2126=[]
                  ;   load_and_call(f_sys_text('$ARRAY'([*],
                                        claz_base_character,
                                        "~S: macro name should be a symbol, not ~S"),
                               Text_Ret)),
                      get_var(GEnv, u_macroname, Macroname_Get13),
                      f_error(
                              [ Text_Ret,
                                with_hash_table_iterator,
                                Macroname_Get13
                              ],
                              ElseResult),
                      _2126=ElseResult
                  ),
                  f_gensym('$ARRAY'([*], claz_base_character, "WHTI-"), Var_Init),
                  LEnv=[bv(u_var, Var_Init)|GEnv],
                  get_var(LEnv, u_hashtable, Hashtable_Get),
                  get_var(LEnv, u_macroname, Macroname_Get21),
                  ( get_var(LEnv, u_body, Body_Get),
                    get_var(LEnv, u_var, Var_Get)
                  ),
                  get_var(LEnv, u_var, Var_Get22)
                ),
                [let, [[Var_Get, [sys_hash_table_iterator, Hashtable_Get]]], [macrolet, [[Macroname_Get21, [], [quote, [sys_hash_table_iterate, Var_Get22]]]]|Body_Get]]=MFResult
              ),
              block_exit(with_hash_table_iterator, MFResult),
              true).


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
f_sys_hash_table_iterator_function(Hash_table_In, FnResult) :-
        GEnv=[bv(hash_table, Hash_table_In)],
        catch(( ( get_var(GEnv, hash_table, Hash_table_Get),
                 load_and_call( f_u_hash_table_entries(Hash_table_Get, Entries_Init)),
                  LEnv=[bv(u_entries, Entries_Init)|GEnv]
                ),
                closure(kw_function, [ClosureEnvironment|LEnv], _Whole, LetResult12, [],  
                  (get_var(ClosureEnvironment, u_entries, Entries_Get), 
                   f_car(Entries_Get, Entry_Init), 
                   LEnv13=[bv(u_entry, Entry_Init)|ClosureEnvironment],
                   get_var(LEnv13, u_entries, Entries_Get17), 
                   f_cdr(Entries_Get17, Entries), 
                   set_var(LEnv13, u_entries, Entries), get_var(LEnv13, u_entry, IFTEST), 
                     (IFTEST\==[]->get_var(LEnv13, u_entry, Entry_Get21), 
                       f_car(Entry_Get21, Car_Ret), get_var(LEnv13, u_entry, Entry_Get22), 
                       f_cdr(Entry_Get22, Cdr_Ret), 
                       nb_setval('$mv_return', [t, Car_Ret, Cdr_Ret]), 
                        LetResult12=t;LetResult12=[])), 
                   [lambda, [], [let, [[u_entry, [car, u_entries]]], [setq, u_entries, [cdr, u_entries]], 
                    [if, u_entry, [values, t, [car, u_entry], [cdr, u_entry]], []]]])=FnResult
              ),
              block_exit(sys_hash_table_iterator_function, FnResult),
              true).
:- fixup_exports.

end_of_file.

