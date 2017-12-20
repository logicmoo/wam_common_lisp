
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_read_pr" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:20 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Pretty printer and reader for obs.",
				     1,
				     224)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:260 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 261)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:262 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 11/6/85: Original version written",
				     1,
				     263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:298 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/24/86: Added fread and major-type",
				     1,
				     299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:336 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/30/86: Changed readers to disallow literal atoms if there is",
				     1,
				     337)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:401 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          no such ob. This is too error prone.",
				     1,
				     402)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/22/86: Added pprulesize, tabulation, etc.",
				     1,
				     450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:495 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/24/86: Got rid of flavors", 1, 496)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:525 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 526)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:527 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     528)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:608 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*unknown-ob*',
			    
			    [ 'ty$create',
			      [quote, 'UNKNOWN'],
			      [],
			      [quote, [prop, [obj], []]]
			    ]
			  ]).
:- f_u_ty_c36_create(u_unknown, [], [u_prop, [u_obj], []], _Ignored),
   set_var(TLEnv3, u_xx_unknown_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:671 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*notype-ob*',
			    
			    [ 'ty$create',
			      [quote, 'NOTYPE'],
			      [],
			      [quote, [[], [actor, from, to, obj, strength], []]]
			    ]
			  ]).
:- f_u_ty_c36_create(u_notype,
		     [],
		     [[], [u_actor, u_from, u_to, u_obj, u_strength], []],
		     _Ignored),
   set_var(TLEnv3, u_xx_notype_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:760 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$read',
			    [stream],
			    ['ob$readlist', [read, stream]]
			  ]).

% annotating U::OB$READ 
wl: lambda_def(defun,
	      u_ob_c36_read,
	      f_u_ob_c36_read,
	      [stream],
	      [[u_ob_c36_readlist, [read, stream]]]).


% annotating U::OB$READ 
wl: arglist_info(u_ob_c36_read,
		[stream],
		[Stream_Param],
		arginfo{ all:[stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream],
			 opt:0,
			 req:[stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$READ 
wl: init_args(exact_only, u_ob_c36_read).


% annotating U::OB$READ 
f_u_ob_c36_read(Stream_Param, FnResult) :-
	cl_read(Stream_Param, C36_readlist_Param),
	f_u_ob_c36_readlist(C36_readlist_Param, C36_readlist_Ret),
	C36_readlist_Ret=FnResult.
:- set_opv(f_u_ob_c36_read, classof, claz_function),
   set_opv(u_ob_c36_read, compile_as, kw_function),
   set_opv(u_ob_c36_read, function, f_u_ob_c36_read),
   DefunResult=u_ob_c36_read.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:816 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$fread',
			    [stream],
			    ['ob$freadlist', [read, stream]]
			  ]).

% annotating U::OB$FREAD 
wl: lambda_def(defun,
	      u_ob_c36_fread,
	      f_u_ob_c36_fread,
	      [stream],
	      [[u_ob_c36_freadlist, [read, stream]]]).


% annotating U::OB$FREAD 
wl: arglist_info(u_ob_c36_fread,
		[stream],
		[Stream_Param],
		arginfo{ all:[stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream],
			 opt:0,
			 req:[stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$FREAD 
wl: init_args(exact_only, u_ob_c36_fread).


% annotating U::OB$FREAD 
f_u_ob_c36_fread(Stream_Param, FnResult) :-
	cl_read(Stream_Param, C36_freadlist_Param),
	f_u_ob_c36_freadlist(C36_freadlist_Param, C36_freadlist_Ret),
	C36_freadlist_Ret=FnResult.
:- set_opv(f_u_ob_c36_fread, classof, claz_function),
   set_opv(u_ob_c36_fread, compile_as, kw_function),
   set_opv(u_ob_c36_fread, function, f_u_ob_c36_fread),
   DefunResult=u_ob_c36_fread.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$readlist',
			    [ppob],
			    
			    [ let,
			      [[ob, ['ob$create-empty']]],
			      
			      [ if,
				[not, ['symbol?', [car, ppob]]],
				
				[ error,
				  '$STRING'("bad ob list format: head of ~A not a symbol"),
				  ppob
				]
			      ],
			      
			      [ let,
				
				[ [type, ['ob$name->ob', [car, ppob]]],
				  ['slot-names', []],
				  [ppformat, []]
				],
				
				[ if,
				  ['null?', type],
				  
				  [ progn,
				    [setq, type, []],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'ob-warn',
				      '$STRING'("Warning: bad ob list format: head of ~A not a type~%"),
				      ppob
				    ],
				    
				    [ if,
				      ['null?', type],
				      [setq, type, '*unknown-ob*']
				    ]
				  ]
				],
				['ob$add', ob, [quote, type], type],
				
				[ setq,
				  ppformat,
				  ['ob$get', type, [quote, ppformat]]
				],
				
				[ if,
				  ['null?', ppformat],
				  
				  [ error,
				    '$STRING'("bad ob list format: ~A not a readable type"),
				    ppob
				  ]
				],
				[setq, 'slot-names', [cadr, ppformat]],
				
				[ yloop,
				  
				  [ initial,
				    [rest, [cdr, ppob]],
				    [elem, []],
				    ['ob-elem', []],
				    ['explicit-slot-name', []]
				  ],
				  [ywhile, rest],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ and,
					
					[ 'memq?',
					  [car, 'slot-names'],
					  [caddr, ppformat]
					],
					[cdr, 'slot-names']
				      ],
				      [setq, 'slot-names', [cdr, 'slot-names']],
				      
				      [ progn,
					[setq, elem, [car, rest]],
					
					[ cond,
					  
					  [ 
					    [ or,
					      ['var?', elem],
					      ['special?', elem]
					    ],
					    
					    [ 'ob$add',
					      ob,
					      
					      [ or,
						'explicit-slot-name',
						[car, 'slot-names']
					      ],
					      elem
					    ],
					    
					    [ if,
					      ['null?', 'explicit-slot-name'],
					      
					      [ setq,
						'slot-names',
						['stuck-cdr', 'slot-names']
					      ]
					    ]
					  ],
					  
					  [ ['string?', elem],
					    
					    [ 'ob$add',
					      ob,
					      
					      [ or,
						'explicit-slot-name',
						[car, 'slot-names']
					      ],
					      elem
					    ],
					    
					    [ if,
					      ['null?', 'explicit-slot-name'],
					      
					      [ setq,
						'slot-names',
						['stuck-cdr', 'slot-names']
					      ]
					    ]
					  ],
					  
					  [ ['pair?', elem],
					    
					    [ if,
					      ['eq?', [car, elem], [quote, quote]],
					      
					      [ 'ob$add',
						ob,
						
						[ or,
						  'explicit-slot-name',
						  [car, 'slot-names']
						],
						[cadr, elem]
					      ],
					      
					      [ 'ob$add',
						ob,
						
						[ or,
						  'explicit-slot-name',
						  [car, 'slot-names']
						],
						['ob$readlist', elem]
					      ]
					    ],
					    
					    [ if,
					      ['null?', 'explicit-slot-name'],
					      
					      [ setq,
						'slot-names',
						['stuck-cdr', 'slot-names']
					      ]
					    ]
					  ],
					  
					  [ 
					    [ or,
					      ['symbol?', elem],
					      ['number?', elem]
					    ],
					    
					    [ cond,
					      
					      [ ['eq?', elem, [quote, obname]],
						
						[ setq,
						  'explicit-slot-name',
						  [quote, obname]
						]
					      ],
					      
					      [ ['eq?', elem, [quote, --]],
						
						[ setq,
						  'slot-names',
						  ['stuck-cdr', 'slot-names']
						]
					      ],
					      
					      [ ['memq?', elem, 'slot-names'],
						
						[ setq,
						  'explicit-slot-name',
						  elem
						]
					      ],
					      
					      [ 
						[ setq,
						  'ob-elem',
						  ['ob$name->ob', elem]
						],
						
						[ 'ob$add',
						  ob,
						  
						  [ or,
						    'explicit-slot-name',
						    [car, 'slot-names']
						  ],
						  'ob-elem'
						],
						
						[ if,
						  
						  [ 'null?',
						    'explicit-slot-name'
						  ],
						  
						  [ setq,
						    'slot-names',
						    ['stuck-cdr', 'slot-names']
						  ]
						]
					      ],
					      
					      [ else,
						
						[ if,
						  
						  [ and,
						    [not, ['number?', elem]],
						    [not, ['string?', elem]],
						    [not, ['nil?', elem]],
						    
						    [ 'neq?',
						      'explicit-slot-name',
						      [quote, obname]
						    ]
						  ],
						  
						  [ progn,
						    
						    [ 'ndbg-roman-nl',
						      '*gate-dbg*',
						      'ob-warn',
						      '$STRING'("Warning: No such ob name ~A, assume it is atom~%"),
						      elem
						    ]
						  ]
						],
						
						[ 'ob$add',
						  ob,
						  
						  [ or,
						    'explicit-slot-name',
						    [car, 'slot-names']
						  ],
						  elem
						],
						
						[ if,
						  
						  [ 'null?',
						    'explicit-slot-name'
						  ],
						  
						  [ setq,
						    'slot-names',
						    ['stuck-cdr', 'slot-names']
						  ]
						]
					      ]
					    ]
					  ],
					  
					  [ ['ob?', elem],
					    
					    [ 'ob$add',
					      ob,
					      
					      [ or,
						'explicit-slot-name',
						[car, 'slot-names']
					      ],
					      elem
					    ],
					    
					    [ if,
					      ['null?', 'explicit-slot-name'],
					      
					      [ setq,
						'slot-names',
						['stuck-cdr', 'slot-names']
					      ]
					    ]
					  ],
					  
					  [ else,
					    
					    [ error,
					      '$STRING'("bad ob list format: unknown elem ~A in ~A"),
					      elem,
					      ppob
					    ]
					  ]
					],
					
					[ setq,
					  rest,
					  
					  [ c,
					    
					    [ setq,
					      'slot-names',
					      ['stuck-cdr', 'slot-names']
					    ]
					  ]
					],
					
					[ ['memq?', elem, 'slot-names'],
					  [setq, 'explicit-slot-name', elem]
					],
					
					[ 
					  [ setq,
					    'ob-elem',
					    ['ob$name->ob', elem]
					  ],
					  
					  [ 'ob$add',
					    ob,
					    
					    [ or,
					      'explicit-slot-name',
					      [car, 'slot-names']
					    ],
					    'ob-elem'
					  ],
					  
					  [ if,
					    ['null?', 'explicit-slot-name'],
					    
					    [ setq,
					      'slot-names',
					      ['stuck-cdr', 'slot-names']
					    ]
					  ]
					],
					
					[ else,
					  
					  [ if,
					    
					    [ and,
					      [not, ['number?', elem]],
					      [not, ['string?', elem]],
					      [not, ['nil?', elem]],
					      
					      [ 'neq?',
						'explicit-slot-name',
						[quote, obname]
					      ]
					    ],
					    
					    [ progn,
					      
					      [ 'ndbg-roman-nl',
						'*gate-dbg*',
						'ob-warn',
						'$STRING'("Warning: No such ob name ~A, assume it is atom~%"),
						elem
					      ]
					    ]
					  ],
					  
					  [ 'ob$add',
					    ob,
					    
					    [ or,
					      'explicit-slot-name',
					      [car, 'slot-names']
					    ],
					    elem
					  ],
					  
					  [ if,
					    ['null?', 'explicit-slot-name'],
					    
					    [ setq,
					      'slot-names',
					      ['stuck-cdr', 'slot-names']
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ ['ob?', elem],
				      
				      [ 'ob$add',
					ob,
					
					[ or,
					  'explicit-slot-name',
					  [car, 'slot-names']
					],
					elem
				      ],
				      
				      [ if,
					['null?', 'explicit-slot-name'],
					
					[ setq,
					  'slot-names',
					  ['stuck-cdr', 'slot-names']
					]
				      ]
				    ],
				    
				    [ else,
				      
				      [ error,
					'$STRING'("bad ob list format: unknown elem ~A in ~A"),
					elem,
					ppob
				      ]
				    ]
				  ],
				  [setq, rest, [cdr, rest]]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$READLIST 
wl: lambda_def(defun,
	      u_ob_c36_readlist,
	      f_u_ob_c36_readlist,
	      [u_ppob],
	      
	      [ 
		[ let,
		  [[u_ob, [u_ob_c36_create_empty]]],
		  
		  [ if,
		    [not, [u_symbol_c63, [car, u_ppob]]],
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(b),
				 #\(a),
				 #\(d),
				 #\(' '),
				 #\(o),
				 #\(b),
				 #\(' '),
				 #\(l),
				 #\(i),
				 #\(s),
				 #\(t),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(m),
				 #\(a),
				 #\(t),
				 #\(:),
				 #\(' '),
				 #\(h),
				 #\(e),
				 #\(a),
				 #\(d),
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(a),
				 #\(' '),
				 #\(s),
				 #\(y),
				 #\(m),
				 #\(b),
				 #\(o),
				 #\(l)
			       ]),
		      u_ppob
		    ]
		  ],
		  
		  [ let,
		    
		    [ [type, [u_ob_c36_name_c62_ob, [car, u_ppob]]],
		      [u_slot_names, []],
		      [u_ppformat, []]
		    ],
		    
		    [ if,
		      [u_null_c63, type],
		      
		      [ progn,
			[setq, type, []],
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_ob_warn,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('W'),
				     #\(a),
				     #\(r),
				     #\(n),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(:),
				     #\(' '),
				     #\(b),
				     #\(a),
				     #\(d),
				     #\(' '),
				     #\(o),
				     #\(b),
				     #\(' '),
				     #\(l),
				     #\(i),
				     #\(s),
				     #\(t),
				     #\(' '),
				     #\(f),
				     #\(o),
				     #\(r),
				     #\(m),
				     #\(a),
				     #\(t),
				     #\(:),
				     #\(' '),
				     #\(h),
				     #\(e),
				     #\(a),
				     #\(d),
				     #\(' '),
				     #\(o),
				     #\(f),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(' '),
				     #\(n),
				     #\(o),
				     #\(t),
				     #\(' '),
				     #\(a),
				     #\(' '),
				     #\(t),
				     #\(y),
				     #\(p),
				     #\(e),
				     #\(~),
				     #\('%')
				   ]),
			  u_ppob
			],
			[if, [u_null_c63, type], [setq, type, u_xx_unknown_ob_xx]]
		      ]
		    ],
		    [u_ob_c36_add, u_ob, [quote, type], type],
		    [setq, u_ppformat, [u_ob_c36_get, type, [quote, u_ppformat]]],
		    
		    [ if,
		      [u_null_c63, u_ppformat],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(b),
				   #\(a),
				   #\(d),
				   #\(' '),
				   #\(o),
				   #\(b),
				   #\(' '),
				   #\(l),
				   #\(i),
				   #\(s),
				   #\(t),
				   #\(' '),
				   #\(f),
				   #\(o),
				   #\(r),
				   #\(m),
				   #\(a),
				   #\(t),
				   #\(:),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(d),
				   #\(a),
				   #\(b),
				   #\(l),
				   #\(e),
				   #\(' '),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e)
				 ]),
			u_ppob
		      ]
		    ],
		    [setq, u_slot_names, [cadr, u_ppformat]],
		    
		    [ u_yloop,
		      
		      [ u_initial,
			[rest, [cdr, u_ppob]],
			[u_elem, []],
			[u_ob_elem, []],
			[u_explicit_slot_name, []]
		      ],
		      [u_ywhile, rest],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ and,
			    [u_memq_c63, [car, u_slot_names], [caddr, u_ppformat]],
			    [cdr, u_slot_names]
			  ],
			  [setq, u_slot_names, [cdr, u_slot_names]],
			  
			  [ progn,
			    [setq, u_elem, [car, rest]],
			    
			    [ cond,
			      
			      [ [or, [u_var_c63, u_elem], [u_special_c63, u_elem]],
				
				[ u_ob_c36_add,
				  u_ob,
				  [or, u_explicit_slot_name, [car, u_slot_names]],
				  u_elem
				],
				
				[ if,
				  [u_null_c63, u_explicit_slot_name],
				  
				  [ setq,
				    u_slot_names,
				    [u_stuck_cdr, u_slot_names]
				  ]
				]
			      ],
			      
			      [ [u_string_c63, u_elem],
				
				[ u_ob_c36_add,
				  u_ob,
				  [or, u_explicit_slot_name, [car, u_slot_names]],
				  u_elem
				],
				
				[ if,
				  [u_null_c63, u_explicit_slot_name],
				  
				  [ setq,
				    u_slot_names,
				    [u_stuck_cdr, u_slot_names]
				  ]
				]
			      ],
			      
			      [ [u_pair_c63, u_elem],
				
				[ if,
				  [u_eq_c63, [car, u_elem], [quote, quote]],
				  
				  [ u_ob_c36_add,
				    u_ob,
				    
				    [ or,
				      u_explicit_slot_name,
				      [car, u_slot_names]
				    ],
				    [cadr, u_elem]
				  ],
				  
				  [ u_ob_c36_add,
				    u_ob,
				    
				    [ or,
				      u_explicit_slot_name,
				      [car, u_slot_names]
				    ],
				    [u_ob_c36_readlist, u_elem]
				  ]
				],
				
				[ if,
				  [u_null_c63, u_explicit_slot_name],
				  
				  [ setq,
				    u_slot_names,
				    [u_stuck_cdr, u_slot_names]
				  ]
				]
			      ],
			      
			      [ 
				[ or,
				  [u_symbol_c63, u_elem],
				  [u_number_c63, u_elem]
				],
				
				[ cond,
				  
				  [ [u_eq_c63, u_elem, [quote, u_obname]],
				    
				    [ setq,
				      u_explicit_slot_name,
				      [quote, u_obname]
				    ]
				  ],
				  
				  [ [u_eq_c63, u_elem, [quote, --]],
				    
				    [ setq,
				      u_slot_names,
				      [u_stuck_cdr, u_slot_names]
				    ]
				  ],
				  
				  [ [u_memq_c63, u_elem, u_slot_names],
				    [setq, u_explicit_slot_name, u_elem]
				  ],
				  
				  [ 
				    [ setq,
				      u_ob_elem,
				      [u_ob_c36_name_c62_ob, u_elem]
				    ],
				    
				    [ u_ob_c36_add,
				      u_ob,
				      
				      [ or,
					u_explicit_slot_name,
					[car, u_slot_names]
				      ],
				      u_ob_elem
				    ],
				    
				    [ if,
				      [u_null_c63, u_explicit_slot_name],
				      
				      [ setq,
					u_slot_names,
					[u_stuck_cdr, u_slot_names]
				      ]
				    ]
				  ],
				  
				  [ u_else,
				    
				    [ if,
				      
				      [ and,
					[not, [u_number_c63, u_elem]],
					[not, [u_string_c63, u_elem]],
					[not, [u_nil_c63, u_elem]],
					
					[ u_neq_c63,
					  u_explicit_slot_name,
					  [quote, u_obname]
					]
				      ],
				      
				      [ progn,
					
					[ u_ndbg_roman_nl,
					  u_xx_gate_dbg_xx,
					  u_ob_warn,
					  '$ARRAY'([*],
						   claz_base_character,
						   
						   [ #\('W'),
						     #\(a),
						     #\(r),
						     #\(n),
						     #\(i),
						     #\(n),
						     #\(g),
						     #\(:),
						     #\(' '),
						     #\('N'),
						     #\(o),
						     #\(' '),
						     #\(s),
						     #\(u),
						     #\(c),
						     #\(h),
						     #\(' '),
						     #\(o),
						     #\(b),
						     #\(' '),
						     #\(n),
						     #\(a),
						     #\(m),
						     #\(e),
						     #\(' '),
						     #\(~),
						     #\('A'),
						     #\(','),
						     #\(' '),
						     #\(a),
						     #\(s),
						     #\(s),
						     #\(u),
						     #\(m),
						     #\(e),
						     #\(' '),
						     #\(i),
						     #\(t),
						     #\(' '),
						     #\(i),
						     #\(s),
						     #\(' '),
						     #\(a),
						     #\(t),
						     #\(o),
						     #\(m),
						     #\(~),
						     #\('%')
						   ]),
					  u_elem
					]
				      ]
				    ],
				    
				    [ u_ob_c36_add,
				      u_ob,
				      
				      [ or,
					u_explicit_slot_name,
					[car, u_slot_names]
				      ],
				      u_elem
				    ],
				    
				    [ if,
				      [u_null_c63, u_explicit_slot_name],
				      
				      [ setq,
					u_slot_names,
					[u_stuck_cdr, u_slot_names]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ [u_ob_c63, u_elem],
				
				[ u_ob_c36_add,
				  u_ob,
				  [or, u_explicit_slot_name, [car, u_slot_names]],
				  u_elem
				],
				
				[ if,
				  [u_null_c63, u_explicit_slot_name],
				  
				  [ setq,
				    u_slot_names,
				    [u_stuck_cdr, u_slot_names]
				  ]
				]
			      ],
			      
			      [ u_else,
				
				[ error,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(b),
					     #\(a),
					     #\(d),
					     #\(' '),
					     #\(o),
					     #\(b),
					     #\(' '),
					     #\(l),
					     #\(i),
					     #\(s),
					     #\(t),
					     #\(' '),
					     #\(f),
					     #\(o),
					     #\(r),
					     #\(m),
					     #\(a),
					     #\(t),
					     #\(:),
					     #\(' '),
					     #\(u),
					     #\(n),
					     #\(k),
					     #\(n),
					     #\(o),
					     #\(w),
					     #\(n),
					     #\(' '),
					     #\(e),
					     #\(l),
					     #\(e),
					     #\(m),
					     #\(' '),
					     #\(~),
					     #\('A'),
					     #\(' '),
					     #\(i),
					     #\(n),
					     #\(' '),
					     #\(~),
					     #\('A')
					   ]),
				  u_elem,
				  u_ppob
				]
			      ]
			    ],
			    
			    [ setq,
			      rest,
			      
			      [ u_c,
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ [u_memq_c63, u_elem, u_slot_names],
			      [setq, u_explicit_slot_name, u_elem]
			    ],
			    
			    [ [setq, u_ob_elem, [u_ob_c36_name_c62_ob, u_elem]],
			      
			      [ u_ob_c36_add,
				u_ob,
				[or, u_explicit_slot_name, [car, u_slot_names]],
				u_ob_elem
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ u_else,
			      
			      [ if,
				
				[ and,
				  [not, [u_number_c63, u_elem]],
				  [not, [u_string_c63, u_elem]],
				  [not, [u_nil_c63, u_elem]],
				  
				  [ u_neq_c63,
				    u_explicit_slot_name,
				    [quote, u_obname]
				  ]
				],
				
				[ progn,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_ob_warn,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('W'),
					       #\(a),
					       #\(r),
					       #\(n),
					       #\(i),
					       #\(n),
					       #\(g),
					       #\(:),
					       #\(' '),
					       #\('N'),
					       #\(o),
					       #\(' '),
					       #\(s),
					       #\(u),
					       #\(c),
					       #\(h),
					       #\(' '),
					       #\(o),
					       #\(b),
					       #\(' '),
					       #\(n),
					       #\(a),
					       #\(m),
					       #\(e),
					       #\(' '),
					       #\(~),
					       #\('A'),
					       #\(','),
					       #\(' '),
					       #\(a),
					       #\(s),
					       #\(s),
					       #\(u),
					       #\(m),
					       #\(e),
					       #\(' '),
					       #\(i),
					       #\(t),
					       #\(' '),
					       #\(i),
					       #\(s),
					       #\(' '),
					       #\(a),
					       #\(t),
					       #\(o),
					       #\(m),
					       #\(~),
					       #\('%')
					     ]),
				    u_elem
				  ]
				]
			      ],
			      
			      [ u_ob_c36_add,
				u_ob,
				[or, u_explicit_slot_name, [car, u_slot_names]],
				u_elem
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ]
			  ]
			],
			
			[ [u_ob_c63, u_elem],
			  
			  [ u_ob_c36_add,
			    u_ob,
			    [or, u_explicit_slot_name, [car, u_slot_names]],
			    u_elem
			  ],
			  
			  [ if,
			    [u_null_c63, u_explicit_slot_name],
			    [setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			  ]
			],
			
			[ u_else,
			  
			  [ error,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(b),
				       #\(a),
				       #\(d),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(' '),
				       #\(l),
				       #\(i),
				       #\(s),
				       #\(t),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(a),
				       #\(t),
				       #\(:),
				       #\(' '),
				       #\(u),
				       #\(n),
				       #\(k),
				       #\(n),
				       #\(o),
				       #\(w),
				       #\(n),
				       #\(' '),
				       #\(e),
				       #\(l),
				       #\(e),
				       #\(m),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_elem,
			    u_ppob
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$READLIST 
wl: arglist_info(u_ob_c36_readlist,
		[u_ppob],
		[Ppob_Param],
		arginfo{ all:[u_ppob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ppob],
			 opt:0,
			 req:[u_ppob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$READLIST 
wl: init_args(exact_only, u_ob_c36_readlist).


% annotating U::OB$READLIST 
f_u_ob_c36_readlist(Ppob_Param, FnResult) :-
	Env=[bv(u_ppob, Ppob_Param)],
	f_u_ob_c36_create_empty(Ob_Init),
	LEnv=[[bv(u_ob, Ob_Init)]|Env],
	f_u_symbol_c63([car, u_ppob], PredArgResult),
	(   PredArgResult==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(b),
				  #\(a),
				  #\(d),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\(' '),
				  #\(l),
				  #\(i),
				  #\(s),
				  #\(t),
				  #\(' '),
				  #\(f),
				  #\(o),
				  #\(r),
				  #\(m),
				  #\(a),
				  #\(t),
				  #\(:),
				  #\(' '),
				  #\(h),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(' '),
				  #\(o),
				  #\(f),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(a),
				  #\(' '),
				  #\(s),
				  #\(y),
				  #\(m),
				  #\(b),
				  #\(o),
				  #\(l)
				]),
		       Ppob_Param
		     ],
		     TrueResult),
	    _146220=TrueResult
	;   _146220=[]
	),
	cl_car(Ppob_Param, C62_ob_Param),
	f_u_ob_c36_name_c62_ob(C62_ob_Param, Type_Init),
	Env=[[bv(type, Type_Init), bv(u_slot_names, []), bv(u_ppformat, [])]|LEnv],
	f_u_null_c63(type, IFTEST25),
	(   IFTEST25\==[]
	->  set_var(Env, setq, type, []),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_ob_warn,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('W'),
					   #\(a),
					   #\(r),
					   #\(n),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(:),
					   #\(' '),
					   #\(b),
					   #\(a),
					   #\(d),
					   #\(' '),
					   #\(o),
					   #\(b),
					   #\(' '),
					   #\(l),
					   #\(i),
					   #\(s),
					   #\(t),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(m),
					   #\(a),
					   #\(t),
					   #\(:),
					   #\(' '),
					   #\(h),
					   #\(e),
					   #\(a),
					   #\(d),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(a),
					   #\(' '),
					   #\(t),
					   #\(y),
					   #\(p),
					   #\(e),
					   #\(~),
					   #\('%')
					 ]),
				u_ppob
			      ],
			      Roman_nl_Ret),
	    f_u_null_c63(type, IFTEST28),
	    (   IFTEST28\==[]
	    ->  get_var(Env, u_xx_unknown_ob_xx, Xx_unknown_ob_xx_Get),
		set_var(Env, type, Xx_unknown_ob_xx_Get),
		TrueResult32=Xx_unknown_ob_xx_Get
	    ;   TrueResult32=[]
	    )
	;   TrueResult32=[]
	),
	get_var(Env, type, Type_Get),
	get_var(Env, u_ob, Ob_Get),
	f_u_ob_c36_add(Ob_Get, type, Type_Get, C36_add_Ret),
	get_var(Env, type, Type_Get35),
	f_u_ob_c36_get(Type_Get35, u_ppformat, Ppformat),
	set_var(Env, u_ppformat, Ppformat),
	f_u_null_c63(u_ppformat, IFTEST36),
	(   IFTEST36\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(b),
				  #\(a),
				  #\(d),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\(' '),
				  #\(l),
				  #\(i),
				  #\(s),
				  #\(t),
				  #\(' '),
				  #\(f),
				  #\(o),
				  #\(r),
				  #\(m),
				  #\(a),
				  #\(t),
				  #\(:),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(a),
				  #\(' '),
				  #\(r),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(a),
				  #\(b),
				  #\(l),
				  #\(e),
				  #\(' '),
				  #\(t),
				  #\(y),
				  #\(p),
				  #\(e)
				]),
		       Ppob_Param
		     ],
		     TrueResult39),
	    _146894=TrueResult39
	;   _146894=[]
	),
	get_var(Env, u_ppformat, Ppformat_Get),
	cl_cadr(Ppformat_Get, Slot_names),
	set_var(Env, u_slot_names, Slot_names),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [rest, [cdr, u_ppob]],
		      [u_elem, []],
		      [u_ob_elem, []],
		      [u_explicit_slot_name, []]
		    ],
		    [u_ywhile, rest],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_memq_c63, [car, u_slot_names], [caddr, u_ppformat]],
			  [cdr, u_slot_names]
			],
			[setq, u_slot_names, [cdr, u_slot_names]],
			
			[ progn,
			  [setq, u_elem, [car, rest]],
			  
			  [ cond,
			    
			    [ [or, [u_var_c63, u_elem], [u_special_c63, u_elem]],
			      
			      [ u_ob_c36_add,
				u_ob,
				[or, u_explicit_slot_name, [car, u_slot_names]],
				u_elem
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ [u_string_c63, u_elem],
			      
			      [ u_ob_c36_add,
				u_ob,
				[or, u_explicit_slot_name, [car, u_slot_names]],
				u_elem
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ [u_pair_c63, u_elem],
			      
			      [ if,
				[u_eq_c63, [car, u_elem], [quote, quote]],
				
				[ u_ob_c36_add,
				  u_ob,
				  [or, u_explicit_slot_name, [car, u_slot_names]],
				  [cadr, u_elem]
				],
				
				[ u_ob_c36_add,
				  u_ob,
				  [or, u_explicit_slot_name, [car, u_slot_names]],
				  [u_ob_c36_readlist, u_elem]
				]
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ [or, [u_symbol_c63, u_elem], [u_number_c63, u_elem]],
			      
			      [ cond,
				
				[ [u_eq_c63, u_elem, [quote, u_obname]],
				  [setq, u_explicit_slot_name, [quote, u_obname]]
				],
				
				[ [u_eq_c63, u_elem, [quote, --]],
				  
				  [ setq,
				    u_slot_names,
				    [u_stuck_cdr, u_slot_names]
				  ]
				],
				
				[ [u_memq_c63, u_elem, u_slot_names],
				  [setq, u_explicit_slot_name, u_elem]
				],
				
				[ 
				  [ setq,
				    u_ob_elem,
				    [u_ob_c36_name_c62_ob, u_elem]
				  ],
				  
				  [ u_ob_c36_add,
				    u_ob,
				    
				    [ or,
				      u_explicit_slot_name,
				      [car, u_slot_names]
				    ],
				    u_ob_elem
				  ],
				  
				  [ if,
				    [u_null_c63, u_explicit_slot_name],
				    
				    [ setq,
				      u_slot_names,
				      [u_stuck_cdr, u_slot_names]
				    ]
				  ]
				],
				
				[ u_else,
				  
				  [ if,
				    
				    [ and,
				      [not, [u_number_c63, u_elem]],
				      [not, [u_string_c63, u_elem]],
				      [not, [u_nil_c63, u_elem]],
				      
				      [ u_neq_c63,
					u_explicit_slot_name,
					[quote, u_obname]
				      ]
				    ],
				    
				    [ progn,
				      
				      [ u_ndbg_roman_nl,
					u_xx_gate_dbg_xx,
					u_ob_warn,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('W'),
						   #\(a),
						   #\(r),
						   #\(n),
						   #\(i),
						   #\(n),
						   #\(g),
						   #\(:),
						   #\(' '),
						   #\('N'),
						   #\(o),
						   #\(' '),
						   #\(s),
						   #\(u),
						   #\(c),
						   #\(h),
						   #\(' '),
						   #\(o),
						   #\(b),
						   #\(' '),
						   #\(n),
						   #\(a),
						   #\(m),
						   #\(e),
						   #\(' '),
						   #\(~),
						   #\('A'),
						   #\(','),
						   #\(' '),
						   #\(a),
						   #\(s),
						   #\(s),
						   #\(u),
						   #\(m),
						   #\(e),
						   #\(' '),
						   #\(i),
						   #\(t),
						   #\(' '),
						   #\(i),
						   #\(s),
						   #\(' '),
						   #\(a),
						   #\(t),
						   #\(o),
						   #\(m),
						   #\(~),
						   #\('%')
						 ]),
					u_elem
				      ]
				    ]
				  ],
				  
				  [ u_ob_c36_add,
				    u_ob,
				    
				    [ or,
				      u_explicit_slot_name,
				      [car, u_slot_names]
				    ],
				    u_elem
				  ],
				  
				  [ if,
				    [u_null_c63, u_explicit_slot_name],
				    
				    [ setq,
				      u_slot_names,
				      [u_stuck_cdr, u_slot_names]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ [u_ob_c63, u_elem],
			      
			      [ u_ob_c36_add,
				u_ob,
				[or, u_explicit_slot_name, [car, u_slot_names]],
				u_elem
			      ],
			      
			      [ if,
				[u_null_c63, u_explicit_slot_name],
				[setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			      ]
			    ],
			    
			    [ u_else,
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(b),
					   #\(a),
					   #\(d),
					   #\(' '),
					   #\(o),
					   #\(b),
					   #\(' '),
					   #\(l),
					   #\(i),
					   #\(s),
					   #\(t),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(m),
					   #\(a),
					   #\(t),
					   #\(:),
					   #\(' '),
					   #\(u),
					   #\(n),
					   #\(k),
					   #\(n),
					   #\(o),
					   #\(w),
					   #\(n),
					   #\(' '),
					   #\(e),
					   #\(l),
					   #\(e),
					   #\(m),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_elem,
				u_ppob
			      ]
			    ]
			  ],
			  
			  [ setq,
			    rest,
			    
			    [ u_c,
			      [setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			    ]
			  ],
			  
			  [ [u_memq_c63, u_elem, u_slot_names],
			    [setq, u_explicit_slot_name, u_elem]
			  ],
			  
			  [ [setq, u_ob_elem, [u_ob_c36_name_c62_ob, u_elem]],
			    
			    [ u_ob_c36_add,
			      u_ob,
			      [or, u_explicit_slot_name, [car, u_slot_names]],
			      u_ob_elem
			    ],
			    
			    [ if,
			      [u_null_c63, u_explicit_slot_name],
			      [setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			    ]
			  ],
			  
			  [ u_else,
			    
			    [ if,
			      
			      [ and,
				[not, [u_number_c63, u_elem]],
				[not, [u_string_c63, u_elem]],
				[not, [u_nil_c63, u_elem]],
				
				[ u_neq_c63,
				  u_explicit_slot_name,
				  [quote, u_obname]
				]
			      ],
			      
			      [ progn,
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_ob_warn,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('W'),
					     #\(a),
					     #\(r),
					     #\(n),
					     #\(i),
					     #\(n),
					     #\(g),
					     #\(:),
					     #\(' '),
					     #\('N'),
					     #\(o),
					     #\(' '),
					     #\(s),
					     #\(u),
					     #\(c),
					     #\(h),
					     #\(' '),
					     #\(o),
					     #\(b),
					     #\(' '),
					     #\(n),
					     #\(a),
					     #\(m),
					     #\(e),
					     #\(' '),
					     #\(~),
					     #\('A'),
					     #\(','),
					     #\(' '),
					     #\(a),
					     #\(s),
					     #\(s),
					     #\(u),
					     #\(m),
					     #\(e),
					     #\(' '),
					     #\(i),
					     #\(t),
					     #\(' '),
					     #\(i),
					     #\(s),
					     #\(' '),
					     #\(a),
					     #\(t),
					     #\(o),
					     #\(m),
					     #\(~),
					     #\('%')
					   ]),
				  u_elem
				]
			      ]
			    ],
			    
			    [ u_ob_c36_add,
			      u_ob,
			      [or, u_explicit_slot_name, [car, u_slot_names]],
			      u_elem
			    ],
			    
			    [ if,
			      [u_null_c63, u_explicit_slot_name],
			      [setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			    ]
			  ]
			]
		      ],
		      
		      [ [u_ob_c63, u_elem],
			
			[ u_ob_c36_add,
			  u_ob,
			  [or, u_explicit_slot_name, [car, u_slot_names]],
			  u_elem
			],
			
			[ if,
			  [u_null_c63, u_explicit_slot_name],
			  [setq, u_slot_names, [u_stuck_cdr, u_slot_names]]
			]
		      ],
		      
		      [ u_else,
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(b),
				     #\(a),
				     #\(d),
				     #\(' '),
				     #\(o),
				     #\(b),
				     #\(' '),
				     #\(l),
				     #\(i),
				     #\(s),
				     #\(t),
				     #\(' '),
				     #\(f),
				     #\(o),
				     #\(r),
				     #\(m),
				     #\(a),
				     #\(t),
				     #\(:),
				     #\(' '),
				     #\(u),
				     #\(n),
				     #\(k),
				     #\(n),
				     #\(o),
				     #\(w),
				     #\(n),
				     #\(' '),
				     #\(e),
				     #\(l),
				     #\(e),
				     #\(m),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(' '),
				     #\(~),
				     #\('A')
				   ]),
			  u_elem,
			  u_ppob
			]
		      ]
		    ],
		    [setq, rest, [cdr, rest]]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ob_c36_readlist, classof, claz_function),
   set_opv(u_ob_c36_readlist, compile_as, kw_function),
   set_opv(u_ob_c36_readlist, function, f_u_ob_c36_readlist),
   DefunResult=u_ob_c36_readlist.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: should punt to freadlist in such a case, so that",
				     7,
				     1037)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" something like ('(?Type) actor (HA)) could be handled.",
				     7,
				     1100)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: The below is semantically wrong wrt ppformat: it enables the",
				     1,
				     1841)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" last element of the cadr to be implicit even if this is supposed",
				     1,
				     1910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to be prevented by the caddr. But this was done to allow obname",
				     1,
				     1977)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" to work.", 1, 2043)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (setq slot-names (stuck-cdr slot-names))",
				     1,
				     2204)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               ((var? elem)", 1, 3431)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (ob$add ob (or explicit-slot-name (car slot-names))",
				     1,
				     3460)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 elem)", 1, 3529)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (if (null? explicit-slot-name)",
				     1,
				     3553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                    (setq slot-names (stuck-cdr slot-names))))",
				     1,
				     3601)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6360 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ndbg-roman-nl',
			    '*gate-dbg*',
			    'ob-warn',
			    '$STRING'("Warning: No such ob name ~A, assume it is atom"),
			    elem
			  ]).
:- f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
		     u_ob_warn,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('W'),
				  #\(a),
				  #\(r),
				  #\(n),
				  #\(i),
				  #\(n),
				  #\(g),
				  #\(:),
				  #\(' '),
				  #\('N'),
				  #\(o),
				  #\(' '),
				  #\(s),
				  #\(u),
				  #\(c),
				  #\(h),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\(' '),
				  #\(n),
				  #\(a),
				  #\(m),
				  #\(e),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(','),
				  #\(' '),
				  #\(a),
				  #\(s),
				  #\(s),
				  #\(u),
				  #\(m),
				  #\(e),
				  #\(' '),
				  #\(i),
				  #\(t),
				  #\(' '),
				  #\(i),
				  #\(s),
				  #\(' '),
				  #\(a),
				  #\(t),
				  #\(o),
				  #\(m)
				]),
		       u_elem
		     ],
		     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6533 **********************/
:- lisp_compile_to_prolog(pkg_user, elem).
:- get_var(TLEnv3, u_elem, Elem_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6537 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6558 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 
			    [ or,
			      ['ob?', elem],
			      ['string?', elem],
			      ['number?', elem]
			    ],
			    elem
			  ]).
:- cl_eval(
	   [ 
	     [ or,
	       [u_ob_c63, u_elem],
	       [u_string_c63, u_elem],
	       [u_number_c63, u_elem]
	     ],
	     u_elem
	   ],
	   _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6657 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ ['pair?', elem],
			    
			    [ if,
			      ['eq?', [car, elem], [quote, quote]],
			      [cadr, elem],
			      ['ob$freadlist', elem]
			    ]
			  ]).
:- cl_eval(
	   [ [u_pair_c63, u_elem],
	     
	     [ if,
	       [u_eq_c63, [car, u_elem], [quote, quote]],
	       [cadr, u_elem],
	       [u_ob_c36_freadlist, u_elem]
	     ]
	   ],
	   _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ else,
			    
			    [ error,
			      '$STRING'("ob$freadlist: unknown elem ~A in ~A"),
			      elem,
			      ppob
			    ]
			  ]).
:- get_var(TLEnv3, u_elem, Elem_Get),
   get_var(TLEnv3, u_ppob, Ppob_Get),
   cl_error(
	    [ '$ARRAY'([*],
		       claz_base_character,
		       
		       [ #\(o),
			 #\(b),
			 #\($),
			 #\(f),
			 #\(r),
			 #\(e),
			 #\(a),
			 #\(d),
			 #\(l),
			 #\(i),
			 #\(s),
			 #\(t),
			 #\(:),
			 #\(' '),
			 #\(u),
			 #\(n),
			 #\(k),
			 #\(n),
			 #\(o),
			 #\(w),
			 #\(n),
			 #\(' '),
			 #\(e),
			 #\(l),
			 #\(e),
			 #\(m),
			 #\(' '),
			 #\(~),
			 #\('A'),
			 #\(' '),
			 #\(i),
			 #\(n),
			 #\(' '),
			 #\(~),
			 #\('A')
		       ]),
	      Elem_Get,
	      Ppob_Get
	    ],
	    Else_Param),
   f_u_else(Else_Param, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6942 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6943 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:6953 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ndbg-roman-nl',
			    '*gate-dbg*',
			    'ob-warn',
			    '$STRING'("Warning: in ob$freadlist, value of slot ~A is nil"),
			    [car, rest]
			  ]).
:- f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
		     u_ob_warn,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('W'),
				  #\(a),
				  #\(r),
				  #\(n),
				  #\(i),
				  #\(n),
				  #\(g),
				  #\(:),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\($),
				  #\(f),
				  #\(r),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(l),
				  #\(i),
				  #\(s),
				  #\(t),
				  #\(','),
				  #\(' '),
				  #\(v),
				  #\(a),
				  #\(l),
				  #\(u),
				  #\(e),
				  #\(' '),
				  #\(o),
				  #\(f),
				  #\(' '),
				  #\(s),
				  #\(l),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(i),
				  #\(s),
				  #\(' '),
				  #\(n),
				  #\(i),
				  #\(l)
				]),
		       [car, rest]
		     ],
		     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7096 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7107 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, rest, [cddr, rest]]).
:- get_var(TLEnv3, rest, Rest_Get),
   cl_cddr(Rest_Get, _Ignored),
   set_var(TLEnv3, rest, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7130 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7131 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7140 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ if,
			    ['eq?', '*notype-ob*', ['ob$ty', ob]],
			    ['ob$removes', ob, [quote, type]]
			  ]).
:- f_u_eq_c63(u_xx_notype_ob_xx, [u_ob_c36_ty, u_ob], IFTEST),
   (   IFTEST\==[]
   ->  get_var(TLEnv3, u_ob, Ob_Get),
       f_u_ob_c36_removes(Ob_Get, type, TrueResult),
       _Ignored=TrueResult
   ;   _Ignored=[]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7227 **********************/
:- lisp_compile_to_prolog(pkg_user, ob).
:- get_var(TLEnv3, u_ob, Ob_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7229 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7230 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7230 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7234)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level printing functions", 1, 7236)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7230 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7267)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Quicky ob print function", 1, 7270)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7296 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    po,
			    [ob],
			    
			    [ if,
			      ['ob?', ob],
			      
			      [ progn,
				
				[ 'ob$pr',
				  ob,
				  '*gate-output*',
				  '*ob-print-options*'
				],
				[newline, '*gate-output*']
			      ],
			      [format, '*gate-output*', '$STRING'("~A~%"), ob]
			    ],
			    '*repl-wont-print*'
			  ]).

% annotating U::PO 
wl: lambda_def(defun,
	      u_po,
	      f_u_po,
	      [u_ob],
	      
	      [ 
		[ if,
		  [u_ob_c63, u_ob],
		  
		  [ progn,
		    
		    [ u_ob_c36_pr,
		      u_ob,
		      u_xx_gate_output_xx,
		      u_xx_ob_print_options_xx
		    ],
		    [u_newline, u_xx_gate_output_xx]
		  ],
		  
		  [ format,
		    u_xx_gate_output_xx,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(~), #\('A'), #\(~), #\('%')]),
		    u_ob
		  ]
		],
		u_xx_repl_wont_print_xx
	      ]).


% annotating U::PO 
wl: arglist_info(u_po,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PO 
wl: init_args(exact_only, u_po).


% annotating U::PO 
f_u_po(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    get_var(Env, u_xx_ob_print_options_xx, Xx_ob_print_options_xx_Get),
	    f_u_ob_c36_pr(Ob_Param,
			  Xx_gate_output_xx_Get,
			  Xx_ob_print_options_xx_Get,
			  C36_pr_Ret),
	    f_u_newline(u_xx_gate_output_xx, TrueResult),
	    _130066=TrueResult
	;   get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get17),
	    cl_format(
		      [ Xx_gate_output_xx_Get17,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(~), #\('A'), #\(~), #\('%')]),
			Ob_Param
		      ],
		      ElseResult),
	    _130066=ElseResult
	),
	get_var(Env, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get),
	Xx_repl_wont_print_xx_Get=FnResult.
:- set_opv(f_u_po, classof, claz_function),
   set_opv(u_po, compile_as, kw_function),
   set_opv(u_po, function, f_u_po),
   DefunResult=u_po.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7484 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    pom,
			    [obs],
			    [map, [quote, list], [lambda, [ob], [po, ob]], obs]
			  ]).

% annotating U::POM 
wl: lambda_def(defun,
	      u_pom,
	      f_u_pom,
	      [u_obs],
	      [[map, [quote, list], [lambda, [u_ob], [u_po, u_ob]], u_obs]]).


% annotating U::POM 
wl: arglist_info(u_pom,
		[u_obs],
		[Obs_Param],
		arginfo{ all:[u_obs],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obs],
			 opt:0,
			 req:[u_obs],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::POM 
wl: init_args(exact_only, u_pom).


% annotating U::POM 
f_u_pom(Obs_Param, FnResult) :-
	Env=[bv(u_obs, Obs_Param)],
	Lambda=closure([Env13|Env], LResult, [u_ob],  (get_var(Env13, u_ob, Ob_Get), f_u_po(Ob_Get, LResult))),
	cl_map(list, Lambda, Obs_Param, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_pom, classof, claz_function),
   set_opv(u_pom, compile_as, kw_function),
   set_opv(u_pom, function, f_u_pom),
   DefunResult=u_pom.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7551 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    px,
			    [ob],
			    ['pretty-print', ['ob$pairs', ob], '*gate-output*'],
			    [newline, '*gate-output*'],
			    '*repl-wont-print*'
			  ]).

% annotating U::PX 
wl: lambda_def(defun,
	      u_px,
	      f_u_px,
	      [u_ob],
	      
	      [ [u_pretty_print, [u_ob_c36_pairs, u_ob], u_xx_gate_output_xx],
		[u_newline, u_xx_gate_output_xx],
		u_xx_repl_wont_print_xx
	      ]).


% annotating U::PX 
wl: arglist_info(u_px,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PX 
wl: init_args(exact_only, u_px).


% annotating U::PX 
f_u_px(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ob_c36_pairs(Ob_Param, Pretty_print_Param),
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	f_u_pretty_print(Pretty_print_Param,
			 Xx_gate_output_xx_Get,
			 Pretty_print_Ret),
	f_u_newline(u_xx_gate_output_xx, Newline_Ret),
	get_var(Env, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get),
	Xx_repl_wont_print_xx_Get=FnResult.
:- set_opv(f_u_px, classof, claz_function),
   set_opv(u_px, compile_as, kw_function),
   set_opv(u_px, function, f_u_px),
   DefunResult=u_px.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7659 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    pox,
			    [self, stream],
			    ['begin-regular-font', stream],
			    [format, stream, '$STRING'("[")],
			    ['end-font', stream],
			    
			    [ format,
			      stream,
			      '$STRING'("~A"),
			      ['ob->string', self]
			    ],
			    ['begin-regular-font', stream],
			    [format, stream, '$STRING'(": ")],
			    ['end-font', stream],
			    ['ob$sprint', self, stream],
			    ['begin-regular-font', stream],
			    [format, stream, '$STRING'("]")],
			    ['end-font', stream]
			  ]).

% annotating U::POX 
wl: lambda_def(defun,
	      u_pox,
	      f_u_pox,
	      [u_self, stream],
	      
	      [ [u_begin_regular_font, stream],
		[format, stream, '$ARRAY'([*], claz_base_character, [#\('[')])],
		[u_end_font, stream],
		
		[ format,
		  stream,
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		  [u_ob_c62_string, u_self]
		],
		[u_begin_regular_font, stream],
		
		[ format,
		  stream,
		  '$ARRAY'([*], claz_base_character, [#\(:), #\(' ')])
		],
		[u_end_font, stream],
		[u_ob_c36_sprint, u_self, stream],
		[u_begin_regular_font, stream],
		[format, stream, '$ARRAY'([*], claz_base_character, [#\(']')])],
		[u_end_font, stream]
	      ]).


% annotating U::POX 
wl: arglist_info(u_pox,
		[u_self, stream],
		[Self_Param, Stream_Param],
		arginfo{ all:[u_self, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, stream],
			 opt:0,
			 req:[u_self, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::POX 
wl: init_args(exact_only, u_pox).


% annotating U::POX 
f_u_pox(Self_Param, Stream_Param, FnResult) :-
	f_u_begin_regular_font(Stream_Param, Regular_font_Ret),
	cl_format([Stream_Param, '$ARRAY'([*], claz_base_character, [#\('[')])],
		  Format_Ret),
	f_u_end_font(Stream_Param, End_font_Ret),
	f_u_ob_c62_string(Self_Param, C62_string_Ret),
	cl_format(
		  [ Stream_Param,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    C62_string_Ret
		  ],
		  Format_Ret33),
	f_u_begin_regular_font(Stream_Param, Regular_font_Ret34),
	cl_format(
		  [ Stream_Param,
		    '$ARRAY'([*], claz_base_character, [#\(:), #\(' ')])
		  ],
		  Format_Ret35),
	f_u_end_font(Stream_Param, End_font_Ret36),
	f_u_ob_c36_sprint(Self_Param, Stream_Param, C36_sprint_Ret),
	f_u_begin_regular_font(Stream_Param, Regular_font_Ret38),
	cl_format([Stream_Param, '$ARRAY'([*], claz_base_character, [#\(']')])],
		  Format_Ret39),
	f_u_end_font(Stream_Param, End_font_Ret40),
	End_font_Ret40=FnResult.
:- set_opv(f_u_pox, classof, claz_function),
   set_opv(u_pox, compile_as, kw_function),
   set_opv(u_pox, function, f_u_pox),
   DefunResult=u_pox.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7659 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7971)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7659 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Print ob in short form", 1, 7973)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7659 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7998)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:7999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ob-sprint-options*',
			    [quote, ['no-newline', short, parens, 'never-prop']]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_ob_sprint_options_xx,
	   [u_no_newline, u_short, u_parens, u_never_prop]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$sprint',
			    [ob, stream],
			    ['ob$pr', ob, stream, '*ob-sprint-options*']
			  ]).

% annotating U::OB$SPRINT 
wl: lambda_def(defun,
	      u_ob_c36_sprint,
	      f_u_ob_c36_sprint,
	      [u_ob, stream],
	      [[u_ob_c36_pr, u_ob, stream, u_xx_ob_sprint_options_xx]]).


% annotating U::OB$SPRINT 
wl: arglist_info(u_ob_c36_sprint,
		[u_ob, stream],
		[Ob_Param, Stream_Param],
		arginfo{ all:[u_ob, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream],
			 opt:0,
			 req:[u_ob, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SPRINT 
wl: init_args(exact_only, u_ob_c36_sprint).


% annotating U::OB$SPRINT 
f_u_ob_c36_sprint(Ob_Param, Stream_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param)],
	get_var(Env, u_xx_ob_sprint_options_xx, Xx_ob_sprint_options_xx_Get),
	f_u_ob_c36_pr(Ob_Param,
		      Stream_Param,
		      Xx_ob_sprint_options_xx_Get,
		      C36_pr_Ret),
	C36_pr_Ret=FnResult.
:- set_opv(f_u_ob_c36_sprint, classof, claz_function),
   set_opv(u_ob_c36_sprint, compile_as, kw_function),
   set_opv(u_ob_c36_sprint, function, f_u_ob_c36_sprint),
   DefunResult=u_ob_c36_sprint.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Print ob with specified options",
				     1,
				     8142)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8176)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8177 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$pr',
			    [ob, stream, 'print-options'],
			    
			    [ if,
			      ['memq?', [quote, fast], 'print-options'],
			      ['ob$fprint', ob, stream],
			      
			      [ let,
				[[old, '*ob-print-options*']],
				
				[ 'unwind-protect',
				  
				  [ progn,
				    
				    [ if,
				      ['neq?', 'print-options', [quote, default]],
				      
				      [ setq,
					'*ob-print-options*',
					'print-options'
				      ]
				    ],
				    ['ob$print1', ob, stream, 0],
				    '*repl-wont-print*'
				  ],
				  [setq, '*ob-print-options*', old]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PR 
wl: lambda_def(defun,
	      u_ob_c36_pr,
	      f_u_ob_c36_pr,
	      [u_ob, stream, u_print_options],
	      
	      [ 
		[ if,
		  [u_memq_c63, [quote, u_fast], u_print_options],
		  [u_ob_c36_fprint, u_ob, stream],
		  
		  [ let,
		    [[u_old, u_xx_ob_print_options_xx]],
		    
		    [ unwind_protect,
		      
		      [ progn,
			
			[ if,
			  [u_neq_c63, u_print_options, [quote, u_default]],
			  [setq, u_xx_ob_print_options_xx, u_print_options]
			],
			[u_ob_c36_print1, u_ob, stream, 0],
			u_xx_repl_wont_print_xx
		      ],
		      [setq, u_xx_ob_print_options_xx, u_old]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PR 
wl: arglist_info(u_ob_c36_pr,
		[u_ob, stream, u_print_options],
		[Ob_Param, Stream_Param, Print_options_Param],
		arginfo{ all:[u_ob, stream, u_print_options],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_print_options],
			 opt:0,
			 req:[u_ob, stream, u_print_options],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PR 
wl: init_args(exact_only, u_ob_c36_pr).


% annotating U::OB$PR 
f_u_ob_c36_pr(Ob_Param, Stream_Param, Print_options_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param), bv(u_print_options, Print_options_Param)],
	f_u_memq_c63([quote, u_fast], u_print_options, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_fprint(Ob_Param, Stream_Param, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env, u_xx_ob_print_options_xx, Xx_ob_print_options_xx_Get),
	    LEnv=[[bv(u_old, Xx_ob_print_options_xx_Get)]|Env],
	    cl_unwind_protect(
			      [ progn,
				
				[ if,
				  
				  [ u_neq_c63,
				    u_print_options,
				    [quote, u_default]
				  ],
				  
				  [ setq,
				    u_xx_ob_print_options_xx,
				    u_print_options
				  ]
				],
				[u_ob_c36_print1, u_ob, stream, 0],
				u_xx_repl_wont_print_xx
			      ],
			      [setq, u_xx_ob_print_options_xx, u_old],
			      Unwind_protect_Ret),
	    FnResult=Unwind_protect_Ret
	).
:- set_opv(f_u_ob_c36_pr, classof, claz_function),
   set_opv(u_ob_c36_pr, compile_as, kw_function),
   set_opv(u_ob_c36_pr, function, f_u_ob_c36_pr),
   DefunResult=u_ob_c36_pr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8177 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8584)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8177 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Print ob with current default options",
				     1,
				     8586)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8177 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8626)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8628 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*typeset?*', []]).
:- set_var(TLEnv3, setq, u_xx_typeset_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8651 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ if,
			    '*typeset?*',
			    
			    [ progn,
			      
			      [ setq,
				'*ob-print-options*',
				[quote, [parens, 'never-prop', typeset]]
			      ],
			      
			      [ setq,
				'*ob-sprint-options*',
				
				[ quote,
				  
				  [ 'no-newline',
				    short,
				    parens,
				    'never-prop',
				    typeset
				  ]
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				'*ob-print-options*',
				[quote, [parens, 'never-prop']]
			      ],
			      
			      [ setq,
				'*ob-sprint-options*',
				
				[ quote,
				  ['no-newline', short, parens, 'never-prop']
				]
			      ]
			    ]
			  ]).
:- get_var(TLEnv3, u_xx_typeset_c63_xx, IFTEST),
   (   IFTEST\==[]
   ->  set_var(TLEnv3,
	       setq,
	       u_xx_ob_print_options_xx,
	       [u_parens, u_never_prop, u_typeset]),
       set_var(TLEnv3,
	       setq,
	       u_xx_ob_sprint_options_xx,
	       [u_no_newline, u_short, u_parens, u_never_prop, u_typeset]),
       _Ignored=[u_no_newline, u_short, u_parens, u_never_prop, u_typeset]
   ;   set_var(TLEnv3, setq, u_xx_ob_print_options_xx, [u_parens, u_never_prop]),
       set_var(TLEnv3,
	       setq,
	       u_xx_ob_sprint_options_xx,
	       [u_no_newline, u_short, u_parens, u_never_prop]),
       _Ignored=[u_no_newline, u_short, u_parens, u_never_prop]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8952 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ob-print-options*',
			    [quote, [parens, 'no-newline']]
			  ]).
:- set_var(TLEnv3, setq, u_xx_ob_print_options_xx, [u_parens, u_no_newline]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ob-sprint-options*',
			    [quote, [parens, 'no-newline']]
			  ]).
:- set_var(TLEnv3, setq, u_xx_ob_sprint_options_xx, [u_parens, u_no_newline]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9050)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" never-prop - override normal printing format and never print things in",
				     1,
				     9052)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              propositional form",
				     1,
				     9125)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9159)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9160 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print',
			    [ob, stream],
			    ['ob$print1', ob, stream, 0]
			  ]).

% annotating U::OB$PRINT 
wl: lambda_def(defun,
	      u_ob_c36_print,
	      f_u_ob_c36_print,
	      [u_ob, stream],
	      [[u_ob_c36_print1, u_ob, stream, 0]]).


% annotating U::OB$PRINT 
wl: arglist_info(u_ob_c36_print,
		[u_ob, stream],
		[Ob_Param, Stream_Param],
		arginfo{ all:[u_ob, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream],
			 opt:0,
			 req:[u_ob, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT 
wl: init_args(exact_only, u_ob_c36_print).


% annotating U::OB$PRINT 
f_u_ob_c36_print(Ob_Param, Stream_Param, FnResult) :-
	f_u_ob_c36_print1(Ob_Param, Stream_Param, 0, C36_print1_Ret),
	C36_print1_Ret=FnResult.
:- set_opv(f_u_ob_c36_print, classof, claz_function),
   set_opv(u_ob_c36_print, compile_as, kw_function),
   set_opv(u_ob_c36_print, function, f_u_ob_c36_print),
   DefunResult=u_ob_c36_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9160 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9160 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Fast ob printer (rarely used; the short option is better)",
				     1,
				     9219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9160 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9279)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9280 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$fprint',
			    [ob, stream],
			    
			    [ if,
			      [>, [length, ['ob$names', ob]], 2],
			      
			      [ format,
				stream,
				'$STRING'("~A"),
				['ob->string', ob]
			      ],
			      
			      [ let,
				
				[ [type, ['ob$ty', ob]],
				  [obj, ['ob$get', ob, [quote, obj]]]
				],
				
				[ if,
				  ['ty?', type],
				  
				  [ if,
				    obj,
				    
				    [ progn,
				      
				      [ format,
					stream,
					'$STRING'("(~A obj "),
					['type->string', type]
				      ],
				      ['ob$fprint', obj, stream],
				      [format, stream, '$STRING'(" ..LDOTZ..)")]
				    ],
				    
				    [ format,
				      stream,
				      '$STRING'("(~A ..LDOTA..)"),
				      ['type->string', type]
				    ]
				  ],
				  [format, stream, '$STRING'("()")]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$FPRINT 
wl: lambda_def(defun,
	      u_ob_c36_fprint,
	      f_u_ob_c36_fprint,
	      [u_ob, stream],
	      
	      [ 
		[ if,
		  [>, [length, [u_ob_c36_names, u_ob]], 2],
		  
		  [ format,
		    stream,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    [u_ob_c62_string, u_ob]
		  ],
		  
		  [ let,
		    
		    [ [type, [u_ob_c36_ty, u_ob]],
		      [u_obj, [u_ob_c36_get, u_ob, [quote, u_obj]]]
		    ],
		    
		    [ if,
		      [u_ty_c63, type],
		      
		      [ if,
			u_obj,
			
			[ progn,
			  
			  [ format,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('('),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(j),
				       #\(' ')
				     ]),
			    [u_type_c62_string, type]
			  ],
			  [u_ob_c36_fprint, u_obj, stream],
			  
			  [ format,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\('.'),
				       #\('.'),
				       #\('L'),
				       #\('D'),
				       #\('O'),
				       #\('T'),
				       #\('Z'),
				       #\('.'),
				       #\('.'),
				       #\(')')
				     ])
			  ]
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('('),
				     #\(~),
				     #\('A'),
				     #\(' '),
				     #\('.'),
				     #\('.'),
				     #\('L'),
				     #\('D'),
				     #\('O'),
				     #\('T'),
				     #\('A'),
				     #\('.'),
				     #\('.'),
				     #\(')')
				   ]),
			  [u_type_c62_string, type]
			]
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\('('), #\(')')])
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$FPRINT 
wl: arglist_info(u_ob_c36_fprint,
		[u_ob, stream],
		[Ob_Param, Stream_Param],
		arginfo{ all:[u_ob, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream],
			 opt:0,
			 req:[u_ob, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$FPRINT 
wl: init_args(exact_only, u_ob_c36_fprint).


% annotating U::OB$FPRINT 
f_u_ob_c36_fprint(Ob_Param, Stream_Param, TrueResult41) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param)],
	f_u_ob_c36_names(Ob_Param, Length_Param),
	cl_length(Length_Param, PredArg1Result),
	(   PredArg1Result>2
	->  f_u_ob_c62_string(Ob_Param, C62_string_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			C62_string_Ret
		      ],
		      TrueResult43),
	    TrueResult41=TrueResult43
	;   f_u_ob_c36_ty(u_ob, Type_Init),
	    f_u_ob_c36_get(Ob_Param, u_obj, Obj_Init),
	    LEnv=[[bv(type, Type_Init), bv(u_obj, Obj_Init)]|Env],
	    f_u_ty_c63(type, IFTEST25),
	    (   IFTEST25\==[]
	    ->  get_var(LEnv, u_obj, IFTEST27),
		(   IFTEST27\==[]
		->  get_var(LEnv, type, Type_Get),
		    f_u_type_c62_string(Type_Get, C62_string_Ret49),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('('),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(o),
					   #\(b),
					   #\(j),
					   #\(' ')
					 ]),
				C62_string_Ret49
			      ],
			      Format_Ret),
		    get_var(LEnv, u_obj, Obj_Get33),
		    f_u_ob_c36_fprint(Obj_Get33, Stream_Param, C36_fprint_Ret),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\('.'),
					   #\('.'),
					   #\('L'),
					   #\('D'),
					   #\('O'),
					   #\('T'),
					   #\('Z'),
					   #\('.'),
					   #\('.'),
					   #\(')')
					 ])
			      ],
			      TrueResult),
		    TrueResult41=TrueResult
		;   get_var(LEnv, type, Type_Get37),
		    f_u_type_c62_string(Type_Get37, C62_string_Ret52),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('('),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\('.'),
					   #\('.'),
					   #\('L'),
					   #\('D'),
					   #\('O'),
					   #\('T'),
					   #\('A'),
					   #\('.'),
					   #\('.'),
					   #\(')')
					 ]),
				C62_string_Ret52
			      ],
			      ElseResult),
		    TrueResult41=ElseResult
		)
	    ;   cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\('('), #\(')')])
			  ],
			  ElseResult42),
		TrueResult41=ElseResult42
	    )
	).
:- set_opv(f_u_ob_c36_fprint, classof, claz_function),
   set_opv(u_ob_c36_fprint, compile_as, kw_function),
   set_opv(u_ob_c36_fprint, function, f_u_ob_c36_fprint),
   DefunResult=u_ob_c36_fprint.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9776 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*visited-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_visited_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9802 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print1',
			    [ob, stream, column],
			    
			    [ if,
			      '*typeset?*',
			      
			      [ if,
				['memq?', [quote, short], '*ob-print-options*'],
				[],
				
				[ format,
				  stream,
				  '$STRING'("~%\\end{flushleft}~%\\begin{tabbing}~%")
				]
			      ]
			    ],
			    [setq, '*visited-obs*', []],
			    ['ob$print2', ob, stream, column, t],
			    
			    [ if,
			      '*typeset?*',
			      
			      [ if,
				['memq?', [quote, short], '*ob-print-options*'],
				[],
				
				[ format,
				  stream,
				  '$STRING'("~%\\end{tabbing}~%\\begin{flushleft}~%")
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PRINT1 
wl: lambda_def(defun,
	      u_ob_c36_print1,
	      f_u_ob_c36_print1,
	      [u_ob, stream, u_column],
	      
	      [ 
		[ if,
		  u_xx_typeset_c63_xx,
		  
		  [ if,
		    [u_memq_c63, [quote, u_short], u_xx_ob_print_options_xx],
		    [],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('%'),
				 #\(\),
				 #\(e),
				 #\(n),
				 #\(d),
				 #\('{'),
				 #\(f),
				 #\(l),
				 #\(u),
				 #\(s),
				 #\(h),
				 #\(l),
				 #\(e),
				 #\(f),
				 #\(t),
				 #\('}'),
				 #\(~),
				 #\('%'),
				 #\(\),
				 #\(b),
				 #\(e),
				 #\(g),
				 #\(i),
				 #\(n),
				 #\('{'),
				 #\(t),
				 #\(a),
				 #\(b),
				 #\(b),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\('}'),
				 #\(~),
				 #\('%')
			       ])
		    ]
		  ]
		],
		[setq, u_xx_visited_obs_xx, []],
		[u_ob_c36_print2, u_ob, stream, u_column, t],
		
		[ if,
		  u_xx_typeset_c63_xx,
		  
		  [ if,
		    [u_memq_c63, [quote, u_short], u_xx_ob_print_options_xx],
		    [],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('%'),
				 #\(\),
				 #\(e),
				 #\(n),
				 #\(d),
				 #\('{'),
				 #\(t),
				 #\(a),
				 #\(b),
				 #\(b),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\('}'),
				 #\(~),
				 #\('%'),
				 #\(\),
				 #\(b),
				 #\(e),
				 #\(g),
				 #\(i),
				 #\(n),
				 #\('{'),
				 #\(f),
				 #\(l),
				 #\(u),
				 #\(s),
				 #\(h),
				 #\(l),
				 #\(e),
				 #\(f),
				 #\(t),
				 #\('}'),
				 #\(~),
				 #\('%')
			       ])
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PRINT1 
wl: arglist_info(u_ob_c36_print1,
		[u_ob, stream, u_column],
		[Ob_Param, Stream_Param, Column_Param],
		arginfo{ all:[u_ob, stream, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_column],
			 opt:0,
			 req:[u_ob, stream, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT1 
wl: init_args(exact_only, u_ob_c36_print1).


% annotating U::OB$PRINT1 
f_u_ob_c36_print1(Ob_Param, Stream_Param, Column_Param, TrueResult34) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param), bv(u_column, Column_Param)],
	get_var(Env, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_memq_c63([quote, u_short], u_xx_ob_print_options_xx, IFTEST19),
	    (   IFTEST19\==[]
	    ->  TrueResult=[]
	    ;   cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('%'),
				       #\(\),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\('{'),
				       #\(f),
				       #\(l),
				       #\(u),
				       #\(s),
				       #\(h),
				       #\(l),
				       #\(e),
				       #\(f),
				       #\(t),
				       #\('}'),
				       #\(~),
				       #\('%'),
				       #\(\),
				       #\(b),
				       #\(e),
				       #\(g),
				       #\(i),
				       #\(n),
				       #\('{'),
				       #\(t),
				       #\(a),
				       #\(b),
				       #\(b),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\('}'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  ElseResult),
		TrueResult=ElseResult
	    )
	;   TrueResult=[]
	),
	set_var(Env, setq, u_xx_visited_obs_xx, []),
	f_u_ob_c36_print2(Ob_Param, Stream_Param, Column_Param, t, T),
	get_var(Env, u_xx_typeset_c63_xx, IFTEST27),
	(   IFTEST27\==[]
	->  f_u_memq_c63([quote, u_short], u_xx_ob_print_options_xx, IFTEST30),
	    (   IFTEST30\==[]
	    ->  TrueResult34=[]
	    ;   cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('%'),
				       #\(\),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\('{'),
				       #\(t),
				       #\(a),
				       #\(b),
				       #\(b),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\('}'),
				       #\(~),
				       #\('%'),
				       #\(\),
				       #\(b),
				       #\(e),
				       #\(g),
				       #\(i),
				       #\(n),
				       #\('{'),
				       #\(f),
				       #\(l),
				       #\(u),
				       #\(s),
				       #\(h),
				       #\(l),
				       #\(e),
				       #\(f),
				       #\(t),
				       #\('}'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  ElseResult33),
		TrueResult34=ElseResult33
	    )
	;   TrueResult34=[]
	).
:- set_opv(f_u_ob_c36_print1, classof, claz_function),
   set_opv(u_ob_c36_print1, compile_as, kw_function),
   set_opv(u_ob_c36_print1, function, f_u_ob_c36_print1),
   DefunResult=u_ob_c36_print1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9802 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (format stream \"{\\\\small\")",
				     15,
				     9914)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9802 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" took out 'small' above because I don't think it works...",
				     3,
				     10014)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:9802 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (format stream \"}\")", 15, 10208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10300 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print2',
			    [ob, stream, column, 'top?'],
			    
			    [ cond,
			      
			      [ ['var?', ob],
				['print-variable-item', stream, ob, column]
			      ],
			      
			      [ ['memq?', ob, '*visited-obs*'],
				
				[ let,
				  
				  [ 
				    [ str,
				      
				      [ format,
					[],
					'$STRING'("^~A"),
					['ob$name', ob]
				      ]
				    ]
				  ],
				  [princ, str, stream],
				  [setq, column, [+, [length, str], column]]
				]
			      ],
			      [else, ['ob$print4', ob, stream, column, 'top?']]
			    ]
			  ]).

% annotating U::OB$PRINT2 
wl: lambda_def(defun,
	      u_ob_c36_print2,
	      f_u_ob_c36_print2,
	      [u_ob, stream, u_column, u_top_c63],
	      
	      [ 
		[ cond,
		  
		  [ [u_var_c63, u_ob],
		    [u_print_variable_item, stream, u_ob, u_column]
		  ],
		  
		  [ [u_memq_c63, u_ob, u_xx_visited_obs_xx],
		    
		    [ let,
		      
		      [ 
			[ u_str,
			  
			  [ format,
			    [],
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(^), #\(~), #\('A')]),
			    [u_ob_c36_name, u_ob]
			  ]
			]
		      ],
		      [princ, u_str, stream],
		      [setq, u_column, [+, [length, u_str], u_column]]
		    ]
		  ],
		  [u_else, [u_ob_c36_print4, u_ob, stream, u_column, u_top_c63]]
		]
	      ]).


% annotating U::OB$PRINT2 
wl: arglist_info(u_ob_c36_print2,
		[u_ob, stream, u_column, u_top_c63],
		[Ob_Param, Stream_Param, Column_Param, Top_c63_Param],
		arginfo{ all:[u_ob, stream, u_column, u_top_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_column, u_top_c63],
			 opt:0,
			 req:[u_ob, stream, u_column, u_top_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT2 
wl: init_args(exact_only, u_ob_c36_print2).


% annotating U::OB$PRINT2 
f_u_ob_c36_print2(Ob_Param, Stream_Param, Column_Param, Top_c63_Param, ElseResult44) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param), bv(u_column, Column_Param), bv(u_top_c63, Top_c63_Param)],
	f_u_var_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_print_variable_item(Stream_Param,
				    Ob_Param,
				    Column_Param,
				    TrueResult45),
	    ElseResult44=TrueResult45
	;   f_u_memq_c63(u_ob, u_xx_visited_obs_xx, IFTEST23),
	    (   IFTEST23\==[]
	    ->  f_u_ob_c36_name(Ob_Param, C36_name_Ret),
		cl_format(
			  [ [],
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(^), #\(~), #\('A')]),
			    C36_name_Ret
			  ],
			  Str_Init),
		LEnv=[[bv(u_str, Str_Init)]|Env],
		get_var(LEnv, u_str, Str_Get),
		cl_princ(Str_Get, Stream_Param, Princ_Ret),
		get_var(LEnv, u_str, Str_Get32),
		cl_length(Str_Get32, Length_Ret),
		get_var(LEnv, u_column, Column_Get33),
		+(Length_Ret, Column_Get33, Column),
		set_var(LEnv, u_column, Column),
		ElseResult44=Column
	    ;   get_var(Env, u_else, IFTEST34),
		(   IFTEST34\==[]
		->  get_var(Env, u_column, Column_Get39),
		    f_u_ob_c36_print4(Ob_Param,
				      Stream_Param,
				      Column_Get39,
				      Top_c63_Param,
				      TrueResult),
		    ElseResult44=TrueResult
		;   ElseResult44=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_print2, classof, claz_function),
   set_opv(u_ob_c36_print2, compile_as, kw_function),
   set_opv(u_ob_c36_print2, function, f_u_ob_c36_print2),
   DefunResult=u_ob_c36_print2.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10614 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print3',
			    [ob, stream, column, 'top?'],
			    
			    [ progn,
			      
			      [ let,
				
				[ 
				  [ str,
				    
				    [ format,
				      [],
				      '$STRING'("#{~A: "),
				      ['ob$name', ob]
				    ]
				  ]
				],
				[princ, str, stream],
				[setq, column, [+, [length, str], column]]
			      ],
			      ['ob$print4', ob, stream, column, 'top?'],
			      [format, stream, '$STRING'("}")],
			      [setq, column, ['1+', column]]
			    ]
			  ]).

% annotating U::OB$PRINT3 
wl: lambda_def(defun,
	      u_ob_c36_print3,
	      f_u_ob_c36_print3,
	      [u_ob, stream, u_column, u_top_c63],
	      
	      [ 
		[ progn,
		  
		  [ let,
		    
		    [ 
		      [ u_str,
			
			[ format,
			  [],
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(#), #\('{'), #\(~), #\('A'), #\(:), #\(' ')]),
			  [u_ob_c36_name, u_ob]
			]
		      ]
		    ],
		    [princ, u_str, stream],
		    [setq, u_column, [+, [length, u_str], u_column]]
		  ],
		  [u_ob_c36_print4, u_ob, stream, u_column, u_top_c63],
		  [format, stream, '$ARRAY'([*], claz_base_character, [#\('}')])],
		  [setq, u_column, ['1+', u_column]]
		]
	      ]).


% annotating U::OB$PRINT3 
wl: arglist_info(u_ob_c36_print3,
		[u_ob, stream, u_column, u_top_c63],
		[Ob_Param, Stream_Param, Column_Param, Top_c63_Param],
		arginfo{ all:[u_ob, stream, u_column, u_top_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_column, u_top_c63],
			 opt:0,
			 req:[u_ob, stream, u_column, u_top_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT3 
wl: init_args(exact_only, u_ob_c36_print3).


% annotating U::OB$PRINT3 
f_u_ob_c36_print3(Ob_Param, Stream_Param, Column_Param, Top_c63_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param), bv(u_column, Column_Param), bv(u_top_c63, Top_c63_Param)],
	f_u_ob_c36_name(Ob_Param, C36_name_Ret),
	cl_format(
		  [ [],
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(#), #\('{'), #\(~), #\('A'), #\(:), #\(' ')]),
		    C36_name_Ret
		  ],
		  Str_Init),
	LEnv=[[bv(u_str, Str_Init)]|Env],
	get_var(LEnv, u_str, Str_Get),
	cl_princ(Str_Get, Stream_Param, Princ_Ret),
	get_var(LEnv, u_str, Str_Get25),
	cl_length(Str_Get25, Length_Ret),
	get_var(LEnv, u_column, Column_Get),
	+(Length_Ret, Column_Get, Column),
	set_var(LEnv, u_column, Column),
	LetResult=Column,
	get_var(Env, u_column, Column_Get29),
	f_u_ob_c36_print4(Ob_Param,
			  Stream_Param,
			  Column_Get29,
			  Top_c63_Param,
			  C36_print4_Ret),
	cl_format([Stream_Param, '$ARRAY'([*], claz_base_character, [#\('}')])],
		  Format_Ret),
	get_var(Env, u_column, Column_Get32),
	'1+'(Column_Get32, Column36),
	set_var(Env, u_column, Column36),
	Column36=FnResult.
:- set_opv(f_u_ob_c36_print3, classof, claz_function),
   set_opv(u_ob_c36_print3, compile_as, kw_function),
   set_opv(u_ob_c36_print3, function, f_u_ob_c36_print3),
   DefunResult=u_ob_c36_print3.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print4',
			    [ob, stream, column, 'top?'],
			    [setq, '*visited-obs*', [cons, ob, '*visited-obs*']],
			    
			    [ 'let*',
			      
			      [ 
				[ head,
				  
				  [ or,
				    ['ob$get', ob, [quote, head]],
				    ['ob$get', ob, [quote, type]]
				  ]
				],
				['head-string', []],
				['slot-column', []],
				['first?', t],
				[ppformat, []]
			      ],
			      
			      [ if,
				['memq?', [quote, parens], '*ob-print-options*'],
				
				[ progn,
				  ['begin-regular-font', stream],
				  [format, stream, '$STRING'("(")],
				  ['end-font', stream],
				  [setq, column, [+, 1, column]]
				]
			      ],
			      
			      [ if,
				head,
				
				[ progn,
				  
				  [ setq,
				    'head-string',
				    
				    [ cond,
				      
				      [ ['ob?', head],
					['symbol->string', ['ob$name', head]]
				      ],
				      
				      [ ['symbol?', head],
					['symbol->string', head]
				      ],
				      [else, '$STRING'("??????")]
				    ]
				  ],
				  ['begin-head-font', stream],
				  
				  [ format,
				    stream,
				    '$STRING'("~A"),
				    'head-string'
				  ],
				  ['end-font', stream],
				  
				  [ setq,
				    'slot-column',
				    
				    [ (+),
				      1,
				      column,
				      ['string-length', 'head-string']
				    ]
				  ],
				  [setq, column, 'slot-column']
				],
				
				[ progn,
				  [setq, head, '*unknown-ob*'],
				  [setq, 'slot-column', column]
				]
			      ],
			      
			      [ setq,
				ppformat,
				
				[ if,
				  ['ty?', head],
				  ['ob$get', head, [quote, ppformat]],
				  ['ob$get', '*unknown-ob*', [quote, ppformat]]
				]
			      ],
			      
			      [ if,
				['null?', ppformat],
				
				[ error,
				  '$STRING'("ob$print: null ppformat for type ~A ob ~A"),
				  head,
				  ['ob->string', ob]
				]
			      ],
			      
			      [ yloop,
				
				[ initial,
				  
				  [ 'short?',
				    
				    [ 'memq?',
				      [quote, short],
				      '*ob-print-options*'
				    ]
				  ],
				  ['slot-values', []],
				  ['slot-name-str', []],
				  ['separate-lines?', []],
				  ['emb-ob?', []],
				  ['slot-value-column', []],
				  ['not-first?', []],
				  ['previous-multiple-and-no-newline?', []],
				  ['previous-embedded-ob?', []]
				],
				[yfor, 'slot-name', in, [cadr, ppformat]],
				
				[ yuntil,
				  
				  [ and,
				    'short?',
				    [>, column, '*ob-short-length*'],
				    [progn, ['do-ldots', stream], t]
				  ]
				],
				
				[ ydo,
				  
				  [ if,
				    
				    [ not,
				      ['memq?', 'slot-name', [caddr, ppformat]]
				    ],
				    
				    [ progn,
				      
				      [ setq,
					'slot-values',
					
					[ if,
					  
					  [ 'memq?',
					    [quote, 'omit-links'],
					    '*ob-print-options*'
					  ],
					  
					  [ 'ob$gets-omit-links',
					    ob,
					    'slot-name'
					  ],
					  ['ob$gets', ob, 'slot-name']
					]
				      ],
				      
				      [ if,
					'slot-values',
					
					[ progn,
					  
					  [ if,
					    [and, head, 'first?'],
					    
					    [ progn,
					      [setq, 'first?', []],
					      
					      [ if,
						'*typeset?*',
						
						[ format,
						  stream,
						  '$STRING'("\\ ")
						],
						[format, stream, '$STRING'(" ")]
					      ],
					      
					      [ if,
						
						[ and,
						  '*typeset?*',
						  
						  [ not,
						    
						    [ 'memq?',
						      [quote, 'no-newline'],
						      '*ob-print-options*'
						    ]
						  ]
						],
						
						[ format,
						  stream,
						  '$STRING'("\\=\\+")
						]
					      ]
					    ]
					  ],
					  
					  [ if,
					    'previous-embedded-ob?',
					    
					    [ if,
					      
					      [ 'memq?',
						[quote, 'no-newline'],
						'*ob-print-options*'
					      ],
					      [progn, []],
					      
					      [ progn,
						['do-newline', stream],
						
						[ if,
						  [not, '*typeset?*'],
						  
						  [ 'print-spaces',
						    stream,
						    'slot-column'
						  ]
						],
						[setq, column, 'slot-column']
					      ]
					    ]
					  ],
					  
					  [ setq,
					    'emb-ob?',
					    
					    [ 'any?',
					      [lambda, [x], ['embedded-ob?', x]],
					      'slot-values'
					    ]
					  ],
					  
					  [ setq,
					    'separate-lines?',
					    
					    [ and,
					      [cdr, 'slot-values'],
					      'emb-ob?'
					    ]
					  ],
					  
					  [ if,
					    
					    [ and,
					      
					      [ not,
						
						[ 'memq?',
						  [quote, 'no-newline'],
						  '*ob-print-options*'
						]
					      ],
					      
					      [ or,
						
						[ and,
						  [>, column, 'slot-column'],
						  [>, column, '*print-length*']
						],
						
						[ and,
						  'emb-ob?',
						  [>, column, 'slot-column'],
						  
						  [ not,
						    
						    [ 'memq?',
						      
						      [ quote,
							'no-embedded-ob-newline'
						      ],
						      '*ob-print-options*'
						    ]
						  ]
						]
					      ]
					    ],
					    
					    [ progn,
					      ['do-newline', stream],
					      
					      [ if,
						[not, '*typeset?*'],
						
						[ 'print-spaces',
						  stream,
						  'slot-column'
						]
					      ],
					      [setq, column, 'slot-column']
					    ],
					    
					    [ if,
					      [>, column, 'slot-column'],
					      
					      [ progn,
						
						[ if,
						  '*typeset?*',
						  
						  [ format,
						    stream,
						    '$STRING'("\\ ")
						  ],
						  
						  [ format,
						    stream,
						    '$STRING'(" ")
						  ]
						],
						[setq, column, [+, 1, column]]
					      ]
					    ]
					  ],
					  
					  [ if,
					    
					    [ or,
					      
					      [ 'neq?',
						[car, ppformat],
						[quote, prop]
					      ],
					      
					      [ 'memq?',
						[quote, 'never-prop'],
						'*ob-print-options*'
					      ],
					      [setq, column, [+, 1, column]]
					    ],
					    
					    [ progn,
					      ['do-newline', stream],
					      
					      [ if,
						[not, '*typeset?*'],
						
						[ 'print-spaces',
						  stream,
						  'slot-column'
						]
					      ],
					      [setq, column, 'slot-column']
					    ]
					  ]
					],
					
					[ setq,
					  'emb-ob?',
					  
					  [ 'any?',
					    [lambda, [x], ['embedded-ob?', x]],
					    'slot-values'
					  ]
					],
					
					[ setq,
					  'separate-lines?',
					  [and, [cdr, 'slot-values'], 'emb-ob?']
					],
					
					[ if,
					  
					  [ and,
					    
					    [ not,
					      
					      [ 'memq?',
						[quote, 'no-newline'],
						'*ob-print-options*'
					      ]
					    ],
					    
					    [ or,
					      
					      [ and,
						[>, column, 'slot-column'],
						[>, column, '*print-length*']
					      ],
					      
					      [ and,
						'emb-ob?',
						[>, column, 'slot-column'],
						
						[ not,
						  
						  [ 'memq?',
						    
						    [ quote,
						      'no-embedded-ob-newline'
						    ],
						    '*ob-print-options*'
						  ]
						]
					      ]
					    ]
					  ],
					  
					  [ progn,
					    ['do-newline', stream],
					    
					    [ if,
					      [not, '*typeset?*'],
					      
					      [ 'print-spaces',
						stream,
						'slot-column'
					      ]
					    ],
					    [setq, column, 'slot-column']
					  ],
					  
					  [ if,
					    [>, column, 'slot-column'],
					    
					    [ progn,
					      
					      [ if,
						'*typeset?*',
						
						[ format,
						  stream,
						  '$STRING'("\\ ")
						],
						[format, stream, '$STRING'(" ")]
					      ],
					      [setq, column, [+, 1, column]]
					    ]
					  ]
					],
					
					[ if,
					  
					  [ or,
					    
					    [ 'neq?',
					      [car, ppformat],
					      [quote, prop]
					    ],
					    
					    [ 'memq?',
					      [quote, 'never-prop'],
					      '*ob-print-options*'
					    ],
					    
					    [ 'memq?',
					      'slot-name',
					      [caddr, ppformat]
					    ],
					    'previous-multiple-and-no-newline?'
					  ],
					  
					  [ progn,
					    ['begin-slot-name-font', stream],
					    
					    [ setq,
					      'slot-name-str',
					      
					      [ 'string-downcase!',
						['symbol->string', 'slot-name']
					      ]
					    ],
					    
					    [ format,
					      stream,
					      '$STRING'("~A "),
					      'slot-name-str'
					    ],
					    ['end-font', stream],
					    
					    [ setq,
					      column,
					      
					      [ (+),
						column,
						1,
						
						[ 'string-length',
						  'slot-name-str'
						]
					      ]
					    ]
					  ]
					],
					[setq, 'slot-value-column', column],
					
					[ if,
					  
					  [ and,
					    '*typeset?*',
					    
					    [ not,
					      
					      [ 'memq?',
						[quote, 'no-newline'],
						'*ob-print-options*'
					      ]
					    ]
					  ],
					  [format, stream, '$STRING'("\\=\\+")]
					],
					[setq, 'not-first?', []],
					
					[ yloop,
					  [initial, [rest, 'slot-values']],
					  [ywhile, rest],
					  
					  [ yuntil,
					    
					    [ and,
					      'short?',
					      [>, column, '*ob-short-length*'],
					      [progn, ['do-ldots', stream], t]
					    ]
					  ],
					  
					  [ ydo,
					    
					    [ if,
					      'not-first?',
					      
					      [ progn,
						
						[ if,
						  '*typeset?*',
						  
						  [ format,
						    stream,
						    '$STRING'("\\ ")
						  ],
						  
						  [ format,
						    stream,
						    '$STRING'(" ")
						  ]
						],
						[setq, column, [+, 1, column]]
					      ]
					    ],
					    [setq, 'not-first?', t],
					    
					    [ setq,
					      column,
					      
					      [ 'print-item',
						stream,
						[car, rest],
						column
					      ]
					    ],
					    
					    [ if,
					      
					      [ and,
						[cdr, rest],
						'separate-lines?'
					      ],
					      
					      [ progn,
						
						[ if,
						  
						  [ 'memq?',
						    [quote, 'no-newline'],
						    '*ob-print-options*'
						  ],
						  
						  [ progn,
						    
						    [ format,
						      stream,
						      '$STRING'(" ")
						    ],
						    [setq, column, [+, 1, column]]
						  ],
						  
						  [ progn,
						    ['do-newline', stream],
						    
						    [ if,
						      [not, '*typeset?*'],
						      
						      [ 'print-spaces',
							stream,
							'slot-value-column'
						      ]
						    ],
						    
						    [ setq,
						      column,
						      'slot-value-column'
						    ]
						  ]
						],
						[setq, 'not-first?', []]
					      ]
					    ],
					    [setq, rest, [cdr, rest]]
					  ]
					],
					
					[ if,
					  
					  [ and,
					    '*typeset?*',
					    
					    [ not,
					      
					      [ 'memq?',
						[quote, 'no-newline'],
						'*ob-print-options*'
					      ]
					    ]
					  ],
					  [format, stream, '$STRING'("\\-")]
					],
					[setq, 'previous-embedded-ob?', t],
					
					[ setq,
					  'previous-multiple-and-no-newline?',
					  
					  [ and,
					    [cdr, 'slot-values'],
					    [not, 'separate-lines?']
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ if,
			      ['memq?', [quote, parens], '*ob-print-options*'],
			      
			      [ progn,
				['begin-regular-font', stream],
				[format, stream, '$STRING'(")")],
				['end-font', stream]
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				'*typeset?*',
				[not, 'top?'],
				'It',
				looks,
				bad,
				to,
				have,
				multiple,
				slots,
				per,
				'line.',
				
				[ setq,
				  'previous-multiple-and-no-newline?',
				  
				  [ and,
				    [cdr, 'slot-values'],
				    [not, 'separate-lines?']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PRINT4 
wl: lambda_def(defun,
	      u_ob_c36_print4,
	      f_u_ob_c36_print4,
	      [u_ob, stream, u_column, u_top_c63],
	      
	      [ [setq, u_xx_visited_obs_xx, [cons, u_ob, u_xx_visited_obs_xx]],
		
		[ let_xx,
		  
		  [ 
		    [ u_head,
		      
		      [ or,
			[u_ob_c36_get, u_ob, [quote, u_head]],
			[u_ob_c36_get, u_ob, [quote, type]]
		      ]
		    ],
		    [u_head_string, []],
		    [u_slot_column, []],
		    [u_first_c63, t],
		    [u_ppformat, []]
		  ],
		  
		  [ if,
		    [u_memq_c63, [quote, u_parens], u_xx_ob_print_options_xx],
		    
		    [ progn,
		      [u_begin_regular_font, stream],
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\('(')])
		      ],
		      [u_end_font, stream],
		      [setq, u_column, [+, 1, u_column]]
		    ]
		  ],
		  
		  [ if,
		    u_head,
		    
		    [ progn,
		      
		      [ setq,
			u_head_string,
			
			[ cond,
			  
			  [ [u_ob_c63, u_head],
			    [u_symbol_c62_string, [u_ob_c36_name, u_head]]
			  ],
			  [[u_symbol_c63, u_head], [u_symbol_c62_string, u_head]],
			  
			  [ u_else,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(?), #\(?), #\(?), #\(?), #\(?), #\(?)])
			  ]
			]
		      ],
		      [u_begin_head_font, stream],
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			u_head_string
		      ],
		      [u_end_font, stream],
		      
		      [ setq,
			u_slot_column,
			[+, 1, u_column, [u_string_length, u_head_string]]
		      ],
		      [setq, u_column, u_slot_column]
		    ],
		    
		    [ progn,
		      [setq, u_head, u_xx_unknown_ob_xx],
		      [setq, u_slot_column, u_column]
		    ]
		  ],
		  
		  [ setq,
		    u_ppformat,
		    
		    [ if,
		      [u_ty_c63, u_head],
		      [u_ob_c36_get, u_head, [quote, u_ppformat]],
		      [u_ob_c36_get, u_xx_unknown_ob_xx, [quote, u_ppformat]]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_ppformat],
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(o),
				 #\(b),
				 #\($),
				 #\(p),
				 #\(r),
				 #\(i),
				 #\(n),
				 #\(t),
				 #\(:),
				 #\(' '),
				 #\(n),
				 #\(u),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(p),
				 #\(p),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(m),
				 #\(a),
				 #\(t),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(' '),
				 #\(t),
				 #\(y),
				 #\(p),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(o),
				 #\(b),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_head,
		      [u_ob_c62_string, u_ob]
		    ]
		  ],
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      
		      [ u_short_c63,
			[u_memq_c63, [quote, u_short], u_xx_ob_print_options_xx]
		      ],
		      [u_slot_values, []],
		      [u_slot_name_str, []],
		      [u_separate_lines_c63, []],
		      [u_emb_ob_c63, []],
		      [u_slot_value_column, []],
		      [u_not_first_c63, []],
		      [u_previous_multiple_and_no_newline_c63, []],
		      [u_previous_embedded_ob_c63, []]
		    ],
		    [u_yfor, u_slot_name, u_in, [cadr, u_ppformat]],
		    
		    [ u_yuntil,
		      
		      [ and,
			u_short_c63,
			[>, u_column, u_xx_ob_short_length_xx],
			[progn, [u_do_ldots, stream], t]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_memq_c63, u_slot_name, [caddr, u_ppformat]]],
			
			[ progn,
			  
			  [ setq,
			    u_slot_values,
			    
			    [ if,
			      
			      [ u_memq_c63,
				[quote, u_omit_links],
				u_xx_ob_print_options_xx
			      ],
			      [u_ob_c36_gets_omit_links, u_ob, u_slot_name],
			      [u_ob_c36_gets, u_ob, u_slot_name]
			    ]
			  ],
			  
			  [ if,
			    u_slot_values,
			    
			    [ progn,
			      
			      [ if,
				[and, u_head, u_first_c63],
				
				[ progn,
				  [setq, u_first_c63, []],
				  
				  [ if,
				    u_xx_typeset_c63_xx,
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(' ')])
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(' ')])
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      u_xx_typeset_c63_xx,
				      
				      [ not,
					
					[ u_memq_c63,
					  [quote, u_no_newline],
					  u_xx_ob_print_options_xx
					]
				      ]
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(=), #\(\), #\(+)])
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				u_previous_embedded_ob_c63,
				
				[ if,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ],
				  [progn, []],
				  
				  [ progn,
				    [u_do_newline, stream],
				    
				    [ if,
				      [not, u_xx_typeset_c63_xx],
				      [u_print_spaces, stream, u_slot_column]
				    ],
				    [setq, u_column, u_slot_column]
				  ]
				]
			      ],
			      
			      [ setq,
				u_emb_ob_c63,
				
				[ u_any_c63,
				  [lambda, [u_x], [u_embedded_ob_c63, u_x]],
				  u_slot_values
				]
			      ],
			      
			      [ setq,
				u_separate_lines_c63,
				[and, [cdr, u_slot_values], u_emb_ob_c63]
			      ],
			      
			      [ if,
				
				[ and,
				  
				  [ not,
				    
				    [ u_memq_c63,
				      [quote, u_no_newline],
				      u_xx_ob_print_options_xx
				    ]
				  ],
				  
				  [ or,
				    
				    [ and,
				      [>, u_column, u_slot_column],
				      [>, u_column, xx_print_length_xx]
				    ],
				    
				    [ and,
				      u_emb_ob_c63,
				      [>, u_column, u_slot_column],
				      
				      [ not,
					
					[ u_memq_c63,
					  [quote, u_no_embedded_ob_newline],
					  u_xx_ob_print_options_xx
					]
				      ]
				    ]
				  ]
				],
				
				[ progn,
				  [u_do_newline, stream],
				  
				  [ if,
				    [not, u_xx_typeset_c63_xx],
				    [u_print_spaces, stream, u_slot_column]
				  ],
				  [setq, u_column, u_slot_column]
				],
				
				[ if,
				  [>, u_column, u_slot_column],
				  
				  [ progn,
				    
				    [ if,
				      u_xx_typeset_c63_xx,
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(\), #\(' ')])
				      ],
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(' ')])
				      ]
				    ],
				    [setq, u_column, [+, 1, u_column]]
				  ]
				]
			      ],
			      
			      [ if,
				
				[ or,
				  [u_neq_c63, [car, u_ppformat], [quote, u_prop]],
				  
				  [ u_memq_c63,
				    [quote, u_never_prop],
				    u_xx_ob_print_options_xx
				  ],
				  [setq, u_column, [+, 1, u_column]]
				],
				
				[ progn,
				  [u_do_newline, stream],
				  
				  [ if,
				    [not, u_xx_typeset_c63_xx],
				    [u_print_spaces, stream, u_slot_column]
				  ],
				  [setq, u_column, u_slot_column]
				]
			      ]
			    ],
			    
			    [ setq,
			      u_emb_ob_c63,
			      
			      [ u_any_c63,
				[lambda, [u_x], [u_embedded_ob_c63, u_x]],
				u_slot_values
			      ]
			    ],
			    
			    [ setq,
			      u_separate_lines_c63,
			      [and, [cdr, u_slot_values], u_emb_ob_c63]
			    ],
			    
			    [ if,
			      
			      [ and,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				],
				
				[ or,
				  
				  [ and,
				    [>, u_column, u_slot_column],
				    [>, u_column, xx_print_length_xx]
				  ],
				  
				  [ and,
				    u_emb_ob_c63,
				    [>, u_column, u_slot_column],
				    
				    [ not,
				      
				      [ u_memq_c63,
					[quote, u_no_embedded_ob_newline],
					u_xx_ob_print_options_xx
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ progn,
				[u_do_newline, stream],
				
				[ if,
				  [not, u_xx_typeset_c63_xx],
				  [u_print_spaces, stream, u_slot_column]
				],
				[setq, u_column, u_slot_column]
			      ],
			      
			      [ if,
				[>, u_column, u_slot_column],
				
				[ progn,
				  
				  [ if,
				    u_xx_typeset_c63_xx,
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(' ')])
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(' ')])
				    ]
				  ],
				  [setq, u_column, [+, 1, u_column]]
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ or,
				[u_neq_c63, [car, u_ppformat], [quote, u_prop]],
				
				[ u_memq_c63,
				  [quote, u_never_prop],
				  u_xx_ob_print_options_xx
				],
				[u_memq_c63, u_slot_name, [caddr, u_ppformat]],
				u_previous_multiple_and_no_newline_c63
			      ],
			      
			      [ progn,
				[u_begin_slot_name_font, stream],
				
				[ setq,
				  u_slot_name_str,
				  
				  [ u_string_downcase_c33,
				    [u_symbol_c62_string, u_slot_name]
				  ]
				],
				
				[ format,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(~), #\('A'), #\(' ')]),
				  u_slot_name_str
				],
				[u_end_font, stream],
				
				[ setq,
				  u_column,
				  
				  [ (+),
				    u_column,
				    1,
				    [u_string_length, u_slot_name_str]
				  ]
				]
			      ]
			    ],
			    [setq, u_slot_value_column, u_column],
			    
			    [ if,
			      
			      [ and,
				u_xx_typeset_c63_xx,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				]
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(\), #\(=), #\(\), #\(+)])
			      ]
			    ],
			    [setq, u_not_first_c63, []],
			    
			    [ u_yloop,
			      [u_initial, [rest, u_slot_values]],
			      [u_ywhile, rest],
			      
			      [ u_yuntil,
				
				[ and,
				  u_short_c63,
				  [>, u_column, u_xx_ob_short_length_xx],
				  [progn, [u_do_ldots, stream], t]
				]
			      ],
			      
			      [ u_ydo,
				
				[ if,
				  u_not_first_c63,
				  
				  [ progn,
				    
				    [ if,
				      u_xx_typeset_c63_xx,
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(\), #\(' ')])
				      ],
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(' ')])
				      ]
				    ],
				    [setq, u_column, [+, 1, u_column]]
				  ]
				],
				[setq, u_not_first_c63, t],
				
				[ setq,
				  u_column,
				  [u_print_item, stream, [car, rest], u_column]
				],
				
				[ if,
				  [and, [cdr, rest], u_separate_lines_c63],
				  
				  [ progn,
				    
				    [ if,
				      
				      [ u_memq_c63,
					[quote, u_no_newline],
					u_xx_ob_print_options_xx
				      ],
				      
				      [ progn,
					
					[ format,
					  stream,
					  '$ARRAY'([*],
						   claz_base_character,
						   [#\(' ')])
					],
					[setq, u_column, [+, 1, u_column]]
				      ],
				      
				      [ progn,
					[u_do_newline, stream],
					
					[ if,
					  [not, u_xx_typeset_c63_xx],
					  
					  [ u_print_spaces,
					    stream,
					    u_slot_value_column
					  ]
					],
					[setq, u_column, u_slot_value_column]
				      ]
				    ],
				    [setq, u_not_first_c63, []]
				  ]
				],
				[setq, rest, [cdr, rest]]
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				u_xx_typeset_c63_xx,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				]
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(\), #\(-)])
			      ]
			    ],
			    [setq, u_previous_embedded_ob_c63, t],
			    
			    [ setq,
			      u_previous_multiple_and_no_newline_c63,
			      
			      [ and,
				[cdr, u_slot_values],
				[not, u_separate_lines_c63]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		],
		
		[ if,
		  [u_memq_c63, [quote, u_parens], u_xx_ob_print_options_xx],
		  
		  [ progn,
		    [u_begin_regular_font, stream],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*], claz_base_character, [#\(')')])
		    ],
		    [u_end_font, stream]
		  ]
		],
		
		[ if,
		  
		  [ and,
		    u_xx_typeset_c63_xx,
		    [not, u_top_c63],
		    u_it,
		    u_looks,
		    u_bad,
		    u_to,
		    u_have,
		    u_multiple,
		    sys_slots,
		    u_per,
		    u_line_c46,
		    
		    [ setq,
		      u_previous_multiple_and_no_newline_c63,
		      [and, [cdr, u_slot_values], [not, u_separate_lines_c63]]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PRINT4 
wl: arglist_info(u_ob_c36_print4,
		[u_ob, stream, u_column, u_top_c63],
		[Ob_Param, Stream_Param, Column_Param, Top_c63_Param],
		arginfo{ all:[u_ob, stream, u_column, u_top_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_column, u_top_c63],
			 opt:0,
			 req:[u_ob, stream, u_column, u_top_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT4 
wl: init_args(exact_only, u_ob_c36_print4).


% annotating U::OB$PRINT4 
f_u_ob_c36_print4(Ob_Param, Stream_Param, Column_Param, Top_c63_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(stream, Stream_Param), bv(u_column, Column_Param), bv(u_top_c63, Top_c63_Param)],
	get_var(Env, u_xx_visited_obs_xx, Xx_visited_obs_xx_Get),
	Xx_visited_obs_xx=[Ob_Param|Xx_visited_obs_xx_Get],
	set_var(Env, u_xx_visited_obs_xx, Xx_visited_obs_xx),
	(   f_u_ob_c36_get(Ob_Param, u_head, FORM1_Res),
	    FORM1_Res\==[],
	    Head_Init=FORM1_Res
	->  true
	;   f_u_ob_c36_get(Ob_Param, type, Type),
	    Head_Init=Type
	),
	LEnv=[[bv(u_head, Head_Init)]|Env],
	LEnv26=[[bv(u_head_string, [])]|LEnv],
	LEnv28=[[bv(u_slot_column, [])]|LEnv26],
	LEnv30=[[bv(u_first_c63, t)]|LEnv28],
	Env=[[bv(u_ppformat, [])]|LEnv30],
	f_u_memq_c63([quote, u_parens], u_xx_ob_print_options_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_begin_regular_font(Stream_Param, Regular_font_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\('(')])
		      ],
		      Format_Ret),
	    f_u_end_font(Stream_Param, End_font_Ret),
	    get_var(Env, u_column, Column_Get),
	    +(1, Column_Get, TrueResult),
	    set_var(Env, u_column, TrueResult),
	    _149468=TrueResult
	;   _149468=[]
	),
	get_var(Env, u_head, IFTEST42),
	(   IFTEST42\==[]
	->  f_u_ob_c63(u_head, IFTEST45),
	    (   IFTEST45\==[]
	    ->  f_u_symbol_c62_string([u_ob_c36_name, u_head], TrueResult55),
		ElseResult54=TrueResult55
	    ;   f_u_symbol_c63(u_head, IFTEST47),
		(   IFTEST47\==[]
		->  f_u_symbol_c62_string(u_head, TrueResult53),
		    ElseResult54=TrueResult53
		;   get_var(Env, u_else, IFTEST49),
		    (   IFTEST49\==[]
		    ->  ElseResult54='$ARRAY'([*], claz_base_character, [#\(?), #\(?), #\(?), #\(?), #\(?), #\(?)])
		    ;   ElseResult54=[]
		    )
		)
	    ),
	    set_var(Env, u_head_string, ElseResult54),
	    f_u_begin_head_font(Stream_Param, Head_font_Ret),
	    get_var(Env, u_head_string, Head_string_Get),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			Head_string_Get
		      ],
		      Format_Ret98),
	    f_u_end_font(Stream_Param, End_font_Ret99),
	    get_var(Env, u_column, Column_Get61),
	    +(1, Column_Get61, _150338),
	    f_u_string_length(u_head_string, String_length_Ret),
	    +(_150338, String_length_Ret, Slot_column),
	    set_var(Env, u_slot_column, Slot_column),
	    get_var(Env, u_slot_column, Slot_column_Get),
	    set_var(Env, u_column, Slot_column_Get),
	    _149640=Slot_column_Get
	;   get_var(Env, u_xx_unknown_ob_xx, Xx_unknown_ob_xx_Get),
	    set_var(Env, u_head, Xx_unknown_ob_xx_Get),
	    get_var(Env, u_column, Column_Get64),
	    set_var(Env, u_slot_column, Column_Get64),
	    _149640=Column_Get64
	),
	f_u_ty_c63(u_head, IFTEST67),
	(   IFTEST67\==[]
	->  f_u_ob_c36_get(IFTEST42, u_ppformat, TrueResult71),
	    Ppformat=TrueResult71
	;   get_var(Env, u_xx_unknown_ob_xx, Xx_unknown_ob_xx_Get70),
	    f_u_ob_c36_get(Xx_unknown_ob_xx_Get70, u_ppformat, ElseResult72),
	    Ppformat=ElseResult72
	),
	set_var(Env, u_ppformat, Ppformat),
	f_u_null_c63(u_ppformat, IFTEST73),
	(   IFTEST73\==[]
	->  f_u_ob_c62_string(Ob_Param, C62_string_Ret),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(o),
				  #\(b),
				  #\($),
				  #\(p),
				  #\(r),
				  #\(i),
				  #\(n),
				  #\(t),
				  #\(:),
				  #\(' '),
				  #\(n),
				  #\(u),
				  #\(l),
				  #\(l),
				  #\(' '),
				  #\(p),
				  #\(p),
				  #\(f),
				  #\(o),
				  #\(r),
				  #\(m),
				  #\(a),
				  #\(t),
				  #\(' '),
				  #\(f),
				  #\(o),
				  #\(r),
				  #\(' '),
				  #\(t),
				  #\(y),
				  #\(p),
				  #\(e),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\(' '),
				  #\(~),
				  #\('A')
				]),
		       IFTEST42,
		       C62_string_Ret
		     ],
		     TrueResult77),
	    _150814=TrueResult77
	;   _150814=[]
	),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_short_c63,
			[u_memq_c63, [quote, u_short], u_xx_ob_print_options_xx]
		      ],
		      [u_slot_values, []],
		      [u_slot_name_str, []],
		      [u_separate_lines_c63, []],
		      [u_emb_ob_c63, []],
		      [u_slot_value_column, []],
		      [u_not_first_c63, []],
		      [u_previous_multiple_and_no_newline_c63, []],
		      [u_previous_embedded_ob_c63, []]
		    ],
		    [u_yfor, u_slot_name, u_in, [cadr, u_ppformat]],
		    
		    [ u_yuntil,
		      
		      [ and,
			u_short_c63,
			[>, u_column, u_xx_ob_short_length_xx],
			[progn, [u_do_ldots, stream], t]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_memq_c63, u_slot_name, [caddr, u_ppformat]]],
			
			[ progn,
			  
			  [ setq,
			    u_slot_values,
			    
			    [ if,
			      
			      [ u_memq_c63,
				[quote, u_omit_links],
				u_xx_ob_print_options_xx
			      ],
			      [u_ob_c36_gets_omit_links, u_ob, u_slot_name],
			      [u_ob_c36_gets, u_ob, u_slot_name]
			    ]
			  ],
			  
			  [ if,
			    u_slot_values,
			    
			    [ progn,
			      
			      [ if,
				[and, u_head, u_first_c63],
				
				[ progn,
				  [setq, u_first_c63, []],
				  
				  [ if,
				    u_xx_typeset_c63_xx,
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(' ')])
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(' ')])
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      u_xx_typeset_c63_xx,
				      
				      [ not,
					
					[ u_memq_c63,
					  [quote, u_no_newline],
					  u_xx_ob_print_options_xx
					]
				      ]
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(=), #\(\), #\(+)])
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				u_previous_embedded_ob_c63,
				
				[ if,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ],
				  [progn, []],
				  
				  [ progn,
				    [u_do_newline, stream],
				    
				    [ if,
				      [not, u_xx_typeset_c63_xx],
				      [u_print_spaces, stream, u_slot_column]
				    ],
				    [setq, u_column, u_slot_column]
				  ]
				]
			      ],
			      
			      [ setq,
				u_emb_ob_c63,
				
				[ u_any_c63,
				  [lambda, [u_x], [u_embedded_ob_c63, u_x]],
				  u_slot_values
				]
			      ],
			      
			      [ setq,
				u_separate_lines_c63,
				[and, [cdr, u_slot_values], u_emb_ob_c63]
			      ],
			      
			      [ if,
				
				[ and,
				  
				  [ not,
				    
				    [ u_memq_c63,
				      [quote, u_no_newline],
				      u_xx_ob_print_options_xx
				    ]
				  ],
				  
				  [ or,
				    
				    [ and,
				      [>, u_column, u_slot_column],
				      [>, u_column, xx_print_length_xx]
				    ],
				    
				    [ and,
				      u_emb_ob_c63,
				      [>, u_column, u_slot_column],
				      
				      [ not,
					
					[ u_memq_c63,
					  [quote, u_no_embedded_ob_newline],
					  u_xx_ob_print_options_xx
					]
				      ]
				    ]
				  ]
				],
				
				[ progn,
				  [u_do_newline, stream],
				  
				  [ if,
				    [not, u_xx_typeset_c63_xx],
				    [u_print_spaces, stream, u_slot_column]
				  ],
				  [setq, u_column, u_slot_column]
				],
				
				[ if,
				  [>, u_column, u_slot_column],
				  
				  [ progn,
				    
				    [ if,
				      u_xx_typeset_c63_xx,
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(\), #\(' ')])
				      ],
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(' ')])
				      ]
				    ],
				    [setq, u_column, [+, 1, u_column]]
				  ]
				]
			      ],
			      
			      [ if,
				
				[ or,
				  [u_neq_c63, [car, u_ppformat], [quote, u_prop]],
				  
				  [ u_memq_c63,
				    [quote, u_never_prop],
				    u_xx_ob_print_options_xx
				  ],
				  [setq, u_column, [+, 1, u_column]]
				],
				
				[ progn,
				  [u_do_newline, stream],
				  
				  [ if,
				    [not, u_xx_typeset_c63_xx],
				    [u_print_spaces, stream, u_slot_column]
				  ],
				  [setq, u_column, u_slot_column]
				]
			      ]
			    ],
			    
			    [ setq,
			      u_emb_ob_c63,
			      
			      [ u_any_c63,
				[lambda, [u_x], [u_embedded_ob_c63, u_x]],
				u_slot_values
			      ]
			    ],
			    
			    [ setq,
			      u_separate_lines_c63,
			      [and, [cdr, u_slot_values], u_emb_ob_c63]
			    ],
			    
			    [ if,
			      
			      [ and,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				],
				
				[ or,
				  
				  [ and,
				    [>, u_column, u_slot_column],
				    [>, u_column, xx_print_length_xx]
				  ],
				  
				  [ and,
				    u_emb_ob_c63,
				    [>, u_column, u_slot_column],
				    
				    [ not,
				      
				      [ u_memq_c63,
					[quote, u_no_embedded_ob_newline],
					u_xx_ob_print_options_xx
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ progn,
				[u_do_newline, stream],
				
				[ if,
				  [not, u_xx_typeset_c63_xx],
				  [u_print_spaces, stream, u_slot_column]
				],
				[setq, u_column, u_slot_column]
			      ],
			      
			      [ if,
				[>, u_column, u_slot_column],
				
				[ progn,
				  
				  [ if,
				    u_xx_typeset_c63_xx,
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(\), #\(' ')])
				    ],
				    
				    [ format,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       [#\(' ')])
				    ]
				  ],
				  [setq, u_column, [+, 1, u_column]]
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ or,
				[u_neq_c63, [car, u_ppformat], [quote, u_prop]],
				
				[ u_memq_c63,
				  [quote, u_never_prop],
				  u_xx_ob_print_options_xx
				],
				[u_memq_c63, u_slot_name, [caddr, u_ppformat]],
				u_previous_multiple_and_no_newline_c63
			      ],
			      
			      [ progn,
				[u_begin_slot_name_font, stream],
				
				[ setq,
				  u_slot_name_str,
				  
				  [ u_string_downcase_c33,
				    [u_symbol_c62_string, u_slot_name]
				  ]
				],
				
				[ format,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(~), #\('A'), #\(' ')]),
				  u_slot_name_str
				],
				[u_end_font, stream],
				
				[ setq,
				  u_column,
				  
				  [ (+),
				    u_column,
				    1,
				    [u_string_length, u_slot_name_str]
				  ]
				]
			      ]
			    ],
			    [setq, u_slot_value_column, u_column],
			    
			    [ if,
			      
			      [ and,
				u_xx_typeset_c63_xx,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				]
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(\), #\(=), #\(\), #\(+)])
			      ]
			    ],
			    [setq, u_not_first_c63, []],
			    
			    [ u_yloop,
			      [u_initial, [rest, u_slot_values]],
			      [u_ywhile, rest],
			      
			      [ u_yuntil,
				
				[ and,
				  u_short_c63,
				  [>, u_column, u_xx_ob_short_length_xx],
				  [progn, [u_do_ldots, stream], t]
				]
			      ],
			      
			      [ u_ydo,
				
				[ if,
				  u_not_first_c63,
				  
				  [ progn,
				    
				    [ if,
				      u_xx_typeset_c63_xx,
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(\), #\(' ')])
				      ],
				      
				      [ format,
					stream,
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(' ')])
				      ]
				    ],
				    [setq, u_column, [+, 1, u_column]]
				  ]
				],
				[setq, u_not_first_c63, t],
				
				[ setq,
				  u_column,
				  [u_print_item, stream, [car, rest], u_column]
				],
				
				[ if,
				  [and, [cdr, rest], u_separate_lines_c63],
				  
				  [ progn,
				    
				    [ if,
				      
				      [ u_memq_c63,
					[quote, u_no_newline],
					u_xx_ob_print_options_xx
				      ],
				      
				      [ progn,
					
					[ format,
					  stream,
					  '$ARRAY'([*],
						   claz_base_character,
						   [#\(' ')])
					],
					[setq, u_column, [+, 1, u_column]]
				      ],
				      
				      [ progn,
					[u_do_newline, stream],
					
					[ if,
					  [not, u_xx_typeset_c63_xx],
					  
					  [ u_print_spaces,
					    stream,
					    u_slot_value_column
					  ]
					],
					[setq, u_column, u_slot_value_column]
				      ]
				    ],
				    [setq, u_not_first_c63, []]
				  ]
				],
				[setq, rest, [cdr, rest]]
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				u_xx_typeset_c63_xx,
				
				[ not,
				  
				  [ u_memq_c63,
				    [quote, u_no_newline],
				    u_xx_ob_print_options_xx
				  ]
				]
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(\), #\(-)])
			      ]
			    ],
			    [setq, u_previous_embedded_ob_c63, t],
			    
			    [ setq,
			      u_previous_multiple_and_no_newline_c63,
			      
			      [ and,
				[cdr, u_slot_values],
				[not, u_separate_lines_c63]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	LetResult33=Yloop_Ret,
	LetResult31=LetResult33,
	LetResult29=LetResult31,
	LetResult27=LetResult29,
	LetResult=LetResult27,
	f_u_memq_c63([quote, u_parens], u_xx_ob_print_options_xx, IFTEST82),
	(   IFTEST82\==[]
	->  f_u_begin_regular_font(Stream_Param, Regular_font_Ret103),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(')')])
		      ],
		      Format_Ret104),
	    f_u_end_font(Stream_Param, TrueResult87),
	    _151038=TrueResult87
	;   _151038=[]
	),
	cl_if(
	      [ and,
		u_xx_typeset_c63_xx,
		[not, u_top_c63],
		u_it,
		u_looks,
		u_bad,
		u_to,
		u_have,
		u_multiple,
		sys_slots,
		u_per,
		u_line_c46,
		
		[ setq,
		  u_previous_multiple_and_no_newline_c63,
		  [and, [cdr, u_slot_values], [not, u_separate_lines_c63]]
		]
	      ],
	      If_Ret),
	If_Ret=FnResult.
:- set_opv(f_u_ob_c36_print4, classof, claz_function),
   set_opv(u_ob_c36_print4, compile_as, kw_function),
   set_opv(u_ob_c36_print4, function, f_u_ob_c36_print4),
   DefunResult=u_ob_c36_print4.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" I don't see why this would be.",
				     23,
				     13408)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" It prints a space after objects below.",
				     23,
				     13463)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (format stream \" \")",
				     1,
				     13530)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (setq column (+ 1 column))",
				     1,
				     13573)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was emb-ob?", 45, 17263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:10892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" It looks bad to have multiple slots per line.",
				     18,
				     17294)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19251 **********************/
:- lisp_compile_to_prolog(pkg_user, 'itch-out-string*').
:- get_var(TLEnv3, u_itch_out_string_xx, Itch_out_string_xx_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19267 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19268 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19268 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (write *switch-out-string* stream)))",
				     1,
				     19271)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19315 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*print-length*', 60]).
:- set_var(TLEnv3, setq, xx_print_length_xx, 60).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19340 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*print-length*', 600]).
:- set_var(TLEnv3, setq, xx_print_length_xx, 600).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19367 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ob-short-length*', 26]).
:- set_var(TLEnv3, setq, u_xx_ob_short_length_xx, 26).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19395 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ob-short-length*', 127]).
:- set_var(TLEnv3, setq, u_xx_ob_short_length_xx, 127).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19425 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'type->string',
			    [type],
			    
			    [ if,
			      '*typeset?*',
			      
			      [ concatenate,
				[quote, string],
				'$STRING'("{\\bf{}"),
				['symbol->string', ['ob$name', type]],
				'$STRING'("}")
			      ],
			      ['symbol->string', ['ob$name', type]]
			    ]
			  ]).

% annotating U::TYPE->STRING 
wl: lambda_def(defun,
	      u_type_c62_string,
	      f_u_type_c62_string,
	      [type],
	      
	      [ 
		[ if,
		  u_xx_typeset_c63_xx,
		  
		  [ concatenate,
		    [quote, string],
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\('{'), #\(\), #\(b), #\(f), #\('{'), #\('}')]),
		    [u_symbol_c62_string, [u_ob_c36_name, type]],
		    '$ARRAY'([*], claz_base_character, [#\('}')])
		  ],
		  [u_symbol_c62_string, [u_ob_c36_name, type]]
		]
	      ]).


% annotating U::TYPE->STRING 
wl: arglist_info(u_type_c62_string,
		[type],
		[Type_Param],
		arginfo{ all:[type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[type],
			 opt:0,
			 req:[type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TYPE->STRING 
wl: init_args(exact_only, u_type_c62_string).


% annotating U::TYPE->STRING 
f_u_type_c62_string(Type_Param, FnResult) :-
	Env=[bv(type, Type_Param)],
	get_var(Env, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_symbol_c62_string([u_ob_c36_name, type], C62_string_Ret),
	    cl_concatenate(string,
			   '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('{'),
				      #\(\),
				      #\(b),
				      #\(f),
				      #\('{'),
				      #\('}')
				    ]),
			   C62_string_Ret,
			   '$ARRAY'([*], claz_base_character, [#\('}')]),
			   TrueResult),
	    FnResult=TrueResult
	;   f_u_symbol_c62_string([u_ob_c36_name, type], ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_type_c62_string, classof, claz_function),
   set_opv(u_type_c62_string, compile_as, kw_function),
   set_opv(u_type_c62_string, function, f_u_type_c62_string),
   DefunResult=u_type_c62_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19606 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob->string',
			    [ob],
			    
			    [ let,
			      [[str, ['symbol->string', ['ob$name', ob]]]],
			      ['string-downcase!', [chdr, str]],
			      
			      [ if,
				'*typeset?*',
				
				[ setq,
				  str,
				  
				  [ concatenate,
				    [quote, string],
				    '$STRING'("{\\sl{}"),
				    str,
				    '$STRING'("}")
				  ]
				]
			      ],
			      str
			    ]
			  ]).

% annotating U::OB->STRING 
wl: lambda_def(defun,
	      u_ob_c62_string,
	      f_u_ob_c62_string,
	      [u_ob],
	      
	      [ 
		[ let,
		  [[u_str, [u_symbol_c62_string, [u_ob_c36_name, u_ob]]]],
		  [u_string_downcase_c33, [u_chdr, u_str]],
		  
		  [ if,
		    u_xx_typeset_c63_xx,
		    
		    [ setq,
		      u_str,
		      
		      [ concatenate,
			[quote, string],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\('{'), #\(\), #\(s), #\(l), #\('{'), #\('}')]),
			u_str,
			'$ARRAY'([*], claz_base_character, [#\('}')])
		      ]
		    ]
		  ],
		  u_str
		]
	      ]).


% annotating U::OB->STRING 
wl: arglist_info(u_ob_c62_string,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB->STRING 
wl: init_args(exact_only, u_ob_c62_string).


% annotating U::OB->STRING 
f_u_ob_c62_string(Ob_Param, Str_Get21) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_symbol_c62_string([u_ob_c36_name, u_ob], Str_Init),
	LEnv=[[bv(u_str, Str_Init)]|Env],
	f_u_string_downcase_c33([u_chdr, u_str], Downcase_c33_Ret),
	get_var(LEnv, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_str, Str_Get21),
	    cl_concatenate(string,
			   '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('{'),
				      #\(\),
				      #\(s),
				      #\(l),
				      #\('{'),
				      #\('}')
				    ]),
			   Str_Get21,
			   '$ARRAY'([*], claz_base_character, [#\('}')]),
			   TrueResult),
	    set_var(LEnv, u_str, TrueResult),
	    _132948=TrueResult
	;   _132948=[]
	).
:- set_opv(f_u_ob_c62_string, classof, claz_function),
   set_opv(u_ob_c62_string, compile_as, kw_function),
   set_opv(u_ob_c62_string, function, f_u_ob_c62_string),
   DefunResult=u_ob_c62_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob->raw-string',
			    [ob],
			    
			    [ let,
			      [[str, ['symbol->string', ['ob$name', ob]]]],
			      ['string-downcase!', [chdr, str]],
			      str
			    ]
			  ]).

% annotating U::OB->RAW-STRING 
wl: lambda_def(defun,
	      u_ob_c62_raw_string,
	      f_u_ob_c62_raw_string,
	      [u_ob],
	      
	      [ 
		[ let,
		  [[u_str, [u_symbol_c62_string, [u_ob_c36_name, u_ob]]]],
		  [u_string_downcase_c33, [u_chdr, u_str]],
		  u_str
		]
	      ]).


% annotating U::OB->RAW-STRING 
wl: arglist_info(u_ob_c62_raw_string,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB->RAW-STRING 
wl: init_args(exact_only, u_ob_c62_raw_string).


% annotating U::OB->RAW-STRING 
f_u_ob_c62_raw_string(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_symbol_c62_string([u_ob_c36_name, u_ob], Str_Init),
	LEnv=[[bv(u_str, Str_Init)]|Env],
	f_u_string_downcase_c33([u_chdr, u_str], Downcase_c33_Ret),
	get_var(LEnv, u_str, Str_Get),
	LetResult=Str_Get,
	LetResult=FnResult.
:- set_opv(f_u_ob_c62_raw_string, classof, claz_function),
   set_opv(u_ob_c62_raw_string, compile_as, kw_function),
   set_opv(u_ob_c62_raw_string, function, f_u_ob_c62_raw_string),
   DefunResult=u_ob_c62_raw_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:19915 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-spaces',
			    [stream, number],
			    
			    [ yloop,
			      [initial, [count, 1]],
			      [ywhile, [<=, count, number]],
			      
			      [ ydo,
				
				[ if,
				  '*typeset?*',
				  [format, stream, '$STRING'("\\ ")],
				  [format, stream, '$STRING'(" ")]
				],
				['increment-me', count]
			      ]
			    ]
			  ]).

% annotating U::PRINT-SPACES 
wl: lambda_def(defun,
	      u_print_spaces,
	      f_u_print_spaces,
	      [stream, number],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [count, 1]],
		  [u_ywhile, [<=, count, number]],
		  
		  [ u_ydo,
		    
		    [ if,
		      u_xx_typeset_c63_xx,
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(\), #\(' ')])
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(' ')])
		      ]
		    ],
		    [u_increment_me, count]
		  ]
		]
	      ]).


% annotating U::PRINT-SPACES 
wl: arglist_info(u_print_spaces,
		[stream, number],
		[Stream_Param, Number_Param],
		arginfo{ all:[stream, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, number],
			 opt:0,
			 req:[stream, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-SPACES 
wl: init_args(exact_only, u_print_spaces).


% annotating U::PRINT-SPACES 
f_u_print_spaces(Stream_Param, Number_Param, FnResult) :-
	Env=[bv(stream, Stream_Param), bv(number, Number_Param)],
	f_u_yloop(
		  [ [u_initial, [count, 1]],
		    [u_ywhile, [<=, count, number]],
		    
		    [ u_ydo,
		      
		      [ if,
			u_xx_typeset_c63_xx,
			
			[ format,
			  stream,
			  '$ARRAY'([*], claz_base_character, [#\(\), #\(' ')])
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*], claz_base_character, [#\(' ')])
			]
		      ],
		      [u_increment_me, count]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_print_spaces, classof, claz_function),
   set_opv(u_print_spaces, compile_as, kw_function),
   set_opv(u_print_spaces, function, f_u_print_spaces),
   DefunResult=u_print_spaces.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:20166 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'embedded-ob?',
			    [x],
			    
			    [ or,
			      ['pair?', x],
			      
			      [ and,
				['ob?', x],
				[not, ['var?', x]],
				[not, ['ob$literal?', x]],
				[not, ['has-interesting-obname?', x]]
			      ]
			    ]
			  ]).

% annotating U::EMBEDDED-OB? 
wl: lambda_def(defun,
	      u_embedded_ob_c63,
	      f_u_embedded_ob_c63,
	      [u_x],
	      
	      [ 
		[ or,
		  [u_pair_c63, u_x],
		  
		  [ and,
		    [u_ob_c63, u_x],
		    [not, [u_var_c63, u_x]],
		    [not, [u_ob_c36_literal_c63, u_x]],
		    [not, [u_has_interesting_obname_c63, u_x]]
		  ]
		]
	      ]).


% annotating U::EMBEDDED-OB? 
wl: arglist_info(u_embedded_ob_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMBEDDED-OB? 
wl: init_args(exact_only, u_embedded_ob_c63).


% annotating U::EMBEDDED-OB? 
f_u_embedded_ob_c63(X_Param, TrueResult23) :-
	Env=[bv(u_x, X_Param)],
	(   f_u_pair_c63(u_x, FORM1_Res),
	    FORM1_Res\==[],
	    TrueResult23=FORM1_Res
	->  true
	;   f_u_ob_c63(u_x, IFTEST),
	    (   IFTEST\==[]
	    ->  f_u_var_c63(u_x, PredArgResult),
		(   PredArgResult==[]
		->  f_u_ob_c36_literal_c63(X_Param, PredArgResult20),
		    (   PredArgResult20==[]
		    ->  f_u_has_interesting_obname_c63(X_Param, Not_Param),
			cl_not(Not_Param, TrueResult),
			TrueResult23=TrueResult
		    ;   TrueResult23=[]
		    )
		;   TrueResult23=[]
		)
	    ;   TrueResult23=[]
	    )
	).
:- set_opv(f_u_embedded_ob_c63, classof, claz_function),
   set_opv(u_embedded_ob_c63, compile_as, kw_function),
   set_opv(u_embedded_ob_c63, function, f_u_embedded_ob_c63),
   DefunResult=u_embedded_ob_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:20333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-item',
			    [stream, item, column],
			    
			    [ cond,
			      
			      [ ['nil?', item],
				['print-symbol', stream, [quote, []], column]
			      ],
			      
			      [ ['symbol?', item],
				['print-symbol', stream, item, column]
			      ],
			      
			      [ ['string?', item],
				
				[ 'print-regular',
				  stream,
				  
				  [ concatenate,
				    [quote, string],
				    '$STRING'("\""),
				    item,
				    '$STRING'("\"")
				  ],
				  column
				]
			      ],
			      
			      [ ['number?', item],
				
				[ 'print-typewriter',
				  stream,
				  [format, [], '$STRING'("~A"), item],
				  column
				]
			      ],
			      
			      [ ['pair?', item],
				['print-pair', stream, item, column]
			      ],
			      
			      [ ['var?', item],
				['print-variable-item', stream, item, column]
			      ],
			      
			      [ ['ob?', item],
				
				[ if,
				  
				  [ or,
				    ['ob$literal?', item],
				    ['has-interesting-obname?', item]
				  ],
				  
				  [ 'print-slanted',
				    stream,
				    
				    [ 'capitalize!',
				      ['symbol->string', ['ob$name', item]]
				    ],
				    column
				  ],
				  ['ob$print2', item, stream, column, []]
				]
			      ],
			      
			      [ else,
				
				[ 'print-regular',
				  stream,
				  '$STRING'("??????"),
				  column
				]
			      ]
			    ]
			  ]).

% annotating U::PRINT-ITEM 
wl: lambda_def(defun,
	      u_print_item,
	      f_u_print_item,
	      [stream, item, u_column],
	      
	      [ 
		[ cond,
		  
		  [ [u_nil_c63, item],
		    [u_print_symbol, stream, [quote, []], u_column]
		  ],
		  [[u_symbol_c63, item], [u_print_symbol, stream, item, u_column]],
		  
		  [ [u_string_c63, item],
		    
		    [ u_print_regular,
		      stream,
		      
		      [ concatenate,
			[quote, string],
			'$ARRAY'([*], claz_base_character, [#\('"')]),
			item,
			'$ARRAY'([*], claz_base_character, [#\('"')])
		      ],
		      u_column
		    ]
		  ],
		  
		  [ [u_number_c63, item],
		    
		    [ u_print_typewriter,
		      stream,
		      
		      [ format,
			[],
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			item
		      ],
		      u_column
		    ]
		  ],
		  [[u_pair_c63, item], [u_print_pair, stream, item, u_column]],
		  
		  [ [u_var_c63, item],
		    [u_print_variable_item, stream, item, u_column]
		  ],
		  
		  [ [u_ob_c63, item],
		    
		    [ if,
		      
		      [ or,
			[u_ob_c36_literal_c63, item],
			[u_has_interesting_obname_c63, item]
		      ],
		      
		      [ u_print_slanted,
			stream,
			
			[ u_capitalize_c33,
			  [u_symbol_c62_string, [u_ob_c36_name, item]]
			],
			u_column
		      ],
		      [u_ob_c36_print2, item, stream, u_column, []]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ u_print_regular,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(?), #\(?), #\(?), #\(?), #\(?), #\(?)]),
		      u_column
		    ]
		  ]
		]
	      ]).


% annotating U::PRINT-ITEM 
wl: arglist_info(u_print_item,
		[stream, item, u_column],
		[Stream_Param, Item_Param, Column_Param],
		arginfo{ all:[stream, item, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, item, u_column],
			 opt:0,
			 req:[stream, item, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-ITEM 
wl: init_args(exact_only, u_print_item).


% annotating U::PRINT-ITEM 
f_u_print_item(Stream_Param, Item_Param, Column_Param, TrueResult66) :-
	Env=[bv(stream, Stream_Param), bv(item, Item_Param), bv(u_column, Column_Param)],
	f_u_nil_c63(item, IFTEST),
	(   IFTEST\==[]
	->  f_u_print_symbol(Stream_Param, [], Column_Param, TrueResult78),
	    TrueResult66=TrueResult78
	;   f_u_symbol_c63(item, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_print_symbol(Stream_Param,
				 Item_Param,
				 Column_Param,
				 TrueResult76),
		TrueResult66=TrueResult76
	    ;   f_u_string_c63(item, IFTEST25),
		(   IFTEST25\==[]
		->  cl_concatenate(string,
				   '$ARRAY'([*], claz_base_character, [#\('"')]),
				   Item_Param,
				   '$ARRAY'([*], claz_base_character, [#\('"')]),
				   Concatenate_Ret),
		    f_u_print_regular(Stream_Param,
				      Concatenate_Ret,
				      Column_Param,
				      TrueResult74),
		    TrueResult66=TrueResult74
		;   f_u_number_c63(item, IFTEST30),
		    (   IFTEST30\==[]
		    ->  cl_format(
				  [ [],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(~), #\('A')]),
				    Item_Param
				  ],
				  Format_Ret),
			f_u_print_typewriter(Stream_Param,
					     Format_Ret,
					     Column_Param,
					     TrueResult72),
			TrueResult66=TrueResult72
		    ;   f_u_pair_c63(item, IFTEST35),
			(   IFTEST35\==[]
			->  f_u_print_pair(Stream_Param,
					   Item_Param,
					   Column_Param,
					   TrueResult70),
			    TrueResult66=TrueResult70
			;   f_u_var_c63(item, IFTEST40),
			    (   IFTEST40\==[]
			    ->  f_u_print_variable_item(Stream_Param,
							Item_Param,
							Column_Param,
							TrueResult68),
				TrueResult66=TrueResult68
			    ;   f_u_ob_c63(item, IFTEST45),
				(   IFTEST45\==[]
				->  (   f_u_ob_c36_literal_c63(Item_Param,
							       FORM1_Res),
					FORM1_Res\==[],
					IFTEST47=FORM1_Res
				    ->  true
				    ;   f_u_has_interesting_obname_c63(Item_Param,
								       Obname_c63_Ret),
					IFTEST47=Obname_c63_Ret
				    ),
				    (   IFTEST47\==[]
				    ->  f_u_symbol_c62_string(
							      [ u_ob_c36_name,
								item
							      ],
							      Capitalize_c33_Param),
					f_u_capitalize_c33(Capitalize_c33_Param,
							   Capitalize_c33_Ret),
					f_u_print_slanted(Stream_Param,
							  Capitalize_c33_Ret,
							  Column_Param,
							  TrueResult),
					TrueResult66=TrueResult
				    ;   f_u_ob_c36_print2(Item_Param,
							  Stream_Param,
							  Column_Param,
							  [],
							  ElseResult),
					TrueResult66=ElseResult
				    )
				;   get_var(Env, u_else, IFTEST59),
				    (   IFTEST59\==[]
				    ->  f_u_print_regular(Stream_Param,
							  '$ARRAY'([*],
								   claz_base_character,
								   
								   [ #\(?),
								     #\(?),
								     #\(?),
								     #\(?),
								     #\(?),
								     #\(?)
								   ]),
							  Column_Param,
							  TrueResult64),
					TrueResult66=TrueResult64
				    ;   TrueResult66=[]
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_print_item, classof, claz_function),
   set_opv(u_print_item, compile_as, kw_function),
   set_opv(u_print_item, function, f_u_print_item),
   DefunResult=u_print_item.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:20333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (format *gate-output* \"Unknown item.~%\")",
				     1,
				     21046)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:20333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" I hope result column doesn't matter because we always do",
				     1,
				     21137)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:20333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" a newline after printing a pair.",
				     1,
				     21196)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:21230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-pair',
			    [stream, pair, column],
			    
			    [ if,
			      
			      [ 'memq?',
				[quote, 'no-newline'],
				'*ob-print-options*'
			      ],
			      
			      [ progn,
				['begin-typewriter-font', stream],
				[format, stream, '$STRING'("(QUOTE ~A)"), pair],
				['end-font', stream],
				column
			      ],
			      
			      [ let,
				
				[ 
				  [ str,
				    
				    [ 'with-output-to-string',
				      [stream1],
				      
				      [ 'pretty-print',
					[list, [quote, quote], pair],
					stream1
				      ]
				    ]
				  ],
				  [result, []]
				],
				
				[ 'walk-string',
				  
				  [ lambda,
				    [c],
				    
				    [ cond,
				      
				      [ ['eq?', c, #\(' ')],
					
					[ if,
					  '*typeset?*',
					  
					  [ setq,
					    result,
					    
					    [ 'append!',
					      result,
					      [list, #\(\), #\(' ')]
					    ]
					  ],
					  
					  [ setq,
					    result,
					    ['append!', result, [list, #\(' ')]]
					  ]
					]
				      ],
				      
				      [ ['eq?', c, #\(10)],
					
					[ if,
					  '*typeset?*',
					  
					  [ setq,
					    result,
					    
					    [ 'append!',
					      result,
					      [list, #\(\), #\(\), #\(10)]
					    ]
					  ],
					  
					  [ setq,
					    result,
					    
					    [ 'append!',
					      result,
					      
					      [ list,
						#\(10),
						['n-spaces', column]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ else,
					
					[ setq,
					  result,
					  ['append!', result, [list, c]]
					]
				      ]
				    ]
				  ],
				  str
				],
				['begin-typewriter-font', stream],
				
				[ format,
				  stream,
				  '$STRING'("~A"),
				  ['list->string', result]
				],
				['end-font', stream],
				column
			      ]
			    ]
			  ]).

% annotating U::PRINT-PAIR 
wl: lambda_def(defun,
	      u_print_pair,
	      f_u_print_pair,
	      [stream, u_pair, u_column],
	      
	      [ 
		[ if,
		  [u_memq_c63, [quote, u_no_newline], u_xx_ob_print_options_xx],
		  
		  [ progn,
		    [u_begin_typewriter_font, stream],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('('),
				 #\('Q'),
				 #\('U'),
				 #\('O'),
				 #\('T'),
				 #\('E'),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(')')
			       ]),
		      u_pair
		    ],
		    [u_end_font, stream],
		    u_column
		  ],
		  
		  [ let,
		    
		    [ 
		      [ u_str,
			
			[ with_output_to_string,
			  [u_stream1],
			  
			  [ u_pretty_print,
			    [list, [quote, quote], u_pair],
			    u_stream1
			  ]
			]
		      ],
		      [u_result, []]
		    ],
		    
		    [ u_walk_string,
		      
		      [ lambda,
			[u_c],
			
			[ cond,
			  
			  [ [u_eq_c63, u_c, #\(' ')],
			    
			    [ if,
			      u_xx_typeset_c63_xx,
			      
			      [ setq,
				u_result,
				[u_append_c33, u_result, [list, #\(\), #\(' ')]]
			      ],
			      
			      [ setq,
				u_result,
				[u_append_c33, u_result, [list, #\(' ')]]
			      ]
			    ]
			  ],
			  
			  [ [u_eq_c63, u_c, #\(10)],
			    
			    [ if,
			      u_xx_typeset_c63_xx,
			      
			      [ setq,
				u_result,
				
				[ u_append_c33,
				  u_result,
				  [list, #\(\), #\(\), #\(10)]
				]
			      ],
			      
			      [ setq,
				u_result,
				
				[ u_append_c33,
				  u_result,
				  [list, #\(10), [u_n_spaces, u_column]]
				]
			      ]
			    ]
			  ],
			  
			  [ u_else,
			    [setq, u_result, [u_append_c33, u_result, [list, u_c]]]
			  ]
			]
		      ],
		      u_str
		    ],
		    [u_begin_typewriter_font, stream],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		      [u_list_c62_string, u_result]
		    ],
		    [u_end_font, stream],
		    u_column
		  ]
		]
	      ]).


% annotating U::PRINT-PAIR 
wl: arglist_info(u_print_pair,
		[stream, u_pair, u_column],
		[Stream_Param, Pair_Param, Column_Get60],
		arginfo{ all:[stream, u_pair, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, u_pair, u_column],
			 opt:0,
			 req:[stream, u_pair, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-PAIR 
wl: init_args(exact_only, u_print_pair).


% annotating U::PRINT-PAIR 
f_u_print_pair(Stream_Param, Pair_Param, Column_Get60, FnResult) :-
	Env=[bv(stream, Stream_Param), bv(u_pair, Pair_Param), bv(u_column, Column_Get60)],
	f_u_memq_c63([quote, u_no_newline], u_xx_ob_print_options_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_begin_typewriter_font(Stream_Param, Typewriter_font_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('('),
				   #\('Q'),
				   #\('U'),
				   #\('O'),
				   #\('T'),
				   #\('E'),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(')')
				 ]),
			Pair_Param
		      ],
		      Format_Ret),
	    f_u_end_font(Stream_Param, End_font_Ret),
	    FnResult=Column_Get60
	;   cl_with_output_to_string([u_stream1],
				     
				     [ u_pretty_print,
				       [list, [quote, quote], u_pair],
				       u_stream1
				     ],
				     Str_Init),
	    LEnv=[[bv(u_str, Str_Init), bv(u_result, [])]|Env],
	    Lambda=closure([Env|LEnv], LResult, [u_c],  (f_u_eq_c63(u_c, #\(' '), IFTEST26), (IFTEST26\==[]->get_var(Env, u_xx_typeset_c63_xx, IFTEST28), (IFTEST28\==[]->f_u_append_c33(u_result, [list, #\(\), #\(' ')], TrueResult), set_var(Env, u_result, TrueResult), TrueResult49=TrueResult;f_u_append_c33(u_result, [list, #\(' ')], ElseResult), set_var(Env, u_result, ElseResult), TrueResult49=ElseResult), LResult=TrueResult49;f_u_eq_c63(u_c, #\(10), IFTEST35), (IFTEST35\==[]->get_var(Env, u_xx_typeset_c63_xx, IFTEST37), (IFTEST37\==[]->f_u_append_c33(u_result, [list, #\(\), #\(\), #\(10)], TrueResult40), set_var(Env, u_result, TrueResult40), TrueResult47=TrueResult40;f_u_append_c33(u_result, [list, #\(10), [u_n_spaces, u_column]], ElseResult41), set_var(Env, u_result, ElseResult41), TrueResult47=ElseResult41), ElseResult50=TrueResult47;get_var(Env, u_else, IFTEST42), (IFTEST42\==[]->f_u_append_c33(u_result, [list, u_c], TrueResult45), set_var(Env, u_result, TrueResult45), ElseResult48=TrueResult45;ElseResult46=[], ElseResult48=ElseResult46), ElseResult50=ElseResult48), LResult=ElseResult50))),
	    get_var(LEnv, u_str, Str_Get),
	    f_u_walk_string(Lambda, Str_Get, Walk_string_Ret),
	    f_u_begin_typewriter_font(Stream_Param, Typewriter_font_Ret69),
	    get_var(LEnv, u_result, Result_Get),
	    f_u_list_c62_string(Result_Get, C62_string_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			C62_string_Ret
		      ],
		      Format_Ret71),
	    f_u_end_font(Stream_Param, End_font_Ret72),
	    FnResult=Column_Get60
	).
:- set_opv(f_u_print_pair, classof, claz_function),
   set_opv(u_print_pair, compile_as, kw_function),
   set_opv(u_print_pair, function, f_u_print_pair),
   DefunResult=u_print_pair.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:22338 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'n-spaces',
			    [n],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, [>, n, 0]],
			      
			      [ ydo,
				[setq, result, [cons, #\(' '), result]],
				[setq, n, [-, n, 1]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::N-SPACES 
wl: lambda_def(defun,
	      u_n_spaces,
	      f_u_n_spaces,
	      [n],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, [>, n, 0]],
		  
		  [ u_ydo,
		    [setq, u_result, [cons, #\(' '), u_result]],
		    [setq, n, [-, n, 1]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::N-SPACES 
wl: arglist_info(u_n_spaces,
		[n],
		[N_Param],
		arginfo{ all:[n],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[n],
			 opt:0,
			 req:[n],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::N-SPACES 
wl: init_args(exact_only, u_n_spaces).


% annotating U::N-SPACES 
f_u_n_spaces(N_Param, FnResult) :-
	Env=[bv(n, N_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, [>, n, 0]],
		    
		    [ u_ydo,
		      [setq, u_result, [cons, #\(' '), u_result]],
		      [setq, n, [-, n, 1]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_n_spaces, classof, claz_function),
   set_opv(u_n_spaces, compile_as, kw_function),
   set_opv(u_n_spaces, function, f_u_n_spaces),
   DefunResult=u_n_spaces.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:22522 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-variable-item',
			    [stream, item, column],
			    
			    [ let,
			      
			      [ [name, ['variable-name', item]],
				[typ, ['variable-type', item]]
			      ],
			      
			      [ cond,
				
				[ [and, name, typ],
				  
				  [ let,
				    
				    [ ['type-string', []],
				      ['name-string', []],
				      [len, []],
				      ['last-char', []]
				    ],
				    
				    [ cond,
				      
				      [ ['eq?', name, [quote, self]],
					
					[ 'print-slanted',
					  stream,
					  '$STRING'("?Self"),
					  column
					]
				      ],
				      
				      [ ['eq?', name, [quote, other]],
					
					[ 'print-slanted',
					  stream,
					  '$STRING'("?Other"),
					  column
					]
				      ],
				      
				      [ 
					[ and,
					  
					  [ setq,
					    'type-string',
					    ['symbol->string', ['ob$name', typ]]
					  ],
					  
					  [ setq,
					    'name-string',
					    ['symbol->string', name]
					  ],
					  
					  [ 'string-equal?',
					    'type-string',
					    'name-string'
					  ]
					],
					
					[ 'print-slanted',
					  stream,
					  
					  [ concatenate,
					    [quote, string],
					    '$STRING'("?"),
					    ['capitalize!', 'name-string']
					  ],
					  column
					]
				      ],
				      
				      [ 
					[ and,
					  
					  [ setq,
					    len,
					    ['string-length', 'name-string']
					  ],
					  
					  [ setq,
					    'last-char',
					    [nthchar, 'name-string', [-, len, 1]]
					  ],
					  ['digit?', 'last-char', 10],
					  
					  [ 'string-equal?',
					    'type-string',
					    
					    [ 'string-slice',
					      'name-string',
					      0,
					      [-, len, 1]
					    ]
					  ]
					],
					
					[ 'print-slanted',
					  stream,
					  
					  [ concatenate,
					    [quote, string],
					    '$STRING'("?"),
					    ['capitalize!', 'name-string']
					  ],
					  column
					]
				      ],
				      
				      [ else,
					
					[ 'print-slanted',
					  stream,
					  
					  [ concatenate,
					    [quote, string],
					    '$STRING'("?"),
					    ['capitalize!', 'name-string'],
					    '$STRING'(":"),
					    'type-string'
					  ],
					  column
					]
				      ]
				    ]
				  ]
				],
				
				[ name,
				  
				  [ 'print-slanted',
				    stream,
				    
				    [ concatenate,
				      [quote, string],
				      '$STRING'("?"),
				      ['capitalize!', ['symbol->string', name]],
				      '$STRING'(":NOTYPE")
				    ],
				    column
				  ]
				],
				
				[ typ,
				  
				  [ 'print-slanted',
				    stream,
				    
				    [ concatenate,
				      [quote, string],
				      '$STRING'("?:"),
				      ['symbol->string', ['ob$name', typ]]
				    ],
				    column
				  ]
				],
				
				[ else,
				  
				  [ 'print-slanted',
				    stream,
				    '$STRING'("??"),
				    column
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PRINT-VARIABLE-ITEM 
wl: lambda_def(defun,
	      u_print_variable_item,
	      f_u_print_variable_item,
	      [stream, item, u_column],
	      
	      [ 
		[ let,
		  
		  [ [sys_name, [u_variable_name, item]],
		    [u_typ, [u_variable_type, item]]
		  ],
		  
		  [ cond,
		    
		    [ [and, sys_name, u_typ],
		      
		      [ let,
			
			[ [u_type_string, []],
			  [u_name_string, []],
			  [u_len, []],
			  [u_last_char, []]
			],
			
			[ cond,
			  
			  [ [u_eq_c63, sys_name, [quote, u_self]],
			    
			    [ u_print_slanted,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(?), #\('S'), #\(e), #\(l), #\(f)]),
			      u_column
			    ]
			  ],
			  
			  [ [u_eq_c63, sys_name, [quote, u_other]],
			    
			    [ u_print_slanted,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(?), #\('O'), #\(t), #\(h), #\(e), #\(r)]),
			      u_column
			    ]
			  ],
			  
			  [ 
			    [ and,
			      
			      [ setq,
				u_type_string,
				[u_symbol_c62_string, [u_ob_c36_name, u_typ]]
			      ],
			      
			      [ setq,
				u_name_string,
				[u_symbol_c62_string, sys_name]
			      ],
			      [u_string_equal_c63, u_type_string, u_name_string]
			    ],
			    
			    [ u_print_slanted,
			      stream,
			      
			      [ concatenate,
				[quote, string],
				'$ARRAY'([*], claz_base_character, [#\(?)]),
				[u_capitalize_c33, u_name_string]
			      ],
			      u_column
			    ]
			  ],
			  
			  [ 
			    [ and,
			      [setq, u_len, [u_string_length, u_name_string]],
			      
			      [ setq,
				u_last_char,
				[u_nthchar, u_name_string, [-, u_len, 1]]
			      ],
			      [u_digit_c63, u_last_char, 10],
			      
			      [ u_string_equal_c63,
				u_type_string,
				[u_string_slice, u_name_string, 0, [-, u_len, 1]]
			      ]
			    ],
			    
			    [ u_print_slanted,
			      stream,
			      
			      [ concatenate,
				[quote, string],
				'$ARRAY'([*], claz_base_character, [#\(?)]),
				[u_capitalize_c33, u_name_string]
			      ],
			      u_column
			    ]
			  ],
			  
			  [ u_else,
			    
			    [ u_print_slanted,
			      stream,
			      
			      [ concatenate,
				[quote, string],
				'$ARRAY'([*], claz_base_character, [#\(?)]),
				[u_capitalize_c33, u_name_string],
				'$ARRAY'([*], claz_base_character, [#\(:)]),
				u_type_string
			      ],
			      u_column
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ sys_name,
		      
		      [ u_print_slanted,
			stream,
			
			[ concatenate,
			  [quote, string],
			  '$ARRAY'([*], claz_base_character, [#\(?)]),
			  [u_capitalize_c33, [u_symbol_c62_string, sys_name]],
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(:),
				     #\('N'),
				     #\('O'),
				     #\('T'),
				     #\('Y'),
				     #\('P'),
				     #\('E')
				   ])
			],
			u_column
		      ]
		    ],
		    
		    [ u_typ,
		      
		      [ u_print_slanted,
			stream,
			
			[ concatenate,
			  [quote, string],
			  '$ARRAY'([*], claz_base_character, [#\(?), #\(:)]),
			  [u_symbol_c62_string, [u_ob_c36_name, u_typ]]
			],
			u_column
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ u_print_slanted,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(?), #\(?)]),
			u_column
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PRINT-VARIABLE-ITEM 
wl: arglist_info(u_print_variable_item,
		[stream, item, u_column],
		[Stream_Param, Item_Param, Column_Param],
		arginfo{ all:[stream, item, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, item, u_column],
			 opt:0,
			 req:[stream, item, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-VARIABLE-ITEM 
wl: init_args(exact_only, u_print_variable_item).


% annotating U::PRINT-VARIABLE-ITEM 
f_u_print_variable_item(Stream_Param, Item_Param, Column_Param, ElseResult74) :-
	Env=[bv(stream, Stream_Param), bv(item, Item_Param), bv(u_column, Column_Param)],
	f_u_variable_name(item, Name_Init),
	f_u_variable_type(item, Typ_Init),
	LEnv=[[bv(sys_name, Name_Init), bv(u_typ, Typ_Init)]|Env],
	get_var(LEnv, sys_name, IFTEST22),
	(   IFTEST22\==[]
	->  get_var(LEnv, u_typ, Typ_Get),
	    IFTEST=Typ_Get
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  Env=[[bv(u_type_string, []), bv(u_name_string, []), bv(u_len, []), bv(u_last_char, [])]|LEnv],
	    f_u_eq_c63(sys_name, [quote, u_self], IFTEST30),
	    (   IFTEST30\==[]
	    ->  f_u_print_slanted(Stream_Param,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(?), #\('S'), #\(e), #\(l), #\(f)]),
				  Column_Param,
				  TrueResult79),
		ElseResult74=TrueResult79
	    ;   f_u_eq_c63(sys_name, [quote, u_other], IFTEST35),
		(   IFTEST35\==[]
		->  f_u_print_slanted(Stream_Param,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\(?),
						 #\('O'),
						 #\(t),
						 #\(h),
						 #\(e),
						 #\(r)
					       ]),
				      Column_Param,
				      TrueResult77),
		    ElseResult74=TrueResult77
		;   f_u_symbol_c62_string([u_ob_c36_name, u_typ], IFTEST41),
		    set_var(Env, u_type_string, IFTEST41),
		    (   IFTEST41\==[]
		    ->  f_u_symbol_c62_string(sys_name, IFTEST43),
			set_var(Env, u_name_string, IFTEST43),
			(   IFTEST43\==[]
			->  f_u_string_equal_c63(u_type_string,
						 u_name_string,
						 TrueResult45),
			    IFTEST39=TrueResult45
			;   IFTEST39=[]
			)
		    ;   IFTEST39=[]
		    ),
		    (   IFTEST39\==[]
		    ->  get_var(Env, u_name_string, Name_string_Get62),
			f_u_capitalize_c33(Name_string_Get62,
					   Capitalize_c33_Ret),
			cl_concatenate(string,
				       '$ARRAY'([*],
						claz_base_character,
						[#\(?)]),
				       Capitalize_c33_Ret,
				       Concatenate_Ret),
			f_u_print_slanted(Stream_Param,
					  Concatenate_Ret,
					  Column_Param,
					  TrueResult75),
			ElseResult74=TrueResult75
		    ;   f_u_string_length(u_name_string, IFTEST52),
			set_var(Env, u_len, IFTEST52),
			(   IFTEST52\==[]
			->  f_u_nthchar(u_name_string, [-, u_len, 1], IFTEST54),
			    set_var(Env, u_last_char, IFTEST54),
			    (   IFTEST54\==[]
			    ->  f_u_digit_c63(u_last_char, 10, IFTEST56),
				(   IFTEST56\==[]
				->  f_u_string_equal_c63(u_type_string,
							 
							 [ u_string_slice,
							   u_name_string,
							   0,
							   [-, u_len, 1]
							 ],
							 TrueResult58),
				    IFTEST50=TrueResult58
				;   IFTEST50=[]
				)
			    ;   IFTEST50=[]
			    )
			;   IFTEST50=[]
			),
			(   IFTEST50\==[]
			->  f_u_capitalize_c33(Name_string_Get62,
					       Capitalize_c33_Ret110),
			    cl_concatenate(string,
					   '$ARRAY'([*],
						    claz_base_character,
						    [#\(?)]),
					   Capitalize_c33_Ret110,
					   Concatenate_Ret111),
			    f_u_print_slanted(Stream_Param,
					      Concatenate_Ret111,
					      Column_Param,
					      TrueResult73),
			    ElseResult74=TrueResult73
			;   get_var(Env, u_else, IFTEST64),
			    (   IFTEST64\==[]
			    ->  f_u_capitalize_c33(Name_string_Get62,
						   Capitalize_c33_Ret112),
				get_var(Env, u_type_string, Type_string_Get),
				cl_concatenate(string,
					       '$ARRAY'([*],
							claz_base_character,
							[#\(?)]),
					       Capitalize_c33_Ret112,
					       '$ARRAY'([*],
							claz_base_character,
							[#\(:)]),
					       Type_string_Get,
					       Concatenate_Ret113),
				f_u_print_slanted(Stream_Param,
						  Concatenate_Ret113,
						  Column_Param,
						  TrueResult71),
				ElseResult74=TrueResult71
			    ;   ElseResult74=[]
			    )
			)
		    )
		)
	    )
	;   get_var(LEnv, sys_name, IFTEST82),
	    (   IFTEST82\==[]
	    ->  f_u_symbol_c62_string(sys_name, Capitalize_c33_Param),
		f_u_capitalize_c33(Capitalize_c33_Param, Capitalize_c33_Ret114),
		cl_concatenate(string,
			       '$ARRAY'([*], claz_base_character, [#\(?)]),
			       Capitalize_c33_Ret114,
			       '$ARRAY'([*],
					claz_base_character,
					
					[ #\(:),
					  #\('N'),
					  #\('O'),
					  #\('T'),
					  #\('Y'),
					  #\('P'),
					  #\('E')
					]),
			       Concatenate_Ret115),
		f_u_print_slanted(Stream_Param,
				  Concatenate_Ret115,
				  Column_Param,
				  TrueResult101),
		ElseResult74=TrueResult101
	    ;   get_var(LEnv, u_typ, IFTEST87),
		(   IFTEST87\==[]
		->  f_u_symbol_c62_string([u_ob_c36_name, u_typ],
					  C62_string_Ret),
		    cl_concatenate(string,
				   '$ARRAY'([*],
					    claz_base_character,
					    [#\(?), #\(:)]),
				   C62_string_Ret,
				   Concatenate_Ret117),
		    f_u_print_slanted(Stream_Param,
				      Concatenate_Ret117,
				      Column_Param,
				      TrueResult99),
		    ElseResult74=TrueResult99
		;   get_var(LEnv, u_else, IFTEST92),
		    (   IFTEST92\==[]
		    ->  f_u_print_slanted(Stream_Param,
					  '$ARRAY'([*],
						   claz_base_character,
						   [#\(?), #\(?)]),
					  Column_Param,
					  TrueResult97),
			ElseResult74=TrueResult97
		    ;   ElseResult74=[]
		    )
		)
	    )
	).
:- set_opv(f_u_print_variable_item, classof, claz_function),
   set_opv(u_print_variable_item, compile_as, kw_function),
   set_opv(u_print_variable_item, function, f_u_print_variable_item),
   DefunResult=u_print_variable_item.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:22522 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: make the type-string bold? Also NOTYPE below.",
				     24,
				     23931)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:24432 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-symbol',
			    [stream, symbol, column],
			    
			    [ let,
			      [[string, ['symbol->string', symbol]]],
			      
			      [ cond,
				
				[ ['string-equal?', string, '$STRING'("!LDOTS")],
				  ['begin-regular-font', stream],
				  [format, stream, '$STRING'("\\ldots")],
				  ['end-font', stream],
				  [+, column, 3]
				],
				
				[ 
				  [ 'string-equal?',
				    string,
				    '$STRING'("!OR-BAR")
				  ],
				  ['begin-regular-font', stream],
				  [format, stream, '$STRING'("$\\mid$")],
				  ['end-font', stream],
				  [+, column, 1]
				],
				
				[ 
				  [ and,
				    
				    [ 'string-equal?',
				      
				      [ 'string-slice',
					string,
					0,
					[min, [-, ['string-length', string], 1], 7]
				      ],
				      '$STRING'("!ITALIC")
				    ],
				    [setq, result, ['string-posq', #\(:), string]]
				  ],
				  
				  [ format,
				    stream,
				    '$STRING'("{\\em{}~A}"),
				    
				    [ 'string-downcase',
				      [nthchdr, string, [+, result, 1]]
				    ]
				  ],
				  
				  [ (+),
				    column,
				    [-, ['string-length', string], result]
				  ]
				],
				
				[ else,
				  ['begin-typewriter-font', stream],
				  [format, stream, '$STRING'("'~A"), string],
				  ['end-font', stream],
				  [+, column, ['string-length', string]]
				]
			      ]
			    ]
			  ]).

% annotating U::PRINT-SYMBOL 
wl: lambda_def(defun,
	      u_print_symbol,
	      f_u_print_symbol,
	      [stream, symbol, u_column],
	      
	      [ 
		[ let,
		  [[string, [u_symbol_c62_string, symbol]]],
		  
		  [ cond,
		    
		    [ 
		      [ u_string_equal_c63,
			string,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(!),
				   #\('L'),
				   #\('D'),
				   #\('O'),
				   #\('T'),
				   #\('S')
				 ])
		      ],
		      [u_begin_regular_font, stream],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(\), #\(l), #\(d), #\(o), #\(t), #\(s)])
		      ],
		      [u_end_font, stream],
		      [+, u_column, 3]
		    ],
		    
		    [ 
		      [ u_string_equal_c63,
			string,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(!),
				   #\('O'),
				   #\('R'),
				   #\(-),
				   #\('B'),
				   #\('A'),
				   #\('R')
				 ])
		      ],
		      [u_begin_regular_font, stream],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\($), #\(\), #\(m), #\(i), #\(d), #\($)])
		      ],
		      [u_end_font, stream],
		      [+, u_column, 1]
		    ],
		    
		    [ 
		      [ and,
			
			[ u_string_equal_c63,
			  
			  [ u_string_slice,
			    string,
			    0,
			    [min, [-, [u_string_length, string], 1], 7]
			  ],
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(!),
				     #\('I'),
				     #\('T'),
				     #\('A'),
				     #\('L'),
				     #\('I'),
				     #\('C')
				   ])
			],
			[setq, u_result, [u_string_posq, #\(:), string]]
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('{'),
				   #\(\),
				   #\(e),
				   #\(m),
				   #\('{'),
				   #\('}'),
				   #\(~),
				   #\('A'),
				   #\('}')
				 ]),
			[string_downcase, [u_nthchdr, string, [+, u_result, 1]]]
		      ],
		      [+, u_column, [-, [u_string_length, string], u_result]]
		    ],
		    
		    [ u_else,
		      [u_begin_typewriter_font, stream],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\('\''), #\(~), #\('A')]),
			string
		      ],
		      [u_end_font, stream],
		      [+, u_column, [u_string_length, string]]
		    ]
		  ]
		]
	      ]).


% annotating U::PRINT-SYMBOL 
wl: arglist_info(u_print_symbol,
		[stream, symbol, u_column],
		[Stream_Param, Symbol_Param, Column_Param],
		arginfo{ all:[stream, symbol, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, symbol, u_column],
			 opt:0,
			 req:[stream, symbol, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-SYMBOL 
wl: init_args(exact_only, u_print_symbol).


% annotating U::PRINT-SYMBOL 
f_u_print_symbol(Stream_Param, Symbol_Param, Column_Param, ElseResult50) :-
	Env=[bv(stream, Stream_Param), bv(symbol, Symbol_Param), bv(u_column, Column_Param)],
	f_u_symbol_c62_string(symbol, String_Init),
	LEnv=[[bv(string, String_Init)]|Env],
	f_u_string_equal_c63(string,
			     '$ARRAY'([*],
				      claz_base_character,
				      
				      [ #\(!),
					#\('L'),
					#\('D'),
					#\('O'),
					#\('T'),
					#\('S')
				      ]),
			     IFTEST),
	(   IFTEST\==[]
	->  f_u_begin_regular_font(Stream_Param, Regular_font_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(\), #\(l), #\(d), #\(o), #\(t), #\(s)])
		      ],
		      Format_Ret),
	    f_u_end_font(Stream_Param, End_font_Ret),
	    +(Column_Param, 3, TrueResult53),
	    ElseResult50=TrueResult53
	;   f_u_string_equal_c63(string,
				 '$ARRAY'([*],
					  claz_base_character,
					  
					  [ #\(!),
					    #\('O'),
					    #\('R'),
					    #\(-),
					    #\('B'),
					    #\('A'),
					    #\('R')
					  ]),
				 IFTEST26),
	    (   IFTEST26\==[]
	    ->  f_u_begin_regular_font(Stream_Param, Regular_font_Ret61),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\($), #\(\), #\(m), #\(i), #\(d), #\($)])
			  ],
			  Format_Ret62),
		f_u_end_font(Stream_Param, End_font_Ret63),
		+(Column_Param, 1, TrueResult51),
		ElseResult50=TrueResult51
	    ;   f_u_string_equal_c63(
				     [ u_string_slice,
				       string,
				       0,
				       [min, [-, [u_string_length, string], 1], 7]
				     ],
				     '$ARRAY'([*],
					      claz_base_character,
					      
					      [ #\(!),
						#\('I'),
						#\('T'),
						#\('A'),
						#\('L'),
						#\('I'),
						#\('C')
					      ]),
				     IFTEST34),
		(   IFTEST34\==[]
		->  f_u_string_posq(#\(:), string, TrueResult),
		    set_var(LEnv, u_result, TrueResult),
		    IFTEST32=TrueResult
		;   IFTEST32=[]
		),
		(   IFTEST32\==[]
		->  f_u_nthchdr(string, [+, u_result, 1], String_downcase_Param),
		    cl_string_downcase(String_downcase_Param,
				       String_downcase_Ret),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('{'),
					   #\(\),
					   #\(e),
					   #\(m),
					   #\('{'),
					   #\('}'),
					   #\(~),
					   #\('A'),
					   #\('}')
					 ]),
				String_downcase_Ret
			      ],
			      Format_Ret65),
		    f_u_string_length(string, String_length_Ret),
		    get_var(LEnv, u_result, Result_Get),
		    -(String_length_Ret, Result_Get, _137268),
		    +(Column_Param, _137268, TrueResult49),
		    ElseResult50=TrueResult49
		;   get_var(LEnv, u_else, IFTEST40),
		    (   IFTEST40\==[]
		    ->  f_u_begin_typewriter_font(Stream_Param,
						  Typewriter_font_Ret),
			cl_format(
				  [ Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\('\''), #\(~), #\('A')]),
				    string
				  ],
				  Format_Ret68),
			f_u_end_font(Stream_Param, End_font_Ret69),
			f_u_string_length(string, String_length_Ret70),
			+(Column_Param, String_length_Ret70, TrueResult47),
			ElseResult50=TrueResult47
		    ;   ElseResult50=[]
		    )
		)
	    )
	).
:- set_opv(f_u_print_symbol, classof, claz_function),
   set_opv(u_print_symbol, compile_as, kw_function),
   set_opv(u_print_symbol, function, f_u_print_symbol),
   DefunResult=u_print_symbol.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:25407 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-slanted',
			    [stream, string, column],
			    ['begin-slanted-font', stream],
			    [format, stream, '$STRING'("~A"), string],
			    ['end-font', stream],
			    [+, column, ['string-length', string]]
			  ]).

% annotating U::PRINT-SLANTED 
wl: lambda_def(defun,
	      u_print_slanted,
	      f_u_print_slanted,
	      [stream, string, u_column],
	      
	      [ [u_begin_slanted_font, stream],
		
		[ format,
		  stream,
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		  string
		],
		[u_end_font, stream],
		[+, u_column, [u_string_length, string]]
	      ]).


% annotating U::PRINT-SLANTED 
wl: arglist_info(u_print_slanted,
		[stream, string, u_column],
		[Stream_Param, String_Param, Column_Param],
		arginfo{ all:[stream, string, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, string, u_column],
			 opt:0,
			 req:[stream, string, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-SLANTED 
wl: init_args(exact_only, u_print_slanted).


% annotating U::PRINT-SLANTED 
f_u_print_slanted(Stream_Param, String_Param, Column_Param, FnResult) :-
	Env=[bv(string, String_Param), bv(u_column, Column_Param)],
	f_u_begin_slanted_font(Stream_Param, Slanted_font_Ret),
	cl_format(
		  [ Stream_Param,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    string
		  ],
		  Format_Ret),
	f_u_end_font(Stream_Param, End_font_Ret),
	f_u_string_length(string, String_length_Ret),
	+(Column_Param, String_length_Ret, _133770),
	_133770=FnResult.
:- set_opv(f_u_print_slanted, classof, claz_function),
   set_opv(u_print_slanted, compile_as, kw_function),
   set_opv(u_print_slanted, function, f_u_print_slanted),
   DefunResult=u_print_slanted.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:25569 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-regular',
			    [stream, string, column],
			    ['begin-slanted-font', stream],
			    [format, stream, '$STRING'("~A"), string],
			    ['end-font', stream],
			    [+, column, ['string-length', string]]
			  ]).

% annotating U::PRINT-REGULAR 
wl: lambda_def(defun,
	      u_print_regular,
	      f_u_print_regular,
	      [stream, string, u_column],
	      
	      [ [u_begin_slanted_font, stream],
		
		[ format,
		  stream,
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		  string
		],
		[u_end_font, stream],
		[+, u_column, [u_string_length, string]]
	      ]).


% annotating U::PRINT-REGULAR 
wl: arglist_info(u_print_regular,
		[stream, string, u_column],
		[Stream_Param, String_Param, Column_Param],
		arginfo{ all:[stream, string, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, string, u_column],
			 opt:0,
			 req:[stream, string, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-REGULAR 
wl: init_args(exact_only, u_print_regular).


% annotating U::PRINT-REGULAR 
f_u_print_regular(Stream_Param, String_Param, Column_Param, FnResult) :-
	Env=[bv(string, String_Param), bv(u_column, Column_Param)],
	f_u_begin_slanted_font(Stream_Param, Slanted_font_Ret),
	cl_format(
		  [ Stream_Param,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    string
		  ],
		  Format_Ret),
	f_u_end_font(Stream_Param, End_font_Ret),
	f_u_string_length(string, String_length_Ret),
	+(Column_Param, String_length_Ret, _133808),
	_133808=FnResult.
:- set_opv(f_u_print_regular, classof, claz_function),
   set_opv(u_print_regular, compile_as, kw_function),
   set_opv(u_print_regular, function, f_u_print_regular),
   DefunResult=u_print_regular.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:25731 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-typewriter',
			    [stream, string, column],
			    ['begin-typewriter-font', stream],
			    [format, stream, '$STRING'("~A"), string],
			    ['end-font', stream],
			    [+, column, ['string-length', string]]
			  ]).

% annotating U::PRINT-TYPEWRITER 
wl: lambda_def(defun,
	      u_print_typewriter,
	      f_u_print_typewriter,
	      [stream, string, u_column],
	      
	      [ [u_begin_typewriter_font, stream],
		
		[ format,
		  stream,
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		  string
		],
		[u_end_font, stream],
		[+, u_column, [u_string_length, string]]
	      ]).


% annotating U::PRINT-TYPEWRITER 
wl: arglist_info(u_print_typewriter,
		[stream, string, u_column],
		[Stream_Param, String_Param, Column_Param],
		arginfo{ all:[stream, string, u_column],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, string, u_column],
			 opt:0,
			 req:[stream, string, u_column],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-TYPEWRITER 
wl: init_args(exact_only, u_print_typewriter).


% annotating U::PRINT-TYPEWRITER 
f_u_print_typewriter(Stream_Param, String_Param, Column_Param, FnResult) :-
	Env=[bv(string, String_Param), bv(u_column, Column_Param)],
	f_u_begin_typewriter_font(Stream_Param, Typewriter_font_Ret),
	cl_format(
		  [ Stream_Param,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    string
		  ],
		  Format_Ret),
	f_u_end_font(Stream_Param, End_font_Ret),
	f_u_string_length(string, String_length_Ret),
	+(Column_Param, String_length_Ret, _133846),
	_133846=FnResult.
:- set_opv(f_u_print_typewriter, classof, claz_function),
   set_opv(u_print_typewriter, compile_as, kw_function),
   set_opv(u_print_typewriter, function, f_u_print_typewriter),
   DefunResult=u_print_typewriter.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:25899 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'capitalize!',
			    [string],
			    ['string-downcase!', [chdr, string]],
			    string
			  ]).

% annotating U::CAPITALIZE! 
wl: lambda_def(defun,
	      u_capitalize_c33,
	      f_u_capitalize_c33,
	      [string],
	      [[u_string_downcase_c33, [u_chdr, string]], string]).


% annotating U::CAPITALIZE! 
wl: arglist_info(u_capitalize_c33,
		[string],
		[String_Param],
		arginfo{ all:[string],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[string],
			 opt:0,
			 req:[string],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CAPITALIZE! 
wl: init_args(exact_only, u_capitalize_c33).


% annotating U::CAPITALIZE! 
f_u_capitalize_c33(String_Param, FnResult) :-
	Env=[bv(string, String_Param)],
	f_u_string_downcase_c33([u_chdr, string], Downcase_c33_Ret),
	string=FnResult.
:- set_opv(f_u_capitalize_c33, classof, claz_function),
   set_opv(u_capitalize_c33, compile_as, kw_function),
   set_opv(u_capitalize_c33, function, f_u_capitalize_c33),
   DefunResult=u_capitalize_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:25989 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'interesting-obname?',
			    ['obname-str'],
			    
			    [ not,
			      
			      [ 'string-equal?',
				
				[ 'string-slice',
				  'obname-str',
				  0,
				  [min, [-, ['string-length', 'obname-str'], 1], 2]
				],
				'$STRING'("OB")
			      ]
			    ]
			  ]).

% annotating U::INTERESTING-OBNAME? 
wl: lambda_def(defun,
	      u_interesting_obname_c63,
	      f_u_interesting_obname_c63,
	      [u_obname_str],
	      
	      [ 
		[ not,
		  
		  [ u_string_equal_c63,
		    
		    [ u_string_slice,
		      u_obname_str,
		      0,
		      [min, [-, [u_string_length, u_obname_str], 1], 2]
		    ],
		    '$ARRAY'([*], claz_base_character, [#\('O'), #\('B')])
		  ]
		]
	      ]).


% annotating U::INTERESTING-OBNAME? 
wl: arglist_info(u_interesting_obname_c63,
		[u_obname_str],
		[Obname_str_Param],
		arginfo{ all:[u_obname_str],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obname_str],
			 opt:0,
			 req:[u_obname_str],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INTERESTING-OBNAME? 
wl: init_args(exact_only, u_interesting_obname_c63).


% annotating U::INTERESTING-OBNAME? 
f_u_interesting_obname_c63(Obname_str_Param, FnResult) :-
	Env=[bv(u_obname_str, Obname_str_Param)],
	f_u_string_equal_c63(
			     [ u_string_slice,
			       u_obname_str,
			       0,
			       [min, [-, [u_string_length, u_obname_str], 1], 2]
			     ],
			     '$ARRAY'([*],
				      claz_base_character,
				      [#\('O'), #\('B')]),
			     Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(f_u_interesting_obname_c63, classof, claz_function),
   set_opv(u_interesting_obname_c63, compile_as, kw_function),
   set_opv(u_interesting_obname_c63, function, f_u_interesting_obname_c63),
   DefunResult=u_interesting_obname_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:26223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'has-interesting-obname?',
			    [ob],
			    
			    [ 'interesting-obname?',
			      ['symbol->string', ['ob$name', ob]]
			    ]
			  ]).

% annotating U::HAS-INTERESTING-OBNAME? 
wl: lambda_def(defun,
	      u_has_interesting_obname_c63,
	      f_u_has_interesting_obname_c63,
	      [u_ob],
	      
	      [ 
		[ u_interesting_obname_c63,
		  [u_symbol_c62_string, [u_ob_c36_name, u_ob]]
		]
	      ]).


% annotating U::HAS-INTERESTING-OBNAME? 
wl: arglist_info(u_has_interesting_obname_c63,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HAS-INTERESTING-OBNAME? 
wl: init_args(exact_only, u_has_interesting_obname_c63).


% annotating U::HAS-INTERESTING-OBNAME? 
f_u_has_interesting_obname_c63(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_symbol_c62_string([u_ob_c36_name, u_ob], Obname_c63_Param),
	f_u_interesting_obname_c63(Obname_c63_Param, Obname_c63_Ret),
	Obname_c63_Ret=FnResult.
:- set_opv(f_u_has_interesting_obname_c63, classof, claz_function),
   set_opv(u_has_interesting_obname_c63, compile_as, kw_function),
   set_opv(u_has_interesting_obname_c63,
	   function,
	   f_u_has_interesting_obname_c63),
   DefunResult=u_has_interesting_obname_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:26315 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob->list',
			    [ob],
			    
			    [ cond,
			      
			      [ ['ob?', ob],
				
				[ yloop,
				  
				  [ initial,
				    
				    [ result,
				      
				      [ list,
					['ob$name', ['ob$get', ob, [quote, type]]]
				      ]
				    ]
				  ],
				  [yfor, 'slot-name', in, ['ob$slot-names', ob]],
				  
				  [ ydo,
				    
				    [ if,
				      ['neq?', 'slot-name', [quote, type]],
				      
				      [ setq,
					result,
					
					[ append,
					  result,
					  
					  [ cons,
					    'slot-name',
					    
					    [ map,
					      [quote, list],
					      [quote, 'ob->list1'],
					      ['ob$gets', ob, 'slot-name']
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ],
			      [[atom, ob], ob],
			      [else, [map, [quote, list], [quote, 'ob->list1'], ob]]
			    ]
			  ]).

% annotating U::OB->LIST 
wl: lambda_def(defun,
	      u_ob_c62_list,
	      f_u_ob_c62_list,
	      [u_ob],
	      
	      [ 
		[ cond,
		  
		  [ [u_ob_c63, u_ob],
		    
		    [ u_yloop,
		      
		      [ u_initial,
			
			[ u_result,
			  
			  [ list,
			    [u_ob_c36_name, [u_ob_c36_get, u_ob, [quote, type]]]
			  ]
			]
		      ],
		      [u_yfor, u_slot_name, u_in, [u_ob_c36_slot_names, u_ob]],
		      
		      [ u_ydo,
			
			[ if,
			  [u_neq_c63, u_slot_name, [quote, type]],
			  
			  [ setq,
			    u_result,
			    
			    [ append,
			      u_result,
			      
			      [ cons,
				u_slot_name,
				
				[ map,
				  [quote, list],
				  [quote, u_ob_c62_list1],
				  [u_ob_c36_gets, u_ob, u_slot_name]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ],
		  [[atom, u_ob], u_ob],
		  [u_else, [map, [quote, list], [quote, u_ob_c62_list1], u_ob]]
		]
	      ]).


% annotating U::OB->LIST 
wl: arglist_info(u_ob_c62_list,
		[u_ob],
		[Ob_Get22],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB->LIST 
wl: init_args(exact_only, u_ob_c62_list).


% annotating U::OB->LIST 
f_u_ob_c62_list(Ob_Get22, ElseResult26) :-
	Env=[bv(u_ob, Ob_Get22)],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ 
			[ u_initial,
			  
			  [ u_result,
			    
			    [ list,
			      [u_ob_c36_name, [u_ob_c36_get, u_ob, [quote, type]]]
			    ]
			  ]
			],
			[u_yfor, u_slot_name, u_in, [u_ob_c36_slot_names, u_ob]],
			
			[ u_ydo,
			  
			  [ if,
			    [u_neq_c63, u_slot_name, [quote, type]],
			    
			    [ setq,
			      u_result,
			      
			      [ append,
				u_result,
				
				[ cons,
				  u_slot_name,
				  
				  [ map,
				    [quote, list],
				    [quote, u_ob_c62_list1],
				    [u_ob_c36_gets, u_ob, u_slot_name]
				  ]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      TrueResult27),
	    ElseResult26=TrueResult27
	;   Ob_Get22\=[CAR|CDR]
	->  ElseResult26=Ob_Get22
	;   get_var(Env, u_else, IFTEST19),
	    (   IFTEST19\==[]
	    ->  cl_map(list, u_ob_c62_list1, Ob_Get22, TrueResult),
		ElseResult26=TrueResult
	    ;   ElseResult26=[]
	    )
	).
:- set_opv(f_u_ob_c62_list, classof, claz_function),
   set_opv(u_ob_c62_list, compile_as, kw_function),
   set_opv(u_ob_c62_list, function, f_u_ob_c62_list),
   DefunResult=u_ob_c62_list.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:26738 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob->list1',
			    [ob],
			    
			    [ cond,
			      [['ob?', ob], ['ob$name', ob]],
			      [[atom, ob], ob],
			      [else, [map, [quote, list], [quote, 'ob->list'], ob]]
			    ]
			  ]).

% annotating U::OB->LIST1 
wl: lambda_def(defun,
	      u_ob_c62_list1,
	      f_u_ob_c62_list1,
	      [u_ob],
	      
	      [ 
		[ cond,
		  [[u_ob_c63, u_ob], [u_ob_c36_name, u_ob]],
		  [[atom, u_ob], u_ob],
		  [u_else, [map, [quote, list], [quote, u_ob_c62_list], u_ob]]
		]
	      ]).


% annotating U::OB->LIST1 
wl: arglist_info(u_ob_c62_list1,
		[u_ob],
		[Ob_Get23],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB->LIST1 
wl: init_args(exact_only, u_ob_c62_list1).


% annotating U::OB->LIST1 
f_u_ob_c62_list1(Ob_Get23, ElseResult27) :-
	Env=[bv(u_ob, Ob_Get23)],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_name(Ob_Get23, TrueResult28),
	    ElseResult27=TrueResult28
	;   Ob_Get23\=[CAR|CDR]
	->  ElseResult27=Ob_Get23
	;   get_var(Env, u_else, IFTEST20),
	    (   IFTEST20\==[]
	    ->  cl_map(list, u_ob_c62_list, Ob_Get23, TrueResult),
		ElseResult27=TrueResult
	    ;   ElseResult27=[]
	    )
	).
:- set_opv(f_u_ob_c62_list1, classof, claz_function),
   set_opv(u_ob_c62_list1, compile_as, kw_function),
   set_opv(u_ob_c62_list1, function, f_u_ob_c62_list1),
   DefunResult=u_ob_c62_list1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_read_pr.cl:26738 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 26852)).
:- true.


% Total time: 11.294 seconds

