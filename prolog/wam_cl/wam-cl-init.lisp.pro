
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "../prolog/wam_cl/wam-cl-init" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
% Start time: Wed Dec 20 13:05:20 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user, [quote, ['in-package', '#:system']]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:28 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defpackage,
			    '$STRING'("SYSTEM"),
			    [':nicknames', '$STRING'("SYS")]
			  ]).
:- cl_defpackage('$ARRAY'([*],
			  claz_base_character,
			  [#\('S'), #\('Y'), #\('S'), #\('T'), #\('E'), #\('M')]),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*],
			      claz_base_character,
			      [#\('S'), #\('Y'), #\('S')])
		   ]
		 ],
		 _Ignored5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:70 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defpackage,
			    '$STRING'("COMMON-LISP"),
			    [':nicknames', '$STRING'("CL"), '$STRING'("LISP")],
			    [':uses', '$STRING'("SYSTEM")]
			  ]).
:- cl_defpackage('$ARRAY'([*],
			  claz_base_character,
			  
			  [ #\('C'),
			    #\('O'),
			    #\('M'),
			    #\('M'),
			    #\('O'),
			    #\('N'),
			    #\(-),
			    #\('L'),
			    #\('I'),
			    #\('S'),
			    #\('P')
			  ]),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, [#\('C'), #\('L')]),
		     '$ARRAY'([*],
			      claz_base_character,
			      [#\('L'), #\('I'), #\('S'), #\('P')])
		   ],
		   
		   [ kw_uses,
		     '$ARRAY'([*],
			      claz_base_character,
			      
			      [ #\('S'),
				#\('Y'),
				#\('S'),
				#\('T'),
				#\('E'),
				#\('M')
			      ])
		   ]
		 ],
		 _Ignored5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:139 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defpackage,
			    '$STRING'("COMMON-LISP-USER"),
			    
			    [ ':nicknames',
			      '$STRING'("U"),
			      '$STRING'("USER"),
			      '$STRING'("CL-USER")
			    ],
			    [':uses', '$STRING'("COMMON-LISP")]
			  ]).
:- cl_defpackage('$ARRAY'([*],
			  claz_base_character,
			  
			  [ #\('C'),
			    #\('O'),
			    #\('M'),
			    #\('M'),
			    #\('O'),
			    #\('N'),
			    #\(-),
			    #\('L'),
			    #\('I'),
			    #\('S'),
			    #\('P'),
			    #\(-),
			    #\('U'),
			    #\('S'),
			    #\('E'),
			    #\('R')
			  ]),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, [#\('U')]),
		     '$ARRAY'([*],
			      claz_base_character,
			      [#\('U'), #\('S'), #\('E'), #\('R')]),
		     '$ARRAY'([*],
			      claz_base_character,
			      
			      [ #\('C'),
				#\('L'),
				#\(-),
				#\('U'),
				#\('S'),
				#\('E'),
				#\('R')
			      ])
		   ],
		   
		   [ kw_uses,
		     '$ARRAY'([*],
			      claz_base_character,
			      
			      [ #\('C'),
				#\('O'),
				#\('M'),
				#\('M'),
				#\('O'),
				#\('N'),
				#\(-),
				#\('L'),
				#\('I'),
				#\('S'),
				#\('P')
			      ])
		   ]
		 ],
		 _Ignored5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:228 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defvar, '*lisp-file-type*', '$STRING'("lisp")]).
:- set_var(TLEnv6,
	   defvar,
	   u_xx_lisp_file_type_xx,
	   '$ARRAY'([*], claz_base_character, [#\(l), #\(i), #\(s), #\(p)])).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:263 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defvar,
			    '*default-pathname-defaults*',
			    '$OBJ'(claz_pathname, "")
			  ]).
:- set_var(TLEnv6,
	   defvar,
	   xx_default_pathname_defaults_xx,
	   '$OBJ'(claz_pathname, "")).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:309 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    dd,
			    [],
			    
			    [ let,
			      
			      [ ['*lisp-file-type*', '$STRING'("cl")],
				
				[ '*default-pathname-defaults*',
				  ['merge-pathnames', '$STRING'("daydreamer/")]
				]
			      ],
			      [load, '$STRING'("dd")]
			    ]
			  ]).

% annotating U::DD 
wl: lambda_def(defun,
	      u_dd,
	      f_u_dd,
	      [],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_xx_lisp_file_type_xx,
		      '$ARRAY'([*], claz_base_character, [#\(c), #\(l)])
		    ],
		    
		    [ xx_default_pathname_defaults_xx,
		      
		      [ merge_pathnames,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(d),
				   #\(a),
				   #\(y),
				   #\(d),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(m),
				   #\(e),
				   #\(r),
				   #\(/)
				 ])
		      ]
		    ]
		  ],
		  [load, '$ARRAY'([*], claz_base_character, [#\(d), #\(d)])]
		]
	      ]).


% annotating U::DD 
wl: arglist_info(u_dd,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DD 
wl: init_args(exact_only, u_dd).


% annotating U::DD 
f_u_dd(FnResult) :-
	Env=[],
	cl_merge_pathnames('$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(d),
				      #\(a),
				      #\(y),
				      #\(d),
				      #\(r),
				      #\(e),
				      #\(a),
				      #\(m),
				      #\(e),
				      #\(r),
				      #\(/)
				    ]),
			   Xx_default_pathname_defaults_xx_Init),
	LEnv=[[bv(u_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, [#\(c), #\(l)]))]|Env],
	save_special(sv(xx_default_pathname_defaults_xx,
			Xx_default_pathname_defaults_xx_Init,
			value,
			Value)),
	cl_load('$ARRAY'([*], claz_base_character, [#\(d), #\(d)]), [], Load_Ret),
	restore_special(sv(xx_default_pathname_defaults_xx,
			   Xx_default_pathname_defaults_xx_Init,
			   value,
			   Value)),
	LetResult=Load_Ret,
	LetResult=FnResult.
:- set_opv(f_u_dd, classof, claz_function),
   set_opv(u_dd, compile_as, kw_function),
   set_opv(u_dd, function, f_u_dd),
   _Ignored5=u_dd.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:447 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'show-ascii-art',
			    [],
			    ['write-line', '$STRING'("  __________    ")],
			    ['write-line', '$STRING'(" / ___  ___ \\   ")],
			    ['write-line', '$STRING'("/ / @ \\/ @ \\ \\  ")],
			    ['write-line', '$STRING'("\\ \\___/\\___/ /\\ ")],
			    ['write-line', '$STRING'(" \\____\\/____/|| ")],
			    ['write-line', '$STRING'(" /     /\\\\\\\\\\// ")],
			    ['write-line', '$STRING'("|     |\\\\\\\\\\\\   ")],
			    
			    [ 'write-line',
			      '$STRING'(" \\      \\\\\\\\\\\\  ")
			    ],
			    ['write-line', '$STRING'("   \\______/\\\\\\\\ ")],
			    ['write-line', '$STRING'("    _||_||_     ")],
			    ['write-line', '$STRING'("                ")]
			  ]).

% annotating U::SHOW-ASCII-ART 
wl: lambda_def(defun,
	      u_show_ascii_art,
	      f_u_show_ascii_art,
	      [],
	      
	      [ 
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(' '),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(/),
			     #\(' '),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(' '),
			     #\(' '),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(' '),
			     #\(\),
			     #\(' '),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(/),
			     #\(' '),
			     #\(/),
			     #\(' '),
			     #\(@),
			     #\(' '),
			     #\(\),
			     #\(/),
			     #\(' '),
			     #\(@),
			     #\(' '),
			     #\(\),
			     #\(' '),
			     #\(\),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(\),
			     #\(' '),
			     #\(\),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(/),
			     #\(\),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(/),
			     #\(' '),
			     #\(/),
			     #\(\),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(\),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(\),
			     #\(/),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(/),
			     #\('|'),
			     #\('|'),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(/),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(/),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(/),
			     #\(/),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('|'),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\('|'),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(' '),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(\),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(\),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\('_'),
			     #\(/),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(\),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\('_'),
			     #\('|'),
			     #\('|'),
			     #\('_'),
			     #\('|'),
			     #\('|'),
			     #\('_'),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' ')
			   ])
		],
		
		[ write_line,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' '),
			     #\(' ')
			   ])
		]
	      ]).


% annotating U::SHOW-ASCII-ART 
wl: arglist_info(u_show_ascii_art,
		[],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[],
			 opt:0,
			 req:0,
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SHOW-ASCII-ART 
wl: init_args(exact_only, u_show_ascii_art).


% annotating U::SHOW-ASCII-ART 
f_u_show_ascii_art(FnResult) :-
	Env=[],
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(' '),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(/),
				 #\(' '),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(' '),
				 #\(' '),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(' '),
				 #\(\),
				 #\(' '),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret16),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(/),
				 #\(' '),
				 #\(/),
				 #\(' '),
				 #\(@),
				 #\(' '),
				 #\(\),
				 #\(/),
				 #\(' '),
				 #\(@),
				 #\(' '),
				 #\(\),
				 #\(' '),
				 #\(\),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret17),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(\),
				 #\(' '),
				 #\(\),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(/),
				 #\(\),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(/),
				 #\(' '),
				 #\(/),
				 #\(\),
				 #\(' ')
			       ]),
		      Write_line_Ret18),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(\),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(\),
				 #\(/),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(/),
				 #\('|'),
				 #\('|'),
				 #\(' ')
			       ]),
		      Write_line_Ret19),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(/),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(/),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(/),
				 #\(/),
				 #\(' ')
			       ]),
		      Write_line_Ret20),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('|'),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\('|'),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(' '),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret21),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(\),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret22),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(\),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\('_'),
				 #\(/),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(\),
				 #\(' ')
			       ]),
		      Write_line_Ret23),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\('_'),
				 #\('|'),
				 #\('|'),
				 #\('_'),
				 #\('|'),
				 #\('|'),
				 #\('_'),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret24),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' '),
				 #\(' ')
			       ]),
		      Write_line_Ret25),
	Write_line_Ret25=FnResult.
:- set_opv(f_u_show_ascii_art, classof, claz_function),
   set_opv(u_show_ascii_art, compile_as, kw_function),
   set_opv(u_show_ascii_art, function, f_u_show_ascii_art),
   _Ignored5=u_show_ascii_art.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:882 **********************/
:- lisp_compile_to_prolog(pkg_user, ['show-ascii-art']).
:- f_u_show_ascii_art(_Ignored5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:900 **********************/
:- lisp_compile_to_prolog(pkg_user, [load, '$STRING'("wam-cl-init-1")]).
:- cl_load('$ARRAY'([*],
		    claz_base_character,
		    
		    [ #\(w),
		      #\(a),
		      #\(m),
		      #\(-),
		      #\(c),
		      #\(l),
		      #\(-),
		      #\(i),
		      #\(n),
		      #\(i),
		      #\(t),
		      #\(-),
		      #\('1')
		    ]),
	   [],
	   _Ignored5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:924 **********************/
:- lisp_compile_to_prolog(pkg_user, [quote, [load, '$STRING'("wam-cl-init2")]]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:948 **********************/
:- lisp_compile_to_prolog(pkg_user, [quote, [load, '$STRING'("wam-cl-init3")]]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:972 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [quote, ['write-line', '$STRING'(" WAM CommonLisp ")]]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1006 **********************/
:- lisp_compile_to_prolog(pkg_user, [quote, ['read-eval-print-loop']]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1006 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) ",
				     0,
				     1045)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1006 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbolinfo('~s','~s').~%\"\r\n    sn (package-name (symbol-package sym))\r\n    (constantp sym)\r\n    (special-operator-p sym)\r\n    (symbol-plist sym)\r\n    sn (symbol-package sym)\r\n    (if (boundp sym) (symbol-value sym))\r\n    (if (fboundp sym) (type-of (symbol-function sym)))\r\n    (fboundp sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n\r\n\r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbol_package('~a','~a').~%\"\r\n    sn (package-name (symbol-package sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n",
				     2,
				     1040)).
:- true.


% Total time: 7.222 seconds

