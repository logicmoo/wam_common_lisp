:- module(setup_lm_packs,
          [ setup_lm_packs/0,
            wamcl_check_packs/0,
            wamcl_required_packs/1,
            wamcl_local_libs_dir/1
          ]).

/*******************************************************************
 *
 * setup_lm_packs.pl
 *
 * Make WAM-CL's Prolog pack dependencies available *without* relying on
 * a user or global (~/AppData or ~/.local) pack installation.
 *
 * The required packs are bundled inside this repository under  <repo>/libs/
 * (one sub-directory per pack).  This file registers that directory as a
 * pack search path, attaches the packs found there, and then checks that
 * every required pack actually resolved.  If a pack is still missing it
 * falls back to installing it from the web.
 *
 *******************************************************************/

:- use_module(library(prolog_pack)).

:- multifile(user:file_search_path/2).
:- dynamic(user:file_search_path/2).
:- dynamic(setup_lm_packs:wamcl_setup_dir/1).

% Remember the directory this file was loaded from (prolog/wam_cl/).
:- ( prolog_load_context(directory, Dir)
   -> retractall(setup_lm_packs:wamcl_setup_dir(_)),
      asserta(setup_lm_packs:wamcl_setup_dir(Dir))
   ; true ).

%!  wamcl_required_packs(-Packs) is det.
%   The packs WAM-CL needs in order to load.
wamcl_required_packs([predicate_streams, dictoo, logicmoo_utils]).

%!  wamcl_local_libs_dir(-Dir) is semidet.
%   Absolute path to the bundled  <repo>/libs/  directory, computed
%   relative to this source file (prolog/wam_cl/ -> ../../libs).
wamcl_local_libs_dir(Libs) :-
    setup_lm_packs:wamcl_setup_dir(Here),
    absolute_file_name('../../libs', Libs,
        [ relative_to(Here), file_type(directory), file_errors(fail) ]),
    exists_directory(Libs),
    !.

%!  wamcl_add_local_packs is det.
%   Register the bundled libs/ dir as a pack path and attach its packs.
wamcl_add_local_packs :-
    ( wamcl_local_libs_dir(Libs)
    -> ( \+ user:file_search_path(pack, Libs)
       -> asserta(user:file_search_path(pack, Libs))
       ; true ),
       catch(attach_packs(Libs, [duplicate(replace)]),
             _,
             catch(attach_packs, _, true)),
       print_message(informational,
                     format('WAM-CL: using bundled packs from ~w', [Libs]))
    ;  print_message(warning,
                     format('WAM-CL: bundled libs/ dir not found; '+
                            'falling back to user/global packs', []))
    ).

%!  wamcl_pack_available(+Pack) is semidet.
%   True when Pack is attached/known to the pack system.
wamcl_pack_available(Pack) :-
    ( catch(prolog_pack:current_pack(Pack), _, fail) -> true
    ; catch(pack_property(Pack, version(_)), _, fail)
    ).

%!  wamcl_check_pack(+Pack) is det.
%   Report on one required pack; try a web install if it is missing.
wamcl_check_pack(Pack) :-
    ( wamcl_pack_available(Pack)
    -> print_message(informational, format('WAM-CL: pack ~w ... OK', [Pack]))
    ;  print_message(warning,
                     format('WAM-CL: pack ~w NOT found - attempting install', [Pack])),
       ( catch(pack_install(Pack, [interactive(false), upgrade(true)]), _, fail),
         wamcl_pack_available(Pack)
       -> print_message(informational, format('WAM-CL: pack ~w installed', [Pack]))
       ;  print_message(warning, format('WAM-CL: pack ~w still missing', [Pack]))
       )
    ).

%!  wamcl_check_packs is det.
%   Attach bundled packs then verify all required packs are present.
wamcl_check_packs :-
    wamcl_add_local_packs,
    wamcl_required_packs(Packs),
    forall(member(Pack, Packs), wamcl_check_pack(Pack)).

%!  setup_lm_packs is det.
setup_lm_packs :- wamcl_check_packs.

:- initialization(wamcl_check_packs, now).
