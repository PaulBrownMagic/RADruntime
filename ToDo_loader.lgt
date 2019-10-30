:- if((
	current_logtalk_flag(prolog_dialect, swi),
	current_prolog_flag(gui, true)
)).


    :- initialization((
		logtalk_load_context(directory, Directory),
		atom_concat(Directory, xpce_hooks, Path),
		consult(Path),
        logtalk_load([ my_library(sitcalc)
                     , os(loader)
                     , rapp
                     , persistency
                     , todo
                     ]),
        define_events(after, sm, do(_), _, xpce_view),
        define_events(after, sm, do(_), _, persistency)
                 )).

:- else.

	:- initialization((
		write('(this example requires SWI-Prolog as the backend compiler)'), nl
	)).

:- endif.
