:- if((
	current_logtalk_flag(prolog_dialect, swi),
	current_prolog_flag(gui, true)
)).


    :- initialization((
		logtalk_load_context(directory, Directory),
		atom_concat(Directory, xpce_hooks, XPath),
		atom_concat(Directory, web_hooks, WPath),
		consult(XPath),
		consult(WPath),
        logtalk_load([ sitcalc(loader)
                     , os(loader)
                     , random(loader)
                     , rapp
                     , persistency
                     , todo
                     ]),
        define_events(after, sm, do(_), _, todo_view),
        define_events(after, sm, do(_), _, persistency),
        persistency::restore(_),
        logtalk_load([ todo_xpce
                     , todo_web
                     ]),
        server::serve,
        app::init
                 )).

:- else.

	:- initialization((
		write('(this example requires SWI-Prolog as the backend compiler)'), nl
	)).

:- endif.
