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
                     , todo
                     ]),
        persistent_manager::new(sm, 'todo_storage.pl'),
        define_events(after, _, do(_), _, todo_view),
        define_events(after, _, do(_), _, persistent_manager),
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
