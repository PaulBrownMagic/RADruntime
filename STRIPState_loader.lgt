:- initialization((
    logtalk_load([ stripstate(loader)
                 , rapp
                 , random(loader)
                 , 'STRIPState_tictactoe' %_example
                 ]),
    define_events(after, sm, do(_), _, unicode_terminal)
             )).
