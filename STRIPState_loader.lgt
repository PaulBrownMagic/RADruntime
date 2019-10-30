:- initialization((
    logtalk_load([ my_library(stripstate)
                 , rapp
                 , random(loader)
                 , 'STRIPState_tictactoe' %_example
                 ]),
    define_events(after, sm, do(_), _, unicode_terminal)
             )).
