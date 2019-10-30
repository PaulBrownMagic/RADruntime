:- initialization((
    logtalk_load([ my_library(sitcalc)
                 , rapp
                 , random(loader)
                 , alt_tictactoe %_example
                 ]),
    define_events(after, sm, do(_), _, unicode_terminal)
             )).
