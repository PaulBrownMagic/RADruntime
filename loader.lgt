:- initialization((
    logtalk_load([ my_library(sitcalc)
                 , rapp
                 , random(loader)
                 , tictactoe_example
                 ]),
    define_events(after, sm, do(_), _, unicode_terminal)
             )).
