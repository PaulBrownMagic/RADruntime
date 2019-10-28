:- initialization((
    logtalk_load([ my_library(sitcalc)
                 , rapp
                 , random(loader)
                 , tictactoe_example
                 ]),
    set_logtalk_flag(events, allow),
    define_events(after, rapp, do(_), _, unicode_terminal)
             )).
