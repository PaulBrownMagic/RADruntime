:- initialization((
    logtalk_load([ my_library(sitcalc)
                 , rapp
                 , meta(loader)
                 , random(loader)
                 , 'SitCalc_tictactoe' %_example
                 ]),
    define_events(after, sm, do(_), _, unicode_terminal)
             )).
