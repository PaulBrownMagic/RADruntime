:- initialization((
    logtalk_load([ my_library(sitcalc)
                 , my_library('SitCalc/simple_example')
                 , radruntime
                 ]),
    set_logtalk_flag(events, allow),
    define_events(after, radruntime, update(_), _, view)
             )).
