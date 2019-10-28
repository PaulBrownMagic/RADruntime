:- object(rapp).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/27
            , comment is 'A prototype for rapid app development.'
            ]).

    :- public(sit/1).
    :- dynamic(sit/1).
    :- mode(sit(?term), zero_or_one).
    :- info(sit/1,
        [ comment is 'The current situation of the application.'
        , argnames is ['Situation']
        ]).

    :- public(init_sit/1).
    :- mode(init_sit(+term), one).
    :- mode(init_sit(-term), zero).
    :- info(init_sit/1,
        [ comment is 'Initialize the application with the given situation, thread-safe.'
        , argnames is ['Situation']
        ]).
    :- synchronized(init/1).
    init_sit(S) :-
        nonvar(S),
        asserta(sit(S)).

    :- private(clobber_sit/1).
    clobber_sit(S) :-
        retractall(sit(_)),
        asserta(sit(S)).

    :- public(do/1).
%:- synchronized(do/1).
    :- mode(do(+object), zero_or_one).
    :- info(do/1,
        [ comment is 'Do the Action in the application, thread-safe.'
        , argnames is ['Action']
        ]).
    do(A) :-
        ::sit(S),
        A::do(S, S1),
        clobber_sit(S1).

:- end_object.


:- object(view,
    implements(monitoring)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/27
            , comment is 'A prototype for rapid app development views.'
            ]).

    % Monitor for actions being done in the application and upate the view
    after(rapp, do(Action), _Sender) :-
        format("Did ~q~n", Action),
        rapp::sit(S),
        ::render(S).

    :- public(render/1).
    :- mode(render(+term), zero_or_one).
    :- info(render/1,
        [ comment is 'Render the given Situation into the chosen view.'
        , argnames is ['Situation']
        ]).

:- end_object.
