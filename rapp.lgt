:- object(sm).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/27
            , comment is 'A Situation Manager.'
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
    :- synchronized(do/1).
    :- mode(do(+object), zero_or_one).
    :- info(do/1,
        [ comment is 'Do the Action in the application, thread-safe.'
        , argnames is ['Action']
        ]).
    do(A) :-
        ::sit(S),
        do(A, S).

    :- public(do/2).
    do(A, S) :-
        A::do(S, S1),
        clobber_sit(S1).

:- end_object.


:- category(actorc).
    :- public(action/1).

    :- set_logtalk_flag(events, allow).

    :- public(do/1).
    :- meta_predicate(do(2)).
    do(A) :-
        ::action(A),
        sm::do(A).

    :- public(do/2).
    :- meta_predicate(do(2, *)).
    do(A, Sit) :-
        ::action(A),
        sm::do(A, Sit).

:- end_category.


:- category(fluentc).

    :- public(holds/1).
    :- meta_predicate(holds(1)).
    holds(Fluent) :-
        sm::sit(S),
        call(::Fluent, S).

:- end_category.


:- category(view,
    implements(monitoring)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/27
            , comment is 'A prototype for rapid app development views.'
            ]).

    % Monitor for actions being done in the application and upate the view
    after(sm, do(_), _Sender) :-
        sm::sit(S),
        ::render(S).

    :- public(render/1).
    :- mode(render(+term), zero_or_one).
    :- info(render/1,
        [ comment is 'Render the given Situation into the chosen view.'
        , argnames is ['Situation']
        ]).

:- end_category.
