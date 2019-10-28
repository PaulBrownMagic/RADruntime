
:- object(radruntime).

    :- public(sit/1).
    :- dynamic(sit/1).
    :- public(init_sit/1).
    init_sit(S) :-
        asserta(sit(S)).
    :- private(clobber_sit/1).
    clobber_sit(S) :-
        retractall(sit(_)),
        asserta(sit(S)).

    :- public(update/1).
    update(A) :-
        ::sit(S),
        A::do(S, S1),
        clobber_sit(S1).

:- end_object.


:- object(view,
    implements(monitoring)).

    after(radruntime, update(Action), _Sender) :-
        format("Did ~q~n", Action),
        radruntime::sit(S),
        ::render(S).

    :- public(render/1).
    render(S) :-
        writeln('Fluents'),
        forall(situation::holds(F, S), writeln(F)),
        nl,
        writeln('Actions'),
        forall(situation::poss(A, S), writeln(A)).

:- end_object.

