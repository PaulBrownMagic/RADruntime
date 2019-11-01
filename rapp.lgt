:- object(metaclass,
    instantiates(metaclass)).

    :- protected(instantiate/2).
    instantiate(Instance, Clauses) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], Clauses).

    :- public(only/1).
    only(Inst) :-
        self(Self),
        findall(Inst, instantiates_class(Inst, Self), [Inst]).

:- end_object.


:- category(sit_man).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/27
            , comment is 'A Situation Manager.'
            ]).

    :- private(sit_/1).
    :- dynamic(sit_/1).

    :- public(sit/1).
    :- mode(sit(?term), zero_or_one).
    :- info(sit/1,
        [ comment is 'The current situation of the application.'
        , argnames is ['Situation']
        ]).
    sit(S) :-
        ::sit_(S).

   :- private(clobber_sit/1).
    clobber_sit(S) :-
        ::retractall(sit_(_)),
        ::assertz(sit_(S)).

:- end_category.


:- object(meta_sm,
    specializes(metaclass)).

    :- public(new/2).
    new(Instance, Sit) :-
        ^^instantiate(Instance, [sit_(Sit)]).

:- end_object.


:- object(situation_manager,
    imports(sit_man),
    instantiates(meta_sm)).

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
        ^^clobber_sit(S1).

    :- public(holds/1).
    holds(F) :-
        ::sit(S),
        situation::holds(F, S).

:- end_object.


:- object(meta_psm,
    implements(monitoring),
    specializes(metaclass)).

    :- public(new/2).
    new(SM, File) :-
        check_make(SM, File),
        ^^instantiate(_, [persisting_file_(File), situation_manager_(SM)]).
    :- public(new/3).
    new(SM, File, Sit) :-
       situation_manager::new(SM, Sit),
        ^^instantiate(_, [persisting_file_(File), situation_manager_(SM)]).

    check_make(SM, _) :-
        % Already exists.
        nonvar(SM),
        instantiates_class(SM, situation_manager), !.
    check_make(SM, File) :-
        (nonvar(SM), \+ current_object(SM) ; var(SM)),
        restore(File, SM).

    restore(File, SM) :-
       os::file_exists(File),
       setup_call_cleanup(open(File, read, Stream), read(Stream, sit(Term)), close(Stream)),
       situation_manager::new(SM, Term), !.
    restore(File, SM) :-
       \+ os::file_exists(File),
       situation::empty(ET),
       situation_manager::new(SM, ET).

   after(SM, do(_), _Sender) :-
       self(Self),
       forall((instantiates_class(Inst, Self), Inst::situation_manager(SM)), Inst::persist).

   after(SM, do(_, SM), _Sender) :-
       self(Self),
       forall((instantiates_class(Inst, Self), Inst::situation_manager(SM)), Inst::persist).

:- end_object.


:- object(persistent_manager,
    imports(sit_man),
    instantiates(meta_psm)).

    :- private([persisting_file_/1, situation_manager_/1]).
    :- public(situation_manager/1).
    situation_manager(SM) :-
        ::situation_manager_(SM).

    :- public(persist/0).
    persist :-
        ::persisting_file_(File),
        ::situation_manager(SM),
        SM::sit(Sit),
        setup_call_cleanup(open(File, write, Stream),
            (write(Stream, 'sit('), writeq(Stream, Sit), write(Stream, ').\n')),
            close(Stream)).

:- end_object.


:- category(actorc).
    :- public(action/1).

    :- set_logtalk_flag(events, allow).

    :- private(acts_upon/1).

    :- public(do/1).
    :- meta_predicate(do(2)).
    do(A) :-
        (::acts_upon(SM) ; situation_manager::only(SM)), !,
        do(A, SM).

    :- public(do/2).
    :- meta_predicate(do(2, *)).
    do(A, SM) :-
        functor(A, Func, Ar),
        ::action(Func/Ar),
        SM::do(A).

:- end_category.


:- category(fluentc).
    :- public(fluent/1).

    :- public(holds/1).
    :- meta_predicate(holds(1)).
    holds(Fluent) :-
        situation_manager::only(SM),
        holds(Fluent, SM).

    :- public(holds/2).
    :- meta_predicate(holds(1, *)).
    holds(Fluent, SM) :-
        functor(Fluent, Func, Ar),
        NAr is Ar + 1,
        ::fluent(Func/NAr),
        SM::sit(S),
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
    after(SM, do(_), _Sender) :-
        SM::current_predicate(sit/1),
        SM::sit(S),
        ::render(S).

    after(SM, do(_, SM), _Sender) :-
        SM::current_predicate(sit/1),
        SM::sit(S),
        ::render(S).

    :- public(render/1).
    :- mode(render(+term), zero_or_one).
    :- info(render/1,
        [ comment is 'Render the given Situation into the chosen view.'
        , argnames is ['Situation']
        ]).

:- end_category.
