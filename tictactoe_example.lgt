:- object(board(_Board_), extends(fluent)).

    holds(s0) :-
        _Board_ = [ [1, 2, 3]
                  , [4, 5, 6]
                  , [7, 8, 9]
                  ].

    holds(do(A, S)) :-
        A = move(C, N),
        board(Old)::holds(S),
        update(N, C, Old).
    holds(do(A, S)) :-
        A \= move(_, _),
        holds(S).

    update(N, C, [R1, R2, R3]) :-
        % In row 1
        list::select(N, R1, C, N1),
        _Board_ = [N1, R2, R3], !.
    update(N, C, [R1, R2, R3]) :-
        % In row 2
        list::select(N, R2, C, N2),
        _Board_ = [R1, N2, R3], !.
    update(N, C, [R1, R2, R3]) :-
        % In row 3
        list::select(N, R3, C, N3),
        _Board_ = [R1, R2, N3], !.

:- end_object.


:- object(available_move(_N_), extends(fluent)).

    holds(S) :-
        board(B)::holds(S),
        list::flatten(B, Flat),
        list::member(_N_, Flat),
        integer(_N_).

:- end_object.


:- object(is_draw, extends(fluent)).

   holds(S) :-
       \+ available_move(_)::holds(S).

:- end_object.


:- object(has_won(_C_), extends(fluent)).

    holds(S) :-
        board(B)::holds(S),
        has_won(_C_, B).

    has_won(C, [ [C, C, C]
               , [_, _, _]
               , [_, _, _]
               ]).
    has_won(C, [ [_, _, _]
               , [C, C, C]
               , [_, _, _]
               ]).
    has_won(C, [ [_, _, _]
               , [_, _, _]
               , [C, C, C]
               ]).
    has_won(C, [ [C, _, _]
               , [C, _, _]
               , [C, _, _]
               ]).
    has_won(C, [ [_, C, _]
               , [_, C, _]
               , [_, C, _]
               ]).
    has_won(C, [ [_, _, C]
               , [_, _, C]
               , [_, _, C]
               ]).
    has_won(C, [ [_, _, C]
               , [_, C, _]
               , [C, _, _]
               ]).
    has_won(C, [ [C, _, _]
               , [_, C, _]
               , [_, _, C]
               ]).

:- end_object.


:- object(game_over, extends(fluent)).

    holds(S) :-
        has_won(_)::holds(S)
        ;
        is_draw::holds(S).

:- end_object.


:- object(player_turn(_C_), extends(fluent)).

    holds(s0) :-
        _C_ = x.
    holds(do(_, S)) :-
        player_turn(x)::holds(S),
        _C_ = o.
    holds(do(_, S)) :-
        player_turn(o)::holds(S),
        _C_ = x.

:- end_object.


:- object(prior_player_turn(_C_), extends(fluent)).

   holds(do(_, S)) :-
       player_turn(_C_)::holds(S).

:- end_object.


:- object(move(_C_, _N_), extends(action)).

    poss(S) :-
        situation::holds(player_turn(_C_) and available_move(_N_), S).

:- end_object.


:- object(unicode_terminal, extends(view)).

    render(Sit) :-
        situation::holds(board(Board) and prior_player_turn(C), Sit),
        print_board(Board),
        ( has_won(C)::holds(Sit), congratulate(C)
        ; is_draw::holds(Sit), write('It''s a draw\n')
        ; true
        ).
    render(Sit) :-
        situation::holds(board(Board) and not prior_player_turn(_), Sit),
        print_board(Board).



    :- public(print_board/1).
    print_board([R1, R2, R3]) :-
        write('┌─┬─┬─┐\n'),
        print_row(R1),
        write('├─┼─┼─┤\n'),
        print_row(R2),
        write('├─┼─┼─┤\n'),
        print_row(R3),
        write('└─┴─┴─┘\n').

    % Helper to print board, prints row.
    print_row(Row) :-
        meta::map(print_tile, Row), write('│\n').

    % Helper to print row, prints one tile
    print_tile(Tile) :-
        integer(Tile), write('│'), write(Tile)
    ;   Tile == o, write('│○')
    ;   Tile == x, write('│×').

    congratulate(Player) :-
        write('Player '), write(Player), write(' wins!\n').

:- end_object.


:- object(tictactoe).

    :- set_logtalk_flag(events, allow).

    :- public(init/0).
    init :-
        rapp::init_sit(s0),
        unicode_terminal::render(s0),
        play.

    :- public(play/0).
    play :-
        rapp::sit(S),
        situation::holds(player_turn(C) and not game_over, S),
        choose_move(C, S, M),
        rapp::do(move(C, M)), !,
        play.
    play :-
        rapp::sit(S),
        game_over::holds(S).

    choose_move(x, S, N) :-
        write('Where should X go?\n'),
        read(N), integer(N),
        available_move(N)::holds(S)
    ; % Move is invalid, notify and recurse
        write('Can''t make that move\n'),
        choose_move(x, S, N).

    choose_move(o, S, N) :-
        choose_random_member(N, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
        available_move(N)::holds(S),
        write('Computer choooses '), write(N), nl.

    :- private(choose_random_member/2).
    :- mode(choose_random_member(-any, +list), zero_or_more).
    :- info(choose_random_member/2,
        [ comment is 'Yield elements from list in random order'
        , argnames is ['Elem', 'List']
        ]).
    choose_random_member(N, L) :-
        fast_random::permutation(L, NL),
        list::member(N, NL).

:- end_object.
