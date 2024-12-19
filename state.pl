:- consult(board).
:- dynamic current_board/1.

init_board :-
    board(Board),
    retractall(current_board(_)),
    assert(current_board(Board)).

save_board(Board) :-
    retractall(current_board(_)),
    assert(current_board(Board)).

load_board(Board) :-
    current_board(Board).

%?- init_board, load_board(Board), spin_column(1, Board, NewBoard), save_board(NewBoard), print_board(NewBoard).
/*
You cannot use assert or retract to store / manipulate game configuration or game state information. If
you need information from the game configuration, you can include it in the GameState term.
*/