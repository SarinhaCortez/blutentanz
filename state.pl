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