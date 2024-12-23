:- consult(board).

choose_spin :-
    board(Board),%tirar
    print_board(Board),
    repeat,
    write('Choose a row (1-4) or column (A-D) to spin (don\'t forget the . after your choice): '),
    read(Input),
    process_spin_input(Input, Board, NewBoard, Success),
    Success == 1,
    print_board(NewBoard), !.

process_spin_input(Input, Board, NewBoard, Success):- 
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard),
    Success = 1.
process_spin_input(Input, Board, NewBoard, Success) :- 
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard),
    Success = 1.
process_spin_input(_Input, _Board, _NewBoard, Success) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D)'),
    Success = 0.

choose_moving_piece(GameState) :-
    [_, _, _, Player] = GameState,
    choose_moving_piece(Player).
choose_moving_piece(pink) :-
    board(Board), %tirar
    print_board(Board),
    possible_pieces_pink(Pieces),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Board, NewBoard, Success),
    Success == 1,
    print_board(NewBoard), !.
choose_moving_piece(blue) :-
    board(Board),
    print_board(Board), 
    possible_pieces_blue(Pieces),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from'),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Board, NewBoard, Success),
    Success == 1,
    print_board(NewBoard), !.

possible_pieces_blue(ListOfPieces) :-
    blue_waiting_figures(N),
    findall(X, (between(N, 5, X)), ListOfPieces).

possible_pieces_pink(ListOfPieces) :-
    pink_waiting_figures(N),
    findall(X, (between(N, 5, X)), ListOfPieces).

parse_move(GameState, Coords, Symbol) :-

choose_move_coords(GameState, )