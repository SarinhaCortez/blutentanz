:- consult(board).
:- consult(redefs).

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
    [_, _, _, Player, _, _, WB, WP] = GameState,
    choose_moving_piece(Player, WB, WP).
choose_moving_piece(pink, _WB, WP) :-
    possible_pieces_pink(Pieces, WP),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from: '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Pieces, Success),
    Success == 1, !.
choose_moving_piece(blue, WB, _WP) :-
    possible_pieces_blue(Pieces, WB),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from: '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Pieces, Success),
    Success == 1, !.

possible_pieces_blue(ListOfPieces, WB) :-
    findall(X, (between(WB, 5, X)), ListOfPieces).
possible_pieces_pink(ListOfPieces, WP) :-
    findall(X, (between(WP, 5, X)), ListOfPieces).

validate_piece_input(Input, Pieces, Success):-
    member(Input, Pieces),!, Success = 1.

validate_piece_input(_Input, _Pieces, Success) :-
    Success = 0.


choose_difficulty(1, _).
choose_difficulty(_, Dif) :-
    repeat, 
    write('DIFFICULTY (don\'t forget the . after your choice):\n 1. Einfach\n 2. Schwer \n Difficulty:'),
    read(Input),
    between(1, 2, Input), !,
    Dif = Input.

choose_mode(Mod) :-
    repeat, 
    write('MODE (don\'t forget the . after your choice):\n 1. Human vs Human\n 2. Human vs Computer \n 3. Computer vs Computer \n Mode:'),
    read(Input),
    between(1, 3, Input), !,
    Mod = Input.

choose_start_player(StartPlayer) :-
    repeat, 
    write('START PLAYER (don\'t forget the . after your choice):\n 1. Blue\n 2. Pink \n Start Player:'),
    read(Input),
    between(1, 2, Input), !,
    StartPlayer = Input.
    
% Move check

input_move(GameState, Symbol, Index) :-
    [Board, _, _, Cp, _, _, _, _] = GameState,
    repeat,
    write('What square do you want to move your piece to? Don\'t  forget the . after your choice: '),
    read(Square), 
    nl,
    repeat,
    write('What symbol do you want to move your piece to? Don\'t forget the . after your choice: '),
    read(Symbol),   
    get_square_index(Board, Square, Symbol, Index).
    



