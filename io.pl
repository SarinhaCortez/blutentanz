:- consult(board).
:- consult(redefs).

choose_spin(GameState, NewGameState) :-
    [Board, _, _, _|_] = GameState,
    repeat,
    write('Choose a row (1-4) or column (A-D) to spin (don\'t forget the . after your choice): '),
    read(Input),
    process_spin_input(Input, Board, NewBoard, Success),
    Success == 1, replace_board(GameState, NewBoard, NewGameState), !.

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

choose_moving_piece(GameState, NewGameState, Piece, X, Y) :-
    [_, _, _, Player, _, _, WB, WP] = GameState,
    NeWB = WB, NeWP = WP,
    choose_moving_piece(Player, WB, WP, NeWB, NeWP, Piece),
    getXY(Piece, X, Y),
    replace_waiting_pieces(GameState, NeWB, NeWP, NewGameState).

choose_moving_piece(pink, _, WP, _, NeWP, Piece) :-
    possible_pieces_pink(Pieces, WP),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from: '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Pieces, Success),
    Success == 1, 
    updateWaiting(Input, WP, NeWP), !,
    getPiece(pink, Input, Piece), !.

choose_moving_piece(blue, WB, _, NeWB, _, Piece) :-
    possible_pieces_blue(Pieces, WB),
    repeat,
    write('What piece do you want to move?(Don\'t forget the . after your choice)\n Choose from: '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Pieces, Success),
    Success == 1,
    updateWaiting(Input, WB, NeWB), !, 
    getPiece(blue, Input, Piece), !.

updateWaiting(Input, Input, NeW) :- NeW is Input - 1, !.
updateWaiting(_, _, _).

possible_pieces_blue(ListOfPieces, WB) :-
    findall(X, (between(WB, 5, X)), ListOfPieces).
possible_pieces_pink(ListOfPieces, WP) :-
    findall(X, (between(WP, 5, X)), ListOfPieces).

getPiece(pink, Input, Piece) :-
    Piece is Input - 1.
getPiece(blue, Input, Piece) :-
    Piece is Input + 4.

getXY(Piece, X, Y, GameState) :-
    [Board | _] = GameState,
    nth1(X, Board, Row),
    nth1(Y, Row, Piece).
getXY(_Piece, X, Y, _GS) :-
    X = 0, Y = 0.

validate_piece_input(Input, Pieces, Success):-
    member(Input, Pieces),!, Success = 1.

validate_piece_input(_Input, _Pieces, Success) :-
    Success = 0.

choose_difficulty(1, Dif) :- Dif = 1.
choose_difficulty(_, Dif) :-
    repeat, 
    write('\nDIFFICULTY (don\'t forget the . after your choice):\n\n 1. Einfach\n 2. Schwer \n Difficulty:'),
    read(Input),
    between(1, 2, Input), !,
    Dif = Input, !.

choose_mode(Mod) :-
    repeat, 
    write('\nMODE (don\'t forget the . after your choice):\n\n 1. Human vs Human\n 2. Human vs Computer \n 3. Computer vs Computer \n Mode:'),
    read(Input),
    between(1, 3, Input), !,
    Mod = Input.

choose_start_player(StartPlayer) :-
    repeat, 
    write('\nSTART PLAYER (don\'t forget the . after your choice):\n\n 1. Blue\n 2. Pink \n Start Player:'),
    read(Input),
    between(1, 2, Input), !,
    player_n(Input, StartPlayer).

player_n(1, Color) :- Color = blue.
player_n(2, Color) :- Color = pink.

    
% Move check
input_move(GameState, Symbol, Y_Index) :- %you mean x index?(1,2,3,4)
    [Board| _] = GameState,
    repeat,
    write('What square do you want to move your piece to? Don\'t forget the . after your choice: '),
    read(Square), 
    nl, %tirei o repeat, não fazia sentido
    write('What symbol do you want to move your piece to? Don\'t forget the . after your choice: '),
    read(Symbol),   
    get_square_index(Board, Square, Symbol, X_Index).
    
replace_waiting_pieces([Board, Mode, Dif, Player, CSb, CSp, _, _], NeWB, NeWP, 
                       [Board, Mode, Dif, Player, CSb, CSp, NeWB, NeWP]).
replace_board([_, Mode, Dif, Player, CSb, CSp, WB, WP], NewBoard, 
                       [NewBoard, Mode, Dif, Player, CSb, CSp, WB, WP]).
