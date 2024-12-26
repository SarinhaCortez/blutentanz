:- consult(board).
:- consult(redefs).

choose_spin(GameState, NewGameState) :-
    [Board, _, _, _|_] = GameState,
    repeat,
    write('Choose a row (1-4) or column (A-D) to spin (Input your choice, then press ENTER, . ,ENTER): '),
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
%returns piece and its xy
choose_moving_piece(GameState, NewGameState, Piece, (X, Y)) :-
    [Board, _, _, Player, _, _, _, WB, WP] = GameState,
    NeWB = WB, NeWP = WP,
    choose_moving_piece(Player, WB, WP, NeWB, NeWP, Piece),
    getXY(Piece, X, Y, Board),
    replace_current_piece_waiting_pieces(GameState, NeWB, NeWP, Piece, NewGameState).

choose_moving_piece(pink, _, WP, _, NeWP, Piece) :-
    possible_pieces_pink(Pieces, WP),
    repeat,
    write('What piece do you want to move?(Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    read(Input),
    validate_piece_input(Input, Pieces, Success),
    Success == 1, 
    updateWaiting(Input, WP, NeWP), !,
    getPiece(pink, Input, Piece), !.

choose_moving_piece(blue, WB, _, NeWB, _, Piece) :-
    possible_pieces_blue(Pieces, WB),
    repeat,
    write('What piece do you want to move?(Input your choice, then press ENTER, . ,ENTER):\n You can choose from '),
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

getXY(Piece, X, Y, Board) :-
    nth1(X, Board, Row),
    nth1(Y, Row, Piece).
getXY(_Piece, X, Y, _B) :-
    X = 0, Y = 0.

validate_piece_input(Input, Pieces, Success):-
    member(Input, Pieces),!, Success = 1.

validate_piece_input(_Input, _Pieces, Success) :-
    Success = 0.

choose_difficulty(1, Dif) :- Dif = 1.
choose_difficulty(_, Dif) :-
    repeat, 
    write('\nDIFFICULTY (Input 1 or 2, then press ENTER, . ,ENTER): :\n\n 1. Einfach\n 2. Schwer \n\nDifficulty:'),
    read(Input),
    between(1, 2, Input), !,
    Dif = Input, !.

choose_mode(Mod) :-
    repeat, 
    write('\nMODE  (Input 1, 2 or 3, then press ENTER, . ,ENTER):\n\n 1. Human vs Human\n 2. Human vs Computer \n 3. Computer vs Computer \n\nMode: '),
    read(Input),
    between(1, 3, Input), !,
    Mod = Input.

choose_start_player(StartPlayer) :-
    repeat, 
    write('\nSTART PLAYER  (Input 1 or 2, then press ENTER, . ,ENTER):\n\n 1. Blue\n 2. Pink \n\nStart Player:'),
    read(Input),
    between(1, 2, Input), !,
    player_n(Input, StartPlayer).

player_n(1, Color) :- Color = blue.
player_n(2, Color) :- Color = pink.

    
% Move check
input_move(Board, Square, PlaceInSquare) :-%internally, square is y and place x
    repeat,
    write('What square do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(SqInput), 
    nl, %tirei o repeat, não fazia sentido
    write('What symbol do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(Symbol), nl,
    get_square_index(Board, SqInput, Symbol, Square, PlaceInSquare, Success), %square is col n
    format('sqinput is ~w, get square index is x:~w, y:~w ~n', [SqInput, PlaceInSquare, Square]),
    Success == 1. 

% Get the index of the square in the board 
get_square_index(Board, Input, Symbol, SquareY, PlaceInSquare, Success) :-
    validate_move_input(Input, ColVisual, RowVisual), !,
    write('Input passed Validation.\n'), 
    SquareY is (RowVisual - 1) * 4 + ColVisual, !,
    nth1(SquareY, Board, SqContent), !,
    nth1(PlaceInSquare, SqContent, Symbol), !,
    Success = 1. 
get_square_index(_Board, _Input, _Symbol, _SquareY, _PlaceInSquare, Success) :-
    write('Oops! This position is not available in the square you chose! Try again.\n'),
    Success = 0.

%input processing
validate_move_input(Input, Col, Row) :- 
    atom_chars(Input, [ColChar, RowChar]),
    member(RowChar, ['1', '2', '3', '4']), !,
    member(ColChar, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(ColChar, Col), !, 
    char_code(RowChar, Code),
    Row is Code - 48.

replace_current_piece_waiting_pieces([Board, Mode, Dif, Player, _, CSb, CSp, _, _], NeWB, NeWP, Piece,
                       [Board, Mode, Dif, Player, Piece, CSb, CSp, NeWB, NeWP]).
replace_board([_, Mode, Dif, Player, CurrentPiece, CSb, CSp, WB, WP], NewBoard, 
                       [NewBoard, Mode, Dif, Player, CurrentPiece, CSb, CSp, WB, WP]).
increase_score([Board, Mode, Dif, pink, CurrentPiece, CSb, CSp, WB, WP], NewScore, [Board, Mode, Dif, pink, CurrentPiece, CSb, NewScore, WB, WP]) :-
    NewScore is CSp + 1.

increase_score([Board, Mode, Dif, blue, CurrentPiece, CSb, CSp, WB, WP], NewScore, [Board, Mode, Dif, blue, CurrentPiece, NewScore, CSp, WB, WP]) :-
    NewScore is CSb + 1.