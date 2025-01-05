% Predicate to check if there are no moves left

has_no_moves([]).
has_no_moves(_) :- false.

% Predicate to get the opponent of a player

opponent(blue, pink).
opponent(pink, blue).

% Predicate to get the player number

player_n(1, blue).
player_n(2, pink).

% Predicate to get the piece values for each player

piece_values(pink, N) :- between(0, 4, N).
piece_values(blue, N) :- between(5, 9, N).

% Predicate to get the column index from a character

column_index('0', 0) :- !.
column_index('a', 1).
column_index('b', 2).
column_index('c', 3).
column_index('d', 4).
column_index(A, Col) :- char_code(A, Code), Code >= 65, Code =< 68, Col is Code - 64.

% Predicate to select the waiting piece (W) from the game state

select_w(GameState, W):-
    [_, _, _, blue, _, _, _, W,_, _] = GameState.
select_w(GameState, W):-
    [_, _, _, pink, _, _, _, _, W, _] = GameState.
select_w(GameState, blue, W):-
    [_, _, _, _, _, _, _, W, _, _] = GameState.
select_w(GameState, pink, W):-
    [_, _, _, _, _, _, _, _, W, _] = GameState.

% Predicate to select the current score (Score) from the game state

select_cs(GameState, Score):-
    [_, _, _, blue, _, Score, _, _,_, _] = GameState.
select_cs(GameState, Score):-
    [_, _, _, pink, _,  _, Score, _,_, _] = GameState.
select_cs(GameState, blue, Score):-
    [_, _, _, _,_, Score, _, _,_, _] = GameState.
select_cs(GameState, pink, Score):-
    [_, _, _, _, _,  _, Score, _,_, _] = GameState.

% Predicate to check if a player can move to a specific cell

can_move_to(blue, '*') :- !.
can_move_to(_, '-') :- !.
can_move_to(pink, '+') :- !.
can_move_to(_, _) :- fail, !.

% Predicate to unpack coordinates from a list of tuples

unpack_coordinates([], [], []).

unpack_coordinates([(X, Y) | Rest], [X | Xs], [Y | Ys]) :-
    unpack_coordinates(Rest, Xs, Ys).
    
valid_coordinate((X, Y)) :-
    (X, Y) \= (0, 0).


% Get Waiting Pieces based on GameState

get_waiting_pieces(WaitingPieces, GameState) :-
    [_, _, _, blue, _, CSB,_, WB|_] = GameState,
    get_waiting_pieces(WaitingPieces, blue, WB, CSB).

get_waiting_pieces(WaitingPieces, GameState) :-
    [_, _, _, pink, _, _,CSP,_, WP|_] = GameState,
    get_waiting_pieces(WaitingPieces, pink, WP, CSP).

% Get Waiting Pieces based on Player, Waiting Number and Current Score

get_waiting_pieces(ListOfPieces, blue, WB, CSB) :-
    max(WB, 1, Min),
    findall(X, (between(Min, 5, X), \+ member(X, CSB)), ListOfPieces), !.

get_waiting_pieces(ListOfPieces, pink, WP, CSP) :-
    max(WP, 1, Min),
    findall(X, (between(Min, 5, X), \+ member(X, CSP)), ListOfPieces), !.

% Predicate to get the internal representation value for a piece from an input (0-5) for a player

get_piece(pink, Input, Piece) :-
    Piece is Input - 1.

get_piece(blue, Input, Piece) :-
    Piece is Input + 4.

% Predicate to get the X and Y coordinates of a piece on the board, if not, returns 0,0

get_x_y(Piece, X, Y, Board) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece), !.

get_x_y(_Piece, 0, 0, _B).

% Predicate to get piece coordinates for blue player, excluding pieces in CSB

get_piece_coordinates(GameState, PieceCoordinates) :-
    [Board, _, _, blue, _, CSB, _, _, _, _] = GameState,
    findall(Piece, (between(5, 9, Piece), \+ member(Piece, CSB)), Pieces),
    findall((Piece, X, Y), (member(Piece, Pieces), get_x_y(Piece, X, Y, Board)), PieceCoordinates).

% Predicate to get piece coordinates for pink player, excluding pieces in CSP

get_piece_coordinates(GameState, PieceCoordinates) :-
    [Board, _, _, pink, _, _, CSP, _, _, _] = GameState,
    findall(Piece, (between(0, 4, Piece), \+ member(Piece, CSP)), Pieces),
    findall((Piece, X, Y), (member(Piece, Pieces), get_x_y(Piece, X, Y, Board)), PieceCoordinates).

% Predicate to get the score for a player from the game state

get_score(pink, GameState, Score) :- 
    [_, _, _, _, _, _, CFp | _] = GameState,
    length(CFp, Score).

get_score(blue, GameState, Score) :- 
    [_, _, _, _, _, CFb | _] = GameState,
    length(CFb, Score).

% Predicate to get the index of the square in the board

get_square_index(Board, Input, Symbol, SquareY, PlaceInSquare, 1) :-
    validate_move_input(Input, ColVisual, RowVisual), !,
    SquareY is (RowVisual - 1) * 4 + ColVisual, !,
    nth1(SquareY, Board, SqContent), !,
    nth1(PlaceInSquare, SqContent, Symbol), !.

get_square_index(_Board, _Input, _Symbol, _SquareY, _PlaceInSquare, 0) :-
    write('Oops! This position is not available in the square you chose! Try again.\n').

% Predicate to validate the move input

validate_move_input(Input, Col, Row) :- 
    atom_chars(Input, [ColChar, RowChar]),
    member(RowChar, ['1', '2', '3', '4']), !,
    member(ColChar, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(ColChar, Col), !, 
    char_code(RowChar, Code),
    Row is Code - 48.

% Predicate to check if a move is valid for a player

is_valid_move(Player, Board, (X, Y)) :-
    length(Board, L),
    Y =< L, Y > 0,
    nth1(Y, Board, Row),
    nth1(X, Row, Char),
    can_move_to(Player, Char), !.

% Predicate to validate the piece input

validate_piece_input(Input, Pieces, 1):-
    member(Input, Pieces),!.
validate_piece_input(_Input, _Pieces, 0).

% Predicate to print the current turn

print_turn(GameState) :-
    [_, _, _, Cp | _] = GameState,
    format_color(Cp),
    write(', your turn!'),
    nl.

% Predicate to print the welcome message for the game

blutentanz :-
    repeat_format_color(22, '+'), nl,
    repeat_format_color(22, '-'), nl,
    write('Welcome to Blutentanz!'), nl,
    repeat_format_color(22, '-'), nl, 
    repeat_format_color(22, '*'), nl.

% Predicate to clean a square on the board

clean_square(0, 0, Board, Board) :- !.
clean_square(X, Y, Board, TempBoard) :-
    X > 0, Y > 0,
    nth1(Y, Board, Square),!, 
    nth1(SpaceX, Square, ' '),!, 
    get_symbol(SpaceX, X, Symbol), %gets the symbol to replace
    replace_in_square(Square, X, Symbol, NewSquare),!,
    replace_in_board(Board, Y, NewSquare, TempBoard).

% Predicate to replace the current piece and waiting pieces in the game state

replace_current_piece_waiting_pieces([Board, Mode, Dif, pink, _, CSb, CSp, WB, _, Type], NewW, Piece,
                       [Board, Mode, Dif, pink, Piece, CSb, CSp, WB, NewW, Type]).

replace_current_piece_waiting_pieces([Board, Mode, Dif, blue, _, CSb, CSp, _, WP, Type], NewW, Piece,
                       [Board, Mode, Dif, blue, Piece, CSb, CSp, NewW, WP, Type]).

% Predicate to replace the board in the game state

replace_board([_, Mode, Dif, Player, CurrentPiece, CSb, CSp, WB, WP, Type], NewBoard, 
                       [NewBoard, Mode, Dif, Player, CurrentPiece, CSb, CSp, WB, WP, Type]).

% Predicate to replace a row in the board

replace_in_board(Board, RowIndex, NewRow, NewBoard) :-
    same_length(Board, NewBoard),
    replace_row(Board, RowIndex, NewRow, NewBoard).

% Predicate to replace a row in the list of rows

replace_row([_|Rest], 1, NewRow, [NewRow|Rest]).
replace_row([Row|Rest], RowIndex, NewRow, [Row|NewRest]) :-
    RowIndex > 1,
    NextIndex is RowIndex - 1,
    replace_row(Rest, NextIndex, NewRow, NewRest).

% Predicate to replace a symbol in a square

replace_in_square([_, TopRight, BottomLeft, BottomRight], 1, Symbol, [Symbol, TopRight, BottomLeft, BottomRight]) :- !.
replace_in_square([TopLeft, _, BottomLeft, BottomRight], 2, Symbol, [TopLeft, Symbol, BottomLeft, BottomRight]):- !.
replace_in_square([TopLeft, TopRight, _, BottomRight], 3, Symbol, [TopLeft, TopRight, Symbol, BottomRight]):-  !.
replace_in_square([TopLeft, TopRight, BottomLeft, _], 4, Symbol, [TopLeft, TopRight, BottomLeft, Symbol]):-  !.

% Predicate to get the symbol for a specific position

get_symbol(1, X, Symbol) :- nth1(X, [' ', '+', '*', '-'], Symbol), !.
get_symbol(2, X, Symbol) :- nth1(X, ['*', ' ', '-', '+'], Symbol), !.
get_symbol(3, X, Symbol) :- nth1(X, ['+', '-', ' ', '*'], Symbol), !.
get_symbol(4, X, Symbol) :- nth1(X, ['-', '*', '+', ' '], Symbol), !.

% Predicate to update the score in the game state

update_score(GameState, X, Y, NewGameState) :-
    [Board,_,_,Player |_] = GameState,

    is_score_point(Player, (X, Y)), !,
    clean_square(X, Y, Board, TempBoard),

    format_color(Player), write(', you won a score point!\n'),

    replace_board(GameState, TempBoard, TempGameState), !,
    get_x_y(ScoredPiece, X, Y, Board),
    increase_score(TempGameState, ScoredPiece, NewGameState).

update_score(GameState, _, _, GameState).

% Update Score without printing message for move evaluation purposes

update_score_test(GameState, X, Y, NewGameState) :-
    [Board,_,_,Player |_] = GameState,

    is_score_point(Player, (X, Y)), !,
    clean_square(X, Y, Board, TempBoard),

    replace_board(GameState, TempBoard, TempGameState), !,
    get_x_y(ScoredPiece, X, Y, Board),
    increase_score(TempGameState, ScoredPiece, NewGameState).

update_score_test(GameState, _, _, GameState).


% Increase Score by adding scored piece to Current Score

increase_score([Board, Mode, Dif, pink, CurrentPiece, CFb, CFp, WB, WP, Type] ,ScoredPiece,[Board, Mode, Dif, pink, CurrentPiece, CFb, NewScore, WB, WP, Type]) :-
    append(CFp, [ScoredPiece], Score),
    remove_duplicates(Score, NewScore).

increase_score([Board, Mode, Dif, blue, CurrentPiece, CFb, CFp, WB, WP, Type] ,ScoredPiece,[Board, Mode, Dif, blue, CurrentPiece, NewScore, CFp, WB, WP, Type]) :-
    append(CFb, [ScoredPiece], Score),
    remove_duplicates(Score, NewScore).

% Predicate to check if a coordinate is a score point for a player

is_score_point(pink, (X, Y)) :-
    member((X, Y), [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)]), !.

is_score_point(blue, (X, Y)) :-
    member((X, Y), [(3, 13), (3, 14), (3, 15), (3, 16), (4, 13), (4, 14), (4, 15), (4, 16)]), !.

% Predicate to switch the turn in the game state

update_score_for_points([], GameState, GameState) :- !.

update_score_for_points([(X, Y) | T], GameState, NewGameState) :-
    update_score_test(GameState, X, Y, TempGameState), 
    update_score_for_points(T, TempGameState, NewGameState). 

% Switch turn - Swap GameStates between players after turn end

switch_turn([Board, 2, Dif, pink, _, CSb, CSp, WB, WP, bot], [Board, 2, Dif, blue, -1, CSb, CSp, WB, WP, human]).
switch_turn([Board, 2, Dif, pink, _, CSb, CSp, WB, WP, human], [Board, 2, Dif, blue, -1, CSb, CSp, WB, WP, bot]).
switch_turn([Board, 2, Dif, blue, _, CSb, CSp, WB, WP, human], [Board, 2, Dif, pink, -1, CSb, CSp, WB, WP, bot]).
switch_turn([Board, 2, Dif, blue, _, CSb, CSp, WB, WP, bot], [Board, 2, Dif, pink, -1, CSb, CSp, WB, WP, human]).

switch_turn([Board, Mode, Dif, pink, _, CSb, CSp, WB, WP, Type], [Board, Mode, Dif, blue, -1, CSb, CSp, WB, WP, Type]).
switch_turn([Board, Mode, Dif, blue, _, CSb, CSp, WB, WP, Type], [Board, Mode, Dif, pink, -1, CSb, CSp, WB, WP, Type]).

% Predicate to update the waiting pieces in the game state

update_waiting_pieces(Input, Input, NewW) :- 
    Input > 0,
    NewW is Input - 1, !.
update_waiting_pieces(_, W, W) :- !.

% Predicate to print text in color

print_in_color(Color, Text) :-  color_code(Color, Code),
                                format('\e[~wm~w\e[0m', [Code, Text]).

% Predicate to get the color code for a color

color_code(cyan, '36').    % Cyan
color_code(white, '37').   % White
color_code(black, '90').   % Black
color_code(b_magenta, '95'). %Pink

% Predicate to repeat a character in color

repeat_format_color(0, _) :- !.
repeat_format_color(N, Char) :-
    N > 0,
    format_color(Char),
    N1 is N - 1,
    repeat_format_color(N1, Char).

% Predicate to format characters in color

format_color('*') :- 
    print_in_color(cyan, '*'), !. 
format_color('o') :- 
    print_in_color(cyan, '_'), !.
format_color('_') :- 
    print_in_color(b_magenta, '_'), !.
format_color('+') :- 
    print_in_color(b_magenta, '+'), !.
format_color('-') :-
    print_in_color(black, '-'), !.

format_color(blue) :-
     print_in_color(cyan, 'B'),
     print_in_color(cyan, 'l'), 
     print_in_color(cyan, 'u'),
     print_in_color(cyan, 'e'), !. 

format_color(pink) :-
     print_in_color(b_magenta, 'P'),
     print_in_color(b_magenta, 'i'), 
     print_in_color(b_magenta, 'n'),
     print_in_color(b_magenta, 'k'), !.

format_color(X) :- 
    between(0, 4, X), !,
    X1 is X+1,
    print_in_color(b_magenta, X1). 

format_color(X) :- 
    between(5, 9, X), !,
    X1 is X-4,
    print_in_color(cyan, X1).

format_color(X) :- 
    print_in_color(white, X).      

