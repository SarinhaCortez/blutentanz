:- consult(board).
:- consult(io).

/*USE MEANINGFUL NAMES FOR PREDICATES AND ARGUMENTS. 
TRY TO WRITE CODE THAT ‘LOOKS DECLARATIVE’ AND AVOID USING ‘IMPERATIVE-LOOKING’ 
CONSTRUCTIONS (E.G., IF-THEN-ELSE CLAUSES). TRY TO WRITE EFFICIENT CODE 
(E.G., USING TAIL RECURSION WHEN POSSIBLE).*/

% play/0 must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.

/*initial_state(+GameConfig, -GameState).
%This predicate receives a desired game configuration
%and returns the corresponding initial game state. Game 
%configuration includes the type of each player and 
%other parameters such as board size, use of optional 
%rules, player names, or other information to provide 
%more flexibility to the game. The game state describes 
%a snapshot of the current game state, including board configuration 
%(typically using list of lists with different atoms for the different pieces), 
%identifies the current player (the one playing next), and possibly captured pieces 
%and/or pieces yet to be played, or any other information that may be required, 
%depending on the game.*/

/*display_game(+GameState). This predicate receives the current 
game state (including the player who will make the next move) and prints the game 
state to the terminal. Appealing and intuitive visualizations will be valued. 
Flexible game state representations and visualization predicates will also be 
valued, for instance those that work with any board size. FOR UNIFORMIZATION 
PURPOSES, COORDINATES SHOULD START AT (1,1) AT THE LOWER LEFT CORNER.*/


/*move(+GameState,+Move,-NewGameState).This predicate is responsible for move validation and execution, receiving 
the current game state and the move to be executed, and (if the move is valid) 
returns the new game state after the move is executed.*/

valid_moves_piece(0, 0, blue, Board, Moves) :-
    PossibleMoves = [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)],
    include(valid_move(blue, Board), PossibleMoves, Moves).
valid_moves_piece(0, 0, pink, Board, Moves) :-
    PossibleMoves = [(3, 13), (3, 14), (3, 15), (3, 16), (4, 13), (4, 14), (4, 15), (4, 16)],
    include(valid_move(pink, Board), PossibleMoves, Moves).
valid_moves_piece(1, Y, Player, Board, Moves) :-
    PossibleMoves = [(2, Y-1), (2, Y), (3, Y-4), (3, Y)],
    include(valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(2, Y, Player, Board, Moves) :-
    PossibleMoves = [(1, Y+1), (1, Y), (4, Y-4), (4, Y)],
    include(valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(3, Y, Player, Board, Moves) :-
    PossibleMoves = [(1, Y), (4, Y), (4, Y-1), (1, Y+4)],
    include(valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(4, Y, Player, Board, Moves) :-
    PossibleMoves = [(2, Y+4), (3, Y), (2, Y), (3, Y+1)],
    include(valid_move(Player, Board), PossibleMoves, Moves).

% Check if a move is valid based on the player and the character in the position
valid_move(Player, Board, (X, Y)) :-
    length(Board, Length),
    Y > 0, Y =< Length,
    nth1(Y, Board, Row),
    nth1(X, Row, Char),
    can_move_to(Player, Char), !.

% Define the rules for moving to a place with a specific character
can_move_to(blue, '*') :- !.
can_move_to(_, '-') :- !.
can_move_to(pink, '+') :- !.

valid_moves(GameState, ListOfMoves).

/*valid_moves(+GameState, -ListOfMoves).This predicate receives the current game state,
and returns a list of all possible valid moves.*/

play :-
    blutentanz,
    choose_mode(Mod), !,
    choose_start_player(Player), !,
    choose_difficulty(Mod, Dif), !,
    GameConfig = [Mod, Dif, Player], !,
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_loop(GameState).

initial_state(GameConfig, GameState) :-
    board(Board),
    shuffle_board(Board, ShuffledBoard),
    [Mode, Dif, Player] = GameConfig,
    GameState = [ShuffledBoard, Mode, Dif, Player, -1, 0, 0, 5, 5]. %minus one is current piece to move

% game over - blue won
game_over(GameState, Winner) :-
    [_, _, _, blue, _, CSb , _ , _ ,_]  = GameState,
    CSb == 5,
    Winner = blue.

% game over - pink won
game_over(GameState, Winner) :-
    [_, _, _, pink, _,  _ , CSp , _ ,_] = GameState,
    CSp == 5,
    Winner = pink.

% game over - display winner
show_winner(Winner) :-
    format_color(Winner),
    write(" won!"),
    nl.    

value(GameState, Player, Value). 
/*value(+GameState, +Player, -Value). This predicate receives the current game state and returns a value measuring how
 good/bad the current game state is to the given Player.*/


/*choose_move(+GameState, +Level, -Move).This predicate receives the current game state and returns the move chosen by the
computer player. Level 1 should return a random valid move. Level 2 should return 
the best play at the time (using a greedy algorithm), considering the evaluation of
the game state as determined by the value/3 predicate. For human players, it should
 interact with the user to read the move.*/

% Human vs. Human
%internally, square is y and place x
choose_move(GameState, _, Move, PieceGameState) :-
    (X, Y) = Move,
    [_, 1, _|_] = GameState,
    repeat,
    choose_moving_piece(GameState, PieceGameState, Piece, (Curr_X, Curr_Y)),
    format('Moving piece: ~w~n', Piece),
    [Board, 1, _, Player, _, _, _, _, _] = PieceGameState,
    input_move(Board, Square, PlaceInSquare),
    format('input move is x:~w, y:~w~n', [PlaceInSquare, Square]),
    valid_moves_piece(Curr_X, Curr_Y, Player, Board, Moves),
    member((PlaceInSquare, Square), Moves),
    print(Moves),nl,
    format('~w is moving from x:~w y:~w to x:~w y:~w ~n', [Player, Curr_X, Curr_Y, Square, PlaceInSquare]),
    X is PlaceInSquare, 
    Y is Square,
    write('choose move reached its end!\n'), !.

% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(Winner).
game_loop(GameState):-
    print_turn(GameState),
    display_game(GameState),
    choose_spin(GameState, SpunGameState),
    display_game(SpunGameState),
    call_choose_and_move(3, SpunGameState, NewGameState), !,
    %move
    game_loop(NewGameState).

clear_data :-
    retractall(board(_)).
    
display_game(GameState) :- 
    print_board(GameState).

move(GameState, Move, NewGameState) :-
    Move = (X, Y),
    [Board, _, _, CurrPiece | _] = GameState,
    getXY(CurrPiece, Old_X, Old_Y, Board),!,
    clean_square(Old_X, Old_Y, Board, TempBoard),!,
    nth1(Y, TempBoard, Square), !,
    replace_in_square(Square, X, CurrPiece, NewSquare), !,
    write('Got after replace in square!\n'),
    replace_in_board(TempBoard, Y, NewSquare, NewBoard),!,
    write('Got after replace in board!\n'),
    replace_board(GameState, NewBoard, TempState), !,
    update_score(TempState, X, Y, NewGameState).

update_score(GameState, X, Y, NewGameState) :-
    [Board,_,_,Player |_] = GameState,
    is_score_point(Player, X, Y), !,
    clean_square(X, Y, Board, TempBoard),
    format_color(Player), write('You won a score point!\n'),
    replace_board(GameState, TempBoard, TempGameState), !,
    increase_score(TempGameState, NewGameState).

is_score_point(pink, X, Y) :-
    ScoringPos = [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)],
    member((X, Y), ScoringPos), !.
is_score_point(blue, X, Y) :-
    ScoringPos = [(3, 13), (3, 14), (3, 15), (3, 16), (4, 13), (4, 14), (4, 15), (4, 16)],
    member((X, Y), ScoringPos), !.
% Helper to replace a row in the board
replace_in_board(Board, RowIndex, NewRow, NewBoard) :-
    nth1(RowIndex, Board, _, TempBoard),
    nth1(RowIndex, TempBoard, NewRow, NewBoard). % move fails here
    
clean_square(0, 0, Board, TempBoard) :- 
    write('Your piece is just starting!\n'), 
    TempBoard = Board, !.
%clean square
clean_square(X, Y, Board, TempBoard) :-
    X>0, Y > 0,
    nth1(Y, Board, Square),!,
    nth1(SpaceX, Square, ' '),!,
    getSymbol(SpaceX, X, Symbol), %gets the symbol to replace
    replace_in_square(Square, X, Symbol, NewSquare),!,
    replace_in_board(Board, Y, NewSquare, TempBoard).

getSymbol(1, X, Symbol) :- nth1(X, [' ', '+', '*', '-'], Symbol), !.
getSymbol(2, X, Symbol) :- nth1(X, ['*', ' ', '-', '+'], Symbol), !.
getSymbol(3, X, Symbol) :- nth1(X, ['+', '-', ' ', '*'], Symbol), !.
getSymbol(4, X, Symbol) :- nth1(X, ['-', '*', '+', ' '], Symbol), !.
replace_in_square([_, TopRight, BottomLeft, BottomRight], 1, Symbol, [Symbol, TopRight, BottomLeft, BottomRight]) :- !.
replace_in_square([TopLeft, _, BottomLeft, BottomRight], 2, Symbol, [TopLeft, Symbol, BottomLeft, BottomRight]):- !.
replace_in_square([TopLeft, TopRight, _, BottomRight], 3, Symbol, [TopLeft, TopRight, Symbol, BottomRight]):-  !.
replace_in_square([TopLeft, TopRight, BottomLeft, _], 4, Symbol, [TopLeft, TopRight, BottomLeft, Symbol]):-  !.


display_game(GameState) :- 
    print_board(GameState).

% configuration(-GameState)
% Init GameState with Board, first Player, empty FearList and TotalMoves
/*
configurations([Board,Player,[],0]):-
    barca,
    set_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).*/

% Example condition predicate (can be customized)


% Main predicate calling choose_move/3 three times
call_choose_and_move(0, _, _) :- !. 
/*
should_stop(N).
call_choose_and_move(N, GameState) :-
    should_stop(N), 
    writeln('Condition met, stopping!'), !.*/

call_choose_and_move(N, GameState, FinalGameState) :-
    N > 0,
    repeat,
    choose_move(GameState, _, (NewX, NewY), PieceGameState),
    format('Move chosen: (~w, ~w)~n', [NewX, NewY]),
    write('Preparing to enter move :)\n'),
    move(PieceGameState,(NewX, NewY), MovedGameState),
    display_game(MovedGameState),
    N1 is N - 1,
    call_choose_and_move(N1, MovedGameState, FinalGameState).
