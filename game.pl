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
can_move_to(blue, '+') :- !.
can_move_to(_, '-') :- !.
can_move_to(pink, '*') :- !.

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
    GameState = [ShuffledBoard, Mode, Dif, Player, 0, 0, 5, 5].

% game over - blue won
game_over(GameState, Winner) :-
    [_, _, _, blue, CSb , _ , _ ,_]  = GameState,
    blue_finished_figures(F),
    CSb == 5,
    Winner = blue.

% game over - pink won
game_over(GameState, Winner) :-
    [_, _, _, pink, _ , CSp , _ ,_] = GameState,
    CSp == 5,
    Winner = pink.

% game over - display winner
show_winner(GameState, Winner) :-
    write(Winner),
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
choose_move(GameState, _, (X_Index, Y_Index)).
    [_, 1, _, _, _, _, _, _] = GameState,
    input_move(GameState, Symbol, Y_Index),
    check_x_index(GameState, Symbol, Y_Index, X_Index),
    valid_move(GameState, X_Index, Y_Index).
/*
% Human vs. Human
choose_move(GameState, _, (X, Y)).
    [Board, 1, _, Player | _] = GameState,
    input_move(GameState, Symbol, Y),
    check_x_index(GameState, Symbol, Y, X),
    valid_moves_piece(X, Y, Player, Board, Moves), 
    member((X,Y), Moves).
*/

check_x_index(GameState, Symbol, Y_Index, X_Index) :-
    [Board, _, _, _ | _] = GameState,   
    nth1(Y_Index, Board, Row),         
    nth1(X_Index, Row, Symbol).

% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_loop(GameState):-
    print_turn(GameState),
    display_game(GameState),
    choose_spin(GameState, SpunGameState),
    display_game(SpunGameState),
    call_choose_and_move(3, SpunGameState, NewGameState), !,
    game_loop(NewGameState).

clear_data :-
    retractall(board(_)).
    
display_game(GameState) :- 
    print_board(GameState).


% Helper to find the empty spot in the square
find_empty_spot(Square, EmptyIndex) :-
    nth1(1, Square, ' ', EmptyIndex), !. 


% Restore square based on empty pos
restore_symbol(Square, 1, [" ", "+", "*", "-"]).
restore_symbol(Square, 2, ["*", " ", "-", "+"]).
restore_symbol(Square, 3, ["-", "*", "+", " "]).
restore_symbol(Square, 4, ["+", "-", " ", "*"]).


% Move the pawn
move(Pawn_Symbol, Old_X, Old_Y, New_X, New_Y, GameState, NewGameState) :-
   
    [Board, _, _, Player, ScoreBlue, ScorePink, _, _] = GameState,

    % Check if move is valid
    valid_moves_piece(Old_X, Old_Y, Player, Board, Moves),
    member((New_X, New_Y), Moves),


    % Find the old square and restore the symbol to the empty spot
    nth1(Old_Y, Board, OldSquare),
    find_empty_spot(OldSquare, EmptyIndex),
    restore_symbol(OldSquare, EmptyIndex, RestoredOldSquare),
    replace_in_board(Board, Old_Y, RestoredOldSquare, TempBoard),

    % Move the pawn to the new square
    nth1(New_Y, TempBoard, NewSquare),
    replace_in_square(NewSquare, New_X, Pawn_Symbol, FinalNewSquare),
    replace_in_board(TempBoard, New_Y, FinalNewSquare, FinalBoard),

    score_update(Player, New_X, New_Y, ScoreBlue, ScorePink, NewScoreBlue, NewScorePink, FinalBoard, FinalRestoredBoard),

    NewGameState = [FinalRestoredBoard, _, _, Player, NewScoreBlue, NewScorePink, _, _].


% Helper to replace a row in the board
replace_in_board(Board, RowIndex, NewRow, NewBoard) :-
    nth1(RowIndex, Board, _, TempBoard),
    nth1(RowIndex, TempBoard, NewRow, NewBoard).

% Helper to replace a symbol in a square
replace_in_square([_, TopRight, BottomLeft, BottomRight], 1, Symbol, [Symbol, TopRight, BottomLeft, BottomRight]).
replace_in_square([TopLeft, _, BottomLeft, BottomRight], 2, Symbol, [TopLeft, Symbol, BottomLeft, BottomRight]).
replace_in_square([TopLeft, TopRight, _, BottomRight], 3, Symbol, [TopLeft, TopRight, Symbol, BottomRight]).
replace_in_square([TopLeft, TopRight, BottomLeft, _], 4, Symbol, [TopLeft, TopRight, BottomLeft, Symbol]).

% Score update for Pink and Blue

score_update(pink, _, Y, ScoreBlue, ScorePink, NewScoreBlue, NewScorePink, FinalBoard, FinalRestoredBoard) :-
    Y >= 0, Y =< 3,  
    NewScorePink is ScorePink + 1,
    nth1(Y, FinalBoard, Square),
    find_empty_spot(Square, EmptyIndex),
    restore_symbol(Square, EmptyIndex, RestoredSquare),
    replace_in_board(FinalBoard, Y, RestoredSquare, FinalRestoredBoard).

score_update(blue, X, Y, ScoreBlue, ScorePink, NewScoreBlue, ScorePink, FinalBoard, FinalRestoredBoard) :-
    Y >= 12, Y =< 15, X = 4,  
    NewScoreBlue is ScoreBlue + 1,
    nth1(Y, FinalBoard, Square),
    find_empty_spot(Square, EmptyIndex),
    restore_symbol(Square, EmptyIndex, RestoredSquare),
    replace_in_board(FinalBoard, Y, RestoredSquare, FinalRestoredBoard).
score_update(_, _, _, ScoreBlue, ScorePink, ScoreBlue, ScorePink, FinalBoard, FinalBoard). % No score change if no goal reached

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
    choose_moving_piece(GameState, PieceGameState, Piece, X, Y),
    choose_move(PieceGameState, _, (NewX, NewY)),
    format('Move chosen: (~w, ~w)~n', [NewX, NewY]),
    move(Pawn_Symbol, Old_X, Old_Y, NewX, NewY, PieceGameState, MovedGameState),
    N1 is N - 1,
    call_choose_and_move(N1, MovedGameState, FinalGameState).