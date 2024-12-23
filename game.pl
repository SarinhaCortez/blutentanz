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
    

% prints current player
print_turn(GameState) :-
    [_, _, _, Cp] = GameState,
    write(Cp),
    write(' ,your turn!'),
    nl.


/*move(+GameState,+Move,-NewGameState).This predicate is responsible for move validation and execution, receiving 
the current game state and the move to be executed, and (if the move is valid) 
returns the new game state after the move is executed.*/


valid_moves_piece(1, Y, Player, Board, Moves) :-
    PossibleMoves = [(2, Y+1), (2, Y), (3, Y+4), (3, Y)],
    include(valid_move(Player, Board), PossibleMoves, Moves).

valid_moves_piece(2, Y, Player, Board, Moves) :-
    PossibleMoves = [(1, Y-1), (1, Y), (4, Y+4), (4, Y)],
    include(valid_move(Player, Board), PossibleMoves, Moves).

valid_moves_piece(3, Y, Player, Board, Moves) :-
    PossibleMoves = [(1, Y), (4, Y), (4, Y+1), (1, Y-4)],
    include(valid_move(Player, Board), PossibleMoves, Moves).

valid_moves_piece(4, Y, Player, Board, Moves) :-
    PossibleMoves = [(2, Y-4), (3, Y), (2, Y), (3, Y-1)],
    include(valid_move(Player, Board), PossibleMoves, Moves).

% Check if a move is valid based on the player and the character in the position
valid_move(GameState, X_Index, Y_Index) :-
    [Board, _, _, Player | _] = GameState,  
    length(Board, Length),                 
    Y_Index >= 0, Y_Index < Length,        
    nth1(Y_Index, Board, Row),             % Access the row corresponding to Y_Index
    nth1(X_Index, Row, Char),              % Access the character at X_Index in the row
    can_move_to(Player, Char), !.          % Check if the Player can move to this Char


% Define the rules for moving to a place with a specific character
can_move_to(blue, '+') :- !.
can_move_to(_, '-') :- !.
can_move_to(pink, '*') :- !.

valid_moves(GameState, ListOfMoves).

/*valid_moves(+GameState, -ListOfMoves).This predicate receives the current game state, and returns a list of all possible 
valid moves.*/

% C

play :-
    blutentanz,
    choose_mode(Mod),
    choose_start_player(Player),
    choose_difficulty(Mod, Dif),
    GameConfig = [Mod, Dif, Player].

initial_state(GameConfig, GameState) :-
    board(Board),
    shuffle_board(Board, ShuffledBoard),
    [Mod, Dif, Player] = GameConfig.
    GameState = [SdBoard, Mode, Dif, Player, 0, 0, 5, 5].

% game over - blue won
game_over(GameState, Winner) :-
    [_, _, _, blue,_ , _ , _ ,_]  = GameState,
    blue_finished_figures(F),
    F == 5,
    Winner is blue.

% game over - pink won
game_over(GameState, Winner) :-
    [_, _, _, pink, _ , _ , _ ,_] = GameState,
    pink_finished_figures(F),
    F == 5,
    Winner is pink.

game_over(GameState, Winner) :-
    [_, _, _, pink, _ , _ , _ ,_] = GameState,
    pink_finished_figures(F),
    F == 5,
    Winner is pink.

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
choose_move(GameState, _, X_Index, Y_Index).
    [_, 1, _, _, _, _, _, _] = GameState,
    input_move(GameState, Symbol, Y_Index),
    check_x_index(GameState, Symbol, Y_Index, X_Index),
    valid_move(GameState, X_Index, Y_Index).

check_x_index(GameState, Symbol, Y_Index, X_Index) :-
    [Board, _, _, _ | _] = GameState,   
    nth1(Y_Index, Board, Row),         
    (nth1(X_Index, Row, Symbol) ->     
        true                          
    ; 
        write('Invalid move: Symbol not found in the specified row.'), nl, 
        fail                          
    ).

% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_loop(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, NewX, NewY),
    move(Pawn_Symbol, Old_X, Old_Y, NewX, NewY, GameState, NewGameState), !,
    game_loop(NewGameState).

clear_data :-
    retractall(board(_)).
display_game(GameState) :- [Board, _, _, _] = Gamestate,
    print_board(Board).

restore_symbol(Board, Old_X, Old_Y, NewBoard) :-
  
    find_empty_spot(Board, Empty_X, Empty_Y),
    
    % Calculate the direction based on the old position relative to the empty spot
    (Old_X =:= Empty_X, Old_Y =:= Empty_Y + 1 -> Direction = '+' ;  % Above
    Old_X =:= Empty_X, Old_Y =:= Empty_Y - 1 -> Direction = '-' ; % Below
    Old_Y =:= Empty_Y, Old_X =:= Empty_X + 1 -> Direction = '*' ; % Right
    Old_Y =:= Empty_Y, Old_X =:= Empty_X - 1 -> Direction = ' '), % Left
    
    % Now restore the old symbol based on the calculated direction
    restore_board_symbol(Board, Old_X, Old_Y, Direction, NewBoard).

% Helper predicate: find_empty_spot/3
% find_empty_spot(+Board, -Empty_X, -Empty_Y)
% Finds the coordinates (Empty_X, Empty_Y) of the empty spot " " on the board.
find_empty_spot(Board, Empty_X, Empty_Y) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, ' '),
    Empty_X = ColIndex,
    Empty_Y = RowIndex.

% Helper predicate: replace_in_board_with_symbol/4
% replace_in_board_with_symbol(+Board, +X, +Y, +Symbol, -NewBoard)
% Replaces the symbol at coordinates (X, Y) on the board with a new symbol.
restore_board_symbol(Board, X, Y, Symbol, NewBoard) :-
    nth1(Y, Board, Row),
    replace_in_row_with_symbol(Row, X, Symbol, NewRow),
    replace_in_board(Board, Y, NewRow, NewBoard).

% Helper predicate: replace_in_row_with_symbol/4
% replace_in_row_with_symbol(+Row, +X, +Symbol, -NewRow)
% Replaces the value at a specific index in a row.
replace_in_row_with_symbol(Row, X, Symbol, NewRow) :-
    nth1(X, Row, _, TempRow), % Remove the old value
    nth1(X, TempRow, Symbol, NewRow).

% Helper predicate: replace_in_board/4
% replace_in_board(+Board, +RowIndex, +NewRow, -NewBoard)
% Replaces a row in the board at a specific index.
replace_in_board(Board, RowIndex, NewRow, NewBoard) :-
    nth1(RowIndex, Board, _, TempBoard), % Remove the old row
    nth1(RowIndex, TempBoard, NewRow, NewBoard).

% Main predicate: move/7
% move(+Pawn_Symbol, +Old_X, +Old_Y, +New_X, +New_Y, +GameState, -NewGameState)
move(Pawn_Symbol, Old_X, Old_Y, New_X, New_Y, GameState, NewGameState) :-
    % Extract relevant parts from GameState
    [Board, _, _, Player, ScoreBlue, ScorePink, _, _] = GameState,

    % Restore the symbol at the old position
    restore_symbol(Board, Old_X, Old_Y, TempBoard),

    % Move the pawn to the new position
    nth1(New_Y, TempBoard, Row), % Find the row at New_Y
    replace_in_row_with_symbol(Row, New_X, Pawn_Symbol, NewRow), % Place pawn at New_X
    replace_in_board(TempBoard, New_Y, NewRow, NewBoard),

    % Check if player is pink and pawn is in the scoring position
    (Player = pink, New_X = 1, New_Y = 1; % Example: check the first row for pink
    Player = blue, New_X = 4, New_Y = 4), % Example: check the last row for blue

    % Update the score based on the player
    (Player = pink -> NewScorePink is ScorePink + 1, NewScoreBlue is ScoreBlue;
     Player = blue -> NewScoreBlue is ScoreBlue + 1, NewScorePink is ScorePink),

    % Set the new game state
    NewGameState = [NewBoard, _, _, Player, NewScoreBlue, NewScorePink, _, _].



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