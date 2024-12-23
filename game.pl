/*USE MEANINGFUL NAMES FOR PREDICATES AND ARGUMENTS. 
TRY TO WRITE CODE THAT ‘LOOKS DECLARATIVE’ AND AVOID USING ‘IMPERATIVE-LOOKING’ 
CONSTRUCTIONS (E.G., IF-THEN-ELSE CLAUSES). TRY TO WRITE EFFICIENT CODE 
(E.G., USING TAIL RECURSION WHEN POSSIBLE).*/
play()
% must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
initial_state(GameConfig,GameState).
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
display_game(GameState).
/*display_game(+GameState). This predicate receives the current 
game state (including the player who will make the next move) and prints the game 
state to the terminal. Appealing and intuitive visualizations will be valued. 
Flexible game state representations and visualization predicates will also be 
valued, for instance those that work with any board size. FOR UNIFORMIZATION 
PURPOSES, COORDINATES SHOULD START AT (1,1) AT THE LOWER LEFT CORNER.*/
move(GameState, Move, NewGameState).
/*move(+GameState,+Move,-NewGameState).This predicate is responsible for move validation and execution, receiving 
the current game state and the move to be executed, and (if the move is valid) 
returns the new game state after the move is executed.*/

valid_moves_piece(X, Y, Player, Board, Moves) :-
    (X = 1, Y >= 5, Y =< 16 -> PossibleMoves = [(2, Y+1), (2, Y), (3, Y+4), (3, Y)];
     X = 2, Y >= 5, Y =< 15 -> PossibleMoves = [(1, Y-1), (1, Y), (4, Y+4), (4, Y)];
     X = 3, Y >= 2, Y =< 12 -> PossibleMoves = [(1, Y), (4, Y), (4, Y+1), (1, Y-4)];
     X = 4, Y >= 1, Y =< 12 -> PossibleMoves = [(2, Y-4), (3, Y), (2, Y), (3, Y-1)];
     PossibleMoves = []),
     include(valid_move(Player, Board), PossibleMoves, Moves).

% Check if a move is valid based on the player and the character in the position
valid_move(Player, Board, (X, Y)) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Char),
    can_move_to(Player, Char).

% Define the rules for moving to a place with a specific character
can_move_to(blue, '+') :- !.
can_move_to(blue, '-') :- !.
can_move_to(pink, '*') :- !.
can_move_to(pink, '-') :- !.

valid_moves(GameState, ListOfMoves).

/*valid_moves(+GameState, -ListOfMoves).This predicate receives the current game state, and returns a list of all possible 
valid moves.*/
game_over(GameState, Winner). 
/*game_over(+GameState, -Winner). This predicate receives the current game state, and verifies whether the game 
is over, in which case it also identifies the winner (or draw). Note that this 
predicate should not print anything to the terminal.*/
value(GameState, Player, Value). 
/*value(+GameState, +Player, -Value). This predicate receives the current game state and returns a value measuring how
 good/bad the current game state is to the given Player.*/
choose_move(GameState, Level, Move). 
/*choose_move(+GameState, +Level, -Move).This predicate receives the current game state and returns the move chosen by the
computer player. Level 1 should return a random valid move. Level 2 should return 
the best play at the time (using a greedy algorithm), considering the evaluation of
the game state as determined by the value/3 predicate. For human players, it should
 interact with the user to read the move.*/

% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_loop(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_loop(NewGameState).

clear_data :-
    retractall(board(_)).

% configuration(-GameState)
% Init GameState with Board, first Player, empty FearList and TotalMoves
configurations([Board,Player,[],0]):-
    barca,
    set_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).