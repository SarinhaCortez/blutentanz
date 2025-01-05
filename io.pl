:- consult(board).

% Mode selection IO
% Prompts the user to choose a mode (1, 2, or 3) and validates the input
choose_mode(Mod) :-
    repeat,
    write('\nMODE  (Input 1, 2 or 3, then press ENTER):\n\n 1. Human vs Human\n 2. Human vs Computer \n 3. Computer vs Computer \n\nMode: '),
    catch(read(Input), _, fail), 
    validate_mode(Input, Mod), !.

% Validates the mode input, ensuring it is an integer between 1 and 3
validate_mode(Input, Input) :-
    integer(Input),
    between(1, 3, Input).

% Handles invalid mode input
validate_mode(_, _) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Choose Difficulty IO
% Sets difficulty to 1 if mode is 1, otherwise prompts the user to choose a difficulty (1 or 2) and validates the input
choose_difficulty(1, 1).
choose_difficulty(_, Dif) :-
    repeat, 
    write('\nDIFFICULTY (Input 1 or 2, then press ENTER, . ,ENTER): :\n\n 1. Easy\n 2. Hard \n\nDifficulty:'),
    catch(read(Input), _, fail),
    validate_difficulty(Input, Dif), !.

% Validates the difficulty input, ensuring it is an integer between 1 and 2
validate_difficulty(Input, Input) :-
    integer(Input),
    between(1, 2, Input), !.

% Handles invalid difficulty input
validate_difficulty(_, _) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Start player selection IO
% Sets start player to blue if mode is not 1, otherwise prompts the user to choose a start player (1 or 2) and validates the input

choose_start_player(2, StartPlayer) :-
    repeat, 
    write('\nWHO ARE YOU (Input 1 or 2, then press ENTER, . ,ENTER):\n\n 1. Blue\n 2. Pink \n\nYou are: '),
    catch(read(Input), _, fail),
    validate_start_player(Input,StartPlayer), 
    write('\nHuman is '), 
    format_color(StartPlayer),
    write(' and Computer is '), 
    opponent(StartPlayer, Opponent), 
    format_color(Opponent), nl, !.
choose_start_player(_, StartPlayer) :-
    repeat, 
    write('\nSTART PLAYER  (Input 1 or 2, then press ENTER, . ,ENTER):\n\n 1. Blue\n 2. Pink \n\nStart Player:'),
    catch(read(Input), _, fail),
    validate_start_player(Input,StartPlayer), !.

%Validates the start player input, ensuring it is an integer between 1 and 2, and maps it to a player color
validate_start_player(Input, StartPlayer) :-
    integer(Input),
    between(1, 2, Input), 
    player_n(Input, StartPlayer).

% Handles invalid start player input
validate_start_player(_,_) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Initial IO
% Prompts the player to choose a row (1-4) or column (A-D) to spin and updates the game state accordingly
choose_spin(GameState, NewGameState) :-
    [Board, _, _, Player|_] = GameState,
    repeat,
    format_color(Player),
    write(', choose a row (1-4) or column (A-D) to spin (Input your choice, then press ENTER, . ,ENTER): '),
    catch(read(Input), _, fail),
    spin(Input, Board, NewBoard, 1),
    replace_board(GameState, NewBoard, NewGameState), !.

% Spins a row if the input is a valid row number (1-4)
spin(Input, Board, NewBoard, 1):- 
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard).

% Spins a column if the input is a valid column letter (A-D)
spin(Input, Board, NewBoard, 1) :- 
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard).

% Randomly chooses a row or column to spin if the input is 0
spin(0, Board, NewBoard, 1) :-
    random(1, 4, Index),
    random_member(SpinType, [spin_row, spin_column]),
    call(SpinType, Index, Board, NewBoard), !.

% Handles invalid spin input
spin(Input, _Board, _NewBoard, 0) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D) Input was'), print(Input), nl.

% Returns piece and its xy
% Prompts the player to choose a piece to move and updates the game state accordingly
choose_piece(GameState, NewGameState, Piece, (X, Y)) :-
    [Board, _, _, Player, _, _,_, WB, WP, _] = GameState,
    select_cs(GameState, CS),
    choose_piece(Player, WB, WP, NewW, Piece, CS),
    get_x_y(Piece, X, Y, Board),
    format('Piece is in x: ~w, y: ~w\n', [X, Y]),
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, NewGameState).

% Prompts the pink player to choose a piece to move and updates the waiting pieces
choose_piece(pink, _, WP, NewW, Piece, CSP) :-
    get_waiting_pieces(Pieces, pink, WP, CSP), !,
    repeat,
    format_color(pink),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail), 
    validate_piece_input(Input, Pieces, 1),
    update_waiting_pieces(Input, WP, NewW),
    get_piece(pink, Input, Piece), !.

% Prompts the blue player to choose a piece to move and updates the waiting pieces
choose_piece(blue, WB, _, NewW, Piece,CSB) :-
    get_waiting_pieces(Pieces, blue, WB, CSB), !,
    repeat,
    format_color(blue),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail), 
    validate_piece_input(Input, Pieces, 1),
    update_waiting_pieces(Input, WB, NewW),
    get_piece(blue, Input, Piece), !.