:- consult(board).

% Mode selection IO
choose_mode(Mod) :-
    repeat,
    write('\nMODE  (Input 1, 2 or 3, then press ENTER):\n\n 1. Human vs Human\n 2. Human vs Computer \n 3. Computer vs Computer \n\nMode: '),
    catch(read(Input), _, fail), 
    validate_mode(Input, Mod), !.

validate_mode(Input, Mod) :-
    integer(Input),
    between(1, 3, Input),
    Mod = Input.

validate_mode(_, _) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Choose Difficulty IO
choose_difficulty(1, Dif) :- Dif = 1.
choose_difficulty(_, Dif) :-
    repeat, 
    write('\nDIFFICULTY (Input 1 or 2, then press ENTER, . ,ENTER): :\n\n 1. Einfach\n 2. Schwer \n\nDifficulty:'),
    catch(read(Input), _, fail),
    validate_difficulty(Input, Dif), !.

validate_difficulty(Input, Dif) :-
    integer(Input),
    between(1, 2, Input), !,
    Dif = Input.

validate_difficulty(_, _) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Start player selection IO
choose_start_player(_, StartPlayer) :- StartPlayer = blue.
choose_start_player(1, StartPlayer) :-
    repeat, 
    write('\nSTART PLAYER  (Input 1 or 2, then press ENTER, . ,ENTER):\n\n 1. Blue\n 2. Pink \n\nStart Player:'),
    catch(read(Input), _, fail),
    validate_start_player(Input,StartPlayer), !.

validate_start_player(Input, StartPlayer) :-
    integer(Input),
    between(1, 2, Input), 
    player_n(Input, StartPlayer).

validate_start_player(_,_) :-
    write('Invalid input. Please try again.\n'),
    fail.

% Initial Spin IO
choose_spin(GameState, NewGameState) :-
    [Board, 1, _, Player|_] = GameState,
    repeat,
    format_color(Player),
    write(', choose a row (1-4) or column (A-D) to spin (Input your choice, then press ENTER, . ,ENTER): '),
    catch(read(Input), _, fail),
    process_spin_input(Input, Board, NewBoard, Success),
    Success == 1,
    replace_board(GameState, NewBoard, NewGameState), !.

spin(Input, Board, NewBoard, Success):- 
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard),
    Success = 1.
spin(Input, Board, NewBoard, Success) :- 
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard),
    Success = 1.
spin(_Input, _Board, _NewBoard, Success) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D)\n'),
    Success = 0.

%returns piece and its xy
choose_piece(GameState, NewGameState, Piece, (X, Y)) :-
    [Board, _, _, Player, _, _, _, WB, WP, _] = GameState,
    choose_piece(Player, WB, WP, NewW, Piece),
    get_x_y(Piece, X, Y, Board),
    format('Piece is in x: ~w, y: ~w\n', [X, Y]),
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, NewGameState).

choose_piece(pink, _, WP, NewW, Piece) :-
    get_available_pieces(Pieces, pink, WP),
    repeat,
    format_color(pink),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail), 
    validate_piece_input(Input, Pieces, Success),
    Success == 1,
    update_waiting_pieces(Input, WP, NewW),
    get_piece(pink, Input, Piece), !.

choose_piece(blue, WB, _, NewW, Piece) :-
    get_available_pieces(Pieces, blue, WB),
    repeat,
    format_color(blue),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail),
    validate_piece_input(Input, Pieces, Success),
    Success == 1,
    updateWaiting(Input, WB, NewW), 
    getPiece(blue, Input, Piece), !.
