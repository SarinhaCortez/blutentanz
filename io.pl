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

% Initial  IO
choose_spin(GameState, NewGameState) :-
    [_, _, _, Player|_] = GameState,
    repeat,
    format_color(Player),
    write(', choose a row (1-4) or column (A-D) to spin (Input your choice, then press ENTER, . ,ENTER): '),
    catch(read(Input), _, fail),
    spin(Input, GameState, NewGameState, Success),
    Success == 1, !.

spin(Input, GameState, NewGameState, 1):- 
    [Board | _] = GameState,
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard),
    replace_board(GameState, NewBoard,SpunGameState),
    scored_spinning(SpunGameState, NewGameState).
spin(Input, GameState, NewGameState, 1) :- 
    [Board | _] = GameState,
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard),
    replace_board(GameState, NewBoard,SpunGameState),
    scored_spinning(SpunGameState, NewGameState).
spin(0, GameState, NewGameState, 1) :-
    [Board | _] = GameState,
    random(1, 4, Index),
    random_member(SpinType, [spin_row, spin_column]),
    call(SpinType, Index, Board, NewBoard), !,
    replace_board(GameState, NewBoard,SpunGameState),
    scored_spinning(SpunGameState, NewGameState).
spin(Input, _GS, _NewGS, 0) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D) Input was'), print(Input), nl.

scored_spinning(GameState, NewGameState) :-
    [Board,_,_,pink,_,CSB|_] = GameState,
    [Board,_,_,blue,_,CSB|_] = OpGameState,
    get_piece_coordinates(GameState, Coords),
    get_piece_coordinates(OpGameState, OpCoords),
    findall(
        (X, Y),
        (member((_, X, Y), Coords), is_score_point(pink, ( X, Y))),
        PinkScorePoints
    ),
    findall(
        (X, Y),
        (member((_, X, Y), OpCoords), is_score_point(blue, ( X, Y))),
        BlueScorePoints
    ),
    update_score_for_points(PinkScorePoints, GameState, UpdatedGameState),
    update_score_for_points(BlueScorePoints, UpdatedGameState, NewGameState).

scored_spinning(GameState, NewGameState) :-
    [Board,_,_,blue,_,_,CSP|_] = GameState,
    [Board,_,_,pink,_,_,CSP|_] = OpGameState,
    get_piece_coordinates(GameState, Coords),
    get_piece_coordinates(OpGameState, OpCoords),
    findall(
        (X, Y),
        (member((_, X, Y), Coords), is_score_point(blue, (X, Y))),
        BlueScorePoints
    ),
    findall(
        (X, Y),
        (member((_, X, Y), OpCoords), is_score_point(pink, (X, Y))),
        PinkScorePoints
    ),
    update_score_for_points(PinkScorePoints, GameState, UpdatedGameState),
    update_score_for_points(BlueScorePoints, UpdatedGameState, NewGameState).

%returns piece and its xy
choose_piece(GameState, NewGameState, Piece, (X, Y)) :-
    [Board, _, _, Player, _, _,_, WB, WP, _] = GameState,
    select_cs(GameState, CS),
    choose_piece(Player, WB, WP, NewW, Piece, CS),
    get_x_y(Piece, X, Y, Board),
    format('Piece is in x: ~w, y: ~w\n', [X, Y]),
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, NewGameState).

choose_piece(pink, _, WP, NewW, Piece, CSP) :-
    get_waiting_pieces(Pieces, pink, WP, CSP), !,
    repeat,
    format_color(pink),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail), 
    validate_piece_input(Input, Pieces, Success),
    Success == 1,
    update_waiting_pieces(Input, WP, NewW),
    get_piece(pink, Input, Piece), !.

choose_piece(blue, WB, _, NewW, Piece,CSB) :-
    get_waiting_pieces(Pieces, blue, WB, CSB), !,
    repeat,
    format_color(blue),
    write(', what piece do you want to move? (Input your choice, then press ENTER, . ,ENTER):\nYou can choose from '),
    print(Pieces),
    catch(read(Input), _, fail), 
    validate_piece_input(Input, Pieces, Success),
    Success == 1,
    update_waiting_pieces(Input, WB, NewW),
    get_piece(blue, Input, Piece), !.


