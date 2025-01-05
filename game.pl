:- consult(io).

% Predicate to get valid moves for a piece at position (0, 0) for blue player
valid_moves_piece(0, 0, blue, Board, Moves) :-
    include(is_valid_move(blue, Board), [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)], Moves).

% Predicate to get valid moves for a piece at position (0, 0) for pink player
valid_moves_piece(0, 0, pink, Board, Moves) :-
    include(is_valid_move(pink, Board), [(3, 13), (3, 14), (3, 15), (3, 16), (4, 13), (4, 14), (4, 15), (4, 16)], Moves).

% Predicate to get valid moves for a piece at position (1, Y) for any player
valid_moves_piece(1, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(2, Y-1), (2, Y), (3, Y-4), (3, Y)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).

% Predicate to get valid moves for a piece at position (2, Y) for any player
valid_moves_piece(2, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(1, Y+1), (1, Y), (4, Y-4), (4, Y)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).

% Predicate to get valid moves for a piece at position (3, Y) for any player
valid_moves_piece(3, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(1, Y), (4, Y), (4, Y-1), (1, Y+4)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).

% Predicate to get valid moves for a piece at position (4, Y) for any player
valid_moves_piece(4, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(2, Y+4), (3, Y), (2, Y), (3, Y+1)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).

% Predicate to start the game
play :-
    blutentanz,
    choose_mode(Mod), !,
    choose_start_player(Mod, Player), !,
    choose_difficulty(Mod, Dif), !,
    initial_state([Mod, Dif, Player], GameState),
    display_game(GameState),
    game_loop(GameState).

% Predicate to display the game state
display_game(GameState) :- 
    print_board(GameState).

% Predicate to initialize the game state
initial_state(GameConfig, GameState) :-
    [3, Dif, Player] = GameConfig,!,
    board(Board),
    shuffle_board(Board, ShuffledBoard), !,
    GameState = [ShuffledBoard, 3, Dif, Player, -1, [], [], 5, 5, bot], !. %minus one is current piece to move
initial_state(GameConfig, GameState) :-
    [Mode, Dif, Player] = GameConfig,
    board(Board),
    shuffle_board(Board, ShuffledBoard),
    GameState = [ShuffledBoard, Mode, Dif, Player, -1, [], [], 5, 5, human], !.

% Predicate to check if the game is over and blue won
game_over(GameState, blue) :-
    [_, _, _, blue, _, CFb , _ , _, _ ,_]  = GameState,
    length(CFb, 5).

% Predicate to check if the game is over and pink won
game_over(GameState, pink) :-
    [_, _, _, pink, _,  _ , CFp , _, _ ,_] = GameState,
    length(CFp, 5).

% Predicate to display the winner if blue won
show_winner(blue) :-
    repeat_format_color(23, '*'), nl,
    repeat_format_color(23, '-'), nl, 
    format_color('*'),
    write(' '), format_color(blue), write(' won! Congrats! '),
    format_color('*'), nl,
    repeat_format_color(23, '-'), nl, 
    repeat_format_color(23, '*'), nl.

% Predicate to display the winner if pink won
show_winner(pink) :-
    repeat_format_color(23, '+'), nl,
    repeat_format_color(23, '-'), nl, 
    format_color('+'),
    write(' '), format_color(pink), write(' won! Congrats! '), 
    format_color('+'), nl,
    repeat_format_color(23, '-'), nl, 
    repeat_format_color(23, '+'), nl.

% Predicate to get valid moves for all pieces in the game state
valid_moves(GameState, ListOfMoves) :-
    get_piece_coordinates(GameState, PieceCoordinates),
    [Board, _, _, Player | _] = GameState,
    findall((Piece, X, Y),
        (
            member((Piece, XPiece, YPiece), PieceCoordinates),
            valid_moves_piece(XPiece, YPiece, Player, Board, Moves),
            member((X, Y), Moves)
        ),
        ListOfMoves).

% Predicate to choose a move for a human player
choose_move(GameState, 1, (Square, PlaceInSquare)) :- %internally, square is y and place x
    [Board, _,_,Player |_] = GameState,
    repeat, nl,
    format_color(Player),
    write(', you may choose your destination square using a combination of a lowercase row char (a to d) and a number (1 to 4). Ex: a4.\n'),
    write('You may also choose an unnocupied symbol, that may be '), format_color('-'), write(' or '), format_color('*'),write(' if you are '),format_color(blue), write(', or '), format_color('-'), write(' or '), format_color('+'),write(' if you are '), format_color(pink),
    write('.\nYou can only move your piece to adjacent symbols, orthogonally. If your input does not fulfill this requirements, you will be asked to input a new one.\n\n'),
    format_color(Player),
    write(', what square do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(SqInput), 
    nl,format_color(Player),
    write(', what symbol do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(Symbol), nl,
    get_square_index(Board, SqInput, Symbol, Square, PlaceInSquare, 1), %square is col n
    format('SQInput is ~w, get square index is x:~w, y:~w ~n', [SqInput, PlaceInSquare, Square]). 

% Predicate to construct a move for a human player
construct_move(GameState, Move, PieceGameState) :-
    Move = (X, Y),
    [Board, _,_, Player | _] = GameState,
    repeat,
    choose_piece(GameState, PieceGameState, _Piece, (Curr_X, Curr_Y)),
    choose_move(PieceGameState, 1, (Square, PlaceInSquare)),
    valid_moves_piece(Curr_X, Curr_Y, Player, Board, Moves),
    member((PlaceInSquare, Square), Moves),
    format('~w is moving from x:~w y:~w to x:~w y:~w ~n', [Player, Curr_X, Curr_Y, Square, PlaceInSquare]),
    X is PlaceInSquare, 
    Y is Square.

% Predicate to handle the game loop
%game over
game_loop(GameState):-
    game_over(GameState, Winner), !,
    show_winner(Winner).
%human turn
game_loop(GameState) :-
    [_, _, _, _, _, _, _, _, _, human] = GameState,
    write('Human turn\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    choose_spin(GameState, SpunGameState), !,
    display_game(SpunGameState),
    call_construct_and_move(3, SpunGameState, FinalGameState), !,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).
%computer turn with difficulty 2
game_loop(GameState) :-
    [_, _, 2, _, _, _, _, _, _, bot] = GameState, % Difficulty 2
    write('Bot turn, greedy algorithm\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    greedy_move(GameState, FinalGameState),
    display_game(FinalGameState),
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).
%computer turn with difficulty 1
game_loop(GameState):-
    [_, _, 1, _, _, _, _, _, _, bot] = GameState, %dif1
    write('Bot turn, with random\n'), nl,
    print_turn(GameState),
    random_moves(GameState, Moves, WGameState),!,
    call_move(WGameState, Moves, FinalGameState),
    display_game(FinalGameState),
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).



% Predicate to execute a move
move(GameState, Move, NewGameState) :-
    Move = (X, Y),
    [Board, _, _, _, CurrPiece | _] = GameState,
    get_x_y(CurrPiece, Old_X, Old_Y, Board),!, write('Old x is '), print(Old_X), write(' Old y is '), print(Old_Y), nl,
    clean_square(Old_X, Old_Y, Board, TempBoard),!,
    nth1(Y, TempBoard, Square), !,
    replace_in_square(Square, X, CurrPiece, NewSquare), !,
    replace_in_board(TempBoard, Y, NewSquare, NewBoard),!,
    replace_board(GameState, NewBoard, TempState), 
    update_score(TempState, X, Y, NewGameState), !.

% Predicate to call construct and move
call_construct_and_move(0, GameState, GameState) :- !.
call_construct_and_move(N, GameState, GameState) :-
    N > 0, 
    write('If you don\'t want to make more moves, you can stop now by pressing \'s\'. If you want to continue, press \'c\'. (after your choice) press ENTER, \'.\', ENTER):'),
    read('s').
call_construct_and_move(N, GameState, FinalGameState) :-
    N > 0, !, 
    repeat,
    construct_move(GameState, (NewX, NewY), PieceGameState), 
    move(PieceGameState,(NewX, NewY), MovedGameState),
    display_game(MovedGameState),
    N1 is N - 1,
    call_construct_and_move(N1, MovedGameState, FinalGameState), !.
call_construct_and_move(_N, GameState, GameState) :-
    valid_moves(GameState, Moves),
    [_, _, _, Player | _] = GameState,
    has_no_moves(Moves), 
    format_color(Player), write(' ran out of possible moves. Switching turn.\n'),!.


% Predicate to call a move
call_move(GameState, [], GameState) :-  !.
call_move(GameState, [(-1,0,0)|_], GameState) :- !.
call_move(GameState, [H|T], FinalGameState) :-
    select_w(GameState, W), W >= 0,
    H = (Piece, X, Y), Move = (X, Y),
    format('Piece: ~w, X: ~w, Y: ~w~n', [Piece, X, Y]),
    replace_current_piece_waiting_pieces(GameState, W, Piece, PieceGameState),
    move(PieceGameState, Move, MovedGameState),
    display_game(MovedGameState),
    call_move(MovedGameState, T, FinalGameState).

random_moves(GameState, Moves, NewGameState) :-
    [_, _, _, Player | _] = GameState,
    display_game(GameState),
    spin(0, GameState, SpunGameState, 1),
    [SpunBoard|_] = SpunGameState,
    format_color(Player), write(' spinned!\n'),
    display_game(SpunGameState),
    random_move(SpunGameState, Move1, GameState1), !,
    random_move(GameState1, Move2, GameState2), !,
    random_move(GameState2, Move3, MovedGameState),!,
    replace_board(MovedGameState, SpunBoard, NewGameState),
    Moves = [Move1, Move2, Move3].
random_move(GameState, Move, NewGameState) :-
    [Board, _, _, Player | _] = GameState,
    select_w(GameState, Player, W), W > 0, 
    valid_moves_piece(0, 0, Player, Board, Moves), 
    \+ has_no_moves(Moves),!,
    findall(Piece, (between(1, W, X), get_piece(Player, X, Piece)), ListOfPieces), !,
    last(ListOfPieces, Piece), 
    random_member(RandomMove, Moves),
    RandomMove = (X, Y),
    Move = (Piece, X, Y), 
    NewW is W - 1,
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, PieceGameState),
    move(PieceGameState, RandomMove, NewGameState).
random_move(GameState, Move, NewGameState) :-
    select_w(GameState, 0), 
    valid_moves(GameState, Moves),
    \+ has_no_moves(Moves), !,
    random_member(Move, Moves),
    Move = (P, X, Y), M = (X, Y),
    replace_current_piece_waiting_pieces(GameState, 0, P, PieceGameState),
    move(PieceGameState, M, NewGameState).
random_move(GameState, (-1,0,0), GameState) :-
    [Board, _, _, Player | _] = GameState,
    valid_moves_piece(0, 0, Player, Board, Moves),
    has_no_moves(Moves), !.
random_move(GameState, (-1,0,0), GameState) :-
    valid_moves(GameState, Moves),
    has_no_moves(Moves), !.

%greedy

% Define the greedy move that selects the best spin based on the criteria.
greedy_move(GameState, FinalGameState) :-
    % Retrieve the current board and player from the game state
    GameState = [_, _, _, Player| _], 
    
    % Generate all possible spins and evaluate them
    Spins = [1,2,3,4,'a','b','c','d'],
    % Evaluate all possible spins and find the best one
    evaluate_spins(Spins, GameState , BestMove),

    % Apply the best move to the board and update the game state
    spin(BestMove, GameState, SpunGameState, 1),

    format_color(Player), write(' spinned!\n'),

    display_game(SpunGameState),

    % Perform 3 greedy moves
    greedy_move_piece(SpunGameState, GameState1), !,
    display_game(GameState1),
    greedy_move_piece(GameState1, GameState2), !,
    display_game(GameState2),
    greedy_move_piece(GameState2, FinalGameState), !.


greedy_move_piece(GameState, NewGameState) :-
    [_, _, _, Player | _] = GameState,

    % Get the list of waiting pieces using get_waiting_pieces
    get_waiting_pieces(WaitingPieces, GameState),

    % Perform the conversion of waiting pieces based on the current player
    findall(NewPiece, (
        member(Piece, WaitingPieces),
        get_piece(Player, Piece, NewPiece) 
    ), ConvertedWaitingPieces),

    % Retrieve all valid moves
    valid_moves(GameState, AllMoves),

    % Filter moves to only include those for waiting pieces
    include(valid_move_for_waiting_piece_param(ConvertedWaitingPieces), AllMoves, FilteredMoves),

    has_no_moves(FilteredMoves), !,
    NewGameState = GameState.


greedy_move_piece(GameState, NewGameState) :-
    [Board, _, _, Player | _] = GameState,

    % Get the list of waiting pieces using get_waiting_pieces
    get_waiting_pieces(WaitingPieces, GameState),

    % Perform the conversion of waiting pieces based on the current player
    findall(NewPiece, (
        member(Piece, WaitingPieces),
        get_piece(Player, Piece, NewPiece) 
    ), ConvertedWaitingPieces),

    % Retrieve all valid moves
    valid_moves(GameState, AllMoves),

    % Filter moves to only include those for waiting pieces
    include(valid_move_for_waiting_piece_param(ConvertedWaitingPieces), AllMoves, FilteredMoves),

    % Ensure there are valid moves after filtering
    \+ has_no_moves(FilteredMoves),

    % Evaluate each filtered move to find the best one
    findall(
        Score-(Piece, X, Y),
        (
            member((Piece, X, Y), FilteredMoves),
            replace_current_piece_waiting_pieces(GameState, _, Piece, TempGameState),
            move(TempGameState, (X, Y), TempResultGameState),
            evaluate_move(GameState, TempResultGameState, Player, Score)
        ),
        MoveScores
    ),
    keysort(MoveScores, SortedMoveScores),
    reverse(SortedMoveScores, [(_-BestMove)|_]),

    % Extract the best move details
    BestMove = (Piece, X, Y),
    get_x_y(Piece, Xnow, Ynow, Board),
    valid_coordinate((Xnow,Ynow)),

    select_w(GameState, Player, W),
    replace_current_piece_waiting_pieces(GameState, W, Piece, TempGameState),
    % Apply the move
    move(TempGameState, (X, Y), NewGameState).


greedy_move_piece(GameState, NewGameState) :-
    [_, _, _, Player | _] = GameState,

    % Get the list of waiting pieces using get_waiting_pieces
    get_waiting_pieces(WaitingPieces, GameState),


    % Perform the conversion of waiting pieces based on the current player
    findall(NewPiece, (
        member(Piece, WaitingPieces),
        get_piece(Player, Piece, NewPiece) 
    ), ConvertedWaitingPieces),

    % Retrieve all valid moves
    valid_moves(GameState, AllMoves),

    % Filter moves to only include those for waiting pieces
    include(valid_move_for_waiting_piece_param(ConvertedWaitingPieces), AllMoves, FilteredMoves),

    % Ensure there are valid moves after filtering
    \+ has_no_moves(FilteredMoves),

    % Evaluate each filtered move to find the best one
    findall(
        Score-(Piece, X, Y),
        (
            member((Piece, X, Y), FilteredMoves),
            replace_current_piece_waiting_pieces(GameState, _, Piece, TempGameState),
            move(TempGameState, (X, Y), TempResultGameState),
            evaluate_move(GameState, TempResultGameState, Player, Score)
        ),
        MoveScores
    ),
    keysort(MoveScores, SortedMoveScores),
    reverse(SortedMoveScores, [(_-BestMove)|_]),


    % Extract the best move details
    BestMove = (Piece, X, Y),

    % Select waiting pieces
    select_w(GameState, Player, W),

    % Decrement W and update game state
    NewW is W - 1,
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, UpdatedGameState),

    % Apply the move
    move(UpdatedGameState, (X, Y), NewGameState).

% Predicate to match valid moves based on waiting pieces
valid_move_for_waiting_piece_param(WaitingPieces, (Piece, _, _)) :-
    member(Piece, WaitingPieces).


% Evaluate a move based on how much it brings the piece closer to the edge
evaluate_move(GameState, NewGameState, Player, Score) :-
    NewGameState = [_, _, _, _, CurrentPiece | _],
    get_piece_coordinates(GameState, CurrentPositions),
    get_piece_coordinates(NewGameState, NewPositions),
   

    include(is_current_piece(CurrentPiece), CurrentPositions, FilteredCurrentPositions),
    include(is_current_piece(CurrentPiece), NewPositions, FilteredNewPositions),
    count_closer_positions(FilteredCurrentPositions, FilteredNewPositions, Player, Score).

is_current_piece(Piece, (Piece, _, _)).

% Evaluate all possible spins
evaluate_spins(Spins, GameState , BestMove) :-

    % For each spin, evaluate the three criteria and assign a score
    findall(
        Score-Spin,
        (
            member(Spin, Spins),
            spin(Spin, GameState, NewGameState, _),
            evaluate_spin(Spin, GameState, NewGameState, Score)
        ),
        SpinScores
    ),
    
    % Sort by score in descending order to prioritize best moves
    keysort(SpinScores, SortedScoresAscending),
    reverse(SortedScoresAscending, SortedScores),

    % Take the first element in the sorted list as the best move
    SortedScores = [(_-BestMove)|_].


% Evaluate a single spin based on three criteria
evaluate_spin(Spin, GameState, NewGameState, Score) :-
    [Board, _, _, Player | _] = GameState,
    [NewBoard, _, _, Player | _] = NewGameState,
    % Calculate how much the spin moves the player's pieces closer to the edge
    closer_to_edge(GameState, NewGameState, EdgeMoveScore),
    
    % Calculate the number of valid moves for the current player and the opponent
    valid_moves(GameState, PlayerMovesBefore),
    opponent(Player, Opponent),
    [Board, _, _, Opponent | _] = OppGameState,
    valid_moves(OppGameState, OpponentMovesBefore),
    
    % Get the current state after the spin
    valid_moves(NewGameState, PlayerMovesAfter),
    [NewBoard, _, _, Opponent | _] = NewOppGameState,
    valid_moves(NewOppGameState, OpponentMovesAfter),

    length(PlayerMovesBefore, PMBcount),
    length(PlayerMovesAfter, PMAcount),
    length(OpponentMovesBefore, OMBcount),
    length(OpponentMovesAfter, OMAcount),
    
    % Calculate the increase in valid moves for the player
    IncreasePlayerMoves is PMAcount - PMBcount,

    % Calculate the decrease in valid moves for the opponent
    DecreaseOpponentMoves is OMBcount - OMAcount,
    
    % Combine all criteria to generate a final score
    Score is EdgeMoveScore + IncreasePlayerMoves + DecreaseOpponentMoves.

% Calculate how much the spin moves the current player's pieces closer to the edge
closer_to_edge(GameState, NewGameState, Score) :-
    [_,_,_,Player | _] = GameState,
    % Get current and new piece positions
    get_piece_coordinates(GameState, CurrentPositions),
    get_piece_coordinates(NewGameState, NewPositions),
    
    % Count how many pieces have moved closer to the target edge
    count_closer_positions(CurrentPositions, NewPositions, Player, Score).

% Count the number of pieces that moved closer to the target edge
count_closer_positions(CurrentPositions, NewPositions, Player, Score) :-
    findall(Score,
        (
            member((Piece, X1, Y1), CurrentPositions),
            member((Piece, X2, Y2), NewPositions),
            evaluate_position_change(Player, X1, Y1, X2, Y2, Score)
        ),
        Scores),
    sum_list(Scores, Score).

% Evaluate position change for blue pieces (row transition)
evaluate_position_change(blue, _, Y1, _, Y2, 10) :-
    Y2 =:= Y1 + 4.

evaluate_position_change(pink, _, 0, _, Y2, 10) :-
    Y2 >= 12, Y2 =< 15.
% Evaluate position change for pink pieces (row transition)
evaluate_position_change(pink, _, Y1, _, Y2, 10) :-
    Y2 =:= Y1 - 4.

% Evaluate position change for blue pieces (within-row movement)
evaluate_position_change(blue, X1, Y1, X2, Y2, 5) :-
    Y2 =:= Y1,
    X2 > 1.

% Evaluate position change for pink pieces (within-row movement)
evaluate_position_change(pink, X1, Y1, X2, Y2, 5) :-
    Y2 =:= Y1,
    X2 < 2.

% Default case: no improvement
evaluate_position_change(_, _, _, _, _, 0).

