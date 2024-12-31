
/*USE MEANINGFUL NAMES FOR PREDICATES AND ARGUMENTS. 
TRY TO WRITE CODE THAT ‘LOOKS DECLARATIVE’ AND AVOID USING ‘IMPERATIVE-LOOKING’ 
CONSTRUCTIONS (E.G., IF-THEN-ELSE CLAUSES). TRY TO WRITE EFFICIENT CODE 
(E.G., USING TAIL RECURSION WHEN POSSIBLE).

/*d FOR UNIFORMIZATION 
PURPOSES, COORDINATES SHOULD START AT (1,1) AT THE LOWER LEFT CORNER.*/
:- consult(io).


valid_moves_piece(0, 0, blue, Board, Moves) :-
    PossibleMoves = [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)],
    include(is_valid_move(blue, Board), PossibleMoves, Moves).
valid_moves_piece(0, 0, pink, Board, Moves) :-
    PossibleMoves = [(3, 13), (3, 14), (3, 15), (3, 16), (4, 13), (4, 14), (4, 15), (4, 16)],
    include(is_valid_move(pink, Board), PossibleMoves, Moves).
valid_moves_piece(1, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(2, Y-1), (2, Y), (3, Y-4), (3, Y)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(2, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(1, Y+1), (1, Y), (4, Y-4), (4, Y)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(3, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(1, Y), (4, Y), (4, Y-1), (1, Y+4)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).
valid_moves_piece(4, Y, Player, Board, Moves) :-
    findall((X, ResY), 
            (member((X, ExprY), [(2, Y+4), (3, Y), (2, Y), (3, Y+1)]), 
            ResY is ExprY),
            PossibleMoves),
    include(is_valid_move(Player, Board), PossibleMoves, Moves).

play :-
    blutentanz,
    choose_mode(Mod), !,
    choose_start_player(Mod, Player), !,
    choose_difficulty(Mod, Dif), !,
    GameConfig = [Mod, Dif, Player], !,
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_loop(GameState).


display_game(GameState) :- 
    print_board(GameState).

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
% game over - blue won
game_over(GameState, Winner) :-
    [_, _, _, blue, _, CFb , _ , _ ,_]  = GameState,
    length(CFb, CSb),
    CSb == 5,
    Winner = blue.

% game over - pink won
game_over(GameState, Winner) :-
    [_, _, _, pink, _,  _ , CFp , _ ,_] = GameState,
    length(CFp, CSp),
    CSp == 5,
    Winner = pink.

% game over - display winner
show_winner(Winner) :-
    format_color(Winner),
    write(" won!"),
    nl.

valid_moves(GameState, ListOfMoves) :-
    get_piece_coordinates(GameState, PieceCoordinates),
    [Board, _, _, Player | _] = GameState,
    findall(Moves, (member((XPiece, YPiece), PieceCoordinates), valid_moves_piece(XPiece, YPiece, Player, Board, Moves)), ListOfListsofMoves),
    append(ListOfListsofMoves, ListOfMoves).

choose_move(GameState, 1, (Square, PlaceInSquare)) :- %internally, square is y and place x
    [Board, _,_,Player |_] = GameState,
    repeat, format_color(Player),
    write(', what square do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(SqInput), 
    nl,format_color(Player),
    write(', what symbol do you want to move your piece to? (Input your choice, then press ENTER, . ,ENTER)'),
    read(Symbol), nl,
    get_square_index(Board, SqInput, Symbol, Square, PlaceInSquare, Success), %square is col n
    format('SQInput is ~w, get square index is x:~w, y:~w ~n', [SqInput, PlaceInSquare, Square]),
    Success == 1. 

% Human vs. Human. Internally, square is y and place x
construct_move(GameState, Move, PieceGameState) :-
    Move = (X, Y),
    [Board | _] = GameState,
    repeat,
    choose_piece(GameState, PieceGameState, Piece, (Curr_X, Curr_Y)),
    format('Moving piece: ~w~n', Piece),
    [_, 1, _, Player | _ ] = PieceGameState,
    choose_move(PieceGameState, 1, (Square, PlaceInSquare)),
    format('Input move is x:~w, y:~w~n', [PlaceInSquare, Square]),
    valid_moves_piece(Curr_X, Curr_Y, Player, Board, Moves),
    write('got after valid_moves_piece!\n'),
    print(Moves),nl,
    member((PlaceInSquare, Square), Moves),
    write('Move is valid! \n'),
    format('~w is moving from x:~w y:~w to x:~w y:~w ~n', [Player, Curr_X, Curr_Y, Square, PlaceInSquare]),
    X is PlaceInSquare, 
    Y is Square,
    write('Construct move reached its end!\n'), !.

% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(Winner).
game_loop(GameState):-
    [_, _, _, _, _, _, _, _, _, human] = GameState,
    write('Human turn\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    choose_spin(GameState, SpunGameState), !,
    display_game(SpunGameState),
    call_construct_and_move(3, SpunGameState, FinalGameState), !,
    print(FinalGameState), nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState), !.
game_loop(GameState):-
    print(GameState),nl,
    [_, _, 2, _, _, _, _, _, _, bot] = GameState, %dif2
    write('Bot turn, w minimax\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    minimax_and_move(GameState, FinalGameState),!,
    print(FinalGameState),nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState), !.
game_loop(GameState):-
    [_, _, 1, _, _, _, _, _, _, bot] = GameState, %dif1
    write('bot turn woth random\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    minimax_and_move(GameState, FinalGameState),!,%random logic called here
    print(FinalGameState),nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).

clear_data :-
    retractall(board(_)).

move(GameState, Move, NewGameState) :-
    Move = (X, Y),
    [Board, _, _, _, CurrPiece | _] = GameState,
    get_x_y(CurrPiece, Old_X, Old_Y, Board),!,
    clean_square(Old_X, Old_Y, Board, TempBoard),!,
    nth1(Y, TempBoard, Square), !,
    replace_in_square(Square, X, CurrPiece, NewSquare), !,
    replace_in_board(TempBoard, Y, NewSquare, NewBoard),!,
    replace_board(GameState, NewBoard, TempState), 
    update_score(TempState, X, Y, NewGameState), !.


call_construct_and_move(0, GameState, GameState) :- !.
call_construct_and_move(N, GameState, FinalGameState) :-
    N > 0,repeat, write('here before construct \n'),!,
    construct_move(GameState, (NewX, NewY), PieceGameState), 
    write('passed construct \n'),!,
    format('Move chosen: (~w, ~w)~n', [NewX, NewY]),
    move(PieceGameState,(NewX, NewY), MovedGameState),
    write('Exited move successfuly!'),
    display_game(MovedGameState),
    N1 is N - 1,
    call_construct_and_move(N1, MovedGameState, FinalGameState), !.
call_construct_and_move(_N, GameState, GameState) :-
    [_,_,_,Player |_] = GameState,
    valid_moves(GameState, Moves),
    (\+ Moves), !, format_color(Player),
    write(', you ran out of possible moves. Switching turn.\n'), !.

minimax_and_move(GameState, FinalGameState) :-
    [Board, _, _, Player | _] = GameState,
    minimax(GameState, 2, Player, BestMove, _),
    BestMove = (Spin, Moves),
    format('Move chosen: ~w~n', [BestMove]),
    spin(Spin, Board, NewBoard, Success),
    Success == 1,
    replace_board(GameState, NewBoard, SpunGameState),
    call_move(SpunGameState, Moves, FinalGameState), !.

call_move(GameState, [], MovedGameState) :-
    MovedGameState is GameState.
call_move(GameState, [H|T], FinalGameState) :-
    [_,_,_, pink, _, _,_,_,WP,_] = GameState,
    H = (Piece, X, Y), Move = (X, Y),
    get_piece(pink, Input, Piece),
    update_waiting_pieces(Input, WP, NewW), !,
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, PieceGameState),
    move(PieceGameState, Move, MovedGameState),
    column_index(Col, Y), get_piece(pink, P, Piece), format_color(pink),
    format(' is moving piece ~w to Row ~w and Column ~w~n', [P, X, Col]),
    display_game(MovedGameState),
    call_move(MovedGameState, T, FinalGameState).
call_move(GameState, [H|T], FinalGameState) :-
    [_,_,_, blue, _, _,_,WB,_] = GameState,
    H = (Piece, X, Y), Move = (X, Y),
    get_piece(blue, Input, Piece),
    update_waiting_pieces(Input, WB, NewW), !,
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, NewGameState),
    move(NewGameState, Move, MovedGameState),
    column_index(Col, Y),  get_piece(blue, P, Piece), format_color(blue),
    format(' is moving piece ~w to Row ~w and Column ~w~n', [P, X, Col]),
    display_game(MovedGameState),
    call_move(MovedGameState, T, FinalGameState).

% Choosing Heuristic
rotation_value(GameState, SortedResult, TotalBenefit) :-
    [Board, Mod, Dif, Player, Piece, CFB, CFP, WB, WP, Type] = GameState,
    Opponent = opponent(Player),
    [Board, Mod, Dif, Opponent, Piece, CFB, CFP, WB, WP, Type]= OpponentGameState,
    get_piece_coordinates(GameState, PlayerPieceCoordinates),
    valid_moves(GameState, PlInitMoves),           
    valid_moves(OpponentGameState, OpInitMoves),      
    include(is_score_point(Player), PlInitMoves, PlayerStartScoreMoves),  
    include(is_score_point(Opponent), OpInitMoves, OpponentStartScoreMoves),    
    unpack_coordinates(PlayerPieceCoordinates, Xs, Ys),      
    list_to_set(Xs, XSet),                             
    list_to_set(Ys, YSet),
    findall((RowIndex, NewBoard), 
        (member(RowIndex, XSet), spin_row(RowIndex, Board, NewBoard)), SpunRows),
    findall((ColChar, NewBoard), 
        (member(ColIndex, YSet), column_index(ColChar, ColIndex), spin_column(ColIndex, Board, NewBoard)), SpunCols),
    append(SpunRows, SpunCols, AllSpunBoards),
    findall((Index, ValidMoves), 
        (member((Index, Board), AllSpunBoards), valid_moves(Board, ValidMoves)), 
        SpunMoves),
    findall((Index, Moves, PlayerScoreMoves), 
        (member((Index, Moves), SpunMoves), 
         include(is_score_point(Player), Moves, PlayerScoreMoves)), 
        PlayerResults),
    findall((Index, Moves, OpponentScoreMoves), 
        (member((Index, Moves), SpunMoves), 
         include(is_score_point(Opponent), Moves, OpponentScoreMoves)), 
        OpponentResults),
    pack_results(PlayerResults, OpponentResults, Result),
    calculate_benefits(Result, PlayerStartScoreMoves, OpponentStartScoreMoves, TotalBenefit),
    sort_by_benefit(Result, SortedResult).
% Sort the results by benefit
sort_by_benefit(Result, SortedResult) :-
    findall(
        (Benefit, MoveLength, Index, Moves),
        (
            member((Index, Moves, PlayerScoreMoves, OpponentScoreMoves), Result),
            compare_rotation_benefits(PlayerScoreMoves, [], OpponentScoreMoves, [], Benefit),
            length(Moves, MoveLength)
        ),
        ResultsWithKeys
    ),
    sort(0, @>=, ResultsWithKeys, SortedResultsWithKeys),
    findall(
        (Index, Moves),
        member((_Benefit, _MoveLength, Index, Moves), SortedResultsWithKeys),
        SortedResult
    ).
calculate_benefits(Result, PlayerStartScoreMoves, OpponentStartScoreMoves, TotalBenefit) :-
    findall(
        Benefit, 
        (
            member((_Index, _Moves, PlayerScoreMoves, OpponentScoreMoves), Result), 
            compare_rotation_benefits(
                PlayerScoreMoves, PlayerStartScoreMoves, 
                OpponentScoreMoves, OpponentStartScoreMoves, 
                Benefit
            )
        ),
        Benefits
    ),
    sum_list(Benefits, TotalBenefit).


% Comparison of rotation benefits
compare_rotation_benefits(CurrScoreMoves, CurrStartScoreMoves, OpponentScoreMoves, OpponentStartScoreMoves, Benefit) :-
    length(CurrScoreMoves, CurrScoreCount),
    length(CurrStartScoreMoves, CurrStartCount),
    PlayerDelta is CurrScoreCount - CurrStartCount,
    length(OpponentScoreMoves, OpponentScoreCount),
    length(OpponentStartScoreMoves, OpponentStartCount),
    OpponentDelta is OpponentScoreCount - OpponentStartCount,
    Benefit is PlayerDelta - OpponentDelta.
% Combining results of player and opponent
pack_results([], [], []).
pack_results([(Index, Moves, PlayerScoreMoves) | PlayerTail], 
                [(Index, Moves, OpponentScoreMoves) | OpponentTail], 
                [(Index, Moves, PlayerScoreMoves, OpponentScoreMoves) | ResultTail]) :-
   pack_results(PlayerTail, OpponentTail, ResultTail).

% Global heuristic for evaluation
value(GameState, Player, Value) :-
    [_,_,_,Player, _|_] = GameState,
    rotation_value(GameState, _Result, Value).
% Minimax with sequential move evaluation after rotation
minimax(GameState, 2, Player, BestMove, BestValue) :-
    rotation_value(GameState, SortedResult, _TotalBenefit),
    evaluate_rotations(SortedResult, GameState, 2, Player, -inf, (BestMove, BestValue)).
minimax(GameState, 0, Player, _, Value) :-
    value(GameState, Player, Value).
evaluate_rotations([], _, _, _, BestSoFar, BestSoFar).
evaluate_rotations([(SpinIndex, Moves) | Rotations], GameState, Depth, Player, (CurrBestMove, CurrBestValue), Best) :-
    [Board | _] = GameState,
    spin(SpinIndex, Board, NewBoard, Success),
    Success == 1,
    replace_board(GameState, NewBoard, RotatedGameState),
    evaluate_sequential_moves(Moves, RotatedGameState, Depth, Player, SpinIndex, (CurrBestMove, CurrBestValue), NewBest),
    evaluate_rotations(Rotations, GameState, Depth, Player, NewBest, Best).
evaluate_sequential_moves([], _, _, _, _, BestSoFar, BestSoFar).
evaluate_sequential_moves([Move | Moves], RotatedGameState, Depth, Player, SpinIndex, (CurrBestMove, CurrBestValue), Best) :-
    move(RotatedGameState, Move, NewGameState),  % Apply current move
    opponent(Player, Opponent),
    NewDepth is Depth - 1,
    minimax(NewGameState, NewDepth, Opponent, _, OpponentValue),  % Predict opponent's response
    Value is -OpponentValue,
    update_best_move(Value, CurrBestValue, (SpinIndex, [(Move)]), CurrBestMove, NewBest),  % Update best move with spin index and move
    evaluate_sequential_moves(Moves, NewGameState, Depth, Player, SpinIndex, NewBest, Best).
% Update the best move
update_best_move(Value, CurrBestValue, Move, _CurrBestMove, NewBest) :-
    Value > CurrBestValue,
    NewBest = (Move, Value).
update_best_move(Value, CurrBestValue, _Move, CurrBestMove, NewBest) :-
    Value =< CurrBestValue,
    NewBest = (CurrBestMove, CurrBestValue).