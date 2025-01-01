
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
    %blutentanz,
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
    [_, _, _, Player | _ ] = PieceGameState,
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
    update_score(TempState, X, Y, NewGameState), 
    write('exiting move\n'),!.


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
    format('Piece: ~w, X: ~w, Y: ~w~n', [Piece, X, Y]),
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
    format('Piece: ~w, X: ~w, Y: ~w~n', [Piece, X, Y]),
    get_piece(blue, Input, Piece),
    update_waiting_pieces(Input, WB, NewW), !,
    replace_current_piece_waiting_pieces(GameState, NewW, Piece, NewGameState),
    move(NewGameState, Move, MovedGameState),
    column_index(Col, Y), 
    get_piece(blue, P, Piece), format_color(blue),
    format(' is moving piece ~w to Row ~w and Column ~w~n', [P, X, Col]),
    display_game(MovedGameState),
    call_move(MovedGameState, T, FinalGameState).

% Choosing Heuristic
rotation_value(GameState, SortedResult, TotalBenefit) :-
    [Board, Mod, Dif, Player, Piece, CFB, CFP, WB, WP, Type] = GameState,
    opponent(Player, Opponent),
    [Board, Mod, Dif, Opponent, Piece, CFB, CFP, WB, WP, Type]= OpponentGameState,
    get_piece_coordinates(GameState, PlayerPieceCoordinates),
    valid_moves(GameState, PInitMoves), 
    %PInitMoves = [FirstMove|_],
    remove_duplicates(PInitMoves, PlInitMoves),
    print(PlInitMoves), nl,        
    valid_moves(OpponentGameState, OInitMoves),  remove_duplicates(OInitMoves, OpInitMoves),
    print(OpInitMoves), nl,
    include(is_score_point(Player), PlInitMoves, PlayerStartScoreMoves),  
    include(is_score_point(Opponent), OpInitMoves, OpponentStartScoreMoves),    
    unpack_coordinates(PlayerPieceCoordinates, Xs, Ys),      
    remove_duplicates(Xs, XSet),                             
    remove_duplicates(Ys, YSet),
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
    keysort(ResultsWithKeys, AscendingSorted),
    reverse(AscendingSorted, SortedResultsWithKeys),

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
    get_score(Player, GameState, PlScore), !,
    Opponent = opponent(Player),
    get_score(Opponent, GameState, OpoScore), !,
    rotation_value(GameState, _, TotalBenefit),
    RValue is TotalBenefit,
    Value is (0.6 * (PlScore - OpoScore) + 0.4 * RValue).  

% --- Minimax with Sequential Move Evaluation After Rotation --- 

minimax(GameState, 2, Player, BestMove, BestValue) :-
    rotation_value(GameState, SortedResult, _TotalBenefit), 
    \+(member(_,SortedResult)), 
    evaluate_rotations([(0, [])], GameState, 2, Player, -inf, (BestMove, BestValue)), write('here\n').

minimax(GameState, 0, Player, _, Value) :-
    value(GameState, Player, Value).

% --- Evaluate Rotations --- 

evaluate_rotations([], _, _, _, BestSoFar, BestSoFar).
evaluate_rotations([(0, [])], GameState, Depth, Player, BestSoFar, Best) :-
    [Board | _] = GameState,
    spin(0, Board, NewBoard, Success), Success == 1,
    replace_board(GameState, NewBoard, RotatedGameState), 
    evaluate_sequential_moves([0], RotatedGameState, Depth, Player, 0, BestSoFar, NewBest),
    evaluate_rotations([], GameState, Depth, Player, NewBest, Best).
evaluate_rotations([(SpinIndex, Moves) | Rotations], GameState, Depth, Player, BestSoFar, Best) :-
    [Board | _] = GameState,
    spin(SpinIndex, Board, NewBoard, Success), Success == 1,
    replace_board(GameState, NewBoard, RotatedGameState),
    evaluate_sequential_moves(Moves, RotatedGameState, Depth, Player, SpinIndex, BestSoFar, NewBest),
    evaluate_rotations(Rotations, GameState, Depth, Player, NewBest, Best).


% --- Evaluate Sequential Moves (with Off-board Pieces Priority) --- 

evaluate_sequential_moves([], _, _, _, _, BestSoFar, BestSoFar).
evaluate_sequential_moves([0 | Moves], GameState, Depth, Player, SpinIndex, BestSoFar, Best) :-    
    
    get_piece_coordinates(GameState, PlayerPieceCoordinates),
    member((0, 0), PlayerPieceCoordinates),!,  % Cut here to prevent other clauses from being checked
    select_piece_off_board(GameState, Move, PieceGameState),
    write('off board move\n'),
    move(PieceGameState, Move, NewGameState),
    NewDepth is Depth - 1,
    minimax(NewGameState, NewDepth, Player, _, OpponentValue),
    Value is -OpponentValue,
    update_best_move(Value, BestSoFar, (SpinIndex, [Move]), NewBest),
    write('newbestie: '), print(NewBest), nl,
    evaluate_sequential_moves(Moves, NewGameState, Depth, Player, SpinIndex, NewBest, Best).
/*
% If no piece is off the board, proceed with regular move evaluation
evaluate_sequential_moves([Move | Moves], GameState, Depth, Player, SpinIndex, BestSoFar, Best) :-
    valid_moves(GameState, ValidMoves),
    select_best_move(GameState, ValidMoves, Move),
    [Board,_,_,Player| _] = GameState,
    select_w(GameState, W),
    get_x_y(Piece, Player, W),
    move(GameState, Move, NewGameState),
    NewDepth is Depth - 1,
    minimax(NewGameState, NewDepth, Player, _, OpponentValue),
    Value is -OpponentValue,
    update_best_move(Value, BestSoFar, (SpinIndex, [Move]), NewBest),
    evaluate_sequential_moves(Moves, NewGameState, Depth, Player, SpinIndex, NewBest, Best).
*/

% --- Select Piece Off the Board --- 

select_piece_off_board(GameState, Move, PieceGameState) :-
    [Board,_,_,Player| _] = GameState,
    select_w(GameState, W),
    get_waiting_pieces(Pieces, Player, W),
    reverse(Pieces, ReversedPieces),
    [Piece | _] = ReversedPieces,
    replace_current_piece_waiting_pieces(GameState, ReversedPieces, Piece, PieceGameState),
    valid_moves_piece(0, 0, Player, Board, ValidMoves),
    random_member(Move, ValidMoves).% Select the best move for the off-board piece


% --- Select the Best Move from Valid Moves --- 

select_best_move(GameState, ValidMoves, BestMove) :-
    write('ValidMoves: '), print(ValidMoves), nl,
    findall(
        (Benefit, Move),
        (
            member(Move, ValidMoves),
            evaluate_move(GameState, Move, Benefit)
        ),
        MovesWithBenefits
    ),
    keysort(MovesWithBenefits, SortedMoves),  % Sort by benefit 
    write('MovesWBF: '), print(SortedMoves), nl,
    write('SortedMoves: '), print(SortedMoves), nl,
    reverse(SortedMoves, DescendingSortedMoves),  % Reverse the order to get the highest benefit first
    [(_BestBenefit, BestMove)] = DescendingSortedMoves.  % Select the move with the highest benefit.

evaluate_move(GameState, Move, Benefit) :-
    move(GameState, Move, NewGameState),
    [_, _, _, Player | _] = NewGameState,
    value(NewGameState, Player, Benefit).  % Calculate the benefit of each move.

% --- Check if Coordinates are Empty or Generate Random Move --- 

is_empty_or_random(GameState, X, Y, [], [], Move) :-
    X == [0], Y == [0], !,
    valid_moves(GameState, AllValidMoves),
    select_best_move(GameState, AllValidMoves, Move).  % Select the best move instead of random.
is_empty_or_random(_, X, Y, X, Y, []).  % Normal behavior if coordinates are not empty.

/*

%global heuristic
average_benefit_for_player_and_opponent(Result, AvgPlayerBenefit, AvgOpponentBenefit) :-
    findall(PlayerBenefit,
        (
            member((_Index, _Moves, PlayerScoreMoves, OpponentScoreMoves), Result),
            compare_rotation_benefits(PlayerScoreMoves, [], OpponentScoreMoves, [], PlayerBenefit)
        ),
        PlayerBenefits),
    findall(OpponentBenefit,
        (
            member((_Index, _Moves, PlayerScoreMoves, OpponentScoreMoves), Result),
            compare_rotation_benefits(OpponentScoreMoves, [], PlayerScoreMoves, [], OpponentBenefit)
        ),
        OpponentBenefits),
    sum_list(PlayerBenefits, TotalPlayerBenefit),
    sum_list(OpponentBenefits, TotalOpponentBenefit),
    length(PlayerBenefits, Count),
    AvgPlayerBenefit is TotalPlayerBenefit / Count,
    AvgOpponentBenefit is TotalOpponentBenefit / Count.
count_moves_for_player_and_opponent(Result, TotalPlayerMoves, TotalOpponentMoves) :-
    findall(PlayerMoves,
        (
            member((_Index, Moves, _PlayerScoreMoves, _OpponentScoreMoves), Result),
            length(Moves, PlayerMoves)
        ),
        PlayerMovesList),
    findall(OpponentMoves,
        (
            member((_Index, Moves, _PlayerScoreMoves, _OpponentScoreMoves), Result),
            length(Moves, OpponentMoves)
        ),
        OpponentMovesList),
    sum_list(PlayerMovesList, TotalPlayerMoves),
    sum_list(OpponentMovesList, TotalOpponentMoves).
net_score_opportunities(Result, PlayerScoreDelta, OpponentScoreDelta) :-
    findall(PlayerScore,
        (
            member((_Index, _Moves, PlayerScoreMoves, _OpponentScoreMoves), Result),
            length(PlayerScoreMoves, PlayerScore)
        ),
        PlayerScores),
    findall(OpponentScore,
        (
            member((_Index, _Moves, _PlayerScoreMoves, OpponentScoreMoves), Result),
            length(OpponentScoreMoves, OpponentScore)
        ),
        OpponentScores),
    sum_list(PlayerScores, TotalPlayerScore),
    sum_list(OpponentScores, TotalOpponentScore),
    PlayerScoreDelta is TotalPlayerScore,
    OpponentScoreDelta is TotalOpponentScore.*/