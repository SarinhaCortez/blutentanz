rotation_value(GameState, SortedResult, TotalBenefit) :-
    [Board, Mod, Dif, Player, Piece, CFB, CFP, WB, WP, Type] = GameState,
    opponent(Player, Opponent),
    [Board, Mod, Dif, Opponent, Piece, CFB, CFP, WB, WP, Type]= OpponentGameState,
    get_piece_coordinates(GameState, PlayerPieceCoordinates),
    valid_moves(GameState, PInitMoves), 
    remove_duplicates(PInitMoves, PlInitMoves),
    print(PlInitMoves), nl,        
    valid_moves(OpponentGameState, OInitMoves),  remove_duplicates(OInitMoves, OpInitMoves),
    print(OpInitMoves), nl,
    include(is_score_point(Player), PlInitMoves, PlayerStartScoreMoves),  
    include(is_score_point(Opponent), OpInitMoves, OpponentStartScoreMoves),    
    unpack_coordinates(PlayerPieceCoordinates, Xs, Ys),      
    remove_duplicates(Xs, X),                             
    remove_duplicates(Ys, Y),
    is_empty_or_random(GameState, X, Y, XSet, YSet, RandomMove), !,  % Check for empty coordinates or assign random move
    (RandomMove \== [] -> (SortedResult = [(RandomMove)], TotalBenefit = 0, Start is 1);  % If RandomMove exists, output it directly
    (
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
        sort_by_benefit(Result, SortedResult),
        Start is 0
    )).
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
    [_,_,_,Player, _|_] = GameState,
    rotation_value(GameState, _Result, Value).

% Minimax with sequential move evaluation after rotation
minimax(GameState, 2, Player, BestMove, BestValue) :-
    rotation_value(GameState, SortedResult, _TotalBenefit, Start),
    [Board |_] = GameState,
    SortedResult = [H|_],write('Move: '), print(H), nl,
    Start == 1 -> spin(0, Board, NewBoard), replace_board(GameState, NewBoard, RotatedGameState), move(RotatedGameState, (H), MovedGameState), minimax(MovedGameState, 2, Player, BestMove, BestValue);
    evaluate_rotations(SortedResult, GameState, 2, Player, -inf, (BestMove, BestValue)), write('here\n').
minimax(GameState, 0, Player, _, Value) :-
    value(GameState, Player, Value).

% Evaluate rotations with fallback for spin index 0
evaluate_rotations([], _, _, _, BestSoFar, BestSoFar).
evaluate_rotations([(SpinIndex, Moves) | Rotations], GameState, Depth, Player, (CurrBestMove, CurrBestValue), Best) :-
    [Board | _] = GameState,
    spin(SpinIndex, Board, NewBoard, Success),
    Success == 1,
    replace_board(GameState, NewBoard, RotatedGameState),
    evaluate_sequential_moves(Moves, RotatedGameState, Depth, Player, SpinIndex, (CurrBestMove, CurrBestValue), NewBest),
    evaluate_rotations(Rotations, GameState, Depth, Player, NewBest, Best).

% Sequential move evaluation
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
% Check if all coordinates are 0 and return a random move if necessary
is_empty_or_random(GameState, X, Y, [], [], RandomMove) :-
    X == [0], Y == [0], !, 
    valid_moves(GameState, AllValidMoves), 
    random_member(RandomMove, AllValidMoves).  % Generate a random valid move
is_empty_or_random(_, X, Y, X, Y, []).  % Otherwise, proceed as normal
%cant choose spin before moving


% --- Minimax with Sequential Move Evaluation After Rotation --- 

minimax(GameState, 2, Player, BestMove, BestValue) :-
    % Check if there are pieces waiting to be placed
    select_w(GameState, Player, WaitingCount),
    WaitingCount > 0, !,
    evaluate_placement_moves(GameState, Player, BestMove, BestValue).
minimax(GameState, 2, Player, BestMove, BestValue) :-
    % Check if there are pieces waiting to be placed
    select_w(GameState, Player, WaitingCount),
    WaitingCount = 0, !,
    rotation_value(GameState, SortedResult, _),
    evaluate_rotations(SortedResult, GameState, 2, Player, -inf, (BestMove, BestValue)).
    
evaluate_placement_moves(GameState, Player, BestMove, BestValue) :-
    [Board | _] = GameState,
    valid_moves_piece(0, 0, Player, Board, EntryMoves),
    write('EntryMoves\n'), print(EntryMoves), nl,

    findall(
        (Value, Move),
        (
            member(Move, EntryMoves),
            simulate_placement(GameState, Move, Player, SimValue),
            Value = SimValue
        ),
        MovesWithValues
    ),
    write('MovesWithValues: '), print(MovesWithValues), nl,  % Debug print
    keysort(MovesWithValues, Sorted),
    write('Sorted: '), print(Sorted), nl,  % Debug print
    last(Sorted, (BestValue, EntryMove)),
    BestMove = (0, [(0, EntryMove)]).  % 0 indicates random rotation

% Predicate to simulate placement of a piece
simulate_placement(GameState, Move, Player, Value) :-
    select_w(GameState, W),
    get_waiting_pieces(Pieces, Player, W),
    [Piece|_] = Pieces,
    replace_current_piece_waiting_pieces(GameState, Pieces, Piece, PieceGameState),
    move(PieceGameState, Move, NewGameState),
    value(NewGameState, Player, Value),
    write('exiting value\n'), !.

% Example usage
% ?- GameState = [[['+', '6', ' ', '*'], ['+', '5', ' ', '8'], ['+', '-', ' ', '7'], ['+', '-', ' ', '*']], _, _, pink, _, _, _, _, _, _], evaluate_placement_moves(GameState, pink, BestMove, BestValue).
% BestMove = (0, [(0, (1, 1))]),
% BestValue = 0.
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
% Modified evaluate_sequential_moves to prioritize piece placement
evaluate_sequential_moves([], GameState, _, Player, SpinIndex, BestSoFar, Best) :-
    % Check if there are more pieces to place
    eces_count(GameState, Player, WaitingCount),
    WaitingCount > 0, !,
    select_piece_off_board(GameState, Move, PieceGameState),
    move(PieceGameState, Move, NewGameState),
   
    value(NewGameState, Player, Value), write('Exiting value\n'),
    update_best_move(Value, BestSoFar, (SpinIndex, [Move]), NewBest),
    evaluate_sequential_moves([], NewGameState, 0, Player, SpinIndex, NewBest, Best).
    
evaluate_sequential_moves([], GameState, _, Player, _SpinIndex, BestSoFar, Best) :-
    get_waiting_pieces_count(GameState, Player, WaitingCount),
    WaitingCount =< 0, !,Best = BestSoFar.
/*
% If no piece is off the board, proceed with regular move evaluation
evaluate_sequential_moves([Move | Moves], GameState, Depth, Player, SpinIndex, BestSoFar, Best) :-
    valid_moves(GameState, ValidMoves),
    select_best_move(GameState, ValidMoves, Move),
    [Board,_,_,Player| _] = GameState,
    (GameState, W),
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