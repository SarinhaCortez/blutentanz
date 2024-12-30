/*USE MEANINGFUL NAMES FOR PREDICATES AND ARGUMENTS. 
TRY TO WRITE CODE THAT ‘LOOKS DECLARATIVE’ AND AVOID USING ‘IMPERATIVE-LOOKING’ 
CONSTRUCTIONS (E.G., IF-THEN-ELSE CLAUSES). TRY TO WRITE EFFICIENT CODE 
(E.G., USING TAIL RECURSION WHEN POSSIBLE).

/*d FOR UNIFORMIZATION 
PURPOSES, COORDINATES SHOULD START AT (1,1) AT THE LOWER LEFT CORNER.*/
:- consult(io).


% Start Game
play :-
    blutentanz,
    choose_mode(Mod), !, % choose game mode
    choose_start_player(Player), !, % choose start player color
    choose_difficulty(Mod, Dif), !, % choose difficulty for PC
    GameConfig = [Mod, Dif, Player], !, 
    initial_state(GameConfig, GameState),
    display_game(GameState), 
    game_loop(GameState). % start game loop

% Initial Game State
initial_state(GameConfig, GameState) :-
    board(Board),
    shuffle_board(Board, ShuffledBoard),
    [Mode, Dif, Player] = GameConfig,
    GameState = [ShuffledBoard, Mode, Dif, Player, -1, [], [], 5, 5]. %minus one is current piece to move

% Display Board
display_game(GameState) :- 
    print_board(GameState).


% Game Loop(+GameState)
% check for winner
game_loop(GameState):-
    game_over(GameState, Winner), !, 
    display_game(GameState),
    show_winner(Winner).

% regular game loop
game_loop(GameState):-
    print_turn(GameState), 
    display_game(GameState),
    choose_spin(GameState, SpunGameState), % choose row or column to spin
    display_game(SpunGameState),
    call_construct_and_move(3, SpunGameState, FinalGameState), !, % current player moves
    print(FinalGameState),nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).

% Game Over
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


% Player Turn Moves

call_construct_and_move(0, GameState, GameState) :- !.
call_construct_and_move(N, GameState, FinalGameState) :-
    N > 0,
    repeat,
    construct_move(GameState, (NewX, NewY), PieceGameState),
    format('Move chosen: (~w, ~w)~n', [NewX, NewY]),
    move(PieceGameState,(NewX, NewY), MovedGameState),
    write('Exited move successfuly!'),
    display_game(MovedGameState),
    N1 is N - 1,
    call_construct_and_move(N1, MovedGameState, FinalGameState).

/*
should_stop(N).
call_choose_and_move(N, GameState) :-
    should_stop(N), 
    writeln('Condition met, stopping!'), !.*/

% Play Logic

% Human vs. Human
%internally, square is y and place x
%choose move is now construct
construct_move(GameState, Move, PieceGameState) :-
    Move = (X, Y),
    [Board, 1 | _] = GameState,
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

valid_moves(GameState, ListOfMoves) :-
    get_piece_coordinates(GameState, PieceCoordinates),
    [Board, _, _, Player | _] = GameState,
    findall(Moves, (member((XPiece, YPiece), PieceCoordinates), valid_moves_piece(XPiece, YPiece, Player, Board, Moves)), ListOfListsofMoves),
    append(ListOfListsofMoves, ListOfMoves).

%choosing heuristic
rotation_value(GameState, SortedResult) :-
    [Board, Mod, Dif, Player, Piece, CFB, CFP, WB, WP] = GameState,
    Opponent = opponent(Player),
    [Board, Mod, Dif, Opponent, Piece, CFB, CFP, WB, WP] = OpponentGameState,
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
    findall((Index, Moves, OponentScoreMoves), 
        (member((Index, Moves), SpunMoves), 
         include(is_score_point(Opponent), Moves, OponentScoreMoves)), 
        OponentResults),
    combine_results(PlayerResults, OponentResults, Result),
    calculate_benefits(Result, PlayerStartScoreMoves, OpponentStartScoreMoves),
    sort_by_benefit(Result, SortedResult).
sort_by_benefit(Result, SortedResult) :-
    findall(
        (Benefit, MoveLength, Index, Moves),
        (
            member((Index, Moves, PlayerScoreMoves, OponentScoreMoves), Result),
            compare_rotation_benefits(PlayerScoreMoves, [], OponentScoreMoves, [], Benefit),
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
compare_rotation_benefits(CurrScoreMoves, CurrStartScoreMoves, OponentScoreMoves, OponentStartScoreMoves, Benefit) :-
    length(CurrScoreMoves, CurrScoreCount),
    length(CurrStartScoreMoves, CurrStartCount),
    PlayerDelta is CurrScoreCount - CurrStartCount,
    length(OponentScoreMoves, OponentScoreCount),
    length(OponentStartScoreMoves, OponentStartCount),
    OpponentDelta is OponentScoreCount - OponentStartCount,
    Benefit is PlayerDelta - OpponentDelta.
calculate_benefits(Result, PlayerStartScoreMoves, OponentStartScoreMoves) :-
    findall(Benefit, 
        (member((Index, Moves, PlayerScoreMoves, OponentScoreMoves), Result), 
         compare_rotation_benefits(PlayerScoreMoves, PlayerStartScoreMoves, 
                                   OponentScoreMoves, OponentStartScoreMoves, 
                                   Benefit)), 
        Benefits).
combine_results([], [], []).
combine_results([(Index, Moves, PlayerScoreMoves) | PlayerTail], 
                [(Index, Moves, OponentScoreMoves) | OpponentTail], 
                [(Index, Moves, PlayerScoreMoves, OponentScoreMoves) | ResultTail]) :-
    combine_results(PlayerTail, OpponentTail, ResultTail).

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
            member((_Index, Moves, PlayerScoreMoves, _OpponentScoreMoves), Result),
            length(Moves, PlayerMoves)
        ),
        PlayerMovesList),
    findall(OpponentMoves,
        (
            member((_Index, Moves, _PlayerScoreMoves, OpponentScoreMoves), Result),
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
    OpponentScoreDelta is TotalOpponentScore.

on_board(PieceCoordinates, Count) :-
    exclude(valid_coordinate, PieceCoordinates, ValidCoordinates),
    length(ValidCoordinates, Count).
value(GameState, Player, Value) :-
    get_score(Player, GameState, PlScore), !,
    Opponent = opponent(Player),
    get_score(Opponent, OpoScore), !,
    rotation_value(GameState, Result),
    average_benefit_for_player_and_opponent(Result, AvgPlayerBenefit, AvgOpponentBenefit),
    count_moves_for_player_and_opponent(Result, TotalPlayerMoves, TotalOpponentMoves),
    net_score_opportunities(Result, PlayerScoreDelta, OpponentScoreDelta),
    RotationVal is (0.5 * (AvgPlayerBenefit - AvgOpponentBenefit)) + 
                 (0.3 * (TotalPlayerMoves - TotalOpponentMoves)) + 
                 (0.2 * (PlayerScoreDelta - OpponentScoreDelta)),
    ScoreWeight is (0.6 * (PlScore - OpoScore)),  
    NumValue is 0.6 * RotationVal + 0.4 * ScoreWeight,
    (NumValue >= 0 -> Value = good ; Value = bad).

% input_move is now choose move in order to follow the conventions
choose_move(GameState, 1, (Square, PlaceInSquare)) :-%internally, square is y and place x
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





