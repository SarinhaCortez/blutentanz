
/*USE MEANINGFUL NAMES FOR PREDICATES AND ARGUMENTS. 
TRY TO WRITE CODE THAT ‘LOOKS DECLARATIVE’ AND AVOID USING ‘IMPERATIVE-LOOKING’ 
CONSTRUCTIONS (E.G., IF-THEN-ELSE CLAUSES). TRY TO WRITE EFFICIENT CODE 
(E.G., USING TAIL RECURSION WHEN POSSIBLE).*/

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
    [_, _, _, blue, _, CFb , _ , _, _ ,_]  = GameState,
    length(CFb, CSb),
    CSb == 5, 
    Winner = blue.

% game over - pink won
game_over(GameState, Winner) :-
    [_, _, _, pink, _,  _ , CFp , _, _ ,_] = GameState,
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
    write('Piece coordinates: '), print(PieceCoordinates), nl,
    [Board, _, _, Player | _] = GameState,
    findall((Piece, X, Y),
        (
            member((Piece, XPiece, YPiece), PieceCoordinates),
            valid_moves_piece(XPiece, YPiece, Player, Board, Moves),
            member((X, Y), Moves)
        ),
        ListOfMoves).

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
    print(Moves),nl,
    member((PlaceInSquare, Square), Moves),
    write('Move is valid! \n'),
    format('~w is moving from x:~w y:~w to x:~w y:~w ~n', [Player, Curr_X, Curr_Y, Square, PlaceInSquare]),
    X is PlaceInSquare, 
    Y is Square.
% game_loop(+GameState)
game_loop(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(Winner).
game_loop(GameState) :-
    [_, _, _, _, _, _, _, _, _, human] = GameState,
    write('Human turn\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    choose_spin(GameState, SpunGameState), !,
    display_game(SpunGameState),
    call_construct_and_move(3, SpunGameState, FinalGameState), !,
    print(FinalGameState), nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).
game_loop(GameState):-
    print(GameState),nl,
    [_, _, 2, _, _, _, _, _, _, bot] = GameState, %dif2
    write('Bot turn, w minimax\n'), nl,
    print_turn(GameState),
    display_game(GameState),
    minimax_and_move(GameState, FinalGameState),!,
    print(FinalGameState),nl,
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).
game_loop(GameState):-
    [_, _, 1, _, _, _, _, _, _, bot] = GameState, %dif1
    write('bot turn with random\n'), nl,
    print_turn(GameState),
    random_moves(GameState, Moves, WGameState),!,
    call_move(WGameState, Moves, FinalGameState),
    display_game(FinalGameState),
    switch_turn(FinalGameState, OtherPlayerGameState),
    game_loop(OtherPlayerGameState).
clear_data :-
    retractall(board(_)).

move(GameState, Move, NewGameState) :-
    Move = (X, Y), write('Move is '), print(Move), nl, 
    [Board, _, _, _, CurrPiece | _] = GameState,
    get_x_y(CurrPiece, Old_X, Old_Y, Board),!, write('Old x is '), print(Old_X), write(' Old y is '), print(Old_Y), nl,
    clean_square(Old_X, Old_Y, Board, TempBoard),!, write('cleaned square\n'),
    nth1(Y, TempBoard, Square), !, write('got square\n'),
    replace_in_square(Square, X, CurrPiece, NewSquare), !,
    replace_in_board(TempBoard, Y, NewSquare, NewBoard),!,
    replace_board(GameState, NewBoard, TempState), 
    update_score(TempState, X, Y, NewGameState), 
    write('exiting move\n'), !.


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
    valid_moves(GameState, Moves),
    [_, _, _, Player | _] = GameState,
    has_no_moves(Moves), 
    format_color(Player), write(' ran out of possible moves. Switching turn.\n'),!.
%unused
minimax_and_move(GameState, FinalGameState) :-
    [Board, _, _, Player | _] = GameState,
    minimax(GameState, 2, Player, BestMove, _),
    BestMove = (Spin, Moves),
    format('Move chosen: ~w~n', [BestMove]),
    spin(Spin, Board, NewBoard, Success),
    Success == 1,
    replace_board(GameState, NewBoard, SpunGameState),
    call_move(SpunGameState, Moves, FinalGameState), !.

call_move(GameState, [], FinalGameState) :-
    write('base case!\n'),
    FinalGameState = GameState, !.
call_move(GameState, [(-1,0,0)|_], FinalGameState) :-
    write('no moves\n'),
    FinalGameState = GameState, !.
call_move(GameState, [H|T], FinalGameState) :-
    select_w(GameState, W), W >= 0,
    H = (Piece, X, Y), Move = (X, Y),
    format('Piece: ~w, X: ~w, Y: ~w~n', [Piece, X, Y]),
    replace_current_piece_waiting_pieces(GameState, W, Piece, PieceGameState),
    write('move in call move\n'),
    print(PieceGameState), nl,
    move(PieceGameState, Move, MovedGameState),
    display_game(MovedGameState),
    call_move(MovedGameState, T, FinalGameState).
%unused. coisas pra a heuristica. não usando, apaga-se (daqui, mais 120 linhas)
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
    findall((ColIndex, NewBoard), 
        (member(ColIndex, YSet), spin_column(ColIndex, Board, NewBoard)), SpunCols),
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

% Global heuristic functions focusing on piece placement priority
value(GameState, Player, Value) :-
    % Get scores
    get_score(Player, GameState, PlScore),
    opponent(Player, Opponent),
    get_score(Opponent, GameState, OpoScore),
    
    % Get count of pieces still waiting to be placed
    select_w(GameState, Player, PlWaiting),
    select_w(GameState, Opponent, OpWaiting),
    
    % Calculate placement priority value
    PlacementValue is (5 - PlWaiting) * 2,  % More value for fewer waiting pieces
    OppPlacementValue is (5 - OpWaiting) * 2,
    write('entering rot val\n'),
    % Calculate rotation benefit
    rotation_value(GameState, _, RotationBenefit),
    
    % Weighted combination of factors
    Value is (0.4 * (PlScore - OpoScore) + 
              0.4 * (PlacementValue - OppPlacementValue) +
              0.2 * RotationBenefit).
%acaba aqui as coisas para a heurística
random_moves(GameState, Moves, NewGameState) :-
    [Board, _, _, Player | _] = GameState,
    display_game(GameState),
    spin(0, Board, SpunBoard, Success),
    format_color(Player), write('Spinned!\n'),
    Success == 1,
    replace_board(GameState, SpunBoard, SpunGameState),
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
    [Board, _, _, Player | _] = GameState,
    select_w(GameState, Player, W), 
    (W = 0; \+ valid_moves_piece(0, 0, Player, Board, Moves)), 
    valid_moves(GameState, Moves),
    \+ has_no_moves(Moves), !,
    random_member(Move, Moves),
    Move = (P, X, Y), M = (X, Y),
    replace_current_piece_waiting_pieces(GameState, W, P, PieceGameState),
    move(PieceGameState, M, NewGameState).
random_move(GameState, Move, NewGameState) :-
    [Board, _, _, Player | _] = GameState,
    valid_moves_piece(0, 0, Player, Board, Moves),
    has_no_moves(Moves), !,
    Move = (-1,0,0), NewGameState = GameState.
random_move(GameState, Move, NewGameState) :-
    valid_moves(GameState, Moves),
    [_, _, _, Player | _] = GameState,
    has_no_moves(Moves), !,
    Move = (-1,0,0), NewGameState = GameState.