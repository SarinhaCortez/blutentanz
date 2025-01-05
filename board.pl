:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(redefs).
:- consult(utils).

% Example board for shuffling when initializing board configuration

board([
  ['+', '-', ' ', '*'],
  [' ', '+', '*', '-'],
  ['-', '*', '+', ' '],
  ['*', ' ', '-', '+'],
  ['+', '-', ' ', '*'],
  [' ', '+', '*', '-'],
  ['-', '*', '+', ' '],
  ['*', ' ', '-', '+'],
  ['+', '-', ' ', '*'],
  [' ', '+', '*', '-'],
  ['-', '*', '+', ' '],
  ['*', ' ', '-', '+'],
  ['+', '-', ' ', '*'],
  [' ', '+', '*', '-'],
  ['-', '*', '+', ' '],
  ['*', ' ', '-', '+']
]).

% Shuffle the board randomly

shuffle_board(Board, ShuffledBoard) :-
    random_permutation(Board, ShuffledBoard).

% Get the N-th square from the board

get_square(N, Board, Square) :-
    nth1(N, Board, Square). 
      
% Print the waiting figures for the pink player

print_pink_waiting_figures_helper(0, _) :- nl, !.

print_pink_waiting_figures_helper(N, FN) :-
    write('  '),
    format_color(FN),
    N1 is N - 1,
    FN1 is FN - 1,
    print_pink_waiting_figures_helper(N1, FN1).

% Tail recursive predicate to print waiting figures for pink player

print_pink_waiting_figures(N) :-
    FN is N - 1,
    print_pink_waiting_figures_helper(N, FN).

% Print the waiting figures for the blue player

print_blue_waiting_figures(0, _) :- nl, !.

print_blue_waiting_figures(N) :-
    print_blue_waiting_figures(N, 4).

% Tail-recursive helper predicate

print_blue_waiting_figures(N, Offset) :-
    write('  '),
    FN is N + Offset,  
    format_color(FN),  
    N1 is N - 1,
    print_blue_waiting_figures(N1, Offset).

% Display the game board

print_board(GameState) :-
    [Board, _, _, _, _, CFb, _, WB, _, _] = GameState, !,

    write(GameState), nl,
    nl, format_color(blue),
    length(CFb, CSb),
    format(' score: ~w figures', [CSb]), nl, nl,
    print_blue_waiting_figures(WB),
    write('  '), repeat_format_color(15, 'o'), nl, nl,
    write('   A   B   C   D  \n'),
    write('  _______________  '), nl,
    print_board_rows(1, Board, GameState),
    nl.

% Base case for printing rows: Stop when the row number exceeds 16

print_board_rows(N, _Board, GameState) :- 
    N > 16,
    [_, _, _, _, _,  _, CFp, _, WP, _] = GameState,

    write('  '), repeat_format_color(15, '_'), nl, nl,
    print_pink_waiting_figures(WP), nl,
    format_color(pink),
    length(CFp, CSp),
    format(' score: ~w figures.', [CSp]), !, nl.

% Recursive case: Print each row and continue with the next row

print_board_rows(N, Board, GameState) :-
    NNext is N + 4,
    get_square(N, Board, Square1),

    N1 is N + 1, get_square(N1, Board, Square2),
    N2 is N + 2, get_square(N2, Board, Square3),
    N3 is N + 3, get_square(N3, Board, Square4),
    P is (N div 4) + 1,

    format_square(Square1, Square2, Square3, Square4, P),
    print_board_rows(NNext, Board, GameState).

% Format and print a row of squares

format_square( [Sq1_1, Sq1_2, Sq1_3, Sq1_4], 
               [Sq2_1, Sq2_2, Sq2_3, Sq2_4], 
               [Sq3_1, Sq3_2, Sq3_3, Sq3_4], 
               [Sq4_1, Sq4_2, Sq4_3, Sq4_4], N) :-
    
    write(N), write('|'), format_color(Sq1_1),format_color(' '), format_color(Sq1_2),
    write('|'), format_color(Sq2_1), format_color(' '), format_color(Sq2_2),
    write('|'), format_color(Sq3_1), format_color(' '), format_color(Sq3_2),
    write('|'), format_color(Sq4_1), format_color(' '), format_color(Sq4_2),
    write('|'),
    nl,

    write(' |'), format_color(Sq1_3), format_color(' '), format_color(Sq1_4),
    write('|'), format_color(Sq2_3), format_color(' '), format_color(Sq2_4),
    write('|'), format_color(Sq3_3), format_color(' '), format_color(Sq3_4),
    write('|'), format_color(Sq4_3), format_color(' '), format_color(Sq4_4),
    write('|'),
    nl,

    write('  _______________'), nl.

% Spin a square in the board 

spin_square_in_board(Pos, Board, NewBoard) :-
    nth1(Pos, Board, Square), 
    spin_square(Square, SpunSquare),
    replace_nth(Board, Pos, SpunSquare, NewBoard), !. 

% Replace the N-th element in a list

replace_nth(List, N, Elem, NewList) :-
    replace_nth_acc(List, N, Elem, [], NewList).

replace_nth_acc([_|T], 1, Elem, Acc, NewList) :-
    reverse(Acc, RevAcc),
    append(RevAcc, [Elem|T], NewList), !.
    
replace_nth_acc([H|T], N, Elem, Acc, NewList) :-
    N > 1,
    N1 is N - 1,
    replace_nth_acc(T, N1, Elem, [H|Acc], NewList).

% Spin a square 90 degrees

spin_square([A, B, C, D], [C, A, D, B]) :- !.

% Spin a row in the board, using tail recursion

spin_row(0, Board, NewBoard) :- 
    random(1, 4, Index),
    random_member(SpinType, [spin_row, spin_column]),
    call(SpinType, Index, Board, NewBoard), !.
    
spin_row(Row, Board, NewBoard) :-
    Start is 4 * (Row - 1) + 1,
    End is 4 * Row,
    spin_row_aux(Start, End, Board, NewBoard).

spin_row_aux(Pos, End, Board, Board) :- Pos > End, !.

spin_row_aux(Pos, End, Board, NewBoard) :-
    Pos =< End, 
    spin_square_in_board(Pos, Board, TempBoard),

    Pos1 is Pos + 1, 
    spin_row_aux(Pos1, End, TempBoard, NewBoard). 

% Spin a column in the board, using tail recursion

spin_column(Col, Board, NewBoard) :-
    Start is Col, 
    End is 16, 
    spin_column_aux(Start, End, Board, NewBoard).

spin_column_aux(Pos, End, Board, Board) :- 
    Pos > End, !. 

spin_column_aux(Pos, End, Board, NewBoard) :-
    Pos =< End, 
    spin_square_in_board(Pos, Board, TempBoard), 
    
    Pos1 is Pos + 4,
    spin_column_aux(Pos1, End, TempBoard, NewBoard).