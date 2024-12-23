:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(redefs).
:- consult(colors).

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

%helpers
shuffle_board(Board, ShuffledBoard) :-
    random_permutation(Board, ShuffledBoard).

get_square(N, Board, Square) :-
    nth1(N, Board, Square).

print_pink_waiting_figures(0) :- nl, !.
print_pink_waiting_figures(N) :-
    write('  '),
    N > 0,
    FN is N - 1,
    format_color(FN),
    N1 is N - 1,
    print_pink_waiting_figures(N1).

print_blue_waiting_figures(0) :- nl, !.
print_blue_waiting_figures(N) :-
    write('  '),
    N > 0,
    FN is N + 4,
    format_color(FN),
    N1 is N - 1,
    print_blue_waiting_figures(N1).

%displaying the board
print_board(Board) :-
    nl,
    blue_finished_figures(Ff),
    format('Blue already passed ~w figures.', Ff), nl, nl,
    blue_waiting_figures(F),
    print_blue_waiting_figures(F),
    write('  '), repeat_format_color(15, 'o'), nl, nl,
    format('   A   B   C   D  ~n', []),
    write('  _______________  '), nl,
    print_board(Board, 1),
    nl. 

print_board(_, N) :- N > 16, 
    write('  '), repeat_format_color(15, '_'), nl,  nl,
    pink_waiting_figures(F), print_pink_waiting_figures(F),
    pink_finished_figures(Ff), nl,
    format('Pink already passed ~w figures.', Ff), !, nl.

print_board(Board, N) :-
    NNext is N + 4,
    get_square(N, Board, Square1),
    N1 is N + 1, get_square(N1, Board, Square2),
    N2 is N + 2, get_square(N2, Board, Square3),
    N3 is N + 3, get_square(N3, Board, Square4),
    P is (N div 4) + 1,
    format_square(Square1, Square2, Square3, Square4, P),
    print_board(Board, NNext).

%formatting options

format_square(Square1, Square2, Square3, Square4, N) :-
    Square1 = [Sq1_1, Sq1_2, Sq1_3, Sq1_4],
    Square2 = [Sq2_1, Sq2_2, Sq2_3, Sq2_4],
    Square3 = [Sq3_1, Sq3_2, Sq3_3, Sq3_4],
    Square4 = [Sq4_1, Sq4_2, Sq4_3, Sq4_4],
    
    write(N), 
    write('|'), format_color(Sq1_1),format_color(' '), format_color(Sq1_2),
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

repeat_format_color(0, _) :- !.
repeat_format_color(N, Char) :-
    N > 0,
    format_color(Char),
    N1 is N - 1,
    repeat_format_color(N1, Char).

format_color('*') :- 
    print_in_color(cyan, '*'), !. 
    
format_color('+') :- 
    print_in_color(b_magenta, '+'), !.

format_color('o') :- 
    print_in_color(cyan, '_'), !.

format_color('_') :- 
    print_in_color(b_magenta, '_'), !.

format_color('-') :-
    print_in_color(black, '-'), !.
    
%print players
format_color(X) :- 
    between(0, 4, X), !,
    X1 is X+1,
    print_in_color(b_magenta, X1). 

format_color(X) :- 
    between(5, 9, X), !,
    X1 is X-4,
    print_in_color(cyan, X1).

format_color(X) :- 
    print_in_color(white, X).      % Default color for any other character

%logic for spinning the squares

spin_square_in_board(Pos, Board, NewBoard) :-
    nth1(Pos, Board, Square), 
    spin_square(Square, SpunSquare),
    replace_nth(Board, Pos, SpunSquare, NewBoard), !. 

replace_nth([_|T], 1, Elem, [Elem|T]) :- !.
replace_nth([H|T], N, Elem, [H|NewT]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(T, N1, Elem, NewT).

spin_square([A, B, C, D], [C, A, D, B]) :- !.

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

% board(Board), print_board(Board).
% shuffle_board(Board, ShuffledBoard), spin_column(1, ShuffledBoard, NewBoard)
column_index('a', 1).
column_index('b', 2).
column_index('c', 3).
column_index('d', 4).
column_index(A, Col) :- char_code(A, Code), Code >= 65, Code =< 68, Col is Code - 64.

%keep the number of waiting figures
pink_waiting_figures(5).
blue_waiting_figures(5).

%keep the number of finished figures
pink_finished_figures(0).
blue_finished_figures(0).

decrease_pink_waiting_figures :-
    pink_waiting_figures(N),
    N > 0,
    N1 is N - 1,
    retractall(pink_waiting_figures(_)),
    assert(pink_waiting_figures(N1)).

decrease_blue_waiting_figures :-
    blue_waiting_figures(N),
    N > 0,
    N1 is N - 1,
    retractall(blue_waiting_figures(_)),
    assert(blue_waiting_figures(N1)).

blutentanz :-
    repeat_format_color(22, '+'), nl,
    repeat_format_color(22, '-'), nl,
    write('Welcome to Blutentanz!'), nl,
    repeat_format_color(22, '-'), nl, 
    repeat_format_color(22, '*'), nl.

%input processing
input_to_indices(Input, Row, Col) :-
    atom_chars(Input, [ColChar, RowChar]),
    column_index(ColChar, Col), !,
    atom_number(RowChar, Row).

% Get the index of the square in the board 
% Currently receiving +, -, *. what about p for pink, b for blue and n for neutral?
get_square_index(Board, Input, Char, Index) :-
    input_to_indices(Input, Row, Col),!,
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Char),!,
    nth1(Index, BoardRow, Char),!.

increase_pink_finished_figures :-
    pink_finished_figures(N),
    N1 is N + 1,
    retractall(pink_finished_figures(_)),
    assert(pink_finished_figures(N1)).

increase_blue_finished_figures :-
    blue_finished_figures(N),
    N1 is N + 1,
    retractall(blue_finished_figures(_)),
    assert(blue_finished_figures(N1)).