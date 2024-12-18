:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(redefs).

board([
  [' ', ' ', '#', ' '],
  ['#', ' ', ' ', ' '],
  [' ', ' ', ' ', '#'],
  [' ', '#', ' ', ' '],
  [' ', ' ', '#', ' '],
  ['#', ' ', ' ', ' '],
  [' ', ' ', ' ', '#'],
  [' ', '#', ' ', ' '],
  [' ', ' ', '#', ' '],
  ['#', ' ', ' ', ' '],
  [' ', '#', ' ', ' '],
  [' ', '#', ' ', ' '],
  ['#', ' ', ' ', ' '],
  [' ', ' ', '#', ' '],
  [' ', ' ', ' ', '#'],
  [' ', '#', ' ', ' ']
]).


%helpers
shuffle_board(Board, ShuffledBoard) :-
    random_permutation(Board, ShuffledBoard).

get_square(N, Board, Square) :-
    nth1(N, Board, Square).

%displaying the board
print_board(Board) :-
    nl,
    format('   A   B   C   D  ~n', []),
    write('  _______________  '), nl,
    print_board(Board, 1),
    nl. 

print_board(_, N) :- N > 16, !. 
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
    format('~w|~w ~w|~w ~w|~w ~w|~w ~w|~n', [N, Sq1_1, Sq1_2, Sq2_1, Sq2_2, Sq3_1, Sq3_2, Sq4_1, Sq4_2]),
    format(' |~w ~w|~w ~w|~w ~w|~w ~w|~n', [Sq1_3, Sq1_4, Sq2_3, Sq2_4, Sq3_3, Sq3_4, Sq4_3, Sq4_4]),
    write('  _______________  '), nl.

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



% board(Board), spin_column(1, Board, NewBoard), print_board(NewBoard).

