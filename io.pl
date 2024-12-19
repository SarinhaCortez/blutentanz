:- consult(state).

choose_and_spin :-
    load_board(Board),
    print_board(Board),
    write('Choose a row (1-4) or column (A-D) to spin (don\'t forget the . after your choice): '),
    read(Input),
    process_input(Input, Board, NewBoard),
    save_board(NewBoard),
    print_board(NewBoard).

process_input(Input, Board, NewBoard):- 
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard).

process_input(Input, Board, NewBoard) :- 
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard).
    
process_input(_Input, _Board, _NewBoard) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D)'), 
    choose_and_spin.
    

column_index('a', 1).
column_index('b', 2).
column_index('c', 3).
column_index('d', 4).
column_index(A, Col) :- char_code(A, Code), Code >= 65, Code =< 68, Col is Code - 64.

