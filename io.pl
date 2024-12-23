:- consult(board).

choose_spin :-
    load_board(Board),
    print_board(Board),
    write('Choose a row (1-4) or column (A-D) to spin (don\'t forget the . after your choice): '),
    read(Input),
    process_spin_input(Input, Board, NewBoard).
/*
repeat,
write('Choose a row (1-4) or column (A-D) to spin (don\'t forget the . after your choice): '), 
read(Input),
X = hello.
issues with these*/

process_spin_input(Input, Board, NewBoard):- 
    member(Input, [1, 2, 3, 4]), !,
    spin_row(Input, Board, NewBoard).

process_spin_input(Input, Board, NewBoard) :- 
    member(Input, ['a', 'b', 'c', 'd', 'A', 'B', 'C', 'D']), !,
    column_index(Input, Col),
    spin_column(Col, Board, NewBoard).
    
process_input(_Input, _Board, _NewBoard) :-
    write('Invalid input. Please choose a row (1-4) or column (A-D)'), 
    fail.

