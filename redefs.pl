:- use_module(library(lists)).

% between 

between(Low, High, Value) :-
    Low =< High,
    Value = Low.
    
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).

% atom_number

atom_number(Atom, Number) :-
    atom(Atom),       
    atom_codes(Atom, Codes), 
    number_codes(Number, Codes). 

atom_number(Atom, Number) :-
    number(Number),   
    number_codes(Number, Codes), 
    atom_codes(Atom, Codes).

% remove_duplicates

remove_duplicates([], []).

remove_duplicates([H|T], Result) :-
    member(H, T),        
    remove_duplicates(T, Result).

remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),        
    remove_duplicates(T, Result).

% sum_list

sum_list([], 0).

sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

% max

max( X, X , X ) .

max( X, Y , X ) :- X > Y .

max( X, Y , Y ) :- X < Y .