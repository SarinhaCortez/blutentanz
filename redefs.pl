:- use_module(library(lists)).

between(Low, High, Value) :-
    Low =< High,
    Value = Low.
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).

atom_number(Atom, Number) :-
    atom(Atom),        % Ensure the first argument is an atom
    atom_codes(Atom, Codes), % Convert atom to list of character codes
    number_codes(Number, Codes). % Convert character codes to a number

atom_number(Atom, Number) :-
    number(Number),    % Ensure the second argument is a number
    number_codes(Number, Codes), % Convert number to character codes
    atom_codes(Atom, Codes). % Convert character codes to an atom

remove_duplicates([], []).

% Recursive case: Check if the head of the list is already in the processed tail
remove_duplicates([H|T], Result) :-
    member(H, T),            % If H is in the tail, skip it
    remove_duplicates(T, Result).

remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),         % If H is not in the tail, keep it
    remove_duplicates(T, Result).
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.