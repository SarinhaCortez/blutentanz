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