between(Low, High, Value) :-
    Low =< High,
    Value = Low.
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).