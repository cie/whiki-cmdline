
read_line(SorA, Line) :-
    get_code(SorA, B), 
    B \== -1,
    (
        B=10 -> Line=[] ; 
        B=13 -> read_line(SorA, Line)
        ;read_line(SorA, T), Line=[B|T]
    ).


for(A:L, B) :- member(A,L), (\+B -> !), fail; true.
forall(A,B) :- A, (\+B->!), fail; true.
