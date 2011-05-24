%:- initialization(main).
:- dynamic(wlist/1, witem/2).

main :- loop, commit.
loop :- 
    repeat, read_prompt('-> ', X), parse(X), fail.


parse(l) :- parse(lists).
parse(lists) :-
    wlist(X), write(X),nl, fail; true.
parse(X) :-
    list_loop(X), nl.




list_loop(L) :-
    atom_concat(L,'> ', Prompt),
    list_parse(L,list),
    repeat, read_prompt(Prompt, X), (list_parse(L,X);!), fail.

list_parse(L,l) :- list_parse(L,list).
list_parse(L, list) :-
    witem(L,X), write(X), nl, fail; true.




read_line(Line) :-
    get_code(B), 
    B \== -1,
    (
        B=10 -> Line=[] ; 
        B=13 -> read_line(Line)
        ;read_line(T), Line=[B|T]
    ).


read_prompt(P, X) :-
    write(P), read_line(L), atom_codes(X, L).
