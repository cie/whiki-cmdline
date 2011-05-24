
:- dynamic(wlist/1, witem/2).
:- initialization(start_backend).

start_backend :- 
    File = 'data/default.pl'
    consult(File),
    open(File, append, S),
    add_stream_alias(S, data_file).

tables([wlist, witem]).

check_valid(X) :-
    X=..[Tab|Row], 
    tables(Tabs),
    memberchk(T,Tabs).

backend(query(X)) :-
    check_valid(X),
    X.
backend(assert(X)) :- 
    check_valid(X),
    assertz(X),
    write_canonical(data_file, X).
backend(retract(X)) :-
    check_valid(X),
    retract(X).
    


commit :- close(data_file).
