
:- dynamic(wlist/1, witem/2).
:- initialization(start_backend).

start_backend :- 
    File = 'data/default.pl',
    (file_exists(File) -> load_data(File) ; true),
    open(File, append, S),
    add_stream_alias(S, data_file).

load_data(File) :-
    open(File, read, S),
    repeat,
    read(S, X),
    (X == end_of_file ->
        true;
        backend_nowrite(X), fail),
    close(S).

tables([wlist, witem]).

check_valid(X) :-
    X=..[Cmd,Row],
    memberchk(Cmd, [query,assert,retract]),
    Row=..[Tab|_], 
    tables(Tabs),
    memberchk(Tab,Tabs).

backend_nowrite(X) :-
    check_valid(X),
    backend_execute(X).

backend(X) :-
    backend_nowrite(X),
    (X \= query(_) -> backend_write(X); true).

backend_execute(query(R)) :- R.
backend_execute(assert(R)) :- assertz(R).
backend_execute(retract(R)) :- retract(R).

backend_write(Term) :-
    write_canonical(data_file, Term),
    write(data_file, '.'),
    nl(data_file).
    

commit :- close(data_file).
