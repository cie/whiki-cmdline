
:- initialization(start_backend).

tables([wlist/1, witem/2, wpar/2, wrel/1]).
:- dynamic(wlist/1, witem/2, wpar/2, wrel/1).

start_backend :-
    (argument_list([File|_]) ; write('Usage: whiki <dbfile>'), nl, fail),
    start_backend(File).
start_backend(File) :- 
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


check_valid(X) :-
    X=..[Cmd,Row],
    memberchk(Cmd, [query,assert,retract]),
    Row=..[Tab|Fields], 
    tables(Tabs),
    memberchk(Tab/Arity,Tabs),
    length(Fields, Arity).

backend_nowrite(X) :-
    check_valid(X),
    backend_execute(X).

@(X) :-
    backend_nowrite(X),
    (X \= query(_) -> backend_write(X); true).

backend_execute(query(R)) :- R.
backend_execute(assert(R)) :- assertz(R).
backend_execute(retract(R)) :- retract(R).

backend_write(Term) :-
    write_canonical(data_file, Term),
    write(data_file, '.'),
    nl(data_file),
    flush_output(data_file).
    

commit :- close(data_file).
