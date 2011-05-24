loop :- main_loop; write(':)'),nl, true.
main_loop :-
    repeat,
    read(X),
    (
        X==end_of_file -> ! 
    ;
        \+ catch(X, E, (write(E),nl)) ->
            write(failed), nl
    ), fail.
