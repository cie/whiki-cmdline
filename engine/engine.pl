:- op(900, fx, '?').

?reload :- ['engine/engine.pl'].

?lists :- backend(query(wlist(L))), write(L), nl, fail; true.
?add_list(L) :- backend(assert(wlist(L))).
?items(L) :- backend(query(witem(L,I))), write(I), nl, fail; true.
?add_item(L,I) :- backend(assert(witem(L,I))).
?add_par(A,B) :- backend(assert(wpar(A,B))).
?add_item(L,P,A) :- ? add_item(L,A), ? add_par(P,A).



?re :- ?reload.
?l :- ?lists.
?al(L) :- ?add_list(L).
?i(L) :- ?items(L).
?ai(L,I) :- ?add_item(L,I).
