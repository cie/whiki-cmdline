
?reload :- ['engine/engine.pl'].

?lists :- @query(wlist(L)), write(L), nl, fail; true.
?add_list(L) :- \+ @query(wlist(L)), @assert(wlist(L)).
?items(L) :-  @query(wlist(L)), (@query(witem(L,I)), \+ @query(wpar(_,I)), write(I), nl, write_descendants(I,1), fail; true).
?add_item(L,I) :- \+ @query(witem(L,I)), @assert(witem(L,I)).
?add_item(L,P,A) :- ?add_item(L,A), ?add_par(P,A).
?add_par(A,B) :-  @query(witem(L,A)), @query(witem(L,B)), \+ @query(wpar(A,B)), @assert(wpar(A,B)).
?descendants(I) :- write_descendants(I,0).
?add_rel(A,B) :- @query(witem(LA,A)), @query(witem(LB,B)), @assert(wrel([LA:A, LB:B])).
?add_descendant(P,A) :- @query(witem(L,P)), ?add_item(L,P,A).
?del_par(A,B) :- @retract(wpar(A,B)).



?re :- ?reload.
?l :- ?lists.
?al(L) :- ?add_list(L).
?i(L) :- ?items(L).
?ai(L,I) :- ?add_item(L,I).
?ai(L,P,I) :- ?add_item(L,P,I).
?ap(A,B) :- ?add_par(A,B).
?ar(A,B) :- ?add_rel(A,B).
?d(A) :- ?descendants(A).
?ad(P,A) :- ?add_descendant(P,A).
?dp(A,B) :- ?del_par(A,B).


write_descendants(I,N) :- N1 is N+1, @query(wpar(I,C)), indent(N), write(C), nl, write_descendants(C, N1), fail;true.

indent(N) :- for(_,1,N), write(' '), fail; true.
