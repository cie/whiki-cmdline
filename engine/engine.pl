
:- discontiguous(('?')/1).

?reload :- ['engine/engine.pl'].

?lists :- @query(wlist(L)), write(L), nl, fail; true.
?add_list(L) :- \+ @query(wlist(L)), @assert(wlist(L)).
?items(L) :-  @query(wlist(L)), (@query(witem(L,I)), \+ @query(wpar(_,I)), write(I), nl, write_descendants(I,1), fail; true).
?items(L, I) :-  @query(witem(L,I)), write(I), nl, write_descendants(I,1).
?add_item(L,I) :- \+ @query(witem(L,I)), @assert(witem(L,I)).
?add_item(L,P,A) :- ?add_item(L,A), ?add_par(P,A).
?add_par(A,B) :-  @query(witem(L,A)), @query(witem(L,B)), \+ @query(wpar(A,B)), \+ @query(wpar(B,A)), @assert(wpar(A,B)).
?descendants(I) :- write_descendants(I,0).
write_descendants(I,N) :- N1 is N+1, @query(wpar(I,C)), indent(N), write(C), nl, write_descendants(C, N1), fail;true.

?add_descendant(P,A) :- @query(witem(L,P)), ?add_item(L,P,A).
?del_par(A,B) :- @retract(wpar(A,B)).
?del_item(A) :- \+ @query(wpar(_,A)), \+ @query(wpar(A,_)), \+ (@query(wrel(X)), member(_:A, X)), @retract(witem(_,A)).
?ancestors(A) :- @query(witem(_,A)), (@query(wpar(B,A)), write(B), nl, ?ancestors(B), fail; true).
?add_rel([A,B|T]) :-
    findall(L, (
        member(I, [A,B|T]),
        @query(witem(L,I))
    ), ItemLists),
    % check if all item is really an item, and every list appears only once
    length([A,B|T], Count), sort(ItemLists, AllLists), length(AllLists, Count),
    findall(I, (
        @query(wlist(L)),
        (
            nth(N,ItemLists,L) -> nth(N,[A,B|T],I);
            I=''
        )
    ), Rel),
    @assert(wrel(Rel)).
?AddRel :- AddRel =.. [add_rel,A,B|T], ?add_rel([A,B|T]).

?rels(A) :-
    @query(witem(LBase,A)),
    findall(I, (
        @query(wlist(L)),
        ( 
            L==LBase -> I=A
            ; I=_ 
        )
    ), Pattern),
    (
        @query(wrel(Pattern)),
        write(Pattern), nl,
        fail;true
    ).


?rels(A,LQuery) :- 
    @query(witem(LBase,A)),
    findall(I, (
        @query(wlist(L)),
        ( 
            L==LBase -> I=A;
            L==LQuery -> I=R
            ; I=_ 
        )
    ), Pattern),
    (
        relation(Pattern),
        write(R), nl,
        fail;true
    ).


relation(Rel) :-
    @query(wrel(RelA)),
    @query(wrel(RelB)),
    findall(I, (
        member(Item, RelA),
        (
            Item == '' -> I=_;
            I=Item
        )
    ), Rel),
    findall(I, (
        member(Item, RelB),
        (
            Item == '' -> I=_;
            I=Item
        )
    ), Rel).


?rr :- relation(Rel), findall(I, (
        member(Item, Rel),
        (
            var(Item) -> I='';
            I=Item
        )
    ), Rec), write(Rec), nl, fail; true.


?r :- @query(wrel(Rel)), write(Rel), nl, fail; true.







?re :- ?reload.
?l :- ?lists.
?al(L) :- ?add_list(L).
?i(L) :- ?items(L).
?i(L,I) :- ?items(L,I).
?ai(L,I) :- ?add_item(L,I).
?ai(L,P,I) :- ?add_item(L,P,I).
?ap(A,B) :- ?add_par(A,B).
?ar(A,B) :- ?add_rel(A,B).
?d(A) :- ?descendants(A).
?ad(P,A) :- ?add_descendant(P,A).
?dp(A,B) :- ?del_par(A,B).
?a(A) :- ?ancestors(A).
?ar([A|T]) :- ?add_rel([A|T]).
?AR :- AR =.. [ar,A,B|T], ?add_rel([A,B|T]).
?r(A,L) :- ?rels(A,L).
?r(A) :- ?rels(A).



indent(N) :- for(_,1,N), write(' '), fail; true.
