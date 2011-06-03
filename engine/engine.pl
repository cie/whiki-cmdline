
:- discontiguous(('?')/1).

?reload :- ['engine/engine.pl'].

?lists :- writeall(L, @query(wlist(L))).
?add_list(L) :- \+ @query(wlist(L)), @assert(wlist(L)).
?items(L) :-  @query(wlist(L)), (@query(witem(L,I)), \+ @query(wpar(_,I)), write(I), nl, write_descendants(I,1), fail; true).
?items(L, I) :-  @query(witem(L,I)), write(I), nl, write_descendants(I,1).
?add_item(L,I) :- \+ @query(witem(_,I)), @assert(witem(L,I)).
?add_item(L,P,I) :- \+ @query(witem(_,I)), @query(witem(L,P)), @assert(witem(L,I)), @assert(wpar(P,I)).
?add_par(A,B) :-  @query(witem(L,A)), @query(witem(L,B)), \+ @query(wpar(A,B)), \+ @query(wpar(B,A)), @assert(wpar(A,B)).
?pars(I) :- @query(witem(_,I)), write(I), nl, write_ancestors(I,1).
write_ancestors(I,N) :- N1 is N+1, @query(wpar(C,I)), indent(N), write(C), nl, write_ancestors(C, N1), fail;true.
?descendants(I) :- @query(witem(_,I)), write(I), nl, write_descendants(I,1).
write_descendants(I,N) :- N1 is N+1, @query(wpar(I,C)), indent(N), write(C), nl, write_descendants(C, N1), fail;true.

?add_descendant(P,A) :- @query(witem(L,P)), ?add_item(L,P,A).
?del_par(A,B) :- @retract(wpar(A,B)).
?del_item(A) :- \+ @query(wpar(_,A)), \+ @query(wpar(A,_)), \+ (@query(wrel(X)), member(_:A, X)), @retract(witem(_,A)).
?ancestors(A) :- @query(witem(_,A)), (@query(wpar(B,A)), write(B), nl, ?ancestors(B), fail; true).
?add_rel(R) :- R=[_,_|_],
    itemset_rel(R,Rel),
    \+ @query(wrel(Rel)),
    @assert(wrel(Rel)).
?AddRel :- AddRel =.. [add_rel,A,B|T], ?add_rel([A,B|T]).

items_lists(R,ItemLists) :- 
    findall(L, (
        member(I, R),
        @query(witem(L,I))
    ), ItemLists).
itemset_rel(R,Rel) :-
    items_lists(R,ItemLists),
    % check if every item is really an item, and every list appears only once
    length(R, Count), sort(ItemLists, AllLists), length(AllLists, Count),
    findall(I, (
        @query(wlist(L)),
        (
            nth(N,ItemLists,L) -> nth(N,R,I);
            I=''
        )
    ), Rel).

% this awsome clause turns [a,'',b,''] into [a,_,b,_] and back
rel_pattern([],[]).
rel_pattern([R|TR],[P|TP]) :- (R='',var(P); R=P, R\=''), rel_pattern(TR,TP).
    
    
    
relation_complement(Rel1, Rel) :-
    count(@query(wlist(_)), N), 
    length(Rel1, N1), N0 is N-N1,
    times(N0, '', L),
    append(Rel1, L, Rel).



?rels(A) :-
    itemset_rel([A],Rel),
    rel_pattern(Rel, Pattern),
    writeall(Pattern, (@query(wrel(R)), relation_complement(R,Pattern)) ).


%truncate([],[]).
%truncate([A|T], [A]) :- all_vars(T).
%truncate([A|T], [A|T1]) :- truncate(T,T1).

%all_vars([]).
%all_vars([A|T]) :- var(A), all_vars(T).


lookup_rels(Items, LQuery) :-
    itemset_rel(Items,Rel),
    writeall(R, 
    (
        ancestor_or_descendant_rel(Rel,Rel1),
        rel_pattern(Rel1,Pattern),
        findall(L, @query(wlist(L)), Lists),
        nth(N,Lists,LQuery),
        relation(Pattern),
        nth(N,Pattern,R),
        \+var(R)
    )).
?Rels :- Rels =.. [rels|T], append(Items, [LQuery], T), Items=[_|_], lookup_rels(Items, LQuery).


ancestor(P,I) :- @query(wpar(P,I)).
ancestor(P,I) :- @query(wpar(C,I)), ancestor(P,C).

ancestor_or_descendant_rel([],[]).
ancestor_or_descendant_rel([R1|T1],[R|T]) :- (R1=R;ancestor(R1,R);ancestor(R,R1)), ancestor_or_descendant_rel(T1,T).


relation(Rel) :-
    @query(wrel(RelA1)), relation_complement(RelA1, RelA),
    @query(wrel(RelB1)), relation_complement(RelB1, RelB),
    rel_pattern(RelA, Rel),
    rel_pattern(RelB, Rel).


?allrels :- relation(Pattern), rel_pattern(Rel, Pattern), write(Rel), nl, fail; true.


?rels :- writeall(Rel, @query(wrel(Rel))).





?re :- ?reload.
?l :- ?lists.
?al(L) :- ?add_list(L).
?i(L) :- ?items(L).
?i(L,I) :- ?items(L,I).
?ai(L,I) :- ?add_item(L,I).
?ai(L,P,I) :- ?add_item(L,P,I).
?ap(A,B) :- ?add_par(A,B).
?ar(A,B) :- ?add_rel(A,B).
?p(A) :- ?pars(A).
?d(A) :- ?descendants(A).
?ad(P,A) :- ?add_descendant(P,A).
?dp(A,B) :- ?del_par(A,B).
?a(A) :- ?ancestors(A).
?ar([A|T]) :- ?add_rel([A|T]).
?AR :- AR =.. [ar,A,B|T], ?add_rel([A,B|T]).
?R :- R =.. [r|T], append(Items, [LQuery], T), Items=[_|_], lookup_rels(Items, LQuery).
?r(A) :- ?rels(A).
?r :- ?rels.
?rr :- ?allrels.




indent(N) :- for(_,1,N), write(' '), fail; true.
writeall(K, Goal) :- Goal, write(K), nl, fail; true.
times(N, E, L) :-
    findall(E, for(_,1,N), L).
count(Goal, N) :-
    findall(_,Goal,L), length(L,N).


/*
 * Hibák:
 * R = [mexico,_,carlos_fuentes,galaicoportugues,edad_media] ? ; !!!!!!!!
 * Őrült baromság. Mi az értelmezése a virtuális relációs tábláinknak és az
 * egyesített relációs táblánknak?
 */
