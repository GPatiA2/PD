% Ejercicio 1 
elimina1([ ],_,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

elimina2([ ],_,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).


elimina3([ ],_,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

/*
 ?- elimina1([a,b,a,c],a,L).
 L = [b, c] .

 ?- elimina2([a,b,a,c],a,L).
 L = [b, c] .

 ?- elimina3([a,b,a,c],a,L).
 L = [b, c] .



 ?- elimina1([a,b,a,c],X,L).
 L = [a, b, a, c].

 ?- elimina2([a,b,a,c],X,L).
 X = a,
 L = [b, c] .

 ?- elimina3([a,b,a,c],X,L).
 X = a,
 L = [b, c] .
*/

% ejercicio 2

simetrico(void,void).
simetrico(arbol(X,Xai,Xad),arbol(X,Yai,Yad)):- simetrico(Xai,Yad), simetrico(Xad,Yai).

sumatree(void,0).
sumatree(arbol(X,Xai,Xad),S):- number(X), sumatree(Xai,Sizq), sumatree(Xad,Sder), S is Sizq+Sder + X.

% maxveces(A, X) ←→ A es un ´arbol binario y X es el elemento que aparece repetido m´as veces en sus nodos.

listree(void, []).
listree(arbol(X, I, D), L) :- listree(I, Z1), listree(D, Z2), append(Z1, [X], L1), append(Z2, L1, L).
countel(X, N, L) :- aggregate(count, member(X, L), N).
rep(X, N, L) :- aggregate(max(N1, X1), countel(X1, N1, L), max(N, X)).
maxveces(A, X) :- listree(A, L1), msort(L1, LS), rep(X, N, LS).


%ejercicio 3
%subtermino(S, T) ←→ S es un subt´ermino de T.
subtermino(X,X).
subtermino(S,FUNCION):- FUNCION =..[X|Xs], subtermino_aux(S,Xs).
subtermino_aux(S,[X|Xs]):-  subtermino(S,X); subtermino_aux(S,Xs).


%subst(T, S, E, NE) ←→ NE es el resultado de reemplazar en la estructura E cada aparicion del termino T por S.
%subst(T, S, E, NE):-


% ejercicio 4
%hanoi 2 lineas, caso base y dos llamadas recursivas
hanoi(1, X, _, Z, [[X, Z]]).
hanoi(N, X, Y, Z, S) :-
    M is (N - 1),
    hanoi(M, X, Z, Y, P1),
    hanoi(1, X, Y, Z, P2),
    hanoi(M, Y, X, Z, P3),
    append(P1, P2, Q),
    append(Q, P3, P).