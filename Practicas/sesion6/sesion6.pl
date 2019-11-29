% Atance Sanz, Diego
% Corrales Agustin, Ignacio

% ej 1
/*
?- profesion(X,Y), quizas_tenga(Y,taladro).
X = roberto,
Y = carpintero.

?- sospechoso(X,Y,relacion_sentimental).
X = alfonso,
Y = filomena.

?- asesinado_con(X,Y), sospechoso(Z,Y,_).
false.


?- profesion(X,Y), quizas_tenga(Y,Z), es(Z,arma_blanca).
X = alfonso,
Y = carnicero,
Z = cuchillo ;
X = roberto,
Y = carpintero,
Z = sierra ;
false.

?- tiene_pasado_turbio(X), sospechoso(X,,).
X = juan ;
X = juan ;
false.


Nuevos predicados:

quizas_tenga(pocero, soga).
es(soga, material_obra).
asesinado_con(pepita, material_obra).
sospechoso(X, Y, mismo_tipo_objeto) :-
	asesinado_con(Y, Tipo_obj),
	profesion(X, Prof),
	es(Obj, Tipo_obj).

Output:

?- profesion(X, Y), quizas_tenga(Y, Z), es(Z, material_obra).
X = pepe,
Y = pocero,
Z = soga.

*/

% ej 2

mezcla([], _, []).
mezcla(_, [], []).
mezcla([E1 | L1], [E2 | L2], [E1 , E2 | L]) :- mezcla(L1, L2, L).

% el 3 A

sublista2([], _, _).
sublista2([X | XS], [X | XSS], P) :- sublista2(XS, XSS, P).
sublista2([X | XS], [X2 | XSS], P) :- sublista(P, [X2 | XSS]).

sublista([], _).
sublista([X | XS], [X | XSS]) :- sublista2(XS ,XSS, [X | XS]).
sublista([X | XS], [_ | XSS]) :- sublista([X | XS], XSS).

% ej 3 B

contenido(XS, YS) :- contenido(XS, YS, YS).
contenido([], _, _).
contenido([X1 | XS], [Y | YS], L) :- contenido([X1 | XS], YS, L).
contenido([X | XS], [X | YS], L) :- contenido(XS, L, L).

% ej 4

nat(c).
nat( s(X) ) :- nat(X).

suma(c, Y, Y):- nat(Y).
suma(s(X), Y, s(Z)):- suma(X, Y, Z).


num_nodos(void,c).
num_nodos(arbol(_,I,D), s(Y) ):-
	num_nodos(I,SumaIzq),
	num_nodos(D,SumaDer),
	suma(SumaIzq, SumaDer ,Y).