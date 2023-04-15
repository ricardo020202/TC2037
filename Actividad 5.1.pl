% Jose Ricardo Rosales Castañeda
% Gamaliel Marines Olvera

% lasto 
% Funcion que tiene exito si el ultimo elemento de la lista es X
% lasto([1, 2, 3, 4], X).  X = 4
lasto([X], X).
lasto([_|T], X) :- lasto(T, X). 

% butlasto
% Funcion que tiene exito si si result contiene los elementos de la lista lst menos el ultimo
% butlasto([1, 2, 3, 4], X).  X = [1, 2, 3]
butlasto([X], []).
butlasto([H|T], [H|T1]) :- butlasto(T, T1).

% enlisto
% Funcion que tiene exito si result contiene los elementos de la lista lst en listas
% enlisto([1, 2, 3, 4], X).  X = [1, 2, 3, 4]
enlisto([], []). 
enlisto([H|T], [[H]|T1]) :- enlisto(T, T1).

% duplicateo
% Funcion que tiene exito si result contiene los elementos de la lista lst duplicados
% duplicateo([1, 2, 3, 4], X).  X = [1, 1, 2, 2, 3, 3, 4, 4]
duplicateo([], []).
duplicateo([H|T], [H, H|T1]) :- duplicateo(T, T1).

% removeo
% Funcion que tiene exito si puede elimiar la primera ocurrencia de X en la lista lst
% removeo(1, [1, 2, 3, 4], X).  X = [2, 3, 4]
removeo(X, [X|T], T).
removeo(X, [H|T], [H|T1]) :- removeo(X, T, T1).

% reverseo
% funcion que tiene exito si result contiene los elementos de la lista lst en orden inverso
% reverseo([1, 2, 3, 4], X).  X = [4, 3, 2, 1]
reverseo([], []).
reverseo([H|T], R) :- reverseo(T, TR), append(TR, [H], R).

% palindromeo
% Funcion que tiene exito si la lista lst es un palindromo
% palindromeo([1, 2, 3, 2, 1]).  true
palindromeo(L) :- reverseo(L, L).

% ratateo
% funcion que tiene exito si result contiene los elementos de la lista lst rotados una posicion a la izquierda
% rotateo([1, 2, 3, 4], X).  X = [2, 3, 4, 1]
rotateo([H|T], Result) :- append(T, [H], Result).

% evensizeo
% Funcion que tiene exito si la lista lst tiene un numero par de elementos
% evensizeo([1, 2, 3, 4]).  true
evensizeo([]).
evensizeo([_|T]) :- oddsizeo(T).
oddsizeo([_]).
oddsizeo([_|T]) :- evensizeo(T).

% splito
% Funcion que tiene  ́exito cuando al dividir lst se obtiene a y b. Los elementos
% primero, tercero, quinto, etc. de lst van en a, mientras que los elementos segundo,
% cuarto, sexto, etc. van en b.
% splito([1, 2, 3, 4], X, Y).  X = [1, 3], Y = [2, 4]
splito([], [], []).
splito([X], [X], []).
splito([X, Y], [X], [Y]).
splito([X, Y|T], [X|L1], [Y|L2]) :- splito(T, L1, L2).

% swappero
% funcion que tiene exito si result contiene los elementos de la lista lst con los elementos A y B intercambiados
% swappero(a, b, [a, b, a, b, b, b, a], Result).  Result = [b, a, b, a, a, a, b]
swappero(_, _, [], []).
swappero(A, B, [A|T], [B|R]) :- swappero(A, B, T, R).
swappero(A, B, [B|T], [A|R]) :- swappero(A, B, T, R).
swappero(A, B, [H|T], [H|R]) :- dif(H, A), dif(H, B), swappero(A, B, T, R).

% equalo
% Funcion logica que tiene  ́exito solo si todos los elementos contenidos en lst se unifican con el
% mismo valor. La funci ́on siempre debe tener  ́exito si lst esta vacia o tiene un solo elemento
% equalo([1, 1, 1, 1]).  true
equalo([]).
equalo([_]).
equalo([X,X|T]) :- equalo([X|T]).

% subseto
% Funcion que tiene exito si la lista lst es un subconjunto de la lista lst2
% subseto([1, 2, 3], [1, 2, 3, 4, 5]).  true
subseto([], _).
subseto([H|T], B) :- member(H, B), subseto(T, B).

% compresso
% funcion que tiene exito si result contiene los elementos de la lista lst sin elementos consecutivos repetidos
% compresso([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).  X = [a, b, c, a, d, e]
compresso([], []).
compresso([X], [X]).
compresso([X, X|T], R) :- compresso([X|T], R).
compresso([X, Y|T], [X|R]) :- dif(X, Y), compresso([Y|T], R).