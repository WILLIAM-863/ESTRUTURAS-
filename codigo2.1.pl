persona(abraham,   hombre, [herbert, homero]).
persona(clancy,    hombre, [marge, patty, selma]).
persona(homero,    hombre, [bart, lisa, maggie]).
persona(herbert,   hombre, []).
persona(bart,      hombre, []).
persona(lisa,      mujer,  []).
persona(maggie,    mujer,  []).
persona(ling,      mujer,  []).

persona(mona,      mujer,  [herbert, homero]).
persona(jackeline, mujer,  [marge, patty, selma]).
persona(marge,     mujer,  [bart, lisa, maggie]).
persona(patty,     mujer,  []).
persona(selma,     mujer,  [ling]).

% Reglas para verificar parentesco usando estructuras
es_mujer(X) :- persona(X, mujer, _).
es_hombre(X) :- persona(X, hombre, _).

% Padre o madre (ahora se deduce de la estructura)
padre(X, Y) :- persona(X, hombre, Hijos),member(Y, Hijos).
madre(X, Y) :- persona(X, mujer, Hijos),member(Y, Hijos).

% Se usa setof para que no repita el ciclo.
abuelo(X, Y) :-
    setof(
        Y,
        (persona(X, hombre, Hijos),
         member(Hijo, Hijos),
         persona(Hijo, _, Nietos),
         member(Y, Nietos)),
        ListaAbuelos
    ),
    member(Y, ListaAbuelos).

abuela(X, Y) :-
    setof(
        Y,
        (persona(X, mujer, Hijos),
         member(Hijo, Hijos),
         persona(Hijo, _, Nietos),
         member(Y, Nietos)),
        ListaAbuelas
    ),
    member(Y, ListaAbuelas).
   
hijo(X, Y) :-
    setof(
        Y,
    (persona(Y, _, Hijos), 
    member(X, Hijos), es_hombre(X)),
        ListaHijo
        ),
    member(Y, ListaHijo).

hija(X, Y) :-
    setof(
        Y,
    (persona(Y, _, Hijos), member(X, Hijos), es_mujer(X)),
        ListaHija),
    member(Y,ListaHija).


hermano(X, Y) :-
    persona(_, _, Hijos),
    member(X, Hijos),
    member(Y, Hijos),
    X \= Y,
    es_hombre(X),!.

hermana(X, Y) :-
    persona(_, _, Hijos),
    member(X, Hijos),
    member(Y, Hijos),
    X \= Y,
    es_mujer(X), !.


tio(X, Y) :-
    hermano(X, A),
    (hijo(Y, A); hija(Y, A)),
    es_hombre(X).

tia(X, Y) :-
    hermana(X, A),
    (hijo(Y, A); hija(Y, A)),
    es_mujer(X).

% Primos (sin repetición)
primos(X, Y) :-
    (persona(ParentX, _, HijosX), member(X, HijosX), A = ParentX),
    (persona(ParentY, _, HijosY), member(Y, HijosY), B = ParentY),
    X \= Y,
    (hermano(A, B); hermana(A, B)),!.

lista_primos(X, ListaPrimos) :-
    findall(P, primos(X, P), ListaPrimos).