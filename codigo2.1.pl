% Definición de estructuras para representar personas y relaciones familiares
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

% Abuelo/abuela
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
   


% Hijo/hija
hijo(X, Y) :-
    setof(
        Y,
    (persona(Y, _, Hijos), 
    member(X, Hijos), es_hombre(X)),
        ListaHijo
        ),
    member(Y, ListaHijo).