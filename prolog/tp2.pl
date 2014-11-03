%Autómatas de ejemplo. Si agregan otros, mejor.

ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1), (s3, b, s2), (s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11], 
        [(s2, a, s3), (s4, a, s5), (s9, a, s10), (s5, d, s6), (s7, g, s8), (s15, g, s11), (s6, i, s7), (s13, l, s14), (s8, m, s9), (s12, o, s13), (s14, o, s15), (s1, p, s2), (s3, r, s4), (s2, r, s12), (s10, s, s11)])).
ejemplo(11, a(s1, [s2,s3], [(s1,a,s2), (s2,b,s3)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2), (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.

%%Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

transicionDesde((D,_,_),D).

transicionPor((_,P,_),P).

transicionHacia((_,_,H),H).

%Auxiliar dada en clase
%desde(+X, -Y).
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.


%%Predicados pedidos.

% 1) %esDeterministico(+Automata)
esDeterministico(a(_,_,[])).
esDeterministico(a(I,F, [X|L])) :- transicionDesde(X,D), transicionPor(X,P), not(member((D,P,_),L)),
                                   esDeterministico(a(I,F,L)), !.

% 2) 
%concatenar (?Lista1,?Lista2,?Lista3)
concatenar([X|Xs], L2, [X|Ys]):- concatenar(Xs, L2, Ys).
concatenar([], L2, L2).

%estadosDeLasTransiciones(+Transiciones, +Estados)
estadosDeLasTransiciones([],[]).
estadosDeLasTransiciones([X|Ls],L):- estadosDeLasTransiciones(Ls,M), transicionDesde(X,D), transicionHacia(X,H),
                                     concatenar([D],[H],L1), concatenar(L1,M,L).

%estadosSinRepetidos(+Automata, -Estado)
estadosSinRepetidos(A,E):- inicialDe(A,I), finalesDe(A,F), transicionesDe(A,T), estadosDeLasTransiciones(T,T1), 
                           concatenar([I],F,E1), concatenar(E1,T1,E2), setof(X, member(X,E2),E).

%estados(+Automata, ?Estados)
estados(A, E):- var(E), estadosSinRepetidos(A,E), !. 
estados(A, E):- nonvar(E), estadosSinRepetidos(A,M), setof(X, member(X,E),N), M=N.

% 3)
%hayTransicion(+Automata, +EstadoInicial, +EstadoFinal)
hayTransicion(A,I,F):- transicionesDe(A,T), Transicion = (I,_,F), member(Transicion,T).

%esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)
esCamino(_, _, _, []):- false.
esCamino(A, X, X, [X]):- estados(A,E), member(X,E), !.
esCamino(A, X, F, [X|[Y|Ls]]):- hayTransicion(A,X,Y), esCamino(A,Y,F,[Y|Ls]), !.



% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% Responder aquí.
%	Respuesta: El predicado esCamino, de la manera en que fue definido, no es reversible con respecto a Camino
%		Cuando el parametro de "camino" queda sin instanciar, y el automata A contiene ciclos, el predicado se cuelga. Ejemplo:
%			ejemplo(4,A), esCamino(A, s1, s3, C).
%		En cambio, cuando el automata A no tiene ciclos, no se cuelga, y el predicado sería reversible con respecto a Camino en ese caso

% 5) 


%caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
caminoDeLongitud(A, 1, [S2], [], S2, S2):- estados(A,E), member(S2,E).
caminoDeLongitud(A, N, C, Etiquetas, S1, S2):- N\=1, estados(A,E), member(S1,E), transicionesDe(A,T), 
                                    Transicion = (S1,Etiqueta,S3),member(Transicion,T), 
                                    Nmenos1 is N-1, caminoDeLongitud(A,Nmenos1,C1,E1,S3,S2),
                                    append([S1],C1,C), append([Etiqueta],E1,Etiquetas).


% 6) alcanzable(+Automata, +Estado)
alcanzable(A,E) :- inicialDe(A,I), estados(A,K), length(K,L), between(2,L,N), caminoDeLongitud(A,N,_,_,I,E), !.
%La idea es que a partir del estado inicial se verifique si existe un camino a E de longitud N, con 1<N<cantidadDeEstados

% 7) automataValido(+Automata)
automataValido(A) :- estadosFinalesValidos(A), todosAlcanzables(A), hayEstadoFinal(A), noHayEstadosFinalesRepetidos(A), noHayTransicionesRepetidas(A).
%Evalúa todas los predicados presentados con A.

%estadosFinalesValidos(+A)
estadosFinalesValidos(A) :- estados(A,E), finalesDe(A,F), subtract(E,F,M), forall(member(X,M), caminoDeLongitud(A,2,_,_,X,_)), !.
%Verifica que exista al menos un camino desde cada estado, a menos que el mismo sea uno final

%todosAlcanzables(+A)
todosAlcanzables(A) :- estados(A,E),inicialDe(A,I), subtract(E,[I],M), forall(member(X,M),alcanzable(A,X)).
%Verifica que todos los estados sean alcanzables desde el inicial, menos el inicial.

%hayEstadoFinal(+A)
hayEstadoFinal(A) :- finalesDe(A,F), not(F = []).
%Dados los estados finales de un autómata, se fija que la lista no sea vacía.

%noHayEstadosFinalesRepetidos(+A)
noHayEstadosFinalesRepetidos(A) :- finalesDe(A,F), borrarDuplicados(F,X), X = F.
%Compara la lista de estados finales con ella misma sin sus repetidos.

% borrarDuplicados(+L, -T):
borrarDuplicados([],[]).
borrarDuplicados([X|Xs], F) :- member(X, Xs), borrarDuplicados(Xs, F), !.
borrarDuplicados([X|Xs], [X|F]) :- not(member(X, Xs)), borrarDuplicados(Xs, F), !.
%Elimina los duplicados de cualquier lista.

%noHayTransicionesRepetidas(+A)
noHayTransicionesRepetidas(A) :- transicionesDe(A,T), borrarDuplicados(T,X), X = T.
%Compara la lista de transiciones con ella misma sin repetidos.

%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% 8) hayCiclo(+Automata)
hayCiclo(A) :- estados(A,E), length(E,L), member(X,E), R is L+1, between(2,R,N), caminoDeLongitud(A,N,_,_, X, X), !.
%La idea es que a partir de cada estado de A se fije si existe un camino de un estado a sí mismo de longitud N, con 1<N<cantidadDeEstados+1

% 9) reconoce(+Automata, ?Palabra)
reconoce(A, P) :- nonvar(P), ground(P), length(P,Len), CantEstados is Len+1, inicialDe(A,Init), finalesDe(A,Finales), caminoDeLongitud(A, CantEstados, _, E, Init, Fin), P = E, member(Fin, Finales), !.
reconoce(A, P) :- nonvar(P), not(ground(P)), length(P,Len), CantEstados is Len+1, inicialDe(A,Init), finalesDe(A,Finales), caminoDeLongitud(A, CantEstados, _, E, Init, Fin), P = E, member(Fin, Finales).
reconoce(A, P) :- var(P), desde(1,N), inicialDe(A,Init), finalesDe(A,Finales), caminoDeLongitud(A, N, _, E, Init, Fin), member(Fin, Finales), P=E.

% 10) 
% minimaLongitudAceptada(+A,-L)
minimaLongitudAceptada(A, L) :- inicialDe(A,Init), finalesDe(A,Finales), desde(1,N), caminoDeLongitud(A, N, _, _, Init, Fin), member(Fin,Finales), !, L=N.

%PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(A, P) :- nonvar(P), minimaLongitudAceptada(A,Len), inicialDe(A,Init), finalesDe(A,Finales), caminoDeLongitud(A, Len, _, E, Init, Fin), P=E, member(Fin, Finales),!.
palabraMasCorta(A, P) :- var(P), minimaLongitudAceptada(A,Len), inicialDe(A,Init), finalesDe(A,Finales), caminoDeLongitud(A, Len, _, E, Init, Fin), P=E, member(Fin, Finales).

%-----------------
%----- Tests -----
%-----------------

% Algunos tests de ejemplo. Deben agregar los suyos.

test(1) :- forall(ejemplo(_, A),  automataValido(A)).
test(2) :- not((ejemploMalo(_, A),  automataValido(A))).
test(3) :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4) :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5) :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6) :- ejemplo(7, A), not(reconoce(A, [b])).
test(7) :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8) :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a], [b]]).
test(9) :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).
test(16) :- ejemplo(5,A), not(alcanzable(A,s5)).
test(17) :- ejemplo(5,A), alcanzable(A,s3).
test(18) :- ejemploMalo(3,A), not(alcanzable(A,s1)).
test(19) :- ejemploMalo(3,A), not(hayCiclo(A)).
test(20) :- ejemploMalo(5,A), not(hayCiclo(A)).
test(21) :- ejemploMalo(2,A), not(alcanzable(A,sf)).
tests :- forall(between(1, 21, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.
