%:- module(Module, PublicList).

%Nuestros datos
alumno_prode('Benavente','Alvarez','Alejandro',160319). %PORTAVOZ
alumno_prode('Doncel','Aparicio','Alberto',160364).   
alumno_prode('Lin','Tsai','Alvin',160267).
%-------------------------------------------

%eliminar_comodines/3 (Registros, RegistrosSinComodines, ListaSimbolos)
% predicado que devielve yes si RegistrosSinComodines es una estructura de tipo  reg/n, que resulta de sustituir los comodines que aparecen en el argumento  Registros/n por variables
%ListaSimbolos  es una lista que contiene todos los símbolos utilizados en el término Registros/n en el mismoorden en los que estos aparecen en los registros.
eliminar_comodines(Regs, R, L):-
    functor(Regs, _, N),                                       % calculamos la ariedad de Regs
    recorrerLista(Regs, R1, L, 1, N),
    R =.. [regs|R1].                                            %pasamos R a lista para poder operar

% recorrerLista/5 (Registros, RegistrosSinComodines, ListaSimbolos, contador, ariedadDeRegistros)
% predicado auxiliar que devuelve en Registros sin comodines la lista de elementos cambiando los comodines por _
%  ademas la devuelve ListaSimbolos el alfabeto utilizado por los registros
recorrerLista(Regs, [_], [], N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B = comodin,                                        % si B es comodin el valor de R en la posicion N es _
    !.

recorrerLista(Regs, [X1],[X1], N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B \= comodin,                                       % si B no es comodin el valor de R en la posicion N es X1 y se añade X1 a L
    !.

recorrerLista(Regs, [_|R],L, N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B = comodin,                                        % si B es comodin el valor de R en la posicion N es _ 
    recorrerLista(Regs, R, L,N2, N).

recorrerLista(Regs, [X1|R], [X1|L], N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B \= comodin,                                       % si B no es comodin el valor de R en la posicion N1 es X1 y se añade X1 a L
    recorrerLista(Regs, R, L, N2, N).

% devuelve en B comodin si A unifica con *
comprobar_comodin(A,B) :-
    A == *,
    B = comodin,
    !.
  
% devuelve A en caso de que no unifique 
comprobar_comodin(A,A). 

%ejecutar_instrucciones/3 (Registros, Instruccion, Resultado)
%predicado que devuelve true si Resultado es Registros tras haberse modificado por una Instruccion
ejecutar_instruccion(Regs,Ins,ES):-
    ground(Regs),           %comprobamos que tanto Regs como Ins son variables inicializadas
    ground(Ins),
    Ins =.. [I,A,B],
    Regs =.. [R|L1],
    I == swap,
    functor(Regs, _, N),
    A =< N,
    B =< N,
    P1 is A-1,              %restamos uno debido a que en nuestra implementación de swap las lista empiezan en 0
    P2 is B-1,
    swap(L1, P1, P2, S),    %hacemos swap con las listas no con registros
    ES =.. [R|S].

ejecutar_instruccion(Regs,Ins,ES):-
    ground(Regs),
    ground(Ins),
    Ins =.. [I,N1,N2],
    Regs =.. [R|L1],
    I == move,
    move(L1, N1, N2, ES),
    ES =.. [R|ES].

%swap/4 (ListaInicial, Posicion1, Posicion2,ListaResultante)
%predicado que devuelve true si ListaResultante es ListaInicial pero con los 
%elementos en Posicion1 y Posicion2 cambiados de lugar
swap( L, P1, P2, L ) :-         %caso en el que Posicion1 y Posicion2 son el mismo número
    P1 = P2.
swap( L, P1, P2, S ) :-         %hacemos estos dos predicados para aceptar tanto swap(1,2) como swap(2,1)    
    swap2( L, P1, P2, S, _, _ ).
swap( L, P1, P2, S ) :-
    swap2( L, P2, P1, S, _, _ ).

swap2( L, _, 0, S, E_P1, E_P2 ) :-  %caso en el que llegamos a P2=0 y realizamos el cambio de números
    L = [E_P2|Ls],
    S = [E_P1|Ls],
    !.
swap2( L, P1, P2, S, E_P1, E_P2 ) :-%caso en el que llegamos a P1=0 y guardamos el estado de los numeros que estaban en las posiciones correspondientes
    P1 = 0,
    L = [E_P1|Es],
    S = [E_P2|Rs],
    N1 is P1 - 1,
    N2 is P2 - 1,
    swap2( Es, N1, N2, Rs, E_P1, E_P2 ),
    !.
swap2( L, P1, P2, S, E_P1, E_P2 ) :-%recorremos la lista
    L = [E|Es],
    S = [E|Rs],
    N1 is P1 - 1,
    N2 is P2 - 1,
    swap2( Es, N1, N2, Rs, E_P1, E_P2 ).

%swap([P1,P2],Regs,ES):-
%    Regs =.. [R|L1],
%    arg(P1,Regs,X1),
%    arg(P2,Regs,X2),
%    copy_swap(L1,[P1,P2],L2,1),
%    ES =.. [R|L2].

%copy_swap([X|L1],[P1,P2],[Y|L2],N):-
%    N \= P1,
%    N \= P2,

move(_,_,_,_).

%eliminar_comodines(Regs, R, L):-
%    Regs =.. A,
%    eliminar_comodin(A, R, L).
%
%eliminar_comodin([],_,_).
%
%eliminar_comodin(A|B, R, L):-
%    comprobar_comodin(A, R, L),
%    eliminar_comodin(B, R, L).
%
%comprobar_comodin(A,R,L):-
%    A =:= '*',
%    B = _,
%    R = R|B.
%
%comprobar_comodin(A,R,L):-
%    A =\= '*',
%    R = R|A,
%    L = L|A.

