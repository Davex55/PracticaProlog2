:- module(Module, PublicList).

%Nuestros datos
alumno_prode('Benavente','Alvarez','Alejandro',160319). %PORTAVOZ
alumno_prode('Doncel','Aparicio','Alberto',160364).   
alumno_prode('Lin','Tsai','Alvin',160267).
%-------------------------------------------

eliminar_comodines(Regs, R, L):-
    functor(Regs, _, N),
    recorrerLista(Regs, R1, L, 1, N),
    R =.. [regs|R1].

%recorrerLista(_, _, 0 , _).
recorrerLista(Regs, [_], [], N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B = comodin,
    !.
recorrerLista(Regs, [X1],[X1], N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B \= comodin,
    !.
recorrerLista(Regs, [_|R],L, N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B = comodin,
    recorrerLista(Regs, R, L,N2, N).
recorrerLista(Regs, [X1|R], [X1|L], N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B \= comodin, 
    recorrerLista(Regs, R, L, N2, N).

comprobar_comodin(A,B) :-
      A == *,
      B = comodin,
      !.
  
comprobar_comodin(A,A). 

ejecutar_instruccion(Regs,Ins,ES):-
    Ins =.. [I,A,B],
    Regs =.. [R|L1],
    I == swap,
    functor(Regs, _, N),
    A =< N,
    B =< N,
    P1 is A-1,
    P2 is B-1,
    swap(L1, P1, P2, S),
    ES =.. [R|S].

ejecutar_instruccion(Regs,Ins,ES):-
    Ins =.. [I,N1,N2],
    Regs =.. [R|L1],
    I == move,
    move(L1, N1, N2, ES),
    ES =.. [I].
    
swap( L, On, With, L ) :-
    On = With.
swap( L, On, With, S ) :-   
    swap2( L, On, With, S, _, _ ).
swap( L, On, With, S ) :-
    swap2( L, With, On, S, _, _ ).

swap2( L, _, 0, S, E_on, E_with ) :-
    L = [E_with|Ls],
    S = [E_on|Ls],
    !.
swap2( L, On, With, S, E_on, E_with ) :-
    On = 0,
    L = [E_on|Es],
    S = [E_with|Rs],
    N1 is On - 1,
    N2 is With - 1,
    swap2( Es, N1, N2, Rs, E_on, E_with ),
    !.
swap2( L, On, With, S, E_on, E_with ) :-
    L = [E|Es],
    S = [E|Rs],
    N1 is On - 1,
    N2 is With - 1,
    swap2( Es, N1, N2, Rs, E_on, E_with ).

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

