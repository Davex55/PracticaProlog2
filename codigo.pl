%Nuestros datos
alumno_prode('Benavente','Alvarez','Alejandro',160319). %PORTAVOZ
alumno_prode('Doncel','Aparicio','Alberto',160364).   
alumno_prode('Lin','Tsai','Alvin',160267).
%-------------------------------------------

eliminar_comodines(Regs, R, L):-
    functor(Regs, _, N),
    %Regs =.. A,
    %length(R, N),
	recorrerLista(Regs, R, L, 1, N).

%recorrerLista(_, _, 0 , _).
recorrerLista(Regs, [_], _, N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B = c(),
    !.
recorrerLista(Regs, [X1],[X1], N ,N) :-
    arg(N, Regs, X1),
    comprobar_comodin(X1, B),
    B \= c(),
    !.
recorrerLista(Regs, [_|R],L, N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B = c(),
    recorrerLista(Regs, R, L,N2, N).
recorrerLista(Regs, [X1|R], [X1|L], N1, N):-
	arg(N1, Regs, X1),
    N2 is N1 + 1,
    comprobar_comodin(X1, B),
    B \= c(), 
    recorrerLista(Regs, R, L, N2, N).

comprobar_comodin(A,B) :-
      A == *,
      B = c(),
      !.
  
comprobar_comodin(A,A). 

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

