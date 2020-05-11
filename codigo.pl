%Nuestros datos
alumno_prode('Benavente','Alvarez','Alejandro',160319). %PORTAVOZ
alumno_prode('Doncel','Aparicio','Alberto',160364).   
alumno_prode('Lin','Tsai','Alvin',160267).
%-------------------------------------------

eliminar_comodines(Regs, R, L):-
    Regs =.. A,
    eliminar_comodin(A, R, L).

eliminar_comodin([],_,_).

eliminar_comodin(A|B, R, L):-
    comprobar_comodin(A, R, L),
    eliminar_comodin(B, R, L).

comprobar_comodin(A,R,L):-
    A =:= '*',
    B = _,
    R = R|B.

comprobar_comodin(A,R,L):-
    A =\= '*',
    R = R|A,
    L = L|A.

