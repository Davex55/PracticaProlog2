%TEST PROLOG

% eliminar_comodines()

?-eliminar_comodines(regs(1,1,+,5,*), regs(1,1,+,5,_), [1,1,+,5]).
      yes

?-eliminar_comodines(regs(1,1,+,*,5), regs(1,1,+,_,5), [1,1,+,5]).
      yes

?-eliminar_comodines(regs(1,1,+,5,*), regs(1,1,+,5,_), [1,1,+,5,_]).
      no

?-eliminar_comodines(regs(1,1,+,5), regs(1,1,+,5), [1,1,+,5]).
      yes

?-eliminar_comodines(regs(a,b,1,+,2,*,c), regs(a,b,1,+,2,_,c), [a,b,1,+,2,c]).
      yes

?-eliminar_comodinesregs(0,*,-,f,*), regs(0,*,-,f,_), [0,-,f]).
      yes

?-eliminar_comodines(regs(a,b,1,+,2,*,c), regs(a,b,1,+,2,_,c),  [1,1,+,5,_]).
      no

% ejecutar_instruccion

