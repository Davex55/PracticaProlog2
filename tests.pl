%TEST PROLOG

% eliminar_comodines()

?-eliminar_comodines(regs(1,1,+,5,*), regs(1,1,+,5,_), [1,1,+,5]).
      yes

?-eliminar_comodines()
      yes

?-eliminar_comodines()
      yes

?-eliminar_comodines()
      yes

% ejecutar_instruccion

