
SUBROUTINE symbol(X,Y,Symx,Opt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Model , Nsym , Ploter , Symbl(20,2)
   COMMON /pltdat/ Model , Ploter
   COMMON /symbls/ Nsym , Symbl
!
! Dummy argument declarations
!
   INTEGER Opt
   REAL X , Y
   INTEGER Symx(2)
!
! Local variable declarations
!
   INTEGER i , sym
!
! End of declarations
!
!
!     (X,Y) = POINT AT WHICH THE SYMBOLS ARE TO BE TYPED.
!     SYMX  = SYMBOLS TO BE TYPED.
!     OPT   = -1 TO INITIATE  THE TYPING MODE.
!           = +1 TO TERMINATE THE TYPING MODE.
!           =  0 TO TYPE THE SYMBOL.
!
!
   IF ( Opt==0 ) THEN
!
      DO i = 1 , 2
         IF ( Symx(i)>0 ) THEN
            sym = Symx(i) - Nsym*((Symx(i)-1)/Nsym)
            sym = Symbl(sym,Ploter)
            CALL type10(X,Y,0,sym,0,0)
         ENDIF
      ENDDO
   ELSE
      CALL tipe(0,0,0,0,0,Opt)
   ENDIF
!
END SUBROUTINE symbol
