!*==symbol.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE symbol(X,Y,Symx,Opt)
   IMPLICIT NONE
   USE C_PLTDAT
   USE C_SYMBLS
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X
   REAL :: Y
   INTEGER , DIMENSION(2) :: Symx
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , sym
   EXTERNAL tipe , type10
!
! End of declarations rewritten by SPAG
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
