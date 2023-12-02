!*==symbol.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE symbol(X,Y,Symx,Opt)
   USE c_pltdat
   USE c_symbls
   IMPLICIT NONE
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
            sym = Symx(i) - nsym*((Symx(i)-1)/nsym)
            sym = symbl(sym,ploter)
            CALL type10(X,Y,0,sym,0,0)
         ENDIF
      ENDDO
   ELSE
      CALL tipe(0,0,0,0,0,Opt)
   ENDIF
!
END SUBROUTINE symbol
