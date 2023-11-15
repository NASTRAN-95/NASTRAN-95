
SUBROUTINE empcor(Mt1x,Mt2x,Pt,Pc,Frsrow,Midrow,Lasrow,Nx,A,Z)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , It1 , It2 , Jj
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
!
! Dummy argument declarations
!
   INTEGER Frsrow , Lasrow , Midrow , Mt1x , Mt2x , Nx , Pc , Pt
   REAL A(1) , Z(1)
!
! Local variable declarations
!
   INTEGER mcb(7) , mt , mt1 , mt2 , n , na , row
!
! End of declarations
!
!
!     EMPTY CORE OF A TRIANGULAR MATRIX
!
!
!
!     MT1      FIRST PART OF THE MATRIX (UP TO ROW -MIDROW-).
!     MT2      REST OF THE MATRIX.
!     PT       PRECISION OF THE MATRIX ON TAPE.
!     PC       ......... .. ... ...... IN CORE.
!     FRSROW   FIRST ROW IN CORE.
!     LAST     LAST  ... .. CORE.
!     N        SIZE OF THE COMPLETE MATRIX.
!     A        LOCATION OF THE COMPLETE MATRIX.
!
   DATA mcb/7*0/
   mt1 = Mt1x
   mt2 = Mt2x
   n = Nx
   mt = mt1
   IF ( Frsrow>Midrow .AND. mt2/=0 ) mt = mt2
   na = 1
   Incr = 1
   It1 = Pc
   It2 = Pt
   Jj = n
   DO row = Frsrow , Lasrow
      Ii = row
      CALL pack(A(na),mt,mcb)
      IF ( row==n ) GOTO 100
      na = na + Pc*(n-row+1)
      IF ( row==Midrow .AND. mt2/=0 ) THEN
         CALL close(mt,1)
         mt = mt2
         CALL gopen(mt,Z,1)
      ENDIF
   ENDDO
   GOTO 99999
!
!     END OF CORE DUMP
!
 100  CALL close(mt,1)
99999 END SUBROUTINE empcor
