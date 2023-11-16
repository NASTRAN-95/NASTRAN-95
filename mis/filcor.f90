
INTEGER FUNCTION filcor(Mt1x,Mt2x,Pc,Frsrow,Midrow,Nx,A,Nza,Z)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr1 , It1 , Jj
   COMMON /unpakx/ It1 , Ii , Jj , Incr1
!
! Dummy argument declarations
!
   INTEGER Frsrow , Midrow , Mt1x , Mt2x , Nx , Nza , Pc
   REAL A(1) , Z(1)
!
! Local variable declarations
!
   INTEGER i , j , k , lasrow , mt , mt1 , mt2 , n , na , nn
!
! End of declarations
!
!
!     FILL CORE WITH A TRIANGULAR MATRIX
!
!
!
!     MT1      FIRST PART OF THE MATRIX (UP TO ROW -MIDROW-).
!     MT2      REST OF THE MATRIX
!     PC       PRECISION OF THE MATRIX IN CORE
!     NX       COLUMN SIZE OF THE MATRIX
!     A        STORAGE FOR THE MATRIX
!     Z        BUFFER FOR GINO
!     FRSROW   FIRST ROW OF THE MATRIX TO BE READ
!     ANSWER   LAST ROW READ
!
   mt1 = Mt1x
   mt2 = Mt2x
   n = Nx
   mt = mt1
   lasrow = Frsrow - 1
   It1 = Pc
   Incr1 = 1
   Jj = n
   IF ( lasrow>=Midrow .AND. mt2/=0 ) mt = mt2
   nn = Nza/Pc
   na = 0
!
!     READ IN EACH ROW
!
 100  IF ( na+n-lasrow>nn ) THEN
      filcor = lasrow
      GOTO 99999
   ELSE
      lasrow = lasrow + 1
      i = Pc*na + 1
      Ii = lasrow
      CALL unpack(*200,mt,A(i))
      GOTO 300
   ENDIF
 200  k = i + lasrow*Pc - 1
   DO j = i , k
      A(j) = 0.0
   ENDDO
 300  IF ( lasrow==n ) THEN
!
!     END OF ROUTINE
!
      CALL close(mt,1)
      filcor = lasrow
   ELSE
      na = na + (n-lasrow+1)
      IF ( lasrow==Midrow .AND. mt2/=0 ) THEN
         CALL close(mt,1)
         mt = mt2
         CALL gopen(mt,Z,0)
      ENDIF
      GOTO 100
   ENDIF
99999 RETURN
END FUNCTION filcor
