!*==filcor.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION filcor(Mt1x,Mt2x,Pc,Frsrow,Midrow,Nx,A,Nza,Z)
   IMPLICIT NONE
   USE C_UNPAKX
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: filcor
   INTEGER :: Mt1x
   INTEGER :: Mt2x
   INTEGER :: Pc
   INTEGER :: Frsrow
   INTEGER :: Midrow
   INTEGER :: Nx
   REAL , DIMENSION(1) :: A
   INTEGER :: Nza
   REAL , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k , lasrow , mt , mt1 , mt2 , n , na , nn
   EXTERNAL close , gopen , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ IN EACH ROW
!
         IF ( na+n-lasrow>nn ) THEN
            filcor = lasrow
            RETURN
         ELSE
            lasrow = lasrow + 1
            i = Pc*na + 1
            Ii = lasrow
            CALL unpack(*20,mt,A(i))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      k = i + lasrow*Pc - 1
         DO j = i , k
            A(j) = 0.0
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         IF ( lasrow==n ) THEN
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
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END FUNCTION filcor
