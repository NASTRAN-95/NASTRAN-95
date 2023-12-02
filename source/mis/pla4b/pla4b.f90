!*==pla4b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla4b(Ke,J)
USE C_PLA42C
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(36) :: Ke
   INTEGER :: J
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , i1 , isave , j1 , j2 , jj , k , k1 , l , l1 , lim , low
   INTEGER , DIMENSION(1) :: iz
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS THE INSERTION ROUTINE FOR THE PLA4 MODULE.  IT ADDS
! THE 6 X 6 DOUBLE PRECISION MATRIX KE TO THE SUBMATRIX OF ORDER
! 6 X JMAX
!*****
!
!
!
!
! VARIABLE CORE
!
!
! PLA42 COMMUNICATIONS BLOCK
!
!
!
!
   !>>>>EQUIVALENCE (Dz(1),Z(1),Iz(1))
!
! SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
! IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
!
   low = Igpct + 1
   lim = Ngpct + low - 2
   IF ( low>lim ) THEN
      isave = low
   ELSE
      DO i = low , lim
         isave = i
         IF ( J<iabs(iz(i+1)) ) THEN
            IF ( J>=iabs(iz(i)) ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDIF
      ENDDO
      IF ( J>=iabs(iz(isave+1)) ) isave = isave + 1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
! ADD KE TO THE SUBMATRIX
!
      l1 = Frowic - 1
      jj = Ipoint + isave - Igpct
      j2 = iz(jj) - 1
      i1 = 0
      lim = Nrowsc - 1
      SPAG_Loop_1_1: DO
         IF ( i1>lim ) RETURN
         k1 = I6x6k + i1*Jmax + j2
         j1 = 0
         l = 6*l1
         k = k1
         DO
            j1 = j1 + 1
            IF ( j1>6 ) THEN
               i1 = i1 + 1
               l1 = l1 + 1
               CYCLE SPAG_Loop_1_1
            ELSE
               k = k + 1
               l = l + 1
               dz(k) = dz(k) + Ke(l)
            ENDIF
         ENDDO
         EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   END SUBROUTINE spag_block_1
END SUBROUTINE pla4b
