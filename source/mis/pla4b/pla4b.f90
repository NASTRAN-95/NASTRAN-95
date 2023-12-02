!*==pla4b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla4b(Ke,J)
   USE c_pla42c
   USE c_zzzzzz
   USE iso_fortran_env
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
   low = igpct + 1
   lim = ngpct + low - 2
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
      L1 = frowic - 1
      Jj = ipoint + Isave - Igpct
      J2 = Iz(Jj) - 1
      I1 = 0
      Lim = nrowsc - 1
      SPAG_Loop_1_1: DO
         IF ( I1>Lim ) RETURN
         K1 = i6x6k + I1*jmax + J2
         J1 = 0
         L = 6*L1
         K = K1
         DO
            J1 = J1 + 1
            IF ( J1>6 ) THEN
               I1 = I1 + 1
               L1 = L1 + 1
               CYCLE SPAG_Loop_1_1
            ELSE
               K = K + 1
               L = L + 1
               Dz(K) = Dz(K) + Ke(L)
            ENDIF
         ENDDO
         EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   END SUBROUTINE spag_block_1
END SUBROUTINE pla4b
