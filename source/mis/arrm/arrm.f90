!*==arrm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE arrm(P,D,Nd)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: P
   REAL(REAL64) , DIMENSION(3) :: D
   INTEGER , DIMENSION(3) :: Nd
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dx , px
   INTEGER :: i , nx
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     SCALED ARITHMETIC ROUTINES--ARRANGING ROUTINE
!
   DO i = 1 , 3
      IF ( D(i)/=0.0D0 ) THEN
         DO WHILE ( dabs(D(i))<1.0 )
            D(i) = D(i)*10.0
            Nd(i) = Nd(i) - 1
         ENDDO
         DO WHILE ( dabs(D(i))>=10.0 )
            D(i) = D(i)*0.1
            Nd(i) = Nd(i) + 1
         ENDDO
      ENDIF
   ENDDO
   IF ( Nd(1)>Nd(2) .AND. Nd(2)>Nd(3) ) RETURN
   IF ( Nd(1)<=Nd(2) .OR. Nd(1)<=Nd(3) ) THEN
      IF ( Nd(2)<Nd(3) ) THEN
      ELSEIF ( Nd(2)==Nd(3) ) THEN
         IF ( dabs(D(2))>=dabs(D(3)) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ELSE
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( Nd(1)<Nd(3) ) THEN
      ELSEIF ( Nd(1)==Nd(3) ) THEN
         IF ( dabs(D(1))>=dabs(D(3)) ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ELSE
         CALL spag_block_2
         RETURN
      ENDIF
      nx = Nd(1)
      dx = D(1)
      px = P(1)
      Nd(1) = Nd(3)
      D(1) = D(3)
      P(1) = P(3)
      Nd(3) = nx
      D(3) = dx
      P(3) = px
   ENDIF
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      IF ( Nd(1)<Nd(2) ) THEN
      ELSEIF ( Nd(1)==Nd(2) ) THEN
         IF ( dabs(D(1))>=dabs(D(2)) ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ELSE
         CALL spag_block_2
         RETURN
      ENDIF
      Nx = Nd(1)
      Dx = D(1)
      Px = P(1)
      Nd(1) = Nd(2)
      D(1) = D(2)
      P(1) = P(2)
      Nd(2) = Nx
      D(2) = Dx
      P(2) = Px
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( Nd(2)<Nd(3) ) THEN
      ELSEIF ( Nd(2)==Nd(3) ) THEN
         IF ( dabs(D(2))>=dabs(D(3)) ) RETURN
      ELSE
         RETURN
      ENDIF
      Nx = Nd(2)
      Dx = D(2)
      Px = P(2)
      Nd(2) = Nd(3)
      D(2) = D(3)
      P(2) = P(3)
      Nd(3) = Nx
      D(3) = Dx
      P(3) = Px
   END SUBROUTINE spag_block_2
END SUBROUTINE arrm
