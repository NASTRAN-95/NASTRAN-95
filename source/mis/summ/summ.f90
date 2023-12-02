!*==summ.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE summ(Sum,Isum,Term1,Iterm1,Term2,Iterm2,N)
USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: Sum
   INTEGER :: Isum
   REAL(REAL64) :: Term1
   INTEGER :: Iterm1
   REAL(REAL64) :: Term2
   INTEGER :: Iterm2
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: factor , temp1 , temp2
   INTEGER :: isave , mult
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
!
   IF ( Term1/=0.0D0 ) THEN
      IF ( Term2==0.0D0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      temp1 = Term1
      temp2 = Term2
      isave = Iterm1
      IF ( Iterm1==Iterm2 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      mult = iabs(Iterm1-Iterm2)
!DVAX TEST TO PREVENT FLOATING PT OVFLOW IF EXPONENT DIFF TOO LARGE
      IF ( mult>37 .AND. Iterm1>Iterm2 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( mult<=37 .OR. Iterm2<=Iterm1 ) THEN
         factor = 10.0D0**mult
         IF ( Iterm1>Iterm2 ) THEN
            temp2 = Term2/factor
         ELSE
            temp1 = Term1/factor
            isave = Iterm2
         ENDIF
         CALL spag_block_2
         RETURN
      ENDIF
   ENDIF
   IF ( N/=1 ) THEN
      Sum = -Term2
   ELSE
      Sum = Term2
   ENDIF
   Isum = Iterm2
   IF ( Sum==0.0D0 ) Isum = 0
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Sum = Term1
      Isum = Iterm1
      IF ( Sum==0.0D0 ) Isum = 0
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( N/=1 ) THEN
         Sum = Temp1 - Temp2
         Isum = Isave
      ELSE
         Sum = Temp1 + Temp2
         Isum = Isave
      ENDIF
      IF ( Sum==0.0D0 ) Isum = 0
   END SUBROUTINE spag_block_2
END SUBROUTINE summ
