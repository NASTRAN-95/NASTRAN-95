!*==alg17.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg17(Istak,Pltsze,Itrig,Title,Ikdum,Ifplot)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Istak
   REAL :: Pltsze
   INTEGER :: Itrig
   REAL , DIMENSION(18) :: Title
   INTEGER :: Ikdum
   INTEGER :: Ifplot
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bal , plttit , xback1 , xback2 , xlen1 , xlen2 , yback1 , yback2 , ylen1 , ylen2
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
   plttit = Pltsze*.1
   IF ( Istak<2 ) THEN
      IF ( Istak==0 ) THEN
         xlen1 = .15*Pltsze
         xlen2 = .70*Pltsze
         xback1 = -1.9 + .20*Pltsze
         xback2 = -6.2 + .20*Pltsze
         IF ( Ikdum==1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ELSE
         xlen1 = .70*Pltsze
         xlen2 = .15*Pltsze
         xback1 = -1.9 - .20*Pltsze
         xback2 = -6.2 - .20*Pltsze
         IF ( Ikdum/=1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
      bal = .25*Pltsze
      ylen1 = .50*Pltsze
      ylen2 = -.15*Pltsze
   ELSE
      bal = .35*Pltsze
      xlen1 = .3*Pltsze
      xlen2 = xlen1
      ylen1 = .25*Pltsze
      ylen2 = -1.*ylen1
      xback1 = -1.9
      xback2 = -6.2
   ENDIF
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Bal = .50*Pltsze
      Ylen1 = .15*Pltsze
      Ylen2 = -.50*Pltsze
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      Yback1 = -(.35+Bal)
      Yback2 = Yback1 - .01*Pltsze - .175
      IF ( Itrig==2 ) Xback1 = Xback1 + 0.35
   END SUBROUTINE spag_block_2
END SUBROUTINE alg17
