!*==calcv.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE calcv(Pvact,Set1,Sub1,Sub2,Core)
   IMPLICIT NONE
   USE C_PATX
   USE C_SYSTEM
   USE C_TWO
   USE C_ZBLPKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pvact
   INTEGER :: Set1
   INTEGER :: Sub1
   INTEGER :: Sub2
   INTEGER , DIMENSION(1) :: Core
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: a1 , lcore
   REAL :: flag
   EXTERNAL andf , bldpk , bldpkn , close , gopen , makmcb , read , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
!
!
!
!
   N = 0
   N3 = 0
   No = 0
   N1 = 0
   CALL makmcb(Pvect,Pvact,0,2,1)
   lcore = Lc - Sysbuf
   CALL gopen(Uset,Core(lcore+1),0)
   lcore = lcore - Sysbuf
   CALL gopen(Pvact,Core(lcore+1),1)
   CALL bldpk(1,1,Pvact,0,0)
   DO
      CALL read(*100,*100,Uset,a1,1,0,flag)
      IF ( andf(Two1(Set1),a1)/=0 ) THEN
         N1 = N1 + 1
         IF ( andf(Two1(Sub1),a1)==0 ) THEN
            IF ( andf(Two1(Sub2),a1)==0 ) THEN
               B1(1) = 2.0
               N3 = N3 + 1
            ELSE
               No = No + 1
               B1(1) = 1.0
            ENDIF
            CALL zblpki
         ELSE
            N = N + 1
         ENDIF
      ENDIF
   ENDDO
 100  CALL bldpkn(Pvact,0,Pvect)
   Pvect(3) = N1
   CALL wrttrl(Pvect)
   CALL close(Uset,1)
   CALL close(Pvact,1)
END SUBROUTINE calcv
