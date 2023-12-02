!*==calcv.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE calcv(Pvact,Set1,Sub1,Sub2,Core)
   USE c_patx
   USE c_system
   USE c_two
   USE c_zblpkx
   IMPLICIT NONE
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
   n = 0
   n3 = 0
   no = 0
   n1 = 0
   CALL makmcb(pvect,Pvact,0,2,1)
   lcore = lc - sysbuf
   CALL gopen(uset,Core(lcore+1),0)
   lcore = lcore - sysbuf
   CALL gopen(Pvact,Core(lcore+1),1)
   CALL bldpk(1,1,Pvact,0,0)
   DO
      CALL read(*100,*100,uset,a1,1,0,flag)
      IF ( andf(two1(Set1),a1)/=0 ) THEN
         n1 = n1 + 1
         IF ( andf(two1(Sub1),a1)==0 ) THEN
            IF ( andf(two1(Sub2),a1)==0 ) THEN
               b1(1) = 2.0
               n3 = n3 + 1
            ELSE
               no = no + 1
               b1(1) = 1.0
            ENDIF
            CALL zblpki
         ELSE
            n = n + 1
         ENDIF
      ENDIF
   ENDDO
 100  CALL bldpkn(Pvact,0,pvect)
   pvect(3) = n1
   CALL wrttrl(pvect)
   CALL close(uset,1)
   CALL close(Pvact,1)
END SUBROUTINE calcv
