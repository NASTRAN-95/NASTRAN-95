
SUBROUTINE calcv(Pvact,Set1,Sub1,Sub2,Core)
   IMPLICIT NONE
   REAL B1(4) , Two1(32)
   INTEGER Lc , N , N1 , N3 , No , Pvect(7) , Sysbuf , Uset
   COMMON /patx  / Lc , N , No , N3 , Uset , Pvect
   COMMON /system/ Sysbuf
   COMMON /two   / Two1
   COMMON /zblpkx/ B1 , N1
   INTEGER Pvact , Set1 , Sub1 , Sub2
   INTEGER Core(1)
   INTEGER a1 , lcore
   INTEGER andf
   REAL flag
   EXTERNAL andf
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
