!*==frmax.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frmax(Ifk,Ifm,N,Ipr,Rsn,Rsm)
USE C_UNPAKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ifk
   INTEGER :: Ifm
   INTEGER :: N
   INTEGER :: Ipr
   REAL(REAL64) :: Rsn
   REAL(REAL64) :: Rsm
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dzk , dzm
   INTEGER :: i
   REAL(REAL64) :: ratinv , ratio
   REAL , DIMENSION(1) :: zk , zm
   EXTERNAL unpack
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (dzk(1),zk(1)) , (dzm(1),zm(1))
   Iprc = Ipr
   Incr = 1
   Rsn = 0.D0
   Rsm = 0.D0
   DO i = 1 , N
      Ip = i
      Np = i
      CALL unpack(*100,Ifk,dzk(1))
      CALL unpack(*100,Ifm,dzm(1))
      IF ( Ipr==2 ) THEN
         IF ( dzk(1)==0.0D0 .OR. dzm(1)==0.0D0 ) CYCLE
         ratio = dzk(1)/dzm(1)
      ELSE
         IF ( zk(1)==0 .OR. zm(1)==0 ) CYCLE
         ratio = zk(1)/zm(1)
      ENDIF
      ratinv = 1.D0/ratio
      IF ( ratio>Rsm ) Rsm = ratio
      IF ( ratinv>Rsn ) Rsn = ratinv
 100  ENDDO
   Rsn = 1.D0/Rsn
END SUBROUTINE frmax
