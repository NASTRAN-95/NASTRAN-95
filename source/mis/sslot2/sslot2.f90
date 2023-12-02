!*==sslot2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sslot2(Iopt,Ipart,Branch,Eigen)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
   INTEGER :: Ipart
   INTEGER :: Branch
   REAL , DIMENSION(3) :: Eigen
!
! Local variable declarations rewritten by SPAG
!
   REAL :: em , twopi , x , y
   INTEGER :: i , ij , j , k , kl , kl2
!
! End of declarations rewritten by SPAG
!
!
!     THE OPTIONS ARE
!         IOPT   -  CSLOT3 = 0,  CSLOT4 = 1
!         IPART  -  FIRST  = 1,  SECOND = 2
!         BRANCH -  SDR2 PROCESS CODE WORD
!
   !>>>>EQUIVALENCE (Consts(2),Twopi)
!
   kl = Iopt + 3
   kl2 = kl + 2
   IF ( Ipart/=2 ) THEN
      DO i = 1 , 11
         Velr(i) = 0.0
         Veli(i) = 0.0
      ENDDO
   ENDIF
   x = 1.0
   y = 0.0
   IF ( Branch==2 ) x = sqrt(abs(Eigen(2)))
   IF ( Branch==5 ) x = twopi*Eigen(1)
   IF ( x/=0.0 ) x = 1.0/x
   IF ( Branch==9 ) THEN
      em = Eigen(2)**2 + Eigen(3)**2
      IF ( em/=0.0 ) THEN
         x = Eigen(2)/em
         y = -Eigen(3)/em
      ENDIF
   ENDIF
   IF ( Ipart==2 ) THEN
      em = x
      x = -y
      y = em
   ENDIF
   Id1 = Elid
   Id2 = Elid
!
   DO i = 1 , kl
      k = Ivec + Sil(i) - 1
      IF ( x/=0.0 ) THEN
         DO j = 1 , kl2
            ij = kl*(j-1) + i
!
            Velr(j) = Sv(ij)*Zz(k)*x + Velr(j)
         ENDDO
      ENDIF
      IF ( y/=0.0 ) THEN
         DO j = 1 , kl2
            ij = kl*(j-1) + i
            Veli(j) = Sv(ij)*Zz(k)*y + Veli(j)
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE sslot2
