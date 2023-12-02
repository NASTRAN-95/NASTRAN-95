!*==sslot2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sslot2(Iopt,Ipart,Branch,Eigen)
   USE c_condas
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_zzzzzz
   IMPLICIT NONE
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
         velr(i) = 0.0
         veli(i) = 0.0
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
   id1 = elid
   id2 = elid
!
   DO i = 1 , kl
      k = ivec + sil(i) - 1
      IF ( x/=0.0 ) THEN
         DO j = 1 , kl2
            ij = kl*(j-1) + i
!
            velr(j) = sv(ij)*zz(k)*x + velr(j)
         ENDDO
      ENDIF
      IF ( y/=0.0 ) THEN
         DO j = 1 , kl2
            ij = kl*(j-1) + i
            veli(j) = sv(ij)*zz(k)*y + veli(j)
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE sslot2
