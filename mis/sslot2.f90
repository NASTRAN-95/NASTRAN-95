
SUBROUTINE sslot2(Iopt,Ipart,Branch,Eigen)
   IMPLICIT NONE
   REAL Consts(5) , Dumy(35) , Sv(95) , Twopi , Veli(11) , Velr(11) , Zz(1)
   INTEGER Elid , Id1 , Id2 , Ivec , Sil(4)
   COMMON /condas/ Consts
   COMMON /sdr2x4/ Dumy , Ivec
   COMMON /sdr2x7/ Elid , Sil , Sv , Id1 , Velr , Id2 , Veli
   COMMON /zzzzzz/ Zz
   INTEGER Branch , Iopt , Ipart
   REAL Eigen(3)
   REAL em , x , y
   INTEGER i , ij , j , k , kl , kl2
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
   IF ( Branch==5 ) x = Twopi*Eigen(1)
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