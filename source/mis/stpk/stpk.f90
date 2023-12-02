!*==stpk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpk(Ek,N,Nstop,Nopen,Nsted,Tsr,Pm,Cr,Ci,Im,J1)
   IMPLICIT NONE
   USE C_STRIPC
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Ek
   INTEGER :: N
   INTEGER :: Nstop
   INTEGER :: Nopen
   INTEGER :: Nsted
   REAL :: Tsr
   REAL , DIMENSION(1) :: Pm
   REAL :: Cr
   REAL :: Ci
   INTEGER :: Im
   INTEGER :: J1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a1 , a2 , beoek , bj0 , bj1 , by0 , by1 , denom , e1k , fcr , tst
   COMPLEX :: cmp0 , r , t , unit , v1 , v2 , w , w2
   INTEGER :: i , j , nn
   INTEGER , SAVE :: nhek , nhsize , nhti , nhtr
   REAL , DIMENSION(37) :: p
   EXTERNAL bug , stpbs0 , stpbs1
!
! End of declarations rewritten by SPAG
!
!     COMPUTES K MATRIX FOR STRIP NUMBER N
!     EK= LOCAL REDUCED FREQUENCY
!     NSTOP  =2 FOR NO CONTROL SURFACE
!     NOPEN  =1 FOR OPEN GAP
!     TSR = GAP/SEMICHORD RATIO  (FOR CLOSED STAGE ONLY)
!     NSTED =1 FOR STEADY CASE
   DATA nhek , nhtr , nhti , nhsize/4HEK   , 4HTR   , 4HTI   , 4HSIZE/
   unit = cmplx(1.0,0.0)
   cmp0 = cmplx(0.0,0.0)
   t = 2.0*unit
   DO i = 1 , 37
      p(i) = Pm(i)
   ENDDO
   DO i = 1 , 4
      DO j = 1 , 4
         Ekm(i,j) = cmp0
      ENDDO
   ENDDO
   a1 = 0.318310
   a2 = 0.101321
   IF ( Nsted==1 ) THEN
!     STEADY CASE
      e1k = 1.E20
      Ekm(1,2) = 2.0
      IF ( Nstop==2 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      Ekm(1,3) = a1*2.0*p(1)
      Ekm(2,3) = a1*p(5)
      Ekm(3,2) = a1*2.0*p(31)
      Ekm(3,3) = a2*(2.0*p(1)*p(31)+p(35))
      Ekm(4,2) = a1*p(8)
      Ekm(4,3) = a2*(p(1)*p(8)+p(10))
      IF ( Nopen==1 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
!     CLOSED STAGE
      Ekm(1,4) = a1*2.0*p(13)
      Ekm(2,4) = a1*p(15)
      tst = amax1(0.01,Tsr)
      Ekm(3,4) = a2*(2.0*p(13)*p(31)+2.0*alog(tst)+p(21))
      Ekm(4,4) = a2*(p(13)*p(8)+p(18))
   ENDIF
   IF ( Nsted/=1 ) THEN
!
!     UNSTEADY CASE, EM(1,1)=(K SUB A)/EK**2, ETC.
      e1k = 1./Ek
      t = cmp0
      v1 = cmp0
      v2 = cmp0
      IF ( Ek>1000.0 ) THEN
         Cr = .5
         Ci = 0.
         w = cmplx(0.0,Ek)
      ELSE
         IF ( Ncirc>0 ) THEN
!  NEXT 8 STATEMENTS ARE FOR GENERATION OF WAGNER FUNCTIONS
            Cr = Bb(1)
            Ci = 0.0
            DO nn = 2 , Nncirc
               beoek = Beta(nn)/Ek
               fcr = Bb(nn)/(1.0+beoek*beoek)
               Cr = Cr + fcr
               Ci = Ci + fcr*beoek
            ENDDO
         ELSE
            CALL stpbs0(Ek,1,bj0,by0)
            CALL stpbs1(Ek,1,bj1,by1)
            denom = (bj1+by0)**2 + (by1-bj0)**2
            Cr = (bj1*(bj1+by0)+by1*(by1-bj0))/denom
!     (CR + I*CI  = THEODORSEN FUNCTION)
            Ci = -(by1*by0+bj1*bj0)/denom
         ENDIF
         t = 2.0*cmplx(Cr,Ci) - unit
         w = cmplx(0.0,Ek)
         v1 = unit/w
         v2 = v1*v1
      ENDIF
      r = t + unit
      w2 = -w*w
      Ekm(1,1) = -(r*v1+1.)
      Ekm(1,1) = Ekm(1,1)*w2
      Ekm(1,2) = -(r*(v2+v1)+v1+0.5)
      Ekm(1,2) = Ekm(1,2)*w2
      Ekm(2,1) = -(0.5)
      Ekm(2,1) = Ekm(2,1)*w2
      Ekm(2,2) = -(v1+0.375)
      Ekm(2,2) = Ekm(2,2)*w2
      IF ( Nstop/=2 ) THEN
         Ekm(1,3) = -a1*(r*(v2*p(1)+0.5*v1*p(2))+v1*p(3)+0.5*p(4))
         Ekm(1,3) = Ekm(1,3)*w2
         Ekm(2,3) = -a1*(v2*p(5)+0.5*v1*p(6)+0.25*p(7))
         Ekm(2,3) = Ekm(2,3)*w2
         Ekm(3,1) = -a1*(r*v1*p(31)+p(3))
         Ekm(3,1) = Ekm(3,1)*w2
         Ekm(3,2) = -a1*(r*(v2+v1)*p(31)+v1*p(32)+0.25*p(6))
         Ekm(3,2) = Ekm(3,2)*w2
         Ekm(3,3) = -a2*(r*(v2*p(1)+0.5*v1*p(2))*p(31)+v2*p(35)+v1*p(36)+0.5*p(37))
         Ekm(3,3) = Ekm(3,3)*w2
         Ekm(4,1) = -a1*0.5*(r*v1*p(8)+p(4))
         Ekm(4,1) = Ekm(4,1)*w2
         Ekm(4,2) = -a1*0.5*(r*(v2+v1)*p(8)+v1*p(9)+0.5*p(7))
         Ekm(4,2) = Ekm(4,2)*w2
         Ekm(4,3) = -a2*(r*(v2*p(1)+0.5*v1*p(2))*0.5*p(8)+v2*p(10)+0.5*v1*p(11)+0.25*p(12))
         Ekm(4,3) = Ekm(4,3)*w2
         IF ( Nopen/=1 ) THEN
!     CLOSED STAGE
            Ekm(1,4) = -a1*(r*(v2*p(13)+v1*p(1))+v1*p(14)+p(3))
            Ekm(1,4) = Ekm(1,4)*w2
            Ekm(2,4) = -a1*(v2*p(15)+2.0*v1*p(5)+0.25*p(6))
            Ekm(2,4) = Ekm(2,4)*w2
            tst = amax1(0.01,Tsr)
            Ekm(3,4) = -a2*(r*(v2*p(13)+v1*p(1))*p(31)+v2*(2.0*alog(tst)+p(21))+v1*p(16)+p(17))
            Ekm(3,4) = Ekm(3,4)*w2
            Ekm(4,4) = -a2*(r*(v2*p(13)+v1*p(1))*0.5*p(8)+v2*p(18)+v1*p(19)+0.5*p(37))
            Ekm(4,4) = Ekm(4,4)*w2
         ELSE
!     OPEN STAGE
            Ekm(1,4) = -a1*(r*v1*p(1)+p(3))
            Ekm(1,4) = Ekm(1,4)*w2
            Ekm(2,4) = -a1*(v1*p(5)+0.25*p(6))
            Ekm(2,4) = Ekm(2,4)*w2
            Ekm(3,4) = -a2*(r*v1*p(1)*p(31)+v1*p(35)+p(17))
            Ekm(3,4) = Ekm(3,4)*w2
            Ekm(4,4) = -a2*(r*0.5*v1*p(1)*p(8)+v1*p(10)+0.5*p(37))
            Ekm(4,4) = Ekm(4,4)*w2
         ENDIF
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL bug(nhek,100,Ek,1)
      CALL bug(nhtr,100,Cr,1)
      CALL bug(nhti,100,Ci,1)
      CALL bug(nhsize,100,N,1)
   END SUBROUTINE spag_block_1
END SUBROUTINE stpk
