!*==saxif1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE saxif1(Iopt)
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(9) :: a
   INTEGER :: i , ir , j , jcol , k , n , ncol , nsta
   INTEGER , DIMENSION(100) :: nest
   REAL :: rho
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES MATRICES WHICH RELATE PRESSURE TO VELOCITY
!     IN A FLUID. IOPT DETERMINES THE ELEMENT TYPE
!
!        IOPT     TYPE
!         0      CAXIF2
!         1      CAXIF3
!         2      CAXIF4
!
   !>>>>EQUIVALENCE (Est(1),Nest(1)) , (Am(1),A(1))
!
   DO i = 1 , 44
      sv(i) = 0.0
   ENDDO
   nid = nest(1)
   IF ( Iopt<1 ) THEN
!
!     CAXIF2 ELEMENTS
!
      IF ( nest(6)<1 ) THEN
         coef = est(4)*(est(13)-est(9))
         IF ( coef==0.0 ) RETURN
         sv(3) = 1.0/coef
         sv(4) = -sv(3)
      ELSEIF ( nest(6)<=1 ) THEN
         coef = est(4)*(est(8)+est(12))
         IF ( coef==0.0 ) RETURN
         sv(1) = -1.0/coef
         sv(2) = sv(1)
      ENDIF
      en = float(nest(6))
      rbar = (est(8)+est(12))/2.0
      zbar = (est(9)+est(13))/2.0
      dr = est(12) - est(8)
      dz = est(13) - est(9)
      r1n = est(8)**nest(6)
      r2n = est(12)**nest(6)
      rbn1 = rbar**(nest(6)-1)
      hm(1) = est(13)/(r1n*dz)
      hm(2) = -est(9)/(r2n*dz)
      hm(3) = -1.0/(r1n*dz)
      hm(4) = 1.0/(r2n*dz)
      el = sqrt(dz**2+dr**2)
      coef = rbn1/(est(4)*el)
      am(1) = en*dr*coef
      am(2) = (en*dr*zbar+rbar*dz)*coef
      am(3) = en*el*coef
      am(4) = en*zbar*el*coef
      sv(5) = am(1)*hm(1) + am(2)*hm(3)
      sv(6) = am(1)*hm(2) + am(2)*hm(4)
      sv(7) = am(3)*hm(1) + am(4)*hm(3)
      sv(8) = am(3)*hm(2) + am(4)*hm(4)
      sil(1) = nest(2)
      sil(2) = nest(3)
      RETURN
   ELSEIF ( Iopt==1 ) THEN
!
!     CAXIF3 ELEMENT
!
      n = nest(7)
      en = float(n)
      rho = est(5)
      DO i = 1 , 3
         sil(i) = nest(i+1)
         ir = 4*(i-1) + 9
         r(i) = est(ir)
         z(i) = est(ir+1)
      ENDDO
      i1 = 1
      i2 = 2
      i3 = 3
      rbar = (r(i1)+r(i2)+r(i3))/3.0
      zbar = (z(i1)+z(i2)+z(i3))/3.0
      iret = 4
   ELSE
!
!     CAXIF4 ELEMENT
!
      n = nest(8)
      en = float(n)
      rho = est(6)*4.0
      DO i = 1 , 4
         sil(i) = nest(i+1)
         ir = 4*(i-1) + 10
         r(i) = est(ir)
         z(i) = est(ir+1)
      ENDDO
      rbar = (r(1)+r(2)+r(3)+r(4))/4.0
      zbar = (z(1)+z(2)+z(3)+z(4))/4.0
      i1 = 1
      i2 = 2
      i3 = 3
      iret = 1
   ENDIF
   SPAG_Loop_1_1: DO
!
!     ACTUAL SUBTRIANGLE CALCULATION
!
      IF ( rho==0.0 ) RETURN
      a(1) = 0.0
      a(2) = -1.0/rho
      a(3) = 0.0
      a(5) = a(2)*en
      a(4) = a(5)/rbar
      a(6) = a(4)*zbar
      a(7) = 0.0
      a(8) = 0.0
      a(9) = a(2)
!
      coef = (r(i2)-r(i1))*(z(i3)-z(i1)) - (r(i3)-r(i1))*(z(i2)-z(i1))
      IF ( coef==0.0 ) RETURN
      hm(1) = (r(i2)*z(i3)-r(i3)*z(i2))/coef
      hm(2) = (r(i3)*z(i1)-r(i1)*z(i3))/coef
      hm(3) = (r(i1)*z(i2)-r(i2)*z(i1))/coef
      hm(4) = (z(i2)-z(i3))/coef
      hm(5) = (z(i3)-z(i1))/coef
      hm(6) = (z(i1)-z(i2))/coef
      hm(7) = (r(i3)-r(i2))/coef
      hm(8) = (r(i1)-r(i3))/coef
      hm(9) = (r(i2)-r(i1))/coef
      DO j = 1 , 3
         jcol = i1
         IF ( j==2 ) jcol = i2
         IF ( j==3 ) jcol = i3
         DO i = 1 , 3
            ij = (2+Iopt)*(i-1) + jcol
            DO k = 1 , 3
               ik = 3*(i-1) + k
               kj = 3*(k-1) + j
               sv(ij) = sv(ij) + a(ik)*hm(kj)
            ENDDO
         ENDDO
      ENDDO
      IF ( iret==1 ) THEN
         i3 = 4
         iret = 2
      ELSEIF ( iret==2 ) THEN
         i2 = 3
         iret = 3
      ELSEIF ( iret==3 ) THEN
         i1 = 2
         iret = 4
      ELSE
!
!     THE CENTROID  CALCULATIONS ARE COMPLETE.
!
         nsta = 3*(Iopt+2)
         ncol = Iopt + 2
         IF ( Iopt==2 ) rho = est(6)
         DO i = 1 , ncol
            j = i + 1
            IF ( j>ncol ) j = j - ncol
            el = sqrt((r(j)-r(i))**2+(z(j)-z(i))**2)*rho
!
            ik = nsta + 2*ncol*(i-1) + i
            ij = ik + j - i
            sv(ik) = -1.0/el
            sv(ij) = -sv(ik)
            coef = -en/((r(i)+r(j))*rho)
            ik = ik + ncol
            ij = ij + ncol
            sv(ik) = coef
            sv(ij) = coef
         ENDDO
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE saxif1
