
SUBROUTINE saxif1(Iopt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(9) , Am(9) , Coef , Dr , Dz , El , En , Est(100) , Hm(9) , R(4) , R1n , R2n , Rbar , Rbn1 , Sv(95) , Z(4) , Zbar
   INTEGER I1 , I2 , I3 , Ij , Ik , Iret , Kj , Nest(100) , Nid , Sil(4)
   COMMON /sdr2x5/ Est , Nid , Sil , Sv
   COMMON /sdr2x6/ Hm , R , Z , Am , Coef , En , El , Rbar , Zbar , R1n , R2n , Rbn1 , Dr , Dz , I1 , I2 , I3 , Iret , Ij , Ik , Kj
!
! Dummy argument declarations
!
   INTEGER Iopt
!
! Local variable declarations
!
   INTEGER i , ir , j , jcol , k , n , ncol , nsta
   REAL rho
!
! End of declarations
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
   EQUIVALENCE (Est(1),Nest(1)) , (Am(1),A(1))
!
   DO i = 1 , 44
      Sv(i) = 0.0
   ENDDO
   Nid = Nest(1)
   IF ( Iopt<1 ) THEN
!
!     CAXIF2 ELEMENTS
!
      IF ( Nest(6)<1 ) THEN
         Coef = Est(4)*(Est(13)-Est(9))
         IF ( Coef==0.0 ) RETURN
         Sv(3) = 1.0/Coef
         Sv(4) = -Sv(3)
      ELSEIF ( Nest(6)<=1 ) THEN
         Coef = Est(4)*(Est(8)+Est(12))
         IF ( Coef==0.0 ) RETURN
         Sv(1) = -1.0/Coef
         Sv(2) = Sv(1)
      ENDIF
      En = float(Nest(6))
      Rbar = (Est(8)+Est(12))/2.0
      Zbar = (Est(9)+Est(13))/2.0
      Dr = Est(12) - Est(8)
      Dz = Est(13) - Est(9)
      R1n = Est(8)**Nest(6)
      R2n = Est(12)**Nest(6)
      Rbn1 = Rbar**(Nest(6)-1)
      Hm(1) = Est(13)/(R1n*Dz)
      Hm(2) = -Est(9)/(R2n*Dz)
      Hm(3) = -1.0/(R1n*Dz)
      Hm(4) = 1.0/(R2n*Dz)
      El = sqrt(Dz**2+Dr**2)
      Coef = Rbn1/(Est(4)*El)
      Am(1) = En*Dr*Coef
      Am(2) = (En*Dr*Zbar+Rbar*Dz)*Coef
      Am(3) = En*El*Coef
      Am(4) = En*Zbar*El*Coef
      Sv(5) = Am(1)*Hm(1) + Am(2)*Hm(3)
      Sv(6) = Am(1)*Hm(2) + Am(2)*Hm(4)
      Sv(7) = Am(3)*Hm(1) + Am(4)*Hm(3)
      Sv(8) = Am(3)*Hm(2) + Am(4)*Hm(4)
      Sil(1) = Nest(2)
      Sil(2) = Nest(3)
      RETURN
   ELSEIF ( Iopt==1 ) THEN
!
!     CAXIF3 ELEMENT
!
      n = Nest(7)
      En = float(n)
      rho = Est(5)
      DO i = 1 , 3
         Sil(i) = Nest(i+1)
         ir = 4*(i-1) + 9
         R(i) = Est(ir)
         Z(i) = Est(ir+1)
      ENDDO
      I1 = 1
      I2 = 2
      I3 = 3
      Rbar = (R(I1)+R(I2)+R(I3))/3.0
      Zbar = (Z(I1)+Z(I2)+Z(I3))/3.0
      Iret = 4
   ELSE
!
!     CAXIF4 ELEMENT
!
      n = Nest(8)
      En = float(n)
      rho = Est(6)*4.0
      DO i = 1 , 4
         Sil(i) = Nest(i+1)
         ir = 4*(i-1) + 10
         R(i) = Est(ir)
         Z(i) = Est(ir+1)
      ENDDO
      Rbar = (R(1)+R(2)+R(3)+R(4))/4.0
      Zbar = (Z(1)+Z(2)+Z(3)+Z(4))/4.0
      I1 = 1
      I2 = 2
      I3 = 3
      Iret = 1
   ENDIF
   DO
!
!     ACTUAL SUBTRIANGLE CALCULATION
!
      IF ( rho==0.0 ) RETURN
      A(1) = 0.0
      A(2) = -1.0/rho
      A(3) = 0.0
      A(5) = A(2)*En
      A(4) = A(5)/Rbar
      A(6) = A(4)*Zbar
      A(7) = 0.0
      A(8) = 0.0
      A(9) = A(2)
!
      Coef = (R(I2)-R(I1))*(Z(I3)-Z(I1)) - (R(I3)-R(I1))*(Z(I2)-Z(I1))
      IF ( Coef==0.0 ) RETURN
      Hm(1) = (R(I2)*Z(I3)-R(I3)*Z(I2))/Coef
      Hm(2) = (R(I3)*Z(I1)-R(I1)*Z(I3))/Coef
      Hm(3) = (R(I1)*Z(I2)-R(I2)*Z(I1))/Coef
      Hm(4) = (Z(I2)-Z(I3))/Coef
      Hm(5) = (Z(I3)-Z(I1))/Coef
      Hm(6) = (Z(I1)-Z(I2))/Coef
      Hm(7) = (R(I3)-R(I2))/Coef
      Hm(8) = (R(I1)-R(I3))/Coef
      Hm(9) = (R(I2)-R(I1))/Coef
      DO j = 1 , 3
         jcol = I1
         IF ( j==2 ) jcol = I2
         IF ( j==3 ) jcol = I3
         DO i = 1 , 3
            Ij = (2+Iopt)*(i-1) + jcol
            DO k = 1 , 3
               Ik = 3*(i-1) + k
               Kj = 3*(k-1) + j
               Sv(Ij) = Sv(Ij) + A(Ik)*Hm(Kj)
            ENDDO
         ENDDO
      ENDDO
      IF ( Iret==1 ) THEN
         I3 = 4
         Iret = 2
      ELSEIF ( Iret==2 ) THEN
         I2 = 3
         Iret = 3
      ELSEIF ( Iret==3 ) THEN
         I1 = 2
         Iret = 4
      ELSE
!
!     THE CENTROID  CALCULATIONS ARE COMPLETE.
!
         nsta = 3*(Iopt+2)
         ncol = Iopt + 2
         IF ( Iopt==2 ) rho = Est(6)
         DO i = 1 , ncol
            j = i + 1
            IF ( j>ncol ) j = j - ncol
            El = sqrt((R(j)-R(i))**2+(Z(j)-Z(i))**2)*rho
!
            Ik = nsta + 2*ncol*(i-1) + i
            Ij = Ik + j - i
            Sv(Ik) = -1.0/El
            Sv(Ij) = -Sv(Ik)
            Coef = -En/((R(i)+R(j))*rho)
            Ik = Ik + ncol
            Ij = Ij + ncol
            Sv(Ik) = Coef
            Sv(Ij) = Coef
         ENDDO
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE saxif1
