
SUBROUTINE dshear
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A , A2 , A3 , A4 , A5 , Avec(4) , B , B2 , B3 , B4 , B5 , C , C2 , C23 , C3 , C4 , C5 , Cep1 , Cep2 , D , D2 ,  &
                  & D3 , D4 , D5 , Dpterm , E , Ep , F , F13 , F24 , Fxx , G , J3x3(9) , Jj(3) , K3x3(9) , Ke(36) , Nu , Nuc ,      &
                  & P(4) , Pa , Sa , Smallu(4) , Smallv(4) , Sum , T , Temp , Term , Term1 , Term2 , Term3 , Term4 , Term5 , Ti(9) ,&
                  & Ui(3) , V12(3) , V12dk , V41(3) , Vd1(3) , Vd2(3) , Vi(3) , Vj(3) , Vjl , Vk(3) , Vkl , Vkn(3) , Vleft(6) ,     &
                  & Vp12(3) , Vp12l , X1 , X2 , X3 , X4 , Xl , Xl13 , Xl24 , Xp , Xq , Y1 , Y2 , Y3
   REAL Alpha , Avgltp , Costh , Deform , Dumcl(32) , Ecpt(100) , Eltemp , Esp , Fmu , Gp1(3) , Gp2(3) , Gp3(3) , Gp4(3) , Gsp ,    &
      & Gsube , Nusp , Rho , Sigc , Sigs , Sigt , Sinth , Stress , Tempel , Tsp , Tsubo , U1(3) , U2(3) , U3(3) , U4(4) , Zz(1)
   INTEGER Icsid1 , Icsid2 , Icsid3 , Icsid4 , Icstm , Iecpt(100) , Ielid , Isilno(4) , Iz(1) , Matflg , Matid , Matidc , Ncstm ,   &
         & Nogo , Npvt
   DOUBLE PRECISION Y4 , Yp , Z
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm , Dumcl , Nogo
   COMMON /ds1adp/ Ke , Ti , Vleft , Vd1 , Vd2 , Vkn , Vk , V12 , V41 , Vp12 , Vi , Vj , Avec , Smallu , Smallv , P , X1 , X2 , X3 ,&
                 & X4 , Y1 , Y2 , Y3 , Y4 , Vkl , Pa , V12dk , Cep1 , Cep2 , Ep , Temp , Yp , Xp , Sa , Xq , B , Xl , A , A2 , A3 , &
                 & A4 , A5 , B2 , B3 , B4 , B5 , C , C2 , C3 , C4 , C5 , D , D2 , D3 , D4 , D5 , Term1 , Term2 , Term3 , Term4 ,    &
                 & Term5 , Xl13 , Xl24 , Vp12l , Vjl , Z , Term , F , E , G , Nu , T , C23 , Nuc , Ui , Dpterm , Sum , F13 , F24 ,  &
                 & Fxx , Jj , J3x3 , K3x3
   COMMON /ds1aet/ Ielid , Isilno , Matid , Tsp , Fmu , Icsid1 , Gp1 , Icsid2 , Gp2 , Icsid3 , Gp3 , Icsid4 , Gp4 , Tempel ,        &
                 & Deform , Avgltp , U1 , U2 , U3 , U4
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Esp , Gsp , Nusp , Rho , Alpha , Tsubo , Gsube , Sigt , Sigc , Sigs
   COMMON /zzzzzz/ Zz
!
! Local variable declarations
!
   INTEGER i , ii , ivlbeg , j , k , kk , mpoint , npoint
!
! End of declarations
!
!
!     THIS COMPUTES THE THE TWO 6X6 DIFFERENTIAL STIFFNESS MATRICES
!     K(NPVT,NPVT) AND K(NPVT,J) WHERE J = 3,4,1,2 IF NPVT = 1,2,3,4
!     RESPECTIVELY.
!
!     ECPT FOR BOTH PANELS
!
!     ECPT( 1)  -  IELID          ELEMENT ID. NO.
!     ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
!     ECPT( 3)  -   ...                   ...
!     ECPT( 4)  -   ...                   ...
!     ECPT( 5)  -   ...                   ...
!     ECPT( 6)  -  MATID          MATERIAL ID.
!     ECPT( 7)  -  T              THICKNESS
!     ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
!     ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
!     ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
!     ECPT(11)  -   ...                      ...
!     ECPT(12)  -   ...                      ...
!     ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
!     ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
!     ECPT(15)  -   ...                      ...
!     ECPT(16)  -   ...                      ...
!     ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
!     ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
!     ECPT(19)  -   ...                      ...
!     ECPT(20)  -   ...                      ...
!     ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
!     ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
!     ECPT(23)  -   ...                      ...
!     ECPT(24)  -   ...                      ...
!     ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
!     ECPT(26)  -  DEFORM         ELEMENT DEFORMATION (NOT USED)
!     ECPT(27)  -  AVGLTP         AVG.ELEM LOADING TEMPERATURE, NOT USED
!     ECPT(28)  -  U1(3)          TRANSLATION DISPLACEMENTS AT PT. 1
!     ECPT(29)  -  ...                         ...
!     ECPT(30)  -  ...                         ...
!     ECPT(31)  -  U2(3)          TRANSLATION DISPLACEMENTS AT PT. 2
!     ECPT(32)  -  ...                         ...
!     ECPT(33)  -  ...                         ...
!     ECPT(34)  -  U3(3)          TRANSLATION DISPLACEMENTS AT PT. 3
!     ECPT(35)  -  ...                         ...
!     ECPT(36)  -  ...                         ...
!     ECPT(37)  -  U4(3)          TRANSLATION DISPLACEMENTS AT PT. 4
!     ECPT(38)  -  ...                         ...
!     ECPT(39)  -  ...                         ...
!
   EQUIVALENCE (Iz(1),Zz(1)) , (Ielid,Iecpt(1),Ecpt(1))
!
!     CALL MAT TO GET MATERIAL PROPERTIES.
!
   Matidc = Matid
   Matflg = 1
   Eltemp = Tempel
   CALL mat(Iecpt(1))
!
!     STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS
!
   E = Esp
   G = Gsp
   Nu = Nusp
   T = Tsp
   C23 = 2.0D0/3.0D0
   Nuc = 1.0D0/(1.0D0+Nu)
!
!     COMPUTE DIAGONAL VECTORS.
!
   DO i = 1 , 3
      Vd1(i) = Gp3(i) - Gp1(i)
      Vd2(i) = Gp4(i) - Gp2(i)
   ENDDO
!
!     COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE
!     PROJECTED AREA, PA
!
   Vkn(1) = Vd1(2)*Vd2(3) - Vd1(3)*Vd2(2)
   Vkn(2) = Vd1(3)*Vd2(1) - Vd1(1)*Vd2(3)
   Vkn(3) = Vd1(1)*Vd2(2) - Vd1(2)*Vd2(1)
   Vkl = dsqrt(Vkn(1)**2+Vkn(2)**2+Vkn(3)**2)
   IF ( Vkl==0.0D0 ) GOTO 100
   Vk(1) = Vkn(1)/Vkl
   Vk(2) = Vkn(2)/Vkl
   Vk(3) = Vkn(3)/Vkl
   Pa = .5D0*Vkl
!
!     COMPUTE  SIDES -12- AND -41-
!
   DO i = 1 , 3
      V12(i) = Gp2(i) - Gp1(i)
      V41(i) = Gp1(i) - Gp4(i)
   ENDDO
!
!     COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12,VI,VJ
!
   V12dk = V12(1)*Vk(1) + V12(2)*Vk(2) + V12(3)*Vk(3)
   Vp12(1) = V12(1) - V12dk*Vk(1)
   Vp12(2) = V12(2) - V12dk*Vk(2)
   Vp12(3) = V12(3) - V12dk*Vk(3)
   Vp12l = dsqrt(Vp12(1)**2+Vp12(2)**2+Vp12(3)**2)
   IF ( Vp12l==0.0D0 ) GOTO 100
   Vi(1) = Vp12(1)/Vp12l
   Vi(2) = Vp12(2)/Vp12l
   Vi(3) = Vp12(3)/Vp12l
   Vj(1) = Vk(2)*Vi(3) - Vk(3)*Vi(2)
   Vj(2) = Vk(3)*Vi(1) - Vk(1)*Vi(3)
   Vj(3) = Vk(1)*Vi(2) - Vk(2)*Vi(1)
!
!     NORMALIZE J FOR GOOD MEASURE
!
   Vjl = dsqrt(Vj(1)**2+Vj(2)**2+Vj(3)**2)
   IF ( Vjl==0.0D0 ) GOTO 100
   Vj(1) = Vj(1)/Vjl
   Vj(2) = Vj(2)/Vjl
   Vj(3) = Vj(3)/Vjl
   X1 = 0.0D0
   Y1 = 0.0D0
   X2 = Vp12l
   Y2 = 0.0D0
   X3 = Vi(1)*Vd1(1) + Vi(2)*Vd1(2) + Vi(3)*Vd1(3)
   Y3 = Vj(1)*Vd1(1) + Vj(2)*Vd1(2) + Vj(3)*Vd1(3)
   X4 = -Vi(1)*V41(1) - Vi(2)*V41(2) - Vi(3)*V41(3)
   Y4 = -Vj(1)*V41(1) - Vj(2)*V41(2) - Vj(3)*V41(3)
!
!     CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.
!     IF NOT, CALL FATAL ERROR MESSAGE.
!
   IF ( Y3<=0.0D0 ) THEN
!
      Iecpt(2) = 2
      GOTO 200
   ELSEIF ( X3<=Y3*X4/Y4 ) THEN
      Iecpt(2) = 4
      GOTO 200
   ELSEIF ( Y4<=0.0D0 ) THEN
      Iecpt(2) = 1
      GOTO 200
   ELSEIF ( X4>=X2-(X2-X3)*Y4/Y3 ) THEN
      Iecpt(2) = 3
      GOTO 200
   ELSE
!
!     TEST FOR PARALLEL EFFECTS.
!
      Cep1 = dabs((Y3-Y4)/(X3-X4))
      Temp = X3 - X2
      Cep2 = dabs((Y4*Temp-Y3*X4)/(X4*Temp+Y4*Y3))
      Ep = 1.0D-1
      IF ( Cep1<Ep ) THEN
         IF ( Cep2<Ep ) THEN
!
!     IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
!
            DO i = 1 , 4
               P(i) = 1.0D0
            ENDDO
            D = -.5D0*(X4/Y4+(X3-X2)/Y3+(Y3-Y4)/(X3-X4))
            Z = Pa/(2.0D0*G*T)*(1.0D0+2.0D0*D**2*Nuc)
         ELSE
!
!     AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO
!     THE LINE CONNECTING POINTS 1 AND 2.
!
            Temp = Y3*X4 - Y4*(X3-X2)
            Yp = X2*Y3*Y4/Temp
            P(1) = Yp - Y1
            P(2) = Yp - Y2
            P(3) = Yp - Y3
            P(4) = Yp - Y4
            Xp = X2*Y3*X4/Temp
            Sa = (X2-Xp)/Yp
            C = (X1-Xp)/Yp
            Z = ((P(1)*P(2)*Pa)/(P(3)*P(4)*2.0D0*G*T))*(1.0D0+C23*Nuc*(Sa**2+Sa*C+C**2))
         ENDIF
      ELSEIF ( Cep2<Ep ) THEN
!
!     AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO
!     THE LINE CONNECTING POINTS 2 AND 3.
!
         D = -.5D0*(X4/Y4+(X3-X2)/Y3)
         Xq = X4 - Y4*(X3-X4)/(Y3-Y4)
         Temp = 1.0D0/dsqrt(1.0D0+D**2)
         P(1) = (Xq-X1-D*Y1)*Temp
         P(2) = (Xq-X2-D*Y2)*Temp
         P(3) = (Xq-X3-D*Y3)*Temp
         P(4) = (Xq-X4-D*Y4)*Temp
         Temp = Xq - X4
         B = (Temp*D+Y4)/(Temp-Y4*D)
         Z = ((P(1)*P(2)*Pa)/(P(3)*P(4)*2.0D0*G*T))*(1.0D0+C23*Nuc*(B**2+B*D+D**2))
      ELSE
!
!     IN THIS CASE NO PARALLEL EFFECTS EXIST.
!
         Xq = X4 - (X3-X4)/(Y3-Y4)*Y4
         Temp = Y3*X4 - Y4*(X3-X2)
         Xp = X2*Y3*X4/Temp
         Yp = X2*Y3*Y4/Temp
         Xl = dsqrt((Xq-Xp)**2+Yp**2)
         D = (Xq-Xp)/Yp
         Temp = Yp/Xl
         P(1) = Temp*(Xq-X1-D*Y1)
         P(2) = Temp*(Xq-X2-D*Y2)
         P(3) = Temp*(Xq-X3-D*Y3)
         P(4) = Temp*(Xq-X4-D*Y4)
         C = Xl/P(1) - D
         B = Xl/P(4) - C
         A = Xl/P(2) - D
         A2 = A**2
         B2 = B**2
         C2 = C**2
         D2 = D**2
         A3 = A2*A
         B3 = B2*B
         C3 = C2*C
         D3 = D2*D
         A4 = A3*A
         B4 = B3*B
         C4 = C3*C
         D4 = D3*D
         A5 = A4*A
         B5 = B4*B
         C5 = C4*C
         D5 = D4*D
         Temp = .5D0*P(1)*P(2)*P(3)*P(4)/Xl**2
         Term = A + B + C23*(A3+B3) + .2D0*(A5+B5)
         Term1 = C + D + C23*(C3+D3) + .2D0*(C5+D5)
         Term2 = B + C + C23*(B3+C3) + .2D0*(B5+C5)
         Term3 = D + A + C23*(D3+A3) + .2D0*(D5+A5)
         Term = Term*dlog(dabs(A+B))
         Term1 = Term1*dlog(dabs(C+D))
         Term2 = Term2*dlog(dabs(B+C))
         Term3 = Term3*dlog(dabs(D+A))
         Term4 = .1D0*((A2-C2)*(B3-D3)+(B2-D2)*(A3-C3))
         Term5 = .2D0*((A-C)*(B4-D4)+(B-D)*(A4-C4))
         F = Temp*(Term+Term1-Term2-Term3+Term4-Term5)
         Z = P(1)*P(2)/(P(3)*P(4)*2.0D0*G*T)*(Pa+4.0D0*Nuc*(F-C23*Pa))
      ENDIF
      Xl13 = dsqrt(X3**2+Y3**2)
      Xl24 = dsqrt((X4-X2)**2+Y4**2)
      Smallu(1) = X3/Xl13
      Smallu(2) = (X4-X2)/Xl24
      Smallu(3) = Smallu(1)
      Smallu(4) = Smallu(2)
      Smallv(1) = Y3/Xl13
      Smallv(2) = Y4/Xl24
      Smallv(3) = Smallv(1)
      Smallv(4) = Smallv(2)
      Temp = X4*Y3 - X3*Y4
      Avec(1) = -.5D0*X2*Y4*Xl13/Temp
      Avec(2) = .5D0*X2*Y3*Xl24/(Temp-X2*(Y3-Y4))
      Avec(3) = -Avec(1)
      Avec(4) = -Avec(2)
!
!     COMPUTE THE SUM GIVEN ON P. 16 OF FMMS-39
!
      Sum = 0.0D0
      DO i = 1 , 4
         ivlbeg = 1
         Vleft(1) = Smallu(i)*Vi(1) + Smallv(i)*Vj(1)
         Vleft(2) = Smallu(i)*Vi(2) + Smallv(i)*Vj(2)
         Vleft(3) = Smallu(i)*Vi(3) + Smallv(i)*Vj(3)
         IF ( Iecpt(4*i+5)/=0 ) THEN
            CALL transd(Iecpt(4*i+5),Ti)
            ivlbeg = 4
            CALL gmmatd(Vleft(1),3,1,1,Ti,3,3,0,Vleft(4))
         ENDIF
         k = 24 + 3*i
         Ui(1) = Ecpt(k+1)
         Ui(2) = Ecpt(k+2)
         Ui(3) = Ecpt(k+3)
         CALL gmmatd(Vleft(ivlbeg),3,1,1,Ui,3,1,0,Dpterm)
         Sum = Sum + Avec(i)*Dpterm
      ENDDO
      F13 = -Avec(1)*Sum/(2.0D0*Z)
      F24 = Avec(2)*F13/Avec(1)
!
!     SEARCH LIST OF SIL NOS. IN THE ECPT FOR THE PIVOT POINT.
!
      DO i = 1 , 4
         ii = i
         IF ( Npvt==Iecpt(i+1) ) GOTO 50
      ENDDO
      CALL mesage(-30,34,Iecpt(1))
 50   IF ( ii==2 .OR. ii==4 ) THEN
         Fxx = F24/Xl24
         i = 2
      ELSE
         Fxx = F13/Xl13
         i = 1
      ENDIF
      Jj(1) = -Vi(1)*Smallv(i) + Vj(1)*Smallu(i)
      Jj(2) = -Vi(2)*Smallv(i) + Vj(2)*Smallu(i)
      Jj(3) = -Vi(3)*Smallv(i) + Vj(3)*Smallu(i)
!
!                     T            T
!     COMPUTE  JJ X JJ  AND VK X VK
!
      CALL gmmatd(Jj,3,1,0,Jj,3,1,1,J3x3)
      CALL gmmatd(Vk,3,1,0,Vk,3,1,1,K3x3)
!
!     SUM THE TWO IN J3X3
!
      DO j = 1 , 9
         J3x3(j) = J3x3(j) + K3x3(j)
      ENDDO
      IF ( ii==2 ) THEN
         kk = 4
      ELSEIF ( ii==3 ) THEN
         kk = 1
      ELSEIF ( ii==4 ) THEN
         kk = 2
      ELSE
         kk = 3
      ENDIF
!
!     ZERO OUT KE
!
      DO i = 1 , 36
         Ke(i) = 0.0D0
      ENDDO
!
!                 D
!     SET UP THE K   MATRIX
!                 II
!
      mpoint = 1
      IF ( Iecpt(4*ii+5)/=0 ) THEN
         CALL transd(Ecpt(4*ii+5),Ti)
         mpoint = 10
         CALL gmmatd(Ti,3,3,1,J3x3(1),3,3,0,K3x3(1))
         CALL gmmatd(K3x3(1),3,3,0,Ti,3,3,0,J3x3(1))
      ENDIF
      k = 1
      j = ii
   ENDIF
   DO
      Ke(1) = Fxx*J3x3(k)
      Ke(2) = Fxx*J3x3(k+1)
      Ke(3) = Fxx*J3x3(k+2)
      Ke(7) = Fxx*J3x3(k+3)
      Ke(8) = Fxx*J3x3(k+4)
      Ke(9) = Fxx*J3x3(k+5)
      Ke(13) = Fxx*J3x3(k+6)
      Ke(14) = Fxx*J3x3(k+7)
      Ke(15) = Fxx*J3x3(k+8)
      CALL ds1b(Ke,Iecpt(j+1))
      IF ( j==kk ) RETURN
!
!                 D
!     SET UP THE K   MATRIX
!                 IJ
!
      j = kk
      IF ( Iecpt(4*j+5)==0 ) THEN
         k = mpoint
      ELSE
         CALL transd(Ecpt(4*j+5),Ti)
         npoint = 10
         IF ( mpoint==10 ) npoint = 1
         CALL gmmatd(J3x3(mpoint),3,3,0,Ti,3,3,0,J3x3(npoint))
         k = npoint
      ENDIF
      Fxx = -Fxx
   ENDDO
!
!     ERROR RETURNS
!
 100  CALL mesage(30,26,Iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   Nogo = 1
   RETURN
 200  CALL mesage(30,27,Iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   Nogo = 1
END SUBROUTINE dshear
