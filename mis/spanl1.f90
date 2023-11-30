
SUBROUTINE spanl1(Iarg)
   IMPLICIT NONE
   REAL A , A2 , A3 , A4 , A5 , Alpha , Avec(4) , B , B2 , B3 , B4 , B5 , C , C2 , C3 , C4 , C5 , Cep1 , Cep2 , Costh , D , D2 ,    &
      & D3 , D4 , D5 , E , Ecpt(100) , Eltemp , Ep , Fmu , G , Gp1(3) , Gp2(3) , Gp3(3) , Gp4(3) , Gsube , Nu , Out(15) , P(4) ,    &
      & Pa , Rho , S(3,4) , Sa , Sigc , Sigs , Sigt , Sinth , Smallu(4) , Smallv(4) , Spcon , Stress , T , Temp , Tempel , Term1 ,  &
      & Term2 , Term3 , Term4 , Term5 , Ti(9) , Tsubo , V12(3) , V12dk , V41(3) , Vd1(3) , Vd2(3) , Vi(3) , Vj(3) , Vk(3) , Vkl ,   &
      & Vkn(3) , Vleft(6) , Vp12(3) , X1 , X2 , X3 , X4 , Xl , Xl13 , Xl24
   INTEGER Icsid1 , Icsid2 , Icsid3 , Icsid4 , Iecpt(100) , Ielid , Isilno(4) , Jelid , Jsilno(4) , Matflg , Matid , Matidc
   REAL Xp , Xq , Xxxxxx(75) , Y1 , Y2 , Y3 , Y4 , Yp , Yyyyyy(93)
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsubo , Gsube , Sigt , Sigc , Sigs
   COMMON /sdr2x5/ Ielid , Isilno , Matid , T , Fmu , Icsid1 , Gp1 , Icsid2 , Gp2 , Icsid3 , Gp3 , Icsid4 , Gp4 , Tempel , Xxxxxx , &
                 & Jelid , Jsilno , S , Out , Yyyyyy
   COMMON /sdr2x6/ Vleft , Ti , Spcon , Vd1 , Vd2 , Vkn , Vk , V12 , V41 , Vp12 , Vi , Vj , Avec , Smallu , Smallv , P , X1 , X2 ,  &
                 & X3 , X4 , Y1 , Y2 , Y3 , Y4 , Vkl , Pa , V12dk , Cep1 , Cep2 , Ep , Temp , Yp , Xp , Sa , Xq , B , Xl , A , A2 , &
                 & A3 , A4 , A5 , B2 , B3 , B4 , B5 , C , C2 , C3 , C4 , C5 , D , D2 , D3 , D4 , D5 , Term1 , Term2 , Term3 ,       &
                 & Term4 , Term5 , Xl13 , Xl24
   INTEGER Iarg
   REAL f , term , vjl , vp12l , z
   INTEGER i , ivlbeg
!*****
! THIS ROUTINE COMPUTES PHASE I PARAMETERS FOR STRESS DATA RECOVERY FOR
! THE SHEAR PANEL (IF IARG = 4) AND THE TWIST PANEL (IF IARG = 5).
! MUCH OF THE CODE WAS LIFTED FROM SUBROUTIVE KPANEL
!*****
!
!                 E C P T  F O R  B O T H  P A N E L S
! ECPT( 1)  -  IELID          ELEMENT ID. NO.
! ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
! ECPT( 3)  -   ...                   ...
! ECPT( 4)  -   ...                   ...
! ECPT( 5)  -   ...                   ...
! ECPT( 6)  -  MATID          MATERIAL ID.
! ECPT( 7)  -  T              THICKNESS
! ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
! ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
! ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
! ECPT(11)  -   ...                      ...
! ECPT(12)  -   ...                      ...
! ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
! ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
! ECPT(15)  -   ...                      ...
! ECPT(16)  -   ...                      ...
! ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
! ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
! ECPT(19)  -   ...                      ...
! ECPT(20)  -   ...                      ...
! ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
! ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
! ECPT(23)  -   ...                      ...
! ECPT(24)  -   ...                      ...
! ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
!
!
!
!
!
!
!
!
!
!
! SDR2 PHASE I INPUT AND OUTPUT BLOCK
!
!
! SDR2 SCRATCH BLOCK
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   EQUIVALENCE (Ielid,Iecpt(1),Ecpt(1))
!
! CALL MAT TO GET MATERIAL PROPERTIES.
!
   Matidc = Matid
   Matflg = 1
   Eltemp = Tempel
   CALL mat(Iecpt(1))
!
! COMPUTE DIAGONAL VECTORS.
!
   DO i = 1 , 3
      Vd1(i) = Gp3(i) - Gp1(i)
      Vd2(i) = Gp4(i) - Gp2(i)
   ENDDO
!
! COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE PROJECTED
! AREA, PA
!
   Vkn(1) = Vd1(2)*Vd2(3) - Vd1(3)*Vd2(2)
   Vkn(2) = Vd1(3)*Vd2(1) - Vd1(1)*Vd2(3)
   Vkn(3) = Vd1(1)*Vd2(2) - Vd1(2)*Vd2(1)
   Vkl = sqrt(Vkn(1)**2+Vkn(2)**2+Vkn(3)**2)
   IF ( Vkl==0.0 ) THEN
      CALL mesage(-30,26,Iecpt(1))
      Iecpt(2) = 2
      CALL mesage(-30,27,Iecpt(1))
   ELSE
      Vk(1) = Vkn(1)/Vkl
      Vk(2) = Vkn(2)/Vkl
      Vk(3) = Vkn(3)/Vkl
      Pa = .5*Vkl
!
! COMPUTE  SIDES -12- AND -41-
!
      DO i = 1 , 3
         V12(i) = Gp2(i) - Gp1(i)
         V41(i) = Gp1(i) - Gp4(i)
      ENDDO
!
! COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI, VJ
!
      V12dk = V12(1)*Vk(1) + V12(2)*Vk(2) + V12(3)*Vk(3)
      Vp12(1) = V12(1) - V12dk*Vk(1)
      Vp12(2) = V12(2) - V12dk*Vk(2)
      Vp12(3) = V12(3) - V12dk*Vk(3)
      vp12l = sqrt(Vp12(1)**2+Vp12(2)**2+Vp12(3)**2)
      IF ( vp12l==0.0 ) THEN
         CALL mesage(-30,26,Iecpt(1))
         Iecpt(2) = 2
         CALL mesage(-30,27,Iecpt(1))
      ELSE
         Vi(1) = Vp12(1)/vp12l
         Vi(2) = Vp12(2)/vp12l
         Vi(3) = Vp12(3)/vp12l
         Vj(1) = Vk(2)*Vi(3) - Vk(3)*Vi(2)
         Vj(2) = Vk(3)*Vi(1) - Vk(1)*Vi(3)
         Vj(3) = Vk(1)*Vi(2) - Vk(2)*Vi(1)
!
! NORMALIZE J FOR GOOD MEASURE
!
         vjl = sqrt(Vj(1)**2+Vj(2)**2+Vj(3)**2)
         IF ( vjl==0.0 ) THEN
            CALL mesage(-30,26,Iecpt(1))
            Iecpt(2) = 2
            CALL mesage(-30,27,Iecpt(1))
         ELSE
            Vj(1) = Vj(1)/vjl
            Vj(2) = Vj(2)/vjl
            Vj(3) = Vj(3)/vjl
            X1 = 0.0
            Y1 = 0.0
            X2 = vp12l
            Y2 = 0.0
            X3 = Vi(1)*Vd1(1) + Vi(2)*Vd1(2) + Vi(3)*Vd1(3)
            Y3 = Vj(1)*Vd1(1) + Vj(2)*Vd1(2) + Vj(3)*Vd1(3)
            X4 = -Vi(1)*V41(1) - Vi(2)*V41(2) - Vi(3)*V41(3)
            Y4 = -Vj(1)*V41(1) - Vj(2)*V41(2) - Vj(3)*V41(3)
!
! CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.  IF NOT,
! CALL FATAL ERROR MESSAGE.
!
            IF ( Y3<=0.0 ) THEN
               Iecpt(2) = 2
               CALL mesage(-30,27,Iecpt(1))
            ELSEIF ( X3<=Y3*X4/Y4 ) THEN
               Iecpt(2) = 4
               CALL mesage(-30,27,Iecpt(1))
            ELSEIF ( Y4<=0.0 ) THEN
               Iecpt(2) = 1
               CALL mesage(-30,27,Iecpt(1))
            ELSEIF ( X4>=X2-(X2-X3)*Y4/Y3 ) THEN
               Iecpt(2) = 3
               CALL mesage(-30,27,Iecpt(1))
            ELSE
!
! TEST FOR PARALLEL EFFECTS.
!
               Temp = X3 - X2
               Ep = 0.01
               IF ( abs(Y3-Y4)<abs(X3-X4)*Ep ) THEN
                  IF ( abs(Y4*Temp-Y3*X4)<abs(X4*Temp+Y4*Y3)*Ep ) THEN
!
! IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
!
                     DO i = 1 , 4
                        P(i) = 1.0
                     ENDDO
                     D = -.5*(X4/Y4+(X3-X2)/Y3+(Y3-Y4)/(X3-X4))
                     z = Pa/(2.0*G*T)*(1.0+2.0*D**2/(1.0+Nu))
                  ELSE
!
! AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO THE
! LINE CONNECTING POINTS 1 AND 2.
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
                     z = ((P(1)*P(2)*Pa)/(P(3)*P(4)*2.0*G*T))*(1.0+2.0/(3.0+3.0*Nu)*(Sa**2+Sa*C+C**2))
                  ENDIF
               ELSEIF ( abs(Y4*Temp-Y3*X4)<abs(X4*Temp+Y4*Y3)*Ep ) THEN
!
! AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO THE
! LINE CONNECTING POINTS 2 AND 3.
!
                  D = -.5*(X4/Y4+(X3-X2)/Y3)
                  Xq = X4 - Y4*(X3-X4)/(Y3-Y4)
                  Temp = 1.0/sqrt(1.0+D**2)
                  P(1) = (Xq-X1-D*Y1)*Temp
                  P(2) = (Xq-X2-D*Y2)*Temp
                  P(3) = (Xq-X3-D*Y3)*Temp
                  P(4) = (Xq-X4-D*Y4)*Temp
                  Temp = Xq - X4
                  B = (Temp*D+Y4)/(Temp-Y4*D)
                  z = ((P(1)*P(2)*Pa)/(P(3)*P(4)*2.0*G*T))*(1.0+2.0/(3.0+3.0*Nu)*(B**2+B*D+D**2))
               ELSE
!
! IN THIS CASE NO PARALLEL EFFECTS EXIST.
!
                  Xq = X4 - (X3-X4)/(Y3-Y4)*Y4
                  Temp = Y3*X4 - Y4*(X3-X2)
                  Xp = X2*Y3*X4/Temp
                  Yp = X2*Y3*Y4/Temp
                  Xl = sqrt((Xq-Xp)**2+Yp**2)
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
                  Temp = .5*P(1)*P(2)*P(3)*P(4)/Xl**2
                  term = A + B + 2.0*(A3+B3)/3.0 + .2*(A5+B5)
                  Term1 = C + D + 2.0*(C3+D3)/3.0 + .2*(C5+D5)
                  Term2 = B + C + 2.0*(B3+C3)/3.0 + .2*(B5+C5)
                  Term3 = D + A + 2.0*(D3+A3)/3.0 + .2*(D5+A5)
                  term = term*alog(abs(A+B))
                  Term1 = Term1*alog(abs(C+D))
                  Term2 = Term2*alog(abs(B+C))
                  Term3 = Term3*alog(abs(D+A))
                  Term4 = .1*((A2-C2)*(B3-D3)+(B2-D2)*(A3-C3))
                  Term5 = .2*((A-C)*(B4-D4)+(B-D)*(A4-C4))
                  f = Temp*(term+Term1-Term2-Term3+Term4-Term5)
                  z = P(1)*P(2)/(P(3)*P(4)*2.0*G*T)*(Pa+4.0/(1.0+Nu)*(f-2.0*Pa/3.0))
               ENDIF
               Xl13 = sqrt(X3**2+Y3**2)
               Xl24 = sqrt((X4-X2)**2+Y4**2)
               Smallu(1) = X3/Xl13
               Smallu(2) = (X4-X2)/Xl24
               Smallu(3) = Smallu(1)
               Smallu(4) = Smallu(2)
               Smallv(1) = Y3/Xl13
               Smallv(2) = Y4/Xl24
               Smallv(3) = Smallv(1)
               Smallv(4) = Smallv(2)
               Temp = X4*Y3 - X3*Y4
               Avec(1) = -.5*X2*Y4*Xl13/Temp
               Avec(2) = .5*X2*Y3*Xl24/(Temp-X2*(Y3-Y4))
               Avec(3) = -Avec(1)
               Avec(4) = -Avec(2)
!
! IF IARG = 4, WE HAVE A SHEAR PANEL, AND IF IARG = 5, A TWIST PANEL.
!
               IF ( Iarg/=4 ) THEN
!
! SINCE WE ARE DEALING WITH A TWIST PANEL STORE -SMALLV IN SMALLU AND
! SMALLU IN SMALLV.
!
                  DO i = 1 , 4
                     Temp = Smallu(i)
                     Smallu(i) = -Smallv(i)
                     Smallv(i) = Temp
                  ENDDO
               ENDIF
!
! COMPUTE THE SINGLE PRECISION CONSTANT SPCON
!
               IF ( Iarg==5 ) THEN
                  Spcon = -1.0/(4.0*z)
               ELSE
                  Spcon = -1.0/(2.0*z*T)
               ENDIF
!
! COMPUTE THE FOUR 1 X 3 MATRICES S
!
               DO i = 1 , 4
                  ivlbeg = 1
                  Vleft(1) = Smallu(i)*Vi(1) + Smallv(i)*Vj(1)
                  Vleft(2) = Smallu(i)*Vi(2) + Smallv(i)*Vj(2)
                  Vleft(3) = Smallu(i)*Vi(3) + Smallv(i)*Vj(3)
                  IF ( Iecpt(4*i+5)/=0 ) THEN
                     ivlbeg = 4
                     CALL transs(Iecpt(4*i+5),Ti)
                     CALL gmmats(Vleft(1),3,1,1,Ti,3,3,0,Vleft(4))
                  ENDIF
                  S(1,i) = Spcon*Vleft(ivlbeg)*Avec(i)
                  S(2,i) = Spcon*Vleft(ivlbeg+1)*Avec(i)
                  S(3,i) = Spcon*Vleft(ivlbeg+2)*Avec(i)
               ENDDO
               Out(1) = Avec(1)
               Out(2) = Avec(2)
               Out(3) = T
               Out(4) = P(2)/P(1)
               Out(5) = P(1)*P(2)/P(3)**2
               Out(6) = P(1)*P(2)/P(4)**2
               Out(7) = Sigs
               Jelid = Ielid
               DO i = 1 , 4
                  Jsilno(i) = Isilno(i)
               ENDDO
               IF ( Iarg/=4 ) RETURN
!*****
!  ADDITIONAL PHASE-1 OUTPUTS FOR SHEAR PANEL FORCES  IN PHASE 2
!*****
               Out(8) = P(1)/P(3)*T
               Out(9) = (P(1)*P(2))/(P(3)*P(4))*T
               Out(10) = P(2)/P(4)*T
               Out(11) = -V12dk/2.0
               Out(12) = X2/2.0
               Out(13) = sqrt((X3-X2)**2+Y3**2)/2.0
               Out(14) = sqrt((X4-X3)**2+(Y4-Y3)**2)/2.0
               Out(15) = sqrt(X4**2+Y4**2)/2.0
               RETURN
            ENDIF
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE spanl1
