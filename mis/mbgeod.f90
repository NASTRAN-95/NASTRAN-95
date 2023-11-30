
SUBROUTINE mbgeod
   IMPLICIT NONE
   REAL Ang(10) , Beta , Boxa , Boxl , Boxw , Cotang(10) , Cr , Ek , Ekbar , Ekm , Gc , Tang(10) , X(12) , Y(12)
   LOGICAL Asym , Cntrl1 , Cntrl2 , Crank1 , Crank2
   INTEGER Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , Mach , Nbox , Ncb , Njj , Npts0 , Npts1 , Npts2 , Nsb , Nsbd , Ntote
   COMMON /mboxa / X , Y , Tang , Ang , Cotang
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   REAL area1 , area2 , areaw , big , tm , xcent , ycent
   INTEGER i
!
!     SUBROUTINE TO COMPUTE GEOMETRY AND INDEXES OF REGIONS
!
!
!     MAIN GEOMETRY
!
   big = -1.0E35
   DO i = 1 , 10
      Tang(i) = 0.0
      Ang(i) = 0.0
   ENDDO
   Y(4) = Y(1)
   Y(6) = Y(3)
!
   IF ( .NOT.(Crank1) ) THEN
      X(2) = X(3)
      Y(2) = Y(3)
      Tang(2) = 0.0
   ENDIF
!
   IF ( .NOT.(Crank2) ) THEN
      X(5) = X(6)
      Y(5) = Y(6)
      Tang(5) = 0.0
   ENDIF
!
   Tang(1) = (X(2)-X(1))/(Y(2)-Y(1))
   Ang(1) = 57.2958*atan(Tang(1))
   IF ( Crank1 ) Tang(2) = (X(3)-X(2))/(Y(3)-Y(2))
   Ang(2) = 57.2958*atan(Tang(2))
   Tang(4) = (X(5)-X(4))/(Y(5)-Y(4))
   Ang(4) = 57.2958*atan(Tang(4))
   IF ( Crank2 ) Tang(5) = (X(6)-X(5))/(Y(6)-Y(5))
   Ang(5) = 57.2958*atan(Tang(5))
!
   areaw = 0.5*(X(1)*(Y(1)-Y(2))+X(2)*(Y(1)-Y(3))+X(3)*(Y(2)-Y(3))+X(4)*(Y(5)-Y(1))+X(5)*(Y(3)-Y(1))+X(6)*(Y(3)-Y(5)))
!
!     CONTROL1 SURFACE GEOMETRY
!
   area1 = 0.0
   IF ( Cntrl1 ) THEN
      Tang(7) = (X(9)-X(8))/(Y(9)-Y(8))
      Ang(7) = 57.2958*atan(Tang(7))
!
      IF ( abs(Y(7)-Y(8))>0.01 ) THEN
!
         tm = (X(7)-X(8))/(Y(7)-Y(8))
         IF ( Y(5)/=Y(7) .OR. X(5)/=X(7) ) THEN
            Y(7) = (tm*Y(8)-Tang(4)*Y(4)+X(4)-X(8))/(tm-Tang(4))
            IF ( Y(7)<=Y(5) ) THEN
               X(7) = Tang(4)*(Y(7)-Y(4)) + X(4)
            ELSE
               Y(7) = (tm*Y(8)-Tang(5)*Y(5)+X(5)-X(8))/(tm-Tang(5))
               X(7) = Tang(5)*(Y(7)-Y(5)) + X(5)
            ENDIF
         ENDIF
      ELSE
         Y(7) = Y(8)
         tm = big
         IF ( Y(7)>Y(5) ) THEN
            X(7) = Tang(5)*(Y(7)-Y(5)) + X(5)
         ELSE
            X(7) = Tang(4)*(Y(7)-Y(4)) + X(4)
         ENDIF
      ENDIF
      Tang(6) = tm
!
      IF ( abs(Y(11)-Y(9))>0.01 ) THEN
!
         tm = (X(11)-X(9))/(Y(11)-Y(9))
         IF ( Y(5)/=Y(11) .OR. X(5)/=X(11) ) THEN
            Y(11) = (tm*Y(9)-Tang(4)*Y(4)+X(4)-X(9))/(tm-Tang(4))
            IF ( Y(11)<=Y(5) ) THEN
               X(11) = Tang(4)*(Y(11)-Y(4)) + X(4)
            ELSE
               Y(11) = (tm*Y(9)-Tang(5)*Y(5)+X(5)-X(9))/(tm-Tang(5))
               X(11) = Tang(5)*(Y(11)-Y(5)) + X(5)
            ENDIF
         ENDIF
      ELSE
         Y(11) = Y(9)
         tm = big
         IF ( Y(11)>Y(5) ) THEN
            X(11) = Tang(5)*(Y(11)-Y(5)) + X(5)
         ELSE
            X(11) = Tang(4)*(Y(11)-Y(4)) + X(4)
         ENDIF
      ENDIF
      Tang(8) = tm
!
      IF ( Y(7)<=Y(5) .AND. Y(11)>=Y(5) ) THEN
!
         area1 = 0.5*(X(5)*(Y(11)-Y(7))+X(8)*(Y(7)-Y(9))+X(9)*(Y(8)-Y(11))+X(7)*(Y(5)-Y(8))+X(11)*(Y(9)-Y(5)))
      ELSE
         area1 = 0.5*((X(8)-X(11))*(Y(7)-Y(9))+(X(9)-X(7))*(Y(8)-Y(11)))
      ENDIF
   ENDIF
!
!     CONTROL2 SURFACE GEOMETRY
!
   area2 = 0.0
   IF ( Cntrl2 ) THEN
      Tang(10) = (X(10)-X(9))/(Y(10)-Y(9))
      Ang(10) = 57.2958*atan(Tang(10))
      IF ( abs(Y(12)-Y(10))>0.01 ) THEN
         tm = (X(12)-X(10))/(Y(12)-Y(10))
         IF ( Y(5)/=Y(12) .OR. X(5)/=X(12) ) THEN
            Y(12) = (tm*Y(10)-Tang(4)*Y(4)+X(4)-X(10))/(tm-Tang(4))
            IF ( Y(12)<=Y(5) ) THEN
               X(12) = Tang(4)*(Y(12)-Y(4)) + X(4)
            ELSE
               Y(12) = (tm*Y(10)-Tang(5)*Y(5)+X(5)-X(10))/(tm-Tang(5))
               X(12) = Tang(5)*(Y(12)-Y(5)) + X(5)
            ENDIF
         ENDIF
      ELSE
         Y(12) = Y(10)
         tm = big
         IF ( Y(12)>Y(5) ) THEN
            X(12) = Tang(5)*(Y(12)-Y(5)) + X(5)
         ELSE
            X(12) = Tang(4)*(Y(12)-Y(4)) + X(4)
         ENDIF
      ENDIF
      Tang(9) = tm
!
      IF ( Y(11)<=Y(5) .AND. Y(12)>=Y(5) ) THEN
!
         area2 = 0.5*(X(5)*(Y(12)-Y(11))+X(9)*(Y(11)-Y(10))+X(10)*(Y(9)-Y(12))+X(11)*(Y(5)-Y(9))+X(12)*(Y(10)-Y(5)))
      ELSE
         area2 = 0.5*((X(9)-X(12))*(Y(11)-Y(10))+(X(10)-X(11))*(Y(9)-Y(12)))
      ENDIF
   ENDIF
!
!     PRINT GEOMETRY DATA
!
   Cr = X(4) - X(1)
   CALL mbprit(areaw,area1,area2)
   Gc = 2.0*Cr**2
   xcent = (X(3)+X(4)+X(6))/4.0
   ycent = Y(3)*(0.333+0.167*(X(6)-X(3))/X(4))
!
   DO i = 1 , 10
      IF ( Tang(i)==0 ) THEN
         Cotang(i) = big
      ELSEIF ( Tang(i)/=big ) THEN
         Cotang(i) = 1./Tang(i)
      ELSE
         Cotang(i) = 0.
      ENDIF
   ENDDO
END SUBROUTINE mbgeod