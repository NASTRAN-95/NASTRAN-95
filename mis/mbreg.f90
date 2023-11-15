
SUBROUTINE mbreg(Ireg,Nw1,Nwn,Nc21,Nc2n,Nc1,Ncn,Nd1,Ndn,Xk,Yk,Xk1,Yk1,Xk2,Yk2,Xwte,Ywte,Kte,Kte1,Kte2,Parea)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Ang(10) , Beta , Boxa , Boxl , Boxw , Cotang(10) , Cr , Ek , Ekbar , Ekm , Gc , Sys , Tang(10) , X(12) , Y(12)
   LOGICAL Asym , Cntrl1 , Cntrl2 , Crank1 , Crank2
   INTEGER Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , Mach , N6 , Nbox , Ncb , Njj , Npts0 , Npts1 , Npts2 , Nsb , Nsbd , Ntote
   COMMON /mboxa / X , Y , Tang , Ang , Cotang
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   COMMON /system/ Sys , N6
!
! Dummy argument declarations
!
   INTEGER Ireg
   INTEGER Kte(1) , Kte1(1) , Kte2(1) , Nc1(1) , Nc21(1) , Nc2n(1) , Ncn(1) , Nd1(1) , Ndn(1) , Nw1(1) , Nwn(1)
   REAL Parea(50,50,3) , Xk(1) , Xk1(1) , Xk2(1) , Xwte(1) , Yk(1) , Yk1(1) , Yk2(1) , Ywte(1)
!
! Local variable declarations
!
   REAL a , pa , xb , xc , xle , xre , xt , ybe , yc , yl , yr , yte
   LOGICAL debug
   INTEGER i , il1 , il2 , iprint , ipt , ir1 , ir2 , ixr , j , j1 , jxr , k , kk , kp , kpt , kxyz , n1 , nn
!
! End of declarations
!
!
!     SUBROUTINE TO COMPUTE LIMITS OF REGION AND PERCENTAGE OF BOX IN
!     EACH
!
   DATA debug/.FALSE./
!
   iprint = 0
   IF ( debug ) iprint = 1
!
   Ireg = 1
   Boxa = Boxl*Boxw
   kpt = 0
   Kc1t = 0
   Kc2t = 0
   DO i = 1 , 50
      Nw1(i) = 0
      Nwn(i) = 0
      Nc1(i) = 0
      Ncn(i) = 0
      Nc21(i) = 0
      Nc2n(i) = 0
      Nd1(i) = 0
      Ndn(i) = 0
      Kte(i) = 0
      Kte1(i) = 0
      Kte2(i) = 0
      Xwte(i) = 0.0
      Ywte(i) = 0.0
      DO j = 1 , 50
         DO kp = 1 , 3
            Parea(i,j,kp) = 0.
         ENDDO
      ENDDO
   ENDDO
   DO i = 1 , 200
      Xk(i) = 0.0
      Yk(i) = 0.0
   ENDDO
   DO i = 1 , 125
      Xk1(i) = 0.0
      Yk1(i) = 0.0
      Xk2(i) = 0.0
      Yk2(i) = 0.0
   ENDDO
!
!     LEADING EDGE OF MAIN
!
   xre = 0.0
   ybe = 0.0
   k = 1
   yr = -0.5*Boxw
   DO i = 1 , Nsb
      yl = yr
      yr = (float(i)-0.5)*Boxw
      xle = xre
      IF ( yr>Y(k+1) ) THEN
         xre = (yr-Y(k+1))*Tang(k+1) + X(k+1)
         kpt = 1
      ELSE
         xre = (yr-Y(k))*Tang(k) + X(k)
      ENDIF
      xt = xle - amod(xle,Boxl)
      xb = xt + Boxl
      j1 = xb/Boxl + 0.01
!
!
      DO j = j1 , Ncb
         IF ( xre>xb ) THEN
!
            IF ( xle>xt ) THEN
!
               IF ( xb>X(k+1) ) THEN
                  ybe = (xb-X(k+1))*Cotang(k+1) + Y(k+1)
                  IF ( kpt==1 ) kpt = 2
               ELSE
                  ybe = (xb-X(k))*Cotang(k) + Y(k)
               ENDIF
               a = 0.5*(xb-xle)*(ybe-yl)
               IF ( kpt==2 ) a = a + (xle*(ybe-Y(k+1))-yl*(xb-X(k+1))+xb*Y(k+1)-ybe*X(k+1))/2.0
               pa = a/Boxa
            ELSE
!
               yte = ybe
               IF ( xb>X(k+1) ) THEN
                  ybe = (xb-X(k+1))*Cotang(k+1) + Y(k+1)
                  IF ( kpt==1 ) kpt = 2
               ELSE
                  ybe = (xb-X(k))*Cotang(k) + Y(k)
               ENDIF
               a = 0.5*(yte+ybe-2.0*yl)*Boxl
               IF ( kpt==2 ) a = a + (xt*(ybe-Y(k+1))-yte*(xb-X(k+1))+xb*Y(k+1)-ybe*X(k+1))/2.0
               pa = a/Boxa
            ENDIF
            xt = xb
            xb = float(j+1)*Boxl
            IF ( kpt==2 ) kpt = 3
            IF ( i==1 ) pa = 2.0*pa - 1.0
            Parea(j,i,1) = pa
         ELSE
            IF ( xle>xt ) THEN
!
               a = 0.5*(xle+xre-2.0*xt)*(yr-yl)
               IF ( kpt>0 ) a = a + (xle*(Y(k+1)-yr)-yl*(X(k+1)-xre)+X(k+1)*yr-xre*Y(k+1))/2.0
               pa = 1.0 - a/Boxa
            ELSE
!
               IF ( xt<=X(k+1) ) THEN
                  yte = (xt-X(k))*Cotang(k) + Y(k)
                  IF ( kpt==1 ) kpt = 2
               ELSE
                  yte = (xt-X(k+1))*Cotang(k+1) + Y(k+1)
               ENDIF
               a = 0.5*(yr-yte)*(xre-xt)
               IF ( kpt==2 ) a = a + (xt*(Y(k+1)-yr)-yte*(X(k+1)-xre)+X(k+1)*yr-xre*Y(k+1))/2.0
               pa = 1.0 - a/Boxa
            ENDIF
            GOTO 50
         ENDIF
      ENDDO
      GOTO 100
!
 50   IF ( i==1 ) pa = 2.0*pa - 1.0
      Parea(j,i,1) = pa
 100  yc = yr - 0.5*Boxw
      IF ( kpt<=0 ) THEN
         xc = (yc-Y(k))*Tang(k) + X(k)
      ELSEIF ( yc<=Y(k+1) ) THEN
         xc = (yc-Y(k))*Tang(k) + X(k)
      ELSE
         xc = (yc-Y(k+1))*Tang(k+1) + X(k+1)
      ENDIF
      Nw1(i) = xle/Boxl + 1.0001
      IF ( kpt>0 ) k = k + 1
      kpt = 0
   ENDDO
   IF ( iprint>0 ) THEN
      WRITE (N6,99001)
99001 FORMAT (4H NW1)
      WRITE (N6,99013) (Nw1(i),i=1,Nsb)
   ENDIF
!
!     TRAILING EDGE OF MAIN
!
   xre = X(4)
   k = 4
   yr = 0.0
   DO i = 1 , Nsb
      yl = yr
      yr = (float(i)-0.5)*Boxw
      xle = xre
      IF ( yr>Y(k+1) ) THEN
         xre = (yr-Y(k+1))*Tang(k+1) + X(k+1)
         kpt = 1
      ELSE
         xre = (yr-Y(k))*Tang(k) + X(k)
      ENDIF
      xt = xle - amod(xle,Boxl)
      xb = xt + Boxl
      j = xb/Boxl + 0.01
      IF ( j>50 ) GOTO 300
      ipt = 0
      IF ( xre>xb .OR. xre<xt ) THEN
!
         ipt = 1
         IF ( xle<xre ) GOTO 200
         ipt = -1
      ELSE
         a = 0.5*(xle+xre-2.0*xt)*(yr-yl)
         IF ( kpt>0 ) a = a + (xle*(Y(k+1)-yr)-yl*(X(k+1)-xre)+X(k+1)*yr-Y(k+1)*xre)/2.0
         IF ( i==1 ) a = 2.0*a
         GOTO 350
      ENDIF
 150  IF ( xre<xt ) THEN
!
         ybe = yte
         IF ( xt<X(k+1) ) THEN
            yte = (xt-X(k+1))*Cotang(k+1) + Y(k+1)
            IF ( kpt==1 ) kpt = 2
         ELSE
            yte = (xt-X(k))*Cotang(k) + Y(k)
         ENDIF
         IF ( xle>xb ) THEN
!
            a = 0.5*Boxl*(yte+ybe-2.0*yl)
            IF ( kpt==2 ) a = a + (xt*(ybe-Y(k+1))-yte*(xb-X(k+1))+xb*Y(k+1)-ybe*X(k+1))/2.0
         ELSE
!
            a = 0.5*(xle-xt)*(yte-yl)
            IF ( kpt==2 ) a = a + (xt*(yl-Y(k+1))-yte*(xle-X(k+1))+xle*Y(k+1)-yl*X(k+1))/2.0
         ENDIF
!
         IF ( i==1 ) a = 2.0*a
         GOTO 250
      ELSE
!
         a = 0.5*(xb-xre)*(yr-yte)
         IF ( kpt>0 .AND. kpt<3 ) a = a - (xre*(yte-Y(k+1))-yr*(xb-X(k+1))+xb*Y(k+1)-yte*X(k+1))/2.0
         IF ( i==1 ) a = 2.0*a
!
         a = Boxa - a
         GOTO 350
      ENDIF
!
 200  IF ( xre>xb ) THEN
!
         yte = ybe
         IF ( xb>X(k+1) ) THEN
            ybe = (xb-X(k+1))*Cotang(k+1) + Y(k+1)
            IF ( kpt==1 ) kpt = 2
         ELSE
            ybe = (xb-X(k))*Cotang(k) + Y(k)
         ENDIF
         IF ( xle<xt ) THEN
!
            a = 0.5*Boxl*(2.0*yr-yte-ybe)
            IF ( kpt==2 ) a = a + (xt*(Y(k+1)-ybe)-yte*(X(k+1)-xb)+X(k+1)*ybe-Y(k+1)*xb)/2.0
            IF ( i==1 ) a = 2.0*a
         ELSE
!
            a = 0.5*(xb-xle)*(ybe-yl)
            IF ( kpt==2 ) a = a - (xle*(Y(k+1)-ybe)-yl*(X(k+1)-xb)+X(k+1)*ybe-Y(k+1)*xb)/2.0
            IF ( i==1 ) a = 2.0*a
            a = Boxa - a
         ENDIF
      ELSE
!
         a = 0.5*(yr-ybe)*(xre-xt)
         IF ( kpt>0 .AND. kpt<3 ) a = a + (xt*(Y(k+1)-yr)-ybe*(X(k+1)-xre)+X(k+1)*yr-Y(k+1)*xre)/2.0
         IF ( i==1 ) a = 2.0*a
         GOTO 350
      ENDIF
 250  pa = a/Boxa
      a = 1.0
      IF ( Parea(j,i,1)>0.0 ) a = Parea(j,i,1)
      Parea(j,i,1) = pa*a
      j = j + ipt
      IF ( j<=50 ) THEN
         xb = float(j)*Boxl
         xt = xb - Boxl
         IF ( kpt==2 ) kpt = 3
         IF ( ipt<=0 ) GOTO 150
         GOTO 200
      ENDIF
 300  Ireg = 2
      RETURN
 350  yc = yr - 0.5*Boxw
      IF ( kpt<=0 ) THEN
         xc = (yc-Y(k))*Tang(k) + X(k)
      ELSEIF ( yc<=Y(k+1) ) THEN
         xc = (yc-Y(k))*Tang(k) + X(k)
      ELSE
         xc = (yc-Y(k+1))*Tang(k+1) + X(k+1)
      ENDIF
      Nwn(i) = amax1(xle,xre)/Boxl + 0.9999
      pa = a/Boxa
      a = 1.0
      IF ( Parea(j,i,1)>0.0 ) a = Parea(j,i,1)
      Parea(j,i,1) = pa*a
      Xwte(i) = xc
      Ywte(i) = yc
      IF ( kpt>0 ) k = k + 1
      kpt = 0
   ENDDO
   IF ( iprint>0 ) THEN
      WRITE (N6,99002)
99002 FORMAT (4H NWN)
      WRITE (N6,99013) (Nwn(i),i=1,Nsb)
   ENDIF
   Ntote = Nsb
!
!     FILL IN MAIN PERCENTAGES
!
   DO i = 1 , Nsb
      n1 = Nw1(i)
      nn = Nwn(i)
      DO j = n1 , nn
         IF ( Parea(j,i,1)<=0.0 ) Parea(j,i,1) = 1.0
      ENDDO
!
!     DIAPHRAGM INDEX
!
      IF ( i/=1 ) THEN
         Nd1(i) = min0(Nw1(i),Nd1(i-1)+1)
         Ndn(i) = max0(Nwn(i),Ndn(i-1)-1)
         IF ( Ndn(i)>Ndn(i-1)+1 ) THEN
            DO k = 2 , i
               kk = i - k + 1
               IF ( Ndn(kk)>=Ndn(kk+1)-1 ) EXIT
               Ndn(kk) = max0(Ndn(kk),Ndn(kk+1)-1)
            ENDDO
         ENDIF
      ELSE
         Nd1(1) = Nw1(1)
         Ndn(1) = Nwn(1)
      ENDIF
   ENDDO
   j = Nsb + 1
   DO WHILE ( Nd1(j-1)<Ndn(j-1)-1 )
      Nd1(j) = Nd1(j-1) + 1
      Ndn(j) = Ndn(j-1) - 1
      j = j + 1
      IF ( j>50 ) THEN
         Ireg = 2
         RETURN
      ENDIF
   ENDDO
!
   Nsbd = j - 1
   IF ( iprint>0 ) THEN
      WRITE (N6,99003)
99003 FORMAT (4H ND1)
      WRITE (N6,99013) (Nd1(i),i=1,Nsbd)
      WRITE (N6,99004)
99004 FORMAT (4H NDN)
      WRITE (N6,99013) (Ndn(i),i=1,Nsbd)
!
      WRITE (N6,99015)
      DO i = 1 , Ncb
         WRITE (N6,99016) i
         WRITE (N6,99014) (Parea(i,j,1),j=1,Nsb)
      ENDDO
   ENDIF
   IF ( Cntrl1 ) CALL mbctr(1,il1,ir1,Ncn,Nc1,Nwn,Nw1,Parea)
   IF ( Cntrl2 ) CALL mbctr(2,il2,ir2,Nc2n,Nc21,Nwn,Nw1,Parea)
   IF ( iprint/=0 ) THEN
      DO kxyz = 1 , 3
         IF ( kxyz==1 ) WRITE (N6,99015)
         IF ( kxyz==2 ) WRITE (N6,99005)
99005    FORMAT (14H PAREA, CNTRL1)
         IF ( kxyz==3 ) WRITE (N6,99006)
99006    FORMAT (14H PAREA, CNTRL2)
         DO i = 1 , Ncb
            WRITE (N6,99016) i
            WRITE (N6,99014) (Parea(i,j,kxyz),j=1,Nsb)
         ENDDO
      ENDDO
   ENDIF
!
!     MAIN BOX CTR. COORDINATES
!
   Kc = 0
   DO i = 1 , Ncb
      ixr = i - 1
      DO j = 1 , Nsb
         IF ( i>=(Nd1(j)) .AND. i<=(Ndn(j)) ) THEN
            IF ( Parea(i,j,1)>=0.005 ) THEN
               jxr = j - 1
               Kc = Kc + 1
               IF ( Kc>=200 ) GOTO 400
               Xk(Kc) = Boxl*(float(ixr)+0.5)
               Yk(Kc) = Boxw*float(jxr)
            ENDIF
         ENDIF
      ENDDO
   ENDDO
   DO j = 1 , Nsb
      Kc = Kc + 1
      IF ( Kc>=200 ) GOTO 400
      Kte(j) = Kc
      Xk(Kc) = Xwte(j)
      Yk(Kc) = Ywte(j)
   ENDDO
   Kct = Kc
   IF ( iprint>0 ) THEN
      WRITE (N6,99007) (i,Xk(i),i=1,Kc)
99007 FORMAT (1H1,23H MAIN BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
      WRITE (N6,99008) (i,Yk(i),i=1,Kc)
99008 FORMAT (1H1,23H MAIN BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
   ENDIF
!
!     CNTRL1 BOX CTR. COORDINATES
!
   IF ( Cntrl1 ) THEN
      Kc1 = 0
      DO i = 1 , Ncb
         ixr = i - 1
         DO j = il1 , ir1
            IF ( Parea(i,j,2)>=0.005 ) THEN
               jxr = j - 1
               Kc1 = Kc1 + 1
               IF ( Kc1>=125 ) GOTO 400
               Xk1(Kc1) = Boxl*(float(ixr)+0.5)
               Yk1(Kc1) = Boxw*float(jxr)
            ENDIF
         ENDDO
      ENDDO
      DO j = il1 , ir1
         Kc1 = Kc1 + 1
         IF ( Kc1>=125 ) GOTO 400
         Kte1(j) = Kc1
         Xk1(Kc1) = Xwte(j)
         Yk1(Kc1) = Ywte(j)
      ENDDO
      Kc1t = Kc1
      IF ( iprint>0 ) THEN
         WRITE (N6,99009) (i,Xk1(i),i=1,Kc1)
99009    FORMAT (1H1,25H CNTRL1 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
         WRITE (N6,99010) (i,Yk1(i),i=1,Kc1)
99010    FORMAT (1H1,25H CNTRL1 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
      ENDIF
   ENDIF
!
!     CNTRL2 BOX CTR. COORDINATES
!
   IF ( Cntrl2 ) THEN
      Kc2 = 0
      DO i = 1 , Ncb
         ixr = i - 1
         DO j = il2 , ir2
            IF ( Parea(i,j,3)>=0.005 ) THEN
               jxr = j - 1
               Kc2 = Kc2 + 1
               IF ( Kc2>=125 ) GOTO 400
               Xk2(Kc2) = Boxl*(float(ixr)+0.5)
               Yk2(Kc2) = Boxw*float(jxr)
            ENDIF
         ENDDO
      ENDDO
      DO j = il2 , ir2
         Kc2 = Kc2 + 1
         IF ( Kc2>=125 ) GOTO 400
         Kte2(j) = Kc2
         Xk2(Kc2) = Xwte(j)
         Yk2(Kc2) = Ywte(j)
      ENDDO
      Kc2t = Kc2
      IF ( iprint>0 ) THEN
         WRITE (N6,99011) (i,Xk2(i),i=1,Kc2)
99011    FORMAT (1H1,25H CNTRL2 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
         WRITE (N6,99012) (i,Yk2(i),i=1,Kc2)
99012    FORMAT (1H1,25H CNTRL2 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
      ENDIF
   ENDIF
   Boxl = Boxl/Cr
   Boxw = Boxw/Cr
   Boxa = Boxa/Cr**2
   DO i = 1 , 12
      X(i) = X(i)/Cr
      Y(i) = Y(i)/Cr
   ENDDO
   DO i = 1 , 50
      Xwte(i) = Xwte(i)/Cr
      Ywte(i) = Ywte(i)/Cr
   ENDDO
   GOTO 99999
 400  Ireg = 2
99013 FORMAT (10I12)
99014 FORMAT (5X,10F9.5)
99015 FORMAT (12H PAREA, MAIN)
99016 FORMAT (4H ROW,I4)
99999 END SUBROUTINE mbreg
