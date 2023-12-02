!*==mbreg.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbreg(Ireg,Nw1,Nwn,Nc21,Nc2n,Nc1,Ncn,Nd1,Ndn,Xk,Yk,Xk1,Yk1,Xk2,Yk2,Xwte,Ywte,Kte,Kte1,Kte2,Parea)
   USE c_mboxa
   USE c_mboxc
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ireg
   INTEGER , DIMENSION(1) :: Nw1
   INTEGER , DIMENSION(1) :: Nwn
   INTEGER , DIMENSION(1) :: Nc21
   INTEGER , DIMENSION(1) :: Nc2n
   INTEGER , DIMENSION(1) :: Nc1
   INTEGER , DIMENSION(1) :: Ncn
   INTEGER , DIMENSION(1) :: Nd1
   INTEGER , DIMENSION(1) :: Ndn
   REAL , DIMENSION(1) :: Xk
   REAL , DIMENSION(1) :: Yk
   REAL , DIMENSION(1) :: Xk1
   REAL , DIMENSION(1) :: Yk1
   REAL , DIMENSION(1) :: Xk2
   REAL , DIMENSION(1) :: Yk2
   REAL , DIMENSION(1) :: Xwte
   REAL , DIMENSION(1) :: Ywte
   INTEGER , DIMENSION(1) :: Kte
   INTEGER , DIMENSION(1) :: Kte1
   INTEGER , DIMENSION(1) :: Kte2
   REAL , DIMENSION(50,50,3) :: Parea
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , pa , xb , xc , xle , xre , xt , ybe , yc , yl , yr , yte
   LOGICAL , SAVE :: debug
   INTEGER :: i , il1 , il2 , iprint , ipt , ir1 , ir2 , ixr , j , j1 , jxr , k , kk , kp , kpt , kxyz , n1 , nn
   EXTERNAL mbctr
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     SUBROUTINE TO COMPUTE LIMITS OF REGION AND PERCENTAGE OF BOX IN
!     EACH
!
   DATA debug/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iprint = 0
         IF ( debug ) iprint = 1
!
         Ireg = 1
         boxa = boxl*boxw
         kpt = 0
         kc1t = 0
         kc2t = 0
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
         yr = -0.5*boxw
         DO i = 1 , nsb
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  yl = yr
                  yr = (float(i)-0.5)*boxw
                  xle = xre
                  IF ( yr>y(k+1) ) THEN
                     xre = (yr-y(k+1))*tang(k+1) + x(k+1)
                     kpt = 1
                  ELSE
                     xre = (yr-y(k))*tang(k) + x(k)
                  ENDIF
                  xt = xle - amod(xle,boxl)
                  xb = xt + boxl
                  j1 = xb/boxl + 0.01
!
!
                  DO j = j1 , ncb
                     IF ( xre>xb ) THEN
!
                        IF ( xle>xt ) THEN
!
                           IF ( xb>x(k+1) ) THEN
                              ybe = (xb-x(k+1))*cotang(k+1) + y(k+1)
                              IF ( kpt==1 ) kpt = 2
                           ELSE
                              ybe = (xb-x(k))*cotang(k) + y(k)
                           ENDIF
                           a = 0.5*(xb-xle)*(ybe-yl)
                           IF ( kpt==2 ) a = a + (xle*(ybe-y(k+1))-yl*(xb-x(k+1))+xb*y(k+1)-ybe*x(k+1))/2.0
                           pa = a/boxa
                        ELSE
!
                           yte = ybe
                           IF ( xb>x(k+1) ) THEN
                              ybe = (xb-x(k+1))*cotang(k+1) + y(k+1)
                              IF ( kpt==1 ) kpt = 2
                           ELSE
                              ybe = (xb-x(k))*cotang(k) + y(k)
                           ENDIF
                           a = 0.5*(yte+ybe-2.0*yl)*boxl
                           IF ( kpt==2 ) a = a + (xt*(ybe-y(k+1))-yte*(xb-x(k+1))+xb*y(k+1)-ybe*x(k+1))/2.0
                           pa = a/boxa
                        ENDIF
                        xt = xb
                        xb = float(j+1)*boxl
                        IF ( kpt==2 ) kpt = 3
                        IF ( i==1 ) pa = 2.0*pa - 1.0
                        Parea(j,i,1) = pa
                     ELSE
                        IF ( xle>xt ) THEN
!
                           a = 0.5*(xle+xre-2.0*xt)*(yr-yl)
                           IF ( kpt>0 ) a = a + (xle*(y(k+1)-yr)-yl*(x(k+1)-xre)+x(k+1)*yr-xre*y(k+1))/2.0
                           pa = 1.0 - a/boxa
                        ELSE
!
                           IF ( xt<=x(k+1) ) THEN
                              yte = (xt-x(k))*cotang(k) + y(k)
                              IF ( kpt==1 ) kpt = 2
                           ELSE
                              yte = (xt-x(k+1))*cotang(k+1) + y(k+1)
                           ENDIF
                           a = 0.5*(yr-yte)*(xre-xt)
                           IF ( kpt==2 ) a = a + (xt*(y(k+1)-yr)-yte*(x(k+1)-xre)+x(k+1)*yr-xre*y(k+1))/2.0
                           pa = 1.0 - a/boxa
                        ENDIF
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (2)
!
                  IF ( i==1 ) pa = 2.0*pa - 1.0
                  Parea(j,i,1) = pa
                  spag_nextblock_2 = 3
               CASE (3)
                  yc = yr - 0.5*boxw
                  IF ( kpt<=0 ) THEN
                     xc = (yc-y(k))*tang(k) + x(k)
                  ELSEIF ( yc<=y(k+1) ) THEN
                     xc = (yc-y(k))*tang(k) + x(k)
                  ELSE
                     xc = (yc-y(k+1))*tang(k+1) + x(k+1)
                  ENDIF
                  Nw1(i) = xle/boxl + 1.0001
                  IF ( kpt>0 ) k = k + 1
                  kpt = 0
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         IF ( iprint>0 ) THEN
            WRITE (n6,99001)
99001       FORMAT (4H NW1)
            WRITE (n6,99013) (Nw1(i),i=1,nsb)
         ENDIF
!
!     TRAILING EDGE OF MAIN
!
         xre = x(4)
         k = 4
         yr = 0.0
         DO i = 1 , nsb
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  yl = yr
                  yr = (float(i)-0.5)*boxw
                  xle = xre
                  IF ( yr>y(k+1) ) THEN
                     xre = (yr-y(k+1))*tang(k+1) + x(k+1)
                     kpt = 1
                  ELSE
                     xre = (yr-y(k))*tang(k) + x(k)
                  ENDIF
                  xt = xle - amod(xle,boxl)
                  xb = xt + boxl
                  j = xb/boxl + 0.01
                  IF ( j>50 ) THEN
                     spag_nextblock_3 = 5
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  ipt = 0
                  IF ( xre>xb .OR. xre<xt ) THEN
!
                     ipt = 1
                     IF ( xle<xre ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     ipt = -1
                  ELSE
                     a = 0.5*(xle+xre-2.0*xt)*(yr-yl)
                     IF ( kpt>0 ) a = a + (xle*(y(k+1)-yr)-yl*(x(k+1)-xre)+x(k+1)*yr-y(k+1)*xre)/2.0
                     IF ( i==1 ) a = 2.0*a
                     spag_nextblock_3 = 6
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  spag_nextblock_3 = 2
               CASE (2)
                  IF ( xre<xt ) THEN
!
                     ybe = yte
                     IF ( xt<x(k+1) ) THEN
                        yte = (xt-x(k+1))*cotang(k+1) + y(k+1)
                        IF ( kpt==1 ) kpt = 2
                     ELSE
                        yte = (xt-x(k))*cotang(k) + y(k)
                     ENDIF
                     IF ( xle>xb ) THEN
!
                        a = 0.5*boxl*(yte+ybe-2.0*yl)
                        IF ( kpt==2 ) a = a + (xt*(ybe-y(k+1))-yte*(xb-x(k+1))+xb*y(k+1)-ybe*x(k+1))/2.0
                     ELSE
!
                        a = 0.5*(xle-xt)*(yte-yl)
                        IF ( kpt==2 ) a = a + (xt*(yl-y(k+1))-yte*(xle-x(k+1))+xle*y(k+1)-yl*x(k+1))/2.0
                     ENDIF
!
                     IF ( i==1 ) a = 2.0*a
                     spag_nextblock_3 = 4
                  ELSE
!
                     a = 0.5*(xb-xre)*(yr-yte)
                     IF ( kpt>0 .AND. kpt<3 ) a = a - (xre*(yte-y(k+1))-yr*(xb-x(k+1))+xb*y(k+1)-yte*x(k+1))/2.0
                     IF ( i==1 ) a = 2.0*a
!
                     a = boxa - a
                     spag_nextblock_3 = 6
                  ENDIF
               CASE (3)
!
                  IF ( xre>xb ) THEN
!
                     yte = ybe
                     IF ( xb>x(k+1) ) THEN
                        ybe = (xb-x(k+1))*cotang(k+1) + y(k+1)
                        IF ( kpt==1 ) kpt = 2
                     ELSE
                        ybe = (xb-x(k))*cotang(k) + y(k)
                     ENDIF
                     IF ( xle<xt ) THEN
!
                        a = 0.5*boxl*(2.0*yr-yte-ybe)
                        IF ( kpt==2 ) a = a + (xt*(y(k+1)-ybe)-yte*(x(k+1)-xb)+x(k+1)*ybe-y(k+1)*xb)/2.0
                        IF ( i==1 ) a = 2.0*a
                     ELSE
!
                        a = 0.5*(xb-xle)*(ybe-yl)
                        IF ( kpt==2 ) a = a - (xle*(y(k+1)-ybe)-yl*(x(k+1)-xb)+x(k+1)*ybe-y(k+1)*xb)/2.0
                        IF ( i==1 ) a = 2.0*a
                        a = boxa - a
                     ENDIF
                  ELSE
!
                     a = 0.5*(yr-ybe)*(xre-xt)
                     IF ( kpt>0 .AND. kpt<3 ) a = a + (xt*(y(k+1)-yr)-ybe*(x(k+1)-xre)+x(k+1)*yr-y(k+1)*xre)/2.0
                     IF ( i==1 ) a = 2.0*a
                     spag_nextblock_3 = 6
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  spag_nextblock_3 = 4
               CASE (4)
                  pa = a/boxa
                  a = 1.0
                  IF ( Parea(j,i,1)>0.0 ) a = Parea(j,i,1)
                  Parea(j,i,1) = pa*a
                  j = j + ipt
                  IF ( j<=50 ) THEN
                     xb = float(j)*boxl
                     xt = xb - boxl
                     IF ( kpt==2 ) kpt = 3
                     IF ( ipt>0 ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  spag_nextblock_3 = 5
               CASE (5)
                  Ireg = 2
                  RETURN
               CASE (6)
                  yc = yr - 0.5*boxw
                  IF ( kpt<=0 ) THEN
                     xc = (yc-y(k))*tang(k) + x(k)
                  ELSEIF ( yc<=y(k+1) ) THEN
                     xc = (yc-y(k))*tang(k) + x(k)
                  ELSE
                     xc = (yc-y(k+1))*tang(k+1) + x(k+1)
                  ENDIF
                  Nwn(i) = amax1(xle,xre)/boxl + 0.9999
                  pa = a/boxa
                  a = 1.0
                  IF ( Parea(j,i,1)>0.0 ) a = Parea(j,i,1)
                  Parea(j,i,1) = pa*a
                  Xwte(i) = xc
                  Ywte(i) = yc
                  IF ( kpt>0 ) k = k + 1
                  kpt = 0
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         IF ( iprint>0 ) THEN
            WRITE (n6,99002)
99002       FORMAT (4H NWN)
            WRITE (n6,99013) (Nwn(i),i=1,nsb)
         ENDIF
         ntote = nsb
!
!     FILL IN MAIN PERCENTAGES
!
         DO i = 1 , nsb
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
                  SPAG_Loop_2_1: DO k = 2 , i
                     kk = i - k + 1
                     IF ( Ndn(kk)>=Ndn(kk+1)-1 ) EXIT SPAG_Loop_2_1
                     Ndn(kk) = max0(Ndn(kk),Ndn(kk+1)-1)
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ELSE
               Nd1(1) = Nw1(1)
               Ndn(1) = Nwn(1)
            ENDIF
         ENDDO
         j = nsb + 1
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
         nsbd = j - 1
         IF ( iprint>0 ) THEN
            WRITE (n6,99003)
99003       FORMAT (4H ND1)
            WRITE (n6,99013) (Nd1(i),i=1,nsbd)
            WRITE (n6,99004)
99004       FORMAT (4H NDN)
            WRITE (n6,99013) (Ndn(i),i=1,nsbd)
!
            WRITE (n6,99015)
            DO i = 1 , ncb
               WRITE (n6,99016) i
               WRITE (n6,99014) (Parea(i,j,1),j=1,nsb)
            ENDDO
         ENDIF
         IF ( cntrl1 ) CALL mbctr(1,il1,ir1,Ncn,Nc1,Nwn,Nw1,Parea)
         IF ( cntrl2 ) CALL mbctr(2,il2,ir2,Nc2n,Nc21,Nwn,Nw1,Parea)
         IF ( iprint/=0 ) THEN
            DO kxyz = 1 , 3
               IF ( kxyz==1 ) WRITE (n6,99015)
               IF ( kxyz==2 ) WRITE (n6,99005)
99005          FORMAT (14H PAREA, CNTRL1)
               IF ( kxyz==3 ) WRITE (n6,99006)
99006          FORMAT (14H PAREA, CNTRL2)
               DO i = 1 , ncb
                  WRITE (n6,99016) i
                  WRITE (n6,99014) (Parea(i,j,kxyz),j=1,nsb)
               ENDDO
            ENDDO
         ENDIF
!
!     MAIN BOX CTR. COORDINATES
!
         kc = 0
         DO i = 1 , ncb
            ixr = i - 1
            DO j = 1 , nsb
               IF ( i>=(Nd1(j)) .AND. i<=(Ndn(j)) ) THEN
                  IF ( Parea(i,j,1)>=0.005 ) THEN
                     jxr = j - 1
                     kc = kc + 1
                     IF ( kc>=200 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Xk(kc) = boxl*(float(ixr)+0.5)
                     Yk(kc) = boxw*float(jxr)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         DO j = 1 , nsb
            kc = kc + 1
            IF ( kc>=200 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Kte(j) = kc
            Xk(kc) = Xwte(j)
            Yk(kc) = Ywte(j)
         ENDDO
         kct = kc
         IF ( iprint>0 ) THEN
            WRITE (n6,99007) (i,Xk(i),i=1,kc)
99007       FORMAT (1H1,23H MAIN BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
            WRITE (n6,99008) (i,Yk(i),i=1,kc)
99008       FORMAT (1H1,23H MAIN BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
         ENDIF
!
!     CNTRL1 BOX CTR. COORDINATES
!
         IF ( cntrl1 ) THEN
            kc1 = 0
            DO i = 1 , ncb
               ixr = i - 1
               DO j = il1 , ir1
                  IF ( Parea(i,j,2)>=0.005 ) THEN
                     jxr = j - 1
                     kc1 = kc1 + 1
                     IF ( kc1>=125 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Xk1(kc1) = boxl*(float(ixr)+0.5)
                     Yk1(kc1) = boxw*float(jxr)
                  ENDIF
               ENDDO
            ENDDO
            DO j = il1 , ir1
               kc1 = kc1 + 1
               IF ( kc1>=125 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Kte1(j) = kc1
               Xk1(kc1) = Xwte(j)
               Yk1(kc1) = Ywte(j)
            ENDDO
            kc1t = kc1
            IF ( iprint>0 ) THEN
               WRITE (n6,99009) (i,Xk1(i),i=1,kc1)
99009          FORMAT (1H1,25H CNTRL1 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
               WRITE (n6,99010) (i,Yk1(i),i=1,kc1)
99010          FORMAT (1H1,25H CNTRL1 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
            ENDIF
         ENDIF
!
!     CNTRL2 BOX CTR. COORDINATES
!
         IF ( cntrl2 ) THEN
            kc2 = 0
            DO i = 1 , ncb
               ixr = i - 1
               DO j = il2 , ir2
                  IF ( Parea(i,j,3)>=0.005 ) THEN
                     jxr = j - 1
                     kc2 = kc2 + 1
                     IF ( kc2>=125 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Xk2(kc2) = boxl*(float(ixr)+0.5)
                     Yk2(kc2) = boxw*float(jxr)
                  ENDIF
               ENDDO
            ENDDO
            DO j = il2 , ir2
               kc2 = kc2 + 1
               IF ( kc2>=125 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Kte2(j) = kc2
               Xk2(kc2) = Xwte(j)
               Yk2(kc2) = Ywte(j)
            ENDDO
            kc2t = kc2
            IF ( iprint>0 ) THEN
               WRITE (n6,99011) (i,Xk2(i),i=1,kc2)
99011          FORMAT (1H1,25H CNTRL2 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
               WRITE (n6,99012) (i,Yk2(i),i=1,kc2)
99012          FORMAT (1H1,25H CNTRL2 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
            ENDIF
         ENDIF
         boxl = boxl/cr
         boxw = boxw/cr
         boxa = boxa/cr**2
         DO i = 1 , 12
            x(i) = x(i)/cr
            y(i) = y(i)/cr
         ENDDO
         DO i = 1 , 50
            Xwte(i) = Xwte(i)/cr
            Ywte(i) = Ywte(i)/cr
         ENDDO
         RETURN
      CASE (2)
         Ireg = 2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99013 FORMAT (10I12)
99014 FORMAT (5X,10F9.5)
99015 FORMAT (12H PAREA, MAIN)
99016 FORMAT (4H ROW,I4)
END SUBROUTINE mbreg
