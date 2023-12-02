!*==xyplot.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xyplot
   IMPLICIT NONE
   USE C_MACHIN
   USE C_PLTDAT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_XXPARM
   USE C_XYPLIN
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dl , dtc , dx , dy , nums , oldx , oldy , r , rnum , s , t , t1 , t2 , xc , xdr , xl , xmaxs , xmins , xpl , xprm , xps ,&
         & xt , xts , y1t , yc , ydr , yl , ylabel , ymaxs , ymins , ypl , yprm , yps , yt , yts , ywmin , yxtr
   INTEGER :: expo , i , icpen , ifield , ifin , ill , iopn , ipchg , itc , j , k , l , label , log , logxs , logys , lstep , mb1 , &
            & mb2 , mb3 , n , nact , ndg , nerr , nitk , nlines , nlpp , ntt , outape , sysbuf
   INTEGER , SAVE :: iclsrw , ie , iplus , irdrw , lcmr , lem , lep , lpltmd , nrwd , xyplt
   INTEGER , DIMENSION(2) :: isym
   INTEGER , DIMENSION(1) :: ix
   INTEGER , DIMENSION(10) , SAVE :: lttn , lttp
   REAL , DIMENSION(22) , SAVE :: tltv
   REAL , DIMENSION(1) :: x , y
   REAL , SAVE :: xlpap , ylpap
   REAL , DIMENSION(2) :: xy
!
! End of declarations rewritten by SPAG
!
!
!     XYPLOT IS AN OUTPUT MODULE
!
!     INFORMATION SUPPLIED BY XYTRAN THROUGH DATA BLOCK XYPLOT
!     IS INTERPRETED AND OUTPUT TO EITHER PLT1(BCD TAPE FILE) OR
!     PLT2(BINARY TAPE FILE) FOR PLOTTING ON AN OFF-LINE PLOTTER.
!
!
!ZZ   COMMON /ZZXYPL/ Z(1)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outape) , (Ksystm(9),Nlpp) , (Ksystm(12),Nlines) , (Z(1),X(1),Ix(1),Xy(1)) ,         &
!>>>>    & (Xy(2),Y(1))
   DATA lpltmd , lcmr , xlpap , ylpap/ - 1 , -1 , -1.0 , -1.0/
   DATA xyplt/101/
   DATA nrwd , irdrw , iclsrw/300 , 0 , 1/
   DATA iplus , ie , lep , lem/1H+ , 1HE , 4H1E+  , 4H1E- /
   DATA lttn/8 , 8 , 5 , 4 , 3 , 2 , 2 , 1 , 1 , 1/ , lttp/15 , 15 , 10 , 6 , 3 , 1 , 1 , 7 , 7 , 7/ , tltv/3. , 6. , 2. , 5. , 8. ,&
      & 2. , 4. , 6. , 8. , 2. , 3. , 5. , 7. , 9. , 2. , 3. , 4. , 5. , 6. , 7. , 8. , 9./
!
!
!     DEFINITION OF COMMON BLOCK /PLTDAT/ CONTENTS
!
!     MODEL  - MODEL NUMBER OF THE CURRENT PLOTTER.
!     IPLTNR - NUMBER OF CURRENT PLOTTER IN USE
!     XWMIN  - MINIMUM X VALUE OF PLOTTING REGION IN PLOTTER COUNTS
!     YLOW   - MIN. Y VALUE OF PLOT. REGION(AFTER TITLES)
!              IN PLOTTER COUNTS
!     AXMAX  - MAX. X VALUE OF PLOT. REGION(LESS MARGIN)
!              IN PLOTTER COUNTS
!     YUP    - MAX. Y VALUE OF PLOT. REGION(LESS MARGIN)
!              IN PLOTTER COUNTS
!     XWMAX  - ACTUAL MAXIMUM REGION SIZE IN X DIRECTION
!              IN PLOTTER COUNTS
!     YWMAX  - ACTUAL MAXIMUM REGION SIZE IN Y DIRECTION
!              IN PLOTTER COUNTS
!     XEDGE  - MARGIN OF X EDGE IN PLOTTER COUNTS (TABLE PLOTTERS ONLY)
!     YEDGE  - MARGIN OF Y EDGE IN PLOTTER COUNTS (TABLE PLOTTERS ONLY)
!     XA     - SPARES
!
!     THE FOLLOWING SYMBOLIC VALUES PERTAIN TO THE CURRENT PLOTTER.
!     AND ARE SET WHEN STPLOT OR PLTSET IS CALLED.
!
!     XYMAX - X AND Y FRAME LIMITS IN PLOTTER COUNTS.
!     CNTPI - PLOTTER COUNTS PER INCH.
!     CCH   - HORIZONTAL PLOTTER COUNTS PER SINGLE CHARACTER
!     CCV   - VERTICAL PLOTTER COUNTS PER SINGLE CHARACTER
!     ALL   - MAXIMUM LINE LENGTH DRAWN WITH SINGLE COMMAND
!             (PLOTTER COUNT)
!     MNP   - MAXIMUM NUMBER OF PENS
!     APOX  - ACTUAL PLOTTER X ORIGIN IN PLOTTER COUNTS
!     APOY  - ACTUAL PLOTTER Y ORIGIN IN PLOTTER COUNTS
!             NOTE - INCREMENTAL PLOTTERS USE AS CURRENT PEN POSITION.
!     ITP   - PLOTTER TYPE.
!     LTAPE - GINO NAME OF THE PLOT TAPE.
!
!     DEFINITION OF I.D. RECORD CONTENTS OF INPUT DATA FILE /XYPLIN/
!
!     IDSB - SUBCASE I.D.               NFRM - FRAME NUMBER
!     NCRV - CURVE NUMBER               IDPE - POINT OR ELEMENT I.D.
!     NCOM - COMPONENT NUMBER           IDMJ - VECTOR NUMBER
!     ITBF - BOTTOM TOP FULL FRAME IND. NWFR - NEW AXIS AND LABEL IND.
!     ISKP - FRAME SKIP NUMBER          D1   - SPARE
!     XMIN - MINIMUM X DATA FOR CURVE   XMAX - MAXIMUM X DATA FOR CURVE
!     YMIN - MINIMUM Y DATA FOR CURVE   YMAX - MAXIMUM Y DATA FOR CURVE
!     XTIC - FIRST X TICK VALUE         XDTC - VALUE BETWEEN X TICKS
!     XLTC - HIGHEST X-VALUE ON FRAME.  NXDG - MAX. DIGITS FOR X-TICKS
!     IXPR - 10 POWER ON PRINTED X TICK NXTT - TOTAL NUMBER OF X TICKS
!     IXVS - X TICKS BETWEEN LABELS     IXDT - DELTA PRINT VALUE X TICKS
!     YTIC - FIRST Y TICK VALUE         YDTC - VALUE BETWEEN Y TICKS
!     YLTC - HIGHEST Y-VALUE ON FRAME.  NYDG - MAX. DIGITS FOR Y-TICKS
!     IYPR - 10 POWER ON PRINTED Y TICK NYTT - TOTAL NUMBER OF Y TICKS
!     IYVS - Y TICKS BETWEEN LABELS     IYDT - DELTA PRINT VALUE Y TICKS
!     ITTC - TICKS W/WO VALUES - TOP    IBTC - TICKS W/WO VALUES - BOTTM
!     ILTC - TICKS W/WO VALUES - LEFT   IRTC - TICKS W/WO VALUES - RIGHT
!     LOGX - LINEAR/LOG - X DIRECTION   LOGY - LINEAR/LOG - Y DIRECTION
!     IXAX - X AXIS/NO AXIS INDICATOR   XINT - X AXIS  Y INTERCEPT
!     IYAX - Y AXIS/NO AXIS INDICATOR   YINT - Y AXIS  X INTERCEPT
!     ICRV - POINT/LINE PLOT INDICATOR  D2   - SPARES
!     TITL - PLOT TITLE                 SBTL - PLOT SUBTITLE
!     CLBL - PLOT LABEL                 CVTL - PLOT CURVE TITLE
!     XATL - X AXIS TITLE               YATL - Y AXIS TITLE
!     IXGD - X GRID LINES               IYGD - Y GRID LINES
!     D3   - SPARES                     IPNR - PEN COLOR
!     IPSZ - PEN SIZE                   NPLT - TYPE OF PLOTTER
!     XPAP - PAPER SIZE(IN.) X DIR.     YPAP - PAPER SIZE(IN.) Y DIR.
!     NCMR - CAMERA NR. FOR SC-4020     D4   - XYTRAN INTERNAL FLAGS
!
!
!     SET IOPN=0 (PLOT TAPE CLOSED) AND NERR=0 (NUMBER OF ID RECORDS
!     WITH WRONG WORD COUNT).  WHEN NERR=5, XYPLOT ASSUMES BAD INPUT
!     FILE AND ABANDONS OPERATION.
!
   mb1 = korsz(Z) - sysbuf
   ipchg = 0
   iopn = 0
   CALL open(*1100,xyplt,Z(mb1),irdrw)
 100  CALL fwdrec(*1400,xyplt)
   nerr = 0
!
!     READ I.D. RECORD ON INPUT DATA FILE
!
 200  CALL read(*1400,*400,xyplt,Idsb,nrwd+1,1,nact)
 300  nerr = nerr + 1
   IF ( nerr<5 ) GOTO 200
   WRITE (outape,99001) Uwm
!
99001 FORMAT (A25,' 992, XYPLOT INPUT DATA FILE ID. RECORDS TOO SHORT.','  XYPLOT ABANDONED.')
   GOTO 1300
 400  IF ( nact/=nrwd ) GOTO 300
!
!     SKIP DATA IF IT WAS FOR THE PAPERPLOTER ONLY
!
   IF ( D4(2)<=0 ) GOTO 100
   IF ( Nwfr/=0 ) THEN
!
!     NEW AXIS, LABELS, ETC. ARE NEEDED.
!
!     NASTRAN PLOTTING SOFTWARE INITIALIZATION.
!
      IF ( Itbf>=0 .AND. iopn/=0 ) CALL stplot(-1)
      Ipltnr = rshift(Nplt,Ihalf)
      Model = Nplt - lshift(Ipltnr,Ihalf) - 100
      IF ( Ncmr>0 ) Icmra = Ncmr
      Ifskp = Iskp
      Cscale = Chrscl
      IF ( Cscale<1. ) Cscale = 1.0
      IF ( Xpap>0. ) Papszx = Xpap
      IF ( Ypap>0. ) Papszy = Ypap
      DO i = 1 , Npens
         Jpsz(i) = Ipsz
      ENDDO
      IF ( Itbf>=0 ) THEN
!
         CALL pltset
         lcmr = Ncmr
         xlpap = Xpap
         ylpap = Ypap
         lpltmd = Nplt
         mb2 = mb1 - Ipltbf
         mb3 = 2*((mb2-1)/2)
         GOTO 900
!
!     LOWER HALF MAY NOT CHANGE FRAME OR PLOTTER OR CALL PLTSET
!
!     IF (NCMR .NE. LCMR  ) GO TO 925
      ELSEIF ( Xpap/=xlpap ) THEN
         WRITE (outape,99005) Swm
         GOTO 1300
      ELSEIF ( Ypap/=ylpap ) THEN
         WRITE (outape,99005) Swm
         GOTO 1300
      ELSE
         IF ( Nplt==lpltmd ) GOTO 900
         WRITE (outape,99005) Swm
         GOTO 1300
      ENDIF
   ENDIF
!
!     READ DATA PAIRS FROM INPUT DATA FILE FOR CURVE TO BE PLOTTED
!
 500  CALL read(*1400,*800,xyplt,Z,mb3,0,nact)
!
!     SET IFIN TO SHOW MORE DATA REMAINING TO BE READ FROM RECORD.
!     SET L AS INDEX TO LAST LEGITIMATE X VALUE OF DATA PAIRS IN CORE.
!
   ifin = 0
   l = mb3 - 1
 600  IF ( ix(l)==1 ) THEN
      l = l - 2
      IF ( l<=0 ) GOTO 700
   ENDIF
!
!     CONVERT DATA POINTS TO PLOTTER COUNTS AND PLOT SYMBOL AT EACH
!     LEGITIMATE POINT WHEN REQUIRED.
!
   IF ( Icrv/=0 ) CALL symbol(0,0,0,-1)
!
   isym(1) = iabs(Icrv) + Ncrv - 1
   isym(2) = 0
!
   DO i = 1 , l , 2
      IF ( ix(i)/=1 ) THEN
         IF ( x(i)<=xmaxs ) THEN
            IF ( x(i)>=xmins ) THEN
               IF ( logxs>0 ) x(i) = alog10(x(i))
               x(i) = xdr*x(i) + xc
               IF ( y(i)<=ymaxs ) THEN
                  IF ( y(i)>=ymins ) THEN
                     IF ( logys>0 ) y(i) = alog10(y(i))
                     y(i) = ydr*y(i) + yc
                     IF ( Icrv/=0 ) CALL symbol(x(i),y(i),isym,0)
                     CYCLE
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         ix(i) = 1
         ix(i+1) = 1
      ENDIF
   ENDDO
   IF ( Icrv/=0 ) CALL symbol(0,0,0,1)
!
!     PLOT LINES BETWEEN LEGITIMATE POINTS WHEN REQUIRED
!
   IF ( Icrv<0 .AND. Ipenn>0 ) Icrv = -Icrv
   IF ( Icrv>=0 ) THEN
      CALL line(0,0,0,0,0,-1)
      oldx = x(1)
      oldy = y(1)
      IF ( ipchg==1 ) THEN
         IF ( icpen==Ipenn ) icpen = Ipens - 1
         icpen = icpen + 1
      ELSE
         icpen = Ipsz
         IF ( Ipens/=0 ) THEN
            icpen = Ipens
            ipchg = 1
         ENDIF
      ENDIF
      DO i = 1 , l , 2
         IF ( ix(i)==1 ) THEN
            oldx = x(i+2)
            oldy = y(i+2)
         ELSE
            t1 = oldx - x(i)
            t2 = oldy - y(i)
            IF ( t1==0 ) THEN
               IF ( t2==0 ) CYCLE
            ENDIF
            CALL line(oldx,oldy,x(i),y(i),icpen,0)
            oldx = x(i)
            oldy = y(i)
         ENDIF
      ENDDO
      CALL line(0,0,0,0,0,1)
   ENDIF
 700  IF ( ifin/=0 ) GOTO 200
   GOTO 500
!
!     ALL DATA PAIRS IN CORE, SET IFIN TO SHOW NO MORE DATA REMAINS
!     FOR PRESENT CURVE.  IF ODD NUMBER OF DATA VALUES OUTPUT WARNING
!     MESSAGE AND CONTINUE.  SET L AS INDEX TO LAST X VALUE OF DATA
!     PAIRS.
!
 800  ifin = 1
   IF ( nact/=(nact/2)*2 ) THEN
      nact = nact - 1
      WRITE (outape,99002) Uwm , Nfrm , Ncrv
99002 FORMAT (A25,' 993, XYPLOT FOUND ODD NR. OF VALUES FOR DATA PAIRS',' IN FRAME',I5,', CURVE NR.',I5,'.  LAST VALUE IGNORED.')
      nlines = nlines + 2
      IF ( nlines>=nlpp ) CALL page
   ENDIF
   l = nact - 1
   IF ( l<=0 ) GOTO 200
   GOTO 600
!
!     SET VALUES FOR FULL FRAME PLOTTING
!
 900  ywmin = 0.
   Ylow = 4.*Ccv
   yxtr = (Ywmax+Ylow)/2.
!
!     START A NEW PLOT IF NECESSARY.
!
   IF ( Itbf>=0 ) THEN
      CALL sopen(*1200,Ltape,Z(mb2),Ipltbf)
      iopn = 1
      CALL stplot(Nfrm)
   ENDIF
   CALL print(0,0,0,0,0,-1)
   IF ( Itbf<0 ) THEN
!
!     MODIFY VALUE FOR LOWER HALF FRAME PLOTTING
!
      Yup = yxtr
      GOTO 1000
   ELSEIF ( Itbf/=0 ) THEN
!
!     MODIFY VALUE FOR UPPER HALF FRAME PLOTTING
!
      Ylow = yxtr
   ENDIF
!
!     SAVE YLOW AND EXPAND REGION SIZE FOR PRINTING OF TITLES.  RESTORE
!     YLOW AFTER PRINTING THE FOUR CURVE TITLES AT BOTTOM OF FRAME.
!
   xprm = Xwmin
   yprm = ywmin
   y1t = Ylow
   Ylow = ywmin
   CALL print(xprm,yprm,1,Clbl(1),32,0)
   yprm = yprm + Ccv
   CALL print(xprm,yprm,1,Sbtl(1),32,0)
   yprm = yprm + Ccv
   CALL print(xprm,yprm,1,Titl(1),32,0)
   yprm = yprm + Ccv
   CALL print(xprm,yprm,1,Cvtl(1),32,0)
   Ylow = y1t
!
!     OUTPUT X AND Y AXES TITLES
!
 1000 yprm = Ylow
   xprm = Xwmin + 8.*Cch
   CALL print(xprm,yprm,1,Xatl(1),32,0)
   yprm = Yup - 2*Ccv
   xprm = Xwmin
   CALL print(xprm,yprm,2,Yatl(1),32,0)
   CALL tipe(0,0,0,0,0,1)
!
!     MEANING OF SYMBOLS USED
!     XDR,XC,YDR,YC - FACTORS TO CONVERT ENGINEERING UNITS TO PLOTTER
!                     COUNTS IN X AND Y DIRECTIONS.
!     CONVERSION IS - PLOTTER COUNTS = ENG. UNITS * XDR  +  XC
!
!     JTC,J1T,J2T,J3T,J4T,J5T - TEMPORARY INTEGER VALUES
!     T1,T2,T3,T4,X1T,Y1T     - TEMPORARY REAL VALUES
!
!     TEST XMAX,XMIN,YMAX, AND YMIN FOR COMPATIBILITY
!
   n = 0
   DO
      dx = Xltc - Xtic
      dy = Yltc - Ytic
      IF ( dx>0.0 .AND. dy>0.0 ) EXIT
      IF ( n/=0 ) THEN
         n = 2
         IF ( dx<=0.0 ) THEN
            Xltc = Xtic + 10.0
            Xdtc = 2.0
            Nxtt = 0
         ENDIF
         IF ( dy<=0.0 ) THEN
            Yltc = Ytic + 10.0
            Ydtc = 2.0
            Nytt = 4
         ENDIF
      ELSE
         IF ( dx<=0.0 ) Xltc = Xtic + Xdtc*float(Nxtt+1)
         IF ( dy<=0.0 ) Yltc = Ytic + Ydtc*float(Nytt+1)
         n = 1
      ENDIF
!
!     PRINT WARNING (N=NO. OF PASSES TO CORRECT)
!
      WRITE (outape,99003) Uwm , n , Nfrm
99003 FORMAT (A25,' 997, NR.',I4,'.  FRAME NR.',I5,' INPUT DATA ','INCOMPATIBLE.  ASSUMPTIONS MAY PRODUCE INVALID PLOT.')
      nlines = nlines + 2
      IF ( nlines>=nlpp ) CALL page
      IF ( n/=1 ) EXIT
   ENDDO
!
!     SAVE XMAX, XMIN, YMAX, YMIN, LOGX AND LOGY FOR USE IF NEXT
!     I.D. RECORD IS NOT A NEW FRAME
!
   logxs = Logx
   logys = Logy
   xmins = Xtic
   xmaxs = Xltc
   ymins = Ytic
   ymaxs = Yltc
!
!     CALCULATE CONVERSION FACTORS
!
   xpl = Xwmax - 7.*Cch
   xps = Xwmin + 8.*Cch
   ypl = Yup - 2.*Ccv
   yps = Ylow + 2.*Ccv
!
!     PUT FRAME AT X AND Y MAXIMUM AND MINIMUM LIMITS
!
   IF ( Ixgd/=0 .OR. Iygd/=0 ) THEN
      CALL axis(0,0,0,0,0,-1)
      CALL axis(xps,yps,xps,ypl,Ipsz,0)
      CALL axis(xps,ypl,xpl,ypl,Ipsz,0)
      CALL axis(xpl,ypl,xpl,yps,Ipsz,0)
      CALL axis(xpl,yps,xps,yps,Ipsz,0)
      CALL axis(0,0,0,0,0,+1)
   ENDIF
   IF ( Logx>0 ) THEN
      Xtic = alog10(Xtic)
      Xltc = alog10(Xltc)
      dx = Xltc - Xtic
      IF ( Iyax==1 ) Yint = alog10(Yint)
   ENDIF
   IF ( Logy>0 ) THEN
      Ytic = alog10(Ytic)
      Yltc = alog10(Yltc)
      dy = Yltc - Ytic
      IF ( Ixax==1 ) Xint = alog10(Xint)
   ENDIF
   xdr = (xpl-xps)/dx
   xc = (xps*Xltc-xpl*Xtic)/dx
   ydr = (ypl-yps)/dy
   yc = (yps*Yltc-ypl*Ytic)/dy
!
!     PREPARE TO CREATE + LABEL ANY REQUESTED TIC MARKS IN THE
!     X-DIRECTION.
!
   IF ( Ittc/=0 .OR. Ixax==1 .OR. Ibtc/=0 .OR. Iygd/=0 ) THEN
      ndg = 0
      IF ( Logx>0 ) THEN
         ntt = Logx + 1
         xts = Xtic*xdr + xc
         dtc = 1.
         ndg = 4
         IF ( Logx<=10 ) THEN
            ill = lttp(Logx)
            nitk = lttn(Logx)
         ENDIF
      ELSE
         dtc = Xdtc
         IF ( dtc>0. .AND. Nxtt>0 ) THEN
            ntt = Nxtt
            xts = Xtic*xdr + xc
            IF ( Ittc>0 .OR. Ibtc>0 ) THEN
               ndg = min0(Nxdg+1,6)
               expo = ndg + Ixpr - 2
               nums = Xtic/10.**expo
               dl = dtc/10.**expo
               lstep = max0(Ixvs+1,1)
            ENDIF
         ELSE
            ntt = 0
         ENDIF
      ENDIF
!
      DO k = 1 , 3
         label = 1
!WKBR 9/93 LOG = XTIC - 1.0 + SIGN(0.1,XTIC)
         log = Xtic - 1.0 + sign(0.1,Xtic) - 1
         IF ( k==2 ) THEN
!
!     TICS ALONG THE X-AXIS.
!
            itc = 0
            IF ( Ixax==1 ) itc = -1
            IF ( itc/=0 ) THEN
               yt = Xint*ydr + yc
               CALL axis(0,0,0,0,0,-1)
               CALL axis(xpl,yt,xps,yt,Ipsz,0)
            ENDIF
         ELSEIF ( k==3 ) THEN
!
!     TICS + LABELS AT THE BOTTOM.
!
            itc = Ibtc
            yt = yps
            yl = yt - Ccv
         ELSE
!
!     TICS + LABELS AT THE TOP.
!
            itc = Ittc
            yt = ypl
            yl = yt + Ccv
         ENDIF
!
         IF ( itc/=0 .AND. ntt>0 ) THEN
            CALL tipe(0,0,0,0,0,-1)
            DO j = 1 , ntt
               r = xts + dtc*xdr*float(j-1)
               CALL tipe(r,yt,1,iplus,1,0)
               IF ( Logx>0 ) THEN
!
!     LABEL THIS LOGARITHMIC CYCLE TIC MARK.
!
                  log = log + 1
                  IF ( itc>=0 ) THEN
                     i = lep
                     IF ( log<0 ) i = lem
                     CALL print(r-Cch,yl,1,i,1,0)
                     CALL typint(r+2.*Cch,yl,1,iabs(log),0,0)
                  ENDIF
                  IF ( Logx<=10 .AND. j/=ntt ) THEN
!
!     CREATE + LABEL THE LOGARITHMIC INTRACYCLE TIC MARKS WITHIN THIS
!     CYCLE.
!
                     DO i = 1 , nitk
                        l = ill + i - 1
                        t = xdr*(alog10(tltv(l))+float(log)) + xc
                        CALL tipe(t,yt,1,iplus,1,0)
                        IF ( itc>=0 ) THEN
                           l = tltv(l) + .01
                           CALL typint(t,yl,1,l,1,0)
                        ENDIF
                     ENDDO
                  ENDIF
               ELSEIF ( itc>=0 .AND. label==j ) THEN
!
!     LABEL THIS LINEAR TIC MARK.
!
                  ifield = ndg
                  rnum = nums + dl*float(j-1)
                  IF ( rnum<0 ) THEN
                     ifield = ifield + 1
                  ELSEIF ( rnum==0 ) THEN
                     GOTO 1002
                  ENDIF
                  t = abs(rnum)
                  IF ( t<1.E-4 ) THEN
                     IF ( t>=5.E-5 ) THEN
                        rnum = sign(1.E-4,rnum)
                     ELSE
                        rnum = 0.
                     ENDIF
                  ENDIF
 1002             CALL typflt(r,yl,1,rnum,ifield,0)
                  label = label + lstep
                  IF ( label>ntt ) THEN
                     r = r + float(ifield)*Cch
                     CALL tipe(r,yl,1,ie,1,0)
                     CALL typint(r+Cch,yl,1,expo,0,0)
                  ENDIF
               ENDIF
!
            ENDDO
            CALL tipe(0,0,0,0,0,+1)
         ENDIF
      ENDDO
      IF ( Iygd/=0 .AND. ntt>0 ) THEN
!
!     DRAW THE Y-DIRECTION GRID NETWORK.
!
         CALL axis(0,0,0,0,0,-1)
!WKBR 9/93 LOG = XTIC - 1.0 + SIGN(0.1,XTIC)
         log = Xtic - 1.0 + sign(0.1,Xtic) - 1
         k = 1
         DO j = 1 , ntt
            k = -k
            r = xts + dtc*xdr*float(ntt-j)
            IF ( k>0 ) CALL axis(r,ypl,r,yps,Ipsz,0)
            IF ( k<0 ) CALL axis(r,yps,r,ypl,Ipsz,0)
            IF ( Logx>0 .AND. Logx<=10 .AND. j/=ntt ) THEN
!
!     DRAW THE Y-DIRECTION GRID LINES WITHIN THIS LOGARITHMIC CYCLE.
!
               log = log + 1
               DO i = 1 , nitk
                  l = ill + nitk - i
                  t = xdr*(alog10(tltv(l))+float(log)) + xc
                  k = -k
                  IF ( k>0 ) CALL axis(t,ypl,t,yps,Ipsz,0)
                  IF ( k<0 ) CALL axis(t,yps,t,ypl,Ipsz,0)
               ENDDO
            ENDIF
!
         ENDDO
         CALL axis(0,0,0,0,0,+1)
      ENDIF
   ENDIF
!
!     PREPARE TO CREATE + LABEL ANY REQUESTED TIC MARKS IN THE
!     Y-DIRECTION.
!
   IF ( Iltc/=0 .OR. Iyax==1 .OR. Irtc/=0 .OR. Ixgd/=0 ) THEN
      ndg = 0
      IF ( Logy>0 ) THEN
         ntt = Logy + 1
         yts = Ytic*ydr + yc
         dtc = 1.
         ndg = 4
         IF ( Logy<=10 ) THEN
            ill = lttp(Logy)
            nitk = lttn(Logy)
         ENDIF
      ELSE
         dtc = Ydtc
         IF ( dtc>0. .AND. Nytt>0 ) THEN
            ntt = Nytt
            yts = Ytic*ydr + yc
            IF ( Iltc>0 .OR. Irtc>0 ) THEN
               ndg = min0(Nydg+1,6)
               expo = ndg + Iypr - 2
               nums = Ytic/10.**expo
               dl = dtc/10.**expo
               lstep = max0(Iyvs+1,1)
            ENDIF
         ELSE
            ntt = 0
         ENDIF
      ENDIF
!
      DO k = 1 , 3
         label = 1
         log = Ytic - 1.0 + sign(0.1,Ytic)
         IF ( k==2 ) THEN
!
!     TICS ALONG THE Y-AXIS.
!
            itc = 0
            IF ( Iyax==1 ) itc = -1
            IF ( itc/=0 ) THEN
               xt = Yint*xdr + xc
               CALL axis(0,0,0,0,0,-1)
               CALL axis(xt,ypl,xt,yps,Ipsz,0)
            ENDIF
         ELSEIF ( k==3 ) THEN
!
!     TICS + LABELS ON THE RIGHT SIDE.
!
            itc = Irtc
            xt = xpl
            xl = xt + Cch
         ELSE
!
!     TICS + LABELS ON THE LEFT SIDE.
!
            itc = Iltc
            xt = xps
            xl = xt - Cch*float(ndg+1)
         ENDIF
!
         IF ( itc/=0 .AND. ntt>0 ) THEN
            CALL tipe(0,0,0,0,0,-1)
            DO j = 1 , ntt
               s = yts + dtc*ydr*float(j-1)
               CALL tipe(xt,s,1,iplus,1,0)
               IF ( Logy>0 ) THEN
!
!     LABEL THIS LOGARITHMIC CYCLE TIC MARK.
!
                  log = log + 1
                  IF ( itc>=0 ) THEN
                     i = lep
                     IF ( log<0 ) i = lem
                     CALL print(xl,s,1,i,1,0)
                     CALL typint(xl+3.*Cch,s,1,iabs(log),0,0)
                  ENDIF
                  IF ( Logy<=10 .AND. j/=ntt ) THEN
!
!     CREATE + LABEL THE LOGARITHMIC INTRACYCLE TIC MARKS WITHIN THIS
!     CYCLE.
!
                     DO i = 1 , nitk
                        l = ill + i - 1
                        t = ydr*(alog10(tltv(l))+float(log)) + yc
                        CALL tipe(xt,t,1,iplus,1,0)
                        IF ( itc>=0 ) THEN
                           l = tltv(l) + .01
                           CALL typint(xl,t,1,l,1,0)
                        ENDIF
                     ENDDO
                  ENDIF
               ELSEIF ( itc>=0 .AND. label==j ) THEN
!
!     LABEL THIS LINEAR TIC MARK.
!
                  ifield = ndg
                  rnum = nums + dl*float(j-1)
                  IF ( rnum<0 ) THEN
                     ifield = ifield + 1
                  ELSEIF ( rnum==0 ) THEN
                     GOTO 1004
                  ENDIF
                  t = abs(rnum)
                  IF ( t<1.E-4 ) THEN
                     IF ( t>=1.E-5 ) THEN
                        rnum = sign(1.E-4,rnum)
                     ELSE
                        rnum = 0.
                     ENDIF
                  ENDIF
 1004             CALL typflt(xl,s,1,rnum,-ifield,0)
                  label = label + lstep
                  ylabel = s
               ENDIF
!
            ENDDO
            IF ( itc>=0 .AND. Logy<=0 ) THEN
               CALL tipe(xl,ylabel-Ccv,1,ie,1,0)
               CALL typint(xl+Cch,ylabel-Ccv,1,expo,0,0)
            ENDIF
            CALL tipe(0,0,0,0,0,+1)
         ENDIF
      ENDDO
      IF ( Ixgd/=0 .AND. ntt>0 ) THEN
!
!     DRAW THE X-DIRECTION GRID NETWORK.
!
         CALL axis(0,0,0,0,0,-1)
         log = Ytic - 1.0 + sign(0.1,Ytic)
         k = 1
         DO j = 1 , ntt
            k = -k
            s = yts + dtc*ydr*float(ntt-j)
            IF ( k>0 ) CALL axis(xps,s,xpl,s,Ipsz,0)
            IF ( k<0 ) CALL axis(xpl,s,xps,s,Ipsz,0)
            IF ( Logy>0 .AND. Logy<=10 .AND. j/=ntt ) THEN
!
!     DRAW THE X-DIRECTION GRID LINES WITHIN THIS LOGARITHMIC CYCLE...
!
               log = log + 1
               DO i = 1 , nitk
                  l = ill + nitk - i
                  t = ydr*(alog10(tltv(l))+float(log)) + yc
                  k = -k
                  IF ( k>0 ) CALL axis(xps,t,xpl,t,Ipsz,0)
                  IF ( k<0 ) CALL axis(xpl,t,xps,t,Ipsz,0)
               ENDDO
            ENDIF
!
         ENDDO
         CALL axis(0,0,0,0,0,+1)
      ENDIF
   ENDIF
   GOTO 500
!
!     OUTPUT WARNING NESSAGES, CLOSE INPUT FILE AND PLOT TAPE AND RETURN
!
 1100 RETURN
 1200 WRITE (outape,99004) Uwm , Ltape
99004 FORMAT (A25,' 994, XYPLOT OUTPUT FILE NAME ',A4,' NOT FOUND.','  XYPLOT ABANDONED.')
 1300 nlines = nlines + 2
   IF ( nlines>=nlpp ) CALL page
 1400 CALL close(xyplt,iclsrw)
   IF ( iopn/=0 ) CALL stplot(-1)
99005 FORMAT (A27,' 998, XYPLOT PLOTTER OR FRAME MAY NOT CHANGE FOR ','LOWER FRAME.  XYPLOT ABANDONED.')
END SUBROUTINE xyplot
