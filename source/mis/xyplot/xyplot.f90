!*==xyplot.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xyplot
   IMPLICIT NONE
   USE c_machin
   USE c_pltdat
   USE c_system
   USE c_xmssg
   USE c_xxparm
   USE c_xyplin
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
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
   mb1 = korsz(z) - sysbuf
   ipchg = 0
   iopn = 0
   CALL open(*1100,xyplt,z(mb1),irdrw)
 100  CALL fwdrec(*1400,xyplt)
   nerr = 0
!
!     READ I.D. RECORD ON INPUT DATA FILE
!
 200  CALL read(*1400,*400,xyplt,idsb,nrwd+1,1,nact)
 300  nerr = nerr + 1
   IF ( nerr<5 ) GOTO 200
   WRITE (outape,99001) uwm
!
99001 FORMAT (A25,' 992, XYPLOT INPUT DATA FILE ID. RECORDS TOO SHORT.','  XYPLOT ABANDONED.')
   GOTO 1300
 400  IF ( nact/=nrwd ) GOTO 300
!
!     SKIP DATA IF IT WAS FOR THE PAPERPLOTER ONLY
!
   IF ( d4(2)<=0 ) GOTO 100
   IF ( nwfr/=0 ) THEN
!
!     NEW AXIS, LABELS, ETC. ARE NEEDED.
!
!     NASTRAN PLOTTING SOFTWARE INITIALIZATION.
!
      IF ( itbf>=0 .AND. iopn/=0 ) CALL stplot(-1)
      ipltnr = rshift(nplt,ihalf)
      model = nplt - lshift(ipltnr,ihalf) - 100
      IF ( ncmr>0 ) icmra = ncmr
      ifskp = iskp
      cscale = chrscl
      IF ( cscale<1. ) cscale = 1.0
      IF ( xpap>0. ) papszx = xpap
      IF ( ypap>0. ) papszy = ypap
      DO i = 1 , npens
         jpsz(i) = ipsz
      ENDDO
      IF ( itbf>=0 ) THEN
!
         CALL pltset
         lcmr = ncmr
         xlpap = xpap
         ylpap = ypap
         lpltmd = nplt
         mb2 = mb1 - ipltbf
         mb3 = 2*((mb2-1)/2)
         GOTO 900
!
!     LOWER HALF MAY NOT CHANGE FRAME OR PLOTTER OR CALL PLTSET
!
!     IF (NCMR .NE. LCMR  ) GO TO 925
      ELSEIF ( xpap/=xlpap ) THEN
         WRITE (outape,99005) swm
         GOTO 1300
      ELSEIF ( ypap/=ylpap ) THEN
         WRITE (outape,99005) swm
         GOTO 1300
      ELSE
         IF ( nplt==lpltmd ) GOTO 900
         WRITE (outape,99005) swm
         GOTO 1300
      ENDIF
   ENDIF
!
!     READ DATA PAIRS FROM INPUT DATA FILE FOR CURVE TO BE PLOTTED
!
 500  CALL read(*1400,*800,xyplt,z,mb3,0,nact)
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
   IF ( icrv/=0 ) CALL symbol(0,0,0,-1)
!
   isym(1) = iabs(icrv) + ncrv - 1
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
                     IF ( icrv/=0 ) CALL symbol(x(i),y(i),isym,0)
                     CYCLE
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         ix(i) = 1
         ix(i+1) = 1
      ENDIF
   ENDDO
   IF ( icrv/=0 ) CALL symbol(0,0,0,1)
!
!     PLOT LINES BETWEEN LEGITIMATE POINTS WHEN REQUIRED
!
   IF ( icrv<0 .AND. ipenn>0 ) icrv = -icrv
   IF ( icrv>=0 ) THEN
      CALL line(0,0,0,0,0,-1)
      oldx = x(1)
      oldy = y(1)
      IF ( ipchg==1 ) THEN
         IF ( icpen==ipenn ) icpen = ipens - 1
         icpen = icpen + 1
      ELSE
         icpen = ipsz
         IF ( ipens/=0 ) THEN
            icpen = ipens
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
 700  IF ( ifin==0 ) GOTO 500
   GOTO 200
!
!     ALL DATA PAIRS IN CORE, SET IFIN TO SHOW NO MORE DATA REMAINS
!     FOR PRESENT CURVE.  IF ODD NUMBER OF DATA VALUES OUTPUT WARNING
!     MESSAGE AND CONTINUE.  SET L AS INDEX TO LAST X VALUE OF DATA
!     PAIRS.
!
 800  ifin = 1
   IF ( nact/=(nact/2)*2 ) THEN
      nact = nact - 1
      WRITE (outape,99002) uwm , nfrm , ncrv
99002 FORMAT (A25,' 993, XYPLOT FOUND ODD NR. OF VALUES FOR DATA PAIRS',' IN FRAME',I5,', CURVE NR.',I5,'.  LAST VALUE IGNORED.')
      nlines = nlines + 2
      IF ( nlines>=nlpp ) CALL page
   ENDIF
   l = nact - 1
   IF ( l>0 ) GOTO 600
   GOTO 200
!
!     SET VALUES FOR FULL FRAME PLOTTING
!
 900  ywmin = 0.
   ylow = 4.*ccv
   yxtr = (ywmax+ylow)/2.
!
!     START A NEW PLOT IF NECESSARY.
!
   IF ( itbf>=0 ) THEN
      CALL sopen(*1200,ltape,z(mb2),ipltbf)
      iopn = 1
      CALL stplot(nfrm)
   ENDIF
   CALL print(0,0,0,0,0,-1)
   IF ( itbf<0 ) THEN
!
!     MODIFY VALUE FOR LOWER HALF FRAME PLOTTING
!
      yup = yxtr
      GOTO 1000
   ELSEIF ( itbf/=0 ) THEN
!
!     MODIFY VALUE FOR UPPER HALF FRAME PLOTTING
!
      ylow = yxtr
   ENDIF
!
!     SAVE YLOW AND EXPAND REGION SIZE FOR PRINTING OF TITLES.  RESTORE
!     YLOW AFTER PRINTING THE FOUR CURVE TITLES AT BOTTOM OF FRAME.
!
   xprm = xwmin
   yprm = ywmin
   y1t = ylow
   ylow = ywmin
   CALL print(xprm,yprm,1,clbl(1),32,0)
   yprm = yprm + ccv
   CALL print(xprm,yprm,1,sbtl(1),32,0)
   yprm = yprm + ccv
   CALL print(xprm,yprm,1,titl(1),32,0)
   yprm = yprm + ccv
   CALL print(xprm,yprm,1,cvtl(1),32,0)
   ylow = y1t
!
!     OUTPUT X AND Y AXES TITLES
!
 1000 yprm = ylow
   xprm = xwmin + 8.*cch
   CALL print(xprm,yprm,1,xatl(1),32,0)
   yprm = yup - 2*ccv
   xprm = xwmin
   CALL print(xprm,yprm,2,yatl(1),32,0)
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
      dx = xltc - xtic
      dy = yltc - ytic
      IF ( dx>0.0 .AND. dy>0.0 ) EXIT
      IF ( n/=0 ) THEN
         n = 2
         IF ( dx<=0.0 ) THEN
            xltc = xtic + 10.0
            xdtc = 2.0
            nxtt = 0
         ENDIF
         IF ( dy<=0.0 ) THEN
            yltc = ytic + 10.0
            ydtc = 2.0
            nytt = 4
         ENDIF
      ELSE
         IF ( dx<=0.0 ) xltc = xtic + xdtc*float(nxtt+1)
         IF ( dy<=0.0 ) yltc = ytic + ydtc*float(nytt+1)
         n = 1
      ENDIF
!
!     PRINT WARNING (N=NO. OF PASSES TO CORRECT)
!
      WRITE (outape,99003) uwm , n , nfrm
99003 FORMAT (A25,' 997, NR.',I4,'.  FRAME NR.',I5,' INPUT DATA ','INCOMPATIBLE.  ASSUMPTIONS MAY PRODUCE INVALID PLOT.')
      nlines = nlines + 2
      IF ( nlines>=nlpp ) CALL page
      IF ( n/=1 ) EXIT
   ENDDO
!
!     SAVE XMAX, XMIN, YMAX, YMIN, LOGX AND LOGY FOR USE IF NEXT
!     I.D. RECORD IS NOT A NEW FRAME
!
   logxs = logx
   logys = logy
   xmins = xtic
   xmaxs = xltc
   ymins = ytic
   ymaxs = yltc
!
!     CALCULATE CONVERSION FACTORS
!
   xpl = xwmax - 7.*cch
   xps = xwmin + 8.*cch
   ypl = yup - 2.*ccv
   yps = ylow + 2.*ccv
!
!     PUT FRAME AT X AND Y MAXIMUM AND MINIMUM LIMITS
!
   IF ( ixgd/=0 .OR. iygd/=0 ) THEN
      CALL axis(0,0,0,0,0,-1)
      CALL axis(xps,yps,xps,ypl,ipsz,0)
      CALL axis(xps,ypl,xpl,ypl,ipsz,0)
      CALL axis(xpl,ypl,xpl,yps,ipsz,0)
      CALL axis(xpl,yps,xps,yps,ipsz,0)
      CALL axis(0,0,0,0,0,+1)
   ENDIF
   IF ( logx>0 ) THEN
      xtic = alog10(xtic)
      xltc = alog10(xltc)
      dx = xltc - xtic
      IF ( iyax==1 ) yint = alog10(yint)
   ENDIF
   IF ( logy>0 ) THEN
      ytic = alog10(ytic)
      yltc = alog10(yltc)
      dy = yltc - ytic
      IF ( ixax==1 ) xint = alog10(xint)
   ENDIF
   xdr = (xpl-xps)/dx
   xc = (xps*xltc-xpl*xtic)/dx
   ydr = (ypl-yps)/dy
   yc = (yps*yltc-ypl*ytic)/dy
!
!     PREPARE TO CREATE + LABEL ANY REQUESTED TIC MARKS IN THE
!     X-DIRECTION.
!
   IF ( ittc/=0 .OR. ixax==1 .OR. ibtc/=0 .OR. iygd/=0 ) THEN
      ndg = 0
      IF ( logx>0 ) THEN
         ntt = logx + 1
         xts = xtic*xdr + xc
         dtc = 1.
         ndg = 4
         IF ( logx<=10 ) THEN
            ill = lttp(logx)
            nitk = lttn(logx)
         ENDIF
      ELSE
         dtc = xdtc
         IF ( dtc>0. .AND. nxtt>0 ) THEN
            ntt = nxtt
            xts = xtic*xdr + xc
            IF ( ittc>0 .OR. ibtc>0 ) THEN
               ndg = min0(nxdg+1,6)
               expo = ndg + ixpr - 2
               nums = xtic/10.**expo
               dl = dtc/10.**expo
               lstep = max0(ixvs+1,1)
            ENDIF
         ELSE
            ntt = 0
         ENDIF
      ENDIF
!
      DO k = 1 , 3
         label = 1
!WKBR 9/93 LOG = XTIC - 1.0 + SIGN(0.1,XTIC)
         log = xtic - 1.0 + sign(0.1,xtic) - 1
         IF ( k==2 ) THEN
!
!     TICS ALONG THE X-AXIS.
!
            itc = 0
            IF ( ixax==1 ) itc = -1
            IF ( itc/=0 ) THEN
               yt = xint*ydr + yc
               CALL axis(0,0,0,0,0,-1)
               CALL axis(xpl,yt,xps,yt,ipsz,0)
            ENDIF
         ELSEIF ( k==3 ) THEN
!
!     TICS + LABELS AT THE BOTTOM.
!
            itc = ibtc
            yt = yps
            yl = yt - ccv
         ELSE
!
!     TICS + LABELS AT THE TOP.
!
            itc = ittc
            yt = ypl
            yl = yt + ccv
         ENDIF
!
         IF ( itc/=0 .AND. ntt>0 ) THEN
            CALL tipe(0,0,0,0,0,-1)
            DO j = 1 , ntt
               r = xts + dtc*xdr*float(j-1)
               CALL tipe(r,yt,1,iplus,1,0)
               IF ( logx>0 ) THEN
!
!     LABEL THIS LOGARITHMIC CYCLE TIC MARK.
!
                  log = log + 1
                  IF ( itc>=0 ) THEN
                     i = lep
                     IF ( log<0 ) i = lem
                     CALL print(r-cch,yl,1,i,1,0)
                     CALL typint(r+2.*cch,yl,1,iabs(log),0,0)
                  ENDIF
                  IF ( logx<=10 .AND. j/=ntt ) THEN
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
                     r = r + float(ifield)*cch
                     CALL tipe(r,yl,1,ie,1,0)
                     CALL typint(r+cch,yl,1,expo,0,0)
                  ENDIF
               ENDIF
!
            ENDDO
            CALL tipe(0,0,0,0,0,+1)
         ENDIF
      ENDDO
      IF ( iygd/=0 .AND. ntt>0 ) THEN
!
!     DRAW THE Y-DIRECTION GRID NETWORK.
!
         CALL axis(0,0,0,0,0,-1)
!WKBR 9/93 LOG = XTIC - 1.0 + SIGN(0.1,XTIC)
         log = xtic - 1.0 + sign(0.1,xtic) - 1
         k = 1
         DO j = 1 , ntt
            k = -k
            r = xts + dtc*xdr*float(ntt-j)
            IF ( k>0 ) CALL axis(r,ypl,r,yps,ipsz,0)
            IF ( k<0 ) CALL axis(r,yps,r,ypl,ipsz,0)
            IF ( logx>0 .AND. logx<=10 .AND. j/=ntt ) THEN
!
!     DRAW THE Y-DIRECTION GRID LINES WITHIN THIS LOGARITHMIC CYCLE.
!
               log = log + 1
               DO i = 1 , nitk
                  l = ill + nitk - i
                  t = xdr*(alog10(tltv(l))+float(log)) + xc
                  k = -k
                  IF ( k>0 ) CALL axis(t,ypl,t,yps,ipsz,0)
                  IF ( k<0 ) CALL axis(t,yps,t,ypl,ipsz,0)
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
   IF ( iltc/=0 .OR. iyax==1 .OR. irtc/=0 .OR. ixgd/=0 ) THEN
      ndg = 0
      IF ( logy>0 ) THEN
         ntt = logy + 1
         yts = ytic*ydr + yc
         dtc = 1.
         ndg = 4
         IF ( logy<=10 ) THEN
            ill = lttp(logy)
            nitk = lttn(logy)
         ENDIF
      ELSE
         dtc = ydtc
         IF ( dtc>0. .AND. nytt>0 ) THEN
            ntt = nytt
            yts = ytic*ydr + yc
            IF ( iltc>0 .OR. irtc>0 ) THEN
               ndg = min0(nydg+1,6)
               expo = ndg + iypr - 2
               nums = ytic/10.**expo
               dl = dtc/10.**expo
               lstep = max0(iyvs+1,1)
            ENDIF
         ELSE
            ntt = 0
         ENDIF
      ENDIF
!
      DO k = 1 , 3
         label = 1
         log = ytic - 1.0 + sign(0.1,ytic)
         IF ( k==2 ) THEN
!
!     TICS ALONG THE Y-AXIS.
!
            itc = 0
            IF ( iyax==1 ) itc = -1
            IF ( itc/=0 ) THEN
               xt = yint*xdr + xc
               CALL axis(0,0,0,0,0,-1)
               CALL axis(xt,ypl,xt,yps,ipsz,0)
            ENDIF
         ELSEIF ( k==3 ) THEN
!
!     TICS + LABELS ON THE RIGHT SIDE.
!
            itc = irtc
            xt = xpl
            xl = xt + cch
         ELSE
!
!     TICS + LABELS ON THE LEFT SIDE.
!
            itc = iltc
            xt = xps
            xl = xt - cch*float(ndg+1)
         ENDIF
!
         IF ( itc/=0 .AND. ntt>0 ) THEN
            CALL tipe(0,0,0,0,0,-1)
            DO j = 1 , ntt
               s = yts + dtc*ydr*float(j-1)
               CALL tipe(xt,s,1,iplus,1,0)
               IF ( logy>0 ) THEN
!
!     LABEL THIS LOGARITHMIC CYCLE TIC MARK.
!
                  log = log + 1
                  IF ( itc>=0 ) THEN
                     i = lep
                     IF ( log<0 ) i = lem
                     CALL print(xl,s,1,i,1,0)
                     CALL typint(xl+3.*cch,s,1,iabs(log),0,0)
                  ENDIF
                  IF ( logy<=10 .AND. j/=ntt ) THEN
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
            IF ( itc>=0 .AND. logy<=0 ) THEN
               CALL tipe(xl,ylabel-ccv,1,ie,1,0)
               CALL typint(xl+cch,ylabel-ccv,1,expo,0,0)
            ENDIF
            CALL tipe(0,0,0,0,0,+1)
         ENDIF
      ENDDO
      IF ( ixgd/=0 .AND. ntt>0 ) THEN
!
!     DRAW THE X-DIRECTION GRID NETWORK.
!
         CALL axis(0,0,0,0,0,-1)
         log = ytic - 1.0 + sign(0.1,ytic)
         k = 1
         DO j = 1 , ntt
            k = -k
            s = yts + dtc*ydr*float(ntt-j)
            IF ( k>0 ) CALL axis(xps,s,xpl,s,ipsz,0)
            IF ( k<0 ) CALL axis(xpl,s,xps,s,ipsz,0)
            IF ( logy>0 .AND. logy<=10 .AND. j/=ntt ) THEN
!
!     DRAW THE X-DIRECTION GRID LINES WITHIN THIS LOGARITHMIC CYCLE...
!
               log = log + 1
               DO i = 1 , nitk
                  l = ill + nitk - i
                  t = ydr*(alog10(tltv(l))+float(log)) + yc
                  k = -k
                  IF ( k>0 ) CALL axis(xps,t,xpl,t,ipsz,0)
                  IF ( k<0 ) CALL axis(xpl,t,xps,t,ipsz,0)
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
 1200 WRITE (outape,99004) uwm , ltape
99004 FORMAT (A25,' 994, XYPLOT OUTPUT FILE NAME ',A4,' NOT FOUND.','  XYPLOT ABANDONED.')
 1300 nlines = nlines + 2
   IF ( nlines>=nlpp ) CALL page
 1400 CALL close(xyplt,iclsrw)
   IF ( iopn/=0 ) CALL stplot(-1)
99005 FORMAT (A27,' 998, XYPLOT PLOTTER OR FRAME MAY NOT CHANGE FOR ','LOWER FRAME.  XYPLOT ABANDONED.')
END SUBROUTINE xyplot
