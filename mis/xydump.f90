
SUBROUTINE xydump(Outfil,Type)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Blkcom , Idoutr(300) , Rbuf(100) , Rz(1) , Value(60) , Vari(3)
   INTEGER Buf(100) , Center , File , Iat , Idin(153) , Idout(300) , Ifile , Ivalue(60) , Machx , Major , Nat , Nbots , Ncard ,     &
         & Nframe , Ntops , Steps , Subc(5) , Tcurve(32) , Two1(32) , Vecid(5) , Vector , Xaxis(32) , Yaxis(32) , Ybaxis(32) ,      &
         & Ytaxis(32) , Z(1)
   LOGICAL Outopn , Paplot , Plot , Print , Punch , Random
   COMMON /blank / Blkcom , Vari , Nframe , Ncard
   COMMON /machin/ Machx
   COMMON /two   / Two1
   COMMON /xywork/ File , Tcurve , Ntops , Print , Ifile , Xaxis , Nbots , Plot , Vector , Yaxis , Vecid , Punch , Major , Ytaxis , &
                 & Subc , Center , Random , Ybaxis , Idin , Buf , Ivalue , Iat , Idout , Outopn , Steps , Nat , Paplot
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Outfil , Type
!
! Local variable declarations
!
   INTEGER begin , curve , eor , i , i1 , i2 , icont , iflag , ipair , is1 , itemp , j , j1 , j2 , k , limit(2,3) , m , mcount , n ,&
         & noeor , npaplt , nx1 , nx2 , oneone(2) , xcycle , ycycle(2)
   LOGICAL dec , intore , null , ok , on , ones
   REAL diff , temp , xinc , xmax , xmin , y1 , y2 , ylimit(2,3) , ymax(2) , ymin(2)
!
! End of declarations
!
!
   EQUIVALENCE (limit(1,1),ylimit(1,1)) , (Z(1),Rz(1)) , (Buf(1),Rbuf(1)) , (Idout(1),Idoutr(1)) , (Ivalue(1),Value(1))
   DATA oneone/1 , 1/ , eor/1/ , noeor/0/
   DATA npaplt/0/
!
!     SET MINIMUM X-DIFFERENCE
!
!     BUT FIRST CONVERT X FROM INTERGER TO REAL IF NECESSARY.
!
   intore = .FALSE.
   dec = Machx==5 .OR. Machx==6 .OR. Machx==21
   j = 1
   is1 = Steps - 1
!
!     NOW SEARCH LIST FOR FIRST NON-ZERO ENTRY
!
   DO WHILE ( Z(Iat+j)==0 )
      j = j + 1
      IF ( j>is1 ) GOTO 100
   ENDDO
!
!              UNIVAC             CDC             CRAY
   IF ( Machx/=3 .AND. Machx/=4 .AND. Machx/=12 ) THEN
!
!     IBM, VAX, UNIX
!
      IF ( .NOT.dec .AND. iabs(Z(Iat+j))>Two1(9) ) GOTO 100
      IF ( dec .AND. (Z(Iat+j)<1 .OR. Z(Iat+j)>127) ) GOTO 100
   ELSEIF ( iabs(Z(Iat+j))>Two1(2) ) THEN
      GOTO 100
   ENDIF
   intore = .TRUE.
   IF ( j==1 ) Rz(Iat+j) = Z(Iat+j)
!
 100  ok = .FALSE.
   DO i = 1 , is1
      j = Iat + i
      IF ( intore ) Rz(j+1) = Z(j+1)
      diff = Rz(j+1) - Rz(j)
      IF ( .NOT.ok ) THEN
         IF ( diff/=0.0 ) THEN
            xinc = diff
            ok = .TRUE.
         ENDIF
      ELSEIF ( diff/=0.0 ) THEN
         xinc = amin1(xinc,diff)
      ENDIF
   ENDDO
   IF ( .NOT.ok ) xinc = 1.0
!
!     SET XMIN AND XMAX FOR ALL DATA
!
   xcycle = 0
   ycycle(1) = 0
   ycycle(2) = 0
   xmin = Rz(Iat+1)
   j = Iat + Steps
   xmax = Rz(j)
!
!     REDUCE THESE LIMITS TO USER SPECIFIED LIMITS
!
   IF ( Ivalue(1)/=1 ) xmin = (Value(1))
   IF ( Ivalue(2)/=1 ) xmax = (Value(2))
!
!     FURTHER EXPAND XLIMITS TO INCLUDE Y-AXIS INTERCEPT
!
   IF ( Ivalue(9)/=1 ) THEN
      IF ( Ivalue(36)==1 .AND. Value(9)<=0.0 ) GOTO 200
      xmin = amin1(xmin,Value(9))
      xmax = amax1(xmax,Value(9))
   ENDIF
!
!     IF X-DIRECTION IS LOG AND XMIN IS NEGATIVE OR ZERO, SET YMIN
!     EQUAL TO THE SMALLEST NON-ZERO POSITIVE VALUE
!
   IF ( Ivalue(36)/=1 ) GOTO 500
 200  IF ( xmin<=0.0 ) THEN
      DO i = 1 , Steps
         j = Iat + i
         IF ( Rz(j)>0.0 ) GOTO 300
      ENDDO
      xmin = 1.0
      xmax = 10.
   ENDIF
   GOTO 400
 300  xmin = Rz(j)
 400  CALL xylog(xmin,xmax,xcycle)
!
!     SWITCH XMIN AND XMAX (SAFETY CHECK) IF NECESSARY
!
 500  IF ( xmin>xmax ) THEN
      temp = xmin
      xmin = xmax
      xmax = temp
   ENDIF
!
!     USING XMIN AND XMAX AS LIMITS DETERMINE Y-LIMITS FOR TOP AND
!     BOTTOM.
!
!     I1 = FIRST STEP WITHIN XMIN TO XMAX
!     I2 = LAST  STEP WITHIN XMIN TO XMAX
!
!     FIRST FIND I1 AND I2
!
   DO i = 1 , Steps
      j = Iat + i
      IF ( xmin<=Rz(j) .AND. Rz(j)<=xmax ) GOTO 600
   ENDDO
   i1 = 0
   GOTO 700
 600  i1 = i
   j = Iat + Steps + 1
   DO i = 1 , Steps
      j = j - 1
      IF ( xmin<=Rz(j) .AND. Rz(j)<=xmax ) GOTO 800
   ENDDO
 700  i2 = 0
   GOTO 900
 800  i2 = j - Iat
 900  IF ( i1/=0 ) THEN
      m = 1
      IF ( Nbots/=0 ) m = 2
      begin = Iat
      DO i = 1 , m
         limit(i,1) = 1
         limit(i,2) = 1
         limit(i,3) = 1
         DO j = 1 , Ntops
            k = j*Steps + begin
            j1 = k + i1
            j2 = k + i2
            IF ( limit(i,1)/=1 ) GOTO 920
!
!     FIND FIRST NON-INTEGER 1 VALUE
!
            DO k = j1 , j2
               IF ( Z(k)/=1 ) GOTO 910
            ENDDO
            CYCLE
 910        ylimit(i,1) = Rz(k)
            ylimit(i,2) = Rz(k)
 920        DO k = j1 , j2
               IF ( Z(k)/=1 ) THEN
                  ylimit(i,1) = amin1(Rz(k),ylimit(i,1))
                  ylimit(i,2) = amax1(Rz(k),ylimit(i,2))
                  IF ( Rz(k)>0.0 ) THEN
                     IF ( limit(i,3)==1 ) ylimit(i,3) = Rz(k)
                     ylimit(i,3) = amin1(ylimit(i,3),Rz(k))
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         begin = Center
!
!     DEFAULT YLIMITS IF ALL CURVES NULL
!
         IF ( limit(i,1)==1 ) THEN
            ylimit(i,1) = 0.0
            ylimit(i,2) = 100.
         ENDIF
         IF ( limit(i,3)==1 ) ylimit(i,3) = 10.0
!
      ENDDO
!
!     SET FINAL Y-LIMITS FOR UPPER AND LOWER CURVES
!
!
!     K=1 IMPLIES WHOLE CURVES
!     K=2 IMPLIES UPPER AND LOWER CURVES
!
      k = 1
      IF ( Nbots>0 ) k = 2
      DO i = 1 , k
         ymin(i) = ylimit(i,1)
         ymax(i) = ylimit(i,2)
!
!     REDUCE THESE CURVE LIMITS TO LIMITS SET BY USER
!
         itemp = 2*(i+k)
         IF ( Ivalue(itemp-1)/=1 ) ymin(i) = (Value(itemp-1))
         IF ( Ivalue(itemp)/=1 ) ymax(i) = (Value(itemp))
!
!     FURTHER EXPAND LIMITS TO INCLUDE X-AXIS
!
         itemp = i + k
         IF ( Ivalue(itemp+8)/=1 ) THEN
            IF ( Ivalue(itemp+35)==1 .AND. Value(itemp+8)<=0.E0 ) GOTO 940
            ymin(i) = amin1(ymin(i),Value(itemp+8))
            ymax(i) = amax1(ymax(i),Value(itemp+8))
         ENDIF
!
!     IF Y-DIRECTION IS LOG AND YMIN IS NEGATIVE OR ZERO SET YMIN
!     EQUAL TO SMALLEST POSITIVE CURVE VALUE WITHIN XLIMITS
!
         IF ( Ivalue(itemp+35)/=1 ) GOTO 960
 940     IF ( ymin(i)<=0.0 ) ymin(i) = ylimit(i,3)
         CALL xylog(ymin(i),ymax(i),ycycle(i))
!
!     SWITCH YMIN AND YMAX (SAFETY CHECK) IF NECESSARY
!
 960     IF ( ymin(i)>ymax(i) ) THEN
            temp = ymin(i)
            ymin(i) = ymax(i)
            ymax(i) = temp
         ENDIF
      ENDDO
   ELSE
!
!     FIND FOLLOWING VALUES FOR CURVES AS A GROUP
!
!    YLIMIT(1,1)=YMIN TOP, YLIMIT(1,2)=YMAX TOP, YLIMIT(1,3)=MIN POS TOP
!    YLIMIT(2,1)=YMIN BOT, YLIMIT(2,2)=YMAX BOT, YLIMIT(2,3)=MIN POS BOT
!
      ymin(1) = 0.0
      ymin(2) = 0.0
      ymax(1) = 10.
      ymax(2) = 10.
   ENDIF
!
!     ALL CURVE LIMITS HAVE NOW BEEN SET FOR THIS FRAME
!
!
!     OUTPUT EACH CURVE AND AN IDOUT RECORD IF PLOTS = .TRUE.
!
!     FILL IDOUT
!
   DO i = 1 , 300
      Idout(i) = 0
   ENDDO
   IF ( Plot .AND. Outopn ) Nframe = Nframe + 1
   Idout(1) = Subc(File)
   Idout(2) = Nframe
   Idout(6) = Vector
   Idout(9) = Ivalue(45)
   IF ( Ivalue(43)==0 ) Value(43) = 1.0
   Idout(43) = Ivalue(43)
   Idoutr(10) = xinc
   Idout(245) = Type
   Idout(246) = Steps
   Idoutr(282) = Value(57)
   IF ( Idoutr(282)<1.0 ) Idoutr(282) = 1.0
   Idout(283) = Ivalue(50)
   IF ( Ivalue(47)==3 ) Idout(283) = Ivalue(41)
   Idout(284) = Ivalue(47)
   Idout(285) = Ivalue(48)
   Idout(286) = Ivalue(49)
   Idout(287) = Ivalue(46)
   Idout(44) = Ivalue(58)
   Idout(45) = Ivalue(59)
   IF ( Print ) Idout(288) = 1
   IF ( Plot ) Idout(289) = 1
   IF ( Paplot ) THEN
      IF ( .NOT.Plot ) Idout(289) = -1
      IF ( Plot ) Idout(289) = 2
      npaplt = npaplt + 1
      Idout(281) = npaplt
   ENDIF
   on = .FALSE.
   IF ( Plot .OR. Paplot ) on = .TRUE.
   IF ( Punch ) Idout(290) = 1
   DO i = 51 , 146
      Idout(i) = Idin(i)
   ENDDO
!
!     BRANCH ON TOP, BOTTOM, OR WHOLE CURVE (FIRST WILL BE TOP OR WHOLE)
!
   i = 3
   IF ( Z(i)==0 .OR. Random ) THEN
!
!     WHOLE CURVE ID
!
      curve = 0
      Idout(7) = 0
      Idout(8) = 1
      Idoutr(11) = xmin
      Idoutr(12) = xmax
      Idoutr(13) = ymin(1)
      Idoutr(14) = ymax(1)
      iflag = 0
      IF ( intore ) iflag = 1
      CALL xytics(Idout(15),Idoutr(15),Ivalue(17),Idout(11),Idout(12),Ivalue(21),xcycle,iflag)
      CALL xytics(Idout(23),Idoutr(23),Ivalue(18),Idout(13),Idout(14),Ivalue(22),ycycle(1),0)
      Idout(31) = Ivalue(33) + Ivalue(25)
      Idout(32) = Ivalue(33) + Ivalue(26)
      Idout(33) = Ivalue(33) + Ivalue(27)
      Idout(34) = Ivalue(33) + Ivalue(28)
      Idout(35) = xcycle
      Idout(36) = ycycle(1)
      Idout(37) = Ivalue(13)
      Idout(38) = Ivalue(10)
      IF ( Idout(38)==1 ) Idout(38) = 0.0
      IF ( Idoutr(38)<ymin(1) ) Idout(37) = 0
      Idout(39) = Ivalue(14)
      Idout(40) = Ivalue(9)
      IF ( Idout(40)==1 ) Idoutr(40) = 0.0
      IF ( Idoutr(40)<xmin ) Idout(39) = 0
      Idout(41) = Ivalue(40)
      Idout(243) = Ivalue(51)
      Idout(244) = Ivalue(52)
      DO i = 1 , 32
         Idout(i+146) = Tcurve(i)
         Idout(i+178) = Xaxis(i)
         Idout(i+210) = Yaxis(i)
      ENDDO
   ELSE
!
!     TOP CURVE ID
!
      curve = 0
      Idout(7) = 1
      Idout(8) = 1
      Idoutr(11) = xmin
      Idoutr(12) = xmax
      Idoutr(13) = ymin(1)
      Idoutr(14) = ymax(1)
      iflag = 0
      IF ( intore ) iflag = 1
      CALL xytics(Idout(15),Idoutr(15),Ivalue(17),Idout(11),Idout(12),Ivalue(21),xcycle,iflag)
      CALL xytics(Idout(23),Idoutr(23),Ivalue(19),Idout(13),Idout(14),Ivalue(23),ycycle(1),0)
      Idout(31) = Ivalue(34) + Ivalue(25)
      Idout(32) = Ivalue(34) + Ivalue(26)
      Idout(33) = Ivalue(34) + Ivalue(29)
      Idout(34) = Ivalue(34) + Ivalue(30)
      Idout(35) = xcycle
      Idout(36) = ycycle(1)
      Idout(37) = Ivalue(15)
      Idout(38) = Ivalue(11)
      IF ( Idout(38)==1 ) Idoutr(38) = 0.0
      IF ( Idoutr(38)<ymin(1) ) Idout(37) = 0
      Idout(39) = Ivalue(14)
      Idout(40) = Ivalue(9)
      IF ( Idout(40)==1 ) Idoutr(40) = 0.0
      IF ( Idoutr(40)<xmin ) Idout(39) = 0
      Idout(41) = Ivalue(40)
      Idout(243) = Ivalue(53)
      Idout(244) = Ivalue(54)
      DO i = 1 , 32
         Idout(i+146) = Tcurve(i)
         Idout(i+178) = Xaxis(i)
         Idout(i+210) = Ytaxis(i)
      ENDDO
   ENDIF
!
!     IDOUT IS COMPLETE   OUTPUT CURVES
!
   ASSIGN 1100 TO icont
   ipair = Iat + Steps
   n = 1
!
 1000 mcount = 0
   DO m = 1 , Nat , 3
      mcount = mcount + 1
!
!     CURVE NUMBER, ID, COMPONENT
!
      Idout(4) = Z(m)
      itemp = m + n
      Idout(5) = Z(itemp)
      IF ( Idout(5)/=1000 ) curve = curve + 1
      Idout(3) = curve
!
!     MEAN RESPONSE IN PLACE OF SUBCASE IF RANDOM
!
      IF ( Random ) Idout(1) = Z(itemp+1)
!
!     SET NUMBER OF ZERO CROSSINGS IF RANDOM
!
      IF ( Random ) Idout(42) = Buf(mcount+20)
!
!     COMPUTE Y1 = YMIN  AND Y2 = YMAX  FOR ALL DATA FOR THIS CURVE
!
      begin = ipair + mcount*Steps - Steps
      null = .TRUE.
      DO k = 1 , Steps
         i = begin + k
         IF ( Z(i)/=1 ) THEN
            IF ( null ) THEN
               nx1 = k
               nx2 = k
               y1 = Rz(i)
               y2 = Rz(i)
               null = .FALSE.
            ELSEIF ( Rz(i)<y1 ) THEN
               y1 = Rz(i)
               nx1 = k
            ELSEIF ( Rz(i)>y2 ) THEN
               y2 = Rz(i)
               nx2 = k
            ENDIF
         ENDIF
      ENDDO
!
      IF ( .NOT.null ) THEN
         nx1 = nx1 + Iat
         nx2 = nx2 + Iat
         Idoutr(297) = y1
         Idoutr(298) = Rz(nx1)
         Idoutr(299) = y2
         Idoutr(300) = Rz(nx2)
      ELSE
         Idoutr(297) = 0.0
         Idoutr(298) = 0.0
         Idoutr(299) = 0.0
         Idoutr(300) = 0.0
      ENDIF
!
!     COMPUTE Y1 AND Y2 FOR DATA BETWEEN XMIN AND XMAX
!
      null = .TRUE.
      IF ( i1/=0 ) THEN
         DO k = i1 , i2
            i = begin + k
            IF ( Z(i)/=1 ) THEN
               IF ( null ) THEN
                  nx1 = k
                  nx2 = k
                  y1 = Rz(i)
                  y2 = Rz(i)
                  null = .FALSE.
               ELSEIF ( Rz(i)<y1 ) THEN
                  y1 = Rz(i)
                  nx1 = k
               ELSEIF ( Rz(i)>y2 ) THEN
                  y2 = Rz(i)
                  nx2 = k
               ENDIF
            ENDIF
         ENDDO
         IF ( .NOT.null ) THEN
            nx1 = nx1 + Iat
            nx2 = nx2 + Iat
            Idoutr(293) = y1
            Idoutr(294) = Rz(nx1)
            Idoutr(295) = y2
            Idoutr(296) = Rz(nx2)
            GOTO 1050
         ENDIF
      ENDIF
      Idoutr(293) = 0.0
      Idoutr(294) = 0.0
      Idoutr(295) = 0.0
      Idoutr(296) = 0.0
!
 1050 Idoutr(291) = Rz(Iat+1)
      itemp = Iat + Steps
      Idoutr(292) = Rz(itemp)
!
!     IDOUT IS COMPLETE FOR THIS CURVE
!
      IF ( Idout(5)/=0 .AND. Idout(5)/=1000 ) CALL xyout(-1,Idout(1),Idoutr(1))
      IF ( on ) CALL write(Outfil,Idout(1),300,eor)
      Idout(8) = 0
!
!     DUMP ALL PAIRS TO PRINTER AND PUNCH,  THOSE IN RANGE TO PLOTTER
!
      y1 = Idoutr(13)
      y2 = Idoutr(14)
      IF ( on ) CALL write(Outfil,oneone(1),2,noeor)
      ones = .TRUE.
      IF ( Idout(5)/=1000 ) THEN
         DO k = 1 , Steps
            i = begin + k
            j = Iat + k
            Buf(1) = Z(j)
            Buf(2) = Z(i)
            IF ( Z(i)/=1 ) THEN
               IF ( k>=i1 .AND. k<=i2 ) THEN
                  IF ( Print .OR. Punch ) CALL xyout(1,Buf(1),Rbuf(1))
                  IF ( Rz(i)>=y1 .AND. Rz(i)<=y2 ) THEN
                     IF ( on ) CALL write(Outfil,Buf(1),2,noeor)
                     ones = .FALSE.
                  ELSEIF ( .NOT.(ones) ) THEN
                     IF ( on ) CALL write(Outfil,oneone(1),2,noeor)
                     ones = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      IF ( on ) CALL write(Outfil,Buf(1),0,eor)
   ENDDO
!
   GOTO icont
!
!     DO BOTTOM CURVES IF ANY
!
 1100 ASSIGN 99999 TO icont
   n = 2
   IF ( Idout(7)>0 ) THEN
!
!     BOTTOM CURVE ID (SET ONLY VALUES THAT CHANGE FROM THE TOP CURVES)
!
      curve = 0
      Idout(7) = -1
      Idoutr(13) = ymin(2)
      Idout(8) = 1
      Idoutr(14) = ymax(2)
      CALL xytics(Idout(23),Idoutr(23),Ivalue(20),Idout(13),Idout(14),Ivalue(24),ycycle(2),0)
      Idout(31) = Ivalue(35) + Ivalue(25)
      Idout(32) = Ivalue(35) + Ivalue(26)
      Idout(33) = Ivalue(35) + Ivalue(31)
      Idout(34) = Ivalue(35) + Ivalue(32)
      Idout(36) = ycycle(2)
      Idout(37) = Ivalue(16)
      Idout(38) = Ivalue(12)
      IF ( Idout(38)==1 ) Idoutr(38) = 0.0
      IF ( Idoutr(38)<ymin(2) ) Idout(37) = 0
      Idout(243) = Ivalue(55)
      Idout(244) = Ivalue(56)
      DO i = 1 , 32
         Idout(i+146) = Tcurve(i)
         Idout(i+178) = Xaxis(i)
         Idout(i+210) = Ybaxis(i)
      ENDDO
      ipair = Center + Steps
      GOTO 1000
   ENDIF
99999 END SUBROUTINE xydump
