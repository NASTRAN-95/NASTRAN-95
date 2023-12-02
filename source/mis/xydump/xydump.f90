!*==xydump.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xydump(Outfil,Type)
   USE c_blank
   USE c_machin
   USE c_two
   USE c_xywork
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Outfil
   INTEGER :: Type
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: begin , curve , i , i1 , i2 , icont , iflag , ipair , is1 , itemp , j , j1 , j2 , k , m , mcount , n , nx1 , nx2 ,    &
            & xcycle
   LOGICAL :: dec , intore , null , ok , on , ones
   REAL :: diff , temp , xinc , xmax , xmin , y1 , y2
   INTEGER , SAVE :: eor , noeor , npaplt
   REAL , DIMENSION(300) :: idoutr
   INTEGER , DIMENSION(2,3) :: limit
   INTEGER , DIMENSION(2) , SAVE :: oneone
   REAL , DIMENSION(100) :: rbuf
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(60) :: value
   INTEGER , DIMENSION(2) :: ycycle
   REAL , DIMENSION(2,3) :: ylimit
   REAL , DIMENSION(2) :: ymax , ymin
   EXTERNAL write , xylog , xyout , xytics
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
!
   !>>>>EQUIVALENCE (limit(1,1),ylimit(1,1)) , (Z(1),Rz(1)) , (Buf(1),Rbuf(1)) , (Idout(1),Idoutr(1)) , (Ivalue(1),Value(1))
   DATA oneone/1 , 1/ , eor/1/ , noeor/0/
   DATA npaplt/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SET MINIMUM X-DIFFERENCE
!
!     BUT FIRST CONVERT X FROM INTERGER TO REAL IF NECESSARY.
!
         intore = .FALSE.
         dec = machx==5 .OR. machx==6 .OR. machx==21
         j = 1
         is1 = steps - 1
!
!     NOW SEARCH LIST FOR FIRST NON-ZERO ENTRY
!
         DO WHILE ( z(iat+j)==0 )
            j = j + 1
            IF ( j>is1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!              UNIVAC             CDC             CRAY
         IF ( machx/=3 .AND. machx/=4 .AND. machx/=12 ) THEN
!
!     IBM, VAX, UNIX
!
            IF ( .NOT.dec .AND. iabs(z(iat+j))>two1(9) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( dec .AND. (z(iat+j)<1 .OR. z(iat+j)>127) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( iabs(z(iat+j))>two1(2) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         intore = .TRUE.
         IF ( j==1 ) rz(iat+j) = z(iat+j)
         spag_nextblock_1 = 2
      CASE (2)
!
         ok = .FALSE.
         DO i = 1 , is1
            j = iat + i
            IF ( intore ) rz(j+1) = z(j+1)
            diff = rz(j+1) - rz(j)
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
         xmin = rz(iat+1)
         j = iat + steps
         xmax = rz(j)
!
!     REDUCE THESE LIMITS TO USER SPECIFIED LIMITS
!
         IF ( ivalue(1)/=1 ) xmin = (value(1))
         IF ( ivalue(2)/=1 ) xmax = (value(2))
!
!     FURTHER EXPAND XLIMITS TO INCLUDE Y-AXIS INTERCEPT
!
         IF ( ivalue(9)/=1 ) THEN
            IF ( ivalue(36)==1 .AND. value(9)<=0.0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            xmin = amin1(xmin,value(9))
            xmax = amax1(xmax,value(9))
         ENDIF
!
!     IF X-DIRECTION IS LOG AND XMIN IS NEGATIVE OR ZERO, SET YMIN
!     EQUAL TO THE SMALLEST NON-ZERO POSITIVE VALUE
!
         IF ( ivalue(36)/=1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( xmin<=0.0 ) THEN
            DO i = 1 , steps
               j = iat + i
               IF ( rz(j)>0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            xmin = 1.0
            xmax = 10.
         ENDIF
         spag_nextblock_1 = 5
      CASE (4)
         xmin = rz(j)
         spag_nextblock_1 = 5
      CASE (5)
         CALL xylog(xmin,xmax,xcycle)
         spag_nextblock_1 = 6
      CASE (6)
!
!     SWITCH XMIN AND XMAX (SAFETY CHECK) IF NECESSARY
!
         IF ( xmin>xmax ) THEN
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
         DO i = 1 , steps
            j = iat + i
            IF ( xmin<=rz(j) .AND. rz(j)<=xmax ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         i1 = 0
         spag_nextblock_1 = 8
      CASE (7)
         i1 = i
         j = iat + steps + 1
         DO i = 1 , steps
            j = j - 1
            IF ( xmin<=rz(j) .AND. rz(j)<=xmax ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         i2 = 0
         spag_nextblock_1 = 10
      CASE (9)
         i2 = j - iat
         spag_nextblock_1 = 10
      CASE (10)
         IF ( i1/=0 ) THEN
            m = 1
            IF ( nbots/=0 ) m = 2
            begin = iat
            DO i = 1 , m
               limit(i,1) = 1
               limit(i,2) = 1
               limit(i,3) = 1
               DO j = 1 , ntops
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        k = j*steps + begin
                        j1 = k + i1
                        j2 = k + i2
                        IF ( limit(i,1)/=1 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     FIND FIRST NON-INTEGER 1 VALUE
!
                        SPAG_Loop_5_1: DO k = j1 , j2
                           IF ( z(k)/=1 ) THEN
                              spag_nextblock_2 = 2
                              EXIT SPAG_Loop_5_1
                           ENDIF
                        ENDDO SPAG_Loop_5_1
                     CASE (2)
                        ylimit(i,1) = rz(k)
                        ylimit(i,2) = rz(k)
                        spag_nextblock_2 = 3
                     CASE (3)
                        DO k = j1 , j2
                           IF ( z(k)/=1 ) THEN
                              ylimit(i,1) = amin1(rz(k),ylimit(i,1))
                              ylimit(i,2) = amax1(rz(k),ylimit(i,2))
                              IF ( rz(k)>0.0 ) THEN
                                 IF ( limit(i,3)==1 ) ylimit(i,3) = rz(k)
                                 ylimit(i,3) = amin1(ylimit(i,3),rz(k))
                              ENDIF
                           ENDIF
                        ENDDO
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               begin = center
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
            IF ( nbots>0 ) k = 2
            DO i = 1 , k
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     ymin(i) = ylimit(i,1)
                     ymax(i) = ylimit(i,2)
!
!     REDUCE THESE CURVE LIMITS TO LIMITS SET BY USER
!
                     itemp = 2*(i+k)
                     IF ( ivalue(itemp-1)/=1 ) ymin(i) = (value(itemp-1))
                     IF ( ivalue(itemp)/=1 ) ymax(i) = (value(itemp))
!
!     FURTHER EXPAND LIMITS TO INCLUDE X-AXIS
!
                     itemp = i + k
                     IF ( ivalue(itemp+8)/=1 ) THEN
                        IF ( ivalue(itemp+35)==1 .AND. value(itemp+8)<=0.E0 ) THEN
                           spag_nextblock_3 = 2
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        ymin(i) = amin1(ymin(i),value(itemp+8))
                        ymax(i) = amax1(ymax(i),value(itemp+8))
                     ENDIF
!
!     IF Y-DIRECTION IS LOG AND YMIN IS NEGATIVE OR ZERO SET YMIN
!     EQUAL TO SMALLEST POSITIVE CURVE VALUE WITHIN XLIMITS
!
                     IF ( ivalue(itemp+35)/=1 ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     spag_nextblock_3 = 2
                  CASE (2)
                     IF ( ymin(i)<=0.0 ) ymin(i) = ylimit(i,3)
                     CALL xylog(ymin(i),ymax(i),ycycle(i))
                     spag_nextblock_3 = 3
                  CASE (3)
!
!     SWITCH YMIN AND YMAX (SAFETY CHECK) IF NECESSARY
!
                     IF ( ymin(i)>ymax(i) ) THEN
                        temp = ymin(i)
                        ymin(i) = ymax(i)
                        ymax(i) = temp
                     ENDIF
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
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
            idout(i) = 0
         ENDDO
         IF ( plot .AND. outopn ) nframe = nframe + 1
         idout(1) = subc(file)
         idout(2) = nframe
         idout(6) = vector
         idout(9) = ivalue(45)
         IF ( ivalue(43)==0 ) value(43) = 1.0
         idout(43) = ivalue(43)
         idoutr(10) = xinc
         idout(245) = Type
         idout(246) = steps
         idoutr(282) = value(57)
         IF ( idoutr(282)<1.0 ) idoutr(282) = 1.0
         idout(283) = ivalue(50)
         IF ( ivalue(47)==3 ) idout(283) = ivalue(41)
         idout(284) = ivalue(47)
         idout(285) = ivalue(48)
         idout(286) = ivalue(49)
         idout(287) = ivalue(46)
         idout(44) = ivalue(58)
         idout(45) = ivalue(59)
         IF ( print ) idout(288) = 1
         IF ( plot ) idout(289) = 1
         IF ( paplot ) THEN
            IF ( .NOT.plot ) idout(289) = -1
            IF ( plot ) idout(289) = 2
            npaplt = npaplt + 1
            idout(281) = npaplt
         ENDIF
         on = .FALSE.
         IF ( plot .OR. paplot ) on = .TRUE.
         IF ( punch ) idout(290) = 1
         DO i = 51 , 146
            idout(i) = idin(i)
         ENDDO
!
!     BRANCH ON TOP, BOTTOM, OR WHOLE CURVE (FIRST WILL BE TOP OR WHOLE)
!
         i = 3
         IF ( z(i)==0 .OR. random ) THEN
!
!     WHOLE CURVE ID
!
            curve = 0
            idout(7) = 0
            idout(8) = 1
            idoutr(11) = xmin
            idoutr(12) = xmax
            idoutr(13) = ymin(1)
            idoutr(14) = ymax(1)
            iflag = 0
            IF ( intore ) iflag = 1
            CALL xytics(idout(15),idoutr(15),ivalue(17),idout(11),idout(12),ivalue(21),xcycle,iflag)
            CALL xytics(idout(23),idoutr(23),ivalue(18),idout(13),idout(14),ivalue(22),ycycle(1),0)
            idout(31) = ivalue(33) + ivalue(25)
            idout(32) = ivalue(33) + ivalue(26)
            idout(33) = ivalue(33) + ivalue(27)
            idout(34) = ivalue(33) + ivalue(28)
            idout(35) = xcycle
            idout(36) = ycycle(1)
            idout(37) = ivalue(13)
            idout(38) = ivalue(10)
            IF ( idout(38)==1 ) idout(38) = 0.0
            IF ( idoutr(38)<ymin(1) ) idout(37) = 0
            idout(39) = ivalue(14)
            idout(40) = ivalue(9)
            IF ( idout(40)==1 ) idoutr(40) = 0.0
            IF ( idoutr(40)<xmin ) idout(39) = 0
            idout(41) = ivalue(40)
            idout(243) = ivalue(51)
            idout(244) = ivalue(52)
            DO i = 1 , 32
               idout(i+146) = tcurve(i)
               idout(i+178) = xaxis(i)
               idout(i+210) = yaxis(i)
            ENDDO
         ELSE
!
!     TOP CURVE ID
!
            curve = 0
            idout(7) = 1
            idout(8) = 1
            idoutr(11) = xmin
            idoutr(12) = xmax
            idoutr(13) = ymin(1)
            idoutr(14) = ymax(1)
            iflag = 0
            IF ( intore ) iflag = 1
            CALL xytics(idout(15),idoutr(15),ivalue(17),idout(11),idout(12),ivalue(21),xcycle,iflag)
            CALL xytics(idout(23),idoutr(23),ivalue(19),idout(13),idout(14),ivalue(23),ycycle(1),0)
            idout(31) = ivalue(34) + ivalue(25)
            idout(32) = ivalue(34) + ivalue(26)
            idout(33) = ivalue(34) + ivalue(29)
            idout(34) = ivalue(34) + ivalue(30)
            idout(35) = xcycle
            idout(36) = ycycle(1)
            idout(37) = ivalue(15)
            idout(38) = ivalue(11)
            IF ( idout(38)==1 ) idoutr(38) = 0.0
            IF ( idoutr(38)<ymin(1) ) idout(37) = 0
            idout(39) = ivalue(14)
            idout(40) = ivalue(9)
            IF ( idout(40)==1 ) idoutr(40) = 0.0
            IF ( idoutr(40)<xmin ) idout(39) = 0
            idout(41) = ivalue(40)
            idout(243) = ivalue(53)
            idout(244) = ivalue(54)
            DO i = 1 , 32
               idout(i+146) = tcurve(i)
               idout(i+178) = xaxis(i)
               idout(i+210) = ytaxis(i)
            ENDDO
         ENDIF
!
!     IDOUT IS COMPLETE   OUTPUT CURVES
!
         ASSIGN 20 TO icont
         ipair = iat + steps
         n = 1
         spag_nextblock_1 = 11
      CASE (11)
!
         mcount = 0
         DO m = 1 , nat , 3
            spag_nextblock_4 = 1
            SPAG_DispatchLoop_4: DO
               SELECT CASE (spag_nextblock_4)
               CASE (1)
                  mcount = mcount + 1
!
!     CURVE NUMBER, ID, COMPONENT
!
                  idout(4) = z(m)
                  itemp = m + n
                  idout(5) = z(itemp)
                  IF ( idout(5)/=1000 ) curve = curve + 1
                  idout(3) = curve
!
!     MEAN RESPONSE IN PLACE OF SUBCASE IF RANDOM
!
                  IF ( random ) idout(1) = z(itemp+1)
!
!     SET NUMBER OF ZERO CROSSINGS IF RANDOM
!
                  IF ( random ) idout(42) = buf(mcount+20)
!
!     COMPUTE Y1 = YMIN  AND Y2 = YMAX  FOR ALL DATA FOR THIS CURVE
!
                  begin = ipair + mcount*steps - steps
                  null = .TRUE.
                  DO k = 1 , steps
                     i = begin + k
                     IF ( z(i)/=1 ) THEN
                        IF ( null ) THEN
                           nx1 = k
                           nx2 = k
                           y1 = rz(i)
                           y2 = rz(i)
                           null = .FALSE.
                        ELSEIF ( rz(i)<y1 ) THEN
                           y1 = rz(i)
                           nx1 = k
                        ELSEIF ( rz(i)>y2 ) THEN
                           y2 = rz(i)
                           nx2 = k
                        ENDIF
                     ENDIF
                  ENDDO
!
                  IF ( .NOT.null ) THEN
                     nx1 = nx1 + iat
                     nx2 = nx2 + iat
                     idoutr(297) = y1
                     idoutr(298) = rz(nx1)
                     idoutr(299) = y2
                     idoutr(300) = rz(nx2)
                  ELSE
                     idoutr(297) = 0.0
                     idoutr(298) = 0.0
                     idoutr(299) = 0.0
                     idoutr(300) = 0.0
                  ENDIF
!
!     COMPUTE Y1 AND Y2 FOR DATA BETWEEN XMIN AND XMAX
!
                  null = .TRUE.
                  IF ( i1/=0 ) THEN
                     DO k = i1 , i2
                        i = begin + k
                        IF ( z(i)/=1 ) THEN
                           IF ( null ) THEN
                              nx1 = k
                              nx2 = k
                              y1 = rz(i)
                              y2 = rz(i)
                              null = .FALSE.
                           ELSEIF ( rz(i)<y1 ) THEN
                              y1 = rz(i)
                              nx1 = k
                           ELSEIF ( rz(i)>y2 ) THEN
                              y2 = rz(i)
                              nx2 = k
                           ENDIF
                        ENDIF
                     ENDDO
                     IF ( .NOT.null ) THEN
                        nx1 = nx1 + iat
                        nx2 = nx2 + iat
                        idoutr(293) = y1
                        idoutr(294) = rz(nx1)
                        idoutr(295) = y2
                        idoutr(296) = rz(nx2)
                        spag_nextblock_4 = 2
                        CYCLE SPAG_DispatchLoop_4
                     ENDIF
                  ENDIF
                  idoutr(293) = 0.0
                  idoutr(294) = 0.0
                  idoutr(295) = 0.0
                  idoutr(296) = 0.0
                  spag_nextblock_4 = 2
               CASE (2)
!
                  idoutr(291) = rz(iat+1)
                  itemp = iat + steps
                  idoutr(292) = rz(itemp)
!
!     IDOUT IS COMPLETE FOR THIS CURVE
!
                  IF ( idout(5)/=0 .AND. idout(5)/=1000 ) CALL xyout(-1,idout(1),idoutr(1))
                  IF ( on ) CALL write(Outfil,idout(1),300,eor)
                  idout(8) = 0
!
!     DUMP ALL PAIRS TO PRINTER AND PUNCH,  THOSE IN RANGE TO PLOTTER
!
                  y1 = idoutr(13)
                  y2 = idoutr(14)
                  IF ( on ) CALL write(Outfil,oneone(1),2,noeor)
                  ones = .TRUE.
                  IF ( idout(5)/=1000 ) THEN
                     DO k = 1 , steps
                        i = begin + k
                        j = iat + k
                        buf(1) = z(j)
                        buf(2) = z(i)
                        IF ( z(i)/=1 ) THEN
                           IF ( k>=i1 .AND. k<=i2 ) THEN
                              IF ( print .OR. punch ) CALL xyout(1,buf(1),rbuf(1))
                              IF ( rz(i)>=y1 .AND. rz(i)<=y2 ) THEN
                                 IF ( on ) CALL write(Outfil,buf(1),2,noeor)
                                 ones = .FALSE.
                              ELSEIF ( .NOT.(ones) ) THEN
                                 IF ( on ) CALL write(Outfil,oneone(1),2,noeor)
                                 ones = .TRUE.
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDIF
                  IF ( on ) CALL write(Outfil,buf(1),0,eor)
                  EXIT SPAG_DispatchLoop_4
               END SELECT
            ENDDO SPAG_DispatchLoop_4
         ENDDO
!
         GOTO icont
!
!     DO BOTTOM CURVES IF ANY
!
 20      ASSIGN 99999 TO icont
         n = 2
         IF ( idout(7)>0 ) THEN
!
!     BOTTOM CURVE ID (SET ONLY VALUES THAT CHANGE FROM THE TOP CURVES)
!
            curve = 0
            idout(7) = -1
            idoutr(13) = ymin(2)
            idout(8) = 1
            idoutr(14) = ymax(2)
            CALL xytics(idout(23),idoutr(23),ivalue(20),idout(13),idout(14),ivalue(24),ycycle(2),0)
            idout(31) = ivalue(35) + ivalue(25)
            idout(32) = ivalue(35) + ivalue(26)
            idout(33) = ivalue(35) + ivalue(31)
            idout(34) = ivalue(35) + ivalue(32)
            idout(36) = ycycle(2)
            idout(37) = ivalue(16)
            idout(38) = ivalue(12)
            IF ( idout(38)==1 ) idoutr(38) = 0.0
            IF ( idoutr(38)<ymin(2) ) idout(37) = 0
            idout(243) = ivalue(55)
            idout(244) = ivalue(56)
            DO i = 1 , 32
               idout(i+146) = tcurve(i)
               idout(i+178) = xaxis(i)
               idout(i+210) = ybaxis(i)
            ENDDO
            ipair = center + steps
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE xydump
