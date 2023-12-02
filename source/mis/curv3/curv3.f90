!*==curv3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv3
   USE c_blank
   USE c_curvc1
   USE c_curvc2
   USE c_curvc3
   USE c_curvtb
   USE c_names
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: avgl , fl , xrange , xsqysq , yrange
   INTEGER :: eor , icrq , idx , jsigma , kctype , noeor
   INTEGER , DIMENSION(7) , SAVE :: mcb
   REAL , DIMENSION(100) :: rbuf
   REAL , DIMENSION(1) :: z
   EXTERNAL bisloc , close , curvit , curvps , fwdrec , gmmats , open , read , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!*****
!  THIS OVERLAY WILL FORM OES1G (IF REQUESTED BY DMAP PARAMETER = 0)
!
!  OES1G OUTPUTS FOR CURRENT SUBCASE WILL BE GROUPED ON THE BASIS OF
!  THE MCSID. THUS THERE WILL BE A PASS FOR EACH -MCSID- HAVING A NON-
!  ZERO COUNT IN TABLE(IMCSID-NMCSID).
!
!  TO CONSERVE CORE FOR SSPLIN UTILITY, THE SIGMAS FOR EACH -MCSID- PASS
!  WILL BE WRITTEN TO SCR4 AS ENTRIES ARE SELECTED FROM SCR2. DEPENDENT
!  POINTS, EXTERNAL-IDS, AND INDEPENDENT POINTS WILL BE PLACED IN CORE
!  AND THEN REDUCED DURING THE PROJECTION SURFACE DETERMINATION PHASE.
!
!     OPEN CORE MAP DURING -CURV3- EXECUTION.
!     =======================================
!
!     FROM-------+------------+
!     CURV1      I  Z(IELTYP) I  MASTER LIST OF ELEMENT TYPES  ON
!     EXECUTION  I    THRU    I  ESTX(SCR1)
!                I  Z(NELTYP) I
!                +------------+
!                I  Z(IMCSID) I  MASTER LIST OF MCSIDS ELEMENTS IN
!                I    THRU    I  PROBLEM REFERENCE, WITH COUNTS OF
!                I  Z(NMCSID) I  OES1M ELEMENTS FOR CURRENT SUBCASE.
!                +------------+
!                I  Z(ICSTM)  I  CSTM FOR EACH -MCSID- IN ABOVE LIST.
!                I    THRU    I  14 WORD ENTRIES. (USER MAY NOT HAVE
!                I  Z(NCSTM)  I  SUPPLIED ALL, BUT MAY BE OK.)
!     FROM-------+------------+
!     AND DURING I  Z(IINDEP) I  INDEPENDENT POINT COORDINATES FOR ONE
!     CURV3      I    THRU    I  -MCSID- OF CURRENT SUBCASE.
!     EXECUTION  I  Z(NINDEP) I  TWO OR THREE WORD ENTRIES POSSIBLE.
!                +------------+
!                I  Z(IDEP)   I  DEPENDENT POINT COORDINATES FOR ONE
!                I    THRU    I  -MCSID- OF CURRENT SUBCASE.
!                I  Z(NDEP)   I  TWO OR FOUR WORD ENTRIES POSSIBLE.
!                +------------+
!                I  Z(IGMAT)  I  G MATRIX FROM SSPLIN UTILITY
!                I    THRU    I  (N-DEPENDENT-PTS BY N-INDEPENDENT-PTS)
!                I  Z(NGMAT)  I
!                +------------+
!                I  Z(ISIGMA) I  OES1M SIGMAS FOR ONE -MCSID- OF CURRENT
!                I    THRU    I  SUBCASE.  6X1 ENTRIES.
!                I  Z(NSIGMA) I
!                +------------+
!                I     .      I  AVAILABLE CORE.
!                I     .      I  (SSPLIN UTILITY USES Z(ISIGMA) THRU
!                I     .      I  Z(LCORE) FOR WORKING SPACE.)
!                I     .      I
!                I  Z(JCORE)  I
!                +------------+
!                I  Z(IBUF4)  I  GINO-BUFFER
!                I            I
!                +------------+
!                I  Z(IBUF3)  I  GINO-BUFFER
!                I            I
!                +------------+
!                I  Z(IBUF2)  I  GINO-BUFFER
!                I            I
!                +------------+
!                I  Z(IBUF1)  I  GINO-BUFFER
!                I            I
!                I  Z(LCORE)  I
!                +------------+
!
!  INPUTS - SCR2 CONTAINING ACTUAL ELEMENT ENTRIES USED TO FORM
!                OES1M FOR CURRENT OES1 SUBCASE. MAY BE MORE THAN
!                ONE -MCSID-. HAS THE SIX SIGMAS OF EACH ELEMENT
!                APPENDED TO EACH ELEMENT.
!
!                       -ELEMENT-ENTRY-
!
!                        MCSID = MATERIAL COORDINATE SYSTEM ID
!                        SIGMA1-X
!                        SIGMA1-Y
!                        SIGMA1-XY
!                        SIGMA2-X
!                        SIGMA2-Y
!                        SIGMA2-XY
!                        XC  *
!                        YC   * MEAN CENTER OF INDEPENDENT POINT
!                        ZC  *
!                        NPTS = NUMBER OF CONNECTED DEPENDENT GRIDS
!                        EXTERNAL GRID IDS (1 FOR EACH POINT)
!                        X,Y,Z  COMPONENTS OF EACH DEPENDENT GRID
!
!           SCR3 CONTAINING OFP TYPE -ID- RECORD TO USE AS A MODEL
!                FOR OES1G -ID- RECORD.
!
!
!           TABLE(IMCSID) THRU Z(NMCSID) CONTAINS PAIRS OF MCSID-S AND
!                COUNTS. (ONE PAIR FOR EACH UNIQUE MCSID OF CURRENT
!                SUBCASE.)
!
!
!           TABLE  Z(ICSTM) TO Z(NCSTM) CONTAINING TRANSFORMATIONS
!
!*****
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
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1))
   !>>>>EQUIVALENCE (Noeor,Rdrew) , (Eor,Cls)
!
   DATA mcb/7*1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  BRING OES1G -ID- RECORD INTO CORE AND MODIFY AS NECESSARY.
!
         file = scr3
         loc = 50
         CALL open(*60,scr3,iz(ibuf1),rdrew)
         CALL read(*80,*100,scr3,idrec(1),146,noeor,nwords)
         CALL close(scr3,clsrew)
!
!
!
!
!  OVERALL LOOP IS ON ENTRIES OF TABLE(IMCSID-NMCSID)
!
         jmcsid = imcsid
         spag_nextblock_1 = 2
      CASE (2)
!
         mcsid = iz(jmcsid)
         indpts = iz(jmcsid+1)
         loc = 100
         IF ( indpts<0 ) GOTO 40
         IF ( indpts==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!  COLLECT DATA REQUIRED FROM SCR2.
!
         file = scr2
!
!  CORE ALLOCATION FOR XC, YC, ZC OF EACH INDEPENDENT POINT.
!
         iindep = ncstm + 1
         nindep = ncstm + 3*indpts
!
!  CORE ALLOCATION FOR EXT-ID,X,Y,Z OF EACH UNIQUE DEPENDENT POINT.
!  (THE QUANTITY OF DEPENDENT POINTS IS NOT YET KNOWN.)
!
         idep = nindep + 1
         ndep = nindep
         loc = 110
         icrq = ndep - jcore
         IF ( ndep>jcore ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL open(*60,scr2,iz(ibuf1),rdrew)
         file = scr3
         CALL open(*60,scr3,iz(ibuf2),wrtrew)
!
         jindep = iindep
         file = scr2
!
!  FIND -INDPTS- NUMBER OF INDEPENDENT ELEMENT POINTS ENTRIES
!  FOR CURRENT -MCSID- PASS. (LOGIC ERROR IF CAN NOT FIND THIS MANY)
!
         DO i = 1 , indpts
            SPAG_Loop_2_1: DO
!
!  READ ELEMENT INDEPENDENT PORTION OF ENTRY
!
               loc = 150
               CALL read(*80,*100,scr2,buf(1),11,noeor,nwords)
               npts = buf(11)
               npts4 = 4*npts
!
!  CHECK MCSID OF ENTRY TO BE SAME AS ONE OF THIS PASS.
!
               IF ( buf(1)==mcsid ) THEN
!
!  YES, THIS ENTRY IS OF CURRENT PASS MCSID. ADD POINT DATA TO CORE.
!  FIRST OUTPUT SIGMAS TO SCR3
!
                  CALL write(scr3,buf(2),6,noeor)
                  z(jindep) = rbuf(8)
                  z(jindep+1) = rbuf(9)
                  z(jindep+2) = rbuf(10)
                  jindep = jindep + 3
!
!  INDEPENDENT POINTS NOT YET IN CORE ARE ADDED.
!
                  CALL read(*80,*100,scr2,buf(1),npts4,noeor,nwords)
                  k = npts
                  DO j = 1 , npts
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
!
!  CHECK IF EXTERNAL ID IS IN TABLE YET.
!
                           IF ( ndep>=idep ) THEN
                              DO l = idep , ndep , 4
                                 IF ( buf(j)==iz(l) ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                              ENDDO
                           ENDIF
!
!  NOT YET IN THUS ADD IT TO TABLE
!
                           icrq = ndep + 4 - jcore
                           IF ( ndep+4>jcore ) THEN
                              spag_nextblock_1 = 6
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           iz(ndep+1) = buf(j)
                           z(ndep+2) = rbuf(k+1)
                           z(ndep+3) = rbuf(k+2)
                           z(ndep+4) = rbuf(k+3)
                           ndep = ndep + 4
                           spag_nextblock_2 = 2
                        CASE (2)
!
                           k = k + 3
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
!
                  ENDDO
                  EXIT SPAG_Loop_2_1
               ELSE
!
!  NO IT IS NOT THUS SKIP BALANCE OF ENTRY.
!
                  loc = 170
                  CALL read(*80,*100,scr2,0,-npts4,noeor,nwords)
               ENDIF
            ENDDO SPAG_Loop_2_1
!
         ENDDO
!*****
!  ALL DATA FOR CURRENT MCSID HAS BEEN COLLECTED FROM SCR2.
!*****
         CALL close(scr2,clsrew)
         CALL close(scr3,clsrew)
!
!  DEPENDENT COORDINATES ARE SORTED ON EXTERNAL-ID.
!
         CALL sort(0,0,4,1,z(idep),ndep-idep+1)
!*****
!  CONVERSION OF INDEPENDENT AND DEPENDENT POINTS TO LOCAL
!  MATERIAL COORDINATE SYSTEM. FIRST GET CSTM DATA TO USE.
!*****
         loc = 400
         CALL bisloc(*40,mcsid,iz(icstm),14,cstms,jp)
         ivmat = icstm + jp + 1
         itran = ivmat + 3
         ictype = iz(ivmat-1)
!
!  FOR EACH POINT
!                               T
!                   (R     )=( T ) ( R     - V     )
!                     LOCAL           BASIC   MCSID
!
!                    (3X1)   (3X3)   (3X1)   (3X1)
!
         DO i = iindep , nindep , 3
            vec(1) = z(i) - z(ivmat)
            vec(2) = z(i+1) - z(ivmat+1)
            vec(3) = z(i+2) - z(ivmat+2)
            CALL gmmats(z(itran),3,3,1,vec(1),3,1,0,z(i))
         ENDDO
!
         DO i = idep , ndep , 4
            vec(1) = z(i+1) - z(ivmat)
            vec(2) = z(i+2) - z(ivmat+1)
            vec(3) = z(i+3) - z(ivmat+2)
            CALL gmmats(z(itran),3,3,1,vec(1),3,1,0,z(i+1))
         ENDDO
!*****
!  CONVERSION OF INDEPENDENT POINT LOCAL COORDINATES TO MAPPING
!  COORDINATES. (IF MCSID IS A RECTANGULAR SYSTEM THEN NO CHANGE.)
!*****
         loc = 490
         IF ( ictype<1 .OR. ictype>3 ) GOTO 40
         IF ( ictype==1 ) THEN
         ELSEIF ( ictype==3 ) THEN
!
!  SPHERICAL COORDINATES
!
            avgl = 0.0
            DO i = iindep , nindep , 3
               xsqysq = z(i)**2 + z(i+1)**2
               fl = sqrt(xsqysq)
               vec(1) = sqrt(xsqysq+z(i+2)**2)
               avgl = avgl + vec(1)
               IF ( vec(1)>0.0 ) THEN
                  vec(2) = atan2(fl,z(i+2))
               ELSE
                  vec(2) = 0.0
               ENDIF
               IF ( fl>0.0 ) THEN
                  vec(3) = atan2(z(i+1),z(i))
               ELSE
                  vec(3) = 0.0
               ENDIF
               z(i) = vec(1)
               z(i+1) = vec(2)
               z(i+2) = vec(3)
            ENDDO
            avgl = avgl/float(indpts)
         ELSE
!
!  CYLINDRICAL COORDINATES
!
            avgl = 0.0
            DO i = iindep , nindep , 3
               vec(1) = sqrt(z(i)**2+z(i+1)**2)
               avgl = avgl + vec(1)
               IF ( vec(1)<=0.0 ) THEN
                  z(i+1) = 0.0
               ELSE
                  z(i+1) = atan2(z(i+1),z(i))
               ENDIF
               z(i) = vec(1)
            ENDDO
            avgl = avgl/float(indpts)
         ENDIF
!*****
!  CONVERSION OF DEPENDENT POINT LOCAL COORDINATES TO MAPPING
!  COORDINATES.
!  (IF MCSID IS RECTANGULAR SYSTEM THEN NO CHANGE.)
!*****
         IF ( ictype==1 ) THEN
         ELSEIF ( ictype==3 ) THEN
!
!  SPHERICAL COORDINATES
!
            DO i = idep , ndep , 4
               xsqysq = z(i+1)**2 + z(i+2)**2
               fl = sqrt(xsqysq)
               vec(1) = sqrt(xsqysq+z(i+3)**2)
               IF ( vec(1)>0.0 ) THEN
                  vec(2) = atan2(fl,z(i+3))
               ELSE
                  vec(2) = 0.0
               ENDIF
               IF ( fl>0.0 ) THEN
                  vec(3) = atan2(z(i+2),z(i+1))
               ELSE
                  vec(3) = 0.0
               ENDIF
               z(i+1) = vec(1)
               z(i+2) = vec(2)
               z(i+3) = vec(3)
            ENDDO
         ELSE
!
!  CYLINDRICAL COORDINATES
!
            DO i = idep , ndep , 4
               vec(1) = sqrt(z(i+1)**2+z(i+2)**2)
               IF ( vec(1)<=0.0 ) THEN
                  z(i+2) = 0.0
               ELSE
                  z(i+2) = atan2(z(i+2),z(i+1))
               ENDIF
               z(i+1) = vec(1)
            ENDDO
         ENDIF
!
!  SET MAXIMUM AND MIMIMUM X,Y,Z VALUES.
!
         DO i = 1 , 3
            vmax(i) = z(iindep+i-1)
            vmin(i) = z(iindep+i-1)
         ENDDO
!
         DO i = iindep , nindep , 3
            DO j = 1 , 3
               vmax(j) = amax1(z(i+j-1),vmax(j))
               vmin(j) = amin1(z(i+j-1),vmin(j))
            ENDDO
         ENDDO
!
!  SET THE X,Y,Z RANGES
!
         DO i = 1 , 3
            vmax(i) = vmax(i) - vmin(i)
            vec(i) = vmax(i)
         ENDDO
!
         IF ( ictype/=1 ) THEN
            vmax(2) = avgl*vmax(2)
            IF ( ictype/=2 ) vmax(3) = avgl*vmax(3)
         ENDIF
!
!  DIRECTION YIELDING MINIMUM RANGE DETERMINES PROJECTION
!
         IF ( vmax(1)<vmax(2) ) THEN
            IF ( vmax(3)>=vmax(1) ) THEN
               k1 = 2
               k2 = 3
               kctype = 1
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( vmax(2)<vmax(3) ) THEN
            k1 = 1
            k2 = 3
            kctype = 2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k1 = 1
         k2 = 2
         kctype = 3
         spag_nextblock_1 = 3
      CASE (3)
!
         xrange = vec(k1)
         yrange = vec(k2)
         IF ( xrange==0 ) xrange = 1.0
         IF ( yrange==0 ) yrange = 1.0
!
!  COORDINATES -K1- AND -K2- WILL BE KEPT.
!
!  TABLE OF INDEPENDENT AND DEPENDENT POINTS ARE REDUCED TO
!  TABLES OF X,Y PAIRS. FIRST TO GAIN SOME CORE, EXTERNAL
!  IDS- ARE WRITTEN TO SCR4.
!
         file = scr4
         loc = 714
         CALL open(*60,scr4,iz(ibuf1),wrtrew)
         DO i = idep , ndep , 4
            CALL write(scr4,iz(i),1,noeor)
         ENDDO
         CALL close(scr4,clsrew)
!
!  REDUCE INDEPENDENT POINTS TO XY PAIRS, SCALE BY X AND Y RANGES
!  RESPECTIVELY, AND COMPRESS IN CORE.
!
         j = iindep
         DO i = iindep , nindep , 3
            z(j) = z(i+k1-1)/xrange
            z(j+1) = z(i+k2-1)/yrange
            j = j + 2
         ENDDO
         nindep = j - 1
!
!  REDUCE DEPENDENT POINTS LIST. (J IS STILL GOOD)
!
         DO i = idep , ndep , 4
            z(j) = z(i+k1)/xrange
            z(j+1) = z(i+k2)/yrange
            j = j + 2
         ENDDO
         idep = nindep + 1
         ndep = j - 1
         depts = (ndep-idep+1)/2
!*****
!  INDEPENDENT AND DEPENDENT POINT COORDINATE LISTS ARE NOW
!  COMPLETE.  CALL FOR INTERPOLATION.
!*****
         CALL curvit(z(iindep),indpts,z(idep),depts,scr5,z(ndep+1),iz(ndep+1),lcore-ndep-1,ip2,15.0,mcsid,xrange,yrange)
!
!  BRING -OES1M- SIGMAS INTO CORE FOR CURRENT -MCSID- PASS.
!
         isigma = iindep + 1
         nsigma = iindep + 6*indpts
         jsigma = isigma - 7
         icrq = nsigma - ibuf3
         IF ( nsigma>=ibuf3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = scr3
         loc = 800
         CALL open(*60,scr3,iz(ibuf1),rdrew)
         CALL read(*80,*20,scr3,iz(isigma),ibuf3-isigma,noeor,nwords)
         loc = 810
         GOTO 40
!
 20      IF ( nwords/=6*indpts ) GOTO 40
         CALL close(scr3,clsrew)
!
!    (SIGMAS                ) = (G)(SIGMAS                        )
!           DEPENDENT POINTS              OES1M INDEPENDENT POINTS
!
!  SINCE THE ORDER OF THE ROWS IN THE G MATRIX ARE IN SORTED EXTERNAL
!  GRID ORDER EACH OUTPUT LINE OF OES1G WILL BE HANDLED ON ITS
!  OWN. THIS ELIMINATES NECESSITY OF HOLDING ANOTHER SIGMA ARRAY
!  IN CORE.
!
         file = oes1g
         loc = 815
         CALL open(*60,oes1g,iz(ibuf1),wrt)
!
!  OUTPUT ID RECORD. PREVIOUSLY PREPARED.
!
         idrec(3) = idrec(3) + 2000
         CALL write(oes1g,idrec(1),146,eor)
         mcb(1) = oes1g
         CALL wrttrl(mcb(1))
         any1g = .TRUE.
!
!  OPEN SCR5 CONTAINING ROWS OF THE G-MATRIX.
!
         file = scr5
         CALL open(*60,scr5,iz(ibuf3),rdrew)
         CALL fwdrec(*80,scr5)
!
!  OPEN SCR4 CONTAINING LIST OF EXTERNAL IDS )
!
         file = scr4
         CALL open(*60,scr4,iz(ibuf2),rdrew)
!
!  COMPUTE AND OUTPUT SIGMAS FOR THE DEPENDENT POINTS
!
         buf(2) = mcsid
         DO i = 1 , depts
!
!  READ THE EXTERNAL ID
!
            file = scr4
            CALL read(*80,*100,scr4,buf(1),1,noeor,nwords)
            file = scr5
!
!  INITIALIZE SIGMAS(DEPENDENT POINT) TO ZERO
!
            DO j = 3 , 8
               rbuf(j) = 0.0
            ENDDO
!
            k = 0
            loc = 825
            DO
!
!  READ ACTIVE INDEX AND G-VALUE FROM SCRATCH 5
!
               CALL read(*80,*30,scr5,rbuf(11),2,noeor,nwords)
               k = k + 10
               idx = jsigma + 6*buf(11)
               DO j = 1 , 6
                  rbuf(j+2) = rbuf(j+2) + rbuf(12)*z(idx+j)
               ENDDO
            ENDDO
!
!  IF THERE WERE ANY G-VALUES THEN NOW COMPLETE THE OUTPUT LINE.
!
 30         IF ( k>0 ) THEN
!
               buf(10) = k + kctype
!
               rbuf(11) = rbuf(6)
               rbuf(12) = rbuf(7)
               rbuf(13) = rbuf(8)
!
!  COMPUTE INVARIANTS FOR EACH LINE
!
               CALL curvps(rbuf(3),rbuf(6))
               CALL curvps(rbuf(11),rbuf(14))
               IF ( strain ) THEN
                  rbuf(5) = 2.0*rbuf(5)
                  rbuf(9) = 2.0*rbuf(9)
                  rbuf(13) = 2.0*rbuf(13)
                  rbuf(17) = 2.0*rbuf(17)
               ENDIF
!
!  APPEND DEVICE CODE TO EXTERNAL ID AND OUTPUT LINE
!
               buf(1) = 10*buf(1) + device
               CALL write(oes1g,buf(1),17,noeor)
            ENDIF
         ENDDO
!
         CALL write(oes1g,0,0,eor)
         IF ( eofos1 .AND. jmcsid+2>nmcsid ) CALL close(oes1g,clsrew)
         CALL close(oes1g,cls)
         CALL close(scr4,clsrew)
         CALL close(scr5,clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!*****
!  ALL INDEPENDENT POINTS OUTPUT TO OES1G FOR 1 ACTIVE MCSID OF
!  CURRENT SUBCASE. GO TO NEXT MCSID.
!*****
         jmcsid = jmcsid + 2
         IF ( jmcsid<=nmcsid ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!*****
!  ALL THROUGH FORMING OES1G FOR CURRENT SUBCASE.
!*****
         RETURN
!*****
!  ERROR CONDITION ENCOUNTERED
!*****
 40      imsg = -logerr
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     imsg = -3
         spag_nextblock_1 = 5
      CASE (6)
         imsg = -8
         lcore = icrq
         spag_nextblock_1 = 5
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv3
