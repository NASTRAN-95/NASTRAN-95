!*==curv3.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv3
   IMPLICIT NONE
   USE C_BLANK
   USE C_CURVC1
   USE C_CURVC2
   USE C_CURVC3
   USE C_CURVTB
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         File = Scr3
         Loc = 50
         CALL open(*60,Scr3,Iz(Ibuf1),Rdrew)
         CALL read(*80,*100,Scr3,Idrec(1),146,noeor,Nwords)
         CALL close(Scr3,Clsrew)
!
!
!
!
!  OVERALL LOOP IS ON ENTRIES OF TABLE(IMCSID-NMCSID)
!
         Jmcsid = Imcsid
         spag_nextblock_1 = 2
      CASE (2)
!
         Mcsid = Iz(Jmcsid)
         Indpts = Iz(Jmcsid+1)
         Loc = 100
         IF ( Indpts<0 ) GOTO 40
         IF ( Indpts==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!  COLLECT DATA REQUIRED FROM SCR2.
!
         File = Scr2
!
!  CORE ALLOCATION FOR XC, YC, ZC OF EACH INDEPENDENT POINT.
!
         Iindep = Ncstm + 1
         Nindep = Ncstm + 3*Indpts
!
!  CORE ALLOCATION FOR EXT-ID,X,Y,Z OF EACH UNIQUE DEPENDENT POINT.
!  (THE QUANTITY OF DEPENDENT POINTS IS NOT YET KNOWN.)
!
         Idep = Nindep + 1
         Ndep = Nindep
         Loc = 110
         icrq = Ndep - Jcore
         IF ( Ndep>Jcore ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL open(*60,Scr2,Iz(Ibuf1),Rdrew)
         File = Scr3
         CALL open(*60,Scr3,Iz(Ibuf2),Wrtrew)
!
         Jindep = Iindep
         File = Scr2
!
!  FIND -INDPTS- NUMBER OF INDEPENDENT ELEMENT POINTS ENTRIES
!  FOR CURRENT -MCSID- PASS. (LOGIC ERROR IF CAN NOT FIND THIS MANY)
!
         DO I = 1 , Indpts
            SPAG_Loop_2_1: DO
!
!  READ ELEMENT INDEPENDENT PORTION OF ENTRY
!
               Loc = 150
               CALL read(*80,*100,Scr2,Buf(1),11,noeor,Nwords)
               Npts = Buf(11)
               Npts4 = 4*Npts
!
!  CHECK MCSID OF ENTRY TO BE SAME AS ONE OF THIS PASS.
!
               IF ( Buf(1)==Mcsid ) THEN
!
!  YES, THIS ENTRY IS OF CURRENT PASS MCSID. ADD POINT DATA TO CORE.
!  FIRST OUTPUT SIGMAS TO SCR3
!
                  CALL write(Scr3,Buf(2),6,noeor)
                  z(Jindep) = rbuf(8)
                  z(Jindep+1) = rbuf(9)
                  z(Jindep+2) = rbuf(10)
                  Jindep = Jindep + 3
!
!  INDEPENDENT POINTS NOT YET IN CORE ARE ADDED.
!
                  CALL read(*80,*100,Scr2,Buf(1),Npts4,noeor,Nwords)
                  K = Npts
                  DO J = 1 , Npts
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
!
!  CHECK IF EXTERNAL ID IS IN TABLE YET.
!
                           IF ( Ndep>=Idep ) THEN
                              DO L = Idep , Ndep , 4
                                 IF ( Buf(J)==Iz(L) ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                              ENDDO
                           ENDIF
!
!  NOT YET IN THUS ADD IT TO TABLE
!
                           icrq = Ndep + 4 - Jcore
                           IF ( Ndep+4>Jcore ) THEN
                              spag_nextblock_1 = 6
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           Iz(Ndep+1) = Buf(J)
                           z(Ndep+2) = rbuf(K+1)
                           z(Ndep+3) = rbuf(K+2)
                           z(Ndep+4) = rbuf(K+3)
                           Ndep = Ndep + 4
                           spag_nextblock_2 = 2
                        CASE (2)
!
                           K = K + 3
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
                  Loc = 170
                  CALL read(*80,*100,Scr2,0,-Npts4,noeor,Nwords)
               ENDIF
            ENDDO SPAG_Loop_2_1
!
         ENDDO
!*****
!  ALL DATA FOR CURRENT MCSID HAS BEEN COLLECTED FROM SCR2.
!*****
         CALL close(Scr2,Clsrew)
         CALL close(Scr3,Clsrew)
!
!  DEPENDENT COORDINATES ARE SORTED ON EXTERNAL-ID.
!
         CALL sort(0,0,4,1,z(Idep),Ndep-Idep+1)
!*****
!  CONVERSION OF INDEPENDENT AND DEPENDENT POINTS TO LOCAL
!  MATERIAL COORDINATE SYSTEM. FIRST GET CSTM DATA TO USE.
!*****
         Loc = 400
         CALL bisloc(*40,Mcsid,Iz(Icstm),14,Cstms,Jp)
         Ivmat = Icstm + Jp + 1
         Itran = Ivmat + 3
         Ictype = Iz(Ivmat-1)
!
!  FOR EACH POINT
!                               T
!                   (R     )=( T ) ( R     - V     )
!                     LOCAL           BASIC   MCSID
!
!                    (3X1)   (3X3)   (3X1)   (3X1)
!
         DO I = Iindep , Nindep , 3
            Vec(1) = z(I) - z(Ivmat)
            Vec(2) = z(I+1) - z(Ivmat+1)
            Vec(3) = z(I+2) - z(Ivmat+2)
            CALL gmmats(z(Itran),3,3,1,Vec(1),3,1,0,z(I))
         ENDDO
!
         DO I = Idep , Ndep , 4
            Vec(1) = z(I+1) - z(Ivmat)
            Vec(2) = z(I+2) - z(Ivmat+1)
            Vec(3) = z(I+3) - z(Ivmat+2)
            CALL gmmats(z(Itran),3,3,1,Vec(1),3,1,0,z(I+1))
         ENDDO
!*****
!  CONVERSION OF INDEPENDENT POINT LOCAL COORDINATES TO MAPPING
!  COORDINATES. (IF MCSID IS A RECTANGULAR SYSTEM THEN NO CHANGE.)
!*****
         Loc = 490
         IF ( Ictype<1 .OR. Ictype>3 ) GOTO 40
         IF ( Ictype==1 ) THEN
         ELSEIF ( Ictype==3 ) THEN
!
!  SPHERICAL COORDINATES
!
            avgl = 0.0
            DO I = Iindep , Nindep , 3
               xsqysq = z(I)**2 + z(I+1)**2
               fl = sqrt(xsqysq)
               Vec(1) = sqrt(xsqysq+z(I+2)**2)
               avgl = avgl + Vec(1)
               IF ( Vec(1)>0.0 ) THEN
                  Vec(2) = atan2(fl,z(I+2))
               ELSE
                  Vec(2) = 0.0
               ENDIF
               IF ( fl>0.0 ) THEN
                  Vec(3) = atan2(z(I+1),z(I))
               ELSE
                  Vec(3) = 0.0
               ENDIF
               z(I) = Vec(1)
               z(I+1) = Vec(2)
               z(I+2) = Vec(3)
            ENDDO
            avgl = avgl/float(Indpts)
         ELSE
!
!  CYLINDRICAL COORDINATES
!
            avgl = 0.0
            DO I = Iindep , Nindep , 3
               Vec(1) = sqrt(z(I)**2+z(I+1)**2)
               avgl = avgl + Vec(1)
               IF ( Vec(1)<=0.0 ) THEN
                  z(I+1) = 0.0
               ELSE
                  z(I+1) = atan2(z(I+1),z(I))
               ENDIF
               z(I) = Vec(1)
            ENDDO
            avgl = avgl/float(Indpts)
         ENDIF
!*****
!  CONVERSION OF DEPENDENT POINT LOCAL COORDINATES TO MAPPING
!  COORDINATES.
!  (IF MCSID IS RECTANGULAR SYSTEM THEN NO CHANGE.)
!*****
         IF ( Ictype==1 ) THEN
         ELSEIF ( Ictype==3 ) THEN
!
!  SPHERICAL COORDINATES
!
            DO I = Idep , Ndep , 4
               xsqysq = z(I+1)**2 + z(I+2)**2
               fl = sqrt(xsqysq)
               Vec(1) = sqrt(xsqysq+z(I+3)**2)
               IF ( Vec(1)>0.0 ) THEN
                  Vec(2) = atan2(fl,z(I+3))
               ELSE
                  Vec(2) = 0.0
               ENDIF
               IF ( fl>0.0 ) THEN
                  Vec(3) = atan2(z(I+2),z(I+1))
               ELSE
                  Vec(3) = 0.0
               ENDIF
               z(I+1) = Vec(1)
               z(I+2) = Vec(2)
               z(I+3) = Vec(3)
            ENDDO
         ELSE
!
!  CYLINDRICAL COORDINATES
!
            DO I = Idep , Ndep , 4
               Vec(1) = sqrt(z(I+1)**2+z(I+2)**2)
               IF ( Vec(1)<=0.0 ) THEN
                  z(I+2) = 0.0
               ELSE
                  z(I+2) = atan2(z(I+2),z(I+1))
               ENDIF
               z(I+1) = Vec(1)
            ENDDO
         ENDIF
!
!  SET MAXIMUM AND MIMIMUM X,Y,Z VALUES.
!
         DO I = 1 , 3
            Vmax(I) = z(Iindep+I-1)
            Vmin(I) = z(Iindep+I-1)
         ENDDO
!
         DO I = Iindep , Nindep , 3
            DO J = 1 , 3
               Vmax(J) = amax1(z(I+J-1),Vmax(J))
               Vmin(J) = amin1(z(I+J-1),Vmin(J))
            ENDDO
         ENDDO
!
!  SET THE X,Y,Z RANGES
!
         DO I = 1 , 3
            Vmax(I) = Vmax(I) - Vmin(I)
            Vec(I) = Vmax(I)
         ENDDO
!
         IF ( Ictype/=1 ) THEN
            Vmax(2) = avgl*Vmax(2)
            IF ( Ictype/=2 ) Vmax(3) = avgl*Vmax(3)
         ENDIF
!
!  DIRECTION YIELDING MINIMUM RANGE DETERMINES PROJECTION
!
         IF ( Vmax(1)<Vmax(2) ) THEN
            IF ( Vmax(3)>=Vmax(1) ) THEN
               K1 = 2
               K2 = 3
               kctype = 1
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( Vmax(2)<Vmax(3) ) THEN
            K1 = 1
            K2 = 3
            kctype = 2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         K1 = 1
         K2 = 2
         kctype = 3
         spag_nextblock_1 = 3
      CASE (3)
!
         xrange = Vec(K1)
         yrange = Vec(K2)
         IF ( xrange==0 ) xrange = 1.0
         IF ( yrange==0 ) yrange = 1.0
!
!  COORDINATES -K1- AND -K2- WILL BE KEPT.
!
!  TABLE OF INDEPENDENT AND DEPENDENT POINTS ARE REDUCED TO
!  TABLES OF X,Y PAIRS. FIRST TO GAIN SOME CORE, EXTERNAL
!  IDS- ARE WRITTEN TO SCR4.
!
         File = Scr4
         Loc = 714
         CALL open(*60,Scr4,Iz(Ibuf1),Wrtrew)
         DO I = Idep , Ndep , 4
            CALL write(Scr4,Iz(I),1,noeor)
         ENDDO
         CALL close(Scr4,Clsrew)
!
!  REDUCE INDEPENDENT POINTS TO XY PAIRS, SCALE BY X AND Y RANGES
!  RESPECTIVELY, AND COMPRESS IN CORE.
!
         J = Iindep
         DO I = Iindep , Nindep , 3
            z(J) = z(I+K1-1)/xrange
            z(J+1) = z(I+K2-1)/yrange
            J = J + 2
         ENDDO
         Nindep = J - 1
!
!  REDUCE DEPENDENT POINTS LIST. (J IS STILL GOOD)
!
         DO I = Idep , Ndep , 4
            z(J) = z(I+K1)/xrange
            z(J+1) = z(I+K2)/yrange
            J = J + 2
         ENDDO
         Idep = Nindep + 1
         Ndep = J - 1
         Depts = (Ndep-Idep+1)/2
!*****
!  INDEPENDENT AND DEPENDENT POINT COORDINATE LISTS ARE NOW
!  COMPLETE.  CALL FOR INTERPOLATION.
!*****
         CALL curvit(z(Iindep),Indpts,z(Idep),Depts,Scr5,z(Ndep+1),Iz(Ndep+1),Lcore-Ndep-1,Ip2,15.0,Mcsid,xrange,yrange)
!
!  BRING -OES1M- SIGMAS INTO CORE FOR CURRENT -MCSID- PASS.
!
         Isigma = Iindep + 1
         Nsigma = Iindep + 6*Indpts
         jsigma = Isigma - 7
         icrq = Nsigma - Ibuf3
         IF ( Nsigma>=Ibuf3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         File = Scr3
         Loc = 800
         CALL open(*60,Scr3,Iz(Ibuf1),Rdrew)
         CALL read(*80,*20,Scr3,Iz(Isigma),Ibuf3-Isigma,noeor,Nwords)
         Loc = 810
         GOTO 40
!
 20      IF ( Nwords/=6*Indpts ) GOTO 40
         CALL close(Scr3,Clsrew)
!
!    (SIGMAS                ) = (G)(SIGMAS                        )
!           DEPENDENT POINTS              OES1M INDEPENDENT POINTS
!
!  SINCE THE ORDER OF THE ROWS IN THE G MATRIX ARE IN SORTED EXTERNAL
!  GRID ORDER EACH OUTPUT LINE OF OES1G WILL BE HANDLED ON ITS
!  OWN. THIS ELIMINATES NECESSITY OF HOLDING ANOTHER SIGMA ARRAY
!  IN CORE.
!
         File = Oes1g
         Loc = 815
         CALL open(*60,Oes1g,Iz(Ibuf1),Wrt)
!
!  OUTPUT ID RECORD. PREVIOUSLY PREPARED.
!
         Idrec(3) = Idrec(3) + 2000
         CALL write(Oes1g,Idrec(1),146,eor)
         mcb(1) = Oes1g
         CALL wrttrl(mcb(1))
         Any1g = .TRUE.
!
!  OPEN SCR5 CONTAINING ROWS OF THE G-MATRIX.
!
         File = Scr5
         CALL open(*60,Scr5,Iz(Ibuf3),Rdrew)
         CALL fwdrec(*80,Scr5)
!
!  OPEN SCR4 CONTAINING LIST OF EXTERNAL IDS )
!
         File = Scr4
         CALL open(*60,Scr4,Iz(Ibuf2),Rdrew)
!
!  COMPUTE AND OUTPUT SIGMAS FOR THE DEPENDENT POINTS
!
         Buf(2) = Mcsid
         DO I = 1 , Depts
!
!  READ THE EXTERNAL ID
!
            File = Scr4
            CALL read(*80,*100,Scr4,Buf(1),1,noeor,Nwords)
            File = Scr5
!
!  INITIALIZE SIGMAS(DEPENDENT POINT) TO ZERO
!
            DO J = 3 , 8
               rbuf(J) = 0.0
            ENDDO
!
            K = 0
            Loc = 825
            DO
!
!  READ ACTIVE INDEX AND G-VALUE FROM SCRATCH 5
!
               CALL read(*80,*30,Scr5,rbuf(11),2,noeor,Nwords)
               K = K + 10
               idx = jsigma + 6*Buf(11)
               DO J = 1 , 6
                  rbuf(J+2) = rbuf(J+2) + rbuf(12)*z(idx+J)
               ENDDO
            ENDDO
!
!  IF THERE WERE ANY G-VALUES THEN NOW COMPLETE THE OUTPUT LINE.
!
 30         IF ( K>0 ) THEN
!
               Buf(10) = K + kctype
!
               rbuf(11) = rbuf(6)
               rbuf(12) = rbuf(7)
               rbuf(13) = rbuf(8)
!
!  COMPUTE INVARIANTS FOR EACH LINE
!
               CALL curvps(rbuf(3),rbuf(6))
               CALL curvps(rbuf(11),rbuf(14))
               IF ( Strain ) THEN
                  rbuf(5) = 2.0*rbuf(5)
                  rbuf(9) = 2.0*rbuf(9)
                  rbuf(13) = 2.0*rbuf(13)
                  rbuf(17) = 2.0*rbuf(17)
               ENDIF
!
!  APPEND DEVICE CODE TO EXTERNAL ID AND OUTPUT LINE
!
               Buf(1) = 10*Buf(1) + Device
               CALL write(Oes1g,Buf(1),17,noeor)
            ENDIF
         ENDDO
!
         CALL write(Oes1g,0,0,eor)
         IF ( Eofos1 .AND. Jmcsid+2>Nmcsid ) CALL close(Oes1g,Clsrew)
         CALL close(Oes1g,Cls)
         CALL close(Scr4,Clsrew)
         CALL close(Scr5,Clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!*****
!  ALL INDEPENDENT POINTS OUTPUT TO OES1G FOR 1 ACTIVE MCSID OF
!  CURRENT SUBCASE. GO TO NEXT MCSID.
!*****
         Jmcsid = Jmcsid + 2
         IF ( Jmcsid<=Nmcsid ) THEN
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
 40      Imsg = -Logerr
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      Imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      Imsg = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     Imsg = -3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         Imsg = -8
         Lcore = icrq
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv3
