
SUBROUTINE curv3
   IMPLICIT NONE
   LOGICAL Any , Any1g , Any1m , Anyout , Eofos1 , First , Foes1g , Strain
   INTEGER Buf(100) , Cls , Clsrew , Cstm , Cstms , Cstype , Depts , Device , Eltype , Eor , Est , Estwds , Ewords , File , Gpl ,   &
         & I , Ibuf1 , Ibuf2 , Ibuf3 , Ibuf4 , Icomp , Icstm , Ictype , Idep , Idoes1 , Idrec(146) , Idscr1 , Ieltyp , Iestx ,      &
         & Iext , Igmat , Iindep , Imatid , Imcsid , Imid , Imsg , Indpts , Ioes1m , Ioutpt , Ip1 , Ip2 , Isig1 , Isig2 , Isigma ,  &
         & Isil , Ising , Itran , Ivmat , Iwords , Ixyz1 , Ixyz2 , Iz(1) , J , Jcore , Jeltyp , Jindep , Jmcsid , Jp , Jsil , K ,   &
         & K1 , K2 , Kmcsid , Kount , L , Lbuf , Lcore , Lcstm , Lext , Lmcsid , Lmid , Loc , Loes1m , Logerr , Lsbuf , Lsil , Lx1 ,&
         & Lx2 , Matid , Mcsid
   INTEGER Mcsids , Mpt , Ncstm , Ndep , Nelems , Neltyp , Nestx , Next , Ngmat , Nindep , Nmcsid , Nmid , Nmids , Noeor , Noes1m , &
         & Npts , Npts4 , Nsigma , Nsil , Nwords , Oes1 , Oes1g , Oes1m , Oldid , Owords , Rd , Rdrew , Sbuf(10) , Scr1 , Scr2 ,    &
         & Scr3 , Scr4 , Scr5 , Sil , Subcas , Sysbuf , Wrt , Wrtrew
   REAL Rbuf(100) , Vec(3) , Vmax(3) , Vmin(3) , Z(1)
   COMMON /blank / Ip1 , Ip2
   COMMON /curvc1/ Lsbuf , Sbuf
   COMMON /curvc2/ Lbuf , Buf
   COMMON /curvc3/ Vec , Vmax , Vmin , Idrec
   COMMON /curvtb/ Imid , Nmid , Lmid , Nmids , Ieltyp , Neltyp , Jeltyp , Icstm , Ncstm , Cstms , Lcstm , Iestx , Nestx , Imcsid , &
                 & Nmcsid , Lmcsid , Mcsids , Jmcsid , Kmcsid , Isil , Nsil , Lsil , Jsil , Ioes1m , Noes1m , Loes1m , Idep , Ndep ,&
                 & Iindep , Nindep , Jindep , Isigma , Nsigma , Igmat , Ngmat , Iext , Next , Lext , Scr1 , Scr2 , Scr3 , Scr4 ,    &
                 & Oes1m , Oes1g , Oes1 , Mpt , Cstm , Est , Sil , Gpl , Jcore , Lcore , Ibuf1 , Ibuf2 , Ibuf3 , Ibuf4 , I , J , K ,&
                 & L , K1 , K2 , Ixyz1 , Ixyz2 , Lx1 , Lx2 , Eltype , Mcsid , Idscr1 , Idoes1 , Npts , Npts4 , Iwords , Nwords ,    &
                 & Subcas , Kount , Isig1 , Isig2 , Loc , File , Imsg , Nelems , Imatid , Icomp , Estwds , Ewords , Jp , Owords ,   &
                 & Matid , Depts , Indpts , Ictype , Ivmat , Itran , Cstype , Ising , Device , Oldid , Any , Eofos1 , First ,       &
                 & Anyout , Foes1g , Strain , Logerr , Any1m , Any1g , Scr5
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Ioutpt
   COMMON /zzzzzz/ Iz
   REAL avgl , fl , xrange , xsqysq , yrange
   INTEGER icrq , idx , jsigma , kctype , mcb(7)
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
   EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1))
   EQUIVALENCE (Noeor,Rdrew) , (Eor,Cls)
!
   DATA mcb/7*1/
!
!  BRING OES1G -ID- RECORD INTO CORE AND MODIFY AS NECESSARY.
!
   File = Scr3
   Loc = 50
   CALL open(*700,Scr3,Iz(Ibuf1),Rdrew)
   CALL read(*800,*900,Scr3,Idrec(1),146,Noeor,Nwords)
   CALL close(Scr3,Clsrew)
!
!
!
!
!  OVERALL LOOP IS ON ENTRIES OF TABLE(IMCSID-NMCSID)
!
   Jmcsid = Imcsid
!
 100  Mcsid = Iz(Jmcsid)
   Indpts = Iz(Jmcsid+1)
   Loc = 100
   IF ( Indpts<0 ) GOTO 600
   IF ( Indpts==0 ) GOTO 400
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
   IF ( Ndep>Jcore ) GOTO 1000
!
   CALL open(*700,Scr2,Iz(Ibuf1),Rdrew)
   File = Scr3
   CALL open(*700,Scr3,Iz(Ibuf2),Wrtrew)
!
   Jindep = Iindep
   File = Scr2
!
!  FIND -INDPTS- NUMBER OF INDEPENDENT ELEMENT POINTS ENTRIES
!  FOR CURRENT -MCSID- PASS. (LOGIC ERROR IF CAN NOT FIND THIS MANY)
!
   DO I = 1 , Indpts
      DO
!
!  READ ELEMENT INDEPENDENT PORTION OF ENTRY
!
         Loc = 150
         CALL read(*800,*900,Scr2,Buf(1),11,Noeor,Nwords)
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
            CALL write(Scr3,Buf(2),6,Noeor)
            Z(Jindep) = Rbuf(8)
            Z(Jindep+1) = Rbuf(9)
            Z(Jindep+2) = Rbuf(10)
            Jindep = Jindep + 3
!
!  INDEPENDENT POINTS NOT YET IN CORE ARE ADDED.
!
            CALL read(*800,*900,Scr2,Buf(1),Npts4,Noeor,Nwords)
            K = Npts
            DO J = 1 , Npts
!
!  CHECK IF EXTERNAL ID IS IN TABLE YET.
!
               IF ( Ndep>=Idep ) THEN
                  DO L = Idep , Ndep , 4
                     IF ( Buf(J)==Iz(L) ) GOTO 105
                  ENDDO
               ENDIF
!
!  NOT YET IN THUS ADD IT TO TABLE
!
               icrq = Ndep + 4 - Jcore
               IF ( Ndep+4>Jcore ) GOTO 1000
               Iz(Ndep+1) = Buf(J)
               Z(Ndep+2) = Rbuf(K+1)
               Z(Ndep+3) = Rbuf(K+2)
               Z(Ndep+4) = Rbuf(K+3)
               Ndep = Ndep + 4
!
 105           K = K + 3
!
            ENDDO
            EXIT
         ELSE
!
!  NO IT IS NOT THUS SKIP BALANCE OF ENTRY.
!
            Loc = 170
            CALL read(*800,*900,Scr2,0,-Npts4,Noeor,Nwords)
         ENDIF
      ENDDO
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
   CALL sort(0,0,4,1,Z(Idep),Ndep-Idep+1)
!*****
!  CONVERSION OF INDEPENDENT AND DEPENDENT POINTS TO LOCAL
!  MATERIAL COORDINATE SYSTEM. FIRST GET CSTM DATA TO USE.
!*****
   Loc = 400
   CALL bisloc(*600,Mcsid,Iz(Icstm),14,Cstms,Jp)
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
      Vec(1) = Z(I) - Z(Ivmat)
      Vec(2) = Z(I+1) - Z(Ivmat+1)
      Vec(3) = Z(I+2) - Z(Ivmat+2)
      CALL gmmats(Z(Itran),3,3,1,Vec(1),3,1,0,Z(I))
   ENDDO
!
   DO I = Idep , Ndep , 4
      Vec(1) = Z(I+1) - Z(Ivmat)
      Vec(2) = Z(I+2) - Z(Ivmat+1)
      Vec(3) = Z(I+3) - Z(Ivmat+2)
      CALL gmmats(Z(Itran),3,3,1,Vec(1),3,1,0,Z(I+1))
   ENDDO
!*****
!  CONVERSION OF INDEPENDENT POINT LOCAL COORDINATES TO MAPPING
!  COORDINATES. (IF MCSID IS A RECTANGULAR SYSTEM THEN NO CHANGE.)
!*****
   Loc = 490
   IF ( Ictype<1 .OR. Ictype>3 ) GOTO 600
   IF ( Ictype==1 ) THEN
   ELSEIF ( Ictype==3 ) THEN
!
!  SPHERICAL COORDINATES
!
      avgl = 0.0
      DO I = Iindep , Nindep , 3
         xsqysq = Z(I)**2 + Z(I+1)**2
         fl = sqrt(xsqysq)
         Vec(1) = sqrt(xsqysq+Z(I+2)**2)
         avgl = avgl + Vec(1)
         IF ( Vec(1)>0.0 ) THEN
            Vec(2) = atan2(fl,Z(I+2))
         ELSE
            Vec(2) = 0.0
         ENDIF
         IF ( fl>0.0 ) THEN
            Vec(3) = atan2(Z(I+1),Z(I))
         ELSE
            Vec(3) = 0.0
         ENDIF
         Z(I) = Vec(1)
         Z(I+1) = Vec(2)
         Z(I+2) = Vec(3)
      ENDDO
      avgl = avgl/float(Indpts)
   ELSE
!
!  CYLINDRICAL COORDINATES
!
      avgl = 0.0
      DO I = Iindep , Nindep , 3
         Vec(1) = sqrt(Z(I)**2+Z(I+1)**2)
         avgl = avgl + Vec(1)
         IF ( Vec(1)<=0.0 ) THEN
            Z(I+1) = 0.0
         ELSE
            Z(I+1) = atan2(Z(I+1),Z(I))
         ENDIF
         Z(I) = Vec(1)
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
         xsqysq = Z(I+1)**2 + Z(I+2)**2
         fl = sqrt(xsqysq)
         Vec(1) = sqrt(xsqysq+Z(I+3)**2)
         IF ( Vec(1)>0.0 ) THEN
            Vec(2) = atan2(fl,Z(I+3))
         ELSE
            Vec(2) = 0.0
         ENDIF
         IF ( fl>0.0 ) THEN
            Vec(3) = atan2(Z(I+2),Z(I+1))
         ELSE
            Vec(3) = 0.0
         ENDIF
         Z(I+1) = Vec(1)
         Z(I+2) = Vec(2)
         Z(I+3) = Vec(3)
      ENDDO
   ELSE
!
!  CYLINDRICAL COORDINATES
!
      DO I = Idep , Ndep , 4
         Vec(1) = sqrt(Z(I+1)**2+Z(I+2)**2)
         IF ( Vec(1)<=0.0 ) THEN
            Z(I+2) = 0.0
         ELSE
            Z(I+2) = atan2(Z(I+2),Z(I+1))
         ENDIF
         Z(I+1) = Vec(1)
      ENDDO
   ENDIF
!
!  SET MAXIMUM AND MIMIMUM X,Y,Z VALUES.
!
   DO I = 1 , 3
      Vmax(I) = Z(Iindep+I-1)
      Vmin(I) = Z(Iindep+I-1)
   ENDDO
!
   DO I = Iindep , Nindep , 3
      DO J = 1 , 3
         Vmax(J) = amax1(Z(I+J-1),Vmax(J))
         Vmin(J) = amin1(Z(I+J-1),Vmin(J))
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
         GOTO 200
      ENDIF
   ELSEIF ( Vmax(2)<Vmax(3) ) THEN
      K1 = 1
      K2 = 3
      kctype = 2
      GOTO 200
   ENDIF
   K1 = 1
   K2 = 2
   kctype = 3
!
 200  xrange = Vec(K1)
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
   CALL open(*700,Scr4,Iz(Ibuf1),Wrtrew)
   DO I = Idep , Ndep , 4
      CALL write(Scr4,Iz(I),1,Noeor)
   ENDDO
   CALL close(Scr4,Clsrew)
!
!  REDUCE INDEPENDENT POINTS TO XY PAIRS, SCALE BY X AND Y RANGES
!  RESPECTIVELY, AND COMPRESS IN CORE.
!
   J = Iindep
   DO I = Iindep , Nindep , 3
      Z(J) = Z(I+K1-1)/xrange
      Z(J+1) = Z(I+K2-1)/yrange
      J = J + 2
   ENDDO
   Nindep = J - 1
!
!  REDUCE DEPENDENT POINTS LIST. (J IS STILL GOOD)
!
   DO I = Idep , Ndep , 4
      Z(J) = Z(I+K1)/xrange
      Z(J+1) = Z(I+K2)/yrange
      J = J + 2
   ENDDO
   Idep = Nindep + 1
   Ndep = J - 1
   Depts = (Ndep-Idep+1)/2
!*****
!  INDEPENDENT AND DEPENDENT POINT COORDINATE LISTS ARE NOW
!  COMPLETE.  CALL FOR INTERPOLATION.
!*****
   CALL curvit(Z(Iindep),Indpts,Z(Idep),Depts,Scr5,Z(Ndep+1),Iz(Ndep+1),Lcore-Ndep-1,Ip2,15.0,Mcsid,xrange,yrange)
!
!  BRING -OES1M- SIGMAS INTO CORE FOR CURRENT -MCSID- PASS.
!
   Isigma = Iindep + 1
   Nsigma = Iindep + 6*Indpts
   jsigma = Isigma - 7
   icrq = Nsigma - Ibuf3
   IF ( Nsigma>=Ibuf3 ) GOTO 1000
   File = Scr3
   Loc = 800
   CALL open(*700,Scr3,Iz(Ibuf1),Rdrew)
   CALL read(*800,*300,Scr3,Iz(Isigma),Ibuf3-Isigma,Noeor,Nwords)
   Loc = 810
   GOTO 600
!
 300  IF ( Nwords/=6*Indpts ) GOTO 600
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
   CALL open(*700,Oes1g,Iz(Ibuf1),Wrt)
!
!  OUTPUT ID RECORD. PREVIOUSLY PREPARED.
!
   Idrec(3) = Idrec(3) + 2000
   CALL write(Oes1g,Idrec(1),146,Eor)
   mcb(1) = Oes1g
   CALL wrttrl(mcb(1))
   Any1g = .TRUE.
!
!  OPEN SCR5 CONTAINING ROWS OF THE G-MATRIX.
!
   File = Scr5
   CALL open(*700,Scr5,Iz(Ibuf3),Rdrew)
   CALL fwdrec(*800,Scr5)
!
!  OPEN SCR4 CONTAINING LIST OF EXTERNAL IDS )
!
   File = Scr4
   CALL open(*700,Scr4,Iz(Ibuf2),Rdrew)
!
!  COMPUTE AND OUTPUT SIGMAS FOR THE DEPENDENT POINTS
!
   Buf(2) = Mcsid
   DO I = 1 , Depts
!
!  READ THE EXTERNAL ID
!
      File = Scr4
      CALL read(*800,*900,Scr4,Buf(1),1,Noeor,Nwords)
      File = Scr5
!
!  INITIALIZE SIGMAS(DEPENDENT POINT) TO ZERO
!
      DO J = 3 , 8
         Rbuf(J) = 0.0
      ENDDO
!
      K = 0
      Loc = 825
      DO
!
!  READ ACTIVE INDEX AND G-VALUE FROM SCRATCH 5
!
         CALL read(*800,*350,Scr5,Rbuf(11),2,Noeor,Nwords)
         K = K + 10
         idx = jsigma + 6*Buf(11)
         DO J = 1 , 6
            Rbuf(J+2) = Rbuf(J+2) + Rbuf(12)*Z(idx+J)
         ENDDO
      ENDDO
!
!  IF THERE WERE ANY G-VALUES THEN NOW COMPLETE THE OUTPUT LINE.
!
 350  IF ( K>0 ) THEN
!
         Buf(10) = K + kctype
!
         Rbuf(11) = Rbuf(6)
         Rbuf(12) = Rbuf(7)
         Rbuf(13) = Rbuf(8)
!
!  COMPUTE INVARIANTS FOR EACH LINE
!
         CALL curvps(Rbuf(3),Rbuf(6))
         CALL curvps(Rbuf(11),Rbuf(14))
         IF ( Strain ) THEN
            Rbuf(5) = 2.0*Rbuf(5)
            Rbuf(9) = 2.0*Rbuf(9)
            Rbuf(13) = 2.0*Rbuf(13)
            Rbuf(17) = 2.0*Rbuf(17)
         ENDIF
!
!  APPEND DEVICE CODE TO EXTERNAL ID AND OUTPUT LINE
!
         Buf(1) = 10*Buf(1) + Device
         CALL write(Oes1g,Buf(1),17,Noeor)
      ENDIF
   ENDDO
!
   CALL write(Oes1g,0,0,Eor)
   IF ( Eofos1 .AND. Jmcsid+2>Nmcsid ) CALL close(Oes1g,Clsrew)
   CALL close(Oes1g,Cls)
   CALL close(Scr4,Clsrew)
   CALL close(Scr5,Clsrew)
!*****
!  ALL INDEPENDENT POINTS OUTPUT TO OES1G FOR 1 ACTIVE MCSID OF
!  CURRENT SUBCASE. GO TO NEXT MCSID.
!*****
 400  Jmcsid = Jmcsid + 2
   IF ( Jmcsid<=Nmcsid ) GOTO 100
!*****
!  ALL THROUGH FORMING OES1G FOR CURRENT SUBCASE.
!*****
 500  RETURN
!*****
!  ERROR CONDITION ENCOUNTERED
!*****
 600  Imsg = -Logerr
   GOTO 500
 700  Imsg = -1
   GOTO 500
 800  Imsg = -2
   GOTO 500
 900  Imsg = -3
   GOTO 500
 1000 Imsg = -8
   Lcore = icrq
   GOTO 500
END SUBROUTINE curv3
