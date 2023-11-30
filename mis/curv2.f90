
SUBROUTINE curv2
   IMPLICIT NONE
   LOGICAL Any , Any1g , Any1m , Anyout , Eofos1 , First , Foes1g , Strain
   INTEGER Buf(100) , Cls , Clsrew , Cstm , Cstms , Cstype , Depts , Device , Eltype , Eor , Est , Estwds , Ewords , File , Gpl ,   &
         & I , Ibuf1 , Ibuf2 , Ibuf3 , Ibuf4 , Icmplx , Icomp , Icstm , Ictype , Idep , Idoes1 , Idrec(146) , Idscr1 , Ieltyp ,     &
         & Iestx , Iext , Igmat , Iindep , Imatid , Imcsid , Imid , Imsg , Indpts , Ioes1m , Ioutpt , Ip1 , Ip2 , Isig1 , Isig2 ,   &
         & Isigma , Isil , Ising , Itran , Ivmat , Iwords , Ixyz1 , Ixyz2 , Iz(1) , J , Jcore , Jeltyp , Jindep , Jmcsid , Jp ,     &
         & Jsil , K , K1 , K2 , Kmcsid , Kount , L , Lbuf , Lcore , Lcstm , Lext , Lmcsid , Lmid , Loc , Loes1m , Logerr , Lsbuf ,  &
         & Lsil , Lx1 , Lx2 , Matid
   REAL Degrad , Raddeg , Rbuf(100) , S4pisq , Val2pi , Valpi , Vec(3) , Vmax(3) , Vmin(3) , Z(1) , Zdum(3)
   INTEGER Mcsid , Mcsids , Mpt , Ncstm , Ndep , Nelems , Neltyp , Nestx , Next , Ngmat , Nindep , Nmcsid , Nmid , Nmids , Noeor ,  &
         & Noes1m , Npts , Npts4 , Nsigma , Nsil , Nwords , Oes1 , Oes1g , Oes1m , Oldid , Owords , Rd , Rdrew , Sbuf(10) , Scr1 ,  &
         & Scr2 , Scr3 , Scr4 , Scr5 , Sil , Subcas , Sysbuf , Wrt , Wrtrew
   COMMON /blank / Ip1 , Ip2 , Icmplx , Zdum
   COMMON /condas/ Valpi , Val2pi , Raddeg , Degrad , S4pisq
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
   INTEGER icrq , iformt , incr , ll1 , ll2 , lll , mcb(7) , mm1 , mm2
   REAL u(9) , ztemp
!*****
! PASSES NEXT SUBCASE OF ELEMENT STRESS OR STRAIN DATA IN OES1
! AND OUTPUTS OES1M FOR THIS SUBCASE. SETS UP FILES AND TABLES
! FOR -CURV3- IF OES1G IS TO BE FORMED.
!
!     OPEN CORE MAP DURING -CURV2- EXECUTION.
!     =======================================
!
!     FROM-------+------------+
!     CURV1      I  Z(IELTYP) I  MASTER LIST OF ELEMENT TYPES THAT
!     EXECUTION  I    THRU    I  EXIST ON ESTX(SCR1)
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
!     AND DURING I  Z(IESTX)  I  SPACE FOR ONE ENTRY OF ESTX(SCR1)
!     CURV2      I    THRU    I  ENTRIES.  (SIZE IS ELEMENT DEPENDENT)
!     EXECUTION  I  Z(NESTX)  I
!                +------------+
!                I  Z(IOES1M) I  TABLE OF INCR-WORD .RIES FOR 1 ELEMENT
!                I    THRU    I  TYPE.  CONTAINS ELEMENT-ID,MCSID,XY-
!                I  Z(NOES1M) I  COMPONENT-CODE, AND (INCR-3) SIGMAS.
!                +------------+  INCR =  9 FOR REAL STRESS
!                I     .      I       = 15 FOR COMPLEX STRESS
!                I     .      I  AVAILABLE CORE.
!                I     .      I
!                I     .      I
!                I     .      I
!                I  Z(JCORE)  I
!                +------------+
!                I  Z(IBUF4)  I  GINO-BUFFER(OES1M)
!                I            I
!                +------------+
!                I  Z(IBUF3)  I  GINO-BUFFER(SCR2 AND SCR3)
!                I            I
!                +------------+
!                I  Z(IBUF2)  I  GINO-BUFFER(SCR1)
!                I            I
!                +------------+
!                I  Z(IBUF1)  I  GINO-BUFFER(OES1)
!                I            I
!                I  Z(LCORE)  I
!                +------------+
!
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
!  OPEN OES1M FOR ANY POSSIBLE OUTPUTS DURING THIS SUBCASE PASS.
!
   Isig1 = 3
   Isig2 = 11
   File = Oes1m
   Loc = 60
   CALL open(*1100,Oes1m,Iz(Ibuf3),Wrt)
!
!  OPEN OES1 NOREWIND TO CONTINUE
!
   First = .TRUE.
   Any = .FALSE.
   File = Oes1
   CALL open(*1100,Oes1,Iz(Ibuf1),Rd)
   File = Scr1
   CALL open(*1100,Scr1,Iz(Ibuf2),Rdrew)
!
!  ZERO ELEMENT COUNTS FOR EACH -MCSID- THIS SUBCASE MAY REFERENCE.
!
   DO I = Imcsid , Nmcsid , 2
      Iz(I+1) = 0
   ENDDO
!
!  READ NEXT ID-RECORD
!
 100  File = Oes1
   Loc = 100
   CALL read(*300,*1300,Oes1,Idrec(1),146,Eor,Nwords)
!
!  CHECK IF STILL SAME SUBCASE UNLESS THIS IS THE FIRST ID-RECORD OF A
!  SUBCASE GROUP.
!
   IF ( .NOT.First ) THEN
!
!  CHECKING FOR CHANGE IN SUBCASE
!
      IF ( Subcas==Idrec(4) ) GOTO 400
!
!  CHANGE IN SUBCASE THUS BACK RECORD OVER THIS ID-RECORD CLOSE
!  OES1, AND WRAP UP OPERATIONS ON OES1M FOR CURRENT SUBCASE.
!
      CALL bckrec(Oes1)
      CALL close(Oes1,Cls)
   ELSE
!
!  YES THIS IS FIRST ID-RECORD OF A SUBCASE GROUP.
!  SET SUBCASE IDENTIFIERS.
!
      Subcas = Idrec(4)
      First = .FALSE.
      GOTO 400
   ENDIF
!
!  CLOSE ESTX(SCR1) AND ESTXX(SCR2).
!
 200  CALL close(Scr1,Clsrew)
   CALL close(Scr2,Clsrew)
   GOTO 900
!
!  END OF FILE ON OES1. SET EOF FLAG AND WRAP UP CURRENT OPERATIONS
!  ON OES1M.
!
 300  Eofos1 = .TRUE.
   CALL close(Oes1,Clsrew)
   CALL close(Oes1m,Clsrew)
   GOTO 200
!
!  ID RECORD ON OES1 WILL BE FOR SOME KIND OF ELEMENT.
!  CHECK TO SEE IF ITS TYPE IS IN THE LIST OF TYPES NOW ON SCR1
!  WHICH IS THE ABBREVIATED EST. IF NOT THEN SKIP THE DATA RECORD
!  AND GO TO NEXT ID RECORD.
!
 400  Eltype = Idrec(3)
   iformt = Idrec(9)
   Owords = Idrec(10)
   DO I = Ieltyp , Neltyp
      IF ( Eltype==Iz(I) ) GOTO 500
   ENDDO
   CALL fwdrec(*300,Oes1)
   GOTO 100
!
!  POSITION TO SCR1 RECORD FOR THIS ELEMENT TYPE. IF IT CAN NOT BE
!  FOUND BY FORWARD SEARCH THERE IS A LOGIC ERROR, OR OES1 ELEMENT
!  TYPES ARE NOT IN SAME ORDER AS EST ELEMENT TYPES.
!
 500  File = Scr1
   Loc = 600
   CALL rewind(Scr1)
   DO
      CALL read(*1200,*1300,Scr1,Buf(1),3,Noeor,Nwords)
      IF ( Buf(1)==Eltype ) THEN
!
!  NOW POSITIONED TO READ ELEMENT ENTRIES FROM ESTX(SCR1) WHICH
!  ARE OK FOR INCLUSION IN OES1M AND OES1G PROCESSING.
!
!  ALSO POSITIONED TO READ OUTPUT STRESS/STRAIN ENTRIES FROM OES1.
!  HOWEVER, ONLY THOSE ALSO ON ESTX(SCR1) WILL BE PULLED.
!
         Anyout = .FALSE.
         Ewords = Buf(2)
         Npts = Buf(3)
         Npts4 = 4*Npts
         Iestx = Ncstm + 1
         Nestx = Ncstm + Ewords
         Ioes1m = Nestx + 1
         Noes1m = Nestx
         Loc = 650
         icrq = Noes1m - Jcore
         IF ( Noes1m>Jcore ) GOTO 1400
         Idscr1 = 0
         DO
!
!  READ NEXT OES1 ENTRY AND SET IDOES1  (STRIPPING OFF DEVICE CODE)
!
            File = Oes1
            Loc = 670
            CALL read(*1200,*600,Oes1,Buf(1),Owords,Noeor,Nwords)
            Idoes1 = Buf(1)/10
            IF ( Idoes1<=0 ) GOTO 1000
            DO
!
!  CHECK FOR MATCH OF ESTX(SCR1) ENTRY ID WITH OES1 ENTRY ID.
!
               IF ( Idoes1<Idscr1 ) EXIT
               IF ( Idoes1==Idscr1 ) THEN
!
!  MATCH FOUND THUS BEGIN OES1M ENTRY CALCULATIONS
!
                  Mcsid = Iz(Iestx+1)
                  Loc = 710
                  CALL tranem(Mcsid,Npts,Z(Iestx+Npts+2),Icomp,u(1),Vec(1))
!
!  FORM AND ADD ENTRY TO CORE. INVARIANTS WILL BE COMPUTED LATER.
!
                  incr = 9
                  IF ( Icmplx==1 ) incr = 15
                  icrq = Noes1m + incr - Jcore
                  IF ( Noes1m+incr>Jcore ) GOTO 1400
                  Iz(Noes1m+1) = Buf(1)
                  Iz(Noes1m+2) = Mcsid
                  Iz(Noes1m+3) = Icomp
                  IF ( Icmplx==1 ) THEN
!
                     IF ( iformt==3 ) THEN
                        DO mm1 = 3 , 10 , 7
                           mm2 = mm1 + 4
                           DO lll = mm1 , mm2 , 2
                              ztemp = Rbuf(lll)*cos(Rbuf(lll+1)*Degrad)
                              Rbuf(lll+1) = Rbuf(lll)*sin(Rbuf(lll+1)*Degrad)
                              Rbuf(lll) = ztemp
                           ENDDO
                        ENDDO
                     ENDIF
                     Zdum(1) = Rbuf(3)
                     Zdum(2) = Rbuf(5)
                     Zdum(3) = Rbuf(7)
                     CALL gmmats(u(1),3,3,0,Zdum,3,1,0,Z(Noes1m+4))
                     Zdum(1) = Rbuf(4)
                     Zdum(2) = Rbuf(6)
                     Zdum(3) = Rbuf(8)
                     CALL gmmats(u(1),3,3,0,Zdum,3,1,0,Z(Noes1m+7))
                     Zdum(1) = Rbuf(10)
                     Zdum(2) = Rbuf(12)
                     Zdum(3) = Rbuf(14)
                     CALL gmmats(u(1),3,3,0,Zdum,3,1,0,Z(Noes1m+10))
                     Zdum(1) = Rbuf(11)
                     Zdum(2) = Rbuf(13)
                     Zdum(3) = Rbuf(15)
                     CALL gmmats(u(1),3,3,0,Zdum,3,1,0,Z(Noes1m+13))
!
                     IF ( iformt==3 ) THEN
                        DO mm1 = 4 , 10 , 6
                           mm2 = mm1 + 2
                           DO lll = mm1 , mm2
                              ll1 = Noes1m + lll
                              ll2 = ll1 + 3
                              ztemp = sqrt(Z(ll1)**2+Z(ll2)**2)
                              IF ( ztemp/=0.0 ) THEN
                                 Z(ll2) = atan2(Z(ll2),Z(ll1))*Raddeg
                                 IF ( Z(ll2)<-0.00005E0 ) Z(ll2) = Z(ll2) + 360.0
                              ELSE
                                 Z(ll2) = 0.0
                              ENDIF
                              Z(ll1) = ztemp
                           ENDDO
                        ENDDO
                     ENDIF
!
                     Noes1m = Noes1m + 15
                  ELSE
!
!  IF STRAINS DO MODIFICATION OF GAMMA
!
                     IF ( Strain ) THEN
                        Rbuf(Isig1+2) = Rbuf(Isig1+2)/2.0
                        Rbuf(Isig2+2) = Rbuf(Isig2+2)/2.0
                     ENDIF
                     CALL gmmats(u(1),3,3,0,Rbuf(Isig1),3,1,0,Z(Noes1m+4))
                     CALL gmmats(u(1),3,3,0,Rbuf(Isig2),3,1,0,Z(Noes1m+7))
!
                     Noes1m = Noes1m + 9
                  ENDIF
!
!
!  IF THIS IS THE FIRST ELEMENT ENTRY TO BE FOUND
!  AND OES1G IS TO BE FORMED, THE ID-RECORD IS SAVED FOR USE BY
!  CURV3 OVERLAY.
!
                  IF ( Foes1g ) THEN
                     IF ( .NOT.(Any) ) THEN
                        File = Scr3
                        Loc = 740
                        CALL open(*1100,Scr3,Iz(Ibuf4),Wrtrew)
                        CALL write(Scr3,Idrec(1),146,Eor)
                        CALL close(Scr3,Clsrew)
!
                        File = Scr2
                        CALL open(*1100,Scr2,Iz(Ibuf4),Wrtrew)
                        Device = mod(Buf(1),10)
                        Any = .TRUE.
                     ENDIF
!
!  OUTPUT SPECIAL ESTXX (SCR2) ENTRY FOR USE BY CURV3.
!
                     CALL write(Scr2,Mcsid,1,Noeor)
                     CALL write(Scr2,Z(Noes1m-5),6,Noeor)
                     CALL write(Scr2,Vec(1),3,Noeor)
                     CALL write(Scr2,Npts,1,Noeor)
                     CALL write(Scr2,Iz(Iestx+2),Npts4,Noeor)
                  ENDIF
                  EXIT
               ELSE
!
!  READ NEXT SCR1 ENTRY AND SET IDSCR1
!
                  File = Scr1
                  Loc = 680
                  CALL read(*1200,*700,Scr1,Iz(Iestx),Ewords,Noeor,Nwords)
                  Idscr1 = Iz(Iestx)
                  IF ( Idscr1<=0 ) GOTO 1000
               ENDIF
            ENDDO
         ENDDO
      ELSE
         CALL fwdrec(*1200,Scr1)
      ENDIF
   ENDDO
!*****
!  END OF ENTRY DATA POSSIBLE FOR THIS ELEMENT TYPE
!******
!
!  SKIP ANY UNUSED DATA IN ESTX (SCR1) DATA RECORD FOR THIS ELEMENT TYPE
!
 600  File = Scr1
   Loc = 900
   CALL fwdrec(*1200,Scr1)
   GOTO 800
!
!  SKIP ANY UNUSED DATA IN OES1 DATA RECORD FOR THIS ELEMENT TYPE.
!
 700  File = Oes1
   Loc = 950
   CALL fwdrec(*1200,Oes1)
!
!  IF ANY ENTRIES WERE FOUND AND COMPLETED AND PLACED IN CORE
!  THEY ARE SORTED ON -MCSID- AND OUTPUT. AS THEY ARE OUTPUT
!  THE INVARIANTS ARE COMPUTED.
!
 800  IF ( Noes1m>=Ioes1m ) THEN
!
!  YES THERE ARE SOME ENTRIES
!
      Loes1m = Noes1m - Ioes1m + 1
      CALL sort(0,0,incr,2,Iz(Ioes1m),Loes1m)
!
!  OUTPUT ID-RECORD, REDEFINE MAJOR-ID FOR OFP MODULE
!
!  RE-DEFINITION MISSING FOR NOW.
!
      Idrec(3) = Idrec(3) + 1000
      CALL write(Oes1m,Idrec(1),146,Eor)
      mcb(1) = Oes1m
      CALL wrttrl(mcb(1))
      Any1m = .TRUE.
!
!  MOVE AXIS CODE AND COMPLETE INVARIANTS OF EACH ENTRY.
!
      Kmcsid = Iz(Ioes1m+1)
      Kount = 0
!
      DO I = Ioes1m , Noes1m , incr
         Buf(1) = Iz(I)
         Buf(2) = Iz(I+1)
         Rbuf(3) = Z(I+3)
         IF ( Icmplx==1 ) THEN
            Rbuf(4) = Z(I+6)
            Rbuf(5) = Z(I+4)
            Rbuf(6) = Z(I+7)
            Rbuf(7) = Z(I+5)
            Rbuf(8) = Z(I+8)
            Buf(9) = Iz(I+2)
            Rbuf(10) = Z(I+9)
            Rbuf(11) = Z(I+12)
            Rbuf(12) = Z(I+10)
            Rbuf(13) = Z(I+13)
            Rbuf(14) = Z(I+11)
            Rbuf(15) = Z(I+14)
            CALL write(Oes1m,Buf(1),15,Noeor)
         ELSE
            Rbuf(4) = Z(I+4)
            Rbuf(5) = Z(I+5)
            CALL curvps(Rbuf(3),Rbuf(6))
            IF ( Strain ) THEN
               Rbuf(5) = 2.0*Rbuf(5)
               Rbuf(9) = 2.0*Rbuf(9)
            ENDIF
            Buf(10) = Iz(I+2)
            Rbuf(11) = Z(I+6)
            Rbuf(12) = Z(I+7)
            Rbuf(13) = Z(I+8)
            CALL curvps(Rbuf(11),Rbuf(14))
            IF ( Strain ) THEN
               Rbuf(13) = 2.0*Rbuf(13)
               Rbuf(17) = 2.0*Rbuf(17)
            ENDIF
            CALL write(Oes1m,Buf(1),17,Noeor)
         ENDIF
!
!  KEEP COUNT OF ELEMENTS IN EACH MCSID GROUP
!
         IF ( Iz(I+1)==Kmcsid ) THEN
            Kount = Kount + 1
            IF ( I+incr-1<Noes1m ) CYCLE
         ENDIF
!
!  CHANGE IN -MCSID- OF OUTPUT ENTRIES OR LAST ENTRY.
!  ADD COUNT OF ELEMENTS OF CURRENT TYPE TO TOTAL COUNT
!  OF ELEMENTS OF THIS -MCSID-.
!
         Loc = 965
         CALL bisloc(*1000,Kmcsid,Iz(Imcsid),2,Mcsids,Jp)
         Iz(Imcsid+Jp) = Iz(Imcsid+Jp) + Kount
         Kount = 1
         Kmcsid = Iz(I+1)
      ENDDO
      CALL write(Oes1m,0,0,Eor)
   ENDIF
   GOTO 100
!*****
!  ALL PROCESSING OF ONE SUBCASE COMPLETE FOR OES1M.
!*****
 900  CALL close(Oes1m,Cls)
   RETURN
!*****
!  ERROR CONDITION ENCOUNTERED
!*****
 1000 Imsg = -Logerr
   GOTO 900
 1100 Imsg = -1
   GOTO 900
 1200 Imsg = -2
   GOTO 900
 1300 Imsg = -3
   GOTO 900
 1400 Imsg = -8
   Lcore = icrq
   GOTO 900
END SUBROUTINE curv2
