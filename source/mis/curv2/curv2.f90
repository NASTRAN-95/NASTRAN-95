!*==curv2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv2
   USE c_blank
   USE c_condas
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
   INTEGER :: eor , icrq , iformt , incr , ll1 , ll2 , lll , mm1 , mm2 , noeor
   INTEGER , DIMENSION(7) , SAVE :: mcb
   REAL , DIMENSION(100) :: rbuf
   REAL , DIMENSION(9) :: u
   REAL , DIMENSION(1) :: z
   REAL :: ztemp
   EXTERNAL bckrec , bisloc , close , curvps , fwdrec , gmmats , open , read , rewind , sort , tranem , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1))
   !>>>>EQUIVALENCE (Noeor,Rdrew) , (Eor,Cls)
!
   DATA mcb/7*1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  OPEN OES1M FOR ANY POSSIBLE OUTPUTS DURING THIS SUBCASE PASS.
!
         isig1 = 3
         isig2 = 11
         file = oes1m
         loc = 60
         CALL open(*100,oes1m,iz(ibuf3),wrt)
!
!  OPEN OES1 NOREWIND TO CONTINUE
!
         first = .TRUE.
         any = .FALSE.
         file = oes1
         CALL open(*100,oes1,iz(ibuf1),rd)
         file = scr1
         CALL open(*100,scr1,iz(ibuf2),rdrew)
!
!  ZERO ELEMENT COUNTS FOR EACH -MCSID- THIS SUBCASE MAY REFERENCE.
!
         DO i = imcsid , nmcsid , 2
            iz(i+1) = 0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!  READ NEXT ID-RECORD
!
         file = oes1
         loc = 100
         CALL read(*20,*140,oes1,idrec(1),146,eor,nwords)
!
!  CHECK IF STILL SAME SUBCASE UNLESS THIS IS THE FIRST ID-RECORD OF A
!  SUBCASE GROUP.
!
         IF ( .NOT.first ) THEN
!
!  CHECKING FOR CHANGE IN SUBCASE
!
            IF ( subcas==idrec(4) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!  CHANGE IN SUBCASE THUS BACK RECORD OVER THIS ID-RECORD CLOSE
!  OES1, AND WRAP UP OPERATIONS ON OES1M FOR CURRENT SUBCASE.
!
            CALL bckrec(oes1)
            CALL close(oes1,cls)
         ELSE
!
!  YES THIS IS FIRST ID-RECORD OF A SUBCASE GROUP.
!  SET SUBCASE IDENTIFIERS.
!
            subcas = idrec(4)
            first = .FALSE.
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!  CLOSE ESTX(SCR1) AND ESTXX(SCR2).
!
         CALL close(scr1,clsrew)
         CALL close(scr2,clsrew)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!  END OF FILE ON OES1. SET EOF FLAG AND WRAP UP CURRENT OPERATIONS
!  ON OES1M.
!
 20      eofos1 = .TRUE.
         CALL close(oes1,clsrew)
         CALL close(oes1m,clsrew)
         spag_nextblock_1 = 3
      CASE (4)
!
!  ID RECORD ON OES1 WILL BE FOR SOME KIND OF ELEMENT.
!  CHECK TO SEE IF ITS TYPE IS IN THE LIST OF TYPES NOW ON SCR1
!  WHICH IS THE ABBREVIATED EST. IF NOT THEN SKIP THE DATA RECORD
!  AND GO TO NEXT ID RECORD.
!
         eltype = idrec(3)
         iformt = idrec(9)
         owords = idrec(10)
         DO i = ieltyp , neltyp
            IF ( eltype==iz(i) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL fwdrec(*20,oes1)
         spag_nextblock_1 = 2
      CASE (5)
!
!  POSITION TO SCR1 RECORD FOR THIS ELEMENT TYPE. IF IT CAN NOT BE
!  FOUND BY FORWARD SEARCH THERE IS A LOGIC ERROR, OR OES1 ELEMENT
!  TYPES ARE NOT IN SAME ORDER AS EST ELEMENT TYPES.
!
         file = scr1
         loc = 600
         CALL rewind(scr1)
         DO
            CALL read(*120,*140,scr1,buf(1),3,noeor,nwords)
            IF ( buf(1)==eltype ) THEN
!
!  NOW POSITIONED TO READ ELEMENT ENTRIES FROM ESTX(SCR1) WHICH
!  ARE OK FOR INCLUSION IN OES1M AND OES1G PROCESSING.
!
!  ALSO POSITIONED TO READ OUTPUT STRESS/STRAIN ENTRIES FROM OES1.
!  HOWEVER, ONLY THOSE ALSO ON ESTX(SCR1) WILL BE PULLED.
!
               anyout = .FALSE.
               ewords = buf(2)
               npts = buf(3)
               npts4 = 4*npts
               iestx = ncstm + 1
               nestx = ncstm + ewords
               ioes1m = nestx + 1
               noes1m = nestx
               loc = 650
               icrq = noes1m - jcore
               IF ( noes1m>jcore ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               idscr1 = 0
               DO
!
!  READ NEXT OES1 ENTRY AND SET IDOES1  (STRIPPING OFF DEVICE CODE)
!
                  file = oes1
                  loc = 670
                  CALL read(*120,*40,oes1,buf(1),owords,noeor,nwords)
                  idoes1 = buf(1)/10
                  IF ( idoes1<=0 ) GOTO 80
                  SPAG_Loop_3_1: DO
!
!  CHECK FOR MATCH OF ESTX(SCR1) ENTRY ID WITH OES1 ENTRY ID.
!
                     IF ( idoes1<idscr1 ) EXIT SPAG_Loop_3_1
                     IF ( idoes1==idscr1 ) THEN
!
!  MATCH FOUND THUS BEGIN OES1M ENTRY CALCULATIONS
!
                        mcsid = iz(iestx+1)
                        loc = 710
                        CALL tranem(mcsid,npts,z(iestx+npts+2),icomp,u(1),vec(1))
!
!  FORM AND ADD ENTRY TO CORE. INVARIANTS WILL BE COMPUTED LATER.
!
                        incr = 9
                        IF ( icmplx==1 ) incr = 15
                        icrq = noes1m + incr - jcore
                        IF ( noes1m+incr>jcore ) THEN
                           spag_nextblock_1 = 8
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        iz(noes1m+1) = buf(1)
                        iz(noes1m+2) = mcsid
                        iz(noes1m+3) = icomp
                        IF ( icmplx==1 ) THEN
!
                           IF ( iformt==3 ) THEN
                              DO mm1 = 3 , 10 , 7
                                 mm2 = mm1 + 4
                                 DO lll = mm1 , mm2 , 2
                                    ztemp = rbuf(lll)*cos(rbuf(lll+1)*degrad)
                                    rbuf(lll+1) = rbuf(lll)*sin(rbuf(lll+1)*degrad)
                                    rbuf(lll) = ztemp
                                 ENDDO
                              ENDDO
                           ENDIF
                           zdum(1) = rbuf(3)
                           zdum(2) = rbuf(5)
                           zdum(3) = rbuf(7)
                           CALL gmmats(u(1),3,3,0,zdum,3,1,0,z(noes1m+4))
                           zdum(1) = rbuf(4)
                           zdum(2) = rbuf(6)
                           zdum(3) = rbuf(8)
                           CALL gmmats(u(1),3,3,0,zdum,3,1,0,z(noes1m+7))
                           zdum(1) = rbuf(10)
                           zdum(2) = rbuf(12)
                           zdum(3) = rbuf(14)
                           CALL gmmats(u(1),3,3,0,zdum,3,1,0,z(noes1m+10))
                           zdum(1) = rbuf(11)
                           zdum(2) = rbuf(13)
                           zdum(3) = rbuf(15)
                           CALL gmmats(u(1),3,3,0,zdum,3,1,0,z(noes1m+13))
!
                           IF ( iformt==3 ) THEN
                              DO mm1 = 4 , 10 , 6
                                 mm2 = mm1 + 2
                                 DO lll = mm1 , mm2
                                    ll1 = noes1m + lll
                                    ll2 = ll1 + 3
                                    ztemp = sqrt(z(ll1)**2+z(ll2)**2)
                                    IF ( ztemp/=0.0 ) THEN
                                       z(ll2) = atan2(z(ll2),z(ll1))*raddeg
                                       IF ( z(ll2)<-0.00005E0 ) z(ll2) = z(ll2) + 360.0
                                    ELSE
                                       z(ll2) = 0.0
                                    ENDIF
                                    z(ll1) = ztemp
                                 ENDDO
                              ENDDO
                           ENDIF
!
                           noes1m = noes1m + 15
                        ELSE
!
!  IF STRAINS DO MODIFICATION OF GAMMA
!
                           IF ( strain ) THEN
                              rbuf(isig1+2) = rbuf(isig1+2)/2.0
                              rbuf(isig2+2) = rbuf(isig2+2)/2.0
                           ENDIF
                           CALL gmmats(u(1),3,3,0,rbuf(isig1),3,1,0,z(noes1m+4))
                           CALL gmmats(u(1),3,3,0,rbuf(isig2),3,1,0,z(noes1m+7))
!
                           noes1m = noes1m + 9
                        ENDIF
!
!
!  IF THIS IS THE FIRST ELEMENT ENTRY TO BE FOUND
!  AND OES1G IS TO BE FORMED, THE ID-RECORD IS SAVED FOR USE BY
!  CURV3 OVERLAY.
!
                        IF ( foes1g ) THEN
                           IF ( .NOT.(any) ) THEN
                              file = scr3
                              loc = 740
                              CALL open(*100,scr3,iz(ibuf4),wrtrew)
                              CALL write(scr3,idrec(1),146,eor)
                              CALL close(scr3,clsrew)
!
                              file = scr2
                              CALL open(*100,scr2,iz(ibuf4),wrtrew)
                              device = mod(buf(1),10)
                              any = .TRUE.
                           ENDIF
!
!  OUTPUT SPECIAL ESTXX (SCR2) ENTRY FOR USE BY CURV3.
!
                           CALL write(scr2,mcsid,1,noeor)
                           CALL write(scr2,z(noes1m-5),6,noeor)
                           CALL write(scr2,vec(1),3,noeor)
                           CALL write(scr2,npts,1,noeor)
                           CALL write(scr2,iz(iestx+2),npts4,noeor)
                        ENDIF
                        EXIT SPAG_Loop_3_1
                     ELSE
!
!  READ NEXT SCR1 ENTRY AND SET IDSCR1
!
                        file = scr1
                        loc = 680
                        CALL read(*120,*60,scr1,iz(iestx),ewords,noeor,nwords)
                        idscr1 = iz(iestx)
                        IF ( idscr1<=0 ) GOTO 80
                     ENDIF
                  ENDDO SPAG_Loop_3_1
               ENDDO
            ELSE
               CALL fwdrec(*120,scr1)
            ENDIF
         ENDDO
!*****
!  END OF ENTRY DATA POSSIBLE FOR THIS ELEMENT TYPE
!******
!
!  SKIP ANY UNUSED DATA IN ESTX (SCR1) DATA RECORD FOR THIS ELEMENT TYPE
!
 40      file = scr1
         loc = 900
         CALL fwdrec(*120,scr1)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!  SKIP ANY UNUSED DATA IN OES1 DATA RECORD FOR THIS ELEMENT TYPE.
!
 60      file = oes1
         loc = 950
         CALL fwdrec(*120,oes1)
         spag_nextblock_1 = 6
      CASE (6)
!
!  IF ANY ENTRIES WERE FOUND AND COMPLETED AND PLACED IN CORE
!  THEY ARE SORTED ON -MCSID- AND OUTPUT. AS THEY ARE OUTPUT
!  THE INVARIANTS ARE COMPUTED.
!
         IF ( noes1m>=ioes1m ) THEN
!
!  YES THERE ARE SOME ENTRIES
!
            loes1m = noes1m - ioes1m + 1
            CALL sort(0,0,incr,2,iz(ioes1m),loes1m)
!
!  OUTPUT ID-RECORD, REDEFINE MAJOR-ID FOR OFP MODULE
!
!  RE-DEFINITION MISSING FOR NOW.
!
            idrec(3) = idrec(3) + 1000
            CALL write(oes1m,idrec(1),146,eor)
            mcb(1) = oes1m
            CALL wrttrl(mcb(1))
            any1m = .TRUE.
!
!  MOVE AXIS CODE AND COMPLETE INVARIANTS OF EACH ENTRY.
!
            kmcsid = iz(ioes1m+1)
            kount = 0
!
            DO i = ioes1m , noes1m , incr
               buf(1) = iz(i)
               buf(2) = iz(i+1)
               rbuf(3) = z(i+3)
               IF ( icmplx==1 ) THEN
                  rbuf(4) = z(i+6)
                  rbuf(5) = z(i+4)
                  rbuf(6) = z(i+7)
                  rbuf(7) = z(i+5)
                  rbuf(8) = z(i+8)
                  buf(9) = iz(i+2)
                  rbuf(10) = z(i+9)
                  rbuf(11) = z(i+12)
                  rbuf(12) = z(i+10)
                  rbuf(13) = z(i+13)
                  rbuf(14) = z(i+11)
                  rbuf(15) = z(i+14)
                  CALL write(oes1m,buf(1),15,noeor)
               ELSE
                  rbuf(4) = z(i+4)
                  rbuf(5) = z(i+5)
                  CALL curvps(rbuf(3),rbuf(6))
                  IF ( strain ) THEN
                     rbuf(5) = 2.0*rbuf(5)
                     rbuf(9) = 2.0*rbuf(9)
                  ENDIF
                  buf(10) = iz(i+2)
                  rbuf(11) = z(i+6)
                  rbuf(12) = z(i+7)
                  rbuf(13) = z(i+8)
                  CALL curvps(rbuf(11),rbuf(14))
                  IF ( strain ) THEN
                     rbuf(13) = 2.0*rbuf(13)
                     rbuf(17) = 2.0*rbuf(17)
                  ENDIF
                  CALL write(oes1m,buf(1),17,noeor)
               ENDIF
!
!  KEEP COUNT OF ELEMENTS IN EACH MCSID GROUP
!
               IF ( iz(i+1)==kmcsid ) THEN
                  kount = kount + 1
                  IF ( i+incr-1<noes1m ) CYCLE
               ENDIF
!
!  CHANGE IN -MCSID- OF OUTPUT ENTRIES OR LAST ENTRY.
!  ADD COUNT OF ELEMENTS OF CURRENT TYPE TO TOTAL COUNT
!  OF ELEMENTS OF THIS -MCSID-.
!
               loc = 965
               CALL bisloc(*80,kmcsid,iz(imcsid),2,mcsids,jp)
               iz(imcsid+jp) = iz(imcsid+jp) + kount
               kount = 1
               kmcsid = iz(i+1)
            ENDDO
            CALL write(oes1m,0,0,eor)
         ENDIF
         spag_nextblock_1 = 2
      CASE (7)
!*****
!  ALL PROCESSING OF ONE SUBCASE COMPLETE FOR OES1M.
!*****
         CALL close(oes1m,cls)
         RETURN
!*****
!  ERROR CONDITION ENCOUNTERED
!*****
 80      imsg = -logerr
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     imsg = -1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 120     imsg = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     imsg = -3
         spag_nextblock_1 = 7
      CASE (8)
         imsg = -8
         lcore = icrq
         spag_nextblock_1 = 7
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv2
