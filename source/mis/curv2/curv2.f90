!*==curv2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv2
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
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
         Isig1 = 3
         Isig2 = 11
         File = Oes1m
         Loc = 60
         CALL open(*100,Oes1m,Iz(Ibuf3),Wrt)
!
!  OPEN OES1 NOREWIND TO CONTINUE
!
         First = .TRUE.
         Any = .FALSE.
         File = Oes1
         CALL open(*100,Oes1,Iz(Ibuf1),Rd)
         File = Scr1
         CALL open(*100,Scr1,Iz(Ibuf2),Rdrew)
!
!  ZERO ELEMENT COUNTS FOR EACH -MCSID- THIS SUBCASE MAY REFERENCE.
!
         DO I = Imcsid , Nmcsid , 2
            Iz(I+1) = 0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!  READ NEXT ID-RECORD
!
         File = Oes1
         Loc = 100
         CALL read(*20,*140,Oes1,Idrec(1),146,eor,Nwords)
!
!  CHECK IF STILL SAME SUBCASE UNLESS THIS IS THE FIRST ID-RECORD OF A
!  SUBCASE GROUP.
!
         IF ( .NOT.First ) THEN
!
!  CHECKING FOR CHANGE IN SUBCASE
!
            IF ( Subcas==Idrec(4) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!  CLOSE ESTX(SCR1) AND ESTXX(SCR2).
!
         CALL close(Scr1,Clsrew)
         CALL close(Scr2,Clsrew)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!  END OF FILE ON OES1. SET EOF FLAG AND WRAP UP CURRENT OPERATIONS
!  ON OES1M.
!
 20      Eofos1 = .TRUE.
         CALL close(Oes1,Clsrew)
         CALL close(Oes1m,Clsrew)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!  ID RECORD ON OES1 WILL BE FOR SOME KIND OF ELEMENT.
!  CHECK TO SEE IF ITS TYPE IS IN THE LIST OF TYPES NOW ON SCR1
!  WHICH IS THE ABBREVIATED EST. IF NOT THEN SKIP THE DATA RECORD
!  AND GO TO NEXT ID RECORD.
!
         Eltype = Idrec(3)
         iformt = Idrec(9)
         Owords = Idrec(10)
         DO I = Ieltyp , Neltyp
            IF ( Eltype==Iz(I) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL fwdrec(*20,Oes1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!  POSITION TO SCR1 RECORD FOR THIS ELEMENT TYPE. IF IT CAN NOT BE
!  FOUND BY FORWARD SEARCH THERE IS A LOGIC ERROR, OR OES1 ELEMENT
!  TYPES ARE NOT IN SAME ORDER AS EST ELEMENT TYPES.
!
         File = Scr1
         Loc = 600
         CALL rewind(Scr1)
         DO
            CALL read(*120,*140,Scr1,Buf(1),3,noeor,Nwords)
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
               IF ( Noes1m>Jcore ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Idscr1 = 0
               DO
!
!  READ NEXT OES1 ENTRY AND SET IDOES1  (STRIPPING OFF DEVICE CODE)
!
                  File = Oes1
                  Loc = 670
                  CALL read(*120,*40,Oes1,Buf(1),Owords,noeor,Nwords)
                  Idoes1 = Buf(1)/10
                  IF ( Idoes1<=0 ) GOTO 80
                  SPAG_Loop_3_1: DO
!
!  CHECK FOR MATCH OF ESTX(SCR1) ENTRY ID WITH OES1 ENTRY ID.
!
                     IF ( Idoes1<Idscr1 ) EXIT SPAG_Loop_3_1
                     IF ( Idoes1==Idscr1 ) THEN
!
!  MATCH FOUND THUS BEGIN OES1M ENTRY CALCULATIONS
!
                        Mcsid = Iz(Iestx+1)
                        Loc = 710
                        CALL tranem(Mcsid,Npts,z(Iestx+Npts+2),Icomp,u(1),Vec(1))
!
!  FORM AND ADD ENTRY TO CORE. INVARIANTS WILL BE COMPUTED LATER.
!
                        incr = 9
                        IF ( Icmplx==1 ) incr = 15
                        icrq = Noes1m + incr - Jcore
                        IF ( Noes1m+incr>Jcore ) THEN
                           spag_nextblock_1 = 8
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Iz(Noes1m+1) = Buf(1)
                        Iz(Noes1m+2) = Mcsid
                        Iz(Noes1m+3) = Icomp
                        IF ( Icmplx==1 ) THEN
!
                           IF ( iformt==3 ) THEN
                              DO mm1 = 3 , 10 , 7
                                 mm2 = mm1 + 4
                                 DO lll = mm1 , mm2 , 2
                                    ztemp = rbuf(lll)*cos(rbuf(lll+1)*Degrad)
                                    rbuf(lll+1) = rbuf(lll)*sin(rbuf(lll+1)*Degrad)
                                    rbuf(lll) = ztemp
                                 ENDDO
                              ENDDO
                           ENDIF
                           Zdum(1) = rbuf(3)
                           Zdum(2) = rbuf(5)
                           Zdum(3) = rbuf(7)
                           CALL gmmats(u(1),3,3,0,Zdum,3,1,0,z(Noes1m+4))
                           Zdum(1) = rbuf(4)
                           Zdum(2) = rbuf(6)
                           Zdum(3) = rbuf(8)
                           CALL gmmats(u(1),3,3,0,Zdum,3,1,0,z(Noes1m+7))
                           Zdum(1) = rbuf(10)
                           Zdum(2) = rbuf(12)
                           Zdum(3) = rbuf(14)
                           CALL gmmats(u(1),3,3,0,Zdum,3,1,0,z(Noes1m+10))
                           Zdum(1) = rbuf(11)
                           Zdum(2) = rbuf(13)
                           Zdum(3) = rbuf(15)
                           CALL gmmats(u(1),3,3,0,Zdum,3,1,0,z(Noes1m+13))
!
                           IF ( iformt==3 ) THEN
                              DO mm1 = 4 , 10 , 6
                                 mm2 = mm1 + 2
                                 DO lll = mm1 , mm2
                                    ll1 = Noes1m + lll
                                    ll2 = ll1 + 3
                                    ztemp = sqrt(z(ll1)**2+z(ll2)**2)
                                    IF ( ztemp/=0.0 ) THEN
                                       z(ll2) = atan2(z(ll2),z(ll1))*Raddeg
                                       IF ( z(ll2)<-0.00005E0 ) z(ll2) = z(ll2) + 360.0
                                    ELSE
                                       z(ll2) = 0.0
                                    ENDIF
                                    z(ll1) = ztemp
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
                              rbuf(Isig1+2) = rbuf(Isig1+2)/2.0
                              rbuf(Isig2+2) = rbuf(Isig2+2)/2.0
                           ENDIF
                           CALL gmmats(u(1),3,3,0,rbuf(Isig1),3,1,0,z(Noes1m+4))
                           CALL gmmats(u(1),3,3,0,rbuf(Isig2),3,1,0,z(Noes1m+7))
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
                              CALL open(*100,Scr3,Iz(Ibuf4),Wrtrew)
                              CALL write(Scr3,Idrec(1),146,eor)
                              CALL close(Scr3,Clsrew)
!
                              File = Scr2
                              CALL open(*100,Scr2,Iz(Ibuf4),Wrtrew)
                              Device = mod(Buf(1),10)
                              Any = .TRUE.
                           ENDIF
!
!  OUTPUT SPECIAL ESTXX (SCR2) ENTRY FOR USE BY CURV3.
!
                           CALL write(Scr2,Mcsid,1,noeor)
                           CALL write(Scr2,z(Noes1m-5),6,noeor)
                           CALL write(Scr2,Vec(1),3,noeor)
                           CALL write(Scr2,Npts,1,noeor)
                           CALL write(Scr2,Iz(Iestx+2),Npts4,noeor)
                        ENDIF
                        EXIT SPAG_Loop_3_1
                     ELSE
!
!  READ NEXT SCR1 ENTRY AND SET IDSCR1
!
                        File = Scr1
                        Loc = 680
                        CALL read(*120,*60,Scr1,Iz(Iestx),Ewords,noeor,Nwords)
                        Idscr1 = Iz(Iestx)
                        IF ( Idscr1<=0 ) GOTO 80
                     ENDIF
                  ENDDO SPAG_Loop_3_1
               ENDDO
            ELSE
               CALL fwdrec(*120,Scr1)
            ENDIF
         ENDDO
!*****
!  END OF ENTRY DATA POSSIBLE FOR THIS ELEMENT TYPE
!******
!
!  SKIP ANY UNUSED DATA IN ESTX (SCR1) DATA RECORD FOR THIS ELEMENT TYPE
!
 40      File = Scr1
         Loc = 900
         CALL fwdrec(*120,Scr1)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!  SKIP ANY UNUSED DATA IN OES1 DATA RECORD FOR THIS ELEMENT TYPE.
!
 60      File = Oes1
         Loc = 950
         CALL fwdrec(*120,Oes1)
         spag_nextblock_1 = 6
      CASE (6)
!
!  IF ANY ENTRIES WERE FOUND AND COMPLETED AND PLACED IN CORE
!  THEY ARE SORTED ON -MCSID- AND OUTPUT. AS THEY ARE OUTPUT
!  THE INVARIANTS ARE COMPUTED.
!
         IF ( Noes1m>=Ioes1m ) THEN
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
            CALL write(Oes1m,Idrec(1),146,eor)
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
               rbuf(3) = z(I+3)
               IF ( Icmplx==1 ) THEN
                  rbuf(4) = z(I+6)
                  rbuf(5) = z(I+4)
                  rbuf(6) = z(I+7)
                  rbuf(7) = z(I+5)
                  rbuf(8) = z(I+8)
                  Buf(9) = Iz(I+2)
                  rbuf(10) = z(I+9)
                  rbuf(11) = z(I+12)
                  rbuf(12) = z(I+10)
                  rbuf(13) = z(I+13)
                  rbuf(14) = z(I+11)
                  rbuf(15) = z(I+14)
                  CALL write(Oes1m,Buf(1),15,noeor)
               ELSE
                  rbuf(4) = z(I+4)
                  rbuf(5) = z(I+5)
                  CALL curvps(rbuf(3),rbuf(6))
                  IF ( Strain ) THEN
                     rbuf(5) = 2.0*rbuf(5)
                     rbuf(9) = 2.0*rbuf(9)
                  ENDIF
                  Buf(10) = Iz(I+2)
                  rbuf(11) = z(I+6)
                  rbuf(12) = z(I+7)
                  rbuf(13) = z(I+8)
                  CALL curvps(rbuf(11),rbuf(14))
                  IF ( Strain ) THEN
                     rbuf(13) = 2.0*rbuf(13)
                     rbuf(17) = 2.0*rbuf(17)
                  ENDIF
                  CALL write(Oes1m,Buf(1),17,noeor)
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
               CALL bisloc(*80,Kmcsid,Iz(Imcsid),2,Mcsids,Jp)
               Iz(Imcsid+Jp) = Iz(Imcsid+Jp) + Kount
               Kount = 1
               Kmcsid = Iz(I+1)
            ENDDO
            CALL write(Oes1m,0,0,eor)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!*****
!  ALL PROCESSING OF ONE SUBCASE COMPLETE FOR OES1M.
!*****
         CALL close(Oes1m,Cls)
         RETURN
!*****
!  ERROR CONDITION ENCOUNTERED
!*****
 80      Imsg = -Logerr
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     Imsg = -1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 120     Imsg = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     Imsg = -3
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         Imsg = -8
         Lcore = icrq
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv2
