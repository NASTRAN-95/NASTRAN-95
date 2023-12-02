!*==sdr2d.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2d
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_SDR2C1
   USE C_SDR2DE
   USE C_SDR2X1
   USE C_SDR2X2
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: axcosi , axsine
   INTEGER , DIMENSION(50) , SAVE :: buf
   INTEGER :: buf0 , bufm1 , idx , ieqex , iflag , ilayer , index , iskip , isvsrc , isvvcn , isvvec , ixstnf , ixstns , limit ,    &
            & ncc , neqex
   REAL , DIMENSION(2) :: bufr
   REAL :: coef1
   INTEGER , DIMENSION(7) :: itr
   INTEGER , DIMENSION(2) , SAVE :: kdefrm , nmef1l , nmes1l , pcomp , pcomp1 , pcomp2
   INTEGER , SAVE :: xset0
   REAL , DIMENSION(1) :: zz
   EXTERNAL bckrec , close , fname , fread , fwdrec , intpk , korsz , locate , mesage , open , preloc , rdtrl , read , rewind ,     &
          & sdr2e , skprec , write , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SDR2D PERFORMS THE FINAL STRESS AND FORCE RECOVERY COMPUTATIONS.
!     CASE CONTROL AND THE DISPLACEMENT VECTOR FILE ARE PROCESSED IN
!     PARALLEL.  THE ESTA IS PASSED ONCE FOR EACH VECTOR IN UGV FOR
!     WHICH A STRESS OR FORCE OUTPUT REQUEST EXISTS.  THE ESTA IS HELD
!     COMPLETELY IN CORE IF POSSIBLE.  STRESS OUTPUT IS WRITTEN ON OES1.
!     FORCE OUTPUT IS WRITTEN ON OEF1.
!
!    1,               IDSTRS   ,IDFORC   ,ILOGIC(2)
   !>>>>EQUIVALENCE (buf(1),bufr(1)) , (Z(1),Zz(1))
!    1,               (IDSTRS,ILOGIC(1)) ,(IDFORC,ILOGIC(2))
   DATA buf/50*0/ , kdefrm/104 , 1/ , xset0/100000000/
   DATA nmes1l/4HOES1 , 4HL   / , nmef1l/4HOEF1 , 4HL   /
   DATA pcomp/5502 , 55/ , pcomp1/5602 , 56/ , pcomp2/5702 , 57/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION
!
         bufm1 = korsz(Z) - Sysbuf + 1
         buf0 = bufm1 - Sysbuf - 1
         Buf1 = buf0 - Sysbuf - 1
         IF ( Comps/=-1 ) Buf1 = bufm1
         Buf2 = Buf1 - Sysbuf - 1
         I2 = 1
         Incr2 = 1
         Icc = 0
         Ilist = 1
         Nlist = 0
         Jlist = 1
         Kfrq = 0
         axsine = .FALSE.
         axcosi = .FALSE.
         Sorc = 0
!
!     READ TRAILER ON INPUT FILE. SET PARAMETERS.
!
         Icb(1) = Ugv
         CALL rdtrl(Icb)
         IF ( Icb(1)/=Ugv ) THEN
!
!     UGV FILE PURGED, CAN NOT PROCESS STRESSES OR FORCES
!
            CALL mesage(30,76,0)
            GOTO 380
         ELSE
            Nvects = Icb(2)
            IF ( Icb(5)>2 ) THEN
!
!     COMPLEX VECTOR.
!
               Ktype = 2
               Qtype2 = 3
               Ktype1 = 3
               Nwds = 14
               Ktypex = 1000
            ELSE
!
!     REAL VECTOR.
!
               Ktype = 1
               Qtype2 = 1
               Ktype1 = 2
               Nwds = 8
               Ktypex = 0
            ENDIF
!
!     OPEN CASE CONTROL AND SKIP HEADER. THEN BRANCH ON APPROACH.
!
            File = Casecc
            CALL open(*400,Casecc,Z(Buf1),Rdrew)
            CALL fwdrec(*420,Casecc)
            Eofcc = .FALSE.
!
            IF ( Branch==1 .OR. Branch==3 .OR. Branch==7 .OR. Branch==10 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Branch==4 .OR. Branch==8 ) THEN
!
!     DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1 - SKIP 1ST DATA RECORD ON
!     CC.
!
               CALL fwdrec(*420,Casecc)
               IF ( App(1)/=Bk1(1) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Branch==5 .OR. Branch==6 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
               File = Pg
               CALL open(*400,File,Z(Buf2),Rdrew)
               I = Ilist
               M = 3
               Ix = 1
               IF ( App(1)==Frq(1) .OR. App(1)==Trn(1) ) Ix = 2
               DO
                  CALL read(*420,*60,File,buf(1),M,0,Flag)
                  Z(I) = buf(M)
                  Z(I+1) = 0
                  I = I + Ix
                  M = 1
               ENDDO
            ENDIF
!            STA,REI,DS0,DS1,FRQ,TRN,BK0,BK1,CEI,PLA
!
!     EIGENVALUES - READ LIST OF MODE NOS. AND EIGENVALUES INTO CORE.
!     BUCKLING POSSIBLE HERE TOO
!
            File = Eigr
            CALL open(*400,Eigr,Z(Buf2),Rdrew)
            CALL fwdrec(*420,Eigr)
            CALL fwdrec(*420,Eigr)
            I = Ilist
            M = 8 - Ktype
            iskip = 0
            index = 2
            IF ( App(1)/=Rei(1) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            SPAG_Loop_1_1: DO
!
!     CHECK TO SEE IF ALL GENERALIZED MASS VALUES ARE ZERO
!
               CALL read(*420,*20,Eigr,buf,M,0,Flag)
               IF ( buf(6)/=0.0 ) THEN
                  index = 0
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
 20      CALL skprec(Eigr,-1)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_2: DO
            CALL read(*420,*40,Eigr,buf(1),M,0,Flag)
            IF ( App(1)/=Rei(1) ) EXIT SPAG_Loop_1_2
            IF ( index==2 ) EXIT SPAG_Loop_1_2
!
!     MATCH CORRECT MODE NOS. AND EIGENVALUES WITH PROPER
!     FORCES AND STRESSES WHEN USING GIVENS METHOD WITH F1.GT.0.0
!
            IF ( index==1 ) EXIT SPAG_Loop_1_2
            IF ( buf(6)/=0.0 ) THEN
               index = 1
               EXIT SPAG_Loop_1_2
            ELSE
               iskip = iskip + 1
            ENDIF
         ENDDO SPAG_Loop_1_2
         Z(I) = buf(1) - iskip
         Z(I+1) = buf(3)
         Z(I+2) = buf(4)
         I = I + Ktype1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(Eigr,Clsrew)
         Nlist = I - Ktype1
         Icc = I
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(File,Clsrew)
         Nlist = I - Ix
         Icc = I
         spag_nextblock_1 = 3
      CASE (3)
!
!     ALLOCATE CORE FOR CASE CONTROL, EDT, GPTT, ESTA, VECTOR
!     BALANCE OF REQUIRED BUFFERS
!       BUF1 = CASECC     BUF5 = GPTT
!       BUF2 = VECTOR     BUF6 = EDT
!       BUF3 = OES1       BUF7 = EQEXIN
!       BUF4 = OEF1       BUF8 = ESTA
!     SOME OF THE ABOVE MAY NOT BE REQUIRED AND THUS WILL NOT BE
!     ALLOCATED..
!
         Buf3 = Buf2 - Sysbuf - 1
         IF ( Stress==0 ) Buf3 = Buf2
         Buf4 = Buf3 - Sysbuf - 1
         IF ( Force==0 ) Buf4 = Buf3
         Buf5 = Buf4 - Sysbuf - 1
         IF ( Tloads==0 ) Buf5 = Buf4
         Buf6 = Buf5 - Sysbuf - 3
         IF ( Kwdedt==0 ) Buf6 = Buf5
         Buf7 = Buf6 - Sysbuf - 1
         IF ( Isopl==0 ) Buf7 = Buf6
         Buf8 = Buf7 - Sysbuf - 1
!
!     IF COMPOSITE ELEMENTS ARE PRESENT, READ PCOMPS INTO CORE
!
         IF ( Comps/=-1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         File = Pcomps
         N = -1
         CALL preloc(*460,Z(Buf2),Pcomps)
         Ipcmp = Icc + 1
         Ipcmp1 = Ipcmp
         Ipcmp2 = Ipcmp
         Npcmp = 0
         Npcmp1 = 0
         Npcmp2 = 0
         N = -2
!
         CALL locate(*80,Z(Buf2),pcomp,idx)
         CALL read(*460,*80,Pcomps,Z(Ipcmp),Buf2-Ipcmp,1,Npcmp)
         CALL mesage(-8,0,Nam)
 80      Ipcmp1 = Ipcmp1 + Npcmp
         Ipcmp2 = Ipcmp1
!
         CALL locate(*100,Z(Buf2),pcomp1,idx)
         CALL read(*460,*100,Pcomps,Z(Ipcmp1),Buf2-Ipcmp1,1,Npcmp1)
         CALL mesage(-8,0,Nam)
 100     Ipcmp2 = Ipcmp2 + Npcmp1
!
         CALL locate(*120,Z(Buf2),pcomp2,idx)
         CALL read(*460,*120,Pcomps,Z(Ipcmp2),Buf2-Ipcmp2,1,Npcmp2)
         CALL mesage(-8,0,Nam)
 120     Icc = Ipcmp2 + Npcmp2 - 1
!
         CALL close(Pcomps,Clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!     IF ESTA FITS IN CORE BUF8 MAY BE BUF7 SINCE IT WILL ONLY BE USED
!     TO READ ESTA IN ONCE..
!
         Iedt = Icc + Kwdcc + 1
         Igptta = Iedt + Kwdedt
         itr(1) = Eqexin
         CALL rdtrl(itr)
         neqex = 2*itr(2)
         IF ( Isopl8/=8 ) neqex = 0
         ieqex = Igptta + Kwdgpt
         Ivec = ieqex + neqex
         Ivecn = Ivec + Ktype*Icb(3) - 1
!
!     IF CONICAL SHELL DOUBLE VECTOR SPACE
!
         IF ( Axic .AND. Ktype==1 ) Ivecn = Ivecn + Icb(3)*Ktype
         Iesta = Ivecn + 1
         Midvec = (Ivec+Ivecn)/2 + 1
         IF ( Axic .AND. Ktype==1 ) Midvec = 0
         IF ( Axic .AND. Ktype==1 ) Ivecn = Ivecn - Icb(3)*Ktype
         IF ( Kwdest<=(Buf7-Iesta) ) Buf8 = Buf7
!
!     OPEN ESTA
!
         File = Esta
         CALL open(*400,Esta,Z(Buf8),Rdrew)
!
!     REMAINING CORE
!
         Icore = Buf8 - Iesta
         Nesta = 0
!
!     WILL ESTA FIT IN CORE
!
         IF ( Icore<=0 ) CALL mesage(-8,0,Nam)
         IF ( Kwdest>Icore ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ESTA WILL FIT. READ IT IN PLACING A ZERO WORD AT END OF EACH
!     RECORD.
!
         I = Iesta
         spag_nextblock_1 = 5
      CASE (5)
         CALL read(*160,*140,Esta,Z(I),Icore,1,Nwords)
         CALL rewind(Esta)
         Icore = Buf8 - Iesta
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 140     I = I + Nwords + 1
         Z(I-1) = 0
         Icore = Icore - Nwords - 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     ALL ESTA NOW IN CORE
!
 160     Nesta = I - 1
         CALL close(Esta,Clsrew)
         IF ( Nesta<=Iesta ) THEN
            WRITE (Opte,99001) Uwm
99001       FORMAT (A25,' 3303, STRESSES OR FORCES REQUESTED FOR SET(S) ','WHICH CONTAIN NO VALID ELEMENTS.')
            GOTO 380
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
         File = Ugv
         CALL open(*400,Ugv,Z(Buf2),Rdrew)
         CALL fwdrec(*420,Ugv)
!
!     IF ANY ISOPARAMETRIC ELEMENTS PRESENT, GET SECOND RECORD OF EQEXIN
!
         IF ( Isopl/=0 ) THEN
            File = Eqexin
            CALL open(*400,Eqexin,Z(Buf7),Rdrew)
            CALL fwdrec(*420,Eqexin)
            CALL fwdrec(*420,Eqexin)
            Isopl = Eqexin
            IF ( Isopl8==8 ) THEN
               CALL fread(Eqexin,Z(ieqex),neqex,0)
               CALL bckrec(Eqexin)
            ENDIF
         ENDIF
!
!     IF ANY STRESS OUTPUT IS REQUESTED,
!     OPEN OES1 AND WRITE HEADER RECORD
!
         IF ( Stress/=0 ) THEN
            File = Oes1
            CALL open(*180,Oes1,Z(Buf3),Wrtrew)
            CALL fname(Oes1,Ocb)
            DO I = 1 , 3
               Ocb(I+2) = Date(I)
            ENDDO
            Ocb(6) = Time
            Ocb(7) = 1
            CALL write(Oes1,Ocb,7,1)
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 180     CALL mesage(1,Oes1,Nam)
         Stress = 0
         spag_nextblock_1 = 7
      CASE (7)
!
!     IF ANY STRESS OR FORCE OUTPUT IS REQUESTED AND COMPOSITE ELEMENTS
!     ARE PRESENT, OPEN OES1L AND OEF1L AND WRITE HEADER RECORDS
!
         IF ( .NOT.(Comps/=-1 .OR. (Stress==0 .AND. Force==0)) ) THEN
            ilayer = 0
            File = Oes1l
            CALL open(*200,Oes1l,Z(bufm1),Wrtrew)
            CALL write(Oes1l,nmes1l,2,1)
            File = Oef1l
            CALL open(*200,Oef1l,Z(buf0),Wrtrew)
            CALL write(Oef1l,nmef1l,2,1)
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 200     CALL mesage(1,File,Nam)
         Stress = 0
         Force = 0
         spag_nextblock_1 = 8
      CASE (8)
!
!     IF ANY FORCE OUTPUT IS REQUESTED,
!     OPEN OEF1 AND WRITE HEADER RECORD
!
         IF ( Force/=0 ) THEN
            File = Oef1
            CALL open(*220,Oef1,Z(Buf4),Wrtrew)
            CALL fname(Oef1,Ocb)
            DO I = 1 , 3
               Ocb(I+2) = Date(I)
            ENDDO
            Ocb(6) = Time
            Ocb(7) = 1
            CALL write(Oef1,Ocb,7,1)
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 220     CALL mesage(1,Oef1,Nam)
         Force = 0
         spag_nextblock_1 = 9
      CASE (9)
         IF ( Stress==0 .AND. Force==0 ) GOTO 380
!
!     INITIALIZE UGV VEC, WHICH WILL BE THE NUMBER OF THE VECTOR WE
!     ARE NOW POSITIONED TO READ.
!
         Ugvvec = 1
         isvvec = Ivec
         isvvcn = Ivecn
         iflag = 0
         spag_nextblock_1 = 10
      CASE (10)
!
!     READ A RECORD IN CASE CONTROL. SET SYMMETRY FLAG.
!
         CALL read(*360,*240,Casecc,Z(Icc+1),Kwdcc+1,1,Flag)
         CALL mesage(8,0,Nam)
         GOTO 380
 240     Ix = Icc + Isymfl
         Symflg = Z(Ix)
         ncc = Icc + Flag
!
!     FOR CONICAL SHELL SET SORC FLAG
!
         Ix = Icc + Isorc
         IF ( iflag==1 ) Sorc = isvsrc
         IF ( Symflg==0 ) Sorc = Z(Ix)
         IF ( Sorc==1 ) axsine = .TRUE.
         IF ( Sorc==2 ) axcosi = .TRUE.
         IF ( Axic .AND. Symflg==0 ) isvsrc = Sorc
         Ivec = isvvec
         Ivecn = isvvcn
         iflag = 0
         IF ( Axic .AND. axsine .AND. axcosi .AND. Ugvvec==3 ) iflag = 1
         IF ( Axic .AND. Sorc==0 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT.
!     IF NOT, TEST FOR RECORD SKIP ON UGV THEN GO TO END OF THIS
!     REQUEST. IF SO, SET POINTERS TO SET DEFINING REQUEST.
!
         Ix = Icc + Istr
         Stresx = Z(Ix)
         Sdest = Z(Ix+1)
         Xsetns = -1
         Ix = Icc + Ielf
         Forcex = Z(Ix)
         Fdest = Z(Ix+1)
         Xsetnf = -1
         Nstrop = Z(Icc+183)
!
!     DEBUG PRINTOUT
!
!
         IF ( Comps==-1 .AND. Nstrop>1 ) ilayer = ilayer + 1
         IF ( Stresx>0 ) THEN
            Ix = Icc + Ilsym
            Isetno = Ix + Z(Ix) + 1
            SPAG_Loop_1_3: DO
               Isets = Isetno + 2
               Nsets = Z(Isetno+1) + Isets - 1
               IF ( Z(Isetno)==Stresx ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR STRESSES
!
                  IF ( Stresx<xset0 ) EXIT SPAG_Loop_1_3
                  Xsetns = Sdest/10
                  Sdest = Sdest - 10*Xsetns
                  IF ( Xsetns==0 ) EXIT SPAG_Loop_1_3
                  ixstns = Ix + Z(Ix) + 1
                  DO
                     Ixsets = ixstns + 2
                     Nxsets = Z(ixstns+1) + Ixsets - 1
                     IF ( Z(ixstns)==Stresx ) EXIT SPAG_Loop_1_3
                     ixstns = Nxsets + 1
                     IF ( ixstns>=ncc ) THEN
                        Stresx = -1
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDDO
               ELSE
                  Isetno = Nsets + 1
                  IF ( Isetno>ncc ) THEN
                     Stresx = -1
                     EXIT SPAG_Loop_1_3
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
         IF ( Forcex>0 ) THEN
            Ix = Icc + Ilsym
            Isetno = Ix + Z(Ix) + 1
            SPAG_Loop_1_4: DO
               Isetf = Isetno + 2
               Nsetf = Z(Isetno+1) + Isetf - 1
               IF ( Z(Isetno)==Forcex ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR FORCES
!
                  IF ( Forcex<xset0 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Xsetnf = Fdest/10
                  Fdest = Fdest - 10*Xsetnf
                  IF ( Xsetnf==0 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ixstnf = Ix + Z(Ix) + 1
                  DO
                     Ixsetf = ixstnf + 2
                     Nxsetf = Z(ixstnf+1) + Ixsetf - 1
                     IF ( Z(ixstnf)==Forcex ) THEN
                        spag_nextblock_1 = 12
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     ixstnf = Nxsetf + 1
                     IF ( ixstnf>=ncc ) THEN
                        Forcex = -1
                        EXIT SPAG_Loop_1_4
                     ENDIF
                  ENDDO
               ELSE
                  Isetno = Nsetf + 1
                  IF ( Isetno>ncc ) THEN
                     Forcex = -1
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
         IF ( .NOT.(Stresx/=0 .OR. Forcex/=0 .OR. Axic) ) THEN
!
!     NO REQUESTS THIS CC RECORD FOR STRESSES OR FORCES.
!     THUS SKIP CORRESPONDING UGV RECORD UNLESS SYMFLG IS ON, IN WHICH
!     CASE WE SKIP NO UGV RECORD SINCE THE SYMMETRY CASE HAS NO UGV
!     VECTOR, BUT IN FACT WOULD HAVE USED A SUMMATION OF THE IMMEDIATELY
!     PRECEEDING LSYM VECTORS.
!
!     IF END OF CC AND NO STRESS OR FORCE OUTPUT REQUEST WE ARE DONE
!
            IF ( Eofcc ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Symflg/=0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fwdrec(*420,Ugv)
            Ugvvec = Ugvvec + 1
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     THERE IS A REQUEST FOR STRESSES AND OR FORCES
!     FIRST DETERMINE APPROPRIATE GPTT AND EDT RECORDS IF REQUIRED
!
         Ix = Icc + Itload
         Tloads = Z(Ix)
         Ngptt = 0
         IF ( Tloads/=0 ) THEN
            File = Gptt
            CALL close(Gptt,Clsrew)
            CALL open(*400,Gptt,Z(Buf5),Rdrew)
!
!     SKIP NAME
!
            CALL read(*420,*440,Gptt,buf,2,0,N)
            SPAG_Loop_1_5: DO
!
!     PICK UP 3 WORDS OF SET INFORMATION
!
               CALL read(*420,*440,Gptt,buf,3,0,N)
               IF ( buf(1)==Tloads ) THEN
                  Deftmp = bufr(2)
                  Tmprec = buf(3)
                  EXIT SPAG_Loop_1_5
               ENDIF
            ENDDO SPAG_Loop_1_5
         ENDIF
!
         Ix = Icc + Ieldef
         Eldef = Z(Ix)
         IF ( Eldef==0 .OR. Kwdedt==0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         File = Edt
         CALL preloc(*400,Z(Buf6),Edt)
         CALL locate(*260,Z(Buf6),kdefrm,Flag)
         Idef = Iedt
         I = Idef
         DO
            CALL read(*420,*260,Edt,buf(1),3,0,Flag)
            IF ( buf(1)==Eldef ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 260     buf(1) = Eldef
         buf(2) = 0
         CALL mesage(-30,46,buf)
         spag_nextblock_1 = 13
      CASE (13)
         CALL read(*420,*280,Edt,buf(1),3,0,Flag)
         IF ( buf(1)/=Eldef ) GOTO 280
         spag_nextblock_1 = 14
      CASE (14)
         Z(I) = buf(2)
         Z(I+1) = buf(3)
         I = I + 2
         IF ( I<Igptta ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL mesage(-8,0,Nam)
 280     Ndef = I - 2
         CALL close(Edt,Clsrew)
         spag_nextblock_1 = 15
      CASE (15)
!
!     UNPACK VECTOR INTO CORE
!
         coef1 = 1.0
         IF ( Symflg==0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SYMMETRY SEQUENCE-- BUILD VECTOR IN CORE.
!
         Ix = Icc + Ilsym
         Lsym = Z(Ix)
!
!     IF SYMFLG IS NEGATIVE, THIS IS A REPEAT SUBCASE.  USE PRESENT
!     VECTOR IN CORE.
!
         IF ( Symflg<0 .AND. App(1)==Sta(1) ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Symflg<0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO I = Ivec , Ivecn
            zz(I) = 0.0
         ENDDO
         IF ( Lsym>Ugvvec-1 ) THEN
            Ocb(1) = Lsym
            Ocb(2) = Ugvvec - 1
            CALL mesage(30,92,Ocb(1))
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            limit = Lsym
            IF ( iflag==1 ) limit = 1
            DO I = 1 , limit
               CALL bckrec(Ugv)
            ENDDO
            Isymn = Ix + Lsym
            I = Ix + 1
            IF ( iflag==1 ) I = I + 1
            J2 = Icb(3)
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
         Coef = zz(I)
         CALL intpk(*300,Ugv,0,Qtype2,0)
         SPAG_Loop_1_6: DO
            CALL zntpki
            Ix = Ivec + Ixx - 1
            IF ( Ktype==1 ) THEN
               zz(Ix) = zz(Ix) + Coef*Xx(1)
            ELSE
               zz(Ix+J2) = zz(Ix+J2) + Coef*Xx(1)
               zz(Ix) = zz(Ix) + Coef*Xx(2)
            ENDIF
            IF ( Eol/=0 ) EXIT SPAG_Loop_1_6
         ENDDO SPAG_Loop_1_6
 300     IF ( iflag==1 ) THEN
!
!     CONICAL SHELL BOTH CASE
!     2 VECTORS IN CORE -
!     2-ND VECTOR IS NOW IN CORE AT Z(IVEC) THRU Z(IVECN)...
!     GET 1-ST VECTOR AND PUT IT AT Z(IVECN+1) THRU Z(2*IVECN-MIDVEC+1)
!
!
            Midvec = Ivec
            Ivec = Ivecn + 1
            Ivecn = Ivecn + (Ivecn-Midvec+1)
            coef1 = zz(Icc+Ilsym+1)
!
!     IF FALL HERE AND SORC=1 THE VECTOR IN CORE IS THE SINE VECTOR AND
!     IF SORC=2 THE VECTOR IN CORE IS THE COSINE VECTOR.  THUS THE FIRST
!     VECTOR WAS THE OTHER VECTOR RESPECTIVELY
!     BY THE WAY THE VECTOR IN CORE IS THE SECOND VECTOR.
!
            CALL bckrec(Ugv)
            CALL bckrec(Ugv)
         ELSE
            I = I + 1
            IF ( I<=Isymn ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     NOT SYMMETRY-- UNPACK VECTOR.
!
         J2 = Icb(3)
         IF ( iflag/=1 ) THEN
            IF ( Ugvvec>Nvects ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         DO I = Ivec , Ivecn
            zz(I) = 0.0
         ENDDO
         CALL intpk(*320,Ugv,0,Qtype2,0)
         SPAG_Loop_1_7: DO
            CALL zntpki
            Ix = Ivec + Ixx - 1
            IF ( Ktype==1 ) THEN
               zz(Ix) = coef1*Xx(1)
            ELSE
               zz(Ix) = coef1*Xx(2)
               zz(Ix+J2) = coef1*Xx(1)
            ENDIF
            IF ( Eol/=0 ) EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
 320     IF ( App(1)==Trn(1) ) THEN
            CALL fwdrec(*340,Ugv)
            Ugvvec = Ugvvec + 1
            CALL fwdrec(*340,Ugv)
            Ugvvec = Ugvvec + 1
         ENDIF
 340     IF ( iflag/=1 ) Ugvvec = Ugvvec + 1
         IF ( iflag==1 ) CALL skprec(Ugv,1)
         spag_nextblock_1 = 18
      CASE (18)
!
!     READY NOW TO SWEEP THROUGH THE ESTA ONCE.
!     SDR2E DOES ALL THE PROCESSING OF PHASE II ELEMENT COMPUTATIONS.
!     THE ESTA FILE, BE IT IN CORE OR NOT, IS SWEPT THRU ONCE FOR THE
!     FOLLOWING CALL.
!
         IF ( iflag==1 ) Sorc = Sorc + 1
         IF ( Sorc==3 ) Sorc = 1
         CALL sdr2e(*380,ieqex,neqex)
         spag_nextblock_1 = 19
      CASE (19)
!
!     CONCLUDE PROCESSING OF THIS VECTOR
!     INITIALIZE FOR NEXT VECTOR
!     CANCEL THIS INITIALIZATION IN SOME CASES IF A REPEAT CASE.
!
         IF ( Branch==2 .OR. Branch==4 .OR. Branch==8 .OR. Branch==9 ) THEN
            Jlist = Jlist + Ktype1
            IF ( .NOT.Eofcc ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( Branch==3 .OR. Branch==7 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Branch==5 ) THEN
!
!     FREQUENCY RESPONSE, PICK UP NEXT VECTOR UNLESS ALL FREQUENCIES
!     COMPLETED
!
            Jlist = Jlist + 2
            IF ( Jlist<=Nlist .AND. Ugvvec<=Nvects ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Kfrq = 0
            Jlist = Ilist
            DO I = Ilist , Nlist , 2
               Z(I+1) = 0
            ENDDO
            IF ( Ugvvec<=Nvects ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Branch==6 ) THEN
!
!     TRANSIENT RESPONSE
!
            Jlist = Jlist + 2
            IF ( Jlist<=Nlist .AND. .NOT.Eofcc ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Jlist>Nlist .OR. Ugvvec>Nvects ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
!
         ELSEIF ( .NOT.Eofcc ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS ANY REMAINING VECTORS WITH LAST CC RECORD
!
         IF ( Ugvvec<=Nvects .AND. Symflg==0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
!
!     EOF HIT ON CASECC FILE
!     PROCESS ANY MORE VECTORS USING LAST CASECC RECORD
!
 360     Eofcc = .TRUE.
         IF ( Nvects>=Ugvvec ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
!
!     WRITE TRAILERS AND CLOSE ANY OPEN FILES
!
         Ocb(2) = 63
         IF ( Stress/=0 ) THEN
            Ocb(1) = Oes1
            CALL wrttrl(Ocb(1))
            IF ( Comps==-1 .AND. ilayer/=0 ) THEN
               Ocb(1) = Oes1l
               CALL wrttrl(Ocb(1))
            ENDIF
         ENDIF
         IF ( Force/=0 ) THEN
            Ocb(1) = Oef1
            CALL wrttrl(Ocb(1))
            IF ( Comps==-1 .AND. ilayer/=0 ) THEN
               Ocb(1) = Oef1l
               CALL wrttrl(Ocb(1))
            ENDIF
         ENDIF
 380     DO I = 1 , 12
            IF ( I==2 ) THEN
               File = Oef1
            ELSEIF ( I==3 ) THEN
               File = Ugv
            ELSEIF ( I==4 ) THEN
               File = Casecc
            ELSEIF ( I==5 ) THEN
               File = Edt
            ELSEIF ( I==6 ) THEN
               File = Gptt
            ELSEIF ( I==7 ) THEN
               File = Pg
            ELSEIF ( I==8 ) THEN
               File = Eigr
            ELSEIF ( I==9 ) THEN
               File = Esta
            ELSEIF ( I==10 ) THEN
               File = Eqexin
            ELSEIF ( I==11 ) THEN
               File = Oes1l
            ELSEIF ( I==12 ) THEN
               File = Oef1l
            ELSE
               File = Oes1
            ENDIF
            CALL close(File,Clsrew)
         ENDDO
         RETURN
!
 400     N = 1
         GOTO 460
 420     N = 2
         GOTO 460
 440     N = 3
 460     CALL mesage(N,File,Nam)
         GOTO 380
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdr2d
