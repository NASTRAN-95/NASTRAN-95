!*==ddrmm2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmm2() !HIDESTARS (*,*,*,*)
   IMPLICIT NONE
   USE C_CLSTRS
   USE C_CONDAS
   USE C_DDRMC1
   USE C_GPTA1
   USE C_MPYADX
   USE C_NAMES
   USE C_STDATA
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: anyxy
   INTEGER , DIMENSION(150) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , i , iapp , id , idata , idvice , ielem , ij , insuf , iomega , iout , ixy , &
            & ixytyp , j , jdata , jlist , jp , jwords , jxy , k , kdata , klist , kxy , l , lentry , lxy , major , minor , ncols , &
            & ndata , next , nomega , npt , numwds , nxy , nxy1 , scrt1 , scrt2 , scrt3 , scrt4 , scrt5 , scrt6 , scrt7 , typout
   INTEGER , DIMENSION(75) :: bufa , bufb
   INTEGER , DIMENSION(3) , SAVE :: dvamid
   INTEGER , DIMENSION(300) :: elwork
   INTEGER , SAVE :: eor , noeor
   INTEGER , DIMENSION(4) :: ia
   REAL , DIMENSION(1) :: ridrec
   INTEGER , DIMENSION(1) :: z
   EXTERNAL bisloc , bldpk , bldpkn , close , ddrmmp , ddrmms , fname , fwdrec , intpk , korsz , magpha , mpyad , numtyp , open ,   &
          & rdtrl , read , setfnd , write , wrttrl , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     PERFORMS SORT2 TYPE PROCESSING FOR MODULE DDRMM.
!
   !>>>>EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2)) , (Scrt3,Scrt(3)) , (Scrt4,Scrt(4)) , (Scrt5,Scrt(5)) , (Scrt6,Scrt(6)) ,          &
!>>>>    & (Scrt7,Scrt(7)) , (Buf1,Buff(1)) , (Buf2,Buff(2)) , (Buf3,Buff(3)) , (Buf4,Buff(4)) , (Buf5,Buff(5)) , (Buf6,Buff(6)) ,       &
!>>>>    & (A(1),Ia(1)) , (Z(1),Rz(1)) , (Buf(1),Rbuf(1),Bufa(1)) , (Bufb(1),Buf(76)) , (Idrec(1),Ridrec(1))
!
   DATA eor , noeor/1 , 0/ , dvamid/2001 , 2010 , 2011/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FORMATION OF DATA-MATRIX AND SUBSEQUENT MULTIPLICATION BY SAME OF
!     THE SOLUTION MATRIX (TRNNSPOSED), AND ULTIMATE OUTPUT OF TRANSIENT
!     OR FREQUENCY SOLUTIONS.
!
         Ipass = 1
         iomega = Nlist + 1
         nomega = iomega - 1
         minor = 0
         spag_nextblock_1 = 2
      CASE (2)
         Col1 = .TRUE.
         Frstid = .TRUE.
         Setid = Sets(1,Ipass)
         Device = Sets(2,Ipass)
         Form = Sets(3,Ipass)
         Istlst = Sets(4,Ipass)
         Lstlst = Sets(5,Ipass)
!
!     GET LIST OF XYPLOT REQUESTED IDS FOR CURRENT SUBCASE AND
!     OUTFIL TYPE.
!
         IF ( Jfile==2 ) THEN
!
!     SPCF
!
            ixytyp = 4
         ELSEIF ( Jfile==3 ) THEN
!
!     STRESS
!
            ixytyp = 6
         ELSEIF ( Jfile==4 ) THEN
!
!     FORCE
!
            ixytyp = 7
         ELSE
!
!     DISPLACEMENT, VELOCITY, ACCELERATION
!
            ixytyp = Ipass
         ENDIF
!
         ixy = nomega + 1
         CALL ddrmmp(*180,z(ixy),buf3-ixy,lxy,ixytyp,Subcas,z(buf3),anyxy)
         IF ( .NOT.anyxy .AND. Setid==0 ) GOTO 100
         nxy = ixy + lxy - 1
         Ierror = 23
         File = scrt4
         CALL open(*120,scrt4,z(buf3),Wrtrew)
         File = scrt5
         CALL open(*120,scrt5,z(buf2),Wrtrew)
         CALL fname(scrt5,Filnam)
         CALL write(scrt5,Filnam,2,eor)
!
!     LOGIC TO BUILD SORT-2 FORMAT DATA MATRIX.
!
!     EACH COLUMN WRITTEN HERE ENCOMPASSES ALL EIGENVALUES FOR
!     ONE COMPONENT OF ONE ID.  THE NUMBER OF COLUMNS THUS EQUALS
!     THE SUM OF ALL COMPONENTS OF ALL REQUESTED ID-S.
!
!     READ AN OFP-ID RECORD AND SET PARAMETERS.
!     (ON ENTRY TO THIS PROCESSOR ONE ID-RECORD IS AT HAND)
!
         File = Infile
         Ierror = 19
         Mcb(1) = scrt5
         Mcb(2) = 0
         Mcb(3) = Nlambs
         Mcb(4) = 2
         Mcb(5) = 1
         Mcb(6) = 0
         Mcb(7) = 0
         IF ( .NOT.(Ipass==1 .AND. Frstid) ) CALL read(*60,*60,Infile,Idrec,146,eor,Nwds)
         spag_nextblock_1 = 3
      CASE (3)
!
!     OFP-ID RECORD IS WRITTEN TO THE MAP FILE ONLY ON CHANGE OF
!     MINOR ID.
!
         major = mod(Idrec(2),1000)
         IF ( major/=Itype1 ) THEN
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
            WRITE (Outpt,99001) Swm , Infile
99001       FORMAT (A27,' 2339.  (DDRMM2-1) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',&
                   &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
            Ipass = 3
            GOTO 100
         ELSE
            idvice = Device
            id = Idrec(5)/10
            IF ( Setid<0 ) THEN
            ELSEIF ( Setid==0 ) THEN
               GOTO 20
            ELSE
               next = 1
               CALL setfnd(*20,z(Istlst),Lstlst,id,next)
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      IF ( anyxy ) THEN
            CALL bisloc(*40,id,z(ixy),1,lxy,jp)
            idvice = 0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ID IS NOT TO BE OUTPUT THUS SKIP UPCOMING OFP-DATA-RECORD.
!
 40      CALL fwdrec(*140,Infile)
         CALL read(*60,*60,Infile,Idrec,146,eor,Nwds)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     ID IS TO BE OUTPUT THUS CONTINUE.
!
         numwds = Nlambs*Idrec(10)
         idata = nxy + 1
         ndata = idata + numwds - 1
         IF ( ndata<buf3 ) THEN
            IF ( .NOT.Frstid ) THEN
               IF ( Idrec(3)==minor ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     CHANGE IN MINOR ID, I.E. NEW ELEMENT TYPE.  COMPLETE CURRENT
!     RECORD OF MAP AND OUTPUT ANOTHER ID-RECORD.
!
               CALL write(scrt4,0,0,eor)
            ELSE
!
!     VERY FIRST ID RECORD,  THUS SET MINOR ID.
!
               Frstid = .FALSE.
            ENDIF
            CALL write(scrt4,Idrec,146,eor)
            minor = Idrec(3)
         ELSE
!
!     INSUFFICIENT CORE
!
            insuf = ndata - buf3
            WRITE (Outpt,99002) Uwm , Infile , insuf
99002       FORMAT (A25,' 2337.  (DDRMM2-2)  DATA BLOCK',I5,' CAN NOT BE ','PROCESSED DUE TO',/5X,'A CORE INSUFFICIENCY OF APPROXI',&
                   &'MATELY',I11,' DECIMAL WORDS.')
            Ipass = 3
            GOTO 100
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     SAME TYPE OF DATA THUS CONTINUE ON.
!
         lentry = Idrec(10)
         I1 = Nwords + 1
         I2 = lentry
!
!     READ AND OUTPUT ONE FULL OFP-DATA RECORD.
!
         CALL read(*140,*160,Infile,z(idata),numwds,eor,Nwds)
         DO i = I1 , I2
!
!     START NEW COLUMN
!
            CALL bldpk(1,1,scrt5,0,0)
            Irow = 0
            jdata = idata + i - 1
            kdata = ndata - lentry + i
            DO j = jdata , kdata , lentry
               Irow = Irow + 1
               A(1) = Rz(j)
!
!     ELIMINATE INTEGERS
!
!     OLD LOGIC -
!     IF (MACH.NE.5 .AND. IABS(IA(1)).LT.100000000) A(1) = 0.0
!     IF (MACH.EQ.5 .AND. (IA(1).LE.127 .AND. IA(1).GE.1)) A(1) = 0.0
!     OLD LOGIC SHOULD INCLUDE ALPHA MACHINE (MACH=21)
!
!     NEW LOGIC, BY G.CHAN/UNISYS  8/91 -
               IF ( numtyp(ia(1))<=1 ) A(1) = 0.0
!
               CALL zblpki
            ENDDO
!
!     COMPLETE COLUMN
!
            CALL bldpkn(scrt5,0,Mcb)
         ENDDO
!
!     OUTPUT TO MAP THE ID PLUS ANY OTHER DATA NECESSARY.
!
         buf(1) = 10*id + idvice
         IF ( Nwords==2 ) buf(2) = z(idata+1)
         Nstxtr = 0
         IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
            Npos = Savdat(minor)/100
            Nstxtr = Savdat(minor) - Npos*100
            DO i = 1 , Nstxtr
               j = Savpos(Npos+i-1)
               buf(i+1) = z(idata+j-1)
            ENDDO
         ENDIF
         CALL write(scrt4,buf,Nwords+Nstxtr,noeor)
!
!     GO FOR NEXT ID.
!
         CALL read(*60,*60,Infile,Idrec,146,eor,Nwds)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON INFILE.  MAP AND DATA MATRIX NOW COMPLETE.
!
 60      CALL wrttrl(Mcb)
         CALL close(scrt5,Clsrew)
         CALL close(Infile,Clsrew)
         CALL write(scrt4,0,0,eor)
         CALL close(scrt4,Clsrew)
!
!     SOLUTION MATRIX MAY BE FOUND BASED ON SORT-2 INFILE.
!
!     SOLVE,
!                               T
!        (MODAL SOLUTION MATRIX)     X      (DATA MATRIX)
!          NLAMBS X NSOLUTIONS             NLAMBS X NCOMPS
!        =======================           ===============
!
!     RESULTANT MATRIX IS NSOLUTIONS BY NCOMPS IN SIZE.
!
!
!     MATRIX MULTIPLY SETUP AND CALL.
!
         Mcba(1) = Uvsol
         IF ( Trnsnt ) Mcba(1) = Scrt(Ipass)
         CALL rdtrl(Mcba)
         Mcbb(1) = scrt5
         CALL rdtrl(Mcbb)
         Mcbc(1) = 0
         Mcbd(1) = scrt6
         Mcbd(2) = 0
         Mcbd(3) = Nsols
         Mcbd(4) = 2
         Mcbd(5) = 1
         Mcbd(6) = 0
         Mcbd(7) = 0
         IF ( .NOT.Trnsnt ) Mcbd(5) = 3
         Itflag = 1
         nxy1 = nxy + 1
         IF ( mod(nxy1,2)==0 ) nxy1 = nxy1 + 1
         Lz = korsz(z(nxy1))
         Isinab = 1
         Isinc = 1
         Iprec = 1
         Iscrt = scrt7
         CALL mpyad(z(nxy1),z(nxy1),z(nxy1))
         Mcbd(1) = scrt6
         CALL wrttrl(Mcbd)
!
!     PRODUCT MATRIX IS NOW OUTPUT USING THE MAP ON SCRT4.
!     EACH COLUMN OF SCRT6 CONTAINS ALL THE TIME OR FREQUENCY STEP
!     VALUES FOR ONE COMPONENT OF ONE ID.
!
!     THUS A NUMBER OF COLUMNS ENCOMPASSING THE COMPONENTS OF ONE ID
!     MUST FIT IN CORE.
!
         Ierror = 20
         File = Outfil
         CALL open(*120,Outfil,z(buf1),Wrt)
         File = scrt4
         CALL open(*120,scrt4,z(buf2),Rdrew)
         File = scrt6
         CALL open(*120,scrt6,z(buf3),Rdrew)
         CALL fwdrec(*140,scrt6)
!
!     READ AN OFP-ID-RECORD FROM THE MAP, AND ALLOCATE SPACE NEEDED
!     FOR SOLUTION DATA.
!
         File = scrt4
 80      CALL read(*100,*160,scrt4,Idrec,146,eor,Nwds)
         minor = Idrec(3)
!
!
!     SET DISPLACEMENT, VELOCITY, OR ACCELERATION OFP MAJOR-ID IF
!     INFILE IS MODAL DISPLACEMETNS = EIGENVECTORS...
!
         IF ( Itype1==7 ) Idrec(2) = dvamid(Ipass)
         IF ( .NOT.Trnsnt ) Idrec(2) = Idrec(2) + 1000
!
!     RESET APPROACH CODE FROM EIGENVALUE TO TRANSIENT OR FREQUENCY
!
         iapp = 5
         IF ( Trnsnt ) iapp = 6
         Idrec(1) = 10*iapp + Device
         lentry = Idrec(10) - Nwords
         ncols = lentry
         IF ( .NOT.Trnsnt ) lentry = lentry + lentry
!
!     IF FREQUENCY RESPONSE PROBLEM AND THIS IS THE VELOCITY OR
!     ACCELERATION PASS THEN MOVE DOWN ANY XY LIST OF POINTS AND
!     ADD AN OMEGA TABLE.  SOMETIMES THE MOVEDOWN OF THE XY LIST IS
!     REDUNDANT.
!
!     XY LIST IS MOVED FROM BOTTOM UP INCASE XY LIST IS LONGER THAN
!     THE OMEGA LIST WILL BE.
!
         IF ( .NOT.(Trnsnt .OR. Ipass==1) ) THEN
            nomega = iomega + Nsols - 1
            IF ( lxy/=0 ) THEN
               jxy = nxy
               kxy = nomega + lxy
               DO i = 1 , lxy
                  z(kxy) = z(jxy)
                  jxy = jxy - 1
                  kxy = kxy - 1
               ENDDO
            ENDIF
         ENDIF
!
         ixy = nomega + 1
         nxy = ixy + lxy - 1
         idata = nxy + 1
         ndata = idata + lentry*Nsols - 1
         typout = 3
         IF ( Trnsnt ) typout = 1
!
!     FILL TITLE, SUBTITLE, AND LABEL FROM CASECC FOR THIS SUBCASE.
!
         DO i = 1 , 96
            Idrec(i+50) = z(Icc+i+37)
         ENDDO
         Idrec(4) = Subcas
!
!     CHECK FOR SUFFICIENT CORE.
!
         IF ( ndata>=buf3 ) THEN
            insuf = ndata - buf3
            WRITE (Outpt,99003) Uwm , Outfil , insuf
99003       FORMAT (A25,' 2338.  (DDRMM2-3)  DATA BLOCK',I5,' MAY NOT BE FULLY COMPLETED DUE TO A CORE INSUFFICIENCY',/5X,          &
                   &'OF APPROXIMATELY',I11,' DECIMAL WORDS.')
            Ipass = 3
            GOTO 100
         ENDIF
         DO
!
!     LOOP ON ID-S AVAILABLE FROM THE MAP
!
!
!     COMPUTE OMEGAS IF NECESSARY
!     (NOTE, VELOCITY PASS MAY NOT ALWAYS OCCUR)
!
            IF ( .NOT.(Trnsnt .OR. Ipass==1) ) THEN
               jlist = iomega - 1
               DO i = Ilist , Nlist
                  jlist = jlist + 1
                  Rz(jlist) = Rz(i)*Twopi
               ENDDO
               IF ( Ipass/=2 ) THEN
                  DO i = iomega , nomega
                     Rz(i) = -Rz(i)**2
                  ENDDO
               ENDIF
            ENDIF
!
            CALL read(*140,*80,scrt4,buf,Nwords,noeor,Nwds)
            Lminor = .TRUE.
            IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
               Npos = Savdat(minor)/100
               Nstxtr = Savdat(minor) - Npos*100
               CALL read(*140,*160,scrt4,Bufsav(1),Nstxtr,noeor,Nwds)
               Lminor = .FALSE.
            ENDIF
!
!     PREPARE AND OUTPUT THE OFP-ID-RECORD AFTER FIRST ENTRY IS COMBINED
!     AS IN THE CASE OF A FREQUENCY COMPLEX PROBLEM.
!
            Idout = .FALSE.
            Idrec(5) = buf(1)
!
!     SET STRESS OR FORCE COMPLEX DATA PTRS IF NECESSARY.
!
            IF ( .NOT.(Trnsnt) ) THEN
               IF ( Itype1==4 ) THEN
!
!     FORCES ASSUMED
!
                  ielem = (Idrec(3)-1)*Incr
                  Lsf = Elem(ielem+19)
                  Nptsf = Elem(ielem+21)
               ELSEIF ( Itype1==5 ) THEN
!
!     STRESSES ASSUMED
!
                  ielem = (Idrec(3)-1)*Incr
                  Lsf = Elem(ielem+18)
                  Nptsf = Elem(ielem+20)
               ENDIF
            ENDIF
!
!     UNPACK DATA FOR ALL COMPONENTS AND ALL SOLUTION STEPS
!     FOR THIS ID.  (NCOLS COLUMNS ARE NEEDED)
!
!
!     ZERO THE DATA SPACE
!
            DO i = idata , ndata
               z(i) = 0
            ENDDO
!
!     UNPACK NOW-ZERO TERMS.
!
            jdata = idata - lentry
            DO i = 1 , ncols
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL intpk(*82,scrt6,0,typout,0)
                     spag_nextblock_2 = 2
                  CASE (2)
                     SPAG_Loop_3_1: DO
!
!     COLUMN I HAS ONE OR MORE NON-ZEROES AVAILABLE.
!
                        CALL zntpki
                        Itemp = jdata + Irowo*lentry
                        IF ( .NOT.Trnsnt ) THEN
                           IF ( Ipass==1 ) THEN
!
!    DISPLACEMENTS, AND SPCFS (FREQ RESPONSE)
!
                              Rz(Itemp) = Aout(1)
                              Itemp = Itemp + ncols
                              Rz(Itemp) = Aout(2)
                              IF ( Ieol<=0 ) CYCLE
                              GOTO 82
                           ELSEIF ( Ipass==2 ) THEN
!
!     VELOCITIES  (FREQ RESPONSE)
!
                              klist = iomega + Irowo - 1
                              Rz(Itemp) = -Rz(klist)*Aout(2)
                              Itemp = Itemp + ncols
                              Rz(Itemp) = Rz(klist)*Aout(1)
                              IF ( Ieol<=0 ) CYCLE
                              GOTO 82
                           ELSEIF ( Ipass==3 ) THEN
!
!     ACCELERATIONS (FREQ RESPONSE)
!
                              klist = iomega + Irowo - 1
                              Rz(Itemp) = Rz(klist)*Aout(1)
                              Itemp = Itemp + ncols
                              Rz(Itemp) = Rz(klist)*Aout(2)
                              IF ( Ieol<=0 ) CYCLE
                              GOTO 82
                           ENDIF
                        ENDIF
                        EXIT SPAG_Loop_3_1
                     ENDDO SPAG_Loop_3_1
!
!     TRANSIENT OUTPUTS
!
                     Rz(Itemp) = Aout(1)
                     IF ( Ieol<=0 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
 82                  jdata = jdata + 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
!
!     OUTPUT LINES OF DATA COMBINING THEM FOR COMPLEX REAL/IMAGINARY OR
!     MAG/PHASE OFP FORMATS IF NECESSARY.
!
            jlist = Ilist - 1
            DO i = idata , ndata , lentry
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     jwords = Nwords
                     ij = i + ncols - 1
                     DO j = i , ij
                        jwords = jwords + 1
                        buf(jwords) = z(j)
                        IF ( .NOT.(Trnsnt) ) THEN
                           Itemp = j + ncols
                           buf(jwords+75) = z(Itemp)
                        ENDIF
                     ENDDO
!
!     IF TRANSIENT, ENTRY IS NOW READY FOR OUTPUT.
!
                     IF ( Trnsnt ) THEN
                        IF ( .NOT.(Lminor) ) THEN
                           DO k = 1 , Nstxtr
                              j = Savpos(Npos+k-1)
                              buf(j) = Bufsav(k)
                           ENDDO
                        ENDIF
                        spag_nextblock_3 = 5
                        CYCLE SPAG_DispatchLoop_3
!
!     MAP COMPLEX OUTPUTS TOGETHER PER -COMPLX- ARRAY.
!
                     ELSEIF ( Itype1==4 .OR. Itype1==5 ) THEN
!
!     ELEMENT STRESS OR FORCE DATA.
!
                        iout = 0
                        l = Nptsf
                        IF ( .NOT.(Lminor) ) THEN
                           DO k = 1 , Nstxtr
                              j = Savpos(Npos+k-1)
                              buf(j) = Bufsav(k)
                           ENDDO
                        ENDIF
                     ELSE
!
!     POINT DATA
!
                        DO k = 3 , 8
                           IF ( Form==3 ) CALL magpha(bufa(k),bufb(k))
                           bufa(k+6) = bufb(k)
                        ENDDO
                        jwords = 14
                        spag_nextblock_3 = 5
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     spag_nextblock_3 = 2
                  CASE (2)
                     npt = Complx(l)
                     IF ( npt<0 ) THEN
                        npt = -npt
                        IF ( Form/=3 ) THEN
                           spag_nextblock_3 = 4
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
!
!     COMPUTE MAGNITUDE/PHASE
!
                        CALL magpha(bufa(npt),bufb(npt))
                     ELSEIF ( npt==0 ) THEN
!
!     MOVE OUTPUT DATA
!
                        DO l = 1 , iout
                           buf(l) = elwork(l)
                        ENDDO
                        jwords = iout
                        spag_nextblock_3 = 5
                        CYCLE SPAG_DispatchLoop_3
                     ELSE
                        spag_nextblock_3 = 4
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     spag_nextblock_3 = 3
                  CASE (3)
                     iout = iout + 1
                     elwork(iout) = bufa(npt)
                     l = l + 1
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  CASE (4)
                     IF ( npt<=Lsf ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     npt = npt - Lsf
                     iout = iout + 1
                     elwork(iout) = bufb(npt)
                     l = l + 1
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  CASE (5)
!
!     CALL DDRMMS TO RECOMPUTE SOME ELEMENT STRESS QUANTITIES
!     IN TRANSIENT PROBLEMS ONLY.
!
                     IF ( Trnsnt .AND. Itype1==5 ) CALL ddrmms(buf,Idrec(3),buf4,buf5)
                     IF ( .NOT.(Idout) ) THEN
                        Idrec(9) = Form
                        Idrec(10) = jwords
                        CALL write(Outfil,Idrec,146,eor)
                        Idout = .TRUE.
                     ENDIF
                     jlist = jlist + 1
                     Rbuf(1) = Rz(jlist)
                     CALL write(Outfil,buf,jwords,noeor)
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
!
!     GO FOR NEXT OUTPUT ID
!
            CALL write(Outfil,0,0,eor)
         ENDDO
!
!  END OF DATA ON MAP FILE (SCRT4).
!
 100     CALL close(Outfil,Cls)
         CALL close(Infile,Clsrew)
         CALL close(scrt4,Clsrew)
         CALL close(scrt6,Clsrew)
         Ipass = Ipass + 1
         IF ( Ipass>Passes ) THEN
            RETURN
         ELSE
!
!     PREPARE FOR ANOTHER PASS
!
            File = Infile
            CALL open(*120,Infile,z(buf1),Rdrew)
            CALL fwdrec(*140,Infile)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     UNDEFINED FILE.
!
 120     RETURN 1
!
!     END OF FILE
!
 140     RETURN 2
!
!     END OF RECORD.
!
 160     RETURN 3
!
!     INSUFFICIENT CORE
!
 180     RETURN 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmm2
