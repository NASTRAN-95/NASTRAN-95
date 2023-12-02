!*==ddrmm2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmm2() !HIDESTARS (*,*,*,*)
   USE c_clstrs
   USE c_condas
   USE c_ddrmc1
   USE c_gpta1
   USE c_mpyadx
   USE c_names
   USE c_stdata
   USE c_system
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
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
         ipass = 1
         iomega = nlist + 1
         nomega = iomega - 1
         minor = 0
         spag_nextblock_1 = 2
      CASE (2)
         col1 = .TRUE.
         frstid = .TRUE.
         setid = sets(1,ipass)
         device = sets(2,ipass)
         form = sets(3,ipass)
         istlst = sets(4,ipass)
         lstlst = sets(5,ipass)
!
!     GET LIST OF XYPLOT REQUESTED IDS FOR CURRENT SUBCASE AND
!     OUTFIL TYPE.
!
         IF ( jfile==2 ) THEN
!
!     SPCF
!
            ixytyp = 4
         ELSEIF ( jfile==3 ) THEN
!
!     STRESS
!
            ixytyp = 6
         ELSEIF ( jfile==4 ) THEN
!
!     FORCE
!
            ixytyp = 7
         ELSE
!
!     DISPLACEMENT, VELOCITY, ACCELERATION
!
            ixytyp = ipass
         ENDIF
!
         ixy = nomega + 1
         CALL ddrmmp(*180,z(ixy),buf3-ixy,lxy,ixytyp,subcas,z(buf3),anyxy)
         IF ( .NOT.anyxy .AND. setid==0 ) GOTO 100
         nxy = ixy + lxy - 1
         ierror = 23
         file = scrt4
         CALL open(*120,scrt4,z(buf3),wrtrew)
         file = scrt5
         CALL open(*120,scrt5,z(buf2),wrtrew)
         CALL fname(scrt5,filnam)
         CALL write(scrt5,filnam,2,eor)
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
         file = infile
         ierror = 19
         mcb(1) = scrt5
         mcb(2) = 0
         mcb(3) = nlambs
         mcb(4) = 2
         mcb(5) = 1
         mcb(6) = 0
         mcb(7) = 0
         IF ( .NOT.(ipass==1 .AND. frstid) ) CALL read(*60,*60,infile,idrec,146,eor,nwds)
         spag_nextblock_1 = 3
      CASE (3)
!
!     OFP-ID RECORD IS WRITTEN TO THE MAP FILE ONLY ON CHANGE OF
!     MINOR ID.
!
         major = mod(idrec(2),1000)
         IF ( major/=itype1 ) THEN
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
            WRITE (outpt,99001) swm , infile
99001       FORMAT (A27,' 2339.  (DDRMM2-1) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',&
                   &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
            ipass = 3
            GOTO 100
         ELSE
            idvice = device
            id = idrec(5)/10
            IF ( setid<0 ) THEN
            ELSEIF ( setid==0 ) THEN
               GOTO 20
            ELSE
               next = 1
               CALL setfnd(*20,z(istlst),lstlst,id,next)
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
 40      CALL fwdrec(*140,infile)
         CALL read(*60,*60,infile,idrec,146,eor,nwds)
         spag_nextblock_1 = 3
      CASE (4)
!
!     ID IS TO BE OUTPUT THUS CONTINUE.
!
         numwds = nlambs*idrec(10)
         idata = nxy + 1
         ndata = idata + numwds - 1
         IF ( ndata<buf3 ) THEN
            IF ( .NOT.frstid ) THEN
               IF ( idrec(3)==minor ) THEN
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
               frstid = .FALSE.
            ENDIF
            CALL write(scrt4,idrec,146,eor)
            minor = idrec(3)
         ELSE
!
!     INSUFFICIENT CORE
!
            insuf = ndata - buf3
            WRITE (outpt,99002) uwm , infile , insuf
99002       FORMAT (A25,' 2337.  (DDRMM2-2)  DATA BLOCK',I5,' CAN NOT BE ','PROCESSED DUE TO',/5X,'A CORE INSUFFICIENCY OF APPROXI',&
                   &'MATELY',I11,' DECIMAL WORDS.')
            ipass = 3
            GOTO 100
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     SAME TYPE OF DATA THUS CONTINUE ON.
!
         lentry = idrec(10)
         i1 = nwords + 1
         i2 = lentry
!
!     READ AND OUTPUT ONE FULL OFP-DATA RECORD.
!
         CALL read(*140,*160,infile,z(idata),numwds,eor,nwds)
         DO i = i1 , i2
!
!     START NEW COLUMN
!
            CALL bldpk(1,1,scrt5,0,0)
            irow = 0
            jdata = idata + i - 1
            kdata = ndata - lentry + i
            DO j = jdata , kdata , lentry
               irow = irow + 1
               a(1) = rz(j)
!
!     ELIMINATE INTEGERS
!
!     OLD LOGIC -
!     IF (MACH.NE.5 .AND. IABS(IA(1)).LT.100000000) A(1) = 0.0
!     IF (MACH.EQ.5 .AND. (IA(1).LE.127 .AND. IA(1).GE.1)) A(1) = 0.0
!     OLD LOGIC SHOULD INCLUDE ALPHA MACHINE (MACH=21)
!
!     NEW LOGIC, BY G.CHAN/UNISYS  8/91 -
               IF ( numtyp(ia(1))<=1 ) a(1) = 0.0
!
               CALL zblpki
            ENDDO
!
!     COMPLETE COLUMN
!
            CALL bldpkn(scrt5,0,mcb)
         ENDDO
!
!     OUTPUT TO MAP THE ID PLUS ANY OTHER DATA NECESSARY.
!
         buf(1) = 10*id + idvice
         IF ( nwords==2 ) buf(2) = z(idata+1)
         nstxtr = 0
         IF ( itype1==5 .AND. savdat(minor)/=0 ) THEN
            npos = savdat(minor)/100
            nstxtr = savdat(minor) - npos*100
            DO i = 1 , nstxtr
               j = savpos(npos+i-1)
               buf(i+1) = z(idata+j-1)
            ENDDO
         ENDIF
         CALL write(scrt4,buf,nwords+nstxtr,noeor)
!
!     GO FOR NEXT ID.
!
         CALL read(*60,*60,infile,idrec,146,eor,nwds)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON INFILE.  MAP AND DATA MATRIX NOW COMPLETE.
!
 60      CALL wrttrl(mcb)
         CALL close(scrt5,clsrew)
         CALL close(infile,clsrew)
         CALL write(scrt4,0,0,eor)
         CALL close(scrt4,clsrew)
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
         mcba(1) = uvsol
         IF ( trnsnt ) mcba(1) = scrt(ipass)
         CALL rdtrl(mcba)
         mcbb(1) = scrt5
         CALL rdtrl(mcbb)
         mcbc(1) = 0
         mcbd(1) = scrt6
         mcbd(2) = 0
         mcbd(3) = nsols
         mcbd(4) = 2
         mcbd(5) = 1
         mcbd(6) = 0
         mcbd(7) = 0
         IF ( .NOT.trnsnt ) mcbd(5) = 3
         itflag = 1
         nxy1 = nxy + 1
         IF ( mod(nxy1,2)==0 ) nxy1 = nxy1 + 1
         lz = korsz(z(nxy1))
         isinab = 1
         isinc = 1
         iprec = 1
         iscrt = scrt7
         CALL mpyad(z(nxy1),z(nxy1),z(nxy1))
         mcbd(1) = scrt6
         CALL wrttrl(mcbd)
!
!     PRODUCT MATRIX IS NOW OUTPUT USING THE MAP ON SCRT4.
!     EACH COLUMN OF SCRT6 CONTAINS ALL THE TIME OR FREQUENCY STEP
!     VALUES FOR ONE COMPONENT OF ONE ID.
!
!     THUS A NUMBER OF COLUMNS ENCOMPASSING THE COMPONENTS OF ONE ID
!     MUST FIT IN CORE.
!
         ierror = 20
         file = outfil
         CALL open(*120,outfil,z(buf1),wrt)
         file = scrt4
         CALL open(*120,scrt4,z(buf2),rdrew)
         file = scrt6
         CALL open(*120,scrt6,z(buf3),rdrew)
         CALL fwdrec(*140,scrt6)
!
!     READ AN OFP-ID-RECORD FROM THE MAP, AND ALLOCATE SPACE NEEDED
!     FOR SOLUTION DATA.
!
         file = scrt4
 80      CALL read(*100,*160,scrt4,idrec,146,eor,nwds)
         minor = idrec(3)
!
!
!     SET DISPLACEMENT, VELOCITY, OR ACCELERATION OFP MAJOR-ID IF
!     INFILE IS MODAL DISPLACEMETNS = EIGENVECTORS...
!
         IF ( itype1==7 ) idrec(2) = dvamid(ipass)
         IF ( .NOT.trnsnt ) idrec(2) = idrec(2) + 1000
!
!     RESET APPROACH CODE FROM EIGENVALUE TO TRANSIENT OR FREQUENCY
!
         iapp = 5
         IF ( trnsnt ) iapp = 6
         idrec(1) = 10*iapp + device
         lentry = idrec(10) - nwords
         ncols = lentry
         IF ( .NOT.trnsnt ) lentry = lentry + lentry
!
!     IF FREQUENCY RESPONSE PROBLEM AND THIS IS THE VELOCITY OR
!     ACCELERATION PASS THEN MOVE DOWN ANY XY LIST OF POINTS AND
!     ADD AN OMEGA TABLE.  SOMETIMES THE MOVEDOWN OF THE XY LIST IS
!     REDUNDANT.
!
!     XY LIST IS MOVED FROM BOTTOM UP INCASE XY LIST IS LONGER THAN
!     THE OMEGA LIST WILL BE.
!
         IF ( .NOT.(trnsnt .OR. ipass==1) ) THEN
            nomega = iomega + nsols - 1
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
         ndata = idata + lentry*nsols - 1
         typout = 3
         IF ( trnsnt ) typout = 1
!
!     FILL TITLE, SUBTITLE, AND LABEL FROM CASECC FOR THIS SUBCASE.
!
         DO i = 1 , 96
            idrec(i+50) = z(icc+i+37)
         ENDDO
         idrec(4) = subcas
!
!     CHECK FOR SUFFICIENT CORE.
!
         IF ( ndata>=buf3 ) THEN
            insuf = ndata - buf3
            WRITE (outpt,99003) uwm , outfil , insuf
99003       FORMAT (A25,' 2338.  (DDRMM2-3)  DATA BLOCK',I5,' MAY NOT BE FULLY COMPLETED DUE TO A CORE INSUFFICIENCY',/5X,          &
                   &'OF APPROXIMATELY',I11,' DECIMAL WORDS.')
            ipass = 3
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
            IF ( .NOT.(trnsnt .OR. ipass==1) ) THEN
               jlist = iomega - 1
               DO i = ilist , nlist
                  jlist = jlist + 1
                  rz(jlist) = rz(i)*twopi
               ENDDO
               IF ( ipass/=2 ) THEN
                  DO i = iomega , nomega
                     rz(i) = -rz(i)**2
                  ENDDO
               ENDIF
            ENDIF
!
            CALL read(*140,*80,scrt4,buf,nwords,noeor,nwds)
            lminor = .TRUE.
            IF ( itype1==5 .AND. savdat(minor)/=0 ) THEN
               npos = savdat(minor)/100
               nstxtr = savdat(minor) - npos*100
               CALL read(*140,*160,scrt4,bufsav(1),nstxtr,noeor,nwds)
               lminor = .FALSE.
            ENDIF
!
!     PREPARE AND OUTPUT THE OFP-ID-RECORD AFTER FIRST ENTRY IS COMBINED
!     AS IN THE CASE OF A FREQUENCY COMPLEX PROBLEM.
!
            idout = .FALSE.
            idrec(5) = buf(1)
!
!     SET STRESS OR FORCE COMPLEX DATA PTRS IF NECESSARY.
!
            IF ( .NOT.(trnsnt) ) THEN
               IF ( itype1==4 ) THEN
!
!     FORCES ASSUMED
!
                  ielem = (idrec(3)-1)*incr
                  lsf = elem(ielem+19)
                  nptsf = elem(ielem+21)
               ELSEIF ( itype1==5 ) THEN
!
!     STRESSES ASSUMED
!
                  ielem = (idrec(3)-1)*incr
                  lsf = elem(ielem+18)
                  nptsf = elem(ielem+20)
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
                        itemp = jdata + irowo*lentry
                        IF ( .NOT.trnsnt ) THEN
                           IF ( ipass==1 ) THEN
!
!    DISPLACEMENTS, AND SPCFS (FREQ RESPONSE)
!
                              rz(itemp) = aout(1)
                              itemp = itemp + ncols
                              rz(itemp) = aout(2)
                              IF ( ieol<=0 ) CYCLE
                              GOTO 82
                           ELSEIF ( ipass==2 ) THEN
!
!     VELOCITIES  (FREQ RESPONSE)
!
                              klist = iomega + irowo - 1
                              rz(itemp) = -rz(klist)*aout(2)
                              itemp = itemp + ncols
                              rz(itemp) = rz(klist)*aout(1)
                              IF ( ieol<=0 ) CYCLE
                              GOTO 82
                           ELSEIF ( ipass==3 ) THEN
!
!     ACCELERATIONS (FREQ RESPONSE)
!
                              klist = iomega + irowo - 1
                              rz(itemp) = rz(klist)*aout(1)
                              itemp = itemp + ncols
                              rz(itemp) = rz(klist)*aout(2)
                              IF ( ieol<=0 ) CYCLE
                              GOTO 82
                           ENDIF
                        ENDIF
                        EXIT SPAG_Loop_3_1
                     ENDDO SPAG_Loop_3_1
!
!     TRANSIENT OUTPUTS
!
                     rz(itemp) = aout(1)
                     IF ( ieol<=0 ) THEN
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
            jlist = ilist - 1
            DO i = idata , ndata , lentry
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     jwords = nwords
                     ij = i + ncols - 1
                     DO j = i , ij
                        jwords = jwords + 1
                        buf(jwords) = z(j)
                        IF ( .NOT.(trnsnt) ) THEN
                           itemp = j + ncols
                           buf(jwords+75) = z(itemp)
                        ENDIF
                     ENDDO
!
!     IF TRANSIENT, ENTRY IS NOW READY FOR OUTPUT.
!
                     IF ( trnsnt ) THEN
                        IF ( .NOT.(lminor) ) THEN
                           DO k = 1 , nstxtr
                              j = savpos(npos+k-1)
                              buf(j) = bufsav(k)
                           ENDDO
                        ENDIF
                        spag_nextblock_3 = 5
                        CYCLE SPAG_DispatchLoop_3
!
!     MAP COMPLEX OUTPUTS TOGETHER PER -COMPLX- ARRAY.
!
                     ELSEIF ( itype1==4 .OR. itype1==5 ) THEN
!
!     ELEMENT STRESS OR FORCE DATA.
!
                        iout = 0
                        l = nptsf
                        IF ( .NOT.(lminor) ) THEN
                           DO k = 1 , nstxtr
                              j = savpos(npos+k-1)
                              buf(j) = bufsav(k)
                           ENDDO
                        ENDIF
                     ELSE
!
!     POINT DATA
!
                        DO k = 3 , 8
                           IF ( form==3 ) CALL magpha(bufa(k),bufb(k))
                           bufa(k+6) = bufb(k)
                        ENDDO
                        jwords = 14
                        spag_nextblock_3 = 5
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     spag_nextblock_3 = 2
                  CASE (2)
                     npt = complx(l)
                     IF ( npt<0 ) THEN
                        npt = -npt
                        IF ( form/=3 ) THEN
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
                  CASE (4)
                     IF ( npt<=lsf ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     npt = npt - lsf
                     iout = iout + 1
                     elwork(iout) = bufb(npt)
                     l = l + 1
                     spag_nextblock_3 = 2
                  CASE (5)
!
!     CALL DDRMMS TO RECOMPUTE SOME ELEMENT STRESS QUANTITIES
!     IN TRANSIENT PROBLEMS ONLY.
!
                     IF ( trnsnt .AND. itype1==5 ) CALL ddrmms(buf,idrec(3),buf4,buf5)
                     IF ( .NOT.(idout) ) THEN
                        idrec(9) = form
                        idrec(10) = jwords
                        CALL write(outfil,idrec,146,eor)
                        idout = .TRUE.
                     ENDIF
                     jlist = jlist + 1
                     rbuf(1) = rz(jlist)
                     CALL write(outfil,buf,jwords,noeor)
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
!
!     GO FOR NEXT OUTPUT ID
!
            CALL write(outfil,0,0,eor)
         ENDDO
!
!  END OF DATA ON MAP FILE (SCRT4).
!
 100     CALL close(outfil,cls)
         CALL close(infile,clsrew)
         CALL close(scrt4,clsrew)
         CALL close(scrt6,clsrew)
         ipass = ipass + 1
         IF ( ipass>passes ) THEN
            RETURN
         ELSE
!
!     PREPARE FOR ANOTHER PASS
!
            file = infile
            CALL open(*120,infile,z(buf1),rdrew)
            CALL fwdrec(*140,infile)
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
