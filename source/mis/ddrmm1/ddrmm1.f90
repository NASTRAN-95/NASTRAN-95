!*==ddrmm1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmm1() !HIDESTARS (*,*,*,*)
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
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , i , iapp , id , idvice , ielem , irow1 , ixy , ixytyp , j , jlist , jp ,    &
            & lentry , lxy , majid , minor , next , nxy , nxy1 , scrt1 , scrt2 , scrt3 , scrt4 , scrt5 , scrt6 , scrt7
   INTEGER , DIMENSION(3) , SAVE :: dvamid
   INTEGER , SAVE :: eor , noeor
   INTEGER , DIMENSION(4) :: ia
   REAL , DIMENSION(6) :: ridrec
   INTEGER , DIMENSION(1) :: z
   EXTERNAL bisloc , bldpk , bldpkn , close , ddrmma , ddrmmp , ddrmms , fname , fwdrec , korsz , mpyad , numtyp , open , rdtrl ,   &
          & read , rewind , setfnd , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PERFORMS SORT1 TYPE PROCESSING FOR MODULE DDRMM.
!
   !>>>>EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2)) , (Scrt3,Scrt(3)) , (Scrt4,Scrt(4)) , (Scrt5,Scrt(5)) , (Scrt6,Scrt(6)) ,          &
!>>>>    & (Scrt7,Scrt(7)) , (Buf1,Buff(1)) , (Buf2,Buff(2)) , (Buf3,Buff(3)) , (Buf4,Buff(4)) , (Buf5,Buff(5)) , (Buf6,Buff(6)) ,       &
!>>>>    & (A(1),Ia(1)) , (Z(1),Rz(1)) , (Idrec(1),Ridrec(1)) , (Buf(1),Rbuf(1))
   DATA eor , noeor/1 , 0/ , dvamid/1 , 10 , 11/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FORMATION OF DATA-MATRIX AND SUBSEQUENT MULTIPLY BY SOLUTION-
!     MATRIX AND ULTIMATE OUTPUT OF TRANSIENT OR FREQUENCY SOLUTIONS.
!
         ipass = 1
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
         ixy = nlist + 1
         CALL ddrmmp(*200,z(ixy),buf3-ixy,lxy,ixytyp,subcas,z(buf3),anyxy)
         IF ( .NOT.anyxy .AND. setid==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nxy = ixy + lxy - 1
!
!     INITIALIZE DATA MATRIX FILE(SCRT5), AND MAPPING TABLE FILE(SCRT4).
!
         ierror = 22
         file = scrt4
         CALL open(*140,scrt4,z(buf3),wrtrew)
         file = scrt5
         CALL open(*140,scrt5,z(buf2),wrtrew)
         CALL fname(scrt5,filnam)
         CALL write(scrt5,filnam,2,eor)
!
!     GENERAL LOGIC TO BUILD SORT1 FORMAT DATA MATRIX.
!
!     EACH COLUMN WRITTEN HERE REPRESENTS ONE EIGENVALUE.
!
!          COMPONENTS FOR FIRST ID   *
!              .                      *
!              .                       *
!              .                        *
!          COMPONENTS FOR NEXT ID        * ONE COLUMN
!              .                        *  OF DATA FOR EACH EIGENVALUE.
!              .                       *
!              .                      *
!             ETC                    *
!     --------------------------------------------- EOR
!
!          IDENTICAL COMPONENTS ARE REPRESENTED IN EACH COLUMN.
!
!
!     READ AN OFP-ID-RECORD AND SET PARAMETERS.
!     (ON ENTRY TO THIS PROCESSOR THE FIRST ID RECORD IS AT HAND)
!
         file = infile
         mcb(1) = scrt5
         mcb(2) = 0
         mcb(3) = 0
         mcb(4) = 2
         mcb(5) = 1
         mcb(6) = 0
         mcb(7) = 0
         IF ( ipass==1 .AND. frstid ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*80,*80,infile,idrec,146,eor,nwds)
         majid = mod(idrec(2),1000)
         IF ( majid/=itype1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     IF FIRST COLUMN, OFP-ID RECORD IS WRITTEN AS IS TO MAP FILE.
!
         IF ( col1 ) THEN
            IF ( .NOT.(.NOT.frstid .AND. ridrec(6)/=lambda) ) CALL write(scrt4,idrec,146,eor)
         ENDIF
         lentry = idrec(10)
         i1 = nwords + 1
         i2 = lentry
         minor = idrec(3)
!
!     IF SAME EIGENVALUE AS THAT OF LAST OFP-ID RECORD THEN CONTINUE.
!
         IF ( .NOT.(frstid) ) THEN
            IF ( ridrec(6)==lambda ) GOTO 20
!
!     NEW EIGENVALUE. COMPLETE CURRENT DATA MATRIX COLUMN AND START
!     NEW COLUMN. PASS ONE IS NOW COMPLETE.
!
            CALL bldpkn(scrt5,0,mcb)
            IF ( col1 ) irow1 = irow
            IF ( irow/=irow1 ) THEN
!
!     DATA INCONSISTENCY ON -INFILE-.
!
               WRITE (outpt,99001) swm , infile
99001          FORMAT (A27,' 2335.  (DDRMM1-1) THE AMOUNT OF DATA IS NOT ','CONSISTENT FOR EACH EIGENVALUE IN DATA BLOCK',I5,/5X,   &
                      &'PROCESSING OF THIS DATA BLOCK TERMINATED.')
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               col1 = .FALSE.
            ENDIF
         ENDIF
!
!     START NEW COLUMN.
!
         CALL bldpk(1,1,scrt5,0,0)
         irow = 0
         frstid = .FALSE.
         lambda = ridrec(6)
!
!     READ A POINT OR ELEMENT ENTRY.
!
 20      CALL read(*160,*60,infile,buf,lentry,noeor,nwds)
         id = buf(1)/10
!
!     CHECK FOR ID IN OUTPUT REQUEST LIST
!
         idvice = device
         IF ( setid<0 ) THEN
         ELSEIF ( setid==0 ) THEN
            GOTO 40
         ELSE
!
!//// NEXT MAY NOT NEED TO BE INITIALIZED EVERY TIME.
!
            next = 1
            CALL setfnd(*40,z(istlst),lstlst,id,next)
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      IF ( .NOT.anyxy ) GOTO 20
         CALL bisloc(*20,id,z(ixy),1,lxy,jp)
         idvice = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     THIS ID IS TO BE OUTPUT.
!
         IF ( col1 ) THEN
            buf(1) = 10*id + idvice
            CALL write(scrt4,buf(1),nwords,noeor)
            nstxtr = 0
            IF ( itype1==5 .AND. savdat(minor)/=0 ) THEN
               npos = savdat(minor)/100
               nstxtr = savdat(minor) - npos*100
               DO i = 1 , nstxtr
                  j = savpos(npos+i-1)
                  bufsav(i) = buf(j)
               ENDDO
               CALL write(scrt4,bufsav(1),nstxtr,noeor)
            ENDIF
         ENDIF
!
!     OUTPUT TO DATA MATRIX THE COMPONENTS OF THIS ENTRY.
!
         DO i = i1 , i2
            irow = irow + 1
            a(1) = rbuf(i)
!
!     GET RID OF INTEGERS.
!
!     OLD LOGIC -
!     IF (MACH.NE.5 .AND.  IABS(IA(1)) .LT.   100000000) A(1) = 0.0
!     IF (MACH.EQ.5 .AND. (IA(1).LE.127.AND.IA(1).GE.1)) A(1) = 0.0
!     OLD LOGIC SHOULD INCLUDE ALPHA MACHINE (MACH=21)
!
!     NEW LOGIC BY G.CHAN/UNISYS, 8/91 -
            IF ( numtyp(ia(1))<=1 ) a(1) = 0.0
!
            CALL zblpki
         ENDDO
         GOTO 20
!
!     END OF CURRENT OFP-DATA RECORD ENCOUNTERED.
!     IF NEXT OFP-ID-RECORD INDICATES ANOTHER OFP-DATA RECORD FOR
!     THIS SAME EIGENVALUE (I.E. A CHANGE IN ELEMENT TYPE) THEN
!     FURTHER CONSTRUCTION OF THE DATA MATRIX COLUMN TAKES PLACE.
!
 60      IF ( col1 ) CALL write(scrt4,0,0,eor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ENCOUNTERED ON INFILE.
!     DATA MATRIX AND MAPING FILE ARE COMPLETE.
!
 80      CALL close(infile,clsrew)
         CALL close(scrt4,clsrew)
!
!     COMPLETE LAST COLUMN OF DATA MATRIX WRITTEN.
!
         IF ( col1 ) irow1 = irow
         IF ( irow/=irow1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bldpkn(scrt5,0,mcb)
         mcb(3) = irow
         CALL wrttrl(mcb)
         CALL close(scrt5,clsrew)
!
!     TO GET SOLUTION MATRIX BASED ON SORT-1 INFILE.
!
!     SOLVE,
!              (DATA MATRIX)     X    (MODAL SOLUTION MATRIX)
!             NCOMPS X NLAMBS           NLAMBS X NSOLUTIONS
!             ===============         =======================
!
!     RESULTANT MATRIX IS NCOMPS BY NSOLUTIONS IN SIZE.
!
!
!     MATRIX MULTIPLY SETUP AND CALL.
!
         mcba(1) = scrt5
         CALL rdtrl(mcba)
         mcbb(1) = uvsol
         IF ( trnsnt ) mcbb(1) = scrt(ipass)
         CALL rdtrl(mcbb)
         mcbc(1) = 0
         mcbd(1) = scrt6
         mcbd(2) = 0
         mcbd(3) = irow
         mcbd(4) = 2
         mcbd(5) = 1
         mcbd(6) = 0
         mcbd(7) = 0
         IF ( .NOT.trnsnt ) mcbd(5) = 3
         nxy1 = nxy + 1
         IF ( mod(nxy1,2)==0 ) nxy1 = nxy1 + 1
         lz = korsz(z(nxy1))
         itflag = 0
         isinab = 1
         isinc = 1
         iprec = 1
         iscrt = scrt7
         CALL mpyad(z(nxy1),z(nxy1),z(nxy1))
         mcbd(1) = scrt6
         CALL wrttrl(mcbd)
!
!     PRODUCT MATRIX IS NOW OUTPUT, USING THE MAP ON SCRT4 FOR EACH
!     COLUMN.  (SORT-1)  PRODUCT MATRIX IS ON SCRATCH DATA BLOCK 6.
!
         ierror = 10
         file = outfil
         CALL open(*140,outfil,z(buf1),wrt)
         file = scrt4
         CALL open(*140,scrt4,z(buf2),rdrew)
         file = scrt6
         CALL open(*140,scrt6,z(buf3),rdrew)
         CALL fwdrec(*160,scrt6)
         jlist = ilist
         spag_nextblock_1 = 6
      CASE (6)
!
!     LOOP ON COLUMNS OF SCRT6.
!
         CALL ddrmma(.TRUE.)
!
!     READ AN OFP-ID-RECORD FROM THE MAP.
!
         file = scrt4
         spag_nextblock_1 = 7
      CASE (7)
         CALL read(*120,*180,scrt4,idrec,146,eor,nwds)
!
!     SET THE FREQUENCY OR TIME AND CLOBBER THE EIGENVALUE.
!
         ridrec(5) = rz(jlist)
         ridrec(6) = 0.0
         idout = .FALSE.
         minor = idrec(3)
!
!     SET NUMBER OF STRESS OR FORCE WORDS AND COMPLEX POINTERS IF
!     NECESSARY
!
         itype2 = idrec(3)
         IF ( itype1/=3 .AND. itype1/=7 ) THEN
            ielem = (itype2-1)*incr
            IF ( itype1==4 ) THEN
!
!     FORCES ASSUMED.
!
               lsf = elem(ielem+19)
               nptsf = elem(ielem+21)
               nwdsf = lsf
            ELSEIF ( itype1==5 ) THEN
!
!     STRESSES ASSUMED.
!
               lsf = elem(ielem+18)
               nptsf = elem(ielem+20)
               nwdsf = lsf
            ELSE
               WRITE (outpt,99002) swm , itype1 , itype2 , infile
99002          FORMAT (A27,' 2334.  (DDRMM-3) ILLEGAL MAJOR OR MINOR OFP-ID ','IDENTIFICATIONS =',2I10,/5X,'DETECTED IN DATA BLOCK',&
                     & I5,'. PROCESSING OF SAID DATA BLOCK DISCONTINUED.')
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     SPCF OR DISPLACEMENTS ASSUMED
!
         ELSEIF ( .NOT.trnsnt ) THEN
            nwdsf = 14
!
!     SET OMEGA IF THIS IS THE VELOCITY OR ACCELERATION PASS
!
            IF ( ipass==1 ) THEN
            ELSEIF ( ipass==3 ) THEN
!
!     OMEGA FOR ACCELERATION PASS
!
               omega = -((twopi*rz(jlist))**2)
            ELSE
!
!     OMEGA FOR VELOCITY PASS
!
               omega = twopi*rz(jlist)
            ENDIF
         ELSE
            nwdsf = 8
         ENDIF
!
         lentry = idrec(10)
         i1 = nwords + 1
         i2 = lentry
!
!
!     SET DISPLACEMENT, VELOCITY, OR ACCELERATION OFP MAJOR ID IF INFILE
!     IS MODAL DISPLACEMENTS.
!
         IF ( itype1==7 ) idrec(2) = dvamid(ipass)
         IF ( .NOT.trnsnt ) idrec(2) = idrec(2) + 1000
!
!     RESET APPROACH CODE FROM EIGENVALUE TO TRANSIENT OR FREQUENCY
!
         iapp = 5
         IF ( trnsnt ) iapp = 6
         idrec(1) = 10*iapp + device
!
!     FILL TITLE, SUBTITLE, AND LABEL FROM CASECC FOR THIS SUBCASE.
!
         DO i = 1 , 96
            idrec(i+50) = z(icc+i+37)
         ENDDO
         idrec(4) = subcas
         DO
!
!     READ FIRST WORDS OF OUTPUT ENTRY FROM MAP.
!
            CALL read(*160,*100,scrt4,buf,nwords,noeor,nwds)
            lminor = .TRUE.
            IF ( itype1==5 .AND. savdat(minor)/=0 ) THEN
               npos = savdat(minor)/100
               nstxtr = savdat(minor) - npos*100
               CALL read(*160,*180,scrt4,bufsav(1),nstxtr,noeor,nwds)
               lminor = .FALSE.
            ENDIF
!
!     GET BALANCE USING UTILITY WHICH WILL COLLECT AND MAP TOGETHER
!     AS REQUIRED REAL OR COMPLEX, AND GENERATE MAGNITUDE/PHASE IF
!     REQUIRED.  (THIS ROUTINE WILL BUFFER DATA IN FROM SCRT6 AS IT
!     NEEDS IT.)
!
            CALL ddrmma(.FALSE.)
!
!     CALL DDRMMS TO RECOMPUTE SOME ELEMENT STRESS QUANTITIES
!     IN TRANSIENT PROBLEMS ONLY.
!
            IF ( trnsnt .AND. itype1==5 ) CALL ddrmms(buf,itype2,buf4,buf5)
            IF ( .NOT.(idout) ) THEN
               idrec(9) = form
               idrec(10) = nwdsf
               CALL write(outfil,idrec,146,eor)
               idout = .TRUE.
            ENDIF
!
!     OUTPUT THE COMPLETED ENTRY TO OFP OUTFIL.
!
            CALL write(outfil,buf,nwdsf,noeor)
         ENDDO
!
!     END OF ENTRIES FOR ONE ID-REC HIT.  IF NO EOF ON MAP WITH
!     NEXT READ, THEN CONTINUE OUTPUT OF THIS SOLUTION COLUMN.
!
 100     CALL write(outfil,0,0,eor)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON MAP.  THUS START NEXT COLUMN IF REQUIRED.
!
 120     jlist = jlist + 1
         IF ( jlist<=nlist ) THEN
            CALL rewind(scrt4)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     ALL DATA OF SOLUTION PRODUCT MATRIX HAS NOW BEEN OUTPUT.
!
         CALL close(outfil,cls)
         CALL close(infile,clsrew)
         CALL close(scrt4,clsrew)
         CALL close(scrt6,clsrew)
         ipass = ipass + 1
         IF ( ipass>passes ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PREPARE FOR ANOTHER PASS
!
         file = infile
         CALL open(*140,infile,z(buf1),rdrew)
         CALL fwdrec(*160,infile)
         spag_nextblock_1 = 2
      CASE (9)
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
         WRITE (outpt,99003) swm , infile
99003    FORMAT (A27,' 2336.  (DDRMM1-2) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',   &
                &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
         spag_nextblock_1 = 10
      CASE (10)
         ipass = 3
         spag_nextblock_1 = 8
      CASE (11)
!
!     COMPLETION OF PASS FOR INPUT MODAL SOLUTION -FILE-.
!
         RETURN
!
!     UNDEFINED FILE.
!
 140     RETURN 1
!
!     END OF FILE HIT.
!
 160     RETURN 2
!
!     END OF RECORD HIT.
!
 180     RETURN 3
!
!     INSUFFICIENT CORE.
!
 200     RETURN 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmm1
