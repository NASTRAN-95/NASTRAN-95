!*==ddrmm1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmm1() !HIDESTARS (*,*,*,*)
   IMPLICIT NONE
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
         Ipass = 1
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
         ixy = Nlist + 1
         CALL ddrmmp(*200,z(ixy),buf3-ixy,lxy,ixytyp,Subcas,z(buf3),anyxy)
         IF ( .NOT.anyxy .AND. Setid==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nxy = ixy + lxy - 1
!
!     INITIALIZE DATA MATRIX FILE(SCRT5), AND MAPPING TABLE FILE(SCRT4).
!
         Ierror = 22
         File = scrt4
         CALL open(*140,scrt4,z(buf3),Wrtrew)
         File = scrt5
         CALL open(*140,scrt5,z(buf2),Wrtrew)
         CALL fname(scrt5,Filnam)
         CALL write(scrt5,Filnam,2,eor)
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
         File = Infile
         Mcb(1) = scrt5
         Mcb(2) = 0
         Mcb(3) = 0
         Mcb(4) = 2
         Mcb(5) = 1
         Mcb(6) = 0
         Mcb(7) = 0
         IF ( Ipass==1 .AND. Frstid ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*80,*80,Infile,Idrec,146,eor,Nwds)
         majid = mod(Idrec(2),1000)
         IF ( majid/=Itype1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     IF FIRST COLUMN, OFP-ID RECORD IS WRITTEN AS IS TO MAP FILE.
!
         IF ( Col1 ) THEN
            IF ( .NOT.(.NOT.Frstid .AND. ridrec(6)/=Lambda) ) CALL write(scrt4,Idrec,146,eor)
         ENDIF
         lentry = Idrec(10)
         I1 = Nwords + 1
         I2 = lentry
         minor = Idrec(3)
!
!     IF SAME EIGENVALUE AS THAT OF LAST OFP-ID RECORD THEN CONTINUE.
!
         IF ( .NOT.(Frstid) ) THEN
            IF ( ridrec(6)==Lambda ) GOTO 20
!
!     NEW EIGENVALUE. COMPLETE CURRENT DATA MATRIX COLUMN AND START
!     NEW COLUMN. PASS ONE IS NOW COMPLETE.
!
            CALL bldpkn(scrt5,0,Mcb)
            IF ( Col1 ) irow1 = Irow
            IF ( Irow/=irow1 ) THEN
!
!     DATA INCONSISTENCY ON -INFILE-.
!
               WRITE (Outpt,99001) Swm , Infile
99001          FORMAT (A27,' 2335.  (DDRMM1-1) THE AMOUNT OF DATA IS NOT ','CONSISTENT FOR EACH EIGENVALUE IN DATA BLOCK',I5,/5X,   &
                      &'PROCESSING OF THIS DATA BLOCK TERMINATED.')
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Col1 = .FALSE.
            ENDIF
         ENDIF
!
!     START NEW COLUMN.
!
         CALL bldpk(1,1,scrt5,0,0)
         Irow = 0
         Frstid = .FALSE.
         Lambda = ridrec(6)
!
!     READ A POINT OR ELEMENT ENTRY.
!
 20      CALL read(*160,*60,Infile,buf,lentry,noeor,Nwds)
         id = buf(1)/10
!
!     CHECK FOR ID IN OUTPUT REQUEST LIST
!
         idvice = Device
         IF ( Setid<0 ) THEN
         ELSEIF ( Setid==0 ) THEN
            GOTO 40
         ELSE
!
!//// NEXT MAY NOT NEED TO BE INITIALIZED EVERY TIME.
!
            next = 1
            CALL setfnd(*40,z(Istlst),Lstlst,id,next)
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
         IF ( Col1 ) THEN
            buf(1) = 10*id + idvice
            CALL write(scrt4,buf(1),Nwords,noeor)
            Nstxtr = 0
            IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
               Npos = Savdat(minor)/100
               Nstxtr = Savdat(minor) - Npos*100
               DO i = 1 , Nstxtr
                  j = Savpos(Npos+i-1)
                  Bufsav(i) = buf(j)
               ENDDO
               CALL write(scrt4,Bufsav(1),Nstxtr,noeor)
            ENDIF
         ENDIF
!
!     OUTPUT TO DATA MATRIX THE COMPONENTS OF THIS ENTRY.
!
         DO i = I1 , I2
            Irow = Irow + 1
            A(1) = Rbuf(i)
!
!     GET RID OF INTEGERS.
!
!     OLD LOGIC -
!     IF (MACH.NE.5 .AND.  IABS(IA(1)) .LT.   100000000) A(1) = 0.0
!     IF (MACH.EQ.5 .AND. (IA(1).LE.127.AND.IA(1).GE.1)) A(1) = 0.0
!     OLD LOGIC SHOULD INCLUDE ALPHA MACHINE (MACH=21)
!
!     NEW LOGIC BY G.CHAN/UNISYS, 8/91 -
            IF ( numtyp(ia(1))<=1 ) A(1) = 0.0
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
 60      IF ( Col1 ) CALL write(scrt4,0,0,eor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ENCOUNTERED ON INFILE.
!     DATA MATRIX AND MAPING FILE ARE COMPLETE.
!
 80      CALL close(Infile,Clsrew)
         CALL close(scrt4,Clsrew)
!
!     COMPLETE LAST COLUMN OF DATA MATRIX WRITTEN.
!
         IF ( Col1 ) irow1 = Irow
         IF ( Irow/=irow1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bldpkn(scrt5,0,Mcb)
         Mcb(3) = Irow
         CALL wrttrl(Mcb)
         CALL close(scrt5,Clsrew)
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
         Mcba(1) = scrt5
         CALL rdtrl(Mcba)
         Mcbb(1) = Uvsol
         IF ( Trnsnt ) Mcbb(1) = Scrt(Ipass)
         CALL rdtrl(Mcbb)
         Mcbc(1) = 0
         Mcbd(1) = scrt6
         Mcbd(2) = 0
         Mcbd(3) = Irow
         Mcbd(4) = 2
         Mcbd(5) = 1
         Mcbd(6) = 0
         Mcbd(7) = 0
         IF ( .NOT.Trnsnt ) Mcbd(5) = 3
         nxy1 = nxy + 1
         IF ( mod(nxy1,2)==0 ) nxy1 = nxy1 + 1
         Lz = korsz(z(nxy1))
         Itflag = 0
         Isinab = 1
         Isinc = 1
         Iprec = 1
         Iscrt = scrt7
         CALL mpyad(z(nxy1),z(nxy1),z(nxy1))
         Mcbd(1) = scrt6
         CALL wrttrl(Mcbd)
!
!     PRODUCT MATRIX IS NOW OUTPUT, USING THE MAP ON SCRT4 FOR EACH
!     COLUMN.  (SORT-1)  PRODUCT MATRIX IS ON SCRATCH DATA BLOCK 6.
!
         Ierror = 10
         File = Outfil
         CALL open(*140,Outfil,z(buf1),Wrt)
         File = scrt4
         CALL open(*140,scrt4,z(buf2),Rdrew)
         File = scrt6
         CALL open(*140,scrt6,z(buf3),Rdrew)
         CALL fwdrec(*160,scrt6)
         jlist = Ilist
         spag_nextblock_1 = 6
      CASE (6)
!
!     LOOP ON COLUMNS OF SCRT6.
!
         CALL ddrmma(.TRUE.)
!
!     READ AN OFP-ID-RECORD FROM THE MAP.
!
         File = scrt4
         spag_nextblock_1 = 7
      CASE (7)
         CALL read(*120,*180,scrt4,Idrec,146,eor,Nwds)
!
!     SET THE FREQUENCY OR TIME AND CLOBBER THE EIGENVALUE.
!
         ridrec(5) = Rz(jlist)
         ridrec(6) = 0.0
         Idout = .FALSE.
         minor = Idrec(3)
!
!     SET NUMBER OF STRESS OR FORCE WORDS AND COMPLEX POINTERS IF
!     NECESSARY
!
         Itype2 = Idrec(3)
         IF ( Itype1/=3 .AND. Itype1/=7 ) THEN
            ielem = (Itype2-1)*Incr
            IF ( Itype1==4 ) THEN
!
!     FORCES ASSUMED.
!
               Lsf = Elem(ielem+19)
               Nptsf = Elem(ielem+21)
               Nwdsf = Lsf
            ELSEIF ( Itype1==5 ) THEN
!
!     STRESSES ASSUMED.
!
               Lsf = Elem(ielem+18)
               Nptsf = Elem(ielem+20)
               Nwdsf = Lsf
            ELSE
               WRITE (Outpt,99002) Swm , Itype1 , Itype2 , Infile
99002          FORMAT (A27,' 2334.  (DDRMM-3) ILLEGAL MAJOR OR MINOR OFP-ID ','IDENTIFICATIONS =',2I10,/5X,'DETECTED IN DATA BLOCK',&
                     & I5,'. PROCESSING OF SAID DATA BLOCK DISCONTINUED.')
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     SPCF OR DISPLACEMENTS ASSUMED
!
         ELSEIF ( .NOT.Trnsnt ) THEN
            Nwdsf = 14
!
!     SET OMEGA IF THIS IS THE VELOCITY OR ACCELERATION PASS
!
            IF ( Ipass==1 ) THEN
            ELSEIF ( Ipass==3 ) THEN
!
!     OMEGA FOR ACCELERATION PASS
!
               Omega = -((Twopi*Rz(jlist))**2)
            ELSE
!
!     OMEGA FOR VELOCITY PASS
!
               Omega = Twopi*Rz(jlist)
            ENDIF
         ELSE
            Nwdsf = 8
         ENDIF
!
         lentry = Idrec(10)
         I1 = Nwords + 1
         I2 = lentry
!
!
!     SET DISPLACEMENT, VELOCITY, OR ACCELERATION OFP MAJOR ID IF INFILE
!     IS MODAL DISPLACEMENTS.
!
         IF ( Itype1==7 ) Idrec(2) = dvamid(Ipass)
         IF ( .NOT.Trnsnt ) Idrec(2) = Idrec(2) + 1000
!
!     RESET APPROACH CODE FROM EIGENVALUE TO TRANSIENT OR FREQUENCY
!
         iapp = 5
         IF ( Trnsnt ) iapp = 6
         Idrec(1) = 10*iapp + Device
!
!     FILL TITLE, SUBTITLE, AND LABEL FROM CASECC FOR THIS SUBCASE.
!
         DO i = 1 , 96
            Idrec(i+50) = z(Icc+i+37)
         ENDDO
         Idrec(4) = Subcas
         DO
!
!     READ FIRST WORDS OF OUTPUT ENTRY FROM MAP.
!
            CALL read(*160,*100,scrt4,buf,Nwords,noeor,Nwds)
            Lminor = .TRUE.
            IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
               Npos = Savdat(minor)/100
               Nstxtr = Savdat(minor) - Npos*100
               CALL read(*160,*180,scrt4,Bufsav(1),Nstxtr,noeor,Nwds)
               Lminor = .FALSE.
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
            IF ( Trnsnt .AND. Itype1==5 ) CALL ddrmms(buf,Itype2,buf4,buf5)
            IF ( .NOT.(Idout) ) THEN
               Idrec(9) = Form
               Idrec(10) = Nwdsf
               CALL write(Outfil,Idrec,146,eor)
               Idout = .TRUE.
            ENDIF
!
!     OUTPUT THE COMPLETED ENTRY TO OFP OUTFIL.
!
            CALL write(Outfil,buf,Nwdsf,noeor)
         ENDDO
!
!     END OF ENTRIES FOR ONE ID-REC HIT.  IF NO EOF ON MAP WITH
!     NEXT READ, THEN CONTINUE OUTPUT OF THIS SOLUTION COLUMN.
!
 100     CALL write(Outfil,0,0,eor)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON MAP.  THUS START NEXT COLUMN IF REQUIRED.
!
 120     jlist = jlist + 1
         IF ( jlist<=Nlist ) THEN
            CALL rewind(scrt4)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     ALL DATA OF SOLUTION PRODUCT MATRIX HAS NOW BEEN OUTPUT.
!
         CALL close(Outfil,Cls)
         CALL close(Infile,Clsrew)
         CALL close(scrt4,Clsrew)
         CALL close(scrt6,Clsrew)
         Ipass = Ipass + 1
         IF ( Ipass>Passes ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PREPARE FOR ANOTHER PASS
!
         File = Infile
         CALL open(*140,Infile,z(buf1),Rdrew)
         CALL fwdrec(*160,Infile)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
         WRITE (Outpt,99003) Swm , Infile
99003    FORMAT (A27,' 2336.  (DDRMM1-2) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',   &
                &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
         spag_nextblock_1 = 10
      CASE (10)
         Ipass = 3
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
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
