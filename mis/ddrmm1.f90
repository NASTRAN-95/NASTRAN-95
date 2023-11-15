
SUBROUTINE ddrmm1(*,*,*,*)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Aout(4) , Lambda , Omega , Pi , Rbuf(150) , Ridrec(6) , Rz(1) , Twopi
   INTEGER Buf(150) , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Buf6 , Buff(6) , Bufsav(10) , Cls , Clsrew , Device , Dhsize , Elem(1) ,   &
         & Entrys , File , Filnam(2) , Form , I1 , I2 , Ia(4) , Icc , Idrec(146) , Ieol , Ieor , Ierror , Ilist , Incr , Infile ,   &
         & Ipass , Iprec , Irow , Irowo , Iscrt , Isinab , Isinc , Istlst , Itemp , Itflag , Itype1 , Itype2 , Jfile , Last , Lsf , &
         & Lstlst , Lz , Mcb(7) , Mcba(7) , Mcbb(7) , Mcbc(7) , Mcbd(7) , Ncc , Ncore , Nelem , Nlambs , Nlist , Npos , Nptsf ,     &
         & Nsols , Nstxtr , Nwds , Nwdsf , Nwords , Outfil , Outpt , Passes , Phase , Rd , Rdrew , Savdat(75) , Savpos(25) , Scrt(7)&
         & , Scrt1 , Scrt2 , Scrt3 , Scrt4 , Scrt5 , Scrt6 , Scrt7 , Setid
   LOGICAL Col1 , Frstid , Idout , Lminor , Sort2 , Trnsnt
   INTEGER Sets(5,3) , Subcas , Sysbuf , Uvsol , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /condas/ Pi , Twopi
   COMMON /ddrmc1/ Idrec , Buff , Passes , Outfil , Jfile , Mcb , Entrys , Sets , Infile , Lambda , File , Sort2 , Col1 , Frstid ,  &
                 & Ncore , Nsols , Dhsize , Filnam , Rbuf , Idout , Icc , Ncc , Ilist , Nlist , Nwds , Setid , Trnsnt , I1 , I2 ,   &
                 & Phase , Itype1 , Itype2 , Nptsf , Lsf , Nwdsf , Scrt , Ierror , Itemp , Device , Form , Istlst , Lstlst , Uvsol ,&
                 & Nlambs , Nwords , Omega , Ipass , Subcas
   COMMON /gpta1 / Nelem , Last , Incr , Elem
   COMMON /mpyadx/ Mcba , Mcbb , Mcbc , Mcbd , Lz , Itflag , Isinab , Isinc , Iprec , Iscrt
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /stdata/ Lminor , Nstxtr , Npos , Savdat , Savpos , Bufsav
   COMMON /system/ Sysbuf , Outpt
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zblpkx/ A , Irow
   COMMON /zntpkx/ Aout , Irowo , Ieol , Ieor
   COMMON /zzzzzz/ Rz
!
! Local variable declarations
!
   LOGICAL anyxy
   INTEGER dvamid(3) , eor , i , iapp , id , idvice , ielem , irow1 , ixy , ixytyp , j , jlist , jp , lentry , lxy , majid , minor ,&
         & next , noeor , nxy , nxy1
   INTEGER korsz , numtyp
!
! End of declarations
!
!
!     PERFORMS SORT1 TYPE PROCESSING FOR MODULE DDRMM.
!
   EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2)) , (Scrt3,Scrt(3)) , (Scrt4,Scrt(4)) , (Scrt5,Scrt(5)) , (Scrt6,Scrt(6)) ,          &
    & (Scrt7,Scrt(7)) , (Buf1,Buff(1)) , (Buf2,Buff(2)) , (Buf3,Buff(3)) , (Buf4,Buff(4)) , (Buf5,Buff(5)) , (Buf6,Buff(6)) ,       &
    & (A(1),Ia(1)) , (Z(1),Rz(1)) , (Idrec(1),Ridrec(1)) , (Buf(1),Rbuf(1))
   DATA eor , noeor/1 , 0/ , dvamid/1 , 10 , 11/
!
!     FORMATION OF DATA-MATRIX AND SUBSEQUENT MULTIPLY BY SOLUTION-
!     MATRIX AND ULTIMATE OUTPUT OF TRANSIENT OR FREQUENCY SOLUTIONS.
!
   Ipass = 1
 100  Col1 = .TRUE.
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
   CALL ddrmmp(*2000,Z(ixy),Buf3-ixy,lxy,ixytyp,Subcas,Z(Buf3),anyxy)
   IF ( .NOT.anyxy .AND. Setid==0 ) GOTO 1300
   nxy = ixy + lxy - 1
!
!     INITIALIZE DATA MATRIX FILE(SCRT5), AND MAPPING TABLE FILE(SCRT4).
!
   Ierror = 22
   File = Scrt4
   CALL open(*1700,Scrt4,Z(Buf3),Wrtrew)
   File = Scrt5
   CALL open(*1700,Scrt5,Z(Buf2),Wrtrew)
   CALL fname(Scrt5,Filnam)
   CALL write(Scrt5,Filnam,2,eor)
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
   Mcb(1) = Scrt5
   Mcb(2) = 0
   Mcb(3) = 0
   Mcb(4) = 2
   Mcb(5) = 1
   Mcb(6) = 0
   Mcb(7) = 0
   IF ( Ipass==1 .AND. Frstid ) GOTO 300
 200  CALL read(*800,*800,Infile,Idrec,146,eor,Nwds)
   majid = mod(Idrec(2),1000)
   IF ( majid/=Itype1 ) GOTO 1400
!
!     IF FIRST COLUMN, OFP-ID RECORD IS WRITTEN AS IS TO MAP FILE.
!
 300  IF ( Col1 ) THEN
      IF ( .NOT.(.NOT.Frstid .AND. Ridrec(6)/=Lambda) ) CALL write(Scrt4,Idrec,146,eor)
   ENDIF
   lentry = Idrec(10)
   I1 = Nwords + 1
   I2 = lentry
   minor = Idrec(3)
!
!     IF SAME EIGENVALUE AS THAT OF LAST OFP-ID RECORD THEN CONTINUE.
!
   IF ( .NOT.(Frstid) ) THEN
      IF ( Ridrec(6)==Lambda ) GOTO 400
!
!     NEW EIGENVALUE. COMPLETE CURRENT DATA MATRIX COLUMN AND START
!     NEW COLUMN. PASS ONE IS NOW COMPLETE.
!
      CALL bldpkn(Scrt5,0,Mcb)
      IF ( Col1 ) irow1 = Irow
      IF ( Irow/=irow1 ) THEN
!
!     DATA INCONSISTENCY ON -INFILE-.
!
         WRITE (Outpt,99001) Swm , Infile
99001    FORMAT (A27,' 2335.  (DDRMM1-1) THE AMOUNT OF DATA IS NOT ','CONSISTENT FOR EACH EIGENVALUE IN DATA BLOCK',I5,/5X,         &
                &'PROCESSING OF THIS DATA BLOCK TERMINATED.')
         GOTO 1500
      ELSE
         Col1 = .FALSE.
      ENDIF
   ENDIF
!
!     START NEW COLUMN.
!
   CALL bldpk(1,1,Scrt5,0,0)
   Irow = 0
   Frstid = .FALSE.
   Lambda = Ridrec(6)
!
!     READ A POINT OR ELEMENT ENTRY.
!
 400  CALL read(*1800,*700,Infile,Buf,lentry,noeor,Nwds)
   id = Buf(1)/10
!
!     CHECK FOR ID IN OUTPUT REQUEST LIST
!
   idvice = Device
   IF ( Setid<0 ) THEN
   ELSEIF ( Setid==0 ) THEN
      GOTO 500
   ELSE
!
!//// NEXT MAY NOT NEED TO BE INITIALIZED EVERY TIME.
!
      next = 1
      CALL setfnd(*500,Z(Istlst),Lstlst,id,next)
   ENDIF
   GOTO 600
 500  IF ( .NOT.anyxy ) GOTO 400
   CALL bisloc(*400,id,Z(ixy),1,lxy,jp)
   idvice = 0
!
!     THIS ID IS TO BE OUTPUT.
!
 600  IF ( Col1 ) THEN
      Buf(1) = 10*id + idvice
      CALL write(Scrt4,Buf(1),Nwords,noeor)
      Nstxtr = 0
      IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
         Npos = Savdat(minor)/100
         Nstxtr = Savdat(minor) - Npos*100
         DO i = 1 , Nstxtr
            j = Savpos(Npos+i-1)
            Bufsav(i) = Buf(j)
         ENDDO
         CALL write(Scrt4,Bufsav(1),Nstxtr,noeor)
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
      IF ( numtyp(Ia(1))<=1 ) A(1) = 0.0
!
      CALL zblpki
   ENDDO
   GOTO 400
!
!     END OF CURRENT OFP-DATA RECORD ENCOUNTERED.
!     IF NEXT OFP-ID-RECORD INDICATES ANOTHER OFP-DATA RECORD FOR
!     THIS SAME EIGENVALUE (I.E. A CHANGE IN ELEMENT TYPE) THEN
!     FURTHER CONSTRUCTION OF THE DATA MATRIX COLUMN TAKES PLACE.
!
 700  IF ( Col1 ) CALL write(Scrt4,0,0,eor)
   GOTO 200
!
!     END OF FILE ENCOUNTERED ON INFILE.
!     DATA MATRIX AND MAPING FILE ARE COMPLETE.
!
 800  CALL close(Infile,Clsrew)
   CALL close(Scrt4,Clsrew)
!
!     COMPLETE LAST COLUMN OF DATA MATRIX WRITTEN.
!
   IF ( Col1 ) irow1 = Irow
   IF ( Irow/=irow1 ) GOTO 1400
   CALL bldpkn(Scrt5,0,Mcb)
   Mcb(3) = Irow
   CALL wrttrl(Mcb)
   CALL close(Scrt5,Clsrew)
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
   Mcba(1) = Scrt5
   CALL rdtrl(Mcba)
   Mcbb(1) = Uvsol
   IF ( Trnsnt ) Mcbb(1) = Scrt(Ipass)
   CALL rdtrl(Mcbb)
   Mcbc(1) = 0
   Mcbd(1) = Scrt6
   Mcbd(2) = 0
   Mcbd(3) = Irow
   Mcbd(4) = 2
   Mcbd(5) = 1
   Mcbd(6) = 0
   Mcbd(7) = 0
   IF ( .NOT.Trnsnt ) Mcbd(5) = 3
   nxy1 = nxy + 1
   IF ( mod(nxy1,2)==0 ) nxy1 = nxy1 + 1
   Lz = korsz(Z(nxy1))
   Itflag = 0
   Isinab = 1
   Isinc = 1
   Iprec = 1
   Iscrt = Scrt7
   CALL mpyad(Z(nxy1),Z(nxy1),Z(nxy1))
   Mcbd(1) = Scrt6
   CALL wrttrl(Mcbd)
!
!     PRODUCT MATRIX IS NOW OUTPUT, USING THE MAP ON SCRT4 FOR EACH
!     COLUMN.  (SORT-1)  PRODUCT MATRIX IS ON SCRATCH DATA BLOCK 6.
!
   Ierror = 10
   File = Outfil
   CALL open(*1700,Outfil,Z(Buf1),Wrt)
   File = Scrt4
   CALL open(*1700,Scrt4,Z(Buf2),Rdrew)
   File = Scrt6
   CALL open(*1700,Scrt6,Z(Buf3),Rdrew)
   CALL fwdrec(*1800,Scrt6)
   jlist = Ilist
!
!     LOOP ON COLUMNS OF SCRT6.
!
 900  CALL ddrmma(.TRUE.)
!
!     READ AN OFP-ID-RECORD FROM THE MAP.
!
   File = Scrt4
 1000 CALL read(*1200,*1900,Scrt4,Idrec,146,eor,Nwds)
!
!     SET THE FREQUENCY OR TIME AND CLOBBER THE EIGENVALUE.
!
   Ridrec(5) = Rz(jlist)
   Ridrec(6) = 0.0
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
99002    FORMAT (A27,' 2334.  (DDRMM-3) ILLEGAL MAJOR OR MINOR OFP-ID ','IDENTIFICATIONS =',2I10,/5X,'DETECTED IN DATA BLOCK',I5,   &
                &'. PROCESSING OF SAID DATA BLOCK DISCONTINUED.')
         GOTO 1600
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
      Idrec(i+50) = Z(Icc+i+37)
   ENDDO
   Idrec(4) = Subcas
   DO
!
!     READ FIRST WORDS OF OUTPUT ENTRY FROM MAP.
!
      CALL read(*1800,*1100,Scrt4,Buf,Nwords,noeor,Nwds)
      Lminor = .TRUE.
      IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
         Npos = Savdat(minor)/100
         Nstxtr = Savdat(minor) - Npos*100
         CALL read(*1800,*1900,Scrt4,Bufsav(1),Nstxtr,noeor,Nwds)
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
      IF ( Trnsnt .AND. Itype1==5 ) CALL ddrmms(Buf,Itype2,Buf4,Buf5)
      IF ( .NOT.(Idout) ) THEN
         Idrec(9) = Form
         Idrec(10) = Nwdsf
         CALL write(Outfil,Idrec,146,eor)
         Idout = .TRUE.
      ENDIF
!
!     OUTPUT THE COMPLETED ENTRY TO OFP OUTFIL.
!
      CALL write(Outfil,Buf,Nwdsf,noeor)
   ENDDO
!
!     END OF ENTRIES FOR ONE ID-REC HIT.  IF NO EOF ON MAP WITH
!     NEXT READ, THEN CONTINUE OUTPUT OF THIS SOLUTION COLUMN.
!
 1100 CALL write(Outfil,0,0,eor)
   GOTO 1000
!
!     END OF FILE ON MAP.  THUS START NEXT COLUMN IF REQUIRED.
!
 1200 jlist = jlist + 1
   IF ( jlist<=Nlist ) THEN
      CALL rewind(Scrt4)
      GOTO 900
   ENDIF
!
!     ALL DATA OF SOLUTION PRODUCT MATRIX HAS NOW BEEN OUTPUT.
!
 1300 CALL close(Outfil,Cls)
   CALL close(Infile,Clsrew)
   CALL close(Scrt4,Clsrew)
   CALL close(Scrt6,Clsrew)
   Ipass = Ipass + 1
   IF ( Ipass>Passes ) GOTO 1600
!
!     PREPARE FOR ANOTHER PASS
!
   File = Infile
   CALL open(*1700,Infile,Z(Buf1),Rdrew)
   CALL fwdrec(*1800,Infile)
   GOTO 100
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
 1400 WRITE (Outpt,99003) Swm , Infile
99003 FORMAT (A27,' 2336.  (DDRMM1-2) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',      &
             &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
 1500 Ipass = 3
   GOTO 1300
!
!     COMPLETION OF PASS FOR INPUT MODAL SOLUTION -FILE-.
!
 1600 RETURN
!
!     UNDEFINED FILE.
!
 1700 RETURN 1
!
!     END OF FILE HIT.
!
 1800 RETURN 2
!
!     END OF RECORD HIT.
!
 1900 RETURN 3
!
!     INSUFFICIENT CORE.
!
 2000 RETURN 4
END SUBROUTINE ddrmm1
