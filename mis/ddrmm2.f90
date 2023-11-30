
SUBROUTINE ddrmm2(*,*,*,*)
   IMPLICIT NONE
   REAL A(4) , Aout(4) , Lambda , Omega , Pi , Rbuf(150) , Ridrec(1) , Rz(1) , Twopi
   INTEGER Buf(150) , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Buf6 , Bufa(75) , Bufb(75) , Buff(6) , Bufsav(10) , Cls , Clsrew ,         &
         & Complx(1) , Device , Dhsize , Elem(1) , Entrys , File , Filnam(2) , Form , I1 , I2 , Ia(4) , Icc , Idrec(146) , Ieol ,   &
         & Ieor , Ierror , Ilist , Incr , Infile , Ipass , Iprec , Irow , Irowo , Iscrt , Isinab , Isinc , Istlst , Itemp , Itflag ,&
         & Itype1 , Itype2 , Jfile , Last , Lsf , Lstlst , Lz , Mcb(7) , Mcba(7) , Mcbb(7) , Mcbc(7) , Mcbd(7) , Ncc , Ncore ,      &
         & Nelem , Nlambs , Nlist , Npos , Nptsf , Nsols , Nstxtr , Nwds , Nwdsf , Nwords , Outfil , Outpt , Passes , Phase , Rd ,  &
         & Rdrew , Savdat(75) , Savpos(25) , Scrt(7) , Scrt1 , Scrt2 , Scrt3 , Scrt4 , Scrt5
   LOGICAL Col1 , Frstid , Idout , Lminor , Sort2 , Trnsnt
   INTEGER Scrt6 , Scrt7 , Setid , Sets(5,3) , Subcas , Sysbuf , Uvsol , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /clstrs/ Complx
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
   LOGICAL anyxy
   INTEGER dvamid(3) , elwork(300) , eor , i , iapp , id , idata , idvice , ielem , ij , insuf , iomega , iout , ixy , ixytyp , j , &
         & jdata , jlist , jp , jwords , jxy , k , kdata , klist , kxy , l , lentry , lxy , major , minor , ncols , ndata , next ,  &
         & noeor , nomega , npt , numwds , nxy , nxy1 , typout
   INTEGER korsz , numtyp
!
!     PERFORMS SORT2 TYPE PROCESSING FOR MODULE DDRMM.
!
   !>>>>EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2)) , (Scrt3,Scrt(3)) , (Scrt4,Scrt(4)) , (Scrt5,Scrt(5)) , (Scrt6,Scrt(6)) ,          &
!>>>>    & (Scrt7,Scrt(7)) , (Buf1,Buff(1)) , (Buf2,Buff(2)) , (Buf3,Buff(3)) , (Buf4,Buff(4)) , (Buf5,Buff(5)) , (Buf6,Buff(6)) ,       &
!>>>>    & (A(1),Ia(1)) , (Z(1),Rz(1)) , (Buf(1),Rbuf(1),Bufa(1)) , (Bufb(1),Buf(76)) , (Idrec(1),Ridrec(1))
!
   DATA eor , noeor/1 , 0/ , dvamid/2001 , 2010 , 2011/
!
!     FORMATION OF DATA-MATRIX AND SUBSEQUENT MULTIPLICATION BY SAME OF
!     THE SOLUTION MATRIX (TRNNSPOSED), AND ULTIMATE OUTPUT OF TRANSIENT
!     OR FREQUENCY SOLUTIONS.
!
   Ipass = 1
   iomega = Nlist + 1
   nomega = iomega - 1
   minor = 0
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
   ixy = nomega + 1
   CALL ddrmmp(*1400,Z(ixy),Buf3-ixy,lxy,ixytyp,Subcas,Z(Buf3),anyxy)
   IF ( .NOT.anyxy .AND. Setid==0 ) GOTO 1000
   nxy = ixy + lxy - 1
   Ierror = 23
   File = Scrt4
   CALL open(*1100,Scrt4,Z(Buf3),Wrtrew)
   File = Scrt5
   CALL open(*1100,Scrt5,Z(Buf2),Wrtrew)
   CALL fname(Scrt5,Filnam)
   CALL write(Scrt5,Filnam,2,eor)
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
   Mcb(1) = Scrt5
   Mcb(2) = 0
   Mcb(3) = Nlambs
   Mcb(4) = 2
   Mcb(5) = 1
   Mcb(6) = 0
   Mcb(7) = 0
   IF ( .NOT.(Ipass==1 .AND. Frstid) ) CALL read(*700,*700,Infile,Idrec,146,eor,Nwds)
!
!     OFP-ID RECORD IS WRITTEN TO THE MAP FILE ONLY ON CHANGE OF
!     MINOR ID.
!
 200  major = mod(Idrec(2),1000)
   IF ( major/=Itype1 ) THEN
!
!     CHANGE IN MAJOR OFP-ID DETECTED ON -INFILE-.
!
      WRITE (Outpt,99001) Swm , Infile
99001 FORMAT (A27,' 2339.  (DDRMM2-1) A CHANGE IN WORD 2 OF THE OFP-ID',' RECORDS OF DATA BLOCK',I5,/5X,'HAS BEEN DETECTED. ',      &
             &' POOCESSING OF THIS DATA BLOCK HAS BEEN TERMINATED.')
      Ipass = 3
      GOTO 1000
   ELSE
      idvice = Device
      id = Idrec(5)/10
      IF ( Setid<0 ) THEN
      ELSEIF ( Setid==0 ) THEN
         GOTO 300
      ELSE
         next = 1
         CALL setfnd(*300,Z(Istlst),Lstlst,id,next)
      ENDIF
      GOTO 500
   ENDIF
 300  IF ( anyxy ) THEN
      CALL bisloc(*400,id,Z(ixy),1,lxy,jp)
      idvice = 0
      GOTO 500
   ENDIF
!
!     ID IS NOT TO BE OUTPUT THUS SKIP UPCOMING OFP-DATA-RECORD.
!
 400  CALL fwdrec(*1200,Infile)
   CALL read(*700,*700,Infile,Idrec,146,eor,Nwds)
   GOTO 200
!
!     ID IS TO BE OUTPUT THUS CONTINUE.
!
 500  numwds = Nlambs*Idrec(10)
   idata = nxy + 1
   ndata = idata + numwds - 1
   IF ( ndata<Buf3 ) THEN
      IF ( .NOT.Frstid ) THEN
         IF ( Idrec(3)==minor ) GOTO 600
!
!     CHANGE IN MINOR ID, I.E. NEW ELEMENT TYPE.  COMPLETE CURRENT
!     RECORD OF MAP AND OUTPUT ANOTHER ID-RECORD.
!
         CALL write(Scrt4,0,0,eor)
      ELSE
!
!     VERY FIRST ID RECORD,  THUS SET MINOR ID.
!
         Frstid = .FALSE.
      ENDIF
      CALL write(Scrt4,Idrec,146,eor)
      minor = Idrec(3)
   ELSE
!
!     INSUFFICIENT CORE
!
      insuf = ndata - Buf3
      WRITE (Outpt,99002) Uwm , Infile , insuf
99002 FORMAT (A25,' 2337.  (DDRMM2-2)  DATA BLOCK',I5,' CAN NOT BE ','PROCESSED DUE TO',/5X,'A CORE INSUFFICIENCY OF APPROXI',      &
             &'MATELY',I11,' DECIMAL WORDS.')
      Ipass = 3
      GOTO 1000
   ENDIF
!
!     SAME TYPE OF DATA THUS CONTINUE ON.
!
 600  lentry = Idrec(10)
   I1 = Nwords + 1
   I2 = lentry
!
!     READ AND OUTPUT ONE FULL OFP-DATA RECORD.
!
   CALL read(*1200,*1300,Infile,Z(idata),numwds,eor,Nwds)
   DO i = I1 , I2
!
!     START NEW COLUMN
!
      CALL bldpk(1,1,Scrt5,0,0)
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
         IF ( numtyp(Ia(1))<=1 ) A(1) = 0.0
!
         CALL zblpki
      ENDDO
!
!     COMPLETE COLUMN
!
      CALL bldpkn(Scrt5,0,Mcb)
   ENDDO
!
!     OUTPUT TO MAP THE ID PLUS ANY OTHER DATA NECESSARY.
!
   Buf(1) = 10*id + idvice
   IF ( Nwords==2 ) Buf(2) = Z(idata+1)
   Nstxtr = 0
   IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
      Npos = Savdat(minor)/100
      Nstxtr = Savdat(minor) - Npos*100
      DO i = 1 , Nstxtr
         j = Savpos(Npos+i-1)
         Buf(i+1) = Z(idata+j-1)
      ENDDO
   ENDIF
   CALL write(Scrt4,Buf,Nwords+Nstxtr,noeor)
!
!     GO FOR NEXT ID.
!
   CALL read(*700,*700,Infile,Idrec,146,eor,Nwds)
   GOTO 200
!
!     END OF FILE ON INFILE.  MAP AND DATA MATRIX NOW COMPLETE.
!
 700  CALL wrttrl(Mcb)
   CALL close(Scrt5,Clsrew)
   CALL close(Infile,Clsrew)
   CALL write(Scrt4,0,0,eor)
   CALL close(Scrt4,Clsrew)
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
   Mcbb(1) = Scrt5
   CALL rdtrl(Mcbb)
   Mcbc(1) = 0
   Mcbd(1) = Scrt6
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
   Lz = korsz(Z(nxy1))
   Isinab = 1
   Isinc = 1
   Iprec = 1
   Iscrt = Scrt7
   CALL mpyad(Z(nxy1),Z(nxy1),Z(nxy1))
   Mcbd(1) = Scrt6
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
   CALL open(*1100,Outfil,Z(Buf1),Wrt)
   File = Scrt4
   CALL open(*1100,Scrt4,Z(Buf2),Rdrew)
   File = Scrt6
   CALL open(*1100,Scrt6,Z(Buf3),Rdrew)
   CALL fwdrec(*1200,Scrt6)
!
!     READ AN OFP-ID-RECORD FROM THE MAP, AND ALLOCATE SPACE NEEDED
!     FOR SOLUTION DATA.
!
   File = Scrt4
 800  CALL read(*1000,*1300,Scrt4,Idrec,146,eor,Nwds)
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
            Z(kxy) = Z(jxy)
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
      Idrec(i+50) = Z(Icc+i+37)
   ENDDO
   Idrec(4) = Subcas
!
!     CHECK FOR SUFFICIENT CORE.
!
   IF ( ndata>=Buf3 ) THEN
      insuf = ndata - Buf3
      WRITE (Outpt,99003) Uwm , Outfil , insuf
99003 FORMAT (A25,' 2338.  (DDRMM2-3)  DATA BLOCK',I5,' MAY NOT BE FULLY COMPLETED DUE TO A CORE INSUFFICIENCY',/5X,                &
             &'OF APPROXIMATELY',I11,' DECIMAL WORDS.')
      Ipass = 3
      GOTO 1000
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
      CALL read(*1200,*800,Scrt4,Buf,Nwords,noeor,Nwds)
      Lminor = .TRUE.
      IF ( Itype1==5 .AND. Savdat(minor)/=0 ) THEN
         Npos = Savdat(minor)/100
         Nstxtr = Savdat(minor) - Npos*100
         CALL read(*1200,*1300,Scrt4,Bufsav(1),Nstxtr,noeor,Nwds)
         Lminor = .FALSE.
      ENDIF
!
!     PREPARE AND OUTPUT THE OFP-ID-RECORD AFTER FIRST ENTRY IS COMBINED
!     AS IN THE CASE OF A FREQUENCY COMPLEX PROBLEM.
!
      Idout = .FALSE.
      Idrec(5) = Buf(1)
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
         Z(i) = 0
      ENDDO
!
!     UNPACK NOW-ZERO TERMS.
!
      jdata = idata - lentry
      DO i = 1 , ncols
         CALL intpk(*840,Scrt6,0,typout,0)
 820     DO
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
                  IF ( Ieol>0 ) GOTO 840
                  CYCLE
               ELSEIF ( Ipass==2 ) THEN
!
!     VELOCITIES  (FREQ RESPONSE)
!
                  klist = iomega + Irowo - 1
                  Rz(Itemp) = -Rz(klist)*Aout(2)
                  Itemp = Itemp + ncols
                  Rz(Itemp) = Rz(klist)*Aout(1)
                  IF ( Ieol>0 ) GOTO 840
                  CYCLE
               ELSEIF ( Ipass==3 ) THEN
!
!     ACCELERATIONS (FREQ RESPONSE)
!
                  klist = iomega + Irowo - 1
                  Rz(Itemp) = Rz(klist)*Aout(1)
                  Itemp = Itemp + ncols
                  Rz(Itemp) = Rz(klist)*Aout(2)
                  IF ( Ieol>0 ) GOTO 840
                  CYCLE
               ENDIF
            ENDIF
            EXIT
         ENDDO
!
!     TRANSIENT OUTPUTS
!
         Rz(Itemp) = Aout(1)
         IF ( Ieol<=0 ) GOTO 820
 840     jdata = jdata + 1
      ENDDO
!
!     OUTPUT LINES OF DATA COMBINING THEM FOR COMPLEX REAL/IMAGINARY OR
!     MAG/PHASE OFP FORMATS IF NECESSARY.
!
      jlist = Ilist - 1
      DO i = idata , ndata , lentry
         jwords = Nwords
         ij = i + ncols - 1
         DO j = i , ij
            jwords = jwords + 1
            Buf(jwords) = Z(j)
            IF ( .NOT.(Trnsnt) ) THEN
               Itemp = j + ncols
               Buf(jwords+75) = Z(Itemp)
            ENDIF
         ENDDO
!
!     IF TRANSIENT, ENTRY IS NOW READY FOR OUTPUT.
!
         IF ( Trnsnt ) THEN
            IF ( .NOT.(Lminor) ) THEN
               DO k = 1 , Nstxtr
                  j = Savpos(Npos+k-1)
                  Buf(j) = Bufsav(k)
               ENDDO
            ENDIF
            GOTO 920
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
                  Buf(j) = Bufsav(k)
               ENDDO
            ENDIF
         ELSE
!
!     POINT DATA
!
            DO k = 3 , 8
               IF ( Form==3 ) CALL magpha(Bufa(k),Bufb(k))
               Bufa(k+6) = Bufb(k)
            ENDDO
            jwords = 14
            GOTO 920
         ENDIF
 860     npt = Complx(l)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( Form/=3 ) GOTO 900
!
!     COMPUTE MAGNITUDE/PHASE
!
            CALL magpha(Bufa(npt),Bufb(npt))
         ELSEIF ( npt==0 ) THEN
!
!     MOVE OUTPUT DATA
!
            DO l = 1 , iout
               Buf(l) = elwork(l)
            ENDDO
            jwords = iout
            GOTO 920
         ELSE
            GOTO 900
         ENDIF
 880     iout = iout + 1
         elwork(iout) = Bufa(npt)
         l = l + 1
         GOTO 860
 900     IF ( npt<=Lsf ) GOTO 880
         npt = npt - Lsf
         iout = iout + 1
         elwork(iout) = Bufb(npt)
         l = l + 1
         GOTO 860
!
!     CALL DDRMMS TO RECOMPUTE SOME ELEMENT STRESS QUANTITIES
!     IN TRANSIENT PROBLEMS ONLY.
!
 920     IF ( Trnsnt .AND. Itype1==5 ) CALL ddrmms(Buf,Idrec(3),Buf4,Buf5)
         IF ( .NOT.(Idout) ) THEN
            Idrec(9) = Form
            Idrec(10) = jwords
            CALL write(Outfil,Idrec,146,eor)
            Idout = .TRUE.
         ENDIF
         jlist = jlist + 1
         Rbuf(1) = Rz(jlist)
         CALL write(Outfil,Buf,jwords,noeor)
      ENDDO
!
!     GO FOR NEXT OUTPUT ID
!
      CALL write(Outfil,0,0,eor)
   ENDDO
!
!  END OF DATA ON MAP FILE (SCRT4).
!
 1000 CALL close(Outfil,Cls)
   CALL close(Infile,Clsrew)
   CALL close(Scrt4,Clsrew)
   CALL close(Scrt6,Clsrew)
   Ipass = Ipass + 1
   IF ( Ipass>Passes ) THEN
      RETURN
   ELSE
!
!     PREPARE FOR ANOTHER PASS
!
      File = Infile
      CALL open(*1100,Infile,Z(Buf1),Rdrew)
      CALL fwdrec(*1200,Infile)
      GOTO 100
   ENDIF
!
!     UNDEFINED FILE.
!
 1100 RETURN 1
!
!     END OF FILE
!
 1200 RETURN 2
!
!     END OF RECORD.
!
 1300 RETURN 3
!
!     INSUFFICIENT CORE
!
 1400 RETURN 4
END SUBROUTINE ddrmm2