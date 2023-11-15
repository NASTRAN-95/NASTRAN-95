
SUBROUTINE sdr2d
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Acc , App(2) , Bgpdt , Bk0(2) , Bk1(2) , Branch , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Buf6 , Buf7 , Buf8 , Bufa(100) ,    &
         & Bufb(4076) , Casecc , Cei(2) , Comps , Cstm , Date(3) , Device , Displ , Ds0(2) , Ds1(2) , Dtype(8) , Edt , Eigr ,       &
         & Eldef , Elemid , Elesta(100) , Eltype , Eof , Eol , Eqexin , Esta , Estawd , Fdest , File , Flag , Force , Forcex ,      &
         & Formt , Frq(2) , Fsetno , Gptt , I , I2 , Iacc , Icb(7) , Icc , Icore , Icstm , Idef , Idispl , Idload , Idum(2) , Iedt ,&
         & Ieigen , Ieldef , Ielem , Ielf , Iesta , Ifrout , Igptta , Iheat , Ilist , Iloads , Ilsym , Incr2 , Intap , Ipart ,      &
         & Ipcmp , Ipcmp1 , Ipcmp2 , Irecx , Ireqx , Iretrn , Isave , Iseq
   REAL All , Any , Clsrew , Coef , Ddrmm , Deform , Deftmp , Diff , Diff1 , Dit , Dum23(35) , Echo , Elwork(300) , End , Eor ,     &
      & Est , Fn , Gptta , Harms , Ophig , Page , Pphig , Rd , Rdrew , Save , Scr3 , Spcn , Stftmp , Strspt , Symm , Temp , Tgrd(33)&
      & , Tgrid(4) , Tline , Wrt , Wrtrew , Xx(4) , Xycdb , Y(4) , Zz(1)
   LOGICAL Axic , Eofcc
   INTEGER Isetf , Isetnf , Isetno , Isets , Isload , Isopl , Isopl8 , Isorc , Ispcf , Istr , Isymfl , Isymn , Itload , Ittl ,      &
         & Ivec , Ivecn , Ivel , Ix , Ixsetf , Ixsets , Ixx , Iy , J , J2 , Jany , Jforc , Jlist , Jstrs , K , Kcount , Kfrq , Khi ,&
         & Klo , Kn , Knset , Ktype , Ktype1 , Ktypex , Kwdcc , Kwdedt , Kwdest , Kwdgpt , Kx , Line , Loadnn , Loads , Lsym , M ,  &
         & Maxlin , Mcb(7) , Method , Midvec , Mpcn , Mpt , Mset , N , N1 , N2 , Nam(2) , Ncstm , Ndef , Nesta , Ngptt , Nharms ,   &
         & Nlist , Nlogic , Nogo , Notset , Npcmp , Npcmp1 , Npcmp2 , Nrigds , Nrings , Nsetf , Nsets , Nstrop , Nvects , Nwdfor ,  &
         & Nwds , Nwdsa
   INTEGER Nwdstr , Nwords , Nx , Nxsetf , Nxsets , Ocb(7) , Oef1 , Oef1l , Oeigr , Oes1 , Oes1l , Ofile , Opg1 , Opte , Oqg1 ,     &
         & Ougv1 , Outfl , Pcomps , Pg , Phig , Pla(22) , Plots , Pugv1 , Qg , Qtype2 , Rei(2) , Retx , Sdest , Setno , Sil , Sorc ,&
         & Sort2 , Spcf , Sta(2) , Stress , Stresx , Symflg , Sysbuf , Time , Tload , Tloads , Tmprec , Trn(2) , Ugv , Ugvvec ,     &
         & Vel , Xsetnf , Xsetns , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / App , Sort2 , Idum , Comps
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /sdr2c1/ Ipcmp , Npcmp , Ipcmp1 , Npcmp1 , Ipcmp2 , Npcmp2 , Nstrop
   COMMON /sdr2de/ Buf6 , Coef , Deftmp , Diff , Diff1 , Device , Estawd , Elemid , Eltype , Eof , Eofcc , Ireqx , Flag , Fn ,      &
                 & Forcex , Fsetno , Formt , Icc , I , Iedt , Isetno , Isetf , Isets , Idef , Isymn , Sdest , Ix , Isetnf , Iseq ,  &
                 & Iretrn , Irecx , Isave , Fdest , Ipart , Ilist , Igptta , Icore , Ielem , Iesta , Buf8 , Jforc , Jstrs , Jany ,  &
                 & Jlist , J , Ktype1 , Khi , Kx , K , Klo , Kn , Ktypex , Kfrq , Kcount , Lsym , M , Midvec , Nwdsa , Nwdstr ,     &
                 & Nlogic , Nwds , Ndef , N , N1 , N2 , Notset , Nsets , Nsetf , Nwords , Nx , Tgrid , Nwdfor , Ngptt , Nesta ,     &
                 & Nvects , Nlist , Ofile , Outfl , Retx , Setno , Stresx , Save , Tload , Ugvvec , Ixsets , Nxsets , Ixsetf ,      &
                 & Nxsetf , Xsetns , Xsetnf , Sorc , Tmprec , Buf7 , Tgrd
   COMMON /sdr2x1/ Ieigen , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym , Ifrout ,&
                 & Isload , Idload , Isorc
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms , Xycdb , Scr3 , Pcomps , Oes1l ,     &
                 & Oef1l
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm , Isopl8
   COMMON /sdr2x7/ Elesta , Bufa , Bufb
   COMMON /sdr2x8/ Elwork
   COMMON /system/ Sysbuf , Opte , Nogo , Intap , Mpcn , Spcn , Method , Loadnn , Symm , Stftmp , Page , Line , Tline , Maxlin ,    &
                 & Date , Time , Echo , Plots , Dum23 , Iheat
   COMMON /unpakx/ Qtype2 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm
   COMMON /zblpkx/ Y , Iy
   COMMON /zntpkx/ Xx , Ixx , Eol , Eor
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   LOGICAL axcosi , axsine
   INTEGER buf(50) , buf0 , bufm1 , idx , ieqex , iflag , ilayer , index , iskip , isvsrc , isvvcn , isvvec , itr(7) , ixstnf ,     &
         & ixstns , kdefrm(2) , limit , ncc , neqex , nmef1l(2) , nmes1l(2) , pcomp(2) , pcomp1(2) , pcomp2(2) , xset0
   REAL bufr(2) , coef1
   INTEGER korsz
!
! End of declarations
!
!
!     SDR2D PERFORMS THE FINAL STRESS AND FORCE RECOVERY COMPUTATIONS.
!     CASE CONTROL AND THE DISPLACEMENT VECTOR FILE ARE PROCESSED IN
!     PARALLEL.  THE ESTA IS PASSED ONCE FOR EACH VECTOR IN UGV FOR
!     WHICH A STRESS OR FORCE OUTPUT REQUEST EXISTS.  THE ESTA IS HELD
!     COMPLETELY IN CORE IF POSSIBLE.  STRESS OUTPUT IS WRITTEN ON OES1.
!     FORCE OUTPUT IS WRITTEN ON OEF1.
!
!    1,               IDSTRS   ,IDFORC   ,ILOGIC(2)
   EQUIVALENCE (buf(1),bufr(1)) , (Z(1),Zz(1))
!    1,               (IDSTRS,ILOGIC(1)) ,(IDFORC,ILOGIC(2))
   DATA buf/50*0/ , kdefrm/104 , 1/ , xset0/100000000/
   DATA nmes1l/4HOES1 , 4HL   / , nmef1l/4HOEF1 , 4HL   /
   DATA pcomp/5502 , 55/ , pcomp1/5602 , 56/ , pcomp2/5702 , 57/
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
      GOTO 4000
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
      CALL open(*4100,Casecc,Z(Buf1),Rdrew)
      CALL fwdrec(*4200,Casecc)
      Eofcc = .FALSE.
!
      IF ( Branch==1 .OR. Branch==3 .OR. Branch==7 .OR. Branch==10 ) GOTO 500
      IF ( Branch==4 .OR. Branch==8 ) THEN
!
!     DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1 - SKIP 1ST DATA RECORD ON
!     CC.
!
         CALL fwdrec(*4200,Casecc)
         IF ( App(1)/=Bk1(1) ) GOTO 500
      ELSEIF ( Branch==5 .OR. Branch==6 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
         File = Pg
         CALL open(*4100,File,Z(Buf2),Rdrew)
         I = Ilist
         M = 3
         Ix = 1
         IF ( App(1)==Frq(1) .OR. App(1)==Trn(1) ) Ix = 2
         DO
            CALL read(*4200,*400,File,buf(1),M,0,Flag)
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
      CALL open(*4100,Eigr,Z(Buf2),Rdrew)
      CALL fwdrec(*4200,Eigr)
      CALL fwdrec(*4200,Eigr)
      I = Ilist
      M = 8 - Ktype
      iskip = 0
      index = 2
      IF ( App(1)/=Rei(1) ) GOTO 200
      DO
!
!     CHECK TO SEE IF ALL GENERALIZED MASS VALUES ARE ZERO
!
         CALL read(*4200,*100,Eigr,buf,M,0,Flag)
         IF ( buf(6)/=0.0 ) THEN
            index = 0
            EXIT
         ENDIF
      ENDDO
   ENDIF
 100  CALL skprec(Eigr,-1)
 200  DO
      CALL read(*4200,*300,Eigr,buf(1),M,0,Flag)
      IF ( App(1)/=Rei(1) ) EXIT
      IF ( index==2 ) EXIT
!
!     MATCH CORRECT MODE NOS. AND EIGENVALUES WITH PROPER
!     FORCES AND STRESSES WHEN USING GIVENS METHOD WITH F1.GT.0.0
!
      IF ( index==1 ) EXIT
      IF ( buf(6)/=0.0 ) THEN
         index = 1
         EXIT
      ELSE
         iskip = iskip + 1
      ENDIF
   ENDDO
   Z(I) = buf(1) - iskip
   Z(I+1) = buf(3)
   Z(I+2) = buf(4)
   I = I + Ktype1
   GOTO 200
 300  CALL close(Eigr,Clsrew)
   Nlist = I - Ktype1
   Icc = I
   GOTO 500
 400  CALL close(File,Clsrew)
   Nlist = I - Ix
   Icc = I
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
 500  Buf3 = Buf2 - Sysbuf - 1
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
   IF ( Comps/=-1 ) GOTO 900
   File = Pcomps
   N = -1
   CALL preloc(*4400,Z(Buf2),Pcomps)
   Ipcmp = Icc + 1
   Ipcmp1 = Ipcmp
   Ipcmp2 = Ipcmp
   Npcmp = 0
   Npcmp1 = 0
   Npcmp2 = 0
   N = -2
!
   CALL locate(*600,Z(Buf2),pcomp,idx)
   CALL read(*4400,*600,Pcomps,Z(Ipcmp),Buf2-Ipcmp,1,Npcmp)
   CALL mesage(-8,0,Nam)
 600  Ipcmp1 = Ipcmp1 + Npcmp
   Ipcmp2 = Ipcmp1
!
   CALL locate(*700,Z(Buf2),pcomp1,idx)
   CALL read(*4400,*700,Pcomps,Z(Ipcmp1),Buf2-Ipcmp1,1,Npcmp1)
   CALL mesage(-8,0,Nam)
 700  Ipcmp2 = Ipcmp2 + Npcmp1
!
   CALL locate(*800,Z(Buf2),pcomp2,idx)
   CALL read(*4400,*800,Pcomps,Z(Ipcmp2),Buf2-Ipcmp2,1,Npcmp2)
   CALL mesage(-8,0,Nam)
 800  Icc = Ipcmp2 + Npcmp2 - 1
!
   CALL close(Pcomps,Clsrew)
!
!     IF ESTA FITS IN CORE BUF8 MAY BE BUF7 SINCE IT WILL ONLY BE USED
!     TO READ ESTA IN ONCE..
!
 900  Iedt = Icc + Kwdcc + 1
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
   CALL open(*4100,Esta,Z(Buf8),Rdrew)
!
!     REMAINING CORE
!
   Icore = Buf8 - Iesta
   Nesta = 0
!
!     WILL ESTA FIT IN CORE
!
   IF ( Icore<=0 ) CALL mesage(-8,0,Nam)
   IF ( Kwdest>Icore ) GOTO 1300
!
!     ESTA WILL FIT. READ IT IN PLACING A ZERO WORD AT END OF EACH
!     RECORD.
!
   I = Iesta
 1000 CALL read(*1200,*1100,Esta,Z(I),Icore,1,Nwords)
   CALL rewind(Esta)
   Icore = Buf8 - Iesta
   GOTO 1300
 1100 I = I + Nwords + 1
   Z(I-1) = 0
   Icore = Icore - Nwords - 1
   GOTO 1000
!
!     ALL ESTA NOW IN CORE
!
 1200 Nesta = I - 1
   CALL close(Esta,Clsrew)
   IF ( Nesta<=Iesta ) THEN
      WRITE (Opte,99001) Uwm
99001 FORMAT (A25,' 3303, STRESSES OR FORCES REQUESTED FOR SET(S) ','WHICH CONTAIN NO VALID ELEMENTS.')
      GOTO 4000
   ENDIF
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
 1300 File = Ugv
   CALL open(*4100,Ugv,Z(Buf2),Rdrew)
   CALL fwdrec(*4200,Ugv)
!
!     IF ANY ISOPARAMETRIC ELEMENTS PRESENT, GET SECOND RECORD OF EQEXIN
!
   IF ( Isopl/=0 ) THEN
      File = Eqexin
      CALL open(*4100,Eqexin,Z(Buf7),Rdrew)
      CALL fwdrec(*4200,Eqexin)
      CALL fwdrec(*4200,Eqexin)
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
      CALL open(*1400,Oes1,Z(Buf3),Wrtrew)
      CALL fname(Oes1,Ocb)
      DO I = 1 , 3
         Ocb(I+2) = Date(I)
      ENDDO
      Ocb(6) = Time
      Ocb(7) = 1
      CALL write(Oes1,Ocb,7,1)
   ENDIF
   GOTO 1500
 1400 CALL mesage(1,Oes1,Nam)
   Stress = 0
!
!     IF ANY STRESS OR FORCE OUTPUT IS REQUESTED AND COMPOSITE ELEMENTS
!     ARE PRESENT, OPEN OES1L AND OEF1L AND WRITE HEADER RECORDS
!
 1500 IF ( .NOT.(Comps/=-1 .OR. (Stress==0 .AND. Force==0)) ) THEN
      ilayer = 0
      File = Oes1l
      CALL open(*1600,Oes1l,Z(bufm1),Wrtrew)
      CALL write(Oes1l,nmes1l,2,1)
      File = Oef1l
      CALL open(*1600,Oef1l,Z(buf0),Wrtrew)
      CALL write(Oef1l,nmef1l,2,1)
   ENDIF
   GOTO 1700
 1600 CALL mesage(1,File,Nam)
   Stress = 0
   Force = 0
!
!     IF ANY FORCE OUTPUT IS REQUESTED,
!     OPEN OEF1 AND WRITE HEADER RECORD
!
 1700 IF ( Force/=0 ) THEN
      File = Oef1
      CALL open(*1800,Oef1,Z(Buf4),Wrtrew)
      CALL fname(Oef1,Ocb)
      DO I = 1 , 3
         Ocb(I+2) = Date(I)
      ENDDO
      Ocb(6) = Time
      Ocb(7) = 1
      CALL write(Oef1,Ocb,7,1)
   ENDIF
   GOTO 1900
 1800 CALL mesage(1,Oef1,Nam)
   Force = 0
 1900 IF ( Stress==0 .AND. Force==0 ) GOTO 4000
!
!     INITIALIZE UGV VEC, WHICH WILL BE THE NUMBER OF THE VECTOR WE
!     ARE NOW POSITIONED TO READ.
!
   Ugvvec = 1
   isvvec = Ivec
   isvvcn = Ivecn
   iflag = 0
!
!     READ A RECORD IN CASE CONTROL. SET SYMMETRY FLAG.
!
 2000 CALL read(*3800,*2100,Casecc,Z(Icc+1),Kwdcc+1,1,Flag)
   CALL mesage(8,0,Nam)
   GOTO 4000
 2100 Ix = Icc + Isymfl
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
   IF ( Axic .AND. Sorc==0 ) GOTO 3900
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT.
!     IF NOT, TEST FOR RECORD SKIP ON UGV THEN GO TO END OF THIS
!     REQUEST. IF SO, SET POINTERS TO SET DEFINING REQUEST.
!
 2200 Ix = Icc + Istr
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
      DO
         Isets = Isetno + 2
         Nsets = Z(Isetno+1) + Isets - 1
         IF ( Z(Isetno)==Stresx ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR STRESSES
!
            IF ( Stresx<xset0 ) EXIT
            Xsetns = Sdest/10
            Sdest = Sdest - 10*Xsetns
            IF ( Xsetns==0 ) EXIT
            ixstns = Ix + Z(Ix) + 1
            DO
               Ixsets = ixstns + 2
               Nxsets = Z(ixstns+1) + Ixsets - 1
               IF ( Z(ixstns)==Stresx ) GOTO 2300
               ixstns = Nxsets + 1
               IF ( ixstns>=ncc ) THEN
                  Stresx = -1
                  GOTO 2300
               ENDIF
            ENDDO
         ELSE
            Isetno = Nsets + 1
            IF ( Isetno>ncc ) THEN
               Stresx = -1
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
 2300 IF ( Forcex>0 ) THEN
      Ix = Icc + Ilsym
      Isetno = Ix + Z(Ix) + 1
      DO
         Isetf = Isetno + 2
         Nsetf = Z(Isetno+1) + Isetf - 1
         IF ( Z(Isetno)==Forcex ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR FORCES
!
            IF ( Forcex<xset0 ) GOTO 2500
            Xsetnf = Fdest/10
            Fdest = Fdest - 10*Xsetnf
            IF ( Xsetnf==0 ) GOTO 2500
            ixstnf = Ix + Z(Ix) + 1
            DO
               Ixsetf = ixstnf + 2
               Nxsetf = Z(ixstnf+1) + Ixsetf - 1
               IF ( Z(ixstnf)==Forcex ) GOTO 2500
               ixstnf = Nxsetf + 1
               IF ( ixstnf>=ncc ) THEN
                  Forcex = -1
                  GOTO 2400
               ENDIF
            ENDDO
         ELSE
            Isetno = Nsetf + 1
            IF ( Isetno>ncc ) THEN
               Forcex = -1
               GOTO 2500
            ENDIF
         ENDIF
      ENDDO
   ENDIF
 2400 IF ( .NOT.(Stresx/=0 .OR. Forcex/=0 .OR. Axic) ) THEN
!
!     NO REQUESTS THIS CC RECORD FOR STRESSES OR FORCES.
!     THUS SKIP CORRESPONDING UGV RECORD UNLESS SYMFLG IS ON, IN WHICH
!     CASE WE SKIP NO UGV RECORD SINCE THE SYMMETRY CASE HAS NO UGV
!     VECTOR, BUT IN FACT WOULD HAVE USED A SUMMATION OF THE IMMEDIATELY
!     PRECEEDING LSYM VECTORS.
!
!     IF END OF CC AND NO STRESS OR FORCE OUTPUT REQUEST WE ARE DONE
!
      IF ( Eofcc ) GOTO 3900
      IF ( Symflg/=0 ) GOTO 2000
      CALL fwdrec(*4200,Ugv)
      Ugvvec = Ugvvec + 1
      GOTO 3700
   ENDIF
!
!     THERE IS A REQUEST FOR STRESSES AND OR FORCES
!     FIRST DETERMINE APPROPRIATE GPTT AND EDT RECORDS IF REQUIRED
!
 2500 Ix = Icc + Itload
   Tloads = Z(Ix)
   Ngptt = 0
   IF ( Tloads/=0 ) THEN
      File = Gptt
      CALL close(Gptt,Clsrew)
      CALL open(*4100,Gptt,Z(Buf5),Rdrew)
!
!     SKIP NAME
!
      CALL read(*4200,*4300,Gptt,buf,2,0,N)
      DO
!
!     PICK UP 3 WORDS OF SET INFORMATION
!
         CALL read(*4200,*4300,Gptt,buf,3,0,N)
         IF ( buf(1)==Tloads ) THEN
            Deftmp = bufr(2)
            Tmprec = buf(3)
            EXIT
         ENDIF
      ENDDO
   ENDIF
!
   Ix = Icc + Ieldef
   Eldef = Z(Ix)
   IF ( Eldef==0 .OR. Kwdedt==0 ) GOTO 3000
   File = Edt
   CALL preloc(*4100,Z(Buf6),Edt)
   CALL locate(*2600,Z(Buf6),kdefrm,Flag)
   Idef = Iedt
   I = Idef
   DO
      CALL read(*4200,*2600,Edt,buf(1),3,0,Flag)
      IF ( buf(1)==Eldef ) GOTO 2800
   ENDDO
 2600 buf(1) = Eldef
   buf(2) = 0
   CALL mesage(-30,46,buf)
 2700 CALL read(*4200,*2900,Edt,buf(1),3,0,Flag)
   IF ( buf(1)/=Eldef ) GOTO 2900
 2800 Z(I) = buf(2)
   Z(I+1) = buf(3)
   I = I + 2
   IF ( I<Igptta ) GOTO 2700
   CALL mesage(-8,0,Nam)
 2900 Ndef = I - 2
   CALL close(Edt,Clsrew)
!
!     UNPACK VECTOR INTO CORE
!
 3000 coef1 = 1.0
   IF ( Symflg==0 ) GOTO 3300
!
!     SYMMETRY SEQUENCE-- BUILD VECTOR IN CORE.
!
   Ix = Icc + Ilsym
   Lsym = Z(Ix)
!
!     IF SYMFLG IS NEGATIVE, THIS IS A REPEAT SUBCASE.  USE PRESENT
!     VECTOR IN CORE.
!
   IF ( Symflg<0 .AND. App(1)==Sta(1) ) GOTO 3600
   IF ( Symflg<0 ) GOTO 2000
   DO I = Ivec , Ivecn
      Zz(I) = 0.0
   ENDDO
   IF ( Lsym>Ugvvec-1 ) THEN
      Ocb(1) = Lsym
      Ocb(2) = Ugvvec - 1
      CALL mesage(30,92,Ocb(1))
      GOTO 3900
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
 3100 Coef = Zz(I)
   CALL intpk(*3200,Ugv,0,Qtype2,0)
   DO
      CALL zntpki
      Ix = Ivec + Ixx - 1
      IF ( Ktype==1 ) THEN
         Zz(Ix) = Zz(Ix) + Coef*Xx(1)
      ELSE
         Zz(Ix+J2) = Zz(Ix+J2) + Coef*Xx(1)
         Zz(Ix) = Zz(Ix) + Coef*Xx(2)
      ENDIF
      IF ( Eol/=0 ) EXIT
   ENDDO
 3200 IF ( iflag==1 ) THEN
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
      coef1 = Zz(Icc+Ilsym+1)
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
      IF ( I>Isymn ) GOTO 3600
      GOTO 3100
   ENDIF
!
!     NOT SYMMETRY-- UNPACK VECTOR.
!
 3300 J2 = Icb(3)
   IF ( iflag/=1 ) THEN
      IF ( Ugvvec>Nvects ) GOTO 3900
   ENDIF
   DO I = Ivec , Ivecn
      Zz(I) = 0.0
   ENDDO
   CALL intpk(*3400,Ugv,0,Qtype2,0)
   DO
      CALL zntpki
      Ix = Ivec + Ixx - 1
      IF ( Ktype==1 ) THEN
         Zz(Ix) = coef1*Xx(1)
      ELSE
         Zz(Ix) = coef1*Xx(2)
         Zz(Ix+J2) = coef1*Xx(1)
      ENDIF
      IF ( Eol/=0 ) EXIT
   ENDDO
 3400 IF ( App(1)==Trn(1) ) THEN
      CALL fwdrec(*3500,Ugv)
      Ugvvec = Ugvvec + 1
      CALL fwdrec(*3500,Ugv)
      Ugvvec = Ugvvec + 1
   ENDIF
 3500 IF ( iflag/=1 ) Ugvvec = Ugvvec + 1
   IF ( iflag==1 ) CALL skprec(Ugv,1)
!
!     READY NOW TO SWEEP THROUGH THE ESTA ONCE.
!     SDR2E DOES ALL THE PROCESSING OF PHASE II ELEMENT COMPUTATIONS.
!     THE ESTA FILE, BE IT IN CORE OR NOT, IS SWEPT THRU ONCE FOR THE
!     FOLLOWING CALL.
!
 3600 IF ( iflag==1 ) Sorc = Sorc + 1
   IF ( Sorc==3 ) Sorc = 1
   CALL sdr2e(*4000,ieqex,neqex)
!
!     CONCLUDE PROCESSING OF THIS VECTOR
!     INITIALIZE FOR NEXT VECTOR
!     CANCEL THIS INITIALIZATION IN SOME CASES IF A REPEAT CASE.
!
 3700 IF ( Branch==2 .OR. Branch==4 .OR. Branch==8 .OR. Branch==9 ) THEN
      Jlist = Jlist + Ktype1
      IF ( .NOT.Eofcc ) GOTO 2000
   ELSEIF ( Branch==3 .OR. Branch==7 ) THEN
      GOTO 3900
   ELSEIF ( Branch==5 ) THEN
!
!     FREQUENCY RESPONSE, PICK UP NEXT VECTOR UNLESS ALL FREQUENCIES
!     COMPLETED
!
      Jlist = Jlist + 2
      IF ( Jlist<=Nlist .AND. Ugvvec<=Nvects ) GOTO 2200
      Kfrq = 0
      Jlist = Ilist
      DO I = Ilist , Nlist , 2
         Z(I+1) = 0
      ENDDO
      IF ( Ugvvec>Nvects ) GOTO 3900
      GOTO 2000
   ELSEIF ( Branch==6 ) THEN
!
!     TRANSIENT RESPONSE
!
      Jlist = Jlist + 2
      IF ( Jlist<=Nlist .AND. .NOT.Eofcc ) GOTO 2000
      IF ( Jlist<=Nlist .AND. Ugvvec<=Nvects ) GOTO 3300
      GOTO 3900
!
   ELSEIF ( .NOT.Eofcc ) THEN
      GOTO 2000
   ENDIF
!
!     PROCESS ANY REMAINING VECTORS WITH LAST CC RECORD
!
   IF ( Ugvvec>Nvects .OR. Symflg/=0 ) GOTO 3900
   GOTO 2200
!
!     EOF HIT ON CASECC FILE
!     PROCESS ANY MORE VECTORS USING LAST CASECC RECORD
!
 3800 Eofcc = .TRUE.
   IF ( Nvects>=Ugvvec ) GOTO 2200
!
!     WRITE TRAILERS AND CLOSE ANY OPEN FILES
!
 3900 Ocb(2) = 63
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
 4000 DO I = 1 , 12
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
 4100 N = 1
   GOTO 4400
 4200 N = 2
   GOTO 4400
 4300 N = 3
 4400 CALL mesage(N,File,Nam)
   GOTO 4000
END SUBROUTINE sdr2d
