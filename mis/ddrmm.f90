
SUBROUTINE ddrmm
   IMPLICIT NONE
   INTEGER Buf(150) , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Buf6 , Buff(6) , Cls , Clsrew , Dhsize , Entrys , File , Filnam(2) , I1 ,  &
         & I2 , Icc , Idout , Idrec(146) , Ierror , Ilist , Infile , Ipass , Istlst , Iswtch , Itemp , Itype1 , Itype2 , Jfile ,    &
         & Lminor , Lsf , Lstlst , Mcb(7) , Ncc , Ncore , Nlambs , Nlist , Npos , Nptsf , Nsols , Nstxtr , Nwds , Nwdsf , Nwords ,  &
         & Outfil , Outpt , Passes , Phase , Rd , Rdrew , Savdat(110) , Scrt(7) , Scrt1 , Scrt2 , Scrt3 , Scrt4 , Scrt5 , Scrt6 ,   &
         & Scrt7 , Setid , Sets(5,3) , Subcas , Sysbuf , Uvsol , Wrt , Wrtrew , Z(1)
   LOGICAL Col1 , Frstid , Sort2 , Trnsnt
   REAL Device , Form , Lambda , Omega , Rbuf(150) , Ridrec(146) , Rz(1) , Xsys(22)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ddrmc1/ Idrec , Buff , Passes , Outfil , Jfile , Mcb , Entrys , Sets , Infile , Lambda , File , Sort2 , Col1 , Frstid ,  &
                 & Ncore , Nsols , Dhsize , Filnam , Rbuf , Idout , Icc , Ncc , Ilist , Nlist , Nwds , Setid , Trnsnt , I1 , I2 ,   &
                 & Phase , Itype1 , Itype2 , Nptsf , Lsf , Nwdsf , Scrt , Ierror , Itemp , Device , Form , Istlst , Lstlst , Uvsol ,&
                 & Nlambs , Nwords , Omega , Ipass , Subcas
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /stdata/ Lminor , Nstxtr , Npos , Savdat
   COMMON /system/ Sysbuf , Outpt , Xsys , Iswtch
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER casecc , dva(3) , eor , frqset , i , ibase , ibuf , icol , ifile(4) , iforce , ifrout , ilsym , inblk(15) , index ,      &
         & isetx , ispcf , istres , iwrt , j , k , kk , l , m , noeor , nsetx , ofile(4) , oublk(15) , pp , subr(2) , uv
   REAL diff , diff1 , frq
   INTEGER korsz
!
!     DYNAMIC-DATA-RECOVERY-MATRIX-METHOD
!
!     DMAP SEQUENCES. ONLY SORT2 IS USED
!
!     (TRANSIENT RESPONCE)
!     ====================
!     DDRMM    CASEXX,UHVT,PPT,IPHIP2,IQP2,IES2,IEF2,XYCDB,EST,MPT,DIT/
!              ZUPV2,ZQP2,ZES2,ZEF2, $
!
!     (FREQUENCY RESPONSE)
!     ====================
!     DDRMM    CASEXX,UHVF,PPF,IPHIP1,IQP1,IES1,IEF1,XYCDB,EST,MPT,DIT/
!              ZUPVC1,ZQPC1,ZESC1,ZEFC1, $
!       OR
!     DDRMM    CASEXX,UHVF,PPF,IPHIP2,IQP2,IES2,IEF2,XYCDB,EST,MPT,DIT/
!              ZUPVC2,ZQPC2,ZESC2,ZEFC2, $
!
   EQUIVALENCE (Rz(1),Z(1)) , (Rbuf(1),Buf(1))
   EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2))
   EQUIVALENCE (Scrt3,Scrt(3)) , (Scrt4,Scrt(4))
   EQUIVALENCE (Scrt5,Scrt(5)) , (Scrt6,Scrt(6))
   EQUIVALENCE (Scrt7,Scrt(7)) , (Ridrec(1),Idrec(1))
   EQUIVALENCE (Buf1,Buff(1)) , (Buf2,Buff(2))
   EQUIVALENCE (Buf3,Buff(3)) , (Buf4,Buff(4))
   EQUIVALENCE (Buf5,Buff(5)) , (Buf6,Buff(6))
   DATA ifrout/145/ , dva/20 , 32 , 29/
   DATA istres , iforce , ispcf/23 , 26 , 35/
   DATA ilsym/166/
   DATA subr/4HDDRM , 4HM   /
   DATA eor , noeor/1 , 0/
   DATA casecc , uv , pp/101 , 102 , 103/
   DATA ifile/104 , 105 , 106 , 107/
   DATA ofile/201 , 202 , 203 , 204/
!
!     DETERMINE OPEN CORE AVAILABLE AND ALLOCATE BUFFERS.
!
   DO i = 1 , 100
      Savdat(i) = 0
   ENDDO
   DO i = 6 , 8
      Savdat(i) = 102
      Savdat(i+11) = 102
   ENDDO
   Savdat(15) = 102
   Savdat(76) = 2
   Savdat(77) = 10
   DO i = 1 , 7
      Scrt(i) = i + 300
   ENDDO
   Ncore = korsz(Z)
   DO i = 1 , 6
      Buff(i) = Ncore - Sysbuf - 2
      Ncore = Buff(i) - 1
   ENDDO
!
!     GET FIRST SUBCASE OF CASE CONTROL INTO CORE
!
   Ierror = 0
   Subcas = 1
   Icc = 1
   File = casecc
   CALL open(*1100,casecc,Z(Buf1),Rdrew)
   CALL fwdrec(*1200,casecc)
   CALL read(*1200,*100,casecc,Z(Icc),Ncore-Icc,noeor,Nwds)
   Ierror = 1
   GOTO 1400
!
 100  Ncc = Icc + Nwds - 1
   CALL close(casecc,Cls)
!
!     READ TRAILER OF SOLUTION DATA BLOCK. IF SOLUTION IS
!     COMPLEX, THEN FREQUENCY RESPONCE IS ASSUMED. IF REAL, THEN
!     TRANSIENT RESPONSE IS ASSUMED.
!
   Mcb(1) = uv
   CALL rdtrl(Mcb)
   Trnsnt = .TRUE.
   IF ( Mcb(5)>2 ) Trnsnt = .FALSE.
!
!     SET NUMBER OF EIGENVALUES = ROWS IN SOLUTION DATA BLOCK
!
   Nlambs = Mcb(3)
!
!     SET NUMBER OF SOLUTIONS.(TIME STEPS X 3, OR FREQUENCYS)
!
   Nsols = Mcb(2)
!
!     OPEN UV AND POSITION OVER HEADER RECORD.
!
   File = uv
   CALL open(*1100,uv,Z(Buf1),Rdrew)
   CALL fwdrec(*1200,uv)
   CALL close(uv,Cls)
!
!     READ LIST OF FREQUENCYS OR TIME STEPS FROM INPUT LOAD MATRIX
!     HEADER.
!
 200  Ilist = Ncc + 1
   File = pp
   CALL open(*1100,pp,Z(Buf1),Rdrew)
   Ierror = 2
   CALL read(*1200,*1300,pp,Buf(1),-2,noeor,Nwds)
   CALL read(*1200,*300,pp,Z(Ilist),Ncore-Ilist,noeor,Entrys)
   GOTO 1400
!
 300  Nlist = Ilist + Entrys - 1
   CALL close(pp,Clsrew)
!
!     IF FREQUENCY RESPONSE PROBLEM, AND USER HAS SPECIFIED A LIST OF
!     FREQUENCYS TO BE USED AS A GUIDE IN DETERMINING A SUBSET OF
!     SOLUTIONS FOR OUTPUT PURPOSES, AND NOT ALL SOLUTIONS WILL BE
!     OUTPUT, THEN A MODIFIED SOLUTION MATRIX IS NOW FORMED ON
!     SCRATCH-1. THIS WILL ELIMINATE UNNECESSARY MATRIX-MULTIPLIES LATER
!
!     IN ANY EVENT THE NEXT SUBCASE-S SOLUTIONS ARE PLACED ON SCRT1.
!
   Uvsol = uv
   IF ( .NOT.(Trnsnt) ) THEN
!
!     EXPAND LIST OF FREQS PLACING A FLAG AFTER EACH.
!
      j = Nlist
      Nlist = Nlist + Entrys
      Ierror = 4
      IF ( Nlist>Ncore ) GOTO 1400
      k = Nlist - 1
      DO i = 1 , Entrys
         Z(k) = Z(j)
         Z(k+1) = 0
         k = k - 2
         j = j - 1
      ENDDO
!
!     SET FLAGS OF FREQUENCYS TO BE OUTPUT.
!
      index = Icc + ifrout - 1
      frqset = Z(index)
      IF ( frqset>0 ) THEN
         index = Icc + ilsym - 1
         index = Z(index) + 1
         DO
            isetx = index + 2
            nsetx = isetx + Z(index+1) - 1
            IF ( Z(index)==frqset ) THEN
!
!     COMPARE REQUESTED FREQS WITH ACTUAL FREQS.
!
               DO i = isetx , nsetx
                  k = 0
                  diff = 1.0E+25
                  frq = Rz(i)
                  DO j = Ilist , Nlist , 2
                     IF ( Z(j+1)==0 ) THEN
                        diff1 = abs(Rz(j)-frq)
                        IF ( diff1<diff ) THEN
                           diff = diff1
                           k = j
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( k/=0 ) Z(k+1) = 1
               ENDDO
               GOTO 350
            ELSE
               index = nsetx + 1
               IF ( index>=Ncc ) THEN
                  frqset = -1
                  EXIT
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     ALL FREQUENCYS TO BE OUTPUT.
!
      DO i = Ilist , Nlist , 2
         Z(i+1) = 1
      ENDDO
!
 350  File = uv
      Ierror = 5
      CALL open(*1100,uv,Z(Buf1),Rd)
      File = Scrt1
      CALL open(*1100,Scrt1,Z(Buf2),Wrtrew)
      CALL fname(Scrt1,Filnam)
      CALL write(Scrt1,Filnam,2,eor)
      File = uv
!
!     COPY SOLUTION COLUMNS TO BE USED BY NOTEING FREQS MARKED FOR USE.
!
      Nsols = 0
      inblk(1) = uv
      oublk(1) = Scrt1
      DO i = Ilist , Nlist , 2
         IF ( Z(i+1)/=0 ) THEN
!
!     BLAST COPY THIS SOLUTION.
!
            icol = (i-Ilist)/2 + 1
            CALL cpystr(inblk,oublk,0,icol)
            Nsols = Nsols + 1
         ELSE
            CALL fwdrec(*1200,uv)
         ENDIF
      ENDDO
!
!     RESET -UV- DATA BLOCK DESIGNATOR TO POINT TO SCRT1, AND WRITE
!     A TRAILER.
!
      CALL close(uv,Cls)
      CALL close(Scrt1,Clsrew)
      Mcb(1) = uv
      CALL rdtrl(Mcb)
      Mcb(1) = Scrt1
      Mcb(2) = Nsols
      CALL wrttrl(Mcb)
      Uvsol = Scrt1
!
!     SHRINK UP THE FREQUENCY LIST TO MATCH SOLUTION MATRIX
!
      j = Ilist - 1
      DO i = Ilist , Nlist , 2
         IF ( Z(i+1)/=0 ) THEN
            j = j + 1
            Z(j) = Z(i)
         ENDIF
      ENDDO
      Nlist = j
   ENDIF
!
!     IF THIS IS A TRANSIENT RESPONSE PROBLEM, THE SOLUTION MATRIX IS
!     NOW PARTITIONED INTO 3 SOLUTION MATRICES FOR DISP, VEL, AND ACCEL.
!
   IF ( Trnsnt ) THEN
      File = uv
      Ierror = 6
      CALL open(*1100,uv,Z(Buf1),Rd)
      Mcb(1) = uv
      inblk(1) = uv
      CALL rdtrl(Mcb)
      DO i = 1 , 3
         File = Scrt(i)
         ibuf = Buff(i+1)
         CALL open(*1100,File,Z(ibuf),Wrtrew)
         CALL fname(File,Filnam)
         CALL write(File,Filnam,2,eor)
         Mcb(1) = File
         Mcb(2) = Nsols/3
         CALL wrttrl(Mcb)
      ENDDO
      Ierror = 7
      File = uv
      DO i = 1 , Nsols , 3
         DO j = 1 , 3
            oublk(1) = Scrt(j)
            CALL cpystr(inblk,oublk,0,i)
         ENDDO
      ENDDO
      CALL close(uv,Clsrew)
      Nsols = Nsols/3
!
      DO i = 1 , 3
         CALL close(Scrt(i),Clsrew)
      ENDDO
   ENDIF
!
!     SDR2 FORMED MODAL SOLUTIONS FOR DISPLACEMENTS, SINGLE-POINT-
!     CONSTRAINT-FORCES, ELEMENT STRESSES, AND ELEMENT FORCES MAY BE
!     PRESENT. (ALL WILL BE SORT1-REAL, OR SORT2-REAL)
!
!     IF THIS IS A TRANSIENT PROBLEM, THE SOLUTIONS PRESENT HAVE BEEN
!     PARTITIONED INTO THE DISPLACEMENT, VELOCITY, AND ACCELERATION
!     SUBSETS. ONLY WHEN OPERATING ON THE MODAL DISPLACEMENTS WILL THE
!     VELOCITY AND ACCELERATION SOLUTION SUBSET MATRICES BE USED.
!
   Jfile = 1
 400  Infile = ifile(Jfile)
!
!     CHECK FOR EXISTENCE OF MODAL SOLUTION -INFILE-.
!
   CALL open(*800,Infile,Z(Buf1),Rdrew)
   CALL fwdrec(*700,Infile)
!
!     INFILE DOES EXIST.SET PARAMETERS FOR PROCESSING
!
!
!     OPEN OFP-FORMAT OUTPUT FILE FOR THIS INFILE.
!
   Outfil = ofile(Jfile)
   iwrt = Wrtrew
   IF ( Subcas>1 ) iwrt = Wrt
   CALL open(*500,Outfil,Z(Buf4),iwrt)
   IF ( Subcas<=1 ) THEN
!
      CALL fname(Outfil,Filnam)
      CALL write(Outfil,Filnam,2,eor)
   ENDIF
   CALL close(Outfil,Cls)
!
!     READ FIRST OFP-ID RECORD AND DETERMINE WHAT THE HELL IS REALLY
!     PRESENT.
!
   Ierror = 14
   CALL read(*700,*700,Infile,Idrec,146,eor,Nwds)
!
!     MAJOR ID AND SORT1 OR SORT2 DETERMINATION.
!
   Itype1 = Idrec(2)/1000
   Sort2 = .FALSE.
   IF ( Itype1>1 ) Sort2 = .TRUE.
   Itype1 = Idrec(2) - Itype1*1000
!
!     BRANCH ON MAJOR ID
!
   IF ( Itype1<1 .OR. Itype1>7 ) GOTO 600
   Passes = 1
   IF ( Itype1==1 .OR. Itype1==2 .OR. Itype1==6 ) GOTO 600
   IF ( Itype1==3 ) THEN
!
!     MODAL SPCF-S ARE ON INFILE.
!
      Itemp = Icc + ispcf - 1
      Nwords = 2
   ELSEIF ( Itype1==4 ) THEN
!
!     MODAL FORCES ARE ON INFILE.
!
      Itemp = Icc + iforce - 1
      Nwords = 1
   ELSEIF ( Itype1==5 ) THEN
!
!     MODAL STRESSES ARE ON INFILE.
!
      Itemp = Icc + istres - 1
      Nwords = 1
   ELSE
!
!     MODAL DISPLACEMENTS = EIGENVECTORS ARE ON INFILE.
!
      Passes = 3
      Nwords = 2
   ENDIF
!
!     DETERMINE DISP, VEL, AND ACCEL SET REQUESTS.
!
   ibase = Icc + ilsym - 1
   ibase = Z(ibase) + 1
   DO i = 1 , Passes
      IF ( Passes/=1 ) Itemp = Icc + dva(i) - 1
      Sets(1,i) = Z(Itemp)
      Sets(2,i) = Z(Itemp+1)
      Sets(3,i) = iabs(Z(Itemp+2))
      Sets(4,i) = 0
      Sets(5,i) = 0
      IF ( Sets(1,i)>0 ) THEN
         index = ibase
         DO
            isetx = index + 2
            IF ( Z(index)==Sets(1,i) ) THEN
               Sets(4,i) = isetx
               Sets(5,i) = Z(index+1)
               EXIT
            ELSE
               index = isetx + Z(index+1)
               IF ( index>=Ncc ) THEN
                  Sets(1,i) = -1
                  EXIT
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     CALL PROCESSOR TO BUILD DATA-MATRIX ON SCRT5 AND MAPPING-DATA ON
!     SCRT4, AND THEN PERFORM OUTPUT OF RESULTS TO OUTFIL.
!
   IF ( Sort2 ) THEN
!
!     SORT2 PROCESSOR
!
      CALL ddrmm2(*1100,*1200,*1300,*1400)
   ELSE
!
!     SORT1 PROCESSOR
!
      CALL ddrmm1(*1100,*1200,*1300,*1400)
   ENDIF
!
!     WRAP UP PROCESSING FOR THIS INFILE.
!
   Mcb(1) = Outfil
   Mcb(2) = 1
   CALL wrttrl(Mcb)
   GOTO 700
 500  WRITE (Outpt,99001) Uwm , Infile
99001 FORMAT (A25,' 2331. (DDRMM-2) OUTPUT DATA BLOCK CORRESPONDING TO',' INPUT MODAL SOLUTION DATA BLOCK',I4,/5X,                  &
             &'IS NOT PRESENT.  INPUT DATA BLOCK IGNORED.')
   GOTO 700
!
!     ILLEGAL INFILE DATA.
!
 600  WRITE (Outpt,99002) Uwm , Infile
99002 FORMAT (A25,' 2332.  (DDRMM-4) INVALID INPUT DATA DETECTED IN ','DATA BLOCK',I5,'. PROCESSING STOPPED FOR THIS DATA BLOCK')
 700  CALL close(Outfil,Clsrew)
   CALL close(Infile,Clsrew)
   CALL close(Scrt4,Clsrew)
   CALL close(Scrt5,Clsrew)
!
!     PROCESS NEXT MODAL SOLUTION INPUT.
!
 800  Jfile = Jfile + 1
   IF ( Jfile<=4 ) GOTO 400
!
!     ALL WORK COMPLETE FOR THIS SUBCASE. IF FREQUENCY RESPONSE PROCESS
!     NEXT SUBCASE.
!
   IF ( Trnsnt ) GOTO 1000
   File = casecc
   Ierror = 471
   CALL open(*1100,casecc,Z(Buf1),Rd)
   CALL read(*1000,*900,casecc,Z(Icc),Ncore-Icc,noeor,Nwds)
   GOTO 1400
!
 900  Ncc = Icc + Nwds - 1
   Subcas = Subcas + 1
   CALL close(casecc,Cls)
   GOTO 200
!
!//// SUBCAS  NUMBER NEEDS TO GET INTO OUTPUT BLOCKS
!
 1000 Ierror = 511
   CALL close(casecc,Clsrew)
   GOTO 1600
!
!     ERRORS FORCING TERMINATION OF THIS MODULE.
!
 1100 kk = 1
   GOTO 1500
 1200 kk = 2
   GOTO 1500
 1300 kk = 3
   GOTO 1500
 1400 kk = 8
 1500 CALL mesage(kk,File,subr)
   WRITE (Outpt,99003) Swm , Ierror
99003 FORMAT (A27,' 2333.  (DDRMM-1) MODULE DDRMM TERMINATED WITH ','VARIABLE IERROR =',I10)
!
!     INSURE ALL FILES CLOSED BEFORE RETURNING.
!
   DO l = 100 , 300 , 100
      DO m = 1 , 11
         Jfile = m + l
         CALL close(Jfile,Clsrew)
      ENDDO
   ENDDO
!
!     INSURE ALL OUT-FILES HAVE AN EOF.
!
 1600 DO l = 201 , 204
      CALL open(*1700,l,Z(Buf1),Wrt)
      CALL close(l,Clsrew)
 1700 ENDDO
END SUBROUTINE ddrmm
