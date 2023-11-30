
SUBROUTINE mred1
!
!     THIS SUBROUTINE IS THE MRED1 MODULE WHICH INITIATES THE MODAL
!     SYNTHESIS CALCULATIONS.
!
!     DMAP CALLING SEQUENCE
!     MRED1    CASECC,GEOM4,DYNAMICS/USETX,EEDX,EQST,DMR/*NAMEA*/
!              S,N,DRY/STEP/S,N,NOUS/S,N,SKIPM/S,N,GPARM/TYPE $
!
!     INPUT DATA
!     GINO   - CASECC - CASE CONTROL
!              GEOM4  - BDYC DATA
!                     - BDYS DATA
!                     - BDYS1 DATA
!              DYNAMICS - EIGR DATA
!     SOF    - EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE
!              BGSS   - BASIC GRID POINT IDENTIFICATION TABLE
!              CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRICES DATA
!
!     OUTPUT DATA
!     GINO   - USETX  - S,R,B DEGREES OF FREEDOM
!              EEDX   - EIGR DATA
!              EQST   - TEMPORARY EQSS
!              DMR    - RIGID BODY MATRIX
!
!     PARAMETERS
!     INPUT  - NAMEA  - INPUT SUBSTRUCTURE NAME (BCD)
!              DRY    - OPERATION MODE (INTEGER)
!              STEP   - CONTROL DATA CASECC RECORD (INTEGER)
!              TYPE   - REAL OR COMPLEX (BCD)
!     OUTPUT - DRY    - MODULE OPERATION FLAG (INTEGER)
!              NOUS   - FIXED POINTS FLAG (INTEGER)
!                     = +1 IF FIXED POINTS DEFINED
!                     = -1 IF NO FIXED POINTS DEFINED
!              SKIPM  - MODES FLAG (INTEGER)
!                     =  0 IF MODES NOT PRESENT
!                     = -1 IF MODES PRESENT
!     OTHERS - GBUF   - GINO BUFFERS
!              SBUF   - SOF  BUFFERS
!              KORLEN - CORE LENGTH
!              NEWNAM - NEW SUBSTRUCTURE NAME
!              BNDSET - BOUNDARY SET IDENTIFICATION NUMBER
!              FIXSET - FIXED SET IDENTIFICATION NUMBER
!              IEIG   - EIGENVALUE SET IDENTIFICATION NUMBER
!              IO     - OUTPUT FLAGS
!              RGRID  - FREEBODY MODES FLAGS
!              RNAME  - FREEBODY SUBSTRUCTURE NAME
!              IRSAVE - RSAVE FLAG
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              NCSUBS - NUMBER OF CONTRIBUTING SUBSTRUCTURES
!              NAMEBS - BEGINNING ADDRESS OF CONTRIBUTING SUBSTRUCTURE
!                       NAMES
!              EQSIND - BEGINNING ADDRESS OF EQSS GROUP ADDRESSES
!              NSLBGN - BEGINNING ADDRESS OF SIL DATA
!              NSIL   - NUMBER OF SIL GROUPS
!              BDYCS  - BEGINNING ADDRESS OF BDYC DATA
!              NBDYCC - NUMBER OF BDYC DATA GROUPS
!              USETL  - LENGTH OF USET ARRAY
!              USTLOC - BEGINNING ADDRESS OF USET ARRAY
!              RGRIDX - FREEBODY MODE RELATIVE X COORDINATE
!              RGRIDY - FREEBODY MODE RELATIVE Y COORDINATE
!              RGRIDZ - FREEBODY MODE RELATIVE Z COORDINATE
!              USRMOD - USERMODE  OPTION FLAG
!              BOUNDS - OLDBOUNDS OPTION FLAG
!
   IMPLICIT NONE
   INTEGER Bdycs , Bndset , Dry , Eqsind , Fixset , Gbuf1 , Gbuf2 , Ieig , Io , Iprntr , Irsave , Korbgn , Korlen , Namebs ,        &
         & Nbdycc , Ncsubs , Newnam(2) , Nous , Nsil , Nslbgn , Oldnam(2) , Rgrid(2) , Rgridx , Rgridy , Rgridz , Rname(2) , Sbuf1 ,&
         & Sbuf2 , Sbuf3 , Skipm , Step , Sysbuf , Type(2) , Usetl , Ustloc , Z(1)
   LOGICAL Bounds , Ponly , Usrmod
   REAL Gprm , Rz(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Oldnam , Dry , Step , Nous , Skipm , Type , Gprm , Gbuf1 , Gbuf2 , Sbuf1 , Sbuf2 , Sbuf3 , Korlen , Newnam ,     &
                 & Bndset , Fixset , Ieig , Io , Rgrid , Rname , Irsave , Korbgn , Ncsubs , Namebs , Eqsind , Nslbgn , Nsil ,       &
                 & Bdycs , Nbdycc , Usetl , Ustloc , Rgridx , Rgridy , Rgridz , Usrmod , Bounds , Ponly
   COMMON /system/ Sysbuf , Iprntr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER all , casecc , cctype(2) , cred , errnam(6) , i , ibf , iblank , ibound , icode , ifile , iflag , ifree , imax , imode , &
         & imsg , irange , isil , itest , itmnam(2) , iuserm , j , k , letrs(2) , lstbit(32) , modes , modnam(2) , module ,         &
         & mtrlra(7) , mtrlrb(7) , mtrlrc(7) , mtrlrd(7) , mtrlre(7) , nheqss , nhloap , nhlods , nmax , nmonic(16) , no , noieig , &
         & noread , nowdsc , nozwds , nrange , nwdsd , nwdsrd , yes
   INTEGER andf , korsz , orf , rshift
   LOGICAL errors
   REAL range(2)
   EXTERNAL andf , orf , rshift
   !>>>>EQUIVALENCE (Z(1),Rz(1))
   DATA nmonic/4HNAMB , 4HBOUN , 4HFIXE , 4HMETH , 4HCMET , 4HOUTP , 4HRGRI , 4HOLDM , 4HOLDB , 4HRSAV , 4HRNAM , 4HRANG , 4HNMAX , &
       &4HUSER , 4HNAMA , 4HGPAR/
   DATA casecc/101/
   DATA modnam/4HMRED , 4H1   /
   DATA errnam/4HLAMS , 4HPHIS , 4HPHIL , 4HGIMS , 4HLMTX , 4HUPRT/
   DATA iblank , yes , no , all/4H     , 4HYES  , 4HNO   , 4HALL /
   DATA letrs/1HM , 1HC/
   DATA cctype/ - 1 , -2/
   DATA cred , nhlods , nhloap , nheqss/4HCRED , 4HLODS , 4HLOAP , 4HEQSS/
!
!     COMPUTE OPEN CORE AND DEFINE GINO, SOF BUFFERS
!
   nozwds = korsz(Z(1))
   Gbuf1 = nozwds - Sysbuf - 2
   Gbuf2 = Gbuf1 - Sysbuf
   Sbuf1 = Gbuf2 - Sysbuf
   Sbuf2 = Sbuf1 - Sysbuf - 1
   Sbuf3 = Sbuf2 - Sysbuf
   Korlen = Sbuf3 - 1
   Korbgn = 1
   IF ( Korlen<=Korbgn ) GOTO 1500
!
!     INITIALIZE SOF
!
   CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
!
!     INITIALIZE CASE CONTROL PARAMETERS
!
   DO i = 1 , 2
      Rgrid(i) = -1
      Newnam(i) = iblank
      Rname(i) = iblank
   ENDDO
   Bndset = 0
   Fixset = 0
   Ieig = 0
   noieig = yes
   Io = 0
   Skipm = 0
   modes = no
   Bounds = .FALSE.
   Ponly = .FALSE.
   ibound = no
   Irsave = no
   Nous = 1
   ifree = no
   nmax = 2147483647
   imax = all
   imode = no
   Usrmod = .FALSE.
   iuserm = 1
   module = 1
   Gprm = 0.0
   ibf = 0
   nrange = 0
   irange = all
   range(1) = -1.0E+35
   range(2) = 1.0E+35
!
!     PROCESS CASE CONTROL
!
   ifile = casecc
   CALL open(*1200,casecc,Z(Gbuf2),0)
   IF ( Step/=0 ) THEN
      DO i = 1 , Step
         CALL fwdrec(*1400,casecc)
      ENDDO
   ENDIF
!
!     READ CASECC AND EXTRACT DATA
!
   CALL read(*1300,*1400,casecc,Z(Korbgn),2,0,noread)
   IF ( Z(Korbgn)==cred ) module = 2
   nowdsc = Z(Korbgn+1)
   DO i = 1 , nowdsc , 3
      CALL read(*1300,*1400,casecc,Z(Korbgn),3,0,noread)
!
!     TEST CASE CONTROL MNEMONICS
!
      DO j = 1 , 16
         IF ( Z(Korbgn)==nmonic(j) ) GOTO 50
      ENDDO
      CYCLE
!
!     SELECT DATA TO EXTRACT
!
 50   IF ( j==2 ) THEN
!
!     EXTRACT BOUNDARY SET
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            Bndset = Z(Korbgn+2)
            ibf = ibf + 2
            CYCLE
         ENDIF
      ELSEIF ( j==3 ) THEN
!
!     EXTRACT FIXED SET
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            Fixset = Z(Korbgn+2)
            ibf = ibf + 1
            CYCLE
         ENDIF
      ELSEIF ( j==4 .OR. j==5 ) THEN
!
!     EXTRACT EIGENVALUE METHOD
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            Ieig = Z(Korbgn+2)
            noieig = no
            CYCLE
         ENDIF
      ELSEIF ( j==6 ) THEN
!
!     EXTRACT OUTPUT FLAGS
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            Io = orf(Io,Z(Korbgn+2))
            CYCLE
         ENDIF
      ELSEIF ( j==7 ) THEN
!
!     EXTRACT RIGID BODY GRID POINT ID
!
         Rgrid(1) = Z(Korbgn+2)
         IF ( Z(Korbgn+1)/=cctype(1) ) Rgrid(1) = 0
         ifree = yes
         CYCLE
      ELSEIF ( j==8 ) THEN
!
!     SET OLDMODES FLAG
!
         IF ( (Z(Korbgn+1)/=cctype(1)) .AND. (Z(Korbgn+1)/=cctype(2)) ) THEN
            IF ( Z(Korbgn+1)==yes ) THEN
               Skipm = -1
               modes = yes
            ENDIF
            CYCLE
         ENDIF
      ELSEIF ( j==9 ) THEN
!
!     SET OLDBOUND FLAG
!
         IF ( (Z(Korbgn+1)/=cctype(1)) .AND. (Z(Korbgn+1)/=cctype(2)) ) THEN
            IF ( Z(Korbgn+1)==yes ) THEN
               Bounds = .TRUE.
               ibound = yes
            ENDIF
            CYCLE
         ENDIF
      ELSEIF ( j==10 ) THEN
!
!     SET RSAVE FLAG
!
         IF ( Z(Korbgn+1)/=no ) Irsave = yes
         CYCLE
      ELSEIF ( j==11 ) THEN
!
!     EXTRACT RIGID BODY SUBSTRUCTURE NAME
!
         DO k = 1 , 2
            Rname(k) = Z(Korbgn+k)
         ENDDO
         IF ( Rgrid(1)<0 ) Rgrid(1) = 0
         ifree = yes
         CYCLE
      ELSEIF ( j==12 ) THEN
!
!     EXTRACT FREQUENCY RANGE
!
         IF ( Z(Korbgn+1)==cctype(2) ) THEN
            irange = iblank
            IF ( nrange==1 ) THEN
               range(2) = Rz(Korbgn+2)
            ELSE
               nrange = 1
               range(1) = Rz(Korbgn+2)
            ENDIF
            CYCLE
         ENDIF
      ELSEIF ( j==13 ) THEN
!
!     EXTRACT MAXIMUM NUMBER OF FREQUENCIES
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            IF ( Z(Korbgn+2)/=0 ) THEN
               nmax = Z(Korbgn+2)
               imax = iblank
            ENDIF
            CYCLE
         ENDIF
      ELSEIF ( j==14 ) THEN
!
!     EXTRACT USERMODE FLAG
!
         IF ( Z(Korbgn+1)==cctype(1) ) THEN
            imode = yes
            Skipm = -1
            Usrmod = .TRUE.
            IF ( Z(Korbgn+2)==2 ) iuserm = 2
            CYCLE
         ENDIF
      ELSEIF ( j==15 ) THEN
!
!     EXTRACT OLD SUBSTRUCTURE NAME
!
         DO k = 1 , 2
            Oldnam(k) = Z(Korbgn+k)
         ENDDO
         CYCLE
      ELSEIF ( j==16 ) THEN
!
!     EXTRACT GPARAM PARAMETER
!
         IF ( Z(Korbgn+1)==cctype(2) ) THEN
            Gprm = Rz(Korbgn+2)
            CYCLE
         ENDIF
      ELSE
!
!     EXTRACT NEW SUBSTRUCTURE NAME
!
         DO k = 1 , 2
            Newnam(k) = Z(Korbgn+k)
         ENDDO
         CYCLE
      ENDIF
!
!     CASECC COMMAND ERROR
!
      WRITE (Iprntr,99001) Uwm , letrs(module) , nmonic(j)
99001 FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE ',A1,'REDUCE OUTPUT ','COMMAND ',A4,'.  COMMAND IGNORED.')
   ENDDO
   CALL close(casecc,1)
!
!     TEST MODULE OPERATION FLAG
!
   IF ( Dry<0 ) THEN
      IF ( Dry/=-2 ) THEN
         WRITE (Iprntr,99002) Uim
99002    FORMAT (A29,' 6630, FOR DRY OPTION IN MODAL REDUCE, INPUT DATA ','WILL BE CHECKED',/36X,'BUT NO SOF TABLE ITEMS WILL BE ', &
                &'CREATED.')
         Dry = -2
      ENDIF
   ELSEIF ( Dry==0 ) THEN
      Skipm = -1
      itest = 0
      CALL fdsub(Newnam,itest)
      IF ( itest/=-1 ) GOTO 1900
      WRITE (Iprntr,99003) Ufm , letrs(module) , Newnam
99003 FORMAT (A23,' 6220, MODULE ',A1,'REDUCE - RUN EQUALS GO AND ','SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
      GOTO 1800
   ELSE
      itest = 0
      CALL fdsub(Newnam,itest)
      IF ( itest/=-1 ) THEN
         IF ( .NOT.(Bounds .OR. (Skipm==-1)) ) THEN
            CALL sfetch(Newnam,nhlods,3,itest)
            IF ( itest==3 ) THEN
!
!     LOADS ONLY PROCESSING
!
               Ponly = .TRUE.
            ELSE
               CALL sfetch(Newnam,nhloap,3,itest)
               IF ( itest==3 ) THEN
                  Ponly = .TRUE.
               ELSE
                  itmnam(1) = Newnam(1)
                  itmnam(2) = Newnam(2)
!
!     PROCESS MODULE FATAL ERRORS
!
                  imsg = -4
                  GOTO 1700
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     TEST OUTPUT OPTION
!
   IF ( andf(Io,1)/=0 ) THEN
      CALL page1
      WRITE (Iprntr,99004) Oldnam , Newnam
!
99004 FORMAT (//38X,46HS U M M A R Y    O F    C U R R E N T    P R O,8H B L E M,//13X,38HNAME OF PSEUDOSTRUCTURE TO BE REDUCED ,   &
             &4(2H. ),2A4,6X,40HNAME GIVEN TO RESULTANT PSEUDOSTRUCTURE ,2A4)
      IF ( ibf==0 ) WRITE (Iprntr,99005)
99005 FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),14X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ))
      IF ( ibf==1 ) WRITE (Iprntr,99006) Fixset
99006 FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),14X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ),I8)
      IF ( ibf==2 ) WRITE (Iprntr,99007) Bndset
99007 FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),I8,6X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ))
      IF ( ibf==3 ) WRITE (Iprntr,99008) Bndset , Fixset
99008 FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),I8,6X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ),I8)
      IF ( Rgrid(1)==-1 ) WRITE (Iprntr,99009) Rname
99009 FORMAT (13X,'RIGID BODY GRID POINT IDENTIFICATION NUMBER .',14X,'RIGID BODY SUBSTRUCTURE NAME ',5(2H. ),2A4)
      IF ( Rgrid(1)/=-1 ) WRITE (Iprntr,99010) Rgrid(1) , Rname
99010 FORMAT (13X,46HRIGID BODY GRID POINT IDENTIFICATION NUMBER . ,I8,6X,30HRIGID BODY SUBSTRUCTURE NAME  ,5(2H. ),2A4)
      IF ( noieig==no ) WRITE (Iprntr,99023) ibound , modes , ifree , imode , Irsave , Ieig
      IF ( noieig/=no ) WRITE (Iprntr,99023) ibound , modes , ifree , imode , Irsave
      IF ( imax==all ) WRITE (Iprntr,99011) imax , Gprm
99011 FORMAT (13X,42HMAXIMUM NUMBER OF FREQUENCIES TO BE USED  ,2(2H. ),A4,10X,14HGPARAM VALUE  ,13(2H. ),1P,E12.6)
      IF ( imax/=all ) WRITE (Iprntr,99012) nmax , Gprm
99012 FORMAT (13X,42HMAXIMUM NUMBER OF FREQUENCIES TO BE USED  ,2(2H. ),I8,6X,14HGPARAM VALUE  ,13(2H. ),1P,E12.6)
      IF ( irange==all ) WRITE (Iprntr,99013) Oldnam , irange
99013 FORMAT (13X,46HNAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ,2A4,6X,32HRANGE OF FREQUENCIES TO BE USED ,4(2H. ),A4)
      IF ( irange/=all ) WRITE (Iprntr,99014) Oldnam , range(1)
99014 FORMAT (13X,46HNAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ,2A4,6X,32HRANGE OF FREQUENCIES TO BE USED ,4(2H. ),1P,E12.6)
   ENDIF
!
!     CHECK FOR OLDMODES, OLDBOUND ERRORS
!
   errors = .FALSE.
   IF ( Ponly ) GOTO 700
   CALL sfetch(Oldnam,errnam(1),3,itest)
   CALL softrl(Oldnam,errnam(2),mtrlra)
   CALL softrl(Oldnam,errnam(4),mtrlrb)
   CALL softrl(Oldnam,errnam(5),mtrlrc)
   CALL softrl(Oldnam,errnam(3),mtrlrd)
   CALL softrl(Oldnam,errnam(6),mtrlre)
   iflag = 1
   IF ( Usrmod ) GOTO 700
   IF ( Skipm>=0 ) THEN
!
!     OLDMODES NOT SET - PHIS, PHIL AND LAMS MUST BE DELETED
!
      IF ( itest>=3 ) GOTO 200
      GOTO 900
!
!     OLDMODES SET - PHIS AND LAMS MUST BE ON SOF
!
   ELSEIF ( itest>3 ) THEN
      GOTO 800
   ENDIF
 100  iflag = 2
   IF ( mtrlra(1)<=2 ) GOTO 400
   GOTO 800
 200  iflag = 2
   IF ( mtrlra(1)<3 ) GOTO 900
 300  iflag = 3
   IF ( mtrlrd(1)<3 ) GOTO 900
!
!     OLDBOUND SET - GIMS AND UPRT MUST BE ON SOF
!
 400  iflag = 4
   IF ( .NOT.Bounds ) THEN
!
!     OLDBOUND NOT SET - GIMS AND LMTX MUST BE DELETED
!
      IF ( mtrlrb(1)>=3 ) GOTO 600
      GOTO 1100
   ELSEIF ( mtrlrb(1)>2 ) THEN
      GOTO 1000
   ENDIF
 500  iflag = 6
   IF ( mtrlre(1)<=2 ) GOTO 700
   GOTO 1000
 600  iflag = 5
   IF ( mtrlrc(1)<3 ) GOTO 1100
!
!     TEST FOR ERRORS
!
 700  IF ( errors ) GOTO 1800
   IF ( iuserm==2 ) WRITE (Iprntr,99015) Uim
99015 FORMAT (A29,' 6636, NMAX AND RANGE SUB COMMANDS ARE IGNORED ','UNDER USERMODES = TYPE 2.')
!
!     READ EQSS GROUP 0 DATA AND TEST OPEN CORE LENGTH
!
   itmnam(2) = Oldnam(2)
   CALL sfetch(Oldnam,nheqss,1,itest)
   IF ( itest==3 ) THEN
      imsg = -1
      GOTO 1700
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      GOTO 1700
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      GOTO 1700
   ELSE
      CALL suread(Z(Korbgn),-1,nwdsrd,itest)
      IF ( Korbgn+nwdsrd>=Sbuf3 ) GOTO 1500
!
!     COMPRESS BASIC SUBSTRUCTURE NAMES AND TEST OPEN CORE LENGTH
!
      Ncsubs = Z(Korbgn+2)
      Namebs = Korbgn
      i = 2*((nwdsrd-4)/2)
      k = 4
      DO j = 1 , i , 2
         Z(Korbgn+j-1) = Z(Korbgn+k)
         Z(Korbgn+j) = Z(Korbgn+k+1)
         IF ( Rgrid(1)>=0 ) THEN
            IF ( Rname(1)==iblank ) THEN
               Rname(1) = Z(Korbgn+j-1)
               Rname(2) = Z(Korbgn+j)
            ENDIF
            IF ( (Z(Korbgn+j-1)==Rname(1)) .AND. (Z(Korbgn+j)==Rname(2)) ) Rgrid(2) = (j+1)/2
         ENDIF
         k = k + 2
      ENDDO
      Eqsind = Korbgn + 2*Ncsubs
      IF ( Eqsind>=Sbuf3 ) GOTO 1500
!
!     TEST OUTPUT OPTION
!
      IF ( andf(Io,1)/=0 ) THEN
         IF ( irange/=all ) THEN
            IF ( Ncsubs<5 ) THEN
               i = 1 + 2*Ncsubs
               DO j = i , 10
                  Z(Korbgn+j-1) = iblank
               ENDDO
            ENDIF
            k = 10
            WRITE (Iprntr,99016) (Z(Korbgn+j-1),Z(Korbgn+j),j=1,k,2) , range(2)
99016       FORMAT (16X,5(2A4,2X),47X,1P,E12.6)
            IF ( Ncsubs>5 ) THEN
               k = k + 1
               i = 2*Ncsubs
               WRITE (Iprntr,99022) (Z(Korbgn+j-1),Z(Korbgn+j),j=k,i,2)
            ENDIF
         ELSE
            i = 2*Ncsubs
            WRITE (Iprntr,99022) (Z(Korbgn+j-1),Z(Korbgn+j),j=1,i,2)
         ENDIF
      ENDIF
!
!     READ EQSS GROUPS TO END-OF-ITEM
!
      Korbgn = Eqsind + 2*Ncsubs
      DO i = 1 , Ncsubs
         IF ( Korbgn>=Sbuf3 ) GOTO 1500
         CALL suread(Z(Korbgn),-1,nwdsrd,itest)
         j = 2*(i-1)
         Z(Eqsind+j) = Korbgn
         Z(Eqsind+j+1) = nwdsrd
         Korbgn = Korbgn + nwdsrd
      ENDDO
      Nslbgn = Korbgn
      CALL suread(Z(Korbgn),-2,nwdsrd,itest)
      Nsil = nwdsrd/2
!
!     TEST OUTPUT OPTION
!
      IF ( andf(rshift(Io,3),1)/=0 ) THEN
         DO i = 1 , Ncsubs
            j = 2*(i-1)
            CALL cmiwrt(1,Oldnam,Z(Namebs+j),Z(Eqsind+j),Z(Eqsind+j+1),Rz,Z)
         ENDDO
         isil = 2*Nsil
         CALL cmiwrt(8,Oldnam,Oldnam,Nslbgn,isil,Rz,Z)
      ENDIF
!
!     DETERMINE USET LENGTH
!
      Korbgn = Nslbgn + nwdsrd
      Ustloc = Korbgn
      icode = Z(Korbgn-2)
      CALL decode(icode,lstbit,nwdsd)
      Usetl = (Z(Korbgn-3)+nwdsd) - 1
!
!     PROCESS FIXED SET
!
      CALL mred1a(1)
      CALL mred1b(1)
!
!     PROCESS BOUNDARY SET
!
      CALL mred1a(2)
      CALL mred1b(2)
!
!     CONVERT EQSS DATA TO UB DATA
!
      IF ( .NOT.(Ponly) ) THEN
         CALL mred1c
!
!     PROCESS EIGENVALUE DATA
!
         IF ( Skipm/=-1 ) CALL mred1d
!
!     PROCESS FREE BODY MODES
!
         CALL mred1e
      ENDIF
      GOTO 1900
   ENDIF
!
!     PHIS, LAMS DO NOT EXIST
!
 800  WRITE (Iprntr,99017) Ufm , errnam(iflag) , Oldnam
99017 FORMAT (A23,' 6617, OLDMODES SET AND REQUESTED SOF ITEM DOES NOT',' EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
   errors = .TRUE.
   IF ( iflag==1 ) GOTO 100
   IF ( iflag==2 ) GOTO 400
!
!     PHIS, PHIR, LAMS NOT DELETED
!
 900  WRITE (Iprntr,99018) Ufm , errnam(iflag) , Oldnam
99018 FORMAT (A23,' 6618, OLDMODES NOT SET AND REQUESTED SOF ITEM MUST',' BE DELETED.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
   errors = .TRUE.
   IF ( iflag==1 ) GOTO 200
   IF ( iflag==2 ) GOTO 300
   IF ( iflag==3 ) GOTO 400
!
!     GIMS, UPRT DOES NOT EXIST
!
 1000 WRITE (Iprntr,99019) Ufm , errnam(iflag) , Oldnam
99019 FORMAT (A23,' 6619, OLDBOUND SET AND REQUESTED SOF ITEM DOES NOT',' EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
   errors = .TRUE.
   IF ( iflag>5 ) GOTO 700
   GOTO 500
!
!     GIMS, LMTX NOT DELETED
!
 1100 WRITE (Iprntr,99020) Ufm , errnam(iflag) , Oldnam
99020 FORMAT (A23,' 6620, OLDBOUND NOT SET AND REQUESTED SOF ITEM MUST',' BE DELETED.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
   errors = .TRUE.
   iflag = iflag - 3
   IF ( iflag==1 ) GOTO 600
   IF ( iflag==2 ) GOTO 700
!
!     PROCESS SYSTEM FATAL ERRORS
!
 1200 imsg = -1
   GOTO 1600
 1300 imsg = -2
   GOTO 1600
 1400 imsg = -3
   GOTO 1600
 1500 imsg = -8
   ifile = 0
 1600 CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
 1700 CALL smsg(imsg,nheqss,itmnam)
   RETURN
!
 1800 CALL sofcls
   Dry = -2
   RETURN
!
!     CLOSE ANY OPEN FILES
!
 1900 CALL sofcls
   IF ( Dry==-2 ) WRITE (Iprntr,99021) letrs(module)
99021 FORMAT (10H0  MODULE ,A1,36HREDUCE TERMINATING DUE TO ABOVE ERRO,3HRS.)
   IF ( Ponly ) Skipm = -1
   RETURN
99022 FORMAT (16X,2A4,2X,2A4,2X,2A4,2X,2A4,2X,2A4)
99023 FORMAT (13X,18HOLDBOUND FLAG SET ,14(2H. ),A4,10X,12HOLDMODES FLA,6HG SET ,11(2H. ),A4,/13X,29HFREE BODY MODES TO BE CALCULA, &
             &5HTED  ,6(2H. ),A4,10X,20HUSER MODES FLAG SET ,10(2H. ),A4,/13X,24HSAVE REDUCTION PRODUCTS ,11(2H. ),A4,10X,7HEIGENVA,&
             &23HLUE EXTRACTION METHOD  ,5(2H. ),I8)
!
END SUBROUTINE mred1