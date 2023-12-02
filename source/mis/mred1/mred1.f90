!*==mred1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , casecc , cred , iblank , nheqss , nhloap , nhlods , no , yes
   INTEGER , DIMENSION(2) , SAVE :: cctype , letrs , modnam
   INTEGER , DIMENSION(6) , SAVE :: errnam
   LOGICAL :: errors
   INTEGER :: i , ibf , ibound , icode , ifile , iflag , ifree , imax , imode , imsg , irange , isil , itest , iuserm , j , k ,     &
            & modes , module , nmax , noieig , noread , nowdsc , nozwds , nrange , nwdsd , nwdsrd
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(32) :: lstbit
   INTEGER , DIMENSION(7) :: mtrlra , mtrlrb , mtrlrc , mtrlrd , mtrlre
   INTEGER , DIMENSION(16) , SAVE :: nmonic
   REAL , DIMENSION(2) :: range
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , close , cmiwrt , decode , fdsub , fwdrec , korsz , mesage , mred1a , mred1b , mred1c , mred1d , mred1e , open ,  &
          & orf , page1 , read , rshift , sfetch , smsg , sofcls , sofopn , softrl , suread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( Korlen<=Korbgn ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         CALL open(*20,casecc,Z(Gbuf2),0)
         IF ( Step/=0 ) THEN
            DO i = 1 , Step
               CALL fwdrec(*60,casecc)
            ENDDO
         ENDIF
!
!     READ CASECC AND EXTRACT DATA
!
         CALL read(*40,*60,casecc,Z(Korbgn),2,0,noread)
         IF ( Z(Korbgn)==cred ) module = 2
         nowdsc = Z(Korbgn+1)
         DO i = 1 , nowdsc , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL read(*40,*60,casecc,Z(Korbgn),3,0,noread)
!
!     TEST CASE CONTROL MNEMONICS
!
                  DO j = 1 , 16
                     IF ( Z(Korbgn)==nmonic(j) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CYCLE
               CASE (2)
!
!     SELECT DATA TO EXTRACT
!
                  IF ( j==2 ) THEN
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
                           range(2) = rz(Korbgn+2)
                        ELSE
                           nrange = 1
                           range(1) = rz(Korbgn+2)
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
                        Gprm = rz(Korbgn+2)
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
99001             FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE ',A1,'REDUCE OUTPUT ','COMMAND ',A4,'.  COMMAND IGNORED.')
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(casecc,1)
!
!     TEST MODULE OPERATION FLAG
!
         IF ( Dry<0 ) THEN
            IF ( Dry/=-2 ) THEN
               WRITE (Iprntr,99002) Uim
99002          FORMAT (A29,' 6630, FOR DRY OPTION IN MODAL REDUCE, INPUT DATA ','WILL BE CHECKED',/36X,                             &
                      &'BUT NO SOF TABLE ITEMS WILL BE ','CREATED.')
               Dry = -2
            ENDIF
         ELSEIF ( Dry==0 ) THEN
            Skipm = -1
            itest = 0
            CALL fdsub(Newnam,itest)
            IF ( itest/=-1 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            WRITE (Iprntr,99003) Ufm , letrs(module) , Newnam
99003       FORMAT (A23,' 6220, MODULE ',A1,'REDUCE - RUN EQUALS GO AND ','SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
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
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
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
99004       FORMAT (//38X,46HS U M M A R Y    O F    C U R R E N T    P R O,8H B L E M,//13X,                                       &
                   &38HNAME OF PSEUDOSTRUCTURE TO BE REDUCED ,4(2H. ),2A4,6X,40HNAME GIVEN TO RESULTANT PSEUDOSTRUCTURE ,2A4)
            IF ( ibf==0 ) WRITE (Iprntr,99005)
99005       FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),14X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ))
            IF ( ibf==1 ) WRITE (Iprntr,99006) Fixset
99006       FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),14X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ),I8)
            IF ( ibf==2 ) WRITE (Iprntr,99007) Bndset
99007       FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),I8,6X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ))
            IF ( ibf==3 ) WRITE (Iprntr,99008) Bndset , Fixset
99008       FORMAT (13X,36HBOUNDARY SET IDENTIFICATION NUMBER  ,5(2H. ),I8,6X,32HFIXED SET IDENTIFICATION NUMBER ,4(2H. ),I8)
            IF ( Rgrid(1)==-1 ) WRITE (Iprntr,99009) Rname
99009       FORMAT (13X,'RIGID BODY GRID POINT IDENTIFICATION NUMBER .',14X,'RIGID BODY SUBSTRUCTURE NAME ',5(2H. ),2A4)
            IF ( Rgrid(1)/=-1 ) WRITE (Iprntr,99010) Rgrid(1) , Rname
99010       FORMAT (13X,46HRIGID BODY GRID POINT IDENTIFICATION NUMBER . ,I8,6X,30HRIGID BODY SUBSTRUCTURE NAME  ,5(2H. ),2A4)
            IF ( noieig==no ) WRITE (Iprntr,99023) ibound , modes , ifree , imode , Irsave , Ieig
            IF ( noieig/=no ) WRITE (Iprntr,99023) ibound , modes , ifree , imode , Irsave
            IF ( imax==all ) WRITE (Iprntr,99011) imax , Gprm
99011       FORMAT (13X,42HMAXIMUM NUMBER OF FREQUENCIES TO BE USED  ,2(2H. ),A4,10X,14HGPARAM VALUE  ,13(2H. ),1P,E12.6)
            IF ( imax/=all ) WRITE (Iprntr,99012) nmax , Gprm
99012       FORMAT (13X,42HMAXIMUM NUMBER OF FREQUENCIES TO BE USED  ,2(2H. ),I8,6X,14HGPARAM VALUE  ,13(2H. ),1P,E12.6)
            IF ( irange==all ) WRITE (Iprntr,99013) Oldnam , irange
99013       FORMAT (13X,46HNAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ,2A4,6X,32HRANGE OF FREQUENCIES TO BE USED ,4(2H. ),A4)
            IF ( irange/=all ) WRITE (Iprntr,99014) Oldnam , range(1)
99014       FORMAT (13X,46HNAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ,2A4,6X,32HRANGE OF FREQUENCIES TO BE USED ,4(2H. ),1P,    &
                  & E12.6)
         ENDIF
!
!     CHECK FOR OLDMODES, OLDBOUND ERRORS
!
         errors = .FALSE.
         IF ( Ponly ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sfetch(Oldnam,errnam(1),3,itest)
         CALL softrl(Oldnam,errnam(2),mtrlra)
         CALL softrl(Oldnam,errnam(4),mtrlrb)
         CALL softrl(Oldnam,errnam(5),mtrlrc)
         CALL softrl(Oldnam,errnam(3),mtrlrd)
         CALL softrl(Oldnam,errnam(6),mtrlre)
         iflag = 1
         IF ( Usrmod ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Skipm>=0 ) THEN
!
!     OLDMODES NOT SET - PHIS, PHIL AND LAMS MUST BE DELETED
!
            IF ( itest<3 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
!
!     OLDMODES SET - PHIS AND LAMS MUST BE ON SOF
!
         ELSEIF ( itest>3 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         iflag = 2
         IF ( mtrlra(1)>2 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         iflag = 2
         IF ( mtrlra(1)<3 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         iflag = 3
         IF ( mtrlrd(1)<3 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     OLDBOUND SET - GIMS AND UPRT MUST BE ON SOF
!
         iflag = 4
         IF ( .NOT.Bounds ) THEN
!
!     OLDBOUND NOT SET - GIMS AND LMTX MUST BE DELETED
!
            IF ( mtrlrb(1)<3 ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( mtrlrb(1)>2 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         iflag = 6
         IF ( mtrlre(1)>2 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         iflag = 5
         IF ( mtrlrc(1)<3 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     TEST FOR ERRORS
!
         IF ( errors ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iuserm==2 ) WRITE (Iprntr,99015) Uim
99015    FORMAT (A29,' 6636, NMAX AND RANGE SUB COMMANDS ARE IGNORED ','UNDER USERMODES = TYPE 2.')
!
!     READ EQSS GROUP 0 DATA AND TEST OPEN CORE LENGTH
!
         itmnam(2) = Oldnam(2)
         CALL sfetch(Oldnam,nheqss,1,itest)
         IF ( itest==3 ) THEN
            imsg = -1
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            IF ( Korbgn+nwdsrd>=Sbuf3 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            IF ( Eqsind>=Sbuf3 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
99016             FORMAT (16X,5(2A4,2X),47X,1P,E12.6)
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
               IF ( Korbgn>=Sbuf3 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
                  CALL cmiwrt(1,Oldnam,Z(Namebs+j),Z(Eqsind+j),Z(Eqsind+j+1),rz,Z)
               ENDDO
               isil = 2*Nsil
               CALL cmiwrt(8,Oldnam,Oldnam,Nslbgn,isil,rz,Z)
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
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (9)
!
!     PHIS, LAMS DO NOT EXIST
!
         WRITE (Iprntr,99017) Ufm , errnam(iflag) , Oldnam
99017    FORMAT (A23,' 6617, OLDMODES SET AND REQUESTED SOF ITEM DOES NOT',' EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
         errors = .TRUE.
         IF ( iflag==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iflag==2 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!
!     PHIS, PHIR, LAMS NOT DELETED
!
         WRITE (Iprntr,99018) Ufm , errnam(iflag) , Oldnam
99018    FORMAT (A23,' 6618, OLDMODES NOT SET AND REQUESTED SOF ITEM MUST',' BE DELETED.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
         errors = .TRUE.
         IF ( iflag==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iflag==2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iflag==3 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
!     GIMS, UPRT DOES NOT EXIST
!
         WRITE (Iprntr,99019) Ufm , errnam(iflag) , Oldnam
99019    FORMAT (A23,' 6619, OLDBOUND SET AND REQUESTED SOF ITEM DOES NOT',' EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
         errors = .TRUE.
         IF ( iflag<=5 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
!     GIMS, LMTX NOT DELETED
!
         WRITE (Iprntr,99020) Ufm , errnam(iflag) , Oldnam
99020    FORMAT (A23,' 6620, OLDBOUND NOT SET AND REQUESTED SOF ITEM MUST',' BE DELETED.  ITEM ',A4,', SUBSTRUCTURE ',2A4,1H.)
         errors = .TRUE.
         iflag = iflag - 3
         IF ( iflag==1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iflag==2 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -1
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 14
      CASE (14)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (15)
         CALL smsg(imsg,nheqss,itmnam)
         RETURN
      CASE (16)
!
         CALL sofcls
         Dry = -2
         RETURN
      CASE (17)
!
!     CLOSE ANY OPEN FILES
!
         CALL sofcls
         IF ( Dry==-2 ) WRITE (Iprntr,99021) letrs(module)
99021    FORMAT (10H0  MODULE ,A1,36HREDUCE TERMINATING DUE TO ABOVE ERRO,3HRS.)
         IF ( Ponly ) Skipm = -1
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (16X,2A4,2X,2A4,2X,2A4,2X,2A4,2X,2A4)
99023 FORMAT (13X,18HOLDBOUND FLAG SET ,14(2H. ),A4,10X,12HOLDMODES FLA,6HG SET ,11(2H. ),A4,/13X,29HFREE BODY MODES TO BE CALCULA, &
             &5HTED  ,6(2H. ),A4,10X,20HUSER MODES FLAG SET ,10(2H. ),A4,/13X,24HSAVE REDUCTION PRODUCTS ,11(2H. ),A4,10X,7HEIGENVA,&
             &23HLUE EXTRACTION METHOD  ,5(2H. ),I8)
!
END SUBROUTINE mred1
