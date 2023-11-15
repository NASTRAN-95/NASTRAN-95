
SUBROUTINE rand5(Nfreq,Npsdl,Ntau,Xycb,Ltab,Ifile,Psdf,Auto,Nfile)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iz(1) , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Auto , Ltab , Nfile , Nfreq , Npsdl , Ntau , Psdf , Xycb
   INTEGER Ifile(1)
!
! Local variable declarations
!
   REAL data(100) , q(2) , r
   INTEGER file , i , iauto , ibuf1 , ibuf2 , ibuf3 , icdone , icore , icrq , ics , if , ii , ip , ip1 , ips , ipsdf , isaa , itau ,&
         & j , jj , k , kk , l , lcore , len , ll , llist , llists , load , mcb1(7) , mcb2(7) , name(2) , ndo , ndone , nload ,     &
         & npoint , nz , oldld
   INTEGER korsz
!
! End of declarations
!
!
!     THIS ROUTINE COMPUTES RANDOM RESPONSE FOR UNCOUPLED POWER SPECTRAL
!     DENSITY COEFFICIENTS
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
!
   DATA name , mcb1 , mcb2/4HRAND , 4H5    , 14*0/
   DATA ipsdf , iauto/4001 , 4002/
! *****
!     DEFINITION OF VARIABLES
! *****
!     NFREQ    NUMBER OF FREQUENCIES
!     NPSDL    NUMBER OF SUBCASES ON PSDL CARDS
!     NTAU     NUMBER OF TIMES
!     XYCB     DATA BLOCK CONTAINING XY USER REQUESTS
!     LTAB     LENGTH OF CORE  USED FOR TABLES BY PRETAB
!     IFILE    ARRY CONTAINING FILE NAMES FOR SORT 2 INPUT FILES
!     PSDF     OUTPUT FILE FOR POWER SPECTRAL DENSITY FUNCTIONS
!     AUTO     OUTPUT FILE FOR AUTOCORRELATION FUNCTIONS
!     NFILE    LENGTH OF IFILE ARRAY
!     MCB1     TRAILER FOR PSDF
!     MCB2     TRAILER FOR AUTO
!     IPSDF    OFP ID FOR  PSDF
!     IAUTO    OFP ID FOR  AUTO
!     LCORE    AVAIABLE CORE FOR ANY LIST
!     IBUF1    BUFFER POINTERS
!     IBUF2
!     IBUF3
!     ITAU     POINTER TO FIRST TAU -1
!     ISAA     POINTER TO FIRST S(AA) -1
!     TAU      TIMES FOR AUTTOCORRELATION
!     SAA      POWER SPECTRAL DENSITY FACTORS
!     ICORE    POINTER  TO FIRST REQUEST -1
!     SYSBUF   LENGTH OF ONE BUFFER
!     NPOINT   NUMBER OF REQUESTS
!     NZ       CORE AVAILABLE FOR STORING PSDF-S
!     IP       POINTER TO FIRST POINT OF CURRENT CORE LOAD
!     NDONE    NUMBER OF REQUESTS PROCESSED
!     OLDLD    LOAD ID OF OLD LOAD SET
!     NDO      NUMBER POSSIBLE TO DO IN CORE
!     ICS      POINTER TO FIRST PSDF VECTOR
!     NLOAD    NUMBER OF PSDL CARDS PROCESSED
!     ICDONE   NUMBER CURRENTLY DONE-- SEVERAL COMP FROM EACH VALUE
!     LOAD     SUBCASE ID FROM INPUT RECORD
!     IF       FORMAT FLAG  IF =0  DATA IS REA/IMAG IF.NE.0 MAG/PHASE
!     LEN      LENGTH OF DATA RECORD
!     Q        MEAN RESPONSE
!     R        AUTO CORRALATION FUNCTION AT TIME TAU
!     IP1      LOCAL POINT POINTER
!
!
! *****
!     CORE LAYOUT DURING EXECUTION
! *****
!     FREQUENCIES   NFREQ OF THEM
!     RANDPS DATA   NPSDL OF THEM  5 WORDS PER CARD
!                   LOAD ID   LOAD ID   X    Y=0. TABLE
!     TAUS          NTAU OF THEM
!     TABLE DATA    LTAB OF IT
!     S(AA)         NFREQ OF THEM  THESE ARE  REEVALUATED WHEN LOAD CHAG
!     REQUESTS      NPOINT OF THEM 5 WORDS PER REQUEST
!                   D.B. ID   COMP O.P. P/P
!     S(J,A)        NO  DO OF THEM      LENGTH = NFREQ
!
!
!     BUFFERS       3 NEEDED
!
!
!     INITALIZE GENERAL VARIABLES, ASSIGN BUFFERS  ETC
!
   mcb1(1) = Psdf
   mcb2(1) = Auto
   lcore = korsz(Z)
   ibuf1 = lcore - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   itau = Nfreq + 5*Npsdl
   isaa = Ntau + Ltab + itau
   icore = isaa + Nfreq
   lcore = lcore - icore - 3*Sysbuf
   icrq = -lcore
   IF ( lcore<=0 ) GOTO 600
!
!     OPEN OUTPUT FILES
!
   CALL gopen(Psdf,Z(ibuf2),1)
   CALL gopen(Auto,Z(ibuf3),1)
!
!     BEGIN LOOP ON EACH FILE
!
   DO i = 1 , Nfile
!
!     BUILD POINT LIST FOR FILE(I)
!
      CALL rand6(Xycb,Z(ibuf1),npoint,Iz(icore+1),Ifile(i),lcore)
      IF ( npoint==0 ) CYCLE
      nz = lcore - 5*npoint
      icrq = -nz
      IF ( nz<=0 ) GOTO 600
!
!     OPEN INPUT FILE
!
      file = Ifile(i)
      CALL open(*300,file,Z(ibuf1),0)
      ip = icore + 1
      ndone = 0
      oldld = 0
      ics = icore + 5*npoint + 1
      llist = 5*npoint
      ips = ip
      llists = llist
 50   ndo = min0(npoint-ndone,nz/Nfreq)
      icrq = max0(npoint-ndone,Nfreq)
      IF ( ndo==0 ) GOTO 600
      nload = 0
!
!     ZERO CORE
!
      jj = ics + ndo*Nfreq - 1
      DO k = ics , jj
         Z(k) = 0.0
      ENDDO
      icdone = 0
 100  DO
!
!     GET READY TO OBTAIN FIRST VALUE
!
         CALL rand2(Ifile(i),Iz(ip),load,if,len,llist)
!
!     CHECK FOR NEW LOAD
!
         IF ( load==0 ) THEN
            IF ( nload==Npsdl ) GOTO 150
            GOTO 200
         ELSE
            IF ( load==oldld ) EXIT
!
!     NEW LOAD --EVALUATE S(AA) FUNCTIONS FOR THIS LOAD
!
            j = Nfreq + 1
            jj = itau
            DO k = j , jj , 5
               IF ( Iz(k)==load ) GOTO 110
            ENDDO
!
!     LOAD NOT NEEDED --REJECT
!
            CYCLE
!
!     GOOD LOAD --EVALUATE
!
 110        oldld = load
            nload = nload + 1
            DO j = 1 , Nfreq
               jj = isaa + j
!
!                TAB      X    F(X)
               CALL tab(Iz(k+4),Z(j),Z(jj))
               IF ( Iz(k+4)==0 ) Z(jj) = 1.0
               Z(jj) = Z(jj)*Z(k+2)
            ENDDO
            EXIT
         ENDIF
      ENDDO
!
!     BRING IN DATA
!
      IF ( len>100 ) GOTO 500
      DO j = 1 , Nfreq
!
!     ACCESS DATA FROM FILE INTO DATA ARRAY
!
         CALL rand2a(data(1))
         ip1 = ip
         ii = icdone
         ll = isaa + j
         DO
!
!     COMPUTE  MAGNITUDE         OF CURRENT COMPONENT
!
            IF ( (len-2)/2<Iz(ip1+2) ) THEN
!
!     REQUEST OUT OF RANGE
!
               CALL mesage(52,Iz(ip1),Iz(ip1+1))
               Iz(ip1+2) = (len-2)/2
            ENDIF
            jj = Iz(ip1+2) + 2
            IF ( if==0 ) THEN
!
!     REAL + IMAGINARY
!
               k = jj + len/2 - 1
               data(jj) = sqrt(data(jj)*data(jj)+data(k)*data(k))
            ENDIF
!
!     COMPUTE POWER SPECTRAL DENSITY FUNCTION
!
            k = ics + ii*Nfreq - 1 + j
            Z(k) = Z(k) + data(jj)*Z(ll)*data(jj)
            IF ( ii==ndo-1 ) EXIT
!
!     IS NEXT REQUEST FROM SAME POINT
!
            IF ( Iz(ip1)/=Iz(ip1+5) .OR. Iz(ip1+1)/=Iz(ip1+6) ) EXIT
            ii = ii + 1
            ip1 = ip1 + 5
         ENDDO
      ENDDO
      llist = llist - 5*(ii-icdone+1)
      icdone = ii + 1
      ip = ip1 + 5
!     HAVE I DONE ALL REQUEST(IN CURRENT CORE)
!
      IF ( icdone/=ndo ) GOTO 100
!
!     HAVE I ADDED IN ALL LOADS
!
      ip = ips
      IF ( nload/=Npsdl ) THEN
!
!     START AGAIN ON NEXT LOAD
!
         llist = llists
         icdone = 0
         GOTO 100
      ENDIF
!
!     ALL LOADS FOR CURRENT BUNCH DONE
!
 150  jj = ip
      j = ndo*5 + jj - 1
      l = ics - Nfreq
      DO k = jj , j , 5
         l = l + Nfreq
!
!     COMPUTE MEAN RESPONSE   Q
!
         CALL rand3(Z(1),Z(l),q,Nfreq)
         IF ( Iz(k+3)/=2 ) THEN
!
!     PSDF REQUESTED
!
!     PUT OUT ID RECORD
!
            mcb1(7) = mcb1(7) + 1
            CALL rand1(Psdf,ipsdf,Iz(k),Iz(k+1),Iz(k+4),q)
!
!     PUT OUT DATA RECORD
!
            DO ll = 1 , Nfreq
               kk = l + ll - 1
               CALL write(Psdf,Z(ll),1,0)
               CALL write(Psdf,Z(kk),1,0)
            ENDDO
            CALL write(Psdf,0,0,1)
         ENDIF
         IF ( Iz(k+3)/=1 ) THEN
!
!     AUTOCORRELATION REQUESTED
!
            IF ( Ntau/=0 ) THEN
               CALL rand1(Auto,iauto,Iz(k),Iz(k+1),Iz(k+4),q)
               mcb2(7) = mcb2(7) + 1
!
!     PUT OUT DATA RECORD
!
               DO ll = 1 , Ntau
                  kk = itau + ll
                  CALL write(Auto,Z(kk),1,0)
!
!     COMPUTE AUTO
!
                  CALL rand4(Z(1),Z(l),Z(kk),r,Nfreq)
                  CALL write(Auto,r,1,0)
               ENDDO
               CALL write(Auto,0,0,1)
            ENDIF
         ENDIF
      ENDDO
      CALL rewind(Ifile(i))
      ndone = ndone + ndo
      IF ( ndone/=npoint ) THEN
!
!     SPILL ON POINT LISTS --GO AGAIN
!
         oldld = 0
         ip = ip + 5*ndo
         ips = ip
         llists = llists - 5*ndo
         GOTO 50
      ENDIF
 200  CALL close(Ifile(i),1)
 300  ENDDO
!
!     ALL STUFF DONE --GET OUT
!
   CALL close(Psdf,1)
   CALL close(Auto,1)
   CALL wrttrl(mcb1)
   CALL wrttrl(mcb2)
   RETURN
!
!     FILE + MISC ERRORS
!
 400  CALL mesage(ip1,file,name)
   RETURN
 500  ip1 = -7
   GOTO 400
 600  ip1 = -8
   file = icrq
   GOTO 400
END SUBROUTINE rand5
