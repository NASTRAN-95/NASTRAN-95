!*==rand5.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rand5(Nfreq,Npsdl,Ntau,Xycb,Ltab,Ifile,Psdf,Auto,Nfile)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nfreq
   INTEGER :: Npsdl
   INTEGER :: Ntau
   INTEGER :: Xycb
   INTEGER :: Ltab
   INTEGER , DIMENSION(1) :: Ifile
   INTEGER :: Psdf
   INTEGER :: Auto
   INTEGER :: Nfile
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(100) :: data
   INTEGER :: file , i , ibuf1 , ibuf2 , ibuf3 , icdone , icore , icrq , ics , if , ii , ip , ip1 , ips , isaa , itau , j , jj , k ,&
            & kk , l , lcore , len , ll , llist , llists , load , ndo , ndone , nload , npoint , nz , oldld
   INTEGER , SAVE :: iauto , ipsdf
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) , SAVE :: mcb1 , mcb2
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(2) :: q
   REAL :: r
   EXTERNAL close , gopen , korsz , mesage , open , rand1 , rand2 , rand2a , rand3 , rand4 , rand6 , rewind , tab , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE COMPUTES RANDOM RESPONSE FOR UNCOUPLED POWER SPECTRAL
!     DENSITY COEFFICIENTS
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA name , mcb1 , mcb2/4HRAND , 4H5    , 14*0/
   DATA ipsdf , iauto/4001 , 4002/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OPEN OUTPUT FILES
!
         CALL gopen(Psdf,Z(ibuf2),1)
         CALL gopen(Auto,Z(ibuf3),1)
!
!     BEGIN LOOP ON EACH FILE
!
         DO i = 1 , Nfile
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
!     BUILD POINT LIST FOR FILE(I)
!
                  CALL rand6(Xycb,Z(ibuf1),npoint,iz(icore+1),Ifile(i),lcore)
                  IF ( npoint==0 ) CYCLE
                  nz = lcore - 5*npoint
                  icrq = -nz
                  IF ( nz<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     OPEN INPUT FILE
!
                  file = Ifile(i)
                  CALL open(*20,file,Z(ibuf1),0)
                  ip = icore + 1
                  ndone = 0
                  oldld = 0
                  ics = icore + 5*npoint + 1
                  llist = 5*npoint
                  ips = ip
                  llists = llist
                  spag_nextblock_2 = 2
               CASE (2)
                  ndo = min0(npoint-ndone,nz/Nfreq)
                  icrq = max0(npoint-ndone,Nfreq)
                  IF ( ndo==0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nload = 0
!
!     ZERO CORE
!
                  jj = ics + ndo*Nfreq - 1
                  DO k = ics , jj
                     Z(k) = 0.0
                  ENDDO
                  icdone = 0
                  spag_nextblock_2 = 3
               CASE (3)
                  SPAG_Loop_2_1: DO
!
!     GET READY TO OBTAIN FIRST VALUE
!
                     CALL rand2(Ifile(i),iz(ip),load,if,len,llist)
!
!     CHECK FOR NEW LOAD
!
                     IF ( load==0 ) THEN
                        IF ( nload/=Npsdl ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSE
                        IF ( load==oldld ) EXIT SPAG_Loop_2_1
!
!     NEW LOAD --EVALUATE S(AA) FUNCTIONS FOR THIS LOAD
!
                        j = Nfreq + 1
                        jj = itau
                        DO k = j , jj , 5
                           IF ( iz(k)==load ) GOTO 2
                        ENDDO
!
!     LOAD NOT NEEDED --REJECT
!
                        CYCLE
!
!     GOOD LOAD --EVALUATE
!
 2                      oldld = load
                        nload = nload + 1
                        DO j = 1 , Nfreq
                           jj = isaa + j
!
!                TAB      X    F(X)
                           CALL tab(iz(k+4),Z(j),Z(jj))
                           IF ( iz(k+4)==0 ) Z(jj) = 1.0
                           Z(jj) = Z(jj)*Z(k+2)
                        ENDDO
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
!
!     BRING IN DATA
!
                  IF ( len>100 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO j = 1 , Nfreq
!
!     ACCESS DATA FROM FILE INTO DATA ARRAY
!
                     CALL rand2a(data(1))
                     ip1 = ip
                     ii = icdone
                     ll = isaa + j
                     SPAG_Loop_3_2: DO
!
!     COMPUTE  MAGNITUDE         OF CURRENT COMPONENT
!
                        IF ( (len-2)/2<iz(ip1+2) ) THEN
!
!     REQUEST OUT OF RANGE
!
                           CALL mesage(52,iz(ip1),iz(ip1+1))
                           iz(ip1+2) = (len-2)/2
                        ENDIF
                        jj = iz(ip1+2) + 2
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
                        IF ( ii==ndo-1 ) EXIT SPAG_Loop_3_2
!
!     IS NEXT REQUEST FROM SAME POINT
!
                        IF ( iz(ip1)/=iz(ip1+5) .OR. iz(ip1+1)/=iz(ip1+6) ) EXIT SPAG_Loop_3_2
                        ii = ii + 1
                        ip1 = ip1 + 5
                     ENDDO SPAG_Loop_3_2
                  ENDDO
                  llist = llist - 5*(ii-icdone+1)
                  icdone = ii + 1
                  ip = ip1 + 5
!     HAVE I DONE ALL REQUEST(IN CURRENT CORE)
!
                  IF ( icdone/=ndo ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
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
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (4)
!
!     ALL LOADS FOR CURRENT BUNCH DONE
!
                  jj = ip
                  j = ndo*5 + jj - 1
                  l = ics - Nfreq
                  DO k = jj , j , 5
                     l = l + Nfreq
!
!     COMPUTE MEAN RESPONSE   Q
!
                     CALL rand3(Z(1),Z(l),q,Nfreq)
                     IF ( iz(k+3)/=2 ) THEN
!
!     PSDF REQUESTED
!
!     PUT OUT ID RECORD
!
                        mcb1(7) = mcb1(7) + 1
                        CALL rand1(Psdf,ipsdf,iz(k),iz(k+1),iz(k+4),q)
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
                     IF ( iz(k+3)/=1 ) THEN
!
!     AUTOCORRELATION REQUESTED
!
                        IF ( Ntau/=0 ) THEN
                           CALL rand1(Auto,iauto,iz(k),iz(k+1),iz(k+4),q)
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
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 5
               CASE (5)
                  CALL close(Ifile(i),1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
 20      ENDDO
!
!     ALL STUFF DONE --GET OUT
!
         CALL close(Psdf,1)
         CALL close(Auto,1)
         CALL wrttrl(mcb1)
         CALL wrttrl(mcb2)
         RETURN
      CASE (2)
!
!     FILE + MISC ERRORS
!
         CALL mesage(ip1,file,name)
         RETURN
      CASE (3)
         ip1 = -7
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         ip1 = -8
         file = icrq
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rand5
