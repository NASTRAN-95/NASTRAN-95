
SUBROUTINE rand8(Nfreq,Npsdl,Ntau,Xycb,Ltab,Ifile,Psdf,Auto,Nfile)
   IMPLICIT NONE
   REAL Degrad , Pi , Radeg , S4pisq , Twopi , Z(1)
   INTEGER Iz(1) , Sysbuf
   COMMON /condas/ Pi , Twopi , Radeg , Degrad , S4pisq
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER Auto , Ltab , Nfile , Nfreq , Npsdl , Ntau , Psdf , Xycb
   INTEGER Ifile(1)
   REAL data(100) , q(2) , r , two , x
   INTEGER file , i , iauto , ibuf1 , ibuf2 , ibuf3 , icdone , icore , icrq , ics , if , ih1 , ih2 , ii , iload , ip , ip1 ,        &
         & ipsave , ipsdf , isaa , isj , itau , j , jj , k , kk , l , l1 , l2 , lcore , len , ll , llist , llists , load , m ,      &
         & mcb1(7) , mcb2(7) , mincr , name(2) , ndo , ndone , nload , npoint , nunq , nz , oldld
   INTEGER korsz
!
!     THIS ROUTINE COMPUTES RANDOM RESPONSE FOR COUPLED POWER SPECTRAL
!       DENSITY COEFICIENTS
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
!
   DATA name , mcb1 , mcb2/4HRAND , 4H8    , 14*0/
   DATA ipsdf , iauto/4001 , 4002/
! *****
!     DEFINITION OF VARIABLES
! *****
!     NFREQ    NUMBER OF FREQUENCIES
!     NPSDL    NUMBER OF PSDL  CARDS
!     NTAU     NUMBER OF TIMES
!     XYCB     DATA BLOCK CONTAINING XY USER REQUESTS
!     LTAB     LENGTH OF CORE USED FOR TABLES BY PRETAB
!     IFILE    ARRAY CONTAINING FILE NAMES FOR SORT 2 INPUT FILES
!     PSDF     OUTPUT FILE FOR POWER SPECTRAL DENSITY FUNCTIONS
!     AUTO     OUTPUT FILE FOR AUTOCORRELATION FUNCTIONS
!     NFILE    LENGTH OF IFILE ARRAY
!     MCB1     TRAILER FOR PSDF
!     MCB2     TRAILER FOR AUTO
!     IPSDF    OFP ID FOR PSDF
!     IAUTO    OFP ID FOR AUTO
!     LCORE    AVAILABLE CORE FOR  LISTS
!     IBUF1    BUFFER POINTERS
!     IBUF2
!     IBUF3
!     ITAU     POINTER TO FIRST TAU-1
!     ISAA     POINTER TO SAB TABLE -1
!     TAU      TIMES FOR AUTOCORRELATION
!     SAB      POWER SPECTRAL DENSITY FACTORS
!     ICORE    POINTER TO FIRST REQUEST-1
!     SYSBUF   LENGTH OF ONE BUFFER
!     NPOINT   TOTAL NUMBER OF REQUESTS
!     NZ       CORE AVAIABLE FOR STORING H VALUES
!     IP       POINTER TO FIRST POINT OF CURRENT CORE LOAD
!     NDONE    NUMBER OF REQUESTS PROCESSED
!     OLDLD    LOAD ID OF OLD LOAD SET
!     NDO      NUMBER POSSIBLE TO DO IN CORE
!     ICS      POINTER TO FIRST H ARRAY
!     NLOAD    NUMBER OF LOADS      PROCESSED ON CURRENT CORE LOAD
!     ICDONE   NUMBER CURRENTLY DONE -- SEVERAL COMP FROM EACH VALUE
!     LOAD     SUBCASE ID FROM INPUT RECORD
!     IF       FORMAT FLAG IF=0  DATA IS REAL/IMAG  IF .NE. 0 MAG/PHASE
!     LEN      LENGTH OF DATA RECORD
!     Q        MEAN  RESPONSE
!     R        AUTOCORRALATION FUNCTION AT TIME TAU
!     IP1      LOCAL POINT POINTER
!     NUNQ     NUMBER OF UNIQUE LOAD ID-S
!     ILOAD    POINTER TO LOAD LIST-1
!     ISJ      POINTER TO SJ ADD AREA-1
!     ICS      H STORAGE -1
!
!
!
! *****
!     CORE LAYOUTDURING EXECUTION
! *****
!     FREQUENCIES   NFREQ OF THEM
!     RANDPS DATA   NPSDL OF THEM  5 WORDS PER CARD
!                   LOAD ID  LOAD ID   X   Y   TABLE
!     TAUS          NTAU OF THEM
!     TABLE DATA    LTAB OF IT
!     S(AB)         NFREQ OF THEM-- THESE ARE REEVALUATED WHEN LOAD CHAN
!     UNIQUE ID-S   NUNQ  OF THEM
!     REQUESTS      NPOINT OF THEM 5 WORDS PER REQUEST
!                   DB   ID   COMP O.P. P/P
!     H-S           LENGTH = 2*NFREQ --REAL+IMAGINARY
!                   NUNQ  H-S PER SET-- NDO SETS
!     SJ COMPUTE    NFREQ OF IT
!
!
!     BUFFERS       3 NEEDED
!
!
!
!
!     INITIALIZE GENERAL VARIABLES--ASSIGN BUFFERS,ETC
!
   mcb1(1) = Psdf
   mcb2(1) = Auto
   lcore = korsz(Z)
   ibuf1 = lcore - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   itau = Nfreq + 5*Npsdl
   isaa = Ntau + Ltab + itau
   lcore = lcore - (isaa+Nfreq+3*Sysbuf)
   icrq = -lcore
   IF ( lcore<=0 ) GOTO 700
!
!     BUILD LIST OF UNIQUE LOAD ID-S
!         REPLACE LOAD ID OF PSDL WITH POINTER TO LIST
!
   nunq = 0
   iload = isaa + Nfreq
   m = iload + 1
   k = m - 1
   i = Nfreq + 1
   jj = itau + 1
   j = 1
   GOTO 200
!
!         SEARCH LIST OF UNIQUE ID-S
!
 100  DO l = m , k
      IF ( Iz(i)==Iz(l) ) GOTO 300
   ENDDO
!
!         SAVE LOAD ID
!
 200  k = k + 1
   nunq = nunq + 1
   Iz(k) = Iz(i)
   l = k
!
!         REPLACE ID WITH POINTER INTO LIST
!
 300  Iz(i) = l - m + 1
!
!         NEXT PSDL CARD
!
   IF ( j==0 ) THEN
!
!         NEXT PSDL CARD
!
      i = i + 4
      j = 1
      IF ( i/=jj ) GOTO 100
!
!         COMPUTE MINIMUM CORE
!
      mincr = nunq*Nfreq*2 + Nfreq
      icore = iload + nunq
      lcore = lcore - nunq
      icrq = mincr - lcore
      IF ( lcore<=mincr ) GOTO 700
!
!         OPEN OUTPUT FILES
!
      CALL gopen(Psdf,Z(ibuf2),1)
      CALL gopen(Auto,Z(ibuf3),1)
!
!         BEGIN LOOP ON EACH FILE
!
      DO i = 1 , Nfile
!
!         BUILD POINT LIST FOR FILE(I)
!
         CALL rand6(Xycb,Z(ibuf1),npoint,Iz(icore+1),Ifile(i),lcore)
         IF ( npoint==0 ) CYCLE
         nz = lcore - 5*npoint
         icrq = -nz
         IF ( nz<=0 ) GOTO 700
!
!         OPEN INPUT FILE
!
         file = Ifile(i)
         CALL open(*400,file,Z(ibuf1),0)
         ip = icore + 1
         ndone = 0
         oldld = 0
         ics = icore + 5*npoint
         llist = 5*npoint
!
!         COMPUTE NUMBER OF POINTS TO DO AT SAME TIME
!
 320     ndo = min0(npoint-ndone,nz/mincr)
         icrq = max0(npoint-ndone,mincr)
         IF ( ndo==0 ) GOTO 700
         llists = llist
         icdone = 0
         ipsave = ip
         nload = 0
 340     DO
!         GET READY TO OBTAIN FIRST VALUE
!
            CALL rand2(Ifile(i),Iz(ip),load,if,len,llist)
            IF ( load==0 ) GOTO 360
!
!         CHECK FOR NEW LOAD
!
            IF ( load==oldld ) EXIT
!
!         NEW LOAD -- SEE IF WANTED
!
            DO kk = 1 , nunq
               l = iload + kk
               IF ( load==Iz(l) ) GOTO 350
            ENDDO
!
!         REJECT LOAD -- NOT NEEDED
!
            CYCLE
!
!         GOOD LOAD -- SAVE DATA
!
 350        oldld = load
!
!         BRING DATA INTO KK-TH H SAVE AREA
!
            kk = ics + (kk-1)*Nfreq*2
            EXIT
         ENDDO
         IF ( len>100 ) GOTO 600
         DO j = 1 , Nfreq
!
!     ACCESS DATA FROM FILE  INTO DATA  ARRAY
!
            CALL rand2a(data(1))
            ip1 = ip
            ii = icdone
            DO
!
!         COMPUTE REAL/IMAG OF CURRENT COMPONENT
!
               IF ( (len-2)/2<Iz(ip1+2) ) THEN
!
!     REQUEST OUT OF RANGE
!
                  CALL mesage(52,Iz(ip1),Iz(ip1+1))
                  Iz(ip1+2) = (len-2)/2
               ENDIF
               jj = Iz(ip1+2) + 2
               k = jj + len/2 - 1
               IF ( if>0 ) THEN
                  x = data(jj)*cos(Degrad*data(k))
                  data(k) = data(jj)*sin(Degrad*data(k))
                  data(jj) = x
               ENDIF
               l = kk + j*2 - 1 + ii*mincr
               Z(l) = data(jj)
               Z(l+1) = data(k)
!
!         TEST FOR CORE OVERFLOW
!
               IF ( ii==ndo-1 ) EXIT
!
!         IS NEXT REQUEST FROM SAME POINT
!
               IF ( Iz(ip1)/=Iz(ip1+5) .OR. Iz(ip1+1)/=Iz(ip1+6) ) EXIT
               ii = ii + 1
               ip1 = ip1 + 5
            ENDDO
         ENDDO
         icdone = ii + 1
         ip = ip1 + 5
         llist = llist - 5*icdone
!
!         HAVE I DONE ALL REQUESTS (IN CURRENT CORE)
!
         IF ( icdone/=ndo ) GOTO 340
!
!         HAVE I ADDED IN ALL LOADS
         nload = nload + 1
         ip = ipsave
         IF ( nload==nunq ) THEN
!
!         ALL LOADS FOR CURRENT BUNCH DONE
!              COMPUTE SJ-S
!
!              ZERO ALL SJ-S
!
            DO j = 1 , ndo
               k = ics + j*mincr - Nfreq
               DO l = 1 , Nfreq
                  jj = k + l
                  Z(jj) = 0.0
               ENDDO
            ENDDO
!
!         FOR EACH PSDL CARD  1. EVALUATE SAB
!              FOR EACH POINT
!                   IN CORE   2. COMPUTE 2*RE(HI*SIJ*HJBAR)
!                             3. ADD TO SJ AT EACH FREQ.
!
            DO j = 1 , Npsdl
!
!         EVALUATE SAB
!
               two = 2.0
               l = Nfreq + (j-1)*5
               IF ( Iz(l+1)==Iz(l+2) ) two = 1.0
               q(1) = Z(l+3)
               r = Z(l+4)
               DO k = 1 , Nfreq
                  jj = isaa + k
!
!
!                TAB     X    F(X)
                  CALL tab(Iz(l+5),Z(k),Z(jj))
                  IF ( Iz(l+5)==0 ) Z(jj) = 1.0
               ENDDO
!
!         FOR EACH POINT IN CORE
!
               DO k = 1 , ndo
                  l2 = ics + k*mincr - Nfreq
                  l1 = ics + (k-1)*mincr - 1 - Nfreq*2
                  DO m = 1 , Nfreq
                     ih1 = Iz(l+1)*Nfreq*2 + l1 + 2*m
                     ih2 = Iz(l+2)*Nfreq*2 + l1 + 2*m
                     jj = isaa + m
                     isj = l2 + m
                     Z(isj) = Z(isj) + Z(jj)*two*((Z(ih1)*q(1)-Z(ih1+1)*r)*Z(ih2)+(Z(ih1+1)*q(1)+Z(ih1)*r)*Z(ih2+1))
                  ENDDO
               ENDDO
            ENDDO
!
!         OUTPUT STUFF IN CORE
!
            jj = ip
            j = ndo*5 + jj - 1
            l = ics - Nfreq
            DO k = jj , j , 5
               l = l + mincr
!
!         CONVERT SJ TO ABSOLUTE VALUE
!
               DO ll = 1 , Nfreq
                  kk = l + ll
                  Z(kk) = abs(Z(kk))
               ENDDO
!
!         COMPUTE MEAN RESPONSE
!
               CALL rand3(Z(1),Z(l+1),q,Nfreq)
               IF ( Iz(k+3)/=2 ) THEN
!
!         PSDF REQUESTED -- PUT OUT ID
!
                  mcb1(7) = mcb1(7) + 1
                  CALL rand1(Psdf,ipsdf,Iz(k),Iz(k+1),Iz(k+4),q)
!
!         PUT OUT DATA RECORDED
!
                  DO ll = 1 , Nfreq
                     kk = l + ll
                     CALL write(Psdf,Z(ll),1,0)
                     CALL write(Psdf,Z(kk),1,0)
                  ENDDO
                  CALL write(Psdf,0,0,1)
               ENDIF
               IF ( Iz(k+3)/=1 ) THEN
!
!         AUTO CORRELATION REQUESTED
!
                  IF ( Ntau/=0 ) THEN
                     CALL rand1(Auto,iauto,Iz(k),Iz(k+1),Iz(k+4),q)
                     mcb2(7) = mcb2(7) + 1
!
!         PUT OUT DATA RECORD
!
                     DO ll = 1 , Ntau
                        kk = itau + ll
                        CALL write(Auto,Z(kk),1,0)
!
!         COMPUTE AUTO
!
                        CALL rand4(Z(1),Z(l+1),Z(kk),r,Nfreq)
                        CALL write(Auto,r,1,0)
                     ENDDO
                     CALL write(Auto,0,0,1)
                  ENDIF
               ENDIF
            ENDDO
!
!         END CORE LOAD
!
            CALL rewind(Ifile(i))
            ndone = ndone + ndo
            IF ( ndone/=npoint ) THEN
!
!         SPILL ON POINT LISTS -- GO AGAIN
!
               oldld = 0
               llist = llists - 5*ndo
               ip = ipsave + 5*ndo
               GOTO 320
            ENDIF
         ELSE
!
!         START AGAIN ON NEXT LOAD
            llist = ndo*5
            icdone = 0
            GOTO 340
         ENDIF
!
!         FINISHED WITH FILE
!
 360     CALL close(Ifile(i),1)
 400  ENDDO
!
!         ALL STUFF DONE -- GET OUT
!
      CALL close(Psdf,1)
      CALL close(Auto,1)
      CALL wrttrl(mcb1)
      CALL wrttrl(mcb2)
      RETURN
   ELSE
      i = i + 1
      j = 0
      GOTO 100
   ENDIF
!
!         FILE + MISC ERRORS
!
 500  CALL mesage(ip1,file,name)
   RETURN
 600  ip1 = -7
   GOTO 500
 700  ip1 = -8
   file = icrq
   GOTO 500
END SUBROUTINE rand8
