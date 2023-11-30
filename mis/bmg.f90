
SUBROUTINE bmg
   IMPLICIT NONE
   INTEGER Cls , Clsrew , Iout , Iprec , Iskp(52) , Kflags(2) , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   REAL Consts(5) , Degrad , Rz(1) , Twopi , Value(2)
   DOUBLE PRECISION Dz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Kflags , Value
   COMMON /condas/ Consts
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Iout , Iskp , Iprec
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION ain , dtemp(3) , dub , t0f(9) , term(3) , ti(9) , vi(3)
   REAL angle , cosphi , fn , kii , phil1 , phip1 , rbuf(10) , sinphi
   INTEGER bdpool , bgpdt , bndfl(2) , buf(10) , buf1 , buf2 , buf3 , core , cstm , dmig(3) , entrys , eor , eqexin , file , flag , &
         & form , i , ibgpdt , icstm , idata , idf , ik , in , inn , is , itable , iz2 , iz6 , iz7 , iz8 , iz9 , j , k , kid , l ,  &
         & lbgpdt , lcstm , matpol , mones(3) , n , n2 , n4 , nabfl(2) , nbgpdt , ncstm , ndata , nextid , nkbfl(2) , nn , nnn ,    &
         & noeor , ntable , point , scrt1 , subr(2)
   LOGICAL head , labfl , lkbfl , nstar
   INTEGER korsz
!
!     HYDROELASTIC BOUNDARY MATRIX GENERATOR
!
!     7/12/73 NO AXIAL SYMMETRY UPPER INTEGRATION LIMIT OF LAST
!             CIRCUMFERENTIAL GRID IS INCORRECT
!
   !>>>>EQUIVALENCE (Consts(2),Twopi) , (Consts(4),Degrad) , (Z(1),Rz(1),Dz(1)) , (buf(1),rbuf(1))
   DATA subr/4HBMG  , 4H    / , nabfl/4HABFL , 4H    /
   DATA bndfl/9614 , 96/ , mones/ - 1 , -1 , -1/
   DATA is/1/ , dmig/114 , 1 , 120/
   DATA eor , noeor/1 , 0/ , nkbfl/4HKBFL , 4H    /
   DATA matpol , bgpdt , eqexin , cstm/101 , 102 , 103 , 104/
   DATA bdpool/201/ , scrt1/301/
   DATA iz2 , iz6 , iz7 , iz8 , iz9/2 , 6 , 7 , 8 , 9/
!
!     DEFINE CORE AND BUFFER POINTERS
!
   core = korsz(Z)
   buf1 = core - Sysbuf - 2
   buf2 = buf1 - Sysbuf - 2
   buf3 = buf2 - Sysbuf - 2
   core = buf3 - 1
   IF ( core<100 ) CALL mesage(-8,0,subr)
   Kflags(1) = -1
   Kflags(2) = -1
!
!     OPEN MATPOOL AND LOCATE THE BNDFL RECORD AS PREPARED BY IFP4.
!
   CALL preloc(*1000,Z(buf1),matpol)
   CALL locate(*1000,Z(buf1),bndfl,flag)
!
!     THIS MODULE DOES NOTHING IF THE MATPOOL IS PURGED OR THE BNDFL
!     RECORD IS ABSENT.  NOW READ THE HEADER DATA OF THIS RECORD.
!
   file = matpol
   CALL read(*1200,*1300,matpol,Z(1),9,noeor,flag)
   in = 10
   nn = Z(iz9) + 9
   IF ( nn+5>core ) CALL mesage(-8,0,subr)
!
!     READ THE INDICES
!
   CALL read(*1200,*1300,matpol,Z(in),Z(iz9),noeor,flag)
   Value(1) = Z(iz6)
   Value(2) = 0.0
   IF ( Z(iz6)==0 ) Value(1) = 1.0
!
!     MODIFY LIST OF INDICES TO FIT THE FOLLOWING TABLE
!
!        M      S1    S2         N              N*
!        -      --    --         -              --
!        0                      ALL             ALL
!
!                               K M
!     .GE.2      S     S        ---             NONE
!                                2
!
!                             (2K+1)M
!     .GE.2      S     A      -------           NONE
!                                4
!
!                                              (2K+1)M
!     .GE.2      A     S        NONE           -------
!                                                 4
!
!                                               K M
!     .GE.2      A     A        NONE            ---
!                                                2
!
!     K MAY BE 0,1,2,..... IN ORDER TO CHECK INDICE FOR MATCH.
!
   IF ( Z(iz6)>0 ) THEN
!
!     M IS POSITIVE THUS CHECK FOR STAR OR NO-STAR INDICES PERMITTED.
!     DETERMINE THE FORM OF THE CHECK EQUATION.
!
!          Z(7) = S1
!          Z(8) = S2
!          Z(6) = M
!
      IF ( Z(iz7)==is ) THEN
         nstar = .FALSE.
      ELSE
         nstar = .TRUE.
      ENDIF
      IF ( Z(iz7)==Z(iz8) ) THEN
         form = -1
      ELSE
         form = 1
      ENDIF
!
!     NOW FORM NEW LIST OF INDICES
!
      inn = nn + 1
      nnn = nn
      DO i = in , nn
         n = (Z(i)-1)/2
         IF ( mod(Z(i),2)/=0 ) THEN
!
!     STAR CASE
!
            IF ( .NOT.nstar ) CYCLE
            IF ( form>=0 ) GOTO 40
         ELSE
!
!     NON-STAR CASE
!
            IF ( nstar ) CYCLE
            IF ( form>=0 ) GOTO 40
         ENDIF
!
!                           K M
!     CHECK USING EQUATION  ---
!                            2
!
         n2 = n*2
         k = n2/Z(iz6)
         IF ( k*Z(iz6)/=n2 ) CYCLE
!
!     GOOD INDICE,  ADD IT TO THE LIST
!
 20      nnn = nnn + 1
         Z(nnn) = Z(i)
         CYCLE
!
!                            (2K+1)M
!     CHECK USING EQUATION   -------
!                               4
!
 40      n4 = n*4
         ik = n4/Z(iz6)
         ik = ik - 1
         k = ik/2
         IF ( (2*k+1)*Z(iz6)==n4 ) GOTO 20
      ENDDO
!
!     LIST IS COMPLETE
!
      in = inn
      nn = nnn
   ENDIF
   labfl = .TRUE.
   IF ( nn<in ) labfl = .FALSE.
!
!     SET LKBFL AS A FLAG INDICATING WHETHER KBFL WILL BE GENERATED
!     ALONG WITH ABFL.  IF G IS NON-ZERO THEN KBFL WILL BE GENERATED.
!
   lkbfl = .TRUE.
   IF ( Rz(iz2)==0.0 ) lkbfl = .FALSE.
   IF ( lkbfl ) Kflags(1) = 0
   IF ( labfl ) Kflags(2) = 0
   IF ( .NOT.labfl .AND. .NOT.lkbfl ) GOTO 1000
!
!     BGPDT IS NOW READ INTO CORE AS 5 WORD ENTRIES, RESERVING FIRST
!     WORD FOR THE EXTERNAL ID.
!
   file = bgpdt
   ibgpdt = nn + 1
   nbgpdt = nn
   CALL gopen(bgpdt,Z(buf2),Rdrew)
   DO
      CALL read(*1200,*100,bgpdt,Z(nbgpdt+2),4,noeor,flag)
      nbgpdt = nbgpdt + 5
      IF ( nbgpdt+5>core ) CALL mesage(-8,0,subr)
   ENDDO
 100  CALL close(bgpdt,Clsrew)
!
!     READ EQEXIN PLACING EXTERNAL ID ON RESPECTIVE BGPDT ENTRY.
!
   file = eqexin
   CALL gopen(eqexin,Z(buf2),Rdrew)
   DO
      CALL read(*1200,*200,eqexin,buf,2,noeor,flag)
      n = 5*buf(2) - 5 + ibgpdt
      Z(n) = buf(1)
   ENDDO
 200  CALL close(eqexin,Clsrew)
   lbgpdt = nbgpdt - ibgpdt + 1
   entrys = lbgpdt/5
!
!     SORT THE BGPDT ON EXTERNAL ID
!
   CALL sort(0,0,5,1,Z(ibgpdt),lbgpdt)
!
!  BLAST CSTM INTO CORE
!
   file = cstm
   CALL gopen(cstm,Z(buf2),Rdrew)
   icstm = nbgpdt + 1
   CALL read(*1200,*300,cstm,Z(icstm),core-icstm,noeor,flag)
   CALL mesage(-8,0,subr)
 300  ncstm = icstm + flag - 1
   lcstm = ncstm - icstm + 1
   CALL close(cstm,Clsrew)
!
!     LOCATE THE T   MATRIX IN THE CSTM DATA BY USING CSID = CDF IN
!                 0F
!
!     THE HEADER DATA.      ( Z(1) )
!
   DO i = icstm , ncstm , 14
      IF ( Z(1)==Z(i) ) GOTO 400
   ENDDO
   WRITE (Iout,99001) Sfm , Z(1)
99001 FORMAT (A25,' 4060, COORDINATE SYSTEM =',I9,' CAN NOT BE FOUND IN CSTM DATA.')
   GOTO 1500
 400  n = i + 5
   DO i = 1 , 9
      t0f(i) = dble(Rz(n))
      n = n + 1
   ENDDO
!
!     OPEN BDPOOL FOR ABFL, AND SCRATCH1 FOR KBFL AND WRITE THE DMIG
!     HEADER INFORMATION.
!
   CALL gopen(bdpool,Z(buf2),Wrtrew)
!
!     WRITE DMIG RECORD ID
!
   CALL write(bdpool,dmig,3,noeor)
   buf(1) = nabfl(1)
   buf(2) = nabfl(2)
   buf(3) = 0
   buf(4) = 1
   buf(5) = 1
   buf(6) = Iprec
   buf(7) = 0
   buf(8) = 0
   buf(9) = 0
   IF ( labfl ) CALL write(bdpool,buf,9,noeor)
   IF ( lkbfl ) THEN
      file = scrt1
      CALL open(*1100,scrt1,Z(buf3),Wrtrew)
      buf(1) = nkbfl(1)
      buf(2) = nkbfl(2)
      CALL write(scrt1,buf,9,noeor)
   ENDIF
!
!     READ SOME FLUID-PT DATA (IDF,R,Z,L,C,S,RHO)
!
   file = matpol
   CALL read(*1200,*1300,matpol,idf,1,noeor,flag)
 500  idata = ncstm + 1
   ndata = ncstm + 6
   CALL read(*1200,*1300,matpol,Z(idata),6,noeor,flag)
!
!     START BUILDING TABLE OF CONNECTED GRID POINTS.
!     READ ID,PHI.  CREATE A 26 WORD ENTRY FOR EACH ID,PHI.
!
   itable = ndata + 1
!
!     INSURE THAT TABLE STARTS ON AN EVEN BOUNDARY FOR DOUBLE
!     PRECISION
!
   IF ( mod(itable,2)/=1 ) itable = itable + 1
   ntable = itable - 1
   DO
      CALL read(*1200,*1300,matpol,Z(ntable+1),2,noeor,flag)
      IF ( Z(ntable+1)==-1 ) THEN
!
!     COMPUTATION AND INSERTION OF PHI   AND PHI   FOR EACH ENTRY.
!                                     0         1
!
         DO i = itable , ntable , 26
!
!     SET UP PHI  IN THIRD SLOT OF ENTRY = (PHI  + PHI   )/2.0
!               0                              I      I-1
!
            IF ( i/=itable ) THEN
               phil1 = Rz(i-25)
!
!     SPECIAL CASE ON FIRST POINT, TEST M TO FIND PHI
!                                                    I-1
!
            ELSEIF ( Z(iz6)>1 ) THEN
               phil1 = Rz(itable+1)
            ELSE
               phil1 = Rz(ntable-24) - Twopi
            ENDIF
            Rz(i+2) = (Rz(i+1)+phil1)/2.0
!
!     SET UP PHI  IN FOURTH SLOT OF ENTRY = (PHI  + PHI   )/2.0
!               1                               I      I+1
!
            IF ( i/=ntable-25 ) THEN
               phip1 = Rz(i+27)
!
!     SPECIAL CASE ON LAST POINT, TEST M TO FIND PHI
!                                                   I+1
!
            ELSEIF ( Z(iz6)>1 ) THEN
               phip1 = Rz(ntable-24)
            ELSE
               phip1 = Rz(itable+1) + Twopi
            ENDIF
            Rz(i+3) = (Rz(i+1)+phip1)/2.0
         ENDDO
!
!     PICK UP NEXT FLUID POINT IDF
!
         nextid = 0
         CALL read(*1200,*700,matpol,nextid,1,noeor,flag)
         IF ( nextid/=idf ) GOTO 700
!
!     NEXTID IS SAME AS CURRENT IDF, THUS ADD ANOTHER ENTRY OF R,Z,L,C,
!     S,RH FIRST MOVE SINGLE ENTRY DOWN UNDER TABLE SO IT CAN GROW.
!
         Z(ntable+1) = Z(idata)
         Z(ntable+2) = Z(idata+1)
         Z(ntable+3) = Z(idata+2)
         Z(ntable+4) = Z(idata+3)
         Z(ntable+5) = Z(idata+4)
         Z(ntable+6) = Z(idata+5)
         idata = ntable + 1
         ndata = ntable + 6
         EXIT
      ELSE
!
!     CONVERT PHI TO RADIANS
!
         Rz(ntable+2) = Rz(ntable+2)*Degrad
         ntable = ntable + 26
         IF ( ntable+26>core ) CALL mesage(-8,0,subr)
      ENDIF
   ENDDO
 600  IF ( ndata+6>core ) CALL mesage(-8,0,subr)
   CALL read(*1200,*1300,matpol,Z(ndata+1),6,noeor,flag)
   ndata = ndata + 6
   DO
!
!     SKIP THE ID-PHI PAIRS AS THEY SHOULD BE IDENTICAL TO ONES ALREADY
!     IN THE TABLE.
!
      CALL read(*1200,*1300,matpol,buf,2,noeor,flag)
      IF ( buf(1)==-1 ) THEN
!
!     READ THE NEXTID
!
         nextid = 0
         CALL read(*1200,*700,matpol,nextid,1,noeor,flag)
         IF ( nextid/=idf ) EXIT
         GOTO 600
      ENDIF
   ENDDO
!
!     SORT THE TABLE ON FIELD ONE OF EACH ENTRY THE ID.
!
 700  CALL sort(0,0,26,1,Z(itable),ntable-itable+1)
!
!                                  T
!     FOR EACH ENTRY GENERATE THE T T   MATRICE AND IF LKBFL = .TRUE.
!                                  I 0F
!
!     THE W  MATRICE.
!          I
!
   DO i = itable , ntable , 26
!
!     LOCATE THE TRANSFORMATION MATRIX IN DOUBLE PRECISION.
!     FIRST LOCATE BGPDT ENTRY
!
      kid = Z(i)
      CALL bisloc(*1400,kid,Z(ibgpdt),5,entrys,point)
      point = point + ibgpdt
      CALL bmgtns(Z(icstm),lcstm,Z(point),ti(1))
!
!     COMPUTE VI MATRIX.  (3X3)
!
      CALL gmmatd(ti(1),3,3,1,t0f(1),3,3,0,Z(i+4))
      IF ( lkbfl ) THEN
         j = (i+4)/2
         Rz(i+22) = Dz(j+3)
         Rz(i+23) = Dz(j+6)
         Rz(i+24) = Dz(j+9)
      ENDIF
   ENDDO
!
!     GENERATION AND OUTPUT OF MATRIX COLUMNS TO THE ABFL MATRIX.
!
   IF ( labfl ) THEN
      DO i = in , nn
!
!     COLUMN INDEX INFORMATION GJ,CJ FOR THIS HARMONIC COLUMN
!
         buf(1) = idf + Z(i)*500000
         buf(2) = 0
         CALL write(bdpool,buf,2,noeor)
!
!     TERMS OF THE COLUMN
!
         DO j = itable , ntable , 26
!
!     3 TERMS FOR THE J-TH ID ARE THE FOLLOWING SUMMATION
!
            term(1) = 0.0D0
            term(2) = 0.0D0
            term(3) = 0.0D0
            DO k = idata , ndata , 6
!
!                     N
!     COMPUTATION OF A
!                     I
!
               ain = Rz(k)*Rz(k+2)
               n = (Z(i)-1)/2
               fn = n
               IF ( n==0 ) THEN
!
!     N = 0
!
                  ain = ain*dble(Rz(j+3)-Rz(j+2))
!
!     N IS POSITIVE, CHECK FOR STAR CASE = N*
!
               ELSEIF ( mod(Z(i),2)/=0 ) THEN
                  dub = (cos(Rz(j+2)*fn)-cos(Rz(j+3)*fn))/fn
                  ain = ain*dub
               ELSE
                  dub = (sin(Rz(j+3)*fn)-sin(Rz(j+2)*fn))/fn
                  ain = ain*dub
               ENDIF
!
!     FORM VI MATRIX FOR THIS POINT
!
               dtemp(1) = Rz(k+3)*cos(Rz(j+1))
               dtemp(2) = Rz(k+3)*sin(Rz(j+1))
               dtemp(3) = Rz(k+4)
               CALL gmmatd(Z(j+4),3,3,0,dtemp(1),3,1,0,vi(1))
               DO l = 1 , 3
                  term(l) = term(l) + ain*vi(l)
               ENDDO
            ENDDO
!
!     OUTPUT THESE 3 TERMS
!
            buf(1) = Z(j)
            DO k = 1 , 3
               buf(2) = k
               rbuf(3) = term(k)
               IF ( rbuf(3)/=0 ) CALL write(bdpool,buf,3,noeor)
            ENDDO
         ENDDO
         CALL write(bdpool,mones,2,noeor)
      ENDDO
   ENDIF
!
!     GENERATION AND OUTPUT OF COLUMNS TO THE KBFL MATRIX.
!
   IF ( lkbfl ) THEN
      DO i = itable , ntable , 26
         cosphi = cos(Rz(i+1))
         sinphi = sin(Rz(i+1))
         angle = Rz(i+3) - Rz(i+2)
!
!     PUT OUT 3 COLUMNS FOR EACH OF THESE CONNECTED GRIDPOINTS
!
!     SOLVE NOW FOR K   V  = 3X1  CONSTANT FOR THE 3 COLUMNS
!                    II  I
!
!     AND IS A SUMMATION
!
         term(1) = 0.0D0
         term(2) = 0.0D0
         term(3) = 0.0D0
         DO j = idata , ndata , 6
            kii = Rz(j)*Rz(j+2)*Rz(j+5)*Rz(iz2)*angle
            dtemp(1) = kii*Rz(j+3)*cosphi
            dtemp(2) = kii*Rz(j+3)*sinphi
            dtemp(3) = kii*Rz(j+4)
            CALL gmmatd(Z(i+4),3,3,0,dtemp(1),3,1,0,vi(1))
            DO k = 1 , 3
               term(k) = term(k) + vi(k)
            ENDDO
         ENDDO
!
!     PUT OUT THE 3 COLUMNS
!
         DO j = 1 , 3
            head = .FALSE.
            l = i + j + 21
            dtemp(1) = dble(Rz(l))*term(1)
            dtemp(2) = dble(Rz(l))*term(2)
            dtemp(3) = dble(Rz(l))*term(3)
            buf(1) = Z(i)
            DO k = 1 , 3
               buf(2) = k
               rbuf(3) = dtemp(k)
!
!     TERM IS NOT WRITTEN IF HAS A ZERO VALUE
!
               IF ( rbuf(3)/=0 ) THEN
                  IF ( .NOT.(head) ) THEN
                     buf(4) = Z(i)
                     buf(5) = j
                     CALL write(scrt1,buf(4),2,noeor)
                     head = .TRUE.
                  ENDIF
                  CALL write(scrt1,buf,3,noeor)
               ENDIF
            ENDDO
            IF ( head ) CALL write(scrt1,mones,2,noeor)
         ENDDO
      ENDDO
   ENDIF
!
!     PROCESS THE NEXT FLUID POINT
!
   IF ( nextid/=0 ) THEN
      idf = nextid
      GOTO 500
   ELSE
!
!     ALL FLUID POINTS HAVE NOW BEEN PROCESSED.  APPEND THE KBFL, IF
!     ANY, DATA TO THE ABFL DATA AND WRAP UP.
!
      IF ( labfl ) CALL write(bdpool,mones,2,noeor)
      IF ( .NOT.lkbfl ) GOTO 900
      CALL write(scrt1,0,0,eor)
      CALL close(scrt1,Clsrew)
      file = scrt1
      CALL open(*1100,scrt1,Z(buf3),Rdrew)
      DO
         CALL read(*1200,*800,scrt1,Z(1),core,noeor,flag)
         CALL write(bdpool,Z(1),core,noeor)
      ENDDO
   ENDIF
 800  CALL write(bdpool,Z(1),flag,noeor)
   CALL write(bdpool,mones,2,eor)
 900  CALL close(bdpool,Clsrew)
!
!     PREPARE AND WRITE TRAILER
!
   buf(1) = bdpool
!
!     SET TRAILER BIT FOR DMIG CARDS
!
   buf(2) = 32768
   buf(3) = 0
   buf(4) = 0
   buf(5) = 0
   buf(6) = 0
   buf(7) = 0
   CALL wrttrl(buf)
   CALL close(scrt1,Clsrew)
!
!     END OF PROCESSING
!
 1000 CALL close(matpol,Clsrew)
   RETURN
!
!     ERROR CONDITIONS
!
 1100 CALL mesage(-1,file,subr)
 1200 CALL mesage(-2,file,subr)
 1300 CALL mesage(-3,file,subr)
   GOTO 1000
 1400 WRITE (Iout,99002) Sfm , Z(i)
99002 FORMAT (A25,' 4061, CONNECTED FLUID POINT ID =',I10,' IS MISSING BGPDT DATA.')
!
 1500 CALL mesage(-61,0,0)
!
END SUBROUTINE bmg