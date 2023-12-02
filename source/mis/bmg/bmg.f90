!*==bmg.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bmg
   USE c_blank
   USE c_condas
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: ain , dub
   REAL :: angle , cosphi , degrad , fn , kii , phil1 , phip1 , sinphi , twopi
   INTEGER , SAVE :: bdpool , bgpdt , cstm , eor , eqexin , is , iz2 , iz6 , iz7 , iz8 , iz9 , matpol , noeor , scrt1
   INTEGER , DIMENSION(2) , SAVE :: bndfl , nabfl , nkbfl , subr
   INTEGER , DIMENSION(10) :: buf
   INTEGER :: buf1 , buf2 , buf3 , core , entrys , file , flag , form , i , ibgpdt , icstm , idata , idf , ik , in , inn , itable , &
            & j , k , kid , l , lbgpdt , lcstm , n , n2 , n4 , nbgpdt , ncstm , ndata , nextid , nn , nnn , ntable , point
   INTEGER , DIMENSION(3) , SAVE :: dmig , mones
   REAL(REAL64) , DIMENSION(3) :: dtemp , term , vi
   REAL(REAL64) , DIMENSION(1) :: dz
   LOGICAL :: head , labfl , lkbfl , nstar
   REAL , DIMENSION(10) :: rbuf
   REAL , DIMENSION(1) :: rz
   REAL(REAL64) , DIMENSION(9) :: t0f , ti
   EXTERNAL bisloc , bmgtns , close , gmmatd , gopen , korsz , locate , mesage , open , preloc , read , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINE CORE AND BUFFER POINTERS
!
         core = korsz(z)
         buf1 = core - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         core = buf3 - 1
         IF ( core<100 ) CALL mesage(-8,0,subr)
         kflags(1) = -1
         kflags(2) = -1
!
!     OPEN MATPOOL AND LOCATE THE BNDFL RECORD AS PREPARED BY IFP4.
!
         CALL preloc(*120,z(buf1),matpol)
         CALL locate(*120,z(buf1),bndfl,flag)
!
!     THIS MODULE DOES NOTHING IF THE MATPOOL IS PURGED OR THE BNDFL
!     RECORD IS ABSENT.  NOW READ THE HEADER DATA OF THIS RECORD.
!
         file = matpol
         CALL read(*160,*180,matpol,z(1),9,noeor,flag)
         in = 10
         nn = z(iz9) + 9
         IF ( nn+5>core ) CALL mesage(-8,0,subr)
!
!     READ THE INDICES
!
         CALL read(*160,*180,matpol,z(in),z(iz9),noeor,flag)
         value(1) = z(iz6)
         value(2) = 0.0
         IF ( z(iz6)==0 ) value(1) = 1.0
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
         IF ( z(iz6)>0 ) THEN
!
!     M IS POSITIVE THUS CHECK FOR STAR OR NO-STAR INDICES PERMITTED.
!     DETERMINE THE FORM OF THE CHECK EQUATION.
!
!          Z(7) = S1
!          Z(8) = S2
!          Z(6) = M
!
            IF ( z(iz7)==is ) THEN
               nstar = .FALSE.
            ELSE
               nstar = .TRUE.
            ENDIF
            IF ( z(iz7)==z(iz8) ) THEN
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
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     n = (z(i)-1)/2
                     IF ( mod(z(i),2)/=0 ) THEN
!
!     STAR CASE
!
                        IF ( .NOT.nstar ) CYCLE
                        IF ( form>=0 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ELSE
!
!     NON-STAR CASE
!
                        IF ( nstar ) CYCLE
                        IF ( form>=0 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
!
!                           K M
!     CHECK USING EQUATION  ---
!                            2
!
                     n2 = n*2
                     k = n2/z(iz6)
                     IF ( k*z(iz6)/=n2 ) CYCLE
                     spag_nextblock_2 = 2
                  CASE (2)
!
!     GOOD INDICE,  ADD IT TO THE LIST
!
                     nnn = nnn + 1
                     z(nnn) = z(i)
                  CASE (3)
!
!                            (2K+1)M
!     CHECK USING EQUATION   -------
!                               4
!
                     n4 = n*4
                     ik = n4/z(iz6)
                     ik = ik - 1
                     k = ik/2
                     IF ( (2*k+1)*z(iz6)==n4 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
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
         IF ( rz(iz2)==0.0 ) lkbfl = .FALSE.
         IF ( lkbfl ) kflags(1) = 0
         IF ( labfl ) kflags(2) = 0
         IF ( .NOT.labfl .AND. .NOT.lkbfl ) GOTO 120
!
!     BGPDT IS NOW READ INTO CORE AS 5 WORD ENTRIES, RESERVING FIRST
!     WORD FOR THE EXTERNAL ID.
!
         file = bgpdt
         ibgpdt = nn + 1
         nbgpdt = nn
         CALL gopen(bgpdt,z(buf2),rdrew)
         DO
            CALL read(*160,*20,bgpdt,z(nbgpdt+2),4,noeor,flag)
            nbgpdt = nbgpdt + 5
            IF ( nbgpdt+5>core ) CALL mesage(-8,0,subr)
         ENDDO
 20      CALL close(bgpdt,clsrew)
!
!     READ EQEXIN PLACING EXTERNAL ID ON RESPECTIVE BGPDT ENTRY.
!
         file = eqexin
         CALL gopen(eqexin,z(buf2),rdrew)
         DO
            CALL read(*160,*40,eqexin,buf,2,noeor,flag)
            n = 5*buf(2) - 5 + ibgpdt
            z(n) = buf(1)
         ENDDO
 40      CALL close(eqexin,clsrew)
         lbgpdt = nbgpdt - ibgpdt + 1
         entrys = lbgpdt/5
!
!     SORT THE BGPDT ON EXTERNAL ID
!
         CALL sort(0,0,5,1,z(ibgpdt),lbgpdt)
!
!  BLAST CSTM INTO CORE
!
         file = cstm
         CALL gopen(cstm,z(buf2),rdrew)
         icstm = nbgpdt + 1
         CALL read(*160,*60,cstm,z(icstm),core-icstm,noeor,flag)
         CALL mesage(-8,0,subr)
 60      ncstm = icstm + flag - 1
         lcstm = ncstm - icstm + 1
         CALL close(cstm,clsrew)
!
!     LOCATE THE T   MATRIX IN THE CSTM DATA BY USING CSID = CDF IN
!                 0F
!
!     THE HEADER DATA.      ( Z(1) )
!
         DO i = icstm , ncstm , 14
            IF ( z(1)==z(i) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (iout,99001) sfm , z(1)
99001    FORMAT (A25,' 4060, COORDINATE SYSTEM =',I9,' CAN NOT BE FOUND IN CSTM DATA.')
         spag_nextblock_1 = 5
      CASE (2)
         n = i + 5
         DO i = 1 , 9
            t0f(i) = dble(rz(n))
            n = n + 1
         ENDDO
!
!     OPEN BDPOOL FOR ABFL, AND SCRATCH1 FOR KBFL AND WRITE THE DMIG
!     HEADER INFORMATION.
!
         CALL gopen(bdpool,z(buf2),wrtrew)
!
!     WRITE DMIG RECORD ID
!
         CALL write(bdpool,dmig,3,noeor)
         buf(1) = nabfl(1)
         buf(2) = nabfl(2)
         buf(3) = 0
         buf(4) = 1
         buf(5) = 1
         buf(6) = iprec
         buf(7) = 0
         buf(8) = 0
         buf(9) = 0
         IF ( labfl ) CALL write(bdpool,buf,9,noeor)
         IF ( lkbfl ) THEN
            file = scrt1
            CALL open(*140,scrt1,z(buf3),wrtrew)
            buf(1) = nkbfl(1)
            buf(2) = nkbfl(2)
            CALL write(scrt1,buf,9,noeor)
         ENDIF
!
!     READ SOME FLUID-PT DATA (IDF,R,Z,L,C,S,RHO)
!
         file = matpol
         CALL read(*160,*180,matpol,idf,1,noeor,flag)
         spag_nextblock_1 = 3
      CASE (3)
         idata = ncstm + 1
         ndata = ncstm + 6
         CALL read(*160,*180,matpol,z(idata),6,noeor,flag)
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
         SPAG_Loop_1_1: DO
            CALL read(*160,*180,matpol,z(ntable+1),2,noeor,flag)
            IF ( z(ntable+1)==-1 ) THEN
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
                     phil1 = rz(i-25)
!
!     SPECIAL CASE ON FIRST POINT, TEST M TO FIND PHI
!                                                    I-1
!
                  ELSEIF ( z(iz6)>1 ) THEN
                     phil1 = rz(itable+1)
                  ELSE
                     phil1 = rz(ntable-24) - twopi
                  ENDIF
                  rz(i+2) = (rz(i+1)+phil1)/2.0
!
!     SET UP PHI  IN FOURTH SLOT OF ENTRY = (PHI  + PHI   )/2.0
!               1                               I      I+1
!
                  IF ( i/=ntable-25 ) THEN
                     phip1 = rz(i+27)
!
!     SPECIAL CASE ON LAST POINT, TEST M TO FIND PHI
!                                                   I+1
!
                  ELSEIF ( z(iz6)>1 ) THEN
                     phip1 = rz(ntable-24)
                  ELSE
                     phip1 = rz(itable+1) + twopi
                  ENDIF
                  rz(i+3) = (rz(i+1)+phip1)/2.0
               ENDDO
!
!     PICK UP NEXT FLUID POINT IDF
!
               nextid = 0
               CALL read(*160,*80,matpol,nextid,1,noeor,flag)
               IF ( nextid/=idf ) GOTO 80
!
!     NEXTID IS SAME AS CURRENT IDF, THUS ADD ANOTHER ENTRY OF R,Z,L,C,
!     S,RH FIRST MOVE SINGLE ENTRY DOWN UNDER TABLE SO IT CAN GROW.
!
               z(ntable+1) = z(idata)
               z(ntable+2) = z(idata+1)
               z(ntable+3) = z(idata+2)
               z(ntable+4) = z(idata+3)
               z(ntable+5) = z(idata+4)
               z(ntable+6) = z(idata+5)
               idata = ntable + 1
               ndata = ntable + 6
               EXIT SPAG_Loop_1_1
            ELSE
!
!     CONVERT PHI TO RADIANS
!
               rz(ntable+2) = rz(ntable+2)*degrad
               ntable = ntable + 26
               IF ( ntable+26>core ) CALL mesage(-8,0,subr)
            ENDIF
         ENDDO SPAG_Loop_1_1
         SPAG_Loop_1_3: DO
            IF ( ndata+6>core ) CALL mesage(-8,0,subr)
            CALL read(*160,*180,matpol,z(ndata+1),6,noeor,flag)
            ndata = ndata + 6
            SPAG_Loop_2_2: DO
!
!     SKIP THE ID-PHI PAIRS AS THEY SHOULD BE IDENTICAL TO ONES ALREADY
!     IN THE TABLE.
!
               CALL read(*160,*180,matpol,buf,2,noeor,flag)
               IF ( buf(1)==-1 ) THEN
!
!     READ THE NEXTID
!
                  nextid = 0
                  CALL read(*160,*80,matpol,nextid,1,noeor,flag)
                  IF ( nextid/=idf ) EXIT SPAG_Loop_2_2
                  CYCLE SPAG_Loop_1_3
               ENDIF
            ENDDO SPAG_Loop_2_2
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!
!     SORT THE TABLE ON FIELD ONE OF EACH ENTRY THE ID.
!
 80      CALL sort(0,0,26,1,z(itable),ntable-itable+1)
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
            kid = z(i)
            CALL bisloc(*200,kid,z(ibgpdt),5,entrys,point)
            point = point + ibgpdt
            CALL bmgtns(z(icstm),lcstm,z(point),ti(1))
!
!     COMPUTE VI MATRIX.  (3X3)
!
            CALL gmmatd(ti(1),3,3,1,t0f(1),3,3,0,z(i+4))
            IF ( lkbfl ) THEN
               j = (i+4)/2
               rz(i+22) = dz(j+3)
               rz(i+23) = dz(j+6)
               rz(i+24) = dz(j+9)
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
               buf(1) = idf + z(i)*500000
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
                     ain = rz(k)*rz(k+2)
                     n = (z(i)-1)/2
                     fn = n
                     IF ( n==0 ) THEN
!
!     N = 0
!
                        ain = ain*dble(rz(j+3)-rz(j+2))
!
!     N IS POSITIVE, CHECK FOR STAR CASE = N*
!
                     ELSEIF ( mod(z(i),2)/=0 ) THEN
                        dub = (cos(rz(j+2)*fn)-cos(rz(j+3)*fn))/fn
                        ain = ain*dub
                     ELSE
                        dub = (sin(rz(j+3)*fn)-sin(rz(j+2)*fn))/fn
                        ain = ain*dub
                     ENDIF
!
!     FORM VI MATRIX FOR THIS POINT
!
                     dtemp(1) = rz(k+3)*cos(rz(j+1))
                     dtemp(2) = rz(k+3)*sin(rz(j+1))
                     dtemp(3) = rz(k+4)
                     CALL gmmatd(z(j+4),3,3,0,dtemp(1),3,1,0,vi(1))
                     DO l = 1 , 3
                        term(l) = term(l) + ain*vi(l)
                     ENDDO
                  ENDDO
!
!     OUTPUT THESE 3 TERMS
!
                  buf(1) = z(j)
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
               cosphi = cos(rz(i+1))
               sinphi = sin(rz(i+1))
               angle = rz(i+3) - rz(i+2)
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
                  kii = rz(j)*rz(j+2)*rz(j+5)*rz(iz2)*angle
                  dtemp(1) = kii*rz(j+3)*cosphi
                  dtemp(2) = kii*rz(j+3)*sinphi
                  dtemp(3) = kii*rz(j+4)
                  CALL gmmatd(z(i+4),3,3,0,dtemp(1),3,1,0,vi(1))
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
                  dtemp(1) = dble(rz(l))*term(1)
                  dtemp(2) = dble(rz(l))*term(2)
                  dtemp(3) = dble(rz(l))*term(3)
                  buf(1) = z(i)
                  DO k = 1 , 3
                     buf(2) = k
                     rbuf(3) = dtemp(k)
!
!     TERM IS NOT WRITTEN IF HAS A ZERO VALUE
!
                     IF ( rbuf(3)/=0 ) THEN
                        IF ( .NOT.(head) ) THEN
                           buf(4) = z(i)
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
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     ALL FLUID POINTS HAVE NOW BEEN PROCESSED.  APPEND THE KBFL, IF
!     ANY, DATA TO THE ABFL DATA AND WRAP UP.
!
            IF ( labfl ) CALL write(bdpool,mones,2,noeor)
            IF ( .NOT.lkbfl ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(scrt1,0,0,eor)
            CALL close(scrt1,clsrew)
            file = scrt1
            CALL open(*140,scrt1,z(buf3),rdrew)
            DO
               CALL read(*160,*100,scrt1,z(1),core,noeor,flag)
               CALL write(bdpool,z(1),core,noeor)
            ENDDO
         ENDIF
 100     CALL write(bdpool,z(1),flag,noeor)
         CALL write(bdpool,mones,2,eor)
         spag_nextblock_1 = 4
      CASE (4)
         CALL close(bdpool,clsrew)
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
         CALL close(scrt1,clsrew)
!
!     END OF PROCESSING
!
 120     CALL close(matpol,clsrew)
         RETURN
!
!     ERROR CONDITIONS
!
 140     CALL mesage(-1,file,subr)
 160     CALL mesage(-2,file,subr)
 180     CALL mesage(-3,file,subr)
         GOTO 120
 200     WRITE (iout,99002) sfm , z(i)
99002    FORMAT (A25,' 4061, CONNECTED FLUID POINT ID =',I10,' IS MISSING BGPDT DATA.')
         spag_nextblock_1 = 5
      CASE (5)
!
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE bmg
