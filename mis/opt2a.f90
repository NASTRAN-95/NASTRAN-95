
SUBROUTINE opt2a(Ip,El,Iel,Pr,Ipr,Rr)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Conv , Core(1) , Parm(8) , Skp(2) , Skq(2) , Skr(2) , Sks , Skt(3) , Sku , Sysbuf , Y(1) , Z(16)
   INTEGER Count , Iy(1) , Iz(10) , Kore , Max , Nelw , Next , Noeor , Nprw , Nrd , Ntotl , Nwdse , Nwdsp , Nwrt , Oes1 , Outtap ,  &
         & Zcor
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Skp , Count , Skq , Kore , Skr , Nwdse , Nwdsp , Sks , Oes1 , Skt , Nelw , Nprw , Sku , Ntotl , Conv
   COMMON /names / Nrd , Noeor , Nwrt , Next
   COMMON /optpw2/ Zcor , Z
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   REAL El(1) , Pr(1) , Rr(1)
   INTEGER Iel(1) , Ip(2,1) , Ipr(1)
!
! Local variable declarations
!
   REAL ae1 , e1 , rc , s13 , s1s , s2s , s3s , x1 , x1a , x2 , x2a , xstar
   INTEGER eid(20) , etyp , i , icp , idel , ides , ier , ifle , ii , iret , irr , izk , j , k , kconv , kel , lel , limit , loce , &
         & locp , locp1 , m , m1 , mel , mest , name(2) , ne , nelr , nesw , oldtyp , pest , plus(5) , pstres , ptelt
   LOGICAL first , unsafe
!
! End of declarations
!
!
!     EQUIVALENT ARE  (EL,IEL), (PR,IPR)
   EQUIVALENCE (Z(1),Iz(1)) , (Core(1),Parm(1),Max) , (Iy(1),Y(1),Parm(8))
   DATA name/4H OPT , 4H2A  /
   DATA plus/4H     , 4H+    , 4H++   , 4H+++  , 4H++++/
!
   nelr = 0
   ne = 0
   ptelt = 0
   idel = 0
   kel = Kore
   kconv = 0
   Conv = 1.0
   icp = Ntotl - 4
!
!     READ HEADER, ODD RECORDS
!
   first = .TRUE.
 100  DO
      CALL read(*2400,*400,Oes1,Z(1),10,Next,i)
      etyp = Iz(3)
      nesw = Iz(10)
      oldtyp = ptelt
      ptelt = Iy(etyp)
      IF ( ptelt>0 ) THEN
         IF ( ptelt<oldtyp .AND. oldtyp/=0 ) THEN
            IF ( kel/=-1 ) kel = Kore
            IF ( ne/=0 ) THEN
               CALL page2(1)
               WRITE (Outtap,99005) (eid(j),j=1,ne)
               ne = 0
            ENDIF
            WRITE (Outtap,99001)
99001       FORMAT (/5X,15HNEXT SUBCASE...)
         ENDIF
!
!     SET POINTERS TO ELEMENT TYPE AND PROPERTIES IN CORE.
!     L = LOCATION OF FIRST, M = MAX LOCATION
!
         lel = Ip(1,ptelt)
         mel = Ip(1,ptelt+1) - 1
         IF ( mel<=lel ) THEN
            CALL fread(Oes1,0,0,Next)
         ELSE
            loce = lel
            locp1 = Ip(2,ptelt) - 1
            IF ( nesw<=Zcor ) EXIT
!
!     ELEMENT TYPE EXCEEDS CORE
!
            ier = -8
            ifle = nesw - Zcor
            GOTO 600
         ENDIF
      ELSE
         CALL fread(Oes1,0,0,Next)
!
!     ELEMENT TYPE NOT TO OPTIMIZE
!
      ENDIF
   ENDDO
!
!     SEQUENTIALLY READ ONE ELEMENT FROM EVEN NUMBERED RECORDS.
!     LOCE IS CURRENT ELEMENT TO COMPARE TO.
!
 200  CALL read(*300,*100,Oes1,Z(1),nesw,Noeor,i)
   ides = Iz(1)/10
   DO WHILE ( ides/=Iel(loce) )
!
!     SCAN THE CORE FILE UNTIL ELEMENT ID .GT. IDES
!
      IF ( ides<Iel(loce) ) GOTO 200
!
!     CORE ELEMENT NOT TO BE OPTIMIZED
!
      loce = loce + Nwdse
      IF ( loce>=mel ) THEN
         CALL fread(Oes1,0,0,Next)
         GOTO 100
!
!     END OF ELEMENT SEARCH FOR THIS TYPE (EOR NOT READ)
!
      ENDIF
   ENDDO
   GOTO 700
!
!     ILLEGAL EOF, EOR
!
 300  ier = -2
   GOTO 500
 400  ier = -3
 500  ifle = Oes1
!
 600  CALL mesage(ier,ifle,name)
!
!     PROCES THIS ELEMENT
!
 700  nelr = nelr + 1
   locp = Iel(loce+4) + locp1
   pest = Ipr(locp+1)/100
   mest = Ipr(locp+1) - pest*100
   rc = 1.0
   x1a = 0.0
   x2a = 0.0
   e1 = 999.
   unsafe = .FALSE.
!
   IF ( ptelt==1 .OR. ptelt==2 ) THEN
!
!     BAR, ELBOW
!
      limit = 2
      pstres = 7
      x2a = abs(Z(7))
      ASSIGN 1600 TO iret
      GOTO 1900
   ELSEIF ( ptelt==3 ) THEN
!
!     IS2D8
!
      m1 = 1
      s1s = 0.0
      s2s = 0.0
      s3s = 0.0
      DO m = 1 , 8
         m1 = m1 + 5
         ii = 3 + loce
         IF ( mest/=2 ) s3s = amax1(s3s,abs(Z(m1+2)/El(ii)))
         ii = ii - 2
         IF ( Z(m1)<0.0 ) ii = ii + 1
         IF ( mest/=1 ) s1s = amax1(s1s,abs(Z(m1)/El(ii)))
         ii = 1 + loce
         IF ( Z(m1+1)<0.0 ) ii = ii + 1
         s2s = amax1(s2s,abs(Z(m1+1)/El(ii)))
         s13 = amax1(s1s,s2s)
         s13 = amax1(s13,s3s)
      ENDDO
      e1 = abs(s13) - 1.0
      Pr(locp+4) = amax1(Pr(locp+4),s13)
      ASSIGN 2200 TO iret
      GOTO 2000
   ELSEIF ( ptelt==4 .OR. ptelt==5 .OR. ptelt==6 .OR. ptelt==16 ) THEN
!
!     TRMEM, QDMEM, QDMEM1, QDMEM2
!
      IF ( mest==1 ) GOTO 1500
      limit = 2
      pstres = 6
      ASSIGN 1400 TO iret
      GOTO 1900
   ELSEIF ( ptelt==7 .OR. ptelt==8 .OR. ptelt==9 .OR. ptelt==12 .OR. ptelt==13 .OR. ptelt==14 .OR. ptelt==17 .OR. ptelt==19 .OR.    &
          & ptelt==20 ) THEN
!
!     TRBSC, TRPLT, QDPLT, TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4
!
      IF ( mest==1 ) GOTO 1200
      limit = 2
      pstres = 7
      ASSIGN 900 TO iret
      GOTO 1900
   ELSEIF ( ptelt==11 ) THEN
!
!     SHEAR
!
      limit = 1
      pstres = 2
      ASSIGN 2200 TO iret
      GOTO 1900
   ELSEIF ( ptelt==15 ) THEN
!
!     TRIM6
!
      IF ( Iel(loce)/=idel ) THEN
         idel = Iel(loce)
         icp = icp + 4
         IF ( kel/=-1 .AND. icp>=kel ) CALL mesage(-8,0,name)
         Iy(icp) = locp
         Iy(icp+4) = -1
      ENDIF
      k = 0
      m1 = -1
      DO i = 1 , 3
         m1 = m1 + 7
         ii = 3 + loce
         s1s = 0.0
         s3s = 0.0
         IF ( mest/=2 ) s3s = abs(Z(m1+2)/El(ii))
         ii = ii - 2
         IF ( Z(m1)<0.0 ) ii = ii + 1
         IF ( mest/=1 ) s1s = abs(Z(m1)/El(ii))
         ii = 1 + loce
         IF ( Z(m1+1)<0.0 ) ii = ii + 1
         s2s = abs(Z(m1+1)/El(ii))
         s13 = amax1(s1s,s2s)
         s13 = amax1(s13,s3s)
         Y(icp+i) = amax1(Y(icp+i),s13)
         Pr(locp+4) = amax1(Pr(locp+4),s13)
         e1 = abs(s13) - 1.0
         IF ( abs(e1)<=Parm(2) ) k = k + 1
      ENDDO
      ASSIGN 2200 TO iret
      IF ( k>=3 ) GOTO 2000
      GOTO 2300
   ELSE
!
!     ROD, TUBE
!
      limit = 1
      pstres = 4
      ASSIGN 800 TO iret
      GOTO 1900
   ENDIF
 800  limit = 2
   pstres = 2
   ASSIGN 2200 TO iret
   GOTO 1900
 900  pstres = 8
   ASSIGN 1000 TO iret
   GOTO 1900
 1000 pstres = 15
   ASSIGN 1100 TO iret
   GOTO 1900
 1100 pstres = 16
   ASSIGN 1200 TO iret
   x1a = amax1(abs(Z(7)),abs(Z(8)))
   x2a = amax1(abs(Z(15)),abs(Z(16)))
   x1a = amax1(x1a,x2a)
   k = 0
   IF ( x1a==abs(Z(8)) .OR. x1a==abs(Z(15)) ) k = 1
   x1a = Z(7+k)
   x2a = Z(16-k)
   GOTO 1900
 1200 IF ( mest==2 ) GOTO 2200
   limit = 1
   pstres = 9
   ASSIGN 1300 TO iret
   GOTO 1900
 1300 pstres = 17
   ASSIGN 2200 TO iret
   GOTO 1900
 1400 pstres = 7
   ASSIGN 1500 TO iret
   GOTO 1900
 1500 IF ( mest==2 ) GOTO 200
   limit = 1
   pstres = 8
   ASSIGN 2200 TO iret
   GOTO 1900
 1600 pstres = 8
   x1a = abs(Z(8))
   ASSIGN 1700 TO iret
   GOTO 1900
 1700 pstres = 14
   ASSIGN 1800 TO iret
   GOTO 1900
 1800 pstres = 15
   ASSIGN 2200 TO iret
!
!     FUNCTION E1  -  RATIO STRESS MINUS LIMIT DIVIDED BY LIMIT,
!     WITH RESET OF -ALPHA-
!     LOCP   = POINTER TO PID OF PROPERTY.
!     LOCE   = POINTER TO EID OF ELEMENT.
!     LIMIT  = 1=SHEAR, 2= COMPRESSION/TENSION.
!     PSTRES = CORRESPONDING STRESS, POINTER TO Z ARRAY.
!
 1900 ii = 3 + loce
   IF ( limit/=1 ) THEN
      ii = ii - 2
      IF ( Z(pstres)<0.0 ) ii = ii + 1
   ENDIF
   IF ( El(ii)<=0.0 ) GOTO 2100
!
!     POSITIVE LIMIT
!
   Pr(locp+4) = amax1(Pr(locp+4),abs(Z(pstres)/El(ii)))
!
!                                        I
!                  NEGATIVE E1, SAFE     I    POSITIVE E1, UNSAFE
!                                        I
!   --+------+------+------+------+------+------+------------------- E1
!    UL     4P     3P     2P      P      0      P  (WHERE P=PARM(2),
!      ++++    +++    ++     +    I             I        UL=UNLOADED)
!            OVER DESIGNED        I REGION WHEREI  UNDER DESIGNED
!            REGION               I  AE1 .LE. P I          REGION
!                      (UNSAFE=.FALSE.)         I  (UNSAFE=.TRUE.)
!
   e1 = abs(Z(pstres)/El(ii)) - 1.0
 2000 IF ( e1>Parm(2) ) unsafe = .TRUE.
   IF ( unsafe ) kel = -1
   ae1 = amin1(ae1,abs(e1))
 2100 GOTO iret
!
 2200 x1 = abs(x1a)
   x2 = abs(x2a)
   IF ( x1/=0.0 .AND. x2/=0.0 ) THEN
      x1a = amin1(x1a,x2a)
      x1 = amin1(x1,x2)/amax1(x1,x2)
      x1 = sign(x1,x1a)
      IF ( abs(x1)>1.0E-8 ) rc = x1
   ENDIF
!
!     SAVE IN RR AN EMPIRICAL ALPHA MODIFIER FOR SPEEDY CONVERGENCE
!
 2300 irr = (locp+Nwdsp)/Nwdsp
   Rr(irr) = rc
!
   IF ( .NOT.(unsafe) ) THEN
!
!     PRINT ELEMENT IDS THAT HAVE CONVERGED, OR OVER DESIGNED
!
      IF ( first ) THEN
         first = .FALSE.
         CALL page2(-3)
         WRITE (Outtap,99002) Uim
99002    FORMAT (A29,' 2304A, THE FOLLOWING ELEMENTS EITHER CONVERGED (NO',' PLUS) OR OVER-DESIGNED (PLUS(ES))',/5X,                &
                &'IN ONE OR MORE ','SUBCASES,  (EACH PLUS INDICATES AN INCREMENTAL PERCENTAGE',                                     &
                &' OF OVER-DESIGN BASED ON CONVERGENCE CRITERION, EPS)',/)
      ENDIF
      xstar = (Pr(locp+4)-1.0) - Parm(2)
      j = ifix(abs(xstar)/Parm(2))
      IF ( j>3 ) j = 3
      ii = 1
      IF ( Pr(locp+4)<1.0E-8 ) ii = 0
      IF ( ii==0 ) j = 4
      eid(ne+1) = Iel(loce)
      eid(ne+2) = plus(j+1)
      ne = ne + 2
      IF ( ne>=20 ) THEN
         ne = 0
         CALL page2(1)
         WRITE (Outtap,99005) eid
      ENDIF
      IF ( kel/=-1 ) THEN
         kel = kel - 1
!WKBR 9/93 IZK = IZ(KEL)
         izk = Iy(kel)
         IF ( Pr(locp+3)<1.0E-6 ) ii = 0
         IF ( j>0 .AND. izk==-1 .AND. ii/=0 ) kconv = kconv - 1
         IF ( ii/=0 ) THEN
            IF ( ae1>Parm(2) ) GOTO 200
         ENDIF
         IF ( Iel(loce)/=izk ) THEN
            IF ( ae1<=Parm(2) .AND. izk==-1 ) THEN
!WKBR 9/93  610 IZ(KEL) = IEL(LOCE)
               Iy(kel) = Iel(loce)
            ELSEIF ( ii/=0 .OR. izk/=-1 ) THEN
!WKBR 9/93 IZ(KEL) = IEL(LOCE)
               Iy(kel) = Iel(loce)
!WKBR 9/93 IF (II .EQ. 0) IZ(KEL) = -1
               IF ( ii==0 ) Iy(kel) = -1
               kconv = kconv + 1
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   GOTO 200
!
!     EOF
!
 2400 IF ( ne>0 ) WRITE (Outtap,99005) (eid(j),j=1,ne)
!
!     IF KEL=-1 HERE, OR
!     IF NUMBER OF ELEMENTS CONVERGED, KORE-KEL, IS LESS THAN NUMBER OF
!     ELEMENTS IN THE PROBLEM, NELW/NWDSE, CONVERGENCE IS INCOMPLETE
!
   IF ( kel/=-1 ) THEN
      IF ( kconv>=Nelw/Nwdse ) THEN
!WKBR CALL PAGE (-4)
         CALL page2(-4)
         WRITE (Outtap,99003) Uim
99003    FORMAT (A29,' 2304B, CONVERGENCE ACHIEVED FOR ALL ELEMENTS ','REQUESTED, AND IN ALL SUBCASE(S)',/5X,                       &
                &'FULLY-STRESSED DESIGN COMPLETED',/)
         Conv = 2.0
         GOTO 99999
      ENDIF
   ENDIF
!
!     IF NELR IS ZERO, NO ELEMENT MATCH MADE
!
   IF ( nelr<=0 ) THEN
      CALL page2(-2)
      WRITE (Outtap,99004) Ufm
99004 FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
      Count = Max + 1
   ENDIF
99005 FORMAT (5X,10(I8,A4))
!
99999 END SUBROUTINE opt2a
