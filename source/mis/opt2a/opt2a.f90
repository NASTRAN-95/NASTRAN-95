!*==opt2a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE opt2a(Ip,El,Iel,Pr,Ipr,Rr)
   USE c_blank
   USE c_names
   USE c_optpw2
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2,1) :: Ip
   REAL , DIMENSION(1) :: El
   INTEGER , DIMENSION(1) :: Iel
   REAL , DIMENSION(1) :: Pr
   INTEGER , DIMENSION(1) :: Ipr
   REAL , DIMENSION(1) :: Rr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ae1 , e1 , rc , s13 , s1s , s2s , s3s , x1 , x1a , x2 , x2a , xstar
   INTEGER , DIMENSION(20) :: eid
   INTEGER :: etyp , i , icp , idel , ides , ier , ifle , ii , iret , irr , izk , j , k , kconv , kel , lel , limit , loce , locp , &
            & locp1 , m , m1 , max , mel , mest , ne , nelr , nesw , oldtyp , pest , pstres , ptelt
   LOGICAL :: first , unsafe
   INTEGER , DIMENSION(1) :: iy
   INTEGER , DIMENSION(10) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(8) :: parm
   INTEGER , DIMENSION(5) , SAVE :: plus
   REAL , DIMENSION(1) :: y
   EXTERNAL fread , mesage , page2 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     EQUIVALENT ARE  (EL,IEL), (PR,IPR)
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Core(1),Parm(1),Max) , (Iy(1),Y(1),Parm(8))
   DATA name/4H OPT , 4H2A  /
   DATA plus/4H     , 4H+    , 4H++   , 4H+++  , 4H++++/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nelr = 0
         ne = 0
         ptelt = 0
         idel = 0
         kel = kore
         kconv = 0
         conv = 1.0
         icp = ntotl - 4
!
!     READ HEADER, ODD RECORDS
!
         first = .TRUE.
 20      SPAG_Loop_1_1: DO
            CALL read(*320,*60,oes1,z(1),10,next,i)
            etyp = iz(3)
            nesw = iz(10)
            oldtyp = ptelt
            ptelt = iy(etyp)
            IF ( ptelt>0 ) THEN
               IF ( ptelt<oldtyp .AND. oldtyp/=0 ) THEN
                  IF ( kel/=-1 ) kel = kore
                  IF ( ne/=0 ) THEN
                     CALL page2(1)
                     WRITE (outtap,99005) (eid(j),j=1,ne)
                     ne = 0
                  ENDIF
                  WRITE (outtap,99001)
99001             FORMAT (/5X,15HNEXT SUBCASE...)
               ENDIF
!
!     SET POINTERS TO ELEMENT TYPE AND PROPERTIES IN CORE.
!     L = LOCATION OF FIRST, M = MAX LOCATION
!
               lel = Ip(1,ptelt)
               mel = Ip(1,ptelt+1) - 1
               IF ( mel<=lel ) THEN
                  CALL fread(oes1,0,0,next)
               ELSE
                  loce = lel
                  locp1 = Ip(2,ptelt) - 1
                  IF ( nesw<=zcor ) EXIT SPAG_Loop_1_1
!
!     ELEMENT TYPE EXCEEDS CORE
!
                  ier = -8
                  ifle = nesw - zcor
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               CALL fread(oes1,0,0,next)
!
!     ELEMENT TYPE NOT TO OPTIMIZE
!
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_2: DO
!
!     SEQUENTIALLY READ ONE ELEMENT FROM EVEN NUMBERED RECORDS.
!     LOCE IS CURRENT ELEMENT TO COMPARE TO.
!
            CALL read(*40,*20,oes1,z(1),nesw,noeor,i)
            ides = iz(1)/10
            DO WHILE ( ides/=Iel(loce) )
!
!     SCAN THE CORE FILE UNTIL ELEMENT ID .GT. IDES
!
               IF ( ides<Iel(loce) ) CYCLE SPAG_Loop_1_2
!
!     CORE ELEMENT NOT TO BE OPTIMIZED
!
               loce = loce + nwdse
               IF ( loce>=mel ) THEN
                  CALL fread(oes1,0,0,next)
                  GOTO 20
!
!     END OF ELEMENT SEARCH FOR THIS TYPE (EOR NOT READ)
!
               ENDIF
            ENDDO
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_2
!
!     ILLEGAL EOF, EOR
!
 40      ier = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      ier = -3
         spag_nextblock_1 = 3
      CASE (3)
         ifle = oes1
         spag_nextblock_1 = 4
      CASE (4)
!
         CALL mesage(ier,ifle,name)
         spag_nextblock_1 = 5
      CASE (5)
!
!     PROCES THIS ELEMENT
!
         nelr = nelr + 1
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
            x2a = abs(z(7))
            ASSIGN 240 TO iret
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
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
               IF ( mest/=2 ) s3s = amax1(s3s,abs(z(m1+2)/El(ii)))
               ii = ii - 2
               IF ( z(m1)<0.0 ) ii = ii + 1
               IF ( mest/=1 ) s1s = amax1(s1s,abs(z(m1)/El(ii)))
               ii = 1 + loce
               IF ( z(m1+1)<0.0 ) ii = ii + 1
               s2s = amax1(s2s,abs(z(m1+1)/El(ii)))
               s13 = amax1(s1s,s2s)
               s13 = amax1(s13,s3s)
            ENDDO
            e1 = abs(s13) - 1.0
            Pr(locp+4) = amax1(Pr(locp+4),s13)
            ASSIGN 300 TO iret
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ptelt==4 .OR. ptelt==5 .OR. ptelt==6 .OR. ptelt==16 ) THEN
!
!     TRMEM, QDMEM, QDMEM1, QDMEM2
!
            IF ( mest==1 ) GOTO 220
            limit = 2
            pstres = 6
            ASSIGN 200 TO iret
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ptelt==7 .OR. ptelt==8 .OR. ptelt==9 .OR. ptelt==12 .OR. ptelt==13 .OR. ptelt==14 .OR. ptelt==17 .OR.             &
                & ptelt==19 .OR. ptelt==20 ) THEN
!
!     TRBSC, TRPLT, QDPLT, TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4
!
            IF ( mest==1 ) GOTO 160
            limit = 2
            pstres = 7
            ASSIGN 100 TO iret
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ptelt==11 ) THEN
!
!     SHEAR
!
            limit = 1
            pstres = 2
            ASSIGN 300 TO iret
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ptelt==15 ) THEN
!
!     TRIM6
!
            IF ( Iel(loce)/=idel ) THEN
               idel = Iel(loce)
               icp = icp + 4
               IF ( kel/=-1 .AND. icp>=kel ) CALL mesage(-8,0,name)
               iy(icp) = locp
               iy(icp+4) = -1
            ENDIF
            k = 0
            m1 = -1
            DO i = 1 , 3
               m1 = m1 + 7
               ii = 3 + loce
               s1s = 0.0
               s3s = 0.0
               IF ( mest/=2 ) s3s = abs(z(m1+2)/El(ii))
               ii = ii - 2
               IF ( z(m1)<0.0 ) ii = ii + 1
               IF ( mest/=1 ) s1s = abs(z(m1)/El(ii))
               ii = 1 + loce
               IF ( z(m1+1)<0.0 ) ii = ii + 1
               s2s = abs(z(m1+1)/El(ii))
               s13 = amax1(s1s,s2s)
               s13 = amax1(s13,s3s)
               y(icp+i) = amax1(y(icp+i),s13)
               Pr(locp+4) = amax1(Pr(locp+4),s13)
               e1 = abs(s13) - 1.0
               IF ( abs(e1)<=parm(2) ) k = k + 1
            ENDDO
            ASSIGN 300 TO iret
            IF ( k<3 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     ROD, TUBE
!
            limit = 1
            pstres = 4
            ASSIGN 80 TO iret
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      limit = 2
         pstres = 2
         ASSIGN 300 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 100     pstres = 8
         ASSIGN 120 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 120     pstres = 15
         ASSIGN 140 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 140     pstres = 16
         ASSIGN 160 TO iret
         x1a = amax1(abs(z(7)),abs(z(8)))
         x2a = amax1(abs(z(15)),abs(z(16)))
         x1a = amax1(x1a,x2a)
         k = 0
         IF ( x1a==abs(z(8)) .OR. x1a==abs(z(15)) ) k = 1
         x1a = z(7+k)
         x2a = z(16-k)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 160     IF ( mest==2 ) GOTO 300
         limit = 1
         pstres = 9
         ASSIGN 180 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 180     pstres = 17
         ASSIGN 300 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 200     pstres = 7
         ASSIGN 220 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     IF ( mest==2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         limit = 1
         pstres = 8
         ASSIGN 300 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 240     pstres = 8
         x1a = abs(z(8))
         ASSIGN 260 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 260     pstres = 14
         ASSIGN 280 TO iret
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 280     pstres = 15
         ASSIGN 300 TO iret
         spag_nextblock_1 = 6
      CASE (6)
!
!     FUNCTION E1  -  RATIO STRESS MINUS LIMIT DIVIDED BY LIMIT,
!     WITH RESET OF -ALPHA-
!     LOCP   = POINTER TO PID OF PROPERTY.
!     LOCE   = POINTER TO EID OF ELEMENT.
!     LIMIT  = 1=SHEAR, 2= COMPRESSION/TENSION.
!     PSTRES = CORRESPONDING STRESS, POINTER TO Z ARRAY.
!
         ii = 3 + loce
         IF ( limit/=1 ) THEN
            ii = ii - 2
            IF ( z(pstres)<0.0 ) ii = ii + 1
         ENDIF
         IF ( El(ii)<=0.0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     POSITIVE LIMIT
!
         Pr(locp+4) = amax1(Pr(locp+4),abs(z(pstres)/El(ii)))
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
         e1 = abs(z(pstres)/El(ii)) - 1.0
         spag_nextblock_1 = 7
      CASE (7)
         IF ( e1>parm(2) ) unsafe = .TRUE.
         IF ( unsafe ) kel = -1
         ae1 = amin1(ae1,abs(e1))
         spag_nextblock_1 = 8
      CASE (8)
         GOTO iret
!
 300     x1 = abs(x1a)
         x2 = abs(x2a)
         IF ( x1/=0.0 .AND. x2/=0.0 ) THEN
            x1a = amin1(x1a,x2a)
            x1 = amin1(x1,x2)/amax1(x1,x2)
            x1 = sign(x1,x1a)
            IF ( abs(x1)>1.0E-8 ) rc = x1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     SAVE IN RR AN EMPIRICAL ALPHA MODIFIER FOR SPEEDY CONVERGENCE
!
         irr = (locp+nwdsp)/nwdsp
         Rr(irr) = rc
!
         IF ( .NOT.(unsafe) ) THEN
!
!     PRINT ELEMENT IDS THAT HAVE CONVERGED, OR OVER DESIGNED
!
            IF ( first ) THEN
               first = .FALSE.
               CALL page2(-3)
               WRITE (outtap,99002) uim
99002          FORMAT (A29,' 2304A, THE FOLLOWING ELEMENTS EITHER CONVERGED (NO',' PLUS) OR OVER-DESIGNED (PLUS(ES))',/5X,          &
                      &'IN ONE OR MORE ','SUBCASES,  (EACH PLUS INDICATES AN INCREMENTAL PERCENTAGE',                               &
                      &' OF OVER-DESIGN BASED ON CONVERGENCE CRITERION, EPS)',/)
            ENDIF
            xstar = (Pr(locp+4)-1.0) - parm(2)
            j = ifix(abs(xstar)/parm(2))
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
               WRITE (outtap,99005) eid
            ENDIF
            IF ( kel/=-1 ) THEN
               kel = kel - 1
!WKBR 9/93 IZK = IZ(KEL)
               izk = iy(kel)
               IF ( Pr(locp+3)<1.0E-6 ) ii = 0
               IF ( j>0 .AND. izk==-1 .AND. ii/=0 ) kconv = kconv - 1
               IF ( ii/=0 ) THEN
                  IF ( ae1>parm(2) ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( Iel(loce)/=izk ) THEN
                  IF ( ae1<=parm(2) .AND. izk==-1 ) THEN
!WKBR 9/93  610 IZ(KEL) = IEL(LOCE)
                     iy(kel) = Iel(loce)
                  ELSEIF ( ii/=0 .OR. izk/=-1 ) THEN
!WKBR 9/93 IZ(KEL) = IEL(LOCE)
                     iy(kel) = Iel(loce)
!WKBR 9/93 IF (II .EQ. 0) IZ(KEL) = -1
                     IF ( ii==0 ) iy(kel) = -1
                     kconv = kconv + 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     EOF
!
 320     IF ( ne>0 ) WRITE (outtap,99005) (eid(j),j=1,ne)
!
!     IF KEL=-1 HERE, OR
!     IF NUMBER OF ELEMENTS CONVERGED, KORE-KEL, IS LESS THAN NUMBER OF
!     ELEMENTS IN THE PROBLEM, NELW/NWDSE, CONVERGENCE IS INCOMPLETE
!
         IF ( kel/=-1 ) THEN
            IF ( kconv>=nelw/nwdse ) THEN
!WKBR CALL PAGE (-4)
               CALL page2(-4)
               WRITE (outtap,99003) uim
99003          FORMAT (A29,' 2304B, CONVERGENCE ACHIEVED FOR ALL ELEMENTS ','REQUESTED, AND IN ALL SUBCASE(S)',/5X,                 &
                      &'FULLY-STRESSED DESIGN COMPLETED',/)
               conv = 2.0
               RETURN
            ENDIF
         ENDIF
!
!     IF NELR IS ZERO, NO ELEMENT MATCH MADE
!
         IF ( nelr<=0 ) THEN
            CALL page2(-2)
            WRITE (outtap,99004) ufm
99004       FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
            count = max + 1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (5X,10(I8,A4))
!
END SUBROUTINE opt2a
