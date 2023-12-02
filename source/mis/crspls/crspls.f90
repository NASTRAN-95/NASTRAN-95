!*==crspls.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE crspls(Jump,Mu,Bp,Rs,Again,N23) !HIDESTARS (*,Jump,Mu,Bp,Rs,Again,N23)
!
!     THIS ROUTINE HANDLES CRBE3 AND CRSPLINE RIGID ELEMENTS
!     CALLED ONLY BY CRIGGP SUBROUTINE
!
!     SINGLE PRECISION VERSION
!
   USE c_crsply
   USE c_gp4fil
   USE c_gp4prm
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Jump
   INTEGER :: Mu
   INTEGER :: Bp
   INTEGER , DIMENSION(3) :: Rs
   LOGICAL :: Again
   INTEGER :: N23
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , b , c
   REAL :: ans , coeff , di , dl , espx , fac , len , leng , ln3 , wt , zk
   INTEGER :: begn , comp , eid , flag , grid , i , i1 , i2 , i3 , ib , id , idl , ie , iend , ii , is , iwt , j , jj , js , k ,    &
            & k1 , k2 , khi , klo , kn2 , kx , l , l38 , lastk , ll , msg , nd , nm , ns , nwds , pass , refg , retn , retn1 ,      &
            & retn2 , retn3 , retn4 , sil , sing
   INTEGER , SAVE :: cm , cn , mask15 , nogox , times
   REAL , DIMENSION(9) :: d
   LOGICAL , SAVE :: debug
   REAL , SAVE :: eps , half , one , zero
   REAL , DIMENSION(36) :: gnn , knn , snn , t , tx , unn , x , y , znn
   INTEGER , DIMENSION(2) :: mcode
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(6) :: sild
   REAL , DIMENSION(6) :: w
   REAL , DIMENSION(1) :: z
   EXTERNAL bckrec , bug1 , gmmats , invers , mesage , read , sswtch , transs , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (wt,iwt) , (dl,idl) , (X1,A(1)) , (X2,B(1)) , (X3,C(1))
   DATA one , zero , half , eps , times , debug/1.0 , 0.0 , 0.5 , 1.0E-10 , 0 , .FALSE./
   DATA cm , cn , nogox , mask15 , name/6 , 12 , 0 , 32767 , 4HCRSP , 4HLS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Again ) RETURN 1
         IF ( debug ) WRITE (nout,99001) kn , knkl1 , Bp , geomp , bgpdt , cstm , rgt , Jump
99001    FORMAT ('0  CRSPLS DEBUG- KN,KNKL1,BP,GEOMP,BGPDT,CSTM,RGT,JUMP=',/3X,8I7)
         kn2 = kn/2
         CALL sswtch(38,l38)
!
!     UNIT MATRIX UNN
!
         DO i = 2 , 35
            unn(i) = zero
         ENDDO
         unn(1) = one
         unn(8) = one
         unn(15) = one
         unn(22) = one
         unn(29) = one
         unn(36) = one
!
!     JUMP = 1 FOR CRBE3 DATA,  JUMP = 2 FOR CRSPLINE DATA
!
         IF ( Jump==2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ CRBE3 DATA ON INPUT FILE
!     =============================
!
!     CLEAR WORKING SPACE
!     READ INPUT CARD, SAVE BEGINING POINTER, BEGN, AND
!     COUNT NUMBER OF WORDS READ, NWDS, IN FIRST PASS
!     (EACH INPUT CARD WILL BE READ TWICE)
!
         begn = 3
         spag_nextblock_1 = 2
      CASE (2)
         pass = 1
         DO i = 1 , 36
            t(i) = zero
            knn(i) = zero
         ENDDO
         CALL read(*480,*480,geomp,buf,3,0,flag)
         nwds = 3
         IF ( debug .OR. l38==1 ) WRITE (nout,99006) buf(1)
         eid = buf(1)
         gpoint = buf(2)
         ASSIGN 20 TO retn
         ASSIGN 440 TO retn1
         kx = buf(3)
         nm = cn
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 20      refg = k
         sil = gpoint
         DO i = 1 , 6
            sild(i) = sil + i - 1
         ENDDO
         x2 = z(k+1)
         y2 = z(k+2)
         z2 = z(k+3)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ WEIGHT FACTORS AND COMPONENTS.
!     GENERATE WEIGHT VECTOR W
!
         CALL read(*460,*460,geomp,iwt,1,0,flag)
         IF ( pass==1 ) nwds = nwds + 1
         IF ( iwt/=-2 ) THEN
            IF ( iwt==-3 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL read(*460,*460,geomp,comp,1,0,flag)
            IF ( pass==1 ) nwds = nwds + 1
            ASSIGN 40 TO retn1
            kx = comp
            nm = cm
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
!
!     UM SET WAS SPECIFIED BY USER. REBUILD SILD WITH THE UM SET, AND
!     CHECK TOTAL NUMBER OF COMPONENTS FOR POSSIBLE ERROR
!
         ELSEIF ( pass==2 ) THEN
            DO
!
!     SKIP TO END OF CARD
!
               CALL read(*460,*460,geomp,j,1,0,flag)
               IF ( j==-3 ) THEN
!
!     UPDATE BEGIN POINTER, AND RETURN FOR ANOTHER RBE3 CARD
!
                  begn = begn + nwds
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ELSE
            jj = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      DO i = 1 , 6
            w(i) = zero
            IF ( buf(cm+i)/=0 ) w(i) = wt
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ GRID POINT, GET TRANSFORMATION MATRIX, AND SUMMING UP
!     WT MATRIX, AND FINALLY KNN MATRIX
!
         CALL read(*460,*460,geomp,grid,1,0,flag)
         IF ( pass==1 ) nwds = nwds + 1
         IF ( grid==-1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 60 TO retn
         gpoint = grid
         GOTO 440
 60      x1 = z(k+1)
         y1 = z(k+2)
         z1 = z(k+3)
         ASSIGN 420 TO retn2
         ASSIGN 80 TO retn3
         zk = z(k)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 80      CALL gmmats(t,6,6,0,unn,6,6,0,x)
         IF ( pass==2 ) THEN
!
!     INSERT THIS GRID MPC EQUATIONS
!
            CALL gmmats(tx,6,6,0,x,6,6,1,knn)
            DO i = 1 , 6
               DO j = 1 , 31 , 6
                  l = i + j - 1
                  knn(l) = knn(l)*w(i)
               ENDDO
            ENDDO
            DO i = 1 , 6
               IF ( buf(cn+i)/=0 ) THEN
                  sil = sild(i)
                  l = (i-1)*6
                  DO j = 1 , 6
                     IF ( buf(cm+j)/=0 ) THEN
                        ans = knn(l+j)
                        IF ( ans/=zero ) THEN
                           mcode(1) = gpoint + j - 1
                           mcode(2) = sil
                           coeff = ans
                           CALL write(rgt,mcode,2,0)
                           CALL write(rgt,coeff,1,0)
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
            DO i = 1 , 36
               tx(i) = x(i)
            ENDDO
            l = 0
            DO i = 1 , 6
               DO j = 1 , 6
                  x(l+j) = x(l+j)*w(i)
               ENDDO
               l = l + 6
            ENDDO
!
!     REPEAT FOR MORE GRID POINT
!
            CALL gmmats(tx,6,6,-1,x,6,6,0,knn)
         ENDIF
         spag_nextblock_1 = 4
      CASE (5)
         CALL read(*460,*460,geomp,grid,1,0,flag)
         nwds = nwds + 1
         IF ( grid==-3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*460,*460,geomp,comp,1,0,flag)
         nwds = nwds + 1
         ASSIGN 100 TO retn1
         kx = comp
         nm = cm
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 100     gpoint = grid
         ASSIGN 120 TO retn
         GOTO 440
 120     DO i = 1 , 6
            IF ( buf(cm+i)/=0 ) THEN
               IF ( jj>6 ) THEN
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO WHILE ( buf(cn+jj)==0 )
                  jj = jj + 1
                  IF ( jj>6 ) THEN
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               sild(jj) = gpoint + i - 1
               jj = jj + 1
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (6)
         IF ( pass==2 ) THEN
            begn = begn + nwds
            spag_nextblock_1 = 2
         ELSE
!
!     STORE DIAG TERMS WITH -1.
!     ADD DEPENDENT SIL TO THE END OF OPEN CORE VIA MU POINTER
!
            DO i = 1 , 6
               IF ( buf(cn+i)/=0 ) THEN
                  mcode(1) = sil + i - 1
                  mcode(2) = sild(i)
                  coeff = -1.
                  CALL write(rgt,mcode,2,0)
                  CALL write(rgt,coeff,1,0)
                  iz(Mu) = mcode(2)
                  Mu = Mu - 1
               ENDIF
            ENDDO
!
!     GET MATRIX READY FOR SECOND PASS, IN TX
!
            sing = -1
            CALL invers(6,knn,6,0,0,len,sing,x)
            IF ( sing==2 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 140 TO retn2
            zk = z(refg)
            spag_nextblock_1 = 13
         ENDIF
         CYCLE
 140     CALL gmmats(knn,6,6,0,t,6,6,0,tx)
!
!     BACK RECORD FOR 2ND PASS
!     SKIP TO WHERE WEIGHT FACTORS BEGIN
!
         CALL bckrec(geomp)
         pass = 2
         i = begn + 3
         CALL read(*460,*460,geomp,buf,-i,0,flag)
         spag_nextblock_1 = 3
      CASE (7)
!
!
!     READ CRSPLINE DATA ON INPUT FILE
!     ================================
!
!     INPUT DATA WILL BE SAVED IN RS ARRAY
!     3 WORDS SAVED FOR EACH GRID - BGPDT POINTER, COMPONENT, AND SIL
!
         CALL read(*480,*480,geomp,buf,3,0,flag)
         eid = buf(1)
         idl = buf(2)
         Rs(1) = buf(3)
         Rs(2) = -1
         Rs(3) = 0
         IF ( debug .OR. l38==1 ) WRITE (nout,99006) buf(1)
         k = 4
         DO
            CALL read(*460,*460,geomp,Rs(k),2,0,flag)
            IF ( Rs(k)/=-1 ) THEN
               Rs(k+2) = 0
               k = k + 3
               IF ( k>Mu ) CALL mesage(-8,0,name)
!
!     END OF INPUT FOR THIS RIGID ELEMENT, NOW COMPUTE LENGTH, INTERNAL
!     NUMBER (BGPDT POINTER), AND CHANGE GRID TO SIL
!
            ELSEIF ( k<8 ) THEN
!
!     ERROR MESSAGES
!
               msg = 131
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( debug ) CALL bug1('RS-     ',310,Rs,k)
               iend = k - 1
               len = zero
               ASSIGN 160 TO retn
!
!     DO 460 I = 1,IEND,3
               i = 1
               gpoint = Rs(i)
               GOTO 440
            ENDIF
         ENDDO
!
!     UPON RETURN FROM 1000, K IS BGPDT AND GPOINT IS SIL
!
 160     Rs(i) = k
         Rs(i+2) = gpoint
         IF ( debug ) WRITE (nout,99002) i , gpoint , k , z(k+1)
99002    FORMAT (/10X,'@430  I, NEW GPOINT & K=',I4,2I6,E11.3)
!
         IF ( i/=1 ) THEN
            x2 = z(k+1)
            y2 = z(k+2)
            z2 = z(k+3)
            len = len + sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
            x1 = x2
            y1 = y2
            z1 = z2
         ELSE
            x1 = z(k+1)
            y1 = z(k+2)
            z1 = z(k+3)
         ENDIF
         i = i + 3
         IF ( i<iend ) THEN
            gpoint = Rs(i)
            GOTO 440
         ELSE
!
            di = len*dl
            is = 1
            IF ( debug ) THEN
               CALL bug1('RS-     ',345,Rs,iend)
               WRITE (nout,99003) len , di
99003          FORMAT ('0  LEN,DI/@470 =',2E14.5)
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     COMPUTATION FOR EACH SEPARATED SPLINE
!     SET NUMBER OF SEGMENTS, NS
!
         ns = 0
         SPAG_Loop_1_1: DO i = is , iend , 3
            IF ( Rs(i+1)==0 ) EXIT SPAG_Loop_1_1
            ns = ns + 1
         ENDDO SPAG_Loop_1_1
!
!     IB = BEGIN,  IE = END,  IS = PRESENT SEGMENT
!     ND = NUMBER OF DEPENDENT POINTS
!
!     ZERO MTRAIX WORKING SPACE KNN ,GNN, AND T
!
         ie = i
         ib = is
         nd = ns - 1
         DO i = 1 , 36
            knn(i) = zero
            gnn(i) = zero
            t(i) = zero
         ENDDO
!
!     COMPUTE FLEXIBILITY MATRIX ZNN AND ITS INVERSE KNN, FOR EACH
!     SPLINE SEGMENT.
!
!     DO 540 I = 1,NS
         i = 0
         spag_nextblock_1 = 9
      CASE (9)
         i = i + 1
         i1 = Rs(is)
         i2 = Rs(is+3)
         ASSIGN 180 TO retn4
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 180     IF ( nogox==1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         x2 = z(i1+1)
         y2 = z(i1+2)
         z2 = z(i1+3)
         x1 = z(i2+1)
         y1 = z(i2+2)
         z1 = z(i2+3)
         x2 = (x2+x1)*half
         y2 = (y2+y1)*half
         z2 = (z2+z1)*half
         i1 = Rs(ie)
         x1 = z(i1+1)
         y1 = z(i1+2)
         z1 = z(i1+3)
!
!     FORM UNN USING BASIC UNN MATRIX
!     DO NOT DESTROY RIGID TRANSFER MATRIX
!
         ASSIGN 200 TO retn3
         GOTO 420
 200     CALL gmmats(unn,6,6,0,znn,6,6,0,snn)
!
!     SUM INTO KNN
!
         CALL gmmats(snn,6,6,-2,unn,6,6,1,knn)
         spag_nextblock_1 = 10
      CASE (10)
         is = is + 3
         IF ( i<ns ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         IF ( nogox==1 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     INVERT KNN
!
         sing = -1
         CALL invers(6,knn,6,0,0,len,sing,snn)
         IF ( sing==2 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     LOOP FOR FINAL CONSTRAINT EQUATIONS
!
         is = ib
         js = Rs(is)
         ii = 0
         spag_nextblock_1 = 11
      CASE (11)
         ii = ii + 1
         i1 = Rs(is)
         id = is + 3
         i2 = Rs(id)
         ASSIGN 220 TO retn4
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 220     x1 = z(i2+1)
         y1 = z(i2+2)
         z1 = z(i2+3)
         x2 = z(i1+1)
         y2 = z(i1+2)
         z2 = z(i1+3)
         x3 = z(i2+1)
         y3 = z(i2+2)
         z3 = z(i2+3)
         x2 = (x2+x3)*half
         y2 = (y2+y3)*half
         z2 = (z2+z3)*half
!
!     Y I+1 I X   S I+1 S
!
         ASSIGN 240 TO retn3
         GOTO 420
 240     CALL gmmats(unn,6,6,0,znn,6,6,0,snn)
         CALL gmmats(snn,6,6,0,unn,6,6,1,y)
         x2 = z(i1+1)
         y2 = z(i1+2)
         z2 = z(i1+3)
!
!     S I+1 I X GIN
!
         ASSIGN 260 TO retn3
         GOTO 420
 260     CALL gmmats(unn,6,6,0,gnn,6,6,0,snn)
         i3 = Rs(ie)
         x3 = z(i3+1)
         y3 = z(i3+2)
         z3 = z(i3+3)
         x2 = z(js+1)
         y2 = z(js+2)
         z2 = z(js+3)
!
!     GNN = G I+1 N
!
         ASSIGN 280 TO retn3
         unn(5) = c(3) - a(3)
         unn(6) = a(2) - c(2)
         unn(10) = a(3) - c(3)
         unn(12) = c(1) - a(1)
         unn(16) = c(2) - a(2)
         unn(17) = a(1) - c(1)
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 280     CALL gmmats(y,6,6,0,unn,6,6,1,znn)
         CALL gmmats(znn,6,6,0,knn,6,6,0,gnn)
         DO j = 1 , 36
            gnn(j) = gnn(j) + snn(j)
         ENDDO
!
!     Y = G I+1 1
!
         ASSIGN 300 TO retn3
         unn(5) = c(3) - b(3)
         unn(6) = b(2) - c(2)
         unn(10) = b(3) - c(3)
         unn(12) = c(1) - b(1)
         unn(16) = c(2) - b(2)
         unn(17) = b(1) - c(1)
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 300     CALL gmmats(gnn,6,6,0,unn,6,6,0,snn)
         ASSIGN 320 TO retn3
         GOTO 420
 320     DO j = 1 , 36
            y(j) = unn(j) - snn(j)
         ENDDO
!
!     TRANSFORM TO GLOBAL AND STORE ANSWERS IN Y AND SNN
!
         ASSIGN 340 TO retn2
         zk = z(i2)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 340     CALL gmmats(t,6,6,1,y,6,6,0,snn)
         CALL gmmats(t,6,6,1,gnn,6,6,0,znn)
         ASSIGN 360 TO retn2
         zk = z(js)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 360     CALL gmmats(snn,6,6,0,t,6,6,0,y)
         ASSIGN 380 TO retn2
         zk = z(i3)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 380     CALL gmmats(znn,6,6,0,t,6,6,0,snn)
!
!     Y = G I 1  SNN = G I N
!
         ASSIGN 400 TO retn1
         kx = Rs(id+1)
         nm = cm
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
!
!     ADD DEPENDENT TO LIST AND MPC EQUATIONS TO RGT
!
 400     IF ( debug ) THEN
            WRITE (nout,99007) y
            WRITE (nout,99007) snn
         ENDIF
         DO j = 1 , 6
            IF ( buf(cm+j)/=0 ) THEN
!
!     SELF TERM FOR DEPENDENT SIL
!
               sil = Rs(id+2) + j - 1
               mcode(1) = sil
               mcode(2) = sil
               coeff = -1.
               CALL write(rgt,mcode,2,0)
               CALL write(rgt,coeff,1,0)
               iz(Mu) = mcode(2)
               Mu = Mu - 1
               IF ( ii>=Mu ) CALL mesage(-8,0,name)
               ll = (j-1)*6
!
!     END ONE DEPENDENT
!
               DO l = 1 , 6
                  ans = y(ll+l)
!
!     TEST FOR COMPUTED ZERO
!
                  espx = eps
                  IF ( j>3 .AND. l<4 ) espx = espx/leng
                  IF ( j<4 .AND. l>3 ) espx = espx*leng
                  IF ( abs(ans)>=espx ) THEN
                     mcode(1) = Rs(ib+2) + l - 1
                     mcode(2) = sil
                     coeff = ans
                     CALL write(rgt,mcode,2,0)
                     CALL write(rgt,coeff,1,0)
                  ENDIF
               ENDDO
!
!     END N INDEPENDENT
!
               DO l = 1 , 6
                  ans = snn(ll+l)
!
!     TEST FOR COMPUTED ZERO
!
                  espx = eps
                  IF ( j>3 .AND. l<4 ) espx = espx/leng
                  IF ( j<4 .AND. l>3 ) espx = espx*leng
                  IF ( abs(ans)>=espx ) THEN
                     mcode(1) = Rs(ie+2) + l - 1
                     mcode(2) = sil
                     coeff = ans
                     CALL write(rgt,mcode,2,0)
                     CALL write(rgt,coeff,1,0)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
!
         is = is + 3
         IF ( ii<nd ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     END BIG DO (720) LOOP
!
         IF ( ie+2>=iend ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         is = ie
         Rs(is+1) = -1
         spag_nextblock_1 = 8
      CASE (13)
!
!     ----------------------------------------------------
!
!     INTERNAL ROUTINE TO BUILD 6X6 BASIC TO GLOBAL MATRIX
!     (T = 0 ON ENTRY)
!
         CALL transs(zk,d)
         j = 1
         DO i = 1 , 15 , 6
            t(i) = d(j)
            t(i+1) = d(j+1)
            t(i+2) = d(j+2)
            t(i+21) = d(j)
            t(i+22) = d(j+1)
            t(i+23) = d(j+2)
            j = j + 3
         ENDDO
         GOTO retn2
!
!     INTERNAL ROUTINE TO MAKE RIGID BODY TRANSFER MATRIX FOR CRSPLINES
!     (UNN = IDENTITY MATRIX ON ENTRY)
!
 420     unn(5) = a(3) - b(3)
         unn(6) = b(2) - a(2)
         unn(10) = b(3) - a(3)
         unn(12) = a(1) - b(1)
         unn(16) = a(2) - b(2)
         unn(17) = b(1) - a(1)
         spag_nextblock_1 = 14
      CASE (14)
         GOTO retn3
      CASE (15)
!
!     INTERNAL ROUTINE TO FORM FLEXIBILITY MATRIX FOR CRSPLINE
!
         DO i = 1 , 36
            znn(i) = zero
         ENDDO
         x1 = z(i1+1)
         y1 = z(i1+2)
         z1 = z(i1+3)
         x2 = z(i2+1)
         y2 = z(i2+2)
         z2 = z(i2+3)
         leng = sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
         IF ( leng==zero ) THEN
            CALL mesage(30,31,eid)
            nogox = 1
         ELSE
            znn(22) = leng
            znn(29) = leng
            znn(36) = leng
            fac = leng/12.0*((3.0*di**2)/(2.0*leng**2)-one)
            ln3 = leng**3/12.0
            znn(1) = ln3 + fac*(x2-x1)**2
            znn(2) = fac*(x2-x1)*(y2-y1)
            znn(3) = fac*(x2-x1)*(z2-z1)
            znn(7) = znn(2)
            znn(8) = ln3 + fac*(y2-y1)**2
            znn(9) = fac*(y2-y1)*(z2-z1)
            znn(13) = znn(3)
            znn(14) = znn(9)
            znn(15) = ln3 + fac*(z2-z1)**2
         ENDIF
         GOTO retn4
      CASE (16)
!
!     INTERNAL ROUTINE TO ABSTRACT CODED DOF
!
         DO i = 1 , 6
            buf(nm+i) = 0
         ENDDO
         IF ( kx>0 ) THEN
            SPAG_Loop_1_2: DO i = 1 , 6
               k1 = kx/10
               k2 = kx - k1*10
               IF ( k2>6 ) EXIT SPAG_Loop_1_2
               buf(nm+k2) = k2
               IF ( k1==0 ) EXIT SPAG_Loop_1_2
               kx = k1
            ENDDO SPAG_Loop_1_2
         ENDIF
         GOTO retn1
!
!     INTERNAL ROUTINE TO PERFORM BINARY SEARCH IN EQEXIN AND
!     CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
!
 440     klo = 0
         khi = kn2
         lastk = 0
         DO
            k = (klo+khi+1)/2
            IF ( lastk==k ) THEN
               WRITE (nout,99004) ufm , gpoint , eid
99004          FORMAT (A23,', UNDEFINED GRID POINT',I9,' SPECIFIED BY RIGID ','ELEMENT ID',I9)
               times = times + 1
               IF ( times>50 ) CALL mesage(-37,0,name)
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ELSE
               lastk = k
               IF ( gpoint<iz(2*k-1) ) THEN
                  khi = k
               ELSEIF ( gpoint==iz(2*k-1) ) THEN
                  k = iz(2*k)
                  gpoint = iz(k+2*kn)
                  k = (k-1)*4 + Bp
                  IF ( gpoint+5>mask15 ) N23 = 3
                  GOTO retn
               ELSE
                  klo = k
               ENDIF
            ENDIF
         ENDDO
 460     CALL mesage(-3,geomp,name)
         spag_nextblock_1 = 17
      CASE (17)
         msg = 38
         spag_nextblock_1 = 18
      CASE (18)
         CALL mesage(30,msg,eid)
         spag_nextblock_1 = 20
      CASE (19)
         WRITE (nout,99005) ufm , eid
99005    FORMAT (A23,', RIGID ELEMENT CRBE3',I9,' HAS ILLEGAL UM SET ','SPECIFICATION')
         spag_nextblock_1 = 21
      CASE (20)
!
         nogo = 1
         nogox = 0
         IF ( Jump==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Jump==2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
!
!     REPOSITION GEOMP FILE FOR NEXT CRBE3 INPUT CARD
!
         nogo = 1
         nogox = 0
         CALL bckrec(geomp)
         i = begn + 1
         CALL read(*460,*460,geomp,j,-i,0,flag)
         DO
            CALL read(*460,*460,geomp,j,1,0,flag)
            i = i + 1
            IF ( j==-3 ) THEN
               begn = i
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
 480     IF ( nogox==1 ) nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
99007 FORMAT ('0  CRSPLS/@670',/,(2X,10E12.4))
END SUBROUTINE crspls
