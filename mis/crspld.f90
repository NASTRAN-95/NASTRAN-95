
SUBROUTINE crspld(*,Jump,Mu,Bp,Rs,Again,N23)
!
!     THIS ROUTINE HANDLES CRBE3 AND CRSPLINE RIGID ELEMENTS
!     CALLED ONLY BY CRIGGP SUBROUTINE
!
!     DOUBLE PRECISION VERSION
!
   IMPLICIT NONE
   DOUBLE PRECISION A(3) , B(3) , C(3) , X1 , X2 , X3 , Y1 , Y2 , Y3 , Z1 , Z2 , Z3
   INTEGER Bgpdt , Buf(20) , Buf1 , Buf2 , Buf3 , Buf4 , Cstm , Geomp , Gpoint , Iz(1) , Kn , Knkl1 , Nogo , Nout , Rgt , Sysbuf ,  &
         & Two16
   CHARACTER*23 Ufm
   REAL Z(1)
   COMMON /crsplx/ X1 , Y1 , Z1 , X2 , Y2 , Z2 , X3 , Y3 , Z3
   COMMON /gp4fil/ Geomp , Bgpdt , Cstm , Rgt
   COMMON /gp4prm/ Buf , Buf1 , Buf2 , Buf3 , Buf4 , Knkl1 , Two16 , Nogo , Gpoint , Kn
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Iz
   LOGICAL Again
   INTEGER Bp , Jump , Mu , N23
   INTEGER Rs(3)
   DOUBLE PRECISION ans , d(9) , di , eps , espx , fac , gnn(36) , half , knn(36) , len , leng , ln3 , one , snn(36) , t(36) ,      &
                  & tx(36) , unn(36) , w(6) , x(36) , y(36) , zero , znn(36)
   INTEGER begn , cm , cn , comp , eid , flag , grid , i , i1 , i2 , i3 , ib , id , idl , ie , iend , ii , is , iwt , j , jj , js , &
         & k , k1 , k2 , khi , klo , kn2 , kx , l , l38 , lastk , ll , mask15 , mcode(2) , msg , name(2) , nd , nm , nogox , ns ,   &
         & nwds , pass , refg , retn , retn1 , retn2 , retn3 , retn4 , sil , sild(6) , sing , times
   REAL coeff , dl , wt , zk
   LOGICAL debug
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (wt,iwt) , (dl,idl) , (X1,A(1)) , (X2,B(1)) , (X3,C(1))
   DATA one , zero , half , eps , times , debug/1.0D+0 , 0.0D+0 , 0.5D+0 , 1.0D-10 , 0 , .FALSE./
   DATA cm , cn , nogox , mask15 , name/6 , 12 , 0 , 32767 , 4HCRSP , 4HLD  /
!
   IF ( Again ) RETURN 1
   IF ( debug ) WRITE (Nout,99001) Kn , Knkl1 , Bp , Geomp , Bgpdt , Cstm , Rgt , Jump
99001 FORMAT ('0  CRSPLD DEBUG- KN,KNKL1,BP,GEOMP,BGPDT,CSTM,RGT,JUMP=',/3X,8I7)
   kn2 = Kn/2
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
!     JUMP=1 FOR CRBE3 DATA,  JUMP=2 FOR CRSPLINE DATA
!
   IF ( Jump==2 ) GOTO 1300
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
 100  pass = 1
   DO i = 1 , 36
      t(i) = zero
      knn(i) = zero
   ENDDO
   CALL read(*4400,*4400,Geomp,Buf,3,0,flag)
   nwds = 3
   IF ( debug .OR. l38==1 ) WRITE (Nout,99006) Buf(1)
   eid = Buf(1)
   Gpoint = Buf(2)
   ASSIGN 200 TO retn
   ASSIGN 3700 TO retn1
   kx = Buf(3)
   nm = cn
   GOTO 3600
 200  refg = k
   sil = Gpoint
   DO i = 1 , 6
      sild(i) = sil + i - 1
   ENDDO
   X2 = Z(k+1)
   Y2 = Z(k+2)
   Z2 = Z(k+3)
!
!     READ WEIGHT FACTORS AND COMPONENTS.
!     GENERATE WEIGHT VECTOR W
!
 300  CALL read(*3800,*3800,Geomp,iwt,1,0,flag)
   IF ( pass==1 ) nwds = nwds + 1
   IF ( iwt/=-2 ) THEN
      IF ( iwt==-3 ) GOTO 1100
      CALL read(*3800,*3800,Geomp,comp,1,0,flag)
      IF ( pass==1 ) nwds = nwds + 1
      ASSIGN 400 TO retn1
      kx = comp
      nm = cm
      GOTO 3600
!
!     UM SET WAS SPECIFIED BY USER. REBUILD SILD WITH THE UM SET, AND
!     CHECK TOTAL NUMBER OF COMPONENTS FOR POSSIBLE ERROR
!
   ELSEIF ( pass==2 ) THEN
      DO
!
!     SKIP TO END OF CARD
!
         CALL read(*3800,*3800,Geomp,j,1,0,flag)
         IF ( j==-3 ) THEN
!
!     UPDATE BEGIN POINTER, AND RETURN FOR ANOTHER CRBE3 CARD
!
            begn = begn + nwds
            GOTO 100
         ENDIF
      ENDDO
   ELSE
      jj = 1
      GOTO 800
   ENDIF
 400  DO i = 1 , 6
      w(i) = zero
      IF ( Buf(cm+i)/=0 ) w(i) = wt
   ENDDO
!
!     READ GRID POINT, GET TRANSFORMATION MATRIX, AND SUMMING UP
!     WT MATRIX, AND FINALLY KNN MATRIX
!
 500  CALL read(*3800,*3800,Geomp,grid,1,0,flag)
   IF ( pass==1 ) nwds = nwds + 1
   IF ( grid==-1 ) GOTO 300
   ASSIGN 600 TO retn
   Gpoint = grid
   GOTO 3700
 600  X1 = Z(k+1)
   Y1 = Z(k+2)
   Z1 = Z(k+3)
   ASSIGN 3300 TO retn2
   ASSIGN 700 TO retn3
   zk = Z(k)
   GOTO 3200
 700  CALL gmmatd(t,6,6,0,unn,6,6,0,x)
   IF ( pass==2 ) THEN
!
!     INSERT THIS GRID MPC EQUATIONS
!
      CALL gmmatd(tx,6,6,0,x,6,6,1,knn)
      DO i = 1 , 6
         DO j = 1 , 31 , 6
            l = i + j - 1
            knn(l) = knn(l)*w(i)
         ENDDO
      ENDDO
      DO i = 1 , 6
         IF ( Buf(cn+i)/=0 ) THEN
            sil = sild(i)
            l = (i-1)*6
            DO j = 1 , 6
               IF ( Buf(cm+j)/=0 ) THEN
                  ans = knn(l+j)
                  IF ( ans/=zero ) THEN
                     mcode(1) = Gpoint + j - 1
                     mcode(2) = sil
                     coeff = ans
                     CALL write(Rgt,mcode,2,0)
                     CALL write(Rgt,coeff,1,0)
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
      CALL gmmatd(tx,6,6,-1,x,6,6,0,knn)
   ENDIF
   GOTO 500
 800  CALL read(*3800,*3800,Geomp,grid,1,0,flag)
   nwds = nwds + 1
   IF ( grid==-3 ) GOTO 1100
   CALL read(*3800,*3800,Geomp,comp,1,0,flag)
   nwds = nwds + 1
   ASSIGN 900 TO retn1
   kx = comp
   nm = cm
   GOTO 3600
 900  Gpoint = grid
   ASSIGN 1000 TO retn
   GOTO 3700
 1000 DO i = 1 , 6
      IF ( Buf(cm+i)/=0 ) THEN
         IF ( jj>6 ) GOTO 4100
         DO WHILE ( Buf(cn+jj)==0 )
            jj = jj + 1
            IF ( jj>6 ) GOTO 4100
         ENDDO
         sild(jj) = Gpoint + i - 1
         jj = jj + 1
      ENDIF
   ENDDO
   GOTO 800
 1100 IF ( pass==2 ) THEN
      begn = begn + nwds
      GOTO 100
   ELSE
!
!     STORE DIAG TERMS WITH -1.
!     ADD DEPENDENT SIL TO THE END OF OPEN CORE VIA MU POINTER
!
      DO i = 1 , 6
         IF ( Buf(cn+i)/=0 ) THEN
            mcode(1) = sil + i - 1
            mcode(2) = sild(i)
            coeff = -1.
            CALL write(Rgt,mcode,2,0)
            CALL write(Rgt,coeff,1,0)
            Iz(Mu) = mcode(2)
            Mu = Mu - 1
         ENDIF
      ENDDO
!
!     GET MATRIX READY FOR SECOND PASS, IN TX
!
      sing = -1
      CALL inverd(6,knn,6,0,0,len,sing,x)
      IF ( sing==2 ) GOTO 3900
      ASSIGN 1200 TO retn2
      zk = Z(refg)
      GOTO 3200
   ENDIF
 1200 CALL gmmatd(knn,6,6,0,t,6,6,0,tx)
!
!     BACK RECORD FOR 2ND PASS
!     SKIP TO WHERE WEIGHT FACTORS BEGIN
!
   CALL bckrec(Geomp)
   pass = 2
   i = begn + 3
   CALL read(*3800,*3800,Geomp,Buf,-i,0,flag)
   GOTO 300
!
!
!     READ CRSPLINE DATA ON INPUT FILE
!     ================================
!
!     INPUT DATA WILL BE SAVED IN RS ARRAY
!     3 WORDS SAVED FOR EACH GRID - BGPDT POINTER, COMPONENT, AND SIL
!
 1300 CALL read(*4400,*4400,Geomp,Buf,3,0,flag)
   eid = Buf(1)
   idl = Buf(2)
   Rs(1) = Buf(3)
   Rs(2) = -1
   Rs(3) = 0
   IF ( debug .OR. l38==1 ) WRITE (Nout,99006) Buf(1)
   k = 4
   DO
      CALL read(*3800,*3800,Geomp,Rs(k),2,0,flag)
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
         GOTO 4000
      ELSE
!WKBD IF (DEBUG) CALL BUG1 ('RS-     ',310,RS,K)
         iend = k - 1
         len = zero
         ASSIGN 1400 TO retn
!
         i = 1
         Gpoint = Rs(i)
         GOTO 3700
      ENDIF
   ENDDO
!
!     UPON RETURN FROM 1000, K IS BGPDT AND GPOINT IS SIL
!
 1400 Rs(i) = k
   Rs(i+2) = Gpoint
   IF ( debug ) WRITE (Nout,99002) i , Gpoint , k , Z(k+1)
99002 FORMAT (/10X,'@430  I, NEW GPOINT & K=',I4,2I6,E11.3)
!
   IF ( i/=1 ) THEN
      X2 = Z(k+1)
      Y2 = Z(k+2)
      Z2 = Z(k+3)
      len = len + dsqrt((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
      X1 = X2
      Y1 = Y2
      Z1 = Z2
   ELSE
      X1 = Z(k+1)
      Y1 = Z(k+2)
      Z1 = Z(k+3)
   ENDIF
   i = i + 3
   IF ( i<iend ) THEN
      Gpoint = Rs(i)
      GOTO 3700
   ELSE
!
      di = len*dl
      is = 1
      IF ( debug ) THEN
!WKBD CALL BUG1 ('RS-     ',345,RS,IEND)
         WRITE (Nout,99003) len , di
99003    FORMAT ('0  LEN,DI/@470 =',2D14.5)
      ENDIF
   ENDIF
!
!     COMPUTATION FOR EACH SEPARATED SPLINE
!     SET NUMBER OF SEGMENTS, NS
!
 1500 ns = 0
   DO i = is , iend , 3
      IF ( Rs(i+1)==0 ) EXIT
      ns = ns + 1
   ENDDO
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
 1600 i = i + 1
   i1 = Rs(is)
   i2 = Rs(is+3)
   ASSIGN 1700 TO retn4
   GOTO 3500
 1700 IF ( nogox==1 ) GOTO 1900
   X2 = Z(i1+1)
   Y2 = Z(i1+2)
   Z2 = Z(i1+3)
   X1 = Z(i2+1)
   Y1 = Z(i2+2)
   Z1 = Z(i2+3)
   X2 = (X2+X1)*half
   Y2 = (Y2+Y1)*half
   Z2 = (Z2+Z1)*half
   i1 = Rs(ie)
   X1 = Z(i1+1)
   Y1 = Z(i1+2)
   Z1 = Z(i1+3)
!
!     FORM UNN USING BASIC UNN MATRIX
!     DO NOT DESTROY RIGID TRANSFER MATRIX
!
   ASSIGN 1800 TO retn3
   GOTO 3300
 1800 CALL gmmatd(unn,6,6,0,znn,6,6,0,snn)
!
!     SUM INTO KNN
!
   CALL gmmatd(snn,6,6,-2,unn,6,6,1,knn)
 1900 is = is + 3
   IF ( i<ns ) GOTO 1600
!
   IF ( nogox==1 ) GOTO 3100
!
!     INVERT KNN
!
   sing = -1
   CALL inverd(6,knn,6,0,0,len,sing,snn)
   IF ( sing==2 ) GOTO 3900
!
!     LOOP FOR FINAL CONSTRAINT EQUATIONS
!
   is = ib
   js = Rs(is)
   ii = 0
 2000 ii = ii + 1
   i1 = Rs(is)
   id = is + 3
   i2 = Rs(id)
   ASSIGN 2100 TO retn4
   GOTO 3500
 2100 X1 = Z(i2+1)
   Y1 = Z(i2+2)
   Z1 = Z(i2+3)
   X2 = Z(i1+1)
   Y2 = Z(i1+2)
   Z2 = Z(i1+3)
   X3 = Z(i2+1)
   Y3 = Z(i2+2)
   Z3 = Z(i2+3)
   X2 = (X2+X3)*half
   Y2 = (Y2+Y3)*half
   Z2 = (Z2+Z3)*half
!
!     Y I+1 I X   S I+1 S
!
   ASSIGN 2200 TO retn3
   GOTO 3300
 2200 CALL gmmatd(unn,6,6,0,znn,6,6,0,snn)
   CALL gmmatd(snn,6,6,0,unn,6,6,1,y)
   X2 = Z(i1+1)
   Y2 = Z(i1+2)
   Z2 = Z(i1+3)
!
!     S I+1 I X GIN
!
   ASSIGN 2300 TO retn3
   GOTO 3300
 2300 CALL gmmatd(unn,6,6,0,gnn,6,6,0,snn)
   i3 = Rs(ie)
   X3 = Z(i3+1)
   Y3 = Z(i3+2)
   Z3 = Z(i3+3)
   X2 = Z(js+1)
   Y2 = Z(js+2)
   Z2 = Z(js+3)
!
!     GNN = G I+1 N
!
   ASSIGN 2400 TO retn3
   unn(5) = C(3) - A(3)
   unn(6) = A(2) - C(2)
   unn(10) = A(3) - C(3)
   unn(12) = C(1) - A(1)
   unn(16) = C(2) - A(2)
   unn(17) = A(1) - C(1)
   GOTO 3400
 2400 CALL gmmatd(y,6,6,0,unn,6,6,1,znn)
   CALL gmmatd(znn,6,6,0,knn,6,6,0,gnn)
   DO j = 1 , 36
      gnn(j) = gnn(j) + snn(j)
   ENDDO
!
!     Y = G I+1 1
!
   ASSIGN 2500 TO retn3
   unn(5) = C(3) - B(3)
   unn(6) = B(2) - C(2)
   unn(10) = B(3) - C(3)
   unn(12) = C(1) - B(1)
   unn(16) = C(2) - B(2)
   unn(17) = B(1) - C(1)
   GOTO 3400
 2500 CALL gmmatd(gnn,6,6,0,unn,6,6,0,snn)
   ASSIGN 2600 TO retn3
   GOTO 3300
 2600 DO j = 1 , 36
      y(j) = unn(j) - snn(j)
   ENDDO
!
!     TRANSFORM TO GLOBAL AND STORE ANSWERS IN Y AND SNN
!
   ASSIGN 2700 TO retn2
   zk = Z(i2)
   GOTO 3200
 2700 CALL gmmatd(t,6,6,1,y,6,6,0,snn)
   CALL gmmatd(t,6,6,1,gnn,6,6,0,znn)
   ASSIGN 2800 TO retn2
   zk = Z(js)
   GOTO 3200
 2800 CALL gmmatd(snn,6,6,0,t,6,6,0,y)
   ASSIGN 2900 TO retn2
   zk = Z(i3)
   GOTO 3200
 2900 CALL gmmatd(znn,6,6,0,t,6,6,0,snn)
!
!     Y = G I 1  SNN = G I N
!
   ASSIGN 3000 TO retn1
   kx = Rs(id+1)
   nm = cm
   GOTO 3600
!
!     ADD DEPENDENT TO LIST AND MPC EQUATIONS TO RGT
!
 3000 IF ( debug ) THEN
      WRITE (Nout,99007) y
      WRITE (Nout,99007) snn
   ENDIF
   DO j = 1 , 6
      IF ( Buf(cm+j)/=0 ) THEN
!
!     SELF TERM FOR DEPENDENT SIL
!
         sil = Rs(id+2) + j - 1
         mcode(1) = sil
         mcode(2) = sil
         coeff = -1.
         CALL write(Rgt,mcode,2,0)
         CALL write(Rgt,coeff,1,0)
         Iz(Mu) = mcode(2)
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
            IF ( dabs(ans)>=espx ) THEN
               mcode(1) = Rs(ib+2) + l - 1
               mcode(2) = sil
               coeff = ans
               CALL write(Rgt,mcode,2,0)
               CALL write(Rgt,coeff,1,0)
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
            IF ( dabs(ans)>=espx ) THEN
               mcode(1) = Rs(ie+2) + l - 1
               mcode(2) = sil
               coeff = ans
               CALL write(Rgt,mcode,2,0)
               CALL write(Rgt,coeff,1,0)
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
   is = is + 3
   IF ( ii<nd ) GOTO 2000
!
!     END BIG DO (720) LOOP
!
 3100 IF ( ie+2>=iend ) GOTO 1300
   is = ie
   Rs(is+1) = -1
   GOTO 1500
!
!     ----------------------------------------------------
!
!     INTERNAL ROUTINE TO BUILD 6X6 BASIC TO GLOBAL MATRIX
!     (T = 0 ON ENTRY)
!
 3200 CALL transd(zk,d)
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
 3300 unn(5) = A(3) - B(3)
   unn(6) = B(2) - A(2)
   unn(10) = B(3) - A(3)
   unn(12) = A(1) - B(1)
   unn(16) = A(2) - B(2)
   unn(17) = B(1) - A(1)
 3400 GOTO retn3
!
!     INTERNAL ROUTINE TO FORM FLEXIBILITY MATRIX FOR CRSPLINE
!
 3500 DO i = 1 , 36
      znn(i) = zero
   ENDDO
   X1 = Z(i1+1)
   Y1 = Z(i1+2)
   Z1 = Z(i1+3)
   X2 = Z(i2+1)
   Y2 = Z(i2+2)
   Z2 = Z(i2+3)
   leng = dsqrt((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
   IF ( leng==zero ) THEN
      CALL mesage(30,31,eid)
      nogox = 1
   ELSE
      znn(22) = leng
      znn(29) = leng
      znn(36) = leng
      fac = leng/12.0D+0*((3.0D+0*di**2)/(2.0D+0*leng**2)-one)
      ln3 = leng**3/12.0D+0
      znn(1) = ln3 + fac*(X2-X1)**2
      znn(2) = fac*(X2-X1)*(Y2-Y1)
      znn(3) = fac*(X2-X1)*(Z2-Z1)
      znn(7) = znn(2)
      znn(8) = ln3 + fac*(Y2-Y1)**2
      znn(9) = fac*(Y2-Y1)*(Z2-Z1)
      znn(13) = znn(3)
      znn(14) = znn(9)
      znn(15) = ln3 + fac*(Z2-Z1)**2
   ENDIF
   GOTO retn4
!
!     INTERNAL ROUTINE TO ABSTRACT CODED DOF
!
 3600 DO i = 1 , 6
      Buf(nm+i) = 0
   ENDDO
   IF ( kx>0 ) THEN
      DO i = 1 , 6
         k1 = kx/10
         k2 = kx - k1*10
         IF ( k2>6 ) EXIT
         Buf(nm+k2) = k2
         IF ( k1==0 ) EXIT
         kx = k1
      ENDDO
   ENDIF
   GOTO retn1
!
!     INTERNAL ROUTINE TO PERFORM BINARY SEARCH IN EQEXIN AND
!     CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
!
 3700 klo = 0
   khi = kn2
   lastk = 0
   DO
      k = (klo+khi+1)/2
      IF ( lastk==k ) THEN
         WRITE (Nout,99004) Ufm , Gpoint , eid
99004    FORMAT (A23,', UNDEFINED GRID POINT',I9,' SPECIFIED BY RIGID ','ELEMENT ID',I9)
         times = times + 1
         IF ( times>50 ) CALL mesage(-37,0,name)
         GOTO 4200
      ELSE
         lastk = k
         IF ( Gpoint<Iz(2*k-1) ) THEN
            khi = k
         ELSEIF ( Gpoint==Iz(2*k-1) ) THEN
            k = Iz(2*k)
            Gpoint = Iz(k+2*Kn)
            k = (k-1)*4 + Bp
            IF ( Gpoint+5>mask15 ) N23 = 3
            GOTO retn
         ELSE
            klo = k
         ENDIF
      ENDIF
   ENDDO
 3800 CALL mesage(-3,Geomp,name)
 3900 msg = 38
 4000 CALL mesage(30,msg,eid)
   GOTO 4200
 4100 WRITE (Nout,99005) Ufm , eid
99005 FORMAT (A23,', RIGID ELEMENT CRBE3',I9,' HAS ILLEGAL UM SET ','SPECIFICATION')
   GOTO 4300
!
 4200 Nogo = 1
   nogox = 0
   IF ( Jump==1 ) GOTO 100
   IF ( Jump==2 ) GOTO 1300
!
!     REPOSITION GEOMP FILE FOR NEXT CRBE3 INPUT CARD
!
 4300 Nogo = 1
   nogox = 0
   CALL bckrec(Geomp)
   i = begn + 1
   CALL read(*3800,*3800,Geomp,j,-i,0,flag)
   DO
      CALL read(*3800,*3800,Geomp,j,1,0,flag)
      i = i + 1
      IF ( j==-3 ) THEN
         begn = i
         GOTO 100
      ENDIF
   ENDDO
!
 4400 IF ( nogox==1 ) Nogo = 1
99006 FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
99007 FORMAT ('0  CRSPLD/@670',/,(2X,10D12.4))
END SUBROUTINE crspld