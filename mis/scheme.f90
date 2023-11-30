
SUBROUTINE scheme(Ig,Inv,Ii3,Int,Icc,Ild,Norig,Ip,Un,Z)
   IMPLICIT NONE
   REAL Dum2(2) , Dum4a(4) , Dum7d(7) , Gdum(3)
   INTEGER Ibuf , Ibuf1 , Icrit , Ifl , Ipass , Kdim , Kmod , Kore , Mach , Maxdeg , Maxgrd , Method , Mindeg , Mm , Nbitin ,       &
         & Nedge , Neq , Neqr , Ngrid , Nn , Nout , Nw , Rd , Rdrew , Rew , Scr1 , Wrt , Wrtrew
   COMMON /banda / Ibuf1 , Dum4a , Method , Icrit
   COMMON /bandb / Nbitin , Kore , Ifl , Ngrid , Ipass , Nw , Kdim
   COMMON /bandd / Dum7d , Neq , Neqr
   COMMON /bands / Nn , Mm , Dum2 , Maxgrd , Maxdeg , Kmod , Mach , Mindeg , Nedge
   COMMON /geomx / Gdum , Scr1
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /system/ Ibuf , Nout
   INTEGER Ii3
   INTEGER Icc(1) , Ig(1) , Ild(1) , Int(1) , Inv(1) , Ip(1) , Norig(1) , Z(1)
   REAL Un(1)
   INTEGER i , j , jump , k , k1 , k2 , k3 , k4 , k5 , loc , m , ngrid1 , sub(2)
!
   DATA sub/4HSCHE , 4HME  /
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     ZERO OUT CORE SPACE AND SET BANDWIDTH IMPROVEMENT FLAG, JUMP
!     JUMP = 1,  NO IMPROVEMENT OF CRITERION SELECTED
!          = 0,  IMPROVEMENT
!
   DO i = 1 , Kore
      Z(i) = 0
   ENDDO
   jump = 1
!
!     READ ELEMENT DATA FROM GEOM2 FILE AND SET UP CONNECTION TABLE IG.
!     ALSO, EXAMINE MPC EQUATIONS.
!
   CALL bread(Ig,Inv,Ii3,Norig,Z)
   IF ( Ngrid<=0 ) RETURN
!
!     NGRID = NO. OF GRID POINTS IN THE PROBLEM
!           =  0, ONE OR MORE SEQGP CARD IS PRESENT IN NASTRAN INPUT
!                 DECK, AND/OR QDSEP ELEMENTS
!           = -1, INSUFFICIENT CORE SPACE (IG TABLE TOO SMALL)
!           = -2, INSUFFICIENT SCRATCH AREA WHILE USING CTHMCK
!           = -3, INSUFFICIENT SCRATCH AREA WHILE USING GIBSTK
!
!     MODIFY IG TO ACCOUNT FOR MPC EQUATIONS AND RIGID ELEMENTS
!
   IF ( Neq+Neqr/=0 ) CALL tiger(Ig,Icc,Inv,Ii3,Norig,Z,Un)
!
!     SORT ORIGINAL GRID NOS. AND OUTPUT THE LIST IN INT, WHERE INT(I)
!     IS THE I-TH ORIGINAL GRID NUMBER.
!     ALSO OUTPUT ILD, WHERE IDL(I) = SORTED INTERNAL NO. CORRESPONDING
!     TO THE UNSORTED BANDIT INTERNAL LABEL I.
!
!     CALL BRIGIT (INV,II3,INT,ILD)
!     BRIGIT AND INTERN ARE NOW REPLACED BY 17 LINES BELOW /G.CHAN 1988
!
   k = 0
   DO i = 1 , Ii3
      IF ( Inv(i)/=0 ) THEN
         k = k + 1
         Int(k) = Inv(i)
      ENDIF
   ENDDO
   CALL sort(0,0,1,1,Int,Nn)
   DO i = 1 , Nn
      j = Int(i)
      IF ( j<=0 ) GOTO 600
      loc = j - 1
      DO
         loc = mod(loc,Kmod) + 1
         IF ( Inv(loc)==0 ) GOTO 600
         IF ( Inv(loc)==j ) THEN
            j = Inv(loc+Ii3)
            Ild(j) = i
            EXIT
         ENDIF
      ENDDO
   ENDDO
!
!     METHOD WAS SET IN BANDIT -
!     METHOD = -1, CM ONLY,    = +1, GPS ONLY,    = 0, BOTH METHODS.
!
   IF ( Method==0 ) THEN
!
!     SAVE ORIGINAL GRID POINT ORDERING (ILD) IN SCR1 FILE
!
      CALL open(*200,Scr1,Z(Ibuf1),Wrtrew)
      CALL write(Scr1,Ild,Nn,1)
      CALL close(Scr1,Rew)
   ENDIF
!
!     RE-SEQUENCE GRIDS WITH CUTHILL-MCKEE ALGORITHM
!
   i = Maxgrd + 2
   j = i + Maxgrd
   IF ( Maxdeg>Maxgrd ) j = j + Maxdeg - Maxgrd
   k = j + Maxgrd
   CALL cthmck(80,1,2,Icrit,Ig,Inv,Inv(i),Inv(j),Inv(k),Int,Icc,Ild,Ip,jump,Un,Z)
   ngrid1 = Ngrid
   IF ( Method<0 ) GOTO 100
   IF ( Method==0 ) THEN
!
!     READ ORIGINAL SEQUENCE BACK IF CTHMCK MAKES NO IMPROVEMENT
!
      IF ( jump/=0 ) THEN
         CALL open(*200,Scr1,Z(Ibuf1),Rdrew)
         CALL read(*300,*300,Scr1,Ild,Nn,1,m)
         CALL close(Scr1,Rew)
      ENDIF
   ENDIF
   DO k1 = 1 , Nn
      Int(k1) = Ild(k1)
   ENDDO
!
!     RESEQUENCE NODES WITH GPS ALGORITHM.
!
   k1 = 1
   k2 = k1 + Kdim
   k3 = k2 + Kdim
   k4 = k3 + Kdim
   k5 = k4 + Kdim/2
   CALL gibstk(Ig,Int,Ild,Inv(i),Inv,Inv(j),Inv(k),Icc,jump,Icrit,Z(k1),Z(k2),Z(k3),Z(k4),Z(k5),Un,Kdim)
!
!     GENERATE SEQGP CARDS AND OUTPUT THEM TO GEOM1 FILE
!
 100  CALL bseqgp(Norig,Ild,jump)
   IF ( ngrid1==-2 .OR. Ngrid==-3 ) GOTO 500
   RETURN
!
!     SCRATCH FILE ERROR
!
 200  k = -1
   GOTO 400
 300  k = -2
 400  CALL mesage(k,Scr1,sub)
!
 500  WRITE (Nout,99001) Kdim
99001 FORMAT (28H0*** BANDIT SCRATCH ARRAY OF,I5,20H WORDS IS TOO SMALL.,/5X,                                                       &
             &57HUSER COULD USE ONE OF THE FOLLOWING OPTIONS AND RESUBMIT ,27HJOB. (USERS MANUAL P.2.1-1),/5X,                      &
             &53HINCREASE SCRATCH ARRAY BY NASTRAN BANDTDIM OPTION, OR,/5X,53HSWITCH TO CUTHILL-MCKEE METHOD ONLY BY  BANDTMTH=1 OR,&
            & /5X,57HSKIP BANDIT COMPUTATION BY SETTING NASTRAN CARD BANDIT=-1,//)
   GOTO 700
!
 600  WRITE (Nout,99002) k , Nn , Ii3 , Kmod , Maxgrd , Maxdeg
99002 FORMAT ('0*** BANDIT FATAL ERROR - TRY TO RERUN JOB WITH ',22H'NASTRAN BANDTDIM = N',' WHERE N = 3,4,...,OR 9',//5X,          &
             &'@17/  K,NN,II3,KMOD,MAXGRD,MAXDEG =',6I8)
 700  CALL mesage(-37,sub,sub)
END SUBROUTINE scheme