!*==scheme.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scheme(Ig,Inv,Ii3,Int,Icc,Ild,Norig,Ip,Un,Z)
   IMPLICIT NONE
   USE C_BANDA
   USE C_BANDB
   USE C_BANDD
   USE C_BANDS
   USE C_GEOMX
   USE C_NAMES
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Inv
   INTEGER :: Ii3
   INTEGER , DIMENSION(1) :: Int
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Ild
   INTEGER , DIMENSION(1) :: Norig
   INTEGER , DIMENSION(1) :: Ip
   REAL , DIMENSION(1) :: Un
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , jump , k , k1 , k2 , k3 , k4 , k5 , loc , m , ngrid1
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL bread , bseqgp , close , cthmck , gibstk , mesage , open , read , sort , tiger , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA sub/4HSCHE , 4HME  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            IF ( j<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            loc = j - 1
            SPAG_Loop_2_1: DO
               loc = mod(loc,Kmod) + 1
               IF ( Inv(loc)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Inv(loc)==j ) THEN
                  j = Inv(loc+Ii3)
                  Ild(j) = i
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
!
!     METHOD WAS SET IN BANDIT -
!     METHOD = -1, CM ONLY,    = +1, GPS ONLY,    = 0, BOTH METHODS.
!
         IF ( Method==0 ) THEN
!
!     SAVE ORIGINAL GRID POINT ORDERING (ILD) IN SCR1 FILE
!
            CALL open(*20,Scr1,Z(Ibuf1),Wrtrew)
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
         IF ( Method>=0 ) THEN
            IF ( Method==0 ) THEN
!
!     READ ORIGINAL SEQUENCE BACK IF CTHMCK MAKES NO IMPROVEMENT
!
               IF ( jump/=0 ) THEN
                  CALL open(*20,Scr1,Z(Ibuf1),Rdrew)
                  CALL read(*40,*40,Scr1,Ild,Nn,1,m)
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
         ENDIF
!
!     GENERATE SEQGP CARDS AND OUTPUT THEM TO GEOM1 FILE
!
         CALL bseqgp(Norig,Ild,jump)
         IF ( ngrid1==-2 .OR. Ngrid==-3 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
!
!     SCRATCH FILE ERROR
!
 20      k = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      k = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(k,Scr1,sub)
         spag_nextblock_1 = 3
      CASE (3)
!
         WRITE (Nout,99001) Kdim
99001    FORMAT (28H0*** BANDIT SCRATCH ARRAY OF,I5,20H WORDS IS TOO SMALL.,/5X,                                                    &
                &57HUSER COULD USE ONE OF THE FOLLOWING OPTIONS AND RESUBMIT ,27HJOB. (USERS MANUAL P.2.1-1),/5X,                   &
                &53HINCREASE SCRATCH ARRAY BY NASTRAN BANDTDIM OPTION, OR,/5X,                                                      &
                &53HSWITCH TO CUTHILL-MCKEE METHOD ONLY BY  BANDTMTH=1 OR,/5X,                                                      &
                &57HSKIP BANDIT COMPUTATION BY SETTING NASTRAN CARD BANDIT=-1,//)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
         WRITE (Nout,99002) k , Nn , Ii3 , Kmod , Maxgrd , Maxdeg
99002    FORMAT ('0*** BANDIT FATAL ERROR - TRY TO RERUN JOB WITH ',22H'NASTRAN BANDTDIM = N',' WHERE N = 3,4,...,OR 9',//5X,       &
                &'@17/  K,NN,II3,KMOD,MAXGRD,MAXDEG =',6I8)
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(-37,sub,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE scheme
