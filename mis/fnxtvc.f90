
SUBROUTINE fnxtvc(V1,V2,V3,V4,V5,Zb,Ifn)
   IMPLICIT NONE
   INTEGER Cndflg , Ibk , Idiag , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset , Ii , Iip , Incr , Incrp , &
         & Ind , Io , Ioptf , Iprc , Istart , Iter , Itp1 , Itp2 , Ksystm(65) , L16 , Mcblt(7) , Mcbrm(7) , Mcbsma(7) , Mcbvec(7) , &
         & Mord , Mrank , Neig , Nn , Nnp , Nonul , Nord , Norew , Northo , Nzero , Sr5fle , Sysbuf
   REAL Critf , Dmpfle , Eofnrw , Epx , Errc , Rd , Rdrew , Rew , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr6fle , Sr7fle , Sr8fle ,    &
      & Timed , Wrt , Wrtrew , Xlmbda
   DOUBLE PRECISION Lambda , Lmbda
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerxx/ Lambda , Cndflg , Iter , Timed , L16 , Ioptf , Epx , Errc , Ind , Lmbda , Ifset , Nzero , Nonul , Idiag , Mrank ,&
                 & Istart
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /opinv / Mcblt , Mcbsma , Mcbvec , Mcbrm
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /system/ Ksystm
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /xmssg / Ufm , Uwm
   INTEGER Ifn
   DOUBLE PRECISION V1(1) , V2(1) , V3(1) , V4(1) , V5(1) , Zb(1)
   DOUBLE PRECISION aii , b(2) , d , db , dbi , depx , depx2 , dsq , dtmp , omdepx , opdepx , sd , sdmax , zero
   INTEGER i , ifg , ifv , ix , iy , name(5) , nord1 , vcdot
!
!     FNXTVC OBTAINS THE REDUCED TRIDIAGONAL MATRIX B WHERE FRBK2
!     PERFORMS THE OPERATIONAL INVERSE.   (DOUBLE PREC VERSION)
!
!           T   -
!      B = V  * A  * V
!
!     V1  = SPACE FOR THE PREVIOUS CURRENT TRIAL VECTOR. INITALLY NULL
!     V2  = SPACE FOR THE CURRENT TRIAL VECTOR. INITIALLY A PSEUDO-
!           RANDOM START VECTOR
!     V3,V4,V5 = WORKING SPACES FOR THREE VECTORS
!     IFN = NO. OF TRIAL VECOTRS EXTRACTED. INITIALLY ZERO.
!     SEE FEER FOR DEFINITIONS OF OTHER PARAMETERS. ALSO PROGRAMMER'S
!           MANUAL PP. 4.48-19G THRU I
!
!     REAL*16, MARKED BY 'CQ', WAS TRIED FOR IMPROVED ACCURACY. BUT THE
!     REAL*16  OPERATIONS ON VAX WERE 10 TIMES SLOWER THAN REAL*8
!     (NUMERIC ACCURACY IS VERY IMPORTANT IN THIS SUBROUTINE)
!
!Q    REAL*16            D         ,DB       ,DSQ      ,SD       ,
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Io)
   DATA name/4HFNXT , 4HVC   , 2*4HBEGN , 4HEND /
   DATA vcdot , zero/4HVC.  , 0.0D+0/
!
!     SR5FLE CONTAINS THE REDUCED TRIDIAGONAL ELEMENTS
!
!     SR6FLE CONTAINS THE G VECTORS
!     SR7FLE CONTAINS THE ORTHOGONAL  VECTORS
!     SR8FLE CONTAINS THE CONDITIONED MAA MATRIX
!
   IF ( Mcblt(7)<0 ) name(2) = vcdot
   name(3) = name(4)
   CALL conmsg(name,3,0)
   Iter = Iter + 1
   Iprc = 2
   Incr = 1
   Incrp = Incr
   Itp1 = Iprc
   Itp2 = Iprc
   ifg = Mcbrm(1)
   ifv = Mcbvec(1)
   depx = Epx
   depx2 = depx**2
   opdepx = 1.0D+0 + depx
   omdepx = 1.0D+0 - depx
!Q    OPDEPX= 1.0Q+0 + DEPX
!Q    OMDEPX= 1.0Q+0 - DEPX
   d = zero
   nord1 = Nord - 1
!
!     NORMALIZE START VECTOR
!
   dsq = zero
   IF ( Ioptf==1 ) THEN
      DO i = 1 , Nord
         dsq = dsq + V2(i)*V2(i)
      ENDDO
   ELSE
      CALL frmltd(Mcbsma(1),V2(1),V3(1),V5(1))
      DO i = 1 , Nord
         dsq = dsq + V2(i)*V3(i)
      ENDDO
   ENDIF
   dsq = 1.0D+0/dsqrt(dsq)
!Q 40 DSQ = 1.0D+0/QSQRT(DSQ)
   DO i = 1 , Nord
      V2(i) = V2(i)*dsq
   ENDDO
   IF ( Northo==0 ) GOTO 200
!
!     ORTHOGONALIZE WITH PREVIOUS VECTORS
!
   DO i = 1 , Nord
      V3(i) = V2(i)
   ENDDO
 100  DO ix = 1 , 14
      Nonul = Nonul + 1
      CALL gopen(ifv,Zb(1),Rdrew)
      IF ( Ioptf==0 ) CALL frmltd(Mcbsma(1),V2(1),V3(1),V5(1))
      sdmax = zero
      DO iy = 1 , Northo
         Ii = 1
         Nn = Nord
         sd = zero
         CALL unpack(*120,ifv,V5(1))
         DO i = 1 , Nord
            sd = sd + V3(i)*V5(i)
         ENDDO
 120     IF ( dabs(sd)>sdmax ) sdmax = dabs(sd)
!Q 90 IF (QABS(SD) .GT. SDMAX) SDMAX = QABS(SD)
         DO i = 1 , Nord
            V2(i) = V2(i) - sd*V5(i)
         ENDDO
      ENDDO
      CALL close(ifv,Eofnrw)
      dsq = zero
      IF ( Ioptf==1 ) THEN
         DO i = 1 , nord1
            dsq = dsq + V2(i)*V2(i)
         ENDDO
      ELSE
         CALL frmltd(Mcbsma(1),V2(1),V3(1),V5(1))
         DO i = 1 , nord1
            dsq = dsq + V2(i)*V3(i)
         ENDDO
      ENDIF
!
! 150 IF (DSQ .LT. DEPX2) GO TO 500
!
!     COMMENTS FORM G.CHAN/UNISYS ABOUT DSQ AND DEPX2 ABOVE,   1/92
!
!     DEPX2 IS SQUARE OF EPX. ORIGINALLY SINCE DAY 1, EPX (FOR VAX AND
!     IBM) IS 10.**-14 AND THEREFORE DEPX2 = 10.**-28. (10.**-24 FOR
!     THE 60/64 BIT MACHINES, USING S.P. COMPUTATION)
!     (EPX WAS SET TO 10.**-10 FOR ALL MACHINES, S.P. AND D.P., 1/92)
!
!     NOTICE THAT DSQ IS THE DIFFERENCE OF TWO CLOSE NUMERIC NUMBERS.
!     THE FINAL VAULES OF DSQ AND THE PRODUCT OF V2*V2 OR V2*V3 APPROACH
!     ONE ANOTHER, AND DEFFER ONLY IN SIGN. THEREFORE, THE NUMBER OF
!     DIGITS (MANTISSA) AS WELL AS THE EXPONENT ARE IMPORTANT HERE
!     (PREVIOUSLY, DO LOOPS 120 AND 140 COVERED 1 THRU NORD)
!
!     MOST OF THE 32 BIT MACHINES HOLD 15 DIGIT IN D.P. WORD, AND SAME
!     FOR THE 64 BIT MACHINES USING S.P. WORD. THEREFORE, CHECKING DSQ
!     DOWN TO 10.**-28 (OR 10.**-24) IS BEYOND THE HARDWARE LIMITS.
!     THIS MAY EXPLAIN SOME TIMES THE RIGID BODY MODES (FREQUENCY = 0.0)
!     GO TO NEGATIVE; IN SOME INSTANCES REACHING -1.E+5 RANGE
!
!     NEXT 7 LINES TRY TO SOLVE THE ABOVE DILEMMA
!
      d = V3(Nord)
      IF ( Ioptf==1 ) d = V2(Nord)
      d = V2(Nord)*d
      dtmp = dsq
      dsq = dsq + d
      IF ( dsq<depx2 ) EXIT
      dtmp = dabs(d/dtmp)
!Q    DTMP = QABS(D/DTMP)
      IF ( dtmp>omdepx .AND. dtmp<opdepx ) EXIT
      d = zero
!
      dsq = dsqrt(dsq)
!Q    DSQ = QSQRT(DSQ)
      IF ( L16/=0 ) WRITE (Io,99001) ix , sdmax , dsq
99001 FORMAT (11X,'ORTH ITER (IX)',I5,',  MAX PROJ (SDMAX)',1P,D16.8,',  NORMAL FACT (DSQ)',1P,D16.8)
      dsq = 1.0D+0/dsq
      DO i = 1 , Nord
         V2(i) = V2(i)*dsq
         V3(i) = V2(i)
      ENDDO
      IF ( sdmax<depx ) GOTO 200
   ENDDO
!
   GOTO 300
 200  IF ( Ifn/=0 ) THEN
!
!     CALCULATE OFF DIAGONAL TERM OF B
!
      d = zero
      DO i = 1 , Nord
         d = d + V2(i)*V4(i)
      ENDDO
!
!     COMMENTS FROM G.CHAN/UNISYS 1/92
!     WHAT HAPPENS IF D IS NEGATIVE HERE? NEXT LINE WOULD BE ALWAY TRUE.
!
      IF ( d<depx*dabs(aii) ) GOTO 300
   ELSE
!
!     SWEEP START VECTOR FOR ZERO ROOTS
!
      dsq = zero
      IF ( Ioptf==1 ) THEN
         CALL frbk2(V2(1),V4(1),V3(1),V5(1))
         DO i = 1 , Nord
            dsq = dsq + V3(i)*V3(i)
         ENDDO
      ELSE
         CALL frsw2(V2(1),V4(1),V3(1),V5(1))
         CALL frmltd(Mcbsma(1),V3(1),V4(1),V5(1))
         DO i = 1 , Nord
            dsq = dsq + V3(i)*V4(i)
         ENDDO
      ENDIF
      dsq = 1.0D+0/dsqrt(dsq)
!Q240 DSQ = 1.0D+0/QSQRT(DSQ)
      DO i = 1 , Nord
         V2(i) = V3(i)*dsq
      ENDDO
   ENDIF
!Q    IF (D .LT. DEPX*QABS(AII)) GO TO 500
   CALL gopen(ifg,Zb(1),Wrt)
   Iip = 1
   Nnp = Nord
   IF ( Ioptf==1 ) THEN
      CALL frbk2(V2(1),V4(1),V3(1),V5(1))
      CALL pack(V4(1),ifg,Mcbrm(1))
      DO i = 1 , Nord
         V4(i) = V3(i)
      ENDDO
   ELSE
      CALL frsw2(V2(1),V4(1),V3(1),V5(1))
      CALL frmltd(Mcbsma(1),V3(1),V4(1),V5(1))
      CALL pack(V2(1),ifg,Mcbrm(1))
   ENDIF
   CALL close(ifg,Norew)
!
!     CALCULATE DIAGONAL TERM OF B
!
   aii = zero
   DO i = 1 , Nord
      aii = aii + V2(i)*V4(i)
   ENDDO
   IF ( d==zero ) THEN
      DO i = 1 , Nord
         V3(i) = V3(i) - aii*V2(i)
      ENDDO
   ELSE
      DO i = 1 , Nord
         V3(i) = V3(i) - aii*V2(i) - d*V1(i)
      ENDDO
   ENDIF
   db = zero
   IF ( Ioptf==1 ) THEN
      DO i = 1 , Nord
         db = db + V3(i)*V3(i)
      ENDDO
   ELSE
      CALL frmltd(Mcbsma(1),V3(1),V4(1),V5(1))
      DO i = 1 , Nord
         db = db + V3(i)*V4(i)
      ENDDO
   ENDIF
   db = dsqrt(db)
!Q480 DB = QSQRT(DB)
   Errc = sngl(db)
   b(1) = aii
   b(2) = d
   CALL write(Sr5fle,b(1),4,1)
   CALL gopen(ifv,Zb(1),Wrt)
   Iip = 1
   Nnp = Nord
   CALL pack(V2(1),ifv,Mcbvec(1))
   CALL close(ifv,Norew)
   Northo = Northo + 1
   Ifn = Northo - Nzero
   IF ( L16/=0 ) WRITE (Io,99002) Ifn , Mord , aii , db , d
99002 FORMAT (5X,'TRIDIAGONAL ELEMENTS ROW (IFN)',I5,/5X,'MORD =',I5,', AII,DB,D = ',1P,3D16.8)
   IF ( Ifn>=Mord ) GOTO 400
!
!     IF NULL VECTOR GENERATED, RETURN TO OBTAIN A NEW SEED VECTOR
!
   IF ( db<depx*dabs(aii) ) GOTO 400
!
!     A GOOD VECTOR IN V2. MOVE IT INTO 'PREVIOUS' VECTOR SPACE V1,
!     NORMALIZE V3 AND V2. LOOP BACK FOR MORE VECTORS.
!
   dbi = 1.0D+0/db
   DO i = 1 , Nord
      V1(i) = V2(i)
      V3(i) = V3(i)*dbi
      V2(i) = V3(i)
   ENDDO
   GOTO 100
!
 300  Mord = Ifn
   WRITE (Io,99003) Uwm , Mord
!
99003 FORMAT (A25,' 2387, PROBLEM SIZE REDUCED TO',I5,' DUE TO -',/5X,'ORTHOGONALITY DRIFT OR NULL TRIAL VECTOR',/5X,               &
             &'ALL EXISTING MODES MAY HAVE BEEN OBTAINED.  USE DIAG 16',' TO DETERMINE ERROR BOUNDS',/)
!
 400  name(3) = name(5)
   CALL conmsg(name,3,0)
END SUBROUTINE fnxtvc