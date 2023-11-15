
SUBROUTINE cf1ort(Sucess,Maxits,Ten2mt,Nzero,Iortho,Vr,Vl,V1,V1l,V2,V2l,Zb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dumaa(42) , Dumxc(7) , Eofnrw , Rd , Rdrew , Rew , Wrt , Wrtrew , Xcdum(3) , Xcdum2(9) , Xcdum3(5)
   INTEGER Idiag , Ii , Incr , Iprc , Iscr7 , Ksys , Nn , Nord2 , Norew , Nout , Numort
   LOGICAL Qpr
   COMMON /feeraa/ Dumaa , Iscr7
   COMMON /feerxc/ Dumxc , Idiag , Xcdum , Nord2 , Xcdum2 , Qpr , Xcdum3 , Numort
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /system/ Ksys , Nout
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
!
! Dummy argument declarations
!
   INTEGER Iortho , Maxits , Nzero
   LOGICAL Sucess
   REAL Ten2mt
   REAL V1(1) , V1l(1) , V2(1) , V2l(1) , Vl(1) , Vr(1)
   INTEGER Zb(1)
!
! Local variable declarations
!
   REAL a(2) , critf , otest(4)
   INTEGER i , j , k , l , ll , mortho
   LOGICAL skip
!
! End of declarations
!
!*******
!     CF1ORT IS A SINGLE-PRECISION ROUTINE (CREATED FOR USE BY
!     THE COMPLEX FEER METHOD) WHICH PERFORMS THE
!     REORTHOGONALIZATION ALGORITHM
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     SUCESS   = LOGICAL INDICATOR FOR SUCCESSFUL REORTHOGONALIZATION
!                (OUTPUT)
!     MAXITS   = MAXIMUM NUMBER OF ALLOWED ITERATIONS (INPUT)
!     TEN2MT   = CONVERGENCE CRITERION
!     NZERO    = NUMBER OF ORTHOGONAL VECTOR PAIRS IN PRIOR
!                NEIGHBORHOODS INCLUDING RESTART
!     IORTHO   = NUMBER OF EXISTING ORTHOGONAL VECTOR PAIRS
!                IN CURRENT NEIGHBORHOOD
!     VR       = RIGHT-HANDED VECTOR TO BE REORTHOGONALIZED
!     VL       = LEFT -HANDED VECTOR TO BE REORTHOGONALIZED
!     V1,V1L,  = WORKING SPACE FOR FOUR VECTORS (V1L MUST
!     V2,V2L     FOLLOW V1 IN CORE)
!     ZB       = WORKING SPACE FOR ONE GINO BUFFER
!*******
   mortho = Nzero + Iortho
   IF ( mortho<=0 ) THEN
      Sucess = .TRUE.
      GOTO 200
   ELSE
      IF ( Qpr ) WRITE (Nout,99001)
99001 FORMAT (1H0,//26H BEGIN REORTHOGONALIZATION,//)
      Numort = Numort + 1
      k = 0
      Sucess = .FALSE.
      Nn = Nord2
      critf = 100.*Ten2mt**2
      DO i = 1 , Nord2
         V2(i) = Vr(i)
         V2l(i) = Vl(i)
      ENDDO
      CALL gopen(Iscr7,Zb(1),Rdrew)
   ENDIF
 100  DO i = 1 , 4
      otest(i) = 0.
   ENDDO
   ll = 2
!*******
!     ENTER LOOP
!*******
   DO i = 1 , mortho
      IF ( i==Nzero+1 ) ll = 0
      IF ( Qpr ) WRITE (Nout,99002) i
99002 FORMAT (1H ,13HUNPACK VECTOR,I4)
!     VALUES ARE UNPACKED INTO BOTH V1 AND V1L
      CALL unpack(*150,Iscr7,V1(1))
      IF ( Qpr ) THEN
         WRITE (Nout,99006) (V1(j),j=1,Nord2)
         WRITE (Nout,99006) (V1l(j),j=1,Nord2)
      ENDIF
!*******
!     OBTAIN RIGHT-HAND INNER-PRODUCT TERM
!*******
      CALL cfnor1(Vr(1),V1l(1),Nord2,1,a(1))
!*******
!     SUBTRACT OFF RIGHT-HAND INNER-PRODUCT TERM
!*******
      DO j = 1 , Nord2 , 2
         l = j + 1
         V2(j) = V2(j) - a(1)*V1(j) + a(2)*V1(l)
         V2(l) = V2(l) - a(1)*V1(l) - a(2)*V1(j)
      ENDDO
!*******
!     COMPUTE MAXIMUM RIGHT-HAND SQUARED-ERROR
!*******
      a(1) = a(1)**2 + a(2)**2
      IF ( otest(ll+1)<a(1) ) otest(ll+1) = a(1)
!*******
!     OBTAIN LEFT-HAND INNER-PRODUCT TERM
!*******
      CALL cfnor1(Vl(1),V1(1),Nord2,1,a(1))
!*******
!     SUBTRACT OFF LEFT-HAND INNER-PRODUCT TERM
!*******
      DO j = 1 , Nord2 , 2
         l = j + 1
         V2l(j) = V2l(j) - a(1)*V1l(j) + a(2)*V1l(l)
         V2l(l) = V2l(l) - a(1)*V1l(l) - a(2)*V1l(j)
      ENDDO
!*******
!     COMPUTE MAXIMUM LEFT-HAND SQUARED-ERROR
!*******
      a(1) = a(1)**2 + a(2)**2
      IF ( otest(ll+2)<a(1) ) otest(ll+2) = a(1)
      CYCLE
 150  IF ( Idiag/=0 ) WRITE (Nout,99003) i
99003 FORMAT (18H ORTHOGONAL VECTOR,I4,39H IS NULL IN REORTHOGONALIZATION ROUTINE)
   ENDDO
   DO i = 1 , Nord2
      Vr(i) = V2(i)
      Vl(i) = V2l(i)
   ENDDO
   skip = .FALSE.
   IF ( Qpr ) THEN
      WRITE (Nout,99006) (Vr(i),i=1,Nord2)
      WRITE (Nout,99006) (Vl(i),i=1,Nord2)
   ENDIF
   DO
!*******
!     TEST FOR CONVERGENCE
!*******
      IF ( Idiag/=0 ) WRITE (Nout,99004) k , critf , otest
99004 FORMAT (32H   REORTHOGONALIZATION ITERATION,I3,9X,14HTARGET VALUE =,E12.4,4X,8HERRORS =,4E12.4)
      IF ( otest(1)<=critf .AND. otest(2)<=critf .AND. otest(3)<=critf .AND. otest(4)<=critf ) THEN
         CALL close(Iscr7,Norew)
         Sucess = .TRUE.
         EXIT
      ELSE
         IF ( .NOT.(skip) ) THEN
            IF ( k==1 .OR. k==3 .OR. k==5 ) THEN
               IF ( Idiag/=0 ) WRITE (Nout,99005)
99005          FORMAT (52H   REORTHOGONALIZATION TOLERANCE TEMPORARILY RELAXED)
               critf = 100.*critf
               skip = .TRUE.
               CYCLE
            ENDIF
         ENDIF
         k = k + 1
         IF ( k>Maxits ) THEN
            CALL close(Iscr7,Norew)
            EXIT
         ELSE
            CALL close(Iscr7,Eofnrw)
            CALL gopen(Iscr7,Zb(1),Rdrew)
            GOTO 100
         ENDIF
      ENDIF
   ENDDO
 200  RETURN
99006 FORMAT (3H --,32(4H----),/(1H ,4E25.16))
END SUBROUTINE cf1ort
