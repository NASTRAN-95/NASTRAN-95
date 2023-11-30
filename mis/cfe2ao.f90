
SUBROUTINE cfe2ao(Tpose,V1,V2,V3,Zb)
   IMPLICIT NONE
   REAL Dum01(2) , Dum02(2) , Dum03(4) , Dumaa(117) , Epsdum(2)
   INTEGER Ib(7) , Idiag , Ik(7) , Im(7) , Ksys , Mcblmb(7) , Nord , Nord2 , Nord4 , Nordp1 , Northo , Nout
   DOUBLE PRECISION Lambda(2)
   LOGICAL Nob , Qpr
   COMMON /feeraa/ Ik , Im , Ib , Dumaa , Mcblmb
   COMMON /feerxc/ Lambda , Dum01 , Nord , Idiag , Epsdum , Northo , Nord2 , Nord4 , Nordp1 , Dum02 , Nob , Dum03 , Qpr
   COMMON /system/ Ksys , Nout
   LOGICAL Tpose(1)
   DOUBLE PRECISION V1(1) , V2(1) , V3(1)
   REAL Zb(1)
   INTEGER i , j , ni , nj
!*******
!     CFE2AO IS A DOUBLE PRECISION ROUTINE WHICH PERFORMS THE OPERATION
!     (A) OR (A)-TRANSPOSE FOR THE COMPLEX FEER METHOD. THIS OPERATION
!     IS CALLED THE EIGENMATRIX MULTIPLICATION.
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     TPOSE    = .FALSE. --- PERFORM OPERATION (A)
!              = .TRUE.  --- PERFORM OPERATION (A)-TRANSPOSE
!     V1       = INPUT  VECTOR
!     V2       = OUTPUT VECTOR
!     V3       = INPUT WORKING SPACE (FOR INTERNAL USE)
!     ZB       = INPUT GINO BUFFER
!*******
!*******
   IF ( Qpr ) WRITE (Nout,99001) Tpose
99001 FORMAT (1H0,12HENTER CFE2AO,8X,11HTRANSPOSE =,L2)
   IF ( Tpose(1) ) THEN
!*******
!     PERFORM OPERATION (A)-TRANSPOSE  = TRANSPOSED EIGENMATRIX
!                                                   MULTIPLICATION
!*******
      IF ( Nob ) THEN
!*******
!     DAMPING MATRIX ABSENT
!*******
!     PERFORM BACKWARD AND FORWARD SWEEPS
!*******
         DO i = 1 , Nord2
            V3(i) = V1(i)
         ENDDO
         CALL cf2fbs(Tpose(1),V3(1),Zb(1))
         IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=1,Nord2)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY TRANSPOSED MASS MATRIX
!*******
         CALL cfe2my(Tpose(1),V3(1),V2(1),Im(1),Zb(1))
         DO i = 1 , Nord2
            V2(i) = -V2(i)
         ENDDO
         IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord2)
      ELSE
!*******
!     CALCULATE RIGHT-HAND SIDE OF SWEEP EQUATION
!*******
         DO i = Nordp1 , Nord2 , 2
            j = i + 1
            ni = i - Nord
            nj = ni + 1
            V3(i) = V1(ni) + Lambda(1)*V1(i) - Lambda(2)*V1(j)
            V3(j) = V1(nj) + Lambda(1)*V1(j) + Lambda(2)*V1(i)
         ENDDO
         IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=Nordp1,Nord2)
!*******
!     PERFORM BACKWARD AND FORWARD SWEEPS
!*******
         CALL cf2fbs(Tpose(1),V3(Nordp1),Zb(1))
         IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=Nordp1,Nord2)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY -(LAMBDA*M+B)-TRANSPOSE
!*******
         CALL cfe2my(Tpose(1),V3(Nordp1),V3(1),Mcblmb(1),Zb(1))
         IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=1,Nord)
!*******
!     COMPUTE UPPER HALF OF OUTPUT VECTOR
!*******
         DO i = 1 , Nord
            j = Nord + i
            V2(i) = V1(j) + V3(i)
         ENDDO
         IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY TRANSPOSED MASS MATRIX
!     (GENERATES NEGATIVE OF LOWER HALF OF OUTPUT VECTOR)
!*******
         CALL cfe2my(Tpose(1),V3(Nordp1),V2(Nordp1),Im(1),Zb(1))
         DO i = Nordp1 , Nord2
            V2(i) = -V2(i)
         ENDDO
         IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=Nordp1,Nord2)
      ENDIF
!*******
!     PERFORM OPERATION (A)  = EIGENMATRIX MULTIPLICATION
!*******
   ELSEIF ( Nob ) THEN
!*******
!     DAMPING MATRIX ABSENT
!*******
!     MULTIPLY INPUT VECTOR BY MASS MATRIX
!*******
      CALL cfe2my(Tpose(1),V1(1),V2(1),Im(1),Zb(1))
      DO i = 1 , Nord2
         V2(i) = -V2(i)
      ENDDO
      IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord2)
!*******
!     PERFORM FORWARD AND BACKWARD SWEEPS
!*******
      CALL cf2fbs(Tpose(1),V2(1),Zb(1))
      IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord2)
   ELSE
!*******
!     MULTIPLY LOWER HALF OF INPUT VECTOR BY MASS MATRIX
!*******
      CALL cfe2my(Tpose(1),V1(Nordp1),V3(1),Im(1),Zb(1))
      IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=1,Nord)
!*******
!     MULTIPLY UPPER HALF OF INPUT VECTOR BY -(LAMBDA*M+B)
!*******
      CALL cfe2my(Tpose(1),V1(1),V3(Nordp1),Mcblmb(1),Zb(1))
      IF ( Qpr ) WRITE (Nout,99002) (V3(i),i=Nordp1,Nord2)
!*******
!     CALCULATE RIGHT-HAND SIDE OF SWEEP EQUATION
!*******
      DO i = 1 , Nord
         j = Nord + i
         V2(i) = -V3(i) + V3(j)
      ENDDO
      IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord)
!*******
!     PERFORM FORWARD AND BACKWARD SWEEPS
!     (GENERATES UPPER HALF OF OUTPUT VECTOR)
!*******
      CALL cf2fbs(Tpose(1),V2(1),Zb(1))
      IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=1,Nord)
!*******
!     COMPUTE LOWER HALF OF OUTPUT VECTOR
!*******
      DO i = 1 , Nord , 2
         j = i + 1
         ni = Nord + i
         nj = ni + 1
         V2(ni) = V1(i) + Lambda(1)*V2(i) - Lambda(2)*V2(j)
         V2(nj) = V1(j) + Lambda(1)*V2(j) + Lambda(2)*V2(i)
      ENDDO
      IF ( Qpr ) WRITE (Nout,99002) (V2(i),i=Nordp1,Nord2)
   ENDIF
99002 FORMAT (3H --,32(4H----),/(1H ,6D21.13))
END SUBROUTINE cfe2ao
