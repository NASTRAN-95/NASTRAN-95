!*==cfe2ao.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfe2ao(Tpose,V1,V2,V3,Zb)
   USE c_feeraa
   USE c_feerxc
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL , DIMENSION(1) :: Tpose
   REAL(REAL64) , DIMENSION(1) :: V1
   REAL(REAL64) , DIMENSION(1) :: V2
   REAL(REAL64) , DIMENSION(1) :: V3
   REAL , DIMENSION(1) :: Zb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , ni , nj
   EXTERNAL cf2fbs , cfe2my
!
! End of declarations rewritten by SPAG
!
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
   IF ( qpr ) WRITE (nout,99001) Tpose
99001 FORMAT (1H0,12HENTER CFE2AO,8X,11HTRANSPOSE =,L2)
   IF ( Tpose(1) ) THEN
!*******
!     PERFORM OPERATION (A)-TRANSPOSE  = TRANSPOSED EIGENMATRIX
!                                                   MULTIPLICATION
!*******
      IF ( nob ) THEN
!*******
!     DAMPING MATRIX ABSENT
!*******
!     PERFORM BACKWARD AND FORWARD SWEEPS
!*******
         DO i = 1 , nord2
            V3(i) = V1(i)
         ENDDO
         CALL cf2fbs(Tpose(1),V3(1),Zb(1))
         IF ( qpr ) WRITE (nout,99002) (V3(i),i=1,nord2)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY TRANSPOSED MASS MATRIX
!*******
         CALL cfe2my(Tpose(1),V3(1),V2(1),im(1),Zb(1))
         DO i = 1 , nord2
            V2(i) = -V2(i)
         ENDDO
         IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord2)
      ELSE
!*******
!     CALCULATE RIGHT-HAND SIDE OF SWEEP EQUATION
!*******
         DO i = nordp1 , nord2 , 2
            j = i + 1
            ni = i - nord
            nj = ni + 1
            V3(i) = V1(ni) + lambda(1)*V1(i) - lambda(2)*V1(j)
            V3(j) = V1(nj) + lambda(1)*V1(j) + lambda(2)*V1(i)
         ENDDO
         IF ( qpr ) WRITE (nout,99002) (V3(i),i=nordp1,nord2)
!*******
!     PERFORM BACKWARD AND FORWARD SWEEPS
!*******
         CALL cf2fbs(Tpose(1),V3(nordp1),Zb(1))
         IF ( qpr ) WRITE (nout,99002) (V3(i),i=nordp1,nord2)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY -(LAMBDA*M+B)-TRANSPOSE
!*******
         CALL cfe2my(Tpose(1),V3(nordp1),V3(1),mcblmb(1),Zb(1))
         IF ( qpr ) WRITE (nout,99002) (V3(i),i=1,nord)
!*******
!     COMPUTE UPPER HALF OF OUTPUT VECTOR
!*******
         DO i = 1 , nord
            j = nord + i
            V2(i) = V1(j) + V3(i)
         ENDDO
         IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord)
!*******
!     MULTIPLY SWEEP OUTPUT VECTOR BY TRANSPOSED MASS MATRIX
!     (GENERATES NEGATIVE OF LOWER HALF OF OUTPUT VECTOR)
!*******
         CALL cfe2my(Tpose(1),V3(nordp1),V2(nordp1),im(1),Zb(1))
         DO i = nordp1 , nord2
            V2(i) = -V2(i)
         ENDDO
         IF ( qpr ) WRITE (nout,99002) (V2(i),i=nordp1,nord2)
      ENDIF
!*******
!     PERFORM OPERATION (A)  = EIGENMATRIX MULTIPLICATION
!*******
   ELSEIF ( nob ) THEN
!*******
!     DAMPING MATRIX ABSENT
!*******
!     MULTIPLY INPUT VECTOR BY MASS MATRIX
!*******
      CALL cfe2my(Tpose(1),V1(1),V2(1),im(1),Zb(1))
      DO i = 1 , nord2
         V2(i) = -V2(i)
      ENDDO
      IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord2)
!*******
!     PERFORM FORWARD AND BACKWARD SWEEPS
!*******
      CALL cf2fbs(Tpose(1),V2(1),Zb(1))
      IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord2)
   ELSE
!*******
!     MULTIPLY LOWER HALF OF INPUT VECTOR BY MASS MATRIX
!*******
      CALL cfe2my(Tpose(1),V1(nordp1),V3(1),im(1),Zb(1))
      IF ( qpr ) WRITE (nout,99002) (V3(i),i=1,nord)
!*******
!     MULTIPLY UPPER HALF OF INPUT VECTOR BY -(LAMBDA*M+B)
!*******
      CALL cfe2my(Tpose(1),V1(1),V3(nordp1),mcblmb(1),Zb(1))
      IF ( qpr ) WRITE (nout,99002) (V3(i),i=nordp1,nord2)
!*******
!     CALCULATE RIGHT-HAND SIDE OF SWEEP EQUATION
!*******
      DO i = 1 , nord
         j = nord + i
         V2(i) = -V3(i) + V3(j)
      ENDDO
      IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord)
!*******
!     PERFORM FORWARD AND BACKWARD SWEEPS
!     (GENERATES UPPER HALF OF OUTPUT VECTOR)
!*******
      CALL cf2fbs(Tpose(1),V2(1),Zb(1))
      IF ( qpr ) WRITE (nout,99002) (V2(i),i=1,nord)
!*******
!     COMPUTE LOWER HALF OF OUTPUT VECTOR
!*******
      DO i = 1 , nord , 2
         j = i + 1
         ni = nord + i
         nj = ni + 1
         V2(ni) = V1(i) + lambda(1)*V2(i) - lambda(2)*V2(j)
         V2(nj) = V1(j) + lambda(1)*V2(j) + lambda(2)*V2(i)
      ENDDO
      IF ( qpr ) WRITE (nout,99002) (V2(i),i=nordp1,nord2)
   ENDIF
99002 FORMAT (3H --,32(4H----),/(1H ,6D21.13))
END SUBROUTINE cfe2ao
