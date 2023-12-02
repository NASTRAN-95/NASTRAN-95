!*==cfe1my.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfe1my(Tpose,Y,X,File,Buf)
   USE c_names
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL , DIMENSION(1) :: Tpose
   REAL , DIMENSION(1) :: Y
   REAL , DIMENSION(1) :: X
   INTEGER , DIMENSION(7) :: File
   INTEGER , DIMENSION(1) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , jj , ncol2
   EXTERNAL close , gopen , intpk , skprec , zntpki
!
! End of declarations rewritten by SPAG
!
!*******
!     CFE1MY FORMS THE COMPLEX SINGLE PRECISION MATRIX
!     PRODUCT X = M*Y FOR THE COMPLEX FEER METHOD
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     TPOSE    = .FALSE. --- USE MATRIX M
!              = .TRUE.  --- USE MATRIX M-TRANSPOSE
!     Y        = INPUT  VECTOR
!     X        = OUTPUT VECTOR
!     FILE     = INPUT MATRIX CONTROL BLOCK FOR THE
!                REQUIRED MATRIX
!     BUF      = INPUT REQUIRED GINO BUFFER
!*******
   ncol2 = File(2) + File(2)
   IF ( File(4)==identy ) THEN
!*******
!     MATRIX IS IDENTITY
!*******
      DO i = 1 , ncol2
         X(i) = Y(i)
      ENDDO
      RETURN
   ELSE
      CALL gopen(File(1),Buf(1),rdrew)
      DO i = 1 , ncol2
         X(i) = 0.
      ENDDO
      IF ( File(4)==diag ) THEN
!*******
!     MATRIX IS DIAGONAL
!*******
         CALL intpk(*100,File(1),0,csp,0)
         SPAG_Loop_1_1: DO
            CALL zntpki
            jj = ii + ii
            ii = jj - 1
            X(ii) = da(1)*Y(ii) - da(2)*Y(jj)
            X(jj) = da(1)*Y(jj) + da(2)*Y(ii)
            IF ( eol/=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
      ELSEIF ( Tpose(1) ) THEN
!*******
!     GENERAL MATRIX-TRANSPOSE*VECTOR PRODUCT
!*******
         DO i = 1 , ncol2 , 2
            j = i + 1
            CALL intpk(*20,File(1),0,csp,0)
            SPAG_Loop_2_2: DO
               CALL zntpki
               jj = ii + ii
               ii = jj - 1
               X(i) = X(i) + da(1)*Y(ii) - da(2)*Y(jj)
               X(j) = X(j) + da(1)*Y(jj) + da(2)*Y(ii)
               IF ( eol/=0 ) EXIT SPAG_Loop_2_2
            ENDDO SPAG_Loop_2_2
 20      ENDDO
      ELSE
!*******
!     GENERAL MATRIX*VECTOR PRODUCT
!*******
         DO i = 1 , ncol2 , 2
            j = i + 1
            IF ( Y(i)==0. .AND. Y(j)==0. ) THEN
               CALL skprec(File(1),1)
            ELSE
               CALL intpk(*40,File(1),0,csp,0)
               SPAG_Loop_2_3: DO
                  CALL zntpki
                  jj = ii + ii
                  ii = jj - 1
                  X(ii) = X(ii) + da(1)*Y(i) - da(2)*Y(j)
                  X(jj) = X(jj) + da(1)*Y(j) + da(2)*Y(i)
                  IF ( eol/=0 ) EXIT SPAG_Loop_2_3
               ENDDO SPAG_Loop_2_3
            ENDIF
 40      ENDDO
      ENDIF
   ENDIF
 100  CALL close(File(1),rew)
END SUBROUTINE cfe1my
