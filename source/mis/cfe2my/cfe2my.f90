!*==cfe2my.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfe2my(Tpose,Y,X,File,Buf)
USE C_NAMES
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL , DIMENSION(1) :: Tpose
   REAL(REAL64) , DIMENSION(1) :: Y
   REAL(REAL64) , DIMENSION(1) :: X
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
!     CFE2MY FORMS THE COMPLEX DOUBLE PRECISION MATRIX
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
   IF ( File(4)==Identy ) THEN
!*******
!     MATRIX IS IDENTITY
!*******
      DO i = 1 , ncol2
         X(i) = Y(i)
      ENDDO
      RETURN
   ELSE
      CALL gopen(File(1),Buf(1),Rdrew)
      DO i = 1 , ncol2
         X(i) = 0.D0
      ENDDO
      IF ( File(4)==Diag ) THEN
!*******
!     MATRIX IS DIAGONAL
!*******
         CALL intpk(*100,File(1),0,Cdp,0)
         SPAG_Loop_1_1: DO
            CALL zntpki
            jj = Ii + Ii
            Ii = jj - 1
            X(Ii) = Da(1)*Y(Ii) - Da(2)*Y(jj)
            X(jj) = Da(1)*Y(jj) + Da(2)*Y(Ii)
            IF ( Eol/=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
      ELSEIF ( Tpose(1) ) THEN
!*******
!     GENERAL MATRIX-TRANSPOSE*VECTOR PRODUCT
!*******
         DO i = 1 , ncol2 , 2
            j = i + 1
            CALL intpk(*20,File(1),0,Cdp,0)
            SPAG_Loop_2_2: DO
               CALL zntpki
               jj = Ii + Ii
               Ii = jj - 1
               X(i) = X(i) + Da(1)*Y(Ii) - Da(2)*Y(jj)
               X(j) = X(j) + Da(1)*Y(jj) + Da(2)*Y(Ii)
               IF ( Eol/=0 ) EXIT SPAG_Loop_2_2
            ENDDO SPAG_Loop_2_2
 20      ENDDO
      ELSE
!*******
!     GENERAL MATRIX*VECTOR PRODUCT
!*******
         DO i = 1 , ncol2 , 2
            j = i + 1
            IF ( Y(i)==0.D0 .AND. Y(j)==0.D0 ) THEN
               CALL skprec(File(1),1)
            ELSE
               CALL intpk(*40,File(1),0,Cdp,0)
               SPAG_Loop_2_3: DO
                  CALL zntpki
                  jj = Ii + Ii
                  Ii = jj - 1
                  X(Ii) = X(Ii) + Da(1)*Y(i) - Da(2)*Y(j)
                  X(jj) = X(jj) + Da(1)*Y(j) + Da(2)*Y(i)
                  IF ( Eol/=0 ) EXIT SPAG_Loop_2_3
               ENDDO SPAG_Loop_2_3
            ENDIF
 40      ENDDO
      ENDIF
   ENDIF
 100  CALL close(File(1),Rew)
END SUBROUTINE cfe2my
