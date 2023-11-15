
SUBROUTINE cfe2my(Tpose,Y,X,File,Buf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cdp , Csp , Eofnrw , Rd , Rdp , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   DOUBLE PRECISION Da(2)
   INTEGER Diag , Eol , Identy , Ii , Lowtri , Norew
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /zntpkx/ Da , Ii , Eol
!
! Dummy argument declarations
!
   INTEGER Buf(1) , File(7)
   LOGICAL Tpose(1)
   DOUBLE PRECISION X(1) , Y(1)
!
! Local variable declarations
!
   INTEGER i , j , jj , ncol2
!
! End of declarations
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
      GOTO 99999
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
         DO
            CALL zntpki
            jj = Ii + Ii
            Ii = jj - 1
            X(Ii) = Da(1)*Y(Ii) - Da(2)*Y(jj)
            X(jj) = Da(1)*Y(jj) + Da(2)*Y(Ii)
            IF ( Eol/=0 ) EXIT
         ENDDO
      ELSEIF ( Tpose(1) ) THEN
!*******
!     GENERAL MATRIX-TRANSPOSE*VECTOR PRODUCT
!*******
         DO i = 1 , ncol2 , 2
            j = i + 1
            CALL intpk(*20,File(1),0,Cdp,0)
            DO
               CALL zntpki
               jj = Ii + Ii
               Ii = jj - 1
               X(i) = X(i) + Da(1)*Y(Ii) - Da(2)*Y(jj)
               X(j) = X(j) + Da(1)*Y(jj) + Da(2)*Y(Ii)
               IF ( Eol/=0 ) EXIT
            ENDDO
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
               DO
                  CALL zntpki
                  jj = Ii + Ii
                  Ii = jj - 1
                  X(Ii) = X(Ii) + Da(1)*Y(i) - Da(2)*Y(j)
                  X(jj) = X(jj) + Da(1)*Y(j) + Da(2)*Y(i)
                  IF ( Eol/=0 ) EXIT
               ENDDO
            ENDIF
 40      ENDDO
      ENDIF
   ENDIF
 100  CALL close(File(1),Rew)
99999 END SUBROUTINE cfe2my
