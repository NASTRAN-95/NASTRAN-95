!*==cmtimu.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cmtimu(Y,X,File,Buf)
   IMPLICIT NONE
   USE C_CINVPX
   USE C_CINVXX
   USE C_NAMES
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: Y
   REAL*8 , DIMENSION(1) :: X
   INTEGER , DIMENSION(1) :: File
   INTEGER , DIMENSION(1) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: filem
   INTEGER :: i , ncol , ncol2
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     CM TIM U FORMS THE MATRIX PRODUCT X = M*Y WHERE ALL MAY BE COMPLEX
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   !>>>>EQUIVALENCE (Ncol,Filek(2))
   DATA name/4HCMTI , 4HMU  /
!
   IF ( File(1)==0 ) THEN
!
!     USE MASS MATRIX
!
      DO i = 1 , 7
         filem(i) = Filemm(i)
      ENDDO
   ELSE
!
!     USE MATRIX OTHER THAN THE MASS MATRIX
!
      DO i = 1 , 7
         filem(i) = File(i)
      ENDDO
   ENDIF
   ncol2 = ncol + ncol
   IF ( filem(4)==Identy ) THEN
!
!     MASS MATRIX IS THE IDENTY
!
      DO i = 1 , ncol2
         X(i) = Y(i)
      ENDDO
      Nzero = 0
      RETURN
   ELSE
      Nzero = 0
      CALL gopen(filem(1),Buf,Rdrew)
      DO i = 1 , ncol2
         X(i) = 0.D0
      ENDDO
      IF ( filem(4)==Diag ) THEN
!
!     FILE ERROR
!
!  35 J = -1
!     GO TO 37
!  36 J = -2
!  37 CALL MESAGE (J,FILEM(1),NAME)
!
!     MASS MATRIX IS DIAGONAL
!
         CALL intpk(*100,filem(1),0,Cdp,0)
         DO
            CALL zntpki
            Ii = Ii + Ii - 1
            X(Ii) = Y(Ii)*Da(1) - Y(Ii+1)*Da(2)
            X(Ii+1) = Y(Ii)*Da(2) + Y(Ii+1)*Da(1)
            Nzero = Nzero + 1
            IF ( Eol/=0 ) EXIT
            IF ( Eor/=0 ) EXIT
         ENDDO
      ELSE
!
!     MASS MATRIX IS NOT DIAGONAL
!
         DO i = 1 , ncol2 , 2
            IF ( Y(i)==0.D0 .AND. Y(i+1)==0.D0 ) THEN
               CALL fwdrec(*100,filem(1))
            ELSE
               CALL intpk(*20,filem(1),0,Cdp,0)
               DO
                  CALL zntpki
                  IF ( Ii==i ) Nzero = Nzero + 1
                  Ii = Ii + Ii - 1
                  X(Ii) = X(Ii) + Da(1)*Y(i) - Da(2)*Y(i+1)
                  X(Ii+1) = X(Ii+1) + Da(1)*Y(i+1) + Da(2)*Y(i)
                  IF ( Eol/=0 ) EXIT
                  IF ( Eor/=0 ) EXIT
               ENDDO
            ENDIF
 20      ENDDO
      ENDIF
   ENDIF
!
 100  CALL close(filem(1),Rew)
   Nzero = 0
END SUBROUTINE cmtimu
