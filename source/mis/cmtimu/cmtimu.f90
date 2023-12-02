!*==cmtimu.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cmtimu(Y,X,File,Buf)
   IMPLICIT NONE
   USE c_cinvpx
   USE c_cinvxx
   USE c_names
   USE c_zntpkx
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
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
         filem(i) = filemm(i)
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
   IF ( filem(4)==identy ) THEN
!
!     MASS MATRIX IS THE IDENTY
!
      DO i = 1 , ncol2
         X(i) = Y(i)
      ENDDO
      nzero = 0
      RETURN
   ELSE
      nzero = 0
      CALL gopen(filem(1),Buf,rdrew)
      DO i = 1 , ncol2
         X(i) = 0.D0
      ENDDO
      IF ( filem(4)==diag ) THEN
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
         CALL intpk(*100,filem(1),0,cdp,0)
         DO
            CALL zntpki
            ii = ii + ii - 1
            X(ii) = Y(ii)*da(1) - Y(ii+1)*da(2)
            X(ii+1) = Y(ii)*da(2) + Y(ii+1)*da(1)
            nzero = nzero + 1
            IF ( eol/=0 ) EXIT
            IF ( eor/=0 ) EXIT
         ENDDO
      ELSE
!
!     MASS MATRIX IS NOT DIAGONAL
!
         DO i = 1 , ncol2 , 2
            IF ( Y(i)==0.D0 .AND. Y(i+1)==0.D0 ) THEN
               CALL fwdrec(*100,filem(1))
            ELSE
               CALL intpk(*20,filem(1),0,cdp,0)
               DO
                  CALL zntpki
                  IF ( ii==i ) nzero = nzero + 1
                  ii = ii + ii - 1
                  X(ii) = X(ii) + da(1)*Y(i) - da(2)*Y(i+1)
                  X(ii+1) = X(ii+1) + da(1)*Y(i+1) + da(2)*Y(i)
                  IF ( eol/=0 ) EXIT
                  IF ( eor/=0 ) EXIT
               ENDDO
            ENDIF
 20      ENDDO
      ENDIF
   ENDIF
!
 100  CALL close(filem(1),rew)
   nzero = 0
END SUBROUTINE cmtimu
