!*==ortck.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ortck(X,Mass,Ibuf,Num,Ndim,Gm,Accum,Eps)
   IMPLICIT NONE
   USE c_names
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(Ndim,1) :: X
   INTEGER :: Mass
   INTEGER , DIMENSION(1) :: Ibuf
   INTEGER :: Num
   INTEGER :: Ndim
   REAL , DIMENSION(Num,1) :: Gm
   REAL*8 , DIMENSION(1) :: Accum
   REAL :: Eps
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iden , j , k , kk , m
   INTEGER , DIMENSION(7) :: im
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
!     ORTCK WILL GENERATE THE GENERALIZED MASS MATRIX FOR CLOSE ROOTS
!     AND MAKE THE EPSILON TEST TO DETERMINE IF THE VECTORS SHOULD BE
!     ORTHOGONALIZED
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
!
   iden = 0
   im(1) = Mass
   CALL rdtrl(im)
   IF ( im(4)==8 ) iden = 1
   IF ( iden/=1 ) CALL gopen(Mass,Ibuf,0)
   k = 1
   DO
      DO i = 1 , Num
         DO j = 1 , Num
            Gm(i,j) = 0.
         ENDDO
      ENDDO
      DO i = 1 , Ndim
         DO j = 1 , Num
            Accum(j) = 0.D0
         ENDDO
         IF ( iden==1 ) THEN
!
!     IDENTITY
!
            ieol = 1
            ii = i
            z(1) = 1.0
            GOTO 40
         ELSE
            CALL intpk(*50,Mass,0,rsp,0)
         ENDIF
 20      IF ( ieol==1 ) THEN
            DO j = 1 , Num
               DO m = 1 , Num
                  Gm(j,m) = Gm(j,m) + Accum(j)*X(i,m)
               ENDDO
            ENDDO
            CYCLE
         ELSE
            CALL zntpki
         ENDIF
 40      DO j = 1 , Num
            Accum(j) = Accum(j) + z(1)*X(ii,j)
         ENDDO
         GOTO 20
 50   ENDDO
      IF ( iden/=1 ) THEN
         CALL rewind(Mass)
         CALL skprec(Mass,1)
      ENDIF
      Gm(1,1) = sqrt(Gm(1,1))
      DO i = 2 , Num
         Gm(i,i) = sqrt(Gm(i,i))
         ii = i - 1
         DO j = 1 , ii
            Gm(i,j) = Gm(i,j)/(Gm(i,i)*Gm(j,j))
         ENDDO
      ENDDO
      DO i = 1 , Num
         DO j = 1 , Ndim
            X(j,i) = X(j,i)/Gm(i,i)
         ENDDO
      ENDDO
      j = 0
      DO
         DO kk = 1 , k
            IF ( abs(Gm(k+1,kk))>=Eps ) THEN
               j = 1
               DO i = 1 , Ndim
                  X(i,k+1) = X(i,k+1) - Gm(k+1,kk)*X(i,kk)
               ENDDO
            ENDIF
         ENDDO
         k = k + 1
         IF ( k>=Num ) THEN
            IF ( iden/=1 ) CALL close(Mass,rew)
            EXIT
         ELSEIF ( j/=0 ) THEN
            GOTO 100
         ENDIF
      ENDDO
      EXIT
 100  ENDDO
END SUBROUTINE ortck
