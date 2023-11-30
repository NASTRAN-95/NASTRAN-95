
SUBROUTINE ortck(X,Mass,Ibuf,Num,Ndim,Gm,Accum,Eps)
   IMPLICIT NONE
   REAL Eofnrw , Rd , Rdrew , Rew , Rsp , Wrt , Wrtrew , Z(4)
   INTEGER Ieol , Ii , Norew
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp
   COMMON /zntpkx/ Z , Ii , Ieol
   REAL Eps
   INTEGER Mass , Ndim , Num
   DOUBLE PRECISION Accum(1)
   REAL Gm(Num,1) , X(Ndim,1)
   INTEGER Ibuf(1)
   INTEGER i , iden , im(7) , j , k , kk , m
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
 100  DO i = 1 , Num
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
         Ieol = 1
         Ii = i
         Z(1) = 1.0
         GOTO 200
      ELSE
         CALL intpk(*300,Mass,0,Rsp,0)
      ENDIF
 150  IF ( Ieol==1 ) THEN
         DO j = 1 , Num
            DO m = 1 , Num
               Gm(j,m) = Gm(j,m) + Accum(j)*X(i,m)
            ENDDO
         ENDDO
         CYCLE
      ELSE
         CALL zntpki
      ENDIF
 200  DO j = 1 , Num
         Accum(j) = Accum(j) + Z(1)*X(Ii,j)
      ENDDO
      GOTO 150
 300  ENDDO
   IF ( iden/=1 ) THEN
      CALL rewind(Mass)
      CALL skprec(Mass,1)
   ENDIF
   Gm(1,1) = sqrt(Gm(1,1))
   DO i = 2 , Num
      Gm(i,i) = sqrt(Gm(i,i))
      Ii = i - 1
      DO j = 1 , Ii
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
         IF ( iden/=1 ) CALL close(Mass,Rew)
         EXIT
      ELSEIF ( j/=0 ) THEN
         GOTO 100
      ENDIF
   ENDDO
END SUBROUTINE ortck