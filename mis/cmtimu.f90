
SUBROUTINE cmtimu(Y,X,File,Buf)
   IMPLICIT NONE
   REAL Cdp , Csp , Dum(21) , Eofnrw , Rd , Rdp , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   DOUBLE PRECISION Da(2)
   INTEGER Diag , Eol , Eor , Filek(7) , Filemm(7) , Identy , Ii , Lowtri , Ncol , Norew , Nzero
   COMMON /cinvpx/ Filek , Filemm
   COMMON /cinvxx/ Dum , Nzero
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /zntpkx/ Da , Ii , Eol , Eor
   INTEGER Buf(1) , File(1)
   DOUBLE PRECISION X(1) , Y(1)
   INTEGER filem(7) , i , name(2) , ncol2
!
!     CM TIM U FORMS THE MATRIX PRODUCT X = M*Y WHERE ALL MAY BE COMPLEX
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   EQUIVALENCE (Ncol,Filek(2))
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
   ncol2 = Ncol + Ncol
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
