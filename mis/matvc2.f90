
SUBROUTINE matvc2(Y,X,Filea,Buf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A(2) , Da
   REAL Cdp , Csp , Eofnrw , Rd , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   INTEGER Diag , Eol , Identy , Idum(27) , Ii , Iopen , Iwtri , Norew , Rdp
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Iwtri , Uprtri ,  &
                 & Sym , Row , Identy
   COMMON /trdxx / Idum , Iopen
   COMMON /zntpkx/ A , Ii , Eol
!
! Dummy argument declarations
!
   REAL Buf(1)
   INTEGER Filea(7)
   DOUBLE PRECISION X(1) , Y(1)
!
! Local variable declarations
!
   INTEGER i , ncol , no , sub(2)
!
! End of declarations
!
!
!     MATVC2 WILL FORM THE PRODUCT X = X + A*Y WHERE A IS A MATRIX
!     AND Y IS A VECTOR
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   EQUIVALENCE (A(1),Da)
   DATA sub/4HMATV , 4HC2  /
!
   IF ( Filea(1)==0 ) RETURN
   ncol = Filea(2)
   IF ( Filea(4)==Identy ) THEN
!
!     MATRIX IS THE IDENTITY
!
      DO i = 1 , ncol
         X(i) = Y(i) + X(i)
      ENDDO
      RETURN
   ELSE
      IF ( Iopen/=1 ) CALL open(*200,Filea(1),Buf,Rdrew)
      CALL fwdrec(*300,Filea(1))
      IF ( Filea(4)==Diag ) THEN
!
!     MATRIX IS DIAGONAL
!
         CALL intpk(*100,Filea(1),0,Rdp,0)
         DO
            CALL zntpki
            X(Ii) = Y(Ii)*Da + X(Ii)
            IF ( Eol/=0 ) EXIT
         ENDDO
      ELSE
!
!     MATRIX IS FULL
!
         DO i = 1 , ncol
            IF ( Y(i)==0.0D0 ) THEN
               CALL fwdrec(*300,Filea(1))
            ELSE
               CALL intpk(*20,Filea(1),0,Rdp,0)
               DO
                  CALL zntpki
                  X(Ii) = Da*Y(i) + X(Ii)
                  IF ( Eol/=0 ) EXIT
               ENDDO
            ENDIF
 20      ENDDO
      ENDIF
   ENDIF
!
 100  CALL rewind(Filea(1))
   IF ( Iopen==0 ) CALL close(Filea(1),Rew)
   RETURN
!
 200  no = -1
   GOTO 400
 300  no = -2
 400  CALL mesage(no,Filea(1),sub(1))
END SUBROUTINE matvc2
