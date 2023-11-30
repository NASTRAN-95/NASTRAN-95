
SUBROUTINE mtmsu1(Y,X,Buf)
   IMPLICIT NONE
   REAL A(4) , Cdp , Csp , Da , Dumm(13) , Eofnrw , Rd , Rdp , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   INTEGER Diag , Eol , Filek(7) , Filem(7) , Identy , Ii , Lowtri , Norew , Nzero
   COMMON /invpwx/ Filek , Filem
   COMMON /invpxx/ Dumm , Nzero
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /zntpkx/ A , Ii , Eol
   REAL Buf(1) , X(1) , Y(1)
   INTEGER i , ncol
!
!     M TIMS U  FORMS THE PRODUCT  X = M*Y
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   EQUIVALENCE (A(1),Da)
!
!
   ncol = Filek(2)
   DO i = 1 , ncol
      X(i) = 0.0
   ENDDO
!
!     MASS MATRIX IS NOT DIAGONAL
!
   Nzero = 0
   DO i = 1 , ncol
      IF ( Y(i)==0.0 ) THEN
         CALL skprec(Filem,1)
         Nzero = Nzero + 1
      ELSE
         CALL intpk(*100,Filem(1),0,Rsp,0)
         Nzero = Nzero + 1
         DO
            CALL zntpki
            X(Ii) = Da*Y(i) + X(Ii)
            IF ( Eol/=0 ) EXIT
         ENDDO
      ENDIF
 100  ENDDO
   CALL rewind(Filem(1))
   CALL skprec(Filem,1)
   Nzero = ncol - Nzero
END SUBROUTINE mtmsu1
