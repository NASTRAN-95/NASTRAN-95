
SUBROUTINE matprt(*,*,A,Option,Column)
   IMPLICIT NONE
   INTEGER Bufsiz , Count , File , I , J , Maxlin , Mcb(7) , Mo , Type , Ui , Uinc , Uj , Utype
   REAL Skp1(6) , Skp2(2)
   COMMON /system/ Bufsiz , Mo , Skp1 , Maxlin , Skp2 , Count
   COMMON /unpakx/ Utype , Ui , Uj , Uinc
   COMMON /xxmprt/ Mcb
   INTEGER Option
   REAL A(1)
   INTEGER Column(1)
   INTEGER cdp , csp , inprew , rdp , rew , rsp
!
!     MATPRT AND PRTMAT ARE CALLED ONLY BY INTPRT
!
!
!     MCB = MATRIX CONTROL BLOCK.
!     A   = ARRAY OF BUFSIZ + I (REAL) OR 2I (COMPLEX) LOCATIONS.
!     OPTION IS AS DESCRIBED IN -VECPRT-.
!     RETURN 1 ... PRINT MATRIX TITLE + COLUMN IDENTIFIER.
!     RETURN 2 ... PRINT COLUMN IDENTIFIER ONLY.
!                  (PRTMAT = RETURN ENTRY POINT)
!     COLUMN = CURRENT COLUMN NUMBER
!
   !>>>>EQUIVALENCE (File,Mcb(1)) , (J,Mcb(2)) , (I,Mcb(3)) , (Type,Mcb(5))
   DATA rsp , rdp , csp , cdp , rew , inprew/1 , 2 , 3 , 4 , 1 , 0/
!
   IF ( I<=0 .OR. J<=0 ) GOTO 99999
   Utype = Type
   IF ( Type==rdp ) Utype = rsp
   IF ( Type==cdp ) Utype = csp
   Ui = 1
   Uj = I
   Uinc = 1
   CALL gopen(File,A,inprew)
   Count = Maxlin
!
   Column(1) = 0
 100  Column(1) = Column(1) + 1
   CALL unpack(*400,File,A(Bufsiz+1))
   CALL vecprt(*200,*300,Utype,I,A(Bufsiz+1),Option)
   GOTO 400
 200  RETURN 1
 300  RETURN 2
!
!
   ENTRY prtmat(*,*,Column)
!     =========================
!
   CALL prtvec(*200,*300)
 400  IF ( Column(1)/=J ) GOTO 100
!
   CALL close(File,rew)
99999 RETURN
END SUBROUTINE matprt