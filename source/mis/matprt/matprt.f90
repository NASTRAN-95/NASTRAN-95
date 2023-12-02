!*==matprt.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE matprt(A,Option,Column) !HIDESTARS (*,*,A,Option,Column)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XXMPRT
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: A
   INTEGER :: Option
   INTEGER , DIMENSION(1) :: Column
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: cdp , csp , inprew , rdp , rew , rsp
   INTEGER :: file , i , j , type
!
! End of declarations rewritten by SPAG
!
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
   IF ( i<=0 .OR. j<=0 ) GOTO 99999
   Utype = type
   IF ( type==rdp ) Utype = rsp
   IF ( type==cdp ) Utype = csp
   Ui = 1
   Uj = i
   Uinc = 1
   CALL gopen(file,A,inprew)
   Count = Maxlin
!
   Column(1) = 0
 100  Column(1) = Column(1) + 1
   CALL unpack(*400,file,A(Bufsiz+1))
   CALL vecprt(*200,*300,Utype,i,A(Bufsiz+1),Option)
   GOTO 400
 200  RETURN 1
 300  RETURN 2
!
!
   ENTRY prtmat(Column) !HIDESTARS (*,*,Column)
!     =========================
!
   CALL prtvec(*200,*300)
 400  IF ( Column(1)/=j ) GOTO 100
!
   CALL close(file,rew)
99999 END SUBROUTINE matprt
