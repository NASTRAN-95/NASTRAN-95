!*==matprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE matprt(*,*,A,Option,Column)
   USE c_system
   USE c_unpakx
   USE c_xxmprt
   IMPLICIT NONE
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
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( i<=0 .OR. j<=0 ) RETURN
         utype = type
         IF ( type==rdp ) utype = rsp
         IF ( type==cdp ) utype = csp
         ui = 1
         uj = i
         uinc = 1
         CALL gopen(file,A,inprew)
         count = maxlin
!
         Column(1) = 0
         spag_nextblock_1 = 2
      CASE (2)
         Column(1) = Column(1) + 1
         CALL unpack(*60,file,A(bufsiz+1))
         CALL vecprt(*20,*40,utype,i,A(bufsiz+1),Option)
         GOTO 60
 20      RETURN 1
 40      RETURN 2
!
!
         ENTRY prtmat(*,*,Column)
!     =========================
!
         CALL prtvec(*20,*40)
 60      IF ( Column(1)/=j ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL close(file,rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE matprt
