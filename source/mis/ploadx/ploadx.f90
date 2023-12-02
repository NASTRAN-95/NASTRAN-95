!*==ploadx.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ploadx
   IMPLICIT NONE
   USE C_CONDSA
   USE C_LOADX
   USE C_SSG1AX
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag , p1 , p3 , rl , zl
   REAL , DIMENSION(12) :: gd
   INTEGER :: i , j , k
   INTEGER , DIMENSION(1) :: igd , islc
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(3) :: p , pn
   REAL , DIMENSION(5) :: slc
   EXTERNAL basglb , fndpnt , fndsil , mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PLOADX BUILDS THE PRESSURE LOADS FROM A PLOADX CARD FOR THE
!     TRIAX6 ELEMENT
!
   !>>>>EQUIVALENCE (slc(1),islc(1),p1) , (slc(2),p3) , (gd(1),igd(1))
   DATA nam/4HPLOA , 4HDX  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL read(*20,*40,Slt,slc,5,0,flag)
         j = 1
         DO i = 1 , 3
            CALL fndpnt(gd(j),islc(i+2))
            j = j + 4
         ENDDO
         rl = gd(10) - gd(2)
         zl = gd(12) - gd(4)
!
!     LOADS IN NORMAL DIRECTION
!
         pn(1) = Pi/30.*(9.0*gd(2)*p1+gd(2)*p3+gd(10)*p1-gd(10)*p3)
         pn(2) = Pi/7.5*(3.*(gd(2)*p1+gd(10)*p3)+2.*(gd(2)*p3+gd(10)*p1))
         pn(3) = Pi/30.*(9.0*gd(10)*p3+gd(2)*p3+gd(10)*p1-gd(2)*p1)
!
         j = 1
         DO i = 1 , 3
            p(1) = -zl*pn(i)
            p(2) = 0.0
            p(3) = rl*pn(i)
!
!     CONVERT TO GLOBAL IF NEEDED, AND INSERT INTO THE LOAD VECTOR
!
            IF ( igd(j)/=0 ) CALL basglb(p,p,gd(j+1),igd(j))
            CALL fndsil(islc(i+2))
            k = islc(i+2)
            Z(k) = Z(k) + p(1)
            Z(k+1) = Z(k+1) + p(2)
            Z(k+2) = Z(k+2) + p(3)
            j = j + 4
         ENDDO
         RETURN
!
!     ERROR MESSAGE
!
 20      j = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      j = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(j,Slt,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE ploadx
