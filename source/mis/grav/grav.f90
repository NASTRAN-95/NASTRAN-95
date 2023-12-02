!*==grav.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE grav(Ngrav,Gvect,Nlist,Ilist,Nloop)
   IMPLICIT NONE
   USE C_LOADX
   USE C_TRANX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ngrav
   REAL , DIMENSION(1) :: Gvect
   INTEGER :: Nlist
   INTEGER , DIMENSION(1) :: Ilist
   INTEGER :: Nloop
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   REAL , DIMENSION(5) :: gl
   INTEGER :: i , j , nl1 , nlist1 , nsave
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(3) :: x
   EXTERNAL fdcstm , mesage , mpyl , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (igl,gl(2))
   DATA name/4HGRAV , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CONVERTS GRAV CARD TO BASIC AND STORES
!     GB = G*TON*V
!
         CALL read(*20,*20,Slt,gl(1),5,0,flag)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 20      CALL mesage(-7,name,name)
         spag_nextblock_1 = 2
      CASE (2)
         Ngrav = Ngrav + 1
         IF ( gl(1)/=0 ) THEN
            CALL fdcstm(gl(1))
            CALL mpyl(To,gl(3),3,3,1,x(1))
            DO i = 1 , 3
               gl(i+2) = x(i)
            ENDDO
         ENDIF
         DO i = 1 , 3
            j = (Ngrav-1)*3 + i
            Gvect(j) = gl(i+2)*gl(2)
         ENDDO
         nl1 = Nloop - Ngrav + 1
         IF ( nl1/=Nlist ) THEN
            nsave = Ilist(nl1)
            nlist1 = Nlist - 1
            DO i = nl1 , nlist1
               Ilist(i) = Ilist(i+1)
            ENDDO
            Ilist(Nlist) = nsave
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE grav
