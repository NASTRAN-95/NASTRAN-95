!*==skpfrm.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE skpfrm(Bframs)
   IMPLICIT NONE
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Bframs
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: a
   INTEGER , DIMENSION(3) , SAVE :: adv10
   INTEGER :: bfrms , i
   INTEGER , SAVE :: con10
   REAL , DIMENSION(2,4) :: save
   REAL , DIMENSION(2) :: xymax
   EXTERNAL wplt10
!
! End of declarations rewritten by SPAG
!
!
   DATA adv10 , con10/1 , 2 , 3 , 3/
!
   DO i = 1 , 2
      save(i,1) = Reg(i,1)
      Reg(i,1) = 0.
      save(i,2) = Reg(i,2)
      Reg(i,2) = Axymax(i) + 2.*Edge(i)
      save(i,3) = Origin(i)
      Origin(i) = 0.
      save(i,4) = Edge(i)
      Edge(i) = 0.
   ENDDO
   xymax(1) = amax1(Reg(1,2),Reg(2,2))
   xymax(2) = amin1(Reg(1,2),Reg(2,2))
   Reg(1,2) = xymax(1)
   Reg(2,2) = xymax(2)
   bfrms = min0(max0(Bframs,1),5)
!
!     PLOTTER 1, 2
!
   a(1) = con10
   a(2) = adv10(Camera)
   DO i = 3 , 6
      a(i) = 0
   ENDDO
   DO i = 1 , bfrms
      CALL wplt10(a,0)
   ENDDO
!
   DO i = 1 , 2
      Reg(i,1) = save(i,1)
      Reg(i,2) = save(i,2)
      Origin(i) = save(i,3)
      Edge(i) = save(i,4)
   ENDDO
!
END SUBROUTINE skpfrm
