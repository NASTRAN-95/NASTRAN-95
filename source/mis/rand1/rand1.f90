!*==rand1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rand1(File,Mid,Type,Id,Comp,Q)
   USE c_output
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Mid
   INTEGER :: Type
   INTEGER :: Id
   INTEGER :: Comp
   INTEGER , DIMENSION(2) :: Q
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , itype
   INTEGER , DIMENSION(50) , SAVE :: idr
   INTEGER , DIMENSION(2,7) , SAVE :: mid1
   EXTERNAL write
!
! End of declarations rewritten by SPAG
!
!
!     PUTS ID RECORD ON RANDOM OUTPUT FILES
!
   DATA mid1/2001 , 4HDISP , 2010 , 4HVELO , 2011 , 4HACCE , 2002 , 4HLOAD , 2003 , 4HSPCF , 2004 , 4HELFO , 2005 , 4HSTRE/
   DATA idr/50*0/
   idr(1) = 50
   idr(3) = Mid
   SPAG_Loop_1_1: DO i = 1 , 7
      IF ( Type==mid1(2,i) ) EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   itype = mid1(1,i)
   idr(2) = itype
   idr(5) = Id*10
   idr(6) = Comp
   idr(8) = Q(1)
   idr(9) = Q(2)
   idr(10) = 2
   CALL write(File,idr(1),50,0)
   CALL write(File,head(1),96,1)
END SUBROUTINE rand1
