!*==dupart.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dupart
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , nogo
   INTEGER , DIMENSION(3) :: ib
   INTEGER , SAVE :: kff , kfs , knn , ksf , kss , scr1 , uset
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage , mpart , upart
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR DMAP MODULE UPARTN
!
!     DMAP CALLING SEQUENCE IS
!     UPARTN    USET,KNN/KFF,KSF,KFS,KSS/C,N,N/C,N,F/C,N,S $
!
   DATA name/4HUPAR , 4HTN  /
   DATA uset , knn , kff , ksf , kfs , kss , scr1/101 , 102 , 201 , 202 , 203 , 204 , 301/
!
!
   nogo = 0
!
!     DECIDE IF CHARACTERS ARE LEGAL BIT NUMBERS
!
   ib(1) = Major(1)
   ib(2) = Sub0(1)
   ib(3) = Sub1(1)
!
   SPAG_Loop_1_1: DO j = 1 , 3
      DO i = 1 , 32
         IF ( ib(j)==Iabit(i) ) THEN
            ib(j) = Ibit(i)
            CYCLE SPAG_Loop_1_1
         ENDIF
      ENDDO
!
!     INVALID
      CALL mesage(59,ib(j),name)
      nogo = 1
   ENDDO SPAG_Loop_1_1
!
   IF ( nogo==1 ) CALL mesage(-7,0,name)
!
   CALL upart(uset,scr1,ib(1),ib(2),ib(3))
   CALL mpart(knn,kff,ksf,kfs,kss)
END SUBROUTINE dupart
