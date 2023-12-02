!*==prefix.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prefix(Iprefx,Name)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iprefx
   INTEGER , DIMENSION(2) :: Name
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iblank , lword , rword
   INTEGER , SAVE :: iblnk
   EXTERNAL lshift , orf , rshift
!
! End of declarations rewritten by SPAG
!
!
   DATA iblnk/4H    /
!
   iblank = iblnk
!
!     THIS ROUTINE PREFIXES THE TWO WORD VARIABLE 'NAME' WITH THE SINGLE
!     CHARACTER PREFIX 'IPREFX'.
!
!     SET RIGHT HAND PORTION OF WORDS TO ZERO.
!
   lword = lshift(rshift(Name(1),Nbpw-4*Nbpc),Nbpw-4*Nbpc)
   rword = lshift(rshift(Name(2),Nbpw-4*Nbpc),Nbpw-4*Nbpc)
   Iprefx = lshift(rshift(Iprefx,Nbpw-Nbpc),Nbpw-Nbpc)
   iblank = rshift(lshift(iblank,4*Nbpc),4*Nbpc)
!
!     MOVE RIGHT WORD ONE CHARACTER AND PREFIX WITH LAST CHARACTER
!     OF LEFT WORD.
!
   rword = orf(lshift(lword,3*Nbpc),rshift(rword,Nbpc))
   rword = lshift(rshift(rword,Nbpw-4*Nbpc),Nbpw-4*Nbpc)
   rword = orf(rword,iblank)
!
!     MOVE LEFT WORD ONE CHARACTER TO RIGHT AND PREFIX WITH INPUT
!     VALUE.
!
   lword = orf(Iprefx,rshift(lword,Nbpc))
   lword = lshift(rshift(lword,Nbpw-4*Nbpc),Nbpw-4*Nbpc)
   lword = orf(lword,iblank)
!
   Name(1) = lword
   Name(2) = rword
END SUBROUTINE prefix
