!*==prefix.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prefix(Iprefx,Name)
   USE c_system
   IMPLICIT NONE
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
   lword = lshift(rshift(Name(1),nbpw-4*nbpc),nbpw-4*nbpc)
   rword = lshift(rshift(Name(2),nbpw-4*nbpc),nbpw-4*nbpc)
   Iprefx = lshift(rshift(Iprefx,nbpw-nbpc),nbpw-nbpc)
   iblank = rshift(lshift(iblank,4*nbpc),4*nbpc)
!
!     MOVE RIGHT WORD ONE CHARACTER AND PREFIX WITH LAST CHARACTER
!     OF LEFT WORD.
!
   rword = orf(lshift(lword,3*nbpc),rshift(rword,nbpc))
   rword = lshift(rshift(rword,nbpw-4*nbpc),nbpw-4*nbpc)
   rword = orf(rword,iblank)
!
!     MOVE LEFT WORD ONE CHARACTER TO RIGHT AND PREFIX WITH INPUT
!     VALUE.
!
   lword = orf(Iprefx,rshift(lword,nbpc))
   lword = lshift(rshift(lword,nbpw-4*nbpc),nbpw-4*nbpc)
   lword = orf(lword,iblank)
!
   Name(1) = lword
   Name(2) = rword
END SUBROUTINE prefix
