
SUBROUTINE prefix(Iprefx,Name)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Junk(38) , Nbpc , Nbpw , Ncpw
   COMMON /system/ Junk , Nbpc , Nbpw , Ncpw
!
! Dummy argument declarations
!
   INTEGER Iprefx
   INTEGER Name(2)
!
! Local variable declarations
!
   INTEGER iblank , iblnk , lword , rword
   INTEGER lshift , orf , rshift
   EXTERNAL lshift , orf , rshift
!
! End of declarations
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
