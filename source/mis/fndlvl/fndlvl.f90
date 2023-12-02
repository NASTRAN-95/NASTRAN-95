!*==fndlvl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndlvl(Name,Newnm)
   IMPLICIT NONE
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER , DIMENSION(2) :: Newnm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: iempty , ll
   INTEGER :: ill , imdi , jdit , k
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , fdit , fdsub , fmdi , rshift
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE LOOKS FOR A LOWER LEVEL SUBSTRUCTUE TO THE
!     SUBSTRUCTURE NAME.  IF NAME DOES HAVE A LOWER LEVEL SUBSTRUCTURE,
!     THE NAME OF ONE OF THESE LOWER LEVEL SUBSTRUCTURES WILL BE
!     RETURNED IN NEWNM.  IF NAME DOES NOT HAVE A LOWER LEVEL
!     SUBSTRUCTURE, NAME WILL BE RETURNED IN NEWNM.  IF NAME IS NOT
!     KNOWN TO THE SYSTEM, BLANKS WILL BE RETURNED IN NEWNM.
!
   DATA ll/2/
   DATA iempty/4H    / , nmsbr/4HFNDL , 4HVL  /
!
!     CHECK IF NAME EXISTS
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),k)
   IF ( k/=-1 ) THEN
!
!     FIND THE LOWER LEVEL SUBSTRUCTURE
!
      CALL fmdi(k,imdi)
      ill = andf(rshift(Buf(imdi+ll),20),1023)
      IF ( ill==0 ) THEN
!
!     NAME DOES NOT HAVE A LOWER LEVEL SUBSTRUCTURE
!
         Newnm(1) = Name(1)
         Newnm(2) = Name(2)
         RETURN
      ENDIF
   ELSE
      Newnm(1) = iempty
      Newnm(2) = iempty
      RETURN
   ENDIF
!
!     NAME DOES HAVE A LOWER LEVEL SUBSTRUCTURE
!
   CALL fdit(ill,jdit)
   Newnm(1) = Buf(jdit)
   Newnm(2) = Buf(jdit+1)
   RETURN
END SUBROUTINE fndlvl
