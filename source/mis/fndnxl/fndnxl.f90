!*==fndnxl.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndnxl(Name,Newnm)
   IMPLICIT NONE
   USE C_SOF
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER , DIMENSION(2) :: Newnm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: hl , iempty
   INTEGER :: i , imdi , jdit , k
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , fdit , fdsub , fmdi
!
! End of declarations rewritten by SPAG
!
!
!     THE SUBROUTINE LOOKS FOR A HIGHER LEVEL SUBSTRUCTURE TO THE
!     SUBSTRUCTURE NAME.  IF NAME DOES HAVE A HIGHER LEVEL SUBSTRUCTURE,
!     THE NAME OF THE HIGHER LEVEL SUBSTRUCTURE WILL BE RETURNED IN
!     NEWNM.  IF NAME DOES NOT HAVE A HIGHER LEVEL SUBSTRUCTURE, NAME
!     WILL BE RETURNED IN NEWNM.  IF NAME IS NOT KNOWN TO THE SYSTEM,
!     BLANKS WILL BE RETURNED IN NEWNM.
!
   DATA hl/2/
   DATA iempty/4H    / , nmsbr/4HFNDN , 4HXL  /
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),k)
   IF ( k/=-1 ) THEN
!
!     FIND THE HIGHER LEVEL SUBSTRUCTURE TO NAME.
!
      CALL fmdi(k,imdi)
      i = andf(Buf(imdi+hl),1023)
      IF ( i==0 ) THEN
!
!     NAME DOES NOT HAVE A HIGHER LEVEL SUBSTRUCTURE.
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
!     NAME DOES HAVE A HIGHER LEVEL SUBSTRUCTURE.
!
   CALL fdit(i,jdit)
   Newnm(1) = Buf(jdit)
   Newnm(2) = Buf(jdit+1)
   RETURN
END SUBROUTINE fndnxl
