!*==crsub.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE crsub(Name,I)
   USE c_sof
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: I
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: iempty , nmsbr
   INTEGER , SAVE :: indsbr
   INTEGER :: jdit
   EXTERNAL chkopn , errmkn , fdit , fdsub
!
! End of declarations rewritten by SPAG
!
!
!     THE SUBROUTINE CREATES AN ENTRY FOR THE SUBSTRUCTURE NAME IN THE
!     DIT THE OUTPUT PARAMETER I INDICATES THAT THE SUBSTRUCTURE NAME
!     IS THE ITH SUBSTRUCTURE IN THE DIT.
!
   DATA iempty/2*4H    /
   DATA indsbr/1/ , nmsbr/4HCRSU , 4HB   /
!
   CALL chkopn(nmsbr(1))
   IF ( ditsiz==ditnsb*2 ) THEN
!
!     NO INTERNAL EMPTY SPACE IN THE MDI.  DIRECTORY FOR THE NEW
!     SUBSTRUCTURE
!
      ditsiz = ditsiz + 2
      I = ditsiz/2
   ELSE
!
!     THERE IS AN EMPTY INTERNAL DIRECTORY SPACE IN THE MDI.
!
      CALL fdsub(iempty(1),I)
      IF ( I==-1 ) THEN
!
!     ERROR MESSAGES.
!
         CALL errmkn(indsbr,5)
         RETURN
      ENDIF
   ENDIF
!
!     UPDATE DIT.
!
   ditnsb = ditnsb + 1
   CALL fdit(I,jdit)
   buf(jdit) = Name(1)
   buf(jdit+1) = Name(2)
   ditup = .TRUE.
END SUBROUTINE crsub
