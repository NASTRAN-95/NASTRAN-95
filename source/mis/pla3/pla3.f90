!*==pla3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla3
   IMPLICIT NONE
   EXTERNAL pla31 , pla32
!
! End of declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE COMPUTES ELEMENT STRESSES FOR NON-LINEAR (STRESS DEPEN-
! DENT) ELEMENTS FOR WHICH THE USER HAS REQUESTED STRESS OUTPUT.  IT
! ALSO UPDATES THE ESTNL DATA BLOCK SO THAT THE OUTPUT FILE, ESTNL1,
! CONTAINS UP-TO-DATE ELEMENT STRESS INFORMATION.
!
! DMAP CALL...
!
!  PLA3  CSTM,MPT,DIT,DELTAUGV,ESTNL,CASECC/ONLES,ESTNL1/V,N,PLACOUNT/
!        V,N,PLSETNO/ $
!
! THIS ROUTINE IS THE MODULE DRIVER...  SUBROUTINE PLA31 READS THE
! INCREMENTAL DISPLACEMENT VECTOR INTO CORE AND APPENDS THE PROPER
! DISPLACEMENT VALUES TO THE ESTNL ENTRY FOR EACH ELEMENT, THEREBY
! CREATING THE ESTNLS, THE ESTNL SCRATCH FILE.  IN PLA32, THE ESTNLS
! FILE IS READ, AND THE PROPER ELEMENT ROUTINE IS CALLED.  THE ELEMENT
! ROUTINE COMPUTES THE ELEMENT STRESSES AND STORES THIS INFORMATION IN
! THE BLOCK /SOUT/.  THE ELEMENT ROUTINE ALSO UPDATES THE EST ENTRY
! WHICH HAS BEEN COMMUNICATED TO TI VIA /PLA32E/.
!*****
!
!
!
   CALL pla31
   CALL pla32
END SUBROUTINE pla3