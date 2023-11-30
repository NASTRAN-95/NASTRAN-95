
SUBROUTINE pla3
   IMPLICIT NONE
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
