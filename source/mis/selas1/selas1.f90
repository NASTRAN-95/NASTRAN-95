!*==selas1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE selas1(Iarg)
   USE c_sdr2x5
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iarg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icoeff
   INTEGER , DIMENSION(6) :: iecpt
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE ELAS ELEMENTS.
!
!
!
!*****
!
!
!
!              E C P T - S  F O R  E L A S  E L E M E N T S
!
!
!
!                  TYPE             TYPE           TYPE           TYPE
!         CELAS1           CELAS2         CELAS3         CELAS4
! ECPT(1) IELID     I      IELID     I    IELID      I   IELID      I
! ECPT(2) IGP1      I      K         R    IS1        I   K          R
! ECPT(3) IGP2      I      IGP1      I    IS2        I   IS1        I
! ECPT(4) IC1       I      IGP2      I    K          R   IS2        I
! ECPT(5) IC2       I      IC1       I    GSUBE      R
! ECPT(6) K         R      IC2       I    S          R
! ECPT(7) GSUBE     R      GSUBE     R
! ECPT(8) S         R      S         R
!
!
!
!
! SDR2 PHASE I INPUT AND OUTPUT BLOCK
!
!
!
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1)) , (Scoeff,Icoeff)
!
! BUILD UP OUTPUT BLOCK DEPENDING UPON WHICH ELEMENT TYPE, ELAS1, ELAS2,
! ELAS3 OR ELAS4, IS BEING WORKED ON.
!
   IF ( Iarg==2 ) THEN
!
! ELAS2
!
      isilno(1) = iecpt(3) + iecpt(5)
      isilno(2) = iecpt(4) + iecpt(6)
      IF ( iecpt(5)>0 ) isilno(1) = isilno(1) - 1
      IF ( iecpt(6)>0 ) isilno(2) = isilno(2) - 1
      stiff = ecpt(2)
      scoeff = ecpt(8)
   ELSEIF ( Iarg==3 ) THEN
!
! ELAS3
!
      isilno(1) = iecpt(2)
      isilno(2) = iecpt(3)
      stiff = ecpt(4)
      scoeff = ecpt(6)
   ELSEIF ( Iarg==4 ) THEN
!
! ELAS4
!
      isilno(1) = iecpt(3)
      isilno(2) = iecpt(4)
      stiff = ecpt(2)
      icoeff = -1
   ELSE
!
! ELAS1
!
      isilno(1) = iecpt(2) + iecpt(4)
      isilno(2) = iecpt(3) + iecpt(5)
      IF ( iecpt(4)>0 ) isilno(1) = isilno(1) - 1
      IF ( iecpt(5)>0 ) isilno(2) = isilno(2) - 1
      stiff = ecpt(6)
      scoeff = ecpt(8)
   ENDIF
!
! STORE ELEMENT ID.
!
   jelid = iecpt(1)
END SUBROUTINE selas1
