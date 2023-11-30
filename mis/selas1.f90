
SUBROUTINE selas1(Iarg)
   IMPLICIT NONE
   REAL Dummy(120) , Ecpt(100) , Scoeff , Stiff
   INTEGER Icoeff , Iecpt(6) , Isilno(2) , Jelid
   COMMON /sdr2x5/ Ecpt , Jelid , Isilno , Stiff , Scoeff , Dummy
   INTEGER Iarg
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
      Isilno(1) = Iecpt(3) + Iecpt(5)
      Isilno(2) = Iecpt(4) + Iecpt(6)
      IF ( Iecpt(5)>0 ) Isilno(1) = Isilno(1) - 1
      IF ( Iecpt(6)>0 ) Isilno(2) = Isilno(2) - 1
      Stiff = Ecpt(2)
      Scoeff = Ecpt(8)
   ELSEIF ( Iarg==3 ) THEN
!
! ELAS3
!
      Isilno(1) = Iecpt(2)
      Isilno(2) = Iecpt(3)
      Stiff = Ecpt(4)
      Scoeff = Ecpt(6)
   ELSEIF ( Iarg==4 ) THEN
!
! ELAS4
!
      Isilno(1) = Iecpt(3)
      Isilno(2) = Iecpt(4)
      Stiff = Ecpt(2)
      Icoeff = -1
   ELSE
!
! ELAS1
!
      Isilno(1) = Iecpt(2) + Iecpt(4)
      Isilno(2) = Iecpt(3) + Iecpt(5)
      IF ( Iecpt(4)>0 ) Isilno(1) = Isilno(1) - 1
      IF ( Iecpt(5)>0 ) Isilno(2) = Isilno(2) - 1
      Stiff = Ecpt(6)
      Scoeff = Ecpt(8)
   ENDIF
!
! STORE ELEMENT ID.
!
   Jelid = Iecpt(1)
END SUBROUTINE selas1