!*==dsmg1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsmg1
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iarg
   EXTERNAL ds1 , ds1a , mesage
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS THE DRIVER FOR THE DIFFERENTIAL STIFFNESS MATRIX
!     GENERATOR MODULE OF THE NASTRAN SYSTEM.  SUBROUTINE DS1 APPENDS
!     TEMPERATURE, ELEMENT DEFORMATION AND DISPLACEMENT INFORMATION TO
!     THE ECPT DATA BLOCK AND A SCRATCH FILE, ECPTDS, OF THIS MERGED
!     INFORMATION IS CREATED.  SUBROUTINE DS1A IS STRUCTURED IDENTICALLY
!     TO SMA1A. IT READS THE ECPTDS FILE AND CREATES A SECOND ORDER
!     APPROXIMATION TO THE KGG, WHICH IS CALLED KDGG.
!
!     DMAP CALL -
!
!     DSMG1    CASECC,GPTT,SIL,EDT,UGV,CSTM,MPT,ECPT,GPCT,DIT/KDGG/
!
   CALL ds1(iarg)
   IF ( iarg<=0 ) THEN
!
!     ECPTDS IS EMPTY. WRITE MESSAGE AND CALL EXIT.
!
      CALL mesage(30,81,0)
      CALL mesage(-61,0,0)
   ENDIF
   CALL ds1a
END SUBROUTINE dsmg1
