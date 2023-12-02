!*==algpb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algpb(Idat,Ntype)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Idat
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: itype
   INTEGER , DIMENSION(4) , SAVE :: na
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   DATA na/2 , 2 , 3 , 1/
!                ZERO, INTEGER, REAL, ALPHA
!
!     RETURN FROM NUMTYP IS            SET NTYPE TO
!       0 -  ZERO                        1 - ALPHA
!       1 -  INTEGER                     2 - INTEGER
!       2 -  REAL                        3 - REAL
!       3 -  BCD
!
!     BLANK IS ALPHA,  ZERO IS INTEGER UNLESS NUMTYP SET IT TO REAL
!
   itype = numtyp(Idat) + 1
   Ntype = na(itype)
END SUBROUTINE algpb
