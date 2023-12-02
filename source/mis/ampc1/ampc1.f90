!*==ampc1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ampc1(Input,Output,Ncol,Z,Mcb)
   IMPLICIT NONE
   USE c_packx
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Output
   INTEGER :: Ncol
   INTEGER , DIMENSION(1) :: Z
   INTEGER , DIMENSION(7) :: Mcb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
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
!     THE PURPOSE OF THIS ROUTINE IS TO COPY NCOL COLUMNS FROM INPUT
!      TO OUTPUT VIA UNPACK AND PACK.
!
!     THE PACK COMMON BLOCKS HAVE BEEN INITIALIZED OUTSIDE THE ROUTINE
!
!
!
!-----------------------------------------------------------------------
!
   DO i = 1 , Ncol
      CALL unpack(*50,Input,Z)
      CALL pack(Z,Output,Mcb)
      CYCLE
!
!     NULL COLUMN
!
 50   CALL bldpk(it1,it2,Output,0,0)
      CALL bldpkn(Output,0,Mcb)
   ENDDO
END SUBROUTINE ampc1
