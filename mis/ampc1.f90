
SUBROUTINE ampc1(Input,Output,Ncol,Z,Mcb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , It1 , It2 , Nn
   COMMON /packx / It1 , It2 , Ii , Nn , Incr
!
! Dummy argument declarations
!
   INTEGER Input , Ncol , Output
   INTEGER Mcb(7) , Z(1)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
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
 50   CALL bldpk(It1,It2,Output,0,0)
      CALL bldpkn(Output,0,Mcb)
   ENDDO
END SUBROUTINE ampc1
