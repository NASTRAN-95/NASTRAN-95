!*==dssize.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dssize(Namfil,Ncols,Nterms,Nstrgs,Nwdtrm)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Namfil
   INTEGER :: Ncols
   INTEGER :: Nterms
   INTEGER :: Nstrgs
   INTEGER :: Nwdtrm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: mcb
!
! End of declarations rewritten by SPAG
!
!
!   DSSIZE DETERMINES THE SIZE OF A GIVEN MATRIX FILE
!      NCOLS  = NUMBER OF COLUMNS
!      NTERMS = TOTAL NUMBER OF NON-ZERO TERMS IN MATRIX
!      NSTRGS = TOTAL NUMBER OF STRINGS OF CONSECUTIVE TERMS IN MATRIX
!      NWDTRM = NUMBER OF WORDS PER TERM
!
   CALL geturn(Namfil)
   IF ( ifilex==0 ) THEN
      Nterms = 0
      Nstrgs = 0
      Ncols = 0
      Nwdtrm = 0
   ELSE
      mcb(1) = Namfil
      CALL rdtrl(mcb)
      Ncols = mcb(2)
      Nstrgs = fcb(16,ifilex)
      Nterms = fcb(17,ifilex)
      Nwdtrm = 2
      IF ( mcb(5)==1 ) Nwdtrm = 1
      IF ( mcb(5)==4 ) Nwdtrm = 4
   ENDIF
END SUBROUTINE dssize
