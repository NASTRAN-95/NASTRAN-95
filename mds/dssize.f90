
SUBROUTINE dssize(Namfil,Ncols,Nterms,Nstrgs,Nwdtrm)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Mem(4)
   COMMON /zzzzzz/ Mem
   INTEGER Namfil , Ncols , Nstrgs , Nterms , Nwdtrm
   INTEGER mcb(7)
!
!   DSSIZE DETERMINES THE SIZE OF A GIVEN MATRIX FILE
!      NCOLS  = NUMBER OF COLUMNS
!      NTERMS = TOTAL NUMBER OF NON-ZERO TERMS IN MATRIX
!      NSTRGS = TOTAL NUMBER OF STRINGS OF CONSECUTIVE TERMS IN MATRIX
!      NWDTRM = NUMBER OF WORDS PER TERM
!
   CALL geturn(Namfil)
   IF ( Ifilex==0 ) THEN
      Nterms = 0
      Nstrgs = 0
      Ncols = 0
      Nwdtrm = 0
   ELSE
      mcb(1) = Namfil
      CALL rdtrl(mcb)
      Ncols = mcb(2)
      Nstrgs = Fcb(16,Ifilex)
      Nterms = Fcb(17,Ifilex)
      Nwdtrm = 2
      IF ( mcb(5)==1 ) Nwdtrm = 1
      IF ( mcb(5)==4 ) Nwdtrm = 4
   ENDIF
END SUBROUTINE dssize