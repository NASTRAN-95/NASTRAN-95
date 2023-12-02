!*==sd2rhd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sd2rhd(Istyp,Isetup)
   USE c_sdr2x4
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Istyp
   INTEGER :: Isetup
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , istyp6 , istyp7 , n1
   INTEGER , DIMENSION(8) , SAVE :: ldmd
   REAL :: rstyp6 , rstyp7
   EXTERNAL page2
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE WRITES HEADING FOR PRECISION CHECK IN SDR2E.
!     WORDS 1,2,6 AND 7 PRESET BY CALLING ROUTINE.
!     ISETUP.NE.0 FIRST CALL.
!
   !>>>>EQUIVALENCE (istyp6,rstyp6) , (istyp7,rstyp7)
   DATA ldmd/4HLOAD , 4HMODE , 4H, FR , 4HEQ.= , 4H, EI , 4HGEN= , 4H, TI , 4HME =/
!
   IF ( Isetup/=0 ) THEN
      IF ( branch==2 .OR. branch==5 ) THEN
!
!     EIGR,FREQ
!
         n1 = 6
         Istyp(3) = ldmd(2)
         Istyp(4) = ldmd(3)
         Istyp(5) = ldmd(4)
      ELSEIF ( branch==6 ) THEN
!
!     TRANSIENT
!
         n1 = 6
         Istyp(3) = ldmd(1)
         Istyp(4) = ldmd(7)
         Istyp(5) = ldmd(8)
      ELSEIF ( branch==8 .OR. branch==9 ) THEN
!
!     BUCKLING, COMPLEX EIGENVALUE
!
         n1 = 6
         Istyp(3) = ldmd(2)
         Istyp(4) = ldmd(5)
         Istyp(5) = ldmd(6)
         IF ( branch==9 ) n1 = 7
      ELSE
!
!     STATICS
!
         n1 = 3
         Istyp(3) = ldmd(1)
      ENDIF
   ENDIF
!
   CALL page2(3)
   istyp6 = Istyp(6)
   istyp7 = Istyp(7)
   IF ( n1==3 ) WRITE (nout,99001) (Istyp(i),i=1,n1)
   IF ( n1==6 ) WRITE (nout,99001) (Istyp(i),i=1,5) , rstyp6
   IF ( n1==7 ) WRITE (nout,99001) (Istyp(i),i=1,5) , rstyp6 , rstyp7
99001 FORMAT (1H0,5X,45HE L E M E N T   P R E C I S I O N   C H E C K,/4X,32HSIGNIFICANT DIGITS FOR SUBCASE =,I7,1H,,I7,3H = ,3A4,  &
            & 1P,2E15.6)
END SUBROUTINE sd2rhd
