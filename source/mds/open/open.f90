!*==open.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE open(Namfil,Buff,Op) !HIDESTARS (*,Namfil,Buff,Op)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Namfil
   INTEGER , DIMENSION(1) :: Buff
   INTEGER :: Op
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: init
   INTEGER :: xname , xop
!
! End of declarations rewritten by SPAG
!
!******
!
! OPEN IS AN INTERMEDIARY TO ENTRY POINT QOPEN IN SUBROUTINE GINO.
! THE MAIN TASK OF OPEN IS TO INSURE THAT DATA BLOCKS WHICH WERE
! WRITTEN AND CLOSED OFF THE LOAD POINT HAVE AN END-OF-FILE BEFORE
! THEY ARE READ.
!
!******
!
!
! TEST FOR CONDITION IN WHICH END-OF-FILE IS TO BE WRITTEN
!
   DATA init/0/
   IF ( init==0 ) THEN
      CALL dsiodd
      init = 1
   ENDIF
   xname = Namfil
   ifilex = 0
   CALL geturn(xname)
   IF ( ifilex==0 ) RETURN 1
   IF ( Op/=1 .AND. Op/=3 ) THEN
      IF ( nblock+nlr>7 ) THEN
         IF ( iprvop/=0 ) THEN
!
! DATA BLOCK WAS PREVIOUSLY OPENED TO WRITE AND IS NOW OFF LOAD POINT.
! WRITE AN END-OF-FILE. IF SPECIAL CALL, RETURN
!
            CALL qopen(*100,Namfil,Buff,3)
            CALL eof(Namfil)
            xop = 2
            IF ( Op==-2 ) xop = 1
            CALL close(Namfil,xop)
            IF ( Op==-2 ) RETURN
!
! NOW OPEN ACCORDING TO OP. IF NECESSARY, POSITION PRIOR TO EOF
!
            lasnam = 0
            CALL geturn(Namfil)
            CALL qopen(*100,Namfil,Buff,Op)
            IF ( Op==2 ) CALL bckrec(Namfil)
            RETURN
         ENDIF
      ENDIF
      IF ( Op==-2 ) RETURN
   ENDIF
!
! NORMAL OPEN CALL
!
   CALL qopen(*100,Namfil,Buff,Op)
!WKBNB NCL93007 11/94
! SET THE COUNT FOR THE TOTAL NUMBER OF STRINGS AND TERMS
! TO ZERO IF FILE IS BEING OPENED FOR WRITE
   IF ( Op==1 ) THEN
      fcb(16,ifilex) = 0
      fcb(17,ifilex) = 0
   ENDIF
!WKBNE NCL93007 11/94
   RETURN
 100  RETURN 1
END SUBROUTINE open
