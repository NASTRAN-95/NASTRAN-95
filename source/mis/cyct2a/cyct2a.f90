!*==cyct2a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cyct2a(Kaa,Kxx,G1,G2,Scr1,Scr2,Scr3)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kaa
   INTEGER :: Kxx
   INTEGER :: G1
   INTEGER :: G2
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iout , isc , isc1 , isc2
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL rdtrl , ssg2b
!
! End of declarations rewritten by SPAG
!
!
   mcb(1) = Kaa
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 ) THEN
      mcb(1) = Kxx
      CALL rdtrl(mcb)
      IF ( mcb(1)>0 ) THEN
         isc2 = Scr2
         mcb(1) = G1
         CALL rdtrl(mcb)
         IF ( mcb(2)<=0 ) isc2 = 0
         mcb(1) = G2
         CALL rdtrl(mcb)
         IF ( mcb(2)<=0 ) THEN
            isc = Kxx
            isc1 = Scr2
            iout = 0
         ELSE
            isc = Scr2
            iout = 1
            isc1 = Kxx
         ENDIF
!
!     NO FIRST TERM IF ISC2=0, NO SECOND TERM IF IOUT=0
!
         IF ( isc2/=0 ) THEN
!
!     COMPUTE FIRST TERM
!
            CALL ssg2b(Kaa,G1,0,Scr1,0,Iprec,1,Scr2)
            CALL ssg2b(G1,Scr1,0,isc,1,Iprec,1,isc1)
         ENDIF
!
!     COMPUTE SECOND TERM
!
!     COMPUTE SECOND TERM
!
         IF ( iout/=0 ) THEN
            CALL ssg2b(Kaa,G2,0,Scr1,0,Iprec,1,Kxx)
            CALL ssg2b(G2,Scr1,isc2,Kxx,1,Iprec,1,Scr3)
         ENDIF
         mcb(1) = Kxx
      ENDIF
   ENDIF
END SUBROUTINE cyct2a
