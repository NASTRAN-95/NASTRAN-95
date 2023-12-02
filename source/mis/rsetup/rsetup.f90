!*==rsetup.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rsetup(Lvl,Lvls1,Lvls2,Nacum,Idim)
   USE c_bandb
   USE c_bandg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Lvl
   INTEGER , DIMENSION(1) :: Lvls1
   INTEGER , DIMENSION(1) :: Lvls2
   INTEGER , DIMENSION(1) :: Nacum
   INTEGER :: Idim
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , itemp
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     SETUP COMPUTES THE REVERSE LEVELING INFO FROM LVLS2 AND STORES
!     IT INTO LVLS2.  NACUM(I) IS INITIALIZED TO NODES/ITH LEVEL FOR
!     NODES ON THE PSEUDO-DIAMETER OF THE GRAPH.  LVL IS INITIALIZED TO
!     NON-ZERO FOR NODES ON THE PSEUDO-DIAM AND NODES IN A DIFFERENT
!     COMPONENT OF THE GRAPH.
!
!
!     IDIM=NUMBER OF LEVELS IN A GIVEN COMPONENT.
!     NACUM IS DIMENSIONED TO IDIM IN SIZE
!
!     DIMENSION EXCEEDED  . . .  STOP JOB.
!
   IF ( idpth<=Idim ) THEN
!
      DO i = 1 , idpth
         Nacum(i) = 0
      ENDDO
      DO i = 1 , n
         Lvl(i) = 1
         Lvls2(i) = idpth + 1 - Lvls2(i)
         itemp = Lvls2(i)
         IF ( itemp<=idpth ) THEN
            IF ( itemp/=Lvls1(i) ) THEN
               Lvl(i) = 0
            ELSE
               Nacum(itemp) = Nacum(itemp) + 1
            ENDIF
         ENDIF
      ENDDO
      RETURN
   ENDIF
   ngrid = -3
END SUBROUTINE rsetup
