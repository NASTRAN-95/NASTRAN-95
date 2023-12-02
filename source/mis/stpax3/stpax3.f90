!*==stpax3.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpax3(Again)
   IMPLICIT NONE
   USE C_ISAVE
   USE C_SDR2DE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: Again
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: cangle , i , j
   INTEGER , DIMENSION(62,14) :: iblock , iclock
   INTEGER , DIMENSION(25) :: iforce
   INTEGER , DIMENSION(100) :: istres
   REAL , DIMENSION(75) :: savef , saves
!
! End of declarations rewritten by SPAG
!
!
!
!
!
! SCRATCH BLOCK
!
!
!
!
   !>>>>EQUIVALENCE (Istres(1),Stress(1)) , (Iforce(1),Force(1)) , (Iblock(1,1),Block(1,1)) , (Iclock(1,1),Clock(1,1)) ,                 &
!>>>>    & (Isavef(1),Savef(1)) , (Isaves(1),Saves(1)) , (Nangle,Cangle)
!
   IF ( .NOT.(Again) ) THEN
      Again = .TRUE.
      Kangle = 0
   ENDIF
   Nangle = Kangle
   Elemid = Klemid
   Nangle = Nangle + 1
   Kangle = Nangle
!
!
!  BRANCH TO INSERT STRESSES AND FORCES INTO FORCE AND STRESS OR
!                                            SAVEF AND SAVES
!
!    KTYPE=1 - REAL OUTPUT FROM BLOCK IS TRANSFERED TO CLOCK, THEN
!              STORED IN FORCE AND STRESS, NOTHING IN SAVEF AND SAVES
!    KTYPE=2 - COMPLEX OUTPUT
!    IPART=1 - IMAGINARY PART OF COMPLEX OUTPUT FROM BLOCK, STORED
!              IN SAVEF AND SAVES
!    IPART=2 - REAL PART OF COMPLEX OUTPUT FROM CLOCK STORED IN
!              FORCE AND STRESS
!
   IF ( Ktype/=2 ) THEN
      DO i = 1 , 62
         DO j = 1 , 14
            Clock(i,j) = Block(i,j)
         ENDDO
      ENDDO
   ENDIF
!
!  OUTPUT FORCES FOR THIS ANGLE
   iforce(1) = Elemid
   Force(2) = Clock(1,cangle)
   DO i = 1 , 16
      Force(2+i) = Clock(46+i,cangle)
   ENDDO
!
! OUTPUT STRESSES
   istres(1) = Elemid
   Stress(2) = Clock(1,cangle)
   DO i = 1 , 45
      Stress(2+i) = Clock(i+1,cangle)
   ENDDO
!
   IF ( Ktype==2 ) THEN
!
!
!  OUTPUT FORCES FOR THIS ANGLE
      Isavef(1) = Elemid
      savef(2) = Block(1,Nangle)
      DO i = 1 , 16
         savef(2+i) = Block(46+i,Nangle)
      ENDDO
!
! OUTPUT STRESSES
      Isaves(1) = Elemid
      saves(2) = Block(1,Nangle)
      DO i = 1 , 45
         saves(2+i) = Block(i+1,Nangle)
      ENDDO
!
      IF ( Nangle==14 ) THEN
         Again = .FALSE.
         RETURN
      ELSEIF ( iblock(1,Nangle+1)==1 ) THEN
         Again = .FALSE.
         RETURN
      ENDIF
   ELSEIF ( cangle==14 ) THEN
      Again = .FALSE.
      RETURN
   ELSEIF ( iclock(1,cangle+1)==1 ) THEN
      Again = .FALSE.
      RETURN
   ENDIF
!
   RETURN
END SUBROUTINE stpax3
