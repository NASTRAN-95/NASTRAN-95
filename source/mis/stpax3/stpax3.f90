!*==stpax3.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpax3(Again)
   USE c_isave
   USE c_sdr2de
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   IMPLICIT NONE
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
      kangle = 0
   ENDIF
   nangle = kangle
   elemid = klemid
   nangle = nangle + 1
   kangle = nangle
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
   IF ( ktype/=2 ) THEN
      DO i = 1 , 62
         DO j = 1 , 14
            clock(i,j) = block(i,j)
         ENDDO
      ENDDO
   ENDIF
!
!  OUTPUT FORCES FOR THIS ANGLE
   iforce(1) = elemid
   force(2) = clock(1,cangle)
   DO i = 1 , 16
      force(2+i) = clock(46+i,cangle)
   ENDDO
!
! OUTPUT STRESSES
   istres(1) = elemid
   stress(2) = clock(1,cangle)
   DO i = 1 , 45
      stress(2+i) = clock(i+1,cangle)
   ENDDO
!
   IF ( ktype==2 ) THEN
!
!
!  OUTPUT FORCES FOR THIS ANGLE
      isavef(1) = elemid
      savef(2) = block(1,nangle)
      DO i = 1 , 16
         savef(2+i) = block(46+i,nangle)
      ENDDO
!
! OUTPUT STRESSES
      isaves(1) = elemid
      saves(2) = block(1,nangle)
      DO i = 1 , 45
         saves(2+i) = block(i+1,nangle)
      ENDDO
!
      IF ( nangle==14 ) THEN
         Again = .FALSE.
         RETURN
      ELSEIF ( iblock(1,nangle+1)==1 ) THEN
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
END SUBROUTINE stpax3
