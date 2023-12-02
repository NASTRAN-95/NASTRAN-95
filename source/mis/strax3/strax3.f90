!*==strax3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strax3(Again)
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
   INTEGER :: cangle , i , j , k
   INTEGER , DIMENSION(22,14) :: iblock , iclock
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
      nangle = 0
   ENDIF
   nangle = nangle + 1
!
!
!  BRANCH TO INSERT STRESSES AND FORCES INTO FORCE AND STRESS OR
!                                            SAVEF AND SAVES
!
!    KTYPE=1 - REAL OUTPUT FROM BLOCK IS TRANSFERED TO CLOCK, THEN
!            STORED IN FORCE AND STRESS, NOTHING IN SAVEF AND SAVES
!    KTYPE=2 - COMPLEX OUTPUT
!    IPART=1 - IMAGINARY PART OF COMPLEX OUTPUT FROM BLOCK, STORED
!              IN SAVEF AND SAVES
!    IPART=2 - REAL PART OF COMPLEX OUTPUT FROM CLOCK STORED IN
!              FORCE AND STRESS
!
   IF ( ktype/=2 ) THEN
      DO i = 1 , 22
         DO j = 1 , 14
            clock(i,j) = block(i,j)
         ENDDO
      ENDDO
   ENDIF
!
!  OUTPUT FORCES FOR THIS ANGLE
!
   iforce(1) = elemid
   force(2) = clock(1,cangle)
   j = 2
   DO i = 1 , 9
      j = j + 1
      force(j) = clock(i+7,cangle)
!
!  OUTPUT CHARGES
      IF ( (i==3) .OR. (i==6) .OR. (i==9) ) THEN
         j = j + 1
         k = 19 + i/3
         force(j) = clock(k,cangle)
      ENDIF
   ENDDO
!
! OUTPUT STRESSES
   istres(1) = elemid
   stress(2) = clock(1,cangle)
   DO i = 1 , 6
      stress(2+i) = clock(i+1,cangle)
   ENDDO
!
!  OUTPUT FLUXES
   DO i = 1 , 3
      stress(i+8) = clock(i+16,cangle)
   ENDDO
!
   IF ( ktype==2 ) THEN
!
!
!  OUTPUT FORCES FOR THIS ANGLE
!
      isavef(1) = elemid
      savef(2) = block(1,nangle)
      j = 2
      DO i = 1 , 9
         j = j + 1
         savef(j) = block(i+7,nangle)
!
!  OUTPUT CHARGES
         IF ( (i==3) .OR. (i==6) .OR. (i==9) ) THEN
            j = j + 1
            k = 19 + i/3
            savef(j) = block(k,nangle)
         ENDIF
      ENDDO
!
! OUTPUT STRESSES
      isaves(1) = elemid
      saves(2) = block(1,nangle)
      DO i = 1 , 6
         saves(2+i) = block(i+1,nangle)
      ENDDO
!
!  OUTPUT FLUXES
      DO i = 1 , 3
         saves(i+8) = block(i+16,nangle)
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
END SUBROUTINE strax3
