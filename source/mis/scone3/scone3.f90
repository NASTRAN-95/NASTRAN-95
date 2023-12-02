!*==scone3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scone3(Again)
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
   INTEGER :: i , j
   INTEGER , DIMENSION(9,14) :: iblock
   INTEGER , DIMENSION(25) :: iforce
   INTEGER , DIMENSION(100) :: istres
!
! End of declarations rewritten by SPAG
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Istres(1),Stress(1))
   !>>>>EQUIVALENCE (Iforce(1),Force(1))
   !>>>>EQUIVALENCE (Iblock(1,1),Block(1,1))
!
   IF ( .NOT.(Again) ) THEN
      Again = .TRUE.
      nangle = 0
   ENDIF
   nangle = nangle + 1
!*****
!     OUTPUT FORCES FOR THIS ANGLE
!*****
   iforce(1) = elemid
   force(2) = block(1,nangle)
   force(3) = block(5,nangle)
   force(4) = block(6,nangle)
   force(5) = block(7,nangle)
   force(6) = block(8,nangle)
   force(7) = block(9,nangle)
!*****
! COMPUTE AND OUTPUT STRESSES AND PRINCIPAL STRESSES
!*****
   istres(1) = elemid
   stress(2) = block(1,nangle)
   DO i = 1 , 2
      zoveri = 0.0
      IF ( iii/=0.0 ) zoveri = zoff(i)/iii
      DO j = 1 , 3
         sig(j) = block(j+1,nangle) + block(j+4,nangle)*zoveri
      ENDDO
      temp = sig(1) - sig(2)
      sig12 = sqrt((temp*0.50E0)**2+sig(3)**2)
      delta = (sig(1)+sig(2))*0.50E0
      sig1 = delta + sig12
      sig2 = delta - sig12
      delta = 2.0E0*sig(3)
      IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
         theta = 0.0E0
      ELSE
         theta = atan2(delta,temp)*28.6478898E0
      ENDIF
      ipt = 8*i - 6
      stress(ipt+1) = zoff(i)
      stress(ipt+2) = sig(1)
      stress(ipt+3) = sig(2)
      stress(ipt+4) = sig(3)
      stress(ipt+5) = theta
      stress(ipt+6) = sig1
      stress(ipt+7) = sig2
      stress(ipt+8) = sig12
   ENDDO
!*****
! SET AGAIN .FALSE. IF SDR2E IS NOT TO CALL THIS ROUTINE AGAIN FOR THIS
! ELEMENT.. E.G. ALL THE ANGLES DESIRED HAVE BEEN PROCESSED...
!*****
   IF ( nangle==14 ) THEN
      Again = .FALSE.
   ELSEIF ( iblock(1,nangle+1)==1 ) THEN
      Again = .FALSE.
   ELSE
      RETURN
   ENDIF
END SUBROUTINE scone3
