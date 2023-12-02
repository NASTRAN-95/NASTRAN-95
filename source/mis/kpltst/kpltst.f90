!*==kpltst.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kpltst(G1,G2,G3,G4)
   USE c_sma1dp
   USE c_sma1et
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: G1
   REAL , DIMENSION(3) :: G2
   REAL , DIMENSION(3) :: G3
   REAL , DIMENSION(3) :: G4
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dh , dl , r1l , r2l
   EXTERNAL page2 , sadotb , saxb
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE WILL VERIFY THAT THE 4 GRID POINTS IN 3 SPACE LIE IN
!     AN APPROXIMATE PLANE. IF NOT THE NOGO FLAG IS SET TRUE AND A
!     MESSAGE IS WRITEN.
!
!
   r13(1) = G3(1) - G1(1)
   r13(2) = G3(2) - G1(2)
   r13(3) = G3(3) - G1(3)
   r24(1) = G4(1) - G2(1)
   r24(2) = G4(2) - G2(2)
   r24(3) = G4(3) - G2(3)
   CALL saxb(r13,r24,rxr)
!
!     NORMALIZE
!
   dl = sqrt(rxr(1)**2+rxr(2)**2+rxr(3)**2)
   IF ( dl>0 ) THEN
      rxr(1) = rxr(1)/dl
      rxr(2) = rxr(2)/dl
      rxr(3) = rxr(3)/dl
      r1l = sqrt(r13(1)**2+r13(2)**2+r13(3)**2)
      r2l = sqrt(r24(1)**2+r24(2)**2+r24(3)**2)
      dl = amin1(r1l,r2l)
      r(1) = G2(1) - G1(1)
      r(2) = G2(2) - G1(2)
      r(3) = G2(3) - G1(3)
      dh = sadotb(r,rxr)
      IF ( dl>0 ) THEN
         IF ( abs(dh/dl)<=0.10 ) RETURN
      ENDIF
   ENDIF
!
!     NOT PLANER
!
   CALL page2(-2)
   WRITE (out,99001) uwm , id
99001 FORMAT (A25,' 4000, ONE SIDE OF ELEMENT',I10,' CONNECTING FOUR POINTS IS NOT APPROXIMATELY PLANER.')
END SUBROUTINE kpltst
