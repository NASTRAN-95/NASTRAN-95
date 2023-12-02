!*==kpltst.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kpltst(G1,G2,G3,G4)
   IMPLICIT NONE
   USE C_SMA1DP
   USE C_SMA1ET
   USE C_SYSTEM
   USE C_XMSSG
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
   R13(1) = G3(1) - G1(1)
   R13(2) = G3(2) - G1(2)
   R13(3) = G3(3) - G1(3)
   R24(1) = G4(1) - G2(1)
   R24(2) = G4(2) - G2(2)
   R24(3) = G4(3) - G2(3)
   CALL saxb(R13,R24,Rxr)
!
!     NORMALIZE
!
   dl = sqrt(Rxr(1)**2+Rxr(2)**2+Rxr(3)**2)
   IF ( dl>0 ) THEN
      Rxr(1) = Rxr(1)/dl
      Rxr(2) = Rxr(2)/dl
      Rxr(3) = Rxr(3)/dl
      r1l = sqrt(R13(1)**2+R13(2)**2+R13(3)**2)
      r2l = sqrt(R24(1)**2+R24(2)**2+R24(3)**2)
      dl = amin1(r1l,r2l)
      R(1) = G2(1) - G1(1)
      R(2) = G2(2) - G1(2)
      R(3) = G2(3) - G1(3)
      dh = sadotb(R,Rxr)
      IF ( dl>0 ) THEN
         IF ( abs(dh/dl)<=0.10 ) RETURN
      ENDIF
   ENDIF
!
!     NOT PLANER
!
   CALL page2(-2)
   WRITE (Out,99001) Uwm , Id
99001 FORMAT (A25,' 4000, ONE SIDE OF ELEMENT',I10,' CONNECTING FOUR POINTS IS NOT APPROXIMATELY PLANER.')
END SUBROUTINE kpltst
