!*==selcam.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE selcam(Camera,Pltnum,Opt)
   USE c_pltdat
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Camera
   INTEGER :: Pltnum
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(17) :: a
   INTEGER , DIMENSION(3) , SAVE :: cam10
   INTEGER :: camnum , i
   INTEGER , DIMENSION(2) , SAVE :: con10
   REAL , DIMENSION(2) :: edge , origin , xymax
   REAL , DIMENSION(2,3) :: save
   EXTERNAL wplt10
!
! End of declarations rewritten by SPAG
!
!
!
!
   !>>>>EQUIVALENCE (Model,Pd(1,1)) , (Ploter,Pd(2,1)) , (Xymax(1),Pd(7,1)) , (Edge(1),Pd(9,1)) , (Camnum,Pd(11,1)) , (Origin(1),Pd(8,2))
   DATA con10 , cam10/1 , 2 , 1 , 2 , 3/
!
   DO i = 1 , 2
      save(i,1) = edge(i)
      edge(i) = 0.
      save(i,2) = origin(i)
      origin(i) = 0.
      save(i,3) = xymax(i)
      xymax(i) = 0.
      a(i) = iabs(Pltnum)
   ENDDO
   camnum = min0(max0(Camera,1),3)
   IF ( Opt==0 ) THEN
!
!     PLOTTER 1, 2
!
      a(3) = a(1)
      a(1) = con10(1)
      a(2) = 0
      a(4) = save(1,3) + 2.*save(1,1) + .1
      a(5) = save(2,3) + 2.*save(2,1) + .1
      a(6) = 0
      CALL wplt10(a,0)
   ENDIF
   a(1) = con10(2)
   a(2) = cam10(camnum)
   DO i = 3 , 6
      a(i) = 0
   ENDDO
   CALL wplt10(a,0)
!
   DO i = 1 , 2
      edge(i) = save(i,1)
      origin(i) = save(i,2)
      xymax(i) = save(i,3)
   ENDDO
END SUBROUTINE selcam
