!*==axis.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE axis(Xa,Ya,Xb,Yb,Penx,Opt)
   USE c_pltdat
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Xa
   REAL :: Ya
   REAL :: Xb
   REAL :: Yb
   INTEGER :: Penx
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: pen
   EXTERNAL axis10
!
! End of declarations rewritten by SPAG
!
!
!     (XA,YA) = STARTING POINT OF THE AXIS.
!     (XB,YB) = TERMINAL POINT OF THE AXIS.
!     PENX    = PEN NUMBER OR LINE DENSITY (DEPENDS ON PLOTTER).
!     OPT     = -1 TO INITIATE  THE LINE MODE.
!             = +1 TO TERMINATE THE LINE MODE.
!             =  0 TO DRAW A LINE.
!
!
   IF ( Opt==0 ) THEN
      pen = max0(Penx,1)
      pen = pen - npens*((pen-1)/npens)
   ENDIF
!
   CALL axis10(Xa,Ya,Xb,Yb,pen,Opt)
END SUBROUTINE axis
