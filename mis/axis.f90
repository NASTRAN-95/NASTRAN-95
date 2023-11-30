
SUBROUTINE axis(Xa,Ya,Xb,Yb,Penx,Opt)
   IMPLICIT NONE
   INTEGER Model , Npens , Ploter
   REAL Skpa(6) , Skpplt(18)
   COMMON /pltdat/ Model , Ploter , Skpplt , Skpa , Npens
   INTEGER Opt , Penx
   REAL Xa , Xb , Ya , Yb
   INTEGER pen
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
      pen = pen - Npens*((pen-1)/Npens)
   ENDIF
!
   CALL axis10(Xa,Ya,Xb,Yb,pen,Opt)
END SUBROUTINE axis