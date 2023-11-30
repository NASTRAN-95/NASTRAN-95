
SUBROUTINE incro(Ax,Ay,Az,Ax1,Ay1,Az1,Ax2,Ay2,Az2,Sgr,Cgr,Sgs,Cgs,Kr,Fl,Beta,Sdelx,Dely,Delr,Deli)
   IMPLICIT NONE
   INTEGER Ind
   REAL K10 , K10t1 , K1it1 , K1rt1 , K20 , K20t2p , K2it2p , K2rt2p
   COMMON /dlm   / K10 , K20 , K1rt1 , K1it1 , K2rt2p , K2it2p , K10t1 , K20t2p
   COMMON /kds   / Ind
   REAL Ax , Ax1 , Ax2 , Ay , Ay1 , Ay2 , Az , Az1 , Az2 , Beta , Cgr , Cgs , Deli , Delr , Dely , Fl , Kr , Sdelx , Sgr , Sgs
   REAL a2i , a2r , aim , are , at1 , at1s , at2 , at2s , azet0 , b2i , b2r , bim , br , bre , c2i , c2r , cim , count , cre ,      &
      & diiji , diijr , dkic , dkii , dkio , dkrc , dkri , dkro , e2 , ee , eps , eta01 , m , pi , r1sqx , t1 , t2 , x0 , xdelx ,   &
      & xdely , xiiji , xiijr , xkic , xkii , xkio , xkrc , xkri , xkro , xmult , y0 , z0 , zero , zet01
!
!     CALCULATES THE UNSTEADY PART OF THE INFLUENCE COEFFICIENT MATRIX
!     ELEMENTS USING  SUBROUTINES  KERNEL, IDF1  AND  IDF2
!
!
!     DKRO = REAL PART OF THE PLANAR KERNEL  *  OUTBOARD POINT
!     DKIO = IMAGINARY PART OF THE PLANAR KERNEL  *  OUTBOARD POINT
!     XKRO = REAL PART OF THE NONPLANAR KERNEL  *  OUTBOARD POINT
!     XKIO = IMAGINARY PART OF THE NONPLANAR KERNEL  *  OUTBOARD POINT
!     DKRI = REAL PART OF THE PLANAR KERNEL  *   INBOARD POINT
!     DKII = IMAGINARY PART OF THE PLANAR KERNEL  *   INBOARD POINT
!     XKRI = REAL PART OF THE NONPLANAR KERNEL  *   INBOARD POINT
!     XKII = IMAGINARY PART OF THE NONPLANAR KERNEL  *   INBOARD POINT
!
   Ind = 1
   m = sqrt(1.0-Beta**2)
   br = Fl/2.
   eps = 0.00001
   pi = 3.14159265
   xdelx = Sdelx
   xdely = Dely
   ee = 0.5*xdely
   e2 = ee**2
   Delr = 0.0
   Deli = 0.0
   at1s = 0.0
   at2s = 0.0
   t1 = 0.0
   t2 = 0.0
   count = 0.
   x0 = Ax
   y0 = Ay
   z0 = Az
   DO
      CALL tker(x0,y0,z0,Kr,br,Sgr,Cgr,Sgs,Cgs,t1,t2,m)
      at1 = abs(t1)
      at2 = abs(t2)
      IF ( at1>at1s ) at1s = at1
      IF ( at2>at2s ) at2s = at2
      IF ( count<0 ) THEN
         dkri = K1rt1 - K10t1
         dkii = K1it1
         xkri = K2rt2p - K20t2p
         xkii = K2it2p
         count = 1.
         x0 = Ax2
         y0 = Ay2
         z0 = Az2
      ELSEIF ( count==0 ) THEN
         dkrc = K1rt1 - K10t1
         dkic = K1it1
         xkrc = K2rt2p - K20t2p
         xkic = K2it2p
         at2 = abs(t2)
         count = -1.
         x0 = Ax1
         y0 = Ay1
         z0 = Az1
      ELSE
         dkro = K1rt1 - K10t1
         dkio = K1it1
         xkro = K2rt2p - K20t2p
         xkio = K2it2p
         x0 = Ax
         y0 = Ay
         z0 = Az
         zero = 0.0
         xiijr = 0.
         xiiji = 0.
         diijr = 0.0
         diiji = 0.0
         xmult = xdelx/(8.0*pi)
         IF ( y0==zero .AND. z0==zero ) THEN
            eta01 = 0.0
            zet01 = 0.0
            r1sqx = 0.0
         ELSEIF ( z0==zero .AND. Sgs==zero ) THEN
            eta01 = y0*Cgs
            zet01 = 0.
            r1sqx = eta01**2
         ELSE
            eta01 = y0*Cgs + z0*Sgs
            zet01 = -y0*Sgs + z0*Cgs
            azet0 = abs(zet01)
            IF ( azet0<=0.0001 ) zet01 = 0.
            r1sqx = eta01**2 + zet01**2
         ENDIF
         are = (dkri-2.*dkrc+dkro)/(2.0*e2)
         aim = (dkii-2.*dkic+dkio)/(2.0*e2)
         bre = (dkro-dkri)/(2.0*ee)
         bim = (dkio-dkii)/(2.0*ee)
         cre = dkrc
         cim = dkic
         IF ( at1s/=0.0 ) THEN
            CALL idf1(ee,e2,eta01,zet01,are,aim,bre,bim,cre,cim,r1sqx,xiijr,xiiji)
            Delr = xmult*xiijr
            Deli = xmult*xiiji
         ENDIF
         IF ( at2s/=0.0 ) THEN
            a2r = (xkri-2.0*xkrc+xkro)/(2.0*e2)
            a2i = (xkii-2.0*xkic+xkio)/(2.0*e2)
            b2r = (xkro-xkri)/(2.0*ee)
            b2i = (xkio-xkii)/(2.0*ee)
            c2r = xkrc
            c2i = xkic
            CALL idf2(ee,e2,eta01,zet01,a2r,a2i,b2r,b2i,c2r,c2i,r1sqx,diijr,diiji)
            Delr = Delr + xmult*diijr
            Deli = Deli + xmult*diiji
         ENDIF
         EXIT
      ENDIF
   ENDDO
!
END SUBROUTINE incro