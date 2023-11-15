
SUBROUTINE subp(I,L,Ls,J,Sgr,Cgr,Yrec,Zrec,Sum,Xic,Delx,Ee,Xlam,Sg,Cg,Ys,Zs)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(3) , F , Fmach , Kr , Refc
   INTEGER Mcb(7) , Nd , Ne , Nrow
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Kr
   COMMON /dlcom / Dum , F
!
! Dummy argument declarations
!
   REAL Cgr , Sgr , Yrec , Zrec
   INTEGER I , J , L , Ls
   COMPLEX Sum
   REAL Cg(1) , Delx(1) , Ee(1) , Sg(1) , Xic(1) , Xlam(1) , Ys(1) , Zs(1)
!
! Local variable declarations
!
   REAL ax , ax1 , ax2 , ay , ay1 , ay2 , az , az1 , az2 , beta , cgs , cl , cv , deli , delii , delr , delri , dely , dij , diji , &
      & dxs , eps , es , fl , flnd , flne , m , sdelx , sgs , sl , sqtl , tl , x , x0 , y0 , z0
   COMPLEX dp , dpll , dplr , dpul , dpur
   INTEGER igo , nobi
!
! End of declarations
!
!
!     COMPUTES ELEMENTS OF THE SUBMATRICES  DPP, DPZ  AND  DPY
!     USING  SUBROUTINES  SNPDF,  INCRO  AND SUBI
!
!
   eps = 0.00001
   m = Fmach
   beta = sqrt(1.0-m*m)
   fl = Refc
   flnd = float(Nd)
   flne = float(Ne)
   sgs = Sg(Ls)
   cgs = Cg(Ls)
   dpur = (0.0,0.0)
   dpul = (0.0,0.0)
   dplr = (0.0,0.0)
   dpll = (0.0,0.0)
   dij = 0.0
   delr = 0.0
   deli = 0.0
   diji = 0.0
   delri = 0.0
   delii = 0.0
!
!     UPPER RIGHT SENDING POINT
!
   igo = 1
   tl = Xlam(J)
   sqtl = sqrt(1.0+tl**2)
   sl = tl/sqtl
   cl = 1.0/sqtl
   x = Xic(I) + F*Delx(I)
   x0 = x - Xic(J)
   y0 = Yrec - Ys(Ls)
   z0 = Zrec - Zs(Ls)
   es = Ee(Ls)
   dxs = Delx(J)
   ax = x0
   ay = y0
   az = z0
   cv = dxs
 100  DO
!
      nobi = 1
      CALL snpdf(sl,cl,tl,sgs,cgs,Sgr,Cgr,x0,y0,z0,es,dij,beta,cv)
      IF ( Kr>eps ) THEN
         sdelx = dxs
         dely = 2.0*es
         ax1 = ax + es*tl
         ay1 = ay + es*cgs
         az1 = az + es*sgs
         ax2 = ax - es*tl
         ay2 = ay - es*cgs
         az2 = az - es*sgs
         CALL incro(ax,ay,az,ax1,ay1,az1,ax2,ay2,az2,Sgr,Cgr,sgs,cgs,Kr,fl,beta,sdelx,dely,delr,deli)
      ENDIF
      dp = cmplx(((dij+diji)-(delr+delri)),(-deli-delii))
      IF ( igo==2 ) THEN
         dpul = dp
         EXIT
      ELSEIF ( igo==3 ) THEN
         dplr = dp
         IF ( Nd==0 ) THEN
            Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
            GOTO 99999
         ELSE
!
!     LOWER LEFT  SENDING POINT
!
            igo = 4
            sgs = Sg(Ls)
            tl = -Xlam(J)
            sl = tl/(sqrt(1.0+tl*tl))
            y0 = Yrec + Ys(Ls)
            ay = y0
         ENDIF
      ELSEIF ( igo==4 ) THEN
         dpll = dp
         Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
         GOTO 99999
      ELSE
         dpur = dp
!
!     TEST FOR ABS(YS(LS)) .LE..001 TAKEN OUT
!
         IF ( Nd==0 ) EXIT
!
!     UPPER LEFT  SENDING POINT
!
         igo = 2
         sgs = -sgs
         tl = -tl
         sl = -sl
         y0 = Yrec + Ys(Ls)
         ay = y0
      ENDIF
   ENDDO
   IF ( Ne==0 ) THEN
      Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
   ELSE
!
!     LOWER RIGHT SENDING POINT
!
      igo = 3
      tl = Xlam(J)
      sl = tl/(sqrt(1.0+tl*tl))
      y0 = Yrec - Ys(Ls)
      z0 = Zrec + Zs(Ls)
      ay = y0
      az = z0
      sgs = -Sg(Ls)
      GOTO 100
   ENDIF
99999 END SUBROUTINE subp
