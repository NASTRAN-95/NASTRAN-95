!*==subp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subp(I,L,Ls,J,Sgr,Cgr,Yrec,Zrec,Sum,Xic,Delx,Ee,Xlam,Sg,Cg,Ys,Zs)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_DLCOM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I
   INTEGER :: L
   INTEGER :: Ls
   INTEGER :: J
   REAL :: Sgr
   REAL :: Cgr
   REAL :: Yrec
   REAL :: Zrec
   COMPLEX :: Sum
   REAL , DIMENSION(1) :: Xic
   REAL , DIMENSION(1) :: Delx
   REAL , DIMENSION(1) :: Ee
   REAL , DIMENSION(1) :: Xlam
   REAL , DIMENSION(1) :: Sg
   REAL , DIMENSION(1) :: Cg
   REAL , DIMENSION(1) :: Ys
   REAL , DIMENSION(1) :: Zs
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ax , ax1 , ax2 , ay , ay1 , ay2 , az , az1 , az2 , beta , cgs , cl , cv , deli , delii , delr , delri , dely , dij ,     &
         & diji , dxs , eps , es , fl , flnd , flne , m , sdelx , sgs , sl , sqtl , tl , x , x0 , y0 , z0
   COMPLEX :: dp , dpll , dplr , dpul , dpur
   INTEGER :: igo , nobi
   EXTERNAL incro , snpdf
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
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
               EXIT SPAG_Loop_1_1
            ELSEIF ( igo==3 ) THEN
               dplr = dp
               IF ( Nd==0 ) THEN
                  Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
                  RETURN
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
               RETURN
            ELSE
               dpur = dp
!
!     TEST FOR ABS(YS(LS)) .LE..001 TAKEN OUT
!
               IF ( Nd==0 ) EXIT SPAG_Loop_1_1
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
         ENDDO SPAG_Loop_1_1
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
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE subp
