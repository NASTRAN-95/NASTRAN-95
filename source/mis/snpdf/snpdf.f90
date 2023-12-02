!*==snpdf.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE snpdf(Sl,Cl,Tl,Sgs,Cgs,Sgr,Cgr,X0,Y0,Z0,Ee,Dij,Beta,Cv)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Sl
   REAL :: Cl
   REAL :: Tl
   REAL :: Sgs
   REAL :: Cgs
   REAL :: Sgr
   REAL :: Cgr
   REAL :: X0
   REAL :: Y0
   REAL :: Z0
   REAL :: Ee
   REAL :: Dij
   REAL :: Beta
   REAL :: Cv
!
! Local variable declarations rewritten by SPAG
!
   REAL :: acab , acbb , cab , cacb , cao , caoone , cave , cbb , cbi , clb , clcgs , clsgs , db2 , dbx , dby , dbz , di2 , do2 ,   &
         & ex , ey , ez , onecbi , ricab , rimag , rix , riy , riz , romag , rox , roy , roz , slb , sqtlb , test1 , test2 , tlb ,  &
         & vby , vbz , viy , viz , voy , voz , vy , vz , ww , x0b
!
! End of declarations rewritten by SPAG
!
!
!     SNPDF CALCULATES THE STEADY PART OF THE INFLUENCE COEFFICIENT
!     MATRIX ELEMENTS
!
   test1 = 0.9999
   test2 = 0.0001*Ee
!
!     ***  TEST1 AND TEST2  SERVE AS A MEASURE OF  'NEARNESS'  WITH
!     RESPECT TO THE BOUND-  AND TRAILING VORTICES RESPECTIVELY - SEE
!     TESTS BELOW
!     NOTE THAT THE MACH NUMBER EFFECT IS ACCOUNTED FOR BY STRETCHING
!     THE  X-COORDINATES AND THE  SWEEP ANGLE OF THE BOUND VORTEX LINE
!
   tlb = Tl/Beta
   sqtlb = sqrt(1.0+tlb**2)
   slb = tlb/sqtlb
   clb = 1.0/sqtlb
   cave = Cv
   clsgs = clb*Sgs
   clcgs = clb*Cgs
   ex = Ee*tlb
   ey = Ee*Cgs
   ez = Ee*Sgs
   x0b = X0/Beta
   rix = x0b + ex
   riy = Y0 + ey
   riz = Z0 + ez
   rimag = sqrt(rix**2+riy**2+riz**2)
   rox = x0b - ex
   roy = Y0 - ey
   roz = Z0 - ez
   romag = sqrt(rox**2+roy**2+roz**2)
   cab = (rix*slb+riy*clcgs+riz*clsgs)/rimag
   cbb = (rox*slb+roy*clcgs+roz*clsgs)/romag
   cbi = -rix/rimag
   cao = rox/romag
   ricab = rimag*cab
   dbx = rix - ricab*slb
   dby = riy - ricab*clcgs
   dbz = riz - ricab*clsgs
   db2 = dbx**2 + dby**2 + dbz**2
   di2 = riy**2 + riz**2
   do2 = roy**2 + roz**2
   acab = abs(cab)
   acbb = abs(cbb)
!
!     ***  THE FOLLOWING IS A TEST TO SEE IF THE RECEIVING POINT LIES ON
!     OR NEAR THE BOUND VORTEX  --  IF SO, THE CONTRIBUTION OF THE BOUND
!     VORTEX IS SET TO ZERO
!
   IF ( acab<=test1 ) THEN
      IF ( acbb<=test1 ) THEN
         cacb = (cab-cbb)/db2
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   IF ( cab*cbb<0 ) THEN
      cacb = 0.
   ELSE
      cacb = 0.5*abs((1./rimag**2)-(1./romag**2))
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      vby = cacb*(dbx*clsgs-dbz*slb)
      vbz = cacb*(dby*slb-dbx*clcgs)
!
!     ***  TEST TO SEE IF THE RECEIVING POINT LIES ON OR NEAR THE
!     INBOARD TRAILING VORTEX  --  IF SO, THE CONTRIBUTION OF THE
!     INBOARD TRAILING VORTEX IS SET TO ZERO
!
      IF ( di2>test2 ) THEN
         onecbi = (1.0-cbi)/di2
         viy = onecbi*riz
         viz = -onecbi*riy
      ELSE
         viy = 0.0
         viz = 0.0
      ENDIF
!
!     ***  TEST TO SEE IF THE RECEIVING POINT LIES ON OR NEAR THE
!     OUTBOARD TRAILING VORTEX  --  IF SO, THE CONTRIBUTION OF THE
!     OUTBOARD TRAILING VORTEX IS SET TO ZERO
!
      IF ( do2>test2 ) THEN
         caoone = (1.0+cao)/do2
         voy = -caoone*roz
         voz = caoone*roy
      ELSE
         voy = 0.0
         voz = 0.0
      ENDIF
      vy = vby + viy + voy
      vz = vbz + viz + voz
      ww = vy*Sgr - vz*Cgr
      Dij = ww*cave/25.132741
   END SUBROUTINE spag_block_1
END SUBROUTINE snpdf
