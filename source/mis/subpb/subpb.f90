!*==subpb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subpb(I,L,Ls,J,Sgr,Cgr,Yrec,Zrec,Sum,Xic,Delx,Ee,Xlam,Sg,Cg,Ys,Zs,Nas,Nasb,Avr,Zb,Yb,Arb,Xle,Xte,X,Nb)
   USE c_amgmn
   IMPLICIT NONE
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
   INTEGER , DIMENSION(1) :: Nas
   INTEGER , DIMENSION(1) :: Nasb
   REAL , DIMENSION(1) :: Avr
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Arb
   REAL , DIMENSION(1) :: Xle
   REAL , DIMENSION(1) :: Xte
   REAL , DIMENSION(1) :: X
   INTEGER :: Nb
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ax , ax1 , ax2 , ay , ay1 , ay1i , ay2 , ay2i , ayi , az , az1 , az1i , az2 , az2i , azi , beta , cgs , cl , cv , da ,   &
         & dar , dcgam , dcgami , dcl , dee , deei , deei2 , deli , delii , delis , delr , delri , delrs , dely , deta , detai ,    &
         & dij , diji , dijs , dmuy , dmuz , dsgam , dsgami , dsl , dsqrtl , dtl , dtlami , dxi , dxle , dxs , dxte , dyb , dzb ,   &
         & dzeta , dzetai , eps , es , fl , flnd , flne , m , sdelx , sgs , sl , sqtl , tl , x0 , x0i , y0 , y0i , z0 , z0i
   COMPLEX :: dp , dpll , dplr , dpul , dpur
   INTEGER :: igo , infl , ioutfl , na , na1 , na2 , noas , nob , nobi
   EXTERNAL incro , snpdf , subi
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
!     USING  SUBROUTINES  SNPDF, INCRO AND SUBI
!
!
         eps = 0.00001
         m = fmach
         beta = sqrt(1.0-m*m)
         fl = refc
         flnd = float(nd)
         flne = float(ne)
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
         infl = 0
         ioutfl = 0
!
!     UPPER RIGHT SENDING POINT
!
         igo = 1
         tl = Xlam(J)
         sqtl = sqrt(1.0+tl**2)
         sl = tl/sqtl
         cl = 1.0/sqtl
         x0 = X(I) - Xic(J)
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
            na2 = 0
            CALL snpdf(sl,cl,tl,sgs,cgs,Sgr,Cgr,x0,y0,z0,es,dij,beta,cv)
            IF ( kr>eps ) THEN
               sdelx = dxs
               dely = 2.0*es
               ax1 = ax + es*tl
               ay1 = ay + es*cgs
               az1 = az + es*sgs
               ax2 = ax - es*tl
               ay2 = ay - es*cgs
               az2 = az - es*sgs
               CALL incro(ax,ay,az,ax1,ay1,az1,ax2,ay2,az2,Sgr,Cgr,sgs,cgs,kr,fl,beta,sdelx,dely,delr,deli)
            ENDIF
            IF ( Nb/=0 ) THEN
               noas = Nas(L)
!
!     CHECK FOR ASSOCIATED BODIES
!
               IF ( noas/=0 ) THEN
                  dijs = dij
                  delrs = delr
                  delis = deli
                  diji = 0.0
                  delri = 0.0
                  delii = 0.0
                  na1 = na2 + 1
                  na2 = na2 + noas
                  IF ( na2>Nb ) na2 = Nb
!
!     START DO-LOOP FOR THE SUMMATION OF THE WING-IMAGE CONTRIBUTIONS
!     OVER  RANGE(P)
!
                  DO na = na1 , na2
                     nob = Nasb(na)
!
!     NOB IS THE SEQUENCE NUMBER OF THE CURRENT BODY ASSOCIATED WITH
!     PANEL  L  IN WHICH THE SENDING POINT  J  LIES
!
                     nobi = nob
                     da = Avr(nob)
                     dar = Arb(nob)
                     dxle = Xle(nob)
                     dxte = Xte(nob)
                     IF ( igo==2 ) THEN
                        dzb = Zb(nob)
                        dyb = -Yb(nob)
                        deta = -Ys(Ls)
                        dzeta = Zs(Ls)
                     ELSEIF ( igo==3 ) THEN
                        dzb = -Zb(nob)
                        dyb = Yb(nob)
                        deta = Ys(Ls)
                        dzeta = -Zs(Ls)
                     ELSEIF ( igo==4 ) THEN
                        dzb = -Zb(nob)
                        dyb = -Yb(nob)
                        deta = -Ys(Ls)
                        dzeta = -Zs(Ls)
                     ELSE
                        dzb = Zb(nob)
                        dyb = Yb(nob)
                        deta = Ys(Ls)
                        dzeta = Zs(Ls)
                     ENDIF
                     dcgam = cgs
                     dsgam = sgs
                     dee = es
                     dxi = Xic(J)
                     IF ( dxi>=dxle .AND. dxi<=dxte ) THEN
                        CALL subi(da,dzb,dyb,dar,deta,dzeta,dcgam,dsgam,dee,dxi,tl,detai,dzetai,dcgami,dsgami,deei,dtlami,dmuy,dmuz,&
                                & infl,ioutfl)
                        dij = 0.0
                        IF ( infl==0 .AND. ioutfl/=0 ) THEN
                           dtl = dtlami
                           dsqrtl = sqrt(1.0+dtl**2)
                           dsl = dtl/dsqrtl
                           dcl = 1.0/dsqrtl
                           x0i = x0
                           y0i = Yrec - detai
                           z0i = Zrec - dzetai
                           CALL snpdf(dsl,dcl,dtl,dsgami,dcgami,Sgr,Cgr,x0i,y0i,z0i,deei,dij,beta,cv)
                           diji = diji + dij
                           IF ( kr>eps ) THEN
                              delr = 0.0
                              deli = 0.0
                              ayi = y0i
                              azi = z0i
                              ay1i = ayi - deei*dcgami
                              az1i = azi - deei*dsgami
                              ay2i = ayi + deei*dcgami
                              az2i = azi + deei*dsgami
                              deei2 = 2.0*deei
                              CALL incro(ax,ayi,azi,ax1,ay1i,az1i,ax2,ay2i,az2i,Sgr,Cgr,dsgami,dcgami,kr,fl,beta,sdelx,deei2,delr,  &
                               & deli)
                              delri = delri + delr
                              delii = delii + deli
                              CYCLE
                           ENDIF
                        ENDIF
                        delri = 0.0
                        delii = 0.0
                     ENDIF
                  ENDDO
                  dij = dijs
                  delr = delrs
                  deli = delis
               ENDIF
            ENDIF
            dp = cmplx(((dij+diji)-(delr+delri)),(-deli-delii))
            IF ( igo==2 ) THEN
               dpul = dp
               EXIT SPAG_Loop_1_1
            ELSEIF ( igo==3 ) THEN
               dplr = dp
               IF ( nd==0 ) THEN
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
               IF ( nd==0 ) EXIT SPAG_Loop_1_1
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
         IF ( ne==0 ) THEN
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
END SUBROUTINE subpb
