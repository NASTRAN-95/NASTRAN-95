
SUBROUTINE subpb(I,L,Ls,J,Sgr,Cgr,Yrec,Zrec,Sum,Xic,Delx,Ee,Xlam,Sg,Cg,Ys,Zs,Nas,Nasb,Avr,Zb,Yb,Arb,Xle,Xte,X,Nb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Fmach , Kr , Refc
   INTEGER Mcb(7) , Nd , Ne , Nrow
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Kr
!
! Dummy argument declarations
!
   REAL Cgr , Sgr , Yrec , Zrec
   INTEGER I , J , L , Ls , Nb
   COMPLEX Sum
   REAL Arb(1) , Avr(1) , Cg(1) , Delx(1) , Ee(1) , Sg(1) , X(1) , Xic(1) , Xlam(1) , Xle(1) , Xte(1) , Yb(1) , Ys(1) , Zb(1) ,     &
      & Zs(1)
   INTEGER Nas(1) , Nasb(1)
!
! Local variable declarations
!
   REAL ax , ax1 , ax2 , ay , ay1 , ay1i , ay2 , ay2i , ayi , az , az1 , az1i , az2 , az2i , azi , beta , cgs , cl , cv , da , dar ,&
      & dcgam , dcgami , dcl , dee , deei , deei2 , deli , delii , delis , delr , delri , delrs , dely , deta , detai , dij , diji ,&
      & dijs , dmuy , dmuz , dsgam , dsgami , dsl , dsqrtl , dtl , dtlami , dxi , dxle , dxs , dxte , dyb , dzb , dzeta , dzetai ,  &
      & eps , es , fl , flnd , flne , m , sdelx , sgs , sl , sqtl , tl , x0 , x0i , y0 , y0i , z0 , z0i
   COMPLEX dp , dpll , dplr , dpul , dpur
   INTEGER igo , infl , ioutfl , na , na1 , na2 , noas , nob , nobi
!
! End of declarations
!
!
!     COMPUTES ELEMENTS OF THE SUBMATRICES  DPP, DPZ  AND  DPY
!     USING  SUBROUTINES  SNPDF, INCRO AND SUBI
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
 100  DO
!
      nobi = 1
      na2 = 0
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
                  CALL subi(da,dzb,dyb,dar,deta,dzeta,dcgam,dsgam,dee,dxi,tl,detai,dzetai,dcgami,dsgami,deei,dtlami,dmuy,dmuz,infl, &
                          & ioutfl)
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
                     IF ( Kr>eps ) THEN
                        delr = 0.0
                        deli = 0.0
                        ayi = y0i
                        azi = z0i
                        ay1i = ayi - deei*dcgami
                        az1i = azi - deei*dsgami
                        ay2i = ayi + deei*dcgami
                        az2i = azi + deei*dsgami
                        deei2 = 2.0*deei
                        CALL incro(ax,ayi,azi,ax1,ay1i,az1i,ax2,ay2i,az2i,Sgr,Cgr,dsgami,dcgami,Kr,fl,beta,sdelx,deei2,delr,deli)
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
99999 END SUBROUTINE subpb
