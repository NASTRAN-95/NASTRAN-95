!*==mbctr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbctr(Ictr,Il1,Ir1,Ncn,Nc1,Nwn,Nw1,Parea)
   USE c_mboxa
   USE c_mboxc
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ictr
   INTEGER :: Il1
   INTEGER :: Ir1
   INTEGER , DIMENSION(1) :: Ncn
   INTEGER , DIMENSION(1) :: Nc1
   INTEGER , DIMENSION(1) :: Nwn
   INTEGER , DIMENSION(1) :: Nw1
   REAL , DIMENSION(50,50,3) :: Parea
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(5) :: cotang , tang , x , y
   INTEGER :: i , j , jb , jt
   REAL :: pa , xb , xf , xf1 , xf2 , xlh , xll , xll1 , xlr , xlt , xrh , xrl , xrr , xrt , xt , ybh , ybl , ybr , ybt , yc , yl , &
         & yl1 , yr , yth , ytl , ytr , ytt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     CONTROL1 SURFACE
!
!     CONVEX ONLY:
!     MUST COMPILE WITH O1 OR LOWER OPTIMIZATION OPTION. IF O2 IS USED,
!     THE COMPILER WOULD GO INTO INFINITE LOOP.
!
!
   IF ( Ictr==2 ) THEN
!
!     CONTROL2 SURFACE
!
      x(1) = xx(11)
      x(2) = xx(9)
      x(3) = xx(10)
      x(4) = xx(12)
      y(1) = yy(11)
      y(2) = yy(9)
      y(3) = yy(10)
      y(4) = yy(12)
      tang(1) = tg(8)
      tang(2) = tg(10)
      tang(3) = tg(9)
      cotang(1) = cotg(8)
      cotang(2) = cotg(10)
      cotang(3) = cotg(9)
   ELSE
      x(1) = xx(7)
      x(2) = xx(8)
      x(3) = xx(9)
      x(4) = xx(11)
      y(1) = yy(7)
      y(2) = yy(8)
      y(3) = yy(9)
      y(4) = yy(11)
      tang(1) = tg(6)
      tang(2) = tg(7)
      tang(3) = tg(8)
      cotang(1) = cotg(6)
      cotang(2) = cotg(7)
      cotang(3) = cotg(8)
   ENDIF
!
   x(5) = xx(5)
   y(5) = yy(5)
   tang(4) = tg(4)
   tang(5) = tg(5)
   cotang(4) = cotg(4)
   cotang(5) = cotg(5)
!
   Il1 = amin1(y(2),y(1))/boxw + 1.5
   Ir1 = amax1(y(3),y(4))/boxw + 1.4999
   DO i = Il1 , Ir1
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            yr = (float(i)-0.5)*boxw
            yl = yr - boxw
!
            xll = (yl-y(2))*tang(1) + x(2)
            xrl = (yr-y(2))*tang(1) + x(2)
            xlh = (yl-y(2))*tang(2) + x(2)
            xrh = (yr-y(2))*tang(2) + x(2)
            xlr = (yl-y(3))*tang(3) + x(3)
            xrr = (yr-y(3))*tang(3) + x(3)
!
            IF ( crank2 .AND. y(5)<=y(1) ) THEN
!
               xlt = (yl-y(1))*tang(5) + x(1)
               xrt = (yr-y(1))*tang(5) + x(1)
            ELSE
!
               xlt = (yl-y(1))*tang(4) + x(1)
               xrt = (yr-y(1))*tang(4) + x(1)
            ENDIF
!
            IF ( yl<=y(2) .AND. yr>=y(2) ) THEN
!
               jt = (x(2)-amod(x(2),boxl)+boxl)/boxl + 0.01
!
            ELSEIF ( yr<y(2) ) THEN
!
               jt = (xrl-amod(xrl,boxl)+boxl)/boxl + 0.01
            ELSE
               jt = (xlh-amod(xlh,boxl)+boxl)/boxl + 0.01
            ENDIF
!
            IF ( yl<y(4) .AND. yr>=y(4) .AND. xrt>=xlt ) THEN
               jb = (x(4)-amod(x(4),boxl)+boxl)/boxl + 0.01
            ELSEIF ( yl>=y(4) ) THEN
!
               jb = (xlr-amod(xlr,boxl)+boxl)/boxl + 0.01
            ELSE
!
               jb = (amax1(xlt,xrt)-amod(amax1(xlt,xrt),boxl)+boxl)/boxl + 0.01
            ENDIF
!
            DO j = jt , jb
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
!
                     xb = float(j)*boxl
                     xt = xb - boxl
!
                     ytl = (xt-x(2))*cotang(1) + y(2)
                     ybl = (xb-x(2))*cotang(1) + y(2)
                     yth = (xt-x(2))*cotang(2) + y(2)
                     ybh = (xb-x(2))*cotang(2) + y(2)
                     ytr = (xt-x(3))*cotang(3) + y(3)
                     ybr = (xb-x(3))*cotang(3) + y(3)
!
                     IF ( crank2 .AND. y(5)<=y(1) ) THEN
!
                        ytt = (xt-x(1))*cotang(5) + y(1)
                        ybt = (xb-x(1))*cotang(5) + y(1)
                     ELSE
!
                        ytt = (xt-x(1))*cotang(4) + y(1)
                        ybt = (xb-x(1))*cotang(4) + y(1)
                     ENDIF
!
!     FULL BOXES
!
                     IF ( yl>=ytl .AND. xt>=xrh .AND. yr<ybr .AND. xb<xrt .AND. xb<xlt ) THEN
!
                        Parea(j,i,2) = 1.0
                        Parea(j,i,1) = 0.0
                     ELSE
!
!     DOUBLE CORNER BOXES
!
                        IF ( yl<=y(2) .AND. yr>=y(2) .AND. xt<x(2) .AND. xb>=x(2) .AND. yl<=y(1) .AND. yr>=y(1) .AND. xt<x(1) .AND. &
                           & xb>=x(1) ) THEN
!
!     LH CORNERS
!
                           IF ( yl<=yth .AND. yr>=yth .AND. yl<=ybt .AND. yr>=ybt )                                                 &
                              & pa = 0.5*((2.0*yr-yth-y(2))*(x(2)-xt)+(2.0*yr-y(2)-y(1))*(x(1)-x(2))+(2.0*yr-y(1)-ybt)*(xb-x(1)))   &
                              & /boxa
                           IF ( xt<xrh .AND. xb>=xrh .AND. yl<=ybt .AND. yr>=ybt ) pa = 0.5*((x(2)-xrh)*(yr-y(2))+(2.0*yr-y(2)-y(1))&
                              & *(x(1)-x(2))+(2.0*yr-y(1)-ybt)*(xb-x(1)))/boxa
                           IF ( yl<=yth .AND. yr>=yth .AND. xt<xrt .AND. xb>=xrt )                                                  &
                              & pa = 0.5*((2.0*yr-yth-y(2))*(x(2)-xt)+(2.0*yr-y(2)-y(1))*(x(1)-x(2))+(xrt-x(1))*(yr-y(1)))/boxa
                           IF ( xt<xrh .AND. xb>=xrh .AND. xt<xrt .AND. xb>=xrt ) pa = 0.5*((x(2)-xrh)*(yr-y(2))+(2.0*yr-y(2)-y(1)) &
                              & *(x(1)-x(2))+(xrt-x(1))*(yr-y(1)))/boxa
                           IF ( i==1 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
!
                        ELSEIF ( yl<y(3) .AND. yr>=y(3) .AND. xt<x(3) .AND. xb>=x(3) .AND. yl<y(4) .AND. yr>=y(4) .AND. xt<x(4)     &
                               & .AND. xb>=x(4) ) THEN
!
!     RH CORNERS
!
                           IF ( yl<yth .AND. yr>=yth .AND. yl<ybt .AND. yr>=ybt )                                                   &
                              & pa = 0.5*((yth+y(3)-2.0*yl)*(x(3)-xt)+(y(3)+y(4)-2.0*yl)*(x(4)-x(3))+(y(4)+ybt-2.0*yl)*(xb-x(4)))   &
                              & /boxa
                           IF ( xt<xlh .AND. xb>=xlh .AND. yl<ybt .AND. yr>=ybt ) pa = 0.5*((x(3)-xlh)*(y(3)-yl)+(y(3)+y(4)-2.0*yl) &
                              & *(x(4)-x(3))+(y(4)+ybt-2.0*yl)*(xb-x(4)))/boxa
                           IF ( yl<yth .AND. yr>=yth .AND. xt<xlt .AND. xb>=xlt )                                                   &
                              & pa = 0.5*((yth+y(3)-2.0*yl)*(x(3)-xt)+(y(3)+y(4)-2.0*yl)*(x(4)-x(3))+(xlt-x(4))*(y(4)-yl))/boxa
                           IF ( xt<xlh .AND. xb>=xlh .AND. xt<xlt .AND. xb>=xlt ) pa = 0.5*((x(3)-xlh)*(y(3)-yl)+(y(3)+y(4)-2.0*yl) &
                              & *(x(4)-x(3))+(xlt-x(4))*(y(4)-yl))/boxa
!
!     SINGLE CORNER BOXES
!
                        ELSEIF ( yl<=y(2) .AND. yr>=y(2) .AND. xt<x(2) .AND. xb>=x(2) ) THEN
!
!     FWD LH CORNER
!
                           IF ( yl<=ybl .AND. yr>=ybl .AND. xt<xrh .AND. xb>=xrh ) pa = .5*((y(2)-ybl)*(xb-x(2))+(2.*xb-x(2)-xrh)   &
                              & *(yr-y(2)))/boxa
                           IF ( xt<xll .AND. xb>=xll .AND. xt<xrh .AND. xb>=xrh )                                                   &
                              & pa = .5*((2.*xb-xll-x(2))*(y(2)-yl)+(2.*xb-xrh-x(2))*(yr-y(2)))/boxa
                           IF ( xt<xll .AND. xb>=xll .AND. yl<ybh .AND. yr>=ybh ) pa = .5*((2.*xb-x(2)-xll)*(y(2)-yl)+(ybh-y(2))    &
                              & *(xb-x(2)))/boxa
                           IF ( yl<=ybl .AND. yr>=ybl .AND. yl<ybh .AND. yr>=ybh ) pa = 0.5*(xb-x(2))*(ybh-ybl)/boxa
                           IF ( i==1 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ELSEIF ( yl<=y(1) .AND. yr>=y(1) .AND. xt<x(1) .AND. xb>=x(1) ) THEN
!
!     AFT LH CORNER
!
                           IF ( x(1)<x(4) ) THEN
                              IF ( yl<ybt .AND. yr>=ybt .AND. yl<=ytl .AND. yr>=ytl )                                               &
                                 & pa = .5*((2.*yr-ytl-y(1))*(x(1)-xt)+(2.*yr-y(1)-ybt)*(xb-x(1)))/boxa
                           ENDIF
                           IF ( yl<=ytl .AND. yr>=ytl .AND. xt<xrt .AND. xb>=xrt ) pa = .5*((y(1)-ytl)*(x(1)-xt)+(x(1)+xrt-2.*xt)   &
                              & *(yr-y(1)))/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. yl<=ytl .AND. yr>=ytl .AND. ytt>=ytl ) pa = 0.5*(ytt-ytl)*(x(1)-xt)/boxa
                           IF ( xt<xrl .AND. xb>=xrl .AND. xt<xrt .AND. xb>=xrt ) pa = 0.5*(xrt-xrl)*(yr-y(1))/boxa
                           IF ( yl<ybt .AND. yr>=ybt .AND. xt<xrl .AND. xb>=xrl ) pa = .5*((x(1)-xrl)*(yr-y(1))+(2.*yr-y(1)-ybt)    &
                              & *(xb-x(1)))/boxa
                           IF ( i==1 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ELSEIF ( yl<y(3) .AND. yr>=y(3) .AND. xt<x(3) .AND. xb>=x(3) ) THEN
!
!     FWD RH CORNER
!
                           IF ( yr>=ybr .AND. yl<ybr .AND. xt<xlh .AND. xb>=xlh ) pa = 0.5*((x(3)-xlh)*(y(3)-yl)+(y(3)+ybr-2.0*yl)  &
                              & *(xb-x(3)))/boxa
                           IF ( xt<xlh .AND. xb>=xlh .AND. xt<xlr .AND. xb>=xlr ) pa = 0.5*(xlr-xlh)*(y(3)-yl)/boxa
                           IF ( yr>=yth .AND. yl<yth .AND. xt<xlr .AND. xb>=xlr ) pa = .5*((yth+y(3)-2.*yl)*(x(3)-xt)+(y(3)-yl)     &
                              & *(xlr-x(3)))/boxa
                           IF ( yr>=yth .AND. yl<yth .AND. yr>=ybr .AND. yl<ybr )                                                   &
                              & pa = 0.5*((yth+y(3)-2.0*yl)*(x(3)-xt)+(y(3)+ybr-2.0*yl)*(xb-x(3)))/boxa
                        ELSEIF ( yl<y(4) .AND. yr>=y(4) .AND. xt<x(4) .AND. xb>=x(4) ) THEN
!
!     AFT RH CORNER
!
                           IF ( x(4)<x(1) ) THEN
                              IF ( yr>=ytr .AND. yl<ytr .AND. yl<ybt .AND. yr>=ybt )                                                &
                                 & pa = 0.5*((ytr+y(4)-2.0*yl)*(x(4)-xt)+(y(4)+ybt-2.0*yl)*(xb-x(4)))/boxa
                           ENDIF
                           IF ( yr>=ytr .AND. yl<ytr .AND. xt<xlt .AND. xb>=xlt ) pa = .5*((xlt-x(4))*(y(4)-yl)+(ytr+y(4)-2.*yl)    &
                              & *(x(4)-xt))/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. yr>=ytr .AND. yl<ytr ) pa = 0.5*(ytr-ytt)*(x(4)-xt)/boxa
                           IF ( xt<xlt .AND. xb>=xlt .AND. xt<xrr .AND. xb>=xrr )                                                   &
                              & pa = 0.5*((xlt+x(4)-2.0*xt)*(y(4)-yl)+(x(4)+xrr-2.0*xt)*(yr-y(4)))/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. xt<xrr .AND. xb>=xrr ) pa = .5*((y(4)-ytt)*(x(4)-xt)+(x(4)+xrr-2.*xt)    &
                              & *(yr-y(4)))/boxa
!
!     HINGE + T. E. BOXES
!
                        ELSEIF ( xt<xrh .AND. (xb>=xlt .OR. xb>=xrt) ) THEN
!
                           IF ( xb>=xlt .AND. yr>=ybt .AND. yl<yth ) pa = 1.0 - 0.5*((yr-yth)*(xrh-xt)+(xb-xlt)*(ybt-yl))/boxa
                           IF ( xb>=xrt .AND. yl<ybt .AND. yl<yth ) pa = 1.0 - 0.5*((yr-yth)*(xrh-xt)+(xb-xrt)*(yr-ybt))/boxa
                           IF ( xt<xlh .AND. xb>=xlt .AND. xb>=xrt ) pa = 0.5*(xrt-xrh+xlt-xlh)/boxl
                           IF ( xt<xlh .AND. yl<ybt .AND. xb>=xrt ) pa = 1.0 - 0.5*((xlh+xrh-2.0*xt)*boxw+(xb-xrt)*(yr-ybt))/boxa
                           IF ( yl<yth .AND. xb>=xlt .AND. xb>=xrt ) pa = 1.0 - 0.5*((2.0*xb-xlt-xrt)*boxw+(xrh-xt)*(yr-yth))/boxa
                           IF ( xt<xlh .AND. yr>=ybt .AND. xb>=xlt ) pa = 1.0 - 0.5*((xlh+xrh-2.0*xt)*boxw+(xb-xlt)*(ybt-yl))/boxa
!
!     SIDE BOXES
!
                        ELSEIF ( xb>=xlh .AND. xt<xrh .AND. yl>=ytl .AND. (xb<x(3) .OR. yr<y(3)) ) THEN
!
!     HINGE LINE
!
                           IF ( yl<yth .AND. yr>=yth .AND. xt<xrh .AND. xb>=xrh ) pa = 1.0 - 0.5*(yr-yth)*(xrh-xt)/boxa
                           IF ( xt<xlh .AND. xb>=xlh .AND. xt<xrh .AND. xb>=xrh ) pa = 0.5*(2.0*xb-xlh-xrh)/boxl
                           IF ( yl<ybh .AND. yr>=ybh .AND. xt<xlh .AND. xb>=xlh ) pa = 0.5*(xb-xlh)*(ybh-yl)/boxa
                           IF ( yl<yth .AND. yr>=yth .AND. yl<ybh .AND. yr>=ybh ) pa = 0.5*(yth+ybh-2.0*yl)/boxw
!
                           IF ( yr>=ybr .AND. yl<ybr .AND. xt<xrr .AND. xb>=xrr .AND. xt<xrh .AND. xb>=xrh .AND. yl<yth .AND.       &
                              & yr>=yth ) pa = 1.0 - 0.5*((yr-yth)*(xrh-xt)+(xb-xrr)*(yr-ybr))/boxa
                           IF ( yr>=ybr .AND. yl<ybr .AND. xt<xrr .AND. xb>=xrr .AND. xt<xrh .AND. xb>=xrh .AND. xt<xlh .AND.       &
                              & xb>=xlh ) pa = 0.5*((2.0*xb-xlh-xrh)*boxw-(yr-ybr)*(xb-xrr))/boxa
                           IF ( yl<yth .AND. yr>=yth .AND. xt<xrh .AND. xb>=xrh .AND. xt<xrr .AND. xb>=xrr .AND. xt<xlr .AND.       &
                              & xb>=xlr ) pa = 0.5*((xlr+xrr-2.0*xt)*boxw-(yr-yth)*(xrh-xt))/boxa
                           IF ( xt<xlh .AND. xb>=xlh .AND. xt<xlr .AND. xb>=xlr .AND. xt<xrh .AND. xb>=xrh .AND. xt<xrr .AND.       &
                              & xb>=xrr ) pa = 0.5*(xlr+xrr-xlh-xrh)/boxl
                        ELSEIF ( yl<=ytl .AND. yr>=ybl .AND. xb>=x(2) .AND. xt<x(1) ) THEN
!
!     LH EDGE
!
                           IF ( i==1 ) THEN
!
                              yl1 = 0.0
                              xll1 = (yl1-y(2))*tang(1) + x(2)
!
                              IF ( xt<xll1 .AND. xb>=xll1 .AND. xt<xrl .AND. xb>=xrl ) pa = 0.5*(2.0*xb-xll1-xrl)*(yr-yl1)/boxa
                              IF ( yl1<=ybl .AND. yr>=ybl .AND. xt<xrl .AND. xb>=xrl ) pa = 0.5*(xb-xrl)*(yr-ybl)/boxa
                              IF ( yl1<=ytl .AND. yr>=ytl .AND. xt<xll .AND. xb>=xll ) pa = 1.0 - 0.5*(ytl-yl1)*(xll1-xt)/boxa
                              IF ( yl1<=ytl .AND. yr>=ytl .AND. yl1<=ybl .AND. yr>=ybl ) pa = 0.5*(2.0*yr-ytl-ybl)/boxw
                           ELSE
!
                              IF ( xt<xll .AND. xb>=xll .AND. xt<xrl .AND. xb>=xrl ) pa = 0.5*(2.0*xb-xll-xrl)/boxl
                              IF ( yl<=ybl .AND. yr>=ybl .AND. xt<xrl .AND. xb>=xrl ) pa = 0.5*(yr-ybl)*(xb-xrl)/boxa
                              IF ( yl<=ytl .AND. yr>=ytl .AND. yl<=ybl .AND. yr>=ybl ) pa = 0.5*(2.0*yr-ytl-ybl)/boxw
                              IF ( yl<=ytl .AND. yr>=ytl .AND. xt<xll .AND. xb>=xll ) pa = 1.0 - 0.5*(xll-xt)*(ytl-yl)/boxa
                           ENDIF
!
                           IF ( yl<=ytl .AND. yr>=ytl .AND. yl<yth .AND. yr>=yth .AND. xt<xll .AND. xb>=xll .AND. xt<xrh .AND.      &
                              & xb>=xrh ) pa = 1.0 - 0.5*((xll-xt)*(ytl-yl)+(yr-yth)*(xrh-xt))/boxa
                           IF ( yl<=ybl .AND. yr>=ybl .AND. yl<=ytl .AND. yr>=ytl .AND. yl<yth .AND. yr>=yth .AND. xt<xrh .AND.     &
                              & xb>=xrh ) pa = 1.0 - 0.5*((ytl+ybl-2.0*yl)*boxl+(yr-yth)*(xrh-xt))/boxa
                           IF ( yl<ybh .AND. yr>=ybh .AND. xt<xll .AND. xb>=xll .AND. yl<=ytl .AND. yr>=ytl .AND. yl<yth .AND.      &
                              & yr>=yth ) pa = 1.0 - 0.5*((ytl-yl)*(xll-xt)+(2.0*yr-yth-ybh)*boxl)/boxa
                           IF ( yl<=ytl .AND. yr>=ytl .AND. yl<yth .AND. yr>=yth .AND. yl<=ybl .AND. yr>=ybl .AND. yl<ybh .AND.     &
                              & yr>=ybh ) pa = 0.5*(yth+ybh-ytl-ybl)/boxw
                           IF ( yl<=ytl .AND. yr>=ytl .AND. yl<=ybl .AND. yr>=ybl .AND. yl<ybt .AND. yr>=ybt .AND. xt<xrt .AND.     &
                              & xb>=xrt ) pa = 0.5*((2.0*yr-ytl-ybl)*boxl-(yr-ybt)*(xb-xrt))/boxa
                           IF ( yl<ytl .AND. yr>=ytl .AND. xt<xll .AND. xb>=xll .AND. xt<xlt .AND. xb>=xlt .AND. yl<ybt .AND.       &
                              & yr>=ybt ) pa = 1.0 - 0.5*((ytl-yl)*(xll-xt)+(xb-xlt)*(ybt-yl))/boxa
                           IF ( xt<xll .AND. xb>=xll .AND. xt<xrl .AND. xb>=xrl .AND. yl<ybt .AND. yr>=ybt .AND. xt<xlt .AND.       &
                              & xb>=xlt ) pa = 0.5*((2.0*xb-xll-xrl)*boxw-(xb-xlt)*(ybt-yl))/boxa
                           IF ( yl<ytl .AND. yr>=ytl .AND. xt<xll .AND. xb>=xll .AND. xt<xlt .AND. xb>=xlt .AND. xt<xrt .AND.       &
                              & xb>=xrt ) pa = 0.5*((xlt+xrt-2.0*xb)*boxw-(ytl-yl)*(xll-xt))/boxa
                           IF ( xt<xll .AND. xb>=xll .AND. xt<xlt .AND. xb>=xlt .AND. xt<xrl .AND. xb>=xrl .AND. xt<xrt .AND.       &
                              & xb>=xrt ) pa = 0.5*(xlt+xrt-xll-xrl)/boxl
                           IF ( i==1 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ELSEIF ( yl<ytr .AND. yr>=ybr .AND. xb>=x(3) .AND. xt<x(4) .AND. yr>=y(4) ) THEN
!
!     RH EDGE
!
                           IF ( xt<xrr .AND. xb>=xrr .AND. xt<xlr .AND. xb>=xlr ) pa = 0.5*(xlr+xrr-2.0*xt)/boxl
                           IF ( yr>=ytr .AND. yl<ytr .AND. xt<xlr .AND. xb>=xlr ) pa = 0.5*(ytr-yl)*(xlr-xt)/boxa
                           IF ( yr>=ytr .AND. yl<ytr .AND. yr>=ybr .AND. yl<ybr ) pa = 0.5*(ytr+ybr-2.0*yl)/boxw
                           IF ( yr>=ybr .AND. yl<ybr .AND. xt<xrr .AND. xb>=xrr ) pa = 1.0 - 0.5*(xb-xrr)*(yr-ybr)/boxa
!
                           IF ( xt<xlt .AND. xb>=xlt .AND. yr>=ybr .AND. yl<ybr .AND. yl<ybt .AND. yr>=ybt .AND. xt<xrr .AND.       &
                              & xb>=xrr ) pa = 1.0 - 0.5*((xb-xlt)*(ybt-yl)+(yr-ybr)*(xb-xrr))/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. yl<ybt .AND. yr>=ybt .AND. yr>=ybr .AND. yl<ybr .AND. xt<xrr .AND.       &
                              & xb>=xrr ) pa = 0.5*((2.0*yr-ytt-ybt)*boxl-(yr-ybr)*(xb-xrr))/boxa
                           IF ( yr>=ytr .AND. yl<ytr .AND. yr>=ybr .AND. yl<ybr .AND. xt<xlt .AND. xb>=xlt .AND. yl<ybt .AND.       &
                              & yr>=ybt ) pa = 0.5*((ytr+ybr-2.0*yl)*boxl-(xb-xlt)*(ybt-yl))/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. yr>=ytr .AND. yl<ytr .AND. yl<ybt .AND. yr>=ybt .AND. yr>=ybr .AND.      &
                              & yl<ybr ) pa = 0.5*(ytr-ytt+ybr-ybt)/boxw
                        ELSE
!
!     TRAILING EDGE
!
                           IF ( yl<ytt .AND. yr>=ytt .AND. xt<xrt .AND. xb>=xrt ) pa = 0.5*(yr-ytt)*(xrt-xt)/boxa
                           IF ( xt<xlt .AND. xb>=xlt .AND. xt<xrt .AND. xb>=xrt ) pa = 0.5*(xlt+xrt-2.0*xt)/boxl
                           IF ( xt<xlt .AND. xb>=xlt .AND. yl<ybt .AND. yr>=ybt ) pa = 1.0 - 0.5*(xb-xlt)*(ybt-yl)/boxa
                           IF ( yl<ytt .AND. yr>=ytt .AND. yl<ybt .AND. yr>=ybt ) pa = 0.5*(2.0*yr-ytt-ybt)/boxw
                           IF ( yl<ybt .AND. yr>=ybt .AND. xt<xrt .AND. xb>=xrt ) pa = 1.0 - 0.5*(yr-ybt)*(xb-xrt)/boxa
                           IF ( xt<xlt .AND. xb>=xlt .AND. yl<ytt .AND. yr>=ytt ) pa = 0.5*(xlt-xt)*(ytt-yl)/boxa
                        ENDIF
!
                        Parea(j,i,2) = pa
                        Parea(j,i,1) = Parea(j,i,1) - pa
                     ENDIF
                  CASE (2)
!
                     Parea(j,i,2) = 2.0*pa
                     Parea(j,i,1) = Parea(j,i,1) - Parea(j,i,2)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
            ENDDO
!
            yc = yr - boxw/2.0
            xf = (yl-y(2))*tang(2) + x(2)
!
            IF ( yc<y(2) ) THEN
               IF ( yc<y(1) ) CYCLE
               xf1 = (yr-y(2))*tang(1) + x(2)
               Nc1(i) = xf1/boxl + 1.0
            ELSEIF ( yc<y(3) ) THEN
               Nc1(i) = xf/boxl + 1.0
               IF ( yc<y(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               IF ( yc>=y(4) ) CYCLE
               xf2 = (yr-y(3))*tang(3) + x(3)
               Nc1(i) = xf2/boxl + 1.0
               IF ( yc<y(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Ncn(i) = Nwn(i)
               CYCLE
            ENDIF
            Nc1(i) = max0(Nc1(i),Nw1(i))
            IF ( yc<y(4) ) THEN
               Ncn(i) = Nwn(i)
            ELSE
               xf2 = (yr-y(3))*tang(3) + x(3)
               Ncn(i) = xf2/boxl + 1.0
            ENDIF
         CASE (2)
            xf1 = (yr-y(2))*tang(1) + x(2)
            Ncn(i) = xf1/boxl + 1.0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
END SUBROUTINE mbctr
