
SUBROUTINE dpzy(Kb,Iz,I,J1,J2,Ifirst,Ilast,Yb,Zb,Avr,Arb,Th1a,Th2a,Nt121,Nt122,Nbaray,Ncaray,Nzykb,Dpz,Dpy)
   IMPLICIT NONE
   REAL Ecore , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   INTEGER Ia , Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc ,   &
         & Infl , Ins , Insbea , Int121 , Int122 , Iria , Isg , Ith1a , Ith2a , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam , Ixle , Ixte , &
         & Iyb , Iyin , Iys , Izb , Izin , Izs , Nb , Nby , Nbz , Next , Nj1 , Nk1 , Np , Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs , Z(1)
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ia , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /zzzzzz/ Z
   INTEGER I , Ifirst , Ilast , Iz , J1 , J2 , Kb , Nzykb
   REAL Arb(1) , Avr(1) , Th1a(1) , Th2a(1) , Yb(1) , Zb(1)
   COMPLEX Dpy(1) , Dpz(1)
   INTEGER Nbaray(1) , Ncaray(1) , Nt121(1) , Nt122(1)
   REAL cgr , cmult , delth , pi , rho , sgr , smult , theta , thm1 , thp1 , yrec , zrec
   INTEGER ix , ix1 , ix2 , ixm1 , ixp1 , j , ksp , l , ls , nbcum , nbxs , nc1
   COMPLEX sum
!   ***   GENERATES ROWS OF THE SUBMATRICES  DPZ  AND DPY  USING
!         SUBROUTINE  SUBP
   pi = 3.1415926
   ix1 = 1
   Iz = Iz + 1
!  IZ  IS THE BODY-ELEMENT NUMBER FOR BODY  KB  --  IZ RUNS FROM  1
!  THROUGH  NBE-SUB-KB
   ix2 = Nt122(Kb)
   IF ( Iz>=Ifirst .AND. Iz<=Ilast ) ix2 = Nt121(Kb)
   DO ix = ix1 , ix2
      l = 1
      ksp = 0
!  L IS THE PANEL NUMBER ASSOCIATED WITH SENDING   POINT  J
      ls = 1
!  LS IS THE STRIP NUMBER ASSOCIATED WITH SENDING   POINT  J
      nbxs = Nbaray(l)
      nc1 = Ncaray(l)
      nbcum = nc1
      ixp1 = ix + 1
      IF ( ixp1>ix2 ) ixp1 = ix1
      ixm1 = ix - 1
      IF ( ixm1==0 ) ixm1 = ix2
      IF ( Iz>=Ifirst .AND. Iz<=Ilast ) THEN
         theta = Th1a(ix)
         thp1 = Th1a(ixp1)
         thm1 = Th1a(ixm1)
      ELSE
         theta = Th2a(ix)
         thp1 = Th2a(ixp1)
         thm1 = Th2a(ixm1)
      ENDIF
      IF ( ix==ix1 ) thm1 = thm1 - 2.0*pi
      IF ( ix==ix2 ) thp1 = thp1 + 2.0*pi
      delth = 0.5*(thp1-thm1)
      yrec = Yb(Kb) + Avr(Kb)*cos(theta)
      zrec = Zb(Kb) + Avr(Kb)*Arb(Kb)*sin(theta)
      rho = sqrt(1.0+(Arb(Kb)**2-1.0)*(cos(theta))**2)
      sgr = -Arb(Kb)*cos(theta)/rho
      cgr = sin(theta)/rho
      smult = sin(theta)*rho/pi
      cmult = cos(theta)*rho/pi
      DO j = J1 , J2
         CALL subpb(I,l,ls,j,sgr,cgr,yrec,zrec,sum,Z(Ixic),Z(Idelx),Z(Iee),Z(Ixlam),Z(Isg),Z(Icg),Z(Iys),Z(Izs),Z(Inas),Z(Inasb+ksp)&
                  & ,Z(Iavr),Z(Izb),Z(Iyb),Z(Iarb),Z(Ixle),Z(Ixte),Z(Ia),Nb)
         IF ( Nzykb==3 ) THEN
            Dpy(j) = Dpy(j) + sum*cmult*delth
         ELSE
            Dpz(j) = Dpz(j) + sum*smult*delth
            IF ( Nzykb/=1 ) Dpy(j) = Dpy(j) + sum*cmult*delth
         ENDIF
         IF ( j/=J2 ) THEN
            IF ( j>=nbxs ) THEN
               ksp = ksp + Z(Inas+l-1)
               l = l + 1
               nc1 = Ncaray(l)
               nbxs = Nbaray(l)
            ENDIF
            IF ( j>=nbcum ) THEN
               ls = ls + 1
               nbcum = nbcum + nc1
            ENDIF
         ENDIF
      ENDDO
   ENDDO
END SUBROUTINE dpzy