
SUBROUTINE dppsb(Ks,I,J1,J2,Sgr,Cgr,Ys,Zs,Nbaray,Ncaray,Dt,Z)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Ecore , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   INTEGER Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc , Infl , &
         & Ins , Insbea , Int121 , Int122 , Iria , Isg , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam , Ixle , Ixte ,   &
         & Iyb , Iyin , Iys , Izb , Izin , Izs , Nb , Nby , Nbz , Next , Nj1 , Nk1 , Np , Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
!
! Dummy argument declarations
!
   REAL Cgr , Sgr
   INTEGER I , J1 , J2 , Ks
   COMPLEX Dt(1)
   INTEGER Nbaray(1) , Ncaray(1) , Z(1)
   REAL Ys(1) , Zs(1)
!
! Local variable declarations
!
   INTEGER j , l , ls , lsp , nbcum , nbxs , nc1
   COMPLEX sum
   REAL yrec , zrec
!
! End of declarations
!
!   ***   GENERATES ROWS OF THE  DPP  SUBMATRIX USING
!         SUBROUTINE  SUBP
   l = 1
!  L IS THE PANEL NUMBER ASSOCIATED WITH SENDING   POINT  J
   ls = 1
   lsp = 0
!  LS IS THE STRIP NUMBER ASSOCIATED WITH SENDING   POINT  J
   nbxs = Nbaray(l)
   nc1 = Ncaray(l)
   nbcum = nc1
   yrec = Ys(Ks)
   zrec = Zs(Ks)
   DO j = J1 , J2
      CALL subpb(I,l,ls,j,Sgr,Cgr,yrec,zrec,sum,Z(Ixic),Z(Idelx),Z(Iee),Z(Ixlam),Z(Isg),Z(Icg),Z(Iys),Z(Izs),Z(Inas),Z(Inasb+lsp),  &
               & Z(Iavr),Z(Izb),Z(Iyb),Z(Iarb),Z(Ixle),Z(Ixte),Z(Ix),Nb)
      Dt(j) = sum
      IF ( j/=J2 ) THEN
         IF ( j>=nbxs ) THEN
            lsp = lsp + Z(Inas+l-1)
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
END SUBROUTINE dppsb
