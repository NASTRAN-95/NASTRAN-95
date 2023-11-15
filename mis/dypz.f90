
SUBROUTINE dypz(Kb,Ks,Ls,I,J1,J2,Nyflag,Sgr,Cgr,Fmach,Arb,Nbea,Lbo,Lso,Jbo,Dt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Ecore , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Z(1)
   INTEGER Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc , Infl , &
         & Ins , Insbea , Int121 , Int122 , Iria , Isg , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam , Ixle , Ixte ,   &
         & Iyb , Iyin , Iys , Izb , Izin , Izs , Nb , Nby , Nbz , Next , Nj1 , Nk1 , Np , Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   REAL Cgr , Fmach , Sgr
   INTEGER I , J1 , J2 , Jbo , Kb , Ks , Lbo , Ls , Lso , Nyflag
   REAL Arb(1)
   COMPLEX Dt(1)
   INTEGER Nbea(1)
!
! Local variable declarations
!
   REAL ar , beta , eps , pi
   INTEGER j , jb , jz , lb , ndy , nyfl
   COMPLEX sum
!
! End of declarations
!
!   ***   GENERATES ROWS OF THE SUBMATRICES  DYP, DYZ  AND DYY
!         USING  SUBROUTINE  SUBB
   ndy = 1
   nyfl = Nyflag
   pi = 3.1415926
   eps = 0.00001
   beta = sqrt(1.0-Fmach**2)
   jz = 0
   lb = Lbo
!  LB  IS THE BODY NUMBER ASSOCIATED WITH SENDING POINT  J
   Ls = Lso
!  LS IS THE INDEX OF THE  Y  AND  Z  COORDINATES OF SENDING POINT  J --
!  LS RUNS FROM NSTRIP+NB-NBY+1  THROUGH  NSTRIP+NB
   jb = Jbo - 1
   ar = Arb(lb)
   DO j = J1 , J2
      jb = jb + 1
      jz = jz + 1
      CALL subb(Kb,Ks,I,j,jb,lb,Ls,ndy,nyfl,pi,eps,Sgr,Cgr,ar,beta,sum,Z(Iria),Z(Idelx),Z(Iyb),Z(Izb),Z(Iys),Z(Izs),Z(Ix))
      Dt(j) = sum
      IF ( jz==Nbea(lb) ) THEN
         jz = 0
         lb = lb + 1
         Ls = Ls + 1
         ar = Arb(lb)
      ENDIF
   ENDDO
END SUBROUTINE dypz
