
SUBROUTINE dpps(Ks,I,J1,J2,Sgr,Cgr,Ys,Zs,Nbaray,Ncaray,Dt,Work)
   IMPLICIT NONE
   REAL F
   INTEGER Icg , Icore , Idelx , Idt , Iee , Inb , Inc , Isg , Ixic , Ixlam , Iys , Izs , Length , Next , Njj , Np , Nstrip , Ntp
   COMMON /dlcom / Np , Nstrip , Ntp , F , Njj , Next , Length , Inc , Inb , Iys , Izs , Iee , Isg , Icg , Ixic , Idelx , Ixlam ,   &
                 & Idt , Icore
   REAL Cgr , Sgr
   INTEGER I , J1 , J2 , Ks
   COMPLEX Dt(1)
   INTEGER Nbaray(1) , Ncaray(1)
   REAL Work(1) , Ys(1) , Zs(1)
   INTEGER j , l , ls , nbcum , nbxs , nc1
   COMPLEX sum
   REAL yrec , zrec
!   ***   GENERATES ROWS OF THE  DPP  SUBMATRIX USING
!         SUBROUTINE  SUBP
   l = 1
!  L IS THE PANEL NUMBER ASSOCIATED WITH SENDING   POINT  J
   ls = 1
!  LS IS THE STRIP NUMBER ASSOCIATED WITH SENDING   POINT  J
   nbxs = Nbaray(l)
   nc1 = Ncaray(l)
   nbcum = nc1
   yrec = Ys(Ks)
   zrec = Zs(Ks)
   DO j = J1 , J2
      CALL subp(I,l,ls,j,Sgr,Cgr,yrec,zrec,sum,Work(Ixic),Work(Idelx),Work(Iee),Work(Ixlam),Work(Isg),Work(Icg),Ys,Zs)
      Dt(j) = sum
      IF ( j/=J2 ) THEN
         IF ( j>=nbxs ) THEN
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
END SUBROUTINE dpps