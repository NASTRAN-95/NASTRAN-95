
SUBROUTINE gend(Ncaray,Nbaray,Ys,Zs,Sg,Cg,Dt,Work,Matout)
   IMPLICIT NONE
   REAL F , Fmach , Refc , Rfk
   INTEGER Icg , Icore , Idelx , Idt , Iee , Inb , Inc , Isg , Ixic , Ixlam , Iys , Izs , Length , Mcb(7) , Nd , Ne , Next , Njj ,  &
         & Np , Nrow , Nstrip , Ntp
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk
   COMMON /dlcom / Np , Nstrip , Ntp , F , Njj , Next , Length , Inc , Inb , Iys , Izs , Iee , Isg , Icg , Ixic , Idelx , Ixlam ,   &
                 & Idt , Icore
   INTEGER Matout
   REAL Cg(1) , Sg(1) , Work(1) , Ys(1) , Zs(1)
   COMPLEX Dt(1)
   INTEGER Nbaray(1) , Ncaray(1)
   REAL cgr , sgr
   INTEGER i , i1 , i2 , idtpt , j1 , j2 , k , ks , nbxr
!  GENERATE THE INFLUENCE COEFFICIENT MATRIX ADPP
   i1 = 1
   i2 = Ntp
   j1 = 1
   j2 = Ntp
!
!     POSITION IN DT TO START OF THIS PART OF MATRIX
!
   idtpt = i1 + Nrow
   DO i = i1 , Njj
      Dt(i) = (0.0,0.0)
   ENDDO
!     DPP LOOP
   k = 1
!     K IS THE PANEL NUMBER
   ks = 1
!     KS IS THE STRIP NUMBER
   nbxr = Ncaray(k)
   DO i = i1 , i2
      sgr = Sg(ks)
      cgr = Cg(ks)
      CALL dpps(ks,i,j1,j2,sgr,cgr,Ys,Zs,Nbaray,Ncaray,Dt(idtpt),Work)
      CALL pack(Dt,Matout,Mcb)
      IF ( i/=i2 ) THEN
         IF ( i==Nbaray(k) ) k = k + 1
         IF ( i==nbxr ) THEN
            ks = ks + 1
            nbxr = nbxr + Ncaray(k)
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE gend
