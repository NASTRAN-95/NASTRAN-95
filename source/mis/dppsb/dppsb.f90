!*==dppsb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dppsb(Ks,I,J1,J2,Sgr,Cgr,Ys,Zs,Nbaray,Ncaray,Dt,Z)
   USE c_dlbdy
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ks
   INTEGER :: I
   INTEGER :: J1
   INTEGER :: J2
   REAL :: Sgr
   REAL :: Cgr
   REAL , DIMENSION(1) :: Ys
   REAL , DIMENSION(1) :: Zs
   INTEGER , DIMENSION(1) :: Nbaray
   INTEGER , DIMENSION(1) :: Ncaray
   COMPLEX , DIMENSION(1) :: Dt
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: j , l , ls , lsp , nbcum , nbxs , nc1
   COMPLEX :: sum
   REAL :: yrec , zrec
   EXTERNAL subpb
!
! End of declarations rewritten by SPAG
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
      CALL subpb(I,l,ls,j,Sgr,Cgr,yrec,zrec,sum,Z(ixic),Z(idelx),Z(iee),Z(ixlam),Z(isg),Z(icg),Z(iys),Z(izs),Z(inas),Z(inasb+lsp),  &
               & Z(iavr),Z(izb),Z(iyb),Z(iarb),Z(ixle),Z(ixte),Z(ix),nb)
      Dt(j) = sum
      IF ( j/=J2 ) THEN
         IF ( j>=nbxs ) THEN
            lsp = lsp + Z(inas+l-1)
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
