!*==dypz.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dypz(Kb,Ks,Ls,I,J1,J2,Nyflag,Sgr,Cgr,Fmach,Arb,Nbea,Lbo,Lso,Jbo,Dt)
   USE c_dlbdy
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kb
   INTEGER :: Ks
   INTEGER :: Ls
   INTEGER :: I
   INTEGER :: J1
   INTEGER :: J2
   INTEGER :: Nyflag
   REAL :: Sgr
   REAL :: Cgr
   REAL :: Fmach
   REAL , DIMENSION(1) :: Arb
   INTEGER , DIMENSION(1) :: Nbea
   INTEGER :: Lbo
   INTEGER :: Lso
   INTEGER :: Jbo
   COMPLEX , DIMENSION(1) :: Dt
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ar , beta , eps , pi
   INTEGER :: j , jb , jz , lb , ndy , nyfl
   COMPLEX :: sum
   EXTERNAL subb
!
! End of declarations rewritten by SPAG
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
      CALL subb(Kb,Ks,I,j,jb,lb,Ls,ndy,nyfl,pi,eps,Sgr,Cgr,ar,beta,sum,z(iria),z(idelx),z(iyb),z(izb),z(iys),z(izs),z(ix))
      Dt(j) = sum
      IF ( jz==Nbea(lb) ) THEN
         jz = 0
         lb = lb + 1
         Ls = Ls + 1
         ar = Arb(lb)
      ENDIF
   ENDDO
END SUBROUTINE dypz
