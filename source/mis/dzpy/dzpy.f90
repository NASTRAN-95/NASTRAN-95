!*==dzpy.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dzpy(Kb,Ks,Ls,I,J1,J2,Nyflag,Sgr,Cgr,Fmach,Arb,Nbea,Dt)
   IMPLICIT NONE
   USE C_DLBDY
   USE C_ZZZZZZ
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
!   ***   GENERATES ROWS OF THE SUBMATRICES  DZP, DZZ  AND  DZY
!         USING  SUBROUTINE  SUBB
   ndy = 0
   nyfl = Nyflag
   pi = 3.1415926
   eps = 0.00001
   beta = sqrt(1.0-Fmach**2)
   jz = 0
   lb = 1
   jb = 0
   ar = Arb(lb)
!  LS IS THE INDEX OF THE  Y  AND  Z  COORDINATES OF SENDING POINT  J --
!  LS RUNS FROM NSTRIP+1  THROUGH  NSTRIP+NBZ
   DO j = J1 , J2
      jb = jb + 1
!  JB  IS THE BODY-ELEMENT NUMBER IN BODY  LB  --  JB  RUNS FROM  1
!  THROUGH  NTZ
      jz = jz + 1
!  JZ  RUNS FROM 1  THROUGH  NBE-SUB-LB
      CALL subb(Kb,Ks,I,j,jb,lb,Ls,ndy,nyfl,pi,eps,Sgr,Cgr,ar,beta,sum,Z(Iria),Z(Idelx),Z(Iyb),Z(Izb),Z(Iys),Z(Izs),Z(Ix))
      Dt(j) = sum
      IF ( jz==Nbea(lb) ) THEN
         jz = 0
         lb = lb + 1
         Ls = Ls + 1
         ar = Arb(lb)
      ENDIF
   ENDDO
END SUBROUTINE dzpy
