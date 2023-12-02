!*==psqad2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE psqad2
   USE c_matin
   USE c_matout
   USE c_pla32c
   USE c_pla32e
   USE c_pla32s
   USE c_pla3es
   USE c_pla3uv
   USE c_plagp
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: esub0 , nu , plaans
   INTEGER :: i , i201 , matid1 , nirof
   INTEGER , DIMENSION(26) :: necpt , necpts
   EXTERNAL invers , mat , mesage , pstq1 , pstq2
!
! End of declarations rewritten by SPAG
!
!  THIS SUBROUTINE IS THE DRIVER FOR THE QUAD2 CALCULATIONS IN
!  PLA3
!
!     ECPT FOR QUAD2
!
!  1  EL.ID
!  2  GRID A
!  3  GRID B
!  4  GRID C
!  5  GRID D
!  6  THETA
!  7  MAT ID
!  8  T
!  9  MS MASS
! 10  CSID 1
! 11  X1
! 12  Y1
! 13  Z1
! 14  CSID 2
! 15  X2
! 16  Y2
! 17  Z2
! 18  CSID 3
! 19  X3
! 20  Y3
! 21  Z3
! 22  CSID 4
! 23  X4
! 24  Y4
! 25  Z4
! 26  TEMP
! 27  EPS0
! 28  EPSS
! 29  ESTAR
! 30  SIGXS
! 31  SIGYS
! 32  SIGXXS
! 33  MXS
! 34  MYS
! 35  MXYS
! 36  VXS
! 37  VYS
! 38  U(A) (6X1)
! 44  U(B) (6X1)
! 50  U(C) (6X1)
! 56  U(D) (6X1)
!
!     ******************************************************************
!
!
!
! SCRATCH BLOCK  325 CELLS
!
!
   !>>>>EQUIVALENCE (Necpt(7),matid1) , (Ecpt(1),Necpt(1)) , (G11,Plaans) , (G13,Nu) , (G11,Esub0) , (Necpts(1),Ecptsa(1)) , (G12,Nirof)
!
! SETUP GP MATRIX FOR PLAMAT
!
   elid = ecpt(1)
   midgp = matid1
   DO i = 1 , 9
      gp(i) = 0.0
   ENDDO
   tau0 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
   IF ( estar/=0.0 ) THEN
      IF ( ipass==1 ) THEN
         matid = matid1
         costh = 1.0
         sinth = 0.0E0
         inflag = 2
!
         CALL mat(ecpt(1))
!
         gp(1) = g11
         gp(2) = g12
         gp(3) = g13
         gp(4) = g12
         gp(5) = g22
         gp(6) = g23
         gp(7) = g13
         gp(8) = g23
         gp(9) = g33
      ELSEIF ( tau0/=0.0 ) THEN
         matid = matid1
         inflag = 1
!
         CALL mat(ecpt(1))
!
         f = 9.0*(esub0-estar)/(4.0*tau0**2*estar)
         sx = (2.0*sigxs-sigys)/3.0
         sy = (2.0*sigys-sigxs)/3.0
         gp(1) = (1.0+sx**2*f)/esub0
         gp(2) = (-nu+sx*sy*f)/esub0
         gp(3) = (2.0*sigxys*sx*f)/esub0
         gp(4) = gp(2)
         gp(5) = (1.0+sy**2*f)/esub0
         gp(6) = (2.0*sigxys*sy*f)/esub0
         gp(7) = gp(3)
         gp(8) = gp(6)
         gp(9) = (2.0*(1.0+nu)+4.0*f*sigxys**2)/esub0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
         idum2 = -1
         CALL invers(3,gp,3,0,0,dum1,idum2,idum3)
!
! CHECK SINGULARITY
!
         IF ( idum2==2 ) CALL mesage(-30,38,ecpt(1))
      ENDIF
   ENDIF
!
! CALCULATE PHASE I STRESSES
!
   DO i = 1 , 32
      ecptsa(i) = ecpt(i)
   ENDDO
   necpts(2) = 1
   necpts(3) = 7
   necpts(4) = 13
   necpts(5) = 19
!
   CALL pstq1(4)
!
!
! CALCULATE PHASE II STRESSES
!
   ivec = 1
   DO i = 1 , 24
      z(i) = ui(i)
   ENDDO
   DO i = 1 , 200
      ecptsa(i) = ph1out(i)
   ENDDO
   s(1) = sigxs
   s(2) = sigys
   s(3) = sigxys
   i201 = 201
   ecptsa(i201) = ecpt(1)
   DO i = 1 , 5
      ecptsa(i+201) = forvec(i)
   ENDDO
!
   CALL pstq2(4)
!
!
!  UPDATE ECPT FOR STRESSES
!
   sigxs = s(1)
   sigys = s(2)
   sigxys = s(3)
!
!     NEW FORCES ARE IN  /PLA3ES/ AT LOCATIONS 202-206
!
   DO i = 1 , 5
      forvec(i) = ecptsa(i+201)
   ENDDO
   tau1 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
   matid = matid1
   inflag = 8
   plaarg = tau1
!
   CALL mat(ecpt(1))
!
!
! TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
!
   IF ( nirof==1 ) THEN
!
      estar = 0.0
      RETURN
   ENDIF
!
! RETURNS EPS SUB 1 GIVEN TAU1
!
   eps1 = plaans
   deps = eps1 - epss
   depss = epss - eps0
   eps2 = eps1 + gamma*deps
   inflag = 6
   plaarg = eps2
!
   CALL mat(ecpt(1))
!
! RETURNS  TAU2 GIVEN EPS2
!
   tau2 = plaans
   estar = 0.0
   IF ( (eps2-eps1)/=0.0 ) estar = (tau2-tau1)/(eps2-eps1)
   eps0 = epss
   epss = eps1
END SUBROUTINE psqad2
