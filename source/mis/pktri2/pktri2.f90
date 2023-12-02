!*==pktri2.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pktri2
   USE c_matin
   USE c_matout
   USE c_pla42c
   USE c_pla42e
   USE c_pla42s
   USE c_pla4es
   USE c_pla4uv
   USE c_plagp
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: esub0 , nu , plaans
   INTEGER :: i , matid1 , nirof
   LOGICAL :: istiff
   INTEGER , DIMENSION(21) :: necpt , necpts
   EXTERNAL invers , mat , mesage , pktq1 , pktq2 , pktrqd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!  THIS SUBROUTINE IS THE DRIVER FOR THE  TRIA2 CALCULATIONS IN
!  PLA4
!
!     ECPT FOR  TRIA2
!
!  1  EL.ID
!  2  GRID A
!  3  GRID B
!  4  GRID C
!  5  THETA
!  6  MAT ID
!  7  T
!  8  MS MASS
!  9  CSID 1
! 10  X1
! 11  Y1
! 12  Z1
! 13  CSID 2
! 14  X2
! 15  Y2
! 16  Z2
! 17  CSID 3
! 18  X3
! 19  Y3
! 20  Z3
! 21  TEMP
! 22  EPS0
! 23  EPSS
! 24  ESTAR
! 25  SIGXS
! 26  SIGYS
! 27  SIGXYS
! 28  U(A) (3X1)
! 31  U(B) (3X1)
! 34  U(C) (3X1)
!
!     ******************************************************************
!
!
!
!
!
! SCRATCH BLOCK  325 CELLS
!
!
   !>>>>EQUIVALENCE (Necpt(6),matid1) , (Ecpt(1),Necpt(1)) , (G11,Plaans) , (G13,Nu) , (G11,Esub0) , (Necpts(1),Ecptsa(1)) , (G12,Nirof)
!
! SETUP GP MATRIX FOR PLAMAT
!
         istiff = .FALSE.
         elid = ecpt(1)
         midgp = matid1
         DO i = 1 , 9
            gp(i) = 0.0
         ENDDO
         tau0 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
         IF ( estar==0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( tau0==0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
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
         IF ( idum2==2 ) THEN
            CALL mesage(30,38,ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
            nogo = 1
            RETURN
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
         IF ( .NOT.(istiff) ) THEN
            istiff = .TRUE.
!
! CALCULATE PHASE I STRESSES
!
            DO i = 1 , 32
               ecptsa(i) = ecpt(i)
            ENDDO
            necpts(2) = 1
            necpts(3) = 4
            necpts(4) = 7
!
            CALL pktq1(2)
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
!
            CALL pktq2(3)
!
!  UPDATE ECPT FOR STRESSES
!
            sigxs = s(1)
            sigys = s(2)
            sigxys = s(3)
            tau1 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
            matid = matid1
            inflag = 8
            plaarg = tau1
!
            CALL mat(ecpt(1))
!
! TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
!
            IF ( nirof==1 ) THEN
               estar = 0.0
            ELSE
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
            ENDIF
!
!  SETUP STIFFNESS CALCULATIONS FOR GP
!
            DO i = 1 , 9
               gp(i) = 0.0
            ENDDO
            tau0 = sqrt(sigxs**2-sigxs*sigys+sigys**2+3.0*sigxys**2)
            IF ( estar/=0.0 .AND. tau0/=0.0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!  SETUP CALL TO ELEMENT STIFFNESS ROUTINE IT WILL ALSO INSERT
!
         DO i = 1 , 32
            ecptsa(i) = ecpt(i)
         ENDDO
         CALL pktrqd(2)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pktri2
