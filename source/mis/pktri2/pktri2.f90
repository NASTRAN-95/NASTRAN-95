!*==pktri2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pktri2
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_PLA42C
   USE C_PLA42E
   USE C_PLA42S
   USE C_PLA4ES
   USE C_PLA4UV
   USE C_PLAGP
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
         Elid = Ecpt(1)
         Midgp = matid1
         DO i = 1 , 9
            Gp(i) = 0.0
         ENDDO
         Tau0 = sqrt(Sigxs**2-Sigxs*Sigys+Sigys**2+3.0*Sigxys**2)
         IF ( Estar==0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Ipass==1 ) THEN
            Matid = matid1
            Costh = 1.0
            Sinth = 0.0E0
            Inflag = 2
!
            CALL mat(Ecpt(1))
!
            Gp(1) = G11
            Gp(2) = G12
            Gp(3) = G13
            Gp(4) = G12
            Gp(5) = G22
            Gp(6) = G23
            Gp(7) = G13
            Gp(8) = G23
            Gp(9) = G33
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Tau0==0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         Matid = matid1
         Inflag = 1
!
         CALL mat(Ecpt(1))
!
         F = 9.0*(esub0-Estar)/(4.0*Tau0**2*Estar)
         Sx = (2.0*Sigxs-Sigys)/3.0
         Sy = (2.0*Sigys-Sigxs)/3.0
         Gp(1) = (1.0+Sx**2*F)/esub0
         Gp(2) = (-nu+Sx*Sy*F)/esub0
         Gp(3) = (2.0*Sigxys*Sx*F)/esub0
         Gp(4) = Gp(2)
         Gp(5) = (1.0+Sy**2*F)/esub0
         Gp(6) = (2.0*Sigxys*Sy*F)/esub0
         Gp(7) = Gp(3)
         Gp(8) = Gp(6)
         Gp(9) = (2.0*(1.0+nu)+4.0*F*Sigxys**2)/esub0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
         Idum2 = -1
         CALL invers(3,Gp,3,0,0,Dum1,Idum2,Idum3)
!
! CHECK SINGULARITY
!
         IF ( Idum2==2 ) THEN
            CALL mesage(30,38,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
            Nogo = 1
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
               Ecptsa(i) = Ecpt(i)
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
            Ivec = 1
            DO i = 1 , 24
               Z(i) = Ui(i)
            ENDDO
            DO i = 1 , 200
               Ecptsa(i) = Ph1out(i)
            ENDDO
            S(1) = Sigxs
            S(2) = Sigys
            S(3) = Sigxys
!
            CALL pktq2(3)
!
!  UPDATE ECPT FOR STRESSES
!
            Sigxs = S(1)
            Sigys = S(2)
            Sigxys = S(3)
            Tau1 = sqrt(Sigxs**2-Sigxs*Sigys+Sigys**2+3.0*Sigxys**2)
            Matid = matid1
            Inflag = 8
            Plaarg = Tau1
!
            CALL mat(Ecpt(1))
!
! TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
!
            IF ( nirof==1 ) THEN
               Estar = 0.0
            ELSE
!
! RETURNS EPS SUB 1 GIVEN TAU1
!
               Eps1 = plaans
               Deps = Eps1 - Epss
               Depss = Epss - Eps0
               Eps2 = Eps1 + Gamma*Deps
               Inflag = 6
               Plaarg = Eps2
!
               CALL mat(Ecpt(1))
!
! RETURNS  TAU2 GIVEN EPS2
!
               Tau2 = plaans
               Estar = 0.0
               IF ( (Eps2-Eps1)/=0.0 ) Estar = (Tau2-Tau1)/(Eps2-Eps1)
               Eps0 = Epss
               Epss = Eps1
            ENDIF
!
!  SETUP STIFFNESS CALCULATIONS FOR GP
!
            DO i = 1 , 9
               Gp(i) = 0.0
            ENDDO
            Tau0 = sqrt(Sigxs**2-Sigxs*Sigys+Sigys**2+3.0*Sigxys**2)
            IF ( Estar/=0.0 .AND. Tau0/=0.0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!  SETUP CALL TO ELEMENT STIFFNESS ROUTINE IT WILL ALSO INSERT
!
         DO i = 1 , 32
            Ecptsa(i) = Ecpt(i)
         ENDDO
         CALL pktrqd(2)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pktri2
