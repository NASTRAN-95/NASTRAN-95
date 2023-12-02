!*==sslot1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sslot1(Iopt)
   IMPLICIT NONE
   USE C_SDR2X5
   USE C_SDR2X6
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iot , j , ncol
   INTEGER , DIMENSION(100) :: necpt , nout
   REAL , DIMENSION(24) :: sv
!
! End of declarations rewritten by SPAG
!
!                  IOPT-  CSLOT3 = 0,  CSLOT4 = 1
!     THE ECPT DATA FOR THESE ELEMENTS ARE
!
!     FIELD   CSLOT3                CSLOT4
!       1       ID                  ID
!       2       SIL1                SIL1
!       3       SIL2                SIL2
!       4       SIL3                SIL3
!       5       RHO                 SIL4
!       6       BULK                RHO
!       7       M                   BULK
!       8       N                   M
!       9       CID1                N
!       10      R1                  CID1
!      11       Z1                  R1
!      12       W1                  Z1
!      13       CID2                W1
!      14       R2                  CID2
!      15       Z2                  R2
!      16       W2                  Z2
!      17       CID3                W2
!      18       R3                  CID3
!      19       Z3                  R3
!      20       W3                  Z3
!      21       TEMP                W3
!      22                           CID4
!      23                           R4
!      24                           Z4
!***** 25                           W4
!***** 26                           TEMP
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1)) , (Out(1),Nout(1)) , (Out(6),Sv(1))
   iot = 6
   DO i = 1 , 30
      nout(i) = 0
   ENDDO
   Nc1 = 1
   Nc2 = 2
   Nc3 = 3
   IF ( Iopt/=0 ) THEN
!****
!     THE CSLOT4 ELEMENT IS CALCULATED AS FOLLOWS
      Rho = Ecpt(6)*4.0
      DO i = 1 , 4
         Nr = 4*(i-1) + 11
         R(i) = Ecpt(Nr)
         Z(i) = Ecpt(Nr+1)
      ENDDO
      ncol = 6
      Iret = 1
   ELSE
!     SET UP FOR THE SLOT3 ELEMENT
!****
      Rho = Ecpt(5)
      Iret = 4
      DO i = 1 , 3
         Nr = 4*(i-1) + 10
         R(i) = Ecpt(Nr)
         Z(i) = Ecpt(Nr+1)
      ENDDO
   ENDIF
   SPAG_Loop_1_1: DO
      A = (R(Nc1)*(Z(Nc2)-Z(Nc3))+R(Nc2)*(Z(Nc3)-Z(Nc1))+R(Nc3)*(Z(Nc1)-Z(Nc2)))
      Fact = -Rho*A
      sv(Nc1) = (Z(Nc2)-Z(Nc3))/Fact + sv(Nc1)
      sv(Nc2) = (Z(Nc3)-Z(Nc1))/Fact + sv(Nc2)
      sv(Nc3) = (Z(Nc1)-Z(Nc2))/Fact + sv(Nc3)
!
      Nr1 = 3 + Iopt + Nc1
      Nr2 = 3 + Iopt + Nc2
      Nr3 = 3 + Iopt + Nc3
!
      sv(Nr1) = (R(Nc3)-R(Nc2))/Fact + sv(Nr1)
      sv(Nr2) = (R(Nc1)-R(Nc3))/Fact + sv(Nr2)
      sv(Nr3) = (R(Nc2)-R(Nc1))/Fact + sv(Nr3)
!
      IF ( Iret==1 ) THEN
         Nc3 = 4
         Iret = 2
      ELSEIF ( Iret==2 ) THEN
         Nc2 = 3
         Iret = 3
      ELSEIF ( Iret==3 ) THEN
         Nc1 = 2
         Iret = 4
      ELSE
!
         Nr = Iopt + 3
         IF ( Iopt==1 ) Rho = Rho/4.0
         DO i = 1 , Nr
            j = i + 1
            IF ( j>Iopt+3 ) j = j - Iopt - 3
            Fact = 1.0/(sqrt((R(j)-R(i))**2+(Z(j)-Z(i))**2)*Rho)
            Ii = Iopt*(i+1) + 4*i + 3
            Fact = 1.0/(sqrt((R(j)-R(i))**2+(Z(j)-Z(i))**2)*Rho)
            Ii = Iopt*(i+1) + 4*i + 3
            Ij = Ii + j - i
            sv(Ii) = Fact
            sv(Ij) = -Fact
         ENDDO
!
!*****
!     WRAP UP OUTPUT
!*****
         nout(1) = necpt(1)
         nout(2) = necpt(2)
         nout(3) = necpt(3)
         nout(4) = necpt(4)
         IF ( Iopt>0 ) nout(5) = necpt(5)
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE sslot1
