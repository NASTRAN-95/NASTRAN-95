
SUBROUTINE sslot1(Iopt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , Col , Ecpt(100) , Fact , Out(100) , R(4) , Rho , Sv(24) , Z(4)
   INTEGER Ii , Ij , Iret , Nc1 , Nc2 , Nc3 , Necpt(100) , Nout(100) , Nr , Nr1 , Nr2 , Nr3
   COMMON /sdr2x5/ Ecpt , Out
   COMMON /sdr2x6/ R , Z , Rho , Fact , A , Nc1 , Nc2 , Nc3 , Iret , Nr , Col , Nr1 , Nr2 , Nr3 , Ii , Ij
!
! Dummy argument declarations
!
   INTEGER Iopt
!
! Local variable declarations
!
   INTEGER i , iot , j , ncol
!
! End of declarations
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
   EQUIVALENCE (Ecpt(1),Necpt(1)) , (Out(1),Nout(1)) , (Out(6),Sv(1))
   iot = 6
   DO i = 1 , 30
      Nout(i) = 0
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
   DO
      A = (R(Nc1)*(Z(Nc2)-Z(Nc3))+R(Nc2)*(Z(Nc3)-Z(Nc1))+R(Nc3)*(Z(Nc1)-Z(Nc2)))
      Fact = -Rho*A
      Sv(Nc1) = (Z(Nc2)-Z(Nc3))/Fact + Sv(Nc1)
      Sv(Nc2) = (Z(Nc3)-Z(Nc1))/Fact + Sv(Nc2)
      Sv(Nc3) = (Z(Nc1)-Z(Nc2))/Fact + Sv(Nc3)
!
      Nr1 = 3 + Iopt + Nc1
      Nr2 = 3 + Iopt + Nc2
      Nr3 = 3 + Iopt + Nc3
!
      Sv(Nr1) = (R(Nc3)-R(Nc2))/Fact + Sv(Nr1)
      Sv(Nr2) = (R(Nc1)-R(Nc3))/Fact + Sv(Nr2)
      Sv(Nr3) = (R(Nc2)-R(Nc1))/Fact + Sv(Nr3)
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
            Sv(Ii) = Fact
            Sv(Ij) = -Fact
         ENDDO
!
!*****
!     WRAP UP OUTPUT
!*****
         Nout(1) = Necpt(1)
         Nout(2) = Necpt(2)
         Nout(3) = Necpt(3)
         Nout(4) = Necpt(4)
         IF ( Iopt>0 ) Nout(5) = Necpt(5)
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE sslot1
