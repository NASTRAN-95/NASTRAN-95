!*==sslot1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sslot1(Iopt)
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
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
   nc1 = 1
   nc2 = 2
   nc3 = 3
   IF ( Iopt/=0 ) THEN
!****
!     THE CSLOT4 ELEMENT IS CALCULATED AS FOLLOWS
      rho = ecpt(6)*4.0
      DO i = 1 , 4
         nr = 4*(i-1) + 11
         r(i) = ecpt(nr)
         z(i) = ecpt(nr+1)
      ENDDO
      ncol = 6
      iret = 1
   ELSE
!     SET UP FOR THE SLOT3 ELEMENT
!****
      rho = ecpt(5)
      iret = 4
      DO i = 1 , 3
         nr = 4*(i-1) + 10
         r(i) = ecpt(nr)
         z(i) = ecpt(nr+1)
      ENDDO
   ENDIF
   SPAG_Loop_1_1: DO
      a = (r(nc1)*(z(nc2)-z(nc3))+r(nc2)*(z(nc3)-z(nc1))+r(nc3)*(z(nc1)-z(nc2)))
      fact = -rho*a
      sv(nc1) = (z(nc2)-z(nc3))/fact + sv(nc1)
      sv(nc2) = (z(nc3)-z(nc1))/fact + sv(nc2)
      sv(nc3) = (z(nc1)-z(nc2))/fact + sv(nc3)
!
      nr1 = 3 + Iopt + nc1
      nr2 = 3 + Iopt + nc2
      nr3 = 3 + Iopt + nc3
!
      sv(nr1) = (r(nc3)-r(nc2))/fact + sv(nr1)
      sv(nr2) = (r(nc1)-r(nc3))/fact + sv(nr2)
      sv(nr3) = (r(nc2)-r(nc1))/fact + sv(nr3)
!
      IF ( iret==1 ) THEN
         nc3 = 4
         iret = 2
      ELSEIF ( iret==2 ) THEN
         nc2 = 3
         iret = 3
      ELSEIF ( iret==3 ) THEN
         nc1 = 2
         iret = 4
      ELSE
!
         nr = Iopt + 3
         IF ( Iopt==1 ) rho = rho/4.0
         DO i = 1 , nr
            j = i + 1
            IF ( j>Iopt+3 ) j = j - Iopt - 3
            fact = 1.0/(sqrt((r(j)-r(i))**2+(z(j)-z(i))**2)*rho)
            ii = Iopt*(i+1) + 4*i + 3
            fact = 1.0/(sqrt((r(j)-r(i))**2+(z(j)-z(i))**2)*rho)
            ii = Iopt*(i+1) + 4*i + 3
            ij = ii + j - i
            sv(ii) = fact
            sv(ij) = -fact
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
