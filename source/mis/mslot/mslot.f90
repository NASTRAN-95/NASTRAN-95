!*==mslot.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mslot(Itype)
USE C_SMA2CL
USE C_SMA2DP
USE C_SMA2ET
USE C_SMA2IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ipvt , j
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL sma2b
!
! End of declarations rewritten by SPAG
!
!*****
!     THIS ROUTINE CALCULATES THE MASS MATRIX TERMS FOR THE
!         CSLOT3 AND CSLOT4 TWO DIMENSIONAL LAPLACE ELEMENTS
!                  IOPT-  CSLOT3 = 0,  CSLOT4 = 1
!*****
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
!
!*****
!*****
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
!*****
   IF ( Itype>0 ) THEN
!*****
!     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
!     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
!*****
      IF ( Ecpt(7)==0.0 .OR. necpt(8)==0 ) RETURN
      K = -1
      SPAG_Loop_1_1: DO
         K = K + 1
         IF ( 2*necpt(9)<K*necpt(8) ) THEN
         ELSEIF ( 2*necpt(9)==K*necpt(8) ) THEN
            necpt(8) = necpt(8)*2
         ELSE
            CYCLE
         ENDIF
         Ecpt(8) = float(necpt(8))/2.0
         DO i = 1 , 4
            Ecpt(i+50) = Ecpt(i)
         ENDDO
         DO i = 6 , 21
            Ecpt(i+49) = Ecpt(i)
         ENDDO
         Ecpt(56) = Ecpt(7)*2.0
         Iret = 1
         EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   ELSE
      IF ( Ecpt(6)==0.0 .OR. necpt(7)==0 ) RETURN
      K = -1
      SPAG_Loop_1_2: DO
         K = K + 1
         IF ( 2*necpt(8)<K*necpt(7) ) THEN
         ELSEIF ( 2*necpt(8)==K*necpt(7) ) THEN
            necpt(7) = necpt(7)*2
         ELSE
            CYCLE
         ENDIF
         Ecpt(7) = float(necpt(7))/2.0
         DO i = 1 , 20
            Ecpt(i+50) = Ecpt(i)
         ENDDO
         Iret = 4
         EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
   SPAG_Loop_1_3: DO
!*****
!     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
!*****
      IF ( (necpt(52)==Npvt) .OR. (necpt(53)==Npvt) .OR. (necpt(54)==Npvt) ) THEN
         DO i = 1 , 3
            Ip = 4*(i-1) + 60
            R(i) = Ecpt(Ip)
            Z(i) = Ecpt(Ip+1)
            W(i) = Ecpt(Ip+2)
            IF ( Npvt==necpt(i+51) ) ipvt = i
         ENDDO
         A2 = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
         Wb = W(1) + W(2) + W(3) + W(ipvt)
         Coef = dabs(A2)*Ecpt(57)/(120.0D0*Ecpt(56))
         i = Npvt
         DO j = 1 , 3
            K = necpt(j+51)
            Mij = Coef*(Wb+W(j))
            IF ( ipvt==j ) Mij = Mij*2.0D0
            CALL sma2b(Mij,K,i,Ifile,0.0D0)
         ENDDO
      ENDIF
      IF ( Iret==1 ) THEN
         Ecpt(54) = Ecpt(5)
         Ecpt(68) = Ecpt(23)
         Ecpt(69) = Ecpt(24)
         Ecpt(70) = Ecpt(25)
         Iret = 2
      ELSEIF ( Iret==2 ) THEN
         Ecpt(53) = Ecpt(4)
         Ecpt(64) = Ecpt(19)
         Ecpt(65) = Ecpt(20)
         Ecpt(66) = Ecpt(21)
         Iret = 3
      ELSEIF ( Iret==3 ) THEN
         Ecpt(52) = Ecpt(3)
         Ecpt(60) = Ecpt(15)
         Ecpt(61) = Ecpt(16)
         Ecpt(62) = Ecpt(17)
         Iret = 4
      ELSE
         EXIT SPAG_Loop_1_3
      ENDIF
   ENDDO SPAG_Loop_1_3
END SUBROUTINE mslot
