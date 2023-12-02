!*==mslot.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mslot(Itype)
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2io
   USE iso_fortran_env
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
      IF ( ecpt(7)==0.0 .OR. necpt(8)==0 ) RETURN
      k = -1
      SPAG_Loop_1_1: DO
         k = k + 1
         IF ( 2*necpt(9)<k*necpt(8) ) THEN
         ELSEIF ( 2*necpt(9)==k*necpt(8) ) THEN
            necpt(8) = necpt(8)*2
         ELSE
            CYCLE
         ENDIF
         ecpt(8) = float(necpt(8))/2.0
         DO i = 1 , 4
            ecpt(i+50) = ecpt(i)
         ENDDO
         DO i = 6 , 21
            ecpt(i+49) = ecpt(i)
         ENDDO
         ecpt(56) = ecpt(7)*2.0
         iret = 1
         EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   ELSE
      IF ( ecpt(6)==0.0 .OR. necpt(7)==0 ) RETURN
      k = -1
      SPAG_Loop_1_2: DO
         k = k + 1
         IF ( 2*necpt(8)<k*necpt(7) ) THEN
         ELSEIF ( 2*necpt(8)==k*necpt(7) ) THEN
            necpt(7) = necpt(7)*2
         ELSE
            CYCLE
         ENDIF
         ecpt(7) = float(necpt(7))/2.0
         DO i = 1 , 20
            ecpt(i+50) = ecpt(i)
         ENDDO
         iret = 4
         EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
   SPAG_Loop_1_3: DO
!*****
!     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
!*****
      IF ( (necpt(52)==npvt) .OR. (necpt(53)==npvt) .OR. (necpt(54)==npvt) ) THEN
         DO i = 1 , 3
            ip = 4*(i-1) + 60
            r(i) = ecpt(ip)
            z(i) = ecpt(ip+1)
            w(i) = ecpt(ip+2)
            IF ( npvt==necpt(i+51) ) ipvt = i
         ENDDO
         a2 = (r(2)-r(1))*(z(3)-z(1)) - (r(3)-r(1))*(z(2)-z(1))
         wb = w(1) + w(2) + w(3) + w(ipvt)
         coef = dabs(a2)*ecpt(57)/(120.0D0*ecpt(56))
         i = npvt
         DO j = 1 , 3
            k = necpt(j+51)
            mij = coef*(wb+w(j))
            IF ( ipvt==j ) mij = mij*2.0D0
            CALL sma2b(mij,k,i,ifile,0.0D0)
         ENDDO
      ENDIF
      IF ( iret==1 ) THEN
         ecpt(54) = ecpt(5)
         ecpt(68) = ecpt(23)
         ecpt(69) = ecpt(24)
         ecpt(70) = ecpt(25)
         iret = 2
      ELSEIF ( iret==2 ) THEN
         ecpt(53) = ecpt(4)
         ecpt(64) = ecpt(19)
         ecpt(65) = ecpt(20)
         ecpt(66) = ecpt(21)
         iret = 3
      ELSEIF ( iret==3 ) THEN
         ecpt(52) = ecpt(3)
         ecpt(60) = ecpt(15)
         ecpt(61) = ecpt(16)
         ecpt(62) = ecpt(17)
         iret = 4
      ELSE
         EXIT SPAG_Loop_1_3
      ENDIF
   ENDDO SPAG_Loop_1_3
END SUBROUTINE mslot
