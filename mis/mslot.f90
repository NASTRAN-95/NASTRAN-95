
SUBROUTINE mslot(Itype)
   IMPLICIT NONE
   DOUBLE PRECISION A2 , Coef , Mij , R(3) , W(3) , Wb , Z(3)
   REAL Dum1(10) , Ecpt(100)
   INTEGER Ifile , Iopt4 , Ip , Iret , K , K4ggsw , Necpt(100) , Npvt
   COMMON /sma2cl/ Iopt4 , K4ggsw , Npvt
   COMMON /sma2dp/ Coef , A2 , Wb , R , Z , W , Mij , Iret , Ip , K
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Dum1 , Ifile
   INTEGER Itype
   INTEGER i , ipvt , j
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
   EQUIVALENCE (Ecpt(1),Necpt(1))
!*****
   IF ( Itype>0 ) THEN
!*****
!     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
!     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
!*****
      IF ( Ecpt(7)==0.0 .OR. Necpt(8)==0 ) RETURN
      K = -1
      DO
         K = K + 1
         IF ( 2*Necpt(9)<K*Necpt(8) ) THEN
         ELSEIF ( 2*Necpt(9)==K*Necpt(8) ) THEN
            Necpt(8) = Necpt(8)*2
         ELSE
            CYCLE
         ENDIF
         Ecpt(8) = float(Necpt(8))/2.0
         DO i = 1 , 4
            Ecpt(i+50) = Ecpt(i)
         ENDDO
         DO i = 6 , 21
            Ecpt(i+49) = Ecpt(i)
         ENDDO
         Ecpt(56) = Ecpt(7)*2.0
         Iret = 1
         EXIT
      ENDDO
   ELSE
      IF ( Ecpt(6)==0.0 .OR. Necpt(7)==0 ) RETURN
      K = -1
      DO
         K = K + 1
         IF ( 2*Necpt(8)<K*Necpt(7) ) THEN
         ELSEIF ( 2*Necpt(8)==K*Necpt(7) ) THEN
            Necpt(7) = Necpt(7)*2
         ELSE
            CYCLE
         ENDIF
         Ecpt(7) = float(Necpt(7))/2.0
         DO i = 1 , 20
            Ecpt(i+50) = Ecpt(i)
         ENDDO
         Iret = 4
         EXIT
      ENDDO
   ENDIF
   DO
!*****
!     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
!*****
      IF ( (Necpt(52)==Npvt) .OR. (Necpt(53)==Npvt) .OR. (Necpt(54)==Npvt) ) THEN
         DO i = 1 , 3
            Ip = 4*(i-1) + 60
            R(i) = Ecpt(Ip)
            Z(i) = Ecpt(Ip+1)
            W(i) = Ecpt(Ip+2)
            IF ( Npvt==Necpt(i+51) ) ipvt = i
         ENDDO
         A2 = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
         Wb = W(1) + W(2) + W(3) + W(ipvt)
         Coef = dabs(A2)*Ecpt(57)/(120.0D0*Ecpt(56))
         i = Npvt
         DO j = 1 , 3
            K = Necpt(j+51)
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
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE mslot
