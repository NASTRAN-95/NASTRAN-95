
SUBROUTINE kslot(Itype)
   IMPLICIT NONE
   DOUBLE PRECISION A2 , Coef , Fir(3) , Fiz(3) , Kij , R(3) , Rki , Z(3)
   REAL Dum1(10) , Ecpt(100) , Sysbuf
   INTEGER Ifile , Iopt4 , Ip , Ipvt , Iret , K4ggsw , Lri , Lrj , Lrk , Necpt(100) , Nneg , Nptj , Npvt , Out
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt
   COMMON /sma1dp/ Coef , Fir , Fiz , R , Z , Rki , A2 , Kij , Nneg , Ip , Nptj , Iret , Lri , Lrj , Lrk , Ipvt
   COMMON /sma1et/ Ecpt
   COMMON /sma1io/ Dum1 , Ifile
   COMMON /system/ Sysbuf , Out , Nogo
   COMMON /xmssg / Ufm
   INTEGER Itype
   INTEGER i , j , k , nj
!
!     THIS ROUTINE CALCULATES THE STIFFNESS MATRIX TERMS FOR THE
!     CSLOT3 AND CSLOT4 TWO DIMENSIONAL LAPLACE ELEMENTS
!
!     IOPT-  CSLOT3 = 0,  CSLOT4 = 1
!
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
!      25                           W4
!      26                           TEMP
!
   EQUIVALENCE (Ecpt(1),Necpt(1))
!
   IF ( Itype>0 ) THEN
!
!     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
!     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
!
      IF ( Ecpt(6)==0.0 .OR. Necpt(8)==0 ) RETURN
      k = -1
      DO
         k = k + 1
         IF ( 2*Necpt(9)<k*Necpt(8) ) THEN
         ELSEIF ( 2*Necpt(9)==k*Necpt(8) ) THEN
            Necpt(8) = Necpt(8)*2
         ELSE
            CYCLE
         ENDIF
         Ecpt(8) = float(Necpt(8))/2.0
!
         Nneg = 0
         Ip = 0
         DO i = 1 , 4
            IF ( Npvt==Necpt(i+1) ) Ip = Ip + 1
            DO j = 1 , 3
               nj = i + j - 1
               IF ( nj>4 ) nj = nj - 4
               Nptj = 4*(nj-1) + 11
               R(j) = Ecpt(Nptj)
               Z(j) = Ecpt(Nptj+1)
            ENDDO
            Coef = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
            IF ( Coef<0 ) THEN
               Nneg = Nneg + 1
            ELSEIF ( Coef==0 ) THEN
               GOTO 200
            ENDIF
         ENDDO
         IF ( Nneg==1 .OR. Nneg==3 ) GOTO 200
         IF ( Ip/=1 ) GOTO 200
!
         DO i = 1 , 4
            Ecpt(i+50) = Ecpt(i)
         ENDDO
         DO i = 7 , 21
            Ecpt(i+49) = Ecpt(i)
         ENDDO
         Ecpt(55) = Ecpt(6)*2.0
         Iret = 1
         EXIT
      ENDDO
   ELSE
      IF ( Ecpt(5)==0.0 .OR. Necpt(7)==0 ) RETURN
      k = -1
      DO
         k = k + 1
         IF ( 2*Necpt(8)<k*Necpt(7) ) THEN
         ELSEIF ( 2*Necpt(8)==k*Necpt(7) ) THEN
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
!
!     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
!
 100  IF ( Necpt(52)==Npvt .OR. Necpt(53)==Npvt .OR. Necpt(54)==Npvt ) THEN
      Coef = 0.0
      A2 = 0.0
      DO i = 1 , 3
         j = i + 1
         IF ( j>3 ) j = j - 3
         k = j + 1
         IF ( k>3 ) k = k - 3
         Lri = 4*i + 56
         Lrj = 4*j + 56
         Lrk = 4*k + 56
         Coef = Coef + Ecpt(Lri+2)
         Fir(i) = Ecpt(Lrk) - Ecpt(Lrj)
         Fiz(i) = Ecpt(Lrj+1) - Ecpt(Lrk+1)
         A2 = A2 + Ecpt(Lri)*Fiz(i)
         IF ( Necpt(i+51)==Npvt ) Ipvt = i
      ENDDO
      IF ( A2==0.0D0 ) GOTO 200
      Coef = Coef*Ecpt(57)/(6.0D0*Ecpt(55)*dabs(A2))
      i = Npvt
      DO j = 1 , 3
         k = Necpt(j+51)
         Kij = Coef*(Fir(Ipvt)*Fir(j)+Fiz(Ipvt)*Fiz(j))
         CALL sma1b(Kij,k,i,Ifile,0.0D0)
      ENDDO
   ENDIF
   IF ( Iret==1 ) THEN
      Ecpt(54) = Ecpt(5)
      Ecpt(68) = Ecpt(23)
      Ecpt(69) = Ecpt(24)
      Ecpt(70) = Ecpt(25)
      Iret = 2
      GOTO 100
   ELSEIF ( Iret==2 ) THEN
      Ecpt(53) = Ecpt(4)
      Ecpt(64) = Ecpt(19)
      Ecpt(65) = Ecpt(20)
      Ecpt(66) = Ecpt(21)
      Iret = 3
      GOTO 100
   ELSEIF ( Iret==3 ) THEN
      Ecpt(52) = Ecpt(3)
      Ecpt(60) = Ecpt(15)
      Ecpt(61) = Ecpt(16)
      Ecpt(62) = Ecpt(17)
      Iret = 4
      GOTO 100
   ELSE
      RETURN
   ENDIF
!
 200  WRITE (Out,99001) Ufm , Necpt(1)
99001 FORMAT (A23,' 2160, BAD GEOMETRY OR ZERO COEFFICIENT FOR SLOT ','ELEMENT NUMBER',I18)
   Nogo = .TRUE.
END SUBROUTINE kslot
