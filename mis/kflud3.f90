
SUBROUTINE kflud3
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Bet2 , Beta , Blog , Constd(5) , Deth , Dpi , Dr , Dz , Dzr , Dzr2 , G00 , G01 , G02 , G10 , G11 , G20 , H(9) , &
                  & Kg(3) , Kq(9) , Kvec(3) , Piro , Prn2 , R , R1 , R12 , R2 , R22 , R3 , Ra , Rb , Rn , Z1 , Z2 , Z3 , Za , Zb
   REAL Dum1(10) , Ecpt(100) , Sysbuf
   INTEGER Ifkgg , Iopt4 , Ipt , Ir , Iret , Jc , K4ggsw , Necpt(100) , Npvt , Out
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /condad/ Constd
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt
   COMMON /sma1dp/ R , R1 , R2 , R3 , Z1 , Z2 , Z3 , Deth , H , Ra , Rb , Za , Zb , Dr , Dz , Beta , Blog , Dzr , Dzr2 , Bet2 ,     &
                 & R12 , R22 , G00 , G10 , G20 , G01 , G11 , G02 , Rn , Piro , Prn2 , Kq , Kvec , Kg , Iret , Ipt , Jc , Ir
   COMMON /sma1et/ Ecpt
   COMMON /sma1io/ Dum1 , Ifkgg
   COMMON /system/ Sysbuf , Out , Nogo
   COMMON /xmssg / Ufm
!
! Local variable declarations
!
   INTEGER i , np(3)
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES THE PSUEDO STIFFNESS MATRIX TERMS
!     FOR THE TRIANGULAR FLUID ELEMENT
!
!     THE ECPT DATA IS THE FOLLOWING
!
!         FIELD         SYMBOL
!           1             ID
!           2             SIL1
!           3             SIL2
!           4             SIL3
!           5             RHO
!           6             BULK
!           7             N
!           8             CSF
!           9             R1
!           10            Z1
!           11            -
!           12            CSF
!           13            R2
!           14            Z2
!           15            -
!           16            CSF
!           17            R3
!           18            Z3
!           19            -
!           20            -
!
   EQUIVALENCE (Constd(1),Dpi) , (Ecpt(1),Necpt(1))
!
!     SELECT POINTS FOR COUNTERCLOCKWISE ORDER
!
   np(1) = Necpt(2)
   np(2) = Necpt(3)
   np(3) = Necpt(4)
   R1 = Ecpt(9)
   Z1 = Ecpt(10)
   R2 = Ecpt(13)
   Z2 = Ecpt(14)
   R3 = Ecpt(17)
   Z3 = Ecpt(18)
   R = (R2-R1)*(Z3-Z1) - (R3-R1)*(Z2-Z1)
   IF ( R<0 ) THEN
      np(2) = np(3)
      np(3) = Necpt(3)
      R2 = Ecpt(17)
      R3 = Ecpt(13)
      Z2 = Ecpt(18)
      Z3 = Ecpt(14)
   ELSEIF ( R==0 ) THEN
      GOTO 200
   ENDIF
   IF ( R1<=0.0D0 .OR. R2<=0.0D0 .OR. R3<=0.0D0 ) THEN
!
      Ir = Necpt(1)/1000
      WRITE (Out,99001) Ufm , Ir
99001 FORMAT (A23,' 5001, NEG. OR ZERO RADIUS DETECTED FOR CFLUID3 OR',' CFLUID4 ELEMENT',I12)
      Nogo = .TRUE.
      GOTO 99999
   ELSE
      Deth = dabs(R)
      H(1) = (R2*Z3-R3*Z2)/Deth
      H(4) = (R3*Z1-R1*Z3)/Deth
      H(7) = (R1*Z2-R2*Z1)/Deth
      H(2) = (Z2-Z3)/Deth
      H(5) = (Z3-Z1)/Deth
      H(8) = (Z1-Z2)/Deth
      H(3) = (R3-R2)/Deth
      H(6) = (R1-R3)/Deth
      H(9) = (R2-R1)/Deth
!
!     THE INTEGRAL PARAMETERS ARE THE SUM DUE TO SIDES 1-2,2-3,3-1.
!
      G00 = 0.0
      G01 = 0.0
      G02 = 0.0
      G10 = 0.0
      G11 = 0.0
      G20 = 0.0
      Iret = 1
      Ra = R1
      Rb = R2
      Za = Z1
      Zb = Z2
   ENDIF
   DO
!
!     THE INTEGRAL PARAMETERS ARE CALCULATED BELOW
!
      Dr = Rb - Ra
      Dz = Zb - Za
      IF ( Dr**2/Deth>1.0D-6 ) THEN
         Beta = Za - Ra*Dz/Dr
         Bet2 = Beta**2
         Blog = Beta*dlog(Ra/Rb)
         Dzr = Dz/Dr
         Dzr2 = Dzr**2
         R12 = (Ra**2-Rb**2)/2.0D0
         R22 = (Ra**3-Rb**3)/3.0D0
         G00 = G00 + Blog - Dz
         G10 = G10 - Beta*Dr + R12*Dzr
         G20 = G20 + Beta*R12 + Dzr*R22
         G01 = G01 + Blog*Beta/2.0D0 - Beta*Dz + Dzr2*R12/2.0D0
         G11 = G11 - Bet2*Dr/2.0D0 + Beta*Dzr*R12 + Dzr2*R22/2.0D0
         G02 = G02 + Bet2*Blog/3.0D0 - Bet2*Dz + Beta*Dzr2*R12 + Dzr*Dzr2*R22/3.0D0
      ENDIF
      IF ( Iret==1 ) THEN
         Iret = 2
         Ra = R2
         Rb = R3
         Za = Z2
         Zb = Z3
      ELSEIF ( Iret==2 ) THEN
         Iret = 3
         Ra = R3
         Rb = R1
         Za = Z3
         Zb = Z1
      ELSE
!
!     FORM THE PSUEDO STIFFNESS MATRIX USING THE PARAMETERS
!
         Rn = Necpt(7)
         IF ( Ecpt(5)<=0.0 ) RETURN
         Piro = Dpi/dble(Ecpt(5))
         IF ( Necpt(7)==0 ) Piro = Piro*2.0D0
         Prn2 = Piro*Rn**2
         Kq(1) = Prn2*G00
         Kq(2) = Prn2*G10
         Kq(3) = Prn2*G01
         Kq(4) = Kq(2)
         Kq(5) = (Piro+Prn2)*G20
         Kq(6) = Prn2*G11
         Kq(7) = Kq(3)
         Kq(8) = Kq(6)
         Kq(9) = Piro*G20 + Prn2*G02
         DO i = 1 , 3
            Ipt = i - 1
            IF ( Npvt==np(i) ) GOTO 100
         ENDDO
         RETURN
      ENDIF
   ENDDO
!
 100  Ipt = 3*Ipt + 1
   CALL gmmatd(H(Ipt),1,3,0,Kq,3,3,0,Kvec)
   CALL gmmatd(Kvec,1,3,0,H(1),3,3,1,Kg)
   Jc = Npvt
   DO i = 1 , 3
      Ir = np(i)
      CALL sma1b(Kg(i),Ir,Jc,Ifkgg,0.0D0)
   ENDDO
 200  RETURN
99999 RETURN
END SUBROUTINE kflud3
