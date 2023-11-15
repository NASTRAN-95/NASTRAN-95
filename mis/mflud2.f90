
SUBROUTINE mflud2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A , B , Constd(5) , Dpi , Dz , F0 , Hpq(4) , I2n0 , I2n1 , I2n2 , I2np2 , K1 , K2 , Kfact , Kh(4) , Pirho , Ri ,&
                  & Rk , Twopr , Z1p , Z2p
   REAL Dum1(10) , Dun2(2) , Ecpt(100)
   INTEGER Ifmgg , Necpt(100) , Npvt
   COMMON /condad/ Constd
   COMMON /sma2cl/ Dun2 , Npvt
   COMMON /sma2dp/ Z1p , Z2p , Rk , Ri , Kfact , F0 , A , B , I2n0 , I2n1 , I2n2 , I2np2 , Dz , Hpq , Pirho , Twopr , Kh , K1 , K2
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Dum1 , Ifmgg
!
! Local variable declarations
!
   REAL dum
   INTEGER i , ifile , j , k , lp , n
   DOUBLE PRECISION r1 , r2 , z1 , z1p1 , z2 , z2p1
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES THE PSUEDO STIFFNESS MATRIX TERMS
!     FOR THE CENTER PLUG FLUID ELEMENT
!
!     THE ECPT DATA BLOCK CONTAINS THE FOLLOWING DATA
!
!         FIELD    SYMBOL
!           1        ID
!           2        SIL1
!           3        SIL2
!           4        RHO
!           5        BULK
!           6        N
!           7        CSF
!           8        R1
!           9        Z1
!           10       -
!           11       CSF
!           12       R2
!           13       Z2
!           14       -
!           15       -
!
   EQUIVALENCE (Constd(1),Dpi) , (Ecpt(1),Necpt(1))
!
!
   IF ( Ecpt(13)<Ecpt(9) ) THEN
      r1 = Ecpt(12)
      r2 = Ecpt(8)
      z1 = Ecpt(13)
      z2 = Ecpt(9)
      i = Necpt(3)
      Necpt(3) = Necpt(2)
      Necpt(2) = i
   ELSE
      r1 = Ecpt(8)
      z1 = Ecpt(9)
      r2 = Ecpt(12)
      z2 = Ecpt(13)
   ENDIF
   IF ( Ecpt(5)<=0.0 ) RETURN
   IF ( r1/=0.0 .AND. r2/=0.0 ) THEN
      IF ( z1/=z2 ) THEN
!
!     CALCULATE THE INTEGRAL PARAMETERS I2N0,I2N1,I2N2,AND I2NP2
!
         k = 2*Necpt(6) + 2
         Rk = k
         B = (r2-r1)/(z2-z1)
         dum = dabs(B)
         IF ( dum>1.0E-6 ) THEN
!
            Z1p = r1**(k+1)
            Z2p = r2**(k+1)
            z1p1 = Z1p*r1
            z2p1 = Z2p*r2
            A = 1.0D0/B
            I2n0 = A/(Rk*(Rk+1.0D0))*(Z2p-Z1p)
            I2n1 = A/(Rk*(Rk+1.0D0))*(Z2p*z2-Z1p*z1-A/(Rk+2.0D0)*(z2p1-z1p1))
            I2n2 = A/(Rk*(Rk+1.0D0))*(Z2p*z2**2-Z1p*z1**2-A/(Rk+2.0D0)*2.0D0*(z2p1*z2-z1p1*z1-A/(Rk+3.0D0)*(z2p1*r2-z1p1*r1)))
            I2np2 = A/((Rk+2.0D0)*(Rk+3.0D0))*(z2p1*r2-z1p1*r1)
         ELSE
            Z1p = ((r1+r2)/2.0D0)**k
            I2n0 = (Z1p/Rk)*(z2-z1)
            I2n1 = I2n0*(z2+z1)/2.0D0
            I2n2 = I2n0*(z2**2+z2*z1+z1**2)/3.0D0
            I2np2 = I2n0*Rk/(Rk+2.0D0)*r1**2
         ENDIF
!
         Dz = z2 - z1
         n = Necpt(6)
         Z1p = r1**n
         Z2p = r2**n
         Hpq(1) = z2/(Dz*Z1p)
         Hpq(2) = -z1/(Dz*Z2p)
         Hpq(3) = -1.0D0/(Dz*Z1p)
         Hpq(4) = 1.0D0/(Dz*Z2p)
         lp = 1
         IF ( Npvt/=Necpt(2) ) THEN
            IF ( Npvt/=Necpt(3) ) GOTO 99999
            lp = 2
         ENDIF
         Pirho = Dpi/dble(Ecpt(5))
         IF ( Necpt(6)==0 ) Pirho = 2.0D0*Pirho
         Kh(1) = Pirho*(I2n0*Hpq(lp)+I2n1*Hpq(lp+2))
         Kh(2) = Pirho*(I2n1*Hpq(lp)+I2n2*Hpq(lp+2))
         K1 = Kh(1)*Hpq(1) + Kh(2)*Hpq(3)
         K2 = Kh(1)*Hpq(2) + Kh(2)*Hpq(4)
         ifile = Ifmgg
         i = Npvt
         j = Necpt(2)
         CALL sma2b(K1,j,i,ifile,0.0D0)
         j = Necpt(3)
         CALL sma2b(K2,j,i,ifile,0.0D0)
      ENDIF
   ENDIF
99999 END SUBROUTINE mflud2
