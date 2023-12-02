!*==kflud2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kflud2
   USE c_condad
   USE c_emgdic
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dpi , r1 , r2 , z1 , z1p1 , z2 , z2p1
   REAL :: dum
   INTEGER :: i , ifile , j , k , lp , n
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL sma1b
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Constd(1),Dpi) , (Ecpt(1),Necpt(1))
!
!
   IF ( ecpt(13)<ecpt(9) ) THEN
      r1 = ecpt(12)
      r2 = ecpt(8)
      z1 = ecpt(13)
      z2 = ecpt(9)
      i = necpt(3)
      necpt(3) = necpt(2)
      necpt(2) = i
   ELSE
      r1 = ecpt(8)
      z1 = ecpt(9)
      r2 = ecpt(12)
      z2 = ecpt(13)
   ENDIF
   IF ( r1==0.0D0 .OR. r2==0.0D0 ) THEN
!
      n = necpt(1)
      IF ( eltype==43 ) n = n/1000
      WRITE (out,99001) ufm , n
99001 FORMAT (A23,' 5000, NEGATIVE OR ZERO RADIUS DETECTED FOR ','CFLUID2/CAXIF2 ELEMENT ID',I9)
      nogo = .TRUE.
      RETURN
   ELSE
      IF ( z1==z2 ) RETURN
!
!     CALCULATE THE INTEGRAL PARAMETERS I2N0,I2N1,I2N2,AND I2NP2
!
      k = 2*necpt(6)
      rk = k
      IF ( k>0 ) THEN
!
         b = (r2-r1)/(z2-z1)
         dum = dabs(b)
         IF ( dum>1.0E-6 ) THEN
            z1p = r1**(k+1)
            z2p = r2**(k+1)
            z1p1 = z1p*r1
            z2p1 = z2p*r2
!
            a = 1.0D0/b
            i2n0 = a/(rk*(rk+1.0D0))*(z2p-z1p)
            i2n1 = a/(rk*(rk+1.0D0))*(z2p*z2-z1p*z1-a/(rk+2.0D0)*(z2p1-z1p1))
            i2n2 = a/(rk*(rk+1.0D0))*(z2p*z2**2-z1p*z1**2-a/(rk+2.0D0)*2.0D0*(z2p1*z2-z1p1*z1-a/(rk+3.0D0)*(z2p1*r2-z1p1*r1)))
            i2np2 = a/((rk+2.0D0)*(rk+3.0D0))*(z2p1*r2-z1p1*r1)
         ELSE
!
            z1p = ((r1+r2)/2.0D0)**k
            i2n0 = (z1p/rk)*(z2-z1)
            i2n1 = i2n0*(z2+z1)/2.0D0
            i2n2 = i2n0*(z2**2+z2*z1+z1**2)/3.0D0
            i2np2 = i2n0*rk/(rk+2.0D0)*r1**2
         ENDIF
      ELSE
!
         i2n0 = 0.0
         i2n1 = 0.0
         i2n2 = 0.0
!
         i2np2 = (z2-z1)*(r2**2+r2*r1+r1**2)/6.0D0
      ENDIF
      dz = z2 - z1
      n = necpt(6)
      z1p = r1**n
      z2p = r2**n
      hpq(1) = z2/(dz*z1p)
      hpq(2) = -z1/(dz*z2p)
      hpq(3) = -1.0D0/(dz*z1p)
      hpq(4) = 1.0D0/(dz*z2p)
      lp = 1
      IF ( npvt/=necpt(2) ) THEN
         IF ( npvt==necpt(3) ) THEN
!
            lp = 2
         ELSE
            RETURN
         ENDIF
      ENDIF
   ENDIF
   IF ( ecpt(4)==0.0 ) RETURN
   pirho = dpi/dble(ecpt(4))
   IF ( n==0 ) pirho = pirho*2.0D0
   rk = n
   twopr = 2.0*pirho*rk**2
   kh(1) = twopr*(i2n0*hpq(lp)+i2n1*hpq(lp+2))
   kh(2) = twopr*(i2n1*hpq(lp)+i2n2*hpq(lp+2)) + pirho*i2np2*hpq(lp+2)
   k1 = kh(1)*hpq(1) + kh(2)*hpq(3)
   k2 = kh(1)*hpq(2) + kh(2)*hpq(4)
   ifile = ifkgg
   i = npvt
   j = necpt(2)
   CALL sma1b(k1,j,i,ifile,0.0D0)
   j = necpt(3)
   CALL sma1b(k2,j,i,ifile,0.0D0)
END SUBROUTINE kflud2
