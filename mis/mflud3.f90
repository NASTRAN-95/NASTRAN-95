
SUBROUTINE mflud3
   IMPLICIT NONE
   DOUBLE PRECISION Are2 , Emass , Piab , R(3) , Z(3)
   REAL Dum(2) , Dum1(10) , Ecpt(100)
   INTEGER Ifmgg , Igrid , Ir , Jp , Jpvt , Necpt(100) , Npvt
   COMMON /sma2cl/ Dum , Npvt
   COMMON /sma2dp/ R , Z , Are2 , Piab , Emass , Jp , Ir , Jpvt , Igrid
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Dum1 , Ifmgg
   INTEGER i
!*****
!     THIS ROUTINE GENERATES THE PSUEDO   MASS    MATRIX TERMS
!     FOR THE TRIANGULAR FLUID ELEMENT
!*****
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
!****
!*****
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
!*****
!*****
!
   IF ( Ecpt(6)==0.0 ) RETURN
!*****
!     STORE THE POINT LOCATIONS AND FIND THE PIVOT POINT
!*****
   Jp = 0
   DO i = 1 , 3
      Ir = 9 + 4*(i-1)
      R(i) = Ecpt(Ir)
      IF ( Ecpt(Ir)<=0.0 ) GOTO 99999
      Z(i) = Ecpt(Ir+1)
      IF ( Npvt==Necpt(i+1) ) Jp = i
   ENDDO
   IF ( Jp/=0 ) THEN
      Are2 = dabs((R(2)-R(1))*(Z(3)-Z(1))-(R(3)-R(1))*(Z(2)-Z(1)))
      Piab = 2.617994D-2*Are2/dble(Ecpt(6))
      IF ( Necpt(7)==0 ) Piab = Piab*2.0D0
      Jpvt = Npvt
      DO i = 1 , 3
         Igrid = Necpt(i+1)
         Emass = Piab*(R(1)+R(2)+R(3)+R(Jp)+R(i))
         IF ( i==Jp ) Emass = Emass*2.0D0
         CALL sma2b(Emass,Igrid,Jpvt,Ifmgg,0.0D0)
      ENDDO
   ENDIF
99999 RETURN
END SUBROUTINE mflud3