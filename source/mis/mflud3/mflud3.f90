!*==mflud3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mflud3
USE C_SMA2CL
USE C_SMA2DP
USE C_SMA2ET
USE C_SMA2IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL sma2b
!
! End of declarations rewritten by SPAG
!
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
      IF ( Ecpt(Ir)<=0.0 ) RETURN
      Z(i) = Ecpt(Ir+1)
      IF ( Npvt==necpt(i+1) ) Jp = i
   ENDDO
   IF ( Jp/=0 ) THEN
      Are2 = dabs((R(2)-R(1))*(Z(3)-Z(1))-(R(3)-R(1))*(Z(2)-Z(1)))
      Piab = 2.617994D-2*Are2/dble(Ecpt(6))
      IF ( necpt(7)==0 ) Piab = Piab*2.0D0
      Jpvt = Npvt
      DO i = 1 , 3
         Igrid = necpt(i+1)
         Emass = Piab*(R(1)+R(2)+R(3)+R(Jp)+R(i))
         IF ( i==Jp ) Emass = Emass*2.0D0
         CALL sma2b(Emass,Igrid,Jpvt,Ifmgg,0.0D0)
      ENDDO
   ENDIF
END SUBROUTINE mflud3
