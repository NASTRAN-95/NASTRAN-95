!*==mflud3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mflud3
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2io
   USE iso_fortran_env
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
   IF ( ecpt(6)==0.0 ) RETURN
!*****
!     STORE THE POINT LOCATIONS AND FIND THE PIVOT POINT
!*****
   jp = 0
   DO i = 1 , 3
      ir = 9 + 4*(i-1)
      r(i) = ecpt(ir)
      IF ( ecpt(ir)<=0.0 ) RETURN
      z(i) = ecpt(ir+1)
      IF ( npvt==necpt(i+1) ) jp = i
   ENDDO
   IF ( jp/=0 ) THEN
      are2 = dabs((r(2)-r(1))*(z(3)-z(1))-(r(3)-r(1))*(z(2)-z(1)))
      piab = 2.617994D-2*are2/dble(ecpt(6))
      IF ( necpt(7)==0 ) piab = piab*2.0D0
      jpvt = npvt
      DO i = 1 , 3
         igrid = necpt(i+1)
         emass = piab*(r(1)+r(2)+r(3)+r(jp)+r(i))
         IF ( i==jp ) emass = emass*2.0D0
         CALL sma2b(emass,igrid,jpvt,ifmgg,0.0D0)
      ENDDO
   ENDIF
END SUBROUTINE mflud3
