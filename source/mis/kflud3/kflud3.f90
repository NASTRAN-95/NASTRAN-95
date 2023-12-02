!*==kflud3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kflud3
   USE c_condad
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
   REAL(REAL64) :: dpi
   INTEGER :: i
   INTEGER , DIMENSION(100) :: necpt
   INTEGER , DIMENSION(3) :: np
   EXTERNAL gmmatd , sma1b
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Constd(1),Dpi) , (Ecpt(1),Necpt(1))
!
!     SELECT POINTS FOR COUNTERCLOCKWISE ORDER
!
   np(1) = necpt(2)
   np(2) = necpt(3)
   np(3) = necpt(4)
   r1 = ecpt(9)
   z1 = ecpt(10)
   r2 = ecpt(13)
   z2 = ecpt(14)
   r3 = ecpt(17)
   z3 = ecpt(18)
   r = (r2-r1)*(z3-z1) - (r3-r1)*(z2-z1)
   IF ( r<0 ) THEN
      np(2) = np(3)
      np(3) = necpt(3)
      r2 = ecpt(17)
      r3 = ecpt(13)
      z2 = ecpt(18)
      z3 = ecpt(14)
   ELSEIF ( r==0 ) THEN
      CALL spag_block_1
      RETURN
   ENDIF
   IF ( r1<=0.0D0 .OR. r2<=0.0D0 .OR. r3<=0.0D0 ) THEN
!
      ir = necpt(1)/1000
      WRITE (out,99001) ufm , ir
99001 FORMAT (A23,' 5001, NEG. OR ZERO RADIUS DETECTED FOR CFLUID3 OR',' CFLUID4 ELEMENT',I12)
      nogo = .TRUE.
      RETURN
   ELSE
      deth = dabs(r)
      h(1) = (r2*z3-r3*z2)/deth
      h(4) = (r3*z1-r1*z3)/deth
      h(7) = (r1*z2-r2*z1)/deth
      h(2) = (z2-z3)/deth
      h(5) = (z3-z1)/deth
      h(8) = (z1-z2)/deth
      h(3) = (r3-r2)/deth
      h(6) = (r1-r3)/deth
      h(9) = (r2-r1)/deth
!
!     THE INTEGRAL PARAMETERS ARE THE SUM DUE TO SIDES 1-2,2-3,3-1.
!
      g00 = 0.0
      g01 = 0.0
      g02 = 0.0
      g10 = 0.0
      g11 = 0.0
      g20 = 0.0
      iret = 1
      ra = r1
      rb = r2
      za = z1
      zb = z2
   ENDIF
   SPAG_Loop_1_1: DO
!
!     THE INTEGRAL PARAMETERS ARE CALCULATED BELOW
!
      dr = rb - ra
      dz = zb - za
      IF ( dr**2/deth>1.0D-6 ) THEN
         beta = za - ra*dz/dr
         bet2 = beta**2
         blog = beta*dlog(ra/rb)
         dzr = dz/dr
         dzr2 = dzr**2
         r12 = (ra**2-rb**2)/2.0D0
         r22 = (ra**3-rb**3)/3.0D0
         g00 = g00 + blog - dz
         g10 = g10 - beta*dr + r12*dzr
         g20 = g20 + beta*r12 + dzr*r22
         g01 = g01 + blog*beta/2.0D0 - beta*dz + dzr2*r12/2.0D0
         g11 = g11 - bet2*dr/2.0D0 + beta*dzr*r12 + dzr2*r22/2.0D0
         g02 = g02 + bet2*blog/3.0D0 - bet2*dz + beta*dzr2*r12 + dzr*dzr2*r22/3.0D0
      ENDIF
      IF ( iret==1 ) THEN
         iret = 2
         ra = r2
         rb = r3
         za = z2
         zb = z3
      ELSEIF ( iret==2 ) THEN
         iret = 3
         ra = r3
         rb = r1
         za = z3
         zb = z1
      ELSE
!
!     FORM THE PSUEDO STIFFNESS MATRIX USING THE PARAMETERS
!
         rn = necpt(7)
         IF ( ecpt(5)<=0.0 ) RETURN
         piro = dpi/dble(ecpt(5))
         IF ( necpt(7)==0 ) piro = piro*2.0D0
         prn2 = piro*rn**2
         kq(1) = prn2*g00
         kq(2) = prn2*g10
         kq(3) = prn2*g01
         kq(4) = kq(2)
         kq(5) = (piro+prn2)*g20
         kq(6) = prn2*g11
         kq(7) = kq(3)
         kq(8) = kq(6)
         kq(9) = piro*g20 + prn2*g02
         DO i = 1 , 3
            ipt = i - 1
            IF ( npvt==np(i) ) EXIT SPAG_Loop_1_1
         ENDDO
         RETURN
      ENDIF
   ENDDO SPAG_Loop_1_1
!
   ipt = 3*ipt + 1
   CALL gmmatd(h(ipt),1,3,0,kq,3,3,0,kvec)
   CALL gmmatd(kvec,1,3,0,h(1),3,3,1,kg)
   jc = npvt
   DO i = 1 , 3
      ir = np(i)
      CALL sma1b(kg(i),ir,jc,ifkgg,0.0D0)
   ENDDO
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
   END SUBROUTINE spag_block_1
END SUBROUTINE kflud3
