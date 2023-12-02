!*==flld.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flld(X01,X02,Y0,Z0,Sgr,Cgr,Sgs,Cgs,Kr,Cbar,Fmach,E,L,Kd1r,Kd1i,Kd2r,Kd2i)
   USE c_dlm
   USE c_kds
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X01
   REAL :: X02
   REAL :: Y0
   REAL :: Z0
   REAL :: Sgr
   REAL :: Cgr
   REAL :: Sgs
   REAL :: Cgs
   REAL :: Kr
   REAL :: Cbar
   REAL :: Fmach
   REAL :: E
   INTEGER :: L
   REAL :: Kd1r
   REAL :: Kd1i
   REAL :: Kd2r
   REAL :: Kd2i
!
! Local variable declarations rewritten by SPAG
!
   REAL :: br , ct1 , delxi , rt1 , rt2 , st1 , t1 , x0
   INTEGER :: i
   COMPLEX :: k1xi1 , k1xi2 , k2xi1 , k2xi2 , kd1 , kd2 , temp1 , temp2
   EXTERNAL tker
!
! End of declarations rewritten by SPAG
!
!
!     CALCULATION OF THE NUMERATOR OF A DOUBLET LINE OF FINITE LENGTH.
!     LIKE KERN, THERE ARE TWO OUTPUT COMPLEX VALUES REPRESENTED BY
!     FOUR REAL NUMBERS AND AN INPUT OPTION.
!
!     WRITTEN BY D. H. LARSON, STRUCTURAL MECHANICS MDAC 11/70
!
!     X01  -   X - XI1
!     X02  -   X - XI2
!     Y0   -   Y - ETA
!     Z0   -   Z - ZETA
!     SGR  -   SIN ( GAMMA-R)
!     CGR  -   COS ( GAMMA-R)
!     SGS  -   SIN ( GAMMA-S)
!     CGS  -   COS ( GAMMA-S)
!     KR   -   REDUCED FREQUENCY
!     BR   -   REFERENCE LENGTH
!     FMACH-   MACH NUMBER
!     E    -
!     L    -   OPTION FLAG USED IN TKER
!     KD1R -   REAL PART OF  KD1
!     KD1I -   IMAGINARY PART OF KD1
!     KD2R -   REAL PART OF  KD2
!     KD2I -   IMAGINARY PART OF KD2
!
!
!     X01 = X-XI1  AND  X02 = X-XI2, DELXI = XI2-XI1
!
   delxi = X01 - X02
!
!     FULL KERNEL FROM -TKER-
!
   ind = 0
   Kd1r = 0.0
   Kd2r = 0.0
   t1 = Kr*delxi/Cbar
   br = Cbar/2.0
   st1 = sin(t1)
   ct1 = cos(t1)
   i = 1
   x0 = X01
   SPAG_Loop_1_1: DO
!
      CALL tker(x0,Y0,Z0,Kr,br,Sgr,Cgr,Sgs,Cgs,rt1,rt2,Fmach)
!
      IF ( i==2 ) THEN
!
         k1xi2 = cmplx(kk1r,kk1i)
         k2xi2 = cmplx(kk2r,kk2i)
         IF ( L/=0 ) THEN
            Kd1r = Kd1r + k10t1
            Kd2r = Kd2r + k20t2p
         ENDIF
!
         temp1 = cmplx(ct1,st1)
         temp2 = cmplx(ct1,-st1)
!
!     DESIRED RESULTS (COMPLEX)
!
         kd1 = k1xi1*temp1 - k1xi2*temp2
         kd2 = k2xi1*temp1 - k2xi2*temp2
!
!     CONVERT TO REAL AND IMAGINARY PARTS
!
         Kd1r = real(kd1) + Kd1r
         Kd1i = aimag(kd1)
         Kd2r = real(kd2) + Kd2r
         Kd2i = aimag(kd2)
         EXIT SPAG_Loop_1_1
      ELSE
         k1xi1 = cmplx(kk1r,kk1i)
         k2xi1 = cmplx(kk2r,kk2i)
         IF ( L/=0 ) THEN
            Kd1r = Kd1r - k10t1
            Kd2r = Kd2r - k20t2p
         ENDIF
!
!     NOW GO CALCULATE FOR XI = XI2
!
         x0 = X02
         i = 2
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE flld
