!*==strm62.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strm62(Ti)
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ftemp , sigx1 , sigx2 , sigxy1 , sigxy2 , sigy1 , sigy2 , temp1
   INTEGER :: i , ii , ii12 , ijk , j , j1 , j2
   INTEGER , DIMENSION(990) :: nph1ou
   INTEGER , DIMENSION(6) :: ns1l
   REAL , DIMENSION(36) :: si
   REAL , DIMENSION(99) :: stout
   REAL , DIMENSION(18) :: str
   REAL , DIMENSION(3) :: stress
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!
!     PHASE II OF STRESS DATA RECOVERY FOR TRIANGULAR MEMBRANE ELEMENT
!     TRIM6
!
!     PHASE I OUTPUT IS THE FOLLOWING
!
!     PH1OUT(1)               ELEMENT ID
!     PH1OUT(2, THRU 7)       6 S1L5
!     PH1OUT(8 THRU 10)       THICKNESSES AT CORNER GRID POINT
!     PH1OUT(11)              REFERENCE TEMPERATURE
!     PH1OUT(12)-(227)        S SUB I MATRICES FOR 4 POINTS
!     PH1OUT(228)-(230)       THERMAL VECTOR - G TIMES ALPHA
!
!
   !>>>>EQUIVALENCE (Ns1l(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(11)) , (Ldtemp,Ftemp)
!
   DO ii = 1 , 4
!
!     ZERO OUT LOCAL STRESSES
!
      sigx1 = 0.0
      sigy1 = 0.0
      sigxy1 = 0.0
      sigx2 = 0.0
      sigy2 = 0.0
      sigxy2 = 0.0
      IF ( ns1l(1)/=0 ) THEN
!
!     ZERO STRESS VECTOR STORAGE
!
         DO i = 1 , 3
            stress(i) = 0.0
         ENDDO
!
!                        I=6
!     STRESS VECTOR =(SUMMATION (5 )(U ) ) - (S )(TEMP      - TEMP   )
!                        I=1      I   I        T      POINT       REF
!
         DO i = 1 , 6
!
!     POINTER TO I-TH SIL IN PH1OUT
!
            Npoint = Ivec + nph1ou(i+1) - 1
!
!     POINTER TO  3X3 S SUB I MATRIX
!
            Npt1 = 12 + (i-1)*9 + (ii-1)*54
!
            CALL gmmats(Ph1out(Npt1),3,3,0,Z(Npoint),3,1,0,Vec(1))
            DO j = 1 , 3
               stress(j) = stress(j) + Vec(j)
               str(j) = stress(j)
            ENDDO
         ENDDO
         IF ( Ldtemp/=(-1) ) THEN
            ii12 = ii*2 - 1
            IF ( ii/=4 ) Tem = Ti(ii12) - Ph1out(11)
            IF ( ii==4 ) Tem = (Ti(1)+Ti(2)+Ti(3)+Ti(4)+Ti(5)+Ti(6))/6.0 - Ph1out(11)
            DO i = 1 , 3
               stress(i) = stress(i) - Ph1out(227+i)*Tem
               str(i) = stress(i)
            ENDDO
         ENDIF
      ENDIF
      IF ( nph1ou(2)==0 ) THEN
         DO i = 1 , 9
            str(i) = 0.0E0
         ENDDO
      ELSE
!
!     COMPUTE PRINCIPAL STRESSES
!
!
!     8 LOCATIONS FOR STRESS AT A POINT AS FOLLOWS
!
!      1. ELEMENT ID
!      2. SIGMA X1
!      3. SIGMA Y1
!      4. SIGMA XY1
!      5. ANGLE OF ZERO SHEAR
!      6. SIGMA PRINCIPAL STRESS 1
!      7. SIGMA PRINCIPAL STRESS 2
!      8. TAU MAX
!
!     FOR EACH POINT, THESE VALUES ARE STORED IN STOUT(1-8,9-16,
!     17-24,25-32) ALSO IN LOCATIONS STR(1-7) EXCEPT THE ELEMENT ID
!     FINALLY, THESE VALUES ARE STORED IN PH1OUT(101-108,109-115,
!     116-122,123-129)
!
         Temp = stress(1) - stress(2)
         temp1 = sqrt((Temp/2.0E0)**2+stress(3)**2)
         str(7) = temp1
         Delta = (stress(1)+stress(2))/2.0
         str(5) = Delta + temp1
         str(6) = Delta - temp1
         Delta = 2.0E0*stress(3)
         IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
            str(4) = 0.0
         ELSE
            str(4) = atan2(Delta,Temp)*28.6478898E0
         ENDIF
      ENDIF
      ijk = (ii-1)*8
      stout(ijk+1) = Ph1out(1)
      DO i = 2 , 8
         stout(ijk+i) = str(i-1)
      ENDDO
   ENDDO
   DO i = 1 , 8
      Ph1out(100+i) = stout(i)
   ENDDO
   DO j = 1 , 3
      DO i = 1 , 7
         j1 = 108 + (j-1)*7 + i
         j2 = j*8 + i + 1
         Ph1out(j1) = stout(j2)
      ENDDO
   ENDDO
END SUBROUTINE strm62
