!*==pstq2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pstq2(Npts)
   USE c_pla32s
   USE c_pla3es
   USE c_pla3uv
   USE c_sout
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Npts
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: nph1ou
   INTEGER , DIMENSION(4) :: nsil
   REAL , DIMENSION(36) :: si
   REAL :: sigx1 , sigx2 , sigxy1 , sigxy2 , sigy1 , sigy2 , z1 , z2
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!  THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA3
!  FOR COMBINATION ELEMENTS
!
!     ****PHASE II OF STRESS DATA RECOVERY*********
!
!     NPTS = 3 IMPLIES STRIA1 OR STRIA2  (PHASE II)
!     NPTS = 4 IMPLIES SQUAD1 OR SQUAD2  (PHASE II)
!
!
   !>>>>EQUIVALENCE (Nsil(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(9))
!
! **********************************************************************
! **********************************************************************
!
!     PHASE I OUTPUT FROM THE PLATE IS THE FOLLWOING
!
!     PH1OUT(1)                        ELEMENT ID
!     PH1OUT(2 THRU 5)                 3 SILS AND DUMMY OR 4 SILS
!     PH1OUT(6)                        I
!     PH1OUT(7 THRU 8)                 Z1 AND Z2
!     PH1OUT(9 THRU 30*NPTS+8)         3 OR 4 S SUB I  5X6 ARRAYS
!
! **********************************************************************
!
!     PHASE I OUTPUT FROM THE MEMBRANE IS THE FOLLOWING
!     NOTE..BEGIN = 30*NPTS+8
!
!     PH1OUT(BEGIN + 1)                ELEMENT ID
!     PH1OUT(BEGIN + 2 THRU BEGIN + 5) 3 SILS AND DUMMY OR 4 SILS
!     PH1OUT(BEGIN + 6)                T SUB 0
!     PH1OUT(BEGIN + 7 THRU BEGIN + 9) S SUB T  3X1 ARRAY
!     PH1OUT(BEGIN + 10 THRU BEGIN + 9*NPTS+9) 3 OR 4 S SUB I 3X3 ARRAYS
!
! **********************************************************************
! **********************************************************************
!
!     THE ABOVE ELEMENTS ARE COMPOSED OF PLATES AND MEMBRANES...
!     SOME MAY ONLY CONTAIN PLATES WHILE OTHERS MAY ONLY CONTAIN
!     MEMBRANES.
!     A CHECK FOR A ZERO FIRST SIL IN THE PHASE I OUTPUT, WHICH
!     INDICATES WHETHER ONE OR THE OTHER HAS BEEN OMITTED, IS MADE BELOW
!
!
!
!     FIRST GET FORCE VECTOR FOR THE PLATE CONSIDERATION
!
!     M ,  M ,  M  ,  V ,  V
!      X    Y    XY    X    Y
!
!                                NPTS
!     THE  5X1 FORCE VECTOR = SUMMATION  (S )(U )
!                                I=1       I   I
!
!
!     ZERO OUT LOCAL STRESSES
!
   sigx1 = 0.0E0
   sigy1 = 0.0E0
   sigxy1 = 0.0E0
   sigx2 = 0.0E0
   sigy2 = 0.0E0
   sigxy2 = 0.0E0
!
   IF ( nsil(1)==0 ) THEN
      z1 = 0.0E0
      z2 = 0.0E0
   ELSE
!
!     FORM SUMMATION
!
      DO i = 1 , Npts
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
         npoint = ivec + nsil(i) - 1
!
         CALL gmmats(si(30*i-29),5,6,0,z(npoint),6,1,0,vec(1))
!
         DO j = 2 , 6
            forvec(j) = forvec(j) + vec(j-1)
         ENDDO
!
      ENDDO
!
!     FORCE VECTOR IS NOW COMPLETE
!
      z1 = ph1out(7)
      z2 = ph1out(8)
!
      z1ovri = -ph1out(7)/ph1out(6)
      z2ovri = -ph1out(8)/ph1out(6)
!
      sigx1 = forvec(2)*z1ovri
      sigy1 = forvec(3)*z1ovri
      sigxy1 = forvec(4)*z1ovri
      sigx2 = forvec(2)*z2ovri
      sigy2 = forvec(3)*z2ovri
!     *******************************
!
      sigxy2 = forvec(4)*z2ovri
   ENDIF
!
!     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
   IF ( nph1ou(30*Npts+10)/=0 ) THEN
!
!
!                        I=NPTS
!     STRESS VECTOR = ( SUMMATION(S )(U ) )
!                        I=1       I   I
!
      DO i = 1 , Npts
!
!     POINTER TO I-TH SIL IN PH1OUT
         npoint = 30*Npts + 9 + i
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
         npoint = ivec + nph1ou(npoint) - 1
!
!     POINTER TO S SUB I 3X3
         npt1 = 30*Npts + 9 + 9*i
!
         CALL gmmats(ph1out(npt1),3,3,0,z(npoint),3,1,0,vec(1))
!
         DO j = 1 , 3
            stress(j) = stress(j) + vec(j)
         ENDDO
!
      ENDDO
!
!
!     ADD MEMBRANE STRESSES TO PLATE STRESSES
!
      sigx1 = sigx1 + stress(1)
      sigy1 = sigy1 + stress(2)
      sigxy1 = sigxy1 + stress(3)
      sigx2 = sigx2 + stress(1)
      sigy2 = sigy2 + stress(2)
      sigxy2 = sigxy2 + stress(3)
   ENDIF
!
!     STRESS OUTPUT VECTOR IS THE FOLLOWING
!
!      1) ELEMENT ID
!      2) Z1 = FIBER DISTANCE 1
!      3) SIG X  1
!      4) SIG Y  1
!      5) SIG XY 1
!      6) ANGLE OF ZERO SHEAR AT Z1
!      7) SIG P1 AT Z1
!      8) SIG P2 AT Z1
!      9) TAU MAX = MAXIMUM SHEAR STRESS AT Z1
!
!     10) ELEMENT ID
!     11) Z2 = FIBER DISTANCE 2
!     12) SIG X  2
!     13) SIG Y  2
!     14) SIG XY 2
!     15) ANGLE OF ZERO SHEAR AT Z2
!     16) SIG P1 AT Z2
!     17) SIG P2 AT Z2
!     S7) SIG P2 AT Z2
!     18) TAU MAX = MAXIMUM SHEAR STRESS AT Z2
!
!
   IF ( nph1ou(2)==0 .AND. nph1ou(30*Npts+10)==0 ) THEN
      DO i = 2 , 18
         str(i) = 0.0E0
      ENDDO
   ELSE
!
!     COMPUTE PRINCIPAL STRESSES
!
      str(1) = ph1out(1)
      str(2) = z1
      str(3) = sigx1
      str(4) = sigy1
      str(5) = sigxy1
      str(10) = ph1out(1)
      str(11) = z2
      str(12) = sigx2
      str(13) = sigy2
      str(14) = sigxy2
!
      DO i = 3 , 12 , 9
         temp = str(i) - str(i+1)
         str(i+6) = sqrt((temp/2.0E0)**2+str(i+2)**2)
         delta = (str(i)+str(i+1))/2.0E0
         str(i+4) = delta + str(i+6)
         str(i+5) = delta - str(i+6)
         delta = 2.0E0*str(i+2)
         IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
            str(i+3) = 0.0E0
         ELSE
            str(i+3) = atan2(delta,temp)*28.6478898E0
         ENDIF
!
      ENDDO
   ENDIF
   str(1) = ph1out(1)
   str(10) = ph1out(1)
!
!
!     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
!
   DO i = 10 , 17
      str(i) = str(i+1)
   ENDDO
!
END SUBROUTINE pstq2
