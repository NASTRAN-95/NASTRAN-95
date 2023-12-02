!*==presax.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE presax(Iharm)
   USE c_condas
   USE c_loadx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iharm
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: card
   REAL :: cossi , pr , prc , prpiez , prs , sinsi , xi , xl
   INTEGER :: file , i , iflag , ip1 , isila , isilb , j , n
   REAL , DIMENSION(4,2) :: gpco
   INTEGER , DIMENSION(6) :: icard
   INTEGER , DIMENSION(2) :: iord
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: piez
   EXTERNAL fndpnt , fndsil , mesage , permut , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE APPLIES PRESSURE LOADS TO AXISYMMETRIC SHELL
!
   !>>>>EQUIVALENCE (icard(1),card(1))
   DATA name/4HPRES , 4HAX  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF VARIABLES
!
!     N        NUMBER OF CURRENT HARMONIC
!     FILE     FILE NAME FOR ERROR MESAGES
!     SLT      STATIC LOADS TABLE
!     CARD     CARD IMAGE OF PRESAX CARD
!     DEGRAD   CONVERSION FACTOR FOR DEGREES TO RADIANS
!     IORD     ARRAY GIVING OPTIMUM ORDER FOR LOOKING UP POINTS IN BGPDT
!     OLD      CURRENT POSITION OF BGPDT
!     GPCO     ARRAY HOLDING BGPDT DATA FOR EACH RING
!     XL       DISTANCE  BETWEEN  RINGS
!     SINSI    SIN  ANGLE BETWEEN RINGS
!     COSSI    COS  ANGLE BETWEEN RINGS
!     ISILA    SIL VALUE  OF CURRENT HARMONIC - RING A
!     ISILB    SIL VALUE  OF CURRENT HARMONIC - RING B
!     IHARM    SUBCASE  INDICATOR  1 = SINE  2 = COSINE
!
!
!     BRING IN PRESAX CARD
!
         file = slt
         CALL read(*20,*40,slt,card(1),6,0,iflag)
         n = icard(6) + 1
         xi = n - 1
!
!     CONVERT PHI1,PHI2 TO RADIANS
!
         card(4) = card(4)*degrad
         card(5) = card(5)*degrad
!
!     PICK UP BGPDT DATA FOR RINGS
!
!     IF 1ST. RING IS NEGATIVE, THIS IS A SURFACE CHARGE LOAD IN A
!     PIEZOELECTRIC PROBLEM
!
         piez = .FALSE.
         IF ( ksystm(78)==1 .AND. icard(2)<=0 ) THEN
            piez = .TRUE.
            icard(2) = -icard(2)
         ENDIF
         CALL permut(icard(2),iord(1),2,old)
         DO i = 1 , 2
            j = iord(i) + 1
            CALL fndpnt(gpco(1,j-1),icard(j))
         ENDDO
         xl = sqrt((gpco(2,2)-gpco(2,1))**2+(gpco(3,2)-gpco(3,1))**2)
         IF ( xl==0.0 ) CALL mesage(-30,26,-1)
         sinsi = (gpco(2,2)-gpco(2,1))/xl
         cossi = (gpco(3,2)-gpco(3,1))/xl
         CALL fndsil(icard(2))
         isila = icard(2)
         CALL fndsil(icard(3))
         isilb = icard(3)
!
!     APPLY LOADS TO ALL HARMONICS
!
         IF ( n==1 ) THEN
!
!     APPLY LOADS TO ZERO HARMONIC - COSINE SUBCASE ONLY
!
            IF ( Iharm/=2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            pr = (card(5)-card(4))
!
!     I .GT. 1  APPLY  SINE AND COSINE FACTORS
!
         ELSEIF ( Iharm==1 ) THEN
!
!     SINE CASE
!
            pr = -(cos(xi*card(5))-cos(xi*card(4)))/xi
         ELSE
!
!     COSINE CASE
!
            pr = (sin(xi*card(5))-sin(xi*card(4)))/xi
         ENDIF
!
!     APPLY LOADS
!
         pr = pr*card(1)*xl
         prpiez = pr
         prc = pr*cossi
         prs = -pr*sinsi
         pr = gpco(2,1)/3.0 + gpco(2,2)/6.0
         IF ( piez ) THEN
!
!     PIEZOELECTRIC
!
            prc = 0.
            prs = 0.
         ENDIF
         z(isila) = z(isila) + prc*pr
         z(isila+2) = z(isila+2) + prs*pr
         IF ( piez ) z(isila+3) = z(isila+3) + prpiez*pr
         pr = gpco(2,2)/3.0 + gpco(2,1)/6.0
         z(isilb) = z(isilb) + prc*pr
         z(isilb+2) = z(isilb+2) + prs*pr
         IF ( piez ) z(isilb+3) = z(isilb+3) + prpiez*pr
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
!
!     FILE ERRORS
!
 20      ip1 = -2
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,file,name(1))
 40      ip1 = -3
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE presax
