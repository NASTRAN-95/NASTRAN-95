
SUBROUTINE presax(Iharm)
   IMPLICIT NONE
   REAL Bgpdt , Degrad , Old , Pi , Radeg , S4pisq , Twopi , Z(1)
   INTEGER Ksystm(80) , Lc , Slt
   COMMON /condas/ Pi , Twopi , Radeg , Degrad , S4pisq
   COMMON /loadx / Lc , Slt , Bgpdt , Old
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER Iharm
   REAL card(6) , cossi , gpco(4,2) , pr , prc , prpiez , prs , sinsi , xi , xl
   INTEGER file , i , icard(6) , iflag , iord(2) , ip1 , isila , isilb , j , n , name(2)
   LOGICAL piez
!
!     THIS ROUTINE APPLIES PRESSURE LOADS TO AXISYMMETRIC SHELL
!
   EQUIVALENCE (icard(1),card(1))
   DATA name/4HPRES , 4HAX  /
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
   file = Slt
   CALL read(*200,*400,Slt,card(1),6,0,iflag)
   n = icard(6) + 1
   xi = n - 1
!
!     CONVERT PHI1,PHI2 TO RADIANS
!
   card(4) = card(4)*Degrad
   card(5) = card(5)*Degrad
!
!     PICK UP BGPDT DATA FOR RINGS
!
!     IF 1ST. RING IS NEGATIVE, THIS IS A SURFACE CHARGE LOAD IN A
!     PIEZOELECTRIC PROBLEM
!
   piez = .FALSE.
   IF ( Ksystm(78)==1 .AND. icard(2)<=0 ) THEN
      piez = .TRUE.
      icard(2) = -icard(2)
   ENDIF
   CALL permut(icard(2),iord(1),2,Old)
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
      IF ( Iharm/=2 ) GOTO 100
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
   Z(isila) = Z(isila) + prc*pr
   Z(isila+2) = Z(isila+2) + prs*pr
   IF ( piez ) Z(isila+3) = Z(isila+3) + prpiez*pr
   pr = gpco(2,2)/3.0 + gpco(2,1)/6.0
   Z(isilb) = Z(isilb) + prc*pr
   Z(isilb+2) = Z(isilb+2) + prs*pr
   IF ( piez ) Z(isilb+3) = Z(isilb+3) + prpiez*pr
 100  RETURN
!
!     FILE ERRORS
!
 200  ip1 = -2
 300  CALL mesage(ip1,file,name(1))
 400  ip1 = -3
   GOTO 300
END SUBROUTINE presax
