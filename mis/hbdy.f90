
SUBROUTINE hbdy(Ecpt,Necpt,Iopt,Rvect,Ivect)
   IMPLICIT NONE
   REAL Consts(5) , Pi
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /condas/ Consts
   COMMON /xmssg / Ufm , Uwm
   INTEGER Iopt
   REAL Ecpt(36) , Rvect(16)
   INTEGER Ivect(5) , Necpt(5)
   REAL area , dx , dxyz(3) , dy , dz , temp , v(3)
   INTEGER flag , i , j , npts
!
!     THIS SUBROUTINE CALCULATES THE GEOMETRIC PROPERTIES OF THE VARIOUS
!     TYPES OF HBDY ELEMENTS. IOPT IS DESCRIBED BELOW
!
!     THE ECPT INPUT DATA IS
!
!     POSITION    DATA
!        1       EL ID
!        2       FLAG
!        3       SIL-1
!        4       SIL-2
!        5       SIL-3
!        6       SIL-4
!        7       SIL-5
!        8       SIL-6
!        9       SIL-7
!       10       SIL-8
!       11       VECTOR V1
!       12       VECTOR V2
!       13       VECTOR V3
!       14       ECPT14
!       15       MAT ID
!       16       A-FACTOR
!       17       EMISSIVITY
!       18       ABSORBTIVIY
!       19       R1
!       20       R2
!       21       CS-1
!       22       X1
!       23       Y1
!       24       Z1
!       25       CS-2
!       26       X2
!       27       Y2
!       28       Z2
!       29       CS-3
!       30       X3
!       31       Y3
!       32       Z3
!       33       CS-4
!       34       X4
!       35       Y4
!       36       Z4
!       37-52    NOT USED
!       53       AVG. EL. TEMP.
!
!     THE VALUE OF FLAG INDICATES THE TYPE OF ELEMENT
!
!       FLAG     TYPE
!       ****     ****
!        1       POINT
!        2       LINE
!        3       REV
!        4       TRIANGLE
!        5       QUADRILATERAL
!        6       ELLIPTIC CYLINDER
!        7       FTUBE
!
!
!     THE OUTPUT DATA IS PLACED IN  VECT AND IVECT
!         THE FORMATS ARE
!
!     POSITION
!          IOPT=  1             2
!      1        EL ID         EL ID
!      2        AREA          AREA
!      3        EMIS          SIL-1
!      4        ---           SIL-2
!      5        SIL-1         SIL-3
!      6        SIL-2         SIL-4
!      7        SIL-3         AREA-1
!      8        SIL-4         AREA-2
!      9        GFACT-1       AREA-3
!     10        GFACT-2       AREA-4
!     11        GFACT-3       N1X
!     12        GFACT-4       N1Y
!     13                      N1Z
!     14                      N2X  -  FOR FLAG = 6 ONLY
!     15                      N2Y  -
!     16                      N2Z  -
!
!
   !>>>>EQUIVALENCE (Consts(1),Pi) , (dxyz(1),dx) , (dxyz(2),dy) , (dxyz(3),dz)
!
!
   DO i = 1 , 16
      Rvect(i) = 0.0
      Ivect(i) = 0
   ENDDO
   Ivect(1) = Necpt(1)
   flag = Necpt(2)
   IF ( flag<=0 .OR. flag>7 ) GOTO 300
   IF ( flag==7 ) Ecpt(16) = Pi*(Ecpt(19)+Ecpt(20))
!
   IF ( flag==2 .OR. flag==7 ) THEN
!
!     FLAG = LINE
!
      Ivect(3) = Necpt(3)
      Ivect(4) = Necpt(4)
      npts = 2
      dx = Ecpt(26) - Ecpt(22)
      dy = Ecpt(27) - Ecpt(23)
      dz = Ecpt(28) - Ecpt(24)
!
      temp = dx**2 + dy**2 + dz**2
      IF ( temp<=1.0E-20 ) GOTO 300
!
!     AREA CALCULATIONS
!
      Rvect(2) = Ecpt(16)*sqrt(temp)
      Rvect(7) = Rvect(2)*0.5
      Rvect(8) = Rvect(7)
!
!     NORMAL VECTOR CALCULATIONS
!
      temp = (dx*Ecpt(11)+dy*Ecpt(12)+dz*Ecpt(13))/temp
      Rvect(11) = Ecpt(11) - temp*dx
      Rvect(12) = Ecpt(12) - temp*dy
      Rvect(13) = Ecpt(13) - temp*dz
!
!     NORMALIZE
!
      CALL sanorm(*100,Rvect(11))
   ELSEIF ( flag==3 ) THEN
!
!     TYPE= REV
!
      Ivect(3) = Necpt(3)
      Ivect(4) = Necpt(4)
      npts = 2
      dx = Ecpt(26) - Ecpt(22)
      dz = Ecpt(28) - Ecpt(24)
      temp = sqrt(dx**2+dz**2)*Pi
      IF ( temp<=1.0E-20 ) GOTO 300
      Rvect(7) = (2.0*Ecpt(22)+Ecpt(26))*temp/3.0
      Rvect(8) = (2.0*Ecpt(26)+Ecpt(22))*temp/3.0
      Rvect(2) = Rvect(7) + Rvect(8)
!
      temp = temp/Pi
      Rvect(11) = dz/temp
      Rvect(13) = -dx/temp
   ELSEIF ( flag==4 ) THEN
!
!     FLAG = AREA3
!
      Ivect(3) = Necpt(3)
      Ivect(4) = Necpt(4)
      Ivect(5) = Necpt(5)
      npts = 3
      dx = Ecpt(26) - Ecpt(22)
      dy = Ecpt(27) - Ecpt(23)
      dz = Ecpt(28) - Ecpt(24)
      Rvect(7) = Ecpt(30) - Ecpt(26)
      Rvect(8) = Ecpt(31) - Ecpt(27)
      Rvect(9) = Ecpt(32) - Ecpt(28)
!
!     CALC. NORMAL VECTOR
!
      CALL saxb(dxyz,Rvect(7),Rvect(11))
!
      CALL sanorm(*300,Rvect(11))
!
      Rvect(2) = temp/2.0
      Rvect(7) = temp/6.0
      Rvect(8) = Rvect(7)
!
      Rvect(9) = Rvect(7)
   ELSEIF ( flag==5 ) THEN
!
!     FLAG = AREA4
!
      DO i = 3 , 6
         Ivect(i) = Necpt(i)
      ENDDO
      npts = 4
      DO i = 1 , 3
!
!     CALCULATE  DIFFERENCE VECTORS
!
!        R2 - R1
!
         Rvect(i+6) = Ecpt(i+25) - Ecpt(i+21)
!
!        R3 - R1
!
         Rvect(i+13) = Ecpt(i+29) - Ecpt(i+21)
!
!        R4 - R2
!
         v(i) = Ecpt(i+33) - Ecpt(i+25)
      ENDDO
!
!        (R3 - R1) X (R4 - R2)
!
      CALL saxb(Rvect(14),v,Rvect(11))
!
!     2*AREA
!
      temp = sqrt(Rvect(11)**2+Rvect(12)**2+Rvect(13)**2)
      Rvect(2) = temp/2.0
!
!     NORMALIZE
!
      CALL sanorm(*300,Rvect(11))
!
      CALL saxb(Rvect(7),Rvect(14),dxyz)
!
!     AREA OF TRIANGLE 123
!
      temp = sqrt(dx**2+dy**2+dz**2)/2.0
!
      CALL saxb(Rvect(7),v,dxyz)
!
!     AREA OF TRIANGLE 412
!
      dx = sqrt(dx**2+dy**2+dz**2)/2.0
!
!     AREA FOR POINTS
!
      Rvect(7) = (Rvect(2)+dx)/6.0
      Rvect(8) = (Rvect(2)+temp)/6.0
      Rvect(9) = (Rvect(2)*2.-dx)/6.0
      Rvect(10) = (Rvect(2)*2.-temp)/6.0
      Rvect(14) = 0.0
      Rvect(15) = 0.0
      Rvect(16) = 0.0
      npts = 4
   ELSEIF ( flag==6 ) THEN
!
!     FLAG = ELCYL
!
      Ivect(3) = Necpt(3)
      Ivect(4) = Necpt(4)
      npts = 2
      dx = Ecpt(26) - Ecpt(22)
      dy = Ecpt(27) - Ecpt(23)
      dz = Ecpt(28) - Ecpt(24)
      temp = sqrt(dx**2+dy**2+dz**2)
      Rvect(2) = temp*Ecpt(16)
      IF ( Iopt==3 ) Rvect(2) = temp
      IF ( temp<=0 ) GOTO 300
      CALL saxb(Ecpt(11),dxyz,Rvect(14))
      CALL saxb(dxyz,Rvect(14),Rvect(11))
!
      CALL sanorm(*300,Rvect(11))
      CALL sanorm(*300,Rvect(14))
      DO i = 1 , 3
         Rvect(i+10) = Rvect(i+10)*Ecpt(20)
         Rvect(i+13) = Rvect(i+13)*Ecpt(19)
      ENDDO
      Rvect(7) = Rvect(2)/2.0
      Rvect(8) = Rvect(7)
   ELSE
!
!     FLAG = POINT
!
      Ivect(3) = Necpt(3)
      Rvect(7) = Ecpt(16)
      Rvect(2) = Ecpt(16)
      CALL sanorm(*100,Ecpt(11))
      npts = 1
   ENDIF
!
!     IOPT EQUALS 1
!     CALCULATE G FACTORS. STORE IN NEW LOCATIONS.
!     WORK FROM LAST TO FIRST
!
!     CHECK FOR ZERO AREA
!
 100  area = Rvect(2)
   IF ( area<1.0E-20 ) GOTO 300
 200  IF ( Iopt>1 ) THEN
!
!     IOPT EQUALS 2
!
      IF ( Iopt==2 ) RETURN
      DO i = 1 , npts
         Rvect(i+6) = Rvect(i+6)*Ecpt(18)
      ENDDO
      RETURN
   ELSE
      DO i = 1 , npts
         j = npts - i + 1
         Rvect(j+8) = Rvect(j+6)/area
      ENDDO
!
      DO i = 1 , 4
         j = 5 - i
         IF ( j<=npts ) THEN
            Ivect(j+4) = Ivect(j+2)
         ELSE
            Ivect(j+4) = 0
         ENDIF
      ENDDO
!
!     STORE EMISSIVITY VALUE
!
      Rvect(3) = Ecpt(17)
      RETURN
   ENDIF
!
 300  WRITE (6,99001) Uwm , Necpt(1)
99001 FORMAT (A25,' 2154, ZERO AREA OR ILLEGAL CONNECTION FOR HBDY ','ELEMENT NUMBER',I9)
   area = 1.0
   GOTO 200
END SUBROUTINE hbdy