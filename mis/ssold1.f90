
SUBROUTINE ssold1(Itype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alfa , Beta(8) , Cmat(18,8) , E , Ecpt(100) , Eltemp , Elvol , Fact , Floc , G , Ge(36) , H(4,4) , Nu , Phiout(170) , R(3,3)&
      & , Rho , Temp(18) , Tempo , Ti(9) , Vol
   INTEGER Itest , J1 , Jloc , Kpt , Matflg , Mfirst , Necpt(100) , Nel , Nmat , Nphi(170) , Npts , Nrow , Ntemp
   COMMON /matin / Nmat , Matflg , Eltemp
   COMMON /matout/ E , G , Nu , Rho , Alfa , Tempo
   COMMON /sdr2x5/ Ecpt , Phiout
   COMMON /sdr2x6/ Cmat , Beta , Temp , Elvol , Vol , Fact , Npts , Nel , Mfirst , Nrow , Itest , Floc , J1 , Jloc , Kpt , Ntemp ,  &
                 & Ge , H , R , Ti
!
! Dummy argument declarations
!
   INTEGER Itype
!
! Local variable declarations
!
   INTEGER i , icord , j , k , m(14,4) , me
!
! End of declarations
!
!*****
!
!  E C P T     TETRA          WEDGE          HEXA
!  -----------------------------------------------
!  ECPT( 1) =  EL ID          EL ID          EL ID
!  ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
!  ECPT( 3) =  GRID-1         GRID-1         GRID-1
!  ECPT( 4) =  GRID-2         GRID-2         GRID-2
!  ECPT( 5) =  GRID-3         GRID-3         GRID-3
!  ECPT( 6) =  GRID-4         GRID-4         GRID-4
!  ECPT( 7) =  CSID-1         GRID-5         GRID-5
!  ECPT( 8) =  X1             GRID-6         GRID-6
!  ECPT( 9) =  Y1             CSID-1         GRID-7
!  ECPT(10) =  Z1             X1             GRID-8
!  ECPT(11) =  CSID-2         Y1             CSID-1
!  ECPT(12) =  X2             Z1             X1
!  ECPT(13) =  Y2             CSID-2         Y1
!  ECPT(14) =  Z2             X2             Z1
!  ECPT(15) =  CSID-3         Y2             CSID-2
!  ECPT(16) =  X3             Z2             X2
!  ECPT(17) =  Y3             CSID-3         Y2
!  ECPT(18) =  Z3             X3             Z2
!  ECPT(19) =  CSID-4         Y3             CSID-3
!  ECPT(20) =  X4             Z3             X3
!  ECPT(21) =  Y4             CSID-4         Y3
!  ECPT(22) =  Z4             X4             Z3
!  ECPT(23) =  EL-TEM         Y4             CSID-4
!  ECPT(24)                   Z4             X4
!  ECPT(25)                   CSID-5         Y4
!  ECPT(26)                   X5             Z4
!  ECPT(27)                   Y5             CSID-5
!  ECPT(28)                   Z5             X5
!  ECPT(29)                   CSID-6         Y5
!  ECPT(30)                   X6             Z5
!  ECPT(31)                   Y6             CSID-6
!  ECPT(32)                   Z6             X6
!  ECPT(33)                   ELTEMP         Y6
!  ECPT(34)                                  Z6
!  ECPT(35)                                  CSID-7
!  ECPT(36)                                  X7
!  ECPT(37)                                  Y7
!  ECPT(38)
!  ECPT(39)                                  CSID-8
!  ECPT(40)                                  X8
!  ECPT(41)                                  Y8
!  ECPT(42)                                  Z8
!  ECPT(43)                                  EL-TEMP
!*****
!
!
   EQUIVALENCE (Nphi(1),Phiout(1))
   EQUIVALENCE (Necpt(1),Ecpt(1))
!
   DATA m(1,1) , m(1,2) , m(1,3) , m(1,4)/1 , 2 , 3 , 4/
!
   DATA m(2,1) , m(2,2) , m(2,3) , m(2,4)/1 , 2 , 3 , 6/
   DATA m(3,1) , m(3,2) , m(3,3) , m(3,4)/1 , 2 , 6 , 5/
   DATA m(4,1) , m(4,2) , m(4,3) , m(4,4)/1 , 4 , 5 , 6/
!
   DATA m(5,1) , m(5,2) , m(5,3) , m(5,4)/1 , 2 , 3 , 6/
   DATA m(6,1) , m(6,2) , m(6,3) , m(6,4)/1 , 3 , 4 , 8/
   DATA m(7,1) , m(7,2) , m(7,3) , m(7,4)/1 , 3 , 8 , 6/
   DATA m(8,1) , m(8,2) , m(8,3) , m(8,4)/1 , 5 , 6 , 8/
   DATA m(9,1) , m(9,2) , m(9,3) , m(9,4)/3 , 6 , 7 , 8/
   DATA m(10,1) , m(10,2) , m(10,3) , m(10,4)/2 , 3 , 4 , 7/
   DATA m(11,1) , m(11,2) , m(11,3) , m(11,4)/1 , 2 , 4 , 5/
   DATA m(12,1) , m(12,2) , m(12,3) , m(12,4)/2 , 4 , 5 , 7/
   DATA m(13,1) , m(13,2) , m(13,3) , m(13,4)/2 , 5 , 6 , 7/
   DATA m(14,1) , m(14,2) , m(14,3) , m(14,4)/4 , 5 , 7 , 8/
!
   IF ( Itype==2 ) THEN
      Npts = 6
      Nel = 3
      Mfirst = 2
   ELSEIF ( Itype==3 ) THEN
      Npts = 8
      Nel = 5
      Mfirst = 5
   ELSEIF ( Itype==4 ) THEN
      Npts = 8
      Nel = 10
      Mfirst = 5
   ELSE
!*****
!     THE TYPE OF THE ELEMENT DETERMINES THE FOLLOWING PARAMETERS
!*****
      Npts = 4
      Nel = 1
      Mfirst = 1
   ENDIF
!*****
!     ZERO OUT ARRAYS
!*****
   Elvol = 0.0
   DO j = 1 , Npts
      Beta(j) = 0.0
      DO i = 1 , 18
         Cmat(i,j) = 0.0
      ENDDO
   ENDDO
!*****
!     LOOP ON SUBELEMENTS
!*****
   DO me = 1 , Nel
      Nrow = Mfirst + me - 1
!*****
!     J  CORRESPONDS TO THE X,Y,AND Z LOCATIONS OF EACH CONNECTED POINT
!*****
      DO j = 1 , 3
         J1 = m(Nrow,1)*4 + Npts + j - 1
!*****
!     I  CORRESPONDS TO POINTS 2,3,AND 4
!*****
         DO i = 1 , 3
            Jloc = m(Nrow,i+1)*4 + Npts + j - 1
!*****
!     ECPT(JLOC) IS THE JTH COMPONENT OF POINT I+1
!*****
            R(i,j) = Ecpt(Jloc) - Ecpt(J1)
         ENDDO
      ENDDO
!*****
!     INVERT THE GEOMETRY MATRIX EXPLICITLY USING VECTOR OPERATORS
!*****
      CALL saxb(R(1,3),R(1,2),Temp)
      H(2,1) = Temp(1) + Temp(2) + Temp(3)
      H(2,2) = R(2,2)*R(3,3) - R(3,2)*R(2,3)
      H(2,3) = R(3,2)*R(1,3) - R(1,2)*R(3,3)
      H(2,4) = R(1,2)*R(2,3) - R(2,2)*R(1,3)
      CALL saxb(R(1,1),R(1,3),Temp)
      H(3,1) = Temp(1) + Temp(2) + Temp(3)
      H(3,2) = R(2,3)*R(3,1) - R(3,3)*R(2,1)
      H(3,3) = R(3,3)*R(1,1) - R(1,3)*R(3,1)
      H(3,4) = R(1,3)*R(2,1) - R(2,3)*R(1,1)
      CALL saxb(R(1,1),R(1,2),Temp)
      H(4,1) = -Temp(1) - Temp(2) - Temp(3)
      H(4,2) = R(2,1)*R(3,2) - R(3,1)*R(2,2)
      H(4,3) = R(3,1)*R(1,2) - R(1,1)*R(3,2)
      H(4,4) = R(1,1)*R(2,2) - R(2,1)*R(1,2)
      Vol = (R(1,3)*Temp(1)+R(2,3)*Temp(2)+R(3,3)*Temp(3))/6.0
      Elvol = Elvol + Vol
      DO i = 1 , 4
         Kpt = m(Nrow,i)
         Beta(Kpt) = Beta(Kpt) + Vol
         Cmat(1,Kpt) = H(2,i)/6.0 + Cmat(1,Kpt)
         Cmat(5,Kpt) = H(3,i)/6.0 + Cmat(5,Kpt)
         Cmat(9,Kpt) = H(4,i)/6.0 + Cmat(9,Kpt)
         Cmat(11,Kpt) = H(4,i)/6.0 + Cmat(11,Kpt)
         Cmat(12,Kpt) = H(3,i)/6.0 + Cmat(12,Kpt)
         Cmat(13,Kpt) = H(4,i)/6.0 + Cmat(13,Kpt)
         Cmat(15,Kpt) = H(2,i)/6.0 + Cmat(15,Kpt)
         Cmat(16,Kpt) = H(3,i)/6.0 + Cmat(16,Kpt)
         Cmat(17,Kpt) = H(2,i)/6.0 + Cmat(17,Kpt)
      ENDDO
   ENDDO
!*****
!     END OF ELEMENT LOOP
!*****
!*****
!     CMAT CONTAINS THE SUM OF THE STRAIN -DISPLACEMENT MATRICES
!                       TIMES THE VOLUME OF THE CONNECTED TETRAHEDRON
!
!     CALL THE MATERIAL  ROUTINE TO OBTAIN PARAMETERS
!*****
   Nmat = Necpt(2)
   Matflg = 1
   Eltemp = Ecpt(5*Npts+3)
   CALL mat(Ecpt(1))
   Fact = E/((1.0+Nu)*(1.0-2.0*Nu))
   DO i = 1 , 36
      Ge(i) = 0.0
   ENDDO
   Ge(1) = Fact*(1.0-Nu)
   Ge(2) = Fact*Nu
   Ge(3) = Ge(2)
   Ge(7) = Ge(2)
   Ge(8) = Ge(1)
   Ge(9) = Ge(2)
   Ge(13) = Ge(2)
   Ge(14) = Ge(2)
   Ge(15) = Ge(1)
   Ge(22) = G
   Ge(29) = G
   Ge(36) = G
!*****
!     EACH CMAT MATRIX IS PREEMULTIPLIED BY THE STRESS-STRAIN GE MATRIX
!         AND DIVIDED BY THE SUM OF THE VOLUMES.
!     IF NECESSARY THE MATRIX IS POST-MULTIPLIED BY A GLOBAL TRANSFORM T
!
!     LOOP ON GRID POINTS
!*****
   DO i = 1 , Npts
      Nphi(i+1) = Necpt(i+2)
      k = Npts + i + 8
      Phiout(k) = Beta(i)/(4.0*Elvol)
      icord = Npts + 4*i - 1
      DO j = 1 , 18
         Cmat(j,i) = Cmat(j,i)/Elvol
      ENDDO
      k = Npts*2 + 18*i - 9
      IF ( Necpt(icord)/=0 ) THEN
         CALL transs(Necpt(icord),Ti)
         CALL gmmats(Cmat(1,i),6,3,0,Ti,3,3,0,Temp)
         CALL gmmats(Ge,6,6,0,Temp,6,3,0,Phiout(k))
      ELSE
         CALL gmmats(Ge,6,6,0,Cmat(1,i),6,3,0,Phiout(k))
      ENDIF
   ENDDO
!
   Nphi(1) = Necpt(1)
   Phiout(Npts+2) = Tempo
   Temp(1) = Alfa
   Temp(2) = Alfa
   Temp(3) = Alfa
   Temp(4) = 0.0
   Temp(5) = 0.0
   Temp(6) = 0.0
!*****
!     THE THERMAL EXPANSION VECTOR IS MULTIPLIED BY THE STRESS-STRAIN
!       MATRIX,GE
!*****
   CALL gmmats(Ge,6,6,0,Temp(1),6,1,0,Phiout(Npts+3))
!*****
!     THE OUTPUT ARRAY IS NOW COMPLETE
!*****
!*****
!     PHIOUT CONTAINS THE FOLLOWING WHERE N IS THE NUMBER OF CORNERS
!
!              ELEMENT ID
!              N SILS
!              T SUB 0
!              6 THERMAL STRESS COEFFICIENTS
!              N VOLUME RATIO COEFFICIENTS
!              N 6 BY 3 MATRICES RELATING STRESS TO DISPLACEMENTS
!
!*****
END SUBROUTINE ssold1
