
SUBROUTINE msolid(Itype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Costh , Cp , Dum1(2) , Dum2(10) , Dumxx(1) , Ecpt(100) , Eltemp , Rho , Sinth , Stress
   DOUBLE PRECISION Emass , Mge(36) , Ptmass , R(3,3)
   LOGICAL Heat
   INTEGER Ifbgg , Ifmgg , Iloc(4) , Itest , J1 , Jloc , Kpt , M1 , Matflg , Matidc , Mfirst , Necpt(100) , Nel , Npts , Npvt , Nrow
   COMMON /hmtout/ Cp
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Rho
   COMMON /sma2cl/ Dum1 , Npvt
   COMMON /sma2dp/ Ptmass , Emass , R , Mge , Npts , Nel , Mfirst , Kpt , Nrow , Jloc , Itest , J1 , M1 , Iloc
   COMMON /sma2et/ Ecpt
   COMMON /sma2ht/ Heat
   COMMON /sma2io/ Dum2 , Ifmgg , Dumxx , Ifbgg
!
! Dummy argument declarations
!
   INTEGER Itype
!
! Local variable declarations
!
   INTEGER i , j , m(14,4) , me , ntemp
!
! End of declarations
!
!
!     THIS ROUTINE CALCULATES THE MASS MATRICES FOR THE SOLID ELEMENTS,
!
!          I =     ELEMENT
!          ***     *******
!          1       CTETRA
!          2       CWEDGE
!          3       CHEXA1
!          4       CHEXA2
!
!     A SERIES OF 6 BY 6 DIAGONAL MATRICES ARE CALUCLATED, ONE PER
!     CONNECTED GRID POINT
!
!     ECPT        TETRA          WEDGE          HEXA
!     -------------------------------------------------
!     ECPT( 1) =  EL ID          EL ID          EL ID
!     ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
!     ECPT( 3) =  GRID-1         GRID-1         GRID-1
!     ECPT( 4) =  GRID-2         GRID-2         GRID-2
!     ECPT( 5) =  GRID-3         GRID-3         GRID-3
!     ECPT( 6) =  GRID-4         GRID-4         GRID-4
!     ECPT( 7) =  CSID-1         GRID-5         GRID-5
!     ECPT( 8) =  X1             GRID-6         GRID-6
!     ECPT( 9) =  Y1             CSID-1         GRID-7
!     ECPT(10) =  Z1             X1             GRID-8
!     ECPT(11) =  CSID-2         Y1             CSID-1
!     ECPT(12) =  X2             Z1             X1
!     ECPT(13) =  Y2             CSID-2         Y1
!     ECPT(14) =  Z2             X2             Z1
!     ECPT(15) =  CSID-3         Y2             CSID-2
!     ECPT(16) =  X3             Z2             X2
!     ECPT(17) =  Y3             CSID-3         Y2
!     ECPT(18) =  Z3             X3             Z2
!     ECPT(19) =  CSID-4         Y3             CSID-3
!     ECPT(20) =  X4             Z3             X3
!     ECPT(21) =  Y4             CSID-4         Y3
!     ECPT(22) =  Z4             X4             Z3
!     ECPT(23) =  EL-TEM         Y4             CSID-4
!     ECPT(24)                   Z4             X4
!     ECPT(25)                   CSID-5         Y4
!     ECPT(26)                   X5             Z4
!     ECPT(27)                   Y5             CSID-5
!     ECPT(28)                   Z5             X5
!     ECPT(29)                   CSID-6         Y5
!     ECPT(30)                   X6             Z5
!     ECPT(31)                   Y6             CSID-6
!     ECPT(32)                   Z6             X6
!     ECPT(33)                   ELTEMP         Y6
!     ECPT(34)                                  Z6
!     ECPT(35)                                  CSID-7
!     ECPT(36)                                  X7
!     ECPT(37)                                  Y7
!     ECPT(38)
!     ECPT(39)                                  CSID-8
!     ECPT(40)                                  X8
!     ECPT(41)                                  Y8
!     ECPT(42)                                  Z8
!     ECPT(43)                                  EL-TEMP
!
   EQUIVALENCE (Ecpt(1),Necpt(1))
   DATA m(1,1) , m(1,2) , m(1,3) , m(1,4)/1 , 2 , 3 , 4/
   DATA m(2,1) , m(2,2) , m(2,3) , m(2,4)/1 , 2 , 3 , 6/
   DATA m(3,1) , m(3,2) , m(3,3) , m(3,4)/1 , 2 , 6 , 5/
   DATA m(4,1) , m(4,2) , m(4,3) , m(4,4)/1 , 4 , 5 , 6/
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
!     SET THE ELEMENT PARAMETERS ACCORDING TO THE TYPE
!                               NPTS = NO. OF CONNECTED POINTS
!                               NEL  = NO. OF SUBELEMENTS
!                               MFIRST=POSITION OF FIRST ROW OF MAPPING
!                                       MATRIX
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
      Npts = 4
      Nel = 1
      Mfirst = 1
   ENDIF
!
!     FETCH THE MATERIAL ID AND THE DENSITY, RHO
!
   Matidc = Necpt(2)
   Matflg = 4
   ntemp = 5*Npts + 3
   Eltemp = Ecpt(ntemp)
   IF ( .NOT.Heat ) CALL mat(Ecpt(1))
   IF ( Heat ) CALL hmat(Ecpt)
   IF ( Heat ) Rho = Cp
   IF ( Rho/=0.0 ) THEN
!
!     ZERO OUT POINT MASS
!
      Ptmass = 0.0D0
!
!     LOOP ON SUBELEMENTS
!
      DO me = 1 , Nel
         Nrow = Mfirst + me - 1
!
!     SET UP POINTERS TO LOCATION VECTORS AND TEST IF ELEMENT IS
!     CONNECTED
!
         Itest = 0
         DO i = 1 , 4
            Kpt = m(Nrow,i)
            IF ( Necpt(Kpt+2)==Npvt ) Itest = 1
!
!     THE LOCATION OF THE VECTOR DATA IN THE ECPT IS
!
            Iloc(i) = 4*Kpt + Npts
         ENDDO
         IF ( Itest/=0 ) THEN
!
!     CALCULATE DIFFERENCE VECTORS FROM THE FIRST VECTOR
!
            DO i = 2 , 4
               DO j = 1 , 3
                  Jloc = Iloc(i) + j - 1
                  J1 = Iloc(1) + j - 1
                  R(i-1,j) = Ecpt(Jloc) - Ecpt(J1)
               ENDDO
            ENDDO
!
!     THE MASS ON EACH POINT DUE TO THE TETRAHEDRON IS
!     (NEGATIVE VALUE OF RHO IS ALLOWED)
!
            Emass = Rho/24.D0*dabs((R(3,1)*(R(1,2)*R(2,3)-R(1,3)*R(2,2))+R(3,2)*(R(1,3)*R(2,1)-R(1,1)*R(2,3))+R(3,3)                &
                  & *(R(1,1)*R(2,2)-R(1,2)*R(2,1))))
            IF ( Itype==4 ) Emass = Emass/2.0D0
!
!     THE MASS IS NOW ADDED TO THE APPROPRIATE POINT
!
            Ptmass = Ptmass + Emass
         ENDIF
      ENDDO
!
!     THE MASSES ARE EXPANDED AND INSERTED
!
      IF ( Heat ) THEN
         CALL sma2b(Ptmass,Npvt,Npvt,Ifbgg,0.0D0)
      ELSE
         DO i = 1 , 36
            Mge(i) = 0.0D0
         ENDDO
         M1 = -1
         Mge(1) = Ptmass
         Mge(8) = Mge(1)
         Mge(15) = Mge(1)
         CALL sma2b(Mge(1),Npvt,M1,Ifmgg,0.0D0)
      ENDIF
   ENDIF
!
!     ALL DONE
!
END SUBROUTINE msolid
