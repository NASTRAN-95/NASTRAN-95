!*==msolid.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE msolid(Itype)
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2ht
   USE c_sma2io
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , me , ntemp
   INTEGER , DIMENSION(14,4) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL hmat , mat , sma2b
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
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
      npts = 6
      nel = 3
      mfirst = 2
   ELSEIF ( Itype==3 ) THEN
      npts = 8
      nel = 5
      mfirst = 5
   ELSEIF ( Itype==4 ) THEN
      npts = 8
      nel = 10
      mfirst = 5
   ELSE
      npts = 4
      nel = 1
      mfirst = 1
   ENDIF
!
!     FETCH THE MATERIAL ID AND THE DENSITY, RHO
!
   matidc = necpt(2)
   matflg = 4
   ntemp = 5*npts + 3
   eltemp = ecpt(ntemp)
   IF ( .NOT.heat ) CALL mat(ecpt(1))
   IF ( heat ) CALL hmat(ecpt)
   IF ( heat ) rho = cp
   IF ( rho/=0.0 ) THEN
!
!     ZERO OUT POINT MASS
!
      ptmass = 0.0D0
!
!     LOOP ON SUBELEMENTS
!
      DO me = 1 , nel
         nrow = mfirst + me - 1
!
!     SET UP POINTERS TO LOCATION VECTORS AND TEST IF ELEMENT IS
!     CONNECTED
!
         itest = 0
         DO i = 1 , 4
            kpt = m(nrow,i)
            IF ( necpt(kpt+2)==npvt ) itest = 1
!
!     THE LOCATION OF THE VECTOR DATA IN THE ECPT IS
!
            iloc(i) = 4*kpt + npts
         ENDDO
         IF ( itest/=0 ) THEN
!
!     CALCULATE DIFFERENCE VECTORS FROM THE FIRST VECTOR
!
            DO i = 2 , 4
               DO j = 1 , 3
                  jloc = iloc(i) + j - 1
                  j1 = iloc(1) + j - 1
                  r(i-1,j) = ecpt(jloc) - ecpt(j1)
               ENDDO
            ENDDO
!
!     THE MASS ON EACH POINT DUE TO THE TETRAHEDRON IS
!     (NEGATIVE VALUE OF RHO IS ALLOWED)
!
            emass = rho/24.D0*dabs((r(3,1)*(r(1,2)*r(2,3)-r(1,3)*r(2,2))+r(3,2)*(r(1,3)*r(2,1)-r(1,1)*r(2,3))+r(3,3)                &
                  & *(r(1,1)*r(2,2)-r(1,2)*r(2,1))))
            IF ( Itype==4 ) emass = emass/2.0D0
!
!     THE MASS IS NOW ADDED TO THE APPROPRIATE POINT
!
            ptmass = ptmass + emass
         ENDIF
      ENDDO
!
!     THE MASSES ARE EXPANDED AND INSERTED
!
      IF ( heat ) THEN
         CALL sma2b(ptmass,npvt,npvt,ifbgg,0.0D0)
      ELSE
         DO i = 1 , 36
            mge(i) = 0.0D0
         ENDDO
         m1 = -1
         mge(1) = ptmass
         mge(8) = mge(1)
         mge(15) = mge(1)
         CALL sma2b(mge(1),npvt,m1,ifmgg,0.0D0)
      ENDIF
   ENDIF
!
!     ALL DONE
!
END SUBROUTINE msolid
