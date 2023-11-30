
SUBROUTINE emadtq(Narg,Mass)
   IMPLICIT NONE
   REAL Cp , Ecpt(100) , Eltemp , Rho
   INTEGER Heat , Iflag , Inflag , Ksystm(55) , Matid , Necpt(7)
   COMMON /emgest/ Ecpt
   COMMON /hmtout/ Cp
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /matout/ Rho
   COMMON /system/ Ksystm , Heat
   INTEGER Narg
   DOUBLE PRECISION Mass(100)
   DOUBLE PRECISION area , fmu , rhod , t , term , v1(3) , v1xv2(3) , v2(3)
   REAL cpd , pi23
   INTEGER i , i1 , i2 , ichek , isub1 , isub2 , isub3 , ncsid , ndof , ngrids , npt1 , npt2 , npt3 , npt4 , npvt , ntype
!  THE EMG MASS DOUBLE PRECISION ROUTINE FOR TRI S, QUAD S, TWIST +
!     SHEAR ELEMENTS
!
!  THIS SUBROUTINE CALCULATES THE MASS MATRIX FOR THE ELEMENTS LISTED
!   BELOW
!
!   NOTE THAT THE OUTPUT MASS MATRIX IS NOT ORDERED BY INCREASING SIL
!     DOUBLE PRECISION VERSION
!
!     ******************************************************************
!     E C P T    L I S T I N G S
!     **************************
!         MTWIST              MQDMEM                        MTRMEM
!         MSHEAR    MQUAD1    MQUAD2    MTRIA1    MTRBSC    MTRIA2
! **********************************************************************
! ECPT( 1)ELEM. ID  ELEM. ID  ELEM. ID  ELEM. ID  ELEM. ID  ELEM. ID
! ECPT( 2)GR.PT. A  GR.PT. A  GR.PT. A  GR.PT. A  GR.PT. A  GR.PT. A
! ECPT( 3)GR.PT. B  GR.PT. B  GR.PT. B  GR.PT. B  GR.PT. B  GR.PT. B
! ECPT( 4)GR.PT. C  GR.PT. C  GR.PT. C  GR.PT. C  GR.PT. C  GR.PT. C
! ECPT( 5)GR.PT. D  GR.PT. D  GR.PT. D  THETA     THETA     THETA
! ECPT( 6)MAT ID    THETA     THETA     MAT ID 1  MAT ID 1  MAT ID
! ECPT( 7)T         MAT ID 1  MAT ID    T1        I         T
! ECPT( 8)N S MASS  T1        T         MAT ID 2  MAT ID 2  NS MASS
! ECPT( 9)CSID 1    MAT ID 2  N S MASS  I         T2        CSID 1
! ECPT(10)X1        I         CSID 1    MAT ID 3  N S MASS  X1
! ECPT(11)Y1        MAT ID 3  X1        T2        Z1        Y1
! ECPT(12)Z1        T2        Y1        N S MASS  Z2        Z1
! ECPT(13)CSID 2    N S MASS  Z1        Z1        CSID 1    CSID 2
! ECPT(14)X2        Z1        CSID 2    Z2        X1        X2
! ECPT(15)Y2        Z2        X2        CSID 1    Y1        Y2
! ECPT(16)Z2        CSID 1    Y2        X1        Z1        Z2
! ECPT(17)CSID 3    X1        Z2        Y1        CSID 2    CSID 3
! ECPT(18)X3        Y1        CSID 3    Z1        X2        X3
! ECPT(19)Y3        Z1        X3        CSID 2    Y2        Y3
! ECPT(20)Z3        CSID 2    Y3        X2        Z2        Z3
! ECPT(21)CSID 4    X2        Z3        Y2        CSID 3    TEMP
! ECPT(22)X4        Y2        CSID 4    Z2        X3
! ECPT(23)Y4        Z2        X4        CSID 3    Y3
! ECPT(24)Z4        CSID 3    Y4        X3        Z3
! ECPT(25)TEMP      X3        Z4        Y3        TEMP
! ECPT(26)          Y3        TEMP      Z3
! ECPT(27)          Z3                  TEMP
! ECPT(28)          CSID 4
! ECPT(29)          X4
! ECPT(30)          Y4
! ECPT(31)          Z4
! ECPT(32)          TEMP
! **********************************************************************
!
!
!
!
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (Iflag,Ecpt(8))
   DATA pi23/2.0943952/
!
!     THIS ROUTINE COMPUTES A MASS MATRIX OF THE FOLLOWING FORM.
!
!   MASS MATRIX = (T1,T1,T1,T2,T2,T2,T3,T3,T3,IF REQ-D T4,T4,T4)  )
!
!                   *******************
!                   NTYPE = 1  -MQDMEM-
!                   NTYPE = 1  -MQUAD2-
!                   NTYPE = 2  -MQUAD1-
!                   NTYPE = 3  -MTRBSC-
!                   NTYPE = 3  -MTRPLT-
!                   NTYPE = 4  -MTRMEM-
!                   NTYPE = 4  -MTRIA2-
!                   NTYPE = 5  -MTRIA1-
!                   NTYPE = 6  -MSHEAR-
!                   NTYPE = 6  -MTWIST-
!                   NTYPE = 7  -MQDPLT-
!                   *******************
!
   ntype = Narg
   ndof = 3
!
!            -MQDMEM-      -MTRPLT-MTRMEM-      -MTWIST-
!            -MQUAD2-MQUAD1-MTRBSC-MTRIA2-MTRIA1-MSHEAR-MQDPLT-
   IF ( ntype==2 ) THEN
!
      ncsid = 16
      ngrids = 4
      Matid = Necpt(7)
      t = Ecpt(8)
      fmu = Ecpt(13)
   ELSEIF ( ntype==3 ) THEN
!
      ncsid = 13
      ngrids = 3
      Matid = Necpt(6)
      t = 0.0E0
      fmu = Ecpt(10)
   ELSEIF ( ntype==4 ) THEN
!
      ncsid = 9
      ngrids = 3
      Matid = Necpt(6)
      t = Ecpt(7)
      fmu = Ecpt(8)
   ELSEIF ( ntype==5 ) THEN
!
      ncsid = 15
      ngrids = 3
      Matid = Necpt(6)
      t = Ecpt(7)
      fmu = Ecpt(12)
   ELSEIF ( ntype==6 ) THEN
      ncsid = 9
      ngrids = 4
      Matid = Necpt(6)
      t = Ecpt(7)
      fmu = Ecpt(8)
   ELSEIF ( ntype==7 ) THEN
      ncsid = 14
      ngrids = 4
      Matid = Necpt(7)
      t = 0.0E0
      fmu = Ecpt(11)
   ELSE
!
      ncsid = 10
      ngrids = 4
      Matid = Necpt(7)
      t = Ecpt(8)
      fmu = Ecpt(9)
   ENDIF
!
!  30 COMPUTE PIVOT TRIANGLE AREA
!
!     FIRST SET UP THE POINTERS TO THE CSID OF THE 3 POINTS FROM THE
!     BASE CSID
!
   DO npvt = 1 , ngrids
      npt1 = 0
      npt2 = 4
      npt3 = 8
      IF ( ngrids/=3 ) THEN
         ichek = 1
!     SELECT 3 POINTS FOR THE PIVOT TRIANGLE OF A QUADRILATERAL
         IF ( npvt==2 ) THEN
         ELSEIF ( npvt==3 ) THEN
            npt1 = 12
         ELSEIF ( npvt==4 ) THEN
            npt2 = 12
         ELSE
            npt3 = 12
         ENDIF
      ENDIF
!
      DO i = 1 , 3
         isub1 = ncsid + npt1 + i
         isub2 = ncsid + npt2 + i
         isub3 = ncsid + npt3 + i
         v1(i) = Ecpt(isub3) - Ecpt(isub1)
         v2(i) = Ecpt(isub3) - Ecpt(isub2)
      ENDDO
!
!     COMPUTE AREA OF QUAD OR TRI USING V1 AND V2
      area = 0.D0
      DO
!
         v1xv2(1) = v1(2)*v2(3) - v1(3)*v2(2)
         v1xv2(2) = v1(3)*v2(1) - v1(1)*v2(3)
         v1xv2(3) = v1(1)*v2(2) - v1(2)*v2(1)
!
         area = area + dsqrt(v1xv2(1)**2+v1xv2(2)**2+v1xv2(3)**2)/2.D0
!
         IF ( ngrids==3 ) EXIT
         IF ( ichek==0 ) EXIT
!
!     COMPUTE AREA OF WHOLE QUAD, FIRST SET UP V1 + V2 THEN TRA TO 600.
!
         IF ( Narg==1 .AND. Iflag==1 ) THEN
            isub1 = ncsid + npt1 + 1
            isub2 = ncsid + npt2 + 1
            isub3 = ncsid + npt3 + 1
            t = pi23*(Ecpt(isub1)+Ecpt(isub2)+Ecpt(isub3))
         ENDIF
         npt1 = ncsid
         npt2 = ncsid + 4
         npt3 = ncsid + 8
         npt4 = ncsid + 12
         DO i = 1 , 3
            npt1 = npt1 + 1
            npt2 = npt2 + 1
            npt3 = npt3 + 1
            npt4 = npt4 + 1
            v1(i) = Ecpt(npt1) - Ecpt(npt3)
            v2(i) = Ecpt(npt2) - Ecpt(npt4)
         ENDDO
!
         ichek = 0
      ENDDO
!     ******************************************************************
!     FINAL COMPUTATION OF TERM AND SHIP OUT OF MATRIX.
!
      IF ( t/=0 ) THEN
!     RHO NOT NEEDED IF T = 0
!
         Inflag = 4
         IF ( Heat==1 ) THEN
!
!      HEAT FORMULATION
!
            CALL hmat(Ecpt)
            cpd = Cp
            Mass(npvt) = (cpd*t)*area/3.D0
            IF ( ngrids==4 ) Mass(npvt) = Mass(npvt)/2.
            CYCLE
         ELSE
            CALL mat(Ecpt(1))
            rhod = Rho
         ENDIF
      ENDIF
!
!
      term = (fmu+rhod*t)*area/3.D0
      IF ( ngrids==4 ) term = term/2.
      i1 = (npvt-1)*3 + 1
      i2 = i1 + 2
      DO i = i1 , i2
         Mass(i) = term
      ENDDO
!
   ENDDO
END SUBROUTINE emadtq