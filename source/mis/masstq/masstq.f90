!*==masstq.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE masstq(Narg)
   IMPLICIT NONE
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2ht
   USE c_sma2io
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Narg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iflag
   INTEGER , DIMENSION(7) :: necpt
   REAL , SAVE :: pi23
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
!     COMMON /MATOUT/RHO
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   !>>>>EQUIVALENCE (Iflag,Ecpt(8))
   DATA pi23/2.0943952/
!
!     THIS ROUTINE COMPUTES A MASS MATRIX OF THE FOLLOWING FORM.
!
!                     TERM 0   0   0   0   0
!                      0  TERM 0   0   0   0
!                      0   0  TERM 0   0   0
!      MASS MATRIX =   0   0   0   0   0   0
!                      0   0   0   0   0   0
!                      0   0   0   0   0   0
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
!
!            -MQDMEM-      -MTRPLT-MTRMEM-      -MTWIST-
!            -MQUAD2-MQUAD1-MTRBSC-MTRIA2-MTRIA1-MSHEAR-MQDPLT-
   IF ( ntype==2 ) THEN
!
      ncsid = 16
      matid = necpt(7)
      t = ecpt(8)
      fmu = ecpt(13)
   ELSEIF ( ntype==3 ) THEN
!
      ncsid = 13
      matid = necpt(6)
      t = 0.0E0
      fmu = ecpt(10)
   ELSEIF ( ntype==4 ) THEN
!
      ncsid = 9
      matid = necpt(6)
      t = ecpt(7)
      fmu = ecpt(8)
   ELSEIF ( ntype==5 ) THEN
!
      ncsid = 15
      matid = necpt(6)
      t = ecpt(7)
      fmu = ecpt(12)
   ELSEIF ( ntype==6 ) THEN
      ncsid = 9
      matid = necpt(6)
      t = ecpt(7)
      fmu = ecpt(8)
   ELSEIF ( ntype==7 ) THEN
      ncsid = 14
      matid = necpt(7)
      t = 0.0E0
      fmu = ecpt(11)
   ELSE
!
      ncsid = 10
      matid = necpt(7)
      t = ecpt(8)
      fmu = ecpt(9)
   ENDIF
!
!  30 COMPUTE PIVOT TRIANGLE AREA
!
!     FIRST SET UP THE POINTERS TO THE CSID OF THE 3 POINTS FROM THE
!     BASE CSID
!
   npt1 = 0
   npt2 = 4
   npt3 = 8
   IF ( ntype>=3 .AND. ntype<=5 ) GOTO 200
   ichek = 1
!     SELECT 3 POINTS FOR THE PIVOT TRIANGLE OF A QUADRILATERAL
!     FIND PIVOT NUMBER FIRST
   DO i = 1 , 4
      IF ( npvt==necpt(i+1) ) THEN
         npivot = i
         GOTO 100
      ENDIF
   ENDDO
!
!     ERROR IF FALL THRU ABOVE LOOP
!
   CALL mesage(-30,34,ecpt(1))
   RETURN
!
!
 100  IF ( npivot<2 ) THEN
      npt3 = 12
   ELSEIF ( npivot/=2 ) THEN
      IF ( npivot==3 ) THEN
         npt1 = 12
      ELSE
         npt2 = 12
      ENDIF
   ENDIF
!
!     ABOVE LOGIC SETS THE 3 POINTS FOR THE PIVOT TRIANGLE OF A QUAD.
!
 200  DO i = 1 , 3
      isub1 = ncsid + npt1 + i
      isub2 = ncsid + npt2 + i
      isub3 = ncsid + npt3 + i
      v1(i) = ecpt(isub3) - ecpt(isub1)
      v2(i) = ecpt(isub3) - ecpt(isub2)
   ENDDO
!
!     COMPUTE AREA OF QUAD OR TRI USING V1 AND V2
   area = 0.0E0
   DO
!
      v1xv2(1) = v1(2)*v2(3) - v1(3)*v2(2)
      v1xv2(2) = v1(3)*v2(1) - v1(1)*v2(3)
      v1xv2(3) = v1(1)*v2(2) - v1(2)*v2(1)
!
      area = area + sqrt(v1xv2(1)**2+v1xv2(2)**2+v1xv2(3)**2)/2.0E0
!
      IF ( ntype>2 .AND. ntype<6 ) EXIT
      IF ( ichek==0 ) EXIT
!
!     COMPUTE AREA OF WHOLE QUAD, FIRST SET UP V1 + V2 THEN TRA TO 600.
!
      IF ( Narg==1 .AND. iflag==1 ) THEN
         isub1 = ncsid + npt1 + 1
         isub2 = ncsid + npt2 + 1
         isub3 = ncsid + npt3 + 1
         t = pi23*(ecpt(isub1)+ecpt(isub2)+ecpt(isub3))
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
         v1(i) = ecpt(npt1) - ecpt(npt3)
         v2(i) = ecpt(npt2) - ecpt(npt4)
      ENDDO
!
      ichek = 0
   ENDDO
!     ******************************************************************
!     FINAL COMPUTATION OF TERM AND SHIP OUT OF MATRIX.
!
   DO i = 1 , 36
      mass(i) = 0.0D0
   ENDDO
   IF ( t/=0 ) THEN
!     RHO NOT NEEDED IF T = 0
!
      inflag = 4
      IF ( heat ) THEN
!*****
!  HEAT FORMULATION.
!*****
         CALL hmat(ecpt)
         mass(1) = (cp*t)*area/3.0
         IF ( ntype<3 .OR. ntype>5 ) mass(1) = mass(1)/2.0D0
         CALL sma2b(mass(1),npvt,npvt,ifbgg,0.0D0)
         GOTO 99999
      ELSE
         CALL mat(ecpt(1))
      ENDIF
   ENDIF
!
!
   term = (fmu+rho*t)*area/3.0E0
   IF ( ntype<3 .OR. ntype>5 ) term = term/2.0E0
   mass(1) = term
   mass(8) = term
   mass(15) = term
!
   CALL sma2b(mass(1),npvt,-1,ifmgg,0.0D0)
!
99999 END SUBROUTINE masstq
