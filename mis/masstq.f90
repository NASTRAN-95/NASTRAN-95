
SUBROUTINE masstq(Narg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Area , Bggind , Cp , Dum4(10) , Dummy(504) , Dumxx(1) , Ecpt(100) , Eltemp , Fmu , Rho , T , Term , V1(3) , V1xv2(3) , V2(3)
   LOGICAL Heat
   INTEGER Ichek , Ifbgg , Iflag , Ifmgg , Inflag , Ioptb , Isub1 , Isub2 , Isub3 , Matid , Ncsid , Necpt(7) , Npivot , Npt1 ,      &
         & Npt2 , Npt3 , Npt4 , Npvt , Ntype
   DOUBLE PRECISION Mass(36)
   COMMON /hmtout/ Cp
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /matout/ Rho
   COMMON /sma2cl/ Ioptb , Bggind , Npvt
   COMMON /sma2dp/ Mass , V1 , V1xv2 , V2 , Term , T , Fmu , Npt1 , Npt3 , Npt2 , Npt4 , Isub1 , Isub3 , Isub2 , Ncsid , Ichek ,    &
                 & Ntype , Npivot , Area , Dummy
   COMMON /sma2et/ Ecpt
   COMMON /sma2ht/ Heat
   COMMON /sma2io/ Dum4 , Ifmgg , Dumxx , Ifbgg
!
! Dummy argument declarations
!
   INTEGER Narg
!
! Local variable declarations
!
   INTEGER i
   REAL pi23
!
! End of declarations
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
   EQUIVALENCE (Necpt(1),Ecpt(1))
   EQUIVALENCE (Iflag,Ecpt(8))
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
   Ntype = Narg
!
!            -MQDMEM-      -MTRPLT-MTRMEM-      -MTWIST-
!            -MQUAD2-MQUAD1-MTRBSC-MTRIA2-MTRIA1-MSHEAR-MQDPLT-
   IF ( Ntype==2 ) THEN
!
      Ncsid = 16
      Matid = Necpt(7)
      T = Ecpt(8)
      Fmu = Ecpt(13)
   ELSEIF ( Ntype==3 ) THEN
!
      Ncsid = 13
      Matid = Necpt(6)
      T = 0.0E0
      Fmu = Ecpt(10)
   ELSEIF ( Ntype==4 ) THEN
!
      Ncsid = 9
      Matid = Necpt(6)
      T = Ecpt(7)
      Fmu = Ecpt(8)
   ELSEIF ( Ntype==5 ) THEN
!
      Ncsid = 15
      Matid = Necpt(6)
      T = Ecpt(7)
      Fmu = Ecpt(12)
   ELSEIF ( Ntype==6 ) THEN
      Ncsid = 9
      Matid = Necpt(6)
      T = Ecpt(7)
      Fmu = Ecpt(8)
   ELSEIF ( Ntype==7 ) THEN
      Ncsid = 14
      Matid = Necpt(7)
      T = 0.0E0
      Fmu = Ecpt(11)
   ELSE
!
      Ncsid = 10
      Matid = Necpt(7)
      T = Ecpt(8)
      Fmu = Ecpt(9)
   ENDIF
!
!  30 COMPUTE PIVOT TRIANGLE AREA
!
!     FIRST SET UP THE POINTERS TO THE CSID OF THE 3 POINTS FROM THE
!     BASE CSID
!
   Npt1 = 0
   Npt2 = 4
   Npt3 = 8
   IF ( Ntype>=3 .AND. Ntype<=5 ) GOTO 200
   Ichek = 1
!     SELECT 3 POINTS FOR THE PIVOT TRIANGLE OF A QUADRILATERAL
!     FIND PIVOT NUMBER FIRST
   DO i = 1 , 4
      IF ( Npvt==Necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
!
!     ERROR IF FALL THRU ABOVE LOOP
!
   CALL mesage(-30,34,Ecpt(1))
   RETURN
!
!
 100  IF ( Npivot<2 ) THEN
      Npt3 = 12
   ELSEIF ( Npivot/=2 ) THEN
      IF ( Npivot==3 ) THEN
         Npt1 = 12
      ELSE
         Npt2 = 12
      ENDIF
   ENDIF
!
!     ABOVE LOGIC SETS THE 3 POINTS FOR THE PIVOT TRIANGLE OF A QUAD.
!
 200  DO i = 1 , 3
      Isub1 = Ncsid + Npt1 + i
      Isub2 = Ncsid + Npt2 + i
      Isub3 = Ncsid + Npt3 + i
      V1(i) = Ecpt(Isub3) - Ecpt(Isub1)
      V2(i) = Ecpt(Isub3) - Ecpt(Isub2)
   ENDDO
!
!     COMPUTE AREA OF QUAD OR TRI USING V1 AND V2
   Area = 0.0E0
   DO
!
      V1xv2(1) = V1(2)*V2(3) - V1(3)*V2(2)
      V1xv2(2) = V1(3)*V2(1) - V1(1)*V2(3)
      V1xv2(3) = V1(1)*V2(2) - V1(2)*V2(1)
!
      Area = Area + sqrt(V1xv2(1)**2+V1xv2(2)**2+V1xv2(3)**2)/2.0E0
!
      IF ( Ntype>2 .AND. Ntype<6 ) EXIT
      IF ( Ichek==0 ) EXIT
!
!     COMPUTE AREA OF WHOLE QUAD, FIRST SET UP V1 + V2 THEN TRA TO 600.
!
      IF ( Narg==1 .AND. Iflag==1 ) THEN
         Isub1 = Ncsid + Npt1 + 1
         Isub2 = Ncsid + Npt2 + 1
         Isub3 = Ncsid + Npt3 + 1
         T = pi23*(Ecpt(Isub1)+Ecpt(Isub2)+Ecpt(Isub3))
      ENDIF
      Npt1 = Ncsid
      Npt2 = Ncsid + 4
      Npt3 = Ncsid + 8
      Npt4 = Ncsid + 12
      DO i = 1 , 3
         Npt1 = Npt1 + 1
         Npt2 = Npt2 + 1
         Npt3 = Npt3 + 1
         Npt4 = Npt4 + 1
         V1(i) = Ecpt(Npt1) - Ecpt(Npt3)
         V2(i) = Ecpt(Npt2) - Ecpt(Npt4)
      ENDDO
!
      Ichek = 0
   ENDDO
!     ******************************************************************
!     FINAL COMPUTATION OF TERM AND SHIP OUT OF MATRIX.
!
   DO i = 1 , 36
      Mass(i) = 0.0D0
   ENDDO
   IF ( T/=0 ) THEN
!     RHO NOT NEEDED IF T = 0
!
      Inflag = 4
      IF ( Heat ) THEN
!*****
!  HEAT FORMULATION.
!*****
         CALL hmat(Ecpt)
         Mass(1) = (Cp*T)*Area/3.0
         IF ( Ntype<3 .OR. Ntype>5 ) Mass(1) = Mass(1)/2.0D0
         CALL sma2b(Mass(1),Npvt,Npvt,Ifbgg,0.0D0)
         GOTO 99999
      ELSE
         CALL mat(Ecpt(1))
      ENDIF
   ENDIF
!
!
   Term = (Fmu+Rho*T)*Area/3.0E0
   IF ( Ntype<3 .OR. Ntype>5 ) Term = Term/2.0E0
   Mass(1) = Term
   Mass(8) = Term
   Mass(15) = Term
!
   CALL sma2b(Mass(1),Npvt,-1,Ifmgg,0.0D0)
!
   RETURN
99999 RETURN
END SUBROUTINE masstq
