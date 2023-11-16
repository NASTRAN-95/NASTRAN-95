
SUBROUTINE drod
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION A , Alpha , Avgltp , Delta , Diff(3) , Dpterm , Dz(1) , E , Fx , Ke(36) , Ta(9) , Tb(9) , Tsub0 , Ua(6) ,       &
                  & Ub(6) , X , Xl , Xm(3) , Xn(3) , Y , Yl , Yvec(3) , Yyt(18) , Z , Zl , Zvec(3) , Zzt(9)
   REAL Alphas , Clsrw , Costh , Cstm , Dit , Ecpt(100) , Ecptds , Eltemp , Eor , Es , Frowic , G , Gpct , Gsube , Outrw , Rho ,    &
      & Rz(1) , Sigc , Sigs , Sigt , Sinth , Stress , Tsub0s
   INTEGER I6x6k , Icstm , Iecpt(19) , Igpct , Inrw , Ipoint , Iz(1) , Jmax , Kggd , Link(10) , Lrowic , Matflg , Matidc , Mpt ,    &
         & N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Nu
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , Cstm , Mpt , Dit , Ecptds , Gpct ,      &
                 & Kggd , Inrw , Outrw , Eor , Neor , Clsrw , Jmax , Frowic , Lrowic , Nrowsc , Nlinks , Link , Nogo
   COMMON /ds1adp/ X , Y , Z , Xl , Xn , Ke , Ta , Tb , A , E , Alpha , Tsub0 , Ua , Ub , Diff , Dpterm , Delta , Avgltp , Fx , Xm ,&
                 & Yyt , Yvec , Zvec , Yl , Zl
   COMMON /ds1aet/ Ecpt
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Es , G , Nu , Rho , Alphas , Tsub0s , Gsube , Sigt , Sigc , Sigs
   COMMON /zzzzzz/ Rz
!
! Local variable declarations
!
   DOUBLE PRECISION d(6) , gx
   INTEGER i , iaypnt , ibasea , ibaseb , ibypnt , idispa , idispb , itemp , j , k , ka , kb
!
! End of declarations
!
!*****
! THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES  K(NPVT,NPVT) AND
! K(NPVT,J) FOR A ROD HAVING END POINTS NUMBERED NPVT AND J.
!*****
!
!
!
!                        E C P T  F O R  T H E  R O D
!
!
!
!                                                                CARD
!                                                 TYPE   TABLE   TYPE
! ECPT( 1)ELEMENT ID.                               I     ECT    CROD
! ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A      I     ECT    CROD
! ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B      I     ECT    CROD
! ECPT( 4)MATERIAL ID.                              I     EPT    PROD
! ECPT( 5)AREA  (A)                                 R     EPT    PROD
! ECPT( 6)POLAR MOMENT OF INERTIA (J)               R     EPT    PROD
! ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT    PROD
! ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT    PROD
! ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT   GRID
! ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
! ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
! ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
! ECPT(17) ELEMENT TEMPERATURE
! ECPT(18) ELEMENT DEFORMATION
! ECPT(19) AVERAGE ELEMENT LOADING TEMPERATURE
! ECPT(20)                ...
! ECPT(21) DISPLACEMENT COOR. FOR GRID PT. A
! ECPT(22)                ...
! ECPT(23)                ...
! ECPT(24) DISPLACEMENT COOR. FOR GRID PT. B
! ECPT(25)                ...
!
!
!
!
!
!
!
! DS1A VARIABLE CORE
!
!
! DS1A COMMON BLOCK
!
!
! ECPT COMMON BLOCK
!
!
! DS1A LOCAL VARIABLE (SCRATCH) BLOCK
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   EQUIVALENCE (Rz(1),Iz(1),Dz(1)) , (Ecpt(1),Iecpt(1)) , (Zzt(1),Yyt(10))
!
! BEGIN EXECUTION
!
   IF ( Iecpt(2)==Npvt ) THEN
      ka = 9
      kb = 13
      idispa = 19
      idispb = 22
   ELSE
      IF ( Iecpt(3)/=Npvt ) CALL mesage(-30,34,Iecpt(1))
      itemp = Iecpt(2)
      Iecpt(2) = Iecpt(3)
      Iecpt(3) = itemp
      ka = 13
      kb = 9
      idispa = 22
      idispb = 19
   ENDIF
!
! AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID POINT.
! SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
! NOW COMPUTE THE LENGTH OF THE ROD.
!
!
! WE STORE THE COORDINATES IN THE D ARRAY SO THAT ALL ARITHMETIC WILL BE
! DOUBLE PRECISION
!
   d(1) = Ecpt(ka+1)
   d(2) = Ecpt(ka+2)
   d(3) = Ecpt(ka+3)
   d(4) = Ecpt(kb+1)
   d(5) = Ecpt(kb+2)
   d(6) = Ecpt(kb+3)
   X = d(1) - d(4)
   Y = d(2) - d(5)
   Z = d(3) - d(6)
   Xl = dsqrt(X**2+Y**2+Z**2)
   IF ( Xl==0.0D0 ) THEN
      CALL mesage(30,26,Iecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
      Nogo = 1
      GOTO 99999
   ELSE
!
! CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
!
      Xn(1) = X/Xl
      Xn(2) = Y/Xl
      Xn(3) = Z/Xl
!
! CALL SUBROUTINE MAT TO FETCH MATERIAL PROPERTIES.
!
      Matidc = Iecpt(4)
      Matflg = 1
      Eltemp = Ecpt(17)
      CALL mat(Iecpt(1))
!
! STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
      Ua(1) = Ecpt(idispa+1)
      Ua(2) = Ecpt(idispa+2)
      Ua(3) = Ecpt(idispa+3)
      Ub(1) = Ecpt(idispb+1)
      Ub(2) = Ecpt(idispb+2)
      Ub(3) = Ecpt(idispb+3)
!
!
! COMPUTE THE DIFFERENCE VECTOR DIFF =  T  * U   -  T  * U
!                                        A    A      B    B
!
      ibasea = 0
      IF ( Iecpt(ka)/=0 ) THEN
         CALL transd(Ecpt(ka),Ta)
         ibasea = 3
         CALL gmmatd(Ta,3,3,0,Ua(1),3,1,0,Ua(4))
      ENDIF
      ibaseb = 0
      IF ( Iecpt(kb)/=0 ) THEN
         CALL transd(Ecpt(kb),Tb)
         ibaseb = 3
         CALL gmmatd(Tb,3,3,0,Ub(1),3,1,0,Ub(4))
      ENDIF
      Diff(1) = Ua(ibasea+1) - Ub(ibaseb+1)
      Diff(2) = Ua(ibasea+2) - Ub(ibaseb+2)
      Diff(3) = Ua(ibasea+3) - Ub(ibaseb+3)
!
! COMPUTE DOT PRODUCT XN . DIFF
!
      CALL gmmatd(Xn,3,1,1,Diff,3,1,0,Dpterm)
!
! COMPUTE AXIAL FORCE FX, AND TORSIONAL FORCE GX
!
      Delta = Ecpt(18)
      Fx = Dpterm - Delta
      IF ( Iecpt(19)/=(-1) ) THEN
         Tsub0 = Tsub0s
         Alpha = Alphas
         Avgltp = Ecpt(19)
         Fx = Fx - Alpha*Xl*(Avgltp-Tsub0)
      ENDIF
      A = Ecpt(5)
      E = Es
      Fx = A*E*Fx/Xl**2
      gx = Ecpt(6)*Fx/A
!
! COMPUTE THE XM VECTOR
!
      Xm(1) = 0.0D0
      Xm(2) = 0.0D0
      Xm(3) = 0.0D0
      i = 1
      IF ( dabs(Xn(2))<dabs(Xn(1)) ) i = 2
      IF ( dabs(Xn(3))<dabs(Xn(i)) ) i = 3
      Xm(i) = 1.0D0
!
! COMPUTE YVEC, THE CROSS PRODUCT XM X XN
!
      Yvec(1) = Xm(2)*Xn(3) - Xm(3)*Xn(2)
      Yvec(2) = Xm(3)*Xn(1) - Xm(1)*Xn(3)
      Yvec(3) = Xm(1)*Xn(2) - Xm(2)*Xn(1)
      Yl = dsqrt(Yvec(1)**2+Yvec(2)**2+Yvec(3)**2)
      Yvec(1) = Yvec(1)/Yl
      Yvec(2) = Yvec(2)/Yl
      Yvec(3) = Yvec(3)/Yl
!
! COMPUTE ZVEC, THE CROSS PRODUCT XN X YVEC
!
      Zvec(1) = Xn(2)*Yvec(3) - Xn(3)*Yvec(2)
      Zvec(2) = Xn(3)*Yvec(1) - Xn(1)*Yvec(3)
      Zvec(3) = Xn(1)*Yvec(2) - Xn(2)*Yvec(1)
      Zl = dsqrt(Zvec(1)**2+Zvec(2)**2+Zvec(3)**2)
      Zvec(1) = Zvec(1)/Zl
      Zvec(2) = Zvec(2)/Zl
      Zvec(3) = Zvec(3)/Zl
!
!                    T                 T
! COMPUTE YVEC * YVEC  AND  ZVEC * ZVEC
!
      CALL gmmatd(Yvec,3,1,0,Yvec,3,1,1,Yyt)
      CALL gmmatd(Zvec,3,1,0,Zvec,3,1,1,Zzt)
!
! ADD THESE TWO MATRICES AND STORE IN YYT
!
      DO i = 1 , 9
         Yyt(i) = Yyt(i) + Zzt(i)
      ENDDO
!
!          T
! COMPUTE T  (YYT) IF POINT A IS NOT IN BASIC COORDINATES
!          A
!
      iaypnt = 1
      IF ( Iecpt(ka)/=0 ) THEN
         iaypnt = 10
         CALL gmmatd(Ta,3,3,1,Yyt,3,3,0,Yyt(10))
!
!          T
! COMPUTE T  (YYT) T  AND STORE IN YYT(1)
!          A        A
!
         CALL gmmatd(Yyt(10),3,3,0,Ta,3,3,0,Yyt(1))
      ENDIF
!
! ZERO OUT KE MATRIX
!
      DO i = 1 , 36
         Ke(i) = 0.0D0
      ENDDO
      k = 1
      j = 2
   ENDIF
   DO
!
! FILL UP THE 6 X 6 KE
!
      Ke(1) = Fx*Yyt(k)
      Ke(2) = Fx*Yyt(k+1)
      Ke(3) = Fx*Yyt(k+2)
      Ke(7) = Fx*Yyt(k+3)
      Ke(8) = Fx*Yyt(k+4)
      Ke(9) = Fx*Yyt(k+5)
      Ke(13) = Fx*Yyt(k+6)
      Ke(14) = Fx*Yyt(k+7)
      Ke(15) = Fx*Yyt(k+8)
      Ke(22) = gx*Yyt(k)
      Ke(23) = gx*Yyt(k+1)
      Ke(24) = gx*Yyt(k+2)
      Ke(28) = gx*Yyt(k+3)
      Ke(29) = gx*Yyt(k+4)
      Ke(30) = gx*Yyt(k+5)
      Ke(34) = gx*Yyt(k+6)
      Ke(35) = gx*Yyt(k+7)
      Ke(36) = gx*Yyt(k+8)
      CALL ds1b(Ke,Iecpt(j))
      IF ( j==3 ) RETURN
      IF ( Iecpt(kb)==0 ) THEN
         k = iaypnt
      ELSE
         ibypnt = 1
         IF ( iaypnt==1 ) ibypnt = 10
         CALL gmmatd(Yyt(iaypnt),3,3,0,Tb,3,3,0,Yyt(ibypnt))
         k = ibypnt
      ENDIF
      j = 3
      Fx = -Fx
      gx = -gx
   ENDDO
99999 RETURN
END SUBROUTINE drod
