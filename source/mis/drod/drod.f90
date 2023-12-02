!*==drod.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE drod
   IMPLICIT NONE
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
   USE c_matin
   USE c_matout
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(6) :: d
   REAL*8 :: gx
   INTEGER :: i , iaypnt , ibasea , ibaseb , ibypnt , idispa , idispb , itemp , j , k , ka , kb
   INTEGER , DIMENSION(19) :: iecpt
   REAL*8 , DIMENSION(9) :: zzt
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Rz(1),Iz(1),Dz(1)) , (Ecpt(1),Iecpt(1)) , (Zzt(1),Yyt(10))
!
! BEGIN EXECUTION
!
   IF ( iecpt(2)==npvt ) THEN
      ka = 9
      kb = 13
      idispa = 19
      idispb = 22
   ELSE
      IF ( iecpt(3)/=npvt ) CALL mesage(-30,34,iecpt(1))
      itemp = iecpt(2)
      iecpt(2) = iecpt(3)
      iecpt(3) = itemp
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
   d(1) = ecpt(ka+1)
   d(2) = ecpt(ka+2)
   d(3) = ecpt(ka+3)
   d(4) = ecpt(kb+1)
   d(5) = ecpt(kb+2)
   d(6) = ecpt(kb+3)
   x = d(1) - d(4)
   y = d(2) - d(5)
   z = d(3) - d(6)
   xl = dsqrt(x**2+y**2+z**2)
   IF ( xl==0.0D0 ) THEN
      CALL mesage(30,26,iecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
      nogo = 1
      GOTO 99999
   ELSE
!
! CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
!
      xn(1) = x/xl
      xn(2) = y/xl
      xn(3) = z/xl
!
! CALL SUBROUTINE MAT TO FETCH MATERIAL PROPERTIES.
!
      matidc = iecpt(4)
      matflg = 1
      eltemp = ecpt(17)
      CALL mat(iecpt(1))
!
! STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
!
      ua(1) = ecpt(idispa+1)
      ua(2) = ecpt(idispa+2)
      ua(3) = ecpt(idispa+3)
      ub(1) = ecpt(idispb+1)
      ub(2) = ecpt(idispb+2)
      ub(3) = ecpt(idispb+3)
!
!
! COMPUTE THE DIFFERENCE VECTOR DIFF =  T  * U   -  T  * U
!                                        A    A      B    B
!
      ibasea = 0
      IF ( iecpt(ka)/=0 ) THEN
         CALL transd(ecpt(ka),ta)
         ibasea = 3
         CALL gmmatd(ta,3,3,0,ua(1),3,1,0,ua(4))
      ENDIF
      ibaseb = 0
      IF ( iecpt(kb)/=0 ) THEN
         CALL transd(ecpt(kb),tb)
         ibaseb = 3
         CALL gmmatd(tb,3,3,0,ub(1),3,1,0,ub(4))
      ENDIF
      diff(1) = ua(ibasea+1) - ub(ibaseb+1)
      diff(2) = ua(ibasea+2) - ub(ibaseb+2)
      diff(3) = ua(ibasea+3) - ub(ibaseb+3)
!
! COMPUTE DOT PRODUCT XN . DIFF
!
      CALL gmmatd(xn,3,1,1,diff,3,1,0,dpterm)
!
! COMPUTE AXIAL FORCE FX, AND TORSIONAL FORCE GX
!
      delta = ecpt(18)
      fx = dpterm - delta
      IF ( iecpt(19)/=(-1) ) THEN
         tsub0 = tsub0s
         alpha = alphas
         avgltp = ecpt(19)
         fx = fx - alpha*xl*(avgltp-tsub0)
      ENDIF
      a = ecpt(5)
      e = es
      fx = a*e*fx/xl**2
      gx = ecpt(6)*fx/a
!
! COMPUTE THE XM VECTOR
!
      xm(1) = 0.0D0
      xm(2) = 0.0D0
      xm(3) = 0.0D0
      i = 1
      IF ( dabs(xn(2))<dabs(xn(1)) ) i = 2
      IF ( dabs(xn(3))<dabs(xn(i)) ) i = 3
      xm(i) = 1.0D0
!
! COMPUTE YVEC, THE CROSS PRODUCT XM X XN
!
      yvec(1) = xm(2)*xn(3) - xm(3)*xn(2)
      yvec(2) = xm(3)*xn(1) - xm(1)*xn(3)
      yvec(3) = xm(1)*xn(2) - xm(2)*xn(1)
      yl = dsqrt(yvec(1)**2+yvec(2)**2+yvec(3)**2)
      yvec(1) = yvec(1)/yl
      yvec(2) = yvec(2)/yl
      yvec(3) = yvec(3)/yl
!
! COMPUTE ZVEC, THE CROSS PRODUCT XN X YVEC
!
      zvec(1) = xn(2)*yvec(3) - xn(3)*yvec(2)
      zvec(2) = xn(3)*yvec(1) - xn(1)*yvec(3)
      zvec(3) = xn(1)*yvec(2) - xn(2)*yvec(1)
      zl = dsqrt(zvec(1)**2+zvec(2)**2+zvec(3)**2)
      zvec(1) = zvec(1)/zl
      zvec(2) = zvec(2)/zl
      zvec(3) = zvec(3)/zl
!
!                    T                 T
! COMPUTE YVEC * YVEC  AND  ZVEC * ZVEC
!
      CALL gmmatd(yvec,3,1,0,yvec,3,1,1,yyt)
      CALL gmmatd(zvec,3,1,0,zvec,3,1,1,zzt)
!
! ADD THESE TWO MATRICES AND STORE IN YYT
!
      DO i = 1 , 9
         yyt(i) = yyt(i) + zzt(i)
      ENDDO
!
!          T
! COMPUTE T  (YYT) IF POINT A IS NOT IN BASIC COORDINATES
!          A
!
      iaypnt = 1
      IF ( iecpt(ka)/=0 ) THEN
         iaypnt = 10
         CALL gmmatd(ta,3,3,1,yyt,3,3,0,yyt(10))
!
!          T
! COMPUTE T  (YYT) T  AND STORE IN YYT(1)
!          A        A
!
         CALL gmmatd(yyt(10),3,3,0,ta,3,3,0,yyt(1))
      ENDIF
!
! ZERO OUT KE MATRIX
!
      DO i = 1 , 36
         ke(i) = 0.0D0
      ENDDO
      k = 1
      j = 2
   ENDIF
   DO
!
! FILL UP THE 6 X 6 KE
!
      ke(1) = fx*yyt(k)
      ke(2) = fx*yyt(k+1)
      ke(3) = fx*yyt(k+2)
      ke(7) = fx*yyt(k+3)
      ke(8) = fx*yyt(k+4)
      ke(9) = fx*yyt(k+5)
      ke(13) = fx*yyt(k+6)
      ke(14) = fx*yyt(k+7)
      ke(15) = fx*yyt(k+8)
      ke(22) = gx*yyt(k)
      ke(23) = gx*yyt(k+1)
      ke(24) = gx*yyt(k+2)
      ke(28) = gx*yyt(k+3)
      ke(29) = gx*yyt(k+4)
      ke(30) = gx*yyt(k+5)
      ke(34) = gx*yyt(k+6)
      ke(35) = gx*yyt(k+7)
      ke(36) = gx*yyt(k+8)
      CALL ds1b(ke,iecpt(j))
      IF ( j==3 ) RETURN
      IF ( iecpt(kb)==0 ) THEN
         k = iaypnt
      ELSE
         ibypnt = 1
         IF ( iaypnt==1 ) ibypnt = 10
         CALL gmmatd(yyt(iaypnt),3,3,0,tb,3,3,0,yyt(ibypnt))
         k = ibypnt
      ENDIF
      j = 3
      fx = -fx
      gx = -gx
   ENDDO
99999 END SUBROUTINE drod
