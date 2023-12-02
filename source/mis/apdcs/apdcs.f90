!*==apdcs.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdcs
   IMPLICIT NONE
   USE C_APD1C
   USE C_APD1D
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) :: acpl
   REAL , SAVE :: degr
   REAL :: ee , sx1 , sx2
   INTEGER :: i , icp , j , k
   INTEGER , DIMENSION(1) :: iz
   REAL , DIMENSION(3) :: ra2 , ra3 , ra4 , rb1 , rb2 , rb3 , rb4 , rcp1 , rcp4 , rx1 , rx4 , v1 , v2 , vx1 , vx2 , vx3
   EXTERNAL gmmats , mesage , sadotb , saxb , write
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (v1(1),rcp1(1)) , (v2(1),rcp4(1))
   DATA degr/.017453293/
   Icpl(2) = 1
! CREATE PANEL COORDINATE SYSTEM
! FIND CP TRANSFORMATION AND CONVERT POINT 1 AND 4 TO BASIC
   IF ( Cp==0 ) THEN
! COORDS ARE IN BASIC
      rb1(1) = X1
      rb1(2) = Y1
      rb1(3) = Z1
      rb4(1) = X4
      rb4(2) = Y4
      rb4(3) = Z4
   ELSE
      IF ( Ncst1/=0 ) THEN
         DO icp = Ncst1 , Ncst2 , 14
            IF ( iz(icp)==Cp ) GOTO 50
         ENDDO
      ENDIF
      CALL mesage(-30,25,Cp)
      RETURN
 50   IF ( iz(icp+1)<2 ) THEN
! CP RECTANGULAR
         rcp1(1) = X1
         rcp1(2) = Y1
         rcp1(3) = Z1
         rcp4(1) = X4
         rcp4(2) = Y4
         rcp4(3) = Z4
      ELSEIF ( iz(icp+1)==2 ) THEN
! CP CYLINDRICAL
         rcp1(1) = X1*cos(Y1*degr)
         rcp1(2) = X1*sin(Y1*degr)
         rcp1(3) = Z1
         rcp4(1) = X4*cos(Y4*degr)
         rcp4(2) = X4*sin(Y4*degr)
         rcp4(3) = Z4
      ELSE
! CP SPHERICAL
         rcp1(1) = X1*sin(Y1*degr)*cos(Z1*degr)
         rcp1(2) = X1*sin(Y1*degr)*sin(Z1*degr)
         rcp1(3) = X1*cos(Y1*degr)
         rcp4(1) = X4*sin(Y4*degr)*cos(Z4*degr)
         rcp4(2) = X4*sin(Y4*degr)*sin(Z4*degr)
         rcp4(3) = X4*cos(Y4*degr)
      ENDIF
! CONVERT TO BASIC
      CALL gmmats(Z(icp+5),3,3,0,rcp1,3,1,0,rb1)
      CALL gmmats(Z(icp+5),3,3,0,rcp4,3,1,0,rb4)
      j = icp + 1
      DO i = 1 , 3
         k = j + i
         rb1(i) = rb1(i) + Z(k)
      ENDDO
      DO i = 1 , 3
         k = j + i
         rb4(i) = rb4(i) + Z(k)
      ENDDO
   ENDIF
! FIND R1 THRU IN R4 AERO CS
   IF ( Acsid==0 ) THEN
      DO i = 1 , 3
         Ra1(i) = rb1(i)
         ra4(i) = rb4(i)
      ENDDO
!
!     STOP IF BODY
!
      IF ( Igid<0 ) RETURN
   ELSE
      j = Iacs + 1
      DO i = 1 , 3
         k = j + i
         rx1(i) = rb1(i) - Z(k)
         rx4(i) = rb4(i) - Z(k)
      ENDDO
      CALL gmmats(Z(Iacs+5),3,3,1,rx1,3,1,0,Ra1)
      CALL gmmats(Z(Iacs+5),3,3,1,rx4,3,1,0,ra4)
   ENDIF
! CALCULATE R2 AND R3 IN AC CS
   DO i = 2 , 3
      ra2(i) = Ra1(i)
      ra3(i) = ra4(i)
   ENDDO
   ra2(1) = Ra1(1) + X12
   ra3(1) = ra4(1) + X43
   ee = sqrt((ra4(3)-Ra1(3))**2+(ra4(2)-Ra1(2))**2)
   Sg = (ra4(3)-Ra1(3))/ee
   Cg = (ra4(2)-Ra1(2))/ee
! LOCATE POINTS 2,3,4 IN PANEL CORDINATE SYSTEM
   Xp2 = X12
   Xp4 = ra4(1) - Ra1(1)
   Xp3 = ra3(1) - Ra1(1)
   Yp4 = ee
! TRANSFORM R2 AND R3 INTO BASIC
   IF ( Acsid==0 ) THEN
      DO i = 1 , 3
         rb2(i) = ra2(i)
         rb3(i) = ra3(i)
      ENDDO
   ELSE
      CALL gmmats(Z(Iacs+5),3,3,0,ra2,3,1,0,rb2)
      CALL gmmats(Z(Iacs+5),3,3,0,ra3,3,1,0,rb3)
      j = Iacs + 1
      DO i = 1 , 3
         k = j + i
         rb2(i) = rb2(i) + Z(k)
         rb3(i) = rb3(i) + Z(k)
      ENDDO
   ENDIF
! FIND PANEL COORDINATE SYSTEM
   DO i = 1 , 3
      vx1(i) = rb2(i) - rb1(i)
      vx2(i) = rb4(i) - rb1(i)
      vx3(i) = rb3(i) - rb1(i)
      IF ( X12==0.0 ) vx1(i) = vx3(i)
   ENDDO
   CALL saxb(vx1,vx2,v1)
   sx1 = sadotb(v1,v1)
   CALL saxb(vx1,vx3,v2)
   sx2 = sadotb(v2,v2)
   IF ( sx1<sx2 ) THEN
      sx2 = 1.0/sqrt(sx2)
      DO i = 1 , 3
         vx3(i) = v2(i)*sx2
      ENDDO
   ELSE
      sx1 = 1.0/sqrt(sx1)
      DO i = 1 , 3
         vx3(i) = v1(i)*sx1
      ENDDO
   ENDIF
   IF ( Acsid/=0 ) THEN
      j = Iacs + 5
      DO i = 1 , 3
         k = j + 3*(i-1)
         vx1(i) = Z(k)
      ENDDO
   ELSE
      vx1(1) = 1.0
      vx1(2) = 0.0
      vx1(3) = 0.0
   ENDIF
   CALL saxb(vx3,vx1,vx2)
   DO i = 1 , 3
      acpl(1,i) = vx1(i)
      acpl(2,i) = vx2(i)
      acpl(3,i) = vx3(i)
   ENDDO
! WRITE TRANSFORMATION ON CSTMA
   Icpl(1) = Mcstm
   CALL write(Cstma,Icpl(1),14,0)
END SUBROUTINE apdcs
