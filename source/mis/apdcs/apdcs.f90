!*==apdcs.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdcs
   USE c_apd1c
   USE c_apd1d
   USE c_zzzzzz
   IMPLICIT NONE
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
   icpl(2) = 1
! CREATE PANEL COORDINATE SYSTEM
! FIND CP TRANSFORMATION AND CONVERT POINT 1 AND 4 TO BASIC
   IF ( cp==0 ) THEN
! COORDS ARE IN BASIC
      rb1(1) = x1
      rb1(2) = y1
      rb1(3) = z1
      rb4(1) = x4
      rb4(2) = y4
      rb4(3) = z4
   ELSE
      IF ( ncst1/=0 ) THEN
         DO icp = ncst1 , ncst2 , 14
            IF ( iz(icp)==cp ) GOTO 50
         ENDDO
      ENDIF
      CALL mesage(-30,25,cp)
      RETURN
 50   IF ( iz(icp+1)<2 ) THEN
! CP RECTANGULAR
         rcp1(1) = x1
         rcp1(2) = y1
         rcp1(3) = z1
         rcp4(1) = x4
         rcp4(2) = y4
         rcp4(3) = z4
      ELSEIF ( iz(icp+1)==2 ) THEN
! CP CYLINDRICAL
         rcp1(1) = x1*cos(y1*degr)
         rcp1(2) = x1*sin(y1*degr)
         rcp1(3) = z1
         rcp4(1) = x4*cos(y4*degr)
         rcp4(2) = x4*sin(y4*degr)
         rcp4(3) = z4
      ELSE
! CP SPHERICAL
         rcp1(1) = x1*sin(y1*degr)*cos(z1*degr)
         rcp1(2) = x1*sin(y1*degr)*sin(z1*degr)
         rcp1(3) = x1*cos(y1*degr)
         rcp4(1) = x4*sin(y4*degr)*cos(z4*degr)
         rcp4(2) = x4*sin(y4*degr)*sin(z4*degr)
         rcp4(3) = x4*cos(y4*degr)
      ENDIF
! CONVERT TO BASIC
      CALL gmmats(z(icp+5),3,3,0,rcp1,3,1,0,rb1)
      CALL gmmats(z(icp+5),3,3,0,rcp4,3,1,0,rb4)
      j = icp + 1
      DO i = 1 , 3
         k = j + i
         rb1(i) = rb1(i) + z(k)
      ENDDO
      DO i = 1 , 3
         k = j + i
         rb4(i) = rb4(i) + z(k)
      ENDDO
   ENDIF
! FIND R1 THRU IN R4 AERO CS
   IF ( acsid==0 ) THEN
      DO i = 1 , 3
         ra1(i) = rb1(i)
         ra4(i) = rb4(i)
      ENDDO
!
!     STOP IF BODY
!
      IF ( igid<0 ) RETURN
   ELSE
      j = iacs + 1
      DO i = 1 , 3
         k = j + i
         rx1(i) = rb1(i) - z(k)
         rx4(i) = rb4(i) - z(k)
      ENDDO
      CALL gmmats(z(iacs+5),3,3,1,rx1,3,1,0,ra1)
      CALL gmmats(z(iacs+5),3,3,1,rx4,3,1,0,ra4)
   ENDIF
! CALCULATE R2 AND R3 IN AC CS
   DO i = 2 , 3
      ra2(i) = ra1(i)
      ra3(i) = ra4(i)
   ENDDO
   ra2(1) = ra1(1) + x12
   ra3(1) = ra4(1) + x43
   ee = sqrt((ra4(3)-ra1(3))**2+(ra4(2)-ra1(2))**2)
   sg = (ra4(3)-ra1(3))/ee
   cg = (ra4(2)-ra1(2))/ee
! LOCATE POINTS 2,3,4 IN PANEL CORDINATE SYSTEM
   xp2 = x12
   xp4 = ra4(1) - ra1(1)
   xp3 = ra3(1) - ra1(1)
   yp4 = ee
! TRANSFORM R2 AND R3 INTO BASIC
   IF ( acsid==0 ) THEN
      DO i = 1 , 3
         rb2(i) = ra2(i)
         rb3(i) = ra3(i)
      ENDDO
   ELSE
      CALL gmmats(z(iacs+5),3,3,0,ra2,3,1,0,rb2)
      CALL gmmats(z(iacs+5),3,3,0,ra3,3,1,0,rb3)
      j = iacs + 1
      DO i = 1 , 3
         k = j + i
         rb2(i) = rb2(i) + z(k)
         rb3(i) = rb3(i) + z(k)
      ENDDO
   ENDIF
! FIND PANEL COORDINATE SYSTEM
   DO i = 1 , 3
      vx1(i) = rb2(i) - rb1(i)
      vx2(i) = rb4(i) - rb1(i)
      vx3(i) = rb3(i) - rb1(i)
      IF ( x12==0.0 ) vx1(i) = vx3(i)
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
   IF ( acsid/=0 ) THEN
      j = iacs + 5
      DO i = 1 , 3
         k = j + 3*(i-1)
         vx1(i) = z(k)
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
   icpl(1) = mcstm
   CALL write(cstma,icpl(1),14,0)
END SUBROUTINE apdcs
