
SUBROUTINE linein(X1,Y1,Z1,X2,Y2,Z2,Hcdl)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Hcdl , X1 , X2 , Y1 , Y2 , Z1 , Z2
!
! Local variable declarations
!
   REAL hcx , hcy , hcz , segl , segx , segy , segz , w(4) , xi(4) , xx , yy , zz
   INTEGER i
!
! End of declarations
!
!
! PERFORMS LINE INTEGRAL FROM (X1,Y1,Z1) TO (X2,Y2,Z2) OF BIOT-SAVART
! FILED DOTTED INTO THE LINE, IE INT(HC.DL)
!
   DATA xi/.06943184 , .33000948 , .66999052 , .93056816/
   DATA w/.17392742 , 2*.32607258 , .173927423/
!
! COMPONENTS OF LINE SEGMENT
!
   Hcdl = 0.
   segx = X2 - X1
   segy = Y2 - Y1
   segz = Z2 - Z1
   segl = sqrt(segx**2+segy**2+segz**2)
   IF ( segl==0. ) RETURN
!
! 4 POINT INTEGRATION OVER LINE SEGMENT(XI= / TO +1)
!
   DO i = 1 , 4
      xx = X1 + segx*xi(i)
      yy = Y1 + segy*xi(i)
      zz = Z1 + segz*xi(i)
      CALL biotsv(xx,yy,zz,hcx,hcy,hcz)
      Hcdl = Hcdl + (hcx*segx+hcy*segy+hcz*segz)*w(i)
   ENDDO
END SUBROUTINE linein
