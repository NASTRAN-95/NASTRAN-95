!*==q4shps.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE q4shps(Xi,Eta,Shp,Sshp)
   IMPLICIT NONE
   DOUBLE PRECISION Di , Etd
   REAL Eta , Xi
   DOUBLE PRECISION Dshp(4) , Dsshp(8)
   REAL Shp(4) , Sshp(8)
   REAL clc(2,4)
   DOUBLE PRECISION dld(2,4)
   INTEGER i
!     &    ENTRY Q4SHPD (DI,ETD,DSHP,DSSHP)
!
!*****
!     COMPUTES SHAPE FUNCTIONS AND THEIR DERIVATIVES
!     FOR THE QUAD4 ELEMENT
!*****
!
   DATA clc/ - 1.0 , -1.0 , 1.0 , -1.0 , 1.0 , 1.0 , -1.0 , 1.0/
   DATA dld/ - 1.0D0 , -1.0D0 , 1.0D0 , -1.0D0 , 1.0D0 , 1.0D0 , -1.0D0 , 1.0D0/
!
!     SINGLE PRECISION -
!
   DO i = 1 , 4
      Shp(i) = 0.25*(1.0+Xi*clc(1,i))*(1.0+Eta*clc(2,i))
      Sshp(i) = 0.25*(1.0+Eta*clc(2,i))*clc(1,i)
      Sshp(i+4) = 0.25*(1.0+Xi*clc(1,i))*clc(2,i)
   ENDDO
   RETURN
!
   ENTRY q4shpd(Di,Etd,Dshp,Dsshp)
!     ================================
!
!     DOUBLE PRECISION -
!
   DO i = 1 , 4
      Dshp(i) = .25D0*(1.D0+Di*dld(1,i))*(1.D0+Etd*dld(2,i))
      Dsshp(i) = .25D0*(1.D0+Etd*dld(2,i))*dld(1,i)
      Dsshp(i+4) = .25D0*(1.D0+Di*dld(1,i))*dld(2,i)
   ENDDO
END SUBROUTINE q4shps
