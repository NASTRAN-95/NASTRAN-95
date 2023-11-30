
SUBROUTINE ihexsd(Type,Shp,Dshp,Jacob,Detj,Eid,Xi,Eta,Zeta,Bxyz)
   IMPLICIT NONE
   INTEGER Op
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /system/ Sysbuf , Op
   COMMON /xmssg / Ufm
   DOUBLE PRECISION Detj , Eta , Xi , Zeta
   INTEGER Eid , Type
   REAL Bxyz(3,8)
   DOUBLE PRECISION Dshp(3,8) , Jacob(3,3) , Shp(8)
   REAL d
   INTEGER i , j , k , ngp
   DOUBLE PRECISION qeta , qxi , qxyz , qzeta , work(3,3) , x , y , z
!
!     DOUBLE PRECISION VERSION
!
!     ISOPARAMETRIC UTILITY ROUTINE.  THIS ROUTINE WILL COMPUTE
!     VALUES OF THE SHAPE FUNCTIONS, THEIR DERIVATIVES WITH RESPECT TO
!     XI,ETA, AND ZETA, THE JACOBIAN MATRIX INVERSE, AND ITS DETERMINANT
!
!                       TYPE = 1       IHEX1
!                       TYPE = 2       IHEX2
!                       TYPE = 3       IHEX3
!
!     SHP    = VALUES OF SHAPE FUNCTIONS
!     DSHP   = DERIVATIVES OF SHAPE FUNCTIONS W.R.T. XI, ETA, ZETA
!     JACOB  = JACOBIAN MATRIX INVERSE
!     DETJ   = DETERMINANT OF JACOBIAN MATRIX
!     XI, ETA, ZETA = ELEMENT COORDINATES AT WHICH THESE COMPUTATIONS
!                     TAKE PLACE
!     BXYZ   = BASIC SYSTEM COORDINATES FOR GRID POINTS
!
!     LOCAL VARIABLES
!     X,Y,Z  = CONSTANTS FOR EACH SHAPE FUNCTION
!     NGP    = NUMBER OF SHAPE FUNCTIONS, ALSO NUMBER OF GRID POINTS
!
!
   ngp = 12*Type - 4
   y = -1.0
   z = -1.0
   IF ( Type==2 ) THEN
!
!     QUADRATIC ELEMENT IHEX2
!
      d = 1.0
      x = 0.0
      DO i = 1 , 20
!            1   2   3   4   5   6   7   8   9   10
         IF ( i==1 .OR. i==6 .OR. i==7 .OR. i==12 .OR. i==18 .OR. i==19 ) THEN
            x = x - d
         ELSEIF ( i==4 .OR. i==5 .OR. i==11 .OR. i==16 .OR. i==17 ) THEN
            y = y + d
         ELSEIF ( i==8 .OR. i==20 ) THEN
            y = y - d
         ELSEIF ( i==9 .OR. i==13 ) THEN
            z = z + 1.0
            y = -1.0
            d = 3.0 - d
         ELSE
            x = x + d
         ENDIF
         IF ( x==0.0 ) THEN
!
!     MID-EDGE POINT, X=0.0
!
            qxi = 1.0 - Xi**2
            qeta = 1.0 + y*Eta
            qzeta = 1.0 + z*Zeta
            Shp(i) = qxi*qeta*qzeta/4.0
            Dshp(1,i) = -Xi*qeta*qzeta/2.0
            Dshp(2,i) = qxi*qzeta*y/4.0
            Dshp(3,i) = qxi*qeta*z/4.0
         ELSEIF ( y==0.0 ) THEN
!
!     MID-EDGE POINT, Y=0.0
!
            qxi = 1.0 + x*Xi
            qeta = 1.0 - Eta**2
            qzeta = 1.0 + z*Zeta
            Shp(i) = qeta*qxi*qzeta/4.0
            Dshp(1,i) = qeta*qzeta*x/4.0
            Dshp(2,i) = -Eta*qzeta*qxi/2.0
            Dshp(3,i) = qeta*qxi*z/4.0
         ELSEIF ( z==0.0 ) THEN
!
!     MID-EDGE POINT, Z=0.0
!
            qxi = 1.0 + x*Xi
            qeta = 1.0 + y*Eta
            qzeta = 1.0 - Zeta**2
            Shp(i) = qzeta*qxi*qeta/4.0
            Dshp(1,i) = qzeta*qeta*x/4.0
            Dshp(2,i) = qzeta*qxi*y/4.0
            Dshp(3,i) = -Zeta*qxi*qeta/2.0
         ELSE
!
!     CORNER POINT
!
            qxi = 1.0 + x*Xi
            qeta = 1.0 + y*Eta
            qzeta = 1.0 + z*Zeta
            qxyz = x*Xi + y*Eta + z*Zeta
            Shp(i) = qxi*qeta*qzeta*(qxyz-2.0)/8.0
            Dshp(1,i) = x*qeta*qzeta*(x*Xi+qxyz-1.0)/8.0
            Dshp(2,i) = y*qxi*qzeta*(y*Eta+qxyz-1.0)/8.0
            Dshp(3,i) = z*qxi*qeta*(z*Zeta+qxyz-1.0)/8.0
         ENDIF
      ENDDO
   ELSEIF ( Type==3 ) THEN
!
!     CUBIC ELEMENT IHEX3
!
      d = 2.0/3.0
      x = -1.0/3.0
      DO i = 1 , 32
!            1   2   3   4   5   6   7   8   9   10
         IF ( i==2 .OR. i==3 .OR. i==4 .OR. i==14 .OR. i==18 .OR. i==22 .OR. i==23 .OR. i==24 ) THEN
            x = x + d
         ELSEIF ( i==5 .OR. i==6 .OR. i==7 .OR. i==15 .OR. i==19 .OR. i==25 .OR. i==26 .OR. i==27 ) THEN
            y = y + d
         ELSEIF ( i==11 .OR. i==12 .OR. i==31 .OR. i==32 ) THEN
            y = y - d
         ELSEIF ( i==13 .OR. i==17 .OR. i==21 ) THEN
            y = -1.0
            z = z + 2.0/3.0
            IF ( z>-1.0 ) d = 2.0
            IF ( z>0.4 ) d = 2.0/3.0
         ELSE
            x = x - d
         ENDIF
         IF ( dabs(x)<0.4 ) THEN
!
!     MID-EDGE POINT, X = + OR - 1/3
!
            qxi = 9.0*(1.0-Xi**2)*(1.0+9.0*x*Xi)/64.0
            qeta = 1.0 + y*Eta
            qzeta = 1.0 + z*Zeta
            qxyz = 9.0*(-2.0*Xi+9.0*x-27.0*Xi*x*Xi)/64.0
            Shp(i) = qxi*qeta*qzeta
            Dshp(1,i) = qeta*qzeta*qxyz
            Dshp(2,i) = qxi*qzeta*y
            Dshp(3,i) = qxi*qeta*z
         ELSEIF ( dabs(y)<0.4 ) THEN
!
!     MID-EDGE POINT Y = + OR - 1/3
!
            qxi = 1.0 + x*Xi
            qeta = 9.0*(1.0-Eta**2)*(1.0+9.0*Eta*y)/64.0
            qzeta = 1.0 + z*Zeta
            qxyz = 9.0*(-2.0*Eta+9.0*y-27.0*Eta*y*Eta)/64.0
            Shp(i) = qeta*qxi*qzeta
            Dshp(1,i) = qeta*qzeta*x
            Dshp(2,i) = qxi*qzeta*qxyz
            Dshp(3,i) = qeta*qxi*z
         ELSEIF ( dabs(z)<0.4 ) THEN
!
!     MID-EDGE POINTS Z = + OR - 1/3
!
            qxi = 1.0 + x*Xi
            qeta = 1.0 + y*Eta
            qzeta = 9.0*(1.0-Zeta**2)*(1.0+9.0*z*Zeta)/64.0
            qxyz = 9.0*(-2.0*Zeta+9.0*z-27.0*z*Zeta**2)/64.0
            Shp(i) = qzeta*qxi*qeta
            Dshp(1,i) = qzeta*qeta*x
            Dshp(2,i) = qzeta*qxi*y
            Dshp(3,i) = qxi*qeta*qxyz
         ELSE
!
!     CORNER POINT
!
            qxi = 1.0 + x*Xi
            qeta = 1.0 + y*Eta
            qzeta = 1.0 + z*Zeta
            qxyz = Xi**2 + Eta**2 + Zeta**2 - 19.0/9.0
            Shp(i) = 9.0*qxi*qeta*qzeta*qxyz/64.0
            Dshp(1,i) = 9.0*qeta*qzeta*(x*(2.0*Xi**2+qxyz)+2.0*Xi)/64.0
            Dshp(2,i) = 9.0*qxi*qzeta*(y*(2.0*Eta**2+qxyz)+2.0*Eta)/64.0
            Dshp(3,i) = 9.0*qxi*qeta*(z*(2.0*Zeta**2+qxyz)+2.0*Zeta)/64.0
         ENDIF
      ENDDO
   ELSE
!
!     LINEAR ELEMENT IHEX1
!
      DO j = 1 , 2
         IF ( j==2 ) z = 1.0
         x = -1.0
         y = -1.0
         DO i = 1 , 4
            IF ( i==3 ) y = 1.0
            IF ( i==2 ) x = 1.0
            IF ( i==4 ) x = -1.0
            k = i + (j-1)*4
            qxi = 1.0 + Xi*x
            qeta = 1.0 + Eta*y
            qzeta = 1.0 + Zeta*z
            Shp(k) = qxi*qeta*qzeta/8.0
            Dshp(1,k) = x*qeta*qzeta/8.0
            Dshp(2,k) = y*qxi*qzeta/8.0
            Dshp(3,k) = z*qxi*qeta/8.0
         ENDDO
      ENDDO
   ENDIF
!
!     COMPUTE JACOBIAN MATRIX
!
   DO i = 1 , 3
      DO j = 1 , 3
         Jacob(i,j) = 0.0
         DO k = 1 , ngp
            Jacob(i,j) = Jacob(i,j) + Dshp(i,k)*dble(Bxyz(j,k))
         ENDDO
      ENDDO
   ENDDO
!
!     COMPUTE INVERSE AND DETERMINANT OF JACOBIAN MATRIX
!
   work(1,1) = Jacob(2,2)*Jacob(3,3) - Jacob(2,3)*Jacob(3,2)
   work(2,1) = Jacob(2,3)*Jacob(3,1) - Jacob(2,1)*Jacob(3,3)
   work(3,1) = Jacob(2,1)*Jacob(3,2) - Jacob(2,2)*Jacob(3,1)
   work(1,2) = Jacob(1,3)*Jacob(3,2) - Jacob(1,2)*Jacob(3,3)
   work(2,2) = Jacob(1,1)*Jacob(3,3) - Jacob(1,3)*Jacob(3,1)
   work(3,2) = Jacob(1,2)*Jacob(3,1) - Jacob(1,1)*Jacob(3,2)
   work(1,3) = Jacob(1,2)*Jacob(2,3) - Jacob(1,3)*Jacob(2,2)
   work(2,3) = Jacob(1,3)*Jacob(2,1) - Jacob(1,1)*Jacob(2,3)
   work(3,3) = Jacob(1,1)*Jacob(2,2) - Jacob(1,2)*Jacob(2,1)
   Detj = 0.0
   DO i = 1 , 3
      Detj = Detj + Jacob(i,2)*work(2,i)
   ENDDO
   IF ( Detj==0.0 ) THEN
!
!     JACOBIAN MATRIX WAS SINGULAR.
!
      WRITE (Op,99001) Ufm , Eid
99001 FORMAT (A23,' 3306, SINGULAR JACOBIAN MATRIX FOR ISOPARAMETRIC ','ELEMENT NO.',I9)
      GOTO 99999
   ENDIF
   DO i = 1 , 3
      DO j = 1 , 3
         Jacob(i,j) = work(i,j)/Detj
      ENDDO
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE ihexsd
