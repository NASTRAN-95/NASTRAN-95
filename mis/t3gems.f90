
SUBROUTINE t3gems(Ierr,Egpdt,Iorder,Gb,Gs,Lx,Ly,Edglen,Shrflx,Aic,Jog,Jok,K11,K22)
   IMPLICIT NONE
   INTEGER Ierr
   REAL Jog , Jok , K11 , K22 , Lx , Ly
   LOGICAL Shrflx
   REAL Aic(18) , Edglen(3) , Egpdt(4,3) , Gb(9) , Gs(4)
   INTEGER Iorder(3)
   REAL aa(9) , bdum(3) , cosa , cosb , cosc , determ , h1 , h2 , sina , sinb , sinc , xx(3) , yy(3)
   INTEGER i , index(3,3) , ising , j , jo
!
!     SINGLE PRECISION ROUTINE TO SET UP THE REQUIRED SHEAR-RELATED
!     TRANSFORMATION TO RELIEVE THE TRIA3 GEOMETRY BIAS IN BENDING.
!
!     INPUT :
!           EGPDT  - BGPDT DATA IN ELEMENT COORD. SYSTEM
!           IORDER - ARRAY OF ORDER INDICATORS FOR REARRANGED DATA
!           GB     - ARRAY OF BENDING MATERIAL PROPERTIES
!           GS     - ARRAY OF SHEAR   MATERIAL PROPERTIES
!           LX     - DIMENSION OF ELEMENT ALONG X-AXIS
!           LY     - DIMENSION OF ELEMENT ALONG Y-AXIS
!           EDGLEN - EDGE LENGTHS
!           SHRFLX - LOGICAL INDICATING THE PRESENCE OF SHEAR FLEX
!     OUTPUT:
!           IERR   - ERROR FLAG
!           AIC    - TRANSFORMATION TO RELIEVE GEOMETRY BIAS
!           JOG    - SHEAR   STIFFNESS FACTOR
!           JOK    - BENDING STIFFNESS FACTOR
!           K11    - BENDING STIFFNESS FACTOR
!           K22    - BENDING STIFFNESS FACTOR
!
!
!     [C]    - TRANSFORMATION TO YIELD GAMMAT ALONG THE ELEMENT SIDES.
!
!     [AA]   - TRANSFORMATION FROM GAMMA0 (AT THE ELEMENT CENTER) TO
!              GAMMAT (ALONG THE ELEMENT SIDES).
!
!                  -1
!     [AIC]  - [AA]  [C]
!
!
!
!
   Ierr = 0
   DO i = 1 , 3
      DO j = 1 , 3
         jo = Iorder(j)
         IF ( i==jo ) THEN
            xx(i) = Egpdt(2,j)
            yy(i) = Egpdt(3,j)
         ENDIF
      ENDDO
   ENDDO
!
   cosa = ((xx(2)-xx(1))/Edglen(1))
   sina = ((yy(2)-yy(1))/Edglen(1))
   cosb = ((xx(3)-xx(2))/Edglen(2))
   sinb = ((yy(3)-yy(2))/Edglen(2))
   cosc = ((xx(1)-xx(3))/Edglen(3))
   sinc = ((yy(1)-yy(3))/Edglen(3))
!
   aa(1) = sina
   aa(2) = cosa
   aa(3) = 1.0
   aa(4) = sinb
   aa(5) = cosb
   aa(6) = 1.0
   aa(7) = sinc
   aa(8) = cosc
   aa(9) = 1.0
!
   CALL invers(3,aa,3,bdum,0,determ,ising,index)
   IF ( ising/=1 ) THEN
!
      Ierr = 1
   ELSE
!
      Aic(1) = aa(1)*sina
      Aic(2) = aa(1)*cosa
      Aic(3) = aa(2)*sinb
      Aic(4) = aa(2)*cosb
      Aic(5) = aa(3)*sinc
      Aic(6) = aa(3)*cosc
      Aic(7) = aa(4)*sina
      Aic(8) = aa(4)*cosa
      Aic(9) = aa(5)*sinb
      Aic(10) = aa(5)*cosb
      Aic(11) = aa(6)*sinc
      Aic(12) = aa(6)*cosc
      Aic(13) = aa(7)*sina
      Aic(14) = aa(7)*cosa
      Aic(15) = aa(8)*sinb
      Aic(16) = aa(8)*cosb
      Aic(17) = aa(9)*sinc
      Aic(18) = aa(9)*cosc
!
!     CALCULATE THE BENDING STIFFNESS FACTORS
!
      h1 = Ly
      h2 = Lx
      K11 = 1.0/(h1*h1)*Gb(5)
      K22 = 1.0/(h2*h2)*Gb(1)
!
      Jok = K11*K22
      IF ( Jok/=0.0 ) Jok = 1.0/Jok
      Jog = 0.0
      IF ( Shrflx ) Jog = Gs(1)*Gs(4) - Gs(2)*Gs(3)
      IF ( Jog/=0.0 ) Jog = 1.0/Jog
   ENDIF
END SUBROUTINE t3gems