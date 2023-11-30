
SUBROUTINE pload
   IMPLICIT NONE
   REAL Bgpdt , Core(1) , Cstm , Old , Slt
   INTEGER Ksys(87) , Ksys88 , Lcore , Nn(11) , Nobld
   COMMON /loadx / Lcore , Slt , Bgpdt , Old , Cstm , Nn , Nobld
   COMMON /system/ Ksys , Ksys88
   COMMON /zzzzzz/ Core
   REAL a1 , a2 , a3 , area , flag , gpco1(3) , gpco2(3) , gpco3(3) , gpco4(3) , pi , ploads(3,4) , pmag , sin1 , sin2 , sin3 , v1 ,&
      & v2 , v3 , vect(3) , vect1(3) , vect2(3) , vect3(3)
   INTEGER gridp(5) , i , igpco(4,4) , in , iord(4) , j , l , n1 , name(2) , pont(4)
!
   EQUIVALENCE (pmag,gridp(1)) , (igpco(2,1),gpco1(1)) , (igpco(2,2),gpco2(1)) , (igpco(2,3),gpco3(1)) , (igpco(2,4),gpco4(1))
   DATA name/4HPLOA , 4HD   / , pi/3.141592654/
!
!
   DO i = 1 , 3
      ploads(i,4) = 0.0
   ENDDO
   CALL read(*200,*200,Slt,gridp(1),5,0,flag)
   pont(1) = gridp(2)
   pont(2) = gridp(3)
   pont(3) = gridp(4)
   pont(4) = gridp(5)
   n1 = 4
   IF ( gridp(5)==0 ) n1 = 3
   CALL permut(pont(1),iord(1),n1,Old)
   DO i = 1 , n1
      l = iord(i)
      CALL fndpnt(igpco(1,l),pont(l))
   ENDDO
   IF ( n1==4 ) THEN
!
!     FOUR  POINTS
!
!
!     TRIANGLE  1,2,3
!
      DO i = 1 , 3
         vect1(i) = gpco1(i) - gpco2(i)
         vect2(i) = gpco3(i) - gpco2(i)
      ENDDO
      CALL cross(vect1(1),vect2(1),vect(1))
      DO i = 1 , 3
         DO j = 1 , 3
            ploads(j,i) = vect(j)
         ENDDO
      ENDDO
!
!     TRIANGLE  2,3,4
!
      DO i = 1 , 3
         vect1(i) = gpco2(i) - gpco3(i)
         vect2(i) = gpco4(i) - gpco3(i)
      ENDDO
      CALL cross(vect1(1),vect2(1),vect(1))
      DO i = 2 , 4
         DO j = 1 , 3
            ploads(j,i) = ploads(j,i) + vect(j)
         ENDDO
      ENDDO
!
!     TRIANGLE  3,1,4
!
      DO i = 1 , 3
         vect1(i) = gpco4(i) - gpco1(i)
         vect2(i) = gpco3(i) - gpco1(i)
      ENDDO
      CALL cross(vect1(1),vect2(1),vect(1))
      DO i = 1 , 4
         IF ( i/=2 ) THEN
            DO j = 1 , 3
               ploads(j,i) = ploads(j,i) + vect(j)
            ENDDO
         ENDIF
      ENDDO
!
!     TRIANGLE (4,1,2)
!
      DO i = 1 , 3
         vect1(i) = gpco4(i) - gpco1(i)
         vect2(i) = gpco2(i) - gpco1(i)
      ENDDO
      CALL cross(vect1(1),vect2(1),vect(1))
      DO i = 1 , 4
         IF ( i/=3 ) THEN
            DO j = 1 , 3
               ploads(j,i) = ploads(j,i) + vect(j)
            ENDDO
         ENDIF
      ENDDO
      pmag = pmag/12.0
   ELSE
!
!     THREE  POINTS
!
      DO i = 1 , 3
         vect3(i) = gpco1(i) - gpco2(i)
         vect2(i) = gpco3(i) - gpco1(i)
         vect1(i) = gpco2(i) - gpco3(i)
      ENDDO
      CALL cross(vect3(1),vect1(1),vect(1))
!
      DO i = 1 , 3
         DO j = 1 , 3
            ploads(j,i) = -vect(j)
         ENDDO
      ENDDO
!
      IF ( Ksys88==1 ) THEN
!
!     IMPLEMENTED BY G.CHAN/UNISYS   3/1990
!     KSYS88 = 1, PRESSURE LOAD IS DISTRIBUTED PROPORTIONALLY TO THE
!     THREE ANGLE SIZES.
!     E.G. A 45-90-45 DEGREE TRIANGLE ELEMENT WILL HAVE TWICE THE LOAD
!     AT THE 90 DEGREE ANGLE TO THAT OF THE 45 DEGREE ANGLE.
!     RECTANGULAR ELEMENT (4 POINTS) IS NOT AFFECTED
!
!     GET AREA(2X), SIDES (VI) AND ANGLES (AI) OF THE TRIANGLE
!
         area = sqrt(vect(1)**2+vect(2)**2+vect(3)**2)
         v1 = sqrt(vect1(1)**2+vect1(2)**2+vect1(3)**2)
         v2 = sqrt(vect2(1)**2+vect2(2)**2+vect2(3)**2)
         v3 = sqrt(vect3(1)**2+vect3(2)**2+vect3(3)**2)
!
!     CHOOSE AN ANGLE, WHICH IS NOT THE LARGEST, TO START COMPUTING
!     THE THREE ANGLES
!
         IF ( v2>v1 .AND. v2>v3 ) THEN
!
            sin3 = area/(v2*v1)
            sin2 = v2*sin3/v3
            sin1 = v1*sin3/v3
            a3 = asin(sin3)
            IF ( sin2>=0.0 ) a2 = asin(sin2)
            IF ( sin1>=0.0 ) a1 = asin(sin1)
            IF ( v1>v2 ) a1 = pi - a3 - a2
            IF ( v2>v1 ) a2 = pi - a3 - a1
         ELSE
            sin2 = area/(v3*v1)
            sin1 = v1*sin2/v2
            sin3 = v3*sin2/v2
            a2 = asin(sin2)
            IF ( sin1>=0.0 ) a1 = asin(sin1)
            IF ( sin3>=0.0 ) a3 = asin(sin3)
            IF ( v1>v3 ) a1 = pi - a2 - a3
            IF ( v3>v1 ) a3 = pi - a2 - a1
         ENDIF
         pmag = 0.5*pmag/pi
         vect(1) = pmag*a1
         vect(2) = pmag*a2
         vect(3) = pmag*a3
      ELSE
!
!     KSYS88 = 0, PRESSURE LOAD IS DISTRIBUTED EVENLY (ONE-THIRD) TO
!     EACH OF THE 3 GRID POINTS. TRIANGULAR GEOMETRY IS NOT CONSIDERED.
!
         pmag = pmag/6
         vect(1) = pmag
         vect(2) = pmag
         vect(3) = pmag
      ENDIF
   ENDIF
!
!     TRANSFORM TO GLOBAL AND ADD CONTRIBUTIONS
!
   DO i = 1 , n1
      DO j = 1 , 3
         IF ( n1==4 ) ploads(j,i) = -ploads(j,i)*pmag
         IF ( n1==3 ) ploads(j,i) = -ploads(j,i)*vect(i)
      ENDDO
      IF ( igpco(1,i)/=0 ) CALL basglb(ploads(1,i),ploads(1,i),igpco(2,i),igpco(1,i))
      CALL fndsil(pont(i))
      DO j = 1 , 3
         in = pont(i) + j - 1
         Core(in) = ploads(j,i) + Core(in)
      ENDDO
   ENDDO
 100  RETURN
!
 200  CALL mesage(-1,Slt,name)
   GOTO 100
END SUBROUTINE pload
