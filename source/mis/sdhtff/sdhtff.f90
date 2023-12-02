!*==sdhtff.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdhtff
   IMPLICIT NONE
   USE C_CONDAS
   USE C_HMTOUT
   USE C_SDR2X4
   USE C_SDR2X5
   USE C_SDR2X6
!
! Local variable declarations rewritten by SPAG
!
   REAL :: area , denom , determ , el , fact , pi , x57 , x68 , xels , y57 , y68 , zlen
   REAL , DIMENSION(12) :: c
   REAL , DIMENSION(3,4) :: dr
   INTEGER :: i , i1 , i2 , i3 , iel , ig , ising , itype , j , lrow , nel , np
   INTEGER , DIMENSION(32) :: ip
   REAL , DIMENSION(9) :: kq
   INTEGER , DIMENSION(18) , SAVE :: nels
   INTEGER , DIMENSION(52) , SAVE :: smap
   REAL , DIMENSION(3) :: vec , vvec , zi
   EXTERNAL gmmats , invers , sadotb , saxb
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE CALCULATES THE PHASE 1 FLUX-TEMPERATURE RELATIONSHIPS
!
   !>>>>EQUIVALENCE (Consts(1),Pi)
   DATA nels/1 , 1 , 4 , 1 , 4 , 1 , 3 , 5 , 10 , 1 , 1 , 1 , 1 , 4 , 1 , 1 , 1 , 1/
   DATA smap/1 , 2 , 3 , 6 , 1 , 2 , 6 , 5 , 1 , 4 , 5 , 6 , 1 , 2 , 3 , 6 , 1 , 3 , 4 , 8 , 1 , 3 , 8 , 6 , 1 , 5 , 6 , 8 , 3 , 6 ,&
      & 7 , 8 , 2 , 3 , 4 , 7 , 1 , 2 , 4 , 5 , 2 , 4 , 5 , 7 , 2 , 5 , 6 , 7 , 4 , 5 , 7 , 8/
!
   IF ( Sub==2 .OR. Sub==3 .OR. Sub==4 .OR. Sub==5 ) THEN
      K(1) = Mato(1)
      K(2) = Mato(2)
      K(3) = K(2)
      K(4) = Mato(3)
      Nq = 2
   ELSEIF ( Sub==6 .OR. Sub==7 .OR. Sub==8 .OR. Sub==9 .OR. Sub==16 .OR. Sub==17 ) THEN
      K(1) = Mato(1)
      K(2) = Mato(2)
      K(3) = Mato(3)
      K(4) = K(2)
      K(5) = Mato(4)
      K(6) = Mato(5)
      K(7) = K(3)
      K(8) = K(6)
      K(9) = Mato(6)
      Nq = 3
   ELSE
      K(1) = Mato(1)
      Nq = 1
   ENDIF
   ip(1) = 1
   ip(2) = 2
   ip(3) = 3
   IF ( Sub==17 ) THEN
!
!     IS2D8-CENTROID ONLY-WE NEED TO CONVERT ONLY GRIDS 5-8 TO LOCAL
!     COORDS
!
      DO i = 1 , 3
         zi(i) = R(i,2) - R(i,1)
      ENDDO
      zlen = sqrt(zi(1)**2+zi(2)**2+zi(3)**2)
      DO i = 1 , 3
         zi(i) = zi(i)/zlen
      ENDDO
      DO i = 5 , 8
         DO j = 1 , 3
            vec(j) = R(j,i) - R(j,1)
         ENDDO
         dr(1,i-4) = sadotb(vec,zi)
         CALL saxb(zi,vec,vvec)
         dr(2,i-4) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
      ENDDO
   ELSEIF ( Sub/=3 .AND. Sub/=5 ) THEN
      IF ( Sub==2 .OR. Sub==4 ) THEN
!
!     MOVE  TRIANGLES TO ELEMENT COORDINATES
!     (CTRIA3?)
!
         DO i = 1 , 3
            dr(i,1) = R(i,2) - R(i,1)
            dr(i,2) = R(i,3) - R(i,1)
         ENDDO
!
         el = dr(1,1)**2 + dr(2,1)**2 + dr(3,1)**2
         el = sqrt(el)
         area = sadotb(dr(1,1),dr(1,2))/el
         CALL saxb(dr(1,1),dr(1,2),dr(1,3))
         dr(2,3) = sqrt(dr(1,3)**2+dr(2,3)**2+dr(3,3)**2)/el
         dr(1,3) = area
         dr(1,1) = 0.0
         dr(1,2) = el
         dr(2,1) = 0.0
         dr(2,2) = 0.0
      ENDIF
   ELSE
!
!     MOVE  QUADS TO ELEMENT COORDINATES
!     (CQUAD4? APPEARENTLY UP TO ELEMENT TYPE 52 ONLY)
!
      DO i = 1 , 3
         dr(i,1) = R(i,2) - R(i,1)
         dr(i,3) = R(i,3) - R(i,1)
         dr(i,2) = R(i,4) - R(i,2)
      ENDDO
      CALL saxb(dr(1,3),dr(1,2),dr(1,4))
!
      el = sqrt(dr(1,1)**2+dr(2,1)**2+dr(3,1)**2)
      area = sqrt(dr(1,4)**2+dr(2,4)**2+dr(3,4)**2)
!
      DO i = 1 , 3
         dr(i,1) = dr(i,1)/el
         dr(i,4) = dr(i,4)/area
      ENDDO
!
      CALL saxb(dr(1,4),dr(1,1),dr(1,2))
      DO i = 1 , 3
         dr(i,4) = R(i,4) - R(i,1)
      ENDDO
      CALL gmmats(dr(1,1),2,3,0,dr(1,3),2,3,1,kq)
      dr(1,3) = kq(1)
      dr(1,4) = kq(2)
      dr(2,3) = kq(3)
      dr(2,4) = kq(4)
      dr(1,2) = el
      dr(1,1) = 0.0
      dr(2,1) = 0.0
      dr(2,2) = 0.0
   ENDIF
!
!     LOOP  ON  SUBELEMENTS  (ONE FOR MOST)
!
   fact = 0.0
   nel = nels(Sub)
   xels = float(nel)
   DO iel = 1 , nel
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
!
            IF ( Sub/=2 .AND. Sub/=3 ) THEN
               IF ( Sub==4 .OR. Sub==5 ) THEN
!
!     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
!
                  Af = 1.0
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Sub==6 ) THEN
!
!     SOLID ELEMENTS
!
                  DO i = 1 , 4
                     ip(i) = i
                  ENDDO
               ELSEIF ( Sub==7 ) THEN
!
!     WEDGE
!
                  lrow = 4*iel - 4
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
               ELSEIF ( Sub==8 .OR. Sub==9 ) THEN
!
!     HEXA1 AND HEXA2 ELEMENTS
!
                  lrow = 4*iel + 8
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
               ELSEIF ( Sub==10 .OR. Sub==11 .OR. Sub==12 .OR. Sub==13 .OR. Sub==14 .OR. Sub==15 .OR. Sub==18 ) THEN
!
!     BOUNDARY HEAT CONVECTION ELEMENTS
!
                  itype = Sub - 9
                  IF ( itype>7 ) RETURN
                  IF ( itype==2 .OR. itype==6 .OR. itype==7 ) THEN
                     np = 2
                     c(1) = 0.5
                     c(2) = 0.5
                     el = sqrt((R(1,1)-R(1,2))**2+(R(2,1)-R(2,2))**2+(R(3,1)-R(3,2))**2)
                     fact = Af*el*K(1)
                  ELSEIF ( itype==3 ) THEN
!
!     RING SURFACE
!
                     el = ((R(1,2)-R(1,1))**2+(R(3,2)-R(3,1))**2)
                     fact = 3.0*(R(1,1)+R(1,2))
                     c(1) = (2.0*R(1,1)+R(1,2))/fact
                     c(2) = (R(1,1)+2.0*R(1,2))/fact
                     fact = (R(1,1)+R(1,2))*pi*sqrt(el)*K(1)
                     np = 2
                  ELSEIF ( itype==4 .OR. itype==5 ) THEN
!
!     TRIANGLES  (ALSO FOR SUBELEMENT OF QUAD)
!
                     DO i = 1 , 3
                        ig = i + iel - 1
                        IF ( ig>4 ) ig = ig - 4
                        ip(i) = ig
                     ENDDO
                     i1 = ip(1)
                     i2 = ip(2)
                     i3 = ip(3)
                     DO i = 1 , 3
                        dr(i,1) = R(i,i2) - R(i,i1)
                        dr(i,2) = R(i,i3) - R(i,i1)
                     ENDDO
                     CALL saxb(dr(1,1),dr(1,2),dr(1,3))
                     area = (sqrt(dr(1,3)**2+dr(2,3)**2+dr(3,3)**2))/2.0
                     IF ( itype==5 ) area = area/2.0
                     fact = fact + area*Mato(1)
                     c(1) = 1.0/3.0
                     c(2) = c(1)
                     c(3) = c(1)
                     np = 3
                  ELSE
                     np = 1
                     c(1) = 1.0
                     fact = Af*K(1)
                  ENDIF
!
!     SUPERIMPOSE C MATRIX INTO CE MATRIX
!
                  DO i = 1 , np
                     ig = ip(i)
                     Ce(ig) = Ce(ig) + c(i)/xels
                     ig = ip(i) + 4
                     Ce(ig) = Ce(ig) - c(i)/xels
                  ENDDO
                  K(1) = fact
                  CYCLE
               ELSEIF ( Sub==16 ) THEN
!
!     ISOPARAMETRIC SOLIDS
!
                  ig = 0
                  DO i = 1 , 3
                     DO j = 1 , Nsil
                        ig = ig + 1
                        Ce(ig) = Dshpb(i,j)
                     ENDDO
                  ENDDO
                  CYCLE
               ELSEIF ( Sub==17 ) THEN
!
!     IS2D8- SINCE CENTROID ONLY, WE CAN EASILY COMPUTE SHAPE FUNCTIONS
!     DERIVATIVES, JACOBIAN,ETC.. THE FINAL RESULT OF DNDX,DNDY=DNL IS
!     GIVEN
!
                  x68 = dr(1,2) - dr(1,4)
                  x57 = dr(1,1) - dr(1,3)
                  y68 = dr(2,2) - dr(2,4)
                  y57 = dr(2,1) - dr(2,3)
                  denom = -x68*y57 + x57*y68
                  DO i = 1 , 24
                     Ce(i) = 0.
                  ENDDO
                  Ce(5) = y68/denom
                  Ce(6) = -y57/denom
                  Ce(7) = -y68/denom
                  Ce(8) = y57/denom
                  Ce(13) = -x68/denom
                  Ce(14) = x57/denom
                  Ce(15) = x68/denom
                  Ce(16) = -x57/denom
                  CYCLE
               ELSE
!
!     RODS,BARS, ETC.
!
                  el = 0.0
                  DO i = 1 , 3
                     el = el + (R(i,1)-R(i,2))**2
                  ENDDO
                  el = sqrt(el)
                  c(1) = -1.0/el
                  c(2) = 1.0/el
                  np = 2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               i1 = ip(1)
               DO i = 1 , 3
                  ig = ip(i+1)
                  DO j = 1 , 3
                     dr(j,i) = R(j,ig) - R(j,i1)
                  ENDDO
               ENDDO
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
               ising = -1
               CALL invers(3,dr,3,c,0,determ,ising,c(4))
               DO i = 1 , 3
                  ig = 4*i - 4
                  c(ig+1) = 0.0
                  DO j = 2 , 4
                     i1 = ig + j
                     c(i1) = dr(j-1,i)
                     c(ig+1) = c(ig+1) - c(i1)
                  ENDDO
               ENDDO
               np = 4
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 2
         CASE (2)
            DO i = 1 , 3
               ig = i + iel - 1
               IF ( ig>4 ) ig = ig - 4
               ip(i) = ig
            ENDDO
            i1 = ip(1)
            i2 = ip(2)
            i3 = ip(3)
            area = dr(1,i1)*(dr(2,i2)-dr(2,i3)) + dr(1,i2)*(dr(2,i3)-dr(2,i1)) + dr(1,i3)*(dr(2,i1)-dr(2,i2))
            c(1) = (dr(2,i2)-dr(2,i3))/area
            c(2) = (dr(2,i3)-dr(2,i1))/area
            c(3) = (dr(2,i1)-dr(2,i2))/area
            c(4) = (dr(1,i3)-dr(1,i2))/area
            c(5) = (dr(1,i1)-dr(1,i3))/area
            c(6) = (dr(1,i2)-dr(1,i1))/area
!
            np = 3
            spag_nextblock_1 = 3
         CASE (3)
!
!     SUPERIMPOSE C MATRICES ONTO CE MATRICES OF THE WHOLE ELEMENT
!
            DO i = 1 , np
               DO j = 1 , Nq
                  i1 = np*(j-1) + i
                  ig = Nsil*(j-1) + ip(i)
                  Ce(ig) = Ce(ig) + c(i1)/xels
               ENDDO
            ENDDO
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE sdhtff
