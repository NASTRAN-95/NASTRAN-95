!*==sdhtff.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdhtff
   USE c_condas
   USE c_hmtout
   USE c_sdr2x4
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
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
   IF ( sub==2 .OR. sub==3 .OR. sub==4 .OR. sub==5 ) THEN
      k(1) = mato(1)
      k(2) = mato(2)
      k(3) = k(2)
      k(4) = mato(3)
      nq = 2
   ELSEIF ( sub==6 .OR. sub==7 .OR. sub==8 .OR. sub==9 .OR. sub==16 .OR. sub==17 ) THEN
      k(1) = mato(1)
      k(2) = mato(2)
      k(3) = mato(3)
      k(4) = k(2)
      k(5) = mato(4)
      k(6) = mato(5)
      k(7) = k(3)
      k(8) = k(6)
      k(9) = mato(6)
      nq = 3
   ELSE
      k(1) = mato(1)
      nq = 1
   ENDIF
   ip(1) = 1
   ip(2) = 2
   ip(3) = 3
   IF ( sub==17 ) THEN
!
!     IS2D8-CENTROID ONLY-WE NEED TO CONVERT ONLY GRIDS 5-8 TO LOCAL
!     COORDS
!
      DO i = 1 , 3
         zi(i) = r(i,2) - r(i,1)
      ENDDO
      zlen = sqrt(zi(1)**2+zi(2)**2+zi(3)**2)
      DO i = 1 , 3
         zi(i) = zi(i)/zlen
      ENDDO
      DO i = 5 , 8
         DO j = 1 , 3
            vec(j) = r(j,i) - r(j,1)
         ENDDO
         dr(1,i-4) = sadotb(vec,zi)
         CALL saxb(zi,vec,vvec)
         dr(2,i-4) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
      ENDDO
   ELSEIF ( sub/=3 .AND. sub/=5 ) THEN
      IF ( sub==2 .OR. sub==4 ) THEN
!
!     MOVE  TRIANGLES TO ELEMENT COORDINATES
!     (CTRIA3?)
!
         DO i = 1 , 3
            dr(i,1) = r(i,2) - r(i,1)
            dr(i,2) = r(i,3) - r(i,1)
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
         dr(i,1) = r(i,2) - r(i,1)
         dr(i,3) = r(i,3) - r(i,1)
         dr(i,2) = r(i,4) - r(i,2)
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
         dr(i,4) = r(i,4) - r(i,1)
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
   nel = nels(sub)
   xels = float(nel)
   DO iel = 1 , nel
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
!
            IF ( sub/=2 .AND. sub/=3 ) THEN
               IF ( sub==4 .OR. sub==5 ) THEN
!
!     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
!
                  af = 1.0
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( sub==6 ) THEN
!
!     SOLID ELEMENTS
!
                  DO i = 1 , 4
                     ip(i) = i
                  ENDDO
               ELSEIF ( sub==7 ) THEN
!
!     WEDGE
!
                  lrow = 4*iel - 4
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
               ELSEIF ( sub==8 .OR. sub==9 ) THEN
!
!     HEXA1 AND HEXA2 ELEMENTS
!
                  lrow = 4*iel + 8
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
               ELSEIF ( sub==10 .OR. sub==11 .OR. sub==12 .OR. sub==13 .OR. sub==14 .OR. sub==15 .OR. sub==18 ) THEN
!
!     BOUNDARY HEAT CONVECTION ELEMENTS
!
                  itype = sub - 9
                  IF ( itype>7 ) RETURN
                  IF ( itype==2 .OR. itype==6 .OR. itype==7 ) THEN
                     np = 2
                     c(1) = 0.5
                     c(2) = 0.5
                     el = sqrt((r(1,1)-r(1,2))**2+(r(2,1)-r(2,2))**2+(r(3,1)-r(3,2))**2)
                     fact = af*el*k(1)
                  ELSEIF ( itype==3 ) THEN
!
!     RING SURFACE
!
                     el = ((r(1,2)-r(1,1))**2+(r(3,2)-r(3,1))**2)
                     fact = 3.0*(r(1,1)+r(1,2))
                     c(1) = (2.0*r(1,1)+r(1,2))/fact
                     c(2) = (r(1,1)+2.0*r(1,2))/fact
                     fact = (r(1,1)+r(1,2))*pi*sqrt(el)*k(1)
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
                        dr(i,1) = r(i,i2) - r(i,i1)
                        dr(i,2) = r(i,i3) - r(i,i1)
                     ENDDO
                     CALL saxb(dr(1,1),dr(1,2),dr(1,3))
                     area = (sqrt(dr(1,3)**2+dr(2,3)**2+dr(3,3)**2))/2.0
                     IF ( itype==5 ) area = area/2.0
                     fact = fact + area*mato(1)
                     c(1) = 1.0/3.0
                     c(2) = c(1)
                     c(3) = c(1)
                     np = 3
                  ELSE
                     np = 1
                     c(1) = 1.0
                     fact = af*k(1)
                  ENDIF
!
!     SUPERIMPOSE C MATRIX INTO CE MATRIX
!
                  DO i = 1 , np
                     ig = ip(i)
                     ce(ig) = ce(ig) + c(i)/xels
                     ig = ip(i) + 4
                     ce(ig) = ce(ig) - c(i)/xels
                  ENDDO
                  k(1) = fact
                  CYCLE
               ELSEIF ( sub==16 ) THEN
!
!     ISOPARAMETRIC SOLIDS
!
                  ig = 0
                  DO i = 1 , 3
                     DO j = 1 , nsil
                        ig = ig + 1
                        ce(ig) = dshpb(i,j)
                     ENDDO
                  ENDDO
                  CYCLE
               ELSEIF ( sub==17 ) THEN
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
                     ce(i) = 0.
                  ENDDO
                  ce(5) = y68/denom
                  ce(6) = -y57/denom
                  ce(7) = -y68/denom
                  ce(8) = y57/denom
                  ce(13) = -x68/denom
                  ce(14) = x57/denom
                  ce(15) = x68/denom
                  ce(16) = -x57/denom
                  CYCLE
               ELSE
!
!     RODS,BARS, ETC.
!
                  el = 0.0
                  DO i = 1 , 3
                     el = el + (r(i,1)-r(i,2))**2
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
                     dr(j,i) = r(j,ig) - r(j,i1)
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
               DO j = 1 , nq
                  i1 = np*(j-1) + i
                  ig = nsil*(j-1) + ip(i)
                  ce(ig) = ce(ig) + c(i1)/xels
               ENDDO
            ENDDO
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE sdhtff
