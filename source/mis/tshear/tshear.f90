!*==tshear.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tshear
   USE c_matin
   USE c_matout
   USE c_ssgett
   USE c_ssgwrk
   USE c_trimex
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: igrid2
   INTEGER , DIMENSION(4,4) :: ncsid
   REAL :: vkl
   EXTERNAL basglb , mat , norm , saxb , ssgetd
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT TEMPERATURE AND DEFORMATION LOADING FOR THE SHEAR PANEL.
!
!     FORMULATION IS THAT OF A PSEUDO-ROD ON EACH EDGE OF THE SHEAR
!     PANEL.
!
!     ECPT( 1)         - ELEMENT ID
!     ECPT( 2 THRU 5)  - 4 GRID SILS
!     ECPT( 6)         - MATERIAL ID
!     ECPT( 7)         - THICKNESS
!     ECPT( 8)         - NON-STRUCTURAL MASS
!     ECPT( 9 THRU 24) - 4 POINTS (CSID,X,Y,Z) REPEATS
!     ECPT(25)         - ELEMENT TEMPERATURE
!     ECPT(26)         - F1 EFFECTIVENESS FACTOR DIRECTION 1, (NOT USED)
!     ECPT(27)         - F2 EFFECTIVENESS FACTOR DIRECTION 2, (NOT USED)
!
   !>>>>EQUIVALENCE (Ncsid,Csid)
!
   f12(1) = 1.00
   f12(2) = 1.00
!
   IF ( f12(1)==0.0 .AND. f12(2)==0.0 ) RETURN
!
!     MATERIAL DATA ACQUISITION
!
   inflag = 1
   matid = mid
   temp = eltemp
   CALL mat(ecpt(1))
!
!     GRID POINT TEMPERATURES
!
   IF ( itemp/=0 ) THEN
      CALL ssgetd(ecpt(1),ti,4)
!
!     ELEMENT DEFORMATION (NOT USED)
!
!     4 NORMALIZED EDGE VECTORS AND LENGTHS
!
      DO i = 1 , 4
         igrid2 = i + 1
         IF ( i==4 ) igrid2 = 1
!
         DO j = 1 , 3
            vec(j,i) = csid(j+1,i) - csid(j+1,igrid2)
         ENDDO
!
         CALL norm(vec(1,i),xl(i))
      ENDDO
!
      IF ( f12(1)<=1.01 .OR. f12(2)<=1.01 ) THEN
!
!     PROJECTED AREA IS NEEDED. FIRST OBTAIN THE DIAGONAL VECTORS.
!
         DO i = 1 , 3
            diag1(i) = csid(i+1,3) - csid(i+1,1)
            diag2(i) = csid(i+1,4) - csid(i+1,2)
         ENDDO
!
!     NORMAL VECTOR (DIAG1 X DIAG2)
!
         CALL saxb(diag1,diag2,diag2)
         CALL norm(diag2,vkl)
         pa = 0.5*vkl
      ENDIF
!
!     LOOP THROUGH LOADS ON 4 EDGES.
!
      tsq = thick*thick
      DO i = 1 , 4
         i12 = mod(i,2)
         IF ( i12==0 ) i12 = 2
         ia = i
         ib = ia + 1
         IF ( i==4 ) ib = 1
!
!     TEMPERATURE
!
         tbar = (ti(ia+1)+ti(ib+1))/2.0 - to1
!
!     EXTENSIONAL AREA
!
         IF ( f12(i12)<=1.01 ) THEN
            area = f12(i12)*pa*thick/(xl(i12)+xl(i12+2))
         ELSE
            area = 0.50*f12(i12)*tsq
         ENDIF
!
         vmag = e1*area*alpha*tbar
         DO j = 1 , 3
            veca(j) = vmag*vec(j,i)
            vecb(j) = -veca(j)
         ENDDO
!
         IF ( ncsid(1,ib)/=0 ) CALL basglb(vecb(1),vecb(1),isils(ib),csid(1,ib))
         in = isils(ib) - 1
         DO j = 1 , 3
            in = in + 1
            pg(in) = pg(in) + vecb(j)
         ENDDO
!
         IF ( ncsid(1,ia)/=0 ) CALL basglb(veca(1),veca(1),isils(ia),csid(1,ia))
         in = isils(ia) - 1
         DO j = 1 , 3
            in = in + 1
            pg(in) = pg(in) + veca(j)
         ENDDO
!
      ENDDO
   ENDIF
END SUBROUTINE tshear
