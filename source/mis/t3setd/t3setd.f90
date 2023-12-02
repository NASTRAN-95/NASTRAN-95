!*==t3setd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE t3setd(Ierr,Sil,Jgpdt,Elth,Gpth,Dgpth,Egpdt,Gpnorm,Epnorm,Iorder,Teb,Tub,Cent,Avgthk,Lx,Ly,Edglen,Elid)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ierr
   INTEGER , DIMENSION(3) :: Sil
   INTEGER , DIMENSION(4,3) :: Jgpdt
   REAL :: Elth
   REAL , DIMENSION(3) :: Gpth
   REAL(REAL64) , DIMENSION(3) :: Dgpth
   REAL(REAL64) , DIMENSION(4,3) :: Egpdt
   REAL(REAL64) , DIMENSION(4,3) :: Gpnorm
   REAL(REAL64) , DIMENSION(4,3) :: Epnorm
   INTEGER , DIMENSION(3) :: Iorder
   REAL(REAL64) , DIMENSION(9) :: Teb
   REAL(REAL64) , DIMENSION(9) :: Tub
   REAL(REAL64) , DIMENSION(3) :: Cent
   REAL(REAL64) :: Avgthk
   REAL(REAL64) :: Lx
   REAL(REAL64) :: Ly
   REAL(REAL64) , DIMENSION(3) :: Edglen
   INTEGER :: Elid
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: area2 , cc , length , small
   REAL(REAL64) , DIMENSION(3,3) :: axis
   REAL , DIMENSION(4,3) :: bgpdt
   REAL(REAL64) , DIMENSION(3) :: edg12 , edg13 , edg23 , x , y , z
   REAL(REAL64) , DIMENSION(9) :: ggu
   INTEGER :: i , ii , ip , ipoint , isil , itemp , j , k , nnode , nodei , nodej , nodek
   INTEGER , DIMENSION(4,3) :: igpdt , igrid
   INTEGER , DIMENSION(3) :: ksil
   REAL , DIMENSION(3) :: tmpthk
   EXTERNAL betrnd , daxb
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     DOUBLE PRECISION ROUTINE TO DO THE SET-UP FOR TRIA3 ELEMENTS
!
!
!     INPUT :
!           SIL    - ARRAY OF SIL NUMBERS
!           JGPDT  - BGPDT DATA (INTEGER ARRAY)
!           ELTH   - ELEMENT THICKNESS FROM EPT
!           GPTH   - GRID POINT THICKNESS DATA
!           ELID   - ELEMENT ID
!     OUTPUT:
!           IERR   - ERROR FLAG
!           SIL    - ARRAY OF SIL NUMBERS       (REARRANGED)
!           JGPDT  - BGPDT DATA (INTEGER ARRAY) (REARRANGED)
!           GPTH   - GRID POINT THICKNESS DATA  (REARRANGED)
!           DGPTH  - GRID POINT THICKNESS DATA  (HIGH PREC)
!           EGPDT  - BGPDT DATA IN ELEMENT COORD. SYSTEM
!           GPNORM - GRID POINT NORMALS
!           EPNORM - GRID POINT NORMALS IN ELEMENT COORD .SYSTEM
!           IORDER - ARRAY OF ORDER INDICATORS FOR REARRANGED DATA
!           TEB    - TRANSFORMATION FROM ELEMENT TO BASIC COORD. SYSTEM
!           TUB    - TRANSFORMATION FROM USER TO BASIC COORD. SYSTEM
!           CENT   - LOCATION OF THE CENTER OF THE ELEMENT
!           AVGTHK - AVERAGE THICKNESS OF THE ELEMENT
!           LX     - DIMENSION OF ELEMENT ALONG X-AXIS
!           LY     - DIMENSION OF ELEMENT ALONG Y-AXIS
!           EDGLEN - EDGE LENGTHS
!
!
   !>>>>EQUIVALENCE (igpdt(1,1),bgpdt(1,1))
!
!
!     INITIALIZE
!
   Ierr = 0
   nnode = 3
!
   DO i = 1 , nnode
      DO j = 1 , 4
         igpdt(j,i) = Jgpdt(j,i)
      ENDDO
   ENDDO
!
!     SET UP THE USER COORDINATE SYSTEM
!
   DO i = 1 , 3
      ii = (i-1)*3
      DO j = 1 , 3
         ggu(ii+j) = dble(bgpdt(j+1,i))
      ENDDO
   ENDDO
   CALL betrnd(Tub,ggu,0,Elid)
!
!     SET UP THE ELEMENT COORDINATE SYSTEM
!
!     1. SET UP THE EDGE VECTORS AND THEIR LENGTHS
!
   DO i = 1 , nnode
      x(i) = bgpdt(2,i)
      y(i) = bgpdt(3,i)
      z(i) = bgpdt(4,i)
   ENDDO
!
   Cent(1) = (x(1)+x(2)+x(3))/3.0D0
   Cent(2) = (y(1)+y(2)+y(3))/3.0D0
   Cent(3) = (z(1)+z(2)+z(3))/3.0D0
!
   edg12(1) = x(2) - x(1)
   edg12(2) = y(2) - y(1)
   edg12(3) = z(2) - z(1)
   Edglen(1) = edg12(1)**2 + edg12(2)**2 + edg12(3)**2
   IF ( Edglen(1)/=0.0D0 ) THEN
      Edglen(1) = dsqrt(Edglen(1))
!
      edg23(1) = x(3) - x(2)
      edg23(2) = y(3) - y(2)
      edg23(3) = z(3) - z(2)
      Edglen(2) = edg23(1)**2 + edg23(2)**2 + edg23(3)**2
      IF ( Edglen(2)/=0.0D0 ) THEN
         Edglen(2) = dsqrt(Edglen(2))
!
         edg13(1) = x(3) - x(1)
         edg13(2) = y(3) - y(1)
         edg13(3) = z(3) - z(1)
         Edglen(3) = edg13(1)**2 + edg13(2)**2 + edg13(3)**2
         IF ( Edglen(3)/=0.0D0 ) THEN
            Edglen(3) = dsqrt(Edglen(3))
!
!     2. FIND THE SMALLEST EDGE LENGTH
!
            small = Edglen(1)
            nodei = 3
            nodej = 1
            nodek = 2
!
            IF ( Edglen(2)<small ) THEN
               small = Edglen(2)
               nodei = 1
               nodej = 2
               nodek = 3
            ENDIF
            IF ( Edglen(3)<small ) THEN
               small = Edglen(3)
               nodei = 2
               nodej = 1
               nodek = 3
            ENDIF
!
!     3. ESTABLISH AXIS 3 AND NORMALIZE IT
!
            CALL daxb(edg12,edg13,axis(1,3))
!
            length = dsqrt(axis(1,3)**2+axis(2,3)**2+axis(3,3)**2)
            axis(1,3) = axis(1,3)/length
            axis(2,3) = axis(2,3)/length
            axis(3,3) = axis(3,3)/length
            area2 = length
!
!     4. ESTABLISH AXES 1 AND 2 AND NORMALIZE THEM
!
            axis(1,1) = (x(nodej)+x(nodek))/2.0D0 - x(nodei)
            axis(2,1) = (y(nodej)+y(nodek))/2.0D0 - y(nodei)
            axis(3,1) = (z(nodej)+z(nodek))/2.0D0 - z(nodei)
!
            length = dsqrt(axis(1,1)**2+axis(2,1)**2+axis(3,1)**2)
            axis(1,1) = axis(1,1)/length
            axis(2,1) = axis(2,1)/length
            axis(3,1) = axis(3,1)/length
!
            CALL daxb(axis(1,3),axis(1,1),axis(1,2))
!
            DO i = 1 , 3
               Teb(i) = axis(i,1)
               Teb(i+3) = axis(i,2)
               Teb(i+6) = axis(i,3)
            ENDDO
!
            Lx = length
            Ly = area2/Lx
!
!
!     THE ELEMENT COORDINATE SYSTEM IS NOW READY
!
!     THE ARRAY IORDER STORES THE ELEMENT NODE ID IN INCREASING SIL
!     ORDER.
!
!     IORDER(1) = NODE WITH LOWEST  SIL NUMBER
!     IORDER(3) = NODE WITH HIGHEST SIL NUMBER
!
!     ELEMENT NODE NUMBER IS THE INTEGER FROM THE NODE LIST  G1,G2,....
!     THAT IS, THE 'I' PART OF THE 'GI' AS THEY ARE LISTED ON THE
!     CONNECTION BULK DATA CARD DESCRIPTION.
!
            DO i = 1 , nnode
               ksil(i) = Sil(i)
            ENDDO
!
            DO i = 1 , nnode
               itemp = 1
               isil = ksil(1)
               DO j = 2 , nnode
                  IF ( isil>ksil(j) ) THEN
                     itemp = j
                     isil = ksil(j)
                  ENDIF
               ENDDO
               Iorder(i) = itemp
               ksil(itemp) = 99999999
            ENDDO
!
!     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE GEOMETRY DATA
!     INTO INCREASING SIL ORDER.
!
            DO i = 1 , nnode
               ksil(i) = Sil(i)
               tmpthk(i) = Gpth(i)
               DO j = 1 , 4
                  igrid(j,i) = igpdt(j,i)
               ENDDO
            ENDDO
            DO i = 1 , nnode
               ipoint = Iorder(i)
               Sil(i) = ksil(ipoint)
               Gpth(i) = tmpthk(ipoint)
               DO j = 1 , 4
                  igpdt(j,i) = igrid(j,ipoint)
                  Jgpdt(j,i) = igpdt(j,i)
               ENDDO
            ENDDO
!
!     THE COORDINATES OF THE ELEMENT GRID POINTS MUST BE TRANSFORMED
!     FROM THE BASIC COORD. SYSTEM TO THE ELEMENT COORD. SYSTEM
!
            DO i = 1 , 3
               ip = (i-1)*3
               DO j = 1 , nnode
                  Egpdt(i+1,j) = 0.0D0
                  DO k = 1 , 3
                     cc = dble(bgpdt((k+1),j)) - Cent(k)
                     Egpdt(i+1,j) = Egpdt(i+1,j) + Teb(ip+k)*cc
                  ENDDO
               ENDDO
            ENDDO
!
!     SET NODAL NORMALS
!
            DO i = 1 , nnode
               Epnorm(1,i) = 0.0D0
               Epnorm(2,i) = 0.0D0
               Epnorm(3,i) = 0.0D0
               Epnorm(4,i) = 1.0D0
               Gpnorm(1,i) = 0.0D0
               Gpnorm(2,i) = Teb(7)
               Gpnorm(3,i) = Teb(8)
               Gpnorm(4,i) = Teb(9)
            ENDDO
!
!     SET NODAL THICKNESSES
!
            Avgthk = 0.0D0
            DO i = 1 , nnode
               IF ( Gpth(i)<0 ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
               IF ( Gpth(i)==0 ) THEN
                  IF ( Elth<=0 ) THEN
                     CALL spag_block_1
                     RETURN
                  ENDIF
                  Gpth(i) = Elth
               ENDIF
               Dgpth(i) = dble(Gpth(i))
               Avgthk = Avgthk + Dgpth(i)/nnode
            ENDDO
            RETURN
         ENDIF
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      Ierr = 1
   END SUBROUTINE spag_block_1
END SUBROUTINE t3setd
