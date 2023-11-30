
SUBROUTINE t3sets(Ierr,Sil,Jgpdt,Elth,Gpth,Dgpth,Egpdt,Gpnorm,Epnorm,Iorder,Teb,Tub,Cent,Avgthk,Lx,Ly,Edglen,Elid)
   IMPLICIT NONE
   REAL Avgthk , Elth , Lx , Ly
   INTEGER Elid , Ierr
   REAL Cent(3) , Dgpth(3) , Edglen(3) , Egpdt(4,3) , Epnorm(4,3) , Gpnorm(4,3) , Gpth(3) , Teb(9) , Tub(9)
   INTEGER Iorder(3) , Jgpdt(4,3) , Sil(3)
   REAL area2 , axis(3,3) , bgpdt(4,3) , cc , edg12(3) , edg13(3) , edg23(3) , ggu(9) , length , small , tmpthk(3) , x(3) , y(3) ,  &
      & z(3)
   INTEGER i , igpdt(4,3) , igrid(4,3) , ii , ip , ipoint , isil , itemp , j , k , ksil(3) , nnode , nodei , nodej , nodek
!
!     SINGLE PRECISION ROUTINE TO DO THE SET-UP FOR TRIA3 ELEMENTS
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
         ggu(ii+j) = bgpdt(j+1,i)
      ENDDO
   ENDDO
   CALL betrns(Tub,ggu,0,Elid)
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
   Cent(1) = (x(1)+x(2)+x(3))/3.0
   Cent(2) = (y(1)+y(2)+y(3))/3.0
   Cent(3) = (z(1)+z(2)+z(3))/3.0
!
   edg12(1) = x(2) - x(1)
   edg12(2) = y(2) - y(1)
   edg12(3) = z(2) - z(1)
   Edglen(1) = edg12(1)**2 + edg12(2)**2 + edg12(3)**2
   IF ( Edglen(1)/=0.0 ) THEN
      Edglen(1) = sqrt(Edglen(1))
!
      edg23(1) = x(3) - x(2)
      edg23(2) = y(3) - y(2)
      edg23(3) = z(3) - z(2)
      Edglen(2) = edg23(1)**2 + edg23(2)**2 + edg23(3)**2
      IF ( Edglen(2)/=0.0 ) THEN
         Edglen(2) = sqrt(Edglen(2))
!
         edg13(1) = x(3) - x(1)
         edg13(2) = y(3) - y(1)
         edg13(3) = z(3) - z(1)
         Edglen(3) = edg13(1)**2 + edg13(2)**2 + edg13(3)**2
         IF ( Edglen(3)/=0.0 ) THEN
            Edglen(3) = sqrt(Edglen(3))
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
            CALL saxb(edg12,edg13,axis(1,3))
!
            length = sqrt(axis(1,3)**2+axis(2,3)**2+axis(3,3)**2)
            axis(1,3) = axis(1,3)/length
            axis(2,3) = axis(2,3)/length
            axis(3,3) = axis(3,3)/length
            area2 = length
!
!     4. ESTABLISH AXES 1 AND 2 AND NORMALIZE THEM
!
            axis(1,1) = (x(nodej)+x(nodek))/2.0 - x(nodei)
            axis(2,1) = (y(nodej)+y(nodek))/2.0 - y(nodei)
            axis(3,1) = (z(nodej)+z(nodek))/2.0 - z(nodei)
!
            length = sqrt(axis(1,1)**2+axis(2,1)**2+axis(3,1)**2)
            axis(1,1) = axis(1,1)/length
            axis(2,1) = axis(2,1)/length
            axis(3,1) = axis(3,1)/length
!
            CALL saxb(axis(1,3),axis(1,1),axis(1,2))
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
                  Egpdt(i+1,j) = 0.0
                  DO k = 1 , 3
                     cc = bgpdt((k+1),j) - Cent(k)
                     Egpdt(i+1,j) = Egpdt(i+1,j) + Teb(ip+k)*cc
                  ENDDO
               ENDDO
            ENDDO
!
!     SET NODAL NORMALS
!
            DO i = 1 , nnode
               Epnorm(1,i) = 0.0
               Epnorm(2,i) = 0.0
               Epnorm(3,i) = 0.0
               Epnorm(4,i) = 1.0
               Gpnorm(1,i) = 0.0
               Gpnorm(2,i) = Teb(7)
               Gpnorm(3,i) = Teb(8)
               Gpnorm(4,i) = Teb(9)
            ENDDO
!
!     SET NODAL THICKNESSES
!
            Avgthk = 0.0
            DO i = 1 , nnode
               IF ( Gpth(i)<0 ) GOTO 100
               IF ( Gpth(i)==0 ) THEN
                  IF ( Elth<=0.0 ) GOTO 100
                  Gpth(i) = Elth
               ENDIF
               Dgpth(i) = Gpth(i)
               Avgthk = Avgthk + Dgpth(i)/nnode
            ENDDO
            GOTO 99999
         ENDIF
      ENDIF
   ENDIF
!
 100  Ierr = 1
99999 RETURN
END SUBROUTINE t3sets