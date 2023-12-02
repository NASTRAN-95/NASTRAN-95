!*==plod4d.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plod4d
   USE c_loadx
   USE c_pindex
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: area , eps , eta , weight , xsi
   REAL , DIMENSION(4,4) :: bgpdt
   INTEGER :: cid , i , ieta , ii , iii , ipoint , isil , itemp , ixsi , j , jb , jp , kk , ksild , ncrd , nsurf , numint , nvct ,  &
            & swp
   REAL(REAL64) , DIMENSION(3,4) :: dpe
   REAL(REAL64) , DIMENSION(8) :: dshp , dshptp
   REAL(REAL64) , DIMENSION(3) :: gauss , v3t , vi , vj , wtgaus
   INTEGER , DIMENSION(4,4) :: ibgpdt , itgrid
   INTEGER , DIMENSION(4) :: iorder , ksil , sil
   INTEGER , DIMENSION(11) :: islt
   INTEGER , DIMENSION(1) :: iz
   REAL , DIMENSION(3) :: locate , nv , nvx
   INTEGER , SAVE :: ndof
   INTEGER , DIMENSION(125) :: nest
   REAL :: p , x
   REAL , DIMENSION(3,4) :: pe
   REAL , DIMENSION(4) :: ppp
   REAL(REAL64) , DIMENSION(4) :: shp , tmpshp
   EXTERNAL basglb , daxb , glbbas , q4shpd
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO PROCESS PLOAD4 BULK DATA TO CREATE LOADS ON QUAD4
!     ELEMENTS
!
!     DOUBLE PRECISION VERSION.
!
!     GRID POINT NUMBERING IS COUNTER-CLOCKWISE
!     GRIDS 1,2,3, AND 4 ARE AT THE CORNERS
!
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (Slt(1),Islt(1))
   !>>>>EQUIVALENCE (Nest(1),Best(1)) , (Numint,Nest(25))
   !>>>>EQUIVALENCE (Sil(1),Nest(2)) , (Bgpdt(1,1),Best(29))
   !>>>>EQUIVALENCE (Ibgpdt(1,1),Bgpdt(1,1))
   DATA ndof/3/
!
!                 EST LISTING
!     --------------------------------------------------
!      1          EID
!      2 THRU 5   SILS, GRIDS 1 THRU 4
!      6 THRU 9   T (MEMBRANE), GRIDS 1 THRU 4
!     10          THETA (MATERIAL)
!     11          TYPE FLAG FOR WORD 10
!     12          ZOFF  (OFFSET)
!     13          MATERIAL ID FOR MEMBRANE
!     14          T (MEMBRANE)
!     15          MATERIAL ID FOR BENDING
!     16          I FACTOR (BENDING)
!     17          MATERIAL ID FOR TRANSVERSE SHEAR
!     18          FACTOR FOR T(S)
!     19          NSM (NON-STRUCTURAL MASS)
!     20 THRU 21  Z1, Z2  (STRESS FIBRE DISTANCES)
!     22          MATERIAL ID FOR MEMBRANE-BENDING COUPLING
!     23          THETA (MATERIAL) FROM PSHELL CARD
!     24          TYPE FLAG FOR WORD 23
!     25          INTEGRATION ORDER
!     26          THETA (STRESS)
!     27          TYPE FLAG FOR WORD 26
!     28          ZOFF1 (OFFSET)  OVERRIDDEN BY EST(12)
!     29 THRU 44  CID,X,Y,Z - GRIDS 1 THRU 4
!     45          ELEMENT TEMPERATURE
!
!                 DATA FROM THE PLOAD4 CARD DESCRIBED HERE
!     ------------------------------------------------------------
!     EID  ELEMENT ID
!     P1,P2,P3,P4 CORNER GRID POINT PRESSURES PER UNIT SURFACE AREA
!     G1,G3  DEFINES QUADRILATERAL SURFACE OF HEXA, QUAD8, AND
!                 PENTA SURFACES ON WHICH PRESSURE LOADS EXIST
!                 OTHERWISE SURFACE IS TRIANGULAR IF G3 IS ZERO OR
!                 BLANK, SURFACE IS TRIANGULAR
!     CID    COORDINATE SYSTEM FOR DEFINITION OF PRESSURE VECTOR
!     N1,N2,N3 COMPONENTS OF PRESSURE DIRECTION VECTOR IF CID
!                 'BLANK' OR ZERO, THE PRESSURE ACTS NORMAL TO THE
!                 SURFACE OF THE ELEMENT
!
!     EQUIVALENT NUMERICAL INTEGRATION POINT LOADS PP(III) ARE
!     OBTAINED VIA BI-LINEAR INTERPOLATION
!
!     GENERAL INITIALIZATION.
!
!     BEST(45) IS THE DATA FOR EST WHICH IS READ IN EXTERN AND IS
!     READY TO BE USED.
!
!     READ FROM PLOAD4 CARDS
!     P1 = PPP(1)
!     P2 = PPP(2)
!     P3 = PPP(3)
!     P4 = PPP(4)
!     CID,N1,N2,N3
!
!     X WILL BE THE LENGTH OF THE PRESSURE VECTOR FOR NORMALIZATION.
!     NV(I) WILL BE THE NORMALIZED PRESSURE VECTOR
!
   x = 0.0
   DO i = 1 , 4
      ppp(i) = slt(i+1)
   ENDDO
   DO i = 1 , 3
      nv(i) = slt(i+8)
      x = x + nv(i)*nv(i)
   ENDDO
   cid = islt(8)
!
   IF ( x/=0.0 ) THEN
      x = sqrt(x)
      DO i = 1 , 3
         nv(i) = nv(i)/x
      ENDDO
   ENDIF
!
   ncrd = 3
!
!     PERFORM TEST FOR PRESENCE OF CONSTANT PRESSURE SET SWP
!
   swp = 1
   IF ( ppp(2)==0.D0 .AND. ppp(3)==0.D0 .AND. ppp(4)==0.D0 ) swp = 0
   nsurf = 4
!
!     THE ARRAY IORDER STORES THE ELEMENT NODE ID IN INCREASING SIL
!     ORDER.
!
!     IORDER(1) = NODE WITH LOWEST  SIL NUMBER
!     IORDER(4) = NODE WITH HIGHEST SIL NUMBER
!
!     ELEMENT NODE NUMBER IS THE INTEGER FROM THE NODE LIST G1,G2,G3,G4.
!     THAT IS, THE 'I' PART OF THE 'GI' AS THEY ARE LISTED ON THE
!     CONNECTIVITY  BULK DATA CARD DESCRIPTION.
!
   ksild = 99999995
   DO i = 1 , 4
      iorder(i) = 0
      ksil(i) = sil(i)
   ENDDO
   DO i = 1 , 4
      itemp = 1
      isil = ksil(1)
      DO j = 2 , 4
         IF ( isil>ksil(j) ) THEN
            itemp = j
            isil = ksil(j)
         ENDIF
      ENDDO
      iorder(i) = itemp
      ksil(itemp) = 99999999
   ENDDO
!
!     ADJUST EST DATA
!
!     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE
!     GEOMETRY DATA INTO INCREASING SIL ORDER.
!
   DO i = 1 , 4
      ksil(i) = sil(i)
      DO j = 1 , 4
         itgrid(j,i) = ibgpdt(j,i)
      ENDDO
   ENDDO
   DO i = 1 , 4
      ipoint = iorder(i)
      sil(i) = ksil(ipoint)
      DO j = 1 , 4
         ibgpdt(j,i) = itgrid(j,ipoint)
      ENDDO
   ENDDO
!
   nvct = ncrd*4
   eps = 0.001D0
!
!     SET VALUES FOR NUMERICAL INTEGRATION POINTS AND WEIGHT FACTORS
!
!     DEFAULT INTEGRATION ORDER IS 2X2
!
   numint = 2
   gauss(1) = -0.57735026918962D0
   gauss(2) = +0.57735026918962D0
   wtgaus(1) = 1.0D0
   wtgaus(2) = 1.0D0
!
!     ZERO OUT THE LOAD ROW SET
!
   DO i = 1 , ndof
      DO j = 1 , 4
         dpe(i,j) = 0.0D0
      ENDDO
   ENDDO
!
!     SET UP THE LOOPS FOR NUMERICAL INTEGRATION
!
   DO ieta = 1 , numint
      eta = gauss(ieta)
      DO ixsi = 1 , numint
         xsi = gauss(ixsi)
         weight = wtgaus(ixsi)*wtgaus(ieta)
         p = 0.0D0
!
!     P1,P2,P3,P4 ARE THE GRID POINT PRESSURE LOADS PER UNIT
!     AREA FROM THE PLOAD4 CARD.  THESE WILL BE USED WITH A
!     BILINEAR SHAPE FUNCTION ROUTINE TO CALCULATE THE NODAL
!     LOADS.
!
!     BILINEAR CASE WHERE THE VALUES OF XSI,ETA ARE INPUT IN
!     EXPLICIT FORM DEPENDING UPON WHICH NUMERICAL INTEGRATION
!     SCHEME IS BEING USED.
!
!
!     NSURF IS AN INTEGER WHICH KEEPS TRACK OF THE SURFACE TYPE
!              NSURF = 3 . . .  TRIANGULAR SURFACE
!              NSURF = 4 . . .  QUADRILATERAL SURFACE
!
!
!     CALL SHAPE FCN. ROUTINE FOR THE BILINEAR QUAD4.  INPUT IS
!     XSI,ETA,III AND EVALUATION OF SHAPE FCN. AT INTEG.PTS
!     WILL BE PERFORMED.
!
         CALL q4shpd(xsi,eta,shp,dshp)
!
         IF ( swp==0 ) p = ppp(1)
         IF ( swp/=0 ) THEN
!
            DO iii = 1 , nsurf
               p = p + shp(iii)*ppp(iii)
            ENDDO
         ENDIF
!
!     SORT THE SHAPE FUNCTIONS AND THEIR DERIVATIVES INTO SIL ORDER.
!
         DO i = 1 , 4
            tmpshp(i) = shp(i)
            dshptp(i) = dshp(i)
            dshptp(i+4) = dshp(i+4)
         ENDDO
         DO i = 1 , 4
            kk = iorder(i)
            shp(i) = tmpshp(kk)
            dshp(i) = dshptp(kk)
            dshp(i+4) = dshptp(kk+4)
         ENDDO
!
!     COMPUTE THE UNIT NORMALS V3T AT EACH GRID POINT.  THESE WILL
!     BE USED TO GET COMPONENTS OF PRESSURE VECTOR ACTING NORMAL TO
!     THE SURFACE.  AREA CALCULATION CHECKS THE GEOMETRY OF THE
!     ELEMENT.
!
         DO i = 1 , 3
            vi(i) = 0.0D0
            vj(i) = 0.0D0
            DO j = 1 , 4
               ii = i + 1
               vi(i) = vi(i) + bgpdt(ii,j)*dshp(j)
               vj(i) = vj(i) + bgpdt(ii,j)*dshp(j+4)
            ENDDO
         ENDDO
!
!     CHECK FOR USER INPUT VECTOR TO ROTATE LOADS
!
         CALL daxb(vi,vj,v3t)
         area = dsqrt(v3t(1)**2+v3t(2)**2+v3t(3)**2)
         IF ( area>0.0D0 ) THEN
!
            IF ( x/=0.0 ) THEN
!
!     CHECK FOR NON-ZERO CID AND NEED TO ROTATE USER'S VECTOR
!
               IF ( cid==0 ) THEN
!
!     NOW ROTATE THE PRESSURE LOAD
!
                  v3t(1) = nv(1)*area
                  v3t(2) = nv(2)*area
                  v3t(3) = nv(3)*area
               ELSE
!
!     COMPUTE THE LOCATION OF THE INTEGRATION POINT SO THAT WE CAN
!     ROTATE THE USER VECTOR PER CID. THIS LOCATION REQUIRED ONLY IF
!     CID IS CYLINDRICAL OR SPHERICAL.
!
                  locate(1) = 0.
                  locate(2) = 0.
                  locate(3) = 0.
                  DO j = 1 , 4
                     locate(1) = locate(1) + bgpdt(2,j)*shp(j)
                     locate(2) = locate(2) + bgpdt(3,j)*shp(j)
                     locate(3) = locate(3) + bgpdt(4,j)*shp(j)
                  ENDDO
                  CALL glbbas(nv(1),nvx(1),locate(1),cid)
!
!     NOW ROTATE THE PRESSURE LOAD
!
                  v3t(1) = nvx(1)*area
                  v3t(2) = nvx(2)*area
                  v3t(3) = nvx(3)*area
               ENDIF
            ENDIF
!
!     COMPUTE THE CONTRIBUTION TO THE LOAD MATRIX FROM THIS
!     INTEGRATION POINT AS NT*P*V3T
!
            DO i = 1 , 4
               DO j = 1 , ndof
                  dpe(j,i) = dpe(j,i) + weight*p*shp(i)*v3t(j)
               ENDDO
            ENDDO
         ELSE
!
            WRITE (nout,99001) sfm , nest(1)
99001       FORMAT (A25,'.  BAD GEOMETRY DETECTED FOR QUAD4 ELEMENT ',I8,' WHILE PROCESSING PLOAD4 DATA.')
            nogo = 1
            RETURN
         ENDIF
!
      ENDDO
   ENDDO
!
!     END OF NUMERICAL INTEGRATION LOOPS
!
!     MOVE DATA FROM DOUBLE PRECISION ARRAY TO SINGLE PRECISION
!
   DO j = 1 , 4
      pe(1,j) = dpe(1,j)
      pe(2,j) = dpe(2,j)
      pe(3,j) = dpe(3,j)
   ENDDO
!
!     ADD ELEMENT LOAD TO OVERALL LOAD.
!
   jb = 25
   DO j = 1 , 4
      jb = jb + 4
      IF ( nest(jb)/=0 ) CALL basglb(pe(1,j),pe(1,j),best(jb+1),nest(jb))
      jp = sil(j) - 1
      DO i = 1 , 3
         z(jp+i) = z(jp+i) + pe(i,j)
      ENDDO
   ENDDO
END SUBROUTINE plod4d
