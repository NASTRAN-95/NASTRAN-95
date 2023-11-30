
SUBROUTINE flface(Type,Ect,Elt,Grid)
   IMPLICIT NONE
   LOGICAL Error
   INTEGER Ibgpdt , Icore , Lcore , Nbgpdt , Nout
   REAL Sysbuf , Z(1)
   CHARACTER*23 Ufm
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Nbgpdt
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Type
   INTEGER Ect(10) , Elt(7) , Grid(4)
   REAL angle(6) , area , cs(3) , dist , heigth(6) , kf(3) , ks(3) , mag , r1(3) , r2(3) , r3(3) , tol
   INTEGER face(4,6,4) , faceid , gf(10) , gf1 , gf2 , gf3 , gf4 , gridf(4) , gs1 , gs2 , gs3 , gs4 , hex1(4,6) , hex2(4,6) , i ,   &
         & if , intype , j , nf , nface(4) , ngridf , ngrids , tetra(4,4) , wedge(4,5)
!
!     LOCATES THE FLUID GRID POINTS DEFINING THE FACE OF A FLUID
!     ELEMENT.  THE FACE MAY BE SPECIFIED IN TWO MANNERS.
!
!     1) FACE NUMBER - ELT(2) LESS THEN ZERO AND FACE = ELT(3)
!     2) STRUCTURAL ELEMENT WHICH COINCIDES WITH FACE -
!                         ELT(2) = ELEMENT ID AND ELT(3)-ELT(6) = GRIDS
!
   EQUIVALENCE (hex1(1,1),face(1,1,1)) , (hex2(1,1),face(1,1,2)) , (tetra(1,1),face(1,1,3)) , (wedge(1,1),face(1,1,4))
!
!     DATA DEFINING FACES OF THE FLUID ELEMENTS
!
!     NUMBER OF GRID POINTS PER ELEMENT
!
!                   FHEX1     FHEX2     FTETRA    FWEDGE
!
   DATA gridf/8 , 8 , 4 , 6/
!
!     NUMBER OF FACES ON EACH ELEMENT
!
   DATA nface/6 , 6 , 4 , 5/
!
!     GRID POINTS WHICH DEFINE FACES FOR FHEX1 ELEMENTS
!
   DATA hex1/1 , 4 , 3 , 2 , 1 , 2 , 6 , 5 , 2 , 3 , 7 , 6 , 3 , 4 , 8 , 7 , 4 , 1 , 5 , 8 , 5 , 6 , 7 , 8/
!
!     GRID POINTS WHICH DEFINE FACES FOR FHEX2 ELEMENTS
!
   DATA hex2/1 , 4 , 3 , 2 , 1 , 2 , 6 , 5 , 2 , 3 , 7 , 6 , 3 , 4 , 8 , 7 , 4 , 1 , 5 , 8 , 5 , 6 , 7 , 8/
!
!     GRID POINTS WHICH DEFINE FACES FOR FTETRA ELEMENTS
!
   DATA tetra/1 , 3 , 2 , -1 , 1 , 2 , 4 , -1 , 2 , 3 , 4 , -1 , 3 , 1 , 4 , -1/
!
!     GRID POINTS WHICH DEFINE FACES FOR FWEDGE ELEMENTS
!
   DATA wedge/1 , 3 , 2 , -1 , 1 , 2 , 5 , 4 , 2 , 3 , 6 , 5 , 3 , 1 , 4 , 6 , 4 , 5 , 6 , -1/
!
!
!     DETERMINE HOW THE FACE IS SPECIFIED
!
!     SUBTRACT IFP CARD NUMBER OF ELEMENT JUST BEFORE CFHEX1 FROM TYPE
   intype = Type - 332
!
   nf = nface(intype)
   IF ( Elt(2)<0 ) THEN
!
!     THE FACE IS DEFINED BY A FACE ID
!
      faceid = Elt(3)
      IF ( faceid<1 .OR. faceid>nf ) THEN
!
!     ILLEGAL FACE NUMBER
!
         WRITE (Nout,99001) Ufm , faceid , Ect(1)
99001    FORMAT (A23,' 8009. FACE',I9,' SPECIFIED FOR FLUID ELEMENT',I9,' IS AN ILLEGAL VALUE.')
         GOTO 200
      ENDIF
   ELSE
!
!     THE FACE IS DEFINED BY STRUCTURAL GRIDS
!
!     INITIALIZE POINTERS TO GRID POINT DATA
!
      ngrids = 4
      IF ( Elt(6)<0 ) ngrids = 3
      gs1 = Ibgpdt + (Elt(3)-1)*4
      gs2 = Ibgpdt + (Elt(4)-1)*4
      gs3 = Ibgpdt + (Elt(5)-1)*4
      gs4 = -1
      IF ( ngrids==4 ) gs4 = Ibgpdt + (Elt(6)-1)*4
!
      ngridf = gridf(intype)
      DO i = 1 , ngridf
         gf(i) = Ibgpdt + (Ect(i+2)-1)*4
      ENDDO
!
!     FIND NORMAL VECTOR TO STRUCTURAL ELEMENT FACE
!
      DO i = 1 , 3
         r1(i) = Z(gs2+i) - Z(gs1+i)
         r2(i) = Z(gs3+i) - Z(gs1+i)
      ENDDO
!
      ks(1) = r1(2)*r2(3) - r1(3)*r2(2)
      ks(2) = r1(3)*r2(1) - r1(1)*r2(3)
      ks(3) = r1(1)*r2(2) - r1(2)*r2(1)
!
      mag = sqrt(ks(1)**2+ks(2)**2+ks(3)**2)
      IF ( mag<1.0E-7 ) THEN
!
!     ERROR CONDITIONS
!
!     BAD GEOMETRY FOR STRUCTURAL ELEMENT
!
         WRITE (Nout,99002) Ufm , Elt(2)
!
!     ERROR FORMATS
!
99002    FORMAT (A23,' 8005. BAD GEOMETRY DEFINED FOR STRUCTURAL ELEMENT',I9)
         GOTO 200
      ELSE
         DO i = 1 , 3
            ks(i) = ks(i)/mag
         ENDDO
!
!     FIND AREA OF STRUCTURE FACE AND TOLERANCE USED IN CHECKING
!     SEPERATIOON
!
         area = mag
         IF ( gs4<0 ) area = mag/2.0
         tol = .2*sqrt(area)
!
!     FIND CENTROID OF STRUCTURAL FACE
!
         DO i = 1 , 3
            cs(i) = Z(gs1+i) + Z(gs2+i) + Z(gs3+i)
            IF ( ngrids==4 ) cs(i) = cs(i) + Z(gs4+i)
            cs(i) = cs(i)/float(ngrids)
         ENDDO
!
!     PROCESS EACH FACE OF THE FLUID ELEMENT - FIRST GET GRID POINTERS
!     POINTERS
!
         DO if = 1 , nf
            i = face(1,if,intype)
            gf1 = gf(i)
            i = face(2,if,intype)
            gf2 = gf(i)
            i = face(3,if,intype)
            gf3 = gf(i)
            i = face(4,if,intype)
            gf4 = -1
            IF ( i>0 ) gf4 = gf(i)
!
!     FIND NORMAL TO FLUID FACE
!
            DO i = 1 , 3
               r2(i) = Z(gf2+i) - Z(gf1+i)
               r3(i) = Z(gf3+i) - Z(gf1+i)
            ENDDO
!
            kf(1) = r2(2)*r3(3) - r2(3)*r3(2)
            kf(2) = r2(3)*r3(1) - r2(1)*r3(3)
            kf(3) = r2(1)*r3(2) - r2(2)*r3(1)
!
            mag = sqrt(kf(1)**2+kf(2)**2+kf(3)**2)
            IF ( mag<1.0E-7 ) GOTO 100
            DO i = 1 , 3
               kf(i) = kf(i)/mag
            ENDDO
!
!     DETERMINE ANGLE BETWEEN FACES
!
            angle(if) = kf(1)*ks(1) + kf(2)*ks(2) + kf(3)*ks(3)
            IF ( abs(angle(if))>.866 ) THEN
!
!     FIND DISTANCE FROM THE CENTROID OF THE STRUCTURE TO THE FLUID
!     FACE.  THE DISTANCE IS MEASURED ALONG THE NORMAL TO THE
!     FLUID FACE
!
               DO i = 1 , 3
                  r2(i) = cs(i) - Z(gf1+i)
               ENDDO
!
               heigth(if) = abs(kf(1)*r2(1)+kf(2)*r2(2)+kf(3)*r2(3))
            ENDIF
!
         ENDDO
!
!     CHOSE THE FACE OF THE FLUID WITH THE SMALLEST DISTANCE TO THE
!     STRUCTURAL ELEMENT AND WITH THE ANGLE BETWEEN THE TWO FACES LESS
!     THEN 30 DEGREES
!
         dist = 1.0E+10
         faceid = 0
         DO if = 1 , nf
            IF ( abs(angle(if))>.866 ) THEN
               IF ( heigth(if)<dist ) THEN
                  dist = heigth(if)
                  faceid = if
               ENDIF
            ENDIF
         ENDDO
         IF ( faceid==0 ) THEN
!
!     NO FACE WITHIN 30 DEGREES FO STRUCTURAL ELEMENT FACE
!
            WRITE (Nout,99003) Ufm , Ect(1) , Elt(2)
99003       FORMAT (A23,' 8007. NO FACE ON FLUID ELEMENT',I9,' IS WITHIN 30 DEGREES OF STRUCTURAL ELEMENT',I9)
            GOTO 200
!
!     VERIFY THAT THE FACE IS WITHIN PROPER TOLERENCE
!
         ELSEIF ( dist>tol ) THEN
!
!     FLUID ELEMENT IS NOT WITHIN TOLERENCE RANGE OF STRUCTURAL ELEMENT
!
            WRITE (Nout,99004) Ufm , Ect(1) , Elt(2)
99004       FORMAT (A23,' 8008. THE DISTANCE BETWEEN FLUID ELEMENT',I9,' AND STRUCTURAL ELEMENT',I9,/30X,                           &
                   &'IS GREATER THAN THE ALLOWED TOLERANCE.')
            GOTO 200
!
!     IF ANGLE WAS COMPUTED NEGATIVE - SWICTH STRUCTURAL GRIDS AROUND
!     IN ELEMENT TABLE RECORD FOR LATER USE
!
         ELSEIF ( angle(faceid)<0.0 ) THEN
            IF ( ngrids==3 ) THEN
!
               i = Elt(3)
               Elt(3) = Elt(5)
               Elt(5) = i
            ELSE
               i = Elt(3)
               Elt(3) = Elt(6)
               Elt(6) = i
               i = Elt(4)
               Elt(4) = Elt(5)
               Elt(5) = i
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     USING THE FACE SPECIFIES OR FOUND - RETURN THE PROPER
!     FLUID GRID POINTS
!
   DO i = 1 , 4
      j = face(i,faceid,intype)
      IF ( j>0 ) THEN
         Grid(i) = Ect(j+2)
      ELSE
         Grid(i) = -1
      ENDIF
   ENDDO
!
   RETURN
!
!     BAD GEOMETRY FOR FLUID ELEMENT
!
 100  WRITE (Nout,99005) Ufm , if , Ect(1)
99005 FORMAT (A23,' 8006. BAD GEOMETRY DEFINED FOR FACE',I9,' OF FLUID ELEMENT',I9)
!
 200  Error = .TRUE.
   RETURN
END SUBROUTINE flface
