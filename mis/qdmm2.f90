
SUBROUTINE qdmm2(Temps,Pg)
   IMPLICIT NONE
   REAL Alps(3) , Costh , Degra , Eltemp , Est(26) , G11 , G12 , G13 , G22 , G23 , G33 , Pi , Radeg , Rho , S4pisq , Sinth ,        &
      & Stress , Tsub0 , Twopi
   INTEGER Inflag , Ioutpt , Ksystm(65) , Matid , Nest(7)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alps , Tsub0
   COMMON /system/ Ksystm
   COMMON /trimex/ Est
   COMMON /xmssg / Ufm , Uwm
   REAL Pg(1) , Temps(1)
   REAL alpha(3) , angl , determ , dummy , et(9) , gsube(9) , icosth , isinth , it , itemp9(9) , jtemp9(9) , k5sum(9,5) , kmat(27) ,&
      & pmat(9) , psum(3,5) , rmat(3,5) , tbar
   INTEGER i , ia , ib , ic , ierror , ising , j , k , l , map(4,3)
   LOGICAL planar
!
!     THERMAL LOAD GENERATION FOR THE QDMEM2 ELEMENT.
!
!     ELEMENT EST ENTRY CONTENTS
!     + + + + + + + + + + + + + + + + + + + + + + + + + +
!     +   1 = ID                                        +
!     +   2 = SIL-PT-A            (ELEMENT CONNECTS     +
!     +   3 = SIL-PT-B             GRID POINTS A,B,     +
!     +   4 = SIL-PT-C             C,D IN THAT ORDER)   +
!     +   5 = SIL-PT-D                                  +
!     +   6 = MATERIAL-ANGLE                            +
!     +   7 = MATERIAL-ID                               +
!     +   8 = THICKNESS OF ELEMENT                      +
!     +   9 = NON-STRUCTURAL-MASS                       +
!     +  10 = COORD-SYS-ID PT-A OR 0                    +
!     +  11 = XA                                        +
!     +  12 = YA                                        +
!     +  13 = ZA                                        +
!     +  14 = COORD-SYS-ID PT-B OR 0                    +
!     +  15 = XB                                        +
!     +  16 = YB                                        +
!     +  17 = ZB                                        +
!     +  18 = COORD-SYS-ID PT-C OR 0                    +
!     +  19 = XC                                        +
!     +  20 = YC                                        +
!     +  21 = ZC                                        +
!     +  22 = COORD-SYS-ID PT-D OR 0                    +
!     +  23 = XD                                        +
!     +  24 = YD                                        +
!     +  25 = ZD                                        +
!     +  26 = AVERAGE OF CONNECTED GRID TEMPERATURES    +
!     + + + + + + + + + + + + + + + + + + + + + + + + + +
!
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (Nest(1),Est(1))
   DATA map/1 , 2 , 3 , 4 , 2 , 3 , 4 , 1 , 5 , 5 , 5 , 5/
!
!     COMPUTE BASIC SIN AND COSINE OF ELEMENT MATERIAL ANGLE.
!
   angl = Est(6)*Degra
   isinth = sin(angl)
   icosth = cos(angl)
!
!     COMPUTE GSUBE MATRIX
!
   Inflag = 2
   Matid = Nest(7)
   Eltemp = Est(26)
   Sinth = 0.0
   Costh = 1.0
   CALL mat(Nest(1))
   gsube(1) = G11
   gsube(2) = G12
   gsube(3) = G13
   gsube(4) = G12
   gsube(5) = G22
   gsube(6) = G23
   gsube(7) = G13
   gsube(8) = G23
   gsube(9) = G33
!
!     FORM  ALPHA = ALPS *(T-T )  3X1 VECTOR USED IN SUB-TRIANGLE CALCS
!                       E     0
!
   tbar = Temps(1) - Tsub0
   alpha(1) = Alps(1)*tbar
   alpha(2) = Alps(2)*tbar
   alpha(3) = Alps(3)*tbar
!
!     NOTE THE ABOVE MAY BE MOVED TO BELOW AND COMPUTED USING THE
!     GRID TEMPS OF SUB-TRIANGLE.  (I.E. TOTAL AVERAGE FOR CENTER POINT
!     ONLY.)  AVERAGE OF WHOLE ELEMENT IS USED EXCLUSIVELY NOW.
!
!     BASIC WHOLE-ELEMENT CALCULATIONS
!
   CALL q2bcs(Est,planar,rmat,et,ierror)
   IF ( ierror<=0 ) THEN
!
!     ZERO SUMMATION ARRAYS
!
      DO i = 1 , 5
         DO j = 1 , 9
            k5sum(j,i) = 0.0
         ENDDO
         psum(1,i) = 0.0
         psum(2,i) = 0.0
         psum(3,i) = 0.0
      ENDDO
!
!     SUB-TRIANGLE COMPUTATIONS AND SUMMATIONS.
!
      DO i = 1 , 4
         ia = map(i,1)
         ib = map(i,2)
         ic = map(i,3)
         it = Est(8)
         CALL q2trms(rmat(1,ia),rmat(1,ib),rmat(1,ic),alpha(1),isinth,icosth,gsube,it,ierror,2,kmat,pmat,dummy,dummy)
         IF ( ierror>0 ) GOTO 100
!
!     SUM IN KCA,KCB,KCC
!
         DO k = 1 , 9
            k5sum(k,ia) = k5sum(k,ia) + kmat(k)
            k5sum(k,ib) = k5sum(k,ib) + kmat(k+9)
            k5sum(k,ic) = k5sum(k,ic) + kmat(k+18)
         ENDDO
!
!     SUM IN PA,PB,PC
!
         DO k = 1 , 3
            psum(k,ia) = psum(k,ia) + pmat(k)
            psum(k,ib) = psum(k,ib) + pmat(k+3)
            psum(k,ic) = psum(k,ic) + pmat(k+6)
         ENDDO
!
      ENDDO
!
!     IF -PLANAR- MODIFY THE K5SUM MATRICES.
!
      IF ( planar ) THEN
         DO i = 1 , 5
            k5sum(7,i) = 0.0
            k5sum(8,i) = 0.0
            k5sum(9,i) = -0.25
         ENDDO
         k5sum(9,5) = 1.0
      ENDIF
!
!     INVERT K   AND NEGATE THE RESULT.
!             55
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(3,k5sum(1,5),3,dummy,0,determ,ising,itemp9)
      IF ( ising/=2 ) THEN
!
         DO i = 1 , 9
            k5sum(i,5) = -k5sum(i,5)
         ENDDO
!
!     4 (3X1) LOAD VECTORS ARE COMPUTED AND ADDED INTO THE P-VECTOR IN
!     CORE
!
!       G        T   T                  -1      T
!     (P ) = (T ) (E) ((PSUM ) + ((-K  ) (K  )) (PSUM ))
!       I      I            I        55    5I        5
!
         DO i = 1 , 4
            CALL gmmats(k5sum(1,5),3,3,0,k5sum(1,i),3,3,0,itemp9)
            CALL gmmats(itemp9,3,3,1,psum(1,5),3,1,0,jtemp9)
            DO j = 1 , 3
               psum(j,i) = psum(j,i) + jtemp9(j)
            ENDDO
            CALL gmmats(et,3,3,1,psum(1,i),3,1,0,jtemp9)
            jtemp9(4) = 0.0
            jtemp9(5) = 0.0
            jtemp9(6) = 0.0
            k = 4*i + 6
            IF ( Nest(k)/=0 ) CALL basglb(jtemp9,jtemp9,Nest(k+1),Nest(k))
!
!     ADD LOAD TO CORE FOR THIS GRID
!                                   I
            l = Nest(i+1)
            DO j = 1 , 3
               Pg(l) = Pg(l) + jtemp9(j)
               l = l + 1
            ENDDO
!
         ENDDO
         RETURN
      ENDIF
   ENDIF
!
!     ERROR CONDITIONS
!
 100  WRITE (Ioutpt,99001) Uwm , Nest(1)
99001 FORMAT (A25,' 3100, ELEMENT THERMAL LOAD COMPUTATION FOR QDMEM2 ','ELEMENT ID =',I9,/5X,'FINDS ILLEGAL GEOMETRY THUS NO ',    &
             &'LOADS OUTPUT FOR ELEMENT-ID NOTED.')
END SUBROUTINE qdmm2