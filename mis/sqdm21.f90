
SUBROUTINE sqdm21
   IMPLICIT NONE
   REAL Alps(3) , Costh , Degra , E(9) , Eltemp , Elthik , Est(100) , G(36) , G11 , G12 , G13 , G22 , G23 , G33 , Gsube(9) ,        &
      & Imat12(12) , Jtemp9(9) , K1sum(9,16) , K5mod(9,5) , K5sum(9,5) , Pi , Pisum(3,5) , Pt(3,4) , R(3,4,5) , Radeg , Reftmp ,    &
      & Rg(4) , Rho , S4pisq , Sg(36) , Sinth , Sisum(9,5) , St(3) , Stress , T(9) , Tsub0 , Twopi
   INTEGER Id , Inflag , Ioutpt , Isils(4) , Ksystm(65) , Matid , Nest(7)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alps , Tsub0
   COMMON /sdr2x5/ Est , Id , Isils , Elthik , Reftmp , K1sum , Sg , Pt , St , Rg
   COMMON /sdr2x6/ K5sum , Sisum , Pisum , R , K5mod , G , T , E , Imat12 , Jtemp9 , Gsube
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   REAL angl , determ , dummy , dvec(3,4) , eti(36) , fmag , icosth , isinth , itemp9(9) , kmat(63) , ktemp9(9) , kvec(3) , pmat(9) &
      & , q(3,3,4) , rmat(3,5) , smat(27) , zmat(9)
   INTEGER i , i1 , i2 , ia , ib , ic , ierror , ii , ising , j , j1 , j2 , j3 , j4 , jpart , k , kk , lpart , map(4,3)
   LOGICAL planar
   REAL sadotb
!
!     PHASE-I STRESS-DATA-RECOVERY ROUTINE FOR THE -QDMEM2- ELEMENT.
!
!     THIS ROUTINE WILL PREPARE FOR USE BY -SQDM22-, THE PHASE-II
!     ROUTINE, A TABLE CONTAINING THE FOLLOWING.
!
!     TABLE WORDS        DISCRIPTION
!     ------------------------------------------------------
!       1 THRU   1       ELEMENT-ID
!       2 THRU   5       4 SILS
!       6 THRU   6       ELEMENT-THICKNESS
!       7 THRU   7       REFERENCE TEMP -TSUB0-
!       8 THRU 151       16 (3X3) KIJ-G MATRICES
!     152 THRU 187       4 (3X3) STRESS MATRICES
!     188 THRU 199       4 (3X1) TEMP VECTORS
!     200 THRU 202       ST (3X1) STRESS-TEMPERATURE VECTOR
!     203 THRU 206       4 SIDE LENGTHS
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
!
!     FOLLOWING COMMON BLOCK MUST BE DIMENSIONED AT LEAST 350 IN SDR2B
!
!
!     WORKING STORAGE BLOCK (KEEP .LE. 300 WORDS)
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
   Gsube(1) = G11
   Gsube(2) = G12
   Gsube(3) = G13
   Gsube(4) = G12
   Gsube(5) = G22
   Gsube(6) = G23
   Gsube(7) = G13
   Gsube(8) = G23
   Gsube(9) = G33
!
!     BASIC WHOLE-ELEMENT CALCULATIONS
!
   CALL q2bcs(Est,planar,rmat,E,ierror)
   IF ( ierror<=0 ) THEN
!
!     ZERO SUMMATION ARRAYS
!
      DO i = 1 , 9
         DO j = 1 , 16
            K1sum(i,j) = 0.0
         ENDDO
         DO j = 1 , 5
            K5sum(i,j) = 0.0
            Sisum(i,j) = 0.0
         ENDDO
      ENDDO
!
      DO i = 1 , 5
         Pisum(1,i) = 0.0
         Pisum(2,i) = 0.0
         Pisum(3,i) = 0.0
      ENDDO
!
!     SUB-TRIANGLES ARE COMPUTED AND RESULTS SUMMED.
!
      DO i = 1 , 4
!
!     CALL TRIANGLE CALCULATION ROUTINE TO GET (3X3) SUB-PARTITIONS
!
         ia = map(i,1)
         ib = map(i,2)
         ic = map(i,3)
!
         CALL q2trms(rmat(1,ia),rmat(1,ib),rmat(1,ic),Alps,isinth,icosth,Gsube,Est(8),ierror,3,kmat,pmat,smat,zmat)
         IF ( ierror>0 ) GOTO 100
!
!     SUM IN KCA,KCB,KCC 3-(3X3)-S STORED FIRST IN KMAT
!
!     ALSO SUM IN KAA,KAB,KBA,KBB = LAST 4-(3X3)-S STORED IN KMAT.
!     THESE GO INTO 4 OF THE 16 POSSIBLE (3X3) SUM MATRICES = ,
!
!     K11,K12,K13,K14,K21,K22,K23,K24,K31,K32,K33,K34,K41,K42,K43,K44
!
!     J1,J2,J3,J4 WILL EACH POINT TO 1 OF THE 16 (3X3)-S.
!
         j1 = 5*ia - 4
         j2 = 4*ia - 4 + ib
         j3 = 4*ib - 4 + ia
         j4 = 5*ib - 4
!
         DO k = 1 , 9
            K5sum(k,ia) = K5sum(k,ia) + kmat(k)
            K5sum(k,ib) = K5sum(k,ib) + kmat(k+9)
            K5sum(k,ic) = K5sum(k,ic) + kmat(k+18)
            K1sum(k,j1) = K1sum(k,j1) + kmat(k+27)
            K1sum(k,j2) = K1sum(k,j2) + kmat(k+36)
            K1sum(k,j3) = K1sum(k,j3) + kmat(k+45)
            K1sum(k,j4) = K1sum(k,j4) + kmat(k+54)
            Sisum(k,ia) = Sisum(k,ia) + smat(k)
            Sisum(k,ib) = Sisum(k,ib) + smat(k+9)
            Sisum(k,ic) = Sisum(k,ic) + smat(k+18)
         ENDDO
!
         DO k = 1 , 3
            Pisum(k,ia) = Pisum(k,ia) + pmat(k)
            Pisum(k,ib) = Pisum(k,ib) + pmat(k+3)
            Pisum(k,ic) = Pisum(k,ic) + pmat(k+6)
         ENDDO
!
      ENDDO
!
!     FORMATION OF THE FOUR (3X3) G MATRICES.
!                     -1
!     (G ) = -(K5SUM  ) (K  )   NOTE.  IF -PLANAR- THEN MODIFIED
!       I           55    5I           K5SUM MATRICES ARE USED.
!
      IF ( planar ) THEN
!
         DO i = 1 , 5
            K5mod(1,i) = K5sum(1,i)
            K5mod(2,i) = K5sum(2,i)
            K5mod(3,i) = K5sum(3,i)
            K5mod(4,i) = K5sum(4,i)
            K5mod(5,i) = K5sum(5,i)
            K5mod(6,i) = K5sum(6,i)
            K5mod(7,i) = 0.0
            K5mod(8,i) = 0.0
            K5mod(9,i) = -0.25
         ENDDO
         K5mod(9,5) = 1.0
      ELSE
         DO i = 1 , 5
            DO j = 1 , 9
               K5mod(j,i) = K5sum(j,i)
            ENDDO
         ENDDO
      ENDIF
!
!     INVERT K5MOD   AND NEGATE RESULT.
!                 55
!
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(3,K5mod(1,5),3,dummy,0,determ,ising,itemp9)
      IF ( ising/=2 ) THEN
!
         DO i = 1 , 9
            K5mod(i,5) = -K5mod(i,5)
         ENDDO
!
!     FORM G MATRICES
!
         DO i = 1 , 4
            CALL gmmats(K5mod(1,5),3,3,0,K5mod(1,i),3,3,0,G(9*i-8))
         ENDDO
!
!     FORM STIFFNESS MATRIX BY ROW-PARTIONS.
!
         DO i = 1 , 4
!                          T
!     IF -PLANAR- FORM (G ) (K  ) FOR USE IN COLUMN-PARTITIONS LOOP.
!                        I    55
!
            IF ( planar ) CALL gmmats(G(9*i-8),3,3,1,K5sum(1,5),3,3,0,itemp9)
!
!     COLUMN-PARTITIONS-LOOP
!
            DO j = 1 , 4
!                                   T
!     FORM (K  ) = (K1SUM  ) + (K  ) (G )
!            IJ          IJ      5I    J
!
               CALL gmmats(K5sum(1,i),3,3,1,G(9*j-8),3,3,0,Jtemp9)
               lpart = 4*i - 4 + j
               DO k = 1 , 9
                  K1sum(k,lpart) = K1sum(k,lpart) + Jtemp9(k)
               ENDDO
!
!     BALANCE OF TERMS IF -PLANAR-
!
!                T            T
!     ADD IN (G ) (K  ) + (G ) (K  )(G )
!              I    5J      I    55   J
!
               IF ( planar ) THEN
                  CALL gmmats(itemp9,3,3,0,G(9*j-8),3,3,0,Jtemp9)
                  CALL gmmats(G(9*i-8),3,3,1,K5sum(1,j),3,3,0,ktemp9)
                  DO k = 1 , 9
                     K1sum(k,lpart) = K1sum(k,lpart) + ktemp9(k) + Jtemp9(k)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
!     CALCULATION OF 4 (Q ) MATRICES, EACH 3X3.
!                        I
!
         DO i = 1 , 4
            ia = map(i,1)
            ib = map(i,2)
            DO j = 1 , 3
               dvec(j,i) = rmat(j,ib) - rmat(j,ia)
            ENDDO
            fmag = sqrt(sadotb(dvec(1,i),dvec(1,i)))
            Rg(i) = fmag
            IF ( fmag<=0 ) GOTO 100
            DO j = 1 , 3
               dvec(j,i) = dvec(j,i)/fmag
            ENDDO
         ENDDO
!
         DO i = 1 , 4
            j = i - 1
            IF ( j==0 ) j = 4
            i1 = map(j,1)
            i2 = map(j,2)
            CALL saxb(dvec(1,i2),dvec(1,i1),kvec)
!
!     NORMALIZE, NEGATE, AND STORE AS DELTA-VEC IN (Q )
!                                                    I
            fmag = sqrt(sadotb(kvec,kvec))
            IF ( fmag<=0 ) GOTO 100
            q(1,3,i) = -kvec(1)/fmag
            q(2,3,i) = -kvec(2)/fmag
            q(3,3,i) = -kvec(3)/fmag
!
!     STORE D VECTORS AS ALPHA- VECTORS IN (Q )
!                                            I
            q(1,1,i) = -dvec(1,i)
            q(2,1,i) = -dvec(2,i)
            q(3,1,i) = -dvec(3,i)
!
            q(1,2,i) = dvec(1,j)
            q(2,2,i) = dvec(2,j)
            q(3,2,i) = dvec(3,j)
!
!     INVERT 3X3
!
!     AGAIN NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED .
!     SUBSEQUENTLY.
!
            ising = -1
            CALL invers(3,q(1,1,i),3,dummy,0,determ,ising,Jtemp9)
            IF ( ising==2 ) GOTO 100
         ENDDO
!
!     FORM FINAL OUTPUTS
!
         DO i = 1 , 4
            ii = 9*i - 8
!
!     TRANSFORMATION ETI = (E)(T )
!                               I
!
            kk = 4*i
            IF ( Nest(kk+6)/=0 ) THEN
               CALL transs(Nest(kk+6),T)
               CALL gmmats(E,3,3,0,T,3,3,0,eti(ii))
            ELSE
!
               kk = ii
               DO j = 1 , 9
                  eti(kk) = E(j)
                  kk = kk + 1
               ENDDO
            ENDIF
!
!       G            E      E
!     (S ) = 0.25( (S ) + (S )(G ) )(E)(T )
!       I            I      5   I        I
!
            CALL gmmats(Sisum(1,5),3,3,0,G(ii),3,3,0,Jtemp9)
            DO j = 1 , 9
               Jtemp9(j) = 0.25*(Jtemp9(j)+Sisum(j,i))
            ENDDO
            CALL gmmats(Jtemp9,3,3,0,eti(ii),3,3,0,Sg(ii))
!
!       T     -         T -
!     (P ) = (P ) + (G ) (P )
!       I      I      I    5
!
            CALL gmmats(G(ii),3,3,1,Pisum(1,5),3,1,0,Pt(1,i))
            DO j = 1 , 3
               Pisum(j,i) = Pt(j,i) + Pisum(j,i)
            ENDDO
            CALL gmmats(q(1,1,i),3,3,1,Pisum(1,i),3,1,0,Pt(1,i))
         ENDDO
!
!     TRANSFORM STIFFNESS MATRIX TO GLOBAL
!
!        G           E
!     (K  ) = (Q )(K  )(E)(T )
!       IJ      I   IJ      J
!
         jpart = 0
         DO i = 1 , 4
            DO j = 1 , 4
               jpart = jpart + 1
               CALL gmmats(q(1,1,i),3,3,1,K1sum(1,jpart),3,3,0,Jtemp9)
               CALL gmmats(Jtemp9,3,3,0,eti(9*j-8),3,3,0,K1sum(1,jpart))
            ENDDO
         ENDDO
!
!     (S ) = (GSUBE)(ALPHAS)
!       T
!
         CALL gmmats(Gsube,3,3,0,Alps,3,1,0,St)
!
!     MISC. DATA FOR PHASE-II
!
         Id = Nest(1)
         Isils(1) = Nest(2)
         Isils(2) = Nest(3)
         Isils(3) = Nest(4)
         Isils(4) = Nest(5)
         Elthik = Est(8)
         Reftmp = Tsub0
         RETURN
      ENDIF
   ENDIF
!
!     ERROR CONDITION
!
 100  WRITE (Ioutpt,99001) Uwm , Nest(1)
99001 FORMAT (A25,' 3101, SINGULARITY OR BAD GEOMETRY FOR QDMEM2 ELEM.',' ID =',I9,/5X,'STRESS OR FORCES WILL BE INCORRECT.')
END SUBROUTINE sqdm21