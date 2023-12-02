!*==sqdm21.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sqdm21
   IMPLICIT NONE
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   USE c_system
   USE c_xmssg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angl , determ , dummy , fmag , icosth , isinth
   REAL , DIMENSION(3,4) :: dvec
   REAL , DIMENSION(36) :: eti
   INTEGER :: i , i1 , i2 , ia , ib , ic , ierror , ii , ioutpt , ising , j , j1 , j2 , j3 , j4 , jpart , k , kk , lpart
   REAL , DIMENSION(9) :: itemp9 , ktemp9 , pmat , zmat
   REAL , DIMENSION(63) :: kmat
   REAL , DIMENSION(3) :: kvec
   INTEGER , DIMENSION(4,3) , SAVE :: map
   INTEGER , DIMENSION(7) :: nest
   LOGICAL :: planar
   REAL , DIMENSION(3,3,4) :: q
   REAL , DIMENSION(3,5) :: rmat
   REAL , DIMENSION(27) :: smat
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   angl = est(6)*degra
   isinth = sin(angl)
   icosth = cos(angl)
!
!     COMPUTE GSUBE MATRIX
!
   inflag = 2
   matid = nest(7)
   eltemp = est(26)
   sinth = 0.0
   costh = 1.0
   CALL mat(nest(1))
   gsube(1) = g11
   gsube(2) = g12
   gsube(3) = g13
   gsube(4) = g12
   gsube(5) = g22
   gsube(6) = g23
   gsube(7) = g13
   gsube(8) = g23
   gsube(9) = g33
!
!     BASIC WHOLE-ELEMENT CALCULATIONS
!
   CALL q2bcs(est,planar,rmat,e,ierror)
   IF ( ierror<=0 ) THEN
!
!     ZERO SUMMATION ARRAYS
!
      DO i = 1 , 9
         DO j = 1 , 16
            k1sum(i,j) = 0.0
         ENDDO
         DO j = 1 , 5
            k5sum(i,j) = 0.0
            sisum(i,j) = 0.0
         ENDDO
      ENDDO
!
      DO i = 1 , 5
         pisum(1,i) = 0.0
         pisum(2,i) = 0.0
         pisum(3,i) = 0.0
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
         CALL q2trms(rmat(1,ia),rmat(1,ib),rmat(1,ic),alps,isinth,icosth,gsube,est(8),ierror,3,kmat,pmat,smat,zmat)
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
            k5sum(k,ia) = k5sum(k,ia) + kmat(k)
            k5sum(k,ib) = k5sum(k,ib) + kmat(k+9)
            k5sum(k,ic) = k5sum(k,ic) + kmat(k+18)
            k1sum(k,j1) = k1sum(k,j1) + kmat(k+27)
            k1sum(k,j2) = k1sum(k,j2) + kmat(k+36)
            k1sum(k,j3) = k1sum(k,j3) + kmat(k+45)
            k1sum(k,j4) = k1sum(k,j4) + kmat(k+54)
            sisum(k,ia) = sisum(k,ia) + smat(k)
            sisum(k,ib) = sisum(k,ib) + smat(k+9)
            sisum(k,ic) = sisum(k,ic) + smat(k+18)
         ENDDO
!
         DO k = 1 , 3
            pisum(k,ia) = pisum(k,ia) + pmat(k)
            pisum(k,ib) = pisum(k,ib) + pmat(k+3)
            pisum(k,ic) = pisum(k,ic) + pmat(k+6)
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
            k5mod(1,i) = k5sum(1,i)
            k5mod(2,i) = k5sum(2,i)
            k5mod(3,i) = k5sum(3,i)
            k5mod(4,i) = k5sum(4,i)
            k5mod(5,i) = k5sum(5,i)
            k5mod(6,i) = k5sum(6,i)
            k5mod(7,i) = 0.0
            k5mod(8,i) = 0.0
            k5mod(9,i) = -0.25
         ENDDO
         k5mod(9,5) = 1.0
      ELSE
         DO i = 1 , 5
            DO j = 1 , 9
               k5mod(j,i) = k5sum(j,i)
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
      CALL invers(3,k5mod(1,5),3,dummy,0,determ,ising,itemp9)
      IF ( ising/=2 ) THEN
!
         DO i = 1 , 9
            k5mod(i,5) = -k5mod(i,5)
         ENDDO
!
!     FORM G MATRICES
!
         DO i = 1 , 4
            CALL gmmats(k5mod(1,5),3,3,0,k5mod(1,i),3,3,0,g(9*i-8))
         ENDDO
!
!     FORM STIFFNESS MATRIX BY ROW-PARTIONS.
!
         DO i = 1 , 4
!                          T
!     IF -PLANAR- FORM (G ) (K  ) FOR USE IN COLUMN-PARTITIONS LOOP.
!                        I    55
!
            IF ( planar ) CALL gmmats(g(9*i-8),3,3,1,k5sum(1,5),3,3,0,itemp9)
!
!     COLUMN-PARTITIONS-LOOP
!
            DO j = 1 , 4
!                                   T
!     FORM (K  ) = (K1SUM  ) + (K  ) (G )
!            IJ          IJ      5I    J
!
               CALL gmmats(k5sum(1,i),3,3,1,g(9*j-8),3,3,0,jtemp9)
               lpart = 4*i - 4 + j
               DO k = 1 , 9
                  k1sum(k,lpart) = k1sum(k,lpart) + jtemp9(k)
               ENDDO
!
!     BALANCE OF TERMS IF -PLANAR-
!
!                T            T
!     ADD IN (G ) (K  ) + (G ) (K  )(G )
!              I    5J      I    55   J
!
               IF ( planar ) THEN
                  CALL gmmats(itemp9,3,3,0,g(9*j-8),3,3,0,jtemp9)
                  CALL gmmats(g(9*i-8),3,3,1,k5sum(1,j),3,3,0,ktemp9)
                  DO k = 1 , 9
                     k1sum(k,lpart) = k1sum(k,lpart) + ktemp9(k) + jtemp9(k)
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
            rg(i) = fmag
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
            CALL invers(3,q(1,1,i),3,dummy,0,determ,ising,jtemp9)
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
            IF ( nest(kk+6)/=0 ) THEN
               CALL transs(nest(kk+6),t)
               CALL gmmats(e,3,3,0,t,3,3,0,eti(ii))
            ELSE
!
               kk = ii
               DO j = 1 , 9
                  eti(kk) = e(j)
                  kk = kk + 1
               ENDDO
            ENDIF
!
!       G            E      E
!     (S ) = 0.25( (S ) + (S )(G ) )(E)(T )
!       I            I      5   I        I
!
            CALL gmmats(sisum(1,5),3,3,0,g(ii),3,3,0,jtemp9)
            DO j = 1 , 9
               jtemp9(j) = 0.25*(jtemp9(j)+sisum(j,i))
            ENDDO
            CALL gmmats(jtemp9,3,3,0,eti(ii),3,3,0,sg(ii))
!
!       T     -         T -
!     (P ) = (P ) + (G ) (P )
!       I      I      I    5
!
            CALL gmmats(g(ii),3,3,1,pisum(1,5),3,1,0,pt(1,i))
            DO j = 1 , 3
               pisum(j,i) = pt(j,i) + pisum(j,i)
            ENDDO
            CALL gmmats(q(1,1,i),3,3,1,pisum(1,i),3,1,0,pt(1,i))
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
               CALL gmmats(q(1,1,i),3,3,1,k1sum(1,jpart),3,3,0,jtemp9)
               CALL gmmats(jtemp9,3,3,0,eti(9*j-8),3,3,0,k1sum(1,jpart))
            ENDDO
         ENDDO
!
!     (S ) = (GSUBE)(ALPHAS)
!       T
!
         CALL gmmats(gsube,3,3,0,alps,3,1,0,st)
!
!     MISC. DATA FOR PHASE-II
!
         id = nest(1)
         isils(1) = nest(2)
         isils(2) = nest(3)
         isils(3) = nest(4)
         isils(4) = nest(5)
         elthik = est(8)
         reftmp = tsub0
         RETURN
      ENDIF
   ENDIF
!
!     ERROR CONDITION
!
 100  WRITE (ioutpt,99001) uwm , nest(1)
99001 FORMAT (A25,' 3101, SINGULARITY OR BAD GEOMETRY FOR QDMEM2 ELEM.',' ID =',I9,/5X,'STRESS OR FORCES WILL BE INCORRECT.')
END SUBROUTINE sqdm21
