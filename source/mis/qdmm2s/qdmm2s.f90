!*==qdmm2s.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qdmm2s
   IMPLICIT NONE
   USE C_CONDAS
   USE C_EMGDIC
   USE C_EMGEST
   USE C_EMGPRM
   USE C_MATIN
   USE C_MATOUT
   USE C_SYSTEM
   USE C_XMSSG
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angl , dict5 , dummy , icosth , idetrm , isinth , it
   INTEGER , DIMENSION(11) :: dict
   REAL , DIMENSION(9) :: et , gsube , itemp9 , jtemp9 , ktemp9
   REAL , DIMENSION(36) :: g , tmat
   INTEGER :: i , ia , ib , ic , ierror , iest , ii , ij , ioutpt , ip , ising , j , j1 , j2 , j3 , j4 , jt , jtt , k , l , lpart , &
            & modk
   LOGICAL :: iheat , planar
   INTEGER , DIMENSION(4) :: ipart
   REAL , DIMENSION(9,16) :: k1sum
   REAL , DIMENSION(9,5) :: k5mod , k5sum
   REAL , DIMENSION(1) :: kij
   REAL , DIMENSION(63) :: kmat
   REAL , DIMENSION(144) :: kout
   INTEGER , DIMENSION(4,3) , SAVE :: map
   INTEGER , DIMENSION(7) :: nest
   REAL , DIMENSION(3,5) :: rmat
   EXTERNAL emastq , emgout , gmmats , invers , mat , q2bcs , q2trms , transs
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES THE STIFFNESS, MASS AND DAMPING MATRICES
!     FOR THE QDMM2 ELEMENT.
!
!     SINGLE PRECISION VERSION
!
!     THIS ROUTINE USES SUBROUTINE E MAS TQ TO CALCULATE THE LUMPED
!     MASS USING THE SAME METHOD AS WITH THE QDMEM ELEMENT.
!
!     THIS ROUTINE MAY NOT BE CALLED IN A HEAT PROBLEM.
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
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (Nest(1),Est(1)) , (dict(5),dict5) , (k1sum(1,1),kij(1)) , (Ksystm(56),Iheat)
   DATA map/1 , 2 , 3 , 4 , 2 , 3 , 4 , 1 , 5 , 5 , 5 , 5/
!
!
!     THIS ELEMENT NOT USED IN A HEAT PROBLEM
!
   IF ( iheat ) THEN
!
      WRITE (ioutpt,99001) Uwm , nest(1)
99001 FORMAT (A25,' 3115, QDMM2 FINDS ELEMENT NUMBER',I10,' PRESENT IN A HEAT FORMULATION AND IS IGNORING SAME.')
      RETURN
   ELSE
!
!     CREATE AN ARRAY POINTING TO THE GRID POINTS ACCORDING TO
!     INCREASING SIL VALUE
!
      DO i = 1 , 4
         ipart(i) = nest(i+1)
      ENDDO
      i = -4
      SPAG_Loop_1_1: DO
         j = 0
         DO k = 1 , 4
            IF ( ipart(k)>=j ) THEN
               j = ipart(k)
               l = k
            ENDIF
         ENDDO
         ipart(l) = i
         i = i + 1
         IF ( i>=0 ) THEN
            DO i = 1 , 4
               ipart(i) = -ipart(i)
            ENDDO
!
!     IF STIFFNESS MATRIX NEEDED
!     SET UP DICT ARRAY AND FOR STIFFNESS MATRIX
!     CALCULATIONS, OTHERWISE SKIP
!
            IF ( Ismd(1)/=0 ) THEN
!
!      COMPUTE BASIC SIN AND COSINE OF ELEMENT MATERIAL ANGLE.
!
               angl = Est(6)*Degra
               isinth = sin(angl)
               icosth = cos(angl)
!
!     COMPUTE GSUBE MATRIX
!
               Inflag = 2
               Matid = nest(7)
               Eltemp = Est(26)
               Sinth = 0.0
               Costh = 1.0
               CALL mat(nest(1))
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
!     BASIC WHOLE-ELEMENT CALCULATIONS
!
               CALL q2bcs(Est,planar,rmat,et,ierror)
               IF ( ierror>0 ) EXIT SPAG_Loop_1_1
!
!     ZERO SUMMATION ARRAYS
!
               DO i = 1 , 9
                  DO j = 1 , 16
                     k1sum(i,j) = 0.0
                  ENDDO
                  DO j = 1 , 5
                     k5sum(i,j) = 0.0
                  ENDDO
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
                  it = Est(8)
!
                  CALL q2trms(rmat(1,ia),rmat(1,ib),rmat(1,ic),dummy,isinth,icosth,gsube,it,ierror,1,kmat,dummy,dummy,dummy)
                  IF ( ierror>0 ) EXIT SPAG_Loop_1_1
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
               CALL invers(3,k5mod(1,5),3,dummy,0,idetrm,ising,itemp9)
               IF ( ising==2 ) THEN
                  WRITE (ioutpt,99002) Ufm , nest(1)
99002             FORMAT (A23,' 3099.  ELEMENT STIFFNESS COMPUTATION FOR QDMEM2 ','ELEMENT ID =',I10,/5X,                           &
                         &'IS IMPOSSIBLE DUE TO SINGULARITY',' IN CONSTRAINT EQUATION.')
                  CALL spag_block_1
                  RETURN
               ELSE
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
!     FORMATION OF THE 4 TRANSFORMATION MATRICES EACH (3X3)
!
                  DO i = 1 , 4
                     iest = 4*i + 6
                     IF ( nest(iest)/=0 ) THEN
!
!     GET TRANSFORMATION MATRIX
!
                        CALL transs(nest(iest),itemp9)
                        CALL gmmats(et,3,3,0,itemp9,3,3,0,tmat(9*i-8))
                     ELSE
!
                        k = 9*i - 9
                        DO j = 1 , 9
                           k = k + 1
                           tmat(k) = et(j)
                        ENDDO
                     ENDIF
!
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
!     FORM (K  ) = (K5SUM  ) + (K  ) (G )
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
!
!     TRANSFORM THIS RESULTANT K   (3X3) STORED AT K1SUM(1,LPART)
!                               IJ
!     TO GLOBAL.
!
                        CALL gmmats(tmat(9*i-8),3,3,1,k1sum(1,lpart),3,3,0,jtemp9)
                        CALL gmmats(jtemp9,3,3,0,tmat(9*j-8),3,3,0,k1sum(1,lpart))
                     ENDDO
                  ENDDO
!
!     FOR THE MATRIX ASSEMBLER -EMG- THE 16 (3X3) PARTITIONS IN K1SUM
!     ARE REARRANGED TO STORE THEM BY ROWS TO A TOTAL OF
!     12X12 RATHER THAN 3X3.  BUT FIRST DICT MUST BE
!     SET UP.  THE SILS MUST BE SORTED SO THAT THE 12X12 WILL
!     BE BY INCREASING SIL VALUE
!
                  dict(1) = Estid
                  dict(2) = 1
                  dict(3) = 12
                  dict(4) = 7
                  dict5 = Ge
                  ip = Iprec
!
!     REORDER K1SUM INTO KOUT AS DESCRIBED ABOVE
!
!         ****          ****
!         * K   K   K   K  *
!         *  AA  AB  AC  AD*
!     K = * K   K   K   K  *
!         *  BA  BB  BC  BD*
!         * K   K   K   K  *
!         *  CA  CB  CC  CD*
!         * K   K   K   K  *
!         *  DA  DB  DC  DD*
!         ****          ****
!
!     WHERE SUBSCRIPTS ARE ARRANGED BY INCREASING SIL VALUE
!
                  DO i = 1 , 4
                     ii = ipart(i)
                     DO j = 1 , 4
                        jtt = ipart(j)
                        jt = (i-1)*4 + j
                        DO k = 1 , 9
                           modk = mod(k,3)
                           IF ( modk==0 ) modk = 3
                           l = (ii-1)*36 + ((k-1)/3)*12 + (jtt-1)*3 + modk
                           kout(l) = k1sum(k,jt)
                        ENDDO
                     ENDDO
                  ENDDO
!
                  CALL emgout(kout,kout,144,1,dict,1,ip)
               ENDIF
            ENDIF
!
!     CALCULATE THE MASS MATRIX HERE.  SUBROUTINE
!     E MAS TQ IS USED TO GENERATE A LUMPED
!     MASS MATRIX EXACTLY LIKE A QDMEM ELEMENT
!
            IF ( Ismd(2)==0 ) RETURN
!
            CALL emastq(1,k1sum)
!
            dict(1) = Estid
            dict(2) = 2
            dict(3) = 12
            dict(4) = 7
            dict(5) = 0
!
!     REARRANGE KIJ BY INCREASING SIL VALUE
!
            DO i = 1 , 4
               ii = 1 + (ipart(i)-1)*3
               ij = (i-1)*3 + 1
               kout(ij) = kij(ii)
               kout(ij+1) = kij(ii+1)
               kout(ij+2) = kij(ii+2)
            ENDDO
!
            CALL emgout(kout,kout,12,1,dict,2,ip)
            RETURN
         ENDIF
      ENDDO SPAG_Loop_1_1
!
!     ELEMENT ERRORS DETECTED.
!
      WRITE (ioutpt,99003) Ufm , nest(1)
99003 FORMAT (A23,' 3098,  QDMEM2 ELEMENT STIFFNESS ROUTINE DETECTS ','ILLEGAL GEOMETRY FOR ELEMENT ID =',I10)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Nogo = .TRUE.
      RETURN
   END SUBROUTINE spag_block_1
!
END SUBROUTINE qdmm2s
