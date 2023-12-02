!*==pkqdpl.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pkqdpl
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_pla42c
   USE c_pla42d
   USE c_pla4es
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: a
   REAL :: degra
   INTEGER :: i , j , k
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   REAL(REAL64) , DIMENSION(8) :: requiv
   REAL , DIMENSION(3) :: vq1 , vq2 , vq3 , vq4
   EXTERNAL gmmatd , mesage , pktrbs , pla4b , transd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES THE STIFFNESS MATRIX FOR QUAD-PLATES IN
!     PLA4
!
!     THIS ROUTINE GENERATES THE FOLLOWING
!
!     FOUR 6X6 STIFFNESS MATRICES WITH RESPECT TO ONE PIVOT POINT OF A
!     QUADRILATERAL PLATE ELEMENT.
!
!     REF.  FMMS-44   JULY  18, 1967   TRI.BENDING ELEMENT STIFF.
!           FMMS-48   AUGUST 1, 1967   QUAD. BENDING ELEMENT STIFF.
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!           PKTRBS - BASIC BENDING TRIANGLE
!           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!           PLA4B  - INSERTION ROUTINE
!           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
!           MESAGE - ERROR MESSAGE WRITER
!
!
!     ECPT LISTS AS OF AUGUST 4, 1967
!
!                 DEFINITION                   DEFINITION
!       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
!     ==================================================================
!     ECPT( 1) = ELEMENT ID         INTEGER ** ELEMENT           INTEGER
!     ECPT( 2) = GRID PT. A         INTEGER ** GRID PT.A         INTEGER
!     ECPT( 3) = GRID PT. B         INTEGER ** GRID PT.B         INTEGER
!     ECPT( 4) = GRID PT. C         INTEGER ** GRID PT.C         INTEGER
!     ECPT( 5) = THETA              REAL    ** GRID PT.D         INTEGER
!     ECPT( 6) = MAT ID 1           INTEGER ** THETA             REAL
!     ECPT( 7) = I  MOM. OF INERT.  REAL    ** MAT ID 1          INTEGER
!     ECPT( 8) = MAT ID 2           INTEGER ** I  MOM. OF INERT. REAL
!     ECPT( 9) = T2                 REAL    ** MAT ID 2          INTEGER
!     ECPT(10) = NON-STRUCT. MASS   REAL    ** T2                REAL
!     ECPT(11) = Z1                 REAL    ** NON-STRUCT. MASS  REAL
!     ECPT(12) = Z2                 REAL    ** Z1                REAL
!     ECPT(13) = COORD. SYS. ID 1   INTEGER ** Z2                REAL
!     ECPT(14) = X1                 REAL    ** COORD. SYS. ID 1  INTEGER
!     ECPT(15) = Y1                 REAL    ** X1                REAL
!     ECPT(16) = Z1                 REAL    ** Y1                REAL
!     ECPT(17) = COORD. SYS. ID 2   INTEGER ** Z1                REAL
!     ECPT(18) = X2                 REAL    ** COORD. SYS. ID 2  INTEGER
!     ECPT(19) = Y2                 REAL    ** X2                REAL
!     ECPT(20) = Z2                 REAL    ** Y2                REAL
!     ECPT(21) = COORD. SYS. ID 3   INTEGER ** Z2                REAL
!     ECPT(22) = X3                 REAL    ** COORD. SYS. ID 3  INTEGER
!     ECPT(23) = Y3                 REAL    ** X3                REAL
!     ECPT(24) = Z3                 REAL    ** Y3                REAL
!     ECPT(25) = ELEMENT TEMP       REAL    ** Z3                REAL
!     ECPT(26) =                            ** COORD. SYS. ID 4  INTEGER
!     ECPT(27) =                            ** X4                REAL
!     ECPT(28) =                            ** Y4                REAL
!     ECPT(29) =                            ** Z4                REAL
!     ECPT(30) =                            ** ELEMENT TEMP      REAL
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Necpt(1),Ecpt(1)) , (R(1,1),Requiv(1)) , (Vq1(1),Ecpt(15)) , (Vq2(1),Ecpt(19)) ,                &
!>>>>    & (Vq3(1),Ecpt(23)) , (Vq4(1),Ecpt(27)) , (A(1),Kout(1))
   DATA m/2 , 4 , 1 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4/
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 4
      IF ( npvt==necpt(i+1) ) THEN
         npivot = i
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
!
   CALL mesage(-30,34,ecpt(1))
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
!
      theta = ecpt(6)*Degra
      sinang = sin(theta)
      cosang = cos(theta)
!
      IF ( Npivot<=2 ) THEN
         jnot = Npivot + 2
      ELSE
         jnot = Npivot - 2
      ENDIF
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
      DO I = 1 , 8
         Requiv(I) = 0.0D0
      ENDDO
!
!     SHIFT ECPT UP TO MATCH PKTRBS FOR CERTAIN VARIABLES.
!
      DO I = 6 , 12
         ecpt(I) = ecpt(I+1)
      ENDDO
!
      DO I = 1 , 3
         d1(I) = dble(Vq3(I)) - dble(Vq1(I))
         d2(I) = dble(Vq4(I)) - dble(Vq2(I))
         a1(I) = dble(Vq2(I)) - dble(Vq1(I))
      ENDDO
!
!     NON-NORMALIZED K-VECTOR = D1 CROSS D2
!
      kvect(1) = d1(2)*d2(3) - d2(2)*d1(3)
      kvect(2) = d1(3)*d2(1) - d2(3)*d1(1)
      kvect(3) = d1(1)*d2(2) - d2(1)*d1(2)
!
!     NORMALIZE K-VECTOR
!
      temp = dsqrt(kvect(1)**2+kvect(2)**2+kvect(3)**2)
      IF ( temp/=0.0D0 ) THEN
         DO I = 1 , 3
            kvect(I) = kvect(I)/temp
         ENDDO
!
!     COMPUTE H = (A1 DOT KVECT)/2
!
         temp = (a1(1)*kvect(1)+a1(2)*kvect(2)+a1(3)*kvect(3))/2.0D0
!
!     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
!
         DO I = 1 , 3
            ivect(I) = a1(I) - temp*kvect(I)
         ENDDO
!
!     NORMALIZE I-VECTOR
!
         temp = dsqrt(ivect(1)**2+ivect(2)**2+ivect(3)**2)
         IF ( temp/=0.0D0 ) THEN
            DO I = 1 , 3
               ivect(I) = ivect(I)/temp
            ENDDO
!
!     J-VECTOR = K CROSS I, AND X3 CALCULATION
!
            jvect(1) = kvect(2)*ivect(3) - ivect(2)*kvect(3)
            jvect(2) = kvect(3)*ivect(1) - ivect(3)*kvect(1)
            jvect(3) = kvect(1)*ivect(2) - ivect(1)*kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
            temp = dsqrt(jvect(1)**2+jvect(2)**2+jvect(3)**2)
            IF ( temp/=0.0D0 ) THEN
               DO I = 1 , 3
                  jvect(I) = jvect(I)/temp
               ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
               r(1,3) = d1(1)*ivect(1) + d1(2)*ivect(2) + d1(3)*ivect(3)
!
!     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
!
               r(1,2) = a1(1)*ivect(1) + a1(2)*ivect(2) + a1(3)*ivect(3)
               r(2,3) = d1(1)*jvect(1) + d1(2)*jvect(2) + d1(3)*jvect(3)
!
!     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
!
               r(1,4) = d2(1)*ivect(1) + d2(2)*ivect(2) + d2(3)*ivect(3) + r(1,2)
               r(2,4) = d2(1)*jvect(1) + d2(2)*jvect(2) + d2(3)*jvect(3)
!
!     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
!
               IF ( r(2,3)>0.0D0 .AND. r(2,4)>0.0D0 ) THEN
                  temp = r(1,2) - (r(1,2)-r(1,3))*r(2,4)/r(2,3)
                  IF ( r(1,4)<temp ) THEN
                     temp = r(2,3)*r(1,4)/r(2,4)
                     IF ( r(1,3)>temp ) THEN
!
! 140 AT 140 THE COORDINATES OF THE PLATE IN THE ELEMENT
!     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
!     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
!     ROW 2 RESPECTIVELY.
!
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
!
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!
!     ZERO OUT KSUM MATRICES
!
                        DO I = 1 , 36
                           ksum(I) = 0.0D0
                        ENDDO
!
                        DO J = 1 , 4
                           IF ( J/=jnot ) THEN
                              km = 3*J - 3
                              subsca = M(km+1)
                              subscb = M(km+2)
                              subscc = M(km+3)
!
                              DO I = 1 , 2
                                 v(I) = r(I,subscb) - r(I,subsca)
                                 vv(I) = r(I,subscc) - r(I,subsca)
                              ENDDO
                              xsubb = dsqrt(v(1)**2+v(2)**2)
                              u1 = v(1)/xsubb
                              u2 = v(2)/xsubb
                              xsubc = u1*vv(1) + u2*vv(2)
                              ysubc = u1*vv(2) - u2*vv(1)
!
                              sinth = sinang*u1 - cosang*u2
                              costh = cosang*u1 + sinang*u2
                              IF ( abs(sinth)<1.0E-06 ) sinth = 0.0E0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
!     TRIANGLE -J-
!
                              CALL pktrbs(1)
!                         U
!     NOW HAVE AT HAND  K    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
!                        IJ                 A(1) THROUGH A(81).
!
!     MAP THE 3 3X3-S FOR THE PIVOT ROW INTO THE SUMMATION ARRAYS...
!
!     SET UP OF T-MATRIX
!
                              t(1) = 1.0D0
                              t(2) = 0.0D0
                              t(3) = 0.0D0
                              t(4) = 0.0D0
                              t(5) = u1
                              t(6) = u2
                              t(7) = 0.0D0
                              t(8) = -u2
                              t(9) = u1
!
!     FIND WHICH POINT OF THE SUBTRIANGLE IS ALSO THE PIVOT OF THE
!     QUADRILATERAL
!
                              SPAG_Loop_2_1: DO I = 1 , 3
                                 npoint = km + I
                                 IF ( M(npoint)==Npivot ) THEN
                                    nbegin = 27*I - 27
                                    EXIT SPAG_Loop_2_1
                                 ENDIF
                              ENDDO SPAG_Loop_2_1
!
                              DO I = 1 , 3
                                 npoint = nbegin + 9*I - 8
                                 CALL gmmatd(t,3,3,1,A(npoint),3,3,0,temp9)
                                 CALL gmmatd(temp9,3,3,0,t,3,3,0,prod9)
!
!     ADD THIS PRODUCT IN NOW.
!
                                 npoint = km + I
                                 npoint = 9*M(npoint) - 9
                                 DO K = 1 , 9
                                    npoint = npoint + 1
                                    ksum(npoint) = ksum(npoint) + prod9(K)/2.0D0
                                 ENDDO
                              ENDDO
                           ENDIF
!
                        ENDDO
!
!     FILL E-MATRIX
!
                        DO I = 1 , 18
                           e(I) = 0.0D0
                        ENDDO
                        e(1) = kvect(1)
                        e(4) = kvect(2)
                        e(7) = kvect(3)
                        e(11) = ivect(1)
                        e(14) = ivect(2)
                        e(17) = ivect(3)
                        e(12) = jvect(1)
                        e(15) = jvect(2)
                        e(18) = jvect(3)
!
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
                        IF ( Necpt(4*Npivot+10)==0 ) THEN
!
                           DO K = 1 , 18
                              tite(K) = e(K)
                           ENDDO
                        ELSE
                           CALL transd(Necpt(4*Npivot+10),t)
                           CALL gmmatd(t,3,3,1,e(1),3,3,0,tite(1))
                           CALL gmmatd(t,3,3,1,e(10),3,3,0,tite(10))
                        ENDIF
!
                        DO J = 1 , 4
!
!     TRANSFORMATIONS AND INSERTION
!
                           IF ( Necpt(4*J+10)==0 ) THEN
                              DO K = 1 , 18
                                 tjte(K) = e(K)
                              ENDDO
                           ELSE
                              CALL transd(Necpt(4*J+10),t)
                              CALL gmmatd(t,3,3,1,e(1),3,3,0,tjte(1))
                              CALL gmmatd(t,3,3,1,e(10),3,3,0,tjte(10))
                           ENDIF
                           CALL gmmatd(ksum(9*J-8),3,3,0,tjte,6,3,1,temp18(1))
                           CALL gmmatd(tite(1),6,3,0,temp18(1),3,6,0,kout(1))
                           CALL pla4b(kout(1),Necpt(J+1))
!
                        ENDDO
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
               CALL mesage(30,35,ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
               nogo = 1
               RETURN
            ENDIF
         ENDIF
      ENDIF
!
      CALL mesage(30,26,ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      nogo = 1
   END SUBROUTINE spag_block_1
END SUBROUTINE pkqdpl
