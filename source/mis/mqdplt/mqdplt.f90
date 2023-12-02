!*==mqdplt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mqdplt
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2io
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: a
   REAL :: degra
   INTEGER :: i , j , k , npt
   INTEGER , DIMENSION(12) , SAVE :: m
   REAL(REAL64) , DIMENSION(36) :: msum
   INTEGER , DIMENSION(100) :: necpt
   REAL(REAL64) , DIMENSION(8) :: requiv
   REAL , DIMENSION(3) :: vq1 , vq2 , vq3 , vq4
   EXTERNAL gmmatd , mesage , mtrbsc , sma2b , transd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES FOUR 6X6 STIFFNESS MATRICES WITH RESPECT
!     TO ONE PIVOT POINT OF A QUADRILATERAL PLATE ELEMENT.
!
!     REF.  FMMS-66   JUNE 23, 1969   TRI.BENDING ELEMENT MASS
!           FMMS-66   JUNE 23, 1969   QUAD. BENDING ELEMENT MASS
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!           MTRBSC - BASIC BENDING TRI. ROUTINE.
!           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
!           SMA2B  - INSERTION ROUTINE
!           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
!           MESAGE - ERROR MESSAGE WRITER
!
!     ALL WRITE STATEMENTS WHICH HAVE BEEN COMMENTED OUT, HAVE BEEN
!     LEFT IN THE PROGRAMMING FOR ANY FUTURE DEBUGGING USE.
!
!     ECPT LISTS AS OF AUGUST 4, 1967
!
!                 DEFINITION                   DEFINITION
!       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
!     ========   =============      =======    ===============   =======
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
!>>>>    & (Vq3(1),Ecpt(23)) , (Vq4(1),Ecpt(27)) , (A(1),Mout(1))
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
!     SHIFT ECPT UP TO MATCH MTRBSC FOR CERTAIN VARIABLES.
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
!     COMPUTE H = A1 DOT KVECT
!
         h = a1(1)*kvect(1) + a1(2)*kvect(2) + a1(3)*kvect(3)
!
!     WRITE (6,109)
!     WRITE (6,119)
!     WRITE (6,1195) H,(D1(I),D2(I),A1(I),I=1,3)
!
!     I-VECTOR = (A1) - H*(KVECT)  NON-NORMALIZED
!
         DO I = 1 , 3
            ivect(I) = a1(I) - h*kvect(I)
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
!     WRITE (6,129) (IVECT(I),I=1,3),(JVECT(I),I=1,3),(KVECT(I),I=1,3),
!    1              ((R(I,J),J=1,4),I=1,2)
!
!     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
!
               IF ( r(2,3)>0.0D0 .AND. r(2,4)>0.0D0 ) THEN
                  temp = r(1,2) - (r(1,2)-r(1,3))*r(2,4)/r(2,3)
                  IF ( r(1,4)<temp ) THEN
                     temp = r(2,3)*r(1,4)/r(2,4)
                     IF ( r(1,3)>temp ) THEN
!
!     AT 140 THE COORDINATES OF THE PLATE IN THE ELEMENT
!     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
!     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
!     ROW 2 RESPECTIVELY.
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!
!     ZERO OUT MSUM MATRICES
!
                        DO I = 1 , 36
                           Msum(I) = 0.0D0
                        ENDDO
                        ptmass = 0.0D0
                        eltemp = ecpt(30)
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
!     WRITE(6,139) XSUBB,XSUBC,YSUBC
!
                              CALL mtrbsc
!                         U
!     NOW HAVE AT HAND  M    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
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
                                    Msum(npoint) = Msum(npoint) + prod9(K)/2.0D0
                                 ENDDO
!
!
                              ENDDO
!
                              ptmass = ptmass + dble(ecpt(10))/4.0D0*xsubb*ysubc
                           ENDIF
                        ENDDO
                        ptmass = ptmass/3.0D0
!
                        DO I = 1 , 36
                           tjte(I) = 0.0D0
                        ENDDO
!
!     FILL E-MATRIX
!
                        DO I = 1 , 9
                           e(I) = 0.0D0
                        ENDDO
                        DO I = 1 , 3
                           npoint = 3*I - 2
                           e(npoint) = ivect(I)
                           e(npoint+1) = jvect(I)
                           e(npoint+2) = kvect(I)
                        ENDDO
!
!
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
                        IF ( Necpt(4*Npivot+10)==0 ) THEN
!
                           DO K = 1 , 9
                              tite(K) = e(K)
                           ENDDO
                        ELSE
                           CALL transd(Necpt(4*Npivot+10),t)
!
!
                           CALL gmmatd(t,3,3,1,e(1),3,3,0,tite(1))
                        ENDIF
!
!
!     TRANSFORMATIONS AND INSERTION
!
                        DO J = 1 , 4
                           nbegin = 9*J - 9
                           DO I = 1 , 36
                              m6x6(I) = 0.0D0
                           ENDDO
                           DO I = 1 , 3
                              npoint = nbegin + I
                              m6x6(I+14) = Msum(npoint)
                              m6x6(I+20) = Msum(npoint+3)
                              m6x6(I+26) = Msum(npoint+6)
                           ENDDO
!
!
                           IF ( Npivot==J ) THEN
!
                              sign = (-1)**J
                              temp = ptmass*h
                              miz = temp/2.0D0*sign
                              iiz = temp*h/2.0D0
                              m6x6(1) = ptmass
                              m6x6(5) = miz
                              m6x6(8) = m6x6(1)
                              m6x6(10) = -miz
                              m6x6(20) = m6x6(10)
                              m6x6(22) = m6x6(22) + iiz
                              m6x6(25) = miz
                              m6x6(29) = m6x6(29) + iiz
                           ENDIF
!
!
                           IF ( Necpt(4*J+10)==0 ) THEN
!
                              DO I = 1 , 3
                                 npoint = 6*I - 5
                                 Npt = npoint + 21
                                 tjte(npoint) = e(I)
                                 tjte(npoint+1) = e(I+3)
                                 tjte(npoint+2) = e(I+6)
                                 tjte(Npt) = e(I)
                                 tjte(Npt+1) = e(I+3)
                                 tjte(Npt+2) = e(I+6)
                              ENDDO
                           ELSE
                              CALL transd(Necpt(4*J+10),t)
                              CALL gmmatd(e(1),3,3,1,t(1),3,3,0,tjte(1))
                              DO I = 1 , 3
                                 npoint = I + 21
                                 tjte(npoint) = tjte(I)
                                 tjte(npoint+6) = tjte(I+3)
                                 tjte(npoint+12) = tjte(I+6)
                              ENDDO
                              DO I = 1 , 3
                                 npoint = I + 21
                                 tjte(I) = tjte(npoint)
                                 tjte(I+6) = tjte(npoint+6)
                                 tjte(I+12) = tjte(npoint+12)
                                 tjte(I+3) = 0.0D0
!
                              ENDDO
                           ENDIF
!
!
                           CALL gmmatd(m6x6(1),6,6,0,tjte(1),6,6,0,temp36(1))
                           CALL gmmatd(tite(1),3,3,0,temp36(1),3,6,0,mout(1))
                           CALL gmmatd(tite(1),3,3,0,temp36(19),3,6,0,mout(19))
!
!
                           CALL sma2b(mout(1),Necpt(J+1),-1,ifmgg,0.0D0)
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
!
      CALL mesage(30,26,ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      nogo = 1
   END SUBROUTINE spag_block_1
END SUBROUTINE mqdplt
