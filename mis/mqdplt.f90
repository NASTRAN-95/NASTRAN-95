
SUBROUTINE mqdplt
   IMPLICIT NONE
   DOUBLE PRECISION A(1) , A1(3) , D1(3) , D2(3) , Dpdum1(27) , Dpdum2(10) , E(9) , H , Iiz , Ivect(3) , Jvect(3) , Kvect(3) ,      &
                  & M6x6(36) , Miz , Mout(36) , Prod9(9) , Ptmass , R(2,4) , Requiv(8) , Sign , T(9) , Temp , Temp36(36) , Temp9(9) &
                  & , Tite(9) , Tjte(36) , U1 , U2 , V(2) , Vv(2) , Xsubb , Xsubc , Ysubc
   REAL Alp12 , Alpha1 , Alpha2 , Consts(5) , Cosang , Costh , Degra , Dum1(10) , Dum2(25) , Dum3(2) , Dumcl(7) , Ecpt(100) ,       &
      & Eltemp , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , Rho , Sigcom , Sigshe , Sigten , Sinang ,   &
      & Sinth , Sp1(33) , Space(2) , Spdum(20) , Stress , Theta , Tsub0 , Vq1(3) , Vq2(3) , Vq3(3) , Vq4(3)
   INTEGER Ifmgg , Inflag , Ising , Jnot , Km , Link(10) , Matid , Nbegin , Necpt(100) , Nogo , Npivot , Npoint , Npvt , Nsubc ,    &
         & Subsca , Subscb , Subscc
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222 , Space
   COMMON /sma2cl/ Dum3 , Npvt , Dumcl , Link , Nogo
   COMMON /sma2dp/ Mout , Tite , Tjte , Temp36 , Dpdum1 , D1 , D2 , A1 , T , V , Vv , Iiz , Miz , Sign , Spdum , M6x6 , Dpdum2 ,    &
                 & Prod9 , Temp9 , Xsubb , Xsubc , Ysubc , E , Temp , Sp1 , Km , Nbegin , Jnot , Npivot , Theta , Nsubc , Ising ,   &
                 & Subsca , Subscb , Subscc , Sinang , Cosang , Npoint , Ivect , Jvect , Kvect , U1 , U2 , R , H , Ptmass
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Dum1 , Ifmgg , Dum2
   INTEGER i , j , k , m(12) , npt
   DOUBLE PRECISION msum(36)
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
   EQUIVALENCE (Consts(4),Degra) , (Necpt(1),Ecpt(1)) , (R(1,1),Requiv(1)) , (Vq1(1),Ecpt(15)) , (Vq2(1),Ecpt(19)) ,                &
    & (Vq3(1),Ecpt(23)) , (Vq4(1),Ecpt(27)) , (A(1),Mout(1))
   DATA m/2 , 4 , 1 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4/
!
!     DETERMINE PIVOT POINT NUMBER
!
   DO i = 1 , 4
      IF ( Npvt==Necpt(i+1) ) THEN
         Npivot = i
         GOTO 100
      ENDIF
   ENDDO
!
!     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
!
   CALL mesage(-30,34,Ecpt(1))
!
 100  Theta = Ecpt(6)*Degra
   Sinang = sin(Theta)
   Cosang = cos(Theta)
!
   IF ( Npivot<=2 ) THEN
      Jnot = Npivot + 2
   ELSE
      Jnot = Npivot - 2
   ENDIF
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
   DO i = 1 , 8
      Requiv(i) = 0.0D0
   ENDDO
!
!     SHIFT ECPT UP TO MATCH MTRBSC FOR CERTAIN VARIABLES.
!
   DO i = 6 , 12
      Ecpt(i) = Ecpt(i+1)
   ENDDO
!
   DO i = 1 , 3
      D1(i) = dble(Vq3(i)) - dble(Vq1(i))
      D2(i) = dble(Vq4(i)) - dble(Vq2(i))
      A1(i) = dble(Vq2(i)) - dble(Vq1(i))
   ENDDO
!
!     NON-NORMALIZED K-VECTOR = D1 CROSS D2
!
   Kvect(1) = D1(2)*D2(3) - D2(2)*D1(3)
   Kvect(2) = D1(3)*D2(1) - D2(3)*D1(1)
   Kvect(3) = D1(1)*D2(2) - D2(1)*D1(2)
!
!     NORMALIZE K-VECTOR
!
   Temp = dsqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
   IF ( Temp/=0.0D0 ) THEN
      DO i = 1 , 3
         Kvect(i) = Kvect(i)/Temp
      ENDDO
!
!     COMPUTE H = A1 DOT KVECT
!
      H = A1(1)*Kvect(1) + A1(2)*Kvect(2) + A1(3)*Kvect(3)
!
!     WRITE (6,109)
!     WRITE (6,119)
!     WRITE (6,1195) H,(D1(I),D2(I),A1(I),I=1,3)
!
!     I-VECTOR = (A1) - H*(KVECT)  NON-NORMALIZED
!
      DO i = 1 , 3
         Ivect(i) = A1(i) - H*Kvect(i)
      ENDDO
!
!     NORMALIZE I-VECTOR
!
      Temp = dsqrt(Ivect(1)**2+Ivect(2)**2+Ivect(3)**2)
      IF ( Temp/=0.0D0 ) THEN
         DO i = 1 , 3
            Ivect(i) = Ivect(i)/Temp
         ENDDO
!
!     J-VECTOR = K CROSS I, AND X3 CALCULATION
!
         Jvect(1) = Kvect(2)*Ivect(3) - Ivect(2)*Kvect(3)
         Jvect(2) = Kvect(3)*Ivect(1) - Ivect(3)*Kvect(1)
         Jvect(3) = Kvect(1)*Ivect(2) - Ivect(1)*Kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
         Temp = dsqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
         IF ( Temp/=0.0D0 ) THEN
            DO i = 1 , 3
               Jvect(i) = Jvect(i)/Temp
            ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
            R(1,3) = D1(1)*Ivect(1) + D1(2)*Ivect(2) + D1(3)*Ivect(3)
!
!     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
!
            R(1,2) = A1(1)*Ivect(1) + A1(2)*Ivect(2) + A1(3)*Ivect(3)
            R(2,3) = D1(1)*Jvect(1) + D1(2)*Jvect(2) + D1(3)*Jvect(3)
!
!     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
!
            R(1,4) = D2(1)*Ivect(1) + D2(2)*Ivect(2) + D2(3)*Ivect(3) + R(1,2)
            R(2,4) = D2(1)*Jvect(1) + D2(2)*Jvect(2) + D2(3)*Jvect(3)
!
!     WRITE (6,129) (IVECT(I),I=1,3),(JVECT(I),I=1,3),(KVECT(I),I=1,3),
!    1              ((R(I,J),J=1,4),I=1,2)
!
!     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
!
            IF ( R(2,3)>0.0D0 .AND. R(2,4)>0.0D0 ) THEN
               Temp = R(1,2) - (R(1,2)-R(1,3))*R(2,4)/R(2,3)
               IF ( R(1,4)<Temp ) THEN
                  Temp = R(2,3)*R(1,4)/R(2,4)
                  IF ( R(1,3)>Temp ) THEN
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
                     DO i = 1 , 36
                        msum(i) = 0.0D0
                     ENDDO
                     Ptmass = 0.0D0
                     Eltemp = Ecpt(30)
!
                     DO j = 1 , 4
                        IF ( j/=Jnot ) THEN
                           Km = 3*j - 3
                           Subsca = m(Km+1)
                           Subscb = m(Km+2)
                           Subscc = m(Km+3)
!
                           DO i = 1 , 2
                              V(i) = R(i,Subscb) - R(i,Subsca)
                              Vv(i) = R(i,Subscc) - R(i,Subsca)
                           ENDDO
                           Xsubb = dsqrt(V(1)**2+V(2)**2)
                           U1 = V(1)/Xsubb
                           U2 = V(2)/Xsubb
                           Xsubc = U1*Vv(1) + U2*Vv(2)
                           Ysubc = U1*Vv(2) - U2*Vv(1)
!
                           Sinth = Sinang*U1 - Cosang*U2
                           Costh = Cosang*U1 + Sinang*U2
                           IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
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
                           T(1) = 1.0D0
                           T(2) = 0.0D0
                           T(3) = 0.0D0
                           T(4) = 0.0D0
                           T(5) = U1
                           T(6) = U2
                           T(7) = 0.0D0
                           T(8) = -U2
                           T(9) = U1
!
!
!     FIND WHICH POINT OF THE SUBTRIANGLE IS ALSO THE PIVOT OF THE
!     QUADRILATERAL
!
                           DO i = 1 , 3
                              Npoint = Km + i
                              IF ( m(Npoint)==Npivot ) THEN
                                 Nbegin = 27*i - 27
                                 EXIT
                              ENDIF
                           ENDDO
!
                           DO i = 1 , 3
                              Npoint = Nbegin + 9*i - 8
                              CALL gmmatd(T,3,3,1,A(Npoint),3,3,0,Temp9)
                              CALL gmmatd(Temp9,3,3,0,T,3,3,0,Prod9)
!
!     ADD THIS PRODUCT IN NOW.
!
                              Npoint = Km + i
                              Npoint = 9*m(Npoint) - 9
                              DO k = 1 , 9
                                 Npoint = Npoint + 1
                                 msum(Npoint) = msum(Npoint) + Prod9(k)/2.0D0
                              ENDDO
!
!
                           ENDDO
!
                           Ptmass = Ptmass + dble(Ecpt(10))/4.0D0*Xsubb*Ysubc
                        ENDIF
                     ENDDO
                     Ptmass = Ptmass/3.0D0
!
                     DO i = 1 , 36
                        Tjte(i) = 0.0D0
                     ENDDO
!
!     FILL E-MATRIX
!
                     DO i = 1 , 9
                        E(i) = 0.0D0
                     ENDDO
                     DO i = 1 , 3
                        Npoint = 3*i - 2
                        E(Npoint) = Ivect(i)
                        E(Npoint+1) = Jvect(i)
                        E(Npoint+2) = Kvect(i)
                     ENDDO
!
!
!              T
!     FORM   T   E      STORE IN TITE-MATRIX (6X3)
!             I
!
                     IF ( Necpt(4*Npivot+10)==0 ) THEN
!
                        DO k = 1 , 9
                           Tite(k) = E(k)
                        ENDDO
                     ELSE
                        CALL transd(Necpt(4*Npivot+10),T)
!
!
                        CALL gmmatd(T,3,3,1,E(1),3,3,0,Tite(1))
                     ENDIF
!
!
!     TRANSFORMATIONS AND INSERTION
!
                     DO j = 1 , 4
                        Nbegin = 9*j - 9
                        DO i = 1 , 36
                           M6x6(i) = 0.0D0
                        ENDDO
                        DO i = 1 , 3
                           Npoint = Nbegin + i
                           M6x6(i+14) = msum(Npoint)
                           M6x6(i+20) = msum(Npoint+3)
                           M6x6(i+26) = msum(Npoint+6)
                        ENDDO
!
!
                        IF ( Npivot==j ) THEN
!
                           Sign = (-1)**j
                           Temp = Ptmass*H
                           Miz = Temp/2.0D0*Sign
                           Iiz = Temp*H/2.0D0
                           M6x6(1) = Ptmass
                           M6x6(5) = Miz
                           M6x6(8) = M6x6(1)
                           M6x6(10) = -Miz
                           M6x6(20) = M6x6(10)
                           M6x6(22) = M6x6(22) + Iiz
                           M6x6(25) = Miz
                           M6x6(29) = M6x6(29) + Iiz
                        ENDIF
!
!
                        IF ( Necpt(4*j+10)==0 ) THEN
!
                           DO i = 1 , 3
                              Npoint = 6*i - 5
                              npt = Npoint + 21
                              Tjte(Npoint) = E(i)
                              Tjte(Npoint+1) = E(i+3)
                              Tjte(Npoint+2) = E(i+6)
                              Tjte(npt) = E(i)
                              Tjte(npt+1) = E(i+3)
                              Tjte(npt+2) = E(i+6)
                           ENDDO
                        ELSE
                           CALL transd(Necpt(4*j+10),T)
                           CALL gmmatd(E(1),3,3,1,T(1),3,3,0,Tjte(1))
                           DO i = 1 , 3
                              Npoint = i + 21
                              Tjte(Npoint) = Tjte(i)
                              Tjte(Npoint+6) = Tjte(i+3)
                              Tjte(Npoint+12) = Tjte(i+6)
                           ENDDO
                           DO i = 1 , 3
                              Npoint = i + 21
                              Tjte(i) = Tjte(Npoint)
                              Tjte(i+6) = Tjte(Npoint+6)
                              Tjte(i+12) = Tjte(Npoint+12)
                              Tjte(i+3) = 0.0D0
!
                           ENDDO
                        ENDIF
!
!
                        CALL gmmatd(M6x6(1),6,6,0,Tjte(1),6,6,0,Temp36(1))
                        CALL gmmatd(Tite(1),3,3,0,Temp36(1),3,6,0,Mout(1))
                        CALL gmmatd(Tite(1),3,3,0,Temp36(19),3,6,0,Mout(19))
!
!
                        CALL sma2b(Mout(1),Necpt(j+1),-1,Ifmgg,0.0D0)
!
                     ENDDO
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
            CALL mesage(30,35,Ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
            Nogo = 1
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!
   CALL mesage(30,26,Ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
   Nogo = 1
END SUBROUTINE mqdplt
