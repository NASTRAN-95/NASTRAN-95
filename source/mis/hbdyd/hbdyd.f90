!*==hbdyd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hbdyd
   USE c_condad
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_hmtout
   USE c_matin
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(5) :: a1
   REAL(REAL64) , DIMENSION(3) :: a2 , a3 , a4
   REAL(REAL64) , DIMENSION(16) :: c
   REAL(REAL64) , DIMENSION(4,4) :: cc
   REAL :: cp , dict5 , h
   INTEGER , DIMENSION(13) :: dict
   REAL , DIMENSION(53) :: ecpt
   INTEGER :: i , i1 , i2 , ic , ij , imhere , irow , isize , j , j1 , j2 , jcol , k , n , outpt
   REAL(REAL64) :: itemp , ke , me
   REAL(REAL64) , DIMENSION(64) :: mast
   REAL(REAL64) , DIMENSION(8,8) :: master
   INTEGER , DIMENSION(53) :: necpt
   INTEGER , DIMENSION(7) , SAVE :: ngrids
   INTEGER , DIMENSION(8) :: set1 , siltab
   INTEGER , DIMENSION(4) :: set2
   EXTERNAL daxb , emgout , hmat , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS IS THE BOUNDARY CONDITION (HEAT) ELEMENT ROUTINE
!     IT PRODUCES THE STIFFNESS AND OR DAMPING ELEMENT MATRICES.
!
   !>>>>EQUIVALENCE (necpt(1),Ecpt(1)) , (set2(1),set1(5)) , (Ecpt1,Ecpt(1)) , (dict5,dict(5)) , (Ksystm(2),Outpt) , (cc(1,1),c(1)) ,    &
!>>>>    & (master(1,1),mast(1))
   DATA ngrids/1 , 2 , 2 , 3 , 4 , 2 , 2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     EST ENTRY FOR -CHBDY- ELEMENT
!     ======================================================
!     ECPT( 1)  = EL-ID       ELEMENT ID
!     ECPT( 2)  = IFLAG       ELEM. TYPE FLAG = (1,2,3,4,5,6,7)
!     ECPT( 3)  = SIL-1       SCALER INDICES
!     ECPT( 4)  = SIL-2
!     ECPT( 5)  = SIL-3
!     ECPT( 6)  = SIL-4
!     ECPT( 7)  = SIL-5
!     ECPT( 8)  = SIL-6
!     ECPT( 9)  = SIL-7
!     ECPT(10)  = SIL-8
!     ECPT(11)  = V1          ORIENTATION VECTOR
!     ECPT(12)  = V2
!     ECPT(13)  = V3
!     ECPT(14)  = ECPT14
!     ECPT(15)  = MATFLG      MAT ID FOR MAT4, MAT5 DATA
!     ECPT(16)  = AF          AREA FACTOR
!     ECPT(17)  = EMISS       EMISSIVITY COEFF
!     ECPT(18)  = ABSORP      ABSORPTIVITY COEFF
!     ECPT(19)  = R1          RADII OF ELIPTICAL CYLINDER
!     ECPT(20)  = R2
!     ECPT(21)  = CSID-1      COORDINATE SYSTEM ID AND
!     ECPT(22)  = X1          COORDINATE GRID POINTS
!     ECPT(23)  = Y1          (1-4 ARE ELEMENT POINTS,
!     ECPT(24)  = Z1
!     ECPT(25)  = CSID-2
!     ECPT(26)  = X2
!     ECPT(27)  = Y2
!     ECPT(28)  = Z2
!     ECPT(29)  = CSID-3
!     ECPT(30)  = X3
!     ECPT(31)  = Y3
!     ECPT(32)  = Z3
!     ECPT(33)  = CSID-4
!     ECPT(34)  = X4
!     ECPT(35)  = Y4
!     ECPT(36)  = Z4
!     ECPT(37)  = CSID-5       5-8 ARE POINTS IN THE FLUID)
!       -ETC-     -ETC-
!     ECPT(53)  = AVGTMP      AVERAGE ELEM. TEMPERATURE
!
!     GENERAL INITIALIZATION
!
         IF ( .NOT.heat ) RETURN
         imhere = 0
         IF ( iflag>=1 .AND. iflag<=7 ) THEN
            IF ( iflag==7 ) af = pi*(dble(r1)+dble(r2))
            n = ngrids(iflag)
            dict(1) = estid
            dict(2) = 1
            dict(4) = 1
            dict5 = 0.0
!
!     MASTER OUTPUT MATRIX OF SIZE UP TO 8 X 8 IS FORMED.  DUPLICATE
!     SILS ARE SUPERIMPOSED RESULTING IN A POSSIBLY SMALLER OUTPUT MATRX
!
!     FOR A GIVEN ELEMENT-ID THE MATRIX OUTPUT WILL BE OF ORDER EQUAL
!     TO THE NUMBER OF UNIQUE SILS PRESENT.
!
!     IFLAG = 1 WILL BE 1X1 OR 2X2    *
!     IFLAG = 2 WILL BE 2X2 UP TO 4X4  *
!     IFLAG = 3 WILL BE 2X2 UP TO 4X4   * (DEPENDING ON GROUDING AND
!     IFLAG = 4 WILL BE 3X3 UP TO 6X6  *   DUPLICATE SILS.)
!     IFLAG = 5 WILL BE 4X4 UP TO 8X8 *
!
!     -SET1- WILL BE A MAP OF OUTPUT POSITIONS FOR SILS 1 THRU 4
!     -SET2- WILL BE A MAP OF OUTPUT POSITIONS FOR SILS 5 THRU 8
!
!
!     FIRST FORM THE TABLE OF UNIQUE SILS.
!
            isize = 0
            SPAG_Loop_1_1: DO i = 1 , 8
               IF ( sils(i)>0 ) THEN
                  IF ( isize>0 ) THEN
                     DO j = 1 , isize
                        IF ( sils(i)==siltab(j) ) CYCLE SPAG_Loop_1_1
                     ENDDO
                  ENDIF
                  isize = isize + 1
                  siltab(isize) = sils(i)
               ENDIF
            ENDDO SPAG_Loop_1_1
            CALL sort(0,0,1,1,siltab(1),isize)
            imhere = 50
            IF ( isize>0 ) THEN
!
!     BUILD -SET1- AND -SET2- MAPS OF WHERE OUTPUTS GO IN MASTER OUTPUT.
!
               DO i = 1 , 8
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        j = 8
                        IF ( sils(i)>0 ) THEN
                           DO j = 1 , isize
                              IF ( sils(i)==siltab(j) ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDDO
                           imhere = 80
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        spag_nextblock_2 = 2
                     CASE (2)
                        set1(i) = j
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               dict(3) = isize
!
!     FORM STIFFNESS -HEAT- IF REQUESTED.
!
               IF ( kmbgg(1)/=0 ) THEN
                  inflag = 1
                  eltemp = avgtmp
                  matid = matflg
                  IF ( matid/=0 ) THEN
                     CALL hmat(necpt)
                     cp = cpx
                     h = hx
                     IF ( h/=0.0 ) THEN
                        IF ( iflag==2 .OR. iflag==6 .OR. iflag==7 ) THEN
!
!     IFLAG = 2, (LINE OR ELLIPTIC CYL. )    **    **
!             2 GRID POINTS           H*AF*L * 2  1 *
!                           (2X2)  C =------ *      *
!                                       6    * 1  2 *
!                                            **    **
!
                           c(1) = h
                           c(2) = af
                           c(3) = ecpt(26) - ecpt(22)
                           c(4) = ecpt(27) - ecpt(23)
                           c(5) = ecpt(28) - ecpt(24)
                           c(1) = c(1)*c(2)*dsqrt(c(3)**2+c(4)**2+c(5)**2)/3.0D0
                           c(2) = c(1)/2.0D0
                           c(5) = c(2)
                           c(6) = c(1)
                        ELSEIF ( iflag==3 ) THEN
!
!     IFLAG = 3, (REVOLUTION), 2 GRID-POINTS     **                **
!                                                *(3X +X )  (X + X )*
!                                        H*2PI*L *   1  2     1   2 *
!                             (2X2)  C = ------- *                  *
!                                          12    *(X + X )  (X +3X )*
!                                                *  1   2     1   2 *
!                                                **                **
!
                           IF ( ecpt(22)>0.0 .AND. ecpt(26)>0.0 ) THEN
                              IF ( ecpt(23)==0.0 .AND. ecpt(27)==0.0 ) THEN
!
!     FILL CONDUCTIVITIY MATRIX
!
                                 c(1) = h
                                 c(2) = pi
                                 c(3) = ecpt(26) - ecpt(22)
                                 c(4) = ecpt(28) - ecpt(24)
!
!     NOTE Y2 AND Y1 ARE 0 FOR REVOLUTION ELEMENT.
!
                                 c(1) = c(1)*c(2)*dsqrt(c(3)**2+c(4)**2)/6.0D0
                                 c(2) = c(1)*dble(ecpt(22)+ecpt(26))
                                 c(5) = c(2)
                                 c(6) = c(1)*dble(ecpt(22)+3.0*ecpt(26))
                                 c(1) = c(1)*dble(3.0*ecpt(22)+ecpt(26))
                                 GOTO 2
                              ENDIF
                           ENDIF
                           WRITE (outpt,99001) ufm , necpt(1)
99001                      FORMAT (A23,' 3088, ILLEGAL GEOMETRY FOR REVOLUTION ELEMENT',I14)
                           nogo = .TRUE.
                           RETURN
                        ELSEIF ( iflag==4 ) THEN
!
!     IFLAG = 4, (TRIANGLE), 3 GRID-POINTS.       **       **
!                                                 * 2  1  1 *
!                                           H * A *         *
!                                (3X3) C =  ----- * 1  2  1 *
!                                            24   *         *
!                                                 * 1  1  2 *
!                                                 **       **
!
!
!     COMPUTE AREA -A- OF TRIANGLE   GET R2-R1 AND R3-R2
!
                           c(1) = ecpt(26) - ecpt(22)
                           c(2) = ecpt(27) - ecpt(23)
                           c(3) = ecpt(28) - ecpt(24)
                           c(4) = ecpt(30) - ecpt(26)
                           c(5) = ecpt(31) - ecpt(27)
                           c(6) = ecpt(32) - ecpt(28)
!
!     (R2-R1) X (R3-R2)  INTO  C(1),C(2),C(3)
!
                           CALL daxb(c(1),c(4),c(1))
                           c(7) = dsqrt(c(1)**2+c(2)**2+c(3)**2)
                           IF ( c(7)<=0.0 ) THEN
                              WRITE (outpt,99002) ufm , necpt(1)
99002                         FORMAT (A23,' 3089, ILLEGAL GEOMETRY FOR TRIANGLE ELEMENT',I14)
                              nogo = .TRUE.
                              RETURN
                           ELSE
                              c(2) = c(7)*dble(h)/24.0D0
                              c(1) = 2.0D0*c(2)
                              c(3) = c(2)
                              c(5) = c(2)
                              c(6) = c(1)
                              c(7) = c(2)
                              c(9) = c(2)
                              c(10) = c(2)
                              c(11) = c(1)
                           ENDIF
                        ELSEIF ( iflag==5 ) THEN
!
!     IFLAG = 5, (QUADRILATERAL), 4 GRID-POINTS.
!
!               ***                                              ***
!               * 2(A2+A3+A4)  (A3+A4)     (A2+A4)      (A2+A3)    *
!               *                                                  *
!               *              2(A1+A3+A4) (A1+A4)      (A1+A3)    *
!   (4X4)  C  = *                                                  *
!               *                           2(A1+A2+A4) (A1+A2)    *
!               *     -SYM-                                        *
!               *                                       2(A1+A2+A3)*
!               ***                                              ***
!
!     R  =  XI, YI, ZI
!      I
!
!     A1 = MAG((R3-R2) X (R4-R3))
!     A2 = MAG((R4-R3) X (R1-R4))
!     A3 = MAG((R1-R4) X (R2-R1))
!     A4 = MAG((R2-R1) X (R3-R2))
!
!
!     R3-R2
!
                           c(1) = ecpt(30) - ecpt(26)
                           c(2) = ecpt(31) - ecpt(27)
                           c(3) = ecpt(32) - ecpt(28)
!
!     R4-R3
!
                           c(4) = ecpt(34) - ecpt(30)
                           c(5) = ecpt(35) - ecpt(31)
                           c(6) = ecpt(36) - ecpt(32)
!
!     R1-R4
!
                           c(7) = ecpt(22) - ecpt(34)
                           c(8) = ecpt(23) - ecpt(35)
                           c(9) = ecpt(24) - ecpt(36)
!
!     R2-R1
!
                           c(10) = ecpt(26) - ecpt(22)
                           c(11) = ecpt(27) - ecpt(23)
                           c(12) = ecpt(28) - ecpt(24)
!
!
                           CALL daxb(c(1),c(4),a1(1))
                           CALL daxb(c(4),c(7),a2(1))
                           CALL daxb(c(7),c(10),a3(1))
                           CALL daxb(c(10),c(1),a4(1))
!
                           c(1) = a1(1)*a2(1) + a1(2)*a2(2) + a1(3)*a2(3)
                           c(2) = a1(1)*a3(1) + a1(2)*a3(2) + a1(3)*a3(3)
                           c(3) = a1(1)*a4(1) + a1(2)*a4(2) + a1(3)*a4(3)
                           IF ( c(1)*c(2)*c(3)<=0.0D0 ) THEN
                              WRITE (outpt,99003) ufm , necpt(1)
99003                         FORMAT (A23,' 3090, ILLEGAL GEOMETRY FOR QUAD. ELEMENT',I14)
                              nogo = .TRUE.
                              RETURN
                           ELSE
                              a1(1) = dsqrt(a1(1)**2+a1(2)**2+a1(3)**2)
                              a1(2) = dsqrt(a2(1)**2+a2(2)**2+a2(3)**2)
                              a1(3) = dsqrt(a3(1)**2+a3(2)**2+a3(3)**2)
                              a1(4) = dsqrt(a4(1)**2+a4(2)**2+a4(3)**2)
                              a1(5) = a1(1) + a1(2) + a1(3) + a1(4)
                              itemp = dble(h)/48.0D0
                              DO i = 1 , 4
                                 ic = 4*(i-1)
                                 DO j = 1 , 4
                                    ij = ic + j
                                    IF ( i==j ) THEN
                                       c(ij) = itemp*(2.0D0*(a1(5)-a1(i)))
                                    ELSE
                                       c(ij) = itemp*(a1(5)-a1(i)-a1(j))
                                    ENDIF
                                 ENDDO
                              ENDDO
                           ENDIF
                        ELSE
!
!     IFLAG = 1, (POINT), 1 GRID-POINT.  (1 X 1)  C = H * AF
!
                           c(1) = h
                           c(2) = af
                           c(1) = c(1)*c(2)
                        ENDIF
!
!     HERE WHEN -C- MATRIX OF SIZE N X N IS READY FOR INSERTION (MAPING)
!     INTO MASTER OUTPUT MATRIX OF SIZE ISIZE X ISIZE.
!
 2                      DO i = 1 , 64
                           mast(i) = 0.0D0
                        ENDDO
!
                        DO i = 1 , n
                           i1 = set1(i)
                           i2 = set2(i)
                           DO j = 1 , n
                              j1 = set1(j)
                              j2 = set2(j)
                              ke = cc(i,j)
                              master(i1,j1) = master(i1,j1) + ke
                              master(i1,j2) = master(i1,j2) - ke
                              master(i2,j1) = master(i2,j1) - ke
                              master(i2,j2) = master(i2,j2) + ke
                           ENDDO
                        ENDDO
!
!     CONDENSE (ISIZE X ISIZE) MATRIX IN (8 X 8) MASTER ARRAY INTO A
!     SINGLE STRAND FOR OUTPUT TO EMGOUT
!
                        k = 0
                        DO jcol = 1 , isize
                           DO irow = 1 , isize
                              k = k + 1
                              mast(k) = master(irow,jcol)
                           ENDDO
                        ENDDO
!
!     OUTPUT VIA EMGOUT THE TRIANGLE IN GLOBAL FOR STIFFNESS MATRIX
!
                        CALL emgout(mast(1),mast(1),k,1,dict,1,iprec)
                     ENDIF
                  ENDIF
               ENDIF
!
!     FORM DAMPING -HEAT- IF REQUESTED.
!
               IF ( kmbgg(3)/=0 ) THEN
                  inflag = 4
                  eltemp = avgtmp
                  matid = matflg
                  IF ( matid/=0 ) THEN
                     CALL hmat(necpt)
                     cp = hx
                     IF ( cp/=0.0 ) THEN
                        IF ( iflag==2 .OR. iflag==6 .OR. iflag==7 ) THEN
!
!     IFLAG = 2, (LINE OR ELLIPTIC CYL. )
!             2 GRID POINTS           CP*AF*L*      *
!                                  C = ------*1 , 1 *
!                                        2   *      *
!
                           c(1) = cp
                           c(2) = af
                           c(3) = ecpt(26) - ecpt(22)
                           c(4) = ecpt(27) - ecpt(23)
                           c(5) = ecpt(28) - ecpt(24)
                           c(1) = c(1)*c(2)*dsqrt(c(3)**2+c(4)**2+c(5)**2)/2.0D0
                           c(2) = c(1)
                        ELSEIF ( iflag==3 ) THEN
!
!     IFLAG = 3, (REVOLUTION), 2 GRID-POINTS
!                                               CP*PI*L *              *
!                                           C = ------- *2X +X , 2X +X *
!                                                  3    *  1  2    2  1*
!
                           c(1) = cp
                           c(2) = pi
                           c(3) = ecpt(26) - ecpt(22)
                           c(4) = ecpt(28) - ecpt(24)
!
!     NOTE Y2 AND Y1 ARE 0 FOR REVOLUTION ELEMENT.
!
                           c(1) = c(1)*c(2)*dsqrt(c(3)**2+c(4)**2)/3.0D0
                           c(2) = c(1)*dble(ecpt(22)+2.0*ecpt(26))
                           c(1) = c(1)*dble(2.0*ecpt(22)+ecpt(26))
                        ELSEIF ( iflag==4 ) THEN
!
!     IFLAG = 4, (TRIANGLE), 3 GRID-POINTS.
!                                          CP*A *         *
!                                      C = ---- * 1, 1, 1 *
!                                           3   *         *
!
!
!     COMPUTE AREA -A- OF TRIANGLE   GET R2-R1 AND R3-R2
!
                           c(1) = ecpt(26) - ecpt(22)
                           c(2) = ecpt(27) - ecpt(23)
                           c(3) = ecpt(28) - ecpt(24)
                           c(4) = ecpt(30) - ecpt(26)
                           c(5) = ecpt(31) - ecpt(27)
                           c(6) = ecpt(32) - ecpt(28)
!
!     (R2-R1) X (R3-R2)  INTO  C(1),C(2),C(3)
!
                           CALL daxb(c(1),c(4),c(1))
                           c(7) = dsqrt(c(1)**2+c(2)**2+c(3)**2)
                           c(1) = c(7)*dble(cp)/6.0D0
                           c(2) = c(1)
                           c(3) = c(1)
                        ELSEIF ( iflag==5 ) THEN
!
!     IFLAG = 5, (QUADRILATERAL), 4 GRID-POINTS.
!
!                                CP *                                  *
!                            C = -- * A +A +A , A +A +A , A +A +A , ETC*
!                                6  *  2  3  4   3  4  1   4  1  2     *
!
!     R  =  XI, YI, ZI
!      I
!
!     A1 = MAG((R3-R2) X (R4-R3))
!     A2 = MAG((R4-R3) X (R1-R4))
!     A3 = MAG((R1-R4) X (R2-R1))
!     A4 = MAG((R2-R1) X (R3-R2))
!
!
!     R3-R2
!
                           c(1) = ecpt(30) - ecpt(26)
                           c(2) = ecpt(31) - ecpt(27)
                           c(3) = ecpt(32) - ecpt(28)
!
!     R4-R3
!
                           c(4) = ecpt(34) - ecpt(30)
                           c(5) = ecpt(35) - ecpt(31)
                           c(6) = ecpt(36) - ecpt(32)
!
!     R1-R4
!
                           c(7) = ecpt(22) - ecpt(34)
                           c(8) = ecpt(23) - ecpt(35)
                           c(9) = ecpt(24) - ecpt(36)
!
!     R2-R1
!
                           c(10) = ecpt(26) - ecpt(22)
                           c(11) = ecpt(27) - ecpt(23)
                           c(12) = ecpt(28) - ecpt(24)
!
!
                           CALL daxb(c(1),c(4),a1(1))
                           CALL daxb(c(4),c(7),a2(1))
                           CALL daxb(c(7),c(10),a3(1))
                           CALL daxb(c(10),c(1),a4(1))
!
                           a1(1) = dsqrt(a1(1)**2+a1(2)**2+a1(3)**2)
                           a1(2) = dsqrt(a2(1)**2+a2(2)**2+a2(3)**2)
                           a1(3) = dsqrt(a3(1)**2+a3(2)**2+a3(3)**2)
                           a1(4) = dsqrt(a4(1)**2+a4(2)**2+a4(3)**2)
                           a1(5) = a1(1) + a1(2) + a1(3) + a1(4)
                           itemp = dble(cp)/12.0D0
                           DO i = 1 , 4
                              c(i) = itemp*(a1(5)-a1(i))
                           ENDDO
                        ELSE
!
!     IFLAG = 1, (POINT), 1 GRID-POINT.  (1 X 1)  C = CP* AF
!
                           c(1) = cp
                           c(2) = af
                           c(1) = c(1)*c(2)
                        ENDIF
!
!     HERE WHEN DIAGONAL C MATRIX OF SIZE 1 X N IS READY FOR INSERTION
!     (MAPING) INTO MASTER DIAGONAL OUTPUT MATRIX OF SIZE 1 X ISIZE.
!
                        DO i = 1 , 8
                           mast(i) = 0.0D0
                        ENDDO
!
                        DO i = 1 , n
                           i1 = set1(i)
                           i2 = set2(i)
                           me = c(i)
                           mast(i1) = mast(i1) + me
                           mast(i2) = mast(i2) + me
                        ENDDO
!
!     OUTPUT VIA EMGOUT THE DIAGONAL MATRIX IN GLOBAL
!
                        dict(2) = 2
                        CALL emgout(mast(1),mast(1),isize,1,dict,3,iprec)
                     ENDIF
                  ENDIF
               ENDIF
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOGIC ERROR
!
         WRITE (outpt,99004) sfm , imhere , necpt(1) , sils
99004    FORMAT (A25,' 3037 FROM HBDYD.',/5X,'LOGIC ERROR,  IMHERE =',I5,'  ELEMENT ID = ',I10,/5X,'SILS =',8I10)
         nogo = .TRUE.
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE hbdyd
