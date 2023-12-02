!*==kslot.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kslot(Itype)
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k , nj
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL sma1b
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES THE STIFFNESS MATRIX TERMS FOR THE
!     CSLOT3 AND CSLOT4 TWO DIMENSIONAL LAPLACE ELEMENTS
!
!     IOPT-  CSLOT3 = 0,  CSLOT4 = 1
!
!     THE ECPT DATA FOR THESE ELEMENTS ARE
!
!     FIELD   CSLOT3                CSLOT4
!       1       ID                  ID
!       2       SIL1                SIL1
!       3       SIL2                SIL2
!       4       SIL3                SIL3
!       5       RHO                 SIL4
!       6       BULK                RHO
!       7       M                   BULK
!       8       N                   M
!       9       CID1                N
!       10      R1                  CID1
!      11       Z1                  R1
!      12       W1                  Z1
!      13       CID2                W1
!      14       R2                  CID2
!      15       Z2                  R2
!      16       W2                  Z2
!      17       CID3                W2
!      18       R3                  CID3
!      19       Z3                  R3
!      20       W3                  Z3
!      21       TEMP                W3
!      22                           CID4
!      23                           R4
!      24                           Z4
!      25                           W4
!      26                           TEMP
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
!
   IF ( Itype>0 ) THEN
!
!     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
!     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
!
      IF ( ecpt(6)==0.0 .OR. necpt(8)==0 ) RETURN
      k = -1
      SPAG_Loop_1_1: DO
         k = k + 1
         IF ( 2*necpt(9)<k*necpt(8) ) THEN
         ELSEIF ( 2*necpt(9)==k*necpt(8) ) THEN
            necpt(8) = necpt(8)*2
         ELSE
            CYCLE
         ENDIF
         ecpt(8) = float(necpt(8))/2.0
!
         nneg = 0
         ip = 0
         DO i = 1 , 4
            IF ( npvt==necpt(i+1) ) ip = ip + 1
            DO j = 1 , 3
               nj = i + j - 1
               IF ( nj>4 ) nj = nj - 4
               nptj = 4*(nj-1) + 11
               r(j) = ecpt(nptj)
               z(j) = ecpt(nptj+1)
            ENDDO
            coef = (r(2)-r(1))*(z(3)-z(1)) - (r(3)-r(1))*(z(2)-z(1))
            IF ( coef<0 ) THEN
               nneg = nneg + 1
            ELSEIF ( coef==0 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDDO
         IF ( nneg==1 .OR. nneg==3 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         IF ( ip/=1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
         DO i = 1 , 4
            ecpt(i+50) = ecpt(i)
         ENDDO
         DO i = 7 , 21
            ecpt(i+49) = ecpt(i)
         ENDDO
         ecpt(55) = ecpt(6)*2.0
         iret = 1
         EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   ELSE
      IF ( ecpt(5)==0.0 .OR. necpt(7)==0 ) RETURN
      k = -1
      SPAG_Loop_1_2: DO
         k = k + 1
         IF ( 2*necpt(8)<k*necpt(7) ) THEN
         ELSEIF ( 2*necpt(8)==k*necpt(7) ) THEN
            necpt(7) = necpt(7)*2
         ELSE
            CYCLE
         ENDIF
         ecpt(7) = float(necpt(7))/2.0
         DO i = 1 , 20
            ecpt(i+50) = ecpt(i)
         ENDDO
         iret = 4
         EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
   SPAG_Loop_1_3: DO
!
!     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
!
      IF ( necpt(52)==npvt .OR. necpt(53)==npvt .OR. necpt(54)==npvt ) THEN
         coef = 0.0
         a2 = 0.0
         DO i = 1 , 3
            j = i + 1
            IF ( j>3 ) j = j - 3
            k = j + 1
            IF ( k>3 ) k = k - 3
            lri = 4*i + 56
            lrj = 4*j + 56
            lrk = 4*k + 56
            coef = coef + ecpt(lri+2)
            fir(i) = ecpt(lrk) - ecpt(lrj)
            fiz(i) = ecpt(lrj+1) - ecpt(lrk+1)
            a2 = a2 + ecpt(lri)*fiz(i)
            IF ( necpt(i+51)==npvt ) ipvt = i
         ENDDO
         IF ( a2==0.0D0 ) EXIT SPAG_Loop_1_3
         coef = coef*ecpt(57)/(6.0D0*ecpt(55)*dabs(a2))
         i = npvt
         DO j = 1 , 3
            k = necpt(j+51)
            kij = coef*(fir(ipvt)*fir(j)+fiz(ipvt)*fiz(j))
            CALL sma1b(kij,k,i,ifile,0.0D0)
         ENDDO
      ENDIF
      IF ( iret==1 ) THEN
         ecpt(54) = ecpt(5)
         ecpt(68) = ecpt(23)
         ecpt(69) = ecpt(24)
         ecpt(70) = ecpt(25)
         iret = 2
      ELSEIF ( iret==2 ) THEN
         ecpt(53) = ecpt(4)
         ecpt(64) = ecpt(19)
         ecpt(65) = ecpt(20)
         ecpt(66) = ecpt(21)
         iret = 3
      ELSEIF ( iret==3 ) THEN
         ecpt(52) = ecpt(3)
         ecpt(60) = ecpt(15)
         ecpt(61) = ecpt(16)
         ecpt(62) = ecpt(17)
         iret = 4
      ELSE
         RETURN
      ENDIF
   ENDDO SPAG_Loop_1_3
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
!
      WRITE (out,99001) ufm , Necpt(1)
99001 FORMAT (A23,' 2160, BAD GEOMETRY OR ZERO COEFFICIENT FOR SLOT ','ELEMENT NUMBER',I18)
      nogo = .TRUE.
   END SUBROUTINE spag_block_1
END SUBROUTINE kslot
