!*==kflud4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kflud4
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ip , iret , j
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL kflud3
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED FOR THE 4-SIDED FLUID ELEMENT. IT REARRANGES
!     THE DATA AND  CALLS THE KFLUD3 ROUTINE FOR EACH SUBELEMENT.
!
!     THE ECPT DATA FOR THE ELEMENT AND ITS SUBELEMENTS ARE
!
!        FIELD      SYMBOL(FLUID4)      SYMBOL(FLUID3)
!           1            ID                  ID
!           2            SIL1                SIL1
!           3            SIL2                SIL2
!           4            SIL3                SIL3
!           5            SIL4                RHO
!           6            RHO                 BULK
!           7            BULK                N
!           8            N                   CSF
!           9            CSF                 R1
!          10            R1                  Z1
!          11            Z1                  -
!          12            -                   CSF
!          13            CSF                 R2
!          14            R2                  Z2
!          15            Z2                  -
!          16            -                   CSF
!          17            CSF                 R3
!          18            R3                  Z3
!          19            Z3                  -
!          20            -
!          21            CSF
!          22            R4
!          23            Z4
!          24            -
!          25            -
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
!
   IF ( necpt(6)<=0.0 ) RETURN
!
!     TEST FOR INTERIOR ANGLES GREATER THAN 180 DEGREES
!
   nneg = 0
   ip = 0
   DO i = 1 , 4
      DO j = 1 , 3
         nj = i + j - 1
         IF ( nj>4 ) nj = nj - 4
         nptj = 4*(nj-1) + 10
         r(j) = ecpt(nptj)
         z(j) = ecpt(nptj+1)
      ENDDO
      IF ( npvt==necpt(i+1) ) ip = ip + 1
      ki = (r(2)-r(1))*(z(3)-z(1)) - (r(3)-r(1))*(z(2)-z(1))
      IF ( ki<0 ) THEN
         nneg = nneg + 1
      ELSEIF ( ki==0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   IF ( nneg/=1 .AND. nneg/=3 ) THEN
      IF ( ip==1 ) THEN
         ecpt(6) = ecpt(6)*2.0
         DO i = 1 , 24
            ecpt(i+50) = ecpt(i)
         ENDDO
         DO i = 5 , 24
            ecpt(i) = ecpt(i+1)
         ENDDO
         iret = 1
         DO
!
            IF ( necpt(2)==npvt .OR. necpt(3)==npvt .OR. necpt(4)==npvt ) CALL kflud3
            IF ( iret==1 ) THEN
               ecpt(4) = ecpt(55)
               ecpt(17) = ecpt(72)
               ecpt(18) = ecpt(73)
               iret = 2
            ELSEIF ( iret==2 ) THEN
               ecpt(13) = ecpt(68)
               ecpt(14) = ecpt(69)
               ecpt(3) = ecpt(54)
               iret = 3
            ELSEIF ( iret==3 ) THEN
               ecpt(9) = ecpt(64)
               ecpt(10) = ecpt(65)
               ecpt(2) = ecpt(53)
               iret = 4
            ELSE
               RETURN
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      Nj = Necpt(1)
      IF ( iaxif/=0 ) Nj = Nj/1000
      WRITE (out,99001) ufm , Nj
99001 FORMAT (A23,' 5002, INTERIOR ANGLE GREATER THAN OR EQUAL TO 180 ','DEGREES FOR ELEMENT',I12)
      nogo = .TRUE.
   END SUBROUTINE spag_block_1
END SUBROUTINE kflud4
