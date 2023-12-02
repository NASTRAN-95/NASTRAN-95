!*==mflud4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mflud4
   USE c_sma2cl
   USE c_sma2et
   USE c_sma2io
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iret
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL mflud3
!
! End of declarations rewritten by SPAG
!
!*****
!     THIS ROUTINE IS USED FOR THE 4-SIDED FLUID ELEMENT. IT REARRANGES
!      THE DATA AND  CALLS THE MFLUD3 ROUTINE FOR EACH SUBELEMENT.
!****
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
!****
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
   IF ( ecpt(7)/=0.0 ) THEN
      ecpt(7) = ecpt(7)*2.0
      DO i = 1 , 24
         ecpt(i+50) = ecpt(i)
      ENDDO
      DO i = 5 , 19
         ecpt(i) = ecpt(i+1)
      ENDDO
      iret = 1
      SPAG_Loop_1_1: DO
!*****
!
!*****
         IF ( (necpt(2)==npvt) .OR. (necpt(3)==npvt) .OR. (necpt(4)==npvt) ) CALL mflud3
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
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
END SUBROUTINE mflud4
