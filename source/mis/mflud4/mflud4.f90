!*==mflud4.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mflud4
   IMPLICIT NONE
   USE C_SMA2CL
   USE C_SMA2ET
   USE C_SMA2IO
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
   IF ( Ecpt(7)/=0.0 ) THEN
      Ecpt(7) = Ecpt(7)*2.0
      DO i = 1 , 24
         Ecpt(i+50) = Ecpt(i)
      ENDDO
      DO i = 5 , 19
         Ecpt(i) = Ecpt(i+1)
      ENDDO
      iret = 1
      SPAG_Loop_1_1: DO
!*****
!
!*****
         IF ( (necpt(2)==Npvt) .OR. (necpt(3)==Npvt) .OR. (necpt(4)==Npvt) ) CALL mflud3
         IF ( iret==1 ) THEN
            Ecpt(4) = Ecpt(55)
            Ecpt(17) = Ecpt(72)
            Ecpt(18) = Ecpt(73)
            iret = 2
         ELSEIF ( iret==2 ) THEN
            Ecpt(13) = Ecpt(68)
            Ecpt(14) = Ecpt(69)
            Ecpt(3) = Ecpt(54)
            iret = 3
         ELSEIF ( iret==3 ) THEN
            Ecpt(9) = Ecpt(64)
            Ecpt(10) = Ecpt(65)
            Ecpt(2) = Ecpt(53)
            iret = 4
         ELSE
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
END SUBROUTINE mflud4
