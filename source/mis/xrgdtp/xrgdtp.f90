!*==xrgdtp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdtp
   IMPLICIT NONE
   USE C_XRGDXX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) , SAVE :: delim
   INTEGER :: k
   INTEGER , DIMENSION(10) , SAVE :: nums
!
! End of declarations rewritten by SPAG
!
!****
!    PURPOSE - XRGDTP DETERMINES A TYPE CODE FOR A CHARACTER
!
!    AUTHOR  - RPK CORPORATION; DECEMBER, 1983
!
!    INPUT
!      /XRGDXX/
!        ICHAR       AN ARRAY IN 80A1 FORMAT
!        ICOL        CURRENT ELEMENT IN THE ARRAY ICHAR
!
!    OUTPUT
!      /XRGDXX/
!        ITYPE       TYPE CODE ASSOCIATED WITH THE CHARACTER
!                    =1, IF CHARACTER IS A NUMBER
!                    =2, IF CHARACTER IS A ','
!                    =3, IF CHARACTER IS A '-'
!                    =4, IF CHARACTER IS A BLANK
!                    =5, OTHERWISE
!        NUMBER      INTEGER VALUE FOR CHARACTER OF ITYPE=1
!
!    LOCAL VARIABLES
!      DELIM         3 WORD ARRAY CONTAINING A COMMA, DASH, AND BLANK
!      NUMS          10 WORD ARRAY OF ALPHA NUMBERS 1,2..0
!      K             K DO LOOP INDEX TO SEARCH DELIM ARRAY
!
!   SUBROUTINES CALLED - NONE
!
!   CALLING SUBROUTINES - XRGDEV
!
!    FUNCTIONS - XRGDTP EXAMINES THE CHARACTER IN ICHAR(ICOL)
!                TO DETERMINE ITS TYPE CODE.
!
!    ERRORS - NONE
!
!****
   DATA nums/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA delim/1H, , 1H- , 1H /
!
   DO k = 1 , 3
      IF ( Ichar(Icol)==delim(k) ) THEN
         Itype = k + 1
         RETURN
      ENDIF
   ENDDO
   DO k = 1 , 10
      IF ( Ichar(Icol)==nums(k) ) THEN
         Itype = 1
         Number = mod(k,10)
         RETURN
      ENDIF
   ENDDO
   Itype = 5
END SUBROUTINE xrgdtp
