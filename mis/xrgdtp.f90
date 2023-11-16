
SUBROUTINE xrgdtp
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ichar(80) , Icol , Icount , Idmap , Ierror , Ignore , Ind , Iphase , Irestr , Iscr , Istate , Itype , Limit(2) ,         &
         & Member(2) , Name(2) , Nsubst , Num(2) , Number , Nument , Record(20)
   COMMON /xrgdxx/ Irestr , Nsubst , Iphase , Icol , Number , Itype , Istate , Ierror , Num , Ind , Nument , Record , Ichar ,       &
                 & Limit , Icount , Idmap , Iscr , Name , Member , Ignore
!
! Local variable declarations
!
   INTEGER delim(3) , k , nums(10)
!
! End of declarations
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
         GOTO 99999
      ENDIF
   ENDDO
   DO k = 1 , 10
      IF ( Ichar(Icol)==nums(k) ) THEN
         Itype = 1
         Number = mod(k,10)
         GOTO 99999
      ENDIF
   ENDDO
   Itype = 5
99999 RETURN
END SUBROUTINE xrgdtp
