
SUBROUTINE setval
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ksystm(65) , Nbpw , Oscar(1) , P(2,5) , Vps(1)
   COMMON /blank / P
   COMMON /oscent/ Oscar
   COMMON /system/ Ksystm
   COMMON /xvps  / Vps
!
! Local variable declarations
!
   INTEGER andf , rshift
   INTEGER i , j , k , subnam(2)
   EXTERNAL andf , rshift
!
! End of declarations
!
!
   EQUIVALENCE (Ksystm(40),Nbpw)
   DATA subnam/4HSETV , 4HAL  /
!
   j = 12
   DO i = 1 , 5
!
!     CHECK ODD PARAMETERS TO FIND VARIABLE ONES
!
      IF ( andf(rshift(Oscar(j+1),Nbpw-1),1)==0 ) GOTO 100
!
!     PARAMETER IS VARIABLE
!
      k = andf(Oscar(j+1),65535)
      P(1,i) = P(2,i)
      Vps(k) = P(1,i)
      j = j + 2
      IF ( andf(rshift(Oscar(j),Nbpw-1),1)==0 ) j = j + 1
   ENDDO
   GOTO 99999
!
 100  IF ( i<=1 ) CALL mesage(-7,0,subnam)
!
99999 END SUBROUTINE setval
