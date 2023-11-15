!
! PARAMETER definitions
!
   INTEGER NUMFCB , NUMSOF
   PARAMETER (NUMFCB=89,NUMSOF=10)
!
! COMMON variable declarations
!
   INTEGER Idslim , Lensof(NUMSOF) , Lginox , Mdsfcb(3,NUMFCB)
   COMMON /ginox / Lginox , Idslim , Mdsfcb , Lensof
!
! End of declarations
!
 
