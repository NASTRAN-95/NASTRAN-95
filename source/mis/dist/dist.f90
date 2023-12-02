!*==dist.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dist(Ideg,Hist,Median,Modd)
   USE c_bands
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Hist
   INTEGER :: Median
   INTEGER :: Modd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , k , max , mm1 , nn2
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTE THE DISTRIBUTION OF NODAL DEGREES WITH MEDIAN AND MODE
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!
!     IDEG(I) = DEGREE OF NODE I
!     HIST(I) = NUMBER OF NODES OF DEGREE I
!
!     COMPUTE HISTOGRAM.
!
   mm1 = mm + 1
   DO i = 1 , mm1
      Hist(i) = 0
   ENDDO
   DO i = 1 , nn
      k = Ideg(i) + 1
      Hist(k) = Hist(k) + 1
   ENDDO
!
!     COMPUTE MODE (MODD).
!
   Modd = 0
   max = 0
   DO i = 1 , mm1
      k = Hist(i)
      IF ( k>max ) THEN
         max = k
         Modd = i - 1
      ENDIF
   ENDDO
!
!     COMPUTE CUMULATIVE DISTRIBUTION, AND MEDIAN.
!
   DO i = 2 , mm1
      Hist(i) = Hist(i) + Hist(i-1)
   ENDDO
   nn2 = nn/2
   SPAG_Loop_1_1: DO i = 1 , mm1
      IF ( Hist(i)>nn2 ) EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   Median = i - 1
END SUBROUTINE dist
