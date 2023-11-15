
SUBROUTINE dist(Ideg,Hist,Median,Modd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Isys , Mm , Nn , Nout
   COMMON /bands / Nn , Mm
   COMMON /system/ Isys , Nout
!
! Dummy argument declarations
!
   INTEGER Median , Modd
   INTEGER Hist(1) , Ideg(1)
!
! Local variable declarations
!
   INTEGER i , k , max , mm1 , nn2
!
! End of declarations
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
   mm1 = Mm + 1
   DO i = 1 , mm1
      Hist(i) = 0
   ENDDO
   DO i = 1 , Nn
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
   nn2 = Nn/2
   DO i = 1 , mm1
      IF ( Hist(i)>nn2 ) EXIT
   ENDDO
   Median = i - 1
END SUBROUTINE dist
