!*==dadd5.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dadd5
   USE c_blank
   USE c_saddx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: amcbs
   INTEGER :: i , j , k
   INTEGER , DIMENSION(5) , SAVE :: inx
   INTEGER , SAVE :: iout
   INTEGER , DIMENSION(5) :: mc
   EXTERNAL korsz , mesage , rdtrl , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     DMAP DRIVER FOR SADD (MATRIX ADD) ROUTINE
!     THE DMAP CALL FOR THIS MODULE IS
!     ADD5 A,B,C,D,E / X / V,N,P1 / V,N,P2 / V,N,P3 / V,N,P4 / V,N,P5 $
!     THE PARAMETERS ARE ALL COMPLEX SINGLE-PRECISION.
!
   !>>>>EQUIVALENCE (Mcbs(1),Amcbs(1)) , (Mcbs(61),Mc(1))
   DATA inx/101 , 102 , 103 , 104 , 105/ , iout/201/
!
   lcore = korsz(core)
!
   DO i = 1 , 67
      mcbs(i) = 0
   ENDDO
!
!     SETUP MATRIX CONTROL BLOCKS OF THE INPUT MATRICES
!
   i = 1
   k = 0
!
   mc(5) = 1
   DO j = 1 , 5
      mcbs(i) = inx(j)
      CALL rdtrl(mcbs(i))
!
!     EXCLUDE NULL MATRICES FROM MCBS ARRAY
!
      IF ( mcbs(i)>0 ) THEN
!
!     MOVE MULTIPLIERS TO MCBS ARRAY
!
         mcbs(i+7) = 1
         amcbs(i+8) = alpha(2*j-1)
         amcbs(i+9) = alpha(2*j)
         IF ( amcbs(i+9)/=0.0 ) mcbs(i+7) = 3
!
!     DETERMINE THE PRECISION AND TYPE OF THE OUTPUT MATRIX
!
         mc(5) = max0(mc(5),mcbs(i+4),mcbs(i+7))
         IF ( mcbs(i+4)==2 ) k = 1
         i = i + 12
      ENDIF
   ENDDO
!
   mc(1) = iout
   nomat = i/12
   IF ( nomat==0 ) RETURN
   IF ( nomat/=1 ) THEN
!
!     CHECK TO ENSURE THAT THE MATRICES BEING ADDED ARE OF THE SAME
!     ORDER
!
      i = 14
      DO j = 2 , nomat
         IF ( mcbs(2)/=mcbs(i) .OR. mcbs(3)/=mcbs(i+1) ) THEN
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 4149, ATTEMPT TO ADD MATRICES OF UNEQUAL ORDER IN ','MODULE ADD5.')
            CALL mesage(-61,0,0)
         ENDIF
         i = i + 12
      ENDDO
   ENDIF
   mc(2) = mcbs(2)
   mc(3) = mcbs(3)
   mc(4) = mcbs(4)
   IF ( mc(5)==3 .AND. k/=0 ) mc(5) = 4
   mc(5) = min0(4,mc(5))
!
!     ADD MATRICES
!
   CALL sadd(core,core)
   CALL wrttrl(mc(1))
!
END SUBROUTINE dadd5
