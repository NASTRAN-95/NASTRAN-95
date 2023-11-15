
SUBROUTINE dadd5
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alpha(10) , Amcbs(1) , Core(1)
   INTEGER Ibuf , Lcore , Mc(5) , Mcbs(67) , Nomat , Nout
   CHARACTER*23 Ufm
   COMMON /blank / Alpha
   COMMON /saddx / Nomat , Lcore , Mcbs
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER i , inx(5) , iout , j , k
   INTEGER korsz
!
! End of declarations
!
!
!     DMAP DRIVER FOR SADD (MATRIX ADD) ROUTINE
!     THE DMAP CALL FOR THIS MODULE IS
!     ADD5 A,B,C,D,E / X / V,N,P1 / V,N,P2 / V,N,P3 / V,N,P4 / V,N,P5 $
!     THE PARAMETERS ARE ALL COMPLEX SINGLE-PRECISION.
!
   EQUIVALENCE (Mcbs(1),Amcbs(1)) , (Mcbs(61),Mc(1))
   DATA inx/101 , 102 , 103 , 104 , 105/ , iout/201/
!
   Lcore = korsz(Core)
!
   DO i = 1 , 67
      Mcbs(i) = 0
   ENDDO
!
!     SETUP MATRIX CONTROL BLOCKS OF THE INPUT MATRICES
!
   i = 1
   k = 0
!
   Mc(5) = 1
   DO j = 1 , 5
      Mcbs(i) = inx(j)
      CALL rdtrl(Mcbs(i))
!
!     EXCLUDE NULL MATRICES FROM MCBS ARRAY
!
      IF ( Mcbs(i)>0 ) THEN
!
!     MOVE MULTIPLIERS TO MCBS ARRAY
!
         Mcbs(i+7) = 1
         Amcbs(i+8) = Alpha(2*j-1)
         Amcbs(i+9) = Alpha(2*j)
         IF ( Amcbs(i+9)/=0.0 ) Mcbs(i+7) = 3
!
!     DETERMINE THE PRECISION AND TYPE OF THE OUTPUT MATRIX
!
         Mc(5) = max0(Mc(5),Mcbs(i+4),Mcbs(i+7))
         IF ( Mcbs(i+4)==2 ) k = 1
         i = i + 12
      ENDIF
   ENDDO
!
   Mc(1) = iout
   Nomat = i/12
   IF ( Nomat==0 ) RETURN
   IF ( Nomat/=1 ) THEN
!
!     CHECK TO ENSURE THAT THE MATRICES BEING ADDED ARE OF THE SAME
!     ORDER
!
      i = 14
      DO j = 2 , Nomat
         IF ( Mcbs(2)/=Mcbs(i) .OR. Mcbs(3)/=Mcbs(i+1) ) THEN
            WRITE (Nout,99001) Ufm
99001       FORMAT (A23,' 4149, ATTEMPT TO ADD MATRICES OF UNEQUAL ORDER IN ','MODULE ADD5.')
            CALL mesage(-61,0,0)
         ENDIF
         i = i + 12
      ENDDO
   ENDIF
   Mc(2) = Mcbs(2)
   Mc(3) = Mcbs(3)
   Mc(4) = Mcbs(4)
   IF ( Mc(5)==3 .AND. k/=0 ) Mc(5) = 4
   Mc(5) = min0(4,Mc(5))
!
!     ADD MATRICES
!
   CALL sadd(Core,Core)
   CALL wrttrl(Mc(1))
!
END SUBROUTINE dadd5
