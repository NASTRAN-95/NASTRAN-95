
SUBROUTINE solve1(A1,R1,Rp,Xi,Lam2,Lam3,Lam4,Cont)
   IMPLICIT NONE
   REAL A1 , Cont , Lam2 , Lam3 , Lam4 , R1 , Rp , Xi
   REAL bb , cosa , psi1 , psi2 , rt , sina , sinsum , sum
!
!     ROUTINE TO SOLVE FOR LAMBDAS AS FNCTS. OF XI
!
!
!
   IF ( Rp==0.0 ) THEN
!
!     ALF1 = ALF2
!
      sina = sin(A1)
      cosa = cos(A1)
      bb = R1 + Xi*cosa
      rt = 0.0E0
      IF ( sina/=0.0E0 ) rt = bb/sina
      psi1 = cosa
      psi2 = 0.0
   ELSE
!
      sum = A1 + Xi/Rp
      sinsum = sin(sum)
      bb = R1 - Rp*(sin(A1)-sinsum)
      rt = 0.0E0
      IF ( sinsum/=0.0E0 ) rt = bb/sinsum
      psi1 = cos(sum)
      psi2 = -sinsum/Rp
!
!     CHECK FOR SHELL CAP CASE
      IF ( A1==0.0 ) THEN
         Lam2 = 0.0E0
         IF ( bb/=0.0E0 ) Lam2 = psi1/bb
         Lam3 = 1.0/Rp
         Lam4 = -1.0/Rp**2
         GOTO 99999
      ENDIF
   ENDIF
!
   Lam2 = 0.0E0
   IF ( bb/=0.0E0 ) Lam2 = psi1/bb
   Lam3 = 0.0E0
   IF ( rt/=0.0E0 ) Lam3 = 1.0E0/rt
   Lam4 = 0.0E0
   IF ( bb/=0.0E0 ) Lam4 = psi2/bb
!
99999 RETURN
END SUBROUTINE solve1
