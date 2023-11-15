
SUBROUTINE skpfil(File,N)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER File , N
!
! End of declarations
!
   IF ( N/=0 ) THEN
      Name = File
      CALL dsgefl
      Irword = N
      IF ( N>0 ) THEN
         IF ( Iprvop/=0 ) CALL dsmsg(4)
         CALL dsskff(N)
      ELSEIF ( (Indclr-Indbas)/=5 ) THEN
         CALL dsskfb(N)
      ELSEIF ( Nblock/=1 ) THEN
         CALL dsskfb(N)
      ENDIF
      CALL dssdcb
   ENDIF
END SUBROUTINE skpfil
