!*==detm5.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm5
USE C_CONDAS
USE C_DETMX
USE C_REGEAN
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(5) :: core
   REAL(REAL64) , DIMENSION(1) :: det , ps
   INTEGER :: i , n2ev2 , nnd , nni , nnp , nz
   INTEGER , DIMENSION(8) :: ipdet
   REAL :: tphi
   EXTERNAL close , gopen , korsz , write
!
! End of declarations rewritten by SPAG
!
!
!     WRITES EIGENVALUE SUMMARY FOR DETERMINANT METHOD
!
!
!
   !>>>>EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1),Core(1))
   !>>>>EQUIVALENCE (Consts(2),Tphi)
!
! ----------------------------------------------------------------------
!
   nz = korsz(Psave) - Lcore - Sysbuf
   CALL gopen(Ipout,ipdet(nz+1),1)
   ipdet(1) = 1
   ipdet(2) = Nfound
   IF ( Mz>0 ) ipdet(2) = ipdet(2) + Mz
   ipdet(3) = Nstart
   ipdet(4) = Ic
   ipdet(5) = Nsmove
   ipdet(6) = Ndcmp
   ipdet(7) = Nfail
   ipdet(8) = Iterm
   DO i = 9 , 12
      ipdet(i) = 0
   ENDDO
   CALL write(Ipout,ipdet(1),12,0)
   IF ( Ndcmp/=0 ) THEN
      n2ev2 = Iadd + Nd
      DO i = 1 , n2ev2
         nnd = i + Idet
         nnp = i + Ips
         nni = i + Ipdeta
!
!     PUT UUT STRRTING POINT SUMMARY
!
         ipdet(1) = i
         core(2) = Psave(nnp)
         core(3) = sqrt(abs(core(2)))
         core(4) = core(3)/tphi
         core(5) = Psave(nnd)
         ipdet(6) = ipdet(nni)
!
!     SCALE DETERMINANTE FOR PRETTY PRINT
!
         IF ( core(5)/=0.0 ) THEN
            DO WHILE ( abs(core(5))>=10.0 )
               core(5) = core(5)*0.1
               ipdet(6) = ipdet(6) + 1
            ENDDO
            DO WHILE ( abs(core(5))<1.0 )
               core(5) = core(5)*10.0
               ipdet(6) = ipdet(6) - 1
            ENDDO
         ENDIF
         CALL write(Ipout,core(1),6,0)
      ENDDO
   ENDIF
   CALL write(Ipout,core(1),0,1)
   CALL close(Ipout,1)
END SUBROUTINE detm5
