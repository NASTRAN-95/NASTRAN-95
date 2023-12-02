!*==detm5.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm5
   USE c_condas
   USE c_detmx
   USE c_regean
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
   nz = korsz(psave) - lcore - sysbuf
   CALL gopen(ipout,ipdet(nz+1),1)
   ipdet(1) = 1
   ipdet(2) = nfound
   IF ( mz>0 ) ipdet(2) = ipdet(2) + mz
   ipdet(3) = nstart
   ipdet(4) = ic
   ipdet(5) = nsmove
   ipdet(6) = ndcmp
   ipdet(7) = nfail
   ipdet(8) = iterm
   DO i = 9 , 12
      ipdet(i) = 0
   ENDDO
   CALL write(ipout,ipdet(1),12,0)
   IF ( ndcmp/=0 ) THEN
      n2ev2 = iadd + nd
      DO i = 1 , n2ev2
         nnd = i + idet
         nnp = i + ips
         nni = i + ipdeta
!
!     PUT UUT STRRTING POINT SUMMARY
!
         ipdet(1) = i
         core(2) = psave(nnp)
         core(3) = sqrt(abs(core(2)))
         core(4) = core(3)/tphi
         core(5) = psave(nnd)
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
         CALL write(ipout,core(1),6,0)
      ENDDO
   ENDIF
   CALL write(ipout,core(1),0,1)
   CALL close(ipout,1)
END SUBROUTINE detm5
