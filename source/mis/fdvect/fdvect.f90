!*==fdvect.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fdvect(Delta,Pk)
   USE c_packx
   USE c_regean
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Delta
   REAL(REAL64) :: Pk
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore
   REAL(REAL64) :: dmax
   INTEGER :: i , icnt , im1 , ipm1 , iprec , kprec , lcore , npro2 , nprob , sysbuf
   INTEGER , DIMENSION(1) :: icore
   INTEGER , DIMENSION(7) :: mcb
   REAL :: x , xmax , y
   EXTERNAL close , detfbs , gopen , korsz , pack , rdtrl , skprec , write
!
! End of declarations rewritten by SPAG
!
!
!    1,                NAME(2)
   !>>>>EQUIVALENCE (Icore(1),Core(1),Dcore(1)) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
!     DATA    NAME  /  4HFDVE,4HCT  /
!
   nprob = ia(3)
   kprec = ia(5)
   IF ( kprec/=1 .AND. kprec/=2 ) kprec = iprec
   npro2 = nprob
   icnt = ivect(2)
   im1 = 1
   lcore = (korsz(core)/2)*2 - lc1 - sysbuf
   x = nprob
   y = icount
   mcb(1) = ib(5)
   CALL rdtrl(mcb(1))
!
   CALL detfbs(npro2+1,icore(lcore+1),mcb,nprob,icount)
!
!     COPY FX ONTO IVECT + NORMALIZE
!
   ipm1 = ivect(1)
   IF ( icnt/=0 ) THEN
      CALL gopen(ivect(1),icore(lcore+1),0)
      CALL skprec(ivect(1),icnt)
      CALL close(ivect(1),2)
      im1 = 3
   ENDIF
   CALL gopen(ivect,icore(lcore+1),im1)
   lcore = lcore - sysbuf
   IF ( kprec==2 ) THEN
      dmax = 0.0D0
      DO i = 1 , nprob
         IF ( dabs(dcore(i))>dmax ) dmax = dabs(dcore(i))
      ENDDO
      DO i = 1 , nprob
         dcore(i) = dcore(i)/dmax
      ENDDO
   ELSE
      xmax = 0.0
      DO i = 1 , nprob
         xmax = amax1(xmax,abs(core(i)))
      ENDDO
      DO i = 1 , nprob
         core(i) = core(i)/xmax
      ENDDO
   ENDIF
   it1p = kprec
   it2p = iprec
   iip = 1
   jjp = nprob
   incrp = 1
   CALL pack(core,ivect,ivect)
   CALL close(ivect(1),1)
   ipm1 = lama
   CALL gopen(lama,icore(lcore+1),3)
   dcore(1) = Pk
   CALL write(lama,core,iprec,1)
   CALL close(lama,2)
END SUBROUTINE fdvect
