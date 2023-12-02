!*==fdvect.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fdvect(Delta,Pk)
USE C_PACKX
USE C_REGEAN
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
   nprob = Ia(3)
   kprec = Ia(5)
   IF ( kprec/=1 .AND. kprec/=2 ) kprec = iprec
   npro2 = nprob
   icnt = Ivect(2)
   im1 = 1
   lcore = (korsz(Core)/2)*2 - Lc1 - sysbuf
   x = nprob
   y = Icount
   mcb(1) = Ib(5)
   CALL rdtrl(mcb(1))
!
   CALL detfbs(npro2+1,icore(lcore+1),mcb,nprob,Icount)
!
!     COPY FX ONTO IVECT + NORMALIZE
!
   ipm1 = Ivect(1)
   IF ( icnt/=0 ) THEN
      CALL gopen(Ivect(1),icore(lcore+1),0)
      CALL skprec(Ivect(1),icnt)
      CALL close(Ivect(1),2)
      im1 = 3
   ENDIF
   CALL gopen(Ivect,icore(lcore+1),im1)
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
         xmax = amax1(xmax,abs(Core(i)))
      ENDDO
      DO i = 1 , nprob
         Core(i) = Core(i)/xmax
      ENDDO
   ENDIF
   It1p = kprec
   It2p = iprec
   Iip = 1
   Jjp = nprob
   Incrp = 1
   CALL pack(Core,Ivect,Ivect)
   CALL close(Ivect(1),1)
   ipm1 = Lama
   CALL gopen(Lama,icore(lcore+1),3)
   dcore(1) = Pk
   CALL write(Lama,Core,iprec,1)
   CALL close(Lama,2)
END SUBROUTINE fdvect
