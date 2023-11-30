
SUBROUTINE fdvect(Delta,Pk)
   IMPLICIT NONE
   REAL Core(1)
   DOUBLE PRECISION Dcore(1)
   INTEGER Ia(14) , Ib(5) , Ib1(9) , Ibuck , Icore(1) , Icount , Iip , Incrp , Iprec , It1p , It2p , Ivect(7) , Jjp , Ksystm(65) ,  &
         & Lama , Lc1 , Loads , Lx , Nsym , Sysbuf
   COMMON /packx / It1p , It2p , Iip , Jjp , Incrp
   COMMON /regean/ Ia , Ivect , Ib , Lc1 , Ib1 , Loads , Lx , Icount , Lama , Ibuck , Nsym
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Core
   REAL Delta
   DOUBLE PRECISION Pk
   DOUBLE PRECISION dmax
   INTEGER i , icnt , im1 , ipm1 , kprec , lcore , mcb(7) , npro2 , nprob
   INTEGER korsz
   REAL x , xmax , y
!
!    1,                NAME(2)
   EQUIVALENCE (Icore(1),Core(1),Dcore(1)) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
!     DATA    NAME  /  4HFDVE,4HCT  /
!
   nprob = Ia(3)
   kprec = Ia(5)
   IF ( kprec/=1 .AND. kprec/=2 ) kprec = Iprec
   npro2 = nprob
   icnt = Ivect(2)
   im1 = 1
   lcore = (korsz(Core)/2)*2 - Lc1 - Sysbuf
   x = nprob
   y = Icount
   mcb(1) = Ib(5)
   CALL rdtrl(mcb(1))
!
   CALL detfbs(npro2+1,Icore(lcore+1),mcb,nprob,Icount)
!
!     COPY FX ONTO IVECT + NORMALIZE
!
   ipm1 = Ivect(1)
   IF ( icnt/=0 ) THEN
      CALL gopen(Ivect(1),Icore(lcore+1),0)
      CALL skprec(Ivect(1),icnt)
      CALL close(Ivect(1),2)
      im1 = 3
   ENDIF
   CALL gopen(Ivect,Icore(lcore+1),im1)
   lcore = lcore - Sysbuf
   IF ( kprec==2 ) THEN
      dmax = 0.0D0
      DO i = 1 , nprob
         IF ( dabs(Dcore(i))>dmax ) dmax = dabs(Dcore(i))
      ENDDO
      DO i = 1 , nprob
         Dcore(i) = Dcore(i)/dmax
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
   It2p = Iprec
   Iip = 1
   Jjp = nprob
   Incrp = 1
   CALL pack(Core,Ivect,Ivect)
   CALL close(Ivect(1),1)
   ipm1 = Lama
   CALL gopen(Lama,Icore(lcore+1),3)
   Dcore(1) = Pk
   CALL write(Lama,Core,Iprec,1)
   CALL close(Lama,2)
END SUBROUTINE fdvect
