
SUBROUTINE cfbsor(Ll,Ul,Bx,Xx,Iopt)
   IMPLICIT NONE
   INTEGER Idum(54) , Iprec , Iz(1) , Jfb(7) , Jfl(7) , Jfu(7) , Jfx(7) , Jprec , Jsign , Jx , Mfb(7) , Mfl(7) , Mflt(7) , Mfx(7) , &
         & Mprec , Msign , Mx
   COMMON /fbsx  / Mfl , Mflt , Mfb , Mfx , Mx , Mprec , Msign
   COMMON /gfbsx / Jfl , Jfu , Jfb , Jfx , Jx , Jprec , Jsign
   COMMON /system/ Idum , Iprec
   COMMON /zzzzzz/ Iz
   INTEGER Bx , Iopt , Ll , Ul , Xx
   INTEGER i , mcb(7) , nz
   INTEGER korsz
!
!
   nz = korsz(Iz)
   mcb(1) = iabs(Bx)
   CALL rdtrl(mcb)
   IF ( Iopt==1 ) THEN
!
!     UNSYMETRIC FBS
!
      Jfl(1) = Ll
      CALL rdtrl(Jfl)
      Jfu(1) = Ul
      CALL rdtrl(Jfu)
      DO i = 1 , 7
         Jfb(i) = mcb(i)
         Jfx(i) = mcb(i)
      ENDDO
      Jfx(1) = Xx
      Jx = nz
      Jprec = Iprec
      Jfx(5) = max0(Jfl(5),Jfb(5))
      Jsign = +1
      IF ( Bx<0 ) Jsign = -1
      CALL gfbs(Iz,Iz)
      CALL wrttrl(Jfx)
   ELSE
!
!     SYMETRIC FBS
!
      Mfl(1) = Ll
      CALL rdtrl(Mfl)
      DO i = 1 , 7
         Mfb(i) = mcb(i)
         Mfx(i) = mcb(i)
      ENDDO
      Mfx(1) = Xx
      Mx = nz
      Mfx(5) = max0(Mfl(5),Mfb(5))
      Mprec = Iprec
      Msign = +1
      IF ( Bx<0 ) Msign = -1
      CALL fbs(Iz,Iz)
      CALL wrttrl(Mfx)
   ENDIF
   RETURN
END SUBROUTINE cfbsor