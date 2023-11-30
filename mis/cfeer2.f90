
SUBROUTINE cfeer2(Iret)
   IMPLICIT NONE
   INTEGER Bbbbar(5) , Dumm(36) , Filea(7) , Filel(7) , Fileu(7) , Incr , Iprec , Irow , Ksystm(65) , Mcblt(7) , Mcbut(7) , Nlast , &
         & Nout , Nz , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Scr9 , Sr1fil , Sr2fil , Sr3fil , Typout
   DOUBLE PRECISION Det(2) , Dz(1) , Mindia
   REAL Dumq(72) , Dumxc(21) , Power , Z(1)
   LOGICAL Qpr
   COMMON /cdcmpx/ Filea , Filel , Fileu , Sr1fil , Sr2fil , Sr3fil , Det , Power , Nz , Mindia , Bbbbar
   COMMON /feeraa/ Dumm , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Scr9 , Dumq , Mcblt , Mcbut
   COMMON /feerxc/ Dumxc , Qpr
   COMMON /system/ Ksystm
   COMMON /unpakx/ Typout , Irow , Nlast , Incr
   COMMON /zzzzzz/ Z
   INTEGER Iret
   INTEGER i , ibuf , ifilxx , itype , j , limit
   INTEGER korsz
!
!     CFEER2 INITIALIZES AND CALLS CDCOMP FOR CFCNTL
!
   !>>>>EQUIVALENCE (Z(1),Dz(1)) , (Nout,Ksystm(2)) , (Iprec,Ksystm(55))
!
   itype = Iprec + 2
   Iret = 0
   Filea(1) = Scr1
   Filel(1) = Scr3
   Fileu(1) = Scr4
   Sr1fil = Scr5
   Sr2fil = Scr6
   Sr3fil = Scr7
   Filea(2) = Dumm(3)
   Filea(3) = Dumm(3)
   Filea(4) = Dumm(4)
   Filea(5) = itype
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = itype
   Nz = korsz(Z)
   Bbbbar(1) = 0
   CALL cdcomp(*200,Z,Z,Z)
!
!     ---------- SPECIAL PRINT -------------------------------
!
   IF ( Qpr ) THEN
      WRITE (Nout,99001)
99001 FORMAT (//,7H CFEER2,//)
      WRITE (Nout,99005)
      Typout = itype
      Irow = 1
      Nlast = Dumm(2)
      limit = 2*Nlast
      Incr = 1
      ibuf = Nz - Ksystm(1) - 2
      ifilxx = Scr3
      DO
         CALL gopen(ifilxx,Z(ibuf),0)
         DO i = 1 , Nlast
            WRITE (Nout,99002) i
99002       FORMAT (1H ,6HCOLUMN,I4)
            CALL unpack(*20,ifilxx,Z)
            IF ( Iprec==2 ) WRITE (Nout,99003) (Dz(j),j=1,limit)
99003       FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
            IF ( Iprec/=2 ) WRITE (Nout,99004) (Z(j),j=1,limit)
99004       FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
 20      ENDDO
         CALL close(ifilxx,1)
         WRITE (Nout,99005)
         IF ( ifilxx==Scr4 ) EXIT
         ifilxx = Scr4
      ENDDO
   ENDIF
!
!     --------------------------------------------------------
!
 100  DO i = 1 , 7
      Mcbut(i) = Fileu(i)
      Mcblt(i) = Filel(i)
   ENDDO
   RETURN
!
 200  Iret = 1
   GOTO 100
99005 FORMAT (1H ,13(10H----------))
END SUBROUTINE cfeer2