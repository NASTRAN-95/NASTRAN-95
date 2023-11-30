
SUBROUTINE ctrnsp(Ix,X,Nx,Filea,B,Sr1fil)
   IMPLICIT NONE
   INTEGER Cdp , Eol , Eor , Ia(4) , Ihalf , Ii , Jprec(2) , Mach , Norew , Nwds(4) , Rdp , Sysbuf
   REAL Csp , Eofnrw , Rd , Rdrew , Rew , Rsp , Wrt , Wrtrew
   COMMON /machin/ Mach , Ihalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /system/ Sysbuf
   COMMON /type  / Jprec , Nwds
   COMMON /zntpkx/ Ia , Ii , Eol , Eor
   INTEGER B , Nx , Sr1fil
   INTEGER Filea(7) , Ix(1)
   REAL X(1)
   INTEGER complf , lshift , orf , rshift
   DOUBLE PRECISION di(2)
   INTEGER i , ifile , iii(6) , incr , iobuf , iprec , istor , j , k , kk , l , name(2) , ncol , no , num , typea
   EXTERNAL complf , lshift , orf , rshift
!
!     TRANS WILL DO AN INCORE TRANSPOSE OF THE UPPER TRIANGLE OF ACTIVE
!     ELEMENTS
!
   !>>>>EQUIVALENCE (iii(3),di(1))
   DATA name/4HCTRN , 4HSP  /
!
!
   num = rshift(complf(0),1)
   iobuf = Nx - 4*Sysbuf
   ifile = Filea(1)
!
!     POSITION INPUT FILE AT START OF THE UPPER TRIANGLE
!
   CALL skprec(Filea(1),B+1)
   typea = Filea(5)
   ncol = Filea(2)
   no = 0
   istor = 1
   iprec = Jprec(typea)
   incr = Nwds(typea) + 1
   k = 1
 100  CALL intpk(*200,Filea(1),0,typea,0)
   DO
      CALL zntpki
      IF ( Ii>k ) THEN
         IF ( Eor==0 ) CALL skprec(Filea(1),1)
         EXIT
      ELSE
!
!     PACK I AND J IN ONE WORD AND STORE IT AND THE NONZERO VALUE
!     IN CORE
!
         l = orf(lshift(Ii,Ihalf),k+B)
         no = no + 1
         Ix(istor) = l
         Ix(istor+1) = Ia(1)
         Ix(istor+2) = Ia(2)
         Ix(istor+3) = Ia(3)
         Ix(istor+4) = Ia(4)
         istor = istor + incr
         IF ( istor+incr>iobuf ) THEN
            no = -8
            CALL mesage(no,ifile,name)
            GOTO 99999
         ELSEIF ( Eol/=0 ) THEN
            EXIT
         ENDIF
      ENDIF
   ENDDO
 200  k = k + 1
   IF ( k+B<=ncol ) GOTO 100
   CALL rewind(Filea(1))
!
!     ALL ELEMENTS ARE IN CORE.  WRITE THEM OUT IN THE TRANSPOSED ORDER
!
   ifile = Sr1fil
   CALL open(*300,Sr1fil,Ix(iobuf),Wrtrew)
   istor = istor - incr
   DO i = 1 , no
      k = num
      DO j = 1 , istor , incr
         IF ( Ix(j)<=k ) THEN
            kk = j
            k = Ix(j)
         ENDIF
      ENDDO
!
!     UNPACK I AND J, AND WRITE OUT I,J,AND A(I,J)
!
      iii(1) = rshift(k,Ihalf)
      iii(2) = k - lshift(iii(1),Ihalf)
      Ix(kk) = num
      IF ( iprec==2 ) THEN
         iii(3) = Ix(kk+1)
         iii(4) = Ix(kk+2)
         iii(5) = 0
         iii(6) = 0
         IF ( typea>2 ) THEN
            iii(5) = Ix(kk+3)
            iii(6) = Ix(kk+4)
         ENDIF
      ELSE
         di(1) = X(kk+1)
         di(2) = 0.D0
         IF ( typea>2 ) di(2) = X(kk+2)
      ENDIF
      CALL write(Sr1fil,iii(1),6,0)
      IF ( kk==istor ) istor = istor - incr
   ENDDO
!
!     WRITE A TRAILER RECORD ON THE FILE
!
   iii(1) = -1
   CALL write(Sr1fil,iii(1),6,0)
   CALL close(Sr1fil,Rew)
   RETURN
!
 300  no = -1
   CALL mesage(no,ifile,name)
99999 RETURN
END SUBROUTINE ctrnsp