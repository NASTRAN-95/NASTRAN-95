!*==transp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE transp(Ix,X,Nx,Filea,B,Sr1fil)
   IMPLICIT NONE
   USE c_machin
   USE c_names
   USE c_system
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ix
   REAL , DIMENSION(1) :: X
   INTEGER :: Nx
   INTEGER , DIMENSION(7) :: Filea
   INTEGER :: B
   INTEGER :: Sr1fil
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: di
   INTEGER :: i , ifile , incr , iobuf , istor , j , k , kk , l , n , ncol , no , num , typea
   INTEGER , DIMENSION(4) :: iii
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     TRANSP WILL DO AN INCORE TRANSPOSE OF THE UPPER TRIANGLE OF
!     ACTIVE ELEMENTS
!     (OUT-OF-CORE TRANSPOSE IS DONE BY TRNSP)
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   !>>>>EQUIVALENCE (iii(3),di)
   DATA name/4HTRAN , 4HSP  /
!
!
   num = rshift(complf(0),1)
   iobuf = Nx - 4*sysbuf
   ifile = Filea(1)
!
!     POSITION INPUT FILE AT START OF THE UPPER TRIANGLE
!
   n = B + 1
   CALL skprec(Filea,n)
   typea = Filea(5)
   ncol = Filea(2)
   no = 0
   istor = 1
   k = 1
 100  CALL intpk(*200,Filea(1),0,typea,0)
   DO
      CALL zntpki
      IF ( ii>k ) THEN
         IF ( eor==0 ) CALL skprec(Filea,1)
         EXIT
      ELSE
!
!     PACK I AND J IN ONE WORD AND STORE IT AND THE NONZERO VALUE
!     IN CORE
!
         l = orf(lshift(ii,ihalf),k+B)
         no = no + 1
         Ix(istor) = l
         Ix(istor+1) = ia(1)
         istor = istor + 2
         IF ( typea==rdp ) THEN
            Ix(istor) = ia(2)
            istor = istor + 1
         ENDIF
         IF ( istor+3>iobuf ) THEN
            no = -8
            CALL mesage(no,ifile,name)
            GOTO 99999
         ELSEIF ( eol/=0 ) THEN
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
   CALL open(*300,Sr1fil,Ix(iobuf),wrtrew)
   incr = typea + 1
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
      iii(1) = rshift(k,ihalf)
      iii(2) = k - lshift(iii(1),ihalf)
      Ix(kk) = num
      IF ( incr==3 ) THEN
         iii(3) = Ix(kk+1)
         iii(4) = Ix(kk+2)
      ELSE
         di = X(kk+1)
      ENDIF
      CALL write(Sr1fil,iii(1),4,0)
      IF ( kk==istor ) istor = istor - incr
   ENDDO
!
!     WRITE A TRAILER RECORD ON THE FILE
!     NOTE - FORMAL GINO FILE TRAILER IS NOT GENERATED HERE
!
   iii(1) = -1
   CALL write(Sr1fil,iii(1),4,0)
   CALL close(Sr1fil,rew)
   RETURN
!
 300  no = -1
   CALL mesage(no,ifile,name)
99999 END SUBROUTINE transp
