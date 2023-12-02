!*==ctrnsp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ctrnsp(Ix,X,Nx,Filea,B,Sr1fil)
   USE c_machin
   USE c_names
   USE c_system
   USE c_type
   USE c_zntpkx
   USE iso_fortran_env
   IMPLICIT NONE
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
   REAL(REAL64) , DIMENSION(2) :: di
   INTEGER :: i , ifile , incr , iobuf , iprec , istor , j , k , kk , l , ncol , no , num , typea
   INTEGER , DIMENSION(6) :: iii
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , complf , intpk , lshift , mesage , open , orf , rewind , rshift , skprec , write , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     TRANS WILL DO AN INCORE TRANSPOSE OF THE UPPER TRIANGLE OF ACTIVE
!     ELEMENTS
!
   !>>>>EQUIVALENCE (iii(3),di(1))
   DATA name/4HCTRN , 4HSP  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         num = rshift(complf(0),1)
         iobuf = Nx - 4*sysbuf
         ifile = Filea(1)
!
!     POSITION INPUT FILE AT START OF THE UPPER TRIANGLE
!
         CALL skprec(Filea(1),B+1)
         typea = Filea(5)
         ncol = Filea(2)
         no = 0
         istor = 1
         iprec = jprec(typea)
         incr = nwds(typea) + 1
         k = 1
         spag_nextblock_1 = 2
      CASE (2)
         CALL intpk(*20,Filea(1),0,typea,0)
         SPAG_Loop_1_1: DO
            CALL zntpki
            IF ( ii>k ) THEN
               IF ( eor==0 ) CALL skprec(Filea(1),1)
               EXIT SPAG_Loop_1_1
            ELSE
!
!     PACK I AND J IN ONE WORD AND STORE IT AND THE NONZERO VALUE
!     IN CORE
!
               l = orf(lshift(ii,ihalf),k+B)
               no = no + 1
               Ix(istor) = l
               Ix(istor+1) = ia(1)
               Ix(istor+2) = ia(2)
               Ix(istor+3) = ia(3)
               Ix(istor+4) = ia(4)
               istor = istor + incr
               IF ( istor+incr>iobuf ) THEN
                  no = -8
                  CALL mesage(no,ifile,name)
                  RETURN
               ELSEIF ( eol/=0 ) THEN
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
 20      k = k + 1
         IF ( k+B<=ncol ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL rewind(Filea(1))
!
!     ALL ELEMENTS ARE IN CORE.  WRITE THEM OUT IN THE TRANSPOSED ORDER
!
         ifile = Sr1fil
         CALL open(*40,Sr1fil,Ix(iobuf),wrtrew)
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
         CALL close(Sr1fil,rew)
         RETURN
!
 40      no = -1
         CALL mesage(no,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ctrnsp
