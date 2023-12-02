!*==eqmcka.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eqmcka(Ip,Bgpdt,Cstm,Eqexin,D,Iscalr)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ip
   INTEGER :: Bgpdt
   INTEGER :: Cstm
   INTEGER :: Eqexin
   INTEGER :: D
   INTEGER :: Iscalr
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6,6) :: dd
   INTEGER :: file , i , ibuf , ibuf1 , iflag , ip1 , j , ncstm , nz
   INTEGER , DIMENSION(1) :: iz
   INTEGER , SAVE :: iz2 , iz3 , iz4 , iz5
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(3) :: r
   REAL , DIMENSION(3,3) :: ti , tr , tt
   EXTERNAL close , fread , fwdrec , gmmats , gopen , korsz , makmcb , mesage , open , pack , pretrs , read , rewind , skprec ,     &
          & transs , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ROUTINE FORMS D MATRIX (ACCTUALLY D TRANSPOSE)
!
!
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
!
   DATA iz2 , iz3 , iz4 , iz5/2 , 3 , 4 , 5/
   DATA name/4HEQMC , 4HKA  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CONVERT  EXTERNAL IP TO INTERNAL IP
!
         ibuf = korsz(Z) - Sysbuf + 1
         file = Eqexin
         CALL gopen(Eqexin,Z(ibuf),0)
         CALL read(*100,*20,Eqexin,iz(1),ibuf-1,0,iflag)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(Eqexin,1)
         DO i = 1 , iflag , 2
            IF ( iz(i)==Ip ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL mesage(41,Ip,name)
         Ip = 0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         Ip = iz(i+1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     FIND RZERO FOR  IP
!
         file = Bgpdt
         r(1) = 0.0
         r(2) = 0.0
         r(3) = 0.0
         CALL gopen(Bgpdt,Z(ibuf),0)
         IF ( Ip/=0 ) THEN
            i = (Ip-1)*4
            CALL fread(Bgpdt,Z,-i,0)
            CALL fread(Bgpdt,i,1,0)
            IF ( i==-1 ) THEN
!
!     SCALAR POINT
!
               CALL mesage(41,Ip,name)
            ELSE
               CALL fread(Bgpdt,r,3,0)
            ENDIF
            CALL rewind(Bgpdt)
            CALL skprec(Bgpdt,1)
         ENDIF
!
!     SET UP TO WRITE D
!
         ibuf1 = ibuf - Sysbuf
         nz = ibuf1 - 5
!
!     BRING IN CSTM
!
         file = Cstm
         CALL open(*60,Cstm,Z(ibuf1),0)
         CALL fwdrec(*100,Cstm)
         CALL read(*100,*40,Cstm,Z(iz5),nz,0,ncstm)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(Cstm,1)
         CALL pretrs(Z(iz5),ncstm)
 60      CALL gopen(D,Z(ibuf1),1)
         CALL makmcb(mcb,D,6,2,1)
         Iscalr = 0
         Ii = 1
         Jj = 6
         It1 = 1
         It2 = 1
         Incr = 1
         DO
!
!     EXAMINE BGPDT
!
            CALL read(*100,*80,Bgpdt,Z(1),4,0,iflag)
            IF ( iz(1)==-1 ) THEN
!
!     SCALAR POINT
!
               DO i = 1 , 6
                  dd(i,1) = 0.0
               ENDDO
               CALL pack(dd,D,mcb)
            ELSE
!
!     COMPUTE  TR
!
               Iscalr = 1
               tr(1,1) = 0.0
               tr(2,2) = 0.0
               tr(3,3) = 0.0
               tr(2,1) = Z(iz4) - r(3)
               tr(1,2) = -tr(2,1)
               tr(3,1) = r(2) - Z(iz3)
               tr(1,3) = -tr(3,1)
               tr(3,2) = Z(iz2) - r(1)
               tr(2,3) = -tr(3,2)
               DO i = 1 , 3
                  DO j = 1 , 3
                     ti(i,j) = 0.0
                     IF ( i==j ) ti(i,j) = 1.0
                  ENDDO
               ENDDO
               IF ( iz(1)/=0 ) THEN
                  CALL transs(iz(1),ti)
                  CALL gmmats(ti,3,3,1,tr,3,3,0,tt)
                  DO i = 1 , 3
                     DO j = 1 , 3
                        tr(i,j) = tt(i,j)
                     ENDDO
                  ENDDO
               ENDIF
!
!     MOVE STUFF INTO  DD
!
               DO i = 1 , 6
                  DO j = 1 , 3
                     IF ( i>3 ) THEN
                        dd(i,j) = tr(i-3,j)
                        dd(j,i) = 0.0
                     ELSE
                        dd(i,j) = ti(j,i)
                        dd(i+3,j+3) = dd(i,j)
                     ENDIF
                  ENDDO
               ENDDO
               DO i = 1 , 6
                  CALL pack(dd(1,i),D,mcb)
               ENDDO
            ENDIF
         ENDDO
!
!     END BGPDT
!
 80      CALL close(Bgpdt,1)
         CALL close(D,1)
         CALL wrttrl(mcb)
         RETURN
 100     DO
            ip1 = -2
!
!     ERROR MESAGES
!
            CALL mesage(ip1,file,name)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         ip1 = -8
         CALL mesage(ip1,file,name)
         GOTO 100
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE eqmcka
