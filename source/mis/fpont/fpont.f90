!*==fpont.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fpont
   USE c_loadx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag , scale , xl
   REAL , DIMENSION(3) :: gpco1 , gpco2 , gpco3 , gpco4 , vect , vect1 , vect2
   INTEGER :: gpid , i , icosyt , in , ip1 , ip2 , ip3 , ip4 , iparm , l , minus , n1 , np , nr
   REAL , DIMENSION(7) :: gridp
   INTEGER , DIMENSION(4,5) :: igpco
   INTEGER , DIMENSION(5) :: iord , pont
   INTEGER , DIMENSION(2) , SAVE :: swload
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     DOES DIRECT,TPONT,FPONT,AND SCALAR LOADS
!
   !>>>>EQUIVALENCE (gpid,gridp(2)) , (gridp(4),ip1) , (gridp(5),ip2) , (gridp(6),ip3) , (gridp(7),ip4) , (igpco(2,1),gpco1(1)) ,        &
!>>>>    & (igpco(2,2),gpco2(1)) , (igpco(2,3),gpco3(1)) , (igpco(2,4),gpco4(1)) , (icosyt,gridp(3))
   DATA swload/4HFPON , 4HT   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nr = 6
         np = 5
         minus = 5
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*20,*40,slt,gridp(2),nr,0,flag)
         scale = gridp(3)
         pont(1) = ip1
         pont(2) = ip2
         IF ( np/=3 ) THEN
            pont(3) = ip3
            pont(4) = ip4
         ENDIF
         pont(np) = gpid
         CALL permut(pont(1),iord(1),np,old)
         DO i = 1 , np
            l = iord(i)
            CALL fndpnt(igpco(1,l),pont(l))
         ENDDO
         IF ( np==3 ) THEN
            DO i = 1 , 3
               vect(i) = gpco2(i) - gpco1(i)
            ENDDO
         ELSE
            DO i = 1 , 3
               vect1(i) = gpco2(i) - gpco1(i)
               vect2(i) = gpco4(i) - gpco3(i)
            ENDDO
            CALL cross(vect1(1),vect2(1),vect(1))
         ENDIF
         CALL norm(vect(1),xl)
         spag_nextblock_1 = 3
      CASE (3)
         IF ( igpco(1,np)/=0 ) CALL basglb(vect(1),vect(1),igpco(2,np),igpco(1,np))
         spag_nextblock_1 = 4
      CASE (4)
         CALL fndsil(gpid)
         gpid = gpid + (ifm-minus)*3 - 1
         DO i = 1 , 3
            in = gpid + i
            core(in) = core(in) + vect(i)*scale
         ENDDO
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 20      n1 = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      n1 = -3
         spag_nextblock_1 = 5
      CASE (5)
         iparm = slt
         CALL mesage(n1,iparm,swload)
         spag_nextblock_1 = 6
      CASE (6)
         RETURN
!
!
         ENTRY tpont
!     ===========
!
!     TPONT PROCESSES FORCE1 AND MOMENT1 CARDS
!
         nr = 4
         np = 3
         minus = 3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY direct
!     ============
!
!     DIRECT PROCESSES FORCE+ MOMENT CARDS
!
         np = 1
         minus = 1
         CALL read(*20,*40,slt,gridp(2),6,0,flag)
         DO i = 1 , 3
            vect(i) = gridp(i+4)
         ENDDO
         CALL fndpnt(igpco(1,1),gpid)
         scale = gridp(4)
         IF ( icosyt==igpco(1,np) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( icosyt/=0 ) CALL glbbas(vect(1),vect(1),igpco(2,1),icosyt)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY sload
!     ===========
!
!     SLOAD PROCESSES SLOAD CARDS
!
         CALL read(*20,*40,slt,gridp(2),2,0,flag)
         CALL fndsil(gpid)
         core(gpid) = core(gpid) + gridp(3)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fpont
