!*==gust3.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gust3(Qhjk,Wj,Pp,Gustl,Pdel,Pgust,Q,Nfreq,Nload,Nrowj,Ncolw)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Qhjk
   INTEGER :: Wj
   INTEGER :: Pp
   INTEGER :: Gustl
   INTEGER :: Pdel
   INTEGER :: Pgust
   REAL :: Q
   INTEGER :: Nfreq
   INTEGER :: Nload
   INTEGER :: Nrowj
   INTEGER :: Ncolw
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ipdel , iqhj , it1 , iwj , iz2 , j , m , nrqhj , ntpdel , ntqhj , ntwz , nz
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: pgc , pgr , qwg , qwgc , qwgr
   EXTERNAL close , dmpfil , fread , gmmatc , gopen , korsz , makmcb , mesage , pack , rdtrl , rewind , skprec , unpack , wrttrl ,  &
          & zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THE PURPOSE OF THIS ROUTINE IS TO MULTIPLY QHJK(+) BY WJ
!     FORMING PDEL
!     PDEL IS THEN MULTIPLIED BY  Q*WG*PP(W)  FORMING PGUST
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HGUST , 1H3/
!
!     INITIALIZE
!
   ibuf1 = korsz(iz) - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   Incr1 = 1
   Incr = 1
   ibuf4 = ibuf3 - Sysbuf
   mcb(1) = Qhjk
   CALL rdtrl(mcb)
   Itc = 3
   Itc1 = Itc
   Itc2 = Itc
   CALL gopen(Wj,iz(ibuf1),0)
   CALL gopen(Qhjk,iz(ibuf2),0)
   CALL gopen(Pdel,iz(ibuf3),1)
!
!     SET UP TO PACK
!
   it1 = 1
   Jj1 = mcb(3)/Nrowj
   nrqhj = mcb(3)
   ntqhj = nrqhj*2
   CALL makmcb(mcb,Pdel,Jj1,2,Itc2)
   Ii = 1
   iqhj = 2*Nfreq + 1
   iwj = iqhj + ntqhj
   ntwz = Nrowj*2
   ipdel = iwj + ntwz
   ntpdel = Jj1*2
   nz = ibuf4 - 1 - ipdel + 2*Jj1
   IF ( nz<0 ) CALL mesage(-8,0,name)
   DO i = 1 , Nfreq
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            Jj = nrqhj
            CALL unpack(*10,Qhjk,Z(iqhj))
!
!     MULTIPY EACH IMAGINARY PART BY K
!
            DO j = 1 , ntqhj , 2
               Z(iqhj+j) = Z(iqhj+j)*Z(2*i)
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
!
!     NULL COLUMN
!
 10         CALL zeroc(Z(iqhj),ntqhj)
            spag_nextblock_1 = 2
         CASE (2)
!
!     BRING WJ COLUMN INTO CORE
!
            Jj = Nrowj
            CALL unpack(*20,Wj,Z(iwj))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
 20         CALL zeroc(Z(iwj),ntwz)
            spag_nextblock_1 = 3
         CASE (3)
!
!     MULTIPLY
!
            CALL gmmatc(Z(iqhj),Jj1,Nrowj,0,Z(iwj),Nrowj,1,0,Z(ipdel))
            CALL pack(Z(ipdel),Pdel,mcb)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(Wj,1)
   CALL close(Qhjk,1)
   CALL close(Pdel,1)
   CALL wrttrl(mcb)
   CALL dmpfil(-Pdel,Z,nz)
!
!     REPEATEDLY READ PDEL MULTIPLYING BY Q,WG, AND PP
!
   CALL gopen(Pdel,iz(ibuf1),0)
   CALL gopen(Pp,iz(ibuf2),0)
   CALL gopen(Gustl,iz(ibuf3),0)
   CALL gopen(Pgust,iz(ibuf4),1)
   CALL makmcb(mcb,Pgust,mcb(3),mcb(4),mcb(5))
   DO i = 1 , Nload
      CALL rewind(Pdel)
      CALL skprec(Pdel,1)
      CALL fread(Gustl,iz,5,1)
      iz2 = 2
      qwg = Q*Z(iz2+1)
      DO j = 1 , Nfreq
         spag_nextblock_2 = 1
         SPAG_DispatchLoop_2: DO
            SELECT CASE (spag_nextblock_2)
            CASE (1)
               Jj = 1
               CALL unpack(*25,Pp,Z)
               qwgr = qwg*Z(1)
               qwgc = qwg*Z(iz2)
               spag_nextblock_2 = 2
               CYCLE SPAG_DispatchLoop_2
 25            qwgr = 0.0
               qwgc = 0.0
               spag_nextblock_2 = 2
            CASE (2)
               Jj = Jj1
               CALL unpack(*30,Pdel,Z)
               spag_nextblock_2 = 3
               CYCLE SPAG_DispatchLoop_2
 30            CALL zeroc(Z,ntpdel)
               spag_nextblock_2 = 3
            CASE (3)
               DO m = 1 , ntpdel , 2
                  pgr = qwgr*Z(m) - qwgc*Z(m+1)
                  pgc = qwgr*Z(m+1) + qwgc*Z(m)
                  Z(m) = pgr
                  Z(m+1) = pgc
               ENDDO
               CALL pack(Z,Pgust,mcb)
               EXIT SPAG_DispatchLoop_2
            END SELECT
         ENDDO SPAG_DispatchLoop_2
      ENDDO
   ENDDO
   CALL close(Pdel,1)
   CALL close(Pp,1)
   CALL close(Gustl,1)
   CALL close(Pgust,1)
   CALL wrttrl(mcb)
END SUBROUTINE gust3
