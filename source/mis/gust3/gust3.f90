!*==gust3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gust3(Qhjk,Wj,Pp,Gustl,Pdel,Pgust,Q,Nfreq,Nload,Nrowj,Ncolw)
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
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
   ibuf1 = korsz(iz) - sysbuf + 1
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   incr1 = 1
   incr = 1
   ibuf4 = ibuf3 - sysbuf
   mcb(1) = Qhjk
   CALL rdtrl(mcb)
   itc = 3
   itc1 = itc
   itc2 = itc
   CALL gopen(Wj,iz(ibuf1),0)
   CALL gopen(Qhjk,iz(ibuf2),0)
   CALL gopen(Pdel,iz(ibuf3),1)
!
!     SET UP TO PACK
!
   it1 = 1
   jj1 = mcb(3)/Nrowj
   nrqhj = mcb(3)
   ntqhj = nrqhj*2
   CALL makmcb(mcb,Pdel,jj1,2,itc2)
   ii = 1
   iqhj = 2*Nfreq + 1
   iwj = iqhj + ntqhj
   ntwz = Nrowj*2
   ipdel = iwj + ntwz
   ntpdel = jj1*2
   nz = ibuf4 - 1 - ipdel + 2*jj1
   IF ( nz<0 ) CALL mesage(-8,0,name)
   DO i = 1 , Nfreq
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            jj = nrqhj
            CALL unpack(*10,Qhjk,z(iqhj))
!
!     MULTIPY EACH IMAGINARY PART BY K
!
            DO j = 1 , ntqhj , 2
               z(iqhj+j) = z(iqhj+j)*z(2*i)
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
!
!     NULL COLUMN
!
 10         CALL zeroc(z(iqhj),ntqhj)
            spag_nextblock_1 = 2
         CASE (2)
!
!     BRING WJ COLUMN INTO CORE
!
            jj = Nrowj
            CALL unpack(*20,Wj,z(iwj))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
 20         CALL zeroc(z(iwj),ntwz)
            spag_nextblock_1 = 3
         CASE (3)
!
!     MULTIPLY
!
            CALL gmmatc(z(iqhj),jj1,Nrowj,0,z(iwj),Nrowj,1,0,z(ipdel))
            CALL pack(z(ipdel),Pdel,mcb)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(Wj,1)
   CALL close(Qhjk,1)
   CALL close(Pdel,1)
   CALL wrttrl(mcb)
   CALL dmpfil(-Pdel,z,nz)
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
      qwg = Q*z(iz2+1)
      DO j = 1 , Nfreq
         spag_nextblock_2 = 1
         SPAG_DispatchLoop_2: DO
            SELECT CASE (spag_nextblock_2)
            CASE (1)
               jj = 1
               CALL unpack(*25,Pp,z)
               qwgr = qwg*z(1)
               qwgc = qwg*z(iz2)
               spag_nextblock_2 = 2
               CYCLE SPAG_DispatchLoop_2
 25            qwgr = 0.0
               qwgc = 0.0
               spag_nextblock_2 = 2
            CASE (2)
               jj = jj1
               CALL unpack(*30,Pdel,z)
               spag_nextblock_2 = 3
               CYCLE SPAG_DispatchLoop_2
 30            CALL zeroc(z,ntpdel)
               spag_nextblock_2 = 3
            CASE (3)
               DO m = 1 , ntpdel , 2
                  pgr = qwgr*z(m) - qwgc*z(m+1)
                  pgc = qwgr*z(m+1) + qwgc*z(m)
                  z(m) = pgr
                  z(m+1) = pgc
               ENDDO
               CALL pack(z,Pgust,mcb)
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
