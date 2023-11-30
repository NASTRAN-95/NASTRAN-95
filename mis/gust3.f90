
SUBROUTINE gust3(Qhjk,Wj,Pp,Gustl,Pdel,Pgust,Q,Nfreq,Nload,Nrowj,Ncolw)
   IMPLICIT NONE
   INTEGER Ii , Ii1 , Incr , Incr1 , Itc , Itc1 , Itc2 , Iz(1) , Jj , Jj1 , Sysbuf
   REAL Z(1)
   COMMON /packx / Itc1 , Itc2 , Ii1 , Jj1 , Incr1
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itc , Ii , Jj , Incr
   COMMON /zzzzzz/ Z
   INTEGER Gustl , Ncolw , Nfreq , Nload , Nrowj , Pdel , Pgust , Pp , Qhjk , Wj
   REAL Q
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ipdel , iqhj , it1 , iwj , iz2 , j , m , mcb(7) , name(2) , nrqhj , ntpdel , ntqhj , &
         & ntwz , nz
   INTEGER korsz
   REAL pgc , pgr , qwg , qwgc , qwgr
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
   ibuf1 = korsz(Iz) - Sysbuf + 1
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
   CALL gopen(Wj,Iz(ibuf1),0)
   CALL gopen(Qhjk,Iz(ibuf2),0)
   CALL gopen(Pdel,Iz(ibuf3),1)
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
      Jj = nrqhj
      CALL unpack(*50,Qhjk,Z(iqhj))
!
!     MULTIPY EACH IMAGINARY PART BY K
!
      DO j = 1 , ntqhj , 2
         Z(iqhj+j) = Z(iqhj+j)*Z(2*i)
      ENDDO
      GOTO 100
!
!     NULL COLUMN
!
 50   CALL zeroc(Z(iqhj),ntqhj)
!
!     BRING WJ COLUMN INTO CORE
!
 100  Jj = Nrowj
      CALL unpack(*150,Wj,Z(iwj))
      GOTO 200
 150  CALL zeroc(Z(iwj),ntwz)
!
!     MULTIPLY
!
 200  CALL gmmatc(Z(iqhj),Jj1,Nrowj,0,Z(iwj),Nrowj,1,0,Z(ipdel))
      CALL pack(Z(ipdel),Pdel,mcb)
   ENDDO
   CALL close(Wj,1)
   CALL close(Qhjk,1)
   CALL close(Pdel,1)
   CALL wrttrl(mcb)
   CALL dmpfil(-Pdel,Z,nz)
!
!     REPEATEDLY READ PDEL MULTIPLYING BY Q,WG, AND PP
!
   CALL gopen(Pdel,Iz(ibuf1),0)
   CALL gopen(Pp,Iz(ibuf2),0)
   CALL gopen(Gustl,Iz(ibuf3),0)
   CALL gopen(Pgust,Iz(ibuf4),1)
   CALL makmcb(mcb,Pgust,mcb(3),mcb(4),mcb(5))
   DO i = 1 , Nload
      CALL rewind(Pdel)
      CALL skprec(Pdel,1)
      CALL fread(Gustl,Iz,5,1)
      iz2 = 2
      qwg = Q*Z(iz2+1)
      DO j = 1 , Nfreq
         Jj = 1
         CALL unpack(*220,Pp,Z)
         qwgr = qwg*Z(1)
         qwgc = qwg*Z(iz2)
         GOTO 240
 220     qwgr = 0.0
         qwgc = 0.0
 240     Jj = Jj1
         CALL unpack(*260,Pdel,Z)
         GOTO 280
 260     CALL zeroc(Z,ntpdel)
 280     DO m = 1 , ntpdel , 2
            pgr = qwgr*Z(m) - qwgc*Z(m+1)
            pgc = qwgr*Z(m+1) + qwgc*Z(m)
            Z(m) = pgr
            Z(m+1) = pgc
         ENDDO
         CALL pack(Z,Pgust,mcb)
      ENDDO
   ENDDO
   CALL close(Pdel,1)
   CALL close(Pp,1)
   CALL close(Gustl,1)
   CALL close(Pgust,1)
   CALL wrttrl(mcb)
END SUBROUTINE gust3