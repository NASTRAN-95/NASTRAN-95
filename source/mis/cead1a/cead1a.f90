!*==cead1a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cead1a(Lami,Phidi,Phidli,Lamd,Phid,Phidl,Nfound,Nvect,Capp)
   USE c_cinvpx
   USE c_condas
   USE c_output
   USE c_packx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lami
   INTEGER :: Phidi
   INTEGER :: Phidli
   INTEGER :: Lamd
   INTEGER :: Phid
   INTEGER :: Phidl
   INTEGER :: Nfound
   INTEGER :: Nvect
   INTEGER :: Capp
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: d1 , d2
   INTEGER , SAVE :: det , hes
   INTEGER :: file , i , ibuf , ibuf1 , iflag , ilama , ip1 , ipos , j , k , l , m , sysbuf
   INTEGER , DIMENSION(7) , SAVE :: ih
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL(REAL64) , DIMENSION(1) :: zd
   EXTERNAL close , clvec , gopen , korsz , makmcb , mesage , open , pack , rdtrl , read , rewind , skprec , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ROUTINE SORTS LAMI, PHIDI AND PHIDLI (INV. POWER), BASED ON LAMI,
!     AND CREATES LAMD, PHID AND PHIDL
!
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf)
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (Z(1),Zd(1))
!
   DATA name/4HCEAD , 4H1A  /
   DATA ih/7*0/
   DATA det , hes/4HDET  , 4HHESS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE POINTER ARRAY
!
         DO i = 1 , Nfound
            iz(i) = i
         ENDDO
!
!     BRING IN  EIGENVALUES
!
         ilama = (Nfound+1)/2 + 1
         ibuf = korsz(iz) - sysbuf + 1
         file = Lami
         CALL open(*20,Lami,iz(ibuf),0)
         k = ilama
         DO i = 1 , Nfound
            CALL read(*40,*60,Lami,zd(k),4,1,iflag)
            k = k + 2
         ENDDO
         CALL close(Lami,1)
         IF ( Nfound/=1 ) THEN
!
!
!     SORT ON SIGN IMAGINARY THEN ON MAG IMAG
!
            jj = Nfound - 1
            DO i = 1 , jj
               ii = i + 1
               m = ilama + 2*i - 2
               DO j = ii , Nfound
                  l = ilama + 2*j - 2
!
!     SIGN IMAG
!
                  d1 = dsign(1.0D0,zd(l+1))
                  d2 = dsign(1.0D0,zd(m+1))
                  IF ( d1==d2 ) THEN
!
!     TEST MAGNITIDE IMAG
!
                     IF ( dabs(zd(l+1))>=dabs(zd(m+1)) ) CYCLE
                  ELSEIF ( d1==1.0D0 ) THEN
                     CYCLE
                  ENDIF
!
!     SWITCH
!
                  d1 = zd(l)
                  zd(l) = zd(m)
                  zd(m) = d1
                  d1 = zd(l+1)
                  zd(l+1) = zd(m+1)
                  zd(m+1) = d1
                  it1 = iz(j)
                  iz(j) = iz(i)
                  iz(i) = it1
               ENDDO
            ENDDO
         ENDIF
!
!     PUT OUT LAMA-S IN ORDER GIVEN BY LIST
!
         CALL gopen(Lamd,iz(ibuf),1)
         ih(2) = 1006
         ih(1) = 90
         CALL write(Lamd,ih,4,0)
         ih(6) = 6
         CALL write(Lamd,ih,6,0)
         CALL write(Lamd,iz,40,0)
         CALL write(Lamd,head,96,1)
         l = 5*Nfound + 2
         DO i = 1 , Nfound
            iz(l) = i
            iz(l+1) = iz(i)
            k = 2*i - 2 + ilama
            z(l+2) = zd(k)
            z(l+3) = zd(k+1)
            z(l+4) = 0.0
            z(l+5) = 0.0
            IF ( abs(z(l+3))>1.0E-3*abs(z(l+2)) ) THEN
               z(l+4) = abs(z(l+3))/twopi
               z(l+5) = -2.0*z(l+2)/abs(z(l+3))
            ENDIF
            CALL write(Lamd,iz(l),6,0)
         ENDDO
         CALL close(Lamd,1)
         ih(1) = Lamd
         CALL wrttrl(ih)
!
!     BRING IN PHIDI IN ORDER NEEDED AND OUTPUT
!
         ibuf1 = ibuf - sysbuf
         CALL gopen(Phid,iz(ibuf1),1)
         it1 = 4
         it2 = 3
         incur = 1
         ii = 1
         ih(1) = Phid
         ih(2) = 0
         ih(4) = 2
         ih(5) = 3
         ih(6) = 0
         k = 1
         DO WHILE ( iz(k)>Nvect )
            k = k + 1
         ENDDO
         file = Phidi
         ipos = 1
         CALL open(*20,Phidi,iz(ibuf),0)
         DO i = 1 , Nvect
            IF ( Nvect/=1 ) THEN
               SPAG_Loop_2_1: DO
                  l = iz(i) - ipos
                  IF ( l<0 ) THEN
!
!     PAST VECTOR NEEDED
!
                     CALL rewind(Phidi)
                     ipos = 1
                     CYCLE
                  ELSEIF ( l/=0 ) THEN
                     CALL skprec(Phidi,l)
                  ENDIF
                  EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
            ENDIF
!
!     BRING IN EIGENVECTORS
!
            CALL read(*40,*10,Phidi,zd(ilama),ibuf1-1,0,m)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
 10         jj = m/4
            ipos = iz(k) + 1
            CALL pack(zd(ilama),Phid,ih)
            k = k + 1
         ENDDO
         CALL close(Phid,1)
         CALL close(Phidi,1)
         ih(3) = jj
         CALL wrttrl(ih)
!
!     OUTPUT PHIDL IF NOT PURGED AND IF AT LEAST ONE INPUT MATRIX IS
!     UNSYMMETRIC
!
         ih(1) = Phidl
         CALL rdtrl(ih)
         IF ( ih(1)<0 ) RETURN
         IF ( Capp==det .OR. Capp==hes ) THEN
            filek(1) = 101
            CALL rdtrl(filek)
            filem(1) = 103
            CALL rdtrl(filem)
            fileb(1) = 102
            CALL rdtrl(fileb)
         ENDIF
         IF ( filek(1)<=0 .OR. filek(4)==6 ) THEN
            IF ( filem(1)<=0 .OR. filem(4)==6 ) THEN
               IF ( fileb(1)<=0 .OR. fileb(4)==6 ) RETURN
            ENDIF
         ENDIF
         CALL gopen(Phidl,iz(ibuf1),1)
         CALL makmcb(ih,Phidl,0,2,3)
         IF ( Capp/=det .AND. Capp/=hes ) THEN
            k = 1
            DO WHILE ( iz(k)>Nvect )
               k = k + 1
            ENDDO
            file = Phidli
            ipos = 1
            CALL open(*20,Phidli,iz(ibuf),0)
            DO i = 1 , Nvect
               IF ( Nvect/=1 ) THEN
                  SPAG_Loop_2_2: DO
                     l = iz(i) - ipos
                     IF ( l<0 ) THEN
!
!     PAST VECTOR NEEDED
!
                        CALL rewind(Phidli)
                        ipos = 1
                        CYCLE
                     ELSEIF ( l/=0 ) THEN
                        CALL skprec(Phidli,l)
                     ENDIF
                     EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
               ENDIF
!
!     BRING IN LEFT EIGENVECTORS
!
               CALL read(*40,*15,Phidli,zd(ilama),ibuf1-1,0,m)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
 15            jj = m/4
               ipos = iz(k) + 1
               CALL pack(zd(ilama),Phidl,ih)
               k = k + 1
            ENDDO
            CALL close(Phidli,1)
         ELSE
            CALL clvec(Lamd,Nvect,Phidl,ih,ibuf,ibuf1)
         ENDIF
         CALL close(Phidl,1)
         ih(3) = jj
         CALL wrttrl(ih)
         RETURN
!
!     ERROR MESAGES
!
 20      ip1 = -1
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(ip1,file,name)
 40      ip1 = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      ip1 = -3
         spag_nextblock_1 = 2
      CASE (3)
         ip1 = -8
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cead1a
