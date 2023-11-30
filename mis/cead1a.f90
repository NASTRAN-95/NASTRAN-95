
SUBROUTINE cead1a(Lami,Phidi,Phidli,Lamd,Phid,Phidl,Nfound,Nvect,Capp)
   IMPLICIT NONE
   REAL Degra , Head(1) , Pi , Radeg , S4pisq , Twopi , Z(1)
   INTEGER Fileb(7) , Filek(7) , Filem(7) , Ii , Incur , It1 , It2 , Iz(1) , Jj , Ksystm(65) , Sysbuf
   DOUBLE PRECISION Zd(1)
   COMMON /cinvpx/ Filek , Filem , Fileb
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /output/ Head
   COMMON /packx / It1 , It2 , Ii , Jj , Incur
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER Capp , Lamd , Lami , Nfound , Nvect , Phid , Phidi , Phidl , Phidli
   DOUBLE PRECISION d1 , d2
   INTEGER det , file , hes , i , ibuf , ibuf1 , iflag , ih(7) , ilama , ip1 , ipos , j , k , l , m , name(2)
   INTEGER korsz
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
!
!     INITIALIZE POINTER ARRAY
!
   DO i = 1 , Nfound
      Iz(i) = i
   ENDDO
!
!     BRING IN  EIGENVALUES
!
   ilama = (Nfound+1)/2 + 1
   ibuf = korsz(Iz) - Sysbuf + 1
   file = Lami
   CALL open(*100,Lami,Iz(ibuf),0)
   k = ilama
   DO i = 1 , Nfound
      CALL read(*300,*400,Lami,Zd(k),4,1,iflag)
      k = k + 2
   ENDDO
   CALL close(Lami,1)
   IF ( Nfound/=1 ) THEN
!
!
!     SORT ON SIGN IMAGINARY THEN ON MAG IMAG
!
      Jj = Nfound - 1
      DO i = 1 , Jj
         Ii = i + 1
         m = ilama + 2*i - 2
         DO j = Ii , Nfound
            l = ilama + 2*j - 2
!
!     SIGN IMAG
!
            d1 = dsign(1.0D0,Zd(l+1))
            d2 = dsign(1.0D0,Zd(m+1))
            IF ( d1==d2 ) THEN
!
!     TEST MAGNITIDE IMAG
!
               IF ( dabs(Zd(l+1))>=dabs(Zd(m+1)) ) CYCLE
            ELSEIF ( d1==1.0D0 ) THEN
               CYCLE
            ENDIF
!
!     SWITCH
!
            d1 = Zd(l)
            Zd(l) = Zd(m)
            Zd(m) = d1
            d1 = Zd(l+1)
            Zd(l+1) = Zd(m+1)
            Zd(m+1) = d1
            It1 = Iz(j)
            Iz(j) = Iz(i)
            Iz(i) = It1
         ENDDO
      ENDDO
   ENDIF
!
!     PUT OUT LAMA-S IN ORDER GIVEN BY LIST
!
   CALL gopen(Lamd,Iz(ibuf),1)
   ih(2) = 1006
   ih(1) = 90
   CALL write(Lamd,ih,4,0)
   ih(6) = 6
   CALL write(Lamd,ih,6,0)
   CALL write(Lamd,Iz,40,0)
   CALL write(Lamd,Head,96,1)
   l = 5*Nfound + 2
   DO i = 1 , Nfound
      Iz(l) = i
      Iz(l+1) = Iz(i)
      k = 2*i - 2 + ilama
      Z(l+2) = Zd(k)
      Z(l+3) = Zd(k+1)
      Z(l+4) = 0.0
      Z(l+5) = 0.0
      IF ( abs(Z(l+3))>1.0E-3*abs(Z(l+2)) ) THEN
         Z(l+4) = abs(Z(l+3))/Twopi
         Z(l+5) = -2.0*Z(l+2)/abs(Z(l+3))
      ENDIF
      CALL write(Lamd,Iz(l),6,0)
   ENDDO
   CALL close(Lamd,1)
   ih(1) = Lamd
   CALL wrttrl(ih)
!
!     BRING IN PHIDI IN ORDER NEEDED AND OUTPUT
!
   ibuf1 = ibuf - Sysbuf
   CALL gopen(Phid,Iz(ibuf1),1)
   It1 = 4
   It2 = 3
   Incur = 1
   Ii = 1
   ih(1) = Phid
   ih(2) = 0
   ih(4) = 2
   ih(5) = 3
   ih(6) = 0
   k = 1
   DO WHILE ( Iz(k)>Nvect )
      k = k + 1
   ENDDO
   file = Phidi
   ipos = 1
   CALL open(*100,Phidi,Iz(ibuf),0)
   DO i = 1 , Nvect
      IF ( Nvect/=1 ) THEN
         DO
            l = Iz(i) - ipos
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
            EXIT
         ENDDO
      ENDIF
!
!     BRING IN EIGENVECTORS
!
      CALL read(*300,*50,Phidi,Zd(ilama),ibuf1-1,0,m)
      GOTO 500
 50   Jj = m/4
      ipos = Iz(k) + 1
      CALL pack(Zd(ilama),Phid,ih)
      k = k + 1
   ENDDO
   CALL close(Phid,1)
   CALL close(Phidi,1)
   ih(3) = Jj
   CALL wrttrl(ih)
!
!     OUTPUT PHIDL IF NOT PURGED AND IF AT LEAST ONE INPUT MATRIX IS
!     UNSYMMETRIC
!
   ih(1) = Phidl
   CALL rdtrl(ih)
   IF ( ih(1)<0 ) RETURN
   IF ( Capp==det .OR. Capp==hes ) THEN
      Filek(1) = 101
      CALL rdtrl(Filek)
      Filem(1) = 103
      CALL rdtrl(Filem)
      Fileb(1) = 102
      CALL rdtrl(Fileb)
   ENDIF
   IF ( Filek(1)<=0 .OR. Filek(4)==6 ) THEN
      IF ( Filem(1)<=0 .OR. Filem(4)==6 ) THEN
         IF ( Fileb(1)<=0 .OR. Fileb(4)==6 ) RETURN
      ENDIF
   ENDIF
   CALL gopen(Phidl,Iz(ibuf1),1)
   CALL makmcb(ih,Phidl,0,2,3)
   IF ( Capp/=det .AND. Capp/=hes ) THEN
      k = 1
      DO WHILE ( Iz(k)>Nvect )
         k = k + 1
      ENDDO
      file = Phidli
      ipos = 1
      CALL open(*100,Phidli,Iz(ibuf),0)
      DO i = 1 , Nvect
         IF ( Nvect/=1 ) THEN
            DO
               l = Iz(i) - ipos
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
               EXIT
            ENDDO
         ENDIF
!
!     BRING IN LEFT EIGENVECTORS
!
         CALL read(*300,*60,Phidli,Zd(ilama),ibuf1-1,0,m)
         GOTO 500
 60      Jj = m/4
         ipos = Iz(k) + 1
         CALL pack(Zd(ilama),Phidl,ih)
         k = k + 1
      ENDDO
      CALL close(Phidli,1)
   ELSE
      CALL clvec(Lamd,Nvect,Phidl,ih,ibuf,ibuf1)
   ENDIF
   CALL close(Phidl,1)
   ih(3) = Jj
   CALL wrttrl(ih)
   RETURN
!
!     ERROR MESAGES
!
 100  ip1 = -1
 200  CALL mesage(ip1,file,name)
 300  ip1 = -2
   GOTO 200
 400  ip1 = -3
   GOTO 200
 500  ip1 = -8
   GOTO 200
END SUBROUTINE cead1a