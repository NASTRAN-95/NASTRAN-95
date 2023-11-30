
SUBROUTINE fa1k(Imeth,K,Rho,Outfil,Ico)
   IMPLICIT NONE
   REAL Dum(52) , Z(1)
   INTEGER Floop , Ii , Incr , Incr1 , Inn , Iout , Iprec , Iti , Ito , Iz(1) , Nn , Nnn , Out , Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Floop
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Out , Dum , Iprec
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Iz
   INTEGER Ico , Imeth , Outfil
   REAL K , Rho
   INTEGER buff , buff1 , fsave , i , idp , ifil , ig , iip , ij , ik , ilop , j , ji , jj , mcb(7) , n , nc , ncm , ncol , ncore , &
         & nd , nf , ni , ni2 , nip , nogo , nrd , nrho , ns(2) , nwc , nwr , qhhl , scr2 , scr3 , scr4 , trl(7) , type
   REAL cmach , eps , ok , omach , temp
   INTEGER korsz
   LOGICAL new
!
!     FA1K BUILDS AN INTERPOLATED MATRIX ON OUTFIL FROM QHHL OR FSAVE
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA fsave/201/ , qhhl/104/ , scr2 , scr3 , scr4/302 , 303 , 304/
   DATA ns/4HFA1K , 4H    /
!
   ncore = korsz(Iz) - Ico
   buff = ncore - Sysbuf
   buff1 = buff - Sysbuf
   trl(1) = fsave
   CALL rdtrl(trl)
!
!     READ IN DEPENDENT POINTS AND SET K AND RHO
!
   jj = trl(3)*3
   ifil = fsave
   CALL gopen(fsave,Iz(buff+1),0)
   CALL read(*1500,*100,fsave,Z,jj,1,nwr)
 100  i = (Floop-1)*3 + 1
   cmach = Z(i)
   K = Z(i+1)
   Rho = Z(i+2)
   Incr1 = 1
   Incr = 1
   Ii = 1
   Inn = 1
   IF ( Imeth==2 ) THEN
!
!     LINEAR SPLINE INTERPOLATION
!
!
!     IS A GOOD MATRIZ ON FSAVE
!
      eps = .001
      new = .TRUE.
      ni = trl(4)
      IF ( Floop==1 ) GOTO 400
      ok = Z(i-2)
      omach = Z(i-3)
      IF ( abs(cmach-omach)<eps ) new = .FALSE.
!
!     REWRITE QHHL IF NEW IS TRUE
!
      IF ( .NOT.new ) THEN
         IF ( ok-K/=0.0 ) trl(7) = trl(7) + 1
         ij = trl(7) + 1
         CALL wrttrl(trl)
         GOTO 1000
      ELSE
         IF ( Floop==1 ) GOTO 400
         GOTO 600
      ENDIF
!
!     SURFACE SPLINE INTERPOLATION
!
   ELSEIF ( Floop/=1 ) THEN
!
!     GET A COLUMN FROM FSAVE AND BUILD QHH ON OUTFIL
!
      nf = 2 + (Floop-1)/trl(7)
      DO i = 1 , nf
         CALL fwdrec(*1500,fsave)
      ENDDO
      GOTO 300
   ELSE
!
!     SET UP CALL TO SPLINE INTERPOLATOR
!
      nrho = trl(7)
      ji = nrho*3
      j = 1
      DO i = 1 , jj , ji
         Z(j) = Z(i)
         Z(j+1) = Z(i+1)
         j = j + 2
      ENDDO
      nd = jj/ji
      ni = trl(4)
      type = 1
      idp = 1
      iip = nd*2 + idp
      ig = iip + 2*ni
      ni2 = ni*2
      CALL read(*1500,*200,fsave,Z(iip),ni2,1,nwr)
   ENDIF
 200  CALL fwdrec(*1500,fsave)
   CALL close(fsave,2)
!
!     REWRITE QHHL SO EACH LIST MATRIX IS A COLUMN
!
   GOTO 1200
 300  Iout = trl(6)
   Iti = Iout
   Ito = Iout
   nwc = 1
   IF ( Ito==2 .OR. Ito==3 ) nwc = 2
   IF ( Ito==4 ) nwc = 4
   mcb(1) = qhhl
   CALL rdtrl(mcb)
   nc = mcb(3)
   Nn = nc
   Nnn = nc*nc
   CALL unpack(*1400,fsave,Z)
   ij = 1
   CALL close(fsave,1)
   CALL gopen(Outfil,Iz(buff+1),1)
   mcb(1) = Outfil
   mcb(2) = 0
   mcb(3) = nc
   mcb(4) = 1
   mcb(5) = Iout
   mcb(6) = 0
   mcb(7) = 0
   DO i = 1 , nc
      CALL pack(Z(ij),Outfil,mcb)
      ij = ij + nc*nwc
   ENDDO
   CALL close(Outfil,1)
   CALL wrttrl(mcb)
   GOTO 99999
!
!     TEST TO SEE IF QHHL HAS ENOUGH MACH NUMBERS
!
 400  nip = ni*2
   nogo = 0
   iip = jj + 1
   CALL read(*1500,*500,fsave,Z(iip),nip,1,nwr)
 500  CALL bckrec(fsave)
   temp = 0.0
   DO i = 1 , jj , 3
      IF ( temp/=Z(i) ) THEN
         temp = Z(i)
         nf = 0
         DO j = 1 , nip , 2
            IF ( temp-Z(iip+j-1)<eps ) nf = nf + 1
         ENDDO
         IF ( nf<=1 ) THEN
            WRITE (Out,99001) Ufm , temp
!
!     ERROR MESSAGES
!
99001       FORMAT (A23,' 2270, LINEAR INTERPOLATION WITHOUT ENOUGH IND. ','MACH NUMBERS EQUAL TO DEP. MACH ',F10.4)
            nogo = 1
         ENDIF
      ENDIF
   ENDDO
   IF ( nogo==1 ) GOTO 1400
 600  j = 1
   nrd = 0
   DO i = 1 , jj , 3
      IF ( abs(cmach-Z(i))<eps ) THEN
         IF ( Z(i+2)==Rho ) THEN
            Z(j) = Z(i)
            Z(j+1) = Z(i+1)
            j = j + 2
            nrd = nrd + 1
         ENDIF
      ENDIF
   ENDDO
   idp = 1
   iip = nrd*2 + idp
   ni2 = ni*2
   CALL read(*1500,*700,fsave,Z(iip),ni2,1,nwr)
 700  CALL fwdrec(*1500,fsave)
   CALL close(fsave,2)
   GOTO 1200
 800  IF ( abs(cmach-Z(iip+i-1))<eps ) THEN
      Z(iip+ik) = Z(iip+i)
      ik = ik + 2
      nf = nf + 1
      ji = ig
      GOTO 1300
   ELSE
!
!     SKIP MATRIX
!
      DO j = 1 , ncm
         CALL fwdrec(*1500,qhhl)
      ENDDO
   ENDIF
 900  i = i + 2
   IF ( i/=jj ) GOTO 800
   CALL close(qhhl,1)
   CALL close(Outfil,1)
   CALL wrttrl(trl)
!
!     SET UP CALL TO SPLINE INTERPOLATION
!
   type = -1
   nd = nrd
   ni = nf
   GOTO 1100
 1000 DO i = 1 , ij
      CALL fwdrec(*1500,fsave)
   ENDDO
   GOTO 300
!
!     CALL MINTRP
!
 1100 ig = iip + 2*ni
   nc = ncore - ig
   nogo = 0
   CALL mintrp(ni,Z(iip),nd,Z(idp),type,0,0,0.0,Outfil,scr2,scr3,scr4,Z(ig),nc,nogo,Iprec)
   IF ( nogo==1 ) GOTO 1400
!
!     INTERPOLATED MATRIX IS ON SCR2 MOVE TO FSAVE
!
   CALL open(*1500,fsave,Iz(buff+1),3)
   CALL gopen(scr2,Iz(buff1+1),0)
   trl(1) = scr2
   CALL rdtrl(trl)
   ncol = trl(2)
   Nn = trl(3)
   Nnn = Nn
   Iti = trl(5)
   Ito = Iti
   Iout = Iti
   trl(1) = fsave
   trl(2) = 0
   trl(6) = 0
   trl(7) = 0
   i = 1
   DO
      CALL unpack(*1400,scr2,Z)
      CALL pack(Z,fsave,trl)
      IF ( i==ncol ) THEN
         CALL close(scr2,1)
         CALL close(fsave,1)
         CALL rdtrl(trl)
         trl(6) = Ito
         IF ( Imeth==2 ) trl(7) = 1
         CALL wrttrl(trl)
!
!     GET COLUMN FROM FSAVE AND BUILD QHH
!
         CALL gopen(fsave,Iz(buff+1),0)
         ij = 3
         GOTO 1000
      ELSE
         i = i + 1
      ENDIF
   ENDDO
!
!     SET UP COLUMN - MATRIX COPY
!
 1200 CALL gopen(qhhl,Iz(buff+1),0)
   trl(1) = qhhl
   CALL rdtrl(trl)
   ncol = trl(2)/trl(3)
   ncm = trl(3)
   CALL gopen(Outfil,Iz(buff1+1),1)
   Nnn = ncm
   Nn = ncm*ncm
   Iti = trl(5)
   Ito = Iti
   Iout = Iti
   nwc = 1
   IF ( Ito==2 .OR. Ito==3 ) nwc = 2
   IF ( Ito==4 ) nwc = 4
   trl(1) = Outfil
   trl(2) = 0
   trl(3) = Nn
   trl(6) = 0
   trl(7) = 0
   IF ( Imeth==1 ) THEN
      j = 1
      ji = ig
   ELSEIF ( Imeth==2 ) THEN
      ig = iip + ni*2
      nf = 0
      ik = 1
      ifil = qhhl
      jj = 2*ni + 1
      i = 1
      GOTO 800
   ENDIF
 1300 DO
!
!     MAKE A COLUMN INTO MATRIX
!
      DO ilop = 1 , ncm
         CALL unpack(*1320,qhhl,Z(ji))
         GOTO 1340
 1320    n = ncm*nwc
         DO ij = 1 , n
            Z(ji+ij-1) = 0.0
         ENDDO
 1340    ji = ji + ncm*nwc
      ENDDO
      CALL pack(Z(ig),Outfil,trl)
      IF ( Imeth==1 ) THEN
         IF ( j==ncol ) THEN
            CALL close(qhhl,1)
            CALL close(Outfil,1)
            CALL wrttrl(trl)
            GOTO 1100
         ELSE
            j = j + 1
            ji = ig
         ENDIF
      ELSEIF ( Imeth==2 ) THEN
         GOTO 900
      ELSE
         EXIT
      ENDIF
   ENDDO
 1400 WRITE (Out,99002) Ufm
99002 FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
   GOTO 1600
 1500 CALL mesage(-3,ifil,ns)
 1600 CALL mesage(-61,0,ns)
99999 RETURN
END SUBROUTINE fa1k