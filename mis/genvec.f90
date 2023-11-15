
SUBROUTINE genvec(*,Ibuf,Filea,Nx,Ix,Ncol,B,Bbar,C,Cbar,R,Ientry)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(35) , Eofnrw , Rd , Rdrew , Rew , Tcons(15) , Wrt , Wrtrew , Xmb(2)
   INTEGER Eol , Ia(4) , Ii , Istv(65) , Isym , Lntime , Norew , Nout , P , Rsp , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /dcompx/ Dum , Isym
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp
   COMMON /ntime / Lntime , Tcons
   COMMON /system/ Istv
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zntpkx/ Ia , Ii , Eol
!
! Dummy argument declarations
!
   INTEGER B , Bbar , C , Cbar , Ientry , Ncol , Nx , R
   INTEGER Filea(1) , Ibuf(1) , Ix(2)
!
! Local variable declarations
!
   INTEGER bb , bbr , bbr1 , bbr2 , bmax , cc , ccr , ccr1 , ccr2 , cmax , dbname(2) , i , i1 , i2 , i3 , i4 , ib , ibbar , ic ,    &
         & icbar , icount , icrq , ifile , ilast , in1 , incrxx , ir , ixy , j , k , l , l11 , mmax , name(2) , namin(2,2) , nmax , &
         & no , rr1 , rr2 , rrr
   INTEGER findc
   REAL time , tt , tt1 , tt2
!
! End of declarations
!
!
!     GENVEC WILL PICK THE OPTIMUM VALUE OF B AND BBAR FOR A GIVEN
!     MATRIX
!
!     COMMON /DESCRP/ LENGTH    ,MAJOR(1)
   EQUIVALENCE (Istv(1),Sysbuf) , (Istv(2),Nout) , (Istv(55),P) , (Tcons(8),Xmb(1))
   DATA name/4HGENV , 4HEC  / , cmax/200/ , namin/4H REA , 1HL , 4HCOMP , 3HLEX/
!
!
   CALL fname(Filea,dbname)
   CALL sswtch(11,l11)
   IF ( l11/=0 ) WRITE (Nout,99001) Filea
99001 FORMAT ('O*** DIAG 11 OUTPUT FROM GENVEC (UNSYMMETRIC DECOMP) FOR',' FILE',I6,/9X,1HB,6X,4HBBAR,9X,1HC,6X,4HCBAR,9X,1HR,3X,   &
             &4HTIME)
!
   bmax = min0(ifix(1.0E+05/sqrt(float(Ncol)*Xmb(P))),Ncol)
   ifile = Filea(1)
   CALL open(*400,Filea(1),Ibuf,Rdrew)
   i1 = Ncol
   i4 = 4*Ncol + 2*cmax
   icrq = i4 - Nx + Sysbuf
   IF ( i4>Nx-Sysbuf ) GOTO 600
   DO i = 1 , i4
      Ix(i) = 0
   ENDDO
   nmax = 0
   mmax = 0
   CALL fwdrec(*500,Filea(1))
!
!     GENERATE THE ROW AND COLUMN VECTORS
!
   DO i = 1 , Ncol
      CALL intpk(*800,Filea(1),0,Rsp,0)
      CALL zntpki
      in1 = i1 + i
      Ix(in1) = Ii
      nmax = max0(nmax,i-Ii+1)
      DO
         IF ( Ix(Ii)==0 ) THEN
            Ix(Ii) = i
            mmax = max0(mmax,Ii-i+1)
         ENDIF
         IF ( Eol/=0 ) EXIT
         CALL zntpki
      ENDDO
   ENDDO
   CALL close(Filea(1),Rew)
   i2 = i1 + Ncol + 1
   i3 = i2 + 2*Ncol
   nmax = min0(nmax,bmax)
   mmax = min0(mmax,bmax)
   mmax = max0(mmax,2)
!
!     SET UP ACTIVE COLUMN BANDWIDTH VECTOR
!
   DO i = 2 , Ncol
      j = Ncol - i + 1
      icount = 0
      DO k = 1 , j
         l = i2 - k
         IF ( Ix(l)<i ) icount = icount + 1
         l = i2 + (j-k)*2
         Ix(l) = max0(Ix(l),icount)
      ENDDO
   ENDDO
!
!     REDUCE LIST TO UNIQUE PAIRS
!
   i = i2
   j = i2 + 2
   k = 2
   DO WHILE ( Ix(j)/=0 )
      IF ( Ix(j)/=Ix(i) ) THEN
         i = i + 2
         Ix(i) = Ix(j)
         Ix(i+1) = k
      ENDIF
      j = j + 2
      k = k + 1
   ENDDO
   i = i + 2
   Ix(i) = 0
   Ix(i+1) = k
   ilast = 0
!
!     BEGIN SEARCH FOR B,BBAR
!
   time = 1000000.
   B = 0
   Bbar = 0
   C = 0
   Cbar = 0
   DO
      bb = Ix(i+1)
      IF ( bb<=bmax ) THEN
!
!    MAKE PRELIMINARY SEARCH
!
         tt1 = 1000000.
         EXIT
      ELSE
         i = i - 2
      ENDIF
   ENDDO
 100  bb = Ix(i+1)
   cc = Ix(i) + 1
   IF ( cc==1 ) cc = 0
   bbr = bb
   ccr = cc
   CALL rcore(bb,bbr,cc,ccr,Ncol,Ientry,Nx,rrr)
   rrr = min0(rrr,bb+bbr-1,Ncol-1)
   IF ( rrr>=2 ) THEN
      CALL timeeq(float(bb),float(bbr),float(cc),float(ccr),float(rrr),Ientry,Ncol,tt)
      IF ( ilast==0 ) ilast = i
      IF ( l11/=0 ) WRITE (Nout,99005) bb , bbr , cc , ccr , rrr , tt
      IF ( tt<=tt1 ) THEN
         tt1 = tt
         bbr1 = bbr
         ccr1 = ccr
         rr1 = rrr
      ENDIF
   ENDIF
   i = i - 2
   IF ( bb>=3 ) THEN
      IF ( i>=i2+2 ) GOTO 100
   ENDIF
   i = i + 2
   IF ( tt1==1000000. ) GOTO 600
   bb = bbr1
   cc = ccr1
   tt1 = 1000000.
!
!     SEARCH ON INCREASING BBAR
!
 200  bbr = bb
   incrxx = max1(.02*float(bb),1.)
 300  ccr = findc(bb,bbr,Ncol,Ix(1),Ix(i3))
   CALL rcore(bb,bbr,cc,ccr,Ncol,Ientry,Nx,rrr)
   rrr = min0(rrr,bb+bbr-1)
   rrr = min0(rrr,Ncol-1)
   IF ( rrr>=2 ) THEN
      CALL timeeq(float(bb),float(bbr),float(cc),float(ccr),float(rrr),Ientry,Ncol,tt)
      IF ( l11/=0 ) WRITE (Nout,99005) bb , bbr , cc , ccr , rrr , tt
      IF ( tt1==1000000. ) tt1 = tt
      IF ( tt<=tt1 ) THEN
         tt1 = tt
         bbr1 = bbr
         ccr1 = ccr
         rr1 = rrr
      ENDIF
   ENDIF
   bbr = bbr + incrxx
   IF ( tt<=1.2*tt1 ) THEN
      IF ( ccr/=0 ) THEN
         IF ( bbr<bmax ) GOTO 300
      ENDIF
   ENDIF
!
!     BEGIN SEARCH ON DECREASING BBAR
!
   tt2 = 1000000.
   bbr = bb - incrxx
   DO WHILE ( bbr>2 )
      ccr = findc(bb,bbr,Ncol,Ix(1),Ix(i3))
      CALL rcore(bb,bbr,cc,ccr,Ncol,Ientry,Nx,rrr)
      rrr = min0(rrr,bb+bbr-1)
      rrr = min0(rrr,Ncol-1)
      IF ( rrr>=2 ) THEN
         CALL timeeq(float(bb),float(bbr),float(cc),float(ccr),float(rrr),Ientry,Ncol,tt)
         IF ( l11/=0 ) WRITE (Nout,99005) bb , bbr , cc , ccr , rrr , tt
         IF ( tt2==1000000. ) tt2 = tt
         IF ( tt<=tt2 ) THEN
            tt2 = tt
            bbr2 = bbr
            ccr2 = ccr
            rr2 = rrr
         ENDIF
      ENDIF
      bbr = bbr - incrxx
      IF ( tt>1.20*tt2 ) EXIT
   ENDDO
   IF ( tt1<time ) THEN
      time = tt1
      B = bb
      C = cc
      Bbar = bbr1
      Cbar = ccr1
      R = rr1
   ENDIF
   IF ( tt2<time ) THEN
      time = tt2
      B = bb
      C = cc
      Bbar = bbr2
      Cbar = ccr2
      R = rr2
   ENDIF
   IF ( tt1==1000000. .AND. tt2==1000000. ) THEN
!
!     TRY TO FIND POSSIBLE SOLUTION WITHIN FEASIBLE RANGE BY VARYING  BB
!
      i = i + 2
      IF ( i>ilast ) GOTO 600
      bb = Ix(i+1)
      cc = Ix(i) + 1
      IF ( bb<=bmax ) GOTO 200
      GOTO 600
   ELSE
      ib = B
      ic = C
      ibbar = Bbar
      icbar = Cbar
      ir = R
      Ix(1) = C
      Ix(2) = R
      CALL page2(4)
      WRITE (Nout,99002) Uim , B , Bbar , C , Cbar , R
99002 FORMAT (A29,' 3028',6X,3HB =,I5,5X,6HBBAR =,I5,/40X,3HC =,I5,5X,6HCBAR =,I5,/40X,3HR =,I5)
      CALL tfin(float(B),float(Bbar),float(C),float(Cbar),float(R),Ientry,float(Ncol),time)
      Ix(1) = time
      CALL page2(3)
      WRITE (Nout,99003) Uim , namin(1,Ientry) , namin(2,Ientry) , dbname , Ncol , Ix(1)
99003 FORMAT (A29,' 3027, UNSYMMETRIC ',2A4,' DECOMPOSITION OF DATA ','BLOCK ',2A4,6H (N = ,I5,1H),/5X,'TIME ESTIMATE = ',I8,       &
             &8H SECONDS)
      CALL tmtogo(ixy)
      IF ( ixy<Ix(1) ) CALL mesage(-50,Ix(1),name)
      RETURN
   ENDIF
 400  no = -1
   GOTO 700
 500  no = -2
   GOTO 700
 600  no = -8
   ifile = icrq
 700  CALL mesage(no,ifile,name)
   RETURN
!
!     NULL COLUMN DISCOVERED
!
 800  WRITE (Nout,99004) Ufm , i , namin(1,Ientry) , namin(2,Ientry)
99004 FORMAT (A23,' 3097, COLUMN',I7,' IS SINGULAR.  UNSYMMETRIC ',2A4,'DECOMP ABORTED.')
   RETURN 1
99005 FORMAT (5I10,F10.2)
!
END SUBROUTINE genvec
