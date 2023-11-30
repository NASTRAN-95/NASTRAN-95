
SUBROUTINE frd2i(Fl,Nfreq,Ncore,Qhhl,Scr2,Scr1,Scr3,Scr4,Nrow)
   IMPLICIT NONE
   REAL Bov , Dum(52) , P(2) , Pi , Q , Rm , Twopi
   INTEGER Ii , Incr , Incr1 , Inn , Iout , Iprec , Isys , Iti , Ito , Iwc(4) , Nn , Nnn , Out
   CHARACTER*23 Ufm
   COMMON /blank / Bov , Q , Rm
   COMMON /condas/ Pi , Twopi
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Isys , Out , Dum , Iprec
   COMMON /type  / P , Iwc
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /xmssg / Ufm
   INTEGER Ncore , Nfreq , Nrow , Qhhl , Scr1 , Scr2 , Scr3 , Scr4
   REAL Fl(1)
   REAL flag , rmi , rms , rmx , xm
   INTEGER i , ibuf1 , ibuf2 , icore , icp , ik , im , indx , ipd , ipi , iscr , j , ji , jj , k , kk , kkk , mcb(7) , n , name(2) ,&
         & nc , nhfrdi , ni , nl , nloop , nogo , nwc , trl(7)
!
   DATA name/4HFRD2 , 4HI   /
   DATA nhfrdi/4HFRDI/
!
   ibuf1 = Ncore - Isys
   ibuf2 = ibuf1 - Isys
   Nrow = 0
   Incr = 1
   Incr1 = 1
   Ii = 1
   Inn = 1
   mcb(1) = Qhhl
   CALL rdtrl(mcb)
   IF ( mcb(1)<0 ) GOTO 300
   Nrow = mcb(3)
   ni = (mcb(2)/mcb(3))*2
   Nnn = Nrow
   Nn = Nrow*Nrow
   Iti = 3
   Ito = Iti
   Iout = Iti
   nwc = Iwc(Iti)
   iscr = Scr1
   nloop = 1
   indx = 0
   xm = Rm
   IF ( Rm<0.0 ) THEN
      iscr = Scr2
      nloop = Nfreq
      indx = 1
   ENDIF
   CALL makmcb(trl,iscr,Nn,mcb(4),Ito)
!
!     MAKE INDEPENDENT FREQ LIST
!
   ipd = 1
   nl = 2*Nfreq
   n = Nfreq + 1
   icore = ibuf1
   ipi = ipd + nl
   DO i = 1 , Nfreq
      Fl(nl) = Fl(n-i)*Twopi*Bov
      Fl(nl-1) = 0.0
      nl = nl - 2
   ENDDO
!
!     MAKE INDEPENDENT FREQ LIST
!
   CALL open(*300,Qhhl,Fl(ibuf2),0)
   CALL gopen(iscr,Fl(ibuf1),1)
   CALL read(*100,*100,Qhhl,Fl(ipi),-3,0,flag)
   CALL read(*100,*100,Qhhl,n,1,0,flag)
   n = n + n
   IF ( Rm<0.0 .AND. n/=ni ) THEN
      WRITE (Out,99001) Ufm , n , ni
99001 FORMAT (A23,', THE NUMBER OF (M,K) PAIRS SPECIFIED ON MKAEROX ','CARDS (',I5,') IS NOT EQUAL ',/5X,                           &
             &'TO THE NUMBER OF FREQUENCIES SPECIFIED (',I5,'),')
      CALL mesage(-37,0,name)
   ENDIF
   ni = min0(ni,n)
   CALL read(*100,*100,Qhhl,Fl(ipi),ni,1,flag)
   IF ( Rm<0.0 ) CALL close(Qhhl,1)
!
   DO kkk = 1 , nloop
      IF ( Rm<0.0 ) THEN
         xm = Fl(2*kkk)
         CALL gopen(Qhhl,Fl(ibuf2),0)
      ENDIF
!
!     FOR RM.GE.0.0, FIND M CLOSEST TO XM
!     FOR RM.LT.0.0, FIND K CLOSEST TO XM
!
      icp = ipi + ni
      rmi = 1.E20
      rms = 0.0
      DO i = 1 , ni , 2
         rmx = abs(Fl(ipi+i+indx-1)-xm)
         rmi = amin1(rmi,rmx)
         IF ( rmx<=rmi ) rms = Fl(ipi+i+indx-1)
      ENDDO
      rmi = rms
!
!     FOR RM.GE.0.0, SELECT ALL K'S ASSOCIATED WITH RMI
!     FOR RM.LT.0.0, SELECT THE K EQUAL TO RMI
!
      k = 0
      DO i = 1 , ni , 2
         IF ( Fl(ipi+i+indx-1)==rmi ) THEN
!
!     MAKE MATRIX INTO COLUMN
!
            Fl(ipi+k+1) = Fl(ipi+i)
            k = k + 2
            ji = icp
            n = Nrow*nwc
            DO j = 1 , Nrow
               CALL unpack(*5,Qhhl,Fl(ji))
               GOTO 10
 5             CALL zeroc(Fl(ji),n)
 10            ji = ji + n
            ENDDO
!
!     DIVIDE IMAG PART OF QHHL BY FREQUENCY
!
            jj = icp + 1
            kk = ji - 1
            DO j = jj , kk , 2
               Fl(j) = Fl(j)/Fl(ipi+i)
            ENDDO
            IF ( Rm<0.0 ) Fl(ipi+i) = -10000.0
            CALL pack(Fl(icp),iscr,trl)
            IF ( Rm<0.0 ) EXIT
         ELSE
!
!     SKIP MATRIX
!
            CALL skprec(Qhhl,Nrow)
         ENDIF
      ENDDO
      CALL close(Qhhl,1)
      CALL close(iscr,1)
   ENDDO
!
   CALL wrttrl(trl)
   CALL bug(nhfrdi,200,k,1)
   CALL bug(nhfrdi,200,Nfreq,1)
   CALL bug(nhfrdi,200,Fl(1),icp)
   IF ( Rm<0.0 ) RETURN
!
!     SETUP TO CALL MINTRP
!
   ni = k/2
   nogo = 0
   nc = Ncore - icp
   CALL dmpfil(-Scr1,Fl(icp),nc)
   im = 0
   ik = 1
   CALL mintrp(ni,Fl(ipi),Nfreq,Fl(ipd),-1,im,ik,0.0,Scr1,Scr2,Scr3,Scr4,Fl(icp),nc,nogo,Iprec)
   IF ( nogo==1 ) THEN
!
      WRITE (Out,99002) Ufm
99002 FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
      GOTO 200
   ELSE
      CALL dmpfil(-Scr2,Fl(icp),nc)
      RETURN
   ENDIF
 100  CALL mesage(-3,Qhhl,name)
 200  CALL mesage(-61,0,name)
 300  CALL close(Qhhl,1)
END SUBROUTINE frd2i