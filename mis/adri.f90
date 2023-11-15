
SUBROUTINE adri(Fl,Nfreq,Ncore,Qhhl,Scr2,Scr1,Scr3,Scr4,Nrow,Ncol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Bov , Dum(52) , P(2) , Pi , Rm , Twopi
   INTEGER Ii , Incr , Incr1 , Inn , Iout , Iprec , Isys , Iti , Ito , Iwc(4) , Nn , Nnn , Out
   CHARACTER*23 Ufm
   COMMON /blank / Bov , Rm
   COMMON /condas/ Pi , Twopi
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Isys , Out , Dum , Iprec
   COMMON /type  / P , Iwc
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Ncol , Ncore , Nfreq , Nogo , Nrow , Qhhl , Scr1 , Scr2 , Scr3 , Scr4
   REAL Fl(1)
!
! Local variable declarations
!
   REAL flag , rmi , rms , rmx
   INTEGER i , ibuf1 , ibuf2 , icp , ik , im , ipd , ipi , j , ji , jj , k , kk , mcb(7) , n , name(2) , nc , nhfrdi , ni , nl ,    &
         & nwc , trl(7)
!
! End of declarations
!
!
   DATA nhfrdi , name/4HFRDI , 4HADRI , 4H    /
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
   CALL open(*300,Qhhl,Fl(ibuf2),0)
   CALL gopen(Scr1,Fl(ibuf1),1)
   CALL read(*100,*100,Qhhl,Fl(1),-2,0,flag)
   CALL read(*100,*100,Qhhl,Ncol,1,0,flag)
   CALL read(*100,*100,Qhhl,n,1,0,flag)
   n = n + n
   ni = (mcb(2)/Ncol)*2
   ni = min0(ni,n)
   Nnn = Nrow
   Nn = Ncol*Nrow
   Iti = 3
   Ito = Iti
   Iout = Iti
   nwc = Iwc(Iti)
   CALL makmcb(trl,Scr1,Nn,mcb(4),Ito)
!
!     MAKE   DEPENDENT FREQ LIST
!
   ipd = 1
   nl = 2*Nfreq
   n = Nfreq + 1
   ipi = ipd + nl
   DO i = 1 , Nfreq
      Fl(nl) = Fl(n-i)*Twopi*Bov
      Fl(nl-1) = 0.0
      nl = nl - 2
   ENDDO
!
!     MAKE INDEPENDENT FREQ LIST
!
   CALL read(*100,*100,Qhhl,Fl(ipi),ni,1,flag)
!
!     FIND M"S CLOSEST TO RM
!
   icp = ipi + ni
   rmi = 1.E20
   rms = 0.0
   DO i = 1 , ni , 2
      rmx = abs(Fl(ipi+i-1)-Rm)
      rmi = amin1(rmi,rmx)
      IF ( rmx<=rmi ) rms = Fl(ipi+i-1)
   ENDDO
   rmi = rms
!
!     DO ALL K"S ASSOCIATED WITH RMI
!
   k = 0
   DO i = 1 , ni , 2
      IF ( Fl(ipi+i-1)==rmi ) THEN
!
!     MAKE MATRIX INTO COLUMN
!
         Fl(ipi+k+1) = Fl(ipi+i)
         k = k + 2
         ji = icp
         n = Nrow*nwc
         DO j = 1 , Ncol
            CALL unpack(*10,Qhhl,Fl(ji))
            GOTO 20
 10         CALL zeroc(Fl(ji),n)
 20         ji = ji + n
         ENDDO
!
!     DIVIDE IMAG PART OF QHHL BY FREQUENCY
!
         jj = icp + 1
         kk = ji - 1
         DO j = jj , kk , 2
            Fl(j) = Fl(j)/Fl(ipi+i)
         ENDDO
         CALL pack(Fl(icp),Scr1,trl)
      ELSE
!
!     SKIP MATRIX
!
         CALL skprec(Qhhl,Ncol)
      ENDIF
   ENDDO
   CALL close(Qhhl,1)
   CALL close(Scr1,1)
   CALL wrttrl(trl)
   CALL bug(nhfrdi,150,k,1)
   CALL bug(nhfrdi,150,Nfreq,1)
   CALL bug(nhfrdi,150,Fl(1),icp)
!
!     SETUP TO CALL MINTRP
!
   ni = k/2
   Nogo = 0
   nc = Ncore - icp
   CALL dmpfil(-Scr1,Fl(icp),nc)
   im = 0
   ik = 1
   CALL mintrp(ni,Fl(ipi),Nfreq,Fl(ipd),-1,im,ik,0.0,Scr1,Scr2,Scr3,Scr4,Fl(icp),nc,Nogo,Iprec)
   IF ( Nogo==1 ) THEN
!
      WRITE (Out,99001) Ufm
99001 FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
!IBMR 6/93  GO TO 240                                                 !*
      GOTO 200
   ELSE
      CALL dmpfil(-Scr2,Fl(icp),nc)
      RETURN
   ENDIF
 100  CALL mesage(3,Qhhl,name)
 200  Nogo = 1
 300  CALL close(Qhhl,1)
END SUBROUTINE adri
