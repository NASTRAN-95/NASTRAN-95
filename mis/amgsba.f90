
SUBROUTINE amgsba(Ajjl,A0,Ar,Nsbe,A,Yb,Zb)
   IMPLICIT NONE
   INTEGER Ecore , Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Ii , Inas , Inasb , Inb , Inbea1 , Inbea2 , &
         & Inc , Incr , Infl , Ins , Insbea , Int121 , Int122 , Iria , Isg , Isk , Ith1a , Ith2a , Iti , Ito , Ix , Ixic , Ixij ,   &
         & Ixis1 , Ixis2 , Ixlam , Ixle , Ixte , Iyb , Iyin , Iys , Izb , Izin , Izs , Mcb(7) , Nb , Nby , Nbz , Nd , Ne , Next ,   &
         & Nj1 , Nk1 , Nn , Np , Nrow , Nsk , Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs , Scr1 , Scr2 , Scr5 , Sysbuf
   REAL Fmach , Pi , Refc , Rfk , Scr3 , Scr4 , Tskj(7) , Twopi , Z(1)
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /condas/ Pi , Twopi
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER Ajjl
   REAL A(1) , A0(1) , Ar(1) , Yb(1) , Zb(1)
   INTEGER Nsbe(1)
   REAL den
   INTEGER i , ib , ibuf1 , ibuf2 , ibuf3 , it , j , name(2) , nbuf , nfsbeb , nfyb , nl , nlsbeb , ns1 , ns2 , ns5 , ntot
!
!     BUILD AJJL FOR DOUBLET LATTICE WITH BODIES
!
   DATA name/4HAMGS , 4HBA  /
   Ii = Nrow + 1
   Nn = Nrow + Nj1
   IF ( Next+2*Nj1>Ecore ) CALL mesage(-8,0,name)
   Incr = 1
   Iti = 3
   Ito = 3
   IF ( Nt0/=0 ) THEN
      nbuf = 1
      IF ( Ntzs/=0 ) nbuf = nbuf + 1
      IF ( Ntys/=0 ) nbuf = nbuf + 1
      ibuf1 = Ecore - nbuf*Sysbuf
      ibuf2 = ibuf1
      IF ( Ntzs/=0 ) ibuf2 = ibuf1 + Sysbuf
      ibuf3 = ibuf2
      IF ( Ntys/=0 ) ibuf3 = ibuf2 + Sysbuf
      ns5 = Nt0*2
      ns1 = Ntzs*2
      ns2 = Ntys*2
      ntot = ns5 + ns1 + ns2
      IF ( Next+ntot>ibuf3 ) CALL mesage(-8,0,name)
!
!     BUILD PANEL AND BODY PART OF AJJL
!
      CALL gopen(Scr5,Z(ibuf1),0)
      IF ( Ntzs/=0 ) CALL gopen(Scr1,Z(ibuf2),0)
      IF ( Ntys/=0 ) CALL gopen(Scr2,Z(ibuf3),0)
      DO i = 1 , Nt0
         CALL fread(Scr5,A,ns5,0)
         IF ( Ntzs/=0 ) CALL fread(Scr1,A(ns5+1),ns1,0)
         IF ( Ntys/=0 ) CALL fread(Scr2,A(ns5+ns1+1),ns2,0)
         CALL pack(A,Ajjl,Mcb)
      ENDDO
      CALL close(Scr5,1)
      CALL close(Scr1,1)
      CALL close(Scr2,1)
   ENDIF
   CALL zeroc(A,2*Nj1)
!
!     ADD DIAGIONAL TERMS OF AJJL FOR SLENDER BODIES
!
   IF ( Ntzs/=0 .OR. Ntys/=0 ) THEN
      i = Nt0*2 + 1
      den = Twopi*2.0
      IF ( Ntzs/=0 ) THEN
         nfsbeb = 1
         nlsbeb = 0
         DO ib = 1 , Nbz
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0+float(Nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0+float(Ne))*A(i)
               CALL pack(A,Ajjl,Mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
      IF ( Ntys/=0 ) THEN
         nfyb = Nb + 1 - Nby
         nfsbeb = 1
         nlsbeb = 0
         nl = nfyb - 1
         IF ( nl/=0 ) THEN
            DO j = 1 , nl
               nlsbeb = nlsbeb + Nsbe(j)
               nfsbeb = nfsbeb + Nsbe(j)
            ENDDO
         ENDIF
         DO ib = nfyb , Nb
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0-float(Nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0-float(Ne))*A(i)
               CALL pack(A,Ajjl,Mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE amgsba
