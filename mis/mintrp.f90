
SUBROUTINE mintrp(Ni,Xi,Nd,Xd,Type,Symm1,Symk1,Dz,Infile,Outfil,Scr,Scr1,G,Ncore,Nogo,Ipres)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER A(7) , B(7) , C(7) , D(7) , Ii , In , Incr , Incru , Iout , Ipre , Isab , Isc , Ita , Iti , Ito , Lcore , Ma(7) , Mc(7) ,&
         & Nmat , Nn , Nnn , Nt , Nwords , Scrm , Sysbuf
   DOUBLE PRECISION Ai , Ar
   COMPLEX Alpha(2)
   REAL Dum(48)
   COMMON /mpyadx/ A , B , C , D , Nwords , Nt , Isab , Isc , Ipre , Scrm
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /saddx / Nmat , Lcore , Ma , Ita , Alpha , Dum , Mc
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Iout , In , Nnn , Incru
!
! Dummy argument declarations
!
   REAL Dz
   INTEGER Infile , Ipres , Ncore , Nd , Ni , Nogo , Outfil , Scr , Scr1 , Symk1 , Symm1 , Type
   REAL G(1) , Xd(2) , Xi(1)
!
! Local variable declarations
!
   INTEGER buff , gpoint , i , isng , ity , j , jj , k , kd , kt , name(2) , ncol , nii
   LOGICAL nimag , spec
!
! End of declarations
!
!
!
!
   EQUIVALENCE (Alpha(1),Ar) , (Alpha(2),Ai)
!
   DATA name/4HMINT , 4HRP  /
!
!-----------------------------------------------------------------------
!
   spec = .FALSE.
   Nogo = 0
!
!     DETERMINE TYPE OF CALL FOR G
!     NEGATIVE VALUE FOR KO CALL LSPLIN, POSITIVE CALL SSPLIN
!
!
!     CHECK CORE NEED AT LEAST 1 BUFFER + G
!
   ity = iabs(Type)
   kd = 0
   IF ( ity>3 ) kd = 1
   ncol = (1+kd)*Nd
   IF ( Sysbuf+ncol*Ni>Ncore ) CALL mesage(-8,0,name)
!
!     PROTECT AGAINST BAD CALL
   IF ( Symk1<0 ) Symk1 = -1
   IF ( Symm1<0 ) Symm1 = -1
   IF ( Symk1>0 ) Symk1 = 1
   IF ( Symm1>0 ) Symm1 = 1
!     TRANSPOSE FLAG ON
   kt = 1
!     SPECIAL CASE
   IF ( Nd==1 .AND. ity<4 ) THEN
!
!     TEST FOR SPECIAL CASE
!
      nii = 2*Ni
      k = 0
      DO i = 1 , nii , 2
         k = k + 1
         IF ( Xi(i)==Xd(1) .AND. Xi(i+1)==Xd(2) ) GOTO 200
      ENDDO
   ENDIF
   IF ( Type<0 ) THEN
      nii = 2*Ni
      DO i = 1 , nii , 2
         Xi(i) = 0.0
      ENDDO
      nii = 2*Nd
      DO i = 1 , nii , 2
         Xd(i) = 0.0
      ENDDO
      CALL lsplin(Ni,Xi,Nd,Xd,Symk1,kd,kt,Dz,-1.0,-1.0,1.0,G,Ncore,isng)
      IF ( isng==2 ) GOTO 400
   ELSE
      CALL ssplin(Ni,Xi,Nd,Xd,Symm1,Symk1,kd,kt,Dz,G,Ncore,isng)
      IF ( isng==2 ) GOTO 400
   ENDIF
!     PUT OUT G
   buff = Ncore - Sysbuf + 1
   nimag = .TRUE.
   IF ( ity==3 .OR. ity==6 ) nimag = .FALSE.
   IF ( .NOT.(nimag) ) THEN
      Iti = Scr
      Scr = Outfil
      Outfil = Iti
   ENDIF
   Ito = 1
   jj = ncol
   Iti = 1
   Nn = Ni
   B(3) = Ni
   B(5) = 1
   gpoint = 1
 100  Incr = 1
   j = 1
   Ii = 1
   B(1) = Scr
   B(2) = 0
   B(4) = 2
   B(6) = 0
   B(7) = 0
   CALL gopen(Scr,G(buff),1)
   DO i = j , jj
      CALL pack(G(gpoint),Scr,B)
      gpoint = gpoint + Ni
   ENDDO
   CALL close(Scr,1)
   CALL wrttrl(B)
   IF ( .NOT.(spec) ) THEN
!
!     MULT INFILE BY G
!
      C(1) = 0
      A(1) = Infile
      CALL rdtrl(A)
      D(1) = Outfil
      D(3) = A(3)
      D(4) = 2
      D(5) = A(5)
      IF ( ity==2 .OR. ity==5 ) D(5) = 1
      IF ( D(5)==1 .AND. A(5)==4 ) D(5) = 2
      Nwords = Ncore
      Nt = 0
      Isab = 1
      Ipre = Ipres
      Scrm = Scr1
      CALL mpyad(G,G,G)
      CALL wrttrl(D)
      IF ( .NOT.(nimag) ) THEN
!
!     IMAG PART ONLY WANTED
!
         Nmat = 1
         Lcore = Ncore
         Ma(1) = Outfil
         CALL rdtrl(Ma)
         Ita = 3
         Alpha(1) = (0.0,-1.0)
         Mc(1) = Scr
         Mc(2) = Ma(2)
         Mc(3) = Ma(3)
         Mc(4) = 2
         Mc(5) = Ma(5)
         Mc(6) = 0
         Mc(7) = 0
         Ai = -1.0D0
         IF ( Ma(5)==4 ) Ita = 4
         IF ( Ita==4 ) Ar = 0.0D0
         CALL sadd(G,G)
         CALL wrttrl(Mc)
      ENDIF
   ENDIF
   GOTO 99999
!
!     PACK OUT COLUMN OF INFILE
!
 200  A(1) = Infile
   CALL rdtrl(A)
   buff = Ncore - Sysbuf + 1
   CALL gopen(Infile,G(buff),0)
   Incru = 1
   In = 1
   Nnn = A(3)
   Iout = A(5)
   IF ( k/=1 ) THEN
      k = k - 1
      CALL skprec(Infile,k)
   ENDIF
   CALL unpack(*300,Infile,G)
   CALL close(Infile,1)
   spec = .TRUE.
   Scr = Outfil
   Iti = A(5)
   Nn = A(3)
   jj = 1
   gpoint = 1
   IF ( ity==3 ) gpoint = 2
   Ito = 1
   IF ( ity==1 ) Ito = 3
   IF ( A(5)==4 ) Ito = Ito + 1
   B(3) = A(3)
   B(5) = Ito
   GOTO 100
 300  CALL mesage(-7,0,name)
 400  Nogo = 1
99999 END SUBROUTINE mintrp
