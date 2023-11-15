
SUBROUTINE gkam
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1) , Degra , Hfreq , Lfreq , Pi , Radeg , S4pisq , Twophi
   INTEGER Icore(2) , Ii , Ii1 , Incr , Incr1 , It1 , It2 , It3 , Jj , Jj1 , Kdamp , Nlmode , Nmode , Nob2dd , Nok2dd , Nom2dd ,    &
         & Noncup , Noue , Nout , Sysbuf
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Noue , Nlmode , Lfreq , Hfreq , Nom2dd , Nob2dd , Nok2dd , Noncup , Nmode , Kdamp
   COMMON /condas/ Pi , Twophi , Radeg , Degra , S4pisq
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ It3 , Ii1 , Jj1 , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER b2dd , bhh , casecc , i , i149 , iblock(11) , icrq , iflag , ip1 , ip2 , isw , k2dd , khh , lama , lc1 , lhset , m2dd ,  &
         & mcb(7) , mhh , mi , modes , name(2) , nosdt , nvect , nz , phia , phidh , phidh1 , scr1 , scr2 , scr3 , sdt , usetd
   REAL block(11) , f , file
   INTEGER korsz
!
! End of declarations
!
!
!     ROUTINE TO ASSEMBLE MODAL MATRICES
!
!     INPUTS = 9
!
!     USETD,PHIA,MI,LAMA,SDT,M2DD,B2DD,K2DD,CASECC
!
!     OUTPUTS = 4
!
!     MHH,BHH,KHH,PHIDH
!
!     SCRATCHES = 4
!
!
   EQUIVALENCE (Core(1),Icore(1)) , (iblock(1),block(1))
!
   DATA name/4HGKAM , 4H    /
   DATA iblock(1) , iblock(7) , block(2) , block(8)/1 , 1 , 1.0 , 1.0/
   DATA usetd , phia , mi , lama , sdt , m2dd , b2dd , k2dd/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108/
   DATA mhh , bhh , khh , phidh/201 , 202 , 203 , 204/
   DATA scr1 , scr2 , scr3 , phidh1 , casecc/301 , 302 , 303 , 304 , 109/
!
!
!     PICK UP AND STORE SELECTED MODES, SAVING EIGENVECTORS
!
   lc1 = korsz(Core)
   nz = lc1 - Sysbuf
   icrq = 2*Sysbuf - nz
   IF ( icrq>0 ) THEN
      ip1 = -8
      file = icrq
      CALL mesage(ip1,ip2,name)
      GOTO 200
   ELSE
!
!     FIND SELECTED SDT INTO CASECC
!
      CALL gopen(casecc,Core(nz+1),0)
      CALL fread(casecc,Icore,166,1)
      CALL close(casecc,1)
      i149 = 149
      nosdt = Icore(i149)
!
!     OPEN  LAMA, PHIA, AND PHI0H
!
      CALL gopen(lama,Core(nz+1),0)
      CALL skprec(lama,1)
      nz = nz - Sysbuf
      CALL gopen(phia,Core(nz+1),0)
      Icore(1) = phia
      CALL rdtrl(Icore)
      nvect = Icore(2)
      nz = nz - Sysbuf
      IF ( Noue<0 ) phidh1 = phidh
      CALL gopen(phidh1,Core(nz+1),1)
      mcb(1) = phia
      CALL rdtrl(mcb)
      mcb(1) = phidh1
      It1 = mcb(5)
      It2 = It1
      It3 = It1
      Incr = 1
      Incr1 = 1
      Ii = 1
      Ii1 = 1
      Jj = mcb(3)
      Jj1 = Jj
      mcb(2) = 0
      mcb(6) = 0
      mcb(7) = 0
      isw = 1
      modes = 1
      DO i = 1 , nvect
         CALL read(*200,*100,lama,Core(nz-6),7,0,iflag)
!
!     PICK UP FREQUENCY
!
         f = Core(nz-2)
         IF ( Nlmode==0 ) THEN
!
!     FREQUENCY RANGE SPECIFICATION
!
            IF ( f>Hfreq ) EXIT
            IF ( f<Lfreq ) THEN
               CALL skprec(phia,1)
               isw = isw + 1
               CYCLE
            ENDIF
         ENDIF
!
!     ACCEPT LAMA
!
         Core(modes) = f*Twophi
         modes = modes + 1
         CALL unpack(*300,phia,Core(modes))
         CALL pack(Core(modes),phidh1,mcb)
         IF ( Nlmode/=0 ) THEN
            IF ( modes>Nlmode ) EXIT
         ENDIF
      ENDDO
   ENDIF
!
!     DONE
!
 100  CALL close(lama,1)
   CALL close(phia,1)
   CALL close(phidh1,1)
   CALL wrttrl(mcb)
!
!     BUILD PHIDH
!
   lhset = modes - 1
   Nmode = isw
   IF ( lhset<=0 ) THEN
!
!     NO MODES SELECTED
!
      ip1 = -47
      CALL mesage(ip1,ip2,name)
   ELSE
      IF ( Noue>=0 ) CALL gkam1b(usetd,scr1,scr2,phidh,phidh1,modes,Core,lhset,Noue,scr3)
!
!     FORM H MATRICES
!
      modes = modes - 1
!
!     SAVE MODES ON SCRATCH3 IN CASE DMI WIPES THEM OUT
!
      nz = lc1 - Sysbuf
      CALL open(*400,scr3,Core(nz+1),1)
      CALL write(scr3,Core(1),modes,1)
      CALL close(scr3,1)
      Noncup = 1
!
!     FORM  MHH
!
      CALL gkam1a(mi,phidh,sdt,scr1,scr2,1,mhh,Nom2dd,Core(1),modes,nosdt,lhset,m2dd,isw,scr3)
      IF ( Nom2dd>=0 ) CALL ssg2c(scr1,scr2,mhh,1,iblock(1))
!
!     FORM  BHH
!
      IF ( nosdt/=0 .OR. Nob2dd>=0 ) THEN
         CALL gkam1a(mi,phidh,sdt,scr1,scr2,2,bhh,Nob2dd,Core(1),modes,nosdt,lhset,b2dd,isw,scr3)
         IF ( Nob2dd>=0 ) CALL ssg2c(scr1,scr2,bhh,1,iblock(1))
      ENDIF
!
!     FORM  KHH
!
      CALL gkam1a(mi,phidh,sdt,scr1,scr2,3,khh,Nok2dd,Core(1),modes,nosdt,lhset,k2dd,isw,scr3)
      IF ( Nok2dd>=0 ) CALL ssg2c(scr1,scr2,khh,1,iblock(1))
      IF ( Nob2dd<0 .AND. Nom2dd<0 .AND. Nok2dd<0 ) Noncup = -1
      RETURN
   ENDIF
 200  DO
      ip2 = lama
      ip1 = -3
      CALL mesage(ip1,ip2,name)
   ENDDO
 300  WRITE (Nout,99001) Sfm
99001 FORMAT (A25,' 2204, UNPACK FOUND NULL COLUMN IN PHIA FILE IN ','GKAM MODULE.')
   ip1 = -37
   CALL mesage(ip1,ip2,name)
   GOTO 200
 400  ip2 = scr3
!
!     ERROR MESAGES
!
   ip1 = -1
   CALL mesage(ip1,ip2,name)
   GOTO 200
END SUBROUTINE gkam
