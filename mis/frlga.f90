
SUBROUTINE frlga(Dlt,Frl,Casecc,Dit,Pp,Lusetd,Nfreq,Nload,Frqset,Fol,Notrd)
   IMPLICIT NONE
   REAL Core(1) , Degra , Ovr(152) , Pi , Radeg , S4pisq , Twophi , Xx
   INTEGER Icore(14) , Ii , Incr , Iprec , It1 , It2 , Itl(3) , Jj , Ksystm(55) , Sysbuf
   COMMON /blank / Xx
   COMMON /condas/ Pi , Twophi , Radeg , Degra , S4pisq
   COMMON /frrdst/ Ovr , Itl
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Core
   INTEGER Casecc , Dit , Dlt , Fol , Frl , Frqset , Lusetd , Nfreq , Nload , Notrd , Pp
   REAL a , c , c1 , c2 , ceb , cosxl , cp , cz , dt , f , fx(2) , head(8) , p , reb , rp , rz , scale , sinxl , tau , theta , tk1 ,&
      & tk2 , xlama
   COMPLEX eb , pow , r1 , r2
   INTEGER file , i , i149 , ib , ibuf , icdty , ifl , iflag , ifrl , igust , igust1 , ihead(8) , iload , ip1 , ipos , isel , isil ,&
         & itabl , itld , itlist(13) , j , k , kk , l , lcore , llist , loadn , lvect , m , mcb(7) , name(6) , nbuf , nbuild ,      &
         & ndload , ndone , nsimpl , nsubl , nt , ntabl , nz , nz1
   INTEGER korsz
!
!     THIS ROUTINE GENERATES LOADS INCORE AT EACH FREQUENCY
!
!     WITH ENTRY POINTS - GUST1A AND FRRD1A
!                         ======     ======
!
   EQUIVALENCE (Core(1),Icore(1)) , (head(1),ihead(1),isil) , (head(2),a) , (head(3),tau) , (head(4),theta) , (Ksystm(1),Sysbuf) ,  &
    & (Ksystm(55),Iprec)
   DATA itlist/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA name/4HDLT  , 4HFRLG , 4HA    , 4HGUST , 4H1A   , 4HFRRD/
   DATA ifrl/4HFRL /
!
!     IDENTIFICATION OF VARIABLES
!
!     NFREQ  = NUMBER OF FREQ IN SELECTED FREQ SET
!     NDONE  =  NUMBER OF FREQUENCIES CURRENTLY BUILT FOR CUR LOAD
!     LLIST  = POINTER TO START OF LOAD TABLE
!     ITABL  = POINTER TO START OF LIST OF TABLES NEEDED FOR CURRENT
!              LOAD
!     ILOAD  = POINTER TO BEGINNING OF LOADS IN CORE
!     IFL    = POINTER TO VALUES OF FREQ  FUNCTIONS
!     NBUILD = NUMBER OF FREQUENCIES WHICH CAN BE BUILT AT ONCE
!     NLOAD  = NUMBER OF LOADS FOUND IN CASE CONTROL
!     LCORE  = AMOUNT OF CORE AVAILABLE TO HOLD  LOADS + F(F)-S
!     FRQSET = SELECT FREQUENCY SET ID
!     LOADN  = SELECTED DYNAMIC LOAD
!     NDLOAD = NUMBER OF DLOAD CARDS
!     NSIMPL = NUMBER OF SIMPLE LOADS
!     NSUBL  = NUMBEL OF  SIMPLE LOADS COMPOSING PRESENT LOAD
!     NTABL  = NUBER OF TABLE ID-S IN PRESENT LOAD
!     ICDTY  = CARD TYPE CODE  1=RLOAD1,  2=RLOAD2
!
!
   GOTO 100
!
!
   ENTRY gust1a(Dlt,Frl,Casecc,Dit,Pp,Lusetd,Nfreq,Nload,Frqset,Fol,Notrd)
!     =======================================================
!
   name(2) = name(4)
   name(3) = name(5)
   GOTO 100
!
!
   ENTRY frrd1a(Dlt,Frl,Casecc,Dit,Pp,Lusetd,Nfreq,Nload,Frqset,Fol,Notrd)
!     =======================================================
!
   name(2) = name(6)
   name(3) = name(5)
!
!
!     INITALIZE
!
 100  It1 = 3
   It2 = 2 + Iprec
   Ii = 1
   Jj = Lusetd
   Incr = 1
   Notrd = -1
   lcore = korsz(Core(1))
!
!     PICK UP AND STORE FREQUENCY SET
!
   ibuf = lcore - Sysbuf + 1
   nz1 = ibuf - 1
   lcore = lcore - 2*Sysbuf
   nz = lcore
   igust = 0
   IF ( Casecc<=0 ) THEN
      Casecc = iabs(Casecc)
      igust = 1
   ENDIF
   file = Casecc
   CALL open(*1900,Casecc,Core(ibuf),0)
   CALL fwdrec(*2100,Casecc)
   CALL fread(Casecc,Core,149,0)
   Frqset = Icore(14)
   Nload = 0
   loadn = Icore(13)
   CALL close(Casecc,1)
   Itl(1) = 2
   i149 = 149
   Itl(2) = Icore(i149)
   Itl(3) = Itl(2) + 1
   itld = 1
!
!     BRING IN AND SAVE FREQ LIST -- CONVERT  W-S TO F    F = TWOPHI* W
!
   file = Frl
   CALL open(*1900,Frl,Core(ibuf),0)
   CALL read(*2100,*200,Frl,Core(1),nz1,0,iflag)
   GOTO 2200
 200  DO i = 3 , iflag
      IF ( Icore(i)==Frqset ) GOTO 300
   ENDDO
   name(1) = ifrl
   CALL mesage(-31,Frqset,name)
 300  k = i - 3
   IF ( k/=0 ) THEN
      DO i = 1 , k
         CALL fwdrec(*2100,Frl)
      ENDDO
   ENDIF
!
!     READ IN  FREQ LIST
!
   CALL read(*2100,*400,Frl,Core(1),nz1,0,Nfreq)
   GOTO 2200
 400  CALL close(Frl,1)
   lcore = lcore - Nfreq
   nz1 = nz1 - Nfreq
   Frqset = k + 1
   llist = Nfreq + 1
!
!     CONVERT TO F
!
   DO i = 1 , Nfreq
      Core(i) = Core(i)/Twophi
   ENDDO
!
!     PUT HEADER ON LOAD FILE
!
   file = Pp
   nz = ibuf - Sysbuf
   nz1 = nz1 - Sysbuf
   CALL open(*1900,Pp,Core(nz),1)
   CALL fname(Pp,mcb(1))
   CALL write(Pp,mcb(1),2,0)
   CALL write(Pp,Core(1),Nfreq,1)
   file = Fol
   CALL open(*500,Fol,Core(ibuf),1)
   CALL fname(Fol,mcb)
   CALL write(Fol,mcb,2,0)
   CALL write(Fol,Core,Nfreq,1)
   CALL close(Fol,1)
   mcb(1) = Fol
   mcb(2) = Nfreq
   mcb(3) = Frqset
   CALL wrttrl(mcb)
!
!     SET UP MCB FOR PP
!
 500  mcb(1) = Pp
   mcb(2) = 0
   mcb(3) = Lusetd
   mcb(4) = 2
   mcb(5) = 2 + Iprec
   mcb(6) = 0
   mcb(7) = 0
 600  DO
!
!     BEGIN LOOP ON LOADS SELECTED
!
      IF ( Nload/=0 ) THEN
         file = Casecc
         CALL open(*1900,Casecc,Core(ibuf),0)
         l = Nload + 1
         DO i = 1 , l
            CALL fwdrec(*2100,Casecc)
         ENDDO
         CALL read(*1800,*2200,Casecc,Core(llist),16,1,iflag)
         loadn = Icore(llist+12)
         CALL close(Casecc,1)
      ENDIF
      Nload = Nload + 1
      IF ( loadn==0 ) THEN
!
!     BUILD ZERO LOAD
!
         DO i = 1 , Nfreq
            CALL bldpk(3,3,Pp,0,0)
            CALL bldpkn(Pp,0,mcb)
         ENDDO
      ELSE
         ndone = 0
         lcore = nz1
!
!     FIND SELECTED LOAD IN DLT
!
         file = Dlt
         CALL open(*1900,Dlt,Core(ibuf),0)
         CALL read(*2100,*700,Dlt,Core(llist),nz1,0,iflag)
         EXIT
      ENDIF
   ENDDO
!
!     IS IT A DLOAD SET
!
 700  ndload = Icore(llist+2)
   nsimpl = iflag - 3 - ndload
   IF ( nsimpl==0 ) CALL mesage(-31,loadn,name)
   IF ( ndload/=0 ) THEN
      k = llist + 2
      DO i = 1 , ndload
         k = k + 1
         IF ( Icore(k)==loadn ) GOTO 900
      ENDDO
   ENDIF
!
!     PROCESS SIMPLE LOAD REQUEST
!
   nsubl = 1
   Core(llist+1) = 1.0
   l = llist + 2 + ndload
   DO i = 1 , nsimpl
      l = l + 1
      IF ( Icore(l)==loadn ) GOTO 800
   ENDDO
   CALL mesage(-31,loadn,name)
!
!     FOUND SIMPLE LOAD  STORE RECORD NUMBER
!
 800  IF ( ndload/=0 ) i = i + 1
   Icore(llist) = i
   ipos = 1
   lcore = lcore - 2
   GOTO 1300
!
!     PROCESS DLOAD SET
!
!     FORMAT OF DLOAD CARD = SET ID, SCALE,SCALE,ID, SCALE, ID, ...,0,-1
!
 900  nz1 = nz1 - iflag
!
!     BRING IN ALL DLOADS
!
   l = llist + iflag
   CALL read(*2100,*1000,Dlt,Core(l),nz1,0,i)
   GOTO 2200
!
!     FIND SELECTED ID
!
 1000 isel = l
   DO WHILE ( Icore(isel)/=loadn )
      DO
         isel = isel + 2
         IF ( Icore(isel+1)==-1 ) THEN
            isel = isel + 2
            EXIT
         ENDIF
      ENDDO
   ENDDO
!
!     FOUND LOAD SET  SELECTED
!
   scale = Core(isel+1)
!
!     CONVERT  SCALE FACTORS TO OVERALL  SCALE +ID-S TO RECORD NUMBERS-1
!
   l = isel + 2
   nsubl = 0
 1100 Core(l) = Core(l)*scale
   k = llist + 2 + ndload
   DO i = 1 , nsimpl
      k = k + 1
      IF ( Icore(l+1)==Icore(k) ) GOTO 1200
   ENDDO
   CALL mesage(-31,Icore(l),name)
!
!     FOUND SIMPLE ID
!
 1200 Icore(l+1) = i + 1
   nsubl = nsubl + 1
   l = l + 2
   IF ( Icore(l+1)>=0 ) GOTO 1100
!
!     MOVE TO LOAD LIST AREA
!
   l = isel + 2
   k = llist
   DO i = 1 , nsubl
      Icore(k) = Icore(l+1)
      Core(k+1) = Core(l)
      l = l + 2
      k = k + 2
   ENDDO
!
!     BUILD LIST OF UNIQUE TABLES NEEDED FOR NSUBL LOADS
!
   ipos = 2
 1300 ntabl = 0
   itabl = llist + 2*nsubl
   DO i = 1 , nsubl
      k = llist + (i-1)*2
      j = Icore(k)
      l = j - ipos
      IF ( l/=0 ) THEN
         DO k = 1 , l
            CALL fwdrec(*2100,Dlt)
         ENDDO
      ENDIF
!
!     READ IN DESCRIPTOR WORDS
!
      ipos = j + 1
      CALL read(*2100,*2300,Dlt,head(1),8,1,iflag)
      icdty = ihead(1)
      nt = 4
      IF ( icdty==1 .OR. icdty==2 ) THEN
      ELSEIF ( icdty==4 ) THEN
!
!     TLOAD2 CARD
!
         Notrd = 1
         CYCLE
      ELSE
!
!     TLOAD 1 CARD
!
         nt = 3
         itld = 2
         Notrd = 1
      ENDIF
      DO m = 3 , nt
         IF ( ihead(m)/=0 ) THEN
            IF ( ntabl/=0 ) THEN
               DO k = 1 , ntabl
                  l = itabl + k
                  IF ( Icore(l)==ihead(m) ) GOTO 1350
               ENDDO
            ENDIF
!
!     STORE NEW TABLE ID
!
            ntabl = ntabl + 1
            k = itabl + ntabl
            Icore(k) = ihead(m)
         ENDIF
 1350 ENDDO
   ENDDO
   CALL rewind(Dlt)
   lcore = lcore - ntabl - 1
   iload = itabl + ntabl + 1
   Icore(itabl) = ntabl
!
!     ALLOCATE CORE
!
   lvect = 2*Lusetd
   nbuild = lcore/(lvect+ntabl*itld)
   nbuild = min0(nbuild,Nfreq)
   IF ( nbuild==0 ) GOTO 2200
   kk = ntabl*nbuild
   ifl = nz - ntabl*nbuild*itld
!
!     LOOP HERE FOR FREQUENCY SPILL
!
   lcore = lcore - ntabl*nbuild
   nbuf = lcore - Sysbuf
   IF ( ntabl==0 ) GOTO 1500
 1400 CALL pretab(Dit,Core(iload),Core(iload),Core(nbuf),nbuf,l,Core(itabl),itlist(1))
   DO j = 1 , ntabl
      l = itabl + j
      DO i = 1 , nbuild
         m = ndone + i
         k = ifl + nbuild*(j-1) + i - 1
         IF ( itld==2 ) THEN
!
!     TRANSFOR LOOK UP FOR TLOAD 1 CARDS
!
            CALL tab1(Core(l),Core(m),fx(1))
            Core(k) = fx(1)
            Core(k+kk) = fx(2)
         ELSE
!
!                 TAB      X       F(X)
            CALL tab(Core(l),Core(m),Core(k))
         ENDIF
      ENDDO
   ENDDO
!
!     READY CORE FOR BUILDING LOADS
!
 1500 k = iload - 1
   DO i = 1 , nbuild
      DO l = 1 , lvect
         k = k + 1
         Core(k) = 0.0
      ENDDO
   ENDDO
!
!     POSITION TO LOAD IN DLT
!
   ipos = 0
   DO i = 1 , nsubl
      k = llist + 2*i - 2
      l = Icore(k) - ipos
      scale = Core(k+1)
      IF ( l/=0 ) THEN
         DO j = 1 , l
            CALL fwdrec(*2100,Dlt)
         ENDDO
      ENDIF
!
!     READ IN 8 WORD LOAD ID
!
      ipos = l + 1 + ipos
      CALL read(*2100,*2200,Dlt,head(1),8,0,iflag)
      icdty = ihead(1)
      tk1 = head(3)
      tk2 = head(4)
      nt = 4
      IF ( icdty==1 .OR. icdty==2 ) THEN
      ELSEIF ( icdty==4 ) THEN
         GOTO 1550
      ELSE
         nt = 3
      ENDIF
!
!     FIND COEFFICIENTS IN TABLE LIST
!
      DO k = 3 , nt
         IF ( ihead(k)/=0 ) THEN
            DO l = 1 , ntabl
               m = itabl + l
               IF ( Icore(m)==ihead(k) ) GOTO 1520
            ENDDO
            GOTO 2300
         ELSE
            ihead(k+3) = -1
            CYCLE
         ENDIF
!
!     COMPUTE POINTER INTO COEF TABLE
!
 1520    ihead(k+3) = ifl + (l-1)*nbuild
         IF ( icdty==3 ) ihead(k+4) = ifl + (l-1)*nbuild + ntabl*nbuild
      ENDDO
!
!     REPEATLY READ IN  4  WORDS --SIL,A,TAU,THETA
!
 1550 igust1 = 0
 1600 IF ( igust/=0 ) THEN
         IF ( igust1==1 ) CYCLE
         igust1 = 1
      ENDIF
      CALL read(*2100,*1700,Dlt,ihead(1),4,0,iflag)
      IF ( igust/=0 ) THEN
         isil = 1
         a = 1.0
         tau = 0.0
         theta = 0.0
      ENDIF
      a = a*scale
      theta = theta*Degra
      DO j = 1 , nbuild
         IF ( icdty/=4 ) THEN
!
!     COMPUTE COEFFICIENTS
!
            c1 = 0.0
            IF ( ihead(6)>=0 ) THEN
               k = ihead(6) + j - 1
               c1 = Core(k)
            ENDIF
            c2 = 0.0
            IF ( ihead(7)>=0 ) THEN
               k = ihead(7) + j - 1
               c2 = Core(k)
            ENDIF
         ENDIF
         l = ndone + j
         m = (j-1)*lvect + 2*isil - 2 + iload
         IF ( icdty==2 ) THEN
!
!     RLOAD2  CARDS
!
            xlama = theta - Core(l)*tau*Twophi + c2*Degra
            Core(m) = a*c1*cos(xlama) + Core(m)
            Core(m+1) = a*c1*sin(xlama) + Core(m+1)
         ELSEIF ( icdty==4 ) THEN
!
!     TLOAD 2 CARDS
!
            f = head(5)
            p = head(6)*Degra
            c = head(7)
            ib = head(8) + .5
            dt = tk2 - tk1
            rz = -c*dt
            cz = -dt*(f-Core(l))*Twophi
!
!     COMPUTE  E(B+1) (ZR2)
!
            CALL frr1a1(rz,cz,ib+1,reb,ceb)
            eb = cmplx(reb,ceb)
            rp = -rz
            cp = p - Core(l)*Twophi*tk2 + Twophi*f*dt
            pow = cmplx(rp,cp)
            r2 = cexp(pow)*eb
!
!     COMPUTE  R1
!
            cz = -dt*(-f-Core(l))*Twophi
!
!     COMPUTE  E(B+1)ZR1
!
            CALL frr1a1(rz,cz,ib+1,reb,ceb)
            eb = cmplx(reb,ceb)
            cp = -p - Core(l)*Twophi*tk2 - Twophi*f*dt
            pow = cmplx(rp,cp)
            r1 = r2 + cexp(pow)*eb
!
!     COMPUTE   P(W)
            r2 = cmplx(0.,-Core(l)*tau*Twophi)
            pow = r1*cexp(r2)
            cp = (dt**(ib+1))/(2.0*(head(8)+1.))
            rz = real(pow)*a*cp
            cz = aimag(pow)*a*cp
            Core(m) = Core(m) + rz
            Core(m+1) = Core(m+1) + cz
         ELSE
!
!     RLOAD 1 CARDS OF TLOAD1 CARDS
!
            xlama = theta - Core(l)*tau*Twophi
            sinxl = sin(xlama)
            cosxl = cos(xlama)
            Core(m) = a*(c1*cosxl-c2*sinxl) + Core(m)
            Core(m+1) = a*(c1*sinxl+c2*cosxl) + Core(m+1)
         ENDIF
      ENDDO
      GOTO 1600
!
!     END OF STUFF IN DLT TABLE
!
 1700 ENDDO
!
!     PACK OUT LOADS BUILT
!
   DO i = 1 , nbuild
      m = (i-1)*lvect + iload
      CALL pack(Core(m),Pp,mcb(1))
   ENDDO
   ndone = ndone + nbuild
   nbuild = min0(nbuild,Nfreq-ndone)
   CALL rewind(Dlt)
   IF ( nbuild/=0 ) GOTO 1400
   CALL close(Dlt,1)
   GOTO 600
!
!     EOF  ON CASECC  END OF ROUTINE
!
 1800 CALL close(Casecc,1)
   CALL wrttrl(mcb(1))
   CALL close(Pp,1)
   RETURN
!
!     ERROR MESAGES
!
 1900 ip1 = -1
 2000 CALL mesage(ip1,file,name(2))
 2100 ip1 = -2
   GOTO 2000
 2200 ip1 = -8
   GOTO 2000
 2300 ip1 = -7
   GOTO 2000
END SUBROUTINE frlga
