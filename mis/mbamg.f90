
SUBROUTINE mbamg(Input,Ajjl,Skj)
   IMPLICIT NONE
   REAL Ang(10) , Beta , Boxa , Boxl , Boxw , Cotang(10) , Cr , Ek , Ekbar , Ekm , Fmach , Gc , Mach , Refc , Rfk , Tang(10) ,      &
      & Tskj(7) , X(12) , Y(12) , Z(1)
   LOGICAL Asym , Cntrl1 , Cntrl2 , Crank1 , Crank2
   INTEGER Ii , Incr , Isk , It0 , Iti , Iz(1) , Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , Mcb(7) , Nbox , Ncb , Nd , Ne , Njj , Nn ,    &
         & Nout , Npts0 , Npts1 , Npts2 , Nrow , Nsb , Nsbd , Nsk , Ntote , Sysbuf
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /mboxa / X , Y , Tang , Ang , Cotang
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   COMMON /packx / Iti , It0 , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Ajjl , Input , Skj
   INTEGER buf1 , i , icore , icorr , icrq , ireg , is , l , na , name(2) , nc1 , nc21 , nc2n , ncap , ncaph , ncn , nd1 , ndn ,    &
         & ndss , nhcapf , nhcont , nhcore , nhxect , nkte , nkte1 , nkte2 , nparea , nphit , nq , nq1 , nq2 , nw1 , nwn , nxk ,    &
         & nxk1 , nxk2 , nxwte , nyk , nyk1 , nyk2 , nywte , scr2
   REAL cmax , rm
   INTEGER korsz
!
!     DRIVER FOR MACH BOX THEORY
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HMBAM , 4HG   /
   DATA nhcore , nhcapf , nhcont/4HCORE , 4HCAPF , 4HCONT/
   DATA scr2/302/
!
!     SCR2 CONTAINS THE INTERPOLATED POINTS
!
!     2 * KCT FOR NPTS0 POINTS
!     2 * KC1T FOR NPTS1 POINTS
!     2 * KC2T FOR NPTS2 POINTS
!
!
!     OPEN CORE POINTERS FIXED DIMENSIONS
!
   nw1 = 1
   nwn = 51
   nc21 = 101
   nc2n = 151
   nc1 = 201
   ncn = 251
   nd1 = 301
   ndn = 351
   nxk = 401
   nyk = 601
   nxk1 = 801
   nyk1 = 926
   nxk2 = 1051
   nyk2 = 1176
   nxwte = 1301
   nywte = 1351
   nkte = 1401
   nkte1 = 1451
   nkte2 = 1501
   nparea = 1551
   icorr = 9051
!
!     INITITALIZE  PUT HEADER DATA IN MBOXC
!
   icore = korsz(Iz) - 4*Sysbuf
   buf1 = icore - Sysbuf
   CALL fread(Input,Njj,9,0)
   Asym = .FALSE.
   IF ( Nd==-1 ) Asym = .TRUE.
   Mach = Fmach
   Beta = sqrt((Mach*Mach)-1.0)
   CALL fread(Input,Z,24,0)
!
!     MOVE X AND Y TO MBOXA
!
   l = 0
   DO i = 1 , 23 , 2
      l = l + 1
      X(l) = Z(i)
      Y(l) = Z(i+1)
   ENDDO
   CALL mbgeod
   Ek = (2.0*Cr/Refc)*Rfk
   cmax = amax1(X(4),X(5),X(6))
   Boxl = cmax/(float(Nbox)+0.50)
   Boxw = Boxl/Beta
   Nsb = Y(3)/Boxw + 0.5
   Nsb = min0(Nsb,50)
   Boxw = Y(3)/(float(Nsb)-0.50)
   Boxl = Boxw*Beta
   Ncb = cmax/Boxl + 0.999
!
!     CALL MBREG TO GENERATE BOXES
!
   icrq = icorr - buf1
   IF ( icorr>buf1 ) THEN
      CALL mesage(-8,icrq,name)
   ELSE
      DO
         CALL mbreg(ireg,Z(nw1),Z(nwn),Z(nc21),Z(nc2n),Z(nc1),Z(ncn),Z(nd1),Z(ndn),Z(nxk),Z(nyk),Z(nxk1),Z(nyk1),Z(nxk2),Z(nyk2),   &
                  & Z(nxwte),Z(nywte),Z(nkte),Z(nkte1),Z(nkte2),Z(nparea))
         IF ( ireg/=2 ) THEN
            CALL mbplot(Z(nw1),Z(nd1),Z(nwn),Z(nc21),Z(nc2n),Z(nc1),Z(ncn),Z(ndn))
!
!     CALL MBMODE TO GENERATE MODE LIKE DATA
!
            CALL gopen(scr2,Z(buf1),1)
            CALL mbmode(Input,scr2,icorr,buf1,Z,Npts0,Kct,Z(nxk),Z(nyk),is,Cr)
            IF ( is==2 ) GOTO 200
            IF ( Cntrl1 ) CALL mbmode(Input,scr2,icorr,buf1,Z,Npts1,Kc1t,Z(nxk1),Z(nyk1),is,Cr)
            IF ( is==2 ) GOTO 200
            IF ( Cntrl2 ) CALL mbmode(Input,scr2,icorr,buf1,Z,Npts2,Kc2t,Z(nxk2),Z(nyk2),is,Cr)
            IF ( is==2 ) GOTO 200
            CALL close(scr2,1)
            Ekbar = (Ek*Boxl*Mach*Mach)/(Beta*Beta)
            Ekm = Ekbar/Mach
            CALL fread(Input,0,0,1)
            CALL bug(nhcore,80,Z,nyk1-1)
            CALL bug(nhcore,80,Z(nyk1),nparea-nyk1)
            CALL dmpfil(scr2,Z(icorr),buf1-icorr)
!
!     MORE DIMENSIONS
!
            IF ( mod(icorr,2)==0 ) icorr = icorr + 1
            ncap = icorr
            ncaph = Ncb*(Ncb+1)/2
!
!     COMPLEX PHIS
!
            icorr = ncap + ncaph*2
            icrq = icorr - buf1
            IF ( icorr>buf1 ) THEN
               CALL mesage(-8,icrq,name)
            ELSE
               CALL mbcap(ncaph,Z(ncap))
               icorr = ncap + ncaph*2
               CALL bug(nhcapf,80,Z(ncap),ncaph*2)
!
!     PUT OUT SKJ
!
               Iti = 1
               It0 = 3
               Ii = Isk
               Nsk = Nsk + 1
               Nn = Nsk
               rm = 1.0
               DO i = 1 , Njj
                  CALL pack(rm,Skj,Tskj)
                  Ii = Ii + 1
                  IF ( i/=Njj ) Nn = Nn + 1
               ENDDO
               Isk = Ii
               Nsk = Nn
!
!     SET UP FOR COLUMN OF AJJL
!
               Iti = 3
               It0 = 3
               Ii = Nrow + 1
               Nn = Nrow + Njj
!
!     GET AJJL MATRIX TERMS
!     MORE DIMENSIONS
!
               nphit = icorr
               ndss = nphit + (3*Nsbd)*2
               nq = ndss + (Ncb*Nsbd)*2
               nq1 = nq + Kct*2
               nq2 = nq1 + Kc1t*2
               na = nq2 + Kc2t*2
               icorr = na + Njj*2
               CALL bug(nhxect,100,X,54)
               CALL bug(nhcont,100,Njj,30)
               icrq = icorr - buf1
               IF ( icorr>buf1 ) THEN
                  CALL mesage(-8,icrq,name)
               ELSE
                  CALL mbdpdh(Ajjl,Z(nxk),Z(nyk),Z(nxk1),Z(nyk1),Z(nxk2),Z(nyk2),Z(nxwte),Z(nywte),Z(nparea),Z(ncap),Z(nphit),      &
                            & Z(ndss),Z(nq),Z(nq1),Z(nq2),Z(ndn),Z(nd1),Z(nw1),Z(nwn),Z(nkte),Z(nkte1),Z(nkte2),Z(nc1),Ncb,Nsbd,    &
                            & scr2,Z(buf1),Z(na))
                  Nrow = Nrow + Njj
               ENDIF
            ENDIF
            EXIT
         ELSEIF ( Nbox<2 ) THEN
            WRITE (Nout,99001) Ufm
99001       FORMAT (A23,' 2425, MACH BOX GENERATION OF BOXES FAILED')
            CALL mesage(-37,0,name)
            CALL mesage(-8,icrq,name)
            EXIT
         ELSE
            Nbox = Nbox - 1
         ENDIF
      ENDDO
   ENDIF
 100  RETURN
!
!     ERROR MESSAGES
!
 200  WRITE (Nout,99002) Ufm
99002 FORMAT (A23,' 2424, MACH BOX CONTROL POINTS IMPROPER SINGULAR ','MATRIX RESULTED')
   CALL mesage(-37,0,name)
   CALL mesage(-8,icrq,name)
   GOTO 100
END SUBROUTINE mbamg
