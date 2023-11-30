
SUBROUTINE alg19(Log1,Log2,Log3,Log5,Nlines,Nspec,Kpts,Rsta,Xsta,R,Zr,B1,B2,Tc,Pi,C1,Nblade,Ccord,Block,Alpb,Epslon,Ifangs,Ipunch,  &
               & Naero)
   IMPLICIT NONE
   INTEGER Iprtc
   COMMON /ud3prt/ Iprtc
   REAL C1 , Pi
   INTEGER Ipunch , Log1 , Log2 , Log3 , Log5 , Naero , Nblade , Nlines , Nspec
   REAL Alpb(10,21) , B1(1) , B2(1) , Block(10,21) , Ccord(1) , Epslon(10,21) , R(10,21) , Rsta(21,10) , Tc(1) , Xsta(21,10) , Zr(1)
   INTEGER Ifangs(1) , Kpts(1)
   REAL ac(21) , aonc , beta1 , beta2 , camber , dadd , deltad(21) , dev(10,21) , devfr , devv(10,5) , dm(11,5) , do , doo(5) ,     &
      & dvfrac(11,5) , dx(10) , f137b(8) , f137s(5) , f142tc(7) , f161d(8,5) , f164xb(8) , f172k(7) , f195m(8,2) , q , rdata(6) ,   &
      & rdte(21) , rr(21,10) , rte(5) , sol(21) , solid , speed , thick , x(10) , x1 , xb , xj , xkdt , xkshpe , xloss(21,10) , xms
   INTEGER i , idata(24) , j , k , kk , l1 , l2 , lmax , lnct , nblad(10) , ncurve(10) , ndatr , ndel(10) , ndpts , neval(10) ,     &
         & nl1(10) , nl2(10) , nle , nliter(10) , nloss(10) , nmach(10) , nout1(10) , nout2(10) , nout3(10) , nr(10) , nrad ,       &
         & nswitc , nte , nterp(10) , nx
!
   DATA f137b/0.0 , 10.0 , 20.0 , 30.0 , 40.0 , 50.0 , 60.0 , 70.0/
   DATA f137s/0.4 , 0.8 , 1.2 , 1.6 , 2.0/
   DATA f142tc/0.0 , 0.02 , 0.04 , 0.06 , 0.08 , 0.10 , 0.12/
   DATA f161d/0.0 , 0.009 , 0.17 , 0.29 , 0.42 , 0.59 , 0.79 , 1.05 , 0.0 , 0.12 , 0.30 , 0.51 , 0.75 , 1.05 , 1.47 , 2.07 , 0.0 ,  &
      & 0.16 , 0.33 , 0.61 , 0.95 , 1.42 , 2.12 , 3.07 , 0.0 , 0.17 , 0.40 , 0.72 , 1.11 , 1.71 , 2.62 , 3.95 , 0.0 , 0.2 , 0.44 ,  &
      & 0.78 , 1.21 , 1.90 , 3.01 , 4.75/
   DATA f195m/0.17 , 0.173 , 0.179 , 0.189 , 0.206 , 0.232 , 0.269 , 0.310 , 0.25 , 0.255 , 0.261 , 0.268 , 0.278 , 0.292 , 0.312 , &
      & 0.342/
   DATA f164xb/0.965 , 0.945 , 0.921 , 0.890 , 0.850 , 0.782 , 0.679 , 0.550/
   DATA f172k/0.0 , 0.160 , 0.331 , 0.521 , 0.74 , 1.0 , 1.300/
!
   lmax = 60
   CALL fread(Log1,idata,6,1)
   nrad = idata(1)
   ndpts = idata(2)
   ndatr = idata(3)
   nswitc = idata(4)
   nle = idata(5)
   nte = idata(6)
   CALL fread(Log1,rdata,2,1)
   xkshpe = rdata(1)
   speed = rdata(2)
   CALL fread(Log1,idata,3,1)
   nout1(nle) = idata(1)
   nout2(nle) = idata(2)
   nout3(nle) = idata(3)
   IF ( Iprtc==1 ) WRITE (Log2,99001) nrad , ndpts , ndatr , nswitc , nle , nte , xkshpe , speed , nle , nout1(nle) , nout2(nle) ,  &
                                    & nout3(nle)
99001 FORMAT (1H1,9X,'DATA INTERFACING ROUTINE - DEVIATION CALCULATIONS',' AND DATA FORMATTING',/10X,69(1H*),/10X,5HINPUT,/10X,     &
            & 5(1H*),//10X,6HNRAD =,I3,9H  NDPTS =,I3,9H  NDATR =,I3,11H  NSWITCH =,I2,7H  NLE =,I2,7H  NTE =,I3,//10X,8HXKSHPE =,  &
            & F7.4,9H  SPEED =,F9.1,//10X,'AT LEADING EDGE ','(STATION,I3,9H) NOUT1 =',I2,9H  NOUT2 =,I2,9H  NOUT3 =,I2)
   lnct = 10
   k = nle + 1
   DO i = k , nte
      CALL fread(Log1,idata,14,1)
      nr(i) = idata(1)
      nterp(i) = idata(2)
      nmach(i) = idata(3)
      nloss(i) = idata(4)
      nl1(i) = idata(5)
      nl2(i) = idata(6)
      neval(i) = idata(7)
      ncurve(i) = idata(8)
      nliter(i) = idata(9)
      ndel(i) = idata(10)
      nout1(i) = idata(11)
      nout2(i) = idata(12)
      nout3(i) = idata(13)
      nblad(i) = idata(14)
      IF ( lnct+6+nr(i)>lmax ) THEN
         IF ( Iprtc/=0 ) WRITE (Log2,99013)
         lnct = 1
      ENDIF
      lnct = lnct + 6 + nr(i)
      IF ( Iprtc==1 ) WRITE (Log2,99002) i , nr(i) , nterp(i) , nmach(i) , nloss(i) , nl1(i) , nl2(i) , neval(i) , ncurve(i) ,      &
                           & nliter(i) , ndel(i) , nout1(i) , nout2(i) , nout3(i) , nblad(i)
99002 FORMAT (/10X,7HSTATION,I3,7H   NR =,I3,9H  NTERP =,I2,9H  NMACH =,I2,9H  NLOSS =,I2,7H  NL1 =,I3,7H  NL2 =,I3,9H  NEVAL =,I2, &
             &8HNCURVE =,I2,10H  NLITER =,I3,8H  NDEL =,I2,/22X,7HNOUT1 =,I2,9H  NOUT2 =,I2,9H  NOUT3 =,I2,9H  NBLAD =,I3)
      l1 = nr(i)
      DO j = 1 , l1
         CALL fread(Log1,rdata,2,1)
         rr(j,i) = rdata(1)
         xloss(j,i) = rdata(2)
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99003) (rr(j,i),xloss(j,i),j=1,l1)
99003 FORMAT (/14X,6HRADIUS,6X,15HLOSS DESCRIPTOR,//,(F20.4,F17.6))
   ENDDO
   IF ( lnct+7+ndpts>lmax ) THEN
      IF ( Iprtc/=0 ) WRITE (Log2,99013)
      lnct = 1
   ENDIF
   lnct = lnct + 2
   IF ( Iprtc==1 ) WRITE (Log2,99004) nrad
99004 FORMAT (/10X,28HDEVIATION FRACTION CURVES AT,I2,6H RADII)
   DO k = 1 , nrad
      IF ( lnct+5+ndpts>lmax ) THEN
         IF ( Iprtc/=0 ) WRITE (Log2,99013)
         lnct = 1
      ENDIF
      lnct = lnct + 5 + ndpts
      CALL fread(Log1,rte(k),1,1)
      DO j = 1 , ndpts
         CALL fread(Log1,rdata,2,1)
         dm(j,k) = rdata(1)
         dvfrac(j,k) = rdata(2)
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99005) rte(k) , (dm(j,k),dvfrac(j,k),j=1,ndpts)
99005 FORMAT (/10X,5HRTE =,F8.4,//15X,2HDM,10X,6HDVFRAC,//,(F20.5,F13.5))
   ENDDO
   DO j = 1 , ndatr
      CALL fread(Log1,rdata,3,1)
      rdte(j) = rdata(1)
      deltad(j) = rdata(2)
      ac(j) = rdata(3)
   ENDDO
   IF ( lnct+3+ndatr>lmax ) THEN
      IF ( Iprtc/=0 ) WRITE (Log2,99013)
      lnct = 1
   ENDIF
   lnct = lnct + 3 + ndatr
   IF ( Iprtc==1 ) WRITE (Log2,99006) (rdte(j),deltad(j),ac(j),j=1,ndatr)
99006 FORMAT (/15X,4HRDTE,6X,6HDELTAD,9X,2HAC,//,(F20.4,F11.3,F13.4))
   IF ( lnct+6+Nlines>lmax ) THEN
      IF ( Iprtc/=0 ) WRITE (Log2,99013)
      lnct = 1
   ENDIF
   lnct = lnct + 6 + Nlines
   IF ( Iprtc==1 ) WRITE (Log2,99007)
99007 FORMAT (/10X,7HRESULTS,/,10X,7(1H*))
   IF ( Iprtc==1 ) WRITE (Log2,99008)
99008 FORMAT (/5X,10HSTREAMLINE,5X,5HBETA1,6X,5HBETA2,5X,6HCAMBER,7X,3HT/C,8X,3HA/C,6X,8HSOLIDITY,4X,11HADDIT. DEVN,4X,             &
             &15HTOTAL DEVIATION,/)
   DO j = 1 , Nlines
      xj = j
      CALL alg15(Zr,B1,Nspec,xj,beta1,1,0)
      CALL alg15(Zr,B2,Nspec,xj,beta2,1,0)
      CALL alg15(Zr,Tc,Nspec,xj,thick,1,0)
      q = 1.0
      IF ( speed>0.0 ) q = -1.0
      camber = (beta1-beta2)*q
      solid = Ccord(j)*float(Nblade)/(Pi*(R(nle,j)+R(nte,j)))
      beta1 = beta1*q
      CALL alg15(f137b,f195m(1,nswitc),8,beta1,xms,1,0)
      CALL alg15(f137b,f164xb,8,beta1,xb,1,0)
      CALL alg15(f142tc,f172k,7,thick,xkdt,1,0)
      DO k = 1 , 5
         CALL alg15(f137b,f161d(1,k),8,beta1,doo(k),1,0)
      ENDDO
      CALL alg15(f137s,doo,5,solid,do,1,1)
      CALL alg15(rdte,deltad,ndatr,R(nte,j),dadd,1,0)
      CALL alg15(rdte,ac,ndatr,R(nte,j),aonc,1,0)
      sol(j) = solid
      dev(nte,j) = (dadd+do*xkshpe*xkdt+camber*solid**(-xb)*(xms+0.5*(aonc-0.5)))*q
      beta2 = beta2*q
      DO i = nle , nte
         CALL alg15(Rsta(1,i),Xsta(1,i),Kpts(i),R(i,j),x(i),1,0)
      ENDDO
      dx(nle) = 0.0
      k = nle + 1
      DO i = k , nte
         dx(i) = dx(i-1) + sqrt((x(i)-x(i-1))**2+(R(i,j)-R(i-1,j))**2)
      ENDDO
      x1 = dx(nte)
      DO i = k , nte
         dx(i) = dx(i)/x1
      ENDDO
      l2 = nte - nle - 1
      k = nle + 1
      DO l1 = 1 , nrad
         CALL alg15(dm(1,l1),dvfrac(1,l1),ndpts,dx(k),devv(k,l1),l2,0)
      ENDDO
      kk = nte - 1
      DO i = k , kk
         DO l1 = 1 , nrad
            doo(l1) = devv(i,l1)
         ENDDO
         CALL alg15(rte,doo,nrad,R(nte,j),devfr,1,0)
         dev(i,j) = dev(nte,j)*devfr
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99009) j , beta1 , beta2 , camber , thick , aonc , solid , dadd , dev(nte,j)
99009 FORMAT (I11,F14.3,2F11.3,2F11.4,F12.5,F14.4,F17.4)
   ENDDO
   IF ( Ifangs(nle)/=0 ) THEN
      IF ( Naero/=0 ) THEN
         idata(1) = Nlines
         idata(2) = 0
         idata(3) = 0
         idata(4) = 0
         idata(5) = 0
         idata(6) = 0
         idata(7) = 0
         idata(8) = 0
         idata(9) = 0
         idata(10) = 0
         idata(11) = 0
         idata(12) = 0
         idata(13) = nout1(nle)
         idata(14) = nout2(nle)
         idata(15) = nout3(nle)
         idata(16) = 0
         CALL write(Log5,idata,16,1)
         CALL write(Log5,0.0,1,1)
         DO j = 1 , Nlines
            rdata(1) = R(nle,j)
            rdata(2) = Alpb(nle,j)
            rdata(3) = 0.0
            rdata(4) = Epslon(nle,j)
            rdata(5) = 0.0
            rdata(6) = 0.0
            CALL write(Log5,rdata,6,1)
            rdata(1) = 0.0
            rdata(2) = 0.0
            rdata(3) = 0.0
            rdata(4) = 0.0
            CALL write(Log5,rdata,4,1)
         ENDDO
         IF ( Ipunch==0 ) GOTO 100
      ENDIF
      WRITE (Log3,99010) Nlines , nout1(nle) , nout2(nle) , nout3(nle)
99010 FORMAT (I3,11(2X,1H0),3I3,3H  0,/,4H 0.0)
      WRITE (Log3,99011) (R(nle,j),Alpb(nle,j),Epslon(nle,j),j=1,Nlines)
99011 FORMAT (2F12.7,12X,F12.7,24X,/,4H 0.0,44X)
   ENDIF
 100  DO i = k , nte
      DO j = 1 , Nlines
         rdte(j) = R(i,j)
      ENDDO
      CALL alg15(rr(1,i),xloss(1,i),nr(i),rdte,deltad,Nlines,0)
      nx = Log5
      IF ( Naero==0 ) nx = Log3
      IF ( nx==Log3 ) THEN
         WRITE (nx,99012) Nlines , nterp(i) , nmach(i) , nloss(i) , nl1(i) , nl2(i) , neval(i) , ncurve(i) , nliter(i) , ndel(i) ,  &
                        & nout1(i) , nout2(i) , nout3(i) , nblad(i) , speed , (R(i,j),Alpb(i,j),deltad(j),Epslon(i,j),Block(i,j),   &
                        & sol(j),dev(i,j),j=1,Nlines)
99012    FORMAT (2I3,3H  0,I3,3H  6,11I3,/,F12.3,/,(6F12.7,/,F12.7,36X))
      ELSE
         idata(1) = Nlines
         idata(2) = nterp(i)
         idata(3) = 0
         idata(4) = nmach(i)
         idata(5) = 6
         idata(6) = nloss(i)
         idata(7) = nl1(i)
         idata(8) = nl2(i)
         idata(9) = neval(i)
         idata(10) = ncurve(i)
         idata(11) = nliter(i)
         idata(12) = ndel(i)
         idata(13) = nout1(i)
         idata(14) = nout2(i)
         idata(15) = nout3(i)
         idata(16) = nblad(i)
         CALL write(Log5,idata,16,1)
         CALL write(Log5,speed,1,1)
         DO j = 1 , Nlines
            rdata(1) = R(i,j)
            rdata(2) = Alpb(i,j)
            rdata(3) = deltad(j)
            rdata(4) = Epslon(i,j)
            rdata(5) = Block(i,j)
            rdata(6) = sol(j)
            CALL write(Log5,rdata,6,1)
            rdata(1) = dev(i,j)
            rdata(2) = 0.0
            rdata(3) = 0.0
            rdata(4) = 0.0
            CALL write(Log5,rdata,4,1)
         ENDDO
      ENDIF
      DO WHILE ( nx/=Log3 )
         nx = Log3
         IF ( Naero==0 .OR. Ipunch==0 ) EXIT
         WRITE (nx,99012) Nlines , nterp(i) , nmach(i) , nloss(i) , nl1(i) , nl2(i) , neval(i) , ncurve(i) , nliter(i) , ndel(i) ,  &
                        & nout1(i) , nout2(i) , nout3(i) , nblad(i) , speed , (R(i,j),Alpb(i,j),deltad(j),Epslon(i,j),Block(i,j),   &
                        & sol(j),dev(i,j),j=1,Nlines)
      ENDDO
   ENDDO
99013 FORMAT (1H1)
END SUBROUTINE alg19