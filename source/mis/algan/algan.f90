!*==algan.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algan
   IMPLICIT NONE
   USE C_CONTRL
   USE C_SYSTEM
   USE C_UD3ANC
   USE C_UD3PRT
   USE C_UDSTR2
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alp , alphb , ang , area , ax , beta1 , beta2 , bx , c1 , chd , cosang , cosstg , dela , delu , dz , eps , f , ipx ,     &
         & ipy , ix , ixd , ixn , ixy , ixyn , iy , iyd , iyn , p , perspj , pi , pltsze , pres , q , rd , sb , scale , sinstg ,    &
         & sq , stackx , stager , t , t1 , t2 , tav3 , thck , torcon , ts , u , vol , w1 , w2 , x1 , x2 , xb , xd , xdel , xdum ,   &
         & xint , xj , xmn , xplot , xsign , xz , y1 , y2 , yd , ydel , yint , ymn , yone , yones , yplot , yy1 , yy2 , yzero ,     &
         & yzeros , z , zinner , zouter , zspmxt
   REAL , DIMENSION(10,21) :: alpb , blafor , block , epslon
   REAL , DIMENSION(21) :: ccord , perspt , rles , tcs , tes , tq , zq , zzs
   INTEGER :: i , idum , ifcord , ifplot , igrd1 , igrd2 , igrd3 , ii , iidum , ijdum , ikdum , inast , iprint , ipunch , irle ,    &
            & irt , irte , isbs , isecn , isplit , istak , j , jd , jloop , k , kpt , lnct , n , n1 , n2 , nblade , nd1 , nd2 ,     &
            & ndum , nelem , nlines , npoint , nsign , nspec , nstad , nstad1 , nstns , nstrd , nt , nz
   INTEGER , DIMENSION(24) :: idata
   REAL , DIMENSION(6) :: rdata
   REAL , DIMENSION(21,10) :: tharr , xcamb
   REAL , DIMENSION(18) :: title
   REAL , DIMENSION(21,70) :: xp , xs , yp , ys , zs
   EXTERNAL alg13 , alg14 , alg15 , alg17 , alg19 , fread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Title(1),Title2(1))
!
   pi = 4.0*atan(1.0)
   c1 = 180.0/pi
   CALL fread(Log1,Title2,18,1)
   IF ( Iprtc==1 ) WRITE (Log2,99001) Title2
99001 FORMAT (1H1,31X,'PROGRAM ALG - COMPRESSOR DESIGN - ANALYTIC MEAN','LINE BLADE SECTION',/32X,65(1H*),//10X,5HTITLE,25X,1H=,    &
            & 18A4)
   CALL fread(Log1,idata,17,1)
   nlines = idata(1)
   nstns = idata(2)
   nz = idata(3)
   nspec = idata(4)
   npoint = idata(5)
   nblade = idata(6)
   istak = idata(7)
   ipunch = idata(8)
   isecn = idata(9)
   ifcord = idata(10)
   ifplot = idata(11)
   iprint = idata(12)
   isplit = idata(13)
   inast = idata(14)
   irle = idata(15)
   irte = idata(16)
   nsign = idata(17)
   Nbldes = nblade
   IF ( Iprtc==0 ) iprint = 3
   IF ( inast==0 .AND. Ipgeom/=-1 ) inast = -4
   IF ( Iprtc==1 ) WRITE (Log2,99002) nlines , nstns , nz , nspec , npoint , nblade , istak , ipunch , isecn , ifcord , ifplot ,    &
                                    & iprint , isplit , inast , irle , irte , nsign
99002 FORMAT (10X,24HNUMBER OF STREAMSURFACES,6X,1H=,I3,/10X,18HNUMBER OF STATIONS,12X,1H=,I3,/10X,27HNUMBER OF CONSTANT-Z PLANES,  &
            & 3X,1H=,I3,/10X,27HNUMBER OF BLADE DATA POINTS,3X,1H=,I3,/10X,31HNUMBER OF POINTS ON SURFACES  =,I3,/10X,              &
             &29HNUMBER OF BLADES IN BLADE ROW,1X,1H=,I3,/10X,5HISTAK,25X,1H=,I3,/10X,6HIPUNCH,24X,1H=,I3,/10X,5HISECN,25X,1H=,I3,/,&
            & 10X,6HIFCORD,24X,1H=,I3,/10X,6HIFPLOT,24X,1H=,I3,/10X,6HIPRINT,24X,1H=,I3,/10X,6HISPLIT,24X,1H=,I3,/10X,5HINAST,25X,  &
             &1H=,I3,/10X,4HIRLE,26X,1H=,I3,/10X,4HIRTE,26X,1H=,I3,/10X,5HNSIGN,25X,1H=,I3)
   CALL fread(Log1,rdata,5,1)
   zinner = rdata(1)
   zouter = rdata(2)
   scale = rdata(3)
   stackx = rdata(4)
   pltsze = rdata(5)
   IF ( Iprtc==1 ) WRITE (Log2,99003) zinner , zouter , scale , stackx , pltsze
99003 FORMAT (/10X,6HZINNER,24X,1H=,F8.4,/10X,6HZOUTER,24X,1H=,F8.4,/10X,5HSCALE,25X,1H=,F8.4,/10X,6HSTACKX,24X,1H=,F8.4,/10X,      &
             &6HPLTSZE,24X,1H=,F8.4,//20X,36HSTREAMSURFACE GEOMETRY SPECIFICATION)
   lnct = 30
   DO i = 1 , nstns
      CALL fread(Log1,idata,2,1)
      Kpts(i) = idata(1)
      Ifangs(i) = idata(2)
      kpt = Kpts(i)
      DO k = 1 , kpt
         CALL fread(Log1,rdata,2,1)
         Xsta(k,i) = rdata(1)
         Rsta(k,i) = rdata(2)
      ENDDO
      IF ( Kpts(i)<2 ) THEN
         Kpts(i) = 2
         Xsta(2,i) = Xsta(1,i)
         Rsta(2,i) = Rsta(1,i) + 1.0
      ENDIF
      DO j = 1 , nlines
         CALL fread(Log1,rdata,2,1)
         R(i,j) = rdata(1)
         blafor(i,j) = rdata(2)
      ENDDO
      idum = Kpts(i)
      IF ( nlines>idum ) idum = nlines
      IF ( lnct>54-idum ) THEN
         IF ( Iprtc/=0 ) WRITE (Log2,99034)
         lnct = 2
      ENDIF
      lnct = lnct + idum + 7
      IF ( inast==0 ) THEN
         IF ( Iprtc==1 ) WRITE (Log2,99004) i , Kpts(i) , i , Ifangs(i)
99004    FORMAT (/10X,'COMPUTING STATION',I3,5X,'NUMBER OF DESCRIBING ','POINTS=',I3,6X,7HIFANGS(,I2,2H)=,I3,//6X,'DESCRIPTION',9X, &
                &'STREAMLINE',5X,5HRADII,/6X,1HX,9X,1HR,11X,6HNUMBER,//)
         DO k = 1 , idum
            IF ( Iprtc==1 .AND. k<=Kpts(i) .AND. k<=nlines ) WRITE (Log2,99035) Xsta(k,i) , Rsta(k,i) , k , R(i,k)
            IF ( Iprtc==1 .AND. k<=Kpts(i) .AND. k>nlines ) WRITE (Log2,99036) Xsta(k,i) , Rsta(k,i)
            IF ( Iprtc==1 .AND. k>Kpts(i) .AND. k<=nlines ) WRITE (Log2,99037) k , R(i,k)
         ENDDO
         IF ( inast==0 ) CYCLE
      ENDIF
      IF ( Iprtc==1 ) WRITE (Log2,99005) i , Kpts(i) , i , Ifangs(i)
99005 FORMAT (/10X,'COMPUTING STATION',I3,5X,'NUMBER OF DESCRIBING ','POINTS=',I3,6X,7HIFANGS(,I2,2H)=,I3,//6X,'DESCRIPTION',9X,    &
             &'STREAMLINE',5X,5HRADII,9X,'DELTA PRESSURE',/6X,1HX,9X,1HR,11X,6HNUMBER,//)
      DO k = 1 , idum
         IF ( Iprtc==1 .AND. k<=Kpts(i) .AND. k<=nlines ) WRITE (Log2,99035) Xsta(k,i) , Rsta(k,i) , k , R(i,k) , blafor(i,k)
         IF ( Iprtc==1 .AND. k<=Kpts(i) .AND. k>nlines ) WRITE (Log2,99036) Xsta(k,i) , Rsta(k,i)
         IF ( Iprtc==1 .AND. k>Kpts(i) .AND. k<=nlines ) WRITE (Log2,99037) k , R(i,k) , blafor(i,k)
      ENDDO
   ENDDO
   sq = 0.0
   sb = 0.0
   IF ( isecn==1 .OR. isecn==3 ) THEN
      IF ( lnct>50-2*nspec ) THEN
         IF ( Iprtc/=0 ) WRITE (Log2,99034)
         lnct = 1
      ENDIF
      lnct = lnct + 10 + 2*nspec
      DO j = 1 , nspec
         CALL fread(Log1,rdata,6,1)
         Zr(j) = rdata(1)
         B1(j) = rdata(2)
         B2(j) = rdata(3)
         Pp(j) = rdata(4)
         Qq(j) = rdata(5)
         Rle(j) = rdata(6)
         CALL fread(Log1,rdata,6,1)
         Tc(j) = rdata(1)
         Te(j) = rdata(2)
         Zz(j) = rdata(3)
         Cord(j) = rdata(4)
         Delx(j) = rdata(5)
         Dely(j) = rdata(6)
         CALL fread(Log1,rdata,2,1)
         S(j) = rdata(1)
         Bs(j) = rdata(2)
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99038) (Zr(j),B1(j),B2(j),Pp(j),Qq(j),Rle(j),Tc(j),Te(j),Zz(j),Cord(j),Delx(j),Dely(j),j=1,nspec)
      IF ( Iprtc==1 .AND. isecn==1 ) WRITE (Log2,99006) (Zr(j),S(j),Bs(j),j=1,nspec)
99006 FORMAT (/10X,'STREAMLINE  INFLECTION  INFLECTION',/11X,'NUMBER',8X,5HPOINT,7X,5HANGLE,//,(10X,F7.2,F14.5,F11.3))
      IF ( Iprtc==1 .AND. isecn==3 ) WRITE (Log2,99007) (Zr(j),S(j),Bs(j),j=1,nspec)
99007 FORMAT (/10X,'STREAMLINE  TRANSITION  DEL ANGLE',/11X,'NUMBER',8X,5HPOINT,6X,7HFROM LE,//,(10X,F7.2,F14.5,F11.3))
   ELSE
      DO isbs = 1 , nspec
         S(isbs) = 0.0
         Bs(isbs) = 0.0
      ENDDO
      IF ( lnct>54-nspec ) THEN
         IF ( Iprtc/=0 ) WRITE (Log2,99034)
         lnct = 1
      ENDIF
      lnct = lnct + nspec + 6
      DO j = 1 , nspec
         CALL fread(Log1,rdata,6,1)
         Zr(j) = rdata(1)
         B1(j) = rdata(2)
         B2(j) = rdata(3)
         Pp(j) = rdata(4)
         Qq(j) = rdata(5)
         Rle(j) = rdata(6)
         CALL fread(Log1,rdata,6,1)
         Tc(j) = rdata(1)
         Te(j) = rdata(2)
         Zz(j) = rdata(3)
         Cord(j) = rdata(4)
         Delx(j) = rdata(5)
         Dely(j) = rdata(6)
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99038) (Zr(j),B1(j),B2(j),Pp(j),Qq(j),Rle(j),Tc(j),Te(j),Zz(j),Cord(j),Delx(j),Dely(j),j=1,nspec)
   ENDIF
   IF ( isplit/=0 ) THEN
      DO j = 1 , nspec
         CALL fread(Log1,rdata,5,1)
         rles(j) = rdata(1)
         tcs(j) = rdata(2)
         tes(j) = rdata(3)
         zzs(j) = rdata(4)
         perspt(j) = rdata(5)
      ENDDO
      IF ( Iprtc==1 ) WRITE (Log2,99008)
99008 FORMAT (/20X,13HSPLITTER DATA,//10X,10HSTREAMLINE,2X,47HLE RADIUS MAX THICK TE THICK  POINT OF PER CENT,/11X,6HNUMBER,7X,     &
             &6H/CHORD,4X,6H/CHORD,3X,8H/2*CHORD,2X,9HMAX THICK,2X,8HSPLITTER,/)
      IF ( Iprtc==1 ) WRITE (Log2,99009) (Zr(j),rles(j),tcs(j),tes(j),zzs(j),perspt(j),j=1,nspec)
99009 FORMAT (10X,F7.2,3X,F8.3,F10.3,3F10.4)
   ENDIF
   IF ( ifplot/=0 .AND. ifplot/=4 ) THEN
      ikdum = 0
      IF ( B1(1)<0.0 ) ikdum = 1
      IF ( ifplot==1 .OR. ifplot==3 ) CALL alg17(istak,pltsze,1,title,ikdum,ifplot)
   ENDIF
   ndum = npoint
   iidum = isecn
   DO j = 1 , nlines
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            npoint = ndum
            isecn = iidum
            DO i = 1 , nstns
               kpt = Kpts(i)
               CALL alg15(Rsta(1,i),Xsta(1,i),kpt,R(i,j),Xhere(i),1,0)
            ENDDO
            X(1) = Xhere(1)
            X(100) = Xhere(nstns)
            ax = (X(100)-X(1))/99.0
            DO i = 2 , 99
               X(i) = X(i-1) + ax
            ENDDO
            CALL alg14(Xhere,R(1,j),nstns,X,xdum,Yprime,100,1)
            CALL alg14(Xhere,R(1,j),nstns,Xhere,xdum,Tanphi(1,j),nstns,1)
            Ss(1) = 0.0
            DO i = 2 , 100
               Ss(i) = Ss(i-1) + ax*sqrt(1.0+((Yprime(i)+Yprime(i-1))/2.0)**2)
            ENDDO
            xj = j
            CALL alg15(Zr,B1,nspec,xj,beta1,1,0)
            CALL alg15(Zr,B2,nspec,xj,beta2,1,0)
            CALL alg15(Zr,Pp,nspec,xj,p,1,0)
            CALL alg15(Zr,Qq,nspec,xj,q,1,0)
            CALL alg15(Zr,Rle,nspec,xj,yzero,1,0)
            CALL alg15(Zr,Tc,nspec,xj,t,1,0)
            CALL alg15(Zr,Te,nspec,xj,yone,1,0)
            CALL alg15(Zr,Delx,nspec,xj,xdel,1,0)
            CALL alg15(Zr,Dely,nspec,xj,ydel,1,0)
            CALL alg15(Zr,Zz,nspec,xj,z,1,0)
            CALL alg15(Zr,Cord,nspec,xj,chd,1,0)
            IF ( isecn/=0 .AND. isecn/=2 ) THEN
               CALL alg15(Zr,S,nspec,xj,sq,1,0)
               CALL alg15(Zr,Bs,nspec,xj,sb,1,0)
            ENDIF
            IF ( isplit/=0 ) THEN
               CALL alg15(Zr,rles,nspec,xj,yzeros,1,1)
               CALL alg15(Zr,tcs,nspec,xj,ts,1,1)
               CALL alg15(Zr,tes,nspec,xj,yones,1,1)
               CALL alg15(Zr,zzs,nspec,xj,zspmxt,1,1)
               CALL alg15(Zr,perspt,nspec,xj,perspj,1,1)
            ENDIF
            CALL alg15(X,Ss,100,stackx,bx,1,1)
            CALL alg13(j,ys,yp,xs,xp,Ysemi,Xsemi,Log1,Log2,npoint,iprint,beta1,beta2,p,q,yzero,t,yone,xdel,ydel,z,chd,lnct,ifcord,  &
                     & sq,sb,isecn,Xsemj,Ysemj,istak,Xhere,X,Ss,nstns,R,Xtemp,Yprime,Rad,Epz,bx,Sigma,ccord,isplit,yzeros,ts,yones, &
                     & zspmxt,perspj,inast,irle,irte,tharr)
            CALL alg15(X,Ss,100,stackx,bx,1,1)
            DO i = 1 , 100
               X(i) = X(i) - stackx
               Ss(i) = Ss(i) - bx
            ENDDO
            DO i = 1 , nstns
               Xhere(i) = Xhere(i) - stackx
            ENDDO
            IF ( ifplot/=0 .AND. ifplot/=2 .AND. ifplot/=4 ) THEN
               xplot = xs(j,1)*scale
               yplot = ys(j,1)*scale
               DO i = 2 , npoint
                  xplot = xs(j,i)*scale
                  yplot = ys(j,i)*scale
               ENDDO
               IF ( isecn==2 ) THEN
                  DO i = 2 , 30
                     xplot = Xsemj(j,i)*scale
                     yplot = Ysemj(j,i)*scale
                  ENDDO
               ENDIF
               DO ii = 1 , npoint
                  i = npoint - ii + 1
                  xplot = xp(j,i)*scale
                  yplot = yp(j,i)*scale
               ENDDO
               DO i = 2 , 30
                  xplot = Xsemi(j,i)*scale
                  yplot = Ysemi(j,i)*scale
               ENDDO
               xplot = xs(j,1)*scale
               yplot = ys(j,1)*scale
            ENDIF
            ijdum = 0
            DO i = 1 , nstns
               IF ( Ifangs(i)>=1 ) ijdum = 1
            ENDDO
            IF ( ijdum/=0 .OR. inast/=0 ) THEN
               CALL alg15(Ss,X,100,Xtemp,Xtemp,100,1)
               DO i = 1 , nstns
                  CALL alg15(Xtemp,Sigma,100,Xhere(i),Theta(j,i),1,1)
                  CALL alg15(Xtemp,Yprime,100,Xhere(i),Alpha(j,i),1,1)
                  Zcamb(j,i) = R(i,j)*cos(Theta(j,i))
                  xcamb(j,i) = Xhere(i)
                  Ycamb(j,i) = R(i,j)*sin(Theta(j,i))
               ENDDO
            ENDIF
            DO i = 1 , npoint
               Xtemp(i) = xs(j,i)
            ENDDO
            CALL alg15(Ss,X,100,Xtemp,Xtemp,npoint,1)
            CALL alg15(Xhere,R(1,j),nstns,Xtemp,Rad,npoint,0)
            k = 1
            DO i = 1 , npoint
               eps = Epz(i,k)
               zs(j,i) = Rad(i)*cos(eps)
               ys(j,i) = Rad(i)*sin(eps)
               xs(j,i) = Xtemp(i)
            ENDDO
            DO i = 1 , npoint
               Xtemp(i) = xp(j,i)
            ENDDO
            CALL alg15(Ss,X,100,Xtemp,Xtemp,npoint,1)
            CALL alg15(Xhere,R(1,j),nstns,Xtemp,Rad,npoint,0)
            k = 2
            DO i = 1 , npoint
               eps = Epz(i,k)
               Zp(j,i) = Rad(i)*cos(eps)
               yp(j,i) = Rad(i)*sin(eps)
               xp(j,i) = Xtemp(i)
            ENDDO
            DO i = 1 , 31
               Xtemp(i) = Xsemi(j,i)
            ENDDO
            CALL alg15(Ss,X,100,Xtemp,Xtemp,31,1)
            CALL alg15(Xhere,R(1,j),nstns,Xtemp,Rad,31,0)
            k = 3
            DO i = 1 , 31
               eps = Epz(i,k)
               Zsemi(j,i) = Rad(i)*cos(eps)
               Ysemi(j,i) = Rad(i)*sin(eps)
               Xsemi(j,i) = Xtemp(i)
            ENDDO
            IF ( isecn==2 ) THEN
               DO i = 1 , 31
                  Xtemp(i) = Xsemj(j,i)
               ENDDO
               CALL alg15(Ss,X,100,Xtemp,Xtemp,31,1)
               CALL alg15(Xhere,R(1,j),nstns,Xtemp,Rad,31,0)
               k = 4
               DO i = 1 , 31
                  eps = Epz(i,k)
                  Zsemj(j,i) = Rad(i)*cos(eps)
                  Ysemj(j,i) = Rad(i)*sin(eps)
                  Xsemj(j,i) = Xtemp(i)
               ENDDO
            ENDIF
            IF ( iprint>=2 ) CYCLE
            IF ( lnct>50 ) THEN
               IF ( Iprtc/=1 ) WRITE (Log2,99034)
               lnct = 1
            ENDIF
            lnct = lnct + 5
            IF ( Iprtc==1 ) WRITE (Log2,99010) j
99010       FORMAT (/10X,38HCARTESIAN COORDINATES ON STREAMSURFACE,I3,//10X,8HPOINT NO,5X,2HZ1,12X,2HX1,12X,2HY1,16X,2HZ2,12X,2HX2, &
                  & 12X,2HY2,/)
            i = 1
            SPAG_Loop_2_1: DO
               IF ( Iprtc==1 ) WRITE (Log2,99011) i , zs(j,i) , xs(j,i) , ys(j,i) , Zp(j,i) , xp(j,i) , yp(j,i)
99011          FORMAT (10X,I5,3X,1P,3E14.5,4X,1P,3E14.5)
               i = i + 1
               lnct = lnct + 1
               IF ( i>npoint ) THEN
                  IF ( lnct>50 ) THEN
                     IF ( Iprtc/=0 ) WRITE (Log2,99034)
                     lnct = 1
                  ENDIF
                  lnct = lnct + 3
                  IF ( isecn/=2 ) THEN
                     IF ( Iprtc==1 ) WRITE (Log2,99039)
                     i = 1
                     EXIT SPAG_Loop_2_1
                  ELSE
                     IF ( Iprtc==1 ) WRITE (Log2,99040)
                     i = 1
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( lnct>59 ) THEN
                  IF ( Iprtc==1 ) WRITE (Log2,99012)
99012             FORMAT (1H1,9X,8HPOINT NO,5X,2HZ1,12X,2HX1,12X,2HY1,16X,2HZ2,12X,2HX2,12X,2HY2,/)
                  lnct = 2
               ENDIF
            ENDDO SPAG_Loop_2_1
            spag_nextblock_1 = 2
         CASE (2)
            IF ( Iprtc==1 ) WRITE (Log2,99013) i , Zsemi(j,i) , Xsemi(j,i) , Ysemi(j,i)
99013       FORMAT (10X,I5,3X,1P,3E14.5)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         CASE (3)
            IF ( Iprtc==1 ) WRITE (Log2,99014) i , Zsemi(j,i) , Xsemi(j,i) , Ysemi(j,i) , Zsemj(j,i) , Xsemj(j,i) , Ysemj(j,i)
99014       FORMAT (10X,I5,3X,1P,3E14.5,4X,1P,3E14.5)
            spag_nextblock_1 = 4
         CASE (4)
            i = i + 1
            lnct = lnct + 1
            IF ( i<=31 ) THEN
               IF ( lnct<=59 .AND. isecn==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( isecn/=2 ) THEN
                  IF ( lnct>59 ) THEN
                     IF ( Iprtc/=0 ) WRITE (Log2,99034)
                     IF ( Iprtc==1 ) WRITE (Log2,99039)
                     lnct = 4
                  ENDIF
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( Iprtc/=0 ) WRITE (Log2,99034)
                  IF ( Iprtc==1 ) WRITE (Log2,99040)
                  lnct = 4
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   IF ( iprint/=1 ) THEN
      vol = 0.0
      DO j = 2 , nlines
         vol = vol + (((xs(j,1)-xp(j,1))**2+(ys(j,1)-yp(j,1))**2)+((xs(j-1,1)-xp(j-1,1))**2+(ys(j-1,1)-yp(j-1,1))**2))              &
             & *(zs(j,1)+Zp(j,1)-zs(j-1,1)-Zp(j-1,1))*pi/32.0
         DO i = 2 , npoint
            vol = vol + ((sqrt((xs(j,i)-xp(j,i))**2+(ys(j,i)-yp(j,i))**2)+sqrt((xs(j,i-1)-xp(j,i-1))**2+(ys(j,i-1)-yp(j,i-1))**2))  &
                & *(sqrt((xs(j,i-1)-xs(j,i))**2+(ys(j,i-1)-ys(j,i))**2)+sqrt((xp(j,i-1)-xp(j,i))**2+(yp(j,i-1)-yp(j,i))**2))        &
                & +(sqrt((xs(j-1,i)-xp(j-1,i))**2+(ys(j-1,i)-yp(j-1,i))**2)                                                         &
                & +sqrt((xs(j-1,i-1)-xp(j-1,i-1))**2+(ys(j-1,i-1)-yp(j-1,i-1))**2))                                                 &
                & *(sqrt((xs(j-1,i-1)-xs(j-1,i))**2+(ys(j-1,i-1)-ys(j-1,i))**2)                                                     &
                & +sqrt((xp(j-1,i-1)-xp(j-1,i))**2+(yp(j-1,i-1)-yp(j-1,i))**2)))*(zs(j,i)+zs(j,i-1)+Zp(j,i)+Zp(j,i-1)-zs(j-1,i)     &
                & -zs(j-1,i-1)-Zp(j-1,i)-Zp(j-1,i-1))/32.0
         ENDDO
      ENDDO
      IF ( lnct>56 ) THEN
         lnct = 1
         IF ( Iprtc/=0 ) WRITE (Log2,99034)
      ENDIF
      lnct = lnct + 4
      IF ( Iprtc==1 ) WRITE (Log2,99015) vol
99015 FORMAT (//40X,25HVOLUME OF BLADE SECTION =,1P,E11.4,/40X,36(1H*))
      IF ( ijdum/=0 ) THEN
         IF ( iprint/=3 ) WRITE (Log2,99034)
         IF ( iprint==3 ) WRITE (Log2,99016)
99016    FORMAT (//)
         IF ( Iprtc==1 ) WRITE (Log2,99017)
99017    FORMAT (1H1,42X,43HBLADE CALCULATIONS FOR AERODYNAMIC ANALYSIS,/43X,43(1H*))
         idum = 7
         lnct = lnct + 4
         IF ( iprint/=3 ) lnct = 3
         DO i = 1 , nstns
            IF ( .NOT.(Ifangs(i)==0 .OR. (isplit>=1 .AND. Ifangs(i)==1)) ) THEN
               DO j = 1 , nlines
                  CALL alg15(Rsta(1,i),Xsta(1,i),Kpts(i),R(i,j),xdum,1,0)
                  CALL alg14(Rsta(1,i),Xsta(1,i),Kpts(i),R(i,j),xdum,zq(j),1,1)
                  DO k = 1 , npoint
                     Ss(k) = xs(j,k)
                     Rad(k) = ys(j,k)
                     Xtemp(k) = xp(j,k)
                     X(k) = yp(j,k)
                  ENDDO
                  xdum = xdum - stackx
                  CALL alg15(Ss,Rad,npoint,xdum,yy1,1,1)
                  CALL alg15(Xtemp,X,npoint,xdum,yy2,1,1)
                  w1 = yy1/R(i,j)
                  w2 = yy2/R(i,j)
                  tq(j) = abs(atan(w1/sqrt(1.-w1**2))-atan(w2/sqrt(1.-w2**2)))/(2.*pi)*float(nblade)
               ENDDO
               CALL alg14(Zcamb(1,i),Ycamb(1,i),nlines,Zcamb(1,i),xdum,Rle,nlines,1)
               IF ( lnct+idum+nlines>59 ) THEN
                  IF ( Iprtc/=0 ) WRITE (Log2,99034)
                  lnct = 2
               ENDIF
               lnct = lnct + idum + nlines
               IF ( Iprtc==1 ) WRITE (Log2,99018) i , nlines
99018          FORMAT (///48X,8HSTATION ,I2,5X,17HNUMBER OF RADII= ,I2,//36X,6HRADIUS,5X,7HSECTION,6X,4HLEAN,9X,5HBLADE,7X,5HTHETA, &
                     & /48X,5HANGLE,6X,5HANGLE,7X,8HBLOCKAGE,/)
               DO j = 1 , nlines
                  eps = (Theta(j,i)-atan(Rle(j)))*c1
                  alphb = Alpha(j,i)
                  alp = (atan((Tanphi(i,j)*tan(eps/c1)+alphb*sqrt(1.+Tanphi(i,j)**2))/(1.-Tanphi(i,j)*zq(j))))*c1
                  alpb(i,j) = alp
                  epslon(i,j) = atan(tan(eps/c1)/sqrt(1.0+zq(j)**2))*c1
                  IF ( isplit>=1 ) THEN
                     CALL fread(Log1,rdata,4,1)
                     xb = rdata(4)
                     IF ( Iprtc==1 ) WRITE (Log2,99019) xb , i , j
99019                FORMAT (90X,14HADDIT. BLOCK =,F7.5,3H I=,I2,3H J=,I2)
                     tq(j) = tq(j) + xb
                  ENDIF
                  IF ( Iprtc==1 ) WRITE (Log2,99020) R(i,j) , alp , eps , tq(j) , Theta(j,i)
99020             FORMAT (30X,5F12.4)
                  block(i,j) = tq(j)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   IF ( ifplot>=2 .AND. ifplot/=4 ) CALL alg17(istak,pltsze,2,title,ikdum,ifplot)
   IF ( iprint/=1 .AND. iprint/=3 ) THEN
      lnct = 2
      IF ( Iprtc==1 ) WRITE (Log2,99021)
99021 FORMAT (1H1,27X,74HBLADE SURFACE GEOMETRY IN CARTESIAN COORDINATES AT SPECIFIED VALUES OF  Z ,/28X,18(4H****),2H**)
   ENDIF
   IF ( iprint/=1 .OR. ifplot>1 ) THEN
      xz = nz - 1
      dz = (zouter-zinner)/xz
      Zout(1) = zinner
      DO j = 3 , nz
         Zout(j-1) = Zout(j-2) + dz
      ENDDO
      Zout(nz) = zouter
      DO i = 1 , npoint
         CALL alg15(zs(1,i),xs(1,i),nlines,Zout,Temp1,nz,0)
         CALL alg15(zs(1,i),ys(1,i),nlines,Zout,Temp2,nz,0)
         CALL alg15(Zp(1,i),xp(1,i),nlines,Zout,Temp3,nz,0)
         CALL alg15(Zp(1,i),yp(1,i),nlines,Zout,Temp4,nz,0)
         DO j = 1 , nz
            xs(j,i) = Temp1(j)
            ys(j,i) = Temp2(j)
            xp(j,i) = Temp3(j)
            yp(j,i) = Temp4(j)
         ENDDO
      ENDDO
      DO i = 1 , 31
         CALL alg15(Zsemi(1,i),Xsemi(1,i),nlines,Zout,Temp1,nz,0)
         CALL alg15(Zsemi(1,i),Ysemi(1,i),nlines,Zout,Temp2,nz,0)
         DO j = 1 , nz
            Xsemi(j,i) = Temp1(j)
            Ysemi(j,i) = Temp2(j)
         ENDDO
      ENDDO
      IF ( isecn==2 ) THEN
         DO i = 1 , 31
            CALL alg15(Zsemj(1,i),Xsemj(1,i),nlines,Zout,Temp1,nz,0)
            CALL alg15(Zsemj(1,i),Ysemj(1,i),nlines,Zout,Temp2,nz,0)
            DO j = 1 , nz
               Xsemj(j,i) = Temp1(j)
               Ysemj(j,i) = Temp2(j)
            ENDDO
         ENDDO
      ENDIF
      DO j = 1 , nz
         rd = sqrt((xs(j,1)-xp(j,1))**2+(ys(j,1)-yp(j,1))**2)/2.0
         area = pi*rd**2/2.0
         beta1 = atan((ys(j,2)+yp(j,2)-ys(j,1)-yp(j,1))/(xs(j,2)+xp(j,2)-xs(j,1)-xp(j,1)))
         xint = area*((xp(j,1)+xs(j,1))/2.0-cos(beta1)*4.0/(3.0*pi)*rd)
         yint = area*((yp(j,1)+ys(j,1))/2.0-sin(beta1)*4.0/(3.0*pi)*rd)
         IF ( isecn==2 ) THEN
            n1 = npoint
            n = n1
            n2 = n1 - 1
            beta2 = atan((ys(j,n1)+yp(j,n1)-ys(j,n2)-yp(j,n2))/(xs(j,n1)+xp(j,n1)-xs(j,n2)-xp(j,n2)))
            xint = xint + area*((xp(j,n)+xs(j,n))/2.+cos(beta2)*4./(3.*pi)*rd)
            yint = yint + area*((yp(j,n)+ys(j,n))/2.+sin(beta2)*4./(3.*pi)*rd)
            area = 2.*area
         ENDIF
         DO i = 2 , npoint
            dela = (sqrt((xs(j,i)-xp(j,i))**2+(ys(j,i)-yp(j,i))**2)+sqrt((xs(j,i-1)-xp(j,i-1))**2+(ys(j,i-1)-yp(j,i-1))**2))        &
                 & *(sqrt((xs(j,i-1)-xs(j,i))**2+(ys(j,i-1)-ys(j,i))**2)+sqrt((xp(j,i-1)-xp(j,i))**2+(yp(j,i-1)-yp(j,i))**2))/4.0
            area = area + dela
            xint = xint + dela*(xs(j,i)+xs(j,i-1)+xp(j,i)+xp(j,i-1))/4.0
            yint = yint + dela*(ys(j,i)+ys(j,i-1)+yp(j,i)+yp(j,i-1))/4.0
         ENDDO
         yint = yint/area
         xint = xint/area
         x1 = (xs(j,1)+xp(j,1))/2.
         y1 = (ys(j,1)+yp(j,1))/2.
         t1 = sqrt((xs(j,1)-xp(j,1))**2+(ys(j,1)-yp(j,1))**2)
         f = 0.
         u = 0.
         DO i = 2 , npoint
            t2 = sqrt((xs(j,i)-xp(j,i))**2+(ys(j,i)-yp(j,i))**2)
            x2 = (xs(j,i)+xp(j,i))/2.
            y2 = (ys(j,i)+yp(j,i))/2.
            delu = sqrt((x2-x1)**2+(y2-y1)**2)
            u = u + delu
            tav3 = (t1**3+t2**3)/2.
            f = f + tav3*delu
            x1 = x2
            y1 = y2
            t1 = t2
         ENDDO
         torcon = ((1./3.)*f)/(1.+(4./3.)*f/area/u**2)
         ix = 0.0
         iy = 0.0
         ixy = 0.0
         DO i = 2 , npoint
            xd = (sqrt((xs(j,i-1)-xp(j,i-1))**2+(ys(j,i-1)-yp(j,i-1))**2)+sqrt((xs(j,i)-xp(j,i))**2+(ys(j,i)-yp(j,i))**2))/2.0
            yd = (sqrt((xs(j,i)-xs(j,i-1))**2+(ys(j,i)-ys(j,i-1))**2)+sqrt((xp(j,i)-xp(j,i-1))**2+(yp(j,i)-yp(j,i-1))**2))/2.0
            ixd = yd*yd*yd*xd/12.0
            iyd = xd*xd*xd*yd/12.0
            ang = atan((ys(j,i)+yp(j,i)-ys(j,i-1)-yp(j,i-1))/(xp(j,i)+xs(j,i)-xp(j,i-1)-xs(j,i-1)))
            cosang = cos(2.0*ang)
            ixn = (ixd+iyd+(ixd-iyd)*cosang)/2.0
            iyn = (ixd+iyd-(ixd-iyd)*cosang)/2.0
            ixyn = 0.0
            IF ( ang/=0.0 ) ixyn = ((ixn-iyn)*cosang-ixd+iyd)/(2.0*sin(2.0*ang))
            dela = xd*yd
            ymn = (ys(j,i)+ys(j,i-1)+yp(j,i)+yp(j,i-1))/4.0 - yint
            xmn = (xs(j,i)+xs(j,i-1)+xp(j,i)+xp(j,i-1))/4.0 - xint
            ix = ix + ixn + dela*ymn*ymn
            iy = iy + iyn + dela*xmn*xmn
            ixy = ixy + ixyn + dela*ymn*xmn
         ENDDO
         ang = atan(2.0*ixy/(iy-ix))
         ipx = (ix+iy)/2.0 + (ix-iy)/2.0*cos(ang) - ixy*sin(ang)
         ipy = (ix+iy)/2.0 - (ix-iy)/2.0*cos(ang) + ixy*sin(ang)
         ang = ang/2.0*c1
         IF ( iprint/=1 .AND. iprint/=3 ) THEN
            IF ( lnct>45 ) THEN
               IF ( Iprtc/=0 ) WRITE (Log2,99034)
               lnct = 1
            ENDIF
            lnct = lnct + 16
            IF ( Iprtc==1 ) WRITE (Log2,99022) j , Zout(j) , area , xint , yint , ix , iy , ixy , ipx , ang , ipy , ang
99022       FORMAT (/50X,14HSECTION NUMBER,I3,3X,5H Z  =,F9.4,/50X,34H**********************************,///20X,                    &
                   &18HSECTION PROPERTIES,7X,12HSECTION AREA,26X,1H=,1P,E12.4,//45X,20HLOCATION OF CENTROID,11X,4HXBAR,3X,1H=,E12.4,&
                  & /45X,22HRELATIVE TO STACK AXIS,9X,4HYBAR,3X,1H=,E12.4,//45X,22HSECOND MOMENTS OF AREA,9X,2HIX,5X,1H=,E12.4,/45X,&
                   &14HABOUT CENTROID,17X,2HIY,5X,1H=,E12.4,/76X,3HIXY,4X,1H=,E12.4,//45X,24HPRINCIPAL SECOND MOMENTS,7X,3HIPX,4X,  &
                   &1H=,E12.4,4H (AT,0P,F7.2,21H DEGREES TO  X  AXIS),/45X,22HOF AREA ABOUT CENTROID,9X,3HIPY,4X,1H=,1P,E12.4,      &
                  & 4H (AT,0P,F7.2,21H DEGREES TO  Y  AXIS))
            IF ( Iprtc==1 ) WRITE (Log2,99023) torcon
99023       FORMAT (/45X,18HTORSIONAL CONSTANT,20X,1H=,1P,E12.4,/)
            lnct = lnct + 3
            IF ( lnct>50 ) THEN
               IF ( Iprtc/=0 ) WRITE (Log2,99034)
               lnct = 1
            ENDIF
            lnct = lnct + 5
            IF ( Iprtc==1 ) WRITE (Log2,99024)
99024       FORMAT (/20X,19HSECTION COORDINATES,/)
            IF ( Iprtc==1 ) WRITE (Log2,99041)
            DO i = 1 , npoint
               lnct = lnct + 1
               IF ( lnct>60 ) THEN
                  lnct = 4
                  IF ( Iprtc/=0 ) WRITE (Log2,99034)
                  IF ( Iprtc==1 ) WRITE (Log2,99041)
               ENDIF
               IF ( Iprtc==1 ) WRITE (Log2,99042) i , xs(j,i) , ys(j,i) , xp(j,i) , yp(j,i)
            ENDDO
            IF ( lnct>55 ) THEN
               lnct = 1
               IF ( Iprtc/=0 ) WRITE (Log2,99034)
            ENDIF
            lnct = lnct + 3
            IF ( Iprtc==1 .AND. isecn==2 ) WRITE (Log2,99044)
            IF ( isecn/=2 ) THEN
               IF ( Iprtc==1 ) WRITE (Log2,99043)
            ENDIF
            DO i = 1 , 31
               lnct = lnct + 1
               IF ( lnct>60 ) THEN
                  IF ( Iprtc/=0 ) WRITE (Log2,99034)
                  IF ( Iprtc==1 .AND. isecn==2 ) WRITE (Log2,99044)
                  IF ( isecn/=2 ) THEN
                     IF ( Iprtc==1 ) WRITE (Log2,99043)
                  ENDIF
                  lnct = 4
               ENDIF
               IF ( Iprtc==1 .AND. isecn==2 ) WRITE (Log2,99042) i , Xsemi(j,i) , Ysemi(j,i) , Xsemj(j,i) , Ysemj(j,i)
               IF ( isecn/=2 ) THEN
                  IF ( Iprtc==1 ) WRITE (Log2,99025) i , Xsemi(j,i) , Ysemi(j,i)
99025             FORMAT (31X,I5,3X,1P,2E14.5)
               ENDIF
            ENDDO
         ENDIF
         IF ( ifplot>=2 ) THEN
            IF ( ifplot==4 ) THEN
               xj = j
               stager = atan((ys(j,npoint)+yp(j,npoint)-ys(j,1)-yp(j,1))/(xs(j,npoint)+xp(j,npoint)-xs(j,1)-xp(j,1)))*c1
               xsign = float(nsign)
               sinstg = sin(stager/c1)
               cosstg = cos(stager/c1)
               yplot = 4.75
               xplot = 4.75*sinstg/cosstg
               IF ( abs(xplot)>22.0 ) THEN
                  xplot = 22.0
                  yplot = -22.0/sinstg*cosstg
               ENDIF
               xplot = -xplot
               yplot = -yplot
               xplot = 22.0
               yplot = -22.0*sinstg/cosstg
               IF ( abs(yplot)>4.75 ) THEN
                  yplot = -4.75
                  xplot = 4.75/sinstg*cosstg
               ENDIF
               xplot = -xplot
               yplot = -yplot
               xplot = scale*(xs(j,1)*cosstg+ys(j,1)*sinstg)
               yplot = scale*(ys(j,1)*cosstg-xs(j,1)*sinstg)
               DO i = 2 , npoint
                  xplot = scale*(xs(j,i)*cosstg+ys(j,i)*sinstg)
                  yplot = scale*(ys(j,i)*cosstg-xs(j,i)*sinstg)
               ENDDO
               IF ( isecn==2 ) THEN
                  DO i = 2 , 30
                     xplot = scale*(Xsemj(j,i)*cosstg+Ysemj(j,i)*sinstg)
                     yplot = scale*(Ysemj(j,i)*cosstg-Xsemj(j,i)*sinstg)
                  ENDDO
               ENDIF
               DO ii = 1 , npoint
                  i = npoint + 1 - ii
                  xplot = scale*(xp(j,i)*cosstg+yp(j,i)*sinstg)
                  yplot = scale*(yp(j,i)*cosstg-xp(j,i)*sinstg)
               ENDDO
               DO i = 2 , 30
                  xplot = scale*(Xsemi(j,i)*cosstg+Ysemi(j,i)*sinstg)
                  yplot = scale*(Ysemi(j,i)*cosstg-Xsemi(j,i)*sinstg)
               ENDDO
               xplot = scale*(xs(j,1)*cosstg+ys(j,1)*sinstg)
               yplot = scale*(ys(j,1)*cosstg-xs(j,1)*sinstg)
            ELSE
               xplot = xs(j,1)*scale
               yplot = ys(j,1)*scale
               DO i = 2 , npoint
                  xplot = xs(j,i)*scale
                  yplot = ys(j,i)*scale
               ENDDO
               IF ( isecn==2 ) THEN
                  DO i = 2 , 30
                     xplot = Xsemj(j,i)*scale
                     yplot = Ysemj(j,i)*scale
                  ENDDO
               ENDIF
               DO ii = 1 , npoint
                  i = npoint + 1 - ii
                  xplot = xp(j,i)*scale
                  yplot = yp(j,i)*scale
               ENDDO
               DO i = 2 , 30
                  xplot = Xsemi(j,i)*scale
                  yplot = Ysemi(j,i)*scale
               ENDDO
               xplot = xs(j,1)*scale
               yplot = ys(j,1)*scale
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   IF ( inast/=0 ) THEN
      xsign = float(nsign)
      WRITE (Log2,99026)
99026 FORMAT (1H0,10X,34HNASTRAN COMPRESSOR BLADE BULK DATA,/10X,36(1H*),//)
      IF ( Ipgeom/=1 ) THEN
         WRITE (Log2,99027)
99027    FORMAT (11X,30H*** CTRIA2 AND PTRIA2 DATA ***,/)
         nstad = irte - irle + 1
         jloop = 0
         nelem = 0
         nstrd = nlines - 1
         irt = irte - 1
         nt = 1995
         DO j = 1 , nstrd
            DO i = irle , irt
               nelem = nelem + 1
               igrd1 = i - 1 + jloop
               igrd3 = igrd1 + nstad
               igrd2 = igrd1 + nstad + 1
               nt = nt + 5
               WRITE (Lpunch,99045) nelem , nt , igrd1 , igrd2 , igrd3
               WRITE (Log2,99046) nelem , nt , igrd1 , igrd2 , igrd3
               IF ( abs(float(inast))>3.5 ) THEN
                  thck = (tharr(j,i)+tharr(j+1,i)+tharr(j+1,i+1)+tharr(j,i+1))/4.
                  pres = -xsign*(blafor(i,j)+blafor(i,j+1)+blafor(i+1,j+1)+blafor(i+1,j))/4.
               ELSE
                  thck = (tharr(j,i)+tharr(j+1,i)+tharr(j+1,i+1))/3.
                  pres = -xsign*(blafor(i,j)+blafor(i,j+1)+blafor(i+1,j+1))/3.
               ENDIF
               WRITE (Lpunch,99047) nt , thck
               WRITE (Log2,99048) nt , thck
               IF ( inast>0 ) WRITE (Lpunch,99049) pres , nelem
               IF ( inast>0 ) WRITE (Log2,99050) pres , nelem
               nelem = nelem + 1
               igrd3 = igrd2
               igrd2 = igrd1 + 1
               IF ( abs(float(inast))<=3.5 ) THEN
                  nt = nt + 5
                  thck = (tharr(j,i)+tharr(j,i+1)+tharr(j+1,i+1))/3.
                  pres = -xsign*(blafor(i,j)+blafor(i+1,j)+blafor(i+1,j+1))/3.
                  WRITE (Lpunch,99047) nt , thck
                  WRITE (Log2,99048) nt , thck
               ENDIF
               WRITE (Lpunch,99045) nelem , nt , igrd1 , igrd2 , igrd3
               WRITE (Log2,99046) nelem , nt , igrd1 , igrd2 , igrd3
               IF ( inast>0 ) WRITE (Lpunch,99049) pres , nelem
               IF ( inast>0 ) WRITE (Log2,99050) pres , nelem
            ENDDO
            jloop = jloop + nstad
         ENDDO
      ENDIF
      WRITE (Log2,99028)
99028 FORMAT (1H0,10X,29H*** BLADE GRID POINT DATA ***,/)
      jd = 0
      DO j = 1 , nlines
         DO i = irle , irte
            jd = jd + 1
            Ycamb(j,i) = -xsign*Ycamb(j,i)
            WRITE (Log2,99029) jd , xcamb(j,i) , Ycamb(j,i) , Zcamb(j,i)
99029       FORMAT (1X,4HGRID,9X,I3,8X,3F8.4)
            WRITE (Lpunch,99030) jd , xcamb(j,i) , Ycamb(j,i) , Zcamb(j,i)
99030       FORMAT (4HGRID,9X,I3,8X,3F8.4)
         ENDDO
      ENDDO
      IF ( Istrml/=-1 .AND. Istrml/=2 ) THEN
         WRITE (Log2,99031)
99031    FORMAT (1H0,10X,27H*** BLADE STREAML1 DATA ***,/)
         nstad = irte - irle + 1
         nstad1 = nstad - 1
         DO j = 1 , nlines
            nd1 = (j-1)*nstad + 1
            nd2 = nd1 + nstad1
            WRITE (Lpunch,99032) j , nd1 , nd2
99032       FORMAT (8HSTREAML1,I8,I8,8H THRU   ,I8)
            WRITE (Log2,99033) j , nd1 , nd2
99033       FORMAT (1X,8HSTREAML1,I8,I8,8H THRU   ,I8)
         ENDDO
      ENDIF
   ENDIF
   IF ( Naero==1 .OR. ipunch==1 ) CALL alg19(Log1,Log2,Log3,Log5,nlines,nspec,Kpts,Rsta,Xsta,R,Zr,B1,B2,Tc,pi,c1,nblade,ccord,block,&
      & alpb,epslon,Ifangs,ipunch,Naero)
99034 FORMAT (1H1)
99035 FORMAT (3X,F8.4,2X,F8.4,8X,I2,9X,F8.4,9X,F8.4)
99036 FORMAT (3X,F8.4,2X,F8.4)
99037 FORMAT (29X,I2,9X,F8.4,9X,F8.4)
99038 FORMAT (/20X,'SECTION GEOMETRY SPECIFICATION',//10X,'STREAMLINE','  INLET',5X,6HOUTLET,4X,6HY2 LE/,4X,6HY2 TE/,3X,            &
             &48HLE RADIUS MAX THICK TE THICK  POINT OF  CHORD OR,3X,7HX STACK,3X,7HY STACK,/11X,6HNUMBER,5X,5HANGLE,5X,5HANGLE,3X, &
             &19HMAX VALUE MAX VALUE,3X,6H/CHORD,4X,6H/CHORD,3X,8H/2*CHORD,2X,18HMAX THICK AXIAL CD,4X,6HOFFSET,4X,6HOFFSET,//,     &
            & (10X,F7.2,3X,F8.3,F10.3,2F10.4,3F10.5,2F10.4,F11.6,F10.6))
99039 FORMAT (/10X,8HPOINT NO,4X,5HZSEMI,9X,5HXSEMI,9X,5HYSEMI,/)
99040 FORMAT (/10X,8HPOINT NO,4X,5HZSEMI,9X,5HXSEMI,9X,5HYSEMI,13X,5HZSEMJ,9X,5HXSEMJ,9X,5HYSEMJ,/)
99041 FORMAT (31X,8HPOINT NO,5X,2HX1,12X,2HY1,16X,2HX2,12X,2HY2,/)
99042 FORMAT (31X,I5,3X,1P,2E14.5,4X,1P,2E14.5)
99043 FORMAT (/31X,8HPOINT NO,5X,5HXSEMI,9X,5HYSEMI,/)
99044 FORMAT (/31X,8HPOINT NO,5X,5HXSEMI,9X,5HYSEMI,12X,5HXSEMJ,9X,5HYSEMJ,/)
99045 FORMAT (6HCTRIA2,7X,I3,4X,I4,3(5X,I3))
99046 FORMAT (1X,6HCTRIA2,7X,I3,4X,I4,3(5X,I3))
99047 FORMAT (6HPTRIA2,6X,I4,7X,1H1,F8.4,6X,2H0.)
99048 FORMAT (1X,6HPTRIA2,6X,I4,7X,1H1,F8.4,6X,2H0.)
99049 FORMAT (6HPLOAD2,8X,2H60,F8.4,5X,I3)
99050 FORMAT (1X,6HPLOAD2,8X,2H60,F8.4,5X,I3)
!     IF (IFPLOT .NE. 0) CALL PLOT (0.0,0.0,-3)
END SUBROUTINE algan
