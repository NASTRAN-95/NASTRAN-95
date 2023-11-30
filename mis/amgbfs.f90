
SUBROUTINE amgbfs(Skj,Ee,Delx,Nc,Nba,Xis2,Xis1,A0,A0p,Nsbe)
   IMPLICIT NONE
   REAL A(4) , Fmach , Refc , Rfk , Scr3 , Scr4 , Scr5 , Z(1)
   INTEGER Ecore , Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Iis , Inas , Inasb , Inb , Inbea1 , Inbea2 ,&
         & Inc , Infl , Ins , Insbea , Int121 , Int122 , Iria , Isg , Isk , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 ,      &
         & Ixlam , Ixle , Ixte , Iyb , Iyin , Iys , Izb , Izin , Izs , Mcb(7) , Nb , Nby , Nbz , Nd , Ne , Next , Nj1 , Nk1 , Np ,  &
         & Nrow , Nsk , Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs , Scr1 , Scr2 , Sysbuf , Tskj(7)
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ A , Iis
   COMMON /zzzzzz/ Z
   INTEGER Skj
   REAL A0(1) , A0p(1) , Delx(1) , Ee(1) , Xis1(1) , Xis2(1)
   INTEGER Nba(1) , Nc(1) , Nsbe(1)
   REAL a02p , dx , p5 , rfkoc , x1 , x2 , x3
   INTEGER i , ia , ib , ibf , ibuf2 , ibzy , icorr , ig , ii , irow , isl , it , izbf , j , jf , jl , k , ks , l , length , name(2)&
         & , nbx , nbxr , ncore , nfb , nfse , nfyb , nha , nhbfs , nhg , nl , nlse , nn , nsb , nt02 , ntp2 , nzy2
!
!     BUILD SKJ CALL BFSMAT THEN SHUFFEL AND DEAL
!
   DATA name/4HAMGB , 4HFS  /
   DATA nhbfs , nhg , nha/4HBFS  , 4HG    , 4HA   /
!
   nsb = Ntys + Ntzs
   nzy2 = nsb*2
   nt02 = Nt0*2
   ntp2 = Ntp*2
   length = Nt0 + nsb
   isl = Isk - 1
   ii = Isk + length
   nn = Nsk + nzy2 + ntp2
   ibuf2 = Ecore
   IF ( nsb/=0 ) THEN
      ibuf2 = Ecore - Sysbuf
!
!     CALL BFSMAT
!     SCR1 HAS NTZS + NTYS ROWS WITH NTO*2 THEN NTZS+NTYS*2 TERMS
!     ROWS ARE Z FOR Z , Y THEN Z FOR ZY , AND Y FOR Y
!
      CALL gopen(Scr1,Z(ibuf2),1)
      icorr = Next
      IF ( Next+length*4>ibuf2 ) THEN
!
!     ERROR MESSAGES
!
         CALL mesage(-8,0,name)
         GOTO 400
      ELSE
         CALL bfsmat(Nd,Ne,Nb,Np,Ntp,length,Nt0,Scr1,jf,jl,Z(Inas),Fmach,Z(Iyb),Z(Izb),Z(Iys),Z(Izs),Z(Ix),Delx,Ee,Z(Ixic),Z(Isg),  &
                   & Z(Icg),Z(Iarb),Z(Iria),Z(Inbea1),Z(Inbea2),Z(Inasb),Z(Inb),Nc,Z(icorr),Z(Iavr),Refc,A0,Xis1,Xis2,Rfk,Nsbe,Nt0)
         CALL write(Scr1,0,0,1)
         CALL close(Scr1,1)
         CALL dmpfil(Scr1,Z(Next),ibuf2-Next)
         CALL gopen(Scr1,Z(ibuf2),0)
         ncore = Nt0*nzy2*2
         IF ( ncore+Next>ibuf2 ) THEN
            CALL mesage(-8,0,name)
            GOTO 400
         ELSE
            CALL zeroc(Z(Next),ncore)
            i = Next
            izbf = 1
            DO j = 1 , nsb
               CALL fread(Scr1,Z(i),nt02,0)
               CALL fread(Scr1,Z(i),-nzy2,0)
               i = i + nt02
               IF ( jf/=0 ) THEN
                  IF ( j>=jf .AND. j<=jl ) THEN
                     izbf = -izbf
                     IF ( izbf<0 ) CYCLE
                     i = i + nt02
                  ENDIF
               ENDIF
               i = i + nt02
            ENDDO
            CALL bckrec(Scr1)
         ENDIF
      ENDIF
   ENDIF
!
!     BUILD NT0 COLUMNS OF SKJ
!
   IF ( Nt0/=0 ) THEN
      ibf = Next - 2
      k = 1
      ks = 1
      nbxr = Nc(k)
      DO i = 1 , Nt0
         CALL bldpk(3,3,Skj,0,0)
         IF ( i<=Ntp ) THEN
            A(1) = 2.0*Ee(ks)*Delx(i)
            A(2) = 0.0
            Iis = isl + (i-1)*2 + 1
            CALL zblpki
            A(1) = (Ee(ks)*Delx(i)**2)/2.0
            Iis = Iis + 1
            CALL zblpki
            IF ( i/=Ntp ) THEN
               IF ( i==Nba(k) ) k = k + 1
               IF ( i==nbxr ) THEN
                  ks = ks + 1
                  nbxr = nbxr + Nc(k)
               ENDIF
            ENDIF
         ENDIF
         IF ( nsb/=0 ) THEN
            ibf = ibf + 2
            DO j = 1 , nzy2
               l = (j-1)*nt02
               A(1) = Z(ibf+l)
               A(2) = Z(ibf+l+1)
               Iis = isl + ntp2 + j
               CALL zblpki
            ENDDO
         ENDIF
         CALL bldpkn(Skj,0,Tskj)
      ENDDO
   ENDIF
!
!     SLENDER BODY ONLY PART OF SKJ  BFS * G
!
   IF ( nsb==0 ) GOTO 300
   ncore = nzy2*nsb*4 + nsb*nsb*2
   IF ( ncore+Next>ibuf2 ) THEN
      CALL mesage(-8,0,name)
      GOTO 400
   ELSE
      CALL zeroc(Z(Next),ncore)
      i = Next
      izbf = 1
      DO j = 1 , nsb
         CALL fread(Scr1,Z(i),-nt02,0)
         CALL fread(Scr1,Z(i),nzy2,0)
         i = i + nzy2
         IF ( jf/=0 ) THEN
            IF ( j>=jf .AND. j<=jl ) THEN
               izbf = -izbf
               IF ( izbf<0 ) CYCLE
               i = i + nzy2
            ENDIF
         ENDIF
         i = i + nzy2
      ENDDO
!
!     BFS AT NEXT  G AT IG
!
      ig = i
      ia = ig + nsb*nsb*2
      nfyb = Nb + 1 - Nby
      irow = ig
      rfkoc = 2.0*Rfk/Refc
      ibzy = 0
      p5 = .5
      IF ( Ntzs==0 ) GOTO 200
      nfse = 1
      nlse = 0
      nfb = 1
      nbx = Nbz
   ENDIF
 100  DO ib = nfb , nbx
      nlse = nlse + Nsbe(ib)
      DO it = nfse , nlse
         dx = Xis2(it) - Xis1(it)
         a02p = 2.0/A0(it)*A0p(it)
         IF ( nfse/=nlse ) THEN
            IF ( it==nfse ) THEN
               x2 = p5*(Xis2(it+1)+Xis1(it+1))
               x1 = p5*(Xis2(it)+Xis1(it))
               Z(irow) = (-1.0/(x2-x1))*dx
               Z(irow+2) = -Z(irow)*(A0(it)/A0(it+1))**2
            ELSEIF ( it==nlse ) THEN
               x1 = p5*(Xis2(it-1)+Xis1(it-1))
               x2 = p5*(Xis2(it)+Xis1(it))
               Z(irow) = (1.0/(x2-x1))*dx
               Z(irow-2) = -Z(irow)*(A0(it)/A0(it-1))**2
            ELSE
               x1 = p5*(Xis2(it-1)+Xis1(it-1))
               x2 = p5*(Xis2(it)+Xis1(it))
               x3 = p5*(Xis2(it+1)+Xis1(it+1))
               Z(irow-2) = (1.0/(x3-x1)-1.0/(x2-x1))*dx*(A0(it)/A0(it-1))**2
               Z(irow) = (1.0/(x2-x1)-1.0/(x3-x2))*dx
               Z(irow+2) = (1.0/(x3-x2)-1.0/(x3-x1))*dx*(A0(it)/A0(it+1))**2
            ENDIF
         ENDIF
         Z(irow) = Z(irow) + dx*a02p
         Z(irow+1) = dx*rfkoc
         irow = irow + nzy2 + 2
      ENDDO
      nfse = nfse + Nsbe(ib)
   ENDDO
 200  IF ( ibzy/=1 ) THEN
      ibzy = 1
      IF ( Ntys/=0 ) THEN
         nfb = nfyb
         nbx = Nb
         nfse = 1
         nlse = 0
         nl = nfyb - 1
         IF ( nl/=0 ) THEN
            DO j = 1 , nl
               nlse = nlse + Nsbe(j)
               nfse = nfse + Nsbe(j)
            ENDDO
         ENDIF
         GOTO 100
      ENDIF
   ENDIF
!
!     MULTIPLY BFS * G
!
   CALL bug(nhbfs,200,Z(Next),nzy2*nzy2)
   CALL bug(nhg,200,Z(ig),nsb*nsb*2)
   CALL gmmatc(Z(Next),nzy2,nsb,0,Z(ig),nsb,nsb,0,Z(ia))
   CALL bug(nha,200,Z(ia),nzy2*nsb*2)
   irow = ia - 2
   DO i = 1 , nsb
      CALL bldpk(3,3,Skj,0,0)
      irow = irow + 2
      k = irow
      DO j = 1 , nzy2
         A(1) = Z(k)
         A(2) = Z(k+1)
         Iis = isl + ntp2 + j
         CALL zblpki
         k = k + nzy2
      ENDDO
      CALL bldpkn(Skj,0,Tskj)
   ENDDO
 300  Isk = ii
   Nsk = nn
   CALL close(Scr1,1)
 400  RETURN
END SUBROUTINE amgbfs
