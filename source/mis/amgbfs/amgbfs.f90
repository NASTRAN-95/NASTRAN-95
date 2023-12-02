!*==amgbfs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgbfs(Skj,Ee,Delx,Nc,Nba,Xis2,Xis1,A0,A0p,Nsbe)
   USE c_amgmn
   USE c_dlbdy
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Skj
   REAL , DIMENSION(1) :: Ee
   REAL , DIMENSION(1) :: Delx
   INTEGER , DIMENSION(1) :: Nc
   INTEGER , DIMENSION(1) :: Nba
   REAL , DIMENSION(1) :: Xis2
   REAL , DIMENSION(1) :: Xis1
   REAL , DIMENSION(1) :: A0
   REAL , DIMENSION(1) :: A0p
   INTEGER , DIMENSION(1) :: Nsbe
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a02p , dx , p5 , rfkoc , x1 , x2 , x3
   INTEGER :: i , ia , ib , ibf , ibuf2 , ibzy , icorr , ig , ii , irow , isl , it , izbf , j , jf , jl , k , ks , l , length ,     &
            & nbx , nbxr , ncore , nfb , nfse , nfyb , nl , nlse , nn , nsb , nt02 , ntp2 , nzy2
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: nha , nhbfs , nhg
   EXTERNAL bckrec , bfsmat , bldpk , bldpkn , bug , close , dmpfil , fread , gmmatc , gopen , mesage , write , zblpki , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     BUILD SKJ CALL BFSMAT THEN SHUFFEL AND DEAL
!
   DATA name/4HAMGB , 4HFS  /
   DATA nhbfs , nhg , nha/4HBFS  , 4HG    , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nsb = ntys + ntzs
         nzy2 = nsb*2
         nt02 = nt0*2
         ntp2 = ntp*2
         length = nt0 + nsb
         isl = isk - 1
         ii = isk + length
         nn = nsk + nzy2 + ntp2
         ibuf2 = ecore
         IF ( nsb/=0 ) THEN
            ibuf2 = ecore - sysbuf
!
!     CALL BFSMAT
!     SCR1 HAS NTZS + NTYS ROWS WITH NTO*2 THEN NTZS+NTYS*2 TERMS
!     ROWS ARE Z FOR Z , Y THEN Z FOR ZY , AND Y FOR Y
!
            CALL gopen(scr1,z(ibuf2),1)
            icorr = next
            IF ( next+length*4>ibuf2 ) THEN
!
!     ERROR MESSAGES
!
               CALL mesage(-8,0,name)
               RETURN
            ELSE
               CALL bfsmat(nd,ne,nb,np,ntp,length,nt0,scr1,jf,jl,z(inas),fmach,z(iyb),z(izb),z(iys),z(izs),z(ix),Delx,Ee,z(ixic),   &
                         & z(isg),z(icg),z(iarb),z(iria),z(inbea1),z(inbea2),z(inasb),z(inb),Nc,z(icorr),z(iavr),refc,A0,Xis1,Xis2, &
                         & rfk,Nsbe,nt0)
               CALL write(scr1,0,0,1)
               CALL close(scr1,1)
               CALL dmpfil(scr1,z(next),ibuf2-next)
               CALL gopen(scr1,z(ibuf2),0)
               ncore = nt0*nzy2*2
               IF ( ncore+next>ibuf2 ) THEN
                  CALL mesage(-8,0,name)
                  RETURN
               ELSE
                  CALL zeroc(z(next),ncore)
                  i = next
                  izbf = 1
                  DO j = 1 , nsb
                     CALL fread(scr1,z(i),nt02,0)
                     CALL fread(scr1,z(i),-nzy2,0)
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
                  CALL bckrec(scr1)
               ENDIF
            ENDIF
         ENDIF
!
!     BUILD NT0 COLUMNS OF SKJ
!
         IF ( nt0/=0 ) THEN
            ibf = next - 2
            k = 1
            ks = 1
            nbxr = Nc(k)
            DO i = 1 , nt0
               CALL bldpk(3,3,Skj,0,0)
               IF ( i<=ntp ) THEN
                  a(1) = 2.0*Ee(ks)*Delx(i)
                  a(2) = 0.0
                  iis = isl + (i-1)*2 + 1
                  CALL zblpki
                  a(1) = (Ee(ks)*Delx(i)**2)/2.0
                  iis = iis + 1
                  CALL zblpki
                  IF ( i/=ntp ) THEN
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
                     a(1) = z(ibf+l)
                     a(2) = z(ibf+l+1)
                     iis = isl + ntp2 + j
                     CALL zblpki
                  ENDDO
               ENDIF
               CALL bldpkn(Skj,0,tskj)
            ENDDO
         ENDIF
!
!     SLENDER BODY ONLY PART OF SKJ  BFS * G
!
         IF ( nsb==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ncore = nzy2*nsb*4 + nsb*nsb*2
         IF ( ncore+next>ibuf2 ) THEN
            CALL mesage(-8,0,name)
            RETURN
         ELSE
            CALL zeroc(z(next),ncore)
            i = next
            izbf = 1
            DO j = 1 , nsb
               CALL fread(scr1,z(i),-nt02,0)
               CALL fread(scr1,z(i),nzy2,0)
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
            nfyb = nb + 1 - nby
            irow = ig
            rfkoc = 2.0*rfk/refc
            ibzy = 0
            p5 = .5
            IF ( ntzs==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nfse = 1
            nlse = 0
            nfb = 1
            nbx = nbz
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO ib = nfb , nbx
            nlse = nlse + Nsbe(ib)
            DO it = nfse , nlse
               dx = Xis2(it) - Xis1(it)
               a02p = 2.0/A0(it)*A0p(it)
               IF ( nfse/=nlse ) THEN
                  IF ( it==nfse ) THEN
                     x2 = p5*(Xis2(it+1)+Xis1(it+1))
                     x1 = p5*(Xis2(it)+Xis1(it))
                     z(irow) = (-1.0/(x2-x1))*dx
                     z(irow+2) = -z(irow)*(A0(it)/A0(it+1))**2
                  ELSEIF ( it==nlse ) THEN
                     x1 = p5*(Xis2(it-1)+Xis1(it-1))
                     x2 = p5*(Xis2(it)+Xis1(it))
                     z(irow) = (1.0/(x2-x1))*dx
                     z(irow-2) = -z(irow)*(A0(it)/A0(it-1))**2
                  ELSE
                     x1 = p5*(Xis2(it-1)+Xis1(it-1))
                     x2 = p5*(Xis2(it)+Xis1(it))
                     x3 = p5*(Xis2(it+1)+Xis1(it+1))
                     z(irow-2) = (1.0/(x3-x1)-1.0/(x2-x1))*dx*(A0(it)/A0(it-1))**2
                     z(irow) = (1.0/(x2-x1)-1.0/(x3-x2))*dx
                     z(irow+2) = (1.0/(x3-x2)-1.0/(x3-x1))*dx*(A0(it)/A0(it+1))**2
                  ENDIF
               ENDIF
               z(irow) = z(irow) + dx*a02p
               z(irow+1) = dx*rfkoc
               irow = irow + nzy2 + 2
            ENDDO
            nfse = nfse + Nsbe(ib)
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         IF ( ibzy/=1 ) THEN
            ibzy = 1
            IF ( ntys/=0 ) THEN
               nfb = nfyb
               nbx = nb
               nfse = 1
               nlse = 0
               nl = nfyb - 1
               IF ( nl/=0 ) THEN
                  DO j = 1 , nl
                     nlse = nlse + Nsbe(j)
                     nfse = nfse + Nsbe(j)
                  ENDDO
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     MULTIPLY BFS * G
!
         CALL bug(nhbfs,200,z(next),nzy2*nzy2)
         CALL bug(nhg,200,z(ig),nsb*nsb*2)
         CALL gmmatc(z(next),nzy2,nsb,0,z(ig),nsb,nsb,0,z(ia))
         CALL bug(nha,200,z(ia),nzy2*nsb*2)
         irow = ia - 2
         DO i = 1 , nsb
            CALL bldpk(3,3,Skj,0,0)
            irow = irow + 2
            k = irow
            DO j = 1 , nzy2
               a(1) = z(k)
               a(2) = z(k+1)
               iis = isl + ntp2 + j
               CALL zblpki
               k = k + nzy2
            ENDDO
            CALL bldpkn(Skj,0,tskj)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         isk = ii
         nsk = nn
         CALL close(scr1,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE amgbfs
