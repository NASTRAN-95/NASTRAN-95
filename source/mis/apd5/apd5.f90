!*==apd5.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd5
   IMPLICIT NONE
   USE C_APD1C
   USE C_APD1D
   USE C_BITPOS
   USE C_BLANK
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) :: acpl
   INTEGER , DIMENSION(4) :: acsix , sildx
   REAL , DIMENSION(3) :: ai , rb1 , vx1 , vx2
   INTEGER , DIMENSION(6,2) :: auset
   INTEGER :: back , eidb , i , i17 , i18 , i19 , i20 , iast , icid , il , in , int , ioc , ipid , ipp , ispan , itk , itn , j , k ,&
            & kk , lca , n , nc5 , ncid , ncrdp , npc , nsize , ntot , nw , nwi , nwt , nwtk , pspa , silc , uk , usa
   REAL :: bloc , ca , caoc , cj1 , cloc , dj1 , dy , fsj1 , xi1j , xi1j1 , xic , xij , xij1 , ya , yj , yj1 , ysp
   INTEGER , DIMENSION(5) :: cid
   REAL , DIMENSION(10) :: head
   INTEGER , DIMENSION(16) :: ic
   INTEGER , DIMENSION(10) :: ihead
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(2) :: necta
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (acsix(2),vx2(1)) , (Z(1),Iz(1)) ,    &
!>>>>    & (Eid,Ic(1)) , (sildx(1),icid) , (sildx(3),silc) , (ai(1),dy) , (ai(2),bloc) , (ai(3),ca) , (head(1),ihead(1))
   DATA nam/4HAPD5 , 4H    /
!
   lca = 16
   nc5 = ((Ca5e-Ca5s)+1)/lca
   Ncam = Ncam + nc5
!
!     INITIAL SETUP
!
   i17 = Ibit(17)
   i18 = Ibit(18)
   i19 = Ibit(19)
   i20 = Ibit(20)
   pspa = orf(Itwo(i17),Itwo(i20))
   usa = orf(pspa,Itwo(i18))
   uk = orf(Itwo(i19),Itwo(i20))
   DO j = 1 , 2
      DO i = 1 , 6
         auset(i,j) = usa
      ENDDO
   ENDDO
   auset(3,2) = uk
   auset(5,2) = uk
   ihead(1) = 5
   silc = Silb
!
!     LOOP ON NC5 MOVING CAERO5 CARD TO COMMON
!
   DO i = 1 , nc5
      ntot = 0
      n = (i-1)*lca - 1
      DO j = 1 , lca
         ic(j) = iz(Ca5s+n+j)
      ENDDO
      Mcstm = Mcstm + 1
      iz(Ca5s+n+2) = Mcstm
      iz(Ca5s+n+8) = 5
      acsix(1) = Mcstm
!
!     FIND PAERO5 CARD
!
      CALL apdoe(Pid,iz,Pa5s,Pa5e,ipid,npc)
      IF ( ipid==0 ) GOTO 1100
!
!     FIND NUMBER OF STRIPS
!
      ispan = Nspan
      iast = 0
      IF ( Nspan==0 ) THEN
         CALL apdoe(Nchord,iz,Naef1,Naef2,iast,Nspan)
         IF ( iast==0 ) GOTO 1000
         Nspan = Nspan - 1
         iast = iast + 1
      ENDIF
      iz(Ca5s+n+4) = Nspan
      iz(Ca5s+n+5) = 1
      ipp = ipid + 7
      npc = npc - 6
      IF ( npc<Nspan ) GOTO 900
      ihead(9) = Nspan
!
!     GET POINTS IN PROPER COORD SYSTEM
!
      CALL apdcs
      head(10) = sqrt(1.0+(Xp4/Yp4)**2)
      IF ( Next+3*Nspan>Left ) GOTO 800
      ioc = Next
!
!     GENERATE DATA FOR BOXES
!
      ncrdp = Ncrd
      fsj1 = apdf(Z(iast),1,ispan)
      yj1 = fsj1*Yp4
      dj1 = fsj1*Xp4
      cj1 = X12 + fsj1*(X43-X12)
      xij1 = dj1
      xi1j1 = dj1 + cj1
      eidb = Eid - 1
      DO j = 1 , Nspan
         yj = yj1
         fsj1 = apdf(Z(iast),j+1,ispan)
         yj1 = fsj1*Yp4
         dj1 = fsj1*Xp4
         cj1 = X12 + fsj1*(X43-X12)
         dy = yj1 - yj
         ya = .5*dy + yj
         ysp = ya
         cloc = X12 - (X12-X43)*ya/Yp4
         bloc = cloc*.5
         caoc = Z(ipp)
         ipp = ipp + 1
         ca = caoc*cloc
         nsize = 2
         IF ( caoc/=0.0 ) nsize = 3
         Nj = Nj + nsize
         Nk = Nk + nsize
         ntot = ntot + nsize
         kk = 0
         DO k = 1 , 3
            Z(ioc+j+kk) = ai(k)
            kk = kk + Nspan
         ENDDO
!
!     EXTERNAL ID S
!
         eidb = eidb + 1
         cid(1) = Cidbx + 1 + 2*(j-1)
         cid(2) = cid(1) + 1
         cid(3) = cid(1) + 2
         cid(4) = cid(3) + 1
         cid(5) = eidb
         ncid = cid(4)
!
!     BGPDT, SPL, AND USET
!
         xij = xij1
         xi1j = xi1j1
         xij1 = dj1
         xi1j1 = dj1 + cj1
         xic = (xij+xij1+bloc)*.5
         vx1(3) = 0
         IF ( j/=1 ) GOTO 40
         ASSIGN 20 TO back
         icid = cid(1)
         vx1(1) = xij
         vx1(2) = yj
         kk = 1
         GOTO 100
 20      ASSIGN 40 TO back
         icid = cid(2)
         vx1(1) = xi1j
         vx1(2) = yj
         kk = 1
         GOTO 100
 40      ASSIGN 60 TO back
         icid = cid(3)
         vx1(1) = xij1
         vx1(2) = yj1
         kk = 1
         GOTO 100
 60      ASSIGN 80 TO back
         icid = cid(4)
         vx1(1) = xi1j1
         vx1(2) = yj1
         kk = 1
         GOTO 100
 80      ASSIGN 120 TO back
         icid = cid(5)
         vx1(1) = xic
         IF ( nsize==3 ) auset(6,2) = uk
         vx1(2) = ysp
         kk = 2
 100     CALL gmmats(acpl,3,3,0,vx1,3,1,0,vx2)
         DO k = 1 , 3
            vx2(k) = vx2(k) + rb1(k)
         ENDDO
         CALL write(Bgpa,acsix,4,0)
         CALL write(Gpla,icid,1,0)
         CALL write(Useta,auset(1,kk),6,0)
!
!     SIL AND EQEXIN
!
         Ncrd = Ncrd + 1
         silc = silc + 6
         Isiln = Isiln + 6
         sildx(4) = Isiln
         Luseta = silc
         sildx(2) = 10*silc + 1
         CALL write(Sila,silc,1,0)
         CALL write(Scr2,Isiln,1,0)
         CALL write(Scr2,silc,1,0)
         CALL write(Scr1,icid,2,0)
         GOTO back
!
!     ECT
!
 120     cid(1) = iapd(1,j,1,ncrdp)
         cid(2) = iapd(2,j,1,ncrdp)
         cid(4) = iapd(1,j+1,1,ncrdp)
         cid(3) = iapd(2,j+1,1,ncrdp)
         cid(5) = cid(3) + 1
         CALL write(Ecta,necta(1),6,0)
         auset(6,2) = usa
      ENDDO
      Cidbx = ncid
!
!     PUT OUT ACPT REC
!
      ihead(2) = ntot
      ihead(4) = Nthry
      IF ( Nthry==1 ) head(10) = 0.0
      ihead(5) = Nthick
      ihead(6) = iz(ipid+1)
      ihead(7) = iz(ipid+3)
      ihead(8) = iz(ipid+5)
!
!     PROPERTY DATA
!
      il = 0
      in = Nspan + 1
!
!     ALPHAS
!
      CALL apdoe(iz(ipid+2),iz,Naef1,Naef2,il,nw)
      IF ( il==0 ) GOTO 500
      IF ( ihead(6)==1 ) THEN
         ihead(3) = nw/2
         IF ( mod(nw,2)/=0 ) GOTO 500
      ELSE
         ihead(3) = nw/in
         IF ( mod(nw,in)/=0 ) GOTO 500
         ihead(6) = Nspan
      ENDIF
!
!     INTEGRALS
!
      int = 0
      itn = 0
      IF ( Nthick==0 ) THEN
!
!     TAUS
!
         CALL apdoe(Z(ipid+6),iz,Naef1,Naef2,itn,nwt)
         IF ( int==0 ) GOTO 400
         IF ( ihead(8)==0 ) GOTO 400
         IF ( ihead(8)/=1 ) THEN
            IF ( nwt/=3*Nspan ) GOTO 400
            ihead(8) = Nspan
         ELSEIF ( nwt/=3 ) THEN
            GOTO 400
         ENDIF
      ELSE
         CALL apdoe(Nthick,iz,Naef1,Naef2,int,nwi)
         IF ( int==0 ) GOTO 700
         IF ( ihead(7)==0 .AND. nwi<6 ) GOTO 700
         IF ( ihead(7)/=0 .AND. nwi/=12 ) GOTO 700
      ENDIF
!
!     THICKNESSES
!
      itk = 0
      IF ( Nthick==0 .OR. ihead(7)/=0 ) THEN
         CALL apdoe(Z(ipid+4),iz,Naef1,Naef2,itk,nwtk)
         IF ( itk==0 ) GOTO 300
         IF ( int==0 ) THEN
            IF ( nwtk<2 ) GOTO 300
            IF ( ihead(8)==Nspan .AND. nwtk<2*Nspan ) GOTO 300
         ELSE
            IF ( ihead(7)/=1 ) ihead(7) = Nspan
            IF ( ihead(7)==Nspan .AND. nwtk<Nspan ) GOTO 300
         ENDIF
      ENDIF
      CALL write(Acpt,ihead,10,0)
      CALL write(Acpt,Z(ioc+1),Nspan*3,0)
      CALL write(Acpt,Z(il+1),nw,0)
      IF ( int/=0 ) CALL write(Acpt,Z(int+1),nwi,0)
      IF ( int/=0 .AND. itk/=0 ) CALL write(Acpt,Z(itk+1),nwtk,0)
      IF ( itn/=0 ) CALL write(Acpt,Z(itn+1),nwt,0)
      IF ( itn/=0 ) CALL write(Acpt,Z(itk+1),nwtk,0)
      CALL write(Acpt,0,0,1)
   ENDDO
   Silb = silc
 200  RETURN
!
!     ERROR MESSAGES
!
 300  i = iz(ipid+4)
   GOTO 600
 400  i = iz(ipid+6)
   GOTO 600
 500  i = ihead(6)
 600  WRITE (Not,99001) Ufm , i , Eid
99001 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ','CARD ID',I9,/29X,'ASSOCIATED WITH CAERO5 ID',I9)
   CALL mesage(-61,0,nam)
   GOTO 200
 700  i = Nthick
   GOTO 600
 800  CALL mesage(-8,0,nam)
 900  i = Pid
   GOTO 600
 1000 i = Nchord
   GOTO 600
 1100 CALL emsg(0,2323,1,2,0)
   WRITE (Not,99002) Pid , Eid
99002 FORMAT (10X,16HPAERO5 CARD NO. ,I8,31H REFERENCED BY CAERO5 CARD NO. ,I8,15H DOES NOT EXIST)
   CALL mesage(-61,0,nam)
   GOTO 200
END SUBROUTINE apd5
