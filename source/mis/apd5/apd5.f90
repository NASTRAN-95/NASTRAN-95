!*==apd5.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd5
   IMPLICIT NONE
   USE c_apd1c
   USE c_apd1d
   USE c_bitpos
   USE c_blank
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (acsix(2),vx2(1)) , (Z(1),Iz(1)) ,    &
!>>>>    & (Eid,Ic(1)) , (sildx(1),icid) , (sildx(3),silc) , (ai(1),dy) , (ai(2),bloc) , (ai(3),ca) , (head(1),ihead(1))
   DATA nam/4HAPD5 , 4H    /
!
   lca = 16
   nc5 = ((ca5e-ca5s)+1)/lca
   ncam = ncam + nc5
!
!     INITIAL SETUP
!
   i17 = ibit(17)
   i18 = ibit(18)
   i19 = ibit(19)
   i20 = ibit(20)
   pspa = orf(itwo(i17),itwo(i20))
   usa = orf(pspa,itwo(i18))
   uk = orf(itwo(i19),itwo(i20))
   DO j = 1 , 2
      DO i = 1 , 6
         auset(i,j) = usa
      ENDDO
   ENDDO
   auset(3,2) = uk
   auset(5,2) = uk
   ihead(1) = 5
   silc = silb
!
!     LOOP ON NC5 MOVING CAERO5 CARD TO COMMON
!
   DO i = 1 , nc5
      ntot = 0
      n = (i-1)*lca - 1
      DO j = 1 , lca
         ic(j) = iz(ca5s+n+j)
      ENDDO
      mcstm = mcstm + 1
      iz(ca5s+n+2) = mcstm
      iz(ca5s+n+8) = 5
      acsix(1) = mcstm
!
!     FIND PAERO5 CARD
!
      CALL apdoe(pid,iz,pa5s,pa5e,ipid,npc)
      IF ( ipid==0 ) GOTO 1100
!
!     FIND NUMBER OF STRIPS
!
      ispan = nspan
      iast = 0
      IF ( nspan==0 ) THEN
         CALL apdoe(nchord,iz,naef1,naef2,iast,nspan)
         IF ( iast==0 ) GOTO 1000
         nspan = nspan - 1
         iast = iast + 1
      ENDIF
      iz(ca5s+n+4) = nspan
      iz(ca5s+n+5) = 1
      ipp = ipid + 7
      npc = npc - 6
      IF ( npc<nspan ) GOTO 900
      ihead(9) = nspan
!
!     GET POINTS IN PROPER COORD SYSTEM
!
      CALL apdcs
      head(10) = sqrt(1.0+(xp4/yp4)**2)
      IF ( next+3*nspan>left ) GOTO 800
      ioc = next
!
!     GENERATE DATA FOR BOXES
!
      ncrdp = ncrd
      fsj1 = apdf(z(iast),1,ispan)
      yj1 = fsj1*yp4
      dj1 = fsj1*xp4
      cj1 = x12 + fsj1*(x43-x12)
      xij1 = dj1
      xi1j1 = dj1 + cj1
      eidb = eid - 1
      DO j = 1 , nspan
         yj = yj1
         fsj1 = apdf(z(iast),j+1,ispan)
         yj1 = fsj1*yp4
         dj1 = fsj1*xp4
         cj1 = x12 + fsj1*(x43-x12)
         dy = yj1 - yj
         ya = .5*dy + yj
         ysp = ya
         cloc = x12 - (x12-x43)*ya/yp4
         bloc = cloc*.5
         caoc = z(ipp)
         ipp = ipp + 1
         ca = caoc*cloc
         nsize = 2
         IF ( caoc/=0.0 ) nsize = 3
         nj = nj + nsize
         nk = nk + nsize
         ntot = ntot + nsize
         kk = 0
         DO k = 1 , 3
            z(ioc+j+kk) = ai(k)
            kk = kk + nspan
         ENDDO
!
!     EXTERNAL ID S
!
         eidb = eidb + 1
         cid(1) = cidbx + 1 + 2*(j-1)
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
         CALL write(bgpa,acsix,4,0)
         CALL write(gpla,icid,1,0)
         CALL write(useta,auset(1,kk),6,0)
!
!     SIL AND EQEXIN
!
         ncrd = ncrd + 1
         silc = silc + 6
         isiln = isiln + 6
         sildx(4) = isiln
         luseta = silc
         sildx(2) = 10*silc + 1
         CALL write(sila,silc,1,0)
         CALL write(scr2,isiln,1,0)
         CALL write(scr2,silc,1,0)
         CALL write(scr1,icid,2,0)
         GOTO back
!
!     ECT
!
 120     cid(1) = iapd(1,j,1,ncrdp)
         cid(2) = iapd(2,j,1,ncrdp)
         cid(4) = iapd(1,j+1,1,ncrdp)
         cid(3) = iapd(2,j+1,1,ncrdp)
         cid(5) = cid(3) + 1
         CALL write(ecta,necta(1),6,0)
         auset(6,2) = usa
      ENDDO
      cidbx = ncid
!
!     PUT OUT ACPT REC
!
      ihead(2) = ntot
      ihead(4) = nthry
      IF ( nthry==1 ) head(10) = 0.0
      ihead(5) = nthick
      ihead(6) = iz(ipid+1)
      ihead(7) = iz(ipid+3)
      ihead(8) = iz(ipid+5)
!
!     PROPERTY DATA
!
      il = 0
      in = nspan + 1
!
!     ALPHAS
!
      CALL apdoe(iz(ipid+2),iz,naef1,naef2,il,nw)
      IF ( il==0 ) GOTO 500
      IF ( ihead(6)==1 ) THEN
         ihead(3) = nw/2
         IF ( mod(nw,2)/=0 ) GOTO 500
      ELSE
         ihead(3) = nw/in
         IF ( mod(nw,in)/=0 ) GOTO 500
         ihead(6) = nspan
      ENDIF
!
!     INTEGRALS
!
      int = 0
      itn = 0
      IF ( nthick==0 ) THEN
!
!     TAUS
!
         CALL apdoe(z(ipid+6),iz,naef1,naef2,itn,nwt)
         IF ( int==0 ) GOTO 400
         IF ( ihead(8)==0 ) GOTO 400
         IF ( ihead(8)/=1 ) THEN
            IF ( nwt/=3*nspan ) GOTO 400
            ihead(8) = nspan
         ELSEIF ( nwt/=3 ) THEN
            GOTO 400
         ENDIF
      ELSE
         CALL apdoe(nthick,iz,naef1,naef2,int,nwi)
         IF ( int==0 ) GOTO 700
         IF ( ihead(7)==0 .AND. nwi<6 ) GOTO 700
         IF ( ihead(7)/=0 .AND. nwi/=12 ) GOTO 700
      ENDIF
!
!     THICKNESSES
!
      itk = 0
      IF ( nthick==0 .OR. ihead(7)/=0 ) THEN
         CALL apdoe(z(ipid+4),iz,naef1,naef2,itk,nwtk)
         IF ( itk==0 ) GOTO 300
         IF ( int==0 ) THEN
            IF ( nwtk<2 ) GOTO 300
            IF ( ihead(8)==nspan .AND. nwtk<2*nspan ) GOTO 300
         ELSE
            IF ( ihead(7)/=1 ) ihead(7) = nspan
            IF ( ihead(7)==nspan .AND. nwtk<nspan ) GOTO 300
         ENDIF
      ENDIF
      CALL write(acpt,ihead,10,0)
      CALL write(acpt,z(ioc+1),nspan*3,0)
      CALL write(acpt,z(il+1),nw,0)
      IF ( int/=0 ) CALL write(acpt,z(int+1),nwi,0)
      IF ( int/=0 .AND. itk/=0 ) CALL write(acpt,z(itk+1),nwtk,0)
      IF ( itn/=0 ) CALL write(acpt,z(itn+1),nwt,0)
      IF ( itn/=0 ) CALL write(acpt,z(itk+1),nwtk,0)
      CALL write(acpt,0,0,1)
   ENDDO
   silb = silc
 200  RETURN
!
!     ERROR MESSAGES
!
 300  i = iz(ipid+4)
   GOTO 600
 400  i = iz(ipid+6)
   GOTO 600
 500  i = ihead(6)
 600  WRITE (not,99001) ufm , i , eid
99001 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ','CARD ID',I9,/29X,'ASSOCIATED WITH CAERO5 ID',I9)
   CALL mesage(-61,0,nam)
   GOTO 200
 700  i = nthick
   GOTO 600
 800  CALL mesage(-8,0,nam)
 900  i = pid
   GOTO 600
 1000 i = nchord
   GOTO 600
 1100 CALL emsg(0,2323,1,2,0)
   WRITE (not,99002) pid , eid
99002 FORMAT (10X,16HPAERO5 CARD NO. ,I8,31H REFERENCED BY CAERO5 CARD NO. ,I8,15H DOES NOT EXIST)
   CALL mesage(-61,0,nam)
   GOTO 200
END SUBROUTINE apd5
