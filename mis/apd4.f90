
SUBROUTINE apd4
   IMPLICIT NONE
   REAL Acpl(3,3) , Acsid , Alzo , Buf11 , Buf12 , C1 , Ca2e , Ca2s , Ca3e , Ca3s , Cp , Cstma , Pa2e , Pa2s , Pa3e , Pa3s , Ra1(3) &
      & , Rb1(3) , S1 , Scr2 , Scr3 , Scr4 , Scr5 , Sysbuf , X1 , X12 , X1p , X4 , X43 , Xop , Xp2 , Xp3 , Xp4 , Y1 , Y4 , Yp4 ,    &
      & Z(1) , Z1 , Z4
   INTEGER Acpt , Bgpa , Buf10 , Ca4e , Ca4s , Cidbx , Ecta , Eid , Gpla , Iacs , Ibit(64) , Ic(16) , Icpl(14) , Igid , Isiln ,     &
         & Itwo(32) , Iz(1) , Lchord , Left , Lspan , Luseta , Mcstm , Naef1 , Naef2 , Nca1 , Nca2 , Ncam , Nchord , Ncrd , Ncst1 , &
         & Ncst2 , Next , Nj , Nk , Not , Npa1 , Npa2 , Nspan , Pa4e , Pa4s , Pid , Scr1 , Sila , Silb , Useta
   CHARACTER*23 Ufm
   COMMON /apd1c / Eid , Pid , Cp , Nspan , Nchord , Lspan , Lchord , Igid , X1 , Y1 , Z1 , X12 , X4 , Y4 , Z4 , X43 , Xop , X1p ,  &
                 & Alzo , Mcstm , Ncst1 , Ncst2 , Cidbx , Acsid , Iacs , Silb , Ncrd , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Ecta ,    &
                 & Bgpa , Gpla , Useta , Sila , Cstma , Acpt , Buf10 , Buf11 , Buf12 , Next , Left , Isiln , Ncam , Naef1 , Naef2 , &
                 & Nca1 , Nca2 , Ca2s , Ca2e , Ca3s , Ca3e , Ca4s , Ca4e , Npa1 , Npa2 , Pa2s , Pa2e , Pa3s , Pa3e , Pa4s , Pa4e
   COMMON /apd1d / Icpl , Yp4 , S1 , C1 , Xp2 , Xp3 , Xp4 , Ra1
   COMMON /bitpos/ Ibit
   COMMON /blank / Nk , Nj , Luseta
   COMMON /system/ Sysbuf , Not
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER acsix(4) , auset(6,2) , back , cid(5) , eidb , i , i17 , i18 , i19 , i20 , iast , icid , icirc , ihead(9) , il , in ,    &
         & ioc , ipid , ipp , ispan , j , k , kk , lca , lcla , n , nam(2) , nc4 , ncid , ncrdp , necta(2) , npc , nsize , ntot ,   &
         & nw , pspa , silc , sildx(4) , uk , usa
   REAL ai(6) , bloc , ca , caoc , cj1 , cloc , d , dj1 , doc , dy , fsj1 , gap , gapoc , head(9) , vx1(3) , vx2(3) , xi1j , xi1j1 ,&
      & xic , xij , xij1 , ya , yj , yj1 , ysp
   REAL apdf
   INTEGER iapd , orf
   EXTERNAL orf
!
   EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (acsix(2),vx2(1)) , (Z(1),Iz(1)) ,    &
    & (Eid,Ic(1)) , (sildx(1),icid) , (sildx(3),silc) , (ai(1),dy) , (ai(2),bloc) , (ai(3),d) , (ai(4),ca) , (ai(5),gap) ,          &
    & (ai(6),nsize) , (head(1),ihead(1))
   DATA nam/4HAPD4 , 4H    /
!
   lca = 16
   nc4 = ((Ca4e-Ca4s)+1)/lca
   Ncam = Ncam + nc4
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
   ihead(1) = 4
   silc = Silb
!
!     LOOP ON NC4 MOVING CAERO4 CARD TO COMMON
!
   DO i = 1 , nc4
      ntot = 0
      n = (i-1)*lca - 1
      DO j = 1 , lca
         Ic(j) = Iz(Ca4s+n+j)
      ENDDO
      Mcstm = Mcstm + 1
      Iz(Ca4s+n+2) = Mcstm
      Iz(Ca4s+n+8) = 4
      acsix(1) = Mcstm
!
!     FIND PAERO4 CARD
!
      CALL apdoe(Pid,Iz,Pa4s,Pa4e,ipid,npc)
      IF ( ipid==0 ) GOTO 900
!
!     FIND NUMBER OF STRIPS
!
      ispan = Nspan
      iast = 0
      IF ( Nspan==0 ) THEN
         CALL apdoe(Nchord,Iz,Naef1,Naef2,iast,Nspan)
         IF ( iast==0 ) GOTO 800
         Nspan = Nspan - 1
         iast = iast + 1
      ENDIF
      Iz(Ca4s+n+4) = Nspan
      Iz(Ca4s+n+5) = 1
      ipp = ipid + 5
      npc = npc - 4
      npc = npc/3
      IF ( npc<Nspan ) GOTO 700
      ihead(8) = Nspan
!
!     GET POINTS IN PROPER COORD SYSTEM
!
      CALL apdcs
      head(9) = 1.0/sqrt(1.0+((Xp4+.25*(X43-X12))/Yp4)**2)
      IF ( Next+6*Nspan>Left ) GOTO 600
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
         dy = (yj1-yj)
         ya = .5*dy + yj
         ysp = ya
         cloc = X12 - (X12-X43)*ya/Yp4
         bloc = cloc*.5
         doc = Z(ipp)
         caoc = Z(ipp+1)
         gapoc = Z(ipp+2)
         ipp = ipp + 3
         d = doc*cloc
         ca = caoc*cloc
         gap = gapoc*cloc
         nsize = 2
         IF ( caoc/=0.0 ) nsize = 3
         Nj = Nj + nsize
         Nk = Nk + nsize
         ntot = ntot + nsize
         kk = 0
         DO k = 1 , 6
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
!     BGPDT , SPL, AND USET
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
 100     CALL gmmats(Acpl,3,3,0,vx1,3,1,0,vx2)
         DO k = 1 , 3
            vx2(k) = vx2(k) + Rb1(k)
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
      ihead(3) = Iz(ipid+1)
      lcla = ihead(3)
      ihead(4) = Iz(ipid+2)
      ihead(5) = Iz(ipid+3)
      icirc = ihead(5)
      ihead(6) = Iz(ipid+4)
      ihead(7) = 0
      il = 0
      in = Nspan + 1
!
!     PROPERTY DATA
!
      IF ( lcla/=0 .OR. icirc/=0 ) THEN
         IF ( lcla/=0 ) THEN
            CALL apdoe(ihead(4),Iz,Naef1,Naef2,il,nw)
            IF ( il==0 ) GOTO 300
            IF ( mod(nw,in)/=0 ) GOTO 300
            ihead(7) = nw/in
         ELSEIF ( icirc/=0 ) THEN
            CALL apdoe(ihead(6),Iz,Naef1,Naef2,il,nw)
            IF ( il==0 ) GOTO 500
            in = 2 + 2*icirc
            IF ( mod(nw,in)/=0 ) GOTO 500
            ihead(7) = nw/in
         ENDIF
      ENDIF
      CALL write(Acpt,ihead,9,0)
      CALL write(Acpt,Z(ioc+1),Nspan*6,0)
      IF ( il/=0 ) CALL write(Acpt,Z(il+1),nw,0)
      CALL write(Acpt,0,0,1)
   ENDDO
   Silb = silc
 200  RETURN
!
!     ERROR MESSAGES
!
 300  i = ihead(4)
 400  WRITE (Not,99001) Ufm , i , Eid
99001 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ','CARD ID',I9,/29X,'ASSOCIATED WITH CAERO4 ID',I9)
   CALL mesage(-61,0,nam)
   GOTO 200
 500  i = ihead(6)
   GOTO 400
 600  CALL mesage(-8,0,nam)
 700  i = Pid
   GOTO 400
 800  i = Nchord
   GOTO 400
 900  CALL emsg(0,2323,1,2,0)
   WRITE (Not,99002) Pid , Eid
99002 FORMAT (10X,16HPAERO4 CARD NO. ,I8,31H REFERENCED BY CAERO4 CARD NO. ,I8,15H DOES NOT EXIST)
   CALL mesage(-61,0,nam)
   GOTO 200
END SUBROUTINE apd4
