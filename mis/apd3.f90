
SUBROUTINE apd3
   IMPLICIT NONE
   REAL Acpl(3,3) , Acsid , Alzo , Buf11 , Buf12 , C1 , Ca2e , Ca2s , Ca4e , Ca4s , Cp , Cstma , Pa2e , Pa2s , Pa4e , Pa4s , Ra1(3) &
      & , Rb1(3) , S1 , Scr2 , Scr3 , Scr4 , Scr5 , Sysbuf , X1 , X12 , X1p , X4 , X43 , Xop , Xp2 , Xp3 , Xp4 , Y1 , Y4 , Yp4 ,    &
      & Z(1) , Z1 , Z4
   INTEGER Acpt , Bgpa , Buf10 , Ca3e , Ca3s , Cidbx , Ecta , Eid , Gpla , Iacs , Ibit(64) , Ic(16) , Icpl(14) , Igid , Isiln ,     &
         & Itwo(32) , Iz(1) , Lchord , Left , Lspan , Luseta , Mcstm , Naef1 , Naef2 , Nca1 , Nca2 , Ncam , Nchord , Ncrd , Ncst1 , &
         & Ncst2 , Next , Nj , Nk , Not , Npa1 , Npa2 , Nspan , Pa3e , Pa3s , Pid , Scr1 , Sila , Silb , Useta
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
   INTEGER acsix(4) , auset(6,2) , back , cid(5) , eidb , i , i17 , i18 , i19 , i20 , ibs , icid , ihead(10) , ilc1 , ilc2 , ilw ,  &
         & ipid , j , k , kk , kkk , lca , n , nam(2) , nc3 , necta(2) , nm , nn , nogo , nwc1 , nwc2 , nww , pspa , ret , silc ,   &
         & sildx(2) , uk , usa
   REAL bnd(24) , vx1(3) , vx2(3)
   LOGICAL cntrl1 , cntrl2 , crank1 , crank2
   INTEGER orf
   EXTERNAL orf
!
   EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (acsix(2),vx2(1)) , (sildx(1),icid) , &
    & (Z(1),Iz(1)) , (Eid,Ic(1)) , (crank1,ihead(3)) , (crank2,ihead(4)) , (cntrl1,ihead(5)) , (cntrl2,ihead(6))
   DATA nam/4HAPD3 , 4H    /
!
   nogo = 0
   lca = 16
   nc3 = ((Ca3e-Ca3s)+1)/lca
   Ncam = Ncam + nc3
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
   ihead(1) = 3
   silc = Silb
!
!     LOOP ON NC3 MOVING CAERO3 CARD TO COMMON
!
   DO i = 1 , nc3
      n = (i-1)*lca - 1
      DO j = 1 , lca
         Ic(j) = Iz(Ca3s+n+j)
      ENDDO
      Mcstm = Mcstm + 1
      Iz(Ca3s+n+2) = Mcstm
      Iz(Ca3s+n+8) = 3
      acsix(1) = Mcstm
!
!     GET POINTS IN PROPER COORD SYSTEM
!
      CALL apdcs
!
!     FIND PAERO3 CARD
!
      j = Pa3s
      DO WHILE ( j<Pa3e )
         IF ( Iz(j)==Pid ) THEN
            ipid = j
            ihead(7) = Iz(ipid+1)
            crank1 = .FALSE.
            crank2 = .FALSE.
            IF ( Z(ipid+5)>0.0 ) crank1 = .TRUE.
            IF ( Z(ipid+7)>0.0 ) crank2 = .TRUE.
            cntrl1 = .FALSE.
            cntrl2 = .FALSE.
            IF ( Iz(ipid+2)>0 ) cntrl1 = .TRUE.
            IF ( Iz(ipid+2)==2 ) cntrl2 = .TRUE.
!
!     GENERATE AERO POINTS FOR CAERO3  PUT POINTS 1-4 IN BGPDT
!
            DO j = 13 , 24
               bnd(j) = 0.0
            ENDDO
            vx1(3) = 0.0
            kk = 1
            ASSIGN 50 TO back
            ibs = Ncrd + 1
            vx1(1) = 0.0
            vx1(2) = 0.0
            bnd(1) = 0.0
            bnd(2) = 0.0
            GOTO 900
         ELSE
            j = j + 4 + Iz(j+3)
         ENDIF
      ENDDO
      GOTO 1500
 50   ASSIGN 100 TO back
      vx1(1) = X12
      vx1(2) = 0.0
      bnd(7) = X12
      bnd(8) = 0.0
      GOTO 900
 100  vx1(1) = Xp4
      vx1(2) = Yp4
      bnd(5) = Xp4
      bnd(6) = Yp4
      ASSIGN 150 TO back
      GOTO 900
 150  ASSIGN 200 TO back
      vx1(1) = Xp4 + X43
      vx1(2) = Yp4
      bnd(11) = vx1(1)
      bnd(12) = vx1(2)
      GOTO 900
!
!     ADD POINTS 5 AND 6 IF THEY EXIST
!
 200  bnd(3) = bnd(5)
      bnd(4) = bnd(6)
      IF ( crank1 ) THEN
         vx1(1) = Z(ipid+4)
         vx1(2) = Z(ipid+5)
         bnd(3) = vx1(1)
         bnd(4) = vx1(2)
         ASSIGN 250 TO back
         GOTO 900
      ENDIF
 250  bnd(9) = bnd(11)
      bnd(10) = bnd(12)
      IF ( crank2 ) THEN
         vx1(1) = Z(ipid+6)
         vx1(2) = Z(ipid+7)
         bnd(9) = vx1(1)
         bnd(10) = vx1(2)
         ASSIGN 300 TO back
         GOTO 900
      ENDIF
!
!      ADD CONTROLS
!
 300  IF ( .NOT.cntrl1 ) GOTO 600
      ASSIGN 350 TO back
      vx1(1) = Z(ipid+8)
      vx1(2) = Z(ipid+9)
      bnd(15) = vx1(1)
      bnd(16) = vx1(2)
      GOTO 900
 350  ASSIGN 400 TO back
      vx1(1) = Z(ipid+10)
      vx1(2) = Z(ipid+11)
      bnd(13) = vx1(1)
      bnd(14) = vx1(2)
      GOTO 900
 400  ASSIGN 450 TO back
      vx1(1) = Z(ipid+12)
      vx1(2) = Z(ipid+13)
      bnd(17) = vx1(1)
      bnd(18) = vx1(2)
      GOTO 900
 450  ASSIGN 500 TO back
      vx1(1) = Z(ipid+14)
      vx1(2) = Z(ipid+15)
      bnd(21) = vx1(1)
      bnd(22) = vx1(2)
      GOTO 900
 500  IF ( .NOT.cntrl2 ) GOTO 600
      ASSIGN 550 TO back
      vx1(1) = Z(ipid+16)
      vx1(2) = Z(ipid+17)
      bnd(19) = vx1(1)
      bnd(20) = vx1(2)
      GOTO 900
 550  ASSIGN 600 TO back
      vx1(1) = Z(ipid+18)
      vx1(2) = Z(ipid+19)
      bnd(23) = vx1(1)
      bnd(24) = vx1(2)
      GOTO 900
!
!     CONNECT POINT TO BOXES FOR ECTA
!
 600  eidb = Eid
      cid(1) = ibs
      cid(2) = ibs + 1
      cid(5) = ibs
      IF ( crank1 ) THEN
         IF ( crank2 ) THEN
            cid(3) = ibs + 5
            cid(4) = ibs + 4
         ELSE
            cid(3) = ibs + 3
            cid(4) = ibs + 4
         ENDIF
      ELSEIF ( crank2 ) THEN
         cid(3) = ibs + 4
         cid(4) = ibs + 2
      ELSE
         cid(3) = ibs + 3
         cid(4) = ibs + 2
      ENDIF
      CALL write(Ecta,necta,6,0)
      eidb = eidb + 1
      cid(1) = ibs + 2
      cid(2) = ibs + 3
      cid(5) = ibs + 2
      ibs = ibs + 4
      IF ( .NOT.(.NOT.crank1 .AND. .NOT.crank2) ) THEN
         IF ( crank1 .AND. crank2 ) THEN
            cid(3) = ibs + 1
            cid(4) = ibs
            ibs = ibs + 2
         ELSE
            cid(3) = ibs
            cid(4) = cid(5)
            ibs = ibs + 1
         ENDIF
         CALL write(Ecta,necta,6,0)
         eidb = eidb + 1
      ENDIF
      IF ( cntrl1 ) THEN
         cid(1) = ibs + 2
         cid(2) = ibs + 3
         cid(3) = ibs + 1
         cid(4) = ibs
         cid(5) = ibs + 2
         CALL write(Ecta,necta,6,0)
         eidb = eidb + 1
         IF ( cntrl2 ) THEN
            cid(3) = ibs + 5
            cid(4) = ibs + 4
            CALL write(Ecta,necta,6,0)
         ENDIF
      ENDIF
!
!     FIND CONTROL POINTS FOR ELEMENT
!
      CALL apdoe(Nspan,Iz,Naef1,Naef2,ilw,nww)
      IF ( ilw==0 ) GOTO 1300
      IF ( nww<6 ) GOTO 1300
      IF ( mod(nww,2)/=0 ) GOTO 1300
      ilc1 = 0
      ilc2 = 0
      nwc1 = 0
      nwc2 = 0
      IF ( cntrl1 ) THEN
         CALL apdoe(Nchord,Iz,Naef1,Naef2,ilc1,nwc1)
         IF ( ilc1==0 ) GOTO 1200
         IF ( nwc1<6 ) GOTO 1200
         IF ( mod(nwc1,2)/=0 ) GOTO 1200
         IF ( cntrl2 ) THEN
            CALL apdoe(Lspan,Iz,Naef1,Naef2,ilc2,nwc2)
            IF ( ilc2==0 ) GOTO 1100
            IF ( nwc2<6 ) GOTO 1100
            IF ( mod(nwc2,2)/=0 ) GOTO 1100
         ENDIF
      ENDIF
      ihead(8) = nww/2
      ihead(9) = nwc1/2
      ihead(10) = nwc2/2
      ihead(2) = ihead(8) + ihead(9) + ihead(10)
      Nk = Nk + ihead(2)
      Nj = Nj + ihead(2)
      Iz(Ca3s+n+4) = ihead(2)
      Iz(Ca3s+n+5) = 1
!
!     START THE ACPT AND ADD THE CONTROL POINTS IN A LOOP
!
      CALL write(Acpt,ihead,10,0)
      CALL write(Acpt,bnd,24,0)
      eidb = Eid - 1
      kk = 2
      nn = nww
      kkk = ilw - 1
      ASSIGN 650 TO ret
!
!     PUT CONTROL POINTS IN TABLE
!
      j = 2
      GOTO 800
 650  IF ( ihead(9)==0 ) GOTO 750
      ASSIGN 700 TO ret
      nn = nwc1
      kkk = ilc1 - 1
      j = 2
      GOTO 800
 700  IF ( ihead(10)/=0 ) THEN
         ASSIGN 750 TO ret
         nn = nwc2
         kkk = ilc2 - 1
         j = 2
         GOTO 800
      ENDIF
 750  CALL write(Acpt,0,0,1)
!
!     GEOMETRY CHECKS
!
      nm = 0
      IF ( bnd(1)>bnd(3) ) nm = 1
      IF ( bnd(3)>bnd(5) ) nm = 1
      IF ( bnd(15)>bnd(17) ) nm = 1
      IF ( cntrl2 .AND. bnd(17)>bnd(19) ) nm = 1
      IF ( bnd(16)<bnd(14) ) nm = 1
      IF ( bnd(18)<bnd(22) ) nm = 1
      IF ( bnd(20)<bnd(24) ) nm = 1
      IF ( nm==1 ) nogo = 1
      IF ( nm==1 ) WRITE (Not,99001) Ufm , Eid
99001 FORMAT (A23,' 2278, PLANFORM GEOMETRY FOR CAERO3 ID',I9,' IS IN ERROR',/5X,'CHECK SWEEP  ANGLE FOR LEADING EDGE ',            &
             &'OR CONTROL SURFACE HINGE LINE.')
      CYCLE
 800  vx1(1) = Z(kkk+j)
      vx1(2) = Z(kkk+j+1)
      CALL write(Acpt,vx1,2,0)
      ASSIGN 850 TO back
      GOTO 900
 850  j = j + 2
      IF ( j<=nn ) GOTO 800
      GOTO ret
!
!     BGPA  GPL  USET
!
 900  CALL gmmats(Acpl,3,3,0,vx1,3,1,0,vx2)
      DO k = 1 , 3
         vx2(k) = vx2(k) + Rb1(k)
      ENDDO
      CALL write(Bgpa,acsix,4,0)
      IF ( kk==2 ) THEN
         eidb = eidb + 1
         icid = eidb
      ELSE
         Cidbx = Cidbx + 1
         icid = Cidbx
      ENDIF
      CALL write(Gpla,icid,1,0)
      CALL write(Useta,auset(1,kk),6,0)
!
!     SIL AND EQEXIN
!
      Ncrd = Ncrd + 1
      silc = silc + 6
      Isiln = Isiln + 6
      Luseta = silc
      sildx(2) = 10*silc + 1
      CALL write(Sila,silc,1,0)
      CALL write(Scr2,Isiln,1,0)
      CALL write(Scr2,silc,1,0)
      CALL write(Scr1,icid,2,0)
      GOTO back
   ENDDO
   Silb = silc
   IF ( nogo==1 ) GOTO 1600
 1000 RETURN
!
!     ERROR MESSAGES
!
 1100 i = Lspan
   GOTO 1400
 1200 i = Nchord
   GOTO 1400
 1300 i = Nspan
 1400 WRITE (Not,99002) Ufm , i , Eid
99002 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ','CARD ID',I9,/29X,'ASSOCIATED WITH CAERO3 ID',I9)
   GOTO 1600
 1500 CALL emsg(0,2323,1,2,0)
   WRITE (Not,99003) Pid , Eid
99003 FORMAT (10X,16HPAERO3 CARD NO. ,I8,31H REFERENCED BY CAERO3 CARD NO. ,I8,15H DOES NOT EXIST)
 1600 CALL mesage(-61,0,nam)
   GOTO 1000
END SUBROUTINE apd3
