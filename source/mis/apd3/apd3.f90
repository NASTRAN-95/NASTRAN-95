!*==apd3.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd3
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
   INTEGER , DIMENSION(4) :: acsix
   INTEGER , DIMENSION(6,2) :: auset
   INTEGER :: back , eidb , i , i17 , i18 , i19 , i20 , ibs , icid , ilc1 , ilc2 , ilw , ipid , j , k , kk , kkk , lca , n , nc3 ,  &
            & nm , nn , nogo , nwc1 , nwc2 , nww , pspa , ret , silc , uk , usa
   REAL , DIMENSION(24) :: bnd
   INTEGER , DIMENSION(5) :: cid
   LOGICAL :: cntrl1 , cntrl2 , crank1 , crank2
   INTEGER , DIMENSION(16) :: ic
   INTEGER , DIMENSION(10) :: ihead
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(2) :: necta , sildx
   REAL , DIMENSION(3) :: rb1 , vx1 , vx2
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
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (acsix(2),vx2(1)) , (sildx(1),icid) , &
!>>>>    & (Z(1),Iz(1)) , (Eid,Ic(1)) , (crank1,ihead(3)) , (crank2,ihead(4)) , (cntrl1,ihead(5)) , (cntrl2,ihead(6))
   DATA nam/4HAPD3 , 4H    /
!
   nogo = 0
   lca = 16
   nc3 = ((ca3e-ca3s)+1)/lca
   ncam = ncam + nc3
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
   ihead(1) = 3
   silc = silb
!
!     LOOP ON NC3 MOVING CAERO3 CARD TO COMMON
!
   DO i = 1 , nc3
      n = (i-1)*lca - 1
      DO j = 1 , lca
         ic(j) = iz(ca3s+n+j)
      ENDDO
      mcstm = mcstm + 1
      iz(ca3s+n+2) = mcstm
      iz(ca3s+n+8) = 3
      acsix(1) = mcstm
!
!     GET POINTS IN PROPER COORD SYSTEM
!
      CALL apdcs
!
!     FIND PAERO3 CARD
!
      j = pa3s
      DO WHILE ( j<pa3e )
         IF ( iz(j)==pid ) THEN
            ipid = j
            ihead(7) = iz(ipid+1)
            crank1 = .FALSE.
            crank2 = .FALSE.
            IF ( z(ipid+5)>0.0 ) crank1 = .TRUE.
            IF ( z(ipid+7)>0.0 ) crank2 = .TRUE.
            cntrl1 = .FALSE.
            cntrl2 = .FALSE.
            IF ( iz(ipid+2)>0 ) cntrl1 = .TRUE.
            IF ( iz(ipid+2)==2 ) cntrl2 = .TRUE.
!
!     GENERATE AERO POINTS FOR CAERO3  PUT POINTS 1-4 IN BGPDT
!
            DO j = 13 , 24
               bnd(j) = 0.0
            ENDDO
            vx1(3) = 0.0
            kk = 1
            ASSIGN 50 TO back
            ibs = ncrd + 1
            vx1(1) = 0.0
            vx1(2) = 0.0
            bnd(1) = 0.0
            bnd(2) = 0.0
            GOTO 900
         ELSE
            j = j + 4 + iz(j+3)
         ENDIF
      ENDDO
      GOTO 1500
 50   ASSIGN 100 TO back
      vx1(1) = x12
      vx1(2) = 0.0
      bnd(7) = x12
      bnd(8) = 0.0
      GOTO 900
 100  vx1(1) = xp4
      vx1(2) = yp4
      bnd(5) = xp4
      bnd(6) = yp4
      ASSIGN 150 TO back
      GOTO 900
 150  ASSIGN 200 TO back
      vx1(1) = xp4 + x43
      vx1(2) = yp4
      bnd(11) = vx1(1)
      bnd(12) = vx1(2)
      GOTO 900
!
!     ADD POINTS 5 AND 6 IF THEY EXIST
!
 200  bnd(3) = bnd(5)
      bnd(4) = bnd(6)
      IF ( crank1 ) THEN
         vx1(1) = z(ipid+4)
         vx1(2) = z(ipid+5)
         bnd(3) = vx1(1)
         bnd(4) = vx1(2)
         ASSIGN 250 TO back
         GOTO 900
      ENDIF
 250  bnd(9) = bnd(11)
      bnd(10) = bnd(12)
      IF ( crank2 ) THEN
         vx1(1) = z(ipid+6)
         vx1(2) = z(ipid+7)
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
      vx1(1) = z(ipid+8)
      vx1(2) = z(ipid+9)
      bnd(15) = vx1(1)
      bnd(16) = vx1(2)
      GOTO 900
 350  ASSIGN 400 TO back
      vx1(1) = z(ipid+10)
      vx1(2) = z(ipid+11)
      bnd(13) = vx1(1)
      bnd(14) = vx1(2)
      GOTO 900
 400  ASSIGN 450 TO back
      vx1(1) = z(ipid+12)
      vx1(2) = z(ipid+13)
      bnd(17) = vx1(1)
      bnd(18) = vx1(2)
      GOTO 900
 450  ASSIGN 500 TO back
      vx1(1) = z(ipid+14)
      vx1(2) = z(ipid+15)
      bnd(21) = vx1(1)
      bnd(22) = vx1(2)
      GOTO 900
 500  IF ( .NOT.cntrl2 ) GOTO 600
      ASSIGN 550 TO back
      vx1(1) = z(ipid+16)
      vx1(2) = z(ipid+17)
      bnd(19) = vx1(1)
      bnd(20) = vx1(2)
      GOTO 900
 550  ASSIGN 600 TO back
      vx1(1) = z(ipid+18)
      vx1(2) = z(ipid+19)
      bnd(23) = vx1(1)
      bnd(24) = vx1(2)
      GOTO 900
!
!     CONNECT POINT TO BOXES FOR ECTA
!
 600  eidb = eid
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
      CALL write(ecta,necta,6,0)
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
         CALL write(ecta,necta,6,0)
         eidb = eidb + 1
      ENDIF
      IF ( cntrl1 ) THEN
         cid(1) = ibs + 2
         cid(2) = ibs + 3
         cid(3) = ibs + 1
         cid(4) = ibs
         cid(5) = ibs + 2
         CALL write(ecta,necta,6,0)
         eidb = eidb + 1
         IF ( cntrl2 ) THEN
            cid(3) = ibs + 5
            cid(4) = ibs + 4
            CALL write(ecta,necta,6,0)
         ENDIF
      ENDIF
!
!     FIND CONTROL POINTS FOR ELEMENT
!
      CALL apdoe(nspan,iz,naef1,naef2,ilw,nww)
      IF ( ilw==0 ) GOTO 1300
      IF ( nww<6 ) GOTO 1300
      IF ( mod(nww,2)/=0 ) GOTO 1300
      ilc1 = 0
      ilc2 = 0
      nwc1 = 0
      nwc2 = 0
      IF ( cntrl1 ) THEN
         CALL apdoe(nchord,iz,naef1,naef2,ilc1,nwc1)
         IF ( ilc1==0 ) GOTO 1200
         IF ( nwc1<6 ) GOTO 1200
         IF ( mod(nwc1,2)/=0 ) GOTO 1200
         IF ( cntrl2 ) THEN
            CALL apdoe(lspan,iz,naef1,naef2,ilc2,nwc2)
            IF ( ilc2==0 ) GOTO 1100
            IF ( nwc2<6 ) GOTO 1100
            IF ( mod(nwc2,2)/=0 ) GOTO 1100
         ENDIF
      ENDIF
      ihead(8) = nww/2
      ihead(9) = nwc1/2
      ihead(10) = nwc2/2
      ihead(2) = ihead(8) + ihead(9) + ihead(10)
      nk = nk + ihead(2)
      nj = nj + ihead(2)
      iz(ca3s+n+4) = ihead(2)
      iz(ca3s+n+5) = 1
!
!     START THE ACPT AND ADD THE CONTROL POINTS IN A LOOP
!
      CALL write(acpt,ihead,10,0)
      CALL write(acpt,bnd,24,0)
      eidb = eid - 1
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
 750  CALL write(acpt,0,0,1)
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
      IF ( nm==1 ) WRITE (not,99001) ufm , eid
99001 FORMAT (A23,' 2278, PLANFORM GEOMETRY FOR CAERO3 ID',I9,' IS IN ERROR',/5X,'CHECK SWEEP  ANGLE FOR LEADING EDGE ',            &
             &'OR CONTROL SURFACE HINGE LINE.')
      CYCLE
 800  vx1(1) = z(kkk+j)
      vx1(2) = z(kkk+j+1)
      CALL write(acpt,vx1,2,0)
      ASSIGN 850 TO back
      GOTO 900
 850  j = j + 2
      IF ( j<=nn ) GOTO 800
      GOTO ret
!
!     BGPA  GPL  USET
!
 900  CALL gmmats(acpl,3,3,0,vx1,3,1,0,vx2)
      DO k = 1 , 3
         vx2(k) = vx2(k) + rb1(k)
      ENDDO
      CALL write(bgpa,acsix,4,0)
      IF ( kk==2 ) THEN
         eidb = eidb + 1
         icid = eidb
      ELSE
         cidbx = cidbx + 1
         icid = cidbx
      ENDIF
      CALL write(gpla,icid,1,0)
      CALL write(useta,auset(1,kk),6,0)
!
!     SIL AND EQEXIN
!
      ncrd = ncrd + 1
      silc = silc + 6
      isiln = isiln + 6
      luseta = silc
      sildx(2) = 10*silc + 1
      CALL write(sila,silc,1,0)
      CALL write(scr2,isiln,1,0)
      CALL write(scr2,silc,1,0)
      CALL write(scr1,icid,2,0)
      GOTO back
   ENDDO
   silb = silc
   IF ( nogo==1 ) GOTO 1600
 1000 RETURN
!
!     ERROR MESSAGES
!
 1100 i = lspan
   GOTO 1400
 1200 i = nchord
   GOTO 1400
 1300 i = nspan
 1400 WRITE (not,99002) ufm , i , eid
99002 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ','CARD ID',I9,/29X,'ASSOCIATED WITH CAERO3 ID',I9)
   GOTO 1600
 1500 CALL emsg(0,2323,1,2,0)
   WRITE (not,99003) pid , eid
99003 FORMAT (10X,16HPAERO3 CARD NO. ,I8,31H REFERENCED BY CAERO3 CARD NO. ,I8,15H DOES NOT EXIST)
 1600 CALL mesage(-61,0,nam)
   GOTO 1000
END SUBROUTINE apd3
