!*==apd2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd2(Iopt,Cao1,Cao2,Ncore,Id)
   IMPLICIT NONE
   USE c_apd12c
   USE c_apd1c
   USE c_apd1d
   USE c_blank
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
   INTEGER , DIMENSION(1) :: Cao1
   INTEGER , DIMENSION(1) :: Cao2
   INTEGER :: Ncore
   INTEGER :: Id
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) :: acpl
   INTEGER :: acsib , back , bet , eidb , i , iao , iaop , iarb , iavr , ibc , ibt , icg , ichord , icid , idelx , ids , iee ,      &
            & ifla1 , ifla2 , inas , inasb , inb , inbea1 , inbea2 , inc , infl , insbea , int121 , int122 , ipc , iret , iria ,    &
            & irib , irsb , isg , ispan , ith1 , ith1a , ith2 , ith2a , ix , ixic , ixis1 , ixis2 , ixlam , ixle , ixte , iyb ,     &
            & iys , izb , izs , j , jchord , jspan , k , kk , kt1 , l , lrib , lrsb , lth1 , lth2 , m , na , nas , nass , nb ,      &
            & nbea1 , nby , nbz , nfl , nint , nja , nka , np , nrib , nrsb , nsb , nsbea , nstrip
   INTEGER , DIMENSION(4) :: acsix
   INTEGER , DIMENSION(5) :: cid
   REAL :: d1 , d2 , oldx , p1 , temp , width
   INTEGER , DIMENSION(1) :: iax , iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(6) :: necta
   INTEGER :: nt121 , nt122 , nth1 , nth2 , nto , ntp , nty , ntys , ntz , ntzs , nwr , pc , ppc
   REAL , SAVE :: pio180
   REAL , DIMENSION(3) :: rb1 , vx1 , vx2
   INTEGER , DIMENSION(2) :: sildx
   INTEGER , DIMENSION(3) , SAVE :: type
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(1),eidb) , (necta(2),cid(1)) , (Key(2),Np) ,          &
!>>>>    & (Key(3),Nstrip) , (Key(4),Ntp) , (Eid,Iax(1)) , (sildx(1),icid) , (acsix(1),acsib) , (acsix(2),vx2(1))
   DATA type/1HZ , 2HZY , 1HY/
   DATA nam/4HAPD2 , 4H    /
   DATA pio180/.0174532925/
!
   ibc = 0
   nb = 0
   IF ( Iopt==1 ) THEN
!
!     CAERO2 WITH CAERO1 ATTACHED
!
      ipc = 1
      ids = Id
      GOTO 1700
   ELSE
!
!     PROCESS CAERO2 WITHOUT CAERO1 ATTACHED
!
      np = 0
      nstrip = 0
      ntp = 0
      nas = 0
      ipc = 1
      ids = Cao2(1)
   ENDIF
 100  IF ( Cao2(ipc)<0 ) GOTO 1600
 200  pc = Cao2(ipc+1) - 1
   IF ( ibc/=nb ) GOTO 700
!
!     LOOP OVER ALL CAERO2 WITH CURRENT ID TO SET UP POINTERS
!
   nb = 0
   ibc = 0
   nbz = 0
   nby = 0
   ntz = 0
   nty = 0
   ntzs = 0
   ntys = 0
   nbea1 = 0
   nsbea = 0
   nfl = 0
   nt121 = 0
   nt122 = 0
   k = ipc
 300  IF ( Cao2(k)/=ids ) GOTO 600
   nb = nb + 1
   l = Cao2(k+1) - 1
   DO m = 1 , 7
      iax(m) = iz(l+m)
   ENDDO
   ASSIGN 400 TO iret
   GOTO 800
 400  IF ( bet/=3 ) THEN
      nbz = nbz + 1
      ntz = ntz + nint
      ntzs = ntzs + nsb
      IF ( bet==1 ) GOTO 500
   ENDIF
   nby = nby + 1
   nty = nty + nint
   ntys = ntys + nsb
 500  nbea1 = nbea1 + nint
   nsbea = nsbea + nsb
   nt121 = nt121 + nth1
   nt122 = nt122 + nth2
   nfl = nfl + kt1
   k = k + 2
   IF ( k<=ncam2*2 ) GOTO 300
 600  ids = Cao2(k)
   nto = ntp + ntz + nty
   nas = nasb
!
!     NOW SET UP POINTERS TO BUILD ACPT IN CORE
!
   i = Ncore
   iz(i) = 2
   iz(i+1) = ntp
   iz(i+2) = ntp*2
   iz(i+3) = np
   iz(i+4) = nb
   iz(i+5) = ntp
   iz(i+6) = nbz
   iz(i+7) = nby
   iz(i+8) = ntz
   iz(i+9) = nty
   iz(i+10) = nto
   iz(i+11) = ntzs
   iz(i+12) = ntys
   iz(i+13) = nstrip
   inc = i + 14
   inb = inc + np
   inas = inb + np
   inbea1 = inas + np
   inbea2 = inbea1 + nb
   insbea = inbea2 + nb
   izb = insbea + nb
   iyb = izb + nb
   iavr = iyb + nb
   iarb = iavr + nb
   infl = iarb + nb
   ixle = infl + nb
   ixte = ixle + nb
   int121 = ixte + nb
   int122 = int121 + nb
   izs = int122 + nb
   iys = izs + nb + nstrip
   iee = iys + nb + nstrip
   isg = iee + nstrip
   icg = isg + nstrip
   ix = icg + nstrip
   idelx = ix + ntp + nbea1
   ixic = idelx + ntp + nbea1
   ixlam = ixic + ntp
   iao = ixlam + ntp
   ixis1 = iao + nsbea
   ixis2 = ixis1 + nsbea
   iaop = ixis2 + nsbea
   iria = iaop + nsbea
   inasb = iria + nbea1
   ifla1 = inasb + nas
   ifla2 = ifla1 + nfl
   ith1a = ifla2 + nfl
   ith2a = ith1a + nt121
   nwr = ith2a + nt122 - Ncore
   na = ith2a + nt122 - 1
   i = na + np*6 + 1
   IF ( i>left ) CALL mesage(-8,0,nam)
!
!     IF PANELS EXIST INSERT DATA FROM SCRATCH FILES
!
   IF ( np/=0 ) THEN
      nass = na
      CALL write(scr3,0,0,1)
      CALL write(scr4,0,0,1)
      CALL write(scr5,0,0,1)
      CALL close(scr3,1)
      CALL close(scr4,1)
      CALL close(scr5,1)
      CALL gopen(scr3,z(buf10),0)
      CALL gopen(scr4,z(buf11),0)
      CALL gopen(scr5,z(buf12),0)
      DO i = 1 , np
         CALL fread(scr5,iz(inc),1,0)
         CALL fread(scr5,iz(inb),1,0)
         CALL fread(scr5,k,1,0)
         DO j = 1 , 6
            iz(na+j) = iz(k+j)
         ENDDO
         inc = inc + 1
         inb = inb + 1
         na = na + 6
      ENDDO
      DO i = 1 , nstrip
         CALL fread(scr3,iz(iys),1,0)
         CALL fread(scr3,iz(izs),1,0)
         CALL fread(scr3,iz(iee),1,0)
         CALL fread(scr3,iz(isg),1,0)
         CALL fread(scr3,iz(icg),1,0)
         iys = iys + 1
         izs = izs + 1
         iee = iee + 1
         isg = isg + 1
         icg = icg + 1
      ENDDO
      DO i = 1 , ntp
         CALL fread(scr4,iz(ixic),1,0)
         CALL fread(scr4,iz(idelx),1,0)
         CALL fread(scr4,iz(ixlam),1,0)
         z(ix) = z(ixic) + .5*z(idelx)
         ixic = ixic + 1
         idelx = idelx + 1
         ixlam = ixlam + 1
         ix = ix + 1
      ENDDO
      CALL close(scr3,1)
      CALL close(scr4,1)
      CALL close(scr5,1)
!
!     FILL IN ASSOCIATED BODIES
!
      na = nass
      DO i = 1 , np
         l = 0
         DO j = 1 , 6
            IF ( iz(na+j)/=0 ) THEN
               l = l + 1
               ibt = ipc
               DO k = 1 , nb
                  m = Cao2(ibt+1)
                  IF ( iz(m)/=iz(na+j) ) THEN
                     ibt = ibt + 2
                  ELSE
                     iz(inasb) = k
                     inasb = inasb + 1
                     GOTO 620
                  ENDIF
               ENDDO
               GOTO 2100
            ENDIF
 620     ENDDO
         iz(inas) = l
         inas = inas + 1
         na = na + 6
      ENDDO
   ENDIF
 700  ibc = ibc + 1
!
!     MOVE TO COMMON
!
   DO j = 1 , 16
      iax(j) = iz(j+pc)
   ENDDO
   iz(pc+2) = acsid
   acsib = acsid
   x4 = x1
   y4 = y1 + 1.0
   z4 = z1
   x43 = x12
   igid = -igid
   CALL apdcs
   igid = -igid
!
!     MOVE AERO CORD SYS TO ICPL
!
   IF ( acsid/=0 ) THEN
      DO i = 1 , 14
         icpl(i) = iz(iacs+i-1)
      ENDDO
   ENDIF
   ASSIGN 1000 TO iret
!
!     FIND PAERO2 CARD
!
 800  IF ( pa2s/=0 ) THEN
      DO j = pa2s , pa2e , 15
         IF ( pid==iz(j) ) GOTO 900
      ENDDO
   ENDIF
   CALL emsg(0,2323,1,2,0)
   WRITE (not,99001) pid , eid
99001 FORMAT (10X,'PAERO2 CARD NO.',I9,' REFERENCED BY CAERO2 CARD NO.',I9,' BUT DOES NOT EXIST.')
!
!     ERROR MESSAGES
!
   CALL mesage(-61,0,nam)
   GOTO 2000
 900  ppc = j
!
!     GET BODY TYPE AND NUMBER OF ELEMENTS
!
   nsb = nspan
   nint = nchord
   bet = iz(ppc+1)
   DO j = 1 , 3
      IF ( bet==type(j) ) EXIT
   ENDDO
   bet = j
   lth1 = iz(ppc+7)
   lth2 = iz(ppc+8)
   nth1 = 0
   nth2 = 0
   kt1 = 0
   IF ( lspan/=0 ) THEN
      CALL apdoe(lspan,iz,naef1,naef2,ispan,jspan)
      IF ( ispan==0 ) THEN
         CALL emsg(0,2326,1,2,0)
         WRITE (not,99008) eid , lspan
         CALL mesage(-61,0,nam)
         GOTO 2000
      ELSE
         nsb = jspan - 1
      ENDIF
   ENDIF
   IF ( lchord/=0 ) THEN
      CALL apdoe(lchord,iz,naef1,naef2,ichord,jchord)
      IF ( ichord==0 ) THEN
         CALL emsg(0,2327,1,2,0)
         WRITE (not,99008) eid , lchord
         CALL mesage(-61,0,nam)
         GOTO 2000
      ELSE
         nint = jchord - 1
      ENDIF
   ENDIF
   IF ( nint/=0 ) THEN
      kt1 = kt1 + 1
      IF ( iz(ppc+9)==0 ) THEN
         WRITE (not,99002) ufm , eid
99002    FORMAT (A23,' 2276, THI1 AND THN1 REQUIRED FOR CAERO2',I9,1H.)
         CALL mesage(-61,0,nam)
         GOTO 2000
      ELSE
         IF ( iz(ppc+11)/=0 ) THEN
            kt1 = kt1 + 1
            IF ( iz(ppc+13)/=0 ) kt1 = kt1 + 1
         ENDIF
         IF ( lth1==0 ) THEN
            j = lth1
            GOTO 2300
         ELSE
            CALL apdoe(lth1,iz,naef1,naef2,ith1,nth1)
            IF ( ith1==0 ) THEN
               j = lth1
               GOTO 2300
            ELSEIF ( lth2/=0 ) THEN
               CALL apdoe(lth2,iz,naef1,naef2,ith2,nth2)
               IF ( ith2==0 ) THEN
                  j = lth2
                  GOTO 2300
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   IF ( nsb<2 ) THEN
      WRITE (not,99003) ufm , eid
99003 FORMAT (A23,' 2277, CAERO2 BODY',I9,' DOES NOT HAVE ENOUGH ','SLENDER ELEMENTS.')
      CALL mesage(-61,0,nam)
      GOTO 2000
   ELSE
      GOTO iret
   ENDIF
!
!     PUT IN TERMS FOR SOME BODY ARRAYS
!
 1000 iz(inbea1) = nint
   IF ( ibc>1 .AND. bet<iz(inbea2-1) ) GOTO 2000
   iz(inbea2) = bet
   iz(insbea) = nsb
   z(izb) = ra1(3)
   z(iyb) = ra1(2)
   z(izs) = ra1(3)
   z(iys) = ra1(2)
   z(iavr) = z(ppc+3)
   z(iarb) = z(ppc+4)
   iz(infl) = kt1
   iz(int121) = nth1
   iz(int122) = nth2
   inbea1 = inbea1 + 1
   inbea2 = inbea2 + 1
   insbea = insbea + 1
   izb = izb + 1
   iyb = iyb + 1
   izs = izs + 1
   iys = iys + 1
   iavr = iavr + 1
   iarb = iarb + 1
   infl = infl + 1
   int121 = int121 + 1
   int122 = int122 + 1
!
!     ADD SOME MISC ARRAYS
!
   IF ( nth1/=0 ) THEN
      DO i = 1 , nth1
         z(ith1a) = z(ith1+i)*pio180
         ith1a = ith1a + 1
      ENDDO
      IF ( nth2/=0 ) THEN
         DO i = 1 , nth2
            z(ith2a) = z(ith2+i)*pio180
            ith2a = ith2a + 1
         ENDDO
      ENDIF
      k = ppc + 9
      IF ( iz(k)/=1 .AND. iz(k+1)/=nint .AND. nth2==0 ) GOTO 2200
      DO i = 1 , kt1
         iz(ifla1) = iz(k)
         iz(ifla2) = iz(k+1)
         k = k + 2
         IF ( iz(ifla1)>iz(ifla2) ) GOTO 2200
         IF ( iz(ifla2)>nint ) GOTO 2200
         IF ( i/=1 ) THEN
            IF ( iz(ifla1)<=iz(ifla2-1) ) GOTO 2200
         ENDIF
         ifla1 = ifla1 + 1
         ifla2 = ifla2 + 1
      ENDDO
   ENDIF
   lrsb = iz(ppc+5)
   lrib = iz(ppc+6)
   IF ( lrsb/=0 ) THEN
      CALL apdoe(lrsb,iz,naef1,naef2,irsb,nrsb)
      IF ( irsb==0 ) THEN
         j = lrsb
         GOTO 2300
      ELSEIF ( nrsb/=nsb+1 ) THEN
         j = lrsb
         GOTO 2300
      ENDIF
   ENDIF
   IF ( lrib/=0 ) THEN
      CALL apdoe(lrib,iz,naef1,naef2,irib,nrib)
      IF ( irib==0 ) THEN
         j = lrib
         GOTO 2300
      ELSEIF ( nrib/=nint+1 ) THEN
         j = lrib
         GOTO 2300
      ENDIF
   ENDIF
   width = z(ppc+3)
!
!     GENERATE ELEMENTS
!
   eidb = eid - 1
   cidbx = cidbx + 1
   vx1(2) = ra1(2)
   vx1(3) = ra1(3)
!
!     PUT IN PROPER MASKS FOR USET
!
   IF ( bet/=1 ) THEN
      auset(2,2) = uk
      auset(6,2) = uk
      IF ( bet/=2 ) THEN
         auset(3,2) = usa
         auset(5,2) = usa
      ENDIF
   ENDIF
!
!     BUMP NJ AND NK
!
   nja = nsb + nint
   nka = nsb*2
   nj = nj + nja
   nk = nk + nka
   iz(Ncore+1) = iz(Ncore+1) + nja
   iz(Ncore+2) = iz(Ncore+2) + nka
   IF ( bet==2 ) THEN
      nj = nj + nja
      nk = nk + nka
      iz(Ncore+1) = iz(Ncore+1) + nja
      iz(Ncore+2) = iz(Ncore+2) + nka
   ENDIF
   i = 1
 1100 eidb = eidb + 1
   cid(1) = cidbx
   cidbx = cidbx + 1
   cid(2) = cidbx
   cid(5) = eidb
!
!     GRID POINTS IN AERO SYSTEM
!
   IF ( i==1 ) THEN
      ASSIGN 1200 TO back
      icid = cid(1)
      IF ( lspan==0 ) vx1(1) = ra1(1) + (x12/nsb)*(i-1)
      IF ( lspan/=0 ) vx1(1) = ra1(1) + z(ispan+i)*x12
      oldx = vx1(1)
      z(ixle) = oldx
      z(ixis1) = oldx
      ixis1 = ixis1 + 1
      kk = 1
      GOTO 1400
   ENDIF
 1200 ASSIGN 1300 TO back
   icid = cid(2)
   IF ( lspan==0 ) vx1(1) = ra1(1) + (x12/nsb)*i
   IF ( lspan/=0 ) vx1(1) = ra1(1) + z(ispan+i+1)*x12
   z(ixte) = vx1(1)
   z(ixis2) = vx1(1)
   ixis2 = ixis2 + 1
   IF ( i/=1 ) z(ixis1) = oldx
   IF ( i/=1 ) ixis1 = ixis1 + 1
   kk = 1
   GOTO 1400
 1300 ASSIGN 1500 TO back
!
!     A0 AND AOP
!
   z(iao) = width
   z(iaop) = 0.0
   IF ( lrsb/=0 ) THEN
      z(iao) = (z(irsb+i)+z(irsb+i+1))*.5
      z(iaop) = (z(irsb+i+1)-z(irsb+i))/(vx1(1)-oldx)
   ENDIF
   iao = iao + 1
   iaop = iaop + 1
   temp = (vx1(1)+oldx)/2.0
   oldx = vx1(1)
   vx1(1) = temp
   icid = cid(5)
   kk = 2
!
!     CONVERT TO BASIC
!
 1400 IF ( acsid==0 ) THEN
      DO k = 1 , 3
         vx2(k) = vx1(k)
      ENDDO
   ELSE
      CALL gmmats(acpl,3,3,0,vx1,3,1,0,vx2)
      DO k = 1 , 3
         vx2(k) = vx2(k) + rb1(k)
      ENDDO
   ENDIF
!
!     PUT OUT BGPDT GPL USET
!
   CALL write(bgpa,acsix,4,0)
   CALL write(gpla,icid,1,0)
   CALL write(useta,auset(1,kk),6,0)
!
!     BUMP POINTERS
!     PUT OUT SIL EQEXIN SILGA
!
   ncrd = ncrd + 1
   silb = silb + 6
   isiln = isiln + 6
   luseta = silb
   sildx(2) = 10*silb + 1
   CALL write(sila,silb,1,0)
   CALL write(scr2,isiln,1,0)
   CALL write(scr2,silb,1,0)
   CALL write(scr1,icid,2,0)
   GOTO back
!
!     PUT OUT ECT
!
 1500 cid(1) = ncrd - 3
   IF ( i==1 ) cid(1) = cid(1) + 1
   cid(2) = ncrd - 1
   cid(3) = cid(1)
   cid(4) = cid(2)
   cid(5) = ncrd
   CALL write(ecta,necta,6,0)
   i = i + 1
   IF ( i<=nsb ) GOTO 1100
!
!     INTEFERENCE CALCULATIONS AND ARRAYS
!
   IF ( nint/=0 ) THEN
      p1 = 1.0/nint
      DO j = 1 , nint
         z(iria) = width
         IF ( lrib/=0 ) z(iria) = .5*(z(irib+j)+z(irib+j+1))
         iria = iria + 1
         d1 = p1*(j-1)
         d2 = p1*j
         IF ( lchord/=0 ) d1 = z(ichord+j)
         IF ( lchord/=0 ) d2 = z(ichord+j+1)
         z(idelx) = x12*(d2-d1)
         z(ix) = ra1(1) + x12*(d1+d2)/2.0
         IF ( j==1 ) z(ixle) = ra1(1) + d1*x12
         IF ( j==nint ) z(ixte) = ra1(1) + d2*x12
         idelx = idelx + 1
         ix = ix + 1
      ENDDO
   ENDIF
   ixle = ixle + 1
   ixte = ixte + 1
   iz(pc+4) = nsb
   iz(pc+5) = 1
   iz(pc+8) = 2
   iz(pc+16) = bet
   IF ( bet/=1 ) THEN
      auset(2,2) = usa
      auset(6,2) = usa
      auset(3,2) = uk
      auset(5,2) = uk
   ENDIF
   IF ( ibc==nb ) CALL write(acpt,iz(Ncore),nwr,1)
 1600 IF ( Iopt==1 ) THEN
      Cao2(ipc) = -Cao2(ipc)
      GOTO 1800
   ELSE
      ipc = ipc + 2
      IF ( ipc>=ncam2*2 ) GOTO 1900
      GOTO 100
   ENDIF
 1700 IF ( Cao2(ipc)==Id ) GOTO 200
 1800 ipc = ipc + 2
   IF ( ipc<ncam2*2 ) GOTO 1700
 1900 RETURN
 2000 DO
      WRITE (not,99004) ufm , eid
99004 FORMAT (A23,' 2273, CAERO2',I9,' NOT INPUT IN Z, ZY, Y SEQUENCE.')
      CALL mesage(-61,0,nam)
   ENDDO
 2100 WRITE (not,99005) ufm , iz(na+j) , Cao2(ibt)
99005 FORMAT (A23,' 2274, ASSOCIATED BODY',I9,' WAS NOT FOUND WITH ','CAERO2 GROUP',I9,1H.)
   CALL mesage(-61,0,nam)
   GOTO 2000
 2200 WRITE (not,99006) ufm , eid
99006 FORMAT (A23,' 2275, CAERO2',I9,' HAS INCONSISTENT USE FOR THI OR',' THN, OR LTH2 IS REQUIRED.')
   CALL mesage(-61,0,nam)
   GOTO 2000
 2300 WRITE (not,99007) ufm , j , eid
99007 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR',' CARD ID',I9,/28X,'ASSOCIATED WITH CAERO2 ID',I9)
   CALL mesage(-61,0,nam)
   GOTO 2000
99008 FORMAT (10X,'CAERO2 ELEMENT NO.',I9,' REFERENCES AEFACT CARD NO.',I9,' WHICH DOES NOT EXIST.')
END SUBROUTINE apd2
