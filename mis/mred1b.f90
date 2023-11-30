
SUBROUTINE mred1b(Mode)
!
!     THIS SUBROUTINE PROCESSES THE BDYS AND BDYS1 DATA FOR THE FIXED
!     IDENTIFICATION SET (FIXSET) AND THE BOUNDARY IDENTIFICATION SET
!     (BNDSET) FOR THE MRED1 MODULE.
!
!     INPUT DATA
!     GINO   - GEOM4  - BDYS DATA
!                     - BDYS1 DATA
!     OTHERS - MODE   - SUBROUTINE PROCESSING FLAG
!                     = 1, PROCESS FIXED ID SET
!                     = 2, PROCESS BOUNDARY ID SET
!
!     OUTPUT DATA
!     GINO   - USETX  - S,R,B DEGREES OF FREEDOM
!
!     PARAMETERS
!     INPUT  - NOUS   - FIXED POINTS FLAG
!                       .GE.  0, FIXED POINTS DEFINED
!                       .EQ. -1, NO FIXED POINTS DEFINED
!              GBUF1  - GINO BUFFER
!              KORLEN - CORE LENGTH
!              IO     - OUTPUT OPTION FLAG
!              NAMEBS - BEGINNING ADDRESS OF BASIC SUBSTRUCTURES NAMES
!              EQSIND - BEGINNING ADDRESS OF EQSS GROUP ADDRESSES
!              NSLBGN - BEGINNING ADDRESS OF SIL DATA
!              KBDYC  - BEGINNING ADDRESS OF BDYC DATA
!              USETX  - USETX OUTPUT FILE NUMBER
!              NBDYCC - NUMBER OF BDYC WORDS
!     OUTPUT - DRY    - MODULE OPERATION FLAG
!     OTHERS - LOCUST - BEGINNING ADDRESS OF USET ARRAY
!              IERR   - NO BDYS/BDYS1 DATA ERROR FLAG
!                       .LT. 2, NO ERRORS
!                       .EQ. 2, ERRORS
!              GRPBGN - ABSOLUTE BEGINNING ADDRESS OF EQSS GROUP DATA
!              GRPEND - ABSOLUTE ENDING ADDRESS OF EQSS GROUP DATA
!              GRPIP  - ABSOLUTE ADDRESS OF EQSS DATA GROUP
!              LOCBGN - BEGINNING ADDRESS OF EQSS DATA FOR SUBSTRUCTURE
!              NFOUND - NUMBER OF EQSS DATA ITEMS FOUND FOR SET ID
!              KPNTBD - ARRAY OF BDYC DOF COMPONENTS
!              KPNTSL - ARRAY OF EQSS DOF COMPONENTS
!              INDSIL - ABSOLUTE INDEX INTO SIL DATA
!              NSILUS - ABSOLUTE INDEX INTO USET ARRAY
!
   IMPLICIT NONE
   INTEGER Bndset , Dry , Eqsind , Fixset , Fuset , Gbuf1 , Idum10(5) , Idum11(10) , Idum12 , Idum13 , Idum14(4) , Idum2(4) ,       &
         & Idum3(4) , Idum4(5) , Idum5(2) , Idum6 , Idum7 , Idum8(6) , Idum9(2) , Ieig , Incru , Io , Iprntr , Irowu , Itwo(32) ,   &
         & Kbdyc , Korbgn , Korlen , Lcore , Line , Locust , Luset , Namebs , Nbdycc , Nlpp , Nous , Nrowu , Nslbgn , Nsub(3) ,     &
         & Oldnam(2) , Typeu , Ua , Ub , Uf , Ui , Ul , Un , Us , Z(1)
   LOGICAL Bounds , Ponly
   REAL Rz(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Idum10 , Ul , Ua , Uf , Us , Un , Idum11 , Ub , Ui
   COMMON /blank / Oldnam , Dry , Idum13 , Nous , Idum2 , Gbuf1 , Idum14 , Korlen , Idum4 , Io , Idum5 , Bndset , Fixset , Ieig ,   &
                 & Korbgn , Idum12 , Namebs , Eqsind , Nslbgn , Idum6 , Kbdyc , Nbdycc , Luset , Locust , Idum3 , Bounds , Ponly
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum7 , Iprntr , Idum8 , Nlpp , Idum9 , Line
   COMMON /two   / Itwo
   COMMON /unpakx/ Typeu , Irowu , Nrowu , Incru
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER Mode
   INTEGER andf , complf , orf , rshift
   INTEGER array(3) , bdy(2) , bdyi(2,2) , bdyj , bdyk , bdyl , bdym , eqst , eqstrl(7) , geom4 , grpbgn , grpend , grpip , i ,     &
         & ibdy , ibits , icode , idum(3) , ierr , ifile , iflag , ifound , imsg , indsil , ioshft(2) , ip , ishift , isub0 ,       &
         & isub1 , item , itest , iwds , j , jwds , k , kpntbd(9) , kpntsl(32) , kwds , l , loc , locbgn , m , modnam(2) , newust , &
         & nfound , nsilus , nxtbdy , ubors , uprt , usetrl(7) , usetx
   EXTERNAL andf , complf , orf , rshift
   EQUIVALENCE (Rz(1),Z(1))
   DATA geom4 , bdyi , usetx/102 , 1210 , 12 , 1310 , 13 , 201/
   DATA modnam/4HMRED , 4H1B  /
   DATA ioshft/11 , 2/
   DATA item/4HUPRT/
   DATA uprt , eqst/301 , 203/
!
!     TEST FOR FIXED SET INPUT
!
   IF ( Nous==-1 .AND. Mode==1 ) GOTO 1600
!
!     CHECK FOR LOADS PROCESSING ONLY
!
   IF ( Ponly ) GOTO 900
!
!     PROCESS BDY(S/S1) BULK DATA FOR SPECIFIED BDYC
!
   ishift = ioshft(Mode)
   IF ( Nbdycc==0 ) GOTO 2400
   Korbgn = Kbdyc + 4*Nbdycc
   IF ( Korbgn>=Korlen ) GOTO 2000
   IF ( andf(rshift(Io,ishift),1)/=0 ) THEN
      CALL page1
      IF ( Mode==1 ) WRITE (Iprntr,99010)
      IF ( Mode==2 ) WRITE (Iprntr,99011)
      Line = Line + 7
   ENDIF
   ibits = Itwo(Ul) + Itwo(Ua) + Itwo(Uf)
   IF ( Mode==2 ) ibits = Itwo(Ui)
   ibits = complf(ibits)
   ierr = 0
   ibdy = 0
   ifile = geom4
!
!     SET BULK DATA PROCESSING FLAG AND READ SET ID
!     IBDY .EQ. 1 - BDYS
!     IBDY .EQ. 2 - BDYS1
!
   nxtbdy = 1
   ifound = 0
   CALL preloc(*1700,Z(Gbuf1),geom4)
 100  ibdy = ibdy + 1
   IF ( ibdy==3 ) THEN
!
!     END OF ID SET PROCESSING
!
      CALL close(geom4,1)
      IF ( ierr==2 ) GOTO 2300
      IF ( ifound==0 ) GOTO 2300
      IF ( Mode==1 ) GOTO 1600
!
!     WRITE USETX DATA
!
      CALL gopen(usetx,Z(Gbuf1),1)
      CALL write(usetx,Z(Locust),Luset,1)
      CALL close(usetx,1)
      usetrl(1) = usetx
      usetrl(2) = 1
      usetrl(3) = Luset
      usetrl(4) = 7
      usetrl(5) = 1
      CALL wrttrl(usetrl)
!
!     VERIFY OLD BOUNDARY UNCHANGED
!
      IF ( .NOT.Bounds ) GOTO 1600
      IF ( Locust+2*Luset<Korlen ) GOTO 900
      GOTO 2000
   ELSE
      DO i = 1 , 2
         bdy(i) = bdyi(i,ibdy)
      ENDDO
      CALL locate(*800,Z(Gbuf1),bdy,iflag)
      GOTO 300
   ENDIF
 200  CALL bckrec(geom4)
   nxtbdy = nxtbdy + 1
   IF ( nxtbdy>Nbdycc ) GOTO 100
   CALL read(*1800,*1900,geom4,idum,3,0,iflag)
 300  CALL read(*1800,*100,geom4,array,ibdy,0,iflag)
!
!     CHECK REQUEST ID
!
   bdyj = 2
   bdyk = 2
   bdyl = 3
   bdym = 2
   IF ( ibdy/=1 ) THEN
      bdyj = 3
      bdyk = 1
      bdyl = 2
      bdym = 3
   ENDIF
   iwds = 2 + 4*(nxtbdy-1)
   DO i = nxtbdy , Nbdycc
      IF ( Z(Kbdyc+iwds)==array(1) ) GOTO 400
      iwds = iwds + 4
   ENDDO
   DO
!
!     FINISH BDY(S/S1) SET ID READING
!
      CALL read(*1800,*1900,geom4,array(bdyj),bdyk,0,iflag)
      IF ( ibdy<2 ) THEN
         IF ( array(2)==-1 .OR. array(3)==-1 ) GOTO 300
      ELSEIF ( array(3)==-1 ) THEN
         GOTO 300
      ENDIF
   ENDDO
!
!     CONTINUE BDY(S/S1) SET ID PROCESSING
!
 400  CALL read(*1800,*1900,geom4,array(bdyj),bdyk,0,iflag)
   IF ( ibdy<2 ) THEN
      IF ( array(2)/=-1 .OR. array(3)/=-1 ) GOTO 500
   ELSEIF ( array(3)/=-1 ) THEN
      GOTO 500
   ENDIF
!
!     CHECK FOR NEXT BDY(S/S1) CARD HAVING SAME SET ID AS CURRENT ID
!
   CALL read(*1800,*200,geom4,array,ibdy,0,iflag)
   IF ( Z(Kbdyc+iwds)/=array(1) ) GOTO 200
   GOTO 400
!
!     LOCATE EQSS DATA FOR SUBSTRUCTURE
!
 500  ifound = 1
   ip = 2*(Z(Kbdyc+iwds+1)-1)
   grpbgn = Z(Eqsind+ip)
   grpend = grpbgn + Z(Eqsind+ip+1)
   k = Z(Eqsind+ip+1)/3
   CALL bisloc(*600,array(bdym),Z(grpbgn),3,k,locbgn)
   grpip = grpbgn + locbgn - 1
   loc = grpip - 3
   DO WHILE ( loc>=grpbgn )
      IF ( Z(loc)<Z(grpip) ) EXIT
      loc = loc - 3
   ENDDO
   locbgn = loc + 3
   nfound = 1
   loc = locbgn + 3
   DO WHILE ( loc<grpend )
      IF ( Z(locbgn)<Z(loc) ) EXIT
      loc = loc + 3
      nfound = nfound + 1
   ENDDO
   GOTO 700
!
!     CANNOT LOCATE EXTERNAL ID
!
 600  CALL page1
   IF ( Mode==1 ) WRITE (Iprntr,99001) Ufm , array(3) , array(2) , array(1) , Z(Namebs+ip) , Z(Namebs+ip+1)
99001 FORMAT (A23,' 6624, GRID POINT',I9,' COMPONENT',I9,' SPECIFIED ','IN FIXED SET',I9,/5X,'FOR SUBSTRUCTURE ',2A4,               &
             &' DOES NOT EXIST.',//////)
   IF ( Mode==2 ) WRITE (Iprntr,99002) Ufm , array(3) , array(2) , array(1) , Z(Namebs+ip) , Z(Namebs+ip+1)
99002 FORMAT (A23,' 6611, GRID POINT',I9,' COMPONENT',I9,' SPECIFIED ','IN BOUNDARY SET',I9,/5X,'FOR SUBSTRUCTURE ',2A4,            &
             &' DOES NOT EXIST.',//////)
   Dry = -2
   GOTO 400
!
!     LOCATE CORRECT IP FOR THIS EXTERNAL ID
!
 700  CALL splt10(array(bdyl),kpntbd,jwds)
   m = 0
   DO i = 1 , nfound
      j = (3*(i-1)) + 2
      icode = Z(locbgn+j)
      CALL decode(icode,kpntsl,kwds)
      DO k = 1 , kwds
         DO l = 1 , jwds
            IF ( kpntsl(k)==kpntbd(l)-1 ) GOTO 720
         ENDDO
         CYCLE
!
!     CONVERT GRID ID AND COMPONENT TO SIL VALUE
!
 720     IF ( andf(rshift(Io,ishift),1)/=0 ) THEN
            IF ( Line>Nlpp ) THEN
               CALL page1
               IF ( Mode==1 ) WRITE (Iprntr,99010)
               IF ( Mode==2 ) WRITE (Iprntr,99011)
               Line = Line + 7
            ENDIF
            IF ( m==0 ) WRITE (Iprntr,99003) array(1) , array(bdym) , array(bdyl)
99003       FORMAT (52X,2(I8,3X),I6)
            m = 1
            Line = Line + 1
         ENDIF
         indsil = Nslbgn + ((2*Z(locbgn+j-1))-2)
         nsilus = Locust + ((Z(indsil)-1)+(k-1))
         kpntbd(l) = 0
!
!     FILL USET ARRAY
!     IF FIXSET - TURN OFF UL, UA, UF BITS AND TURN ON US BIT
!     IF BNDSET - TURN OFF UI BIT AND TURN ON UB BIT
!
         ubors = Us
         IF ( Mode==2 ) ubors = Ub
         Z(nsilus) = andf(Z(nsilus),ibits)
         Z(nsilus) = orf(Z(nsilus),Itwo(ubors))
      ENDDO
   ENDDO
!
!     CHECK THAT ALL IP FOUND
!
   DO i = 1 , jwds
      IF ( kpntbd(i)/=0 ) THEN
         IF ( Mode==1 ) WRITE (Iprntr,99004) Uwm , array(bdym) , Z(Namebs+ip) , Z(Namebs+ip+1)
99004    FORMAT (A25,' 6625, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/32X,'INCLUDED IN A FIXED',        &
                &' SET DO NOT EXIST.  REQUEST WILL BE IGNORED.')
         IF ( Mode==2 ) WRITE (Iprntr,99005) Uwm , array(bdym) , Z(Namebs+ip) , Z(Namebs+ip+1)
99005    FORMAT (A25,' 6610, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/32X,'INCLUDED IN A NON-',         &
                &'EXISTING BOUNDARY SET.  REQUEST WILL BE IGNORED.')
         EXIT
      ENDIF
   ENDDO
   GOTO 400
!
!     SET NO DATA AVAILABLE FLAG
!
 800  ierr = ierr + 1
   GOTO 100
 900  CALL softrl(Oldnam,item,usetrl)
   IF ( usetrl(1)==1 ) THEN
      Nrowu = usetrl(3)
      IF ( Ponly ) Luset = Nrowu
      IF ( Nrowu/=Luset ) GOTO 1500
!
!     GET OLD UPRT VECTOR
!
      Typeu = usetrl(5)
      CALL mtrxi(uprt,Oldnam,item,0,itest)
      newust = Locust + Luset
      IF ( Ponly ) newust = Locust
      IF ( Ponly .AND. newust+Nrowu>=Korlen ) GOTO 2000
      Irowu = 1
      Incru = 1
      CALL gopen(uprt,Z(Gbuf1),0)
      CALL unpack(*1000,uprt,Rz(newust))
      GOTO 1100
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      GOTO 2500
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      GOTO 2500
   ELSEIF ( itest==5 .OR. itest==6 ) THEN
      imsg = -3
      GOTO 2500
   ELSE
      WRITE (Iprntr,99006) Ufm , modnam , item , Oldnam
99006 FORMAT (A23,' 6215, MODULE ',2A4,8H - ITEM ,A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
      Dry = -2
      RETURN
   ENDIF
 1000 DO i = 1 , Luset
      Rz(newust+i-1) = 0.0
   ENDDO
 1100 CALL close(uprt,1)
   IF ( Ponly ) GOTO 1400
!
!     GET NEW UPRT VECTOR
!
   Lcore = Korlen - (newust+Luset)
   Fuset = usetx
   CALL calcv(uprt,Un,Ui,Ub,Z(newust+Luset))
   Typeu = 1
   Nrowu = Luset
   CALL gopen(uprt,Z(Gbuf1),0)
   CALL unpack(*1200,uprt,Rz(newust+Luset))
   GOTO 1300
 1200 DO i = 1 , Luset
      Rz(newust+Luset+i-1) = 0.0
   ENDDO
 1300 CALL close(uprt,1)
!
!     CHECK OLD, NEW UPRT VECTORS AND COUNT NUMBER OF ROWS IN 0, 1
!     SUBSETS AND SAVE IN EQST TRAILER FOR USE IN MRED2A
!
 1400 isub0 = 0
   isub1 = 0
   DO i = 1 , Luset
      IF ( Rz(newust+i-1)==0.0 ) isub0 = isub0 + 1
      IF ( Rz(newust+i-1)==1.0 ) isub1 = isub1 + 1
      IF ( .NOT.(Ponly) ) THEN
         IF ( Rz(newust+i-1)/=Rz(newust+Luset+i-1) ) GOTO 1500
      ENDIF
   ENDDO
   eqstrl(1) = eqst
   eqstrl(6) = isub0
   eqstrl(7) = isub1
   CALL wrttrl(eqstrl)
   GOTO 1600
!
!     BOUNDARY POINTS ARE NOT THE SAME
!
 1500 WRITE (Iprntr,99007) Ufm , Oldnam
99007 FORMAT (A23,' 6637, OLDBOUND HAS BEEN SPECIFIED BUT THE BOUNDARY',' POINTS FOR SUBSTRUCTURE ',2A4,' HAVE BEEN CHANGED.')
   Dry = -2
 1600 RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 1700 imsg = -1
   GOTO 2200
 1800 imsg = -2
   GOTO 2100
 1900 imsg = -3
   GOTO 2100
 2000 imsg = -8
   ifile = 0
 2100 CALL close(geom4,1)
 2200 CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 2300 IF ( Mode==1 ) WRITE (Iprntr,99008) Ufm , Fixset
99008 FORMAT (A23,' 6626, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE FIXED SET',I9,1H.)
   IF ( Mode==2 ) WRITE (Iprntr,99009) Ufm , Bndset
99009 FORMAT (A23,' 6607, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE BOUNDARY SET',I9,1H.)
 2400 Dry = -2
   CALL sofcls
   CALL close(geom4,1)
   RETURN
 2500 CALL smsg(imsg,item,Oldnam)
   RETURN
!
99010 FORMAT (//45X,40HTABLE OF GRID POINTS COMPOSING FIXED SET,//53X,5HFIXED,/53X,25HSET ID   GRID POINT   DOF,/53X,               &
             &26HNUMBER   ID NUMBER    CODE,/)
99011 FORMAT (1H0,44X,43HTABLE OF GRID POINTS COMPOSING BOUNDARY SET,//52X,8HBOUNDARY,/53X,25HSET ID   GRID POINT   DOF,/53X,       &
             &26HNUMBER   ID NUMBER    CODE,/)
!
END SUBROUTINE mred1b
