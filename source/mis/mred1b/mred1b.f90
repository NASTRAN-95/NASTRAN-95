!*==mred1b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_BITPOS
   USE C_BLANK
   USE C_PATX
   USE C_SYSTEM
   USE C_TWO
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: array , idum
   INTEGER , DIMENSION(2) :: bdy
   INTEGER , DIMENSION(2,2) , SAVE :: bdyi
   INTEGER :: bdyj , bdyk , bdyl , bdym , grpbgn , grpend , grpip , i , ibdy , ibits , icode , ierr , ifile , iflag , ifound ,      &
            & imsg , indsil , ip , ishift , isub0 , isub1 , itest , iwds , j , jwds , k , kwds , l , loc , locbgn , m , newust ,    &
            & nfound , nsilus , nxtbdy , ubors
   INTEGER , SAVE :: eqst , geom4 , item , uprt , usetx
   INTEGER , DIMENSION(7) :: eqstrl , usetrl
   INTEGER , DIMENSION(2) , SAVE :: ioshft , modnam
   INTEGER , DIMENSION(9) :: kpntbd
   INTEGER , DIMENSION(32) :: kpntsl
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , bckrec , bisloc , calcv , close , complf , decode , gopen , locate , mesage , mtrxi , orf , page1 , preloc ,     &
          & read , rshift , smsg , sofcls , softrl , splt10 , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Rz(1),Z(1))
   DATA geom4 , bdyi , usetx/102 , 1210 , 12 , 1310 , 13 , 201/
   DATA modnam/4HMRED , 4H1B  /
   DATA ioshft/11 , 2/
   DATA item/4HUPRT/
   DATA uprt , eqst/301 , 203/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST FOR FIXED SET INPUT
!
         IF ( Nous==-1 .AND. Mode==1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK FOR LOADS PROCESSING ONLY
!
         IF ( Ponly ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS BDY(S/S1) BULK DATA FOR SPECIFIED BDYC
!
         ishift = ioshft(Mode)
         IF ( Nbdycc==0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Korbgn = Kbdyc + 4*Nbdycc
         IF ( Korbgn>=Korlen ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         CALL preloc(*140,Z(Gbuf1),geom4)
 20      ibdy = ibdy + 1
         IF ( ibdy==3 ) THEN
!
!     END OF ID SET PROCESSING
!
            CALL close(geom4,1)
            IF ( ierr==2 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ifound==0 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mode==1 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            IF ( .NOT.Bounds ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Locust+2*Luset>=Korlen ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 1 , 2
               bdy(i) = bdyi(i,ibdy)
            ENDDO
            CALL locate(*80,Z(Gbuf1),bdy,iflag)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      CALL bckrec(geom4)
         nxtbdy = nxtbdy + 1
         IF ( nxtbdy>Nbdycc ) GOTO 20
         CALL read(*160,*180,geom4,idum,3,0,iflag)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*160,*20,geom4,array,ibdy,0,iflag)
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
               IF ( Z(Kbdyc+iwds)==array(1) ) EXIT SPAG_Loop_1_1
               iwds = iwds + 4
            ENDDO
            DO
!
!     FINISH BDY(S/S1) SET ID READING
!
               CALL read(*160,*180,geom4,array(bdyj),bdyk,0,iflag)
               IF ( ibdy<2 ) THEN
                  IF ( array(2)==-1 .OR. array(3)==-1 ) CYCLE SPAG_Loop_1_1
               ELSEIF ( array(3)==-1 ) THEN
                  CYCLE SPAG_Loop_1_1
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
!
!     CONTINUE BDY(S/S1) SET ID PROCESSING
!
         CALL read(*160,*180,geom4,array(bdyj),bdyk,0,iflag)
         IF ( ibdy<2 ) THEN
            IF ( array(2)/=-1 .OR. array(3)/=-1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( array(3)/=-1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK FOR NEXT BDY(S/S1) CARD HAVING SAME SET ID AS CURRENT ID
!
         CALL read(*160,*40,geom4,array,ibdy,0,iflag)
         IF ( Z(Kbdyc+iwds)==array(1) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 40
      CASE (4)
!
!     LOCATE EQSS DATA FOR SUBSTRUCTURE
!
         ifound = 1
         ip = 2*(Z(Kbdyc+iwds+1)-1)
         grpbgn = Z(Eqsind+ip)
         grpend = grpbgn + Z(Eqsind+ip+1)
         k = Z(Eqsind+ip+1)/3
         CALL bisloc(*60,array(bdym),Z(grpbgn),3,k,locbgn)
         grpip = grpbgn + locbgn - 1
         loc = grpip - 3
         SPAG_Loop_1_2: DO WHILE ( loc>=grpbgn )
            IF ( Z(loc)<Z(grpip) ) EXIT SPAG_Loop_1_2
            loc = loc - 3
         ENDDO SPAG_Loop_1_2
         locbgn = loc + 3
         nfound = 1
         loc = locbgn + 3
         SPAG_Loop_1_3: DO WHILE ( loc<grpend )
            IF ( Z(locbgn)<Z(loc) ) EXIT SPAG_Loop_1_3
            loc = loc + 3
            nfound = nfound + 1
         ENDDO SPAG_Loop_1_3
!
!     LOCATE CORRECT IP FOR THIS EXTERNAL ID
!
         CALL splt10(array(bdyl),kpntbd,jwds)
         m = 0
         DO i = 1 , nfound
            j = (3*(i-1)) + 2
            icode = Z(locbgn+j)
            CALL decode(icode,kpntsl,kwds)
            DO k = 1 , kwds
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     DO l = 1 , jwds
                        IF ( kpntsl(k)==kpntbd(l)-1 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     CYCLE
                  CASE (2)
!
!     CONVERT GRID ID AND COMPONENT TO SIL VALUE
!
                     IF ( andf(rshift(Io,ishift),1)/=0 ) THEN
                        IF ( Line>Nlpp ) THEN
                           CALL page1
                           IF ( Mode==1 ) WRITE (Iprntr,99010)
                           IF ( Mode==2 ) WRITE (Iprntr,99011)
                           Line = Line + 7
                        ENDIF
                        IF ( m==0 ) WRITE (Iprntr,99001) array(1) , array(bdym) , array(bdyl)
99001                   FORMAT (52X,2(I8,3X),I6)
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
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
         ENDDO
!
!     CHECK THAT ALL IP FOUND
!
         SPAG_Loop_1_4: DO i = 1 , jwds
            IF ( kpntbd(i)/=0 ) THEN
               IF ( Mode==1 ) WRITE (Iprntr,99002) Uwm , array(bdym) , Z(Namebs+ip) , Z(Namebs+ip+1)
99002          FORMAT (A25,' 6625, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/32X,'INCLUDED IN A FIXED',  &
                      &' SET DO NOT EXIST.  REQUEST WILL BE IGNORED.')
               IF ( Mode==2 ) WRITE (Iprntr,99003) Uwm , array(bdym) , Z(Namebs+ip) , Z(Namebs+ip+1)
99003          FORMAT (A25,' 6610, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/32X,'INCLUDED IN A NON-',   &
                      &'EXISTING BOUNDARY SET.  REQUEST WILL BE IGNORED.')
               EXIT SPAG_Loop_1_4
            ENDIF
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CANNOT LOCATE EXTERNAL ID
!
 60      CALL page1
         IF ( Mode==1 ) WRITE (Iprntr,99004) Ufm , array(3) , array(2) , array(1) , Z(Namebs+ip) , Z(Namebs+ip+1)
99004    FORMAT (A23,' 6624, GRID POINT',I9,' COMPONENT',I9,' SPECIFIED ','IN FIXED SET',I9,/5X,'FOR SUBSTRUCTURE ',2A4,            &
                &' DOES NOT EXIST.',//////)
         IF ( Mode==2 ) WRITE (Iprntr,99005) Ufm , array(3) , array(2) , array(1) , Z(Namebs+ip) , Z(Namebs+ip+1)
99005    FORMAT (A23,' 6611, GRID POINT',I9,' COMPONENT',I9,' SPECIFIED ','IN BOUNDARY SET',I9,/5X,'FOR SUBSTRUCTURE ',2A4,         &
                &' DOES NOT EXIST.',//////)
         Dry = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     SET NO DATA AVAILABLE FLAG
!
 80      ierr = ierr + 1
         GOTO 20
      CASE (5)
         CALL softrl(Oldnam,item,usetrl)
         IF ( usetrl(1)==1 ) THEN
            Nrowu = usetrl(3)
            IF ( Ponly ) Luset = Nrowu
            IF ( Nrowu/=Luset ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     GET OLD UPRT VECTOR
!
            Typeu = usetrl(5)
            CALL mtrxi(uprt,Oldnam,item,0,itest)
            newust = Locust + Luset
            IF ( Ponly ) newust = Locust
            IF ( Ponly .AND. newust+Nrowu>=Korlen ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Irowu = 1
            Incru = 1
            CALL gopen(uprt,Z(Gbuf1),0)
            CALL unpack(*100,uprt,rz(newust))
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itest==3 ) THEN
            imsg = -1
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itest==5 .OR. itest==6 ) THEN
            imsg = -3
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (Iprntr,99006) Ufm , modnam , item , Oldnam
99006       FORMAT (A23,' 6215, MODULE ',2A4,8H - ITEM ,A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
            Dry = -2
            RETURN
         ENDIF
 100     DO i = 1 , Luset
            rz(newust+i-1) = 0.0
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
         CALL close(uprt,1)
         IF ( Ponly ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GET NEW UPRT VECTOR
!
         Lcore = Korlen - (newust+Luset)
         Fuset = usetx
         CALL calcv(uprt,Un,Ui,Ub,Z(newust+Luset))
         Typeu = 1
         Nrowu = Luset
         CALL gopen(uprt,Z(Gbuf1),0)
         CALL unpack(*120,uprt,rz(newust+Luset))
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 120     DO i = 1 , Luset
            rz(newust+Luset+i-1) = 0.0
         ENDDO
         spag_nextblock_1 = 7
      CASE (7)
         CALL close(uprt,1)
         spag_nextblock_1 = 8
      CASE (8)
!
!     CHECK OLD, NEW UPRT VECTORS AND COUNT NUMBER OF ROWS IN 0, 1
!     SUBSETS AND SAVE IN EQST TRAILER FOR USE IN MRED2A
!
         isub0 = 0
         isub1 = 0
         DO i = 1 , Luset
            IF ( rz(newust+i-1)==0.0 ) isub0 = isub0 + 1
            IF ( rz(newust+i-1)==1.0 ) isub1 = isub1 + 1
            IF ( .NOT.(Ponly) ) THEN
               IF ( rz(newust+i-1)/=rz(newust+Luset+i-1) ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         eqstrl(1) = eqst
         eqstrl(6) = isub0
         eqstrl(7) = isub1
         CALL wrttrl(eqstrl)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     BOUNDARY POINTS ARE NOT THE SAME
!
         WRITE (Iprntr,99007) Ufm , Oldnam
99007    FORMAT (A23,' 6637, OLDBOUND HAS BEEN SPECIFIED BUT THE BOUNDARY',' POINTS FOR SUBSTRUCTURE ',2A4,' HAVE BEEN CHANGED.')
         Dry = -2
         spag_nextblock_1 = 10
      CASE (10)
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 140     imsg = -1
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 160     imsg = -2
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 180     imsg = -3
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 12
      CASE (12)
         CALL close(geom4,1)
         spag_nextblock_1 = 13
      CASE (13)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (14)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( Mode==1 ) WRITE (Iprntr,99008) Ufm , Fixset
99008    FORMAT (A23,' 6626, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE FIXED SET',I9,1H.)
         IF ( Mode==2 ) WRITE (Iprntr,99009) Ufm , Bndset
99009    FORMAT (A23,' 6607, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE BOUNDARY SET',I9,1H.)
         spag_nextblock_1 = 15
      CASE (15)
         Dry = -2
         CALL sofcls
         CALL close(geom4,1)
         RETURN
      CASE (16)
         CALL smsg(imsg,item,Oldnam)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99010 FORMAT (//45X,40HTABLE OF GRID POINTS COMPOSING FIXED SET,//53X,5HFIXED,/53X,25HSET ID   GRID POINT   DOF,/53X,               &
             &26HNUMBER   ID NUMBER    CODE,/)
99011 FORMAT (1H0,44X,43HTABLE OF GRID POINTS COMPOSING BOUNDARY SET,//52X,8HBOUNDARY,/53X,25HSET ID   GRID POINT   DOF,/53X,       &
             &26HNUMBER   ID NUMBER    CODE,/)
!
END SUBROUTINE mred1b
