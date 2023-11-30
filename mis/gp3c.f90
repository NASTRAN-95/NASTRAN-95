
SUBROUTINE gp3c
   IMPLICIT NONE
   INTEGER Buf(50) , Buf1 , Buf2 , Cardid(60) , Clsrew , Elem(1) , Geom2 , Geom3 , Idno(30) , Igrav , Incr , Ipld3 , Ipload , Last ,&
         & Load(2) , Mask(60) , Nelem , Nopld2 , Nout , Ntypes , Pload2(2) , Pload3(2) , Rdrew , Scr2 , Wrtrew , Z(1)
   REAL Buf3 , Carddt(60) , Eqexin , Gptt , Rd , Rz(1) , Scr1 , Slt , Status(60) , Sysbuf , Temp(2) , Tempd(2) , Tempp1(2) ,        &
      & Tempp2(2) , Tempp3(2) , Temprb(2) , Two(32) , Wrt
   COMMON /gp3com/ Geom3 , Eqexin , Geom2 , Slt , Gptt , Scr1 , Scr2 , Buf1 , Buf2 , Buf , Cardid , Idno , Carddt , Mask , Status , &
                 & Ntypes , Ipload , Igrav , Pload2 , Load , Nopld2 , Temp , Tempd , Tempp1 , Tempp2 , Tempp3 , Temprb , Buf3 ,     &
                 & Pload3 , Ipld3
   COMMON /gpta1 / Nelem , Last , Incr , Elem
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf , Nout
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   INTEGER andf
   INTEGER d1 , d2 , faces(6,12) , file , i , id1 , id2 , idl , incl , incrd , itype , ix , j , j1 , j2 , k , n , n3304 , n3305 ,   &
         & nam(2) , nface , ngps , nogo , np , npld2 , nwdect , nwds , pl2 , pl3 , pl3err(14) , pld(3)
   REAL flag , p(12)
   EXTERNAL andf
!
!     GP3C EXECUTES ONLY IF PLOAD2 AND/OR PLOAD3 CARDS ARE PRESENT. ITS
!     FUNCTION IS TO --
!     (1) PROCESS PLOAD2 CARDS SO THAT THEIR FORMAT IS IDENTICAL TO
!         PLOAD CARDS.  IF A PLOAD RECORD EXISTS ON GEOM3, PLOAD2 DATA
!         IS APPENDED TO THE DATA, SORTED, AND ALL RESULTING PLOAD DATA
!         IS WRITTEN ON SCR2.
!     (2) PROCESS PLOAD3 CARDS SO THAT ALL PRESSURES APPLIED TO AN ISO-
!         PARAMETRIC SOLID ARE GATHERED IN ONE ENTRY AND SORTED BY THE
!         FACE NUMBER TO WHICH THE PRESSURE IS APPLIED.  THE SORTED
!         PRESSURES AND GRID POINT NUMBERS FOR EACH ELEMENT ARE WRITTEN
!         ON SCR2.
!
!
   !>>>>EQUIVALENCE (Rz(1),Z(1))
!
!  FACE                 IHEX1             IHEX2             IHEX3
!   NO                D1      D2        D1      D2        D1      D2
   DATA faces/1 , 3 , 1 , 5 , 1 , 7 , 2 , 4 , 3 , 7 , 4 , 10 , 1 , 6 , 1 , 15 , 1 , 24 , 2 , 5 , 3 , 13 , 4 , 21 , 2 , 7 , 3 , 17 , &
      & 4 , 27 , 3 , 6 , 5 , 15 , 7 , 24 , 3 , 8 , 5 , 19 , 7 , 30 , 4 , 7 , 7 , 17 , 10 , 27 , 1 , 8 , 1 , 19 , 1 , 30 , 4 , 5 ,   &
      & 7 , 13 , 10 , 21 , 5 , 7 , 13 , 17 , 21 , 27 , 6 , 8 , 15 , 19 , 24 , 30/
!
   DATA n3304 , n3305 , pl3err/4H3304 , 4H3305 , 4H0*** , 4H USE , 4HR FA , 4HTAL  , 4HMESS , 4HAGE  , 4H330* , 4H, PL , 4HOAD3 ,   &
       &4H CAR , 4HD FR , 4HOM L , 4HOAD  , 4HSET /
   DATA nam/4HGP3C , 4H    /
!
!     CHECK TRAILER BITS FOR PRESENCE OF PLOAD2 AND PLOAD3 CARDS.
!     IF NONE EXIST, RETURN.  OTHERWISE, BRANCH AND INITIALIZE TO
!     PROCESS ONE OF THESE CARD TYPES.
!
   nogo = 0
   pl2 = 0
   pl3 = 0
   j = (Pload2(2)-1)/16
   k = Pload2(2) - 16*j
   IF ( andf(Buf(j+2),Two(k+16))/=0 ) pl2 = 1
   j = (Pload3(2)-1)/16
   k = Pload3(2) - 16*j
   IF ( andf(Buf(j+2),Two(k+16))/=0 ) pl3 = 1 - 2*pl2
   file = Scr2
   IF ( pl2/=pl3 ) CALL open(*1700,Scr2,Z(Buf2),Wrtrew)
   IF ( pl2/=0 ) THEN
      Nopld2 = 1
      pld(1) = Pload2(1)
      pld(2) = Pload2(2)
      pld(3) = 24
      incrd = 3
      incl = 6
      idl = 2
      GOTO 200
   ENDIF
 100  IF ( pl3==0 ) GOTO 1600
   Nopld2 = Nopld2 + 2
   pld(1) = Pload3(1)
   pld(2) = Pload3(2)
   pld(3) = 255
   incrd = 5
   incl = 39
   idl = 1
!
!     READ PLOAD2 OR PLOAD3 CARDS INTO CORE IN AN EXPANDED FORMAT.
!     SET THE SET ID NEGATIVE TO INDICATE THE CARD IS NOT YET CONVERTED.
!
 200  i = 1
   file = Geom3
   CALL preloc(*1700,Z(Buf1),Geom3)
   CALL locate(*1900,Z(Buf1),pld,flag)
   IF ( pl2==1 ) THEN
      pld(1) = Cardid(Ipload)
      pld(2) = Cardid(Ipload+1)
   ENDIF
 300  CALL read(*1800,*700,Geom3,Z(i),incrd,0,flag)
   Z(i) = -Z(i)
   IF ( pl2==1 ) GOTO 600
   IF ( i>=incl ) THEN
      DO j = 2 , i , incl
         k = j
         IF ( Z(j)==Z(i+2) ) THEN
            IF ( Z(j-1)==Z(i) ) GOTO 500
         ENDIF
      ENDDO
   ENDIF
 400  p(1) = Rz(i+1)
   Z(i+1) = Z(i+2)
   Rz(i+2) = p(1)
   Z(i+14) = Z(i+3)
   Z(i+15) = Z(i+4)
   Z(i+3) = -1
   GOTO 600
 500  j = k + 2
   DO WHILE ( Z(j)/=-1 )
      j = j + 1
      IF ( j>k+12 ) GOTO 400
   ENDDO
   Rz(j) = Rz(i+1)
   IF ( j<k+12 ) Z(j+1) = -1
   j = k + 15 + 2*(j-k-2)
   Z(j) = Z(i+3)
   Z(j+1) = Z(i+4)
   GOTO 300
 600  Z(i+incl-1) = 0
   i = i + incl
   IF ( i<Buf2 ) GOTO 300
   CALL mesage(-8,0,nam)
 700  CALL close(Geom3,Clsrew)
   npld2 = i - incl
   nwds = i - 1
!
!     POSITION TO FIRST DATA RECORD ON GEOM2.
!
   file = Geom2
   CALL open(*1100,Geom2,Z(Buf1),Rdrew)
   CALL fwdrec(*1800,Geom2)
!
!     READ 3-WORD RECORD ID. LOOK FOR ID IN ELEM TABLE.
!     IF NOT THERE, SKIP RECORD.
!     IF PROCESSING PLOAD2, AND NOT A TWO-DIMENSIONAL ELEMENT, SKIP REC.
!     IF PROCESSING PLOAD3, AND NOT AN ISOPARAMETRIC ELEMENT, SKIP REC.
!     OTHERWISE,  INITIALIZE PARAMETERS.
!
 800  CALL read(*1100,*800,Geom2,Buf,3,0,flag)
   DO i = 1 , Last , Incr
      IF ( Buf(1)==Elem(i+3) ) GOTO 1000
   ENDDO
 900  CALL fwdrec(*1800,Geom2)
   GOTO 800
 1000 ngps = Elem(i+9)
   itype = Elem(i+2)
!
!   . IF ELEMENT TYPE IS 68 (QUADTS) THEN USE FIRST FOUR  GRID POINTS
!   . IF ELEMENT TYPE IS 69 (TRIATS) THEN USE FIRST THREE GRID POINTS
!
   IF ( itype==68 .OR. itype==69 ) ngps = ngps/2
   IF ( pl2==1 .AND. (ngps<3 .OR. ngps>4) ) GOTO 900
   IF ( pl3==1 .AND. (itype<65 .OR. itype>67) ) GOTO 900
   itype = 2*(itype-64) - 1
   nwdect = Elem(i+5)
   j1 = Elem(i+12)
   j2 = j1 + ngps - 1
   DO
!
!     READ EACH ELEMENT IN RECORD. LOOK FOR ELEMENT ID MATCH IN PLOAD2
!     OR PLOAD3 LIST.  IF FOUND, SET THE SET ID POSITIVE TO INDICATE
!     ENTRY IS CONVERTED.
!
      CALL read(*1800,*800,Geom2,Buf,nwdect,0,flag)
      DO i = 1 , npld2 , incl
         IF ( Z(i)<=0 ) THEN
            IF ( Z(i+idl)==Buf(1) ) THEN
               Z(i) = -Z(i)
               ix = i
               IF ( pl3==1 ) THEN
!
!     FIND THE DIAGONALS ON THE PLOAD3 CARD ON THE ELEMENT CARD TO
!     DETERMINE THE FACES TO WHICH THE PRESSURES ARE APPLIED.  SORT
!     THE PRESSURES BY FACE NUMBER AND APPEN+ THE GRID POINT NUMBERS
!     FROM THE ELEMENT CARD TO THE PLOAD3 ENTRY.
!
                  np = 0
                  DO j = 1 , 12
                     IF ( Z(i+j+1)==-1 ) EXIT
                     np = np + 1
                     p(j) = Rz(i+j+1)
                  ENDDO
                  DO j = 1 , 6
                     Rz(i+j) = 0.0
                  ENDDO
                  DO j = 1 , np
                     k = i + 14 + 2*(j-1)
                     id1 = Z(k)
                     id2 = Z(k+1)
                     DO k = j1 , j2
                        IF ( id1==Buf(k) ) GOTO 1002
                     ENDDO
                     GOTO 1006
 1002                id1 = k - j1 + 1
                     DO k = j1 , j2
                        IF ( id2==Buf(k) ) GOTO 1004
                     ENDDO
                     GOTO 1006
 1004                id2 = k - j1 + 1
                     d1 = min0(id1,id2)
                     d2 = max0(id1,id2)
                     DO k = 1 , 12
                        nface = (k+1)/2
                        IF ( d1==faces(itype,k) ) THEN
                           IF ( d2==faces(itype+1,k) ) GOTO 1008
                        ENDIF
                     ENDDO
 1006                nogo = 1
                     pl3err(7) = n3305
                     WRITE (Nout,99001) pl3err , Z(i) , Buf(1)
99001                FORMAT (14A4,I9,' HAS INVALID GRID POINT NUMBERS FOR ELEMENT',I9)
                     CYCLE
 1008                Rz(i+nface) = Rz(i+nface) + p(j)
                  ENDDO
                  ix = ix + 7
                  DO j = j1 , j2
                     Z(ix) = Buf(j)
                     ix = ix + 1
                  ENDDO
                  IF ( ix+1-i<=39 ) THEN
                     k = i + 38
                     DO j = ix , k
                        Z(j) = 0
                     ENDDO
                  ENDIF
               ELSE
!
!     PLACE GRID POINT NUMBERS FROM ELEMENT CARD IN PLOAD2 ENTRY TO
!     MAKE IT LOOK LIKE PLOAD CARD.
!
                  DO j = j1 , j2
                     Z(ix+2) = Buf(j)
                     ix = ix + 1
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDDO
!
!     HERE WHEN END-OF-FILE ON GEOM2 IS ENCOUNTERED.
!     MAKE SURE ALL PLOAD2 OR PLOAD3 ENTRIES HAVE BEEN CONVERTED.
!
 1100 CALL close(Geom2,Clsrew)
   DO i = 1 , npld2 , incl
      IF ( Z(i)<=0 ) THEN
         nogo = 1
         Buf(1) = -Z(i)
         Buf(2) = Z(i+idl)
         IF ( pl2==1 ) CALL mesage(30,105,Buf)
         pl3err(7) = n3304
         IF ( pl3==1 ) WRITE (Nout,99002) pl3err , Buf(1) , Buf(2)
!
!     PLOAD3 CARD ERRORS
!
99002    FORMAT (14A4,I9,' REFERENCES MISSING OR NON-ISOPARAMETRIC ELEMENT',I9)
      ENDIF
   ENDDO
   IF ( nogo/=0 ) CALL mesage(-61,0,0)
   IF ( pl3==1 ) GOTO 1400
!
!     LOCATE PLOAD RECORD ON GEOM3. IF PRESENT, READ PLOAD DATA INTO
!     CORE (AFTER PLOAD2 DATA) AND SORT COMBINED DATA ON SET ID.
!
   CALL preloc(*1700,Z(Buf1),Geom3)
   CALL locate(*1300,Z(Buf1),Cardid(Ipload),flag)
   i = npld2 + 6
   DO
      CALL read(*1800,*1200,Geom3,Z(i),6,0,flag)
      i = i + 6
      IF ( i>=Buf2 ) THEN
         CALL mesage(-8,0,nam)
         EXIT
      ENDIF
   ENDDO
 1200 npld2 = i - 6
   nwds = i - 1
   CALL sort(0,0,6,1,Z,nwds)
 1300 CALL close(Geom3,Clsrew)
!
!     WRITE DATA ON SCR2, SET FLAG TO INDICATE AND RETURN.
!
 1400 CALL write(Scr2,pld,3,0)
   CALL write(Scr2,Z,nwds,1)
   IF ( pl2/=1 ) GOTO 1600
 1500 pl2 = -pl2
   pl3 = -pl3
   GOTO 100
 1600 CALL close(Scr2,Clsrew)
   RETURN
 1700 DO
      n = -1
!
!     ERROR MESSAGES.
!
      CALL mesage(n,file,nam)
   ENDDO
 1800 n = -2
   CALL mesage(n,file,nam)
   GOTO 1700
!
!     ABNORMAL RETURN.
!
 1900 IF ( pl3<0 ) GOTO 1500
   CALL close(Geom3,Clsrew)
   RETURN
END SUBROUTINE gp3c