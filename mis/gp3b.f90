
SUBROUTINE gp3b
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(50) , Buf1 , Buf2 , Buf3 , Clsrew , Eqexin , Ett , Geom3 , Geomp , Gptt , Idno(30) , Igrav , Ipld3 , Ipload , Load(2)&
         & , Mask(60) , Nograv , Noload , Nopld2 , Notemp , Nout , Ntypes , Rd , Rdrew , Scr1 , Slt , Temp(2) , Tempd(2) , Tempg(2) &
         & , Tempp1(2) , Tempp2(2) , Tempp3(2) , Tempp4(2) , Temprb(2) , Wrt , Wrtrew , Z(1)
   REAL Carddt(60) , Cardid(60) , Geom2 , Pload2(2) , Pload3(2) , Scr2 , Status(60) , Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Nograv , Noload , Notemp
   COMMON /gp3com/ Geom3 , Eqexin , Geom2 , Slt , Ett , Scr1 , Scr2 , Buf1 , Buf2 , Buf , Cardid , Idno , Carddt , Mask , Status ,  &
                 & Ntypes , Ipload , Igrav , Pload2 , Load , Nopld2 , Temp , Tempd , Tempp1 , Tempp2 , Tempp3 , Temprb , Buf3 ,     &
                 & Pload3 , Ipld3 , Tempg , Tempp4
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER file , flag , i , id , ifile , isave , itabl , itempd , j , k , khi , klo , kn , l , n , n1 , nam(2) , neqx , ni ,       &
         & nodef , nogo , ntempd , nx
   LOGICAL intern
!
! End of declarations
!
!
!     GP3B BUILDS THE GRID POINT TEMPERATURE TABLE (GPTT).
!     TEMPD AND TEMP CARDS ARE READ.
!     THE GPTT HEADER CONTAINS THE FILE NAME PLUS 3 WORDS FOR EACH
!     TEMPERATURE SET.
!       WORD 1 = TEMP SET ID.
!       WORD 2 = DEFAULT TEMP OR -1 IF NO DEFAULT TEMP.
!       WORD 3 = RECORD NO. (AFTER HEADER RECORD) OF TEMPERATURE DATA
!                FOR THE SET, OR
!                ZERO IF ONLY A DEFAULT TEMP IS DEFINED FOR THE SET.
!     DATA RECORDS OF THE GPTT CONSIST OF PAIRS OF EXTERNAL INDEX AND
!     TEMPERATURE. EACH DATA RECORD IS SORTED ON EXTERNAL INDEX.
!
!     AN IDENTICAL SET OF RECORDS WITH INTERNAL INDICES IS APPENDED AT
!     THE END OF THE GPTT.
!
!
   EQUIVALENCE (Geom3,Geomp) , (Gptt,Scr1)
   DATA nam/4HGP3B , 4H    /
!
!     TURN NODEF FLAG ON
!
   id = 0
   nodef = 0
!
!     READ EQEXIN INTO CORE
!
   file = Eqexin
   CALL open(*1400,Eqexin,Z(Buf2),Rdrew)
   CALL fwdrec(*1500,Eqexin)
   CALL read(*1500,*100,Eqexin,Z,Buf3,1,neqx)
   CALL mesage(-8,0,nam)
 100  CALL close(Eqexin,Clsrew)
   kn = neqx/2
   itempd = neqx + 1
   itabl = itempd
!
!     READ TEMPERATURE DEFAULT CARDS (IF PRESENT)
!
   file = Geomp
   CALL preloc(*99999,Z(Buf1),Geomp)
   CALL locate(*300,Z(Buf1),Tempd,flag)
   i = itempd
   nodef = 1
   Notemp = 1
   DO
      CALL read(*1500,*200,Geomp,Z(i),2,0,flag)
      i = i + 2
   ENDDO
 200  itabl = i
   ntempd = i - 2
   n = itabl - itempd
   CALL sort(0,0,2,1,Z(itempd),n)
!
!     READ TEMP CARDS.  DETERMINE NO. OF TEMP SETS
!     FOR EACH SET ID, LOOK UP THE DEFAULT TEMPERATURE
!     WRITE SET ID, DEFAULT TEMP (OR -1) AND RECORD NUMBER
!     OF THE TEMPERATURE DATA (OR 0) IN THE GPTT HEADER
!
 300  j = 0
   k = itempd
   i = itabl
   l = 1
   file = Geomp
   CALL locate(*1300,Z(Buf1),Temp,flag)
   Notemp = 1
   file = Gptt
   CALL open(*1400,Gptt,Z(Buf2),Wrtrew)
   CALL fname(Gptt,Buf)
   CALL write(Gptt,Buf,2,0)
!
!     OPEN ETT AS TEMPORARY SCRATCH TO FORM IDENTICAL FILE WITH
!     INTERNAL NOTATION
!
   file = Ett
   CALL open(*1400,Ett,Z(Buf3),Wrtrew)
   CALL fname(Ett,Buf)
   CALL write(Ett,Buf,2,0)
   file = Geomp
   DO
      CALL read(*1500,*400,Geomp,Buf,3,0,flag)
      j = j + 1
      IF ( id/=Buf(1) ) THEN
         id = Buf(1)
         Z(i) = j
         i = i + 1
         IF ( nodef==0 ) THEN
            Buf(2) = -1
         ELSE
            DO WHILE ( k<=ntempd )
               IF ( id<Z(k) ) THEN
                  Buf(2) = -1
               ELSEIF ( id==Z(k) ) THEN
                  Buf(2) = Z(k+1)
                  k = k + 2
               ELSE
                  Buf(1) = Z(k)
                  Buf(2) = Z(k+1)
                  Buf(3) = 0
                  CALL write(Gptt,Buf,3,0)
                  CALL write(Ett,Buf,3,0)
                  k = k + 2
                  CYCLE
               ENDIF
               GOTO 320
            ENDDO
            Buf(2) = -1
         ENDIF
 320     Buf(3) = l
         Buf(1) = id
         l = l + 1
         CALL write(Gptt,Buf,3,0)
         CALL write(Ett,Buf,3,0)
         j = 0
      ENDIF
   ENDDO
 400  IF ( nodef/=0 ) THEN
      IF ( k<=ntempd ) THEN
         Buf(3) = 0
         DO l = k , ntempd , 2
            Buf(1) = Z(l)
            Buf(2) = Z(l+1)
            CALL write(Ett,Buf,3,0)
            CALL write(Gptt,Buf,3,0)
         ENDDO
      ENDIF
   ENDIF
   CALL write(Gptt,0,0,1)
   CALL write(Ett,0,0,1)
   CALL bckrec(Geomp)
   n = i
   Z(n) = j + 1
   i = itabl + 1
!
!     READ EACH TEMP SET
!     SORT ON EXTERNAL INDEX AND WRITE ON GPTT
!
   ifile = Gptt
   intern = .FALSE.
   isave = i
   nogo = 0
 500  CALL read(*1500,*1600,Geomp,0,-3,0,flag)
   n1 = n + 1
 600  j = n1
   nx = Z(i)
   ni = 1
 700  CALL read(*1500,*1600,Geomp,Buf,3,0,flag)
   IF ( intern ) THEN
!
!     INTERNAL BINARY SEARCH ROUTINE.
!
      klo = 1
      khi = kn
      k = (klo+khi+1)/2
      DO
         IF ( Buf(2)<Z(2*k-1) ) THEN
            khi = k
         ELSEIF ( Buf(2)==Z(2*k-1) ) THEN
            Buf(2) = Z(2*k)
            EXIT
         ELSE
            klo = k
         ENDIF
         IF ( khi-klo<1 ) THEN
            CALL mesage(-30,9,Buf)
            CALL mesage(j,file,nam)
            GOTO 99999
         ELSEIF ( khi-klo==1 ) THEN
            IF ( k==klo ) THEN
               k = khi
            ELSE
               k = klo
            ENDIF
            klo = khi
         ELSE
            k = (klo+khi+1)/2
         ENDIF
      ENDDO
   ENDIF
   Z(j) = Buf(2)
   Z(j+1) = Buf(3)
   j = j + 2
   IF ( j>=Buf3 ) THEN
      j = -8
      CALL mesage(j,file,nam)
      GOTO 99999
   ELSE
      ni = ni + 1
      IF ( ni<=nx ) GOTO 700
      nx = j - n1
      CALL sort(0,0,2,1,Z(n1),nx)
!
!     TEST FOR UNIQUENESS OF POINT AND TEMPERATURE
!
      khi = j - 1
      klo = n1 + 2
      k = j
      IF ( klo<khi ) THEN
         k = klo
         DO j = klo , khi , 2
            IF ( Z(j)/=Z(j-2) ) THEN
!
!     VALID TEMPERATURE
!
               Z(k) = Z(j)
               Z(k+1) = Z(j+1)
               k = k + 2
            ELSE
!
!     NOT FATAL IF SAME TEMPERATURE
!
               IF ( Z(j+1)/=Z(j-1) ) nogo = nogo + 1
               IF ( .NOT.(intern) ) THEN
                  CALL page2(2)
                  WRITE (Nout,99001) Ufm , Z(j-1) , Z(j+1) , Z(j)
99001             FORMAT (A23,' 2100, TEMPERATURE SPECIFIED HAS ',1P,E10.3,4H AND,1P,E10.3,' FOR GRID',I9)
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
      nx = k - n1
      CALL write(ifile,Z(n1),nx,1)
      i = i + 1
      IF ( i<=n ) GOTO 600
!
!     NOW DO SAME AS ABOVE WITH OUTPUT IN INTERNAL INDEX NOTATION.
!
      IF ( nogo/=0 ) CALL mesage(-61,nogo,0)
      IF ( .NOT.(intern) ) THEN
         CALL bckrec(Geomp)
         intern = .TRUE.
         ifile = Ett
         i = isave
         GOTO 500
      ENDIF
   ENDIF
!
!     NOW APPEND ENTIRE ETT FILE TO GPTT FILE
!
 800  file = Ett
   CALL close(Ett,Clsrew)
   CALL open(*1400,Ett,Z(Buf3),Rdrew)
 900  DO
      CALL read(*1100,*1000,Ett,Z,Buf3-1,0,flag)
      CALL write(Gptt,Z,Buf3-1,0)
   ENDDO
 1000 CALL write(Gptt,Z,flag,1)
   GOTO 900
 1100 CALL close(Gptt,Clsrew)
   CALL close(Ett,Clsrew)
 1200 CALL close(Geomp,Clsrew)
   GOTO 99999
!
!     NO TEMP CARDS PRESENT. IF NO DEFAULT CARDS, NO GPTT.
!     OTHERWISE, GPTT IS COMPRISED ONLY OF DEFAULT TEMPERATURES.
!     WRITE THE SET IDS AND DEFAULT TEMPS IN THE HEADER RECORD.
!
 1300 IF ( nodef==0 ) GOTO 1200
   file = Gptt
   CALL open(*1400,Gptt,Z(Buf2),Wrtrew)
   CALL fname(Gptt,Buf)
   CALL write(Gptt,Buf,2,0)
   file = Ett
   CALL open(*1400,Ett,Z(Buf3),Wrtrew)
   CALL fname(Ett,Buf)
   CALL write(Ett,Buf,2,0)
   Buf(3) = 0
   DO k = itempd , ntempd , 2
      Buf(1) = Z(k)
      Buf(2) = Z(k+1)
      CALL write(Gptt,Buf,3,0)
   ENDDO
   CALL write(Ett,Buf,3,0)
   CALL write(Gptt,0,0,1)
   CALL write(Ett,0,0,1)
   GOTO 800
!
!     FATAL ERROR MESAGES
!
 1400 j = -1
   CALL mesage(j,file,nam)
   GOTO 99999
 1500 j = -2
   CALL mesage(j,file,nam)
   GOTO 99999
 1600 j = -3
   CALL mesage(j,file,nam)
!
99999 RETURN
END SUBROUTINE gp3b
