
SUBROUTINE gp2
   IMPLICIT NONE
   INTEGER Cls , Clsrew , Elem(1) , Iaxif , Incr , Junk(36) , Last , Nbpc , Nbpw , Nelem , Nfile(6) , Noect , Rd , Rdrew , Sysbuf , &
         & Two(32) , Wrt , Wrtrew , Z(1)
   COMMON /blank / Noect
   COMMON /gpta1 / Nelem , Last , Incr , Elem
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /setup / Nfile
   COMMON /system/ Sysbuf , Junk , Iaxif , Nbpc , Nbpw
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   INTEGER b(34) , buf1 , buf2 , buf3 , cbar , cbeam , ect , eqexin , file , genel(2) , geom2 , geomp , gp2h(2) , i , igrid , ii ,  &
         & ijk , ijk1 , j , k , khi , klo , kn , l , lx , m , mcb(7) , mm , n , n1 , name , ncore , nogeo2 , nogo , nread , ns ,    &
         & nud , nui , nz , ret , ret1
   REAL flag
   INTEGER korsz
!
!     GP2 BUILDS THE ELEMENT CONNECTION TABLE (ECT).
!     STRUCTURAL ELEMENT CONNECTION CARDS ARE ON GEOM2.
!     EACH EXTERNAL GRID PT. NO. IS CONVERTED TO AN INTERNAL INDEX.
!     IN ADDITION, GENERAL ELEMENT CARDS ARE READ AND
!     EXTERNAL GRID NUMBERS ARE CONVERTED TO INTERNAL NUMBERS.
!
!
   !>>>>EQUIVALENCE (geomp,geom2)
!
!     INPUT  DATA FILES
   DATA geom2 , eqexin/101 , 102/
!
!     OUTPUT DATA FILES
   DATA ect/201/
!
!     MISC   DATA
   DATA gp2h/4HGP2  , 4H    / , cbar/4HBAR / , cbeam/4HBEAM/
!
!     GENEL DATA CARDS PROCESSED BY GP2 IN ADDITION TO ELEMENTS.
   DATA genel/4301 , 43/
!
!
!     PERFORM GENERAL INITIALIZATION
!
   CALL delset
   buf1 = korsz(Z) - Sysbuf - 2
   buf2 = buf1 - Sysbuf
   Noect = -1
   buf3 = buf2 - Sysbuf
   mcb(1) = geom2
   CALL rdtrl(mcb)
!
!     READ EQEXIN INTO CORE
!
   file = eqexin
   CALL open(*1900,eqexin,Z(buf1),Rdrew)
   CALL fwdrec(*2000,eqexin)
   CALL read(*2000,*100,eqexin,Z,buf2,1,n)
   CALL mesage(-8,0,gp2h)
 100  CALL close(eqexin,Clsrew)
   kn = n/2
   n1 = n + 1
!
!     OPEN GEOM2. IF PURGED, RETURN.
!     OTHERWISE, OPEN ECT AND WRITE HEADER RECORD.
!
   nogeo2 = 0
   CALL preloc(*200,Z(buf1),geom2)
   nogeo2 = 1
!
   Noect = 1
   nogo = 0
   file = ect
   CALL open(*1900,ect,Z(buf2),Wrtrew)
   CALL fname(ect,b)
   CALL write(ect,b,2,1)
   GOTO 300
 200  RETURN
 300  DO
!
!     READ 3-WORD ID FROM GEOM2. SEARCH ELEMENT TABLE FOR MATCH.
!     IF FOUND, BRANCH TO ELEMENT CODE. IF NOT FOUND, SEARCH GENEL
!     TABLE  FOR MATCH. IF FOUND BRANCH TO APPROPRIATE CODE. IF NOT
!     FOUND, SKIP RECORD AND CONTINUE.
!
      CALL read(*1500,*2100,geom2,b,3,0,flag)
      DO i = 1 , Last , Incr
         IF ( Elem(i+3)==b(1) ) GOTO 400
      ENDDO
      IF ( genel(1)==b(1) ) THEN
         k = (i+1)/2
!
!     GENERAL ELEMENTS-- WRITE 3-WORD ID ON ECT. READ ALL GENELS,
!     CONVERT EXTERNAL GRID NOS. TO INTERNAL NOS. AND WRITE THEM ON ECT.
!
         CALL write(ect,b,3,0)
         file = geom2
         l = 2
         ASSIGN 1300 TO ret
         ASSIGN 2400 TO ret1
         GOTO 1100
      ELSE
         CALL fwdrec(*1500,geom2)
      ENDIF
   ENDDO
!
!     WRITE 3-WORD ID ON ECT. READ ALL CARDS FOR ELEMENT AND
!     CONVERT EXTERNAL GRID NOS. TO INTERNAL NOS.  WRITE ENTRIES ON ECT
!     DIRECTLY AFTER CONVERSION.
!
 400  ASSIGN 700 TO ret
   ASSIGN 2300 TO ret1
   CALL write(ect,b,3,0)
   m = Elem(i+5)
   lx = Elem(i+12)
   mm = lx + Elem(i+9)
   name = Elem(i)
   ii = n1
   file = geom2
 500  CALL read(*2000,*1000,file,b,m,0,flag)
!
!     CHECK LATER TO SEE IF RESTRICTION APPLIES TO AXIF PROBLEMS
!
   IF ( Iaxif==0 ) THEN
      IF ( Nbpw<=32 .AND. b(1)>16777215 ) THEN
         nogo = 1
         CALL mesage(30,138,b)
      ENDIF
   ENDIF
!                                  16777215 = 2**24 - 1
   l = lx
 600  IF ( b(l)/=0 ) GOTO 1600
 700  l = l + 1
   IF ( l<mm ) GOTO 600
   IF ( name==cbeam ) THEN
      IF ( b(8)==0 ) GOTO 900
      ASSIGN 800 TO ret
      l = 8
      GOTO 1600
   ELSE
      IF ( name/=cbar ) GOTO 900
!
!     SPECIAL PROCESSING FOR BAR AND BEAM ELEMENTS
!
      IF ( b(8)==1 ) GOTO 900
      ASSIGN 800 TO ret
      l = 5
      GOTO 1600
   ENDIF
 800  ASSIGN 700 TO ret
!
 900  CALL write(ect,b,m,0)
   GOTO 500
!
!     CURRENT ELEMENT IS COMPLETE
!
 1000 CALL write(ect,0,0,1)
   GOTO 300
 1100 ijk = 0
   CALL read(*2000,*1400,geom2,b,1,0,flag)
   CALL write(ect,b,1,0)
 1200 CALL read(*2000,*2100,geom2,b(2),2,0,flag)
   IF ( b(2)/=-1 ) GOTO 1600
   nud = b(3)
   IF ( ijk/=0 ) THEN
      CALL write(ect,b(2),2,0)
      CALL read(*2000,*2100,geom2,ijk1,1,0,flag)
      CALL write(ect,ijk1,1,0)
      ncore = buf2 - n1
      nz = (nui*(nui+1))/2
      nread = 0
      DO
         n = min0(ncore,nz-nread)
         CALL read(*2000,*2100,geom2,Z(n1),n,0,flag)
         CALL write(ect,Z(n1),n,0)
         nread = nread + n
         IF ( nread>=nz ) THEN
            CALL read(*2000,*2100,geom2,ijk,1,0,flag)
            CALL write(ect,ijk,1,0)
            IF ( ijk==0 ) GOTO 1100
            ns = nui*nud
            nread = 0
            DO
               n = min0(ncore,ns-nread)
               CALL read(*2000,*2100,geom2,Z(n1),n,0,flag)
               CALL write(ect,Z(n1),n,0)
               nread = nread + n
               IF ( nread>=ns ) GOTO 1100
            ENDDO
         ENDIF
      ENDDO
   ELSE
      nui = b(3)
      ijk = 1
   ENDIF
 1300 CALL write(ect,b(2),2,0)
   GOTO 1200
 1400 CALL write(ect,0,0,1)
   GOTO 300
!
!     CLOSE FILES, WRITE TRAILER AND RETURN.
!
 1500 CALL close(geom2,Clsrew)
   CALL close(ect,Clsrew)
   mcb(1) = geom2
   CALL rdtrl(mcb)
   mcb(1) = ect
   CALL wrttrl(mcb)
   IF ( nogo/=0 ) CALL mesage(-61,0,0)
   RETURN
!
!
!     INTERNAL BINARY SEARCH ROUTINE
!     ==============================
!
 1600 klo = 1
   khi = kn
   igrid = b(l)
 1700 k = (klo+khi+1)/2
 1800 IF ( igrid<Z(2*k-1) ) THEN
      khi = k
   ELSEIF ( igrid==Z(2*k-1) ) THEN
      b(l) = Z(2*k)
      GOTO ret
   ELSE
      klo = k
   ENDIF
   IF ( khi-klo<1 ) THEN
      GOTO ret1
   ELSEIF ( khi-klo==1 ) THEN
      IF ( k==klo ) THEN
         k = khi
      ELSE
         k = klo
      ENDIF
      klo = khi
      GOTO 1800
   ELSE
      GOTO 1700
   ENDIF
!
!
!     FATAL ERROR MESSAGES
!
 1900 j = -1
   GOTO 2200
 2000 j = -2
   GOTO 2200
 2100 j = -3
 2200 CALL mesage(j,file,gp2h)
 2300 k = 7
   GOTO 2500
 2400 k = 61
 2500 b(2) = igrid
   CALL mesage(30,k,b)
   nogo = 1
   GOTO ret
END SUBROUTINE gp2