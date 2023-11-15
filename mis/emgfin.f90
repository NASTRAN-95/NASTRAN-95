
SUBROUTINE emgfin
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Date(3) , Dictn(3) , Est , Flags(3) , Ibuf , Icmbar , Icore , Ix(6) , Jcore , Lcstm , Lhmat , Line ,      &
         & Lmat , Mach , Matrix(3) , Mpt , Ncore , Nlpp , Nok4gg , Nokmb(3) , Nout , Precis , Rdrew , Wrtrew
   REAL Cmass , Corex(1) , Corey(201) , Cstm , Dit , Dum12(12) , Dummy(11) , Geom2 , Head(96) , Rd , Rx(200) , Sk1p2(2) , Skip2(2) ,&
      & Skip6(6) , Skp(4) , Surfac , Volume , Wrt , Z(2)
   LOGICAL Error , Heat , Linear
   COMMON /blank / Nokmb , Nok4gg , Cmass , Dummy , Volume , Surfac
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2 , Matrix , Dictn
   COMMON /emgprm/ Icore , Jcore , Ncore , Dum12 , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /hmatdd/ Skp , Linear
   COMMON /machin/ Mach
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /output/ Head
   COMMON /system/ Ibuf , Nout , Skip6 , Nlpp , Skip2 , Line , Sk1p2 , Date
   COMMON /zzzzzz/ Corex
!
! Local variable declarations
!
   REAL d2 , d3 , inpi(10) , tmas2 , tmas3 , tvol2 , tvol3
   INTEGER i , ibuf1 , ibuf2 , inp , j , l , ln , mcb(7) , n4 , n7 , ngpt , nokdgg , nrec , scr3 , scr4 , sil(32) , sub(2) , vafile
!
! End of declarations
!
!
!     THIS ROUTINE OF THE -EMG- MODULE WRAPS UP THE WORK OF THE MODULE.
!
   EQUIVALENCE (Corex(1),Corey(1),Rx(1),Ix(1)) , (Z(1),Corey(201))
   DATA vafile , scr4/207 , 304/
   DATA d2 , d3/4H2-D  , 4H3-D /
   DATA sub , scr3/4HEMGF , 4HIN   , 303/
   DATA inpi/4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
!
!     CLOSE ALL FILES, EXCEPT SCR4
!
   DO i = 1 , 3
      Nokmb(i) = -Flags(i) - 1
      IF ( Flags(i)+1==0 ) Flags(i) = 0
      IF ( Flags(i)==0 ) Nokmb(i) = -1
      CALL close(Matrix(i),Clsrew)
      CALL close(Dictn(i),Clsrew)
   ENDDO
   CALL close(Est,Clsrew)
!
!     HEAT ONLY - SET NONILINEAR FLAG BASED ON VALUE PREVIOUSLY SET BY
!     HMAT ROUTINE
!
   IF ( Heat .AND. .NOT.Linear ) nokdgg = +1
   IF ( Heat .AND. Linear ) nokdgg = -1
!
!  WRITE TRAILERS FOR FILES PREPARED.
!
   IF ( Error ) THEN
!
      IF ( Volume/=0. .OR. Surfac/=0. ) CALL close(scr4,Clsrew)
      GOTO 800
   ELSE
      DO i = 1 , 3
!
!     PRECISION IS STORED IN FIRST DATA WORD OF TRAILER.
!
         IF ( Flags(i)/=0 ) THEN
            mcb(1) = Matrix(i)
            CALL rdtrl(mcb)
            IF ( mcb(1)>0 ) THEN
               mcb(2) = Precis
               mcb(3) = 0
               mcb(4) = 0
               mcb(5) = 0
               mcb(6) = 0
               mcb(7) = 0
               CALL wrttrl(mcb)
!
               mcb(1) = Dictn(i)
               CALL rdtrl(mcb)
               IF ( mcb(1)>0 ) THEN
                  mcb(2) = Precis
                  mcb(3) = 0
                  mcb(4) = 0
                  mcb(5) = 0
                  mcb(6) = 0
                  mcb(7) = 0
                  CALL wrttrl(mcb)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( Volume<=0.0 .AND. Surfac<=0.0 ) GOTO 800
!
!     COMPUTE AND PRINT VOLUMES AND SURFACE AREAS FOR THE 2-D AND 3-D
!     ELEM. IF USER REQUESTED VIA PARAM CARD.
!
      CALL close(scr4,Clsrew)
      ibuf1 = Icore + 200
      ibuf2 = ibuf1 + Ibuf
      CALL open(*900,scr4,Z(ibuf1),Rdrew)
      tvol2 = 0.0
      tvol3 = 0.0
      tmas2 = 0.0
      tmas3 = 0.0
      nrec = 0
      Line = Nlpp
      inp = 0
!
!     CHECK ANY REQUEST TO SAVE VOLUME AND AREA COMPUTATIONS ON OUTPUT
!     FILE SET INP TO APPROPRIATE VALUE IF IT IS AN INPI FILE
!
      mcb(1) = vafile
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<=0 ) GOTO 200
      CALL fname(vafile,Z(1))
      DO i = 1 , 10
         IF ( Z(1)==inpi(i) ) GOTO 50
      ENDDO
      GOTO 100
 50   inp = i + 13
      IF ( inp==14 .AND. Mach==2 ) inp = 24
      vafile = scr3
      mcb(1) = scr3
 100  CALL open(*1000,vafile,Z(ibuf2),Wrtrew)
      CALL write(vafile,Z(1),2,0)
      CALL write(vafile,Head(1),96,0)
      CALL write(vafile,Date(1),3,1)
      nrec = 1
   ENDIF
 200  CALL read(*400,*300,scr4,Rx,201,1,i)
   WRITE (Nout,99001)
99001 FORMAT (' *** WARNING,   RX TOO SMALL IN EMGFIN ***')
 300  ngpt = Ix(6)
   IF ( ngpt>=3 ) THEN
      IF ( Line>=Nlpp ) THEN
         Line = 5
         CALL page1
         WRITE (Nout,99002) (i,i=1,6)
99002    FORMAT (17X,'V O L U M E S,  M A S S E S,  A N D  S U R F A C E ',' A R E A S  O F  2-  A N D  3-  D  E L E M E N T S',    &
               & ///10X,7HELEMENT,8X,3HEID,8X,6HVOLUME,7X,4HMASS,1X,6(3X,7HSURFACE,I2),/10X,29(4H----),/)
         IF ( Volume<=0.0 ) WRITE (Nout,99003)
99003    FORMAT (10X,42H(NO MASS AND VOLUME COMPUTATION REQUESTED),/)
         IF ( Surfac<=0.0 ) WRITE (Nout,99004)
99004    FORMAT (10X,39H(NO SURFACE AREA COMPUTATION REQUESTED),/)
         IF ( Volume<=0.0 .OR. Surfac<=0.0 ) Line = Line + 2
      ENDIF
      l = 5
!
!     ENTRIES IN RX ARRAY, AS SAVED IN SCR4 BY KTRIQD,KTETRA,IHEXI,
!     EMGPRO
!        RX( 1),RX(2) = ELEMENT BCD NAME
!        IX( 3) = ELEMENT ID
!        RX( 4) = VOLUME (SOLID), OR THICKNESS (PLATE)
!        RX( 5) = TOTAL MASS (SOLID), OR DENSITY (PLATE)
!        IX( 6) = NO. OF GRID POINTS, = NGPT
!        IX(7)...IX(6+NGPT) = SIL OF THE GRID POINTS
!        RX( 7+NPGT) = CID OF 1ST GRID POINT
!        RX( 8+NPGT) = X COORD. OF 1ST GRID POINT
!        RX( 9+NPGT) = Y COORD. OF 1ST GRID POINT
!        RX(10+NPGT) = Z COORD. OF 1ST GRID POINT
!        IX(11+NPGT...) = REPEAT FOR OTHER GRID POINTS
!
!     CALL SFAREA TO COMPUTE AREAS, 6 VALUES ARE RETURNED IN RX(6...11)
!     AND NO. OF SURFACES IN NGPT
!     VOLUME AND MASS ARE ALSO COMPUTED FOR THE PLATE ELEMENTS.
!
      DO i = 1 , ngpt
         sil(i) = Ix(6+i)
      ENDDO
      ln = ngpt
      CALL sfarea(ln,Rx,Ix(ngpt+7))
      l = 5
      IF ( Surfac>0.0 ) l = 5 + ln
      IF ( Volume>0.0 ) WRITE (Nout,99005) (Ix(i),i=1,3) , (Rx(i),i=4,l)
99005 FORMAT (10X,2A4,I10,2X,8E12.4)
      IF ( Volume<=0.0 ) WRITE (Nout,99006) (Ix(i),i=1,3) , (Rx(i),i=6,l)
99006 FORMAT (10X,2A4,I10,26X,6E12.4)
      Line = Line + 1
!
      IF ( nrec/=0 ) THEN
         nrec = nrec + 1
         Ix(5) = (ln*100) + ngpt
         CALL write(vafile,Rx(1),l,0)
         n4 = ngpt*4
         n7 = n4 + 7
         j = 1
         DO i = 7 , n7 , 4
            Ix(i) = sil(j)
            j = j + 1
         ENDDO
         CALL write(vafile,Rx(ngpt+7),n4,1)
      ENDIF
!
!     A RECORD IS SAVED IN VAFILE FOR EACH ELEM., HAVING THE FOLLOWING
!     DATA
!
!        WORDS  1,2 ELEMENT BCD NAME
!                 3 ELEMENT ID, INTEGER
!                 4 VOLUME (3-D ELEMS), ZERO (2-D ELEMS), REAL
!                 5 (NO. OF SURFACES, N)*100 + NO. OF GRID PTS, INTEGER
!                 6 AREA OF FIRST SURFACE, REAL
!        7 THRU 5+N REPEAT FOR N SURFACES, REAL
!             5+N+1 SIL OF FIRST GRID POINT, INTEGER
!         5+N+2,3,4 X,Y,Z COORDINATES OF THE FIRST GRID POINT, REAL
!               ... REPEAT LAST 4 WORDS FOR OTHER GRID POINTS, REAL
!
      IF ( Volume>0.0 ) THEN
         IF ( ngpt>1 ) THEN
            tvol3 = tvol3 + Rx(4)
            tmas3 = tmas3 + Rx(5)
         ELSE
            tvol2 = tvol2 + Rx(4)
            tmas2 = tmas2 + Rx(5)
         ENDIF
      ENDIF
   ENDIF
   GOTO 200
 400  CALL close(scr4,Clsrew)
   IF ( nrec/=0 ) THEN
      CALL close(vafile,Clsrew)
      mcb(2) = nrec
      DO i = 3 , 7
         mcb(i) = 0
      ENDDO
      CALL wrttrl(mcb(1))
   ENDIF
   IF ( Volume<=0.0 ) GOTO 800
   IF ( tvol2>0.0 ) WRITE (Nout,99010) tvol2 , tmas2 , d2
   IF ( tvol3>0.0 ) WRITE (Nout,99010) tvol3 , tmas3 , d3
   IF ( nrec<=0 ) GOTO 800
!
!     IF OUTPUT FILE REQUESTED BY USER IS AN INPI FILE, COPY FROM VAFILE
!     TO INPI, A FORTRAN WRITTEN BINARY FILE
!
   IF ( inp==0 ) GOTO 700
   CALL open(*1000,vafile,Z(ibuf2),Rdrew)
 500  CALL read(*700,*600,vafile,Z(1),ibuf2,1,j)
   j = -8
   CALL mesage(j,vafile,sub)
   GOTO 99999
 600  WRITE (inp) (Z(i),i=1,j)
   GOTO 500
 700  CALL close(vafile,Clsrew)
   CALL fname(207,Z(1))
   WRITE (Nout,99007) Z(1) , Z(2) , Date
99007 FORMAT ('0*** VOLUMES AND EXTERNAL SURFACE AREAS WERE SAVED IN ','OUTPUT FILE ',2A4,4H ON ,I2,1H/,I2,3H/19,I2)
   IF ( inp==0 ) WRITE (Nout,99008)
99008 FORMAT (1H+,91X,21H(A GINO WRITTEN FILE))
   IF ( inp/=0 ) WRITE (Nout,99009) inp
99009 FORMAT (1H+,91X,28H(A FORTRAN BINARY FILE, UNIT,I3,1H))
 800  Volume = 0.0
   Surfac = 0.0
   RETURN
 900  CALL mesage(-1,scr4,sub)
 1000 j = -1
   CALL mesage(j,vafile,sub)
99010 FORMAT (/6X,24H* TOTAL VOLUME AND MASS=,2E12.4,3H  (,A4,9HELEMENTS))
99999 END SUBROUTINE emgfin
