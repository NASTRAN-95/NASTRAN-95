!*==emgfin.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgfin
   USE c_blank
   USE c_emgfil
   USE c_emgprm
   USE c_hmatdd
   USE c_machin
   USE c_names
   USE c_output
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: d2 , d3
   INTEGER :: i , ibuf1 , ibuf2 , inp , j , l , ln , n4 , n7 , ngpt , nokdgg , nrec
   REAL , DIMENSION(10) , SAVE :: inpi
   INTEGER , DIMENSION(6) :: ix
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(200) :: rx
   INTEGER , SAVE :: scr3 , scr4 , vafile
   INTEGER , DIMENSION(32) :: sil
   INTEGER , DIMENSION(2) , SAVE :: sub
   REAL :: tmas2 , tmas3 , tvol2 , tvol3
   REAL , DIMENSION(2) :: z
   EXTERNAL close , fname , mesage , open , page1 , rdtrl , read , sfarea , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE OF THE -EMG- MODULE WRAPS UP THE WORK OF THE MODULE.
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1),Rx(1),Ix(1)) , (Z(1),Corey(201))
   DATA vafile , scr4/207 , 304/
   DATA d2 , d3/4H2-D  , 4H3-D /
   DATA sub , scr3/4HEMGF , 4HIN   , 303/
   DATA inpi/4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CLOSE ALL FILES, EXCEPT SCR4
!
         DO i = 1 , 3
            nokmb(i) = -flags(i) - 1
            IF ( flags(i)+1==0 ) flags(i) = 0
            IF ( flags(i)==0 ) nokmb(i) = -1
            CALL close(matrix(i),clsrew)
            CALL close(dictn(i),clsrew)
         ENDDO
         CALL close(est,clsrew)
!
!     HEAT ONLY - SET NONILINEAR FLAG BASED ON VALUE PREVIOUSLY SET BY
!     HMAT ROUTINE
!
         IF ( heat .AND. .NOT.linear ) nokdgg = +1
         IF ( heat .AND. linear ) nokdgg = -1
!
!  WRITE TRAILERS FOR FILES PREPARED.
!
         IF ( error ) THEN
!
            IF ( volume/=0. .OR. surfac/=0. ) CALL close(scr4,clsrew)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 1 , 3
!
!     PRECISION IS STORED IN FIRST DATA WORD OF TRAILER.
!
               IF ( flags(i)/=0 ) THEN
                  mcb(1) = matrix(i)
                  CALL rdtrl(mcb)
                  IF ( mcb(1)>0 ) THEN
                     mcb(2) = precis
                     mcb(3) = 0
                     mcb(4) = 0
                     mcb(5) = 0
                     mcb(6) = 0
                     mcb(7) = 0
                     CALL wrttrl(mcb)
!
                     mcb(1) = dictn(i)
                     CALL rdtrl(mcb)
                     IF ( mcb(1)>0 ) THEN
                        mcb(2) = precis
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
            IF ( volume<=0.0 .AND. surfac<=0.0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE AND PRINT VOLUMES AND SURFACE AREAS FOR THE 2-D AND 3-D
!     ELEM. IF USER REQUESTED VIA PARAM CARD.
!
            CALL close(scr4,clsrew)
            ibuf1 = icore + 200
            ibuf2 = ibuf1 + ibuf
            CALL open(*120,scr4,z(ibuf1),rdrew)
            tvol2 = 0.0
            tvol3 = 0.0
            tmas2 = 0.0
            tmas3 = 0.0
            nrec = 0
            line = nlpp
            inp = 0
!
!     CHECK ANY REQUEST TO SAVE VOLUME AND AREA COMPUTATIONS ON OUTPUT
!     FILE SET INP TO APPROPRIATE VALUE IF IT IS AN INPI FILE
!
            mcb(1) = vafile
            CALL rdtrl(mcb(1))
            IF ( mcb(1)<=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fname(vafile,z(1))
            DO i = 1 , 10
               IF ( z(1)==inpi(i) ) GOTO 10
            ENDDO
            GOTO 20
 10         inp = i + 13
            IF ( inp==14 .AND. mach==2 ) inp = 24
            vafile = scr3
            mcb(1) = scr3
 20         CALL open(*140,vafile,z(ibuf2),wrtrew)
            CALL write(vafile,z(1),2,0)
            CALL write(vafile,head(1),96,0)
            CALL write(vafile,date(1),3,1)
            nrec = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*60,*40,scr4,rx,201,1,i)
         WRITE (nout,99001)
99001    FORMAT (' *** WARNING,   RX TOO SMALL IN EMGFIN ***')
 40      ngpt = ix(6)
         IF ( ngpt>=3 ) THEN
            IF ( line>=nlpp ) THEN
               line = 5
               CALL page1
               WRITE (nout,99002) (i,i=1,6)
99002          FORMAT (17X,'V O L U M E S,  M A S S E S,  A N D  S U R F A C E ',                                                   &
                      &' A R E A S  O F  2-  A N D  3-  D  E L E M E N T S',///10X,7HELEMENT,8X,3HEID,8X,6HVOLUME,7X,4HMASS,1X,     &
                      &6(3X,7HSURFACE,I2),/10X,29(4H----),/)
               IF ( volume<=0.0 ) WRITE (nout,99003)
99003          FORMAT (10X,42H(NO MASS AND VOLUME COMPUTATION REQUESTED),/)
               IF ( surfac<=0.0 ) WRITE (nout,99004)
99004          FORMAT (10X,39H(NO SURFACE AREA COMPUTATION REQUESTED),/)
               IF ( volume<=0.0 .OR. surfac<=0.0 ) line = line + 2
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
               sil(i) = ix(6+i)
            ENDDO
            ln = ngpt
            CALL sfarea(ln,rx,ix(ngpt+7))
            l = 5
            IF ( surfac>0.0 ) l = 5 + ln
            IF ( volume>0.0 ) WRITE (nout,99005) (ix(i),i=1,3) , (rx(i),i=4,l)
99005       FORMAT (10X,2A4,I10,2X,8E12.4)
            IF ( volume<=0.0 ) WRITE (nout,99006) (ix(i),i=1,3) , (rx(i),i=6,l)
99006       FORMAT (10X,2A4,I10,26X,6E12.4)
            line = line + 1
!
            IF ( nrec/=0 ) THEN
               nrec = nrec + 1
               ix(5) = (ln*100) + ngpt
               CALL write(vafile,rx(1),l,0)
               n4 = ngpt*4
               n7 = n4 + 7
               j = 1
               DO i = 7 , n7 , 4
                  ix(i) = sil(j)
                  j = j + 1
               ENDDO
               CALL write(vafile,rx(ngpt+7),n4,1)
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
            IF ( volume>0.0 ) THEN
               IF ( ngpt>1 ) THEN
                  tvol3 = tvol3 + rx(4)
                  tmas3 = tmas3 + rx(5)
               ELSE
                  tvol2 = tvol2 + rx(4)
                  tmas2 = tmas2 + rx(5)
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(scr4,clsrew)
         IF ( nrec/=0 ) THEN
            CALL close(vafile,clsrew)
            mcb(2) = nrec
            DO i = 3 , 7
               mcb(i) = 0
            ENDDO
            CALL wrttrl(mcb(1))
         ENDIF
         IF ( volume<=0.0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( tvol2>0.0 ) WRITE (nout,99010) tvol2 , tmas2 , d2
         IF ( tvol3>0.0 ) WRITE (nout,99010) tvol3 , tmas3 , d3
         IF ( nrec<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IF OUTPUT FILE REQUESTED BY USER IS AN INPI FILE, COPY FROM VAFILE
!     TO INPI, A FORTRAN WRITTEN BINARY FILE
!
         IF ( inp==0 ) GOTO 100
         CALL open(*140,vafile,z(ibuf2),rdrew)
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*100,*80,vafile,z(1),ibuf2,1,j)
         j = -8
         CALL mesage(j,vafile,sub)
         RETURN
 80      WRITE (inp) (z(i),i=1,j)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 100     CALL close(vafile,clsrew)
         CALL fname(207,z(1))
         WRITE (nout,99007) z(1) , z(2) , date
99007    FORMAT ('0*** VOLUMES AND EXTERNAL SURFACE AREAS WERE SAVED IN ','OUTPUT FILE ',2A4,4H ON ,I2,1H/,I2,3H/19,I2)
         IF ( inp==0 ) WRITE (nout,99008)
99008    FORMAT (1H+,91X,21H(A GINO WRITTEN FILE))
         IF ( inp/=0 ) WRITE (nout,99009) inp
99009    FORMAT (1H+,91X,28H(A FORTRAN BINARY FILE, UNIT,I3,1H))
         spag_nextblock_1 = 4
      CASE (4)
         volume = 0.0
         surfac = 0.0
         RETURN
 120     CALL mesage(-1,scr4,sub)
 140     j = -1
         CALL mesage(j,vafile,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99010 FORMAT (/6X,24H* TOTAL VOLUME AND MASS=,2E12.4,3H  (,A4,9HELEMENTS))
END SUBROUTINE emgfin
