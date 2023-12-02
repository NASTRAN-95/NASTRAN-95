!*==ginofl.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ginofl
!
!     ROUTINE FOR GINOFILE MODULE
!
!     MODULE GINOFILE WILL CAPTURE ONE SCRATCH FILE (301 THRU 309) OF
!     PREVIOUS DAMP MODULE, AND MAKE IT A LEGITIMATE GINO FILE.
!     THE SCRATCH FILE CAN BE A TABLE DATA BLOCK OR A MATRIX DATA BLOCK.
!     USE DMAP ALTER TO PLACE THIS MODULE IMMEDIATELY AFTER ANY NASTRAN
!     EXECUTABLE DMAP MODULE WHOSE SCRATCH FILE IS TO BE CAPTURED
!
!     IT IS USER'S RESPONSIBILITY TO SEE THAT NO FIAT TABLE RE-
!     ARRANGEMENT BY MODULE XSFA BETWEEN THIS GINOFILE MODULE AND THE
!     PREVIOUS INTENDED MODULE
!
!     GINOFILE  /OUTFL/C,N,P1/C,N,P2/C,N,P3  $
!
!     INPUT   FILE = NONE
!     OUTPUT  FILE = OUTFL, ANY UNIQUE NAME
!     SCRATCH FILE = 301
!     PARAMETERS   -
!             P1   = SCRATCH FILE NUMBER, 301,302,303,...,309
!                    (NO DEFAULT)
!             P2   = ADDITIONAL NUMBER OF RECORDS IN P1 FILE TO BE
!                    SKIPPED (NOT INCLUDING HEADER RECORD, WHETHER IT
!                    EXISTS OR NOT, DEFAULT = 0)
!             P3   = NO. OF RECORDS TO BE COPIED TO OUTPUT FILE OUTFL,
!                    STARTING FROM THE P2+1 RECORD, OR UP TO EOF RECORD
!                    (DEFAULT JJ=999999)
!
!     THIS GINOFILE MODULE SHOULD BE MAPPED IN ALL LINKS EXCEPT LINK1
!
!     WRITTEN BY G.CHAN/UNISYS, MAY 1988
!     DEFINITELY THIS IS NOT AN ORDINARY JOB FOR AMATEUR OR SEASONNED
!     PROGRAMMERS, MY FRIENDS
!
!     THE TRICKY PART OF THIS PROGRAM IS THAT GINOFILE MODULE USES ONLY
!     ONE OUTPUT FILE AND ONE SCRATCH FILE, WHICH IS 301
!     THE PROBLEMS HERE ARE (1) HOW TO CAPTURE OTHER SCRATCH FILE OF THE
!     PREVIOUS DMAP MODULE, SAY 303, WHILE ONLY 301 IS AVAILABLE. AND
!     (2) HOW TO CAPTURE SCRATCH1 FILE WHILE THE ORIGINAL 301 GINO DATA,
!     SUCH AS TRAILER, UNIT NUMBER, FILE INDEX ETC. ARE GONE. (THE
!     ORIGINAL 301 GINO DATA HAS BEEN ZEROED OUT TO GIVE ROOM FOR THE
!     NEW SCRATCH1 BEING ASSIGNED TO GINOFILE MODULE).
!
   IMPLICIT NONE
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xsortx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: debug
   INTEGER :: file , fisti , fisti1 , i , ibuf1 , ibuf2 , icrq , ii , index , irctyp , k , kore , l , ncol , nwds , scr , scrx ,    &
            & trl2
   INTEGER , DIMENSION(2) :: fn
   INTEGER , DIMENSION(7) :: itrl , trl
   INTEGER , SAVE :: iz2 , outfl , scra , tch
   CHARACTER(6) , DIMENSION(2) , SAVE :: mxtb
   INTEGER , DIMENSION(2) , SAVE :: name
   CHARACTER(6) :: tbmx
   INTEGER , DIMENSION(9) , SAVE :: tchi
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
   !>>>>EQUIVALENCE (Scr,P1)
   DATA name/4HGINO , 4HFL  / , scra , tch/4HSCRA , 4H    /
!WKBR DATA    BLANK /  4H           /,  IZ2 / 2   /,OUTFL  / 201   /
   DATA iz2/2/ , outfl/201/
   DATA tchi/4HTCH1 , 4HTCH2 , 4HTCH3 , 4HTCH4 , 4HTCH5 , 4HTCH6 , 4HTCH7 , 4HTCH8 , 4HTCH9/
   DATA mxtb/'MATRIX' , 'TABLE '/ , debug/.FALSE./
!
!     CHECK SCRATCH FILE PARAMETER
!
   IF ( scr>300 .AND. scr<400 ) THEN
      IF ( scr>=310 ) THEN
         WRITE (nout,99001) ufm
99001    FORMAT (A23,', GINOFILE IS PROGRAMMED TO PROCESS ONLY THE FIRST ','9 SCRATCH FILES')
         CALL mesage(-61,0,0)
      ENDIF
!
!     SETUP CORE, BUFFERS, AND GINO OUTPUT FILE NAME
!
      kore = korsz(z(1))
      ibuf1 = kore - sysbuf - 1
      ibuf2 = ibuf1 - sysbuf
      kore = ibuf2 - 1
      CALL fname(outfl,fn)
!
!     RECAPTURE SCRATCH FILE NUMBER, TRAILER, AND INDEX POINTER IN FIAT
!     AND FIST
!     NOTE -
!     IT IS HERE THAT SCRATCH FILE IS LIMITED FROM 301 THRU 309
!     SCRATCH FILES 310 AND HIGHER MAY NOT HAVE UNIQUE 8-LETTER NAMES
!     IN ALL COMPUTERS.
!
      index = 0
      k = fiat(3)*icfiat - 2
      tch = tchi(scr-300)
      DO i = 4 , k , icfiat
         IF ( fiat(i+1)==scra .AND. fiat(i+2)==tch ) GOTO 50
      ENDDO
      WRITE (nout,99002) ufm , scr
99002 FORMAT (A23,', SCRATCH FILE',I4,' DOES NOT EXIST IN FIAT TABLE. ','THIS ERROR MAY DUE TO',/5X,                                &
             &'USER ERROR, OR GINOFILE WAS PRECEDED BY XSFA MODULE')
      CALL mesage(-37,0,name)
 50   index = i
      IF ( debug ) WRITE (6,99003) index
99003 FORMAT (5X,'INDEX =',I6)
      IF ( scr==301 ) THEN
!
!     IF SCRATCH FILE IS 301, THE TRAILER IN FIAT HAS BEEN INITIALIZED
!     TO ZEROS. MUST RECLAIM THE TRAILER FROM /XSORTX/, SAVED BY WRTTRL
!
!     THE LABEL COMMON /XSORTX/, DEFINED VIA SEMDBD AND AVAILABLE IN ALL
!     LINKS, IS ORIGNALLY USED ONLY BY XSORT2 ROUTINE WHICH WAS EXECUTED
!     IN EARLY LINK1. THUS IT IS SAFE TO SAVE THE SCRATCH 301 TRAILER IN
!     /XSORTX/. NOTE THE OTHER SCARTCH FILES 302 THRU 309 DO NOT HAVE
!     THIS PROBLEM
!
         fiat(index+3) = save(1)
         fiat(index+4) = save(2)
         fiat(index+5) = save(3)
         IF ( icfiat/=8 ) THEN
            fiat(index+8) = save(4)
            fiat(index+9) = save(5)
            fiat(index+10) = save(6)
         ENDIF
      ENDIF
!
!     LOCATE 301 IN FIST TABLE AND SWAP FIAT INDEX THAT POINTS TO THE
!     TARGET SCR FILE
!
      k = fist(2)*2 + 2
      DO i = 3 , k , 2
         IF ( fist(i)==301 ) GOTO 100
      ENDDO
      CALL mesage(-37,0,name)
 100  fisti = i
      fisti1 = fist(i+1)
      fist(i+1) = index - 1
      IF ( debug ) WRITE (6,99004) i , fist(i) , fist(i+1) , index
99004 FORMAT (10X,' I,FIST(I),FIST(I+1),INDEX =',4I6)
!
!     NOW, WE CAN READ THE SCRATCH FILE TRAILER
!
      trl(1) = 301
      CALL rdtrl(trl(1))
      trl(1) = scr
      tbmx = mxtb(2)
      IF ( trl(7)>0 ) tbmx = mxtb(1)
      WRITE (nout,99005) uim , tch , trl , tbmx , fn
99005 FORMAT (A29,' FROM GINOFILE MODULE',/5X,'TRAILER OF SCRA',A4,' FILE IN PREVIOUS MODULE = (',I3,1H),5I5,I8,/5X,A6,             &
             &' CONTENTS OF THIS FILE WILL BE TRANSFERRED TO GINO FILE ',2A4,/)
!
!     SWAP SCR AND SCRX (301) FILE
!     OPEN SCRS FILE, AND SKIP P2 RECORDS IF REQUESTED BY USER
!     (DEFAULT SKIP 1 HEADER RECORD IF IT EXISTS)
!
      trl2 = trl(2)
      file = scr
      scrx = 301
      CALL open(*1100,scrx,z(ibuf1),0)
      nwds = trl(5)
      IF ( nwds==3 ) nwds = 2
      nwds = trl(3)*nwds
      itypeu = trl(5)
      irowu = 1
      jrowu = trl(3)
      incru = 1
      itypep = itypeu
      jtypep = itypeu
      irowp = 1
      jrowp = trl(3)
      incrp = 1
      itrl(1) = outfl
      itrl(2) = 0
      itrl(3) = trl(3)
      itrl(4) = trl(4)
      itrl(5) = trl(5)
      itrl(6) = 0
      itrl(7) = 0
      CALL rectyp(scrx,irctyp)
      IF ( irctyp==0 ) THEN
         CALL read(*1000,*200,scrx,z,2,1,k)
      ELSE
         icrq = nwds - kore
         IF ( icrq<=0 ) GOTO 300
         CALL mesage(-8,scrx,name)
         CALL read(*1000,*200,scrx,z,2,1,k)
      ENDIF
   ELSE
      WRITE (nout,99006) uwm , scr
99006 FORMAT (A25,', SCRATCH FILE PARAMETER ERROR. GINOFILE ABORTED AND',' NO OUTPUT GINO FILE CREATED',/5X,'FIRST PARAMETER =',I5)
      GOTO 99999
   ENDIF
!WKBR  140 IF (Z(1).NE.SCRA .OR. Z(IZ2).NE.TCH) CALL BCKREC (SCRX,1)
 200  IF ( z(1)/=scra .OR. z(iz2)/=tch ) CALL bckrec(scrx)
 300  ncol = 0
   IF ( p3<=0 ) p3 = 999999
   IF ( p2>0 ) THEN
      DO ii = 1 , p2
         CALL fwdrec(*1000,scrx)
      ENDDO
   ENDIF
!
!     OPEN OUTPUT GINO FILE AND WRITE A HEADER RECORD
!
   file = outfl
   CALL open(*1100,outfl,z(ibuf2),1)
   CALL write(outfl,fn,2,1)
 400  CALL rectyp(scrx,irctyp)
   IF ( irctyp==0 ) THEN
      DO
!
!     COPY SCRATCH FILE DATA DIRECTLY TO OUTPUT FILE
!
         CALL read(*900,*700,scrx,z,kore,0,k)
         CALL write(outfl,z,kore,0)
      ENDDO
   ELSE
!
!     PROCESS STRING-FORMATED RECORD HERE
!
      CALL unpack(*500,scrx,z)
      GOTO 600
   ENDIF
 500  DO l = 1 , nwds
      z(l) = 0
   ENDDO
 600  CALL pack(z,outfl,itrl)
   GOTO 800
 700  CALL write(outfl,z,k,1)
 800  ncol = ncol + 1
   IF ( ncol<p3 ) GOTO 400
!
!     ALL DONE, CLOSE ALL FILES, WRITE TRAILER, AND ISSUE FRIENDLY
!     MESSAGES
!
 900  CALL close(scrx,1)
   CALL close(outfl,1)
   trl(1) = outfl
   trl(2) = ncol
   IF ( ncol>itrl(2) ) CALL wrttrl(trl)
   IF ( ncol==itrl(2) ) CALL wrttrl(itrl)
   WRITE (nout,99007) uim , tch , fn
99007 FORMAT (A29,', DATA TRANSFER FROM PREVIOUS SCRA',A4,' FILE TO ',2A4,' IS ACCOMPLISHED')
   IF ( p2>0 ) WRITE (nout,99008) tch , p2
99008 FORMAT (5X,'FIRST',I5,' RECORDS IN SCRA',A4,' FILE WERE SKIPPED ','BEFORE DATA TRANSFER')
   IF ( p3<999999 ) WRITE (nout,99009) p3
99009 FORMAT (5X,'LAST RECORD COPIED WAS RECORD NO.',I5)
   WRITE (nout,99010) fn , trl
99010 FORMAT (5X,'TRAILER OF THE NEW GINO FILE ',2A4,'  = (',I3,1H),5I5,I8)
!
!     IF SCRATCH FILE CONTAINS MATRIX DATA, CHECK NO. OF COLUMNS
!
   IF ( tbmx==mxtb(1) .AND. ncol/=trl2 .AND. (p2==0 .AND. p3==999999) ) WRITE (nout,99011) uim , tch , fn
99011 FORMAT (A29,', POSSIBLE ERROR IN GINOFILE WAS DETECTED',/5X,'NUMBERS OF COLUMNS IN INPUT FILE SCAR',A4,' AND OUTPUT FILE ',   &
            & 2A4,' DISAGREE',//)
!
!     RESET FIST ORIGINAL INDEX FOR SCRATCH FILE 301
!
   fist(fisti+1) = fisti1
   GOTO 99999
!
!     ERRORS
!
 1000 k = -2
   GOTO 1200
 1100 k = -1
 1200 CALL mesage(k,file,name)
!
99999 END SUBROUTINE ginofl
