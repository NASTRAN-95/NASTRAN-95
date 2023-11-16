
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
!
! COMMON variable declarations
!
   INTEGER Fiat(3) , Fist(2) , Icfiat , Incrp , Incru , Irowp , Irowu , Itypep , Itypeu , Jrowp , Jrowu , Jtypep , Nout , P1 , P2 , &
         & P3 , Save(6) , Scr , Skip(21) , Sysbuf , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / P1 , P2 , P3
   COMMON /packx / Itypep , Jtypep , Irowp , Jrowp , Incrp
   COMMON /system/ Sysbuf , Nout , Skip , Icfiat
   COMMON /unpakx/ Itypeu , Irowu , Jrowu , Incru
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xsortx/ Save
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   LOGICAL debug
   INTEGER file , fisti , fisti1 , fn(2) , i , ibuf1 , ibuf2 , icrq , ii , index , irctyp , itrl(7) , iz2 , k , kore , l , name(2) ,&
         & ncol , nwds , outfl , scra , scrx , tch , tchi(9) , trl(7) , trl2
   INTEGER korsz
   CHARACTER*6 mxtb(2) , tbmx
!
! End of declarations
!
!
   EQUIVALENCE (Scr,P1)
   DATA name/4HGINO , 4HFL  / , scra , tch/4HSCRA , 4H    /
!WKBR DATA    BLANK /  4H           /,  IZ2 / 2   /,OUTFL  / 201   /
   DATA iz2/2/ , outfl/201/
   DATA tchi/4HTCH1 , 4HTCH2 , 4HTCH3 , 4HTCH4 , 4HTCH5 , 4HTCH6 , 4HTCH7 , 4HTCH8 , 4HTCH9/
   DATA mxtb/'MATRIX' , 'TABLE '/ , debug/.FALSE./
!
!     CHECK SCRATCH FILE PARAMETER
!
   IF ( Scr>300 .AND. Scr<400 ) THEN
      IF ( Scr>=310 ) THEN
         WRITE (Nout,99001) Ufm
99001    FORMAT (A23,', GINOFILE IS PROGRAMMED TO PROCESS ONLY THE FIRST ','9 SCRATCH FILES')
         CALL mesage(-61,0,0)
      ENDIF
!
!     SETUP CORE, BUFFERS, AND GINO OUTPUT FILE NAME
!
      kore = korsz(Z(1))
      ibuf1 = kore - Sysbuf - 1
      ibuf2 = ibuf1 - Sysbuf
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
      k = Fiat(3)*Icfiat - 2
      tch = tchi(Scr-300)
      DO i = 4 , k , Icfiat
         IF ( Fiat(i+1)==scra .AND. Fiat(i+2)==tch ) GOTO 50
      ENDDO
      WRITE (Nout,99002) Ufm , Scr
99002 FORMAT (A23,', SCRATCH FILE',I4,' DOES NOT EXIST IN FIAT TABLE. ','THIS ERROR MAY DUE TO',/5X,                                &
             &'USER ERROR, OR GINOFILE WAS PRECEDED BY XSFA MODULE')
      CALL mesage(-37,0,name)
 50   index = i
      IF ( debug ) WRITE (6,99003) index
99003 FORMAT (5X,'INDEX =',I6)
      IF ( Scr==301 ) THEN
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
         Fiat(index+3) = Save(1)
         Fiat(index+4) = Save(2)
         Fiat(index+5) = Save(3)
         IF ( Icfiat/=8 ) THEN
            Fiat(index+8) = Save(4)
            Fiat(index+9) = Save(5)
            Fiat(index+10) = Save(6)
         ENDIF
      ENDIF
!
!     LOCATE 301 IN FIST TABLE AND SWAP FIAT INDEX THAT POINTS TO THE
!     TARGET SCR FILE
!
      k = Fist(2)*2 + 2
      DO i = 3 , k , 2
         IF ( Fist(i)==301 ) GOTO 100
      ENDDO
      CALL mesage(-37,0,name)
 100  fisti = i
      fisti1 = Fist(i+1)
      Fist(i+1) = index - 1
      IF ( debug ) WRITE (6,99004) i , Fist(i) , Fist(i+1) , index
99004 FORMAT (10X,' I,FIST(I),FIST(I+1),INDEX =',4I6)
!
!     NOW, WE CAN READ THE SCRATCH FILE TRAILER
!
      trl(1) = 301
      CALL rdtrl(trl(1))
      trl(1) = Scr
      tbmx = mxtb(2)
      IF ( trl(7)>0 ) tbmx = mxtb(1)
      WRITE (Nout,99005) Uim , tch , trl , tbmx , fn
99005 FORMAT (A29,' FROM GINOFILE MODULE',/5X,'TRAILER OF SCRA',A4,' FILE IN PREVIOUS MODULE = (',I3,1H),5I5,I8,/5X,A6,             &
             &' CONTENTS OF THIS FILE WILL BE TRANSFERRED TO GINO FILE ',2A4,/)
!
!     SWAP SCR AND SCRX (301) FILE
!     OPEN SCRS FILE, AND SKIP P2 RECORDS IF REQUESTED BY USER
!     (DEFAULT SKIP 1 HEADER RECORD IF IT EXISTS)
!
      trl2 = trl(2)
      file = Scr
      scrx = 301
      CALL open(*1100,scrx,Z(ibuf1),0)
      nwds = trl(5)
      IF ( nwds==3 ) nwds = 2
      nwds = trl(3)*nwds
      Itypeu = trl(5)
      Irowu = 1
      Jrowu = trl(3)
      Incru = 1
      Itypep = Itypeu
      Jtypep = Itypeu
      Irowp = 1
      Jrowp = trl(3)
      Incrp = 1
      itrl(1) = outfl
      itrl(2) = 0
      itrl(3) = trl(3)
      itrl(4) = trl(4)
      itrl(5) = trl(5)
      itrl(6) = 0
      itrl(7) = 0
      CALL rectyp(scrx,irctyp)
      IF ( irctyp==0 ) THEN
         CALL read(*1000,*200,scrx,Z,2,1,k)
      ELSE
         icrq = nwds - kore
         IF ( icrq<=0 ) GOTO 300
         CALL mesage(-8,scrx,name)
         CALL read(*1000,*200,scrx,Z,2,1,k)
      ENDIF
   ELSE
      WRITE (Nout,99006) Uwm , Scr
99006 FORMAT (A25,', SCRATCH FILE PARAMETER ERROR. GINOFILE ABORTED AND',' NO OUTPUT GINO FILE CREATED',/5X,'FIRST PARAMETER =',I5)
      GOTO 99999
   ENDIF
!WKBR  140 IF (Z(1).NE.SCRA .OR. Z(IZ2).NE.TCH) CALL BCKREC (SCRX,1)
 200  IF ( Z(1)/=scra .OR. Z(iz2)/=tch ) CALL bckrec(scrx)
 300  ncol = 0
   IF ( P3<=0 ) P3 = 999999
   IF ( P2>0 ) THEN
      DO ii = 1 , P2
         CALL fwdrec(*1000,scrx)
      ENDDO
   ENDIF
!
!     OPEN OUTPUT GINO FILE AND WRITE A HEADER RECORD
!
   file = outfl
   CALL open(*1100,outfl,Z(ibuf2),1)
   CALL write(outfl,fn,2,1)
 400  CALL rectyp(scrx,irctyp)
   IF ( irctyp==0 ) THEN
      DO
!
!     COPY SCRATCH FILE DATA DIRECTLY TO OUTPUT FILE
!
         CALL read(*900,*700,scrx,Z,kore,0,k)
         CALL write(outfl,Z,kore,0)
      ENDDO
   ELSE
!
!     PROCESS STRING-FORMATED RECORD HERE
!
      CALL unpack(*500,scrx,Z)
      GOTO 600
   ENDIF
 500  DO l = 1 , nwds
      Z(l) = 0
   ENDDO
 600  CALL pack(Z,outfl,itrl)
   GOTO 800
 700  CALL write(outfl,Z,k,1)
 800  ncol = ncol + 1
   IF ( ncol<P3 ) GOTO 400
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
   WRITE (Nout,99007) Uim , tch , fn
99007 FORMAT (A29,', DATA TRANSFER FROM PREVIOUS SCRA',A4,' FILE TO ',2A4,' IS ACCOMPLISHED')
   IF ( P2>0 ) WRITE (Nout,99008) tch , P2
99008 FORMAT (5X,'FIRST',I5,' RECORDS IN SCRA',A4,' FILE WERE SKIPPED ','BEFORE DATA TRANSFER')
   IF ( P3<999999 ) WRITE (Nout,99009) P3
99009 FORMAT (5X,'LAST RECORD COPIED WAS RECORD NO.',I5)
   WRITE (Nout,99010) fn , trl
99010 FORMAT (5X,'TRAILER OF THE NEW GINO FILE ',2A4,'  = (',I3,1H),5I5,I8)
!
!     IF SCRATCH FILE CONTAINS MATRIX DATA, CHECK NO. OF COLUMNS
!
   IF ( tbmx==mxtb(1) .AND. ncol/=trl2 .AND. (P2==0 .AND. P3==999999) ) WRITE (Nout,99011) Uim , tch , fn
99011 FORMAT (A29,', POSSIBLE ERROR IN GINOFILE WAS DETECTED',/5X,'NUMBERS OF COLUMNS IN INPUT FILE SCAR',A4,' AND OUTPUT FILE ',   &
            & 2A4,' DISAGREE',//)
!
!     RESET FIST ORIGINAL INDEX FOR SCRATCH FILE 301
!
   Fist(fisti+1) = fisti1
   GOTO 99999
!
!     ERRORS
!
 1000 k = -2
   GOTO 1200
 1100 k = -1
 1200 CALL mesage(k,file,name)
!
99999 RETURN
END SUBROUTINE ginofl
