
SUBROUTINE case
   IMPLICIT NONE
   INTEGER App(2) , Count , Loop , Sysbuf , Z(1)
   REAL Clsrew , Rd , Rdrew , Wrt , Wrtrew
   COMMON /blank / App , Count , Loop
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER branch , buf(20) , buf1 , buf2 , buf3 , casecc , casexx , error(2) , file , i , ib2pp , ifreq , ifrqst , ik2pp , ilf ,   &
         & iload , ils , im2pp , imeth , imrqst , iocnt , irand , irset , iry , itfl , j , m8 , mcb(7) , n , nam(2) , ncc , nogo ,  &
         & nrigds , psdl , rfmts(40)
   INTEGER korsz
!
!     CASE READS THE CASE CONTROL DATA BLOCK AND WRITES A NEW
!     DATA BLOCK WHICH CONTAINS ONLY THOSE RECORDS WHICH DESCRIBE THE
!     CURRENT CASE IN THE LOOP. ADDITIONALLY, THE LOOP CONTROL PARAMETER
!     IS SET.
!
!
!
!     DATA DESCRIBING DATA BLOCK FILE NAMES AND POSITION
!     OF PARAMETERS IN THE CASE CONTROL RECORD.
!
   DATA casecc/101/ , casexx/201/ , ik2pp/139/ , im2pp/141/ , ib2pp/143/ , itfl/15/ , psdl/102/ , irand/163/
   DATA error/4HPSDL , 4HCASE/
   DATA ifreq/14/ , imeth/5/
!
!     DATA DEFINING RIGID FORMATS.
!
   DATA nrigds/10/ , rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!
!     MISC DATA
!
   DATA nam/4HCASE , 4H    / , mcb/7*0/
!
!     PERFORM BUFFER ALLOCATION.
!
   buf1 = korsz(Z) - Sysbuf + 1
   buf3 = buf1 - Sysbuf
   buf2 = buf3 - Sysbuf
   iry = 0
   m8 = -8
   IF ( Count<=0 ) Count = 1
   Loop = 1
   iocnt = Count
!
!     SET PARAMETER FOR APPROACH.
!
   n = 2*nrigds - 1
   DO i = 1 , n , 2
      IF ( rfmts(i)==App(1) ) GOTO 100
   ENDDO
   CALL mesage(30,75,App)
   i = 19
 100  branch = (i+1)/2
!
!     OPEN CASECC. SKIP RECORDS ALREADY PROCESSED. OPEN CASEXX.
!     WRITE HEADER RECORD. THEN BRANCH ON APPROACH.
!
   file = casecc
   CALL open(*1600,casecc,Z(buf1),Rdrew)
   DO i = 1 , Count
      CALL fwdrec(*1700,casecc)
   ENDDO
   file = casexx
   CALL open(*1600,casexx,Z(buf2),Wrtrew)
   CALL fname(casexx,buf)
   CALL write(casexx,buf,2,1)
   IF ( branch==1 .OR. branch==3 .OR. branch==4 .OR. branch==7 .OR. branch==8 .OR. branch==10 ) GOTO 1500
   IF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
      CALL read(*1700,*1400,casecc,Z,buf2,1,ncc)
      CALL mesage(m8,0,nam)
      GOTO 1400
   ELSE
!
!     COMPLEX EIGENVALUES OR FREQUENCY RESPONSE.
!
      CALL read(*1700,*200,casecc,Z,buf2,1,ncc)
      CALL mesage(m8,0,nam)
   ENDIF
 200  buf(1) = Z(ik2pp)
   buf(2) = Z(ik2pp+1)
   buf(3) = Z(im2pp)
   buf(4) = Z(im2pp+1)
   buf(5) = Z(ib2pp)
   buf(6) = Z(ib2pp+1)
   buf(7) = Z(itfl)
   irset = Z(irand)
   ifrqst = Z(ifreq)
   imrqst = Z(imeth)
   IF ( branch==5 .AND. irset/=0 ) iry = 1
   IF ( iry==0 ) GOTO 900
!
!     BUILD LIST OF UNIQUE LOAD ID-S
!
   file = psdl
   CALL open(*800,psdl,Z(buf3),Rdrew)
   CALL fwdrec(*1300,psdl)
   ils = buf2
   ilf = buf2 - 1
 300  DO
      CALL read(*1300,*700,psdl,Z(ncc+1),6,0,j)
      IF ( Z(ncc+1)==irset ) THEN
         j = 1
         iload = Z(ncc+2)
         IF ( ils/=ilf+1 ) EXIT
         GOTO 500
      ENDIF
   ENDDO
 400  DO i = ils , ilf
      IF ( Z(i)==iload ) GOTO 600
   ENDDO
!
!     NEW LOAD ID
!
 500  ils = ils - 1
   Z(ils) = iload
 600  IF ( j==0 ) GOTO 300
   j = 0
   iload = Z(ncc+3)
   GOTO 400
!
!     END OF PSDL RECORD
!
 700  CALL close(psdl,Clsrew)
   IF ( ils==ilf+1 ) CALL mesage(-31,irset,error(1))
   buf2 = ils - 1
   GOTO 900
!
!     NO PSDL IS EQUIVALENT TO NO RANDOM
!
 800  iry = 0
 900  CALL write(casexx,Z,ncc,1)
   Count = Count + 1
   IF ( iry/=0 ) THEN
!
!     CHECK  SUBCASE ID-S
!
      DO i = ils , ilf
         IF ( Z(1)==Z(i) ) GOTO 1000
      ENDDO
   ENDIF
   GOTO 1100
!
!     MARK USED
!
 1000 Z(i) = -Z(i)
 1100 CALL read(*1300,*1200,casecc,Z,buf2,1,ncc)
   CALL mesage(m8,0,nam)
 1200 IF ( Z(ik2pp)/=buf(1) .OR. Z(ik2pp+1)/=buf(2) .OR. Z(im2pp)/=buf(3) .OR. Z(im2pp+1)/=buf(4) .OR. Z(ib2pp)/=buf(5) .OR.        &
         & Z(ib2pp+1)/=buf(6) ) GOTO 1500
   IF ( Z(itfl)/=buf(7) ) GOTO 1500
   IF ( Z(imeth)/=0 .AND. Z(imeth)/=imrqst ) GOTO 1500
!
!     TEST FOR CHANGED FREQUENCY SET
!
   IF ( Z(ifreq)==ifrqst .OR. branch/=5 ) GOTO 900
   GOTO 1500
 1300 Count = -1
   GOTO 1500
 1400 CALL write(casexx,Z,ncc,1)
   Count = Count + 1
   CALL read(*1300,*1500,casecc,Z,buf2,1,ncc)
!
!     CLOSE FILES. WRITE TRAILER. RETURN.
!
 1500 CALL close(casecc,Clsrew)
   CALL close(casexx,Clsrew)
   mcb(1) = casexx
   mcb(2) = Count
   CALL wrttrl(mcb)
   IF ( Count<=1 .AND. iocnt==1 ) Loop = -1
!
!     CHECK ALL PSDL ACCOUNTED FOR
!
   IF ( iry/=0 ) THEN
      nogo = 0
      DO i = ils , ilf
         IF ( Z(i)>=0 ) THEN
            nogo = -1
            CALL mesage(33,Z(i),nam)
         ENDIF
      ENDDO
      IF ( nogo<0 ) CALL mesage(-7,0,nam)
   ENDIF
   RETURN
!
!     FATAL FILE ERRORS.
!
 1600 n = -1
   GOTO 1800
 1700 n = -2
   file = casecc
 1800 DO
      CALL mesage(n,file,nam)
   ENDDO
END SUBROUTINE case