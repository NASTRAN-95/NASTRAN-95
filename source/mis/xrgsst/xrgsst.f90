!*==xrgsst.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xrgsst(Newsol)
   IMPLICIT NONE
   USE c_phas11
   USE c_phas25
   USE c_phas28
   USE c_phas31
   USE c_phas37
   USE c_system
   USE c_xmssg
   USE c_xrgdxx
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Newsol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: begin , blank , delete , eight , end , five , ind11 , ind25 , ind28 , ind31 , ind37 , insert , one , seven
   INTEGER :: i , icflag , iflag , j , k , nmap
   INTEGER , DIMENSION(2) :: imap
   INTEGER , DIMENSION(5) , SAVE :: lflag
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     PURPOSE - XRGSST PROCESSES SUBSTRUCTURE CONTROLS CARDS IN
!               A RIGID FORMAT (I.E., THE ****PHS- CARDS)
!
!     AUTHOR  - RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       ARGUMENTS
!           NEWSOL     SOLUTION NUMBER
!       OTHER
!         /SYSTEM/
!           OPTAPE     UNIT NUMBER CONTAINING THE PRINT FILE
!         /XRGDXX/
!           ICHAR      CONTAINS THE CARD IMAGE IN 80A1 FORMAT
!           IDMAP      CURRENT DMAP SEQUENCE NUMBER
!           IPHASE     PHASE NUMBER
!           RECORD     CARD IMAGE IN 20A4 FORMAT
!
!     OUTPUT
!         /XRGDXX/
!           ICOL       COLUMN NUMBER LAST PROCESSED
!           IERROR     ERROR FLAG - NON-ZERO IF AN ERROR OCCURRED
!
!     LOCAL VARIABLES
!         BEGIN        CONTAINS THE VALUE 1HB
!         BLANK        CONTAINS THE VALUE 1H
!         DELETE       CONTAINS THE VALUE 1HD
!         EIGHT        CONTAINS THE VALUE 1H8
!         END          CONTAINS THE VALUE 1HE
!         FIVE         CONTAINS THE VALUE 1H5
!         ICFLAG       FLAG TO DISTINGUISH WHICH COMMON BLOCK IS
!                      BEING PROCESSED
!                      =1, /PHAS11/ ; =2, /PHAS25/ ; =3, /PHAS28/
!                      =4, /PHAS31/ ; =5, /PHAS37/
!         IFLAG        FLAG FOR THE KIND OF COMMAND BEING PROCESSED
!                      =1, FOR INSERT; =2, FOR DELETE;
!                      =3, FOR DELETE BEGIN; =4 FOR DELETE END
!         IMAP         2 WORD ARRAY FOR DMAP NUMBERS
!         IND11        INDEX FOR COMMON /PHAS11/
!         IND25        INDEX FOR COMMON /PHAS25/
!         IND28        INDEX FOR COMMON /PHAS28/
!         IND31        INDEX FOR COMMON /PHAS31/
!         IND37        INDEX FOR COMMON /PHAS37/
!         LFLAG        ARRAY USED FOR THE LAST FLAG (I.E., IFLAG)
!                      THAT WAS APPLIED TO A GIVEN COMMON - THIS
!                      IS USED TO CHECK FOR MATCHING 'DB' AND 'DE'
!                      SUBCOMMANDS
!         NMAP         NUMBER OF DMAP NUMBERS IN THE ARRAY IMAP
!         ONE          CONTAINS THE VALUE 1H1
!         SEVEN        CONTAINS THE VALUE 1H7
!
!     FUNCTIONS
!       XRGSST PROCESSES SUBSTRUCTURE CONTROL COMMANDS WITHIN THE
!       RIGID FORMAT.  THE COMMANDS ARE OF THE FOLLOWING FORMAT;
!       ****PHS- I=   (OR INSTEAD OF I=; D=, DB= OR DE= ) WHERE
!       THE '-' OF PHS IS THE PHASE NUMBER AND = REFERS TO THE
!       APPROPIATE ASCM== SUBROUTINE.  FOR THE I= SUBCOMMAND,
!       TWO NUMBERS ( N AND 0 ) ARE ADDED TO THE APPROPIATE
!       COMMON.  FOR THE D= SUBCOMMAND, TWO NUMBERS ( N1 AND N1 )
!       ARE ADDED TO THE APPROPIATE COMMON.  FOR THE DB=
!       SUBCOMMAND, ONE NUMBER IS ADDED TO THE COMMON AND
!       FOR THE DE= SUBCOMMAND, ONE NUMBER IS ADDED TO THE COMMON.
!       THE NUMBER THAT IS ADDED TO THE COMMONS
!       IS THE CURRENT DMAP SEQUENCE NUMBER AS FOUND IN THE
!       VARIABLE IDMAP.
!       THE I= COMMAND CORRESPONDS TO A DMAP ALTER INSERT
!       OF THE FORM ALTER N,0.  THE D= SUBCOMMAND CORRESPONDS
!       TO THE DMAP DELETE COMMAND  ALTER N1,N1.  THE DB=
!       SUBCOMMAND GIVES THE FIRST OF A RANGE OF DMAP NUMBERS
!       STATEMENTS TO BE DELETED AND THE DE= GIVES THE LAST
!       VALUE OF THE RANGE OF DMAP STATEMENTS TO BE DELETED.
!       THE COMMONS ARE NAMED PHAS== WHERE THE FIRST = REFERS
!       TO THE PHASE NUMBER AND THE SECOND = REFERS TO THE
!       APPROPIATE ASCM== SUBROUTINE.
!
!     SUBROUTINES CALLED - XDCODE
!
!     CALLING SUBROUTINES - XRGDFM
!
!     ERRORS
!       ERROR MESSAGES 8031,8032,8033,8035 ARE ISSUED
!
   DATA blank/1H / , delete/1HD/ , begin/1HB/
   DATA end/1HE/ , lflag/5*0/
   DATA one/1H1/ , five/1H5/ , seven/1H7/ , eight/1H8/
   DATA ind11/0/ , ind25/0/ , ind28/0/ , ind31/0/
   DATA ind37/0/ , insert/1HI/
!
   CALL xdcode
   icol = 9
   DO WHILE ( ichar(icol)==blank )
      IF ( icol>=80 ) GOTO 99999
      icol = icol + 1
   ENDDO
   IF ( ichar(icol)/=insert ) THEN
      IF ( ichar(icol)/=delete ) GOTO 100
      icol = icol + 1
      IF ( ichar(icol)==begin ) THEN
         iflag = 3
         nmap = 1
         imap(1) = idmap
         icol = icol + 1
      ELSEIF ( ichar(icol)==end ) THEN
         iflag = 4
         nmap = 1
         imap(1) = idmap
         icol = icol + 1
      ELSE
         iflag = 2
         nmap = 2
         imap(1) = idmap
         imap(2) = idmap
      ENDIF
   ELSE
      iflag = 1
      nmap = 2
      imap(1) = idmap
      imap(2) = 0
      icol = icol + 1
   ENDIF
   IF ( iphase==1 ) THEN
      IF ( ichar(icol)/=one ) GOTO 100
      icflag = 1
   ELSEIF ( iphase/=2 ) THEN
      IF ( ichar(icol)/=one ) THEN
         IF ( ichar(icol)/=seven ) GOTO 100
         icflag = 5
      ELSE
         icflag = 4
      ENDIF
   ELSEIF ( ichar(icol)/=five ) THEN
      IF ( ichar(icol)/=eight ) GOTO 100
      icflag = 3
   ELSE
      icflag = 2
   ENDIF
   IF ( iflag==4 .AND. lflag(icflag)/=3 ) THEN
      WRITE (optape,99001) ufm , record
99001 FORMAT (A23,' 8033, ',34H A 'DE' ENTRY HAS NO MATCHING 'DB',' ENTRY - ERROR ON CARD',//20X,20A4)
      GOTO 400
   ELSE
      IF ( iflag==3 .AND. lflag(icflag)==3 ) GOTO 300
      IF ( iflag<=2 .AND. lflag(icflag)==3 ) GOTO 300
      lflag(icflag) = iflag
      icol = icol + 1
      IF ( icflag==2 ) THEN
         IF ( ind25+nmap>14 ) GOTO 200
         DO k = 1 , nmap
            ind25 = ind25 + 1
            ipas25(ind25) = imap(k)
         ENDDO
         GOTO 99999
      ELSEIF ( icflag==3 ) THEN
         IF ( ind28+nmap>14 ) GOTO 200
         DO k = 1 , nmap
            ind28 = ind28 + 1
            ipas28(ind28) = imap(k)
         ENDDO
         GOTO 99999
      ELSEIF ( icflag==4 ) THEN
         IF ( ind31+nmap>2 ) GOTO 200
         DO k = 1 , nmap
            ind31 = ind31 + 1
            ipas31(ind31) = imap(k)
         ENDDO
         GOTO 99999
      ELSEIF ( icflag==5 ) THEN
         IF ( ind37+nmap>6 ) GOTO 200
         DO k = 1 , nmap
            ind37 = ind37 + 1
            ipas37(ind37) = imap(k)
         ENDDO
         GOTO 99999
      ELSE
         IF ( ind11+nmap>8 ) GOTO 200
         DO k = 1 , nmap
            ind11 = ind11 + 1
            ipas11(ind11) = imap(k)
         ENDDO
         GOTO 99999
      ENDIF
   ENDIF
!
!     ERRORS
!
 100  j = 0
   k = 1
   WRITE (optape,99002) ufm , icol , record , j , (i,i=1,8) , k , (j,i=1,8)
99002 FORMAT (A23,' 8031, INVALID PARAMETER NEAR COLUMN ',I3,' IN THE FOLLOWING CARD',//20X,20A4,/,(20X,I1,I9,7I10))
   ierror = 1
   GOTO 400
 200  WRITE (optape,99003) ufm , iphase , record
99003 FORMAT (A23,' 8032, ',19H' TOO MANY '****PHS,I1,9H' ENTRIES,' ERROR OCCURRED ON CARD',//20X,20A4)
   GOTO 400
 300  WRITE (optape,99004) ufm , record
99004 FORMAT (A23,' 8035, ',41H ATTEMP TO NEST 'DB'S OR NO MATCHING 'DE',' - ERROR OCCURRED ON THE FOLLOWING CARD',/20X,20A4)
 400  ierror = 1
99999 END SUBROUTINE xrgsst
