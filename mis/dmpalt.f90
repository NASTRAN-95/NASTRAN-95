
!*DECK,DMPALT
SUBROUTINE dmpalt(Isize,Iopen,Iptape)
   IMPLICIT NONE
   INTEGER Altfil , Idum(115) , Isysbf , Newalt , Nmdmap , Nogo , Nout
   COMMON /altrxx/ Altfil , Newalt , Nogo
   COMMON /system/ Isysbf , Nout
   COMMON /xrgdxx/ Idum , Nmdmap
   INTEGER Iptape , Isize
   INTEGER Iopen(1)
   INTEGER ialter(2) , icard(18) , icheck , icount , idmap1 , idmap2 , iend , iflag , igoto , inumbr , ioccur , ioffst , ipoint ,   &
         & ireqd , isubr(2) , itemp , j , jgoto , logic , n2dmap , nwords , oldalt , xalter(2)
!******************************************************************
!                              NOTICE                             *
!                              ------                             *
!                                                                 *
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED  *
!  A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES    *
!  WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.          *
!******************************************************************
!
!
!
!
   DATA oldalt/0/
   DATA isubr/4HDMPA , 4HLT  /
   DATA xalter/4HXALT , 4HER  /
!
   ipoint = 1
   CALL write(Iptape,xalter,2,1)
   IF ( Newalt==0 ) GOTO 200
   n2dmap = 2*Nmdmap
   CALL skpfil(Altfil,1)
   CALL read(*1700,*100,Altfil,Iopen,Isize,1,iend)
   ireqd = n2dmap - Isize
   CALL mesage(-8,ireqd,isubr)
 100  IF ( iend/=n2dmap ) GOTO 1800
   ipoint = ipoint + iend
   CALL rewind(Altfil)
!
 200  CALL read(*1900,*300,Altfil,Iopen(ipoint),19,1,iflag)
   nwords = 19
   logic = 200
   WRITE (Nout,99001) nwords , logic
99001 FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL NUMBER OF ','WORDS (',I5,') ENCOUNTERED WHILE READING ',                          &
             &'A RECORD IN THE FIRST FILE OF THE ALTER SCRATCH ','FILE.'/'     LOGIC ERROR NO. = ',I5)
   CALL mesage(-61,0,0)
   GOTO 1900
!
 300  IF ( iflag==2 ) THEN
      logic = 300
      ASSIGN 400 TO jgoto
      GOTO 1300
!
   ELSEIF ( iflag==4 ) THEN
      nwords = 4
      logic = 400
      IF ( Newalt==0 ) THEN
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         GOTO 1900
      ELSE
         logic = 410
         ASSIGN 500 TO jgoto
         GOTO 1300
      ENDIF
!
   ELSEIF ( iflag==5 ) THEN
      nwords = 5
      logic = 500
      IF ( Newalt==0 ) THEN
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         GOTO 1900
      ELSE
         logic = 510
         ASSIGN 700 TO jgoto
         GOTO 1300
      ENDIF
!
   ELSEIF ( iflag/=9 ) THEN
!
!     PROCESS DMAP STATEMENTS HERE
!
      nwords = iflag
      logic = 700
      IF ( iflag/=18 ) THEN
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         GOTO 1900
      ELSE
         CALL write(Iptape,Iopen(ipoint),18,1)
         GOTO 200
      ENDIF
   ELSE
      nwords = 9
      logic = 600
      IF ( Newalt==0 ) THEN
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         GOTO 1900
      ELSE
         logic = 610
         ASSIGN 900 TO jgoto
         GOTO 1300
      ENDIF
   ENDIF
!
!     PROCESS ALTER CARDS HERE
!
 400  ialter(1) = Iopen(ipoint)
   ialter(2) = Iopen(ipoint+1)
   IF ( ialter(2)/=0 .AND. ialter(2)<ialter(1) ) THEN
      itemp = ialter(2)
      ialter(2) = ialter(1)
      ialter(1) = itemp
   ENDIF
   GOTO 1200
!
!     PROCESS INSERT CARDS HERE
!
 500  idmap1 = Iopen(ipoint)
   idmap2 = Iopen(ipoint+1)
   ioccur = Iopen(ipoint+2)
   ioffst = Iopen(ipoint+3)
   ASSIGN 600 TO igoto
   GOTO 1500
 600  ialter(1) = inumbr
   ialter(2) = 0
   GOTO 1200
!
!     PROCESS DELETE CARDS WITH ONE FIELD HERE
!
 700  idmap1 = Iopen(ipoint)
   idmap2 = Iopen(ipoint+1)
   ioccur = Iopen(ipoint+2)
   ioffst = Iopen(ipoint+3)
   icheck = Iopen(ipoint+4)
   logic = 520
   IF ( icheck/=0 ) THEN
      WRITE (Nout,99002) logic
99002 FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL CONTROL WORD ','WHILE PROCESSING THE FOLLOWING ALTER CONTROL CARD'//5X,18A4)
      CALL mesage(-61,0,0)
      GOTO 1900
   ELSE
      ASSIGN 800 TO igoto
      GOTO 1500
   ENDIF
 800  ialter(1) = inumbr
   ialter(2) = inumbr
   GOTO 1200
!
!     PROCESS DELETE CARDS WITH TWO FIELDS HERE
!
 900  idmap1 = Iopen(ipoint)
   idmap2 = Iopen(ipoint+1)
   ioccur = Iopen(ipoint+2)
   ioffst = Iopen(ipoint+3)
   icheck = Iopen(ipoint+4)
   logic = 620
   IF ( icheck/=1 ) THEN
      WRITE (Nout,99002) logic
      CALL mesage(-61,0,0)
      GOTO 1900
   ELSE
      ASSIGN 1000 TO igoto
      GOTO 1500
   ENDIF
 1000 ialter(1) = inumbr
   idmap1 = Iopen(ipoint+5)
   idmap2 = Iopen(ipoint+6)
   ioccur = Iopen(ipoint+7)
   ioffst = Iopen(ipoint+8)
   ASSIGN 1100 TO igoto
   GOTO 1500
 1100 ialter(2) = inumbr
   IF ( ialter(2)/=0 .AND. ialter(2)<ialter(1) ) THEN
      itemp = ialter(2)
      ialter(2) = ialter(1)
      ialter(1) = itemp
   ENDIF
!
!     WRITE ALTER CONTROL DATA ON THE NEW PROBLEM TAPE
!
 1200 IF ( ialter(1)/=0 ) THEN
      IF ( ialter(1)>oldalt ) THEN
         IF ( ialter(2)==0 .OR. ialter(1)<=ialter(2) ) THEN
            CALL write(Iptape,ialter,2,1)
            oldalt = ialter(1)
            IF ( ialter(2)/=0 ) oldalt = ialter(2)
            GOTO 200
         ENDIF
      ENDIF
      Nogo = 1
      WRITE (Nout,99003) icard
!***********************************************************************
99003 FORMAT ('0*** USER FATAL MESSAGE, THE DATA ON THE ','FOLLOWING ALTER CONTROL CARD IS NOT IN PROPER ','SEQUENCE OR ORDER --'// &
            & 5X,18A4)
   ENDIF
   GOTO 200
!
!     INTERNAL SUBROUTINE TO READ IN AN ALTER CONTROL CARD IMAGE
!
 1300 CALL read(*1700,*1400,Altfil,icard,19,1,iflag)
   nwords = 19
   WRITE (Nout,99001) nwords , logic
   CALL mesage(-61,0,0)
   GOTO 1900
 1400 GOTO jgoto
!
!     INTERNAL SUBROUTINE TO FIND THE DMAP STATEMENT NUMBER
!     FOR A DMAP STATEMENT WITH A GIVEN OCCURENCE FLAG AND
!     AN OFFSET FLAG
!
 1500 icount = 0
   DO j = 1 , iend , 2
      IF ( idmap1==Iopen(j) .AND. idmap2==Iopen(j+1) ) THEN
         icount = icount + 1
         IF ( icount>=ioccur ) THEN
            inumbr = (j+1)/2 + ioffst
            IF ( inumbr<1 .OR. inumbr>Nmdmap ) THEN
               Nogo = 1
               inumbr = 0
               WRITE (Nout,99004) icard
99004          FORMAT ('0*** USER FATAL MESSAGE, ILLEGAL OFFSET FLAG ','SPECIFIED ON THE FOLLOWING ALTER CONTROL CARD --'//5X,18A4)
            ENDIF
            GOTO 1600
         ENDIF
      ENDIF
   ENDDO
   Nogo = 1
   inumbr = 0
   IF ( icount>0 ) WRITE (Nout,99005) icard
99005 FORMAT ('0*** USER FATAL MESSAGE, ILLEGAL OCCURENCE FLAG ','SPECIFIED ON THE FOLLOWING ALTER CONTROL CARD --'//5X,18A4)
   IF ( icount==0 ) WRITE (Nout,99006) icard
99006 FORMAT ('0*** USER FATAL MESSAGE, NON-EXISTENT NOMINAL ','DMAP STATEMENT SPECIFIED ON THE FOLLOWING ',                        &
            & 'ALTER CONTROL CARD --'//5X,18A4)
 1600 GOTO igoto
!
!     ERROR MESSAGES
!
 1700 CALL mesage(-2,Altfil,isubr)
 1800 WRITE (Nout,99007) iend , n2dmap
99007 FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL NUMBER OF ','WORDS (',I5,') ENCOUNTERED IN THE SECOND ',                          &
             &'FILE OF THE ALTER SCRATCH FILE.'/'     EXPECTED NUMBER OF WORDS = ',I5)
   CALL mesage(-61,0,0)
!
 1900 RETURN
!***********************************************************************
!                              NOTICE                             *
!                              ------                             *
!                                                                 *
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED  *
!  A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES    *
!  WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.          *
!******************************************************************
END SUBROUTINE dmpalt