!*==dmpalt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
!*DECK,DMPALT
SUBROUTINE dmpalt(Isize,Iopen,Iptape)
   IMPLICIT NONE
   USE C_ALTRXX
   USE C_SYSTEM
   USE C_XRGDXX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Isize
   INTEGER , DIMENSION(1) :: Iopen
   INTEGER :: Iptape
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: ialter
   INTEGER , DIMENSION(18) :: icard
   INTEGER :: icheck , icount , idmap1 , idmap2 , iend , iflag , igoto , inumbr , ioccur , ioffst , ipoint , ireqd , itemp , j ,    &
            & jgoto , logic , n2dmap , nwords
   INTEGER , DIMENSION(2) , SAVE :: isubr , xalter
   INTEGER , SAVE :: oldalt
   EXTERNAL mesage , read , rewind , skpfil , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ipoint = 1
         CALL write(Iptape,xalter,2,1)
         IF ( Newalt==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n2dmap = 2*Nmdmap
         CALL skpfil(Altfil,1)
         CALL read(*240,*20,Altfil,Iopen,Isize,1,iend)
         ireqd = n2dmap - Isize
         CALL mesage(-8,ireqd,isubr)
 20      IF ( iend/=n2dmap ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ipoint = ipoint + iend
         CALL rewind(Altfil)
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL read(*99999,*40,Altfil,Iopen(ipoint),19,1,iflag)
         nwords = 19
         logic = 200
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         RETURN
!
 40      IF ( iflag==2 ) THEN
            logic = 300
            ASSIGN 60 TO jgoto
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
!
         ELSEIF ( iflag==4 ) THEN
            nwords = 4
            logic = 400
            IF ( Newalt==0 ) THEN
               WRITE (Nout,99001) nwords , logic
               CALL mesage(-61,0,0)
               RETURN
            ELSE
               logic = 410
               ASSIGN 80 TO jgoto
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
         ELSEIF ( iflag==5 ) THEN
            nwords = 5
            logic = 500
            IF ( Newalt==0 ) THEN
               WRITE (Nout,99001) nwords , logic
               CALL mesage(-61,0,0)
               RETURN
            ELSE
               logic = 510
               ASSIGN 120 TO jgoto
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
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
               RETURN
            ELSE
               CALL write(Iptape,Iopen(ipoint),18,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            nwords = 9
            logic = 600
            IF ( Newalt==0 ) THEN
               WRITE (Nout,99001) nwords , logic
               CALL mesage(-61,0,0)
               RETURN
            ELSE
               logic = 610
               ASSIGN 160 TO jgoto
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PROCESS ALTER CARDS HERE
!
 60      ialter(1) = Iopen(ipoint)
         ialter(2) = Iopen(ipoint+1)
         IF ( ialter(2)/=0 .AND. ialter(2)<ialter(1) ) THEN
            itemp = ialter(2)
            ialter(2) = ialter(1)
            ialter(1) = itemp
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS INSERT CARDS HERE
!
 80      idmap1 = Iopen(ipoint)
         idmap2 = Iopen(ipoint+1)
         ioccur = Iopen(ipoint+2)
         ioffst = Iopen(ipoint+3)
         ASSIGN 100 TO igoto
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     ialter(1) = inumbr
         ialter(2) = 0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS DELETE CARDS WITH ONE FIELD HERE
!
 120     idmap1 = Iopen(ipoint)
         idmap2 = Iopen(ipoint+1)
         ioccur = Iopen(ipoint+2)
         ioffst = Iopen(ipoint+3)
         icheck = Iopen(ipoint+4)
         logic = 520
         IF ( icheck/=0 ) THEN
            WRITE (Nout,99002) logic
            CALL mesage(-61,0,0)
            RETURN
         ELSE
            ASSIGN 140 TO igoto
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     ialter(1) = inumbr
         ialter(2) = inumbr
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS DELETE CARDS WITH TWO FIELDS HERE
!
 160     idmap1 = Iopen(ipoint)
         idmap2 = Iopen(ipoint+1)
         ioccur = Iopen(ipoint+2)
         ioffst = Iopen(ipoint+3)
         icheck = Iopen(ipoint+4)
         logic = 620
         IF ( icheck/=1 ) THEN
            WRITE (Nout,99002) logic
            CALL mesage(-61,0,0)
            RETURN
         ELSE
            ASSIGN 180 TO igoto
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 180     ialter(1) = inumbr
         idmap1 = Iopen(ipoint+5)
         idmap2 = Iopen(ipoint+6)
         ioccur = Iopen(ipoint+7)
         ioffst = Iopen(ipoint+8)
         ASSIGN 200 TO igoto
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 200     ialter(2) = inumbr
         IF ( ialter(2)/=0 .AND. ialter(2)<ialter(1) ) THEN
            itemp = ialter(2)
            ialter(2) = ialter(1)
            ialter(1) = itemp
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     WRITE ALTER CONTROL DATA ON THE NEW PROBLEM TAPE
!
         IF ( ialter(1)/=0 ) THEN
            IF ( ialter(1)>oldalt ) THEN
               IF ( ialter(2)==0 .OR. ialter(1)<=ialter(2) ) THEN
                  CALL write(Iptape,ialter,2,1)
                  oldalt = ialter(1)
                  IF ( ialter(2)/=0 ) oldalt = ialter(2)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            Nogo = 1
            WRITE (Nout,99003) icard
!***********************************************************************
99003       FORMAT ('0*** USER FATAL MESSAGE, THE DATA ON THE ','FOLLOWING ALTER CONTROL CARD IS NOT IN PROPER ',                   &
                   &'SEQUENCE OR ORDER --'//5X,18A4)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     INTERNAL SUBROUTINE TO READ IN AN ALTER CONTROL CARD IMAGE
!
         CALL read(*240,*220,Altfil,icard,19,1,iflag)
         nwords = 19
         WRITE (Nout,99001) nwords , logic
         CALL mesage(-61,0,0)
         RETURN
 220     GOTO jgoto
      CASE (5)
!
!     INTERNAL SUBROUTINE TO FIND THE DMAP STATEMENT NUMBER
!     FOR A DMAP STATEMENT WITH A GIVEN OCCURENCE FLAG AND
!     AN OFFSET FLAG
!
         icount = 0
         DO j = 1 , iend , 2
            IF ( idmap1==Iopen(j) .AND. idmap2==Iopen(j+1) ) THEN
               icount = icount + 1
               IF ( icount>=ioccur ) THEN
                  inumbr = (j+1)/2 + ioffst
                  IF ( inumbr<1 .OR. inumbr>Nmdmap ) THEN
                     Nogo = 1
                     inumbr = 0
                     WRITE (Nout,99004) icard
99004                FORMAT ('0*** USER FATAL MESSAGE, ILLEGAL OFFSET FLAG ','SPECIFIED ON THE FOLLOWING ALTER CONTROL CARD --'//5X,&
                           & 18A4)
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         Nogo = 1
         inumbr = 0
         IF ( icount>0 ) WRITE (Nout,99005) icard
99005    FORMAT ('0*** USER FATAL MESSAGE, ILLEGAL OCCURENCE FLAG ','SPECIFIED ON THE FOLLOWING ALTER CONTROL CARD --'//5X,18A4)
         IF ( icount==0 ) WRITE (Nout,99006) icard
99006    FORMAT ('0*** USER FATAL MESSAGE, NON-EXISTENT NOMINAL ','DMAP STATEMENT SPECIFIED ON THE FOLLOWING ',                     &
                &'ALTER CONTROL CARD --'//5X,18A4)
         spag_nextblock_1 = 6
      CASE (6)
         GOTO igoto
!
!     ERROR MESSAGES
!
 240     CALL mesage(-2,Altfil,isubr)
         spag_nextblock_1 = 7
      CASE (7)
         WRITE (Nout,99007) iend , n2dmap
99007    FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL NUMBER OF ','WORDS (',I5,') ENCOUNTERED IN THE SECOND ',                       &
                &'FILE OF THE ALTER SCRATCH FILE.'/'     EXPECTED NUMBER OF WORDS = ',I5)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL NUMBER OF ','WORDS (',I5,') ENCOUNTERED WHILE READING ',                          &
             &'A RECORD IN THE FIRST FILE OF THE ALTER SCRATCH ','FILE.'/'     LOGIC ERROR NO. = ',I5)
99002 FORMAT ('0*** SYSTEM FATAL MESSAGE, ILLEGAL CONTROL WORD ','WHILE PROCESSING THE FOLLOWING ALTER CONTROL CARD'//5X,18A4)
!
!***********************************************************************
!                              NOTICE                             *
!                              ------                             *
!                                                                 *
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED  *
!  A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES    *
!  WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.          *
!******************************************************************
99999 END SUBROUTINE dmpalt
