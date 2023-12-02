!*==setinp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE setinp
!
   USE c_blank
   USE c_gpta1
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , blnk , elem , eor , exce , excl , go , grid , ilxx , incl , inprew , norew , outrew , poin , rew , set , &
                   & stop , thru , to
   INTEGER :: allon , b1 , b2 , b3 , b4 , elgp , encard , i , icont , idx , ie , iorew , iwrd , j , jc , mode , n , nelx , ngpx ,   &
            & nt , nw , org , porg , setid , tra , word , xx
   INTEGER , DIMENSION(2) :: awrd
   INTEGER , DIMENSION(65) :: card
   REAL(REAL64) :: dwrd
   INTEGER , DIMENSION(1) :: el , gp
   REAL :: fwrd
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(20) :: pcard
   INTEGER , DIMENSION(200) :: pocard
   INTEGER , DIMENSION(100) :: typ
   EXTERNAL bckrec , close , clstab , complf , delset , fread , gopen , ifp1pc , korsz , mesage , open , rdmode , rdmodx , rdword , &
          & read , rshift , write , xrcard , xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (X(1),El(1),Gp(1))
   !>>>>EQUIVALENCE (word,awrd(1),iwrd,fwrd,dwrd)
   DATA inprew , outrew , rew , norew , eor/0 , 1 , 1 , 3 , 1000000/
   DATA blnk , stop , go , name/4H     , 4HSTOP , 4HGO   , 4H SET , 3HINP/
   DATA set , incl , excl , elem , grid , poin , exce , to/3HSET , 4HINCL , 4HEXCL , 4HELEM , 4HGRID , 4HPOIN , 4HEXCE , 2HTO/
   DATA thru , all , ilxx/4HTHRU , 3HALL , 2HXX/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL delset
         b1 = korsz(x) - 5*bufsiz + 1
         b2 = b1 + bufsiz
         b3 = b2 + bufsiz
         b4 = b3 + bufsiz
         nogo = 0
         org = 0
         porg = -1
         allon = complf(0)
         pocard(200) = rshift(allon,1)
         encard = pocard(200)
!
!     OPEN ALL NECESSARY FILES
!
         iorew = inprew
         IF ( intr>0 ) THEN
            pcdb = ipcdb
            iorew = outrew
         ENDIF
         CALL open(*360,pcdb,x(b1),iorew)
         IF ( intr<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         WRITE (nout,99001)
!
99001    FORMAT (' ENTER PLOT DEFINITION OR ''GO'' IF DONE.')
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            DO j = 1 , 20
               pcard(j) = blnk
            ENDDO
            DO j = 1 , 199
               pocard(j) = blnk
            ENDDO
            CALL xread(*20,pcard)
            IF ( pcard(1)==stop ) THEN
               nogo = 1
               RETURN
            ELSEIF ( pcard(1)==go ) THEN
               CALL close(pcdb,rew)
               IF ( intr>10 ) nout = 1
               CALL open(*360,pcdb,x(b1),inprew)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL xrcard(pocard,199,pcard)
               CALL ifp1pc(1,icont,pocard,org,porg)
               IF ( nogo==0 ) THEN
                  WRITE (1,99002) pcard
99002             FORMAT (20A4)
                  ie = 1
                  DO j = 1 , 199
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           IF ( pocard(j)==0 ) THEN
                              DO jc = 1 , 5
                                 IF ( pocard(j+jc)/=blnk ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                              ENDDO
                              nw = j
                              GOTO 2
                           ENDIF
                           spag_nextblock_2 = 2
                        CASE (2)
                           IF ( pocard(j)==encard ) THEN
                              nw = j
                              GOTO 2
                           ENDIF
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
                  ENDDO
                  nw = 80
 2                CALL write(pcdb,pocard,nw,ie)
               ELSE
                  nogo = 0
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
 20      WRITE (nout,99003)
99003    FORMAT (' BAD CARD TRY AGIAN')
         spag_nextblock_1 = 2
      CASE (3)
         IF ( intr<=0 ) CALL fread(pcdb,0,-2,1)
         CALL gopen(plot,x(b2),outrew)
         CALL gopen(mset,x(b3),outrew)
         CALL gopen(msetid,x(b4),outrew)
         CALL rdmodx(pcdb,mode,word)
 40      SPAG_Loop_1_2: DO
!
!     READ MODE FLAG.  SHOULD BE ALPHABETIC
!
            CALL read(*340,*340,pcdb,mode,1,0,i)
            IF ( mode<0 ) THEN
               i = 1
               IF ( mode==-4 ) i = 2
               CALL fread(pcdb,0,-i,0)
            ELSEIF ( mode/=0 ) THEN
               IF ( mode<eor ) THEN
                  mode = mode + 1
                  CALL rdword(mode,word)
                  CALL rdword(mode,word)
                  IF ( word/=set .OR. mode/=0 ) EXIT SPAG_Loop_1_2
!
!     THIS CARD DEFINES A NEW SET
!
                  ASSIGN 100 TO tra
                  CALL rdmode(*380,*60,*40,mode,word)
                  GOTO 100
               ELSE
                  CALL fread(pcdb,0,0,1)
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
!
!     THIS CARD IS A PLOT CONTROL CARD
!
 60      CALL bckrec(pcdb)
         spag_nextblock_1 = 4
      CASE (4)
         CALL read(*340,*80,pcdb,card,65,1,i)
         WRITE (nout,99004)
99004    FORMAT ('  ARRAY CARD OF 65 TOO SAMLL')
         CALL mesage(-37,0,name)
 80      CALL write(plot,card,i,1)
         IF ( card(i)/=0 ) GOTO 40
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     setid = iwrd
         nelx = 0
         ngpx = b1
         nt = 0
         xx = 1
         elgp = 0
!
         IF ( mode<=0 ) CALL rdmode(*220,*120,*320,mode,word)
 120     CALL rdword(mode,word)
!
!     CHECK FOR AN -INCLUDE- OR -EXCLUDE- CARD
!
 140     IF ( word/=incl .AND. word/=excl .AND. word/=exce ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( word==incl ) xx = 1
         IF ( word==excl ) xx = -1
         IF ( word==exce ) xx = -xx
         IF ( mode==0 ) CALL rdmode(*220,*160,*320,mode,word)
 160     CALL rdword(mode,word)
         spag_nextblock_1 = 6
      CASE (6)
         IF ( word/=grid ) THEN
            IF ( word/=elem ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ELEMENTS ARE TO BE INCLUDED OR EXCLUDED (BY ID OR TYPE)
!
            elgp = 0
            IF ( mode<0 ) THEN
            ELSEIF ( mode==0 ) THEN
!
!     A LIST OF ELEMENT OR GRID POINT ID-S CAN BE EXPLICITLY LISTED, OR
!     PREFERABLY A RANGE CAN BE SPECIFIED (SEPARATED BY THE WORD -TO-
!     OR -THRU-)
!
               CALL rdmode(*220,*120,*320,mode,word)
            ELSE
               GOTO 120
            ENDIF
            GOTO 220
         ENDIF
!
!     A LIST OF GRID POINTS IS TO BE INCLUDED OR EXCLUDED (PERTAIN ONLY
!     TO DEFORMED PLOTS)
!
 180     IF ( mode<=0 ) CALL rdmode(*180,*200,*320,mode,word)
 200     CALL rdword(mode,word)
         IF ( word/=poin .OR. mode/=0 ) GOTO 140
         elgp = 1
         CALL rdmode(*220,*120,*320,mode,word)
 220     ASSIGN 240 TO tra
         GOTO 380
 240     IF ( nelx+1>=ngpx ) CALL mesage(-8,0,name)
         IF ( elgp/=0 ) THEN
            ngpx = ngpx - 1
            gp(ngpx) = isign(iwrd,xx)
         ELSE
            nelx = nelx + 1
            el(nelx) = isign(iwrd,xx)
         ENDIF
!
         CALL rdmode(*380,*260,*320,mode,word)
 260     CALL rdword(mode,word)
         IF ( word/=to .AND. word/=thru ) GOTO 140
         IF ( mode/=0 ) GOTO 140
         ASSIGN 280 TO tra
         CALL rdmode(*380,*140,*320,mode,word)
 280     IF ( nelx+2>=ngpx ) CALL mesage(-8,0,name)
         IF ( elgp/=0 ) THEN
            gp(ngpx-1) = to
            gp(ngpx-2) = isign(iwrd,xx)
            ngpx = ngpx - 2
         ELSE
            el(nelx+1) = to
            el(nelx+2) = iwrd
            nelx = nelx + 2
         ENDIF
         CALL rdmode(*220,*120,*320,mode,word)
         GOTO 220
 300     CALL rdword(mode,word)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( word==incl .OR. word==excl .OR. word==exce ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( word==grid .OR. word==elem ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( word/=all ) THEN
!
            DO i = 1 , ntypes
               idx = (i-1)*incr
!
!     SKIP ELEMENTS WITH
!       1 GRID
!       SCALAR CONNECTIONS POSSIBLE
!       SPECIAL PLOTTER MNEMONIC OF -XX-
!
               IF ( ne(idx+10)>1 .AND. ne(idx+11)==0 ) THEN
                  IF ( ne(idx+16)/=ilxx ) THEN
                     IF ( awrd(1)==ne(idx+1) .AND. awrd(2)==ne(idx+2) ) THEN
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            WRITE (nout,99005) ufm , awrd
99005       FORMAT (A23,' 699,',2A4,' ELEMENT IS INVALID')
            nogo = 1
            elgp = 0
!
!     AN ELEMENT TYPE CAN BE INCLUDED OR EXCLUDED
!
            IF ( mode<=0 ) CALL rdmode(*220,*300,*320,mode,word)
            GOTO 300
         ELSE
            i = ntypes + 1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         nt = nt + 2
!
!     SECOND WORD FOR EACH TYP LOCATES ELEMENT INCLUDE/EXCLUDE SEARCH
!     POINTER.  ELEMENT ID-S GIVEN PRIOR TO NELX ARE SKIPPED
!
         typ(nt-1) = isign(i,xx)
         typ(nt) = nelx + 1
         elgp = 0
         IF ( mode<=0 ) CALL rdmode(*220,*300,*320,mode,word)
         GOTO 300
!
!     A SET HAS BEEN COMPLETELY DEFINED.  FIRST, WRITE THE SET ID
!
 320     IF ( nelx/=0 .OR. nt/=0 ) THEN
            CALL write(msetid,setid,1,0)
            CALL write(mset,setid,1,0)
!
!     WRITE THE SET OF EXPICIT ELEMENT ID-S
!
            CALL write(mset,nelx,1,0)
            CALL write(mset,el,nelx,0)
!
!     DELETE ALL ELEMENT TYPE DUPLICATES + WRITE REMAINING ONES
!
            n = 0
            IF ( nt/=0 ) THEN
               DO j = 1 , nt , 2
                  xx = typ(j)
                  IF ( xx/=0 ) THEN
                     DO i = j , nt , 2
                        IF ( i/=j .AND. iabs(xx)==iabs(typ(i)) ) THEN
!
!     DELETE BOTH IF NEGATIVE OF OTHER
!
                           IF ( xx==-typ(i) ) typ(j) = 0
                           typ(i) = 0
                        ENDIF
                     ENDDO
                     IF ( typ(j)/=0 ) THEN
                        n = n + 2
                        typ(n-1) = xx
                        typ(n) = typ(j+1)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            CALL write(mset,n,1,0)
            CALL write(mset,typ,n,0)
!
!     WRITE THE SET OF EXPLICIT GRID POINT ID-S
!
            n = b1 - ngpx
            CALL write(mset,n,1,0)
            CALL write(mset,gp(ngpx),n,1)
            nsets = nsets + 1
         ENDIF
         GOTO 40
!
!     END OF -PCDB-
!
 340     CALL clstab(mset,rew)
         CALL clstab(plot,rew)
         CALL clstab(msetid,norew)
         CALL close(pcdb,rew)
         IF ( nsets==0 ) WRITE (nout,99006) uim
99006    FORMAT (A29,', NO SETS EXIST IN PLOT PACKAGE')
         IF ( nogo/=0 ) CALL mesage(-61,0,0)
 360     RETURN
!
!     READ AN INTEGER
!
 380     IF ( mode/=-1 ) THEN
            IF ( mode==-4 ) iwrd = dwrd
            IF ( mode/=-4 ) iwrd = fwrd
         ENDIF
         GOTO tra
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE setinp
