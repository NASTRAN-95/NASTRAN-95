!*==exi2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE exi2
USE C_BLANK
USE C_MACHIN
USE C_NAMES
USE C_SYSTEM
USE C_TYPE
USE C_XMSSG
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bar , dit , eog , eoi , jh , leof , mdi , plts , q4 , scr1 , sof , sp , srd , swrt , t3
   INTEGER :: buf1 , buf2 , buf3 , buf4 , i , idm , iprc , irw , item , itest , itm , j , k , lcore , n , ncol , ncore , nos , np2 ,&
            & nwds , offset , prec , rc
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(7) :: hdr , mcb
   INTEGER , DIMENSION(2) :: name
   INTEGER , DIMENSION(2) , SAVE :: subr
   LOGICAL :: usrmsg
   REAL , DIMENSION(6) , SAVE :: zero
   EXTERNAL bldpk , bldpkn , close , delete , exfort , exlvl , gopen , ittype , korsz , lshift , mesage , mtrxo , page , sfetch ,   &
          & smsg , sofcls , sofopn , softrl , suwrt , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     EXI2 PERFORMS EXTERNAL FORMAT SOFIN OPERATIONS
!
   !>>>>EQUIVALENCE (Z(1),Dz(1)) , (A(1),Da)
   DATA sof , srd , swrt , eoi , sp/4HSOF  , 1 , 2 , 3 , 1/
   DATA leof , jh , scr1 , subr/4H$EOF , 1 , 301 , 4HEXI2 , 4H    /
   DATA dit , mdi , eog , zero/4HDIT  , 4HMDI  , 2 , 6*0.0/
   DATA q4 , t3 , bar , plts/2HQ4 , 2HT3 , 2HBR , 4HPLTS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ncore = korsz(Z)
         i = ncore - Lbuf
         IF ( Mach==12 ) i = i - Lbuf
         ncore = i - 1
         irw = Iadd
         Iadd = i
         CALL exfort(3,Unit,0,0,irw,0,0)
         buf1 = ncore - Sysbuf + 1
         buf2 = buf1 - Sysbuf - 1
         buf3 = buf2 - Sysbuf
         buf4 = buf3 - Sysbuf
         ncore = buf4 - 1
         nos = 0
         idm = 1
         usrmsg = .TRUE.
         lcore = ncore
         IF ( ncore<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
         CALL page
!
!     READ THE HEADER OF THE NEXT ITEM AND FETCH THE ITEM ON THE SOF
!
         CALL exfort(srd,Unit,jh,hdr,7,sp,0)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            name(1) = hdr(1)
            name(2) = hdr(2)
            item = hdr(3)
            itest = hdr(7)
            IF ( itest==eoi ) itest = eog
            IF ( hdr(1)==dit .OR. hdr(1)==mdi ) THEN
!
!     READ DIT AND MDI
!
               nos = hdr(5)/2
               lcore = ncore - hdr(5)*4
               idm = lcore + 1
               IF ( 6*nos>lcore ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL exfort(srd,Unit,hdr(4),Z,hdr(5),sp,0)
               DO i = 1 , nos
                  Z(idm+4*i-4) = Z(2*i-1)
                  Z(idm+4*i-3) = Z(2*i)
               ENDDO
               CALL exfort(srd,Unit,jh,hdr,7,sp,0)
               CALL exfort(srd,Unit,hdr(4),Z,hdr(5),sp,0)
               DO i = 1 , nos
                  j = idm + 4*i - 2
                  k = 6*i - 6
                  Z(j) = lshift(Z(k+1),20) + lshift(Z(k+2),10) + Z(k+3)
                  Z(j+1) = lshift(Z(k+4),20) + lshift(Z(k+5),10) + Z(k+6)
               ENDDO
               CALL exfort(srd,Unit,jh,hdr,7,sp,0)
            ELSE
               IF ( hdr(3)==-1 .OR. hdr(1)==leof ) THEN
!
!     NORMAL MODULE COMPLETION
!
                  CALL sofcls
                  RETURN
               ELSE
                  itm = ittype(hdr(3))
                  IF ( itm==1 ) THEN
!
!     MATRICES
!
!
!     READ TRAILER
!
                     CALL softrl(hdr(1),hdr(3),mcb(1))
                     rc = mcb(1)
                     IF ( rc/=3 ) THEN
                        Line = Line + 2
                        IF ( Line>Nlpp ) CALL page
                        IF ( rc==3 ) THEN
                        ELSEIF ( rc==4 ) THEN
                           CALL exlvl(nos,Z(idm),hdr,Z,lcore)
                        ELSEIF ( rc==5 ) THEN
                           CALL smsg(3,hdr(3),hdr)
                           usrmsg = .FALSE.
                        ELSE
                           WRITE (Nout,99003) Uwm , hdr(1) , hdr(2) , hdr(3)
                           usrmsg = .FALSE.
                        ENDIF
                     ENDIF
                     CALL exfort(srd,Unit,hdr(4),mcb(2),6,sp,0)
                     ncol = mcb(2)
                     prec = mcb(5)
                     mcb(1) = scr1
                     mcb(2) = 0
                     mcb(6) = 0
                     mcb(7) = 0
                     IF ( usrmsg ) CALL gopen(scr1,Z(buf4),Wrtrew)
!
!     READ MATRIX ONE COLUMN AT A TIME AND PACK ON SCR2
!
                     DO j = 1 , ncol
                        CALL exfort(srd,Unit,jh,hdr,7,sp,0)
                        IF ( hdr(1)/=name(1) .OR. hdr(2)/=name(2) ) EXIT SPAG_Loop_1_1
                        IF ( hdr(3)/=item ) EXIT SPAG_Loop_1_1
                        nwds = hdr(5)
                        IF ( nwds*1.4>ncore ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        CALL exfort(srd,Unit,hdr(4),Z,nwds,prec,dz)
                        IF ( usrmsg ) THEN
                           CALL bldpk(prec,prec,scr1,0,0)
                           iprc = Prc(prec)
                           n = Nword(prec) + iprc
                           k = 1
                           DO WHILE ( Z(k)>=0 )
                              Irow = Z(k)
                              A(1) = Z(k+iprc)
                              IF ( prec/=1 ) THEN
                                 A(2) = Z(k+iprc+1)
                                 IF ( prec>3 ) THEN
                                    A(3) = Z(k+4)
                                    A(4) = Z(k+5)
                                 ENDIF
                              ENDIF
                              CALL zblpki
                              k = k + n
                           ENDDO
                           CALL bldpkn(scr1,0,mcb)
                        ENDIF
                     ENDDO
                     IF ( .NOT.usrmsg ) GOTO 5
                     CALL wrttrl(mcb)
                     CALL close(scr1,Rew)
                     CALL mtrxo(scr1,hdr,hdr(3),0,rc)
                  ELSE
                     rc = 3
                     CALL sfetch(hdr(1),hdr(3),swrt,rc)
                     IF ( rc/=3 ) THEN
                        Line = Line + 2
                        IF ( Line>Nlpp ) CALL page
                        IF ( rc/=3 ) THEN
                           IF ( rc==4 ) THEN
                              CALL exlvl(nos,Z(idm),hdr,Z,lcore)
                              rc = 3
                              CALL sfetch(hdr(1),hdr(3),swrt,rc)
                              IF ( rc==3 ) GOTO 2
                           ELSEIF ( rc/=5 ) THEN
                              WRITE (Nout,99003) Uwm , hdr(1) , hdr(2) , hdr(3)
                              usrmsg = .FALSE.
                              GOTO 2
                           ENDIF
                           CALL smsg(rc-2,hdr(3),hdr)
                           usrmsg = .FALSE.
                        ENDIF
                     ENDIF
!
!     TABLES
!
!
!     ELSETS TABLE CORRECTION BY G.CHAN/UNISYS   4/91
!
!     IN 91 VERSION, ELEMENT PLOT SYMBOL LINE HAS 2 WORDS, SYMBOL AND
!     NO. OF GRID POINT PER ELEMENT, NGPEL, WRITTEN OUT BY EXO2 USING
!     FORMAT 25. THE ELSETS DATA LINE COMING UP NEXT USE FORMAT 10 FOR
!     ELEMENTS WITH NO OFFSETS, FORMAT 26 FOR BAR WHICH HAS 6 OFFSET
!     VALUES, AND FORMATS 27 AND 28 FOR TRIA3 AND QUAD4 WHICH HAS 1
!     OFFSET VALUE EACH.
!     IN 90 AND EARLIER VERSIONS, ONLY ONE ELEMENT PLOT SYMBOL WORD WAS
!     WRITTEN OUT, AND ON ELSETS DATA LINE COMING UP NEXT, FORMAT 10
!     WAS USED FOR ALL ELEMENTS. NO OFFSET DATA FOR THE BAR, QUAD4 AND
!     TRIA3 ELEMENTS. NGPEL WAS THE FIRST WORD ON THE ELSETS DATA LINE.
!     ALSO, THE 90 AND EARLIER VERSIONS DID NOT COUNT PROPERTY ID, PID,
!     ON THE ELSETS DATA LINE. THUS THE TOTAL NO. OF WORDS MAY BE IN
!     ERROR AND MAY CAUSE EXTRA ZEROS AT THE END OF THE DATA LINE.
!
!     THEREFORE, IF THE 90 OR EARLIER EXTERNAL SOF FILE WAS USED, WE
!     NEED TO ADD THE OFFSETS (1 OR 6 FLOATING POINTS ZEROS) TO THE BAR,
!     QUAD4 AND TRIA3 ELEMENTS FOR THE ELSETS TABLE.
!     (AS OF 4/91, THESE CHANGES HAVE NOT BEEN TESTED)
!
 2                   offset = 0
                     SPAG_Loop_2_3: DO
                        nwds = hdr(5)
                        IF ( nwds>lcore ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        CALL exfort(srd,Unit,hdr(4),Z,nwds,sp,0)
                        IF ( offset/=0 ) THEN
                           j = 1
                           CALL suwrt(Z(1),1,j)
                           np2 = Z(1) + 2
                           SPAG_Loop_3_2: DO k = 2 , nwds , np2
                              IF ( Z(k)==0 ) EXIT SPAG_Loop_3_2
                              CALL suwrt(Z(k),np2,j)
                              CALL suwrt(zero,offset,j)
                           ENDDO SPAG_Loop_3_2
                           Z(1) = 0
                           nwds = 1
                        ENDIF
                        CALL suwrt(Z,nwds,itest)
                        IF ( hdr(7)/=eoi ) THEN
                           CALL exfort(srd,Unit,jh,hdr,7,sp,0)
                           IF ( hdr(1)/=name(1) .OR. hdr(2)/=name(2) ) EXIT SPAG_Loop_1_1
                           IF ( hdr(3)/=item ) EXIT SPAG_Loop_1_1
                           itest = hdr(7)
                           IF ( itest==eoi ) itest = eog
                           IF ( item==plts .AND. hdr(5)==1 .AND. hdr(4)==10 ) THEN
                              offset = 0
                              IF ( Z(1)==bar ) offset = 6
                              IF ( Z(1)==q4 .OR. Z(1)==t3 ) offset = 1
                           ENDIF
                           IF ( hdr(4)>0 ) CYCLE
                        ENDIF
                        itest = eoi
                        CALL suwrt(0,0,itest)
                        EXIT SPAG_Loop_2_3
                     ENDDO SPAG_Loop_2_3
                  ENDIF
!
!     WRITE USER MESSAGE
!
                  IF ( usrmsg ) THEN
                     Line = Line + 1
                     IF ( Line>Nlpp ) CALL page
                     WRITE (Nout,99001) Uim , hdr(1) , hdr(2) , hdr(3) , Uname , sof
99001                FORMAT (A29,' 6357, SUBSTRUCTURE ',2A4,' ITEM ',A4,' SUCCESSFULLY COPIED FROM ',2A4,' TO ',A4)
                  ENDIF
               ENDIF
 5             usrmsg = .TRUE.
               CALL exfort(srd,Unit,jh,hdr,7,sp,0)
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     NO EOI FOR ITEM AND A NEW ITEM WAS READ
!
         Line = Line + 2
         IF ( Line>Nlpp ) CALL page
         WRITE (Nout,99002) Uwm , name(1) , name(2) , item , Uname
99002    FORMAT (A25,' 6363, INCOMPLETE DATA FOR SUBSTRUCTURE ',2A4,' ITEM ',A4,' ON ',2A4,'. THE ITEM WILL NOT BE COPIED.')
         IF ( itm==0 ) CALL delete(name,item,rc)
         IF ( itm==1 ) CALL close(scr1,Rew)
         usrmsg = .TRUE.
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     ABNORMAL MODULE COMPLETION
!
         CALL mesage(8,0,subr)
         Dry = -2
         CALL sofcls
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     MESSAGE TEXTS
!
99003 FORMAT (A25,' 6346, SUBSTRUCTURE ',2A4,' ITEM ',A4,' NOT COPIED.  IT ALREADY EXISTS ON THE SOF.')
END SUBROUTINE exi2
