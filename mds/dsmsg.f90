
SUBROUTINE dsmsg(Iflag)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Avblks , Blksiz , Dirsiz , Filnam(10) , Filsiz(10) , Hiblk , Iflpos(2,MAXPRI) , Isystm(175) , Iwr , Loglin , Lout ,      &
         & Nfiles , Nllog , Supsiz
   COMMON /ddiosv/ Iflpos
   COMMON /logout/ Lout
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks , Hiblk
   COMMON /system/ Isystm
   INTEGER Iflag
   INTEGER blank , i , iii , iname , index , itemp , k , loop , xname(2) , Gino(52)
!
!
   EQUIVALENCE (Ieor,Gino(1))
   EQUIVALENCE (Isystm(2),Iwr) , (Isystm(151),Nllog) , (Isystm(152),Loglin)
!
   DATA blank/1H /
   DATA iname/4HDSMS/
!
   CALL fname(Name,xname)
   IF ( xname(1)==0 ) THEN
      xname(1) = blank
      xname(2) = blank
   ENDIF
   IF ( iabs(Iflag)/=777 ) THEN
      IF ( Iflag/=1 .AND. Iflag/=2 .AND. Iflag/=8 ) WRITE (Iwr,99001) Iflag
99001 FORMAT (' I/O SUBSYSTEM ERROR NUMBER',I10)
      IF ( Iflag>100 ) THEN
         itemp = Iflag - 100
         IF ( itemp==1 ) THEN
         ELSEIF ( itemp==2 ) THEN
            WRITE (Iwr,99002) xname , Ifilex
99002       FORMAT (' INCORRECT BLOCK NUMBER ENCOUNTERED',' ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==3 ) THEN
            WRITE (Iwr,99003) xname , Ifilex
99003       FORMAT (' EXPECTED RH, SB, EF, OR EB CONTROL WORD',' ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==4 ) THEN
            WRITE (Iwr,99004) xname , Ifilex
99004       FORMAT (' EXPECTED RT CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==5 ) THEN
            WRITE (Iwr,99005) xname , Ifilex
99005       FORMAT (' EXPECTED RH OR EF CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==6 ) THEN
            WRITE (Iwr,99006) xname , Ifilex
99006       FORMAT (' EXPECTED RH, EB OR SB CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==7 ) THEN
            WRITE (Iwr,99007) xname , Ifilex
99007       FORMAT (' REFERENCE IS MADE TO FILE ',2A4,' THAT IS NOT OPENED-UNIT=',I4)
         ELSEIF ( itemp==8 ) THEN
            WRITE (Iwr,99008) xname , Ifilex
99008       FORMAT (' INSUFFICIENT SPACE FOR I/O CONTROL WORDS ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==9 ) THEN
            WRITE (Iwr,99009) xname , Ifilex
99009       FORMAT (' TOO MANY TERMS WRITTEN TO STRING ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==10 ) THEN
            WRITE (Iwr,99010) xname , Ifilex
99010       FORMAT (' EXPECTED A SB OR EB CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==11 ) THEN
            WRITE (Iwr,99011) xname , Ifilex
99011       FORMAT (' EXPECTED A CH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==12 ) THEN
            WRITE (Iwr,99012) xname , Ifilex
99012       FORMAT (' EXPECTED A SE, SD, CT, OR SH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==13 ) THEN
            WRITE (Iwr,99013) xname , Ifilex
99013       FORMAT (' ERROR  - CLR.GT. LCW  ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==14 ) THEN
            WRITE (Iwr,99014) xname , Ifilex
99014       FORMAT (' EXPECTED A RT CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==15 ) THEN
            WRITE (Iwr,99015) xname , Ifilex
99015       FORMAT (' EXPECTED A CH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==16 ) THEN
            WRITE (Iwr,99016) xname , Ifilex
99016       FORMAT (' EXPECTED A CH,ST,SH,SD,RT, OR SE CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==17 ) THEN
            WRITE (Iwr,99017) xname , Ifilex
99017       FORMAT (' EXPECTED A ST CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==18 ) THEN
            WRITE (Iwr,99018) xname , Ifilex
99018       FORMAT (' TYPIN OR TYPOUT FOR MATRIX PACK IS OUT OF RANGE ON',' FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==19 ) THEN
            WRITE (Iwr,99019) xname , Ifilex
99019       FORMAT (' NON-ASCENDING ROW NUMBER GIVEN',' ON FILE ',2A4,' UNIT=',I10)
         ELSEIF ( itemp==20 ) THEN
            WRITE (Iwr,99020) xname , Ifilex
99020       FORMAT (' FILE NAME DOES NOT MATCH STRING CONTROL BLOCK FOR ','FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==21 ) THEN
            WRITE (Iwr,99021) xname , Ifilex
99021       FORMAT (' INVALID UNIT NUMBER IN MDSFCB FOR FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==22 ) THEN
            WRITE (Iwr,99022) xname , Ifilex
99022       FORMAT (' INSUFFICIENT NUMBER OF FILES AVAILABLE FOR FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==23 ) THEN
!1010 WRITE ( IWR, 1015 ) IOERR
         ENDIF
      ELSE
         IF ( Iflag==2 ) THEN
            WRITE (Lout,99041) 'CLOSE ' , xname , Iocode
            Loglin = Loglin + 1
            GOTO 200
         ELSEIF ( Iflag==3 ) THEN
            WRITE (Iwr,99023) xname , Ifilex
99023       FORMAT (' BUFFER CONFLICTS WITH EXISTING BUFFERS',' ON FILE ',2A4,' LOGICAL UNIT',I4)
         ELSEIF ( Iflag==4 ) THEN
            WRITE (Iwr,99024) xname , Ifilex
99024       FORMAT (' ATTEMPT TO READ FILE OPENED FOR WRITE',' FILE=',2A4,' UNIT=',I4)
            GOTO 100
         ELSEIF ( Iflag==5 ) THEN
            WRITE (Iwr,99025) xname , Ifilex
99025       FORMAT (' FILE IS ALREADY OPENED-FILE ',2A4,' UNIT=',I4)
         ELSEIF ( Iflag==6 ) THEN
            WRITE (Iwr,99026) xname , Ifilex
99026       FORMAT (' ATTEMPT TO WRITE LESS THAN ONE WORD',' ON FILE ',2A4,' UNIT= ',I4)
         ELSEIF ( Iflag==7 ) THEN
            WRITE (Iwr,99027) xname , Ifilex
99027       FORMAT (' ATTEMPT TO WRITE ON FILE OPENED FOR READ ','-FILE=',2A4,' UNIT =',I4)
            GOTO 100
         ELSEIF ( Iflag==8 ) THEN
            WRITE (Lout,99028) xname , Ifilex , Idsn
99028       FORMAT (//,' ****** GINO SUBSYSTEM WILL EXTEND FILE ',2A4,' ON UNIT',I4,' TO UNIT',I4,' ******')
            GOTO 200
         ELSEIF ( Iflag==9 ) THEN
            WRITE (Iwr,99029) Nblock
99029       FORMAT (//,' INSUFFICIENT SPACE ALLOCATION ON FILE NPTP',' -, NUMBER OF BLOCKS WRITTEN WERE ',I10)
         ELSE
            WRITE (Lout,99041) 'OPEN ' , xname , Iocode
            Loglin = Loglin + 1
            GOTO 200
         ENDIF
         CALL mesage(-61,0,0)
         GOTO 200
      ENDIF
   ENDIF
 100  WRITE (Iwr,99030) Ioerr , Name , xname , Ifilex
99030 FORMAT (' I/O ERROR #',I6,' ON FILE ',Z8,' NAME=',2A4,' UNIT=',I4)
   WRITE (Iwr,99031)
99031 FORMAT (//' CONTENTS OF MDSFCB')
   DO i = 1 , MAXFCB
      CALL dshxdd(i,Mdsfcb(1,i),3)
   ENDDO
   WRITE (Iwr,99032)
99032 FORMAT (//' CONTENTS OF FCB')
   DO i = 1 , 80
!WKBR NCL93007 11/94
!      WRITE ( IWR, 92003 ) I, ( FCB(K,I),K=1,15)
      WRITE (Iwr,99033) i , (Fcb(k,i),k=1,17)
!WKBR NCL93007 11/04
!92003 FORMAT(I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4)
99033 FORMAT (I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4,2I8)
   ENDDO
   WRITE (Iwr,99034) Idbbas , Idbfre , Idbdir , Indbas , Indclr , Indcbp , Nblock , Lenalc , Iocode , Ifilex , Name , Maxalc ,      &
                   & Maxblk , Maxdsk , Idblen , Idbadr , Ibasbf , Inddir , Numopn , Numcls , Numwri , Numrea , Lenopc
99034 FORMAT (/,' CONTENTS OF / DBM / FOLLOW:',/,' IDBBAS =',I8,' IDBFRE =',I8,' IDBDIR =',I8,' INDBAS =',I8,/,' INDCLR =',I8,      &
             &' INDCBP =',I8,' NBLOCK =',I8,' LENALC =',I8,/,' IOCODE =',I8,' IFILEX =',I8,' NAME   =',I8,' MAXALC =',I8,/,         &
             &' MAXBLK =',I8,' MAXDSK =',I8,' IDBLEN =',I8,' IDBADR =',I8,/,' IBASBF =',I8,' INDDIR =',I8,' NUMOPN =',I8,           &
            & ' NUMCLS =',I8,/,' NUMWRI =',I8,' NUMREA =',I8,' LENOPC =',I8)
   IF ( Iflag<118 .OR. Iflag>120 ) THEN
      WRITE (Iwr,99035)
99035 FORMAT (//' CONTENTS OF SOFCOM ')
      CALL dshxdp(Nfiles,16)
      WRITE (Iwr,99036)
99036 FORMAT (//' CONTENTS OF SYS ')
      CALL dshxdp(Blksiz,1)
      WRITE (Iwr,99037)
99037 FORMAT (//' CONTENTS OF /DSIO/')
      CALL dshxdp(Ieor,59)
      WRITE (Iwr,99038)
99038 FORMAT (//' CONTENTS OF /DDIOSV/')
      DO i = 1 , MAXPRI
         WRITE (Iwr,99039) i , Iflpos(1,i) , Iflpos(2,i)
99039    FORMAT (I5,2I10)
      ENDDO
      loop = (Nbuff+Lendsp)/8 + 4
      index = Indbas
      WRITE (Iwr,99040)
99040 FORMAT (//' CONTENTS OF I/O BUFFER')
      DO i = 1 , loop
         iii = (i-1)*8 + 1
         CALL dshxdd(iii,Ibase(index),8)
         index = index + 8
      ENDDO
      CALL dbmdia
   ENDIF
   IF ( Iflag/=777 ) THEN
      CALL mesage(-61,0,0)
   ELSE
!     CALL TRBK( IWR )
      RETURN
   ENDIF
 200  RETURN
99041 FORMAT (40X,A6,2A4,2X,I2)
99042 FORMAT (' ERROR DURING I/O REQUEST - ERROR FLAG=',Z8)
END SUBROUTINE dsmsg
