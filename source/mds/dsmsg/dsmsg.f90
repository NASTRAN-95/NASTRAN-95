!*==dsmsg.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsmsg(Iflag)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_GINOX
   USE I_XNSTRN
   USE C_DDIOSV
   USE C_LOGOUT
   USE C_SOFCOM
   USE C_SYS
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iflag
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , iname
   INTEGER , DIMENSION(52) :: gino
   INTEGER :: i , iii , index , itemp , iwr , k , loglin , loop , nllog
   INTEGER , DIMENSION(2) :: xname
!
! End of declarations rewritten by SPAG
!
!
!
   !>>>>EQUIVALENCE (Ieor,Gino(1))
   !>>>>EQUIVALENCE (Isystm(2),Iwr) , (Isystm(151),Nllog) , (Isystm(152),Loglin)
!
   DATA blank/1H /
   DATA iname/4HDSMS/
!
   CALL fname(name,xname)
   IF ( xname(1)==0 ) THEN
      xname(1) = blank
      xname(2) = blank
   ENDIF
   IF ( iabs(Iflag)/=777 ) THEN
      IF ( Iflag/=1 .AND. Iflag/=2 .AND. Iflag/=8 ) WRITE (iwr,99001) Iflag
99001 FORMAT (' I/O SUBSYSTEM ERROR NUMBER',I10)
      IF ( Iflag>100 ) THEN
         itemp = Iflag - 100
         IF ( itemp==1 ) THEN
         ELSEIF ( itemp==2 ) THEN
            WRITE (iwr,99002) xname , ifilex
99002       FORMAT (' INCORRECT BLOCK NUMBER ENCOUNTERED',' ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==3 ) THEN
            WRITE (iwr,99003) xname , ifilex
99003       FORMAT (' EXPECTED RH, SB, EF, OR EB CONTROL WORD',' ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==4 ) THEN
            WRITE (iwr,99004) xname , ifilex
99004       FORMAT (' EXPECTED RT CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==5 ) THEN
            WRITE (iwr,99005) xname , ifilex
99005       FORMAT (' EXPECTED RH OR EF CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==6 ) THEN
            WRITE (iwr,99006) xname , ifilex
99006       FORMAT (' EXPECTED RH, EB OR SB CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==7 ) THEN
            WRITE (iwr,99007) xname , ifilex
99007       FORMAT (' REFERENCE IS MADE TO FILE ',2A4,' THAT IS NOT OPENED-UNIT=',I4)
         ELSEIF ( itemp==8 ) THEN
            WRITE (iwr,99008) xname , ifilex
99008       FORMAT (' INSUFFICIENT SPACE FOR I/O CONTROL WORDS ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==9 ) THEN
            WRITE (iwr,99009) xname , ifilex
99009       FORMAT (' TOO MANY TERMS WRITTEN TO STRING ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==10 ) THEN
            WRITE (iwr,99010) xname , ifilex
99010       FORMAT (' EXPECTED A SB OR EB CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==11 ) THEN
            WRITE (iwr,99011) xname , ifilex
99011       FORMAT (' EXPECTED A CH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==12 ) THEN
            WRITE (iwr,99012) xname , ifilex
99012       FORMAT (' EXPECTED A SE, SD, CT, OR SH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==13 ) THEN
            WRITE (iwr,99013) xname , ifilex
99013       FORMAT (' ERROR  - CLR.GT. LCW  ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==14 ) THEN
            WRITE (iwr,99014) xname , ifilex
99014       FORMAT (' EXPECTED A RT CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==15 ) THEN
            WRITE (iwr,99015) xname , ifilex
99015       FORMAT (' EXPECTED A CH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==16 ) THEN
            WRITE (iwr,99016) xname , ifilex
99016       FORMAT (' EXPECTED A CH,ST,SH,SD,RT, OR SE CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==17 ) THEN
            WRITE (iwr,99017) xname , ifilex
99017       FORMAT (' EXPECTED A ST CONTROL WORD ON FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==18 ) THEN
            WRITE (iwr,99018) xname , ifilex
99018       FORMAT (' TYPIN OR TYPOUT FOR MATRIX PACK IS OUT OF RANGE ON',' FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==19 ) THEN
            WRITE (iwr,99019) xname , ifilex
99019       FORMAT (' NON-ASCENDING ROW NUMBER GIVEN',' ON FILE ',2A4,' UNIT=',I10)
         ELSEIF ( itemp==20 ) THEN
            WRITE (iwr,99020) xname , ifilex
99020       FORMAT (' FILE NAME DOES NOT MATCH STRING CONTROL BLOCK FOR ','FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==21 ) THEN
            WRITE (iwr,99021) xname , ifilex
99021       FORMAT (' INVALID UNIT NUMBER IN MDSFCB FOR FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==22 ) THEN
            WRITE (iwr,99022) xname , ifilex
99022       FORMAT (' INSUFFICIENT NUMBER OF FILES AVAILABLE FOR FILE ',2A4,' UNIT=',I4)
         ELSEIF ( itemp==23 ) THEN
!1010 WRITE ( IWR, 1015 ) IOERR
         ENDIF
      ELSE
         IF ( Iflag==2 ) THEN
            WRITE (Lout,99041) 'CLOSE ' , xname , iocode
            loglin = loglin + 1
            GOTO 200
         ELSEIF ( Iflag==3 ) THEN
            WRITE (iwr,99023) xname , ifilex
99023       FORMAT (' BUFFER CONFLICTS WITH EXISTING BUFFERS',' ON FILE ',2A4,' LOGICAL UNIT',I4)
         ELSEIF ( Iflag==4 ) THEN
            WRITE (iwr,99024) xname , ifilex
99024       FORMAT (' ATTEMPT TO READ FILE OPENED FOR WRITE',' FILE=',2A4,' UNIT=',I4)
            GOTO 100
         ELSEIF ( Iflag==5 ) THEN
            WRITE (iwr,99025) xname , ifilex
99025       FORMAT (' FILE IS ALREADY OPENED-FILE ',2A4,' UNIT=',I4)
         ELSEIF ( Iflag==6 ) THEN
            WRITE (iwr,99026) xname , ifilex
99026       FORMAT (' ATTEMPT TO WRITE LESS THAN ONE WORD',' ON FILE ',2A4,' UNIT= ',I4)
         ELSEIF ( Iflag==7 ) THEN
            WRITE (iwr,99027) xname , ifilex
99027       FORMAT (' ATTEMPT TO WRITE ON FILE OPENED FOR READ ','-FILE=',2A4,' UNIT =',I4)
            GOTO 100
         ELSEIF ( Iflag==8 ) THEN
            WRITE (Lout,99028) xname , ifilex , idsn
99028       FORMAT (//,' ****** GINO SUBSYSTEM WILL EXTEND FILE ',2A4,' ON UNIT',I4,' TO UNIT',I4,' ******')
            GOTO 200
         ELSEIF ( Iflag==9 ) THEN
            WRITE (iwr,99029) nblock
99029       FORMAT (//,' INSUFFICIENT SPACE ALLOCATION ON FILE NPTP',' -, NUMBER OF BLOCKS WRITTEN WERE ',I10)
         ELSE
            WRITE (Lout,99041) 'OPEN ' , xname , iocode
            loglin = loglin + 1
            GOTO 200
         ENDIF
         CALL mesage(-61,0,0)
         GOTO 200
      ENDIF
   ENDIF
 100  WRITE (iwr,99030) ioerr , name , xname , ifilex
99030 FORMAT (' I/O ERROR #',I6,' ON FILE ',Z8,' NAME=',2A4,' UNIT=',I4)
   WRITE (iwr,99031)
99031 FORMAT (//' CONTENTS OF MDSFCB')
   DO i = 1 , maxfcb
      CALL dshxdd(i,mdsfcb(1,i),3)
   ENDDO
   WRITE (iwr,99032)
99032 FORMAT (//' CONTENTS OF FCB')
   DO i = 1 , 80
!WKBR NCL93007 11/94
!      WRITE ( IWR, 92003 ) I, ( FCB(K,I),K=1,15)
      WRITE (iwr,99033) i , (fcb(k,i),k=1,17)
!WKBR NCL93007 11/04
!92003 FORMAT(I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4)
99033 FORMAT (I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4,2I8)
   ENDDO
   WRITE (iwr,99034) idbbas , idbfre , idbdir , indbas , indclr , indcbp , nblock , lenalc , iocode , ifilex , name , maxalc ,      &
                   & maxblk , maxdsk , idblen , idbadr , ibasbf , inddir , numopn , numcls , numwri , numrea , lenopc
99034 FORMAT (/,' CONTENTS OF / DBM / FOLLOW:',/,' IDBBAS =',I8,' IDBFRE =',I8,' IDBDIR =',I8,' INDBAS =',I8,/,' INDCLR =',I8,      &
             &' INDCBP =',I8,' NBLOCK =',I8,' LENALC =',I8,/,' IOCODE =',I8,' IFILEX =',I8,' NAME   =',I8,' MAXALC =',I8,/,         &
             &' MAXBLK =',I8,' MAXDSK =',I8,' IDBLEN =',I8,' IDBADR =',I8,/,' IBASBF =',I8,' INDDIR =',I8,' NUMOPN =',I8,           &
            & ' NUMCLS =',I8,/,' NUMWRI =',I8,' NUMREA =',I8,' LENOPC =',I8)
   IF ( Iflag<118 .OR. Iflag>120 ) THEN
      WRITE (iwr,99035)
99035 FORMAT (//' CONTENTS OF SOFCOM ')
      CALL dshxdp(Nfiles,16)
      WRITE (iwr,99036)
99036 FORMAT (//' CONTENTS OF SYS ')
      CALL dshxdp(Blksiz,1)
      WRITE (iwr,99037)
99037 FORMAT (//' CONTENTS OF /DSIO/')
      CALL dshxdp(ieor,59)
      WRITE (iwr,99038)
99038 FORMAT (//' CONTENTS OF /DDIOSV/')
      DO i = 1 , maxpri
         WRITE (iwr,99039) i , Iflpos(1,i) , Iflpos(2,i)
99039    FORMAT (I5,2I10)
      ENDDO
      loop = (nbuff+lendsp)/8 + 4
      index = indbas
      WRITE (iwr,99040)
99040 FORMAT (//' CONTENTS OF I/O BUFFER')
      DO i = 1 , loop
         iii = (i-1)*8 + 1
         CALL dshxdd(iii,ibase(index),8)
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
