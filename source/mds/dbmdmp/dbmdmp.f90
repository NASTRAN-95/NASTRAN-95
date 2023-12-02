!*==dbmdmp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmdmp
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icnt , itotal , itotbk , ival , ivaln , ivalp , k , next
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!     DBMDMP - DUMPS THE IN MEMORY DATA BASE DIRECTORY
!********************************************************************
   WRITE (Iwr,99001) idbbas , idbfre , idbdir , indbas , indclr , indcbp , nblock , lenalc , iocode , ifilex , name , maxalc ,      &
                   & maxblk , maxdsk , idblen , idbadr , ibasbf , inddir , numopn , numcls , numwri , numrea , lenopc
99001 FORMAT (/,' CONTENTS OF / DBM / FOLLOW:',/,' IDBBAS =',I8,' IDBFRE =',I8,' IDBDIR =',I8,' INDBAS =',I8,/,' INDCLR =',I8,      &
             &' INDCBP =',I8,' NBLOCK =',I8,' LENALC =',I8,/,' IOCODE =',I8,' IFILEX =',I8,' NAME   =',I8,' MAXALC =',I8,/,         &
             &' MAXBLK =',I8,' MAXDSK =',I8,' IDBLEN =',I8,' IDBADR =',I8,/,' IBASBF =',I8,' INDDIR =',I8,' NUMOPN =',I8,           &
            & ' NUMCLS =',I8,/,' NUMWRI =',I8,' NUMREA -',I8,' LENOPC =',I8)
   WRITE (Iwr,99002)
99002 FORMAT (/,' CONTENTS OF FCB FOLLOW:',/)
   DO i = 1 , 80
      WRITE (Iwr,99003) i , (fcb(k,i),k=1,15)
99003 FORMAT (I3,'-',I3,I7,4I5,I12,I2,4I7,2A4,I4)
   ENDDO
   CALL dbmdia
!      WRITE ( IWR, 906 )
!      WRITE ( IWR, 907 )
   next = idbfre
   itotal = 0
   itotbk = 0
   icnt = 0
   IF ( next/=0 ) THEN
      DO
         icnt = icnt + 1
         IF ( next==0 ) EXIT
         ival = next
         ivalp = Mem(next)
         ivaln = Mem(next+1)
         IF ( Mem(next)==0 ) ivalp = 0
         IF ( Mem(next+1)==0 ) ivaln = 0
         itotal = itotal + Mem(next+2)
         itotbk = itotbk + 1
!      WRITE ( IWR, 908 ) ICNT,IVAL,IVALP,IVALN,MEM(NEXT+2)
!      WRITE( IWR, 909 )
         next = Mem(next+1)
      ENDDO
   ENDIF
!     WRITE( IWR, 910 ) ITOTAL, ITOTBK
   RETURN
99004 FORMAT (///,31X,' DUMP OF FREE CHAIN',/,13X,' ( BLOCK ADDRESSES IN BYTES,  BLOCK LENGTHS IN WORDS )',/)
99005 FORMAT (10X,'  BLOCK NO    BLOCK ADDRESS  PREV. BLOCK   NEXT BLOCK    LENGTH')
99006 FORMAT (I17,I20,I13,I13,I10)
99007 FORMAT (//' *************** NO FREE SPACE REMAINS **************')
99008 FORMAT (///,' TOTAL FREE SPACE IN WORDS            =',I10,/,' NUMBER OF BLOCKS IN FREE SPACE CHAIN =',I10)
END SUBROUTINE dbmdmp
