!*==smchlp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
 
SUBROUTINE smchlp
   IMPLICIT NONE
   USE i_smcomx
!
! SMCHLP WRITES THE CONTENTS OF THE KEY PARAMETERS IN THE SYMMETRIC
! DECOMPOSITION COMMON BLOCKS
!
   WRITE (nout,99001) ncol , ierror , ivwrds , maxnac , nspill , maxinlop , idbase , idbmax , ibuf1 , ibuf2 , opnscr , ioloop ,     &
                    & lascol , krow , krows , krown , kridx , kridxn , jridxn , jrow , jrows , jrown , jridx , jvidx , irow1 ,      &
                    & irown , kfrcol , klscol , klsrow , iol , iil , ktype , iskip , indexv , kcol , maxncol , memfre , memcol1 ,   &
                    & memlck , memlas , memcoln , ispill , nbandw , nvterm
99001 FORMAT (//,' NCOL   =',I9,' IERROR  =',I9,' IVWRDS =',I9,' MAXNAC =',I9,/,' NSPILL =',I9,' MAXINLOP=',I9,' IDBASE =',I9,      &
             &' IDBMAX =',I9,/,' IBUF1  =',I9,' IBUF2   =',I9,' OPNSCR =',L9,' IOLOOP =',I9,/,' LASCOL =',I9,' KROW    =',I9,       &
             &' KROWS  =',I9,' KROWN  =',I9,/,' KRIDX  =',I9,' KRIDXN  =',I9,' JRIDXN =',I9,' JROW   =',I9,/,' JROWS  =',I9,        &
             &' JROWN   =',I9,' JRIDX  =',I9,' JVIDX  =',I9,/,' IROW1  =',I9,' IROWN   =',I9,' KFRCOL =',I9,' KLSCOL =',I9,/,       &
             &' KLSROW =',I9,' IOL     =',I9,' IIL    =',I9,' KTYPE  =',I9,/,' ISKIP  =',I9,' INDEXV  =',I9,' KCOL   =',I9,         &
             &' MAXNCOL=',I9,/,' MEMFRE =',I9,' MEMCOL1 =',I9,' MEMLCK =',I9,' MEMLAS =',I9,/,' MEMCOLN=',I9,' ISPILL  =',I9,       &
             &' NBANDW =',I9,' NVTERM =',I9)
   WRITE (nout,99002) isysbf , isprec
99002 FORMAT (/,' ISYSBF =',I9,' ISPREC   =',I9)
   WRITE (nout,99003) mblk , moblk
99003 FORMAT (/,' MBLK (INPUT MATRIX STRING BLOCK)=',/,3(5I10,/),/,' MOBLK (OUTPUT MATRIX STRING BLOCK)=',/,3(5I10,/))
   WRITE (nout,99004) lcore , power , mindd , chlsky , iscr1
99004 FORMAT (/,' LCORE  =',I9,' POWER   =',I9,' MINDD   =',E16.8,/,' CHLSKY =',I9,' ISCR1   =',I9)
   WRITE (nout,99005) mcb , lll
99005 FORMAT ('  INPUT MATRIX MCB=',/,7I8,/,' OUTPUT MATRIX MCB=',/,7I8)
END SUBROUTINE smchlp
