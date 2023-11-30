
 
 
SUBROUTINE smchlp
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
!
! SMCHLP WRITES THE CONTENTS OF THE KEY PARAMETERS IN THE SYMMETRIC
! DECOMPOSITION COMMON BLOCKS
!
   WRITE (Nout,99001) Ncol , Ierror , Ivwrds , Maxnac , Nspill , Maxinlop , Idbase , Idbmax , Ibuf1 , Ibuf2 , Opnscr , Ioloop ,     &
                    & Lascol , Krow , Krows , Krown , Kridx , Kridxn , Jridxn , Jrow , Jrows , Jrown , Jridx , Jvidx , Irow1 ,      &
                    & Irown , Kfrcol , Klscol , Klsrow , Iol , Iil , Ktype , Iskip , Indexv , Kcol , Maxncol , Memfre , Memcol1 ,   &
                    & Memlck , Memlas , Memcoln , Ispill , Nbandw , Nvterm
99001 FORMAT (//,' NCOL   =',I9,' IERROR  =',I9,' IVWRDS =',I9,' MAXNAC =',I9,/,' NSPILL =',I9,' MAXINLOP=',I9,' IDBASE =',I9,      &
             &' IDBMAX =',I9,/,' IBUF1  =',I9,' IBUF2   =',I9,' OPNSCR =',L9,' IOLOOP =',I9,/,' LASCOL =',I9,' KROW    =',I9,       &
             &' KROWS  =',I9,' KROWN  =',I9,/,' KRIDX  =',I9,' KRIDXN  =',I9,' JRIDXN =',I9,' JROW   =',I9,/,' JROWS  =',I9,        &
             &' JROWN   =',I9,' JRIDX  =',I9,' JVIDX  =',I9,/,' IROW1  =',I9,' IROWN   =',I9,' KFRCOL =',I9,' KLSCOL =',I9,/,       &
             &' KLSROW =',I9,' IOL     =',I9,' IIL    =',I9,' KTYPE  =',I9,/,' ISKIP  =',I9,' INDEXV  =',I9,' KCOL   =',I9,         &
             &' MAXNCOL=',I9,/,' MEMFRE =',I9,' MEMCOL1 =',I9,' MEMLCK =',I9,' MEMLAS =',I9,/,' MEMCOLN=',I9,' ISPILL  =',I9,       &
             &' NBANDW =',I9,' NVTERM =',I9)
   WRITE (Nout,99002) Isysbf , Isprec
99002 FORMAT (/,' ISYSBF =',I9,' ISPREC   =',I9)
   WRITE (Nout,99003) Mblk , Moblk
99003 FORMAT (/,' MBLK (INPUT MATRIX STRING BLOCK)=',/,3(5I10,/),/,' MOBLK (OUTPUT MATRIX STRING BLOCK)=',/,3(5I10,/))
   WRITE (Nout,99004) Lcore , Power , Mindd , Chlsky , Iscr1
99004 FORMAT (/,' LCORE  =',I9,' POWER   =',I9,' MINDD   =',E16.8,/,' CHLSKY =',I9,' ISCR1   =',I9)
   WRITE (Nout,99005) Mcb , Lll
99005 FORMAT ('  INPUT MATRIX MCB=',/,7I8,/,' OUTPUT MATRIX MCB=',/,7I8)
END SUBROUTINE smchlp
