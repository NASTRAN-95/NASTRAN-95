!
! COMMON variable declarations
!
   REAL Ajji , Ajjr , Dbc(7) , Dum1(37) , Dum2(14) , Ptshft , Scr3 , Shftpt
   INTEGER Chlsky , Ibuf1 , Ibuf2 , Idbase , Idbmax , Ierror , Iil , Iiloop , Indexv , Indexvd , Iol , Ioloop , Irow1 , Irown ,     &
         & Iscr1 , Iscr2 , Iskip , Ispill , Isprec , Isysbf , Ivwrds , Jcol , Jridx , Jridxn , Jrow , Jrown , Jrows , Jvidx ,       &
         & Karows , Kcol , Keep , Kfrcol , Klscol , Klsrow , Kprec , Kridx , Kridxn , Krow , Krown , Krows , Ktype , Lascol ,       &
         & Lcore , Lll(7) , Maxinlop , Maxnac , Maxnar , Maxncol , Mblk(15) , Mcb(7) , Memcol1 , Memcoln , Memfre , Memlas ,        &
         & Memlck , Moblk(15) , Mxrecl , Nbandw , Nbpw , Ncol , Nout , Nr , Nspill , Nvterm , Power , Sturm
   DOUBLE PRECISION Dajji , Dajjr , Ddc , Ddr , Mindd
   LOGICAL Opnscr
   COMMON /sfact / Mcb , Lll , Dbc , Iscr1 , Iscr2 , Lcore , Ddr , Ddc , Power , Scr3 , Mindd , Chlsky
   COMMON /smcomx/ Ncol , Ierror , Ivwrds , Maxnac , Nspill , Maxinlop , Idbase , Idbmax , Ibuf1 , Ibuf2 , Opnscr , Ioloop ,        &
                 & Iiloop , Lascol , Krow , Krows , Krown , Kridx , Kridxn , Jridxn , Jrow , Jrows , Jrown , Jridx , Jvidx , Irow1 ,&
                 & Irown , Kfrcol , Klscol , Klsrow , Iol , Iil , Ktype , Iskip , Indexv , Indexvd , Jcol , Karows , Mxrecl ,       &
                 & Nvterm , Kcol , Maxncol , Memfre , Memcol1 , Memlck , Memlas , Memcoln , Ispill , Kprec , Nbandw , Maxnar ,      &
                 & Mblk , Moblk
   COMMON /smcomy/ Dajjr , Dajji , Ajjr , Ajji
   COMMON /sturmx/ Sturm , Shftpt , Keep , Ptshft , Nr
   COMMON /system/ Isysbf , Nout , Dum1 , Nbpw , Dum2 , Isprec
!
! End of declarations
!
