
SUBROUTINE vdr
   IMPLICIT NONE
   INTEGER App(2) , Buf(50) , Casecc , Iaacc , Iacc , Iadisp , Iavel , Idisp , Idload , Ielf , Ifrout , Iloads , Ilsym , Imode ,    &
         & Infile , Ipnl , Ispcf , Istr , Ittl , Ivel , Masks(6) , Modal(2) , Nam(2) , Opnl1 , Outfle , Output , Pnl , Sdr2 ,       &
         & Sort2 , Sscell , Trn(2) , Vdrreq
   REAL Buf1 , Buf2 , Buf3 , Cei(2) , Direct(2) , Dumi(68) , Eqdyn , Form(2) , Frq(2) , Oeigs , Pp , Scr1 , Scr2 , Usetd , Vdrcom , &
      & Xset0 , Xycdb
   COMMON /blank / App , Form , Sort2 , Output , Sdr2 , Imode
   COMMON /system/ Dumi , Sscell
   COMMON /vdrcom/ Vdrcom , Idisp , Ivel , Iacc , Ispcf , Iloads , Istr , Ielf , Iadisp , Iavel , Iaacc , Ipnl , Ittl , Ilsym ,     &
                 & Ifrout , Idload , Casecc , Eqdyn , Usetd , Infile , Oeigs , Pp , Xycdb , Pnl , Outfle , Opnl1 , Scr1 , Scr2 ,    &
                 & Buf1 , Buf2 , Buf3 , Nam , Buf , Masks , Cei , Frq , Trn , Direct , Xset0 , Vdrreq , Modal
   INTEGER i , mcb(7)
!
!     VDR IS THE CONTROL PROGRAM FOR THE VECTOR DATA RECOVERY MODULE
!
!                                                          OPHID
!                                  PHID                    OUDVC1
!                                  UDVF CLAMA              OUDV1
!             CASECC  EQDYN  USETD UDVT PPF          PHLD  OPHIH  OPNL1
!     VDR     CASEXX,HEQDYN,HUSETD,PHIH,TOL   ,XYCBD,PNLH /OUHVC1,HOPNL1
!                                  UHVT HTOL         HPNLD OUHV1
!                                  HUDVT                   HOUVD1
!
!                   TRANRESP     DIRECT
!              /C,N,FREQRESP/C,N,MODAL /V,N,SORT2/V,N,OUTPUT/V,N,SDR2
!                   CEIGN
!
!              /V,N,FMODE  $      PROGRAMMER'S MANUAL PP. 4.60-1 TRHU -7
!
!
!
!     EXECUTE THE PHASES OF VDR.
!
   DO i = 1 , 50
      Buf(i) = 0
   ENDDO
   Casecc = 101
   Output = -1
   Sort2 = -1
   CALL vdra
   IF ( Sscell/=0 ) Sdr2 = 1
   IF ( Vdrreq==0 ) RETURN
   mcb(1) = Infile
   CALL rdtrl(mcb)
   IF ( mcb(1)==Infile ) CALL vdrb(Infile,Outfle,Iadisp)
   IF ( App(1)/=Trn(1) ) RETURN
   mcb(1) = Pnl
   CALL rdtrl(mcb)
   IF ( mcb(1)/=Pnl ) RETURN
   CALL vdrb(Pnl,Opnl1,Ipnl)
END SUBROUTINE vdr