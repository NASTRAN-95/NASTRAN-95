
SUBROUTINE ttlpge(Topt)
   IMPLICIT NONE
   INTEGER Idate(3) , Ipage , Ksystm(100) , Lpch , Machx , Nlpp , Nout
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   REAL Z(1)
   COMMON /chmach/ Mchnam , Machos
   COMMON /machin/ Machx
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER Topt
   INTEGER card(20) , fchar , i , ncmnam , ncmos
   CHARACTER*28 mchttl
   CHARACTER*15 vn
!
!
   !>>>>EQUIVALENCE (Ksystm(2),Nout) , (Ksystm(42),Idate(1)) , (Ksystm(9),Nlpp) , (Ksystm(11),Ipage) , (Ksystm(91),Lpch)
!
!     ASSEMBLE MCHTTL AND VN LINE
!
   mchttl = ' '
   vn = ' '
   ncmnam = index(Mchnam,' ') - 1
   IF ( ncmnam<=-1 ) ncmnam = 11
   ncmos = index(Machos,' ') - 1
   IF ( ncmos<=-1 ) ncmos = 7
   fchar = (11-ncmnam)/2 + 1
   mchttl(fchar:fchar+ncmnam+16) = Mchnam(1:ncmnam)//' COMPUTER SYSTEMS'
   fchar = (7-ncmos)/2 + 1
   vn(fchar:fchar+ncmos+7) = Machos(1:ncmos)//' VERSION'
!
!     SET TOPT DEFAULT TO +2 FOR THE MAIN FRAMES, OR TO -1 FOR UNIX
!     BASE WORKSTATION
!
   IF ( Topt==-9 ) THEN
      Topt = +2
      IF ( Machx>=6 .AND. Machx<=20 ) Topt = -1
   ENDIF
!
!     BRANCH ON OPTION
!
!     TOPT = 1, PRINT ONE NASTRAN LOGO TITLE PAGE
!          = 2, PRINT TWO NASTRAN LOGO TITLE PAGES
!          = 3, PRINT DUMMY MESSAGE AND ONE SHORT TITLE PAGE
!          = 4, READ AND PRINT ONE LINE USER INPUT CARD AND PRINT ONE
!               NASTRAN SHORT TITLE PAGE
!          = 0, OR .GE.5, NO TITLE PAGE PRINTED
!          = NEGATIVE INTEGER, PRINT ONE NASTRAN SHORT TITLE PAGE
!
   IF ( Topt/=2 .AND. Topt/=1 ) THEN
!
      IF ( Topt<0 ) THEN
      ELSEIF ( Topt==0 ) THEN
         GOTO 100
      ELSEIF ( Topt<4 ) THEN
!
!     TOPT = 3
!
         WRITE (Nout,99017)
         WRITE (Nout,99001)
99001    FORMAT (' THIS COMMENT CAN BE USED TO IDENTIFY LOCAL FIXES - ','TO CHANGE, UPDATE DECK TTLPGE.')
      ELSEIF ( Topt==4 ) THEN
!
!     TOPT = 4
!
         WRITE (Nout,99017)
         CALL xread(*100,card)
         WRITE (Nout,99002) card
99002    FORMAT (1X,20A4)
      ELSE
         GOTO 100
      ENDIF
!
!     TOPT = NEGATIVE (AND 3, AND 4)
!
      IF ( Ipage<=0 ) CALL page1
      WRITE (Nout,99003) mchttl
99003 FORMAT (//////34X,4H****,/32X,1H*,6X,1H*,/31X,1H*,8X,1H*,/31X,16H*  N A S T R A N,/31X,1H*,8X,1H*,/32X,1H*,6X,1H*,/34X,4H****,&
            & ///25X,A28)
      WRITE (Nout,99004) vn , Idate(2) , Idate(3)
99004 FORMAT (27X,A20,//26X,17HSYSTEM RELEASE - ,A3,A2,4H ED.)
      WRITE (Nout,99005)
99005 FORMAT (/32X,'DISTRIBUTED BY',//9X,'COMPUTER SOFTWARE MANAGE','MENT AND INFORMATION CENTER (COSMIC)',/17X,'UNIVERSITY ',      &
             &'OF GEORGIA, ATHENS, GEORGIA 30602',/17X,'PHONE: (706)542-3265',6X,'FAX: (706)542-4807')
   ELSE
!
!     TOPT = 1, OR 2
!
      DO i = 1 , Topt
         IF ( Ipage<=0 .OR. i==2 ) WRITE (Nout,99017)
         IF ( Nlpp>48 ) WRITE (Nout,99006)
99006    FORMAT (///)
         WRITE (Nout,99007) mchttl , vn
99007    FORMAT (34X,17(1HM),/28X,29(1HM),/25X,35(1HM),/22X,20(1HM),1X,20(1HM),22X,1H/,6X,A28,/20X,45(1HM),18X,2H//,9X,A20)
         WRITE (Nout,99008)
99008    FORMAT (18X,16(1HM),2X,31(1HM),14X,3H///,/16X,53(1HM),10X,4(1H/),/14X,13(1HM),9X,35(1HM),6X,5(1H/))
         WRITE (Nout,99009) Idate(2) , Idate(3)
99009    FORMAT (13X,12(1HM),2X,9(1HM),2X,34(1HM),3X,6(1H/),9X,3X,18HSYSTEM RELEASE  - ,A3,A2,4H ED.)
         WRITE (Nout,99010)
99010    FORMAT (12X,12(1HM),1X,13(1HM),3X,15(1HM),2X,15(1HM),6(1H/),/11X,12(1HM),1X,17(1HM),2X,28(1HM),6(1H/),/10X,13(1HM),1X,     &
                &19(1HM),2X,24(1HM),6(1H/),/9X,5(1HM),2X,7(1HM),1X,13(1HM),1X,7(1HM),2X,19(1HM),8(1H/),2HMM,/9X,14(1HM),1X,23(1HM), &
               & 2X,14(1HM),8(1H/),1H-,4(1HM),43X,1H*,1X,1H*,1X,1H*,/8X,16(1HM),1X,24(1HM),1X,9(1HM),9(1H/),2H--,7(1HM),41X,1H*,5X, &
                &1H*)
         WRITE (Nout,99011)
99011    FORMAT (8X,16(1HM),1X,25(1HM),2X,4(1HM),10(1H/),2H--,9(1HM),41X,1H*,2X,1HR,2X,1H*,/8X,16(1HM),1X,27(1HM),1X,1HM,8(1H/),    &
                &4HMM--,11(1HM),41X,1H*,5X,1H*,/7X,8(1HM),4X,6(1HM),4X,5(1HM),5X,10(1HM),8X,4H//MM,11X,2HMM,3X,6(1HM),7X,5(1HM),8X, &
               & 4(1HM),6X,4(1HM),2X,1H*,1X,1H*,1X,1H*,/7X,9(1HM),4X,6(1HM),2X,7(1HM),4X,6(1HM),14H///   /// M  M,                  &
                &25HM- MMM   MMM MMM  M   MMM,7X,4(1HM),9X,4(1HM),6X,2HMM)
         WRITE (Nout,99012)
99012    FORMAT (7X,9(1HM),5X,5(1HM),2X,6(1HM),3H  M,3X,8(1H/),3X,4(1HM),5H MM--,5(1HM),3X,7(1HM),21H  M    MMM     MM MMM,8X,5(1HM)&
               & ,5X,2HMM,/7X,9(1HM),2X,1HM,4X,5HMMM  ,4(1HM),6H// ///,3X,5(1H/),13HMMM   MMMM-- ,6(1HM),3X,7(1HM),                 &
                &41H  M    MMM     M   MMM       MM MMMM   MM,/7X,9(1HM),2X,2HMM,4X,2HMM,3X,4(1H/),2X,3H///,4X,8(1HM),4X,4H--M ,    &
                &7(1HM),3X,7(1HM),2X,1HM,3X,3HMMM,5X,2HMM,3X,4(1HM),6X,2HMM,2X,4(1HM),2X,2HMM)
         WRITE (Nout,99013)
99013    FORMAT (7X,9(1HM),2X,4(1HM),6X,11H/ /// ///MM,4X,8(1HM),4H---M,4X,6(1HM),3X,7(1HM),2X,6(1HM),6X,1HM,5X,4(1HM),5X,2HMM,4X,  &
                &6(1HM),/7X,9(1HM),2X,5(1H/),5X,4H// M,11X,6(1HM),3H---,4(1HM),4X,5(1HM),3X,7(1HM),7H  M MMM,6X,11(1HM),5X,2HMM,5X, &
               & 5(1HM),/7X,2HMM,7(1H/),2X,6(1HM),4X,3HMMM,2X,7(1HM),4X,7HMMM----,4HMMMM,4X,6HM MMMM,3X,7(1HM),8H  M  MMM,4X,2HMM,  &
               & 7X,4(1HM),4X,2HMM,6X,4(1HM),/5X,4(1H/),6(1HM),4X,7(1HM),2X,2HMM,4X,5(1HM),5X,5H----M,9X,6HMM MMM,5X,5(1HM),3X,2HMM,&
               & 3X,2HMM,2X,4(1HM),5X,6(1HM),2X,4(1HM),7X,2HMM)
         WRITE (Nout,99014)
99014    FORMAT (3X,2H//,3X,26(1HM),1X,6(1HM),4(1H-),16(1HM),1X,15(1HM),6X,3HMMM,/8X,27(1HM),7H MM----,19(1HM),1X,15(1HM),/8X,      &
               & 27(1HM),3H---,23(1HM),1X,15(1HM),/9X,24(1HM),7H---MM  ,22(1HM),1X,13(1HM),/9X,22(1HM),2H--,6(1HM),4X,19(1HM),1X,   &
                &5(1HM),2X,6(1HM),/10X,19(1HM),3H---,7(1HM),4X,19(1HM),1X,12(1HM),/11X,9(1HM),1X,6(1HM),2H--,33(1HM),1X,11(1HM),    &
               & /12X,13(1HM),3H---,33(1HM),1X,11(1HM))
         WRITE (Nout,99015)
99015    FORMAT (13X,11(1HM),2H--,22(1HM),2X,9(1HM),2X,11(1HM),/14X,8(1HM),2H--,26(1HM),9X,12(1HM),/16X,5(1HM),2H--,46(1HM),24X,    &
                &14HDISTRIBUTED BY,/18X,4HMM--,13(1HM),2X,30(1HM),/19X,1H-,45(1HM),5X,                                              &
                &51HCOMPUTER SOFTWARE MANAGEMENT AND INFORMATION CENTER,9H (COSMIC),/18X,1H-,3X,41(1HM),26X,                        &
                &22HUNIVERSITY OF  GEORGIA,/17X,1H-,7X,35(1HM),29X,22HATHENS, GEORGIA  30602,/28X,29(1HM),/1X,14X,19X,17(1HM),28X,  &
                &40HPHONE: (706)542-3265   FAX: (706)542-480,1H7)
99016    FORMAT (1H+,93X,A4,10H VERSION -,I5,1HK)
      ENDDO
   ENDIF
!
!     CALL NSINFO TO PRINTOUT INSTALLATION-CENTER-TO-USER MESSAGES,
!     FROM THE THIRD SECTION OF THE NASINFO FILE
!
 100  CALL nsinfo(3)
99017 FORMAT (1H1)
!
END SUBROUTINE ttlpge