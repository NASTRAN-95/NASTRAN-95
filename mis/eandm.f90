
SUBROUTINE eandm(Itype,Ido,Nextz,Lcore,Nbdys,All,Nelout)
   IMPLICIT NONE
   REAL A(4) , Ecpt(200) , Z(1)
   INTEGER Ii , Incr , Incur , Irow , Ita , Itb , Ithrml , Iz(1) , Jj , Ksystm(64) , Last , Ne(1) , Necpt(1) , Nelems , Nrowsp ,    &
         & Outpt , Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Nrowsp
   COMMON /emecpt/ Ecpt
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /packx / Ita , Itb , Ii , Jj , Incur
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
   REAL All
   INTEGER Ido , Itype , Lcore , Nbdys , Nelout , Nextz
   INTEGER buf1 , eltype , est , estwds , file , hcflds , icore , idx , iflag , ijk , istart , iwords , jj1 , kcount , mcb(7) ,     &
         & mcb1(7) , mcb2(7) , n , nam(2) , name(2) , ncount , nel , ngrids , nwords , remfls , scr6 , slt
   LOGICAL done
!
!     COMPUTES ADDITIONAL LOAD IN ZIEKIEWICZ PAPER DUE TO SPECIFIED
!     MAGNETIC FIELD OR CURRENT LOOP
!
!     ITYPE = 20  SPCFLD
!     ITYPE = 21  CEMLOOP
!     ITYPE = 22  GEMLOOP
!     ITYPE = 23  MDIPOLE
!     ITYPE = 24  REMFLUX
!     IDO   = NUMBER OF CARDS OF PRESENT TYPE
!     NEXTZ = NEXT AVAILABLE POINTER INTO OPEN CORE
!     LAST AVAILABLE POINTER INTO OPEN CORE
!     *** ALL CEMLOOP, SPCFLD, GEMLOOP, AND MDIPOLE CARDS WERE COMBINED
!     INTO ONE SPCFLD-TYPE CARD WITH 3*NROWSP WORDS-HCX, HCY, HCZ AT
!     EACH POINT AND IS INDICATED BY ITYPE =-20. THESE 3*NROWSP WORDS
!     ARE WRITTEN TO HCFLDS FOR LATER USE. THE OTHER CARDS ARE STILL ON
!     SLT FOR USE IN THE NUMERICAL INTEGRATION.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Z(1),Iz(1)) , (Ksystm(56),Ithrml) , (Ecpt(1),Necpt(1))
   DATA nam/4HEAND , 4HM   /
   DATA est , slt , hcflds , remfls , scr6/105 , 205 , 304 , 305 , 306/
   DATA mcb/304 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA mcb1/305 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA done/.FALSE./
!
!     CHECK IF THERMAL FORMULATION
!
   IF ( Ithrml==0 ) RETURN
!
!     READ A CARD TYPE FROM SLT. TYPE=-20 IS THE COMBINATION HC FOR ALL
!     CARD TYPES EXCEPT REMFLUX AND SIGNIFIES END OF A SUBCASE. READ AND
!     PACK IT. SAME FOR TYPE = 24(REMFLUX). FOR TYPES 20-24, COMPUTE
!     LOAD = INTEGRAL(GRAD(NI)*MU*HC)*D(VOL). WE WILL USE NUMERICAL
!     INTEGRATION FOR ITYPE=24-REMFLUX-ONLY ONE CARD GIVING FLUX IN
!     EACH ELEMENT COMPUTE INTEGRAL(GRAD NI*BR)*D(VOL)
!
   buf1 = Lcore - Sysbuf + 1
   icore = buf1 - 1
   IF ( Nextz>buf1 ) GOTO 700
!
   IF ( Itype==-20 .OR. Itype==24 ) THEN
      IF ( Ido/=1 ) GOTO 400
!
!     END OF SUBCASE-WRAP UP SCR6 AND CALL HCCOM TO COMBINE CENTROID
!     RESULTS IF NOT REMFLUX, CALL HCCOM NOW.IF REMFLUX, WAIT UNTIL
!     KCOUNT IS SET.
!
      CALL close(scr6,1)
      IF ( Itype==-20 ) CALL hccom(Itype,Lcore,icore,Nextz,kcount)
      Jj = Nrowsp
!
!     ITYPE=-20 OR +24--END OF SUBCASE. IF +24, WRITE ZEROS TO HCFLDS
!     AND HCCENS AND REMFLUX VECTOR TO REMFLS. THEN CONTINUE ON TO
!     COMPUTE LOADS. IF ITYPE=-20, WRITE ZRROS TO REMFLS, GRID POINT
!     HC VALUES TO HCFLDS AND CENTROIDAL VALUES TO HCCENS (ALREADY DONE
!     IN HCCOM). FOR ITYPE=-20, NO FURTHER PROCESSING IS DONE SINCE
!     LOADS HAVE ALREADY BEEN COMPUTED.
!
      Ita = 1
      Itb = 1
      Ii = 1
      Jj = 3*Nrowsp
      Incur = 1
      mcb(3) = Jj
      mcb2(1) = est
      CALL rdtrl(mcb2)
      nel = mcb2(2)
      jj1 = 3*nel
      mcb1(3) = jj1
!
!     READ IN THE ONE SPCFLD OR REMFLUX-TYPE CARD
!
      nwords = 3*Nrowsp
      IF ( Itype==24 ) THEN
         nwords = 3*nel
         Jj = nwords
         jj1 = 3*Nrowsp
      ENDIF
      istart = Nextz
      IF ( Nextz+nwords-1>icore ) GOTO 700
      CALL fread(slt,Z(Nextz),nwords,0)
!
!     CREATE A ZERO VECTOR FOR EITHER REMFLS OR HCFLDS(WHICHEVER IS NOT
!     USED IN THIS SET ID-REMEMBER THAT SPCFLD AND REMFLUX CANNOT HAVE
!     THE SAME SET ID
!
!     PACK THE 3*NROWSP HC FIELD OUT TO BE USED LATER BY EMFLD. HCFLDS
!     WILL CONTAIN ONE COLUMN PER CASE CONTROL SIMPLE SELECTION
!     (SIMPLE LOADS ON LOAD CARDS ARE INCLUDED). COMBIN WILL COMBINE
!     FOR LOAD BULK DATA CARDS AND PUT LOADS IN ORDER OF SELECTION ONTO
!     HCFL (SAME HOLDS FOR 3*NEL WORDS OF REMFLS)
!
      IF ( Itype==24 ) THEN
         CALL pack(Z(Nextz),remfls,mcb1)
         CALL wrttrl(mcb1)
         Jj = jj1
         CALL bldpk(1,1,hcflds,0,0)
         CALL bldpkn(hcflds,0,mcb)
         CALL wrttrl(mcb)
      ELSE
         CALL pack(Z(Nextz),hcflds,mcb)
         CALL wrttrl(mcb)
         Jj = jj1
         CALL bldpk(1,1,remfls,0,0)
         CALL bldpkn(remfls,0,mcb1)
         CALL wrttrl(mcb1)
      ENDIF
!
!     RETURN JJ TO VALUE EXPECTED IN EXTERN
!
      Jj = Nrowsp
      IF ( Itype==-20 ) RETURN
   ENDIF
!
!     GET INFO FROM EST
!
   file = est
   CALL gopen(est,Z(buf1),0)
   ncount = 0
   IF ( .NOT.done ) kcount = 0
!
!     READ IN ALL CARDS OF THIS TYPE FOR THIS SUBCASE. NO NEED TO READ
!     IN THE ONE REMFLUX CARD SINCE IT WAS DONE ABOVE.
!
   ijk = Itype - 19
   IF ( ijk==2 ) THEN
      iwords = 12
   ELSEIF ( ijk==3 ) THEN
      iwords = 48
   ELSEIF ( ijk==4 ) THEN
      iwords = 9
   ELSEIF ( ijk==5 ) THEN
      GOTO 100
   ELSE
      iwords = 3*Nrowsp
      IF ( Ido/=1 ) GOTO 400
   ENDIF
   nwords = iwords*Ido
   IF ( Nextz+nwords-1>icore ) GOTO 700
   CALL fread(slt,Z(Nextz),nwords,0)
   istart = Nextz
!
 100  CALL read(*300,*600,est,eltype,1,0,iflag)
   idx = (eltype-1)*Incr
   estwds = Ne(idx+12)
   ngrids = Ne(idx+10)
   name(1) = Ne(idx+1)
   name(2) = Ne(idx+2)
   DO
!
      CALL read(*500,*100,est,Ecpt,estwds,0,iflag)
      ncount = ncount + 1
      IF ( .NOT.(done) ) THEN
         IF ( eltype<65 ) kcount = kcount + 3
         IF ( eltype==65 ) kcount = kcount + 27
         IF ( eltype==66 .OR. eltype==67 ) kcount = kcount + 63
         IF ( eltype==80 ) kcount = kcount + 27
      ENDIF
!
      IF ( eltype>80 ) EXIT
      IF ( eltype==2 .OR. eltype==4 .OR. eltype==5 .OR. eltype==7 .OR. eltype==8 .OR. eltype==11 .OR. eltype==12 .OR.               &
         & eltype==13 .OR. eltype==14 .OR. eltype==15 .OR. eltype==20 .OR. eltype==21 .OR. eltype==22 .OR. eltype==23 .OR.          &
         & eltype==24 .OR. eltype==25 .OR. eltype==26 .OR. eltype==27 .OR. eltype==28 .OR. eltype==29 .OR. eltype==30 .OR.          &
         & eltype==31 .OR. eltype==32 .OR. eltype==33 .OR. eltype==35 .OR. eltype==38 .OR. eltype==43 .OR. eltype==44 .OR.          &
         & eltype==45 .OR. eltype==46 .OR. eltype==47 .OR. eltype==48 .OR. eltype==49 .OR. eltype==50 .OR. eltype==51 .OR.          &
         & eltype==52 .OR. eltype==53 .OR. eltype==54 .OR. eltype==55 .OR. eltype==56 .OR. eltype==57 .OR. eltype==58 .OR.          &
         & eltype==59 .OR. eltype==60 .OR. eltype==61 .OR. eltype==62 .OR. eltype==63 .OR. eltype==64 .OR. eltype==68 .OR.          &
         & eltype==69 .OR. eltype==70 .OR. eltype==71 .OR. eltype==72 .OR. eltype==73 .OR. eltype==74 .OR. eltype==75 .OR.          &
         & eltype==76 .OR. eltype==77 .OR. eltype==78 .OR. eltype==79 ) EXIT
      IF ( eltype==6 .OR. eltype==9 .OR. eltype==16 .OR. eltype==17 .OR. eltype==18 .OR. eltype==19 .OR. eltype==36 .OR.            &
         & eltype==37 .OR. eltype==80 ) THEN
         CALL em2d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
      ELSEIF ( eltype==39 .OR. eltype==40 .OR. eltype==41 .OR. eltype==42 .OR. eltype==65 .OR. eltype==66 .OR. eltype==67 ) THEN
         CALL em3d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
      ELSE
!
         CALL em1d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
      ENDIF
   ENDDO
!
   WRITE (Outpt,99001) Ufm , name
99001 FORMAT (A23,', ELEMENT TYPE ',2A4,' WAS USED IN AN E AND M ','PROBLEM. NOT A LEGAL TYPE')
 200  CALL mesage(-61,0,0)
!
!     DONE
!
 300  CALL close(est,1)
   IF ( Itype==24 ) THEN
      CALL hccom(Itype,Lcore,icore,Nextz,kcount)
      Jj = Nrowsp
   ELSE
      CALL write(scr6,0,0,1)
   ENDIF
   done = .TRUE.
   RETURN
!
!     FATAL ERROR MESSAGES
!
 400  WRITE (Outpt,99002) Ufm , nam
99002 FORMAT (A23,', LOGIC ERROR IN SUBROUTINE ',2A4,'. ONLY ONE SPCFLD OR REMFLUX SHOULD NOW EXIST')
   GOTO 200
!
 500  n = -2
   GOTO 800
 600  n = -3
   GOTO 800
 700  n = -8
   file = 0
 800  CALL mesage(n,file,nam)
END SUBROUTINE eandm