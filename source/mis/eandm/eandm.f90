!*==eandm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eandm(Itype,Ido,Nextz,Lcore,Nbdys,All,Nelout)
   USE c_blank
   USE c_emecpt
   USE c_gpta1
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zblpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
   INTEGER :: Ido
   INTEGER :: Nextz
   INTEGER :: Lcore
   INTEGER :: Nbdys
   REAL :: All
   INTEGER :: Nelout
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , eltype , estwds , file , icore , idx , iflag , ijk , istart , ithrml , iwords , jj1 , kcount , n , ncount ,    &
            & nel , ngrids , nwords , outpt , sysbuf
   LOGICAL , SAVE :: done
   INTEGER , SAVE :: est , hcflds , remfls , scr6 , slt
   INTEGER , DIMENSION(1) :: iz , necpt
   INTEGER , DIMENSION(7) , SAVE :: mcb , mcb1
   INTEGER , DIMENSION(7) :: mcb2
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(2) :: name
   EXTERNAL bldpk , bldpkn , close , em1d , em2d , em3d , fread , gopen , hccom , mesage , pack , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK IF THERMAL FORMULATION
!
         IF ( ithrml==0 ) RETURN
!
!     READ A CARD TYPE FROM SLT. TYPE=-20 IS THE COMBINATION HC FOR ALL
!     CARD TYPES EXCEPT REMFLUX AND SIGNIFIES END OF A SUBCASE. READ AND
!     PACK IT. SAME FOR TYPE = 24(REMFLUX). FOR TYPES 20-24, COMPUTE
!     LOAD = INTEGRAL(GRAD(NI)*MU*HC)*D(VOL). WE WILL USE NUMERICAL
!     INTEGRATION FOR ITYPE=24-REMFLUX-ONLY ONE CARD GIVING FLUX IN
!     EACH ELEMENT COMPUTE INTEGRAL(GRAD NI*BR)*D(VOL)
!
         buf1 = Lcore - sysbuf + 1
         icore = buf1 - 1
         IF ( Nextz>buf1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         IF ( Itype==-20 .OR. Itype==24 ) THEN
            IF ( Ido/=1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     END OF SUBCASE-WRAP UP SCR6 AND CALL HCCOM TO COMBINE CENTROID
!     RESULTS IF NOT REMFLUX, CALL HCCOM NOW.IF REMFLUX, WAIT UNTIL
!     KCOUNT IS SET.
!
            CALL close(scr6,1)
            IF ( Itype==-20 ) CALL hccom(Itype,Lcore,icore,Nextz,kcount)
            jj = nrowsp
!
!     ITYPE=-20 OR +24--END OF SUBCASE. IF +24, WRITE ZEROS TO HCFLDS
!     AND HCCENS AND REMFLUX VECTOR TO REMFLS. THEN CONTINUE ON TO
!     COMPUTE LOADS. IF ITYPE=-20, WRITE ZRROS TO REMFLS, GRID POINT
!     HC VALUES TO HCFLDS AND CENTROIDAL VALUES TO HCCENS (ALREADY DONE
!     IN HCCOM). FOR ITYPE=-20, NO FURTHER PROCESSING IS DONE SINCE
!     LOADS HAVE ALREADY BEEN COMPUTED.
!
            ita = 1
            itb = 1
            ii = 1
            jj = 3*nrowsp
            incur = 1
            mcb(3) = jj
            mcb2(1) = est
            CALL rdtrl(mcb2)
            nel = mcb2(2)
            jj1 = 3*nel
            mcb1(3) = jj1
!
!     READ IN THE ONE SPCFLD OR REMFLUX-TYPE CARD
!
            nwords = 3*nrowsp
            IF ( Itype==24 ) THEN
               nwords = 3*nel
               jj = nwords
               jj1 = 3*nrowsp
            ENDIF
            istart = Nextz
            IF ( Nextz+nwords-1>icore ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fread(slt,z(Nextz),nwords,0)
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
               CALL pack(z(Nextz),remfls,mcb1)
               CALL wrttrl(mcb1)
               jj = jj1
               CALL bldpk(1,1,hcflds,0,0)
               CALL bldpkn(hcflds,0,mcb)
               CALL wrttrl(mcb)
            ELSE
               CALL pack(z(Nextz),hcflds,mcb)
               CALL wrttrl(mcb)
               jj = jj1
               CALL bldpk(1,1,remfls,0,0)
               CALL bldpkn(remfls,0,mcb1)
               CALL wrttrl(mcb1)
            ENDIF
!
!     RETURN JJ TO VALUE EXPECTED IN EXTERN
!
            jj = nrowsp
            IF ( Itype==-20 ) RETURN
         ENDIF
!
!     GET INFO FROM EST
!
         file = est
         CALL gopen(est,z(buf1),0)
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
            GOTO 20
         ELSE
            iwords = 3*nrowsp
            IF ( Ido/=1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         nwords = iwords*Ido
         IF ( Nextz+nwords-1>icore ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fread(slt,z(Nextz),nwords,0)
         istart = Nextz
!
 20      CALL read(*40,*80,est,eltype,1,0,iflag)
         idx = (eltype-1)*incr
         estwds = ne(idx+12)
         ngrids = ne(idx+10)
         name(1) = ne(idx+1)
         name(2) = ne(idx+2)
         SPAG_Loop_1_1: DO
!
            CALL read(*60,*20,est,ecpt,estwds,0,iflag)
            ncount = ncount + 1
            IF ( .NOT.(done) ) THEN
               IF ( eltype<65 ) kcount = kcount + 3
               IF ( eltype==65 ) kcount = kcount + 27
               IF ( eltype==66 .OR. eltype==67 ) kcount = kcount + 63
               IF ( eltype==80 ) kcount = kcount + 27
            ENDIF
!
            IF ( eltype>80 ) EXIT SPAG_Loop_1_1
            IF ( eltype==2 .OR. eltype==4 .OR. eltype==5 .OR. eltype==7 .OR. eltype==8 .OR. eltype==11 .OR. eltype==12 .OR.         &
               & eltype==13 .OR. eltype==14 .OR. eltype==15 .OR. eltype==20 .OR. eltype==21 .OR. eltype==22 .OR. eltype==23 .OR.    &
               & eltype==24 .OR. eltype==25 .OR. eltype==26 .OR. eltype==27 .OR. eltype==28 .OR. eltype==29 .OR. eltype==30 .OR.    &
               & eltype==31 .OR. eltype==32 .OR. eltype==33 .OR. eltype==35 .OR. eltype==38 .OR. eltype==43 .OR. eltype==44 .OR.    &
               & eltype==45 .OR. eltype==46 .OR. eltype==47 .OR. eltype==48 .OR. eltype==49 .OR. eltype==50 .OR. eltype==51 .OR.    &
               & eltype==52 .OR. eltype==53 .OR. eltype==54 .OR. eltype==55 .OR. eltype==56 .OR. eltype==57 .OR. eltype==58 .OR.    &
               & eltype==59 .OR. eltype==60 .OR. eltype==61 .OR. eltype==62 .OR. eltype==63 .OR. eltype==64 .OR. eltype==68 .OR.    &
               & eltype==69 .OR. eltype==70 .OR. eltype==71 .OR. eltype==72 .OR. eltype==73 .OR. eltype==74 .OR. eltype==75 .OR.    &
               & eltype==76 .OR. eltype==77 .OR. eltype==78 .OR. eltype==79 ) EXIT SPAG_Loop_1_1
            IF ( eltype==6 .OR. eltype==9 .OR. eltype==16 .OR. eltype==17 .OR. eltype==18 .OR. eltype==19 .OR. eltype==36 .OR.      &
               & eltype==37 .OR. eltype==80 ) THEN
               CALL em2d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
            ELSEIF ( eltype==39 .OR. eltype==40 .OR. eltype==41 .OR. eltype==42 .OR. eltype==65 .OR. eltype==66 .OR. eltype==67 )   &
                   & THEN
               CALL em3d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
            ELSE
!
               CALL em1d(eltype,istart,Itype,ncount,Ido,iwords,Nbdys,All,Nelout)
            ENDIF
         ENDDO SPAG_Loop_1_1
!
         WRITE (outpt,99001) ufm , name
99001    FORMAT (A23,', ELEMENT TYPE ',2A4,' WAS USED IN AN E AND M ','PROBLEM. NOT A LEGAL TYPE')
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-61,0,0)
!
!     DONE
!
 40      CALL close(est,1)
         IF ( Itype==24 ) THEN
            CALL hccom(Itype,Lcore,icore,Nextz,kcount)
            jj = nrowsp
         ELSE
            CALL write(scr6,0,0,1)
         ENDIF
         done = .TRUE.
         RETURN
      CASE (3)
!
!     FATAL ERROR MESSAGES
!
         WRITE (outpt,99002) ufm , nam
99002    FORMAT (A23,', LOGIC ERROR IN SUBROUTINE ',2A4,'. ONLY ONE SPCFLD OR REMFLUX SHOULD NOW EXIST')
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 60      n = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      n = -3
         spag_nextblock_1 = 5
      CASE (4)
         n = -8
         file = 0
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE eandm
