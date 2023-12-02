!*==sdr2b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2b
!
!     SDR2B PROCESSES THE EST. FOR EACH ELEMENT IN THE MASTER SET,
!     PRELIMINARY COMPUTATIONS ARE MADE. IF THE PROBLEM CONTAINS EXTRA
!     POINTS, SIL NOS. ARE CONVERTED TO SILD NOS. THE DATA IS WRITTEN
!     ON ESTA FOR INPUT TO SDR2D WHERE FINAL STRESS AND FORCE RECOVERY
!     COMPUTATIONS ARE MADE.
!
!
   USE c_blank
   USE c_gpta1
   USE c_hmatdd
   USE c_names
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_sdr2x5
   USE c_sdr2x6
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: anyout , heat , reject
   REAL , DIMENSION(1) :: bufr , zz
   INTEGER :: eltype , flag , i , id , idsave , ielem , imat , ioutpt , ipr , iprec , itabl , itherm , jltype , k , khi , klo , kn ,&
            & knsil , kx , l , local , m8 , n , n12 , n1mat , n2mat , ngps , nip , noep , nsil , nwds , nwdsa , ret1 , sysbuf
   INTEGER , SAVE :: iz1st , star
   INTEGER , DIMENSION(2) , SAVE :: kdefrm , mmre , name
   EXTERNAL close , delset , fwdrec , locate , mesage , open , prehma , preloc , premat , pretrs , rdtrl , read , saxif1 , sbar1 ,  &
          & scone1 , sdhtf1 , sdum11 , sdum21 , sdum31 , sdum41 , sdum51 , sdum61 , sdum71 , sdum81 , sdum91 , selas1 , selbo1 ,    &
          & sihex1 , spanl1 , sqdm11 , sqdm21 , sqdme1 , sqdpl1 , squd41 , srod1 , ss2d81 , sslot1 , ssold1 , stord1 , stpax1 ,     &
          & strap1 , strax1 , strbs1 , stri31 , strir1 , strm61 , strme1 , strp11 , strpl1 , strqd1 , strsl1 , stube1 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!WKBI 7/94 SPR 94007
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt) , (Ksystm(55),Iprec) , (Ksystm(56),Itherm) , (Z(1),Zz(1)) , (Bufr(1),Buf(1))
   DATA name/4HSDR2 , 4HB   / , star/4H* * /
   DATA kdefrm/104 , 1/
   DATA iz1st/1/
!WKBI 7/94 SPR 94007
   DATA mmre/4HMMRE , 4HIGEN/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!            IZ1ST  IS THE START OF OPEN CORE AVAILABLE
!
!
!     IF APPROACH IS COMPLEX EIGENVALUES, FREQUENCY OR TRANSIENT
!     RESPONSE, TEST FOR EXTRA POINTS. IF PRESENT, READ EQUIVALENCE
!     TABLE (SIL,SILD) INTO CORE.
!
         CALL delset
         heat = .FALSE.
         IF ( itherm/=0 ) heat = .TRUE.
         isopl = 0
         icstm = iz1st
         m8 = -8
         noep = 0
!WKBR 7/94 SPR 94007
!     IF (APP(1).EQ.CEI(1) .OR. APP(1).EQ.FRQ(1) .OR. APP(1).EQ.TRN(1))
!    1    GO TO 20
         IF ( app(1)/=cei(1) .AND. app(1)/=frq(1) .AND. app(1)/=trn(1) .AND. app(1)/=mmre(1) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icb(1) = sil
         CALL rdtrl(icb)
         noep = icb(3)
         IF ( noep==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = sil
         CALL open(*160,sil,z(buf1),rdrew)
         CALL fwdrec(*180,sil)
         CALL fwdrec(*180,sil)
         CALL read(*180,*20,sil,z,buf2,1,nsil)
         CALL mesage(m8,0,nam)
 20      CALL close(sil,clsrew)
         knsil = nsil/2
         icstm = nsil + 1
         IF ( nsil>=mset ) THEN
            mset = buf2 - 1
            all = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ THE CSTM INTO CORE (IF PRESENT).
!
         ncstm = 0
         file = cstm
         CALL open(*60,cstm,z(buf1),rdrew)
         CALL fwdrec(*180,cstm)
         CALL read(*180,*40,cstm,z(icstm),buf2-icstm,1,ncstm)
         CALL mesage(m8,0,nam)
 40      CALL close(cstm,clsrew)
         CALL pretrs(z(icstm),ncstm)
 60      imat = icstm + ncstm
         IF ( imat>=mset ) THEN
            mset = buf2 - 1
            all = 1
         ENDIF
!
!     READ MATERIAL PROPERTY DATA INTO CORE.
!
         n1mat = buf2 - imat
         IF ( .NOT.heat ) THEN
!
            CALL premat(z(imat),z(imat),z(buf1),n1mat,n2mat,mpt,dit)
         ELSE
!
!     FOR HEAT PROBLEMS ONLY, -HMAT- ROUTINE IS USED.
!
            ihmat = imat
            nhmat = buf1 + sysbuf
            mptmpt = mpt
            idit = dit
            CALL prehma(z)
            n2mat = nhmat - ihmat + 1 - 2*(sysbuf+1)
         ENDIF
         IF ( imat+n2mat>=mset ) THEN
            mset = buf2 - 1
            all = 1
         ENDIF
!
!     OPEN EST AND ESTA.
!
         file = est
         CALL open(*240,est,z(buf1),rdrew)
         CALL fwdrec(*180,est)
         file = esta
         CALL open(*160,esta,z(buf2),wrtrew)
         file = est
         kwdest = 0
         kwdedt = 0
         kwdgpt = 0
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ ELEMENT TYPE. SET PARAMETERS AS A FUNCTION OF ELEM TYPE.
!
         CALL read(*120,*200,est,eltype,1,0,flag)
         IF ( eltype<1 .OR. eltype>nelem ) THEN
!
!     ELEMENT UNDEFINE TO SDR2BD
!
            WRITE (ioutpt,99002) star , star , eltype
            CALL fwdrec(*180,est)
            GOTO 100
         ELSE
            anyout = .FALSE.
            ipr = iprec
            IF ( ipr/=1 ) ipr = 0
            jltype = 2*eltype - ipr
            ielem = (eltype-1)*incr
            nwds = elem(ielem+12)
            nwdsa = elem(ielem+17)
            IF ( heat ) nwdsa = 142
            ngps = elem(ielem+10)
         ENDIF
!
!     READ DATA FOR AN ELEMENT.
!     DETERMINE IF ELEMENT BELONGS TO MASTER SET.
!
 80      CALL read(*180,*100,est,buf,nwds,0,flag)
         DO i = 1 , nwds
            scrtch(100+i) = bufr(i)
         ENDDO
         strspt = 0
         isopl = -1
         idsave = buf(1)
         IF ( all==0 ) THEN
            itabl = mset
            kn = knset
            l = 1
            n12 = 1
            ASSIGN 80 TO ret1
!
!     DECODE ELEMENT ID SINCE THIS IS A CONICAL SHELL PROBLEM
!
            IF ( axic ) buf(1) = buf(1)/1000
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     CALL APPROPRIATE ELEMENT SUBROUTINE.
!
         buf(1) = idsave
!
         IF ( strain ) THEN
!
!     IF THE STRAIN FLAG IS TURNED ON, IGNORE ALL ELEMENTS
!WKBR NCL93012 3/94 EXCEPT CTRIA1, CTRIA2, CQUAD1 AND CQUAD2 ELEMENTS
!     EXCEPT CTRIA1, CTRIA2, CTRIA3, CQUAD1, CQUAD2 AND CQUAD4 ELEMENTS
!
!WKBR NCL93012 3/94     1    ELTYPE.EQ.19) GO TO 112
            IF ( eltype/=6 .AND. eltype/=17 .AND. eltype/=18 .AND. eltype/=19 .AND. eltype/=64 .AND. eltype/=83 ) THEN
               WRITE (ioutpt,99001) swm , elem(ielem+1) , elem(ielem+2)
99001          FORMAT (A27,', STRAIN REQUEST FOR ',2A4,' ELEMENTS WILL',/5X,'NOT BE HONORED AS THIS OUTPUT IS NOT DEFINED FOR THIS '&
                     & ,'ELEMENT TYPE.')
               CALL fwdrec(*180,est)
               GOTO 100
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
         IF ( heat ) THEN
!
!     HEAT PROBLEMS (ALL ELEMENTS).
!
            CALL sdhtf1(eltype,reject)
            IF ( eltype>=65 .AND. eltype<=67 ) THEN
               IF ( eltype==65 .AND. strspt>=9 ) strspt = 0
               IF ( strspt>=21 ) strspt = 0
            ENDIF
            IF ( reject ) THEN
               CALL fwdrec(*180,est)
               GOTO 100
            ENDIF
         ELSE
            local = jltype - 100
            IF ( local<=0 ) THEN
!
!     PAIRED -GO TO- ENTRIES PER ELEMENT SINGLE/DOUBLE PRECISION
!
!             1 CROD      2 C.....    3 CTUBE     4 CSHEAR    5 CTWIST
!
!             6 CTRIA1    7 CTRBSC    8 CTRPLT    9 CTRMEM   10 CONROD
!
!            11 ELAS1    12 ELAS2    13 ELAS3    14 ELAS4    15 CQDPLT
!
!            16 CQDMEM   17 CTRIA2   18 CQUAD2   19 CQUAD1   20 CDAMP1
!
!            21 CDAMP2   22 CDAMP3   23 CDAMP4   24 CVISC    25 CMASS1
!
!            26 CMASS2   27 CMASS3   28 CMASS4   29 CONM1    30 CONM2
!
!            31 PLOTEL   32 C.....   33 C.....   34 CBAR     35 CCONE
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
               IF ( jltype==1 .OR. jltype==2 .OR. jltype==19 .OR. jltype==20 ) THEN
!
                  CALL srod1
               ELSEIF ( jltype==3 .OR. jltype==4 .OR. jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR.               &
                      & jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR.             &
                      & jltype==49 .OR. jltype==50 .OR. jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR.             &
                      & jltype==55 .OR. jltype==56 .OR. jltype==57 .OR. jltype==58 .OR. jltype==59 .OR. jltype==60 .OR.             &
                      & jltype==61 .OR. jltype==62 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR. jltype==66 .OR.             &
                      & jltype==85 .OR. jltype==86 .OR. jltype==87 .OR. jltype==88 .OR. jltype==89 .OR. jltype==90 .OR.             &
                      & jltype==91 .OR. jltype==92 ) THEN
                  WRITE (ioutpt,99002) swm , elem(ielem+1) , elem(ielem+2) , eltype
                  CALL fwdrec(*180,est)
                  GOTO 100
               ELSEIF ( jltype==5 .OR. jltype==6 ) THEN
                  CALL stube1
               ELSEIF ( jltype==7 .OR. jltype==8 ) THEN
                  k = 4
                  CALL spanl1(k)
               ELSEIF ( jltype==9 .OR. jltype==10 ) THEN
                  k = 5
                  CALL spanl1(k)
               ELSEIF ( jltype==11 .OR. jltype==12 ) THEN
                  k = 1
                  CALL strqd1(k)
               ELSEIF ( jltype==13 .OR. jltype==14 ) THEN
                  CALL strbs1(0)
               ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
                  CALL strpl1
               ELSEIF ( jltype==17 .OR. jltype==18 ) THEN
                  CALL strme1(0)
               ELSEIF ( jltype==21 .OR. jltype==22 ) THEN
                  k = 1
                  CALL selas1(k)
               ELSEIF ( jltype==23 .OR. jltype==24 ) THEN
                  k = 2
                  CALL selas1(k)
               ELSEIF ( jltype==25 .OR. jltype==26 ) THEN
                  k = 3
                  CALL selas1(k)
               ELSEIF ( jltype==27 .OR. jltype==28 ) THEN
                  k = 4
                  CALL selas1(k)
               ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
                  CALL sqdpl1
               ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                  CALL sqdme1
               ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
                  k = 2
                  CALL strqd1(k)
               ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
                  k = 4
                  CALL strqd1(k)
               ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
                  k = 3
                  CALL strqd1(k)
               ELSEIF ( jltype==67 .OR. jltype==68 ) THEN
                  CALL sbar1
               ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
                  CALL scone1
               ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                  CALL strir1
               ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                  CALL strap1
               ELSEIF ( jltype==75 .OR. jltype==76 ) THEN
                  CALL stord1
               ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                  CALL ssold1(1)
               ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                  CALL ssold1(2)
               ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                  CALL ssold1(3)
               ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                  CALL ssold1(4)
               ELSEIF ( jltype==93 .OR. jltype==94 ) THEN
                  k = 0
                  CALL saxif1(k)
               ELSEIF ( jltype==95 .OR. jltype==96 ) THEN
                  k = 1
                  CALL saxif1(k)
               ELSEIF ( jltype==97 .OR. jltype==98 ) THEN
                  k = 2
                  CALL saxif1(k)
               ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
                  k = 0
                  CALL sslot1(k)
               ELSE
                  GOTO 90
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!
!            51 CSLOT4   52 CHBDY    53 CDUM1    54 CDUM2    55 CDUM3
!
!            56 CDUM4    57 CDUM5    58 CDUM6    59 CDUM7    60 CDUM8
!
!            61 CDUM9    62 CQDMEM1  63 CQDMEM2  64 CQUAD4   65 CIHEX1
!
!            66 CIHEX2   67 CIHEX3   68 CQUADTS  69 CTRIATS  70 CTRIAAX
!
!            71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!            76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!            81 CELBOW   82 CFTUBE   83 CTRIA3
!
 90         IF ( local==1 .OR. local==2 ) THEN
               k = 1
               CALL sslot1(k)
            ELSEIF ( local==3 .OR. local==4 .OR. local==43 .OR. local==44 .OR. local==51 .OR. local==52 .OR. local==53 .OR.         &
                   & local==54 .OR. local==55 .OR. local==56 .OR. local==57 .OR. local==58 .OR. local==63 .OR. local==64 ) THEN
               WRITE (ioutpt,99002) swm , elem(ielem+1) , elem(ielem+2) , eltype
               CALL fwdrec(*180,est)
               GOTO 100
            ELSEIF ( local==5 .OR. local==6 ) THEN
               CALL sdum11
!
!     IF EXTRA POINTS PRESENT, CONVERT SIL NOS. TO SILD NOS.
!
               nwdsa = elem(ielem+17)
            ELSEIF ( local==7 .OR. local==8 ) THEN
               CALL sdum21
               nwdsa = elem(ielem+17)
            ELSEIF ( local==9 .OR. local==10 ) THEN
               CALL sdum31
               nwdsa = elem(ielem+17)
            ELSEIF ( local==11 .OR. local==12 ) THEN
               CALL sdum41
               nwdsa = elem(ielem+17)
            ELSEIF ( local==13 .OR. local==14 ) THEN
               CALL sdum51
               nwdsa = elem(ielem+17)
            ELSEIF ( local==15 .OR. local==16 ) THEN
               CALL sdum61
               nwdsa = elem(ielem+17)
            ELSEIF ( local==17 .OR. local==18 ) THEN
               CALL sdum71
               nwdsa = elem(ielem+17)
            ELSEIF ( local==19 .OR. local==20 ) THEN
               CALL sdum81
               nwdsa = elem(ielem+17)
            ELSEIF ( local==21 .OR. local==22 ) THEN
               CALL sdum91
               nwdsa = elem(ielem+17)
            ELSEIF ( local==23 .OR. local==24 ) THEN
               CALL sqdm11
            ELSEIF ( local==25 .OR. local==26 ) THEN
               CALL sqdm21
            ELSEIF ( local==27 .OR. local==28 ) THEN
               CALL squd41
            ELSEIF ( local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 ) THEN
               CALL sihex1(eltype-64,strspt,nip)
               IF ( strspt>=nip**3+1 ) strspt = 0
            ELSEIF ( local==35 .OR. local==36 ) THEN
            ELSEIF ( local==37 .OR. local==38 ) THEN
            ELSEIF ( local==39 .OR. local==40 ) THEN
               CALL strax1
            ELSEIF ( local==41 .OR. local==42 ) THEN
               CALL stpax1
            ELSEIF ( local==45 .OR. local==46 ) THEN
               CALL strm61
            ELSEIF ( local==47 .OR. local==48 ) THEN
               CALL strp11
            ELSEIF ( local==49 .OR. local==50 ) THEN
               CALL strsl1
            ELSEIF ( local==59 .OR. local==60 ) THEN
               CALL ss2d81
               isopl8 = 8
            ELSEIF ( local==61 .OR. local==62 ) THEN
               CALL selbo1
            ELSEIF ( local==65 .OR. local==66 ) THEN
               CALL stri31
            ELSE
               CALL srod1
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( noep==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = ngps + 101
         itabl = 1
         kn = knsil
         n12 = 2
         ASSIGN 260 TO ret1
         l = 102
!WKBNB 7/94 SPR 94006
! REMOVE COMPONENT FROM SIL AND THEN ADD AFTER SILD NUMBER FOUND FOR
! CELAS1 AND CELAS2 ELEMENTS-SEE SUBROUTINE SELAS1
         IF ( eltype==11 ) THEN
! SET SIL NUMBER TO SIL OF GRID POINT WITHOUT COMPONENT CODE INCLUDED FOR
! CELAS1 SO SIL NUMBER CAN BE FOUND IN SILD
            buf(l) = buf(2)
            buf(l+1) = buf(3)
         ELSEIF ( eltype==12 ) THEN
! SET SIL NUMBER TO SIL OF GRID POINT WITHOUT COMPONENT CODE INCLUDED FOR
! CELAS2 SO SIL NUMBER CAN BE FOUND IN SILD
            buf(l) = buf(3)
            buf(l+1) = buf(4)
         ENDIF
!WKBNE 7/94 SPR 94006
         IF ( buf(l)/=0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         SPAG_Loop_1_1: DO
            l = l + 1
!WKBR 7/94 SPR 94006 IF (L      .GT. N) GO TO 410
            IF ( l>n ) THEN
!WKBNB 7/94 SPR94006
               IF ( eltype==11 ) THEN
! ADD COMPONENT CODES FOR SILD NUMBERS FOR CELAS1
                  IF ( buf(4)/=0 ) buf(102) = buf(102) + buf(4) - 1
                  IF ( buf(5)/=0 ) buf(103) = buf(103) + buf(5) - 1
               ELSEIF ( eltype==12 ) THEN
! ADD COMPONENT CODES FOR SILD NUMBERS FOR CELAS2
                  IF ( buf(5)/=0 ) buf(102) = buf(102) + buf(5) - 1
                  IF ( buf(6)/=0 ) buf(103) = buf(103) + buf(6) - 1
               ENDIF
               EXIT SPAG_Loop_1_1
            ELSEIF ( buf(l)/=0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 8
      CASE (8)
!WKBNE 7/94 SPR 94006
!
!     WRITE ELEMENT COMPUTATIONS ON ESTA. GO TO READ ANOTHER ELEMENT.
!
         IF ( .NOT.(anyout) ) THEN
            CALL write(esta,eltype,1,0)
            kwdest = kwdest + 2
            anyout = .TRUE.
         ENDIF
         CALL write(esta,bufa,nwdsa,0)
!
!     DIAG 20 OUTPUT ONLY
!
!     CALL BUG (4HESTA,0,BUFA,NWDSA)
!
         kwdest = kwdest + nwdsa
         IF ( strspt==0 ) GOTO 80
         strspt = strspt + 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE RECORD FOR CURRENT ELEMENT TYPE.
!     GO TO READ ANOTHER ELEM TYPE.
!
 100     IF ( anyout ) CALL write(esta,0,0,1)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE FILES.
!
 120     CALL close(est,clsrew)
         CALL close(esta,clsrew)
!
!     IF ELEMENT DEFORMATIONS, DETERMINE MAXIMUM NO. OF
!     WORDS IN ANY ONE DEFORMATION SET.
!
         IF ( eldef==0 ) RETURN
         CALL preloc(*240,z(buf1),edt)
         CALL locate(*220,z(buf1),kdefrm,flag)
         id = 0
         k = 0
         DO
            CALL read(*220,*140,edt,buf,3,0,flag)
            IF ( buf(1)==id ) THEN
               k = k + 3
            ELSE
               kwdedt = max0(kwdedt,k)
               k = 3
               id = buf(1)
            ENDIF
         ENDDO
 140     kwdedt = max0(kwdedt,k)
         CALL close(edt,clsrew)
         RETURN
!
!
!     FATAL FILE ERRORS.
!
 160     n = -1
         CALL mesage(n,file,nam)
         GOTO 220
 180     n = -2
         CALL mesage(n,file,nam)
         GOTO 220
 200     n = -3
         CALL mesage(n,file,nam)
!
!     ABNORMAL RETURN FROM SDR2B.
!
 220     CALL close(edt,clsrew)
         eldef = 0
 240     CALL mesage(30,79,0)
         stress = 0
         force = 0
         any = 0
         RETURN
      CASE (9)
!
!
!     BINARY SEARCH ROUTINE
!
         klo = 1
         khi = kn
         spag_nextblock_1 = 10
      CASE (10)
         k = (klo+khi+1)/2
         DO
            kx = itabl + n12*(k-1)
            IF ( buf(l)<z(kx) ) THEN
               khi = k
            ELSEIF ( buf(l)==z(kx) ) THEN
               IF ( n12==1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               buf(l) = z(kx+1)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               klo = k
            ENDIF
            IF ( khi-klo<1 ) THEN
               GOTO ret1
            ELSEIF ( khi-klo==1 ) THEN
               IF ( k==klo ) THEN
                  k = khi
               ELSE
                  k = klo
               ENDIF
               klo = khi
            ELSE
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 260     DO
            CALL mesage(-61,0,name)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (A27,' 2184,  STRESS OR FORCE REQUEST FOR ELEMENT ',2A4,' (NASTRAN ELEM. TYPE =',I4,1H),/5X,'WILL NOT BE HONORED',     &
             &' AS THIS ELEMENT IS NOT A STRUCTURAL ELEMENT.')
END SUBROUTINE sdr2b
