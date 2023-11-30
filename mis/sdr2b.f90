
SUBROUTINE sdr2b
!
!     SDR2B PROCESSES THE EST. FOR EACH ELEMENT IN THE MASTER SET,
!     PRELIMINARY COMPUTATIONS ARE MADE. IF THE PROBLEM CONTAINS EXTRA
!     POINTS, SIL NOS. ARE CONVERTED TO SILD NOS. THE DATA IS WRITTEN
!     ON ESTA FOR INPUT TO SDR2D WHERE FINAL STRESS AND FORCE RECOVERY
!     COMPUTATIONS ARE MADE.
!
!
   IMPLICIT NONE
   INTEGER Acc , All , Any , App(2) , Bgpdt , Bk0(2) , Bk1(2) , Branch , Buf(100) , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Bufa(100) ,  &
         & Bufb(4176) , Casecc , Cei(2) , Clsrew , Cstm , Ddrmm , Deform , Displ , Dit , Ds0(2) , Ds1(2) , Dtype(8) , Edt , Eigr ,  &
         & Eldef , Elem(1) , End , Eqexin , Est , Esta , File , Force , Frq(2) , Gptt , Gptta , Harms , Iacc , Icb(7) , Icstm ,     &
         & Idispl , Idit , Idummy(7) , Ieigen , Ieldef , Ielf , Ihmat , Iloads , Ilsym , Incr , Ioutpt , Iprec , Isopl , Isopl8 ,   &
         & Ispcf , Istr , Isymfl , Itherm , Itload , Ittl , Ivec , Ivecn , Ivel , Knset , Ksystm(63) , Ktype , Kwdcc , Kwdedt ,     &
         & Kwdest , Kwdgpt , Last , Loads , Mcb(7) , Mpt , Mptmpt , Mset
   LOGICAL Axic , Strain
   REAL Bufr(1) , Scrtch(300) , Zz(1)
   INTEGER Nam(2) , Ncstm , Nelem , Nharms , Nhmat , Nrigds , Nrings , Ocb(7) , Oef1 , Oeigr , Oes1 , Opg1 , Ophig , Oqg1 , Ougv1 , &
         & Pg , Phig , Pla(22) , Pphig , Pugv1 , Qg , Rd , Rdrew , Rei(2) , Sil , Sort2 , Spcf , Sta(2) , Stress , Strspt , Symflg ,&
         & Sysbuf , Temp , Tloads , Trn(2) , Ugv , Vel , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / App , Sort2 , Idummy , Strain
   COMMON /gpta1 / Nelem , Last , Incr , Elem
   COMMON /hmatdd/ Ihmat , Nhmat , Mptmpt , Idit
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /sdr2x1/ Ieigen , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm , Isopl8
   COMMON /sdr2x5/ Buf , Bufa , Bufb
   COMMON /sdr2x6/ Scrtch
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   LOGICAL anyout , heat , reject
   INTEGER eltype , flag , i , id , idsave , ielem , imat , ipr , itabl , iz1st , jltype , k , kdefrm(2) , khi , klo , kn , knsil , &
         & kx , l , local , m8 , mmre(2) , n , n12 , n1mat , n2mat , name(2) , ngps , nip , noep , nsil , nwds , nwdsa , ret1 , star
!WKBI 7/94 SPR 94007
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt) , (Ksystm(55),Iprec) , (Ksystm(56),Itherm) , (Z(1),Zz(1)) , (Bufr(1),Buf(1))
   DATA name/4HSDR2 , 4HB   / , star/4H* * /
   DATA kdefrm/104 , 1/
   DATA iz1st/1/
!WKBI 7/94 SPR 94007
   DATA mmre/4HMMRE , 4HIGEN/
!            IZ1ST  IS THE START OF OPEN CORE AVAILABLE
!
!
!     IF APPROACH IS COMPLEX EIGENVALUES, FREQUENCY OR TRANSIENT
!     RESPONSE, TEST FOR EXTRA POINTS. IF PRESENT, READ EQUIVALENCE
!     TABLE (SIL,SILD) INTO CORE.
!
   CALL delset
   heat = .FALSE.
   IF ( Itherm/=0 ) heat = .TRUE.
   Isopl = 0
   Icstm = iz1st
   m8 = -8
   noep = 0
!WKBR 7/94 SPR 94007
!     IF (APP(1).EQ.CEI(1) .OR. APP(1).EQ.FRQ(1) .OR. APP(1).EQ.TRN(1))
!    1    GO TO 20
   IF ( App(1)/=Cei(1) .AND. App(1)/=Frq(1) .AND. App(1)/=Trn(1) .AND. App(1)/=mmre(1) ) GOTO 200
   Icb(1) = Sil
   CALL rdtrl(Icb)
   noep = Icb(3)
   IF ( noep==0 ) GOTO 200
   File = Sil
   CALL open(*1700,Sil,Z(Buf1),Rdrew)
   CALL fwdrec(*1800,Sil)
   CALL fwdrec(*1800,Sil)
   CALL read(*1800,*100,Sil,Z,Buf2,1,nsil)
   CALL mesage(m8,0,Nam)
 100  CALL close(Sil,Clsrew)
   knsil = nsil/2
   Icstm = nsil + 1
   IF ( nsil>=Mset ) THEN
      Mset = Buf2 - 1
      All = 1
   ENDIF
!
!     READ THE CSTM INTO CORE (IF PRESENT).
!
 200  Ncstm = 0
   File = Cstm
   CALL open(*400,Cstm,Z(Buf1),Rdrew)
   CALL fwdrec(*1800,Cstm)
   CALL read(*1800,*300,Cstm,Z(Icstm),Buf2-Icstm,1,Ncstm)
   CALL mesage(m8,0,Nam)
 300  CALL close(Cstm,Clsrew)
   CALL pretrs(Z(Icstm),Ncstm)
 400  imat = Icstm + Ncstm
   IF ( imat>=Mset ) THEN
      Mset = Buf2 - 1
      All = 1
   ENDIF
!
!     READ MATERIAL PROPERTY DATA INTO CORE.
!
   n1mat = Buf2 - imat
   IF ( .NOT.heat ) THEN
!
      CALL premat(Z(imat),Z(imat),Z(Buf1),n1mat,n2mat,Mpt,Dit)
   ELSE
!
!     FOR HEAT PROBLEMS ONLY, -HMAT- ROUTINE IS USED.
!
      Ihmat = imat
      Nhmat = Buf1 + Sysbuf
      Mptmpt = Mpt
      Idit = Dit
      CALL prehma(Z)
      n2mat = Nhmat - Ihmat + 1 - 2*(Sysbuf+1)
   ENDIF
   IF ( imat+n2mat>=Mset ) THEN
      Mset = Buf2 - 1
      All = 1
   ENDIF
!
!     OPEN EST AND ESTA.
!
   File = Est
   CALL open(*2100,Est,Z(Buf1),Rdrew)
   CALL fwdrec(*1800,Est)
   File = Esta
   CALL open(*1700,Esta,Z(Buf2),Wrtrew)
   File = Est
   Kwdest = 0
   Kwdedt = 0
   Kwdgpt = 0
!
!     READ ELEMENT TYPE. SET PARAMETERS AS A FUNCTION OF ELEM TYPE.
!
 500  CALL read(*1500,*1900,Est,eltype,1,0,flag)
   IF ( eltype<1 .OR. eltype>Nelem ) THEN
!
!     ELEMENT UNDEFINE TO SDR2BD
!
      WRITE (Ioutpt,99002) star , star , eltype
      GOTO 1000
   ELSE
      anyout = .FALSE.
      ipr = Iprec
      IF ( ipr/=1 ) ipr = 0
      jltype = 2*eltype - ipr
      ielem = (eltype-1)*Incr
      nwds = Elem(ielem+12)
      nwdsa = Elem(ielem+17)
      IF ( heat ) nwdsa = 142
      ngps = Elem(ielem+10)
   ENDIF
!
!     READ DATA FOR AN ELEMENT.
!     DETERMINE IF ELEMENT BELONGS TO MASTER SET.
!
 600  CALL read(*1800,*1400,Est,Buf,nwds,0,flag)
   DO i = 1 , nwds
      Scrtch(100+i) = Bufr(i)
   ENDDO
   Strspt = 0
   Isopl = -1
   idsave = Buf(1)
   IF ( All==0 ) THEN
      itabl = Mset
      kn = Knset
      l = 1
      n12 = 1
      ASSIGN 600 TO ret1
!
!     DECODE ELEMENT ID SINCE THIS IS A CONICAL SHELL PROBLEM
!
      IF ( Axic ) Buf(1) = Buf(1)/1000
      GOTO 2200
   ENDIF
!
!     CALL APPROPRIATE ELEMENT SUBROUTINE.
!
 700  Buf(1) = idsave
!
   IF ( Strain ) THEN
!
!     IF THE STRAIN FLAG IS TURNED ON, IGNORE ALL ELEMENTS
!WKBR NCL93012 3/94 EXCEPT CTRIA1, CTRIA2, CQUAD1 AND CQUAD2 ELEMENTS
!     EXCEPT CTRIA1, CTRIA2, CTRIA3, CQUAD1, CQUAD2 AND CQUAD4 ELEMENTS
!
!WKBR NCL93012 3/94     1    ELTYPE.EQ.19) GO TO 112
      IF ( eltype/=6 .AND. eltype/=17 .AND. eltype/=18 .AND. eltype/=19 .AND. eltype/=64 .AND. eltype/=83 ) THEN
         WRITE (Ioutpt,99001) Swm , Elem(ielem+1) , Elem(ielem+2)
99001    FORMAT (A27,', STRAIN REQUEST FOR ',2A4,' ELEMENTS WILL',/5X,'NOT BE HONORED AS THIS OUTPUT IS NOT DEFINED FOR THIS ',     &
                &'ELEMENT TYPE.')
         CALL fwdrec(*1800,Est)
         GOTO 1400
      ENDIF
   ENDIF
!
 800  IF ( heat ) THEN
!
!     HEAT PROBLEMS (ALL ELEMENTS).
!
      CALL sdhtf1(eltype,reject)
      IF ( eltype>=65 .AND. eltype<=67 ) THEN
         IF ( eltype==65 .AND. Strspt>=9 ) Strspt = 0
         IF ( Strspt>=21 ) Strspt = 0
      ENDIF
      IF ( .NOT.reject ) GOTO 1100
      CALL fwdrec(*1800,Est)
      GOTO 1400
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
         ELSEIF ( jltype==3 .OR. jltype==4 .OR. jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR. jltype==43 .OR.     &
                & jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. jltype==50 .OR.   &
                & jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR. jltype==57 .OR.   &
                & jltype==58 .OR. jltype==59 .OR. jltype==60 .OR. jltype==61 .OR. jltype==62 .OR. jltype==63 .OR. jltype==64 .OR.   &
                & jltype==65 .OR. jltype==66 .OR. jltype==85 .OR. jltype==86 .OR. jltype==87 .OR. jltype==88 .OR. jltype==89 .OR.   &
                & jltype==90 .OR. jltype==91 .OR. jltype==92 ) THEN
            GOTO 900
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
            GOTO 850
         ENDIF
         GOTO 1100
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
 850  IF ( local==1 .OR. local==2 ) THEN
         k = 1
         CALL sslot1(k)
      ELSEIF ( local==3 .OR. local==4 .OR. local==43 .OR. local==44 .OR. local==51 .OR. local==52 .OR. local==53 .OR. local==54 .OR.&
             & local==55 .OR. local==56 .OR. local==57 .OR. local==58 .OR. local==63 .OR. local==64 ) THEN
         GOTO 900
      ELSEIF ( local==5 .OR. local==6 ) THEN
         CALL sdum11
!
!     IF EXTRA POINTS PRESENT, CONVERT SIL NOS. TO SILD NOS.
!
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==7 .OR. local==8 ) THEN
         CALL sdum21
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==9 .OR. local==10 ) THEN
         CALL sdum31
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==11 .OR. local==12 ) THEN
         CALL sdum41
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==13 .OR. local==14 ) THEN
         CALL sdum51
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==15 .OR. local==16 ) THEN
         CALL sdum61
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==17 .OR. local==18 ) THEN
         CALL sdum71
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==19 .OR. local==20 ) THEN
         CALL sdum81
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==21 .OR. local==22 ) THEN
         CALL sdum91
         nwdsa = Elem(ielem+17)
      ELSEIF ( local==23 .OR. local==24 ) THEN
         CALL sqdm11
      ELSEIF ( local==25 .OR. local==26 ) THEN
         CALL sqdm21
      ELSEIF ( local==27 .OR. local==28 ) THEN
         CALL squd41
      ELSEIF ( local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 ) THEN
         CALL sihex1(eltype-64,Strspt,nip)
         IF ( Strspt>=nip**3+1 ) Strspt = 0
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
         Isopl8 = 8
      ELSEIF ( local==61 .OR. local==62 ) THEN
         CALL selbo1
      ELSEIF ( local==65 .OR. local==66 ) THEN
         CALL stri31
      ELSE
         CALL srod1
      ENDIF
      GOTO 1100
   ENDIF
 900  WRITE (Ioutpt,99002) Swm , Elem(ielem+1) , Elem(ielem+2) , eltype
 1000 CALL fwdrec(*1800,Est)
   GOTO 1400
 1100 IF ( noep==0 ) GOTO 1300
   n = ngps + 101
   itabl = 1
   kn = knsil
   n12 = 2
   ASSIGN 2500 TO ret1
   l = 102
!WKBNB 7/94 SPR 94006
! REMOVE COMPONENT FROM SIL AND THEN ADD AFTER SILD NUMBER FOUND FOR
! CELAS1 AND CELAS2 ELEMENTS-SEE SUBROUTINE SELAS1
   IF ( eltype==11 ) THEN
! SET SIL NUMBER TO SIL OF GRID POINT WITHOUT COMPONENT CODE INCLUDED FOR
! CELAS1 SO SIL NUMBER CAN BE FOUND IN SILD
      Buf(l) = Buf(2)
      Buf(l+1) = Buf(3)
   ELSEIF ( eltype==12 ) THEN
! SET SIL NUMBER TO SIL OF GRID POINT WITHOUT COMPONENT CODE INCLUDED FOR
! CELAS2 SO SIL NUMBER CAN BE FOUND IN SILD
      Buf(l) = Buf(3)
      Buf(l+1) = Buf(4)
   ENDIF
!WKBNE 7/94 SPR 94006
   IF ( Buf(l)/=0 ) GOTO 2200
 1200 DO
      l = l + 1
!WKBR 7/94 SPR 94006 IF (L      .GT. N) GO TO 410
      IF ( l>n ) THEN
!WKBNB 7/94 SPR94006
         IF ( eltype==11 ) THEN
! ADD COMPONENT CODES FOR SILD NUMBERS FOR CELAS1
            IF ( Buf(4)/=0 ) Buf(102) = Buf(102) + Buf(4) - 1
            IF ( Buf(5)/=0 ) Buf(103) = Buf(103) + Buf(5) - 1
         ELSEIF ( eltype==12 ) THEN
! ADD COMPONENT CODES FOR SILD NUMBERS FOR CELAS2
            IF ( Buf(5)/=0 ) Buf(102) = Buf(102) + Buf(5) - 1
            IF ( Buf(6)/=0 ) Buf(103) = Buf(103) + Buf(6) - 1
         ENDIF
         EXIT
      ELSEIF ( Buf(l)/=0 ) THEN
         GOTO 2200
      ENDIF
   ENDDO
!WKBNE 7/94 SPR 94006
!
!     WRITE ELEMENT COMPUTATIONS ON ESTA. GO TO READ ANOTHER ELEMENT.
!
 1300 IF ( .NOT.(anyout) ) THEN
      CALL write(Esta,eltype,1,0)
      Kwdest = Kwdest + 2
      anyout = .TRUE.
   ENDIF
   CALL write(Esta,Bufa,nwdsa,0)
!
!     DIAG 20 OUTPUT ONLY
!
!     CALL BUG (4HESTA,0,BUFA,NWDSA)
!
   Kwdest = Kwdest + nwdsa
   IF ( Strspt==0 ) GOTO 600
   Strspt = Strspt + 1
   GOTO 800
!
!     CLOSE RECORD FOR CURRENT ELEMENT TYPE.
!     GO TO READ ANOTHER ELEM TYPE.
!
 1400 IF ( anyout ) CALL write(Esta,0,0,1)
   GOTO 500
!
!     CLOSE FILES.
!
 1500 CALL close(Est,Clsrew)
   CALL close(Esta,Clsrew)
!
!     IF ELEMENT DEFORMATIONS, DETERMINE MAXIMUM NO. OF
!     WORDS IN ANY ONE DEFORMATION SET.
!
   IF ( Eldef==0 ) RETURN
   CALL preloc(*2100,Z(Buf1),Edt)
   CALL locate(*2000,Z(Buf1),kdefrm,flag)
   id = 0
   k = 0
   DO
      CALL read(*2000,*1600,Edt,Buf,3,0,flag)
      IF ( Buf(1)==id ) THEN
         k = k + 3
      ELSE
         Kwdedt = max0(Kwdedt,k)
         k = 3
         id = Buf(1)
      ENDIF
   ENDDO
 1600 Kwdedt = max0(Kwdedt,k)
   CALL close(Edt,Clsrew)
   RETURN
!
!
!     FATAL FILE ERRORS.
!
 1700 n = -1
   CALL mesage(n,File,Nam)
   GOTO 2000
 1800 n = -2
   CALL mesage(n,File,Nam)
   GOTO 2000
 1900 n = -3
   CALL mesage(n,File,Nam)
!
!     ABNORMAL RETURN FROM SDR2B.
!
 2000 CALL close(Edt,Clsrew)
   Eldef = 0
 2100 CALL mesage(30,79,0)
   Stress = 0
   Force = 0
   Any = 0
   RETURN
!
!
!     BINARY SEARCH ROUTINE
!
 2200 klo = 1
   khi = kn
 2300 k = (klo+khi+1)/2
 2400 kx = itabl + n12*(k-1)
   IF ( Buf(l)<Z(kx) ) THEN
      khi = k
   ELSEIF ( Buf(l)==Z(kx) ) THEN
      IF ( n12==1 ) GOTO 700
      Buf(l) = Z(kx+1)
      GOTO 1200
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
      GOTO 2400
   ELSE
      GOTO 2300
   ENDIF
 2500 DO
      CALL mesage(-61,0,name)
   ENDDO
99002 FORMAT (A27,' 2184,  STRESS OR FORCE REQUEST FOR ELEMENT ',2A4,' (NASTRAN ELEM. TYPE =',I4,1H),/5X,'WILL NOT BE HONORED',     &
             &' AS THIS ELEMENT IS NOT A STRUCTURAL ELEMENT.')
END SUBROUTINE sdr2b
