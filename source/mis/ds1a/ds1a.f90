!*==ds1a.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ds1a
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_DS1AAA
   USE C_DS1ADP
   USE C_DS1AET
   USE C_GPTA1
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , c , fj , temp
   INTEGER , SAVE :: bar , beam
   INTEGER :: buffr1 , buffr2 , buffr3 , file , i , i1 , i2 , i3 , ibeg , idx , ifile , ifirst , iflag , ii , iityp , ilast ,       &
            & imat1 , inc , ioutpt , ip , ipr , iprec , isys , itemp , itype , izmax , j , jj , jjj , jlast , jtyp , kkk , l38 ,    &
            & left , leftt , lim , lincor , local , low , m , matcr , nfree , nwords
   REAL*8 , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) :: inpvt
   INTEGER , DIMENSION(20) :: itypi
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcbkgg
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(9) :: ndum
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES THE MATRIX KGGD WHICH IS THE SECOND ORDER
!     APPROXIMATION TO THE STIFFNESS MATRIX KGG.
!
   !>>>>EQUIVALENCE (Ksystm(1),Isys) , (Ksystm(2),Ioutpt) , (Ksystm(46),Ndum(1)) , (Ksystm(55),Iprec)
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA name/4HDS1A , 4H    / , bar , beam/4HBAR  , 4HBEAM/
!
!     DEFINE VARIABLES IN COMMON /DS1AAA/
!
   Cstm = 106
   Mpt = 107
   Dit = 110
   Ecptds = 301
   Gpct = 109
   Kggd = 201
   Inrw = 0
   Outrw = 1
   Eor = 1
   Neor = 0
   Clsrw = 1
   Nlinks = 10
   Nogo = 0
   iityp = 0
   j = 26
   Ne(j) = bar
   CALL sswtch(38,l38)
!
!     DETERMINE SIZE OF VARIABLE CORE, AND SET UP BUFFERS
!
   ipr = iprec
   CALL delset
   izmax = korsz(Z)
   buffr1 = izmax - isys
   buffr2 = buffr1 - isys
   buffr3 = buffr2 - isys
   leftt = buffr3 - 1
!
!     READ THE CSTM INTO CORE
!
   ifile = Cstm
   Ncstm = 0
   Icstm = 0
   CALL open(*200,Cstm,Z(buffr1),Inrw)
   CALL fwdrec(*1400,Cstm)
   CALL read(*1500,*100,Cstm,Z(Icstm+1),leftt,Eor,Ncstm)
   CALL mesage(-8,0,name)
 100  leftt = leftt - Ncstm
!
!     PRETRD SETS UP SUBSEQUENT CALLS TO TRANSD.
!
   CALL pretrs(Z(Icstm+1),Ncstm)
   CALL pretrd(Z(Icstm+1),Ncstm)
   CALL close(Cstm,Clsrw)
 200  imat1 = Ncstm
!
!     CALL PREMAT TO READ MPT AND DIT INTO CORE.
!
   CALL premat(Z(imat1+1),Z(imat1+1),Z(buffr1),leftt,matcr,Mpt,Dit)
   leftt = leftt - matcr
   Igpct = Ncstm + matcr
!
!     OPEN KGGD, ECPTDS AND GPCT
!
   CALL gopen(Kggd,Z(buffr1),Outrw)
   CALL makmcb(mcbkgg,Kggd,0,6,ipr)
   CALL gopen(Ecptds,Z(buffr2),Inrw)
   CALL gopen(Gpct,Z(buffr3),Inrw)
!
!     READ THE FIRST TWO WORDS OF NEXT GPCT RECORD INTO INPVT(1).
!     INPVT(1) IS THE PIVOT POINT.  INPVT(1) .GT. 0 IMPLIES THE PIVOT
!     POINT IS A GRID POINT.  INPVT(1) .LT. 0 IMPLIES THE PIVOT POINT IS
!     A SCALAR POINT.  INPVT(2) IS THE NUMBER OF WORDS IN THE REMAINDER
!     OF THIS RECORD OF THE GPCT.
!
 300  file = Gpct
   CALL read(*1300,*1200,Gpct,inpvt(1),2,Neor,iflag)
   Ngpct = inpvt(2)
   CALL fread(Gpct,iz(Igpct+1),Ngpct,Eor)
   IF ( inpvt(1)<0 ) GOTO 1200
!
!     FROWIC IS THE FIRST ROW IN CORE. (1 .LE. FROWIC .LE. 6)
!
   Frowic = 1
!
!     DECREMENT THE AMOUNT OF CORE REMAINING.
!
   left = leftt - 2*Ngpct
   IF ( left<=0 ) CALL mesage(-8,0,name)
   Ipoint = Igpct + Ngpct
   Npoint = Ngpct
   I6x6k = Ipoint + Npoint
   I6x6k = (I6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE, WHICH WILL ENABLE SUBROUTINE DS1B TO
!     INSERT THE 6 X 6 MATRICES INTO KGGD.
!
   iz(Ipoint+1) = 1
   i1 = 1
   i = Igpct
   j = Ipoint + 1
   DO
      i1 = i1 + 1
      IF ( i1>Ngpct ) THEN
!
!     JMAX = NO. OF COLUMNS OF KGGD THAT WILL BE GENERATED WITH THE
!     CURRENT GRID POINT.
!
         inc = 5
         ilast = Igpct + Ngpct
         jlast = Ipoint + Npoint
         IF ( iz(ilast)<0 ) inc = 0
         Jmax = iz(jlast) + inc
!
!     IF 2*6*JMAX .LT. LEFT THERE ARE NO SPILL LOGIC PROBLEMS FOR
!     KGGD SINCE THE WHOLE DOUBLE PRECISION SUBMATRIX OF ORDER 6 X JMAX
!     CAN FIT IN CORE.
!
         itemp = 6*Jmax
         IF ( 2*itemp<left ) THEN
            Nrowsc = 6
!
!     LROWIC IS THE LAST ROW IN CORE. (1 .LE. LROWIC .LE. 6)
!
            Lrowic = Frowic + Nrowsc - 1
         ELSE
            name(2) = inpvt(1)
            CALL mesage(30,85,name)
            Nrowsc = 3
            DO WHILE ( 2*Nrowsc*Jmax>=left )
               Nrowsc = Nrowsc - 1
               IF ( Nrowsc==0 ) CALL mesage(-8,0,name)
            ENDDO
            Lrowic = Frowic + Nrowsc - 1
         ENDIF
         EXIT
      ELSE
         i = i + 1
         j = j + 1
         inc = 6
         IF ( iz(i)<0 ) inc = 1
         iz(j) = iz(j-1) + inc
      ENDIF
   ENDDO
!
!     ZERO OUT THE KGGD SUBMATRIX IN CORE.
!
 400  low = I6x6k + 1
   lim = I6x6k + Jmax*Nrowsc
   DO i = low , lim
      dz(i) = 0.0D0
   ENDDO
!
!     INITIALIZE THE LINK VECTOR TO -1.
!
   DO i = 1 , Nlinks
      Link(i) = -1
   ENDDO
!
!     TURN FIRST PASS INDICATOR ON.
!
 500  ifirst = 1
!
!     READ THE 1ST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT.
!     IF NPVT .LT. 0, THE REMAINDER OF THE ECPT RECORD IS NULL SO THAT
!     1 OR 6 NULL COLUMNS MUST BE GENERATED
!
   file = Ecptds
   CALL fread(Ecptds,Npvt,1,Neor)
   IF ( Npvt<0 ) GOTO 1200
 600  DO
!
!     READ THE NEXT ELEMENT TYPE INTO THE CELL ITYPE.
!
      CALL read(*1400,*900,Ecptds,itype,1,Neor,iflag)
!
!     READ THE ECPT ENTRY FOR THE CURRENT TYPE INTO THE ECPT ARRAY. THE
!     NUMBER OF WORDS TO BE READ WILL BE NWORDS(ITYPE).
!
      ip = iprec
      IF ( ip/=1 ) ip = 0
      jtyp = 2*itype - ip
      nfree = 3
      IF ( itype==2 .OR. itype==35 .OR. itype==75 ) nfree = 6
!               BEAM             CONEAX           TRSHL
      IF ( itype>=53 .AND. itype<=61 ) nfree = mod(ndum(itype-52),10)
!                DUM1              DUM9
      idx = (itype-1)*Incr
      nwords = Ne(idx+12) + 2 + nfree*Ne(idx+10)
      IF ( itype>=65 .AND. itype<=67 ) nwords = nwords + Ne(idx+10) - 1
!               IHEX1             IHEX3
      IF ( itype==80 ) nwords = nwords + Ne(idx+10)
!                 IS2D8
      IF ( itype==35 ) nwords = nwords + 1
!                CONEAX
      IF ( Ne(idx+12)<=0 ) CALL mesage(-61,0,name)
      CALL fread(Ecptds,Ecpt,nwords,Neor)
      itemp = Ne(idx+24)
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
      IF ( ifirst/=1 ) THEN
!
!     THIS IS NOT THE FIRST PASS.  IF ITYPE(TH) ELEMENT ROUTINE IS IN
!     CORE, PROCESS IT.
!
         IF ( itemp==lincor ) EXIT
!
!     THE ITYPE(TH) ELEMENT ROUTINE IS NOT IN CORE.  IF THIS ELEMENT
!     ROUTINE IS IN A LINK THAT ALREADY HAS BEEN PROCESSED READ THE NEXT
!     ELEMENT.
!
!
!     SET A TO BE PROCESSED LATER FLAG FOR THE LINK IN WHICH THE ELEMENT
!     RESIDES
!
         IF ( Link(itemp)/=1 ) Link(itemp) = 0
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
      ELSEIF ( Link(itemp)/=1 ) THEN
!
!     SET THE CURRENT LINK IN CORE = ITEMP AND IFIRST = 0
!
         lincor = itemp
         ifirst = 0
         EXIT
      ENDIF
   ENDDO
!
!     CALL THE PROPER ELEMENT ROUTINE.
!
   IF ( itype<=0 .OR. itype>Nelems ) CALL mesage(-7,0,name)
!
!     IF DIAG 38 IS ON, ECHO TYPE OF ELEMENT BEING PROCESSED
!
   IF ( l38/=0 ) THEN
      IF ( iityp/=0 ) THEN
         DO ii = 1 , iityp
            IF ( itype==itypi(ii) ) GOTO 700
         ENDDO
         IF ( iityp>=20 ) GOTO 700
      ENDIF
      iityp = iityp + 1
      itypi(iityp) = itype
      WRITE (ioutpt,99001) Ne(idx+1) , Ne(idx+2) , itype
99001 FORMAT ('0*** DS1 MODULE PROCESSING ',2A4,' ELEMENTS (ELEM.TYPE',I4,1H))
   ENDIF
!
 700  local = jtyp - 100
   IF ( local<=0 ) THEN
!
!        1-CROD       2-CBEAM      3-CTUBE      4-CSHEAR     5-CTWIST
!
!        6-CTRIA1     7-CTRBSC     8-CTRPLT     9-CTRMEM     10-CONROD
!
!        11-CELAS1    12-CELAS2    13-CELAS3    14-CELAS4    15-CQDPLT
!
!        16-CQDMEM    17-CTRIA2    18-CQUAD2    19-CQUAD1    20-CDAMP1
!
!        21-CDAMP2    22-CDAMP3    23-CDAMP4    24-CVISC     25-CMASS1
!
!        26-CMASS2    27-CMASS3    28-CMASS4    29-CONM1     30-CONM2
!
!        31-PLOTEL    32-X         33-X         34-CBAR      35-CCONEAX
!
!        36-CTRIARG   37-CTRAPRG   38-CTORDRG   39-CTETRA    40-CWEDGE
!
!        41-CHEXA1    42-CHEXA2    43-CFLUID2   44-CFLUID3   45-CFLUID4
!
!        46-CFLMASS   47-CAXIF2    48-CAXIF3    49-CAXIF4    50-CSLOT3
!
      IF ( jtyp==1 .OR. jtyp==2 .OR. jtyp==19 .OR. jtyp==20 ) THEN
!
!     ROD
!
         CALL drod
      ELSEIF ( jtyp==3 .OR. jtyp==4 ) THEN
!
!     BAR
!
         CALL dbar
      ELSEIF ( jtyp==5 .OR. jtyp==6 ) THEN
!
!     TUBE
!
         temp = Ecpt(5) - Ecpt(6)
         a = temp*Ecpt(6)*Pi
         fj = .25*a*(temp**2+Ecpt(6)**2)
         c = .5*Ecpt(5)
         m = 26
         DO i = 1 , 18
            m = m - 1
            Ecpt(m) = Ecpt(m-1)
         ENDDO
         CALL drod
      ELSEIF ( jtyp==7 .OR. jtyp==8 ) THEN
!
!     SHEAR
!
         CALL dshear
      ELSEIF ( jtyp==9 .OR. jtyp==10 .OR. jtyp==13 .OR. jtyp==14 .OR. jtyp==15 .OR. jtyp==16 .OR. jtyp==21 .OR. jtyp==22 .OR.       &
             & jtyp==23 .OR. jtyp==24 .OR. jtyp==25 .OR. jtyp==26 .OR. jtyp==27 .OR. jtyp==28 .OR. jtyp==29 .OR. jtyp==30 .OR.      &
             & jtyp==39 .OR. jtyp==40 .OR. jtyp==41 .OR. jtyp==42 .OR. jtyp==43 .OR. jtyp==44 .OR. jtyp==45 .OR. jtyp==46 .OR.      &
             & jtyp==47 .OR. jtyp==48 .OR. jtyp==49 .OR. jtyp==50 .OR. jtyp==51 .OR. jtyp==52 .OR. jtyp==53 .OR. jtyp==54 .OR.      &
             & jtyp==55 .OR. jtyp==56 .OR. jtyp==57 .OR. jtyp==58 .OR. jtyp==59 .OR. jtyp==60 .OR. jtyp==61 .OR. jtyp==62 .OR.      &
             & jtyp==63 .OR. jtyp==64 .OR. jtyp==65 .OR. jtyp==66 .OR. jtyp==67 .OR. jtyp==68 .OR. jtyp==71 .OR. jtyp==72 .OR.      &
             & jtyp==73 .OR. jtyp==74 .OR. jtyp==75 .OR. jtyp==76 .OR. jtyp==77 .OR. jtyp==78 .OR. jtyp==79 .OR. jtyp==80 .OR.      &
             & jtyp==81 .OR. jtyp==82 .OR. jtyp==83 .OR. jtyp==84 .OR. jtyp==85 .OR. jtyp==86 .OR. jtyp==87 .OR. jtyp==88 .OR.      &
             & jtyp==89 .OR. jtyp==90 .OR. jtyp==91 .OR. jtyp==92 .OR. jtyp==93 .OR. jtyp==94 .OR. jtyp==95 .OR. jtyp==96 .OR.      &
             & jtyp==97 .OR. jtyp==98 .OR. jtyp==99 .OR. jtyp==100 ) THEN
         GOTO 1600
      ELSEIF ( jtyp==11 .OR. jtyp==12 ) THEN
!
!     TRIA1
!
         CALL dtria(1)
      ELSEIF ( jtyp==17 .OR. jtyp==18 ) THEN
!
!     TRMEM
!
         CALL dtrmem(0)
      ELSEIF ( jtyp==31 .OR. jtyp==32 ) THEN
!
!     QDMEM
!
         CALL dqdmem
      ELSEIF ( jtyp==33 .OR. jtyp==34 ) THEN
!
!     TRIA2
!
         CALL dtria(2)
      ELSEIF ( jtyp==35 .OR. jtyp==36 ) THEN
!
!     QUAD2
!
         CALL dquad(2)
      ELSEIF ( jtyp==37 .OR. jtyp==38 ) THEN
!
!     QUAD1
!
         CALL dquad(1)
      ELSEIF ( jtyp==69 .OR. jtyp==70 ) THEN
!
!     CONE
!
         CALL dcone
      ELSE
         GOTO 800
      ENDIF
      GOTO 600
   ENDIF
!
!
!        51-CSLOT4    52-CHBDY     53-CDUM1     54-CDUM2     55-CDUM3
!
!        56-CDUM4     57-CDUM5     58-CDUM6     59-CDUM7     60-CDUM8
!
!        61-CDUM9     62-CQDMEM1   63-CQDMEM2   64-CQUAD4    65-CIHEX1
!
!        66-CIHEX2    67-CIHEX3    68-CQUADTS   69-CTRIATS   70-CTRIAAX
!
!        71-CTRAPAX   72-CAERO1    73-CTRIM6    74-CTRPLT1   75-CTRSHL
!
!        76-CFHEX1    77-CFHEX2    78-CFTETRA   79-CFWEDGE   80-CIS2D8
!
!        81-CELBOW    82-FTUBE     83-CTRIA3    84-CPSE2     85-CPSE3
!
!        86-CPSE4
!
 800  IF ( local==1 .OR. local==2 .OR. local==3 .OR. local==4 .OR. local==23 .OR. local==24 .OR. local==25 .OR. local==26 .OR.      &
         & local==39 .OR. local==40 .OR. local==41 .OR. local==42 .OR. local==43 .OR. local==44 .OR. local==45 .OR. local==46 .OR.  &
         & local==47 .OR. local==48 .OR. local==51 .OR. local==52 .OR. local==53 .OR. local==54 .OR. local==55 .OR. local==56 .OR.  &
         & local==57 .OR. local==58 .OR. local==61 .OR. local==62 .OR. local==63 .OR. local==64 ) GOTO 1600
   IF ( local==5 .OR. local==6 ) THEN
!
!     DUMMY ELEMENTS
!
      CALL ddum1
   ELSEIF ( local==7 .OR. local==8 ) THEN
      CALL ddum2
   ELSEIF ( local==9 .OR. local==10 ) THEN
      CALL ddum3
   ELSEIF ( local==11 .OR. local==12 ) THEN
      CALL ddum4
   ELSEIF ( local==13 .OR. local==14 ) THEN
      CALL ddum5
   ELSEIF ( local==15 .OR. local==16 ) THEN
      CALL ddum6
   ELSEIF ( local==17 .OR. local==18 ) THEN
      CALL ddum7
   ELSEIF ( local==19 .OR. local==20 ) THEN
      CALL ddum8
   ELSEIF ( local==21 .OR. local==22 ) THEN
      CALL ddum9
   ELSEIF ( local==27 .OR. local==28 ) THEN
!
!     QUAD4
!
      CALL dquad(4)
   ELSEIF ( local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 ) THEN
!
!     IHEX1,IHEX2,IHEX3
!
!
!     QUADTS
!
!
!    TRIATS
!
      CALL dihex(itype-64)
   ELSEIF ( local==35 .OR. local==36 ) THEN
   ELSEIF ( local==37 .OR. local==38 ) THEN
   ELSEIF ( local==49 ) THEN
      CALL dtshls
   ELSEIF ( local==50 ) THEN
      CALL dtshld
   ELSEIF ( local==59 .OR. local==60 ) THEN
      CALL dis2d8
   ELSEIF ( local==65 .OR. local==66 ) THEN
!
!     TRIA3
!
      CALL dtria(3)
   ELSEIF ( local==67 .OR. local==68 ) THEN
!
!     PRESSURE STIFFNESS ELEMENTS
!
      CALL dpse2
   ELSEIF ( local==69 .OR. local==70 ) THEN
      CALL dpse3
   ELSEIF ( local==71 .OR. local==72 ) THEN
      CALL dpse4
   ELSE
      CALL drod
   ENDIF
   GOTO 600
!
!     AT STATEMENT NO. 500 WE HAVE HIT AN EOR ON THE ECPT FILE.  SEARCH
!     THE LINK VECTOR TO DETERMINE IF THERE ARE LINKS TO BE PROCESSED.
!
 900  Link(lincor) = 1
   DO i = 1 , Nlinks
      IF ( Link(i)==0 ) GOTO 1100
   ENDDO
!
!     CHECK NOGO FLAG. IF NOGO=1, SKIP BLDPK AND PROCESS ANOTHER RECORD
!     FROM THE GPCT TABLE
!
   IF ( Nogo==1 ) GOTO 300
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE UNTO THE KGG FILE.
!
   ifile = Kggd
   i1 = 0
   DO
      i2 = 0
      ibeg = I6x6k + i1*Jmax
      CALL bldpk(2,ipr,ifile,0,0)
      DO
         i2 = i2 + 1
         IF ( i2>Ngpct ) THEN
            CALL bldpkn(ifile,0,mcbkgg)
            i1 = i1 + 1
            IF ( i1<Nrowsc ) GOTO 1000
!
!     TEST TO SEE IF THE LAST ROW IN CORE, LROWIC, = THE TOTAL NO. OF
!     ROWS TO BE COMPUTED = 6.  IF IT IS, WE ARE DONE.  IF NOT, THE
!     ECPTDS MUST BE BACKSPACED.
!
            IF ( Lrowic==6 ) GOTO 300
            CALL bckrec(Ecptds)
            Frowic = Frowic + Nrowsc
            Lrowic = Lrowic + Nrowsc
            GOTO 400
         ELSE
            jj = Igpct + i2
            Index = iabs(iz(jj)) - 1
            lim = 6
            IF ( iz(jj)<0 ) lim = 1
            jjj = Ipoint + i2
            kkk = ibeg + iz(jjj) - 1
            i3 = 0
            DO
               i3 = i3 + 1
               IF ( i3>lim ) EXIT
               Index = Index + 1
               kkk = kkk + 1
               Dpword = dz(kkk)
               IF ( Dpword/=0.0D0 ) CALL zblpki
            ENDDO
         ENDIF
      ENDDO
      GOTO 1200
 1000 ENDDO
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
 1100 CALL bckrec(Ecptds)
   GOTO 500
 1200 IF ( Nogo/=1 ) THEN
!
!     HERE WE HAVE A PIVOT POINT WITH NO ELEMENTS CONNECTED, SO THAT
!     NULL COLUMNS MUST BE OUTPUT ON THE KGGD FILE.
!
      file = Ecptds
      lim = 6
      IF ( inpvt(1)<0 ) lim = 1
      DO i = 1 , lim
         CALL bldpk(2,ipr,Kggd,0,0)
         CALL bldpkn(Kggd,0,mcbkgg)
      ENDDO
      CALL fwdrec(*1400,Ecptds)
   ENDIF
   GOTO 300
!
!     CHECK NOGO FLAG. IF NOGO=1, TERMINATE EXECUTION
!
 1300 IF ( Nogo==1 ) CALL mesage(-61,0,0)
!
!     WRAP UP BEFORE RETURN
!
   CALL close(Ecptds,Clsrw)
   CALL close(Gpct,Clsrw)
   CALL close(Kggd,Clsrw)
   mcbkgg(3) = mcbkgg(2)
   IF ( mcbkgg(6)==0 ) GOTO 1700
   CALL wrttrl(mcbkgg)
   j = 26
   Ne(j) = beam
   RETURN
!
!     ERROR RETURNS
!
 1400 CALL mesage(-2,file,name)
 1500 CALL mesage(-3,file,name)
 1600 CALL mesage(-7,file,name)
 1700 WRITE (ioutpt,99002) Ufm
99002 FORMAT (A23,' 2402, NULL DIFFERENTIAL STIFFNESS MATRIX ','GENERATED IN SUBROUTINE DS1A.')
   CALL mesage(-61,0,0)
END SUBROUTINE ds1a
