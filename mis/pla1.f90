
SUBROUTINE pla1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bufr1 , Bufr2 , Bufr3 , Bufr4 , Bufsz , Clsnrw , Clsrw , Cstm , Dit , Ecpt , Ecptnl , Eor , Estl , Estltr(7) , Estnl ,   &
         & Estnlt(7) , Frowic , Gpct , I6x6k , Icstm , Iecpt(100) , Igpct , Incr , Index , Indstr , Inflag , Inrw , Iopt4 , Ipoint ,&
         & Iprec , Isp1(37) , Isp2(14) , Isyspt , Itype , Iz(1) , Jmax , Kggl , Kgglpg , Kickof , Last , Left , Link(10) , Llllll(2)&
         & , Lrowic , Matid , Mcbkgg(7) , Mpt , Nbpw , Ncstm , Ne(1) , Nelems , Neor , Ngpct , Nlel , Nlinks , Nnlel , Nogo ,       &
         & Nonlst , Nplalp , Npoint , Npvt , Nrowsc , Outrw , Plaset , Tnrows , Trail(7)
   DOUBLE PRECISION Dpdum(300) , Dpword , Dz(1)
   REAL Dum1 , Dum2 , Dum4 , Dum5 , Dum6 , Dum7 , Dum8 , Dum9 , E , Filler(4) , Plfact(2) , Xecpt(100) , Yyyyyy(19) , Z(1)
   LOGICAL Heat
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Kgglpg , Nplalp , Kickof , Plaset , Nonlst , Plfact
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /matin / Matid , Inflag , Filler
   COMMON /matout/ Indstr , Yyyyyy
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , Dum4 , Dum5 , Dum6
   COMMON /sma1cl/ Iopt4 , Dum7 , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Dum8 , Dum9 , Nogo
   COMMON /sma1dp/ Dpdum
   COMMON /sma1et/ Xecpt
   COMMON /sma1ht/ Heat
   COMMON /sma1io/ Cstm , Mpt , Dit , Bufr1 , Ecpt , Bufr2 , Gpct , Bufr3 , Bufr4 , Itype , Kggl , Estnl , Ecptnl , Dum1 , Estl ,   &
                 & Dum2 , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Trail
   COMMON /system/ Bufsz , Isyspt , Isp1 , Nbpw , Isp2 , Iprec
   COMMON /xmssg / Ufm , Uwm
   COMMON /zblpkx/ Dpword , Llllll , Index
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   LOGICAL all , phase1
   INTEGER casecc , est , file , i , i1 , i2 , i3 , ibeg , icc , idx , ielid , ifirst , iflag , ilast , ileft , imat , inc ,        &
         & inpvt(2) , ipass , iplset , ipr , iset , isetno , istset , isym , itemp , izmax , j , jj , jjj , jlast , jplset ,        &
         & jstset , jsym , ka , kb , kkk , lim , lincor , low , lset , mused , name(2) , ncc , nn , nosd , npvtwr , nset , nwds ,   &
         & outfil , plaary(90) , planos(2) , setno , yessd
   REAL fnn , hmpt
   INTEGER korsz
!
! End of declarations
!
!
!     THIS FUNCTIONAL MODULE IS THE FIRST OF FOUR FUNCTIONAL MODULES
!     UNIQUE TO THE PIECE-WISE LINEAR ANALYSIS (DISPLACEMENT METHOD)
!     RIGID FORMAT
!
!
!     PLA1   CSTM,MPT,ECPT,GPCT,DIT,CASECC,EST /KGGL,ECPTNL,ESTL,ESTNL/
!            V,N,KGGLPG/V,N,NPLALIM/V,N,ECPTNLPG/V,N,PLSETNO/
!            V,N,NONLSTR/V,N,PLFACT/ $
!
!     THE OUTPUT DATA BLOCKS AND PARAMETERS ARE DEFINED AS FOLLOWS -
!
!     KGGL   IS THE STIFFNESS MATRIX OF LINEAR (NON-STRESS DEPENDENT)
!            ELEMENTS
!     ECPTNL IS A SUBSET OF THE ECPT WHICH CONTAINS ECPT PLUS STRESS
!            INFORMATION FOR THE NON-LINEAR (STRESS DEPENDENT) ELEMENTS
!     ESTL,  A SUBSET OF THE EST, CONTAINS ALL LINEAR ELEMENTS
!     ESTNL, THE COMPLEMENT OF THE ESTL, CONTAINS INFORMATION FOR THE
!            NON-LINEAR ELEMENTS
!
!     PARAMETER NAMES BELOW ARE FORTRAN VARIABLE NAMES RATHER THAN DMAP
!     PARAMETER NAMES
!
!     KGGLPG IS THE PURGE FLAG FOR THE KGGL AND ESTL DATA BLOCKS.  IT IS
!            SET = -1 (PURGE=YES) IF ALL ELEMENTS ARE STRESS DEPENDENT
!     NPLALP IS THE NUMBER OF PASSES THAT WILL BE MADE THRU THE PLA LOOP
!     KICKOF IS SET = -1 (KICK THE USER OFF THE MACHINE = YES) IF THE
!            DIT IS PURGED OR ALL ELEMENTS ARE NON-STRESS DEPENDENT
!     PLASET IS THE SET NUMBER ON THE PLFACT CARD THAT IS OBTAINED FROM
!            THE FIRST RECORD OF CASECC
!     NONLST IS THE FLAG SUCH THAT IF IT IS A -1 THE USER DOES NOT WISH
!            TO OUTPUT HIS NON-LINEAR STRESSES.  HENCE PLA3 WILL NOT BE
!            CALLED
!     PLFACT IS THE FIRST PIECE-WISE LINEAR FACTOR TO BE USED FROM
!            PLASET
!
   EQUIVALENCE (Z(1),Iz(1),Dz(1)) , (Iecpt(1),Xecpt(1)) , (Mcbkgg(1),Estltr(1)) , (Trail(1),Estnlt(1)) , (Trail(2),Nnlel) ,         &
    & (Estltr(2),Nlel) , (fnn,nn) , (Indstr,E)
   DATA casecc , jstset , jplset , jsym/106 , 23 , 164 , 200/
   DATA name/4HPLA1 , 4H    / , hmpt/4HMPT /
   DATA planos/1103 , 11/
   DATA plaary/1 , 0 , 1 , 0 , 0 , 1 , 0 , 0 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 30*0/
!
!     IF THE DIT HAS BEEN PURGED, WE CANNOT PROCEED FURTHER
!
   ipr = Iprec
   CALL delset
   Trail(1) = Dit
   CALL rdtrl(Trail)
   IF ( Trail(1)<0 ) CALL mesage(-1,Dit,name)
!
!     INITIALIZE HEAT PARAMETER
!
   Heat = .FALSE.
!
!     INITIALIZE MODULE PARAMETERS AND SET IOPT4 = 0, SO THAT ELEMENT
!     ROUTINES WILL NOT CALCULATE STRUCTURAL DAMPING MATRICES.
!
   Kgglpg = -1
   Nplalp = 1
   Kickof = -1
   Plaset = -1
   Nonlst = 1
   Plfact(1) = 1.0
   Iopt4 = 0
   Estnl = 204
   est = 107
   phase1 = .TRUE.
   ASSIGN 2700 TO nosd
!
!     SET UP BUFFERS AND INITIALIZE FILE TRAILERS.
!
   izmax = korsz(Z)
   Bufr1 = izmax - Bufsz
   Bufr2 = Bufr1 - Bufsz
   Bufr3 = Bufr2 - Bufsz
   Bufr4 = Bufr3 - Bufsz
   Left = Bufr4 - 1
   CALL makmcb(Mcbkgg,Kggl,0,6,ipr)
   CALL makmcb(Trail,Ecptnl,0,0,0)
!
!     CHECK PLAARY SIZE
!
   IF ( Nelems>90 ) WRITE (Isyspt,99001) Uwm
99001 FORMAT (A25,' 2151, -PLAARY- ARRAY IS SMALLER THAN MAXIMUM ','NUMBER OF ELEMENT TYPES.')
!
!     OPEN THE KGGL FILE FOR OUTPUT
!
   CALL gopen(Kggl,Z(Bufr1),1)
!
!     ATTEMPT TO READ THE CSTM INTO CORE
!
   Icstm = 0
   Ncstm = 0
   file = Cstm
   CALL open(*200,Cstm,Z(Bufr2),Inrw)
   CALL fwdrec(*4600,Cstm)
   CALL read(*4600,*100,Cstm,Z(Icstm+1),Left,Eor,Ncstm)
!
!     INSUFFICIENT CORE - CALL MESAGE
!
   CALL mesage(-8,0,name)
 100  Left = Left - Ncstm
   CALL close(Cstm,Clsrw)
!
!     CALL PRETRD TO SET UP FUTURE CALLS TO TRANSD.
!
   CALL pretrd(Z(Icstm+1),Ncstm)
   CALL pretrs(Z(Icstm+1),Ncstm)
!
!     CALL PREMAT TO READ THE MPT AND THE DIT AND TO SET UP FUTURE CALLS
!     TO SUBROUTINE MAT.  NOTE NEGATIVE ISGN FOR DIT TO TRIGGER PLA FLAG
!     IN MAT.
!
 200  imat = Ncstm
   CALL premat(Iz(imat+1),Z(imat+1),Z(Bufr2-3),Left,mused,Mpt,-Dit)
   Left = Left - mused
   Igpct = Ncstm + mused
!
!     OPEN THE INPUT FILES ECPT AND GPCT AND THE OUTPUT FILE ECPTNL.
!
   CALL gopen(Ecpt,Z(Bufr2),0)
   CALL gopen(Gpct,Z(Bufr3),0)
   CALL gopen(Ecptnl,Z(Bufr4),1)
   ileft = Left
!
!     BEGIN MAIN LOOP FOR PROCESSING THE ECPT.
!
 300  CALL read(*3200,*3100,Gpct,inpvt(1),2,Neor,iflag)
   Ngpct = inpvt(2)
   Left = ileft - 2*Ngpct
   IF ( Left<=0 ) CALL mesage(-8,0,name)
   CALL fread(Gpct,Iz(Igpct+1),Ngpct,Eor)
!
!     FROWIC IS THE FIRST ROW IN CORE (1 .LE. FROWIC .LE. 6)
!
   Frowic = 1
   Ipoint = Igpct + Ngpct
   Npoint = Ngpct
   I6x6k = Ipoint + Npoint
!
!     MAKE I6X6K A DOUBLE PRECISION INDEX (I6X6K POINTS TO THE 0TH
!     LOCATION OF THE 6 X 6 SUBMATRIX OF KGGL IN CORE)
!
   I6x6k = (I6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE WHICH WILL ENABLE SUBROUTINE SMA1B TO
!     ADD THE ELEMENT STIFFNESS MATRICES TO KGGL.
!
   Iz(Ipoint+1) = 1
   i1 = 1
   i = Igpct
   j = Ipoint + 1
   DO
      i1 = i1 + 1
      IF ( i1>Ngpct ) THEN
!
!     JMAX = THE NO. OF COLUMNS OF KGGL THAT WILL BE GENERATED WITH THE
!     CURRENT PIVOT POINT.
!
         inc = 5
         ilast = Igpct + Ngpct
         jlast = Ipoint + Npoint
         IF ( Iz(ilast)<0 ) inc = 0
         Jmax = Iz(jlast) + inc
!
!     TNROWS = TOTAL NO. OF ROWS OF THE MATRIX TO BE GENERATED
!
         Tnrows = 6
         IF ( inpvt(1)<0 ) Tnrows = 1
         IF ( 2*Tnrows*Jmax<Left ) THEN
            Nrowsc = Tnrows
         ELSE
!
!     THE WHOLE SUBMATRIX CANNOT FIT IN CORE
!
            IF ( Tnrows==1 ) CALL mesage(-8,0,name)
            Nrowsc = 3
            plaary(39) = name(1)
            DO
               plaary(40) = Npvt
               CALL mesage(30,85,plaary(39))
               IF ( 2*Nrowsc*Jmax<Left ) EXIT
               Nrowsc = Nrowsc - 1
               IF ( Nrowsc==0 ) CALL mesage(-8,0,name)
            ENDDO
         ENDIF
         Frowic = 1
         Lrowic = Frowic + Nrowsc - 1
!
!     ZERO OUT THE KGGL SUBMATRIX IN CORE.
!
         low = I6x6k + 1
         lim = I6x6k + Jmax*Nrowsc
         DO i = low , lim
            Dz(i) = 0.0D0
         ENDDO
!
!     INITIALIZE THE LINK VECTOR TO -1
!
         DO i = 1 , Nlinks
            Link(i) = -1
         ENDDO
         lincor = 1
         file = Ecpt
!
!     TURN FIRST PASS, FIRST ELEMENT READ ON THE CURRENT PASS OF THE
!     ECPT RECORD, AND PIVOT POINT WRITTEN INDICATORS ON.
!
         ipass = 1
         npvtwr = 0
         EXIT
      ELSE
         i = i + 1
         j = j + 1
         inc = 6
         IF ( Iz(i)<0 ) inc = 1
         Iz(j) = Iz(j-1) + inc
      ENDIF
   ENDDO
 400  ifirst = 1
!
!     READ THE FIRST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT
!
   CALL fread(Ecpt,Npvt,1,Neor)
 500  DO
!
!     READ THE NEXT ELEMENT TYPE INTO ITYPE, AND READ THE PRESCRIBED NO.
!     OF WORDS INTO THE XECPT ARRAY.
!
      CALL read(*4600,*2800,Ecpt,Itype,1,Neor,iflag)
      idx = (Itype-1)*Incr
      nn = Ne(idx+12)
      CALL fread(Ecpt,Xecpt,nn,Neor)
      itemp = Ne(idx+22)
      IF ( ipass/=1 ) GOTO 2700
!
!     THIS IS THE FIRST PASS.  IF THE ELEMENT IS IN THE PLA SET, CALL
!     THE MAT ROUTINE TO FIND OUT IF ANY OF THE MATERIAL PROPERTIES IS
!     STRESS DEPENDENT.
!
!
!              CROD      CBEAM     CTUBE     CSHEAR    CTWIST    CTRIA1
!                 1        2         3          4         5         6
!              CTRBSC    CTRPLT    CTRMEM    CONROD    ELAS1     ELAS2
!               7           8         9        10        11        12
!              ELAS3     ELAS4     CQDPLT    CQDMEM    CTRIA2    CQUAD2
!               13        14         15        16        17        18
!              CQUAD1    CDAMP1    CDAMP2    CDAMP3    CDAMP4    CVISC
!               19        20        21         22        23        24
!              MASS1     CMASS2    CMASS3    CMASS4    CONM1     CONM2
!                25        26        27        28        29        30
!              PLOTEL    REACT     QUAD3     CBAR      CCONE
!                31        32        33       34         35
      IF ( Itype<=35 ) THEN
         IF ( Itype==2 .OR. Itype==32 .OR. Itype==33 ) GOTO 4800
         IF ( Itype==3 ) GOTO 800
         IF ( Itype==4 .OR. Itype==5 .OR. Itype==7 .OR. Itype==8 .OR. Itype==11 .OR. Itype==12 .OR. Itype==13 .OR. Itype==14 .OR.   &
            & Itype==15 .OR. Itype==35 ) GOTO 2700
         IF ( Itype==6 ) GOTO 1000
         IF ( Itype==9 ) GOTO 1200
         IF ( Itype==16 ) GOTO 1400
         IF ( Itype==17 ) GOTO 1600
         IF ( Itype==18 ) GOTO 1800
         IF ( Itype==19 ) GOTO 2000
         IF ( Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR. Itype==25 .OR. Itype==26 .OR.              &
            & Itype==27 .OR. Itype==28 .OR. Itype==29 .OR. Itype==30 .OR. Itype==31 ) THEN
         ELSEIF ( Itype==34 ) THEN
            GOTO 2200
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
!     ROD
!
 600  Matid = Iecpt(4)
   ASSIGN 700 TO yessd
   GOTO 2500
 700  Xecpt(18) = 0.0
   Xecpt(19) = 0.0
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(20) = E
   IF ( phase1 ) THEN
      nwds = 20
      GOTO 2600
   ELSE
      Xecpt(21) = 0.0
      nwds = 21
      GOTO 3800
   ENDIF
!
!     TUBE
!
 800  Matid = Iecpt(4)
   ASSIGN 900 TO yessd
   GOTO 2500
 900  Xecpt(17) = 0.0
   Xecpt(18) = 0.0
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(19) = E
   IF ( phase1 ) THEN
      nwds = 19
      GOTO 2600
   ELSE
      Xecpt(20) = 0.0
      nwds = 20
      GOTO 3800
   ENDIF
!
!     TRIA1
!
 1000 Matid = Iecpt(6)
   ASSIGN 1100 TO yessd
   GOTO 2500
 1100 DO i = 28 , 33
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(30) = E
   IF ( phase1 ) THEN
      nwds = 33
      GOTO 2600
   ELSE
      DO i = 34 , 38
         Xecpt(i) = 0.0
      ENDDO
      nwds = 38
      GOTO 3800
   ENDIF
!
!     TRMEM
!
 1200 Matid = Iecpt(6)
   ASSIGN 1300 TO yessd
   GOTO 2500
 1300 DO i = 22 , 27
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(24) = E
   nwds = 27
   IF ( .NOT.(phase1) ) GOTO 3800
   GOTO 2600
!
!     QDMEM
!
 1400 Matid = Iecpt(7)
   ASSIGN 1500 TO yessd
   GOTO 2500
 1500 DO i = 27 , 32
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(29) = E
   nwds = 32
   IF ( .NOT.(phase1) ) GOTO 3800
   GOTO 2600
!
!     TRIA2
!
 1600 Matid = Iecpt(6)
   ASSIGN 1700 TO yessd
   GOTO 2500
 1700 DO i = 22 , 27
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(24) = E
   IF ( phase1 ) THEN
      nwds = 27
      GOTO 2600
   ELSE
      DO i = 28 , 32
         Xecpt(i) = 0.0
      ENDDO
      nwds = 32
      GOTO 3800
   ENDIF
!
!     QUAD2
!
 1800 Matid = Iecpt(7)
   ASSIGN 1900 TO yessd
   GOTO 2500
 1900 DO i = 27 , 32
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(29) = E
   IF ( phase1 ) THEN
      nwds = 32
      GOTO 2600
   ELSE
      DO i = 33 , 37
         Xecpt(i) = 0.0
      ENDDO
      nwds = 37
      GOTO 3800
   ENDIF
!
!     QUAD1
!
 2000 Matid = Iecpt(7)
   ASSIGN 2100 TO yessd
   GOTO 2500
 2100 DO i = 33 , 38
      Xecpt(i) = 0.0
   ENDDO
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(35) = E
   IF ( phase1 ) THEN
      nwds = 38
      GOTO 2600
   ELSE
      DO i = 39 , 43
         Xecpt(i) = 0.0
      ENDDO
      nwds = 43
      GOTO 3800
   ENDIF
!
!     BAR - IF COORDINATE 1 OF EITHER PT. A OR PT. B IS PINNED THE
!           ELEMENT IS TREATED AS LINEAR (NON-STRESS DEPENDENT)
!
 2200 IF ( Iecpt(8)/=0 .OR. Iecpt(9)/=0 ) THEN
      ka = Iecpt(8)
      kb = Iecpt(9)
      DO WHILE ( mod(ka,10)/=1 .AND. mod(kb,10)/=1 )
         ka = ka/10
         kb = kb/10
         IF ( ka<=0 .AND. kb<=0 ) GOTO 2300
      ENDDO
      GOTO 2700
   ENDIF
 2300 Matid = Iecpt(16)
   ASSIGN 2400 TO yessd
   GOTO 2500
 2400 Xecpt(43) = 0.0
   Xecpt(44) = 0.0
   Inflag = 1
   CALL mat(Iecpt(1))
   Xecpt(45) = E
   IF ( phase1 ) THEN
      nwds = 45
      GOTO 2600
   ELSE
      DO i = 46 , 50
         Xecpt(i) = 0.0
      ENDDO
      nwds = 50
      GOTO 3800
   ENDIF
!
!     TEST TO SEE IF ELEMENT IS STRESS DEPENDENT.
!
 2500 Inflag = 5
   CALL mat(Iecpt(1))
   IF ( Indstr<=0 ) THEN
      GOTO nosd
   ELSE
      GOTO yessd
   ENDIF
!
!     WRITE AN ENTRY ONTO ECPTNL
!
 2600 IF ( npvtwr<=0 ) THEN
      npvtwr = 1
      CALL write(Ecptnl,Npvt,1,Neor)
      Kickof = 1
   ENDIF
   CALL write(Ecptnl,Itype,1,Neor)
   CALL write(Ecptnl,Xecpt,nwds,Neor)
   Nnlel = Nnlel + 1
   GOTO 500
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT,
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
 2700 Kgglpg = 1
   IF ( ifirst==1 ) THEN
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
      IF ( Link(itemp)==1 ) GOTO 500
!
!     SET THE CURRENT LINK IN CORE = ITEMP AND IFIRST = 0
!
      lincor = itemp
      ifirst = 0
!
!     THIS IS NOT THE FIRST PASS.  IF ITYPE(TH) ELEMENT ROUTINE IS IN
!     CORE, PROCESS IT.
!
   ELSEIF ( itemp/=lincor ) THEN
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
      GOTO 500
   ENDIF
!
!     CALL THE PROPER ELEMENT ROUTINE.
!
!            CROD  CBEAM  CTUBE  CSHEAR  CTWIST  CTRIA1  CTRBSC
!              1     2      3       4       5       6       7
!          CTRPLT  CTRMEM  CONROD  ELAS1  ELAS2  ELAS3  ELAS4
!             8       9      10      11     12     13     14
!          CQDPLT  CQDMEM  CTRIA2  CQUAD2 CQUAD1  CDAMP1  CDAMP2
!            15      16      17      18      19     20      21
!          CDAMP3  CDAMP4  CVISC  CMASS1  CMASS2  CMASS3  CMASS4
!            22      23      24     25      26      27      28
!           CONM1  CONM2   PLOTEL REACT   QUAD3   CBAR    CCONE
!             29     30      31     32      33     34       35
   IF ( Itype<=35 ) THEN
      IF ( Itype==2 .OR. Itype==32 .OR. Itype==33 ) GOTO 4800
      IF ( Itype==3 ) THEN
         CALL ktube
      ELSEIF ( Itype==4 ) THEN
         CALL kpanel(4)
      ELSEIF ( Itype==5 ) THEN
         CALL kpanel(5)
      ELSEIF ( Itype==6 ) THEN
         CALL ktriqd(1)
      ELSEIF ( Itype==7 ) THEN
         CALL ktrbsc(0)
      ELSEIF ( Itype==8 ) THEN
         CALL ktrplt
      ELSEIF ( Itype==9 ) THEN
         CALL ktrmem(0)
      ELSEIF ( Itype==11 ) THEN
         CALL kelas(1)
      ELSEIF ( Itype==12 ) THEN
         CALL kelas(2)
      ELSEIF ( Itype==13 ) THEN
         CALL kelas(3)
      ELSEIF ( Itype==14 ) THEN
         CALL kelas(4)
      ELSEIF ( Itype==15 ) THEN
         CALL kqdplt
      ELSEIF ( Itype==16 ) THEN
         CALL kqdmem
      ELSEIF ( Itype==17 ) THEN
         CALL ktriqd(2)
      ELSEIF ( Itype==18 ) THEN
         CALL ktriqd(4)
      ELSEIF ( Itype==19 ) THEN
         CALL ktriqd(3)
      ELSEIF ( Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR. Itype==25 .OR. Itype==26 .OR.             &
             & Itype==27 .OR. Itype==28 .OR. Itype==29 .OR. Itype==30 .OR. Itype==31 ) THEN
      ELSEIF ( Itype==34 ) THEN
         CALL kbar
      ELSEIF ( Itype==35 ) THEN
         IF ( Nbpw<=32 ) CALL kconed
         IF ( Nbpw>32 ) CALL kcones
      ELSE
         CALL krod
      ENDIF
   ENDIF
   GOTO 500
!
!     AN END OF LOGICAL RECORD HAS BEEN HIT ON THE ECPT.  IF NPVTWR = 0,
!     THE PIVOT POINT HAS NOT BEEN WRITTEN ON ECPTNL AND NO ELEMENTS IN
!     THE CURRENT ECPT RECORD ARE PLASTIC.
!
 2800 IF ( ipass==1 ) THEN
      IF ( npvtwr<=0 ) THEN
         CALL write(Ecptnl,-Npvt,1,Eor)
      ELSE
         CALL write(Ecptnl,0,0,Eor)
      ENDIF
   ENDIF
   ipass = 2
   Link(lincor) = 1
   DO i = 1 , Nlinks
      IF ( Link(i)==0 ) GOTO 2900
   ENDDO
!
!     WRITE THE NO. OF ROWS IN CORE UNTO THE KGGL FILE USING ZBLPKI.
!
   i1 = 0
   GOTO 3000
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED
!
 2900 CALL bckrec(Ecpt)
   GOTO 400
 3000 i2 = 0
   ibeg = I6x6k + i1*Jmax
   CALL bldpk(2,ipr,Kggl,0,0)
   DO
      i2 = i2 + 1
      IF ( i2>Ngpct ) THEN
         CALL bldpkn(Kggl,0,Mcbkgg)
         i1 = i1 + 1
         IF ( i1<Nrowsc ) GOTO 3000
!
!     IF LROWIC = TNROWS, PROCESSING OF THE CURRENT ECPT RECORD HAS BEEN
!     COMPLETED.
!
         IF ( Lrowic==Tnrows ) GOTO 300
         CALL bckrec(Ecpt)
         Frowic = Frowic + Nrowsc
         Lrowic = Lrowic + Nrowsc
         ipass = 2
         GOTO 400
      ELSE
         jj = Igpct + i2
         Index = iabs(Iz(jj)) - 1
         lim = 6
         IF ( Iz(jj)<0 ) lim = 1
         jjj = Ipoint + i2
         kkk = ibeg + Iz(jjj) - 1
         i3 = 0
         DO
            i3 = i3 + 1
            IF ( i3>lim ) EXIT
            Index = Index + 1
            kkk = kkk + 1
            Dpword = Dz(kkk)
            IF ( Dpword/=0.0D0 ) CALL zblpki
         ENDDO
      ENDIF
   ENDDO
!
!     NO ELEMENTS ARE CONNECTED TO THE PIVOT POINT.  OUTPUT ZERO
!     COLUMN(S).  ALSO, WRITE NEGATIVE PIVOT POINT ON ECPTNL.
!
 3100 lim = 6
   IF ( inpvt(1)<0 ) lim = 1
   DO i = 1 , lim
      CALL bldpk(2,ipr,Kggl,0,0)
      CALL bldpkn(Kggl,0,Mcbkgg)
   ENDDO
   CALL skprec(Ecpt,1)
   CALL write(Ecptnl,-iabs(inpvt(1)),1,Eor)
   GOTO 300
!
!     ECPT PROCESSING HAS BEEN COMPLETED SINCE AN EOF HAS BEEN READ ON
!     GPCT.
!
 3200 CALL close(Gpct,Clsrw)
   CALL close(Ecpt,Clsrw)
   CALL close(Kggl,Clsrw)
   CALL close(Ecptnl,Clsrw)
   IF ( Kickof==-1 ) GOTO 4400
   IF ( Mcbkgg(6)/=0 ) THEN
      Mcbkgg(3) = Mcbkgg(2)
   ELSE
      DO i = 2 , 7
         Mcbkgg(i) = 0
      ENDDO
   ENDIF
   CALL wrttrl(Mcbkgg)
   CALL wrttrl(Trail)
!
!     BEGIN EST PROCESSING
!
   Left = Bufr4 - 1
   icc = Ncstm + mused
   all = .FALSE.
   phase1 = .FALSE.
!
!     READ THE FIRST RECORD OF CASECC INTO CORE.
!
   file = casecc
   CALL gopen(casecc,Z(Bufr1),0)
   CALL read(*4600,*3300,casecc,Iz(icc+1),Left,Eor,ncc)
   CALL mesage(-8,0,name)
 3300 iplset = icc + jplset
   Plaset = Iz(iplset)
   istset = icc + jstset
   IF ( Iz(istset)<0 ) THEN
      all = .TRUE.
   ELSEIF ( Iz(istset)==0 ) THEN
      Nonlst = -1
   ELSE
!
!     THE USER HAS REQUESTED A PROPER SUBSET OF HIS SET OF ELEMENTS FOR
!     WHICH HE WANTS STRESS OUTPUT.  FIND THE SET IN OPEN CORE AND
!     DETERMINE ZERO POINTER AND LENGTH OF THE SET.
!
      isym = icc + jsym
      isetno = isym + Iz(isym) + 1
      lset = Iz(isetno+1)
      DO
         iset = isetno + 2
         nset = Iz(isetno+1) + iset - 1
         IF ( Iz(isetno)==Iz(istset) ) THEN
            Iz(nset+1) = 2**14 + 1
            EXIT
         ELSE
            isetno = nset + 1
            IF ( isetno>=ncc ) THEN
               all = .TRUE.
               Iz(nset+1) = 2**14 + 1
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   CALL close(casecc,Clsrw)
   IF ( Plaset/=-1 ) THEN
!
!     SEARCH THE MPT FOR THE PLA SET
!
      file = Mpt
      CALL preloc(*4500,Z(Bufr1-3),Mpt)
      CALL locate(*4900,Z(Bufr1-3),planos,iflag)
   ELSE
      jj = 1
      Plfact(1) = 1.0
      GOTO 3500
   ENDIF
!
!     READ A PLA SET NO.
!
 3400 CALL read(*4900,*4900,Mpt,setno,1,Neor,iflag)
   jj = 0
   DO
      CALL read(*4900,*4900,Mpt,nn,1,Neor,iflag)
      IF ( nn==-1 ) THEN
         IF ( setno/=Plaset ) GOTO 3400
         Nplalp = jj
         Plfact(2) = 0.0
         CALL close(Mpt,Clsrw)
         EXIT
      ELSE
         jj = jj + 1
         IF ( jj==1 ) Plfact(1) = fnn
      ENDIF
   ENDDO
!
!     PROCESS THE EST
!
 3500 Estltr(1) = Estl
   Estnlt(1) = Estnl
   DO i = 2 , 7
      Estltr(i) = 0
      Estnlt(i) = 0
   ENDDO
   ASSIGN 4000 TO nosd
   CALL gopen(est,Z(Bufr1),0)
   CALL gopen(Estl,Z(Bufr2),1)
   CALL gopen(Estnl,Z(Bufr3),1)
   file = est
 3600 DO
!
!     READ THE ELEMENT TYPE.  IF THE ELEMENT TYPE IS ADMISSIBLE TO
!     PIECEWISE LINEAR ANALYSIS, WRITE IT TWICE.  OTHERWISE GO TO NEXT
!     RECORD.
!
      CALL read(*4300,*4700,est,Itype,1,Neor,iflag)
      IF ( plaary(Itype)==1 ) THEN
         CALL write(Estl,Itype,1,Neor)
         CALL write(Estnl,Itype,1,Neor)
         EXIT
      ELSE
         CALL skprec(est,1)
      ENDIF
   ENDDO
!
!     READ THE EST ENTRY
!
 3700 idx = (Itype-1)*Incr
   nwds = Ne(idx+12)
   CALL read(*4600,*4200,est,Xecpt,nwds,Neor,iflag)
   IF ( plaary(Itype)==0 ) GOTO 4000
   IF ( Itype>38 ) GOTO 4000
!              CROD      CBEAM     CTUBE     CSHEAR    CTWIST
!                1         2         3         4         5
!              CTRIA1    CTRBSC    CTRPLT    CTRMEM    CONROD
!                6         7         8          9       10
!              CELAS1    CELAS2    CELAS3    CELAS4    CQDPLT
!                11        12        13        14        15
!              CQDMEM    CTRIA2    CQUAD2    CQUAD1    CDAMP1
!                16        17        18        19        20
!              CDAMP2    CDAMP3    CDAMP4    CVISC     CMASS1
!                21        22        23        24        25
!              CMASS2    CMASS3    CMASS4    CONM1     CONM2
!                26        27        28        29        30
!              PLOTEL    REACT     QUAD3     CBAR      CCONE
!                31        32        33        34        35
!              CTRIARG   CTRAPRG   CTORDRG
!                36        37        38
   IF ( Itype==1 .OR. Itype==10 ) GOTO 600
   IF ( Itype==2 .OR. Itype==4 .OR. Itype==5 .OR. Itype==7 .OR. Itype==8 .OR. Itype==11 .OR. Itype==12 .OR. Itype==13 .OR.          &
      & Itype==14 .OR. Itype==15 .OR. Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR. Itype==25 .OR.     &
      & Itype==26 .OR. Itype==27 .OR. Itype==28 .OR. Itype==29 .OR. Itype==30 .OR. Itype==31 .OR. Itype==32 .OR. Itype==33 .OR.     &
      & Itype==35 .OR. Itype==36 .OR. Itype==37 .OR. Itype==38 ) GOTO 4000
   IF ( Itype==3 ) GOTO 800
   IF ( Itype==6 ) GOTO 1000
   IF ( Itype==9 ) GOTO 1200
   IF ( Itype==16 ) GOTO 1400
   IF ( Itype==17 ) GOTO 1600
   IF ( Itype==18 ) GOTO 1800
   IF ( Itype==19 ) GOTO 2000
   IF ( Itype==34 ) GOTO 2200
!
!     THE ELEMENT IS STRESS DEPENDENT.  DETERMINE IF STRESS OUTPUT IS
!     REQUESTED.
!     AN EXAMPLE... IF WE HAVE IN CASE CONTROL
!     SET 5 = 1,2,3,98THRU100,4THRU15,81,18,82,90,92
!     THEN THE WORDS IN CASE CONTROL ARE...
!       IZ(ISETNO) = 5,12,1,2,3,4,-15,18,81,82,90,92,98,-100 = IZ(NSET)
!
 3800 IF ( .NOT.(all) ) THEN
      IF ( Nonlst==-1 ) GOTO 3700
      ielid = Iecpt(1)
      i = iset
      DO WHILE ( i<=nset )
         IF ( Iz(i+1)<0 ) THEN
            IF ( ielid>=Iz(i) .AND. ielid<=iabs(Iz(i+1)) ) GOTO 3900
            IF ( ielid<Iz(i) ) EXIT
            i = i + 2
         ELSE
            IF ( ielid==Iz(i) ) GOTO 3900
            IF ( ielid<Iz(i) ) EXIT
            i = i + 1
         ENDIF
         IF ( Iz(i)<=0 ) THEN
            all = .TRUE.
            Llllll(1) = Iz(istset)
            Llllll(2) = Iz(i)
            CALL mesage(30,92,Llllll)
            GOTO 3900
         ENDIF
      ENDDO
      GOTO 3700
   ENDIF
 3900 outfil = Estnl
   Nnlel = Nnlel + 1
   GOTO 4100
 4000 outfil = Estl
   Nlel = Nlel + 1
 4100 CALL write(outfil,Xecpt,nwds,Neor)
   GOTO 3700
 4200 CALL write(Estl,0,0,Eor)
   CALL write(Estnl,0,0,Eor)
   GOTO 3600
!
!     WRAP UP ROUTINE
!
 4300 CALL close(est,Clsrw)
   CALL close(Estl,Clsrw)
   CALL close(Estnl,Clsrw)
   CALL wrttrl(Estltr)
   CALL wrttrl(Estnlt)
 4400 RETURN
!
!     FATAL ERRORS
!
 4500 CALL mesage(-1,file,name)
 4600 CALL mesage(-2,file,name)
 4700 CALL mesage(-3,file,name)
 4800 CALL mesage(-30,87,Itype)
!
!     UNABLE TO FIND PLFACT CARD IN THE MPT WHICH WAS CHOSEN BY THE USER
!     IN CASECC.
!
 4900 Trail(1) = hmpt
   Trail(2) = name(1)
   CALL mesage(-32,Plaset,Trail)
END SUBROUTINE pla1
