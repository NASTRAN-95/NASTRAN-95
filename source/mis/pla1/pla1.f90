!*==pla1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla1
USE C_BLANK
USE C_GPTA1
USE C_MATIN
USE C_MATOUT
USE C_SMA1BK
USE C_SMA1CL
USE C_SMA1DP
USE C_SMA1ET
USE C_SMA1HT
USE C_SMA1IO
USE C_SYSTEM
USE C_XMSSG
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: all , phase1
   INTEGER , SAVE :: casecc , jplset , jstset , jsym
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: e , fnn
   INTEGER :: est , file , i , i1 , i2 , i3 , ibeg , icc , idx , ielid , ifirst , iflag , ilast , ileft , imat , inc , ipass ,      &
            & iplset , ipr , iset , isetno , istset , isym , itemp , izmax , j , jj , jjj , jlast , ka , kb , kkk , lim , lincor ,  &
            & low , lset , mused , ncc , nlel , nn , nnlel , nosd , npvtwr , nset , nwds , outfil , setno , yessd
   INTEGER , DIMENSION(7) :: estltr , estnlt
   REAL , SAVE :: hmpt
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(2) :: inpvt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name , planos
   INTEGER , DIMENSION(90) , SAVE :: plaary
   EXTERNAL bckrec , bldpk , bldpkn , close , delset , fread , fwdrec , gopen , kbar , kconed , kcones , kelas , korsz , kpanel ,   &
          & kqdmem , kqdplt , krod , ktrbsc , ktriqd , ktrmem , ktrplt , ktube , locate , makmcb , mat , mesage , open , preloc ,   &
          & premat , pretrd , pretrs , rdtrl , read , skprec , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1)) , (Iecpt(1),Xecpt(1)) , (Mcbkgg(1),Estltr(1)) , (Trail(1),Estnlt(1)) , (Trail(2),Nnlel) ,         &
!>>>>    & (Estltr(2),Nlel) , (fnn,nn) , (Indstr,E)
   DATA casecc , jstset , jplset , jsym/106 , 23 , 164 , 200/
   DATA name/4HPLA1 , 4H    / , hmpt/4HMPT /
   DATA planos/1103 , 11/
   DATA plaary/1 , 0 , 1 , 0 , 0 , 1 , 0 , 0 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 30*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         ASSIGN 240 TO nosd
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
99001    FORMAT (A25,' 2151, -PLAARY- ARRAY IS SMALLER THAN MAXIMUM ','NUMBER OF ELEMENT TYPES.')
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
         CALL open(*40,Cstm,Z(Bufr2),Inrw)
         CALL fwdrec(*420,Cstm)
         CALL read(*420,*20,Cstm,Z(Icstm+1),Left,Eor,Ncstm)
!
!     INSUFFICIENT CORE - CALL MESAGE
!
         CALL mesage(-8,0,name)
 20      Left = Left - Ncstm
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
 40      imat = Ncstm
         CALL premat(iz(imat+1),Z(imat+1),Z(Bufr2-3),Left,mused,Mpt,-Dit)
         Left = Left - mused
         Igpct = Ncstm + mused
!
!     OPEN THE INPUT FILES ECPT AND GPCT AND THE OUTPUT FILE ECPTNL.
!
         CALL gopen(Ecpt,Z(Bufr2),0)
         CALL gopen(Gpct,Z(Bufr3),0)
         CALL gopen(Ecptnl,Z(Bufr4),1)
         ileft = Left
         spag_nextblock_1 = 2
      CASE (2)
!
!     BEGIN MAIN LOOP FOR PROCESSING THE ECPT.
!
         CALL read(*300,*280,Gpct,inpvt(1),2,Neor,iflag)
         Ngpct = inpvt(2)
         Left = ileft - 2*Ngpct
         IF ( Left<=0 ) CALL mesage(-8,0,name)
         CALL fread(Gpct,iz(Igpct+1),Ngpct,Eor)
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
         iz(Ipoint+1) = 1
         i1 = 1
         i = Igpct
         j = Ipoint + 1
         SPAG_Loop_1_2: DO
            i1 = i1 + 1
            IF ( i1>Ngpct ) THEN
!
!     JMAX = THE NO. OF COLUMNS OF KGGL THAT WILL BE GENERATED WITH THE
!     CURRENT PIVOT POINT.
!
               inc = 5
               ilast = Igpct + Ngpct
               jlast = Ipoint + Npoint
               IF ( iz(ilast)<0 ) inc = 0
               Jmax = iz(jlast) + inc
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
                  SPAG_Loop_2_1: DO
                     plaary(40) = Npvt
                     CALL mesage(30,85,plaary(39))
                     IF ( 2*Nrowsc*Jmax<Left ) EXIT SPAG_Loop_2_1
                     Nrowsc = Nrowsc - 1
                     IF ( Nrowsc==0 ) CALL mesage(-8,0,name)
                  ENDDO SPAG_Loop_2_1
               ENDIF
               Frowic = 1
               Lrowic = Frowic + Nrowsc - 1
!
!     ZERO OUT THE KGGL SUBMATRIX IN CORE.
!
               low = I6x6k + 1
               lim = I6x6k + Jmax*Nrowsc
               DO i = low , lim
                  dz(i) = 0.0D0
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
               EXIT SPAG_Loop_1_2
            ELSE
               i = i + 1
               j = j + 1
               inc = 6
               IF ( iz(i)<0 ) inc = 1
               iz(j) = iz(j-1) + inc
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 3
      CASE (3)
         ifirst = 1
!
!     READ THE FIRST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT
!
         CALL fread(Ecpt,Npvt,1,Neor)
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_3: DO
!
!     READ THE NEXT ELEMENT TYPE INTO ITYPE, AND READ THE PRESCRIBED NO.
!     OF WORDS INTO THE XECPT ARRAY.
!
            CALL read(*420,*260,Ecpt,Itype,1,Neor,iflag)
            idx = (Itype-1)*Incr
            nn = Ne(idx+12)
            CALL fread(Ecpt,Xecpt,nn,Neor)
            itemp = Ne(idx+22)
            IF ( ipass/=1 ) GOTO 240
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
               IF ( Itype==2 .OR. Itype==32 .OR. Itype==33 ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==3 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==4 .OR. Itype==5 .OR. Itype==7 .OR. Itype==8 .OR. Itype==11 .OR. Itype==12 .OR. Itype==13 .OR.            &
                  & Itype==14 .OR. Itype==15 .OR. Itype==35 ) GOTO 240
               IF ( Itype==6 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==9 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==16 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==17 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==18 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==19 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR. Itype==25 .OR. Itype==26 .OR.        &
                  & Itype==27 .OR. Itype==28 .OR. Itype==29 .OR. Itype==30 .OR. Itype==31 ) THEN
               ELSEIF ( Itype==34 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  EXIT SPAG_Loop_1_3
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 5
      CASE (5)
!
!     ROD
!
         Matid = iecpt(4)
         ASSIGN 60 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 60      Xecpt(18) = 0.0
         Xecpt(19) = 0.0
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(20) = e
         IF ( phase1 ) THEN
            nwds = 20
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Xecpt(21) = 0.0
            nwds = 21
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (6)
!
!     TUBE
!
         Matid = iecpt(4)
         ASSIGN 80 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 80      Xecpt(17) = 0.0
         Xecpt(18) = 0.0
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(19) = e
         IF ( phase1 ) THEN
            nwds = 19
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Xecpt(20) = 0.0
            nwds = 20
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (7)
!
!     TRIA1
!
         Matid = iecpt(6)
         ASSIGN 100 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 100     DO i = 28 , 33
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(30) = e
         IF ( phase1 ) THEN
            nwds = 33
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 34 , 38
               Xecpt(i) = 0.0
            ENDDO
            nwds = 38
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (8)
!
!     TRMEM
!
         Matid = iecpt(6)
         ASSIGN 120 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 120     DO i = 22 , 27
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(24) = e
         nwds = 27
         IF ( phase1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     QDMEM
!
         Matid = iecpt(7)
         ASSIGN 140 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 140     DO i = 27 , 32
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(29) = e
         nwds = 32
         IF ( phase1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
!
!     TRIA2
!
         Matid = iecpt(6)
         ASSIGN 160 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 160     DO i = 22 , 27
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(24) = e
         IF ( phase1 ) THEN
            nwds = 27
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 28 , 32
               Xecpt(i) = 0.0
            ENDDO
            nwds = 32
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (11)
!
!     QUAD2
!
         Matid = iecpt(7)
         ASSIGN 180 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 180     DO i = 27 , 32
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(29) = e
         IF ( phase1 ) THEN
            nwds = 32
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 33 , 37
               Xecpt(i) = 0.0
            ENDDO
            nwds = 37
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (12)
!
!     QUAD1
!
         Matid = iecpt(7)
         ASSIGN 200 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 200     DO i = 33 , 38
            Xecpt(i) = 0.0
         ENDDO
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(35) = e
         IF ( phase1 ) THEN
            nwds = 38
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 39 , 43
               Xecpt(i) = 0.0
            ENDDO
            nwds = 43
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (13)
!
!     BAR - IF COORDINATE 1 OF EITHER PT. A OR PT. B IS PINNED THE
!           ELEMENT IS TREATED AS LINEAR (NON-STRESS DEPENDENT)
!
         IF ( iecpt(8)/=0 .OR. iecpt(9)/=0 ) THEN
            ka = iecpt(8)
            kb = iecpt(9)
            DO WHILE ( mod(ka,10)/=1 .AND. mod(kb,10)/=1 )
               ka = ka/10
               kb = kb/10
               IF ( ka<=0 .AND. kb<=0 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            GOTO 240
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
         Matid = iecpt(16)
         ASSIGN 220 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 220     Xecpt(43) = 0.0
         Xecpt(44) = 0.0
         Inflag = 1
         CALL mat(iecpt(1))
         Xecpt(45) = e
         IF ( phase1 ) THEN
            nwds = 45
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 46 , 50
               Xecpt(i) = 0.0
            ENDDO
            nwds = 50
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (15)
!
!     TEST TO SEE IF ELEMENT IS STRESS DEPENDENT.
!
         Inflag = 5
         CALL mat(iecpt(1))
         IF ( Indstr<=0 ) THEN
            GOTO nosd
         ELSE
            GOTO yessd
         ENDIF
      CASE (16)
!
!     WRITE AN ENTRY ONTO ECPTNL
!
         IF ( npvtwr<=0 ) THEN
            npvtwr = 1
            CALL write(Ecptnl,Npvt,1,Neor)
            Kickof = 1
         ENDIF
         CALL write(Ecptnl,Itype,1,Neor)
         CALL write(Ecptnl,Xecpt,nwds,Neor)
         nnlel = nnlel + 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT,
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
 240     Kgglpg = 1
         IF ( ifirst==1 ) THEN
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
            IF ( Link(itemp)==1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
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
            IF ( Itype==2 .OR. Itype==32 .OR. Itype==33 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            ELSEIF ( Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR. Itype==25 .OR. Itype==26 .OR.       &
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
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     AN END OF LOGICAL RECORD HAS BEEN HIT ON THE ECPT.  IF NPVTWR = 0,
!     THE PIVOT POINT HAS NOT BEEN WRITTEN ON ECPTNL AND NO ELEMENTS IN
!     THE CURRENT ECPT RECORD ARE PLASTIC.
!
 260     IF ( ipass==1 ) THEN
            IF ( npvtwr<=0 ) THEN
               CALL write(Ecptnl,-Npvt,1,Eor)
            ELSE
               CALL write(Ecptnl,0,0,Eor)
            ENDIF
         ENDIF
         ipass = 2
         Link(lincor) = 1
         DO i = 1 , Nlinks
            IF ( Link(i)==0 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     WRITE THE NO. OF ROWS IN CORE UNTO THE KGGL FILE USING ZBLPKI.
!
         i1 = 0
         SPAG_Loop_1_4: DO
            i2 = 0
            ibeg = I6x6k + i1*Jmax
            CALL bldpk(2,ipr,Kggl,0,0)
            DO
               i2 = i2 + 1
               IF ( i2>Ngpct ) THEN
                  CALL bldpkn(Kggl,0,Mcbkgg)
                  i1 = i1 + 1
                  IF ( i1<Nrowsc ) CYCLE SPAG_Loop_1_4
!
!     IF LROWIC = TNROWS, PROCESSING OF THE CURRENT ECPT RECORD HAS BEEN
!     COMPLETED.
!
                  IF ( Lrowic==Tnrows ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL bckrec(Ecpt)
                  Frowic = Frowic + Nrowsc
                  Lrowic = Lrowic + Nrowsc
                  ipass = 2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  jj = Igpct + i2
                  Index = iabs(iz(jj)) - 1
                  lim = 6
                  IF ( iz(jj)<0 ) lim = 1
                  jjj = Ipoint + i2
                  kkk = ibeg + iz(jjj) - 1
                  i3 = 0
                  SPAG_Loop_3_5: DO
                     i3 = i3 + 1
                     IF ( i3>lim ) EXIT SPAG_Loop_3_5
                     Index = Index + 1
                     kkk = kkk + 1
                     Dpword = dz(kkk)
                     IF ( Dpword/=0.0D0 ) CALL zblpki
                  ENDDO SPAG_Loop_3_5
               ENDIF
            ENDDO
            GOTO 280
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 17
      CASE (17)
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED
!
         CALL bckrec(Ecpt)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     NO ELEMENTS ARE CONNECTED TO THE PIVOT POINT.  OUTPUT ZERO
!     COLUMN(S).  ALSO, WRITE NEGATIVE PIVOT POINT ON ECPTNL.
!
 280     lim = 6
         IF ( inpvt(1)<0 ) lim = 1
         DO i = 1 , lim
            CALL bldpk(2,ipr,Kggl,0,0)
            CALL bldpkn(Kggl,0,Mcbkgg)
         ENDDO
         CALL skprec(Ecpt,1)
         CALL write(Ecptnl,-iabs(inpvt(1)),1,Eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ECPT PROCESSING HAS BEEN COMPLETED SINCE AN EOF HAS BEEN READ ON
!     GPCT.
!
 300     CALL close(Gpct,Clsrw)
         CALL close(Ecpt,Clsrw)
         CALL close(Kggl,Clsrw)
         CALL close(Ecptnl,Clsrw)
         IF ( Kickof==-1 ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         CALL read(*420,*320,casecc,iz(icc+1),Left,Eor,ncc)
         CALL mesage(-8,0,name)
 320     iplset = icc + jplset
         Plaset = iz(iplset)
         istset = icc + jstset
         IF ( iz(istset)<0 ) THEN
            all = .TRUE.
         ELSEIF ( iz(istset)==0 ) THEN
            Nonlst = -1
         ELSE
!
!     THE USER HAS REQUESTED A PROPER SUBSET OF HIS SET OF ELEMENTS FOR
!     WHICH HE WANTS STRESS OUTPUT.  FIND THE SET IN OPEN CORE AND
!     DETERMINE ZERO POINTER AND LENGTH OF THE SET.
!
            isym = icc + jsym
            isetno = isym + iz(isym) + 1
            lset = iz(isetno+1)
            SPAG_Loop_1_6: DO
               iset = isetno + 2
               nset = iz(isetno+1) + iset - 1
               IF ( iz(isetno)==iz(istset) ) THEN
                  iz(nset+1) = 2**14 + 1
                  EXIT SPAG_Loop_1_6
               ELSE
                  isetno = nset + 1
                  IF ( isetno>=ncc ) THEN
                     all = .TRUE.
                     iz(nset+1) = 2**14 + 1
                     EXIT SPAG_Loop_1_6
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_6
         ENDIF
         CALL close(casecc,Clsrw)
         IF ( Plaset/=-1 ) THEN
!
!     SEARCH THE MPT FOR THE PLA SET
!
            file = Mpt
            CALL preloc(*400,Z(Bufr1-3),Mpt)
            CALL locate(*460,Z(Bufr1-3),planos,iflag)
         ELSE
            jj = 1
            Plfact(1) = 1.0
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_7: DO
!
!     READ A PLA SET NO.
!
            CALL read(*460,*460,Mpt,setno,1,Neor,iflag)
            jj = 0
            SPAG_Loop_2_8: DO
               CALL read(*460,*460,Mpt,nn,1,Neor,iflag)
               IF ( nn==-1 ) THEN
                  IF ( setno/=Plaset ) CYCLE SPAG_Loop_1_7
                  Nplalp = jj
                  Plfact(2) = 0.0
                  CALL close(Mpt,Clsrw)
                  EXIT SPAG_Loop_2_8
               ELSE
                  jj = jj + 1
                  IF ( jj==1 ) Plfact(1) = fnn
               ENDIF
            ENDDO SPAG_Loop_2_8
            EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
         spag_nextblock_1 = 18
      CASE (18)
!
!     PROCESS THE EST
!
         estltr(1) = Estl
         estnlt(1) = Estnl
         DO i = 2 , 7
            estltr(i) = 0
            estnlt(i) = 0
         ENDDO
         ASSIGN 340 TO nosd
         CALL gopen(est,Z(Bufr1),0)
         CALL gopen(Estl,Z(Bufr2),1)
         CALL gopen(Estnl,Z(Bufr3),1)
         file = est
         spag_nextblock_1 = 19
      CASE (19)
         SPAG_Loop_1_9: DO
!
!     READ THE ELEMENT TYPE.  IF THE ELEMENT TYPE IS ADMISSIBLE TO
!     PIECEWISE LINEAR ANALYSIS, WRITE IT TWICE.  OTHERWISE GO TO NEXT
!     RECORD.
!
            CALL read(*380,*440,est,Itype,1,Neor,iflag)
            IF ( plaary(Itype)==1 ) THEN
               CALL write(Estl,Itype,1,Neor)
               CALL write(Estnl,Itype,1,Neor)
               EXIT SPAG_Loop_1_9
            ELSE
               CALL skprec(est,1)
            ENDIF
         ENDDO SPAG_Loop_1_9
         spag_nextblock_1 = 20
      CASE (20)
!
!     READ THE EST ENTRY
!
         idx = (Itype-1)*Incr
         nwds = Ne(idx+12)
         CALL read(*420,*360,est,Xecpt,nwds,Neor,iflag)
         IF ( plaary(Itype)==0 ) GOTO 340
         IF ( Itype>38 ) GOTO 340
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
         IF ( Itype==1 .OR. Itype==10 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==2 .OR. Itype==4 .OR. Itype==5 .OR. Itype==7 .OR. Itype==8 .OR. Itype==11 .OR. Itype==12 .OR. Itype==13 .OR.    &
            & Itype==14 .OR. Itype==15 .OR. Itype==20 .OR. Itype==21 .OR. Itype==22 .OR. Itype==23 .OR. Itype==24 .OR.              &
            & Itype==25 .OR. Itype==26 .OR. Itype==27 .OR. Itype==28 .OR. Itype==29 .OR. Itype==30 .OR. Itype==31 .OR.              &
            & Itype==32 .OR. Itype==33 .OR. Itype==35 .OR. Itype==36 .OR. Itype==37 .OR. Itype==38 ) GOTO 340
         IF ( Itype==3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==6 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==9 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==16 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==17 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==18 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==19 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Itype==34 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
!
!     THE ELEMENT IS STRESS DEPENDENT.  DETERMINE IF STRESS OUTPUT IS
!     REQUESTED.
!     AN EXAMPLE... IF WE HAVE IN CASE CONTROL
!     SET 5 = 1,2,3,98THRU100,4THRU15,81,18,82,90,92
!     THEN THE WORDS IN CASE CONTROL ARE...
!       IZ(ISETNO) = 5,12,1,2,3,4,-15,18,81,82,90,92,98,-100 = IZ(NSET)
!
         IF ( .NOT.(all) ) THEN
            IF ( Nonlst/=-1 ) THEN
               ielid = iecpt(1)
               i = iset
               SPAG_Loop_1_10: DO WHILE ( i<=nset )
                  IF ( iz(i+1)<0 ) THEN
                     IF ( ielid>=iz(i) .AND. ielid<=iabs(iz(i+1)) ) THEN
                        spag_nextblock_1 = 22
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( ielid<iz(i) ) EXIT SPAG_Loop_1_10
                     i = i + 2
                  ELSE
                     IF ( ielid==iz(i) ) THEN
                        spag_nextblock_1 = 22
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( ielid<iz(i) ) EXIT SPAG_Loop_1_10
                     i = i + 1
                  ENDIF
                  IF ( iz(i)<=0 ) THEN
                     all = .TRUE.
                     Llllll(1) = iz(istset)
                     Llllll(2) = iz(i)
                     CALL mesage(30,92,Llllll)
                     spag_nextblock_1 = 22
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_1_10
            ENDIF
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
         outfil = Estnl
         nnlel = nnlel + 1
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 340     outfil = Estl
         nlel = nlel + 1
         spag_nextblock_1 = 23
      CASE (23)
         CALL write(outfil,Xecpt,nwds,Neor)
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 360     CALL write(Estl,0,0,Eor)
         CALL write(Estnl,0,0,Eor)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
!
!     WRAP UP ROUTINE
!
 380     CALL close(est,Clsrw)
         CALL close(Estl,Clsrw)
         CALL close(Estnl,Clsrw)
         CALL wrttrl(estltr)
         CALL wrttrl(estnlt)
         spag_nextblock_1 = 24
      CASE (24)
         RETURN
!
!     FATAL ERRORS
!
 400     CALL mesage(-1,file,name)
 420     CALL mesage(-2,file,name)
 440     CALL mesage(-3,file,name)
         spag_nextblock_1 = 25
      CASE (25)
         CALL mesage(-30,87,Itype)
!
!     UNABLE TO FIND PLFACT CARD IN THE MPT WHICH WAS CHOSEN BY THE USER
!     IN CASECC.
!
 460     Trail(1) = hmpt
         Trail(2) = name(1)
         CALL mesage(-32,Plaset,Trail)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla1
