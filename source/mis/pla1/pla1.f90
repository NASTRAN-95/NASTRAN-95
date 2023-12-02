!*==pla1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla1
   USE c_blank
   USE c_gpta1
   USE c_matin
   USE c_matout
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE c_zblpkx
   USE c_zzzzzz
   USE iso_fortran_env
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
         ipr = iprec
         CALL delset
         trail(1) = dit
         CALL rdtrl(trail)
         IF ( trail(1)<0 ) CALL mesage(-1,dit,name)
!
!     INITIALIZE HEAT PARAMETER
!
         heat = .FALSE.
!
!     INITIALIZE MODULE PARAMETERS AND SET IOPT4 = 0, SO THAT ELEMENT
!     ROUTINES WILL NOT CALCULATE STRUCTURAL DAMPING MATRICES.
!
         kgglpg = -1
         nplalp = 1
         kickof = -1
         plaset = -1
         nonlst = 1
         plfact(1) = 1.0
         iopt4 = 0
         estnl = 204
         est = 107
         phase1 = .TRUE.
         ASSIGN 240 TO nosd
!
!     SET UP BUFFERS AND INITIALIZE FILE TRAILERS.
!
         izmax = korsz(z)
         bufr1 = izmax - bufsz
         bufr2 = bufr1 - bufsz
         bufr3 = bufr2 - bufsz
         bufr4 = bufr3 - bufsz
         left = bufr4 - 1
         CALL makmcb(mcbkgg,kggl,0,6,ipr)
         CALL makmcb(trail,ecptnl,0,0,0)
!
!     CHECK PLAARY SIZE
!
         IF ( nelems>90 ) WRITE (isyspt,99001) uwm
99001    FORMAT (A25,' 2151, -PLAARY- ARRAY IS SMALLER THAN MAXIMUM ','NUMBER OF ELEMENT TYPES.')
!
!     OPEN THE KGGL FILE FOR OUTPUT
!
         CALL gopen(kggl,z(bufr1),1)
!
!     ATTEMPT TO READ THE CSTM INTO CORE
!
         icstm = 0
         ncstm = 0
         file = cstm
         CALL open(*40,cstm,z(bufr2),inrw)
         CALL fwdrec(*420,cstm)
         CALL read(*420,*20,cstm,z(icstm+1),left,eor,ncstm)
!
!     INSUFFICIENT CORE - CALL MESAGE
!
         CALL mesage(-8,0,name)
 20      left = left - ncstm
         CALL close(cstm,clsrw)
!
!     CALL PRETRD TO SET UP FUTURE CALLS TO TRANSD.
!
         CALL pretrd(z(icstm+1),ncstm)
         CALL pretrs(z(icstm+1),ncstm)
!
!     CALL PREMAT TO READ THE MPT AND THE DIT AND TO SET UP FUTURE CALLS
!     TO SUBROUTINE MAT.  NOTE NEGATIVE ISGN FOR DIT TO TRIGGER PLA FLAG
!     IN MAT.
!
 40      imat = ncstm
         CALL premat(iz(imat+1),z(imat+1),z(bufr2-3),left,mused,mpt,-dit)
         left = left - mused
         igpct = ncstm + mused
!
!     OPEN THE INPUT FILES ECPT AND GPCT AND THE OUTPUT FILE ECPTNL.
!
         CALL gopen(ecpt,z(bufr2),0)
         CALL gopen(gpct,z(bufr3),0)
         CALL gopen(ecptnl,z(bufr4),1)
         ileft = left
         spag_nextblock_1 = 2
      CASE (2)
!
!     BEGIN MAIN LOOP FOR PROCESSING THE ECPT.
!
         CALL read(*300,*280,gpct,inpvt(1),2,neor,iflag)
         ngpct = inpvt(2)
         left = ileft - 2*ngpct
         IF ( left<=0 ) CALL mesage(-8,0,name)
         CALL fread(gpct,iz(igpct+1),ngpct,eor)
!
!     FROWIC IS THE FIRST ROW IN CORE (1 .LE. FROWIC .LE. 6)
!
         frowic = 1
         ipoint = igpct + ngpct
         npoint = ngpct
         i6x6k = ipoint + npoint
!
!     MAKE I6X6K A DOUBLE PRECISION INDEX (I6X6K POINTS TO THE 0TH
!     LOCATION OF THE 6 X 6 SUBMATRIX OF KGGL IN CORE)
!
         i6x6k = (i6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE WHICH WILL ENABLE SUBROUTINE SMA1B TO
!     ADD THE ELEMENT STIFFNESS MATRICES TO KGGL.
!
         iz(ipoint+1) = 1
         i1 = 1
         i = igpct
         j = ipoint + 1
         SPAG_Loop_1_2: DO
            i1 = i1 + 1
            IF ( i1>ngpct ) THEN
!
!     JMAX = THE NO. OF COLUMNS OF KGGL THAT WILL BE GENERATED WITH THE
!     CURRENT PIVOT POINT.
!
               inc = 5
               ilast = igpct + ngpct
               jlast = ipoint + npoint
               IF ( iz(ilast)<0 ) inc = 0
               jmax = iz(jlast) + inc
!
!     TNROWS = TOTAL NO. OF ROWS OF THE MATRIX TO BE GENERATED
!
               tnrows = 6
               IF ( inpvt(1)<0 ) tnrows = 1
               IF ( 2*tnrows*jmax<left ) THEN
                  nrowsc = tnrows
               ELSE
!
!     THE WHOLE SUBMATRIX CANNOT FIT IN CORE
!
                  IF ( tnrows==1 ) CALL mesage(-8,0,name)
                  nrowsc = 3
                  plaary(39) = name(1)
                  SPAG_Loop_2_1: DO
                     plaary(40) = npvt
                     CALL mesage(30,85,plaary(39))
                     IF ( 2*nrowsc*jmax<left ) EXIT SPAG_Loop_2_1
                     nrowsc = nrowsc - 1
                     IF ( nrowsc==0 ) CALL mesage(-8,0,name)
                  ENDDO SPAG_Loop_2_1
               ENDIF
               frowic = 1
               lrowic = frowic + nrowsc - 1
!
!     ZERO OUT THE KGGL SUBMATRIX IN CORE.
!
               low = i6x6k + 1
               lim = i6x6k + jmax*nrowsc
               DO i = low , lim
                  dz(i) = 0.0D0
               ENDDO
!
!     INITIALIZE THE LINK VECTOR TO -1
!
               DO i = 1 , nlinks
                  link(i) = -1
               ENDDO
               lincor = 1
               file = ecpt
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
         CALL fread(ecpt,npvt,1,neor)
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_3: DO
!
!     READ THE NEXT ELEMENT TYPE INTO ITYPE, AND READ THE PRESCRIBED NO.
!     OF WORDS INTO THE XECPT ARRAY.
!
            CALL read(*420,*260,ecpt,itype,1,neor,iflag)
            idx = (itype-1)*incr
            nn = ne(idx+12)
            CALL fread(ecpt,xecpt,nn,neor)
            itemp = ne(idx+22)
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
            IF ( itype<=35 ) THEN
               IF ( itype==2 .OR. itype==32 .OR. itype==33 ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==3 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==4 .OR. itype==5 .OR. itype==7 .OR. itype==8 .OR. itype==11 .OR. itype==12 .OR. itype==13 .OR.            &
                  & itype==14 .OR. itype==15 .OR. itype==35 ) GOTO 240
               IF ( itype==6 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==9 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==16 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==17 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==18 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==19 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itype==20 .OR. itype==21 .OR. itype==22 .OR. itype==23 .OR. itype==24 .OR. itype==25 .OR. itype==26 .OR.        &
                  & itype==27 .OR. itype==28 .OR. itype==29 .OR. itype==30 .OR. itype==31 ) THEN
               ELSEIF ( itype==34 ) THEN
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
         matid = iecpt(4)
         ASSIGN 60 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 60      xecpt(18) = 0.0
         xecpt(19) = 0.0
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(20) = e
         IF ( phase1 ) THEN
            nwds = 20
            spag_nextblock_1 = 16
         ELSE
            xecpt(21) = 0.0
            nwds = 21
            spag_nextblock_1 = 21
         ENDIF
      CASE (6)
!
!     TUBE
!
         matid = iecpt(4)
         ASSIGN 80 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 80      xecpt(17) = 0.0
         xecpt(18) = 0.0
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(19) = e
         IF ( phase1 ) THEN
            nwds = 19
            spag_nextblock_1 = 16
         ELSE
            xecpt(20) = 0.0
            nwds = 20
            spag_nextblock_1 = 21
         ENDIF
      CASE (7)
!
!     TRIA1
!
         matid = iecpt(6)
         ASSIGN 100 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 100     DO i = 28 , 33
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(30) = e
         IF ( phase1 ) THEN
            nwds = 33
            spag_nextblock_1 = 16
         ELSE
            DO i = 34 , 38
               xecpt(i) = 0.0
            ENDDO
            nwds = 38
            spag_nextblock_1 = 21
         ENDIF
      CASE (8)
!
!     TRMEM
!
         matid = iecpt(6)
         ASSIGN 120 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 120     DO i = 22 , 27
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(24) = e
         nwds = 27
         IF ( phase1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (9)
!
!     QDMEM
!
         matid = iecpt(7)
         ASSIGN 140 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 140     DO i = 27 , 32
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(29) = e
         nwds = 32
         IF ( phase1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (10)
!
!     TRIA2
!
         matid = iecpt(6)
         ASSIGN 160 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 160     DO i = 22 , 27
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(24) = e
         IF ( phase1 ) THEN
            nwds = 27
            spag_nextblock_1 = 16
         ELSE
            DO i = 28 , 32
               xecpt(i) = 0.0
            ENDDO
            nwds = 32
            spag_nextblock_1 = 21
         ENDIF
      CASE (11)
!
!     QUAD2
!
         matid = iecpt(7)
         ASSIGN 180 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 180     DO i = 27 , 32
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(29) = e
         IF ( phase1 ) THEN
            nwds = 32
            spag_nextblock_1 = 16
         ELSE
            DO i = 33 , 37
               xecpt(i) = 0.0
            ENDDO
            nwds = 37
            spag_nextblock_1 = 21
         ENDIF
      CASE (12)
!
!     QUAD1
!
         matid = iecpt(7)
         ASSIGN 200 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 200     DO i = 33 , 38
            xecpt(i) = 0.0
         ENDDO
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(35) = e
         IF ( phase1 ) THEN
            nwds = 38
            spag_nextblock_1 = 16
         ELSE
            DO i = 39 , 43
               xecpt(i) = 0.0
            ENDDO
            nwds = 43
            spag_nextblock_1 = 21
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
         matid = iecpt(16)
         ASSIGN 220 TO yessd
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 220     xecpt(43) = 0.0
         xecpt(44) = 0.0
         inflag = 1
         CALL mat(iecpt(1))
         xecpt(45) = e
         IF ( phase1 ) THEN
            nwds = 45
            spag_nextblock_1 = 16
         ELSE
            DO i = 46 , 50
               xecpt(i) = 0.0
            ENDDO
            nwds = 50
            spag_nextblock_1 = 21
         ENDIF
      CASE (15)
!
!     TEST TO SEE IF ELEMENT IS STRESS DEPENDENT.
!
         inflag = 5
         CALL mat(iecpt(1))
         IF ( indstr<=0 ) THEN
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
            CALL write(ecptnl,npvt,1,neor)
            kickof = 1
         ENDIF
         CALL write(ecptnl,itype,1,neor)
         CALL write(ecptnl,xecpt,nwds,neor)
         nnlel = nnlel + 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT,
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
 240     kgglpg = 1
         IF ( ifirst==1 ) THEN
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
            IF ( link(itemp)==1 ) THEN
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
            IF ( link(itemp)/=1 ) link(itemp) = 0
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
         IF ( itype<=35 ) THEN
            IF ( itype==2 .OR. itype==32 .OR. itype==33 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( itype==3 ) THEN
               CALL ktube
            ELSEIF ( itype==4 ) THEN
               CALL kpanel(4)
            ELSEIF ( itype==5 ) THEN
               CALL kpanel(5)
            ELSEIF ( itype==6 ) THEN
               CALL ktriqd(1)
            ELSEIF ( itype==7 ) THEN
               CALL ktrbsc(0)
            ELSEIF ( itype==8 ) THEN
               CALL ktrplt
            ELSEIF ( itype==9 ) THEN
               CALL ktrmem(0)
            ELSEIF ( itype==11 ) THEN
               CALL kelas(1)
            ELSEIF ( itype==12 ) THEN
               CALL kelas(2)
            ELSEIF ( itype==13 ) THEN
               CALL kelas(3)
            ELSEIF ( itype==14 ) THEN
               CALL kelas(4)
            ELSEIF ( itype==15 ) THEN
               CALL kqdplt
            ELSEIF ( itype==16 ) THEN
               CALL kqdmem
            ELSEIF ( itype==17 ) THEN
               CALL ktriqd(2)
            ELSEIF ( itype==18 ) THEN
               CALL ktriqd(4)
            ELSEIF ( itype==19 ) THEN
               CALL ktriqd(3)
            ELSEIF ( itype==20 .OR. itype==21 .OR. itype==22 .OR. itype==23 .OR. itype==24 .OR. itype==25 .OR. itype==26 .OR.       &
                   & itype==27 .OR. itype==28 .OR. itype==29 .OR. itype==30 .OR. itype==31 ) THEN
            ELSEIF ( itype==34 ) THEN
               CALL kbar
            ELSEIF ( itype==35 ) THEN
               IF ( nbpw<=32 ) CALL kconed
               IF ( nbpw>32 ) CALL kcones
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
               CALL write(ecptnl,-npvt,1,eor)
            ELSE
               CALL write(ecptnl,0,0,eor)
            ENDIF
         ENDIF
         ipass = 2
         link(lincor) = 1
         DO i = 1 , nlinks
            IF ( link(i)==0 ) THEN
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
            ibeg = i6x6k + i1*jmax
            CALL bldpk(2,ipr,kggl,0,0)
            DO
               i2 = i2 + 1
               IF ( i2>ngpct ) THEN
                  CALL bldpkn(kggl,0,mcbkgg)
                  i1 = i1 + 1
                  IF ( i1<nrowsc ) CYCLE SPAG_Loop_1_4
!
!     IF LROWIC = TNROWS, PROCESSING OF THE CURRENT ECPT RECORD HAS BEEN
!     COMPLETED.
!
                  IF ( lrowic==tnrows ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL bckrec(ecpt)
                  frowic = frowic + nrowsc
                  lrowic = lrowic + nrowsc
                  ipass = 2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  jj = igpct + i2
                  index = iabs(iz(jj)) - 1
                  lim = 6
                  IF ( iz(jj)<0 ) lim = 1
                  jjj = ipoint + i2
                  kkk = ibeg + iz(jjj) - 1
                  i3 = 0
                  SPAG_Loop_3_5: DO
                     i3 = i3 + 1
                     IF ( i3>lim ) EXIT SPAG_Loop_3_5
                     index = index + 1
                     kkk = kkk + 1
                     dpword = dz(kkk)
                     IF ( dpword/=0.0D0 ) CALL zblpki
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
         CALL bckrec(ecpt)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     NO ELEMENTS ARE CONNECTED TO THE PIVOT POINT.  OUTPUT ZERO
!     COLUMN(S).  ALSO, WRITE NEGATIVE PIVOT POINT ON ECPTNL.
!
 280     lim = 6
         IF ( inpvt(1)<0 ) lim = 1
         DO i = 1 , lim
            CALL bldpk(2,ipr,kggl,0,0)
            CALL bldpkn(kggl,0,mcbkgg)
         ENDDO
         CALL skprec(ecpt,1)
         CALL write(ecptnl,-iabs(inpvt(1)),1,eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ECPT PROCESSING HAS BEEN COMPLETED SINCE AN EOF HAS BEEN READ ON
!     GPCT.
!
 300     CALL close(gpct,clsrw)
         CALL close(ecpt,clsrw)
         CALL close(kggl,clsrw)
         CALL close(ecptnl,clsrw)
         IF ( kickof==-1 ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mcbkgg(6)/=0 ) THEN
            mcbkgg(3) = mcbkgg(2)
         ELSE
            DO i = 2 , 7
               mcbkgg(i) = 0
            ENDDO
         ENDIF
         CALL wrttrl(mcbkgg)
         CALL wrttrl(trail)
!
!     BEGIN EST PROCESSING
!
         left = bufr4 - 1
         icc = ncstm + mused
         all = .FALSE.
         phase1 = .FALSE.
!
!     READ THE FIRST RECORD OF CASECC INTO CORE.
!
         file = casecc
         CALL gopen(casecc,z(bufr1),0)
         CALL read(*420,*320,casecc,iz(icc+1),left,eor,ncc)
         CALL mesage(-8,0,name)
 320     iplset = icc + jplset
         plaset = iz(iplset)
         istset = icc + jstset
         IF ( iz(istset)<0 ) THEN
            all = .TRUE.
         ELSEIF ( iz(istset)==0 ) THEN
            nonlst = -1
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
         CALL close(casecc,clsrw)
         IF ( plaset/=-1 ) THEN
!
!     SEARCH THE MPT FOR THE PLA SET
!
            file = mpt
            CALL preloc(*400,z(bufr1-3),mpt)
            CALL locate(*460,z(bufr1-3),planos,iflag)
         ELSE
            jj = 1
            plfact(1) = 1.0
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_7: DO
!
!     READ A PLA SET NO.
!
            CALL read(*460,*460,mpt,setno,1,neor,iflag)
            jj = 0
            SPAG_Loop_2_8: DO
               CALL read(*460,*460,mpt,nn,1,neor,iflag)
               IF ( nn==-1 ) THEN
                  IF ( setno/=plaset ) CYCLE SPAG_Loop_1_7
                  nplalp = jj
                  plfact(2) = 0.0
                  CALL close(mpt,clsrw)
                  EXIT SPAG_Loop_2_8
               ELSE
                  jj = jj + 1
                  IF ( jj==1 ) plfact(1) = fnn
               ENDIF
            ENDDO SPAG_Loop_2_8
            EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
         spag_nextblock_1 = 18
      CASE (18)
!
!     PROCESS THE EST
!
         estltr(1) = estl
         estnlt(1) = estnl
         DO i = 2 , 7
            estltr(i) = 0
            estnlt(i) = 0
         ENDDO
         ASSIGN 340 TO nosd
         CALL gopen(est,z(bufr1),0)
         CALL gopen(estl,z(bufr2),1)
         CALL gopen(estnl,z(bufr3),1)
         file = est
         spag_nextblock_1 = 19
      CASE (19)
         SPAG_Loop_1_9: DO
!
!     READ THE ELEMENT TYPE.  IF THE ELEMENT TYPE IS ADMISSIBLE TO
!     PIECEWISE LINEAR ANALYSIS, WRITE IT TWICE.  OTHERWISE GO TO NEXT
!     RECORD.
!
            CALL read(*380,*440,est,itype,1,neor,iflag)
            IF ( plaary(itype)==1 ) THEN
               CALL write(estl,itype,1,neor)
               CALL write(estnl,itype,1,neor)
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
         idx = (itype-1)*incr
         nwds = ne(idx+12)
         CALL read(*420,*360,est,xecpt,nwds,neor,iflag)
         IF ( plaary(itype)==0 ) GOTO 340
         IF ( itype>38 ) GOTO 340
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
         IF ( itype==1 .OR. itype==10 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==2 .OR. itype==4 .OR. itype==5 .OR. itype==7 .OR. itype==8 .OR. itype==11 .OR. itype==12 .OR. itype==13 .OR.    &
            & itype==14 .OR. itype==15 .OR. itype==20 .OR. itype==21 .OR. itype==22 .OR. itype==23 .OR. itype==24 .OR.              &
            & itype==25 .OR. itype==26 .OR. itype==27 .OR. itype==28 .OR. itype==29 .OR. itype==30 .OR. itype==31 .OR.              &
            & itype==32 .OR. itype==33 .OR. itype==35 .OR. itype==36 .OR. itype==37 .OR. itype==38 ) GOTO 340
         IF ( itype==3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==6 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==9 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==16 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==17 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==18 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==19 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==34 ) THEN
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
            IF ( nonlst/=-1 ) THEN
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
                     llllll(1) = iz(istset)
                     llllll(2) = iz(i)
                     CALL mesage(30,92,llllll)
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
         outfil = estnl
         nnlel = nnlel + 1
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 340     outfil = estl
         nlel = nlel + 1
         spag_nextblock_1 = 23
      CASE (23)
         CALL write(outfil,xecpt,nwds,neor)
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 360     CALL write(estl,0,0,eor)
         CALL write(estnl,0,0,eor)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
!
!     WRAP UP ROUTINE
!
 380     CALL close(est,clsrw)
         CALL close(estl,clsrw)
         CALL close(estnl,clsrw)
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
         CALL mesage(-30,87,itype)
!
!     UNABLE TO FIND PLFACT CARD IN THE MPT WHICH WAS CHOSEN BY THE USER
!     IN CASECC.
!
 460     trail(1) = hmpt
         trail(2) = name(1)
         CALL mesage(-32,plaset,trail)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla1
