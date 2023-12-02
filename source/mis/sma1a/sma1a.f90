!*==sma1a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sma1a
   USE c_blank
   USE c_gpta1
   USE c_sma1bk
   USE c_sma1cl
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
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , i1 , i2 , i3 , ibeg , idx , ifile , ifirst , iflag , ilast , imcb , inc , iparm , ipr , iprec , iretrn , itemp ,  &
            & itype , itypx , ixx , j , jj , jjj , jlast , kkk , left , lim , lincor , low , sysprt
   INTEGER , DIMENSION(2) :: inpvt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: nogo , noheat
   EXTERNAL bckrec , bldpk , bldpkn , detck , fread , kdum1 , kdum2 , kdum3 , kdum4 , kdum5 , kdum6 , kdum7 , kdum8 , kdum9 ,       &
          & mesage , page2 , read , skprec , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE FORMERLY GENERATED THE KGG AND K4GG MATRICES FOR
!     THE SMA1 MODULE.  THESE OPERATIONS ARE NOW PERFORMED IN THE EMG
!     AND EMA MODULES AND SMA1A IS RETAINED IN SKELETAL FORM TO PROVIDE
!     A VEHICLE FOR USER-PROVIDED ELEMENTS.
!
   !>>>>EQUIVALENCE (Ksystm(2),Sysprt) , (Ksystm(3),Nogo) , (Ksystm(55),Iprec) , (Z(1),Iz(1),Dz(1))
   DATA name/4HSMA1 , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FLAG FOR ERROR CHECK IF A NON-HEAT ELEMENT IS REFERENCED
!     IN A -HEAT- FORMULATION.
!
         noheat = .FALSE.
         ipr = iprec
!
!     READ THE FIRST TWO WORDS OF NEXT GPCT RECORD INTO INPVT(1).
!     INPVT(1) IS THE PIVOT POINT.  INPVT(1) .GT. 0 IMPLIES THE PIVOT
!     POINT IS A GRID POINT.  INPVT(1) .LT. 0 IMPLIES THE PIVOT POINT IS
!     A SCALAR POINT.  INPVT(2) IS THE NUMBER OF WORDS IN THE REMAINDER
!     OF THIS RECORD OF THE GPCT.
!
         IF ( nogo ) WRITE (sysprt,99001) swm
99001    FORMAT (A27,' 2055, NOGO FLAG IS ON AT ENTRY TO SMA1A AND IS ','BEING TURNED OFF.')
         nogo = .FALSE.
         spag_nextblock_1 = 2
      CASE (2)
         idetck = 0
         CALL read(*100,*80,ifgpct,inpvt(1),2,neor,iflag)
         ngpct = inpvt(2)
         CALL read(*100,*120,ifgpct,iz(igpct+1),ngpct,eor,iflag)
!
!     FROWIC IS THE FIRST ROW IN CORE. (1 .LE. FROWIC .LE. 6)
!
         frowic = 1
!
!     DECREMENT THE AMOUNT OF CORE REMAINING.
!
         left = lleft - 2*ngpct
         IF ( left<=0 ) THEN
            iparm = -8
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ipoint = igpct + ngpct
            npoint = ngpct
            i6x6k = ipoint + npoint
            i6x6k = (i6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE, WHICH WILL ENABLE SUBROUTINE SMA1B
!     TO ADD THE ELEMENT STRUCTURAL AND/OR DAMPING MATRICES TO KGG AND
!     K4GG.
!
            iz(ipoint+1) = 1
            i1 = 1
            i = igpct
            j = ipoint + 1
            SPAG_Loop_1_1: DO
               i1 = i1 + 1
               IF ( i1>ngpct ) THEN
!
!     JMAX = THE NUMBER OF COLUMNS OF KGG THAT WILL BE GENERATED WITH
!     THE CURRENT GRID POINT.
!
                  inc = 5
                  ilast = igpct + ngpct
                  jlast = ipoint + npoint
                  IF ( iz(ilast)<0 ) inc = 0
                  jmax = iz(jlast) + inc
!
!     TNROWS = THE TOTAL NUMBER OF ROWS OF THE MATRIX TO BE GENERATED
!              FOR THE CURRENT PIVOT POINT.
!     TNROWS = 6 IF THE CURRENT PIVOT POINT IS A GRID POINT.
!     TNROWS = 1 IF THE CURRENT PIVOT POINT IS A SCALAR POINT.
!
                  tnrows = 6
                  IF ( inpvt(1)<0 ) tnrows = 1
!
!     IF 2*TNROWS*JMAX .LT. LEFT THERE ARE NO SPILL LOGIC PROBLEMS FOR
!     THE KGG SINCE THE WHOLE DOUBLE PRECISION SUBMATRIX OF ORDER TNROWS
!     X JMAX CAN FIT IN CORE.
!
                  itemp = tnrows*jmax
                  IF ( 2*itemp<left ) THEN
                     nrowsc = tnrows
                  ELSE
                     name(2) = inpvt(1)
                     CALL mesage(30,85,name)
!
!     THE WHOLE MATRIX CANNOT FIT IN CORE, DETERMINE HOW MANY ROWS CAN
!     FIT. IF TNROWS = 1, WE CAN DO NOTHING FURTHER.
!
                     IF ( tnrows==1 ) THEN
                        iparm = -8
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        nrowsc = 3
                        DO WHILE ( 2*nrowsc*jmax>=left )
                           nrowsc = nrowsc - 1
                           IF ( nrowsc==0 ) CALL mesage(-8,0,name)
                        ENDDO
                     ENDIF
                  ENDIF
                  frowic = 1
!
!     LROWIC IS THE LAST ROW IN CORE. (1 .LE. LROWIC .LE. 6)
!
                  lrowic = frowic + nrowsc - 1
                  EXIT SPAG_Loop_1_1
               ELSE
                  i = i + 1
                  j = j + 1
                  inc = 6
                  IF ( iz(i)<0 ) inc = 1
                  iz(j) = iz(j-1) + inc
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     ZERO OUT THE KGG SUBMATRIX IN CORE
!
         low = i6x6k + 1
         lim = i6x6k + jmax*nrowsc
         DO i = low , lim
            dz(i) = 0.0D0
         ENDDO
!
!     CHECK TO SEE IF THE K4GG MATRIX IS DESIRED.
!
         IF ( iopt4/=0 ) THEN
!
!     SINCE THE K4GG MATRIX IS TO BE COMPUTED, DETERMINE IF IT TOO CAN
!     FIT INTO CORE
!
            IF ( nrowsc/=tnrows ) THEN
!
!     OPEN A SCRATCH FILE FOR K4GG.
!
               CALL mesage(-8,0,name)
            ELSEIF ( 4*tnrows*jmax>=left ) THEN
               CALL mesage(-8,0,name)
            ENDIF
!
!     THIS CODE TO BE FILLED IN LATER
!     ===============================
!
            i6x64 = i6x6k + jmax*tnrows
            low = i6x64 + 1
            lim = i6x64 + jmax*tnrows
            DO i = low , lim
               dz(i) = 0.0D0
            ENDDO
         ENDIF
!
!     INITIALIZE THE LINK VECTOR TO -1.
!
         DO i = 1 , nlinks
            link(i) = -1
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     TURN FIRST PASS INDICATOR ON.
!
         ifirst = 1
!
!     READ THE 1ST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT.
!
         CALL fread(ifecpt,npvt,1,0)
         DO
!
!     READ THE NEXT ELEMENT TYPE INTO THE CELL ITYPE.
!
            CALL read(*140,*20,ifecpt,itype,1,neor,iflag)
            IF ( itype>=53 .OR. itype<=61 ) THEN
!
!     READ THE ECPT ENTRY FOR THE CURRENT TYPE INTO THE ECPT ARRAY. THE
!     NUMBER OF WORDS TO BE READ WILL BE NWORDS(ITYPE).
!
               idx = (itype-1)*incr
               CALL fread(ifecpt,ecpt,ne(idx+12),0)
               itemp = ne(idx+22)
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
               IF ( ifirst==1 ) THEN
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
                  IF ( link(itemp)==1 ) CYCLE
!
!     SET THE CURRENT LINK IN CORE = ITEMP AND IFIRST = 0
!
                  lincor = itemp
                  ifirst = 0
                  itypx = itype - 52
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
                  CYCLE
               ENDIF
!
!     CALL THE PROPER ELEMENT ROUTINE.
!
!                                  CDUM1   CDUM2   CDUM3   CDUM4
!                                    53      54      55      56
!          CDUM5   CDUM6   CDUM7   CDUM8   CDUM9
!            57      58      59      60      61
               IF ( itypx==2 ) THEN
                  CALL kdum2
               ELSEIF ( itypx==3 ) THEN
                  CALL kdum3
               ELSEIF ( itypx==4 ) THEN
                  CALL kdum4
               ELSEIF ( itypx==5 ) THEN
                  CALL kdum5
               ELSEIF ( itypx==6 ) THEN
                  CALL kdum6
               ELSEIF ( itypx==7 ) THEN
                  CALL kdum7
               ELSEIF ( itypx==8 ) THEN
                  CALL kdum8
               ELSEIF ( itypx==9 ) THEN
                  CALL kdum9
               ELSE
!
!
                  CALL kdum1
               ENDIF
            ELSE
               CALL page2(-3)
               WRITE (sysprt,99002) ufm , itype
99002          FORMAT (A23,' 2201, ELEMENT TYPE',I4,' NO LONGER SUPPORTED BY ','SMA1 MODULE.',/5X,                                  &
                      &'USE EMG AND EMA MODULES FOR ELEMENT MATRIX GENERATION')
               nogo = .TRUE.
               GOTO 100
            ENDIF
         ENDDO
!
!     AT STATEMENT NO. 500 WE HAVE HIT AN EOR ON THE ECPT FILE.  SEARCH
!     THE LINK VECTOR TO DETERMINE IF THERE ARE LINKS TO BE PROCESSED.
!
 20      link(lincor) = 1
         DO i = 1 , nlinks
            IF ( link(i)==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!    CHECK NOGOO FLAG. IF 1 SKIP BKDPK AND PROCESS ANOTHER GRID POINT
!    FROM GPCT
!
         IF ( nogoo==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IF NO GENERAL ELEMENTS EXIST, CHECK FOR GRID POINT SINGULARITIES.
!
!WKBR IF (DODET) CALL DETCK (0)
         IF ( dodet ) CALL detck(0,npvt,ifgpst)
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE UNTO THE KGG FILE.
!
         ASSIGN 40 TO iretrn
         ifile = ifkgg
         imcb = 1
         i1 = 0
         spag_nextblock_1 = 6
      CASE (5)
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
         CALL bckrec(ifecpt)
         spag_nextblock_1 = 4
      CASE (6)
         SPAG_Loop_1_2: DO
            i2 = 0
            ibeg = i6x6k + i1*jmax
            CALL bldpk(2,ipr,ifile,0,0)
            DO
               i2 = i2 + 1
               IF ( i2>ngpct ) THEN
                  CALL bldpkn(ifile,0,mcbkgg(imcb))
                  i1 = i1 + 1
                  IF ( i1<nrowsc ) CYCLE SPAG_Loop_1_2
                  GOTO iretrn
               ELSE
                  jj = igpct + i2
                  index = iabs(iz(jj)) - 1
                  lim = 6
                  IF ( iz(jj)<0 ) lim = 1
                  jjj = ipoint + i2
                  kkk = ibeg + iz(jjj) - 1
                  i3 = 0
                  SPAG_Loop_3_3: DO
                     i3 = i3 + 1
                     IF ( i3>lim ) EXIT SPAG_Loop_3_3
                     index = index + 1
                     kkk = kkk + 1
                     dpword = dz(kkk)
                     IF ( dpword/=0.0D0 ) CALL zblpki
                  ENDDO SPAG_Loop_3_3
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
!     IF THE K4GG IS CALLED FOR, BLDPK IT.
!
 40      IF ( iopt4/=0 ) THEN
            IF ( iopt4/=-1 ) THEN
!
!     THE K4GG MATRIX IS IN CORE.
!
               ASSIGN 60 TO iretrn
               i6x6k = i6x64
               ifile = if4gg
               imcb = 8
               i1 = 0
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     HERE WE NEED LOGIC TO READ K4GG FROM A SCRATCH FILE AND INSERT.
!
!
!     TEST TO SEE IF THE LAST ROW IN CORE, LROWIC, = THE TOTAL NO. OF
!     ROWS TO BE COMPUTED, TNROWS.  IF IT IS, WE ARE DONE.  IF NOT, THE
!     ECPT MUST BE BACKSPACED.
!
 60      IF ( lrowic==tnrows ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bckrec(ifecpt)
         frowic = frowic + nrowsc
         lrowic = lrowic + nrowsc
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK NOGOO = 1 SKIP BLDPK AND PROCESS ANOTHER RECORD
!
 80      IF ( nogoo/=1 ) THEN
!
!     HERE WE HAVE A PIVOT POINT WITH NO ELEMENTS CONNECTED, SO THAT
!     NULL COLUMNS MUST BE OUTPUT ON THE KGG AND K4GG FILES.  IF DODET
!     IS TRUE, CALL THE DETERMINANT CHECK ROUTINE TO WRITE SINGULARITY
!     INFORMATION.
!
            npvt = iabs(inpvt(1))
            IF ( inpvt(1)>0 ) THEN
               lim = 6
               ixx = 1
            ELSE
               lim = 1
               ixx = -1
            ENDIF
!WKBR  706 IF (DODET) CALL DETCK (IXX)
            IF ( dodet ) CALL detck(ixx,npvt,ifgpst)
            DO i = 1 , lim
               CALL bldpk(2,ipr,ifkgg,0,0)
               CALL bldpkn(ifkgg,0,mcbkgg)
               IF ( iopt4==1 ) THEN
                  CALL bldpk(2,ipr,if4gg,0,0)
                  CALL bldpkn(if4gg,0,mcb4gg)
               ENDIF
            ENDDO
            CALL skprec(ifecpt,1)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     RETURN SINCE AN EOF HAS BEEN HIT ON THE GPCT FILE
!
 100     IF ( .NOT.nogo .AND. nogoo==0 ) RETURN
         iparm = -61
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR RETURNS
!
 120     ifile = ifgpct
         iparm = -3
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     ifile = ifecpt
         iparm = -2
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(iparm,ifile,name)
         CALL mesage(-30,87,itype)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sma1a
