!*==sma1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sma1a
USE C_BLANK
USE C_GPTA1
USE C_SMA1BK
USE C_SMA1CL
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
         IF ( nogo ) WRITE (sysprt,99001) Swm
99001    FORMAT (A27,' 2055, NOGO FLAG IS ON AT ENTRY TO SMA1A AND IS ','BEING TURNED OFF.')
         nogo = .FALSE.
         spag_nextblock_1 = 2
      CASE (2)
         Idetck = 0
         CALL read(*100,*80,Ifgpct,inpvt(1),2,Neor,iflag)
         Ngpct = inpvt(2)
         CALL read(*100,*120,Ifgpct,iz(Igpct+1),Ngpct,Eor,iflag)
!
!     FROWIC IS THE FIRST ROW IN CORE. (1 .LE. FROWIC .LE. 6)
!
         Frowic = 1
!
!     DECREMENT THE AMOUNT OF CORE REMAINING.
!
         left = Lleft - 2*Ngpct
         IF ( left<=0 ) THEN
            iparm = -8
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Ipoint = Igpct + Ngpct
            Npoint = Ngpct
            I6x6k = Ipoint + Npoint
            I6x6k = (I6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE, WHICH WILL ENABLE SUBROUTINE SMA1B
!     TO ADD THE ELEMENT STRUCTURAL AND/OR DAMPING MATRICES TO KGG AND
!     K4GG.
!
            iz(Ipoint+1) = 1
            i1 = 1
            i = Igpct
            j = Ipoint + 1
            SPAG_Loop_1_1: DO
               i1 = i1 + 1
               IF ( i1>Ngpct ) THEN
!
!     JMAX = THE NUMBER OF COLUMNS OF KGG THAT WILL BE GENERATED WITH
!     THE CURRENT GRID POINT.
!
                  inc = 5
                  ilast = Igpct + Ngpct
                  jlast = Ipoint + Npoint
                  IF ( iz(ilast)<0 ) inc = 0
                  Jmax = iz(jlast) + inc
!
!     TNROWS = THE TOTAL NUMBER OF ROWS OF THE MATRIX TO BE GENERATED
!              FOR THE CURRENT PIVOT POINT.
!     TNROWS = 6 IF THE CURRENT PIVOT POINT IS A GRID POINT.
!     TNROWS = 1 IF THE CURRENT PIVOT POINT IS A SCALAR POINT.
!
                  Tnrows = 6
                  IF ( inpvt(1)<0 ) Tnrows = 1
!
!     IF 2*TNROWS*JMAX .LT. LEFT THERE ARE NO SPILL LOGIC PROBLEMS FOR
!     THE KGG SINCE THE WHOLE DOUBLE PRECISION SUBMATRIX OF ORDER TNROWS
!     X JMAX CAN FIT IN CORE.
!
                  itemp = Tnrows*Jmax
                  IF ( 2*itemp<left ) THEN
                     Nrowsc = Tnrows
                  ELSE
                     name(2) = inpvt(1)
                     CALL mesage(30,85,name)
!
!     THE WHOLE MATRIX CANNOT FIT IN CORE, DETERMINE HOW MANY ROWS CAN
!     FIT. IF TNROWS = 1, WE CAN DO NOTHING FURTHER.
!
                     IF ( Tnrows==1 ) THEN
                        iparm = -8
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        Nrowsc = 3
                        DO WHILE ( 2*Nrowsc*Jmax>=left )
                           Nrowsc = Nrowsc - 1
                           IF ( Nrowsc==0 ) CALL mesage(-8,0,name)
                        ENDDO
                     ENDIF
                  ENDIF
                  Frowic = 1
!
!     LROWIC IS THE LAST ROW IN CORE. (1 .LE. LROWIC .LE. 6)
!
                  Lrowic = Frowic + Nrowsc - 1
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
         low = I6x6k + 1
         lim = I6x6k + Jmax*Nrowsc
         DO i = low , lim
            dz(i) = 0.0D0
         ENDDO
!
!     CHECK TO SEE IF THE K4GG MATRIX IS DESIRED.
!
         IF ( Iopt4/=0 ) THEN
!
!     SINCE THE K4GG MATRIX IS TO BE COMPUTED, DETERMINE IF IT TOO CAN
!     FIT INTO CORE
!
            IF ( Nrowsc/=Tnrows ) THEN
!
!     OPEN A SCRATCH FILE FOR K4GG.
!
               CALL mesage(-8,0,name)
            ELSEIF ( 4*Tnrows*Jmax>=left ) THEN
               CALL mesage(-8,0,name)
            ENDIF
!
!     THIS CODE TO BE FILLED IN LATER
!     ===============================
!
            I6x64 = I6x6k + Jmax*Tnrows
            low = I6x64 + 1
            lim = I6x64 + Jmax*Tnrows
            DO i = low , lim
               dz(i) = 0.0D0
            ENDDO
         ENDIF
!
!     INITIALIZE THE LINK VECTOR TO -1.
!
         DO i = 1 , Nlinks
            Link(i) = -1
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
         CALL fread(Ifecpt,Npvt,1,0)
         DO
!
!     READ THE NEXT ELEMENT TYPE INTO THE CELL ITYPE.
!
            CALL read(*140,*20,Ifecpt,itype,1,Neor,iflag)
            IF ( itype>=53 .OR. itype<=61 ) THEN
!
!     READ THE ECPT ENTRY FOR THE CURRENT TYPE INTO THE ECPT ARRAY. THE
!     NUMBER OF WORDS TO BE READ WILL BE NWORDS(ITYPE).
!
               idx = (itype-1)*Incr
               CALL fread(Ifecpt,Ecpt,Ne(idx+12),0)
               itemp = Ne(idx+22)
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
                  IF ( Link(itemp)==1 ) CYCLE
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
                  IF ( Link(itemp)/=1 ) Link(itemp) = 0
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
               WRITE (sysprt,99002) Ufm , itype
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
 20      Link(lincor) = 1
         DO i = 1 , Nlinks
            IF ( Link(i)==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!    CHECK NOGOO FLAG. IF 1 SKIP BKDPK AND PROCESS ANOTHER GRID POINT
!    FROM GPCT
!
         IF ( Nogoo==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IF NO GENERAL ELEMENTS EXIST, CHECK FOR GRID POINT SINGULARITIES.
!
!WKBR IF (DODET) CALL DETCK (0)
         IF ( Dodet ) CALL detck(0,Npvt,Ifgpst)
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE UNTO THE KGG FILE.
!
         ASSIGN 40 TO iretrn
         ifile = Ifkgg
         imcb = 1
         i1 = 0
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
         CALL bckrec(Ifecpt)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         SPAG_Loop_1_2: DO
            i2 = 0
            ibeg = I6x6k + i1*Jmax
            CALL bldpk(2,ipr,ifile,0,0)
            DO
               i2 = i2 + 1
               IF ( i2>Ngpct ) THEN
                  CALL bldpkn(ifile,0,Mcbkgg(imcb))
                  i1 = i1 + 1
                  IF ( i1<Nrowsc ) CYCLE SPAG_Loop_1_2
                  GOTO iretrn
               ELSE
                  jj = Igpct + i2
                  Index = iabs(iz(jj)) - 1
                  lim = 6
                  IF ( iz(jj)<0 ) lim = 1
                  jjj = Ipoint + i2
                  kkk = ibeg + iz(jjj) - 1
                  i3 = 0
                  SPAG_Loop_3_3: DO
                     i3 = i3 + 1
                     IF ( i3>lim ) EXIT SPAG_Loop_3_3
                     Index = Index + 1
                     kkk = kkk + 1
                     Dpword = dz(kkk)
                     IF ( Dpword/=0.0D0 ) CALL zblpki
                  ENDDO SPAG_Loop_3_3
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
!     IF THE K4GG IS CALLED FOR, BLDPK IT.
!
 40      IF ( Iopt4/=0 ) THEN
            IF ( Iopt4/=-1 ) THEN
!
!     THE K4GG MATRIX IS IN CORE.
!
               ASSIGN 60 TO iretrn
               I6x6k = I6x64
               ifile = If4gg
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
 60      IF ( Lrowic==Tnrows ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bckrec(Ifecpt)
         Frowic = Frowic + Nrowsc
         Lrowic = Lrowic + Nrowsc
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK NOGOO = 1 SKIP BLDPK AND PROCESS ANOTHER RECORD
!
 80      IF ( Nogoo/=1 ) THEN
!
!     HERE WE HAVE A PIVOT POINT WITH NO ELEMENTS CONNECTED, SO THAT
!     NULL COLUMNS MUST BE OUTPUT ON THE KGG AND K4GG FILES.  IF DODET
!     IS TRUE, CALL THE DETERMINANT CHECK ROUTINE TO WRITE SINGULARITY
!     INFORMATION.
!
            Npvt = iabs(inpvt(1))
            IF ( inpvt(1)>0 ) THEN
               lim = 6
               ixx = 1
            ELSE
               lim = 1
               ixx = -1
            ENDIF
!WKBR  706 IF (DODET) CALL DETCK (IXX)
            IF ( Dodet ) CALL detck(ixx,Npvt,Ifgpst)
            DO i = 1 , lim
               CALL bldpk(2,ipr,Ifkgg,0,0)
               CALL bldpkn(Ifkgg,0,Mcbkgg)
               IF ( Iopt4==1 ) THEN
                  CALL bldpk(2,ipr,If4gg,0,0)
                  CALL bldpkn(If4gg,0,Mcb4gg)
               ENDIF
            ENDDO
            CALL skprec(Ifecpt,1)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     RETURN SINCE AN EOF HAS BEEN HIT ON THE GPCT FILE
!
 100     IF ( .NOT.nogo .AND. Nogoo==0 ) RETURN
         iparm = -61
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR RETURNS
!
 120     ifile = Ifgpct
         iparm = -3
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     ifile = Ifecpt
         iparm = -2
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(iparm,ifile,name)
         CALL mesage(-30,87,itype)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sma1a
