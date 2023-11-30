
SUBROUTINE sma1a
   IMPLICIT NONE
   INTEGER Clsnrw , Clsrw , Eor , Frowic , I6x64 , I6x6k , Icstm , Idetck , Idum1 , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei ,       &
         & Ifgpct , Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Incr , Index , Inrw ,       &
         & Iopt4 , Ipoint , Iprec , Iz(1) , Jmax , K4ggsw , Ksystm(65) , Last , Link(10) , Lleft , Lrowic , Mcb4gg(7) , Mcbkgg(7) , &
         & N6x64 , N6x6k , Ncstm , Ne(1) , Nelems , Neor , Ngpct , Nlinks , Nogenl , Nogoo , Nok4gg , Npoint , Npvt , Nrowsc ,      &
         & Option(2) , Outrw , Sysprt , Tnrows
   LOGICAL Dodet , Heat , Nogo
   DOUBLE PRECISION Dpword , Dz(1)
   REAL Dum(2) , Ecpt(200) , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Nogenl , Nok4gg , Option
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Lleft , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet , Nogoo
   COMMON /sma1et/ Ecpt
   COMMON /sma1ht/ Heat
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zblpkx/ Dpword , Dum , Index
   COMMON /zzzzzz/ Z
   INTEGER i , i1 , i2 , i3 , ibeg , idx , ifile , ifirst , iflag , ilast , imcb , inc , inpvt(2) , iparm , ipr , iretrn , itemp ,  &
         & itype , itypx , ixx , j , jj , jjj , jlast , kkk , left , lim , lincor , low , name(2)
   LOGICAL noheat
!
!     THIS SUBROUTINE FORMERLY GENERATED THE KGG AND K4GG MATRICES FOR
!     THE SMA1 MODULE.  THESE OPERATIONS ARE NOW PERFORMED IN THE EMG
!     AND EMA MODULES AND SMA1A IS RETAINED IN SKELETAL FORM TO PROVIDE
!     A VEHICLE FOR USER-PROVIDED ELEMENTS.
!
   EQUIVALENCE (Ksystm(2),Sysprt) , (Ksystm(3),Nogo) , (Ksystm(55),Iprec) , (Z(1),Iz(1),Dz(1))
   DATA name/4HSMA1 , 4HA   /
!
!     FLAG FOR ERROR CHECK IF A NON-HEAT ELEMENT IS REFERENCED
!     IN A -HEAT- FORMULATION.
!
   noheat = .FALSE.
   ipr = Iprec
!
!     READ THE FIRST TWO WORDS OF NEXT GPCT RECORD INTO INPVT(1).
!     INPVT(1) IS THE PIVOT POINT.  INPVT(1) .GT. 0 IMPLIES THE PIVOT
!     POINT IS A GRID POINT.  INPVT(1) .LT. 0 IMPLIES THE PIVOT POINT IS
!     A SCALAR POINT.  INPVT(2) IS THE NUMBER OF WORDS IN THE REMAINDER
!     OF THIS RECORD OF THE GPCT.
!
   IF ( Nogo ) WRITE (Sysprt,99001) Swm
99001 FORMAT (A27,' 2055, NOGO FLAG IS ON AT ENTRY TO SMA1A AND IS ','BEING TURNED OFF.')
   Nogo = .FALSE.
 100  Idetck = 0
   CALL read(*1000,*900,Ifgpct,inpvt(1),2,Neor,iflag)
   Ngpct = inpvt(2)
   CALL read(*1000,*1100,Ifgpct,Iz(Igpct+1),Ngpct,Eor,iflag)
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
      GOTO 1300
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
      Iz(Ipoint+1) = 1
      i1 = 1
      i = Igpct
      j = Ipoint + 1
      DO
         i1 = i1 + 1
         IF ( i1>Ngpct ) THEN
!
!     JMAX = THE NUMBER OF COLUMNS OF KGG THAT WILL BE GENERATED WITH
!     THE CURRENT GRID POINT.
!
            inc = 5
            ilast = Igpct + Ngpct
            jlast = Ipoint + Npoint
            IF ( Iz(ilast)<0 ) inc = 0
            Jmax = Iz(jlast) + inc
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
                  GOTO 1300
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
            EXIT
         ELSE
            i = i + 1
            j = j + 1
            inc = 6
            IF ( Iz(i)<0 ) inc = 1
            Iz(j) = Iz(j-1) + inc
         ENDIF
      ENDDO
   ENDIF
!
!     ZERO OUT THE KGG SUBMATRIX IN CORE
!
 200  low = I6x6k + 1
   lim = I6x6k + Jmax*Nrowsc
   DO i = low , lim
      Dz(i) = 0.0D0
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
         Dz(i) = 0.0D0
      ENDDO
   ENDIF
!
!     INITIALIZE THE LINK VECTOR TO -1.
!
   DO i = 1 , Nlinks
      Link(i) = -1
   ENDDO
!
!     TURN FIRST PASS INDICATOR ON.
!
 300  ifirst = 1
!
!     READ THE 1ST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT.
!
   CALL fread(Ifecpt,Npvt,1,0)
   DO
!
!     READ THE NEXT ELEMENT TYPE INTO THE CELL ITYPE.
!
      CALL read(*1200,*400,Ifecpt,itype,1,Neor,iflag)
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
         WRITE (Sysprt,99002) Ufm , itype
99002    FORMAT (A23,' 2201, ELEMENT TYPE',I4,' NO LONGER SUPPORTED BY ','SMA1 MODULE.',/5X,                                        &
                &'USE EMG AND EMA MODULES FOR ELEMENT MATRIX GENERATION')
         Nogo = .TRUE.
         GOTO 1000
      ENDIF
   ENDDO
!
!     AT STATEMENT NO. 500 WE HAVE HIT AN EOR ON THE ECPT FILE.  SEARCH
!     THE LINK VECTOR TO DETERMINE IF THERE ARE LINKS TO BE PROCESSED.
!
 400  Link(lincor) = 1
   DO i = 1 , Nlinks
      IF ( Link(i)==0 ) GOTO 500
   ENDDO
!
!    CHECK NOGOO FLAG. IF 1 SKIP BKDPK AND PROCESS ANOTHER GRID POINT
!    FROM GPCT
!
   IF ( Nogoo==1 ) GOTO 100
!
!     IF NO GENERAL ELEMENTS EXIST, CHECK FOR GRID POINT SINGULARITIES.
!
!WKBR IF (DODET) CALL DETCK (0)
   IF ( Dodet ) CALL detck(0,Npvt,Ifgpst)
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE UNTO THE KGG FILE.
!
   ASSIGN 700 TO iretrn
   ifile = Ifkgg
   imcb = 1
   i1 = 0
   GOTO 600
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
 500  CALL bckrec(Ifecpt)
   GOTO 300
 600  i2 = 0
   ibeg = I6x6k + i1*Jmax
   CALL bldpk(2,ipr,ifile,0,0)
   DO
      i2 = i2 + 1
      IF ( i2>Ngpct ) THEN
         CALL bldpkn(ifile,0,Mcbkgg(imcb))
         i1 = i1 + 1
         IF ( i1<Nrowsc ) GOTO 600
         GOTO iretrn
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
!     IF THE K4GG IS CALLED FOR, BLDPK IT.
!
 700  IF ( Iopt4/=0 ) THEN
      IF ( Iopt4/=-1 ) THEN
!
!     THE K4GG MATRIX IS IN CORE.
!
         ASSIGN 800 TO iretrn
         I6x6k = I6x64
         ifile = If4gg
         imcb = 8
         i1 = 0
         GOTO 600
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
 800  IF ( Lrowic==Tnrows ) GOTO 100
   CALL bckrec(Ifecpt)
   Frowic = Frowic + Nrowsc
   Lrowic = Lrowic + Nrowsc
   GOTO 200
!
!     CHECK NOGOO = 1 SKIP BLDPK AND PROCESS ANOTHER RECORD
!
 900  IF ( Nogoo/=1 ) THEN
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
   GOTO 100
!
!     RETURN SINCE AN EOF HAS BEEN HIT ON THE GPCT FILE
!
 1000 IF ( .NOT.Nogo .AND. Nogoo==0 ) RETURN
   iparm = -61
   GOTO 1300
!
!     ERROR RETURNS
!
 1100 ifile = Ifgpct
   iparm = -3
   GOTO 1300
 1200 ifile = Ifecpt
   iparm = -2
 1300 CALL mesage(iparm,ifile,name)
   CALL mesage(-30,87,itype)
END SUBROUTINE sma1a
