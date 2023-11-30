
SUBROUTINE sma2a
   IMPLICIT NONE
   REAL Bggind , Dum(2) , Ecpt(200) , Wtmass , Z(1)
   INTEGER Clsnrw , Clsrw , Eor , Frowic , I6x6b , I6x6m , Icmas , Icmbar , Icmqd1 , Icmqd2 , Icmqdp , Icmrod , Icmtr1 , Icmtr2 ,   &
         & Icmtrb , Icmtrp , Icmtub , Icstm , Idum1 , Idum2 , Idum3 , Idum4 , Idum5 , Ifbgg , Ifcstm , Ifdit , Ifecpt , Ifgpct ,    &
         & Ifmgg , Ifmpt , Igbgg , Igecpt , Iggpct , Igmgg , Igpct , Incr , Index , Inrw , Ioptb , Ipoint , Iprec , Isew1(53) ,     &
         & Isys , Iz(1) , Jmax , Last , Link(10) , Lleft , Lrowic , Mcbbgg(7) , Mcbmgg(7) , N6x6b , N6x6m , Ncstm , Ne(1) , Nelems ,&
         & Neor , Ngpct , Nlinks , Nobgg , Nogo , Nomgg , Npoint , Npvt , Nrowsc , Outrw , Tnrows
   DOUBLE PRECISION Dpword , Dz(1)
   LOGICAL Heat
   CHARACTER*23 Ufm
   COMMON /blank / Wtmass , Nomgg , Nobgg , Icmas , Icmbar , Icmrod , Icmqd1 , Icmqd2 , Icmtr1 , Icmtr2 , Icmtub , Icmqdp , Icmtrp ,&
                 & Icmtrb
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /sma2bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6m , N6x6m , I6x6b , N6x6b
   COMMON /sma2cl/ Ioptb , Bggind , Npvt , Lleft , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Nogo
   COMMON /sma2et/ Ecpt
   COMMON /sma2ht/ Heat
   COMMON /sma2io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Idum2 , Idum3 , Ifmgg , Igmgg , Ifbgg ,     &
                 & Igbgg , Idum4 , Idum5 , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbmgg , Mcbbgg
   COMMON /system/ Isys , Isew1 , Iprec
   COMMON /xmssg / Ufm
   COMMON /zblpkx/ Dpword , Dum , Index
   COMMON /zzzzzz/ Z
   INTEGER i , i1 , i2 , i3 , ibeg , idx , ifile , ifirst , iflag , ilast , imcb , inc , inpvt(2) , iparm , ipr , iretrn , itemp ,  &
         & itype , itypx , j , jj , jjj , jlast , kkk , left , lim , lincor , low , name(2) , sysprt
!
!     THIS SUBROUTINE FORMERLY GENERATED THE MGG AND BGG MATRICES FOR
!     THE SMA2 MODULE.  THESE OPERATIONS ARE NOW PERFORMED IN THE EMG
!     AND EMA MODULES AND SMA2A IS RETAINED IN SKELETAL FORM TO PROVIDE
!     A VEHICLE FOR USER-SUPPLIED ELEMENTS.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA name/4HSMA2 , 4HA   /
!
   ipr = Iprec
!
!     READ THE FIRST TWO WORDS OF NEXT GPCT RECORD INTO INPVT(1).
!     INPVT(1) IS THE PIVOT POINT.  INPVT(1) .GT. 0 IMPLIES THE PIVOT
!     POINT IS A GRID POINT.  INPVT(1) .LT. 0 IMPLIES THE PIVOT POINT
!     IS A SCALAR POINT.  INPVT(2) IS THE NUMBER OF WORDS IN THE
!     REMAINDER OF THIS RECORD OF THE GPCT.
!
 100  CALL read(*1000,*900,Ifgpct,inpvt(1),2,Neor,iflag)
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
      CALL mesage(-8,ifile,name)
      GOTO 1200
   ELSE
      Ipoint = Igpct + Ngpct
      Npoint = Ngpct
      I6x6m = Ipoint + Npoint
      I6x6m = (I6x6m-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE, WHICH WILL ENABLE SUBROUTINE INSERT
!     TO ADD THE ELEMENT MASS AND/OR DAMPING MATRICES TO MGG AND/OR BGG.
!
      Iz(Ipoint+1) = 1
      i1 = 1
      i = Igpct
      j = Ipoint + 1
      DO
         i1 = i1 + 1
         IF ( i1>Ngpct ) THEN
!
!     JMAX = THE NUMBER OF COLUMNS OF MGG THAT WILL BE GENERATED WITH
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
!     THE MGG SINCE THE WHOLE DOUBLE PRECISION SUBMATRIX OF ORDER
!     TNROWS*JMAX CAN FIT IN CORE.
!
            itemp = Tnrows*Jmax
            IF ( 2*itemp<left ) THEN
               Nrowsc = Tnrows
            ELSE
               CALL mesage(30,86,inpvt)
!
!     THE WHOLE MATRIX CANNOT FIT IN CORE, DETERMINE HOW MANY ROWS CAN
!     FIT. IF TNROWS = 1, WE CAN DO NOTHING FURTHER.
!
               IF ( Tnrows==1 ) THEN
                  CALL mesage(-8,ifile,name)
                  GOTO 1200
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
!     ZERO OUT THE MGG SUBMATRIX IN CORE
!
 200  low = I6x6m + 1
   lim = I6x6m + Jmax*Nrowsc
   DO i = low , lim
      Dz(i) = 0.0D0
   ENDDO
!
!     CHECK TO SEE IF BGG MATRIX IS DESIRED.
!
   IF ( Ioptb/=0 ) THEN
!
!     SINCE THE BGG MATRIX IS TO BE COMPUTED,DETERMINE WHETHER OR NOT IT
!     TOO CAN FIT IN CORE.
!
      IF ( Nrowsc/=Tnrows ) THEN
!
!     OPEN A SCRATCH FILE FOR BGG
!
         CALL mesage(-8,0,name)
      ELSEIF ( 4*Tnrows*Jmax>=left ) THEN
         CALL mesage(-8,0,name)
      ENDIF
!
!     THIS CODE TO BE FILLED IN LATER
!     ===============================
!
      I6x6b = I6x6m + Jmax*Tnrows
      low = I6x6b + 1
      lim = I6x6b + Jmax*Tnrows
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
      IF ( itype>=53 .AND. itype<=61 ) THEN
!
!     READ THE ECPT ENTRY FOR THE CURRENT TYPE INTO THE ECPT ARRAY. THE
!     NUMBER OF WORDS TO BE READ WILL BE NWORDS(ITYPE).
!
         idx = (itype-1)*Incr
         CALL fread(Ifecpt,Ecpt,Ne(idx+12),0)
         itemp = Ne(idx+23)
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
            CALL mdum2
         ELSEIF ( itypx==3 ) THEN
            CALL mdum3
         ELSEIF ( itypx==4 ) THEN
            CALL mdum4
         ELSEIF ( itypx==5 ) THEN
            CALL mdum5
         ELSEIF ( itypx==6 ) THEN
            CALL mdum6
         ELSEIF ( itypx==7 ) THEN
            CALL mdum7
         ELSEIF ( itypx==8 ) THEN
            CALL mdum8
         ELSEIF ( itypx==9 ) THEN
            CALL mdum9
         ELSE
!
!
            CALL mdum1
         ENDIF
      ELSE
         CALL page2(-3)
         sysprt = Isew1(1)
         WRITE (sysprt,99001) Ufm , itype
99001    FORMAT (A23,' 2202, ELEMENT TYPE',I4,' NO LONGER SUPPORTED BY ','SMA2 MODULE.',/5X,                                        &
                &'USE EMG AND EMA MODULES FOR ELEMENT MATRIX GENERATION')
         Nogo = 1
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
!     CHECK NOGO = 1 SKIP BLDPK
!
   IF ( Nogo==1 ) GOTO 100
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE UNTO THE MGG FILE.
!
   ASSIGN 700 TO iretrn
!
!     HEAT TRANSFER PROBLEM, SKIP MGG
!
   IF ( Heat ) GOTO 700
!
   ifile = Ifmgg
   imcb = 1
!
!     MULTIPLY THE MASS MATRIX BY THE PARAMETER WTMASS IF IT IS NOT
!     UNITY
!
   IF ( Wtmass/=1.0 ) THEN
      low = I6x6m + 1
      lim = I6x6m + Jmax*Nrowsc
      DO i = low , lim
         Dz(i) = Dz(i)*Wtmass
      ENDDO
   ENDIF
   i1 = 0
   GOTO 600
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
 500  CALL bckrec(Ifecpt)
   GOTO 300
 600  i2 = 0
   ibeg = I6x6m + i1*Jmax
   CALL bldpk(2,ipr,ifile,0,0)
   DO
      i2 = i2 + 1
      IF ( i2>Ngpct ) THEN
         CALL bldpkn(ifile,0,Mcbmgg(imcb))
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
!     IF THE BGG IS CALLED FOR BLDPK IT.
!
 700  IF ( Ioptb/=0 ) THEN
      IF ( Ioptb/=-1 ) THEN
!
!     THE BGG MATRIX IS IN CORE
!
         ASSIGN 800 TO iretrn
         I6x6m = I6x6b
         ifile = Ifbgg
         imcb = 8
         i1 = 0
         GOTO 600
      ENDIF
   ENDIF
!
!     HERE WE NEED LOGIC TO READ BGG FROM A SCRATCH FILE AND INSERT
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
!     CHECK NOGO = 1 SKIP BLDPK
!
 900  IF ( Nogo/=1 ) THEN
!
!     HERE WE HAVE A PIVOT POINT WITH NO ELEMENTS CONNECTED, SO THAT
!     NULL COLUMNS MUST BE OUTPUT ON THE MGG AND BGG FILES.
!
      lim = 6
      IF ( inpvt(1)<0 ) lim = 1
      DO i = 1 , lim
         IF ( .NOT.(Heat) ) THEN
            CALL bldpk(2,ipr,Ifmgg,0,0)
            CALL bldpkn(Ifmgg,0,Mcbmgg)
         ENDIF
         IF ( Ioptb==1 ) THEN
            CALL bldpk(2,ipr,Ifbgg,0,0)
            CALL bldpkn(Ifbgg,0,Mcbbgg)
         ENDIF
      ENDDO
      CALL skprec(Ifecpt,1)
   ENDIF
   GOTO 100
!
!     RETURN SINCE AN EOF HAS BEEN HIT ON THE GPCT FILE
!
 1000 IF ( Nogo==1 ) CALL mesage(-61,0,name)
   RETURN
!
!     ERROR RETURNS
!
 1100 ifile = Ifgpct
   iparm = 3
   GOTO 1300
 1200 ifile = Ifecpt
   iparm = 2
 1300 CALL mesage(-iparm,ifile,name)
   CALL mesage(-30,87,itype)
!
END SUBROUTINE sma2a