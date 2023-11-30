
SUBROUTINE linel(Iz,Nwds,Opcor,Opt,X,Pen,Deform,Gplst)
   IMPLICIT NONE
   INTEGER Elset , G(3) , Iout , Merr , Ngp , Nnn , Offscl , Pedge
   REAL Skp1(9) , Skp2(2) , Skp3(7) , Skp4 , Skp5(15) , Skp6(235)
   COMMON /blank / Ngp , Skp1 , Skp2 , Elset , Skp3 , Merr
   COMMON /drwdat/ Skp5 , Pedge
   COMMON /pltscr/ Nnn , G
   COMMON /system/ Skp4 , Iout
   COMMON /xxparm/ Skp6 , Offscl
   INTEGER Deform , Nwds , Opcor , Opt , Pen
   INTEGER Gplst(1) , Iz(1)
   REAL X(3,1)
   INTEGER elid , etyp , i , i1 , i2 , ii , iret , j , k , k2d8 , kae , kbar , kfh1 , kfh2 , kfte , kfwd , khb , khx1 , khx2 ,      &
         & kix1 , kix2 , kix3 , kq4 , kt3 , ktet , ktm6 , ktrplt , ktrshl , kweg , l , l1 , ldx(9) , lid , ll , m , m1(16) , n ,    &
         & name(2) , ng(121) , ngpel , ngpelx , ngtyp(2,13) , nm1 , offset , type
!
!     CALL TO LINEL IS AS FOLLOWS -
!
!     (1)
!     OPT = ZERO (INPUT) - TO CREATE COMPLETE LINE CONNECTION TABLE OF
!     **********           ELEMENTS OF ALL TYPES, TO BE USED BY SUPLT
!                          SUBROUTINE
!        INPUT-
!           OPCOR (INPUT) = NUMBER OF WORDS OF OPEN CORE FOR -IZ-
!        OUTPUT-
!           IZ   = LIST OF GRID POINT ELEMENET CONNECTIONS AND POINTERS
!                  TO EACH GRID POINT, FROM IZ(1) THRU IZ(NWDS). DATA
!                  COMPOSED OF   1. GPCT,  AND 2. NGP WORDS OF CONTROL
!                  POINTERS
!           NWDS = NO. OF WORDS IN IZ PRIOR TO POINTER ARRAY.
!                  I.E. 1 LESS THAN LOCATION OF POINTERS,
!                = 0 IF ARRAY NOT CREATED
!           OPT  = NWDS
!
!     (2)
!     OPT = NONZERO (INPUT) - LOAD INTO CORE THE GRID POINT CNNECTION
!     *************           LIST OF ALL ELEMENTS OF THE SAME TYPE
!
!        INPUT-
!           NWDS  = ETYP, 2 BCD WORDS (CALLING ROUTINE HAS ALREADY READ
!                   THIS WORD FROM DATA BLOCK ELSET)
!           OPT   = MO. OF GRID POINT CONNECTIONS PER ELEMENT, NGPEL
!                   (CALLING ROUTINE HAS ALREADY READ THIS WORD)
!           OPCOR = OPEN CORE AVAILABLE W.R.T. IZ(1)
!           GPLST = A SUBSET LIST OF GRID POINTS PERTAINING TO THOSE
!                   POINTS USED ONLY IN THIS PLOT
!        OUTPUT-
!           IZ    = GRID POINT CONNECTION LIST FOR ALL ELEMENTS OF THIS
!                   TYPE, OR AS MANY ELEMS OF THIS TYPE AS CORE ALLOWS.
!           NWDS  = TOTAL LENGTH OF TABLE IZ
!           OPT   = NUMBER OF CONNECTIONS PER ELEMENT
!           (IF INSUFF. CORE TO READ ALL THE ELEMENTS, BOTH NWDS AND OPT
!           ARE SET TO NEGATIVE UPON RETURN. FURTHER CALLS MUST BE MADE
!           TO COMPLETE THIS ELEMENT
!           IF ILLEGAL ELEMENT IS ENCOUNTERED, NWDS AND OPT ARE SET TO
!           ZERO, AND ELSET IS SPACED OVER THE ELEMENT)
!
!           (NOTE THAT  'DO 100 I=1,NWDS,OPT'  MAY THEN BE USED
!           BUT IT IS MORE EFFICIENT TO USE  'DO 100 I=1,NWDS' AND CHECK
!           ZERO AS THE COMMAND TO LIFT THE PEN)
!
!     EACH ELEMENT TYPE HAS THE FOLLOWING DATA IN ELSET FILE
!           ELTYP = BCD SYMBOL (1 WORD)
!           NGPEL = NUM. GRID POINTS.
!                   IF NEGATIVE OR .GT. 4 NOT A CLOSED LOOP
!           ELID  = ELEMENT ID
!           G     = NGPEL GRIDS.
!           LOOP THRU ELID AND G UNTIL ELID = 0 (I.E. NO MORE ELEMS OF
!                                              THIS TYPE)
!     (3)
!     ELEMENT OFFSET PLOT (UNDEFORMED PLOT ONLY, PEDGE=3),
!     *******************
!     IF ELEMENTS WITH OFFSET ARE PRESENT, CALL OFSPLT TO PLOT THEM OUT
!     AND DO NOT INCLUDE THEM IN THE IZ TABLE
!     IF OFFSET COMMAND IS REQUESTED BY USER VIA THE PLOT CARD
!     (PEDGE = 3), SKIP COMPLETELY THE GENERATION OF THE IZ TABLE
!
!     OFFSET n OPTION (ON PLOT CONNAND CARD IN CASE CONTROL SECTION) -
!       n .LT. 0, SKIP OFFSET VALUES ON GENERAL PLOTS. (PEDGE.NE.3)
!       n =    0, OFFSET VALUES INCLUDED IN ALL GENERAL PLOTS (PEDGE=3)
!       n .GT. 0, PLOT ONLY THOSE ELEMENTS HAVING OFFSET DATA, OFFSET
!                 DATA ARE MAGNIFIED n TIMES. (PEDGE=3)
!     SUBROUTINE PLOT SETS THE PEDGE FLAG, AND PLTSET SETS THE OFFSCL.
!
   DATA name/4HLINE , 1HL/ , nm1 , m1/16 , 4H(33X , 4H,13H , 4HELEM , 4HENT  , 4HTYPE , 4H ,A5 , 4H,4HW , 4HITH, , 4HI8,2 , 4H4H G ,&
       &4HRIDS , 4H SKI , 4HPPED , 4H IN  , 4HLINE , 4HL.) /
!
!     SPECIAL ELEMENT CONNECTION PATTERNS
!
   DATA ldx/2HD1 , 2HD2 , 2HD3 , 2HD4 , 2HD5 , 2HD6 , 2HD7 , 2HD8 , 2HD9/
   DATA ktet/2HTE/ , kweg/2HWG/ , khx1/2HH1/ , khx2/2HH2/ , kix1/2HXL/ , kix2/2HXQ/ , kix3/2HXC/ , kae/2HAE/ , ktm6/2HT6/ ,         &
       &ktrplt/2HP6/ , ktrshl/2HSL/ , kfh1/2HFA/ , kfh2/2HFB/ , kfwd/2HFW/ , kfte/2HFT/ , k2d8/2HD8/ , khb/2HHB/ , kbar/2HBR/ ,     &
       &kt3/2HT3/ , kq4/2HQ4/
!
!     NGTYP(1,TYPE) = LOCATION WORD 1 IN -NG-, +N = POINTER TO G
!                                              -N = THRU POINTER TO G
!     BE SURE TO KEEP PEN DOWN                  0 = LIFT PEN
!     AS MUCH AS POSSIBLE.
!     NGTYP(2,TYPE) = NUMBER OF ENTRIES/ELEMENT MINUS 1 IN TABLE IZ
!
   DATA ngtyp/0 , 0 , 3 , 9 , 10 , 14 , 22 , 19 , 37 , 30 , 56 , 43 , 79 , 6 , 83 , 7 , 86 , 9 , 95 , 10 , 102 , 8 , 108 , 2 , 110 ,&
      & 7/
!    1 - LINE,TRIANGLE,QUAD
!    2 - TETRA (WORD 3)
!    3 - WEDGE (WORD 10)
!    4 - HEXA  (WORD 22)
!    5 - IHEXA2 (WORD 37)
!    6 - IHEXA3 (WORD 56)
!    7 - AREO (WORD 79)
!    8 - TRIM6, TRPLT1, AND TRSHL (WORD 83)
!    9 - IS2D8 (WORD 86)
!   1O - POINT (WORD 95)
!   11 - LINE (WORD 102)
!   12 - REV OR ELIP CYL. (WORD 108)
!   13 - AREA3 (WORD 110)
!   14 - AREA4 (WORD 116)
   DATA ng/1 , -5 , 1 , -4 , 1 , 3 , 0 , 2 , 4 , 1 , -3 , 1 , 4 , -6 , 4 , 0 , 5 , 2 , 0 , 3 , 6 , 1 , -4 , 1 , 5 , -8 , 5 , 0 , 6 ,&
      & 2 , 0 , 3 , 7 , 0 , 8 , 4 , 1 , -8 , 1 , 9 , 13 , -20 , 13 , 0 , 15 , 10 , 3 , 0 , 5 , 11 , 17 , 0 , 19 , 12 , 7 , 1 , -12 ,&
      & 1 , 13 , 17 , 21 , -32 , 21 , 0 , 24 , 18 , 14 , 4 , 0 , 7 , 15 , 19 , 27 , 0 , 30 , 20 , 16 , 10 , 1 , -4 , 1 , 0 , 1 ,    &
      & -6 , 1 , 1 , 5 , 2 , 6 , 3 , 7 , 4 , 8 , 1 , 2 , -6 , 7 , 2 , 0 , 1 , 8 , 3 , -6 , 3 , 0 , 7 , 8 , 1 , 2 , 1 , -3 , 1 , 0 , &
      & 4 , 5 , 1 , -4 , 1 , 0 , 5 , 6/
!
   k = 1
   IF ( Opt/=0 ) THEN
      etyp = Nwds
      i = Opt
      GOTO 200
   ENDIF
!
 100  IF ( Opt/=0 ) GOTO 700
   CALL read(*1600,*900,Elset,etyp,1,0,i)
   CALL fread(Elset,i,1,0)
!
 200  ngpel = iabs(i)
   ngpelx = ngpel
   offset = 0
   IF ( etyp==kbar ) offset = 6
   IF ( etyp==kt3 .OR. etyp==kq4 ) offset = 1
!
   type = 1
   IF ( etyp==ktet .OR. etyp==kfte ) type = 2
   IF ( etyp==kweg .OR. etyp==kfwd ) type = 3
   IF ( etyp==khx1 .OR. etyp==khx2 .OR. etyp==kfh1 .OR. etyp==kfh2 .OR. etyp==kix1 ) type = 4
   IF ( etyp==kix2 ) type = 5
   IF ( etyp==kix3 ) type = 6
   IF ( etyp==kae ) type = 7
   IF ( etyp==ktm6 .OR. etyp==ktrplt .OR. etyp==ktrshl ) type = 8
   IF ( etyp==k2d8 ) type = 9
   IF ( etyp==khb ) type = 10
!                   CHBDY TYPE = 10,11,12,13,14
!
   IF ( type/=1 ) THEN
!
!     COMPLEX ELEMENT
!
      l1 = ngtyp(1,type)
      m = ngtyp(2,type)
   ELSE
!
!     SIMPLE ELEMENT
!
      IF ( ngpel>2 .AND. i>0 ) ngpelx = ngpel + 1
      IF ( ngpel>4 ) THEN
!
!     CHECK FOR PDUM ELEMENTS BEFORE REJECTING
!
         DO ii = 1 , 9
            IF ( etyp==ldx(ii) ) CALL pdumi(*100,*800,*600,ii,m,Opcor,ngpel,k,Elset,Opt)
         ENDDO
         GOTO 600
      ELSE
         l1 = 1
         m = ngpelx
      ENDIF
   ENDIF
   IF ( ngpelx>Nnn ) GOTO 600
!
!     READ THE ELEMENT DATA
!
 300  CALL fread(Elset,elid,1,0)
   IF ( elid<=0 ) GOTO 100
   CALL fread(Elset,lid,1,0)
   CALL fread(Elset,G,ngpel,0)
   IF ( ngpel/=ngpelx ) G(ngpelx) = G(1)
!
!     CALL OFSPLT TO PROCESS OFFSET PLOT
!
   IF ( offset/=0 ) CALL ofsplt(*300,etyp,elid,G,offset,X,Deform,Gplst)
   IF ( type>=10 .AND. type<=14 ) THEN
!
!     SPECIAL HANDLING FOR CHBDY
!
      type = 9 + G(ngpel)
      l1 = ngtyp(1,type)
      m = ngtyp(2,type)
   ENDIF
!
   l = l1
!
   IF ( Opt/=0 ) THEN
!
!     ON CONVERSION REMOVE ABOVE CODE
!
!     LOAD ELEMENT INTO CORE
!
      n = k + m
!
!     THIS TEST PROTECTS THE CORE FOR THE FIRST ELEMENT READ
!
      IF ( n+1>Opcor ) GOTO 600
      i1 = 0
      i2 = ng(l)
      GOTO 500
   ELSE
!
!     CREATING CONNECTION ARRAY FOR SUPLT
!
      ll = 0
      i1 = 0
   ENDIF
 400  DO
      i2 = ng(l)
      IF ( i1==0 ) EXIT
      IF ( i2<0 ) THEN
!
!     THRU RANGE
!
         i2 = -i2
         i2 = min0(i2,m)
         j = i1 + 1
         i1 = G(i1)
         IF ( 2*(i2-j+1)+k>Opcor ) THEN
!
            Opt = 0
            Nwds = 0
            GOTO 1500
         ELSE
            DO i = j , i2
               Iz(k) = min0(G(i),i1)
               Iz(k+1) = max0(G(i),i1)
               k = k + 2
               ll = ll + 1
               i1 = G(i)
            ENDDO
            IF ( ll==m-1 ) ll = ll - 1
            EXIT
         ENDIF
      ELSEIF ( i2==0 ) THEN
!
         i1 = 0
         l = l + 1
      ELSE
!
         IF ( k+1>Opcor ) GOTO 800
         Iz(k) = min0(G(i2),G(i1))
         Iz(k+1) = max0(G(i2),G(i1))
         k = k + 2
         EXIT
      ENDIF
   ENDDO
   ll = ll + 1
   i1 = i2
   IF ( ll>=m ) GOTO 300
   l = l + 1
   GOTO 400
 500  i1 = i2
   l = l + 1
   i2 = ng(l)
   IF ( i1==0 ) THEN
!
      Iz(k) = G(i2)
   ELSEIF ( i2<0 ) THEN
      i2 = -i2
!
!     NEXT LINE FOR ELEMENTS WITH MORE THAN ONE THRU POINTER
!
      IF ( n/=k+m ) i1 = i1 + 1
      DO i = i1 , i2
         Iz(k) = G(i)
         k = k + 1
      ENDDO
      k = k - 1
   ELSEIF ( i2==0 ) THEN
      Iz(k) = i2
   ELSE
      Iz(k) = G(i2)
   ENDIF
   k = k + 1
   IF ( k<n ) GOTO 500
!
!     STORE ZERO AT THE END OF EACH ELEMENT
!
   Iz(k) = 0
   k = k + 1
   IF ( k+m+1<=Opcor ) GOTO 300
   GOTO 800
!
!     ILLEGAL ELEMENT, NO CORE FOR 1 ELEMENT
!
 600  G(1) = 2
   G(2) = etyp
   G(3) = ngpel
   CALL wrtprt(Merr,G,m1,nm1)
   DO
!
!     READ TO THE END OF THIS ELEMENT
!
      CALL fread(Elset,elid,1,0)
      IF ( elid<=0 ) THEN
!
!     NOTE THAT BOTH OPT AND NWDS=0 FOR ILLEGAL ELEMENTS
!
         IF ( Opt==0 ) GOTO 100
         Opt = 0
         Nwds = 0
         GOTO 1500
      ELSE
         j = 1 + ngpel + offset
         CALL fread(Elset,0,-j,0)
      ENDIF
   ENDDO
!
!     END OF OPT.NE.0
!
 700  Nwds = k - 1
   Opt = m + 2
   GOTO 1500
!
!     INSUFFICIENT CORE FOR ALL ELEMENTS
!
 800  IF ( Opt==0 ) THEN
      Opt = 0
      Nwds = 0
   ELSE
      Nwds = 1 - k
      Opt = -(m+2)
   ENDIF
   GOTO 1500
!
!     SORT
!
 900  IF ( Pedge==3 ) THEN
      Nwds = 0
      GOTO 1500
   ELSE
      IF ( Opt/=0 ) GOTO 700
      IF ( k<=1 ) THEN
         Nwds = 0
         GOTO 1500
      ELSE
         CALL sort(0,0,2,1,Iz,k-1)
!
!     NWDS IS SET TO NO. OF WORDS PRIOR TO ELIMINATING DUPLICATES
!
         Nwds = k - 1
         IF ( Nwds<=2 ) GOTO 1300
         ASSIGN 1300 TO iret
      ENDIF
   ENDIF
!
!     ELIMINATE DUPLICATE ENTRIES FROM LIST SORTED ON FIRST ENTRY
!
 1000 i = 1
   l = 1
   ll = Iz(l)
!
!
   DO j = 3 , Nwds , 2
      IF ( Iz(j)/=ll ) THEN
!
!     NEW PIVOT
!
         l = i + 2
         ll = Iz(j)
      ELSEIF ( Iz(j+1)<Iz(i+1) ) THEN
!
!     SECOND COLUMN OUT-OF-SORT
!     LOAD ENTRY SORTED.  CHECK PREVIOUS ENTRIES
!     L = LOWER LIMIT OF COLUMN 1 FOR MERGING
!     K SET TO FIRST ENTRY OF NEXT NEW ENTRY IN LIST INITIALLY
!
         k = i
         DO WHILE ( k>l )
            IF ( Iz(j+1)<Iz(k-1) ) THEN
               k = k - 2
            ELSEIF ( Iz(j+1)==Iz(k-1) ) THEN
               GOTO 1200
            ELSE
               EXIT
            ENDIF
         ENDDO
         GOTO 1050
      ELSEIF ( Iz(j+1)==Iz(i+1) ) THEN
         CYCLE
      ENDIF
!
!     UNIQUE ENTRY FOR PIVOT FOUND
!
      Iz(i+2) = ll
      Iz(i+3) = Iz(j+1)
      GOTO 1100
!
!     LOAD ENTRY INTO LOCATION
!
 1050 n = Iz(j+1)
      m = i + 2
      DO
         Iz(m+1) = Iz(m-1)
         m = m - 2
         IF ( m<=k ) THEN
            Iz(k+1) = n
            Iz(i+2) = ll
            EXIT
         ENDIF
      ENDDO
!
!     INCREMENT FOR ENTRY LOADED
!
 1100 i = i + 2
 1200 ENDDO
!
!     NWDS RESET TO NO. WORDS AFTER ELIMINATING DUPLICATE ENTRIES
!
   Nwds = i + 1
   GOTO iret
!
!
!     K IS SET TO THE NEXT PART OF CORE WHICH WILL BE FILLED WITH THE
!     HIGHER ENTRY IN THE FIRST POSITION
!
 1300 k = Nwds + 1
   IF ( 2*Nwds>Opcor ) THEN
      Nwds = 0
      GOTO 1500
   ELSE
      DO i = 1 , Nwds , 2
         Iz(k) = Iz(i+1)
         Iz(k+1) = Iz(i)
         k = k + 2
      ENDDO
      Nwds = k - 1
      CALL sort(0,0,2,1,Iz,Nwds)
      ASSIGN 1400 TO iret
      GOTO 1000
   ENDIF
!
 1400 IF ( Nwds+Ngp+1>Opcor ) THEN
      Nwds = 0
   ELSE
      k = 1
      j = 1
      l = 1
      m = 1 + Nwds
      i = 0
      Iz(m) = 1
      DO
!
!     CREATE A GPCT --- M = POINTER FOR POINTER ARRAY
!                       L = SIL NUMBER
!                       J = POINTER TO NEXT GPCT ENTRY
!
         IF ( Iz(k)==l ) THEN
!
!     CONNECTED POINT
!
            Iz(j) = Iz(k+1)
            k = k + 2
            j = j + 1
            i = i + 1
         ELSE
            DO
!
!     NEW PIVOT
!
               m = m + 1
               Iz(m) = Iz(m-1) + i
               l = l + 1
               i = 0
               IF ( l>Ngp ) THEN
!
!     EFFICIENCY PLOT POSSIBLE
!
                  Opt = Nwds
                  GOTO 1500
               ELSEIF ( k<=Nwds ) THEN
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDIF
 1500 RETURN
!
 1600 CALL mesage(-2,Elset,name)
   GOTO 1500
END SUBROUTINE linel