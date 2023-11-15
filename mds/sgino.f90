
SUBROUTINE sgino
!
!     REVISED  9/90 BY G.CHAN/UNISYS. TO REACTIVATE PLT1 FILE
!
!     THE HIGH POINTS OF PLT1 FILE ARE
!       NEW 130 COLUMN FORMAT RECORD
!       MACHINE PORTABLE FILE
!       NO DATA RECONSTRUCTION REQUIRED WHEN PLT1 IS USED BY AN EXTERNAL
!          TRANSLATOR PROGRAM
!                                          PLT2 FILE       PLT1 FILE
!     ---------------------------------  -------------  --------------
!     FILE TYPE, SEQUENTIAL FORMATTED    NO CARRIGE CTRL CARRIAGE CTRL
!     RECORD  TYPE                        ASSCII/BINARY*    ASCII
!     RECORD  LENGTH                       3000 BYTES     130 COLUMNS
!     FORTRAN FORMAT                      (10(180A4))*   (5(2I3,4I5))
!     PLOT COMMANDS PER PHYSICAL RECORD        100            5
!     DATA TYPE PER COMMAND (TOTAL)         30 BYTES       26 DECIMALS
!          COMMAND, P   (SEE USER'S MANUAL   1 BYTE         3 DIGITS
!          CONTROL, C    PAGE 4.4-2)         1 BYTE         3 DITITS
!          FIRST  VALUE, R                   5 BYTES        5 DIGITS
!          SECOND VALUE, S                   5 BYTES        5 DIGITS
!          THIRD  VALUE, T                   5 BYTES        5 DIGITS
!          FOURTH VALUE, U                   5 BYTES        5 DIGITS
!          FILLER (ALL ZEROS)                8 BYTES          NONE
!     DATA BYTE PACKING                        YES            NO
!     FILE - EDITED, PRINTED, SCREEN VIEWING   NO             YES
!     PORTABLE FILE AMONG MACHINES             NO             YES
!     FORTRAN UNIT NUMBER                      13             12
!     DISC STORAGE REQUIREMENT                  -           25% LESS
!     IF MAGNETIC TAPE - TRACK AND PARITY     9,ODD          9,ODD
!     (* 1. ASCII RECORD, BUT DATA STORED IN BINARY BYTES.
!           (IN EARLY NASTRAN PLOT TAPE DESIGN, A BYTE HAD 6
!           BITS. BUT IT IS NO LONGER TRUE. NOW, A BYTE CAN
!           BE 6, 8 OR 9 BITS, DEPENDING ON THE MACHINE)
!        2. SINCE THE RECORD LENGTH IS 3000 BYTES, A FORMAT
!           OF (750A4) IS SUFFICIENT)
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   CHARACTER*80 Dsnames(80)
   INTEGER Idum1 , Nbpc , Nbpw , Ncpw , Nout , Skpsys(36)
   COMMON /dsname/ Dsnames
   COMMON /system/ Idum1 , Nout , Skpsys , Nbpc , Nbpw , Ncpw
!
! Dummy argument declarations
!
   INTEGER Eorx , Ibfsz , N , Pltape
   INTEGER A(1) , Buf(1)
!
! Local variable declarations
!
   INTEGER bfsz , eof , eor , format(3) , formtx(3) , irecsz , j , lbuf(1) , name(2) , nb , nbits , noff , nw , plt1 , plt2 , pltx ,&
         & ptape , shift , tra , word
   CHARACTER*7 fortn , none
   INTEGER locfx
   LOGICAL nopack , open
!
! End of declarations
!
!WKBNB
!WKBNE
   DATA open/.FALSE./ , name/4H SGI , 2HNO/
   DATA plt1 , plt2 , pltx/4HPLT1 , 4HPLT2 , 0/
   DATA format/4H(10( , 4H180A , 4H4)) / , formtx/4H(5(2 , 4HI3,4 , 4HI5))/
   DATA fortn , none/'FORTRAN' , 'NONE   '/
   DATA shift , nbits/0 , 0/
!
   GOTO 99999
!
!
   ENTRY sopen(*,Pltape,Buf,Ibfsz)
!     ================================
!
!     PLT2 FILE -
!     IBFSZ (FIRST WORD OF /XXPARM/), IS THE PLOT FILE BUFFER SIZE. IT
!     IS SET EQUAL TO PDATA(12,1)/NCPW IN PLTSET. PDATA(12,1) IS
!     INITIALIZED  IN PLOTBD VIA DATA(12,1) WHICH IS EQUIVALENT TO
!     PBFSIZ(1,1). COMPLICATED ISN'T IT?
!
!     (PBFSIZ(1,1)=3000, NCPW=4, IBFSZ AND BFSZ ARE THEREFORE =750 EACH
!     EACH PHYSICAL RECORD HOLDS 100 (=3000/30) PLOT COMMANDS)
!
!     NOTE - BOTH PLT2 AND PLT1 ARE SEQUENTIAL FILES, NOT DIRECT ACCESS
!     FILES. THE RECORD LENGTH, IF USED, IS BASED ON NO. CHARACTERS PER
!     WORD
!
   ptape = 10
   IF ( Pltape/=plt1 .AND. Pltape/=plt2 ) RETURN 1
   pltx = Pltape
   nopack = pltx==plt1
   IF ( nopack ) THEN
!
!     PLT1 -
!
!WKBR 10   PTAPE  = 12
      nopack = .TRUE.
      bfsz = 30
      irecsz = (bfsz/6)*(2*3+4*5)
      none = fortn
      format(1) = formtx(1)
      format(2) = formtx(2)
      format(3) = formtx(3)
!
!     NOFF CAN BE SET TO ZERO IF LBUF IS LOCALLY DIMENSIONED TO 30 WORDS
!     AND OPEN CORE IS NOT USED
!
      noff = locfx(Buf(1)) - locfx(lbuf(1))
   ELSE
!
!     PLT2 -
!
      bfsz = Ibfsz
      irecsz = Ncpw*bfsz
      noff = locfx(Buf(1)) - locfx(lbuf(1))
   ENDIF
!
!     OPEN STATEMENT ADDED TO SET OUTPUT RECORDSIZE GREATER THAN DEFAULT
!     (COMMENTS FORM G.C./UNISYS 1989 -
!     RECORDSIZE IS NOT ALLOWED FOR SEQUENTIAL FILE IN SOME COMPILERS,
!     (e.g. DEC/ULTRIX(RISC), AND BLOCKSIZE AND ORGANINZATION ARE NOT
!     DEFINED. RECORDTYPE='FIXED' IS ALSO NOT ALLOWED FOR SEQUENTIAL
!     FORMATTED FILE.
!     FOR UNICOS, RECL IS NOT ALLOWED IF ASSCESS=SEQUENTIAL)
!
!     FOR MACHINES THAT DO NOT HAVE 'APPEN' FEATURE
!
   IF ( open ) GOTO 200
!     MA = 'A'
!     IF (NONE .EQ. 'NONE') MA = 'M'
!     IF (MACH .EQ IBM) CALL FILEDEF (PTAPE,RECFM,FB(MA))
!WKBI
!HP  5      ,CARRIAGECONTROL = NONE
!HP  6      ,RECL  = IRECSZ
!            RECL IS NEEDED BY VAX, AND POSSIBLY OTHER MACHINES)
   OPEN (UNIT=ptape,FILE=Dsnames(10),STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=j)
   IF ( j/=0 ) THEN
!
!WKBI
!HP  5      ,CARRIAGECONTROL = NONE
!HP  6      ,RECL  = IRECSZ
!            RECL IS NEEDED BY VAX, AND POSSIBLY OTHER MACHINES)
      OPEN (UNIT=ptape,FILE=Dsnames(10),STATUS='NEW',FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=j)
      IF ( j/=0 ) THEN
         WRITE (Nout,99001) pltx , ptape
99001    FORMAT ('0*** SYSTEM FATAL ERROR. SGINO CAN NOT OPEN ',A4,' FILE, FORTRAN UNIT',I5)
         CALL mesage(-61,0,0)
      ENDIF
      GOTO 200
   ELSE
      DO
         READ (ptape,99002,END=100) j
99002    FORMAT (A1)
      ENDDO
   ENDIF
 100  BACKSPACE ptape
!
 200  open = .TRUE.
   nb = 1
   IF ( nopack ) THEN
!
      ASSIGN 400 TO tra
   ELSE
      ASSIGN 300 TO tra
      word = 0
      nbits = Nbpw - Nbpc
      shift = nbits
   ENDIF
   GOTO 99999
!
!
   ENTRY swrite(Pltape,A,N,Eorx)
!     ==============================
!
!     SWRITE IS CALLED ONLY BY WPLT10
!
   IF ( Pltape/=pltx ) GOTO 800
   eor = Eorx
   nw = 1
   IF ( nopack ) GOTO 400
!
!     ORIGINAL BYTE PACKING LOGIC
!
 300  DO WHILE ( nw<=N )
!UNIX IF (A(NW) .NE. 0) WORD =  OR(ISHFT(A(NW),SHIFT),WORD)
      IF ( A(nw)/=0 ) word = ior(ishft(A(nw),shift),word)
      nw = nw + 1
      IF ( shift==0 ) THEN
         lbuf(nb+noff) = word
         IF ( nb==bfsz ) GOTO 900
         word = 0
         nb = nb + 1
         shift = nbits
      ELSE
         shift = shift - Nbpc
      ENDIF
   ENDDO
!
   IF ( eor==0 ) GOTO 99999
   eor = 0
   IF ( shift/=nbits ) THEN
!
      lbuf(nb+noff) = word
      GOTO 900
   ELSE
      nb = nb - 1
      IF ( nb>0 ) GOTO 900
!
      nb = 1
      GOTO 99999
   ENDIF
!
!     NON BYTE PACKING LOGIC
!
 400  DO WHILE ( nw<=N )
      lbuf(nb+noff) = A(nw)
      nw = nw + 1
      nb = nb + 1
      IF ( nb>bfsz ) THEN
         nb = nb - 1
         GOTO 900
      ENDIF
   ENDDO
!
   IF ( eor==0 ) GOTO 99999
   eor = 0
   IF ( nb<bfsz ) THEN
      DO j = nb , bfsz
         lbuf(j+noff) = 0
      ENDDO
   ENDIF
   nb = bfsz
   GOTO 900
!
!
   ENTRY sclose(Pltape)
!     =====================
!
   eof = 0
!
 500  IF ( Pltape/=pltx ) GOTO 800
   IF ( .NOT.nopack .AND. shift/=nbits ) THEN
      lbuf(nb+noff) = word
      ASSIGN 600 TO tra
      GOTO 900
   ELSE
      nb = nb - 1
      IF ( nb<=0 ) GOTO 700
      ASSIGN 600 TO tra
      GOTO 900
   ENDIF
 600  ASSIGN 300 TO tra
   IF ( nopack ) ASSIGN 400 TO tra
 700  IF ( eof==0 ) THEN
      pltx = 0
   ELSE
      ENDFILE ptape
   ENDIF
   nb = 1
   GOTO 99999
!
 800  WRITE (Nout,99003) pltx , Pltape
99003 FORMAT ('0*** SYSTEM FATAL ERROR FROM SGINO. ',A4,' FILE OR ',A4,' FILE GOT LOST')
   CALL errtrc(name)
!
!
   ENTRY seof(Pltape)
!     ===================
!
   eof = 1
   GOTO 500
!
 900  WRITE (ptape,format) (lbuf(noff+j),j=1,nb)
   nb = 1
   word = 0
   shift = nbits
   GOTO tra
!
99999 END SUBROUTINE sgino
