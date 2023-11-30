
SUBROUTINE rfopen(Member,Lu)
   IMPLICIT NONE
   INTEGER Ibuf , In , Mach , Nogo , Nout
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /machin/ Mach
   COMMON /system/ Ibuf , Nout , Nogo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xxread/ In
   INTEGER Lu
   INTEGER Member(2)
   CHARACTER*8 add(3) , free8 , mb8
   CHARACTER*1 bk , mb1(8)
   CHARACTER*44 dsn , rfdir
   INTEGER facsf
   INTEGER i , j , lenr
   CHARACTER*5 mb5
   CHARACTER*6 mb6
!
!     THIS .MIS ROUTINE OPENS THE RIGID FORMAT FILE, AS AN ORDINARY
!     FORTRAN FILE. USE REGULAR FORTRAN READ TO READ THE FILE
!
!     ENTRY POINT RFCLSE TO CLOSE IT
!
!     IF RIGID FORMAT FILE OPENS OK, LU IS THE FORTRAN UNIT NUMBER
!     OTHERWISE, LU = 0
!
!     THIS ROUTINE REPLACES ALL THE MACHINE DEPENDENT DSXOPN, DSXCLS,
!     DSXREA, AND DSXFRE ROUTINES. PLUS DSXRDS, DSXIO, AND DSXSIO IN
!     IBM VERSION, AND DSXRET AND DSXZER IN CDC
!
!     NOTE - FORTRAN UNIT 'IN' IS USED TO READ THE RIGID FORMAT FILE.
!            UNIT 'IN' IS SYNCHRONOUS WITH ANY READFILE OR NESTED
!            READFILE OPERATION.
!
!     WRITTEN BY G.CHAN/UNISYS.   10/1990
!
!WKBI
   EQUIVALENCE (mb1(1),mb5,mb6,mb8)
   DATA bk , add(1) , add(3) , free8/' ' , '@ADD,E ' , ' .  ' , '@FREE   '/
!
   CALL a42k8(Member(1),Member(2),mb8)
   IF ( Mach/=3 ) THEN
      In = In + 1
      IF ( In<60 ) In = 60
      j = 5
      IF ( mb1(6)/=bk ) j = 6
!
!           DUMMY  IBM  UNVC  CDC  VAX  ULTRIX  SUN   AIX   HP
!             S/G  MAC  CRAY CNVX  NEC  FUJTSU   DG  AMDL PRIME
!             486 DUMMY ALFA RESV
!            ---- ----  ---- ---- ----  ------ ----  ---- -----
      IF ( Mach==1 .OR. Mach==20 ) GOTO 200
      IF ( Mach==2 .OR. Mach==4 .OR. Mach==5 .OR. Mach==6 .OR. Mach==7 .OR. Mach==8 .OR. Mach==9 .OR. Mach==10 .OR. Mach==19 .OR.   &
         & Mach==21 ) THEN
         rfdir = ' '
         CALL getenv('RFDIR',rfdir)
         DO i = 44 , 1 , -1
            IF ( rfdir(i:i)/=' ' ) THEN
               lenr = i
               GOTO 20
            ENDIF
         ENDDO
         lenr = 44
 20      dsn = ' '
         dsn = rfdir(1:lenr)//'/'//mb6
!WKBR IF (J .EQ. 6) OPEN (UNIT=IN,FILE=MB6,ACCESS='SEQUENTIAL',ERR=100,
!
!     OTHERS -
!
         OPEN (UNIT=In,FILE=dsn,ACCESS='SEQUENTIAL',ERR=200,FORM='FORMATTED',STATUS='OLD')
      ELSEIF ( Mach==11 .OR. Mach==12 .OR. Mach==13 .OR. Mach==14 .OR. Mach==15 .OR. Mach==16 .OR. Mach==17 .OR. Mach==18 .OR.      &
             & Mach==22 ) THEN
!
         OPEN (UNIT=In,FILE=mb8,ACCESS='SEQUENTIAL',ERR=200,STATUS='OLD',FORM='FORMATTED')
      ELSE
         GOTO 100
      ENDIF
!
!     VERIFY FILE EXISTANCE
!
      READ (In,99001,ERR=200,END=200) j
99001 FORMAT (A1)
      REWIND In
      Lu = In
      GOTO 300
   ENDIF
!
!     UNIVAC ONLY -
!     ADD FILE TO INPUT STREAM
!
 100  add(2) = mb8
   j = facsf(add)
   Lu = 5
   GOTO 300
!
!WKBR100  WRITE  (NOUT,110) SFM,MB8
 200  WRITE (Nout,99002) Sfm , dsn
!WKBR 110  FORMAT (A25,', RFOPEN CAN NOT OPEN ',A8)
99002 FORMAT (A25,', RFOPEN CAN NOT OPEN ',A44)
!
   IF ( Mach>7 .AND. Mach/=21 ) WRITE (Nout,99003) Mach
99003 FORMAT (5X,'MACHINE',I4,' IS NOT AVAILABLE/RFOPEN')
   Lu = 0
   Nogo = 1
!
 300  RETURN
!
!
   ENTRY rfclse(Lu)
!     =================
!
   IF ( Mach==3 ) THEN
!
      add(1) = free8
      j = facsf(add)
   ELSE
      IF ( Lu<60 ) WRITE (Nout,99004) Sfm , Lu
99004 FORMAT (A25,'. RFCLSE/RFOPEN ERROR.  LU =',I4)
      CLOSE (UNIT=Lu)
      In = In - 1
      IF ( In<60 ) In = 0
   ENDIF
   Lu = 0
END SUBROUTINE rfopen
