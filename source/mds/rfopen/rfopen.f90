!*==rfopen.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rfopen(Member,Lu)
   USE c_machin
   USE c_system
   USE c_xmssg
   USE c_xxread
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Member
   INTEGER :: Lu
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(8) , DIMENSION(3) , SAVE :: add
   CHARACTER(1) , SAVE :: bk
   CHARACTER(44) :: dsn , rfdir
   CHARACTER(8) , SAVE :: free8
   INTEGER :: i , j , lenr
   CHARACTER(1) , DIMENSION(8) :: mb1
   CHARACTER(5) :: mb5
   CHARACTER(6) :: mb6
   CHARACTER(8) :: mb8
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (mb1(1),mb5,mb6,mb8)
   DATA bk , add(1) , add(3) , free8/' ' , '@ADD,E ' , ' .  ' , '@FREE   '/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL a42k8(Member(1),Member(2),mb8)
         IF ( mach/=3 ) THEN
            in = in + 1
            IF ( in<60 ) in = 60
            j = 5
            IF ( mb1(6)/=bk ) j = 6
!
!           DUMMY  IBM  UNVC  CDC  VAX  ULTRIX  SUN   AIX   HP
!             S/G  MAC  CRAY CNVX  NEC  FUJTSU   DG  AMDL PRIME
!             486 DUMMY ALFA RESV
!            ---- ----  ---- ---- ----  ------ ----  ---- -----
            IF ( mach==1 .OR. mach==20 ) GOTO 20
            IF ( mach==2 .OR. mach==4 .OR. mach==5 .OR. mach==6 .OR. mach==7 .OR. mach==8 .OR. mach==9 .OR. mach==10 .OR.           &
               & mach==19 .OR. mach==21 ) THEN
               rfdir = ' '
               CALL getenv('RFDIR',rfdir)
               DO i = 44 , 1 , -1
                  IF ( rfdir(i:i)/=' ' ) THEN
                     lenr = i
                     GOTO 5
                  ENDIF
               ENDDO
               lenr = 44
 5             dsn = ' '
               dsn = rfdir(1:lenr)//'/'//mb6
!WKBR IF (J .EQ. 6) OPEN (UNIT=IN,FILE=MB6,ACCESS='SEQUENTIAL',ERR=100,
!
!     OTHERS -
!
               OPEN (UNIT=in,FILE=dsn,ACCESS='SEQUENTIAL',ERR=20,FORM='FORMATTED',STATUS='OLD')
            ELSEIF ( mach==11 .OR. mach==12 .OR. mach==13 .OR. mach==14 .OR. mach==15 .OR. mach==16 .OR. mach==17 .OR. mach==18 .OR.&
                   & mach==22 ) THEN
!
               OPEN (UNIT=in,FILE=mb8,ACCESS='SEQUENTIAL',ERR=20,STATUS='OLD',FORM='FORMATTED')
            ELSE
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     VERIFY FILE EXISTANCE
!
            READ (in,99001,ERR=20,END=20) j
99001       FORMAT (A1)
            REWIND in
            Lu = in
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     UNIVAC ONLY -
!     ADD FILE TO INPUT STREAM
!
         add(2) = mb8
         j = facsf(add)
         Lu = 5
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!WKBR100  WRITE  (NOUT,110) SFM,MB8
 20      WRITE (nout,99002) sfm , dsn
!WKBR 110  FORMAT (A25,', RFOPEN CAN NOT OPEN ',A8)
99002    FORMAT (A25,', RFOPEN CAN NOT OPEN ',A44)
!
         IF ( mach>7 .AND. mach/=21 ) WRITE (nout,99003) mach
99003    FORMAT (5X,'MACHINE',I4,' IS NOT AVAILABLE/RFOPEN')
         Lu = 0
         nogo = 1
         spag_nextblock_1 = 3
      CASE (3)
!
         RETURN
!
!
         ENTRY rfclse(Lu)
!     =================
!
         IF ( mach==3 ) THEN
!
            add(1) = free8
            j = facsf(add)
         ELSE
            IF ( Lu<60 ) WRITE (nout,99004) sfm , Lu
99004       FORMAT (A25,'. RFCLSE/RFOPEN ERROR.  LU =',I4)
            CLOSE (UNIT=Lu)
            in = in - 1
            IF ( in<60 ) in = 0
         ENDIF
         Lu = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rfopen
