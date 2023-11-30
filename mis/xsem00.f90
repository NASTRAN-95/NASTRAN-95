
SUBROUTINE xsem00
   IMPLICIT NONE
   INTEGER Databf(1) , Fist(2) , Fstrst , Inoscr(200) , Isperlnk , Itmbgn , Linknm(15) , Linkno , Lxlink , Mach , Mask , Mask2 ,    &
         & Mask3 , Maxlnk , Mxlink(1) , Nbpc , Nbpw , Ncpw , Nin , Nmsg , Nout , Param(100) , Plotf , Sysbuf , Vps(1)
   REAL Xx(20) , Xxx(16) , Xxxx(53)
   COMMON /blank / Param
   COMMON /machin/ Mach
   COMMON /msgx  / Nmsg
   COMMON /oscent/ Inoscr
   COMMON /sem   / Mask , Mask2 , Mask3 , Linknm
   COMMON /system/ Sysbuf , Xx , Linkno , Xxx , Nbpc , Nbpw , Ncpw , Xxxx , Isperlnk
   COMMON /xfist / Fist
   COMMON /xlink / Lxlink , Maxlnk , Mxlink
   COMMON /xpfist/ Fstrst
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Databf
   INTEGER andf , khrfn1 , korsz , lshift , rshift
   INTEGER equiv(2) , errflg , error_id , exit , fistnm , i , iblnk , ibuf1 , idin , ierr , ifile , ii , iplot , j , j1 , j2 , j3 , &
         & j5 , kode , kscr , ktime , l , ldiag , ll , mm , modno , modx , numbr(10) , opntr , parml , parmn , pool , purge(2) ,    &
         & scrtch(3) , subnam(2) , typecd , vparml , vpsx , wordb(4) , worde(2) , xequ , xpur , xsav , ychk
   CHARACTER*80 ft05 , ft06 , infile , output , proj
   LOGICAL lvax
   CHARACTER*4 wordc
! **********************************************************************
! THE PURPOSE OF THIS ROUTINE IS TO EXECUTE THE PREFACE AND THEN TO
! EXECUTE MODULES ACCORDING TO THE DMAP.  THE DMAP IS READ FROM THE
! OSCAR.  FOR EACH MODULE TO BE EXECUTED, THE FIST AND XVPS ARE SETUP.
!
!WKBD 5/95
!     INTEGER ORF
!WKBI 5/95
!
!
!
!
   EQUIVALENCE (Xx(1),Nout) , (Xx(3),Nin)
                              ! input file number
   EQUIVALENCE (Xx(19),Plotf)
   EQUIVALENCE (Xx(17),Itmbgn)
!WKBI 5/95
   EQUIVALENCE (wordc,wordb)
!
   DATA pool/4HPOOL/ , scrtch/4HSCRA , 4HTCH0 , 4HTCH0/ , numbr/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/ ,        &
       &wordb/4HSEM1 , 4HBEGN , 4H     , 4H    / , worde/4HBEGN , 4HEND / , iblnk/4H    / , modx/215/ , exit/4HEXIT/
   DATA subnam/4HXSEM , 2H00/
   DATA equiv , purge/4HEQUI , 4HV    , 4HPURG , 4HE   /
   DATA xequ , xpur/4HXEQU , 4HXPUR/
   DATA xsav , ychk/4HXSAV , 4HXCHK/
!     Set varables
!*****
! INITIALIZE MACHINE DEPENDENT CONSTANTS
   CALL btstrp
!----------------------------------------------------------------------
! hgs 12/06/2104 - The NASA delivery uses redirected input and out put
!                  so these files are not explicitly opened. I need
!                  to change the stdin and stdout to allow the use of
!                  GDB in the script file. Therefore the following mod-
!                  ifications explicitly open the stdin and stdout files
!                  using the FTN5 and FTN6 ENV set by the script.
!
!
!     open inout and output files
!
   CALL getenv('PROJ',proj)
   CALL getenv('FT05',ft05)
   CALL getenv('FT06',ft06)
   output = trim(proj)//'/'//trim(ft06)
   error_id = 0
   ifile = Nout
   OPEN (Nout,FILE=output,FORM='formatted',STATUS='unknown',IOSTAT=ierr,ERR=100)
   ifile = Nin
   infile = trim(proj)//'/'//trim(ft05)
   OPEN (Nin,FILE=infile,FORM='formatted',STATUS='unknown',IOSTAT=ierr,ERR=200)
!
   lvax = Mach==5
!*****
! EXECUTE PREFACE
!*****
   kscr = lshift(1,Nbpw-4*Nbpc)
   CALL tdate(Xx(14))
   CALL conmsg(wordb,2,1)
   CALL semint(0)
   Isperlnk = 1
   wordb(2) = worde(2)
   CALL conmsg(wordb,2,1)
   iplot = Plotf
   IF ( Plotf<0 ) Plotf = 1
   ibuf1 = korsz(Databf) - Sysbuf
   GOTO 500
 100  error_id = -2
   GOTO 300
 200  error_id = -1
!
!     open error
!
 300  WRITE (Nout,*) 'Error in opening file =' , ifile , ' IOSTAT = ' , ierr
   SELECT CASE (error_id)
   CASE (-1)
      WRITE (Nout,'(a)') 'File name: ' , infile
   CASE (-2)
      WRITE (Nout,'(a)') 'File name: ' , output
   END SELECT
   CALL pexit    ! close down and exit with ierror
   RETURN
!*****
! RETURN HERE AFTER MODULE HAS EXECUTED
!*****
 400  IF ( Inoscr(4)/=xsav .AND. Inoscr(4)/=ychk ) THEN
      wordb(4) = worde(2)
!      CALL CONMSG(WORDB,4,0)
      CALL conmsg(wordb,4,222222)
   ENDIF
 500  IF ( Nmsg>0 ) CALL msgwrt
   CALL open(*1400,pool,Databf(ibuf1),2)
!*****
! READ THE OSCAR ENTRY
!*****
 600  CALL read(*1500,*700,pool,Inoscr,200,1,errflg)
!
! OSCAR RECORD TOO LARGE FOR /OSCENT/
   kode = 290
   GOTO 1700
 700  IF ( Inoscr(6)>=0 ) GOTO 600
!*****
! TRY AGAIN IF EXECUTE FLAG IS OFF
!*****
   CALL close(pool,2)
   typecd = andf(Inoscr(3),Mask)
!*****
! NOW DETERMINE TYPE OF OSCAR FORMAT
!*****
   IF ( typecd>2 ) GOTO 1200
!*****
!*****
! NOW PROCESSING TYPE O AND F
!*****
 800  modno = Inoscr(2)
   Fist(2) = Fstrst
   opntr = 7
   ASSIGN 1000 TO mm
   fistnm = 101
!*****
! PROCESS FILES IN OSCAR ENTRY.
!*****
 900  j = Inoscr(opntr)
   opntr = opntr + 1
   IF ( j/=0 ) THEN
      DO i = 1 , j
         CALL gnfist(Inoscr(opntr),fistnm,modno)
         IF ( modno<0 ) GOTO 800
         IF ( modno==0 ) GOTO 1300
         opntr = opntr + 3
         fistnm = fistnm + 1
      ENDDO
   ENDIF
   GOTO mm
!*****
! SETUP TO PROCESS OUTPUT FILES
!*****
 1000 IF ( typecd/=2 ) THEN
      ASSIGN 1100 TO mm
      fistnm = 201
      GOTO 900
   ENDIF
!*****
! PROCESS SCRATCH FILES
!*****
 1100 j1 = Inoscr(opntr)
   IF ( j1/=0 ) THEN
      fistnm = 301
      scrtch(2) = scrtch(3)
      ll = 1
      l = 0
      DO j = 1 , j1
         l = l + 1
         IF ( l==10 ) scrtch(2) = khrfn1(scrtch(2),3,numbr(ll),1)
         scrtch(2) = khrfn1(scrtch(2),4,numbr(l),1)
         CALL gnfist(scrtch,fistnm,modno)
         IF ( l==10 ) THEN
            l = 0
            ll = ll + 1
         ENDIF
         IF ( modno<0 ) GOTO 800
         IF ( modno==0 ) GOTO 1300
         fistnm = fistnm + 1
      ENDDO
   ENDIF
   opntr = opntr + 1
!*****
! NOW PROCESS PARAMETER LIST IN OSCAR
!  PARMN = NO. OF PARAMETERS TO PROCESS
!*****
   parmn = Inoscr(opntr)
   IF ( parmn/=0 ) THEN
      ii = 1
      opntr = opntr + 1
      DO j2 = 1 , parmn
         IF ( Inoscr(opntr)<0 ) THEN
!*****
! MOVE VARIABLE INTO COMMON VIA VPS TABLE
!*****
            vpsx = andf(Inoscr(opntr),Mask3)
            opntr = opntr + 1
            vparml = Vps(vpsx-1)
            DO j5 = 1 , vparml
               Param(ii) = Vps(vpsx)
               ii = ii + 1
               vpsx = vpsx + 1
            ENDDO
         ELSE
!*****
! NOW PROCESS CONSTANT PARAMETER
!*****
            parml = Inoscr(opntr)
            opntr = opntr + 1
            DO j3 = 1 , parml
               Param(ii) = Inoscr(opntr)
               ii = ii + 1
               opntr = opntr + 1
            ENDDO
         ENDIF
      ENDDO
   ENDIF
 1200 modx = rshift(Inoscr(3),16)
!*****
! MODULE IS IN THIS LINK
! PRINT TIME MODULE BEGAN EXECUTION IF FUNCTIONAL MODULE
!*****
   wordb(2) = Inoscr(4)
   wordb(3) = Inoscr(5)
   IF ( Inoscr(4)==xequ .OR. Inoscr(4)==xpur ) THEN
      IF ( Inoscr(4)/=xequ ) THEN
         wordb(2) = purge(1)
         wordb(3) = purge(2)
      ELSE
         wordb(2) = equiv(1)
         wordb(3) = equiv(2)
      ENDIF
   ENDIF
   CALL tmtogo(ktime)
   IF ( ktime<=0 .AND. wordb(2)/=exit ) CALL mesage(-50,0,wordb(2))
   IF ( Inoscr(4)/=xsav .AND. Inoscr(4)/=ychk ) THEN
      wordb(1) = iblnk
      wordb(4) = worde(1)
!
!     EXTRACT DMAP SEQUENCE NUMBER
!
      idin = andf(Inoscr(6),Mask)
!WKBIB 5/95
      WRITE (wordc,99001) idin
99001 FORMAT (I4)
!WKBIE 5/95
!WKBDB 5/95
!      DO 251  I =1,4
!      ICHR  = IDIN -(IDIN/10)*10 +1
!      L = NBPW-NBPC
!      IF (.NOT.LVAX)  WORDB(1) =
!     *    ORF(RSHIFT(WORDB(1),NBPC),LSHIFT(RSHIFT(NUMBR(ICHR),L),L))
!      IF (LVAX)  WORDB(1)=KHRFN1(WORDB(1),5-I,NUMBR(ICHR),1)
!      IDIN = IDIN/10
!      IF(IDIN .EQ. 0)  GO TO 252
!  251 CONTINUE
!WKBDE 5/95
!      CALL CONMSG(WORDB,4,0)
      CALL conmsg(wordb,4,111111)
   ENDIF
   GOTO 1800
!*****
!                   E R R O R   M E S S A G E S
!*****
! MODULE REQUIREMENTS EXCEED AVAILABLE FILES
 1300 Inoscr(6) = andf(Inoscr(6),Mask)
   CALL mesage(-18,Inoscr(6),Inoscr(4))
!
! UNEXPECTED ALTERNATE RETURN TAKEN WHILE ATTEMPTING TO OPEN POOL TAPE.
 1400 kode = 270
   GOTO 1700
!
! OSCAR FILE POSITIONED INCORRECTLY - HIT EOF.
 1500 kode = 280
   GOTO 1700
!
! LINK SPECIFICATIONS INCORRECT FOR THIS MODULE.
 1600 WRITE (Nout,99002) wordb , modx
99002 FORMAT (/1X,4A4,I9)
   kode = 940
!
!
 1700 WRITE (Nout,99003) kode
99003 FORMAT (64H0*** SYSTEM FATAL MESSAGE 1006, LINK DRIVER LOGIC ERROR - CODE =,I4)
   CALL mesage(-37,0,subnam)
!**********************************************************************
! EXECUTE MODULE
 1800 CALL sswtch(2,ldiag)
!     IF ( LDIAG .NE. 0 .AND. MODX .GT. 14 ) CALL DBMDIA
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 .OR. modx==2 .OR. modx==4 ) GOTO 1600
      IF ( modx==3 ) THEN
         CALL xchk
      ELSEIF ( modx==5 ) THEN
         CALL xcei
      ELSEIF ( modx==6 ) THEN
         CALL xcei
      ELSEIF ( modx==7 ) THEN
         CALL xcei
      ELSEIF ( modx==8 ) THEN
         CALL xsave
      ELSEIF ( modx==9 ) THEN
         CALL xpurge
      ELSEIF ( modx==10 ) THEN
         CALL xequiv
      ELSE
         GOTO 1900
      ENDIF
      GOTO 400
   ENDIF
 1900 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL xcei
      ELSEIF ( modx==2 ) THEN
         CALL xcei
      ELSEIF ( modx==3 ) THEN
         CALL xcei
      ELSEIF ( modx==4 ) THEN
         CALL dadd
      ELSEIF ( modx==5 ) THEN
         CALL dadd5
      ELSEIF ( modx==6 ) THEN
         CALL amg
      ELSEIF ( modx==7 ) THEN
         CALL amp
      ELSEIF ( modx==8 ) THEN
         CALL apd
      ELSEIF ( modx==9 ) THEN
         CALL bmg
      ELSEIF ( modx==10 ) THEN
         CALL case
      ELSE
         GOTO 2000
      ENDIF
      GOTO 400
   ENDIF
 2000 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL cyct1
      ELSEIF ( modx==2 ) THEN
         CALL cyct2
      ELSEIF ( modx==3 ) THEN
         CALL cead
      ELSEIF ( modx==4 ) THEN
         CALL curv
      ELSEIF ( modx==5 ) THEN
      ELSEIF ( modx==6 ) THEN
         CALL ddr
      ELSEIF ( modx==7 ) THEN
         CALL ddr1
      ELSEIF ( modx==8 ) THEN
         CALL ddr2
      ELSEIF ( modx==9 ) THEN
         CALL ddrmm
      ELSEIF ( modx==10 ) THEN
         CALL ddcomp
      ELSE
         GOTO 2100
      ENDIF
      GOTO 400
   ENDIF
 2100 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL diagon
      ELSEIF ( modx==2 ) THEN
         CALL dpd
      ELSEIF ( modx==3 ) THEN
         CALL dschk
      ELSEIF ( modx==4 ) THEN
         CALL dsmg1
      ELSEIF ( modx==5 ) THEN
         CALL dsmg2
      ELSEIF ( modx==6 ) THEN
      ELSEIF ( modx==7 ) THEN
         CALL dumod1
      ELSEIF ( modx==8 ) THEN
         CALL dumod2
      ELSEIF ( modx==9 ) THEN
         CALL dumod3
      ELSEIF ( modx==10 ) THEN
         CALL dumod4
      ELSE
         GOTO 2200
      ENDIF
      GOTO 400
   ENDIF
 2200 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
      ELSEIF ( modx==2 ) THEN
         CALL ema1
      ELSEIF ( modx==3 ) THEN
!         SET LINKNO TO FLAG SUBROUTINE SMA1B TO CALL EMG1B
         Linkno = Linknm(8)
         CALL emg
         Linkno = Linknm(1)
      ELSEIF ( modx==4 ) THEN
         CALL fa1
      ELSEIF ( modx==5 ) THEN
         CALL fa2
      ELSEIF ( modx==6 ) THEN
         CALL dfbs
      ELSEIF ( modx==7 ) THEN
         CALL frlg
      ELSEIF ( modx==8 ) THEN
         CALL frrd
      ELSEIF ( modx==9 ) THEN
      ELSEIF ( modx==10 ) THEN
         CALL gi
      ELSE
         GOTO 2300
      ENDIF
      GOTO 400
   ENDIF
 2300 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL gkad
      ELSEIF ( modx==2 ) THEN
         CALL gkam
      ELSEIF ( modx==3 ) THEN
         CALL gp1
      ELSEIF ( modx==4 ) THEN
         CALL gp2
      ELSEIF ( modx==5 ) THEN
         CALL gp3
      ELSEIF ( modx==6 ) THEN
         CALL gp4
      ELSEIF ( modx==7 ) THEN
         CALL gpcyc
      ELSEIF ( modx==8 ) THEN
         CALL gpfdr
      ELSEIF ( modx==9 ) THEN
         CALL dumod5
      ELSEIF ( modx==10 ) THEN
         CALL gpwg
      ELSE
         GOTO 2400
      ENDIF
      GOTO 400
   ENDIF
 2400 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
      ELSEIF ( modx==2 ) THEN
         CALL input
      ELSEIF ( modx==3 ) THEN
         CALL inptt1
      ELSEIF ( modx==4 ) THEN
         CALL inptt2
      ELSEIF ( modx==5 ) THEN
         CALL inptt3
      ELSEIF ( modx==6 ) THEN
         CALL inptt4
      ELSEIF ( modx==7 ) THEN
         CALL matgen
      ELSEIF ( modx==8 ) THEN
         CALL matgpr
      ELSEIF ( modx==9 ) THEN
         CALL matprn
      ELSEIF ( modx==10 ) THEN
         CALL prtint
      ELSE
         GOTO 2500
      ENDIF
      GOTO 400
   ENDIF
 2500 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL mce1
      ELSEIF ( modx==2 ) THEN
         CALL mce2
      ELSEIF ( modx==3 ) THEN
         CALL merge1
      ELSEIF ( modx==4 ) THEN
      ELSEIF ( modx==5 ) THEN
         CALL moda
      ELSEIF ( modx==6 ) THEN
         CALL modacc
      ELSEIF ( modx==7 ) THEN
         CALL modb
      ELSEIF ( modx==8 ) THEN
         CALL modc
      ELSEIF ( modx==9 ) THEN
         CALL dmpyad
      ELSEIF ( modx==10 ) THEN
         CALL mtrxin
      ELSE
         GOTO 2600
      ENDIF
      GOTO 400
   ENDIF
 2600 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL ofp
      ELSEIF ( modx==2 ) THEN
         CALL optpr1
      ELSEIF ( modx==3 ) THEN
         CALL optpr2
      ELSEIF ( modx==4 ) THEN
      ELSEIF ( modx==5 ) THEN
         CALL outpt
      ELSEIF ( modx==6 ) THEN
         CALL outpt1
      ELSEIF ( modx==7 ) THEN
         CALL outpt2
      ELSEIF ( modx==8 ) THEN
         CALL outpt3
      ELSEIF ( modx==9 ) THEN
         CALL outpt4
      ELSEIF ( modx==10 ) THEN
         CALL qparam
      ELSE
         GOTO 2700
      ENDIF
      GOTO 400
   ENDIF
 2700 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL paraml
      ELSEIF ( modx==2 ) THEN
         CALL qparmr
      ELSEIF ( modx==3 ) THEN
         CALL partn1
      ELSEIF ( modx==4 ) THEN
      ELSEIF ( modx==5 ) THEN
         CALL mred1
      ELSEIF ( modx==6 ) THEN
         CALL mred2
      ELSEIF ( modx==7 ) THEN
         CALL cmrd2
      ELSEIF ( modx==8 ) THEN
         CALL pla1
      ELSEIF ( modx==9 ) THEN
         CALL pla2
      ELSEIF ( modx==10 ) THEN
         CALL pla3
      ELSE
         GOTO 2800
      ENDIF
      GOTO 400
   ENDIF
 2800 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL pla4
      ELSEIF ( modx==2 ) THEN
      ELSEIF ( modx==3 ) THEN
         CALL dplot
      ELSEIF ( modx==4 ) THEN
         CALL dpltst
      ELSEIF ( modx==5 ) THEN
         CALL plttra
      ELSEIF ( modx==6 ) THEN
         CALL prtmsg
      ELSEIF ( modx==7 ) THEN
         CALL prtprm
      ELSEIF ( modx==8 ) THEN
         CALL random
      ELSEIF ( modx==9 ) THEN
         CALL rbmg1
      ELSEIF ( modx==10 ) THEN
         CALL rbmg2
      ELSE
         GOTO 2900
      ENDIF
      GOTO 400
   ENDIF
 2900 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL rbmg3
      ELSEIF ( modx==2 ) THEN
         CALL rbmg4
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL reig
      ELSEIF ( modx==5 ) THEN
         CALL rmg
      ELSEIF ( modx==6 ) THEN
         CALL scalar
      ELSEIF ( modx==7 ) THEN
         CALL sce1
      ELSEIF ( modx==8 ) THEN
         CALL sdr1
      ELSEIF ( modx==9 ) THEN
         CALL sdr2
      ELSEIF ( modx==10 ) THEN
         CALL sdr3
      ELSE
         GOTO 3000
      ENDIF
      GOTO 400
   ENDIF
 3000 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL sdrht
      ELSEIF ( modx==2 ) THEN
         CALL seemat
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL setval
      ELSEIF ( modx==5 ) THEN
         CALL sma1
      ELSEIF ( modx==6 ) THEN
         CALL sma2
      ELSEIF ( modx==7 ) THEN
         CALL sma3
      ELSEIF ( modx==8 ) THEN
         CALL smp1
      ELSEIF ( modx==9 ) THEN
         CALL smp2
      ELSEIF ( modx==10 ) THEN
         CALL smpyad
      ELSE
         GOTO 3100
      ENDIF
      GOTO 400
   ENDIF
 3100 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL solve
      ELSEIF ( modx==2 ) THEN
      ELSEIF ( modx==3 ) THEN
         CALL ssg1
      ELSEIF ( modx==4 ) THEN
         CALL ssg2
      ELSEIF ( modx==5 ) THEN
         CALL ssg3
      ELSEIF ( modx==6 ) THEN
         CALL ssg4
      ELSEIF ( modx==7 ) THEN
         CALL ssght
      ELSEIF ( modx==8 ) THEN
         CALL ta1
      ELSEIF ( modx==9 ) THEN
         CALL tabpch
      ELSEIF ( modx/=10 ) THEN
         GOTO 3200
      ENDIF
      GOTO 400
   ENDIF
 3200 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL tabfmt
      ELSEIF ( modx==2 ) THEN
         CALL tabpt
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL timtst
      ELSEIF ( modx==5 ) THEN
         CALL trd
      ELSEIF ( modx==6 ) THEN
         CALL trht
      ELSEIF ( modx==7 ) THEN
         CALL trlg
      ELSEIF ( modx==8 ) THEN
         CALL dtranp
      ELSEIF ( modx==9 ) THEN
         CALL dumerg
      ELSEIF ( modx==10 ) THEN
         CALL dupart
      ELSE
         GOTO 3300
      ENDIF
      GOTO 400
   ENDIF
 3300 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL vdr
      ELSEIF ( modx==2 ) THEN
         CALL vec
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL xyplot
      ELSEIF ( modx==5 ) THEN
         CALL xyprpt
      ELSEIF ( modx==6 ) THEN
         CALL xytran
      ELSEIF ( modx==7 ) THEN
      ELSEIF ( modx==8 ) THEN
         CALL comb1
      ELSEIF ( modx==9 ) THEN
         CALL comb2
      ELSEIF ( modx==10 ) THEN
         CALL exio
      ELSE
         GOTO 3400
      ENDIF
      GOTO 400
   ENDIF
 3400 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL rcovr
      ELSEIF ( modx==2 ) THEN
         CALL emfld
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL rcovr3
      ELSEIF ( modx==5 ) THEN
         CALL reduce
      ELSEIF ( modx==6 ) THEN
         CALL sgen
      ELSEIF ( modx==7 ) THEN
         CALL sofi
      ELSEIF ( modx==8 ) THEN
         CALL sofo
      ELSEIF ( modx==9 ) THEN
         CALL sofut
      ELSEIF ( modx==10 ) THEN
         CALL subph1
      ELSE
         GOTO 3500
      ENDIF
      GOTO 400
   ENDIF
 3500 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL pltmrg
      ELSEIF ( modx==2 ) THEN
      ELSEIF ( modx==3 ) THEN
         CALL copy
      ELSEIF ( modx==4 ) THEN
         CALL switch
      ELSEIF ( modx==5 ) THEN
         CALL mpy3
      ELSEIF ( modx==6 ) THEN
         CALL ddcmps
      ELSEIF ( modx==7 ) THEN
         CALL lodapp
      ELSEIF ( modx==8 ) THEN
         CALL gpstgn
      ELSEIF ( modx==9 ) THEN
         CALL eqmck
      ELSEIF ( modx==10 ) THEN
         CALL adr
      ELSE
         GOTO 3600
      ENDIF
      GOTO 400
   ENDIF
 3600 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL frrd2
      ELSEIF ( modx==2 ) THEN
         CALL gust
      ELSEIF ( modx==3 ) THEN
         CALL ift
      ELSEIF ( modx==4 ) THEN
         CALL lamx
      ELSEIF ( modx==5 ) THEN
         CALL ema
      ELSEIF ( modx==6 ) THEN
         CALL anisop
      ELSEIF ( modx==7 ) THEN
      ELSEIF ( modx==8 ) THEN
         CALL gencos
      ELSEIF ( modx==9 ) THEN
         CALL ddamat
      ELSEIF ( modx==10 ) THEN
         CALL ddampg
      ELSE
         GOTO 3700
      ENDIF
      GOTO 400
   ENDIF
 3700 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL nrlsum
      ELSEIF ( modx==2 ) THEN
         CALL genpar
      ELSEIF ( modx==3 ) THEN
         CALL casege
      ELSEIF ( modx==4 ) THEN
         CALL desvel
      ELSEIF ( modx==5 ) THEN
         CALL prolat
      ELSEIF ( modx==6 ) THEN
         CALL magbdy
      ELSEIF ( modx==7 ) THEN
         CALL comugv
      ELSEIF ( modx==8 ) THEN
         CALL flbmg
      ELSEIF ( modx==9 ) THEN
         CALL gfsma
      ELSEIF ( modx==10 ) THEN
         CALL trail
      ELSE
         GOTO 3800
      ENDIF
      GOTO 400
   ENDIF
 3800 modx = modx - 10
   IF ( modx>=1 .AND. modx<=10 ) THEN
      IF ( modx==1 ) THEN
         CALL scan
      ELSEIF ( modx==2 ) THEN
      ELSEIF ( modx==3 ) THEN
         CALL pthbdy
      ELSEIF ( modx==4 ) THEN
         CALL varian
      ELSEIF ( modx==5 ) THEN
         CALL fvrst1
      ELSEIF ( modx==6 ) THEN
         CALL fvrst2
      ELSEIF ( modx==7 ) THEN
         CALL alg
      ELSEIF ( modx==8 ) THEN
         CALL apdb
      ELSEIF ( modx==9 ) THEN
         CALL prompt
      ELSEIF ( modx==10 ) THEN
         CALL olplot
      ELSE
         GOTO 3900
      ENDIF
      GOTO 400
   ENDIF
 3900 modx = modx - 10
   IF ( modx>=1 .AND. modx<=7 ) THEN
      IF ( modx==1 ) THEN
         CALL inptt5
      ELSEIF ( modx==2 ) THEN
         CALL outpt5
      ELSEIF ( modx==3 ) THEN
      ELSEIF ( modx==4 ) THEN
         CALL qparmd
      ELSEIF ( modx==5 ) THEN
         CALL ginofl
      ELSEIF ( modx==6 ) THEN
         CALL dbase
      ELSEIF ( modx==7 ) THEN
         CALL normal
      ELSE
         GOTO 1600
      ENDIF
      GOTO 400
   ENDIF
   GOTO 1600
END SUBROUTINE xsem00
