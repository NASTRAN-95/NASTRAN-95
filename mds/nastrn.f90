
PROGRAM nastrn
   IMPLICIT NONE
   INCLUDE 'NASNAMES.COM'
!
! COMMON variable declarations
!
   INTEGER Ibasbf , Idbadr , Idbbas , Idbdir , Idbfre , Idblen , Ifilex , Indbas , Indcbp , Indclr , Inddir , Iocode , Irdict ,     &
         & Iropen , Isystm(94) , Iz(400000000) , Lastad , Lenalc , Lenopc , Lout , Maxalc , Maxblk , Maxdsk , Name , Nblock ,       &
         & Numcls , Numopn , Numrea , Numwri , Sperlk
   CHARACTER*80 Sdsn(10)
   REAL Systm(94)
   COMMON /dbm   / Idbbas , Idbfre , Idbdir , Indbas , Indclr , Indcbp , Nblock , Lenalc , Iocode , Ifilex , Name , Maxalc ,        &
                 & Maxblk , Maxdsk , Idblen , Idbadr , Ibasbf , Inddir , Numopn , Numcls , Numwri , Numrea , Lenopc
   COMMON /logout/ Lout
   COMMON /lstadd/ Lastad
   COMMON /resdic/ Irdict , Iropen
   COMMON /sofdsn/ Sdsn
   COMMON /system/ Isystm , Sperlk
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER i , iocmem , len
   INTEGER locfx
   CHARACTER*5 tmp
   CHARACTER*80 value
!
! End of declarations
!
!
   EQUIVALENCE (Isystm,Systm)
   Lenopc = 400000000
!
!     SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/
!
   Isystm(18) = 0
   CALL second(Systm(18))
   CALL waltim(Isystm(32))
!
!     EXECUTE NASTRAN SUPER LINK
!
   len = 80
   value = ' '
   CALL btstrp
   CALL getenv('DBMEM',value)
   READ (value,*) Idblen
   CALL getenv('OCMEM',value)
   READ (value,*) iocmem
   IF ( iocmem>Lenopc ) THEN
      PRINT * , ' LARGEST VALUE FOR OPEN CORE ALLOWED IS:' , Lenopc
      CALL mesage(-61,0,0)
   ENDIF
   IF ( Idblen/=0 ) Idblen = Lenopc - iocmem
   Lastad = locfx(Iz(iocmem))
   IF ( Idblen/=0 ) Idbadr = locfx(Iz(iocmem+1))
   Lenopc = iocmem
   CALL dbmint
   Lout = 3
   Irdict = 4
   Sperlk = 1
   Isystm(11) = 1
   value = ' '
   CALL getenv('RFDIR',Rfdir)
   value = ' '
   CALL getenv('DIRCTY',Dirtry)
   len = index(Dirtry,' ') - 1
   DO i = 1 , 90
      IF ( i<=9 ) WRITE (tmp,99001) i
99001 FORMAT ('scr',I1)
      IF ( i>9 ) WRITE (tmp,99002) i
99002 FORMAT ('scr',I2)
      Dsnames(i) = Dirtry(1:len)//'/'//tmp
   ENDDO
   CALL getenv('LOGNM',Log)
   Dsnames(3) = Log
   CALL getenv('OPTPNM',Optp)
   Dsnames(7) = Optp
   CALL getenv('NPTPNM',Nptp)
   Dsnames(8) = Nptp
   CALL getenv('FTN11',Out11)
   Dsnames(11) = Out11
   CALL getenv('FTN12',In12)
   Dsnames(12) = In12
   CALL getenv('FTN13',value)
   Dsnames(13) = value
   CALL getenv('FTN14',value)
   Dsnames(14) = value
   CALL getenv('FTN15',value)
   Dsnames(15) = value
   CALL getenv('FTN16',value)
   Dsnames(16) = value
   CALL getenv('FTN17',value)
   Dsnames(17) = value
   CALL getenv('FTN18',value)
   Dsnames(18) = value
   CALL getenv('FTN19',value)
   Dsnames(19) = value
   CALL getenv('FTN20',value)
   Dsnames(20) = value
   CALL getenv('FTN21',value)
   Dsnames(21) = value
   CALL getenv('PLTNM',Plot)
   Dsnames(10) = Plot
   CALL getenv('DICTNM',Dic)
   Dsnames(4) = Dic
   CALL getenv('PUNCHNM',Punch)
   Dsnames(1) = Punch
   CALL getenv('SOF1',value)
   Sdsn(1) = value
   CALL getenv('SOF2',value)
   Sdsn(2) = value
   CALL getenv('SOF3',value)
   Sdsn(3) = value
   CALL getenv('SOF4',value)
   Sdsn(4) = value
   CALL getenv('SOF5',value)
   Sdsn(5) = value
   CALL getenv('SOF6',value)
   Sdsn(6) = value
   CALL getenv('SOF7',value)
   Sdsn(7) = value
   CALL getenv('SOF8',value)
   Sdsn(8) = value
   CALL getenv('SOF9',value)
   Sdsn(9) = value
   CALL getenv('SOF10',value)
   Sdsn(10) = value
   OPEN (3,FILE=Dsnames(3),STATUS='UNKNOWN')
   IF ( Dsnames(11)/='none' ) OPEN (11,FILE=Dsnames(11),STATUS='UNKNOWN')
   IF ( Dsnames(12)/='none' ) OPEN (12,FILE=Dsnames(12),STATUS='UNKNOWN')
   IF ( Dsnames(10)/='none' ) OPEN (10,FILE=Dsnames(10),STATUS='UNKNOWN')
   IF ( Dsnames(4)/='none' ) OPEN (4,FILE=Dsnames(4),STATUS='UNKNOWN')
   IF ( Dsnames(1)/='none' ) OPEN (1,FILE=Dsnames(1),STATUS='UNKNOWN')
   CALL xsem00
END PROGRAM nastrn
