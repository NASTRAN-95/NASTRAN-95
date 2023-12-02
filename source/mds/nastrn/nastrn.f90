!*==nastrn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
PROGRAM nastrn
   IMPLICIT NONE
   USE I_NASNAMES
   USE C_DBM
   USE C_LOGOUT
   USE C_LSTADD
   USE C_RESDIC
   USE C_SOFDSN
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iocmem , len
   REAL , DIMENSION(94) :: systm
   CHARACTER(5) :: tmp
   CHARACTER(80) :: value
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Isystm,Systm)
   Lenopc = 400000000
!
!     SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/
!
   Isystm(18) = 0
   CALL second(systm(18))
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
   CALL getenv('RFDIR',rfdir)
   value = ' '
   CALL getenv('DIRCTY',dirtry)
   len = index(dirtry,' ') - 1
   DO i = 1 , 90
      IF ( i<=9 ) WRITE (tmp,99001) i
99001 FORMAT ('scr',I1)
      IF ( i>9 ) WRITE (tmp,99002) i
99002 FORMAT ('scr',I2)
      dsnames(i) = dirtry(1:len)//'/'//tmp
   ENDDO
   CALL getenv('LOGNM',log)
   dsnames(3) = log
   CALL getenv('OPTPNM',optp)
   dsnames(7) = optp
   CALL getenv('NPTPNM',nptp)
   dsnames(8) = nptp
   CALL getenv('FTN11',out11)
   dsnames(11) = out11
   CALL getenv('FTN12',in12)
   dsnames(12) = in12
   CALL getenv('FTN13',value)
   dsnames(13) = value
   CALL getenv('FTN14',value)
   dsnames(14) = value
   CALL getenv('FTN15',value)
   dsnames(15) = value
   CALL getenv('FTN16',value)
   dsnames(16) = value
   CALL getenv('FTN17',value)
   dsnames(17) = value
   CALL getenv('FTN18',value)
   dsnames(18) = value
   CALL getenv('FTN19',value)
   dsnames(19) = value
   CALL getenv('FTN20',value)
   dsnames(20) = value
   CALL getenv('FTN21',value)
   dsnames(21) = value
   CALL getenv('PLTNM',plot)
   dsnames(10) = plot
   CALL getenv('DICTNM',dic)
   dsnames(4) = dic
   CALL getenv('PUNCHNM',punch)
   dsnames(1) = punch
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
   OPEN (3,FILE=dsnames(3),STATUS='UNKNOWN')
   IF ( dsnames(11)/='none' ) OPEN (11,FILE=dsnames(11),STATUS='UNKNOWN')
   IF ( dsnames(12)/='none' ) OPEN (12,FILE=dsnames(12),STATUS='UNKNOWN')
   IF ( dsnames(10)/='none' ) OPEN (10,FILE=dsnames(10),STATUS='UNKNOWN')
   IF ( dsnames(4)/='none' ) OPEN (4,FILE=dsnames(4),STATUS='UNKNOWN')
   IF ( dsnames(1)/='none' ) OPEN (1,FILE=dsnames(1),STATUS='UNKNOWN')
   CALL xsem00
END PROGRAM nastrn
