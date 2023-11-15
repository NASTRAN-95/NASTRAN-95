
SUBROUTINE plttra
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cls , Clsrew , Rd , Rdrew , Wrt , Wrtrew
   INTEGER Lusep , Luset , Not , Sysbuf , Z(1)
   CHARACTER*23 Ufm
   COMMON /blank / Luset , Lusep
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Not
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER a(4) , b(2) , bgpdp , bgpdt , buf1 , buf2 , buf3 , buf4 , delta , file , flag , mcb(7) , nadd , name(2) , ndx , ns , nz ,&
         & plt(2) , s1 , s2 , sil , sip
   INTEGER korsz
   LOGICAL leof
!
! End of declarations
!
!
!     PLTTRA MODIFIES THE SIL AND BGPDT TABLES FOR THE PURPOSE OF
!     PLOTTING SPECIAL SCALAR GRID POINTS
!
!     INPUT  SIL  BGPDT  LUSET
!     OUTPUT SIP  BGPDP  LUSEP
!
!     SPECIAL SCALAR GRID POINTS
!     BGPDT(I,1)= 1  SIL(I+1)-SIL(I)=1
!     BGPDP(I,1)=-2  SIP(I+1)-SIP(I)=6
!
!     LUSET IS THE VALUE OF SIL(LAST+1) IF IT EXISTED
!     LUSEP IS THE VALUE OF SIP(LAST+1) IF IT EXISTED
!
   EQUIVALENCE (a(3),b(1)) , (file,mcb(1))
   DATA bgpdt , sil , bgpdp , sip/101 , 102 , 201 , 202/
   DATA plt/4HPLTT , 4HRA  / , mcb/7*0/
   DATA leof/.FALSE./
!
   nadd = 0
   ns = 0
!
!     LOCATE STORAGE AREA FOR FILE BUFFERS
!
   nz = korsz(Z)
   buf1 = nz - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   IF ( buf4<=0 ) CALL mesage(-8,nz,plt)
!
!     READ TRAILER RECORDS OF INPUT FILES AND CHECK COMPATABILITY
!     OPEN AND FOREWARD SPACE LABEL RECORD OF INPUT FILES
!     OPEN AND WRITE LABEL RECORD OF OUTPUT FILES
!
   file = bgpdt
   CALL rdtrl(mcb)
   CALL fname(file,name)
   IF ( file<=0 ) GOTO 400
   CALL open(*400,bgpdt,Z(buf2),Rdrew)
   CALL fwdrec(*600,bgpdt)
!
   file = sil
   CALL rdtrl(mcb)
   CALL fname(file,name)
   IF ( file<=0 ) GOTO 400
   IF ( mcb(3)/=Luset ) THEN
      WRITE (Not,99001) Ufm , Luset , mcb(3)
99001 FORMAT (A23,' 5011, FIRST PARAMETER',I6,' NE TRAILER RECORD ','PARAMETER',I6)
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
      CALL open(*400,sil,Z(buf1),Rdrew)
      CALL fwdrec(*600,sil)
!
      file = sip
      CALL fname(sip,a)
      CALL open(*500,sip,Z(buf3),Wrtrew)
      CALL write(sip,a,2,1)
!
      file = bgpdp
      CALL open(*500,bgpdp,Z(buf4),Wrtrew)
      CALL fname(bgpdp,b)
      CALL write(bgpdp,b,2,1)
!
!     READ SIL(I)
!
      file = sil
      CALL read(*600,*700,sil,s1,1,0,flag)
   ENDIF
!
!     READ SIL(I+1)
!
 100  file = sil
   CALL read(*600,*300,sil,s2,1,0,flag)
!
!     READ BGPDT(I,J)
!
 200  file = bgpdt
   CALL read(*600,*700,bgpdt,a,4,0,flag)
   delta = 0
   ns = ns + 1
!
!     CHECK IF SPECIAL SCALAR GRID POINT
!
   IF ( a(1)>=0 .AND. s2-s1/=6 ) THEN
      IF ( s2-s1/=1 ) THEN
         WRITE (Not,99002) Ufm , ns
99002    FORMAT (A23,' 5012, ENTRY',I6,' OF SIL TABLE INCOMPATIBLE WITH ','NEXT ENTRY')
         CALL mesage(-61,0,0)
         GOTO 99999
      ELSE
!
!     SPECIAL SCALAR GRID POINT
!
         delta = 5
         a(1) = -2
      ENDIF
   ENDIF
   s1 = s1 + nadd
!
!     WRITE SIP AND BGPDP TABLE ENTRIES
!
   CALL write(sip,s1,1,0)
   CALL write(bgpdp,a,4,0)
   nadd = nadd + delta
   IF ( leof ) THEN
      Lusep = Luset + nadd
!
!     CLOSE OUTPUT FILES AND WRITE TRAILER RECORDS
!
      CALL close(sil,Clsrew)
      CALL close(bgpdt,Clsrew)
      CALL close(sip,Clsrew)
      CALL close(bgpdp,Clsrew)
      mcb(1) = bgpdp
      mcb(3) = 0
      CALL wrttrl(mcb)
      mcb(1) = sip
      mcb(3) = Lusep
      CALL wrttrl(mcb)
      RETURN
   ELSE
      s1 = s2
      GOTO 100
   ENDIF
!
!     SIL(I) IS SIL(LAST)
!
 300  leof = .TRUE.
   s2 = Luset + 1
   GOTO 200
!
 400  Lusep = Luset
   RETURN
!
!     ERROR DIAGNOSTICS
!
 500  ndx = -1
   GOTO 800
 600  ndx = -2
   GOTO 800
 700  ndx = -3
 800  CALL mesage(ndx,file,plt)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE plttra
