
SUBROUTINE emsg(Nchar,No,Isys,Iwf,Itext)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Imach , Ncpw , Nout
   REAL Sysbuf(41)
   COMMON /system/ Sysbuf
!
! Dummy argument declarations
!
   INTEGER Isys , Iwf , Nchar , No
   INTEGER Itext(1)
!
! Local variable declarations
!
   INTEGER i , imsg(2,4) , k , m , nline , no1 , nword
!
! End of declarations
!
!     ISYS = 1   USER           IWF  =     1   WARNING
!          = 2   SYSTEM             =     2   FATAL
!
   EQUIVALENCE (Ncpw,Sysbuf(41)) , (Nout,Sysbuf(2)) , (Imach,Sysbuf(22))
   DATA imsg/4HUSER , 1H  , 4HSYST , 4HEM   , 4HWARN , 4HING  , 4HFATA , 1HL/
   nword = (Nchar+Ncpw-1)/Ncpw
   nline = (Nchar+9+131)/132 + 2
   CALL page2(-nline)
   no1 = iabs(No)
   k = Iwf + 2
   WRITE (Nout,99001) (imsg(i,Isys),i=1,2) , (imsg(m,k),m=1,2) , no1
99001 FORMAT (1H0,4H*** ,4A4,I4,1H,)
   IF ( Nchar==0 ) RETURN
   IF ( Imach==2 .OR. Imach==5 ) THEN
!
!     360/370
!
      WRITE (Nout,99002) (Itext(i),i=1,nword)
99002 FORMAT (10X,30A4,A2)
   ELSEIF ( Imach==4 ) THEN
!
!     CDC
!
      WRITE (Nout,99003) (Itext(i),i=1,nword)
99003 FORMAT (10X,12A10,A2)
   ELSE
!
!     7094
!
      WRITE (Nout,99004) (Itext(i),i=1,nword)
99004 FORMAT (10X,20A6,A2)
   ENDIF
   IF ( No<0 ) CALL mesage(-61,0,0)
END SUBROUTINE emsg
