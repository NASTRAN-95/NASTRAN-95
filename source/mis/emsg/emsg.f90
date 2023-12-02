!*==emsg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emsg(Nchar,No,Isys,Iwf,Itext)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nchar
   INTEGER :: No
   INTEGER :: Isys
   INTEGER :: Iwf
   INTEGER , DIMENSION(1) :: Itext
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , imach , k , m , ncpw , nline , no1 , nout , nword
   INTEGER , DIMENSION(2,4) , SAVE :: imsg
   EXTERNAL mesage , page2
!
! End of declarations rewritten by SPAG
!
!     ISYS = 1   USER           IWF  =     1   WARNING
!          = 2   SYSTEM             =     2   FATAL
!
   !>>>>EQUIVALENCE (Ncpw,Sysbuf(41)) , (Nout,Sysbuf(2)) , (Imach,Sysbuf(22))
   DATA imsg/4HUSER , 1H  , 4HSYST , 4HEM   , 4HWARN , 4HING  , 4HFATA , 1HL/
   nword = (Nchar+ncpw-1)/ncpw
   nline = (Nchar+9+131)/132 + 2
   CALL page2(-nline)
   no1 = iabs(No)
   k = Iwf + 2
   WRITE (nout,99001) (imsg(i,Isys),i=1,2) , (imsg(m,k),m=1,2) , no1
99001 FORMAT (1H0,4H*** ,4A4,I4,1H,)
   IF ( Nchar==0 ) RETURN
   IF ( imach==2 .OR. imach==5 ) THEN
!
!     360/370
!
      WRITE (nout,99002) (Itext(i),i=1,nword)
99002 FORMAT (10X,30A4,A2)
   ELSEIF ( imach==4 ) THEN
!
!     CDC
!
      WRITE (nout,99003) (Itext(i),i=1,nword)
99003 FORMAT (10X,12A10,A2)
   ELSE
!
!     7094
!
      WRITE (nout,99004) (Itext(i),i=1,nword)
99004 FORMAT (10X,20A6,A2)
   ENDIF
   IF ( No<0 ) CALL mesage(-61,0,0)
END SUBROUTINE emsg
