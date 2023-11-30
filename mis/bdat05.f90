
SUBROUTINE bdat05
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Conset , Geom4 , Iauto , Idry , Inpt , Lcore , Npsub , Outt , Scbdat , Score , Scr2 , Z(1)
   REAL Buf4 , Buf5 , Casecc , Combo(7,5) , Scconn , Scmcon , Scr1 , Scsfil , Sctoc , Step , Toler
   LOGICAL Tdat(6)
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub
   COMMON /cmb004/ Tdat
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , flag , gnew(2) , ifile , imsg
!
!     THIS SUBROUTINE PROCESSES THE GNEW BULK DATA
!
   DATA gnew/1410 , 14/ , aaa/4HBDAT , 4H05  /
!
   ifile = Scr2
   CALL open(*200,Scr2,Z(Buf3),1)
   ifile = Geom4
   CALL locate(*100,Z(Buf1),gnew,flag)
   WRITE (Outt,99001) Ufm
99001 FORMAT (A23,' 6532, THE GNEW OPTION IS NOT CURRENTLY AVAILABLE.')
   Idry = -2
   RETURN
!
 100  CALL eof(Scbdat)
   CALL close(Scr2,1)
   RETURN
!
 200  imsg = -1
   CALL mesage(imsg,ifile,aaa)
END SUBROUTINE bdat05
