
SUBROUTINE bdat06
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Combo(7,5) , Geom4 , Iauto , Idry , Ierr , Ihead(96) , Inam(2) , Inpt , Iprint , Isort ,     &
         & Ititl(96) , Lcore , Mcon , Npsub , Outt , Scbdat , Score , Scr1 , Scr2 , Z(1)
   REAL Buf5 , Casecc , Conect , Conset , Origin(7,3) , Restct(7,7) , Scconn , Scmcon , Scsfil , Sctoc , Step , Toler , Tran
   LOGICAL Tdat(6)
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /cmb004/ Tdat
   COMMON /cmbfnd/ Inam , Ierr
   COMMON /output/ Ititl , Ihead
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , flag , gtran(2) , i , ic , id(5) , ifile , ihd(96) , imsg , is , kk , n , nn
   INTEGER andf , rshift
   LOGICAL print
   EXTERNAL andf , rshift
!
!     THIS SUBROUTINE PROCESSES THE GTRAN BULK DATA
!
!WKBI 8/94 ALPHA-VMS
   DATA gtran/1510 , 15/ , aaa/4HBDAT , 4H06  /
   DATA ihd/11*4H     , 4H  SU , 4HMMAR , 4HY OF , 4H PRO , 4HCESS , 4HED G , 4HTRAN , 4H BUL , 4HK DA , 4HTA   , 19*4H     ,       &
      & 4H PSE , 4HUDO- , 4H     , 4H     , 4H COM , 4HPONE , 4HNT   , 4H     , 4H   T , 4HRANS , 2*4H     , 4HGRID , 4H     ,      &
       &4HREFE , 4HRENC , 4HE    , 14*4H     , 4H  ST , 4HRUCT , 4HURE  , 4HNO.  , 4H   S , 4HTRUC , 4HTURE , 4H NO. , 4H     ,     &
       &4H  SE , 4HT ID , 2*4H     , 4H ID  , 4H     , 4HTRAN , 4HS  I , 4HD    , 7*2H  /
!
   ifile = Scr1
   kk = 0
   print = .FALSE.
   IF ( andf(rshift(Iprint,5),1)==1 ) print = .TRUE.
   DO i = 1 , 96
      Ihead(i) = ihd(i)
   ENDDO
   CALL open(*600,Scr2,Z(Buf3),1)
   ifile = Scbdat
   CALL locate(*500,Z(Buf1),gtran,flag)
   IF ( print ) CALL page
   ifile = Geom4
 100  DO
      CALL read(*700,*300,Geom4,id,1,0,n)
      DO i = 1 , Npsub
         IF ( id(1)==Combo(i,3) ) GOTO 200
      ENDDO
      CALL read(*700,*800,Geom4,id,-4,0,n)
   ENDDO
 200  Tdat(6) = .TRUE.
   kk = kk + 1
   CALL read(*700,*800,Geom4,id(2),4,0,n)
   CALL finder(id(2),is,ic)
   IF ( Ierr==1 ) THEN
      WRITE (Outt,99001) Ufm , id(2) , id(3)
99001 FORMAT (A23,' 6530, THE BASIC SUBSTRUCTURE ',2A4,/30X,'REFERED TO BY A GTRAN BULK DATA CARD WHICH CANNOT BE ',                &
             &'FOUNDD IN THE PROBLEM TABLE OF CONTENTS.')
      Idry = -2
   ENDIF
   IF ( print ) CALL page2(1)
   IF ( print ) WRITE (Outt,99002) is , ic , id(1) , id(4) , id(5)
!
99002 FORMAT (36X,I1,14X,I5,8X,I8,4X,I8,4X,I8)
   id(3) = id(1)
   id(1) = is
   id(2) = ic
   id(4) = ic*1000000 + id(4)
   Z(Buf4+kk) = id(5)
   CALL write(Scr2,id,5,0)
   GOTO 100
 300  CALL write(Scr2,id,0,1)
   CALL close(Scr2,1)
   IF ( .NOT.Tdat(6) ) GOTO 500
   ifile = Scr2
   CALL open(*600,Scr2,Z(Buf3),2)
   CALL read(*700,*400,Scr2,Z(Score),Lcore,0,nn)
   imsg = -8
   GOTO 900
 400  CALL sort(0,0,5,1,Z(Score),nn)
   CALL write(Scbdat,Z(Score),nn,1)
 500  CALL eof(Scbdat)
   Z(Buf4) = kk
   CALL close(Scr2,1)
   IF ( print ) CALL page2(3)
   IF ( print ) WRITE (Outt,99003)
99003 FORMAT (/5X,'NOTE - THE PSEUDOSTRUCTURE AND COMPONENT NUMBERS RE','FER TO THEIR POSITIONS IN THE PROBLEM TABLE OF CONTENTS.')
   RETURN
!
 600  imsg = -1
   GOTO 900
 700  imsg = -2
   GOTO 900
 800  imsg = -3
 900  CALL mesage(imsg,ifile,aaa)
   RETURN
END SUBROUTINE bdat06