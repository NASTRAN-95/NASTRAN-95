
SUBROUTINE bdat04
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Conset , Geom4 , Iauto , Idat(3) , Idry , Ierr , Ihead(96) , Inam(2) , Inpt , Iot , Ipage , Iprint ,       &
         & Isort , Ititl(96) , Itline , Junk(6) , Lcore , Line , Maxlin , Mcon , Npsub , Outt , Scbdat , Scr2
   REAL Buf3 , Buf4 , Buf5 , Casecc , Combo(7,5) , Conect , Origin(7,3) , Restct(7,7) , Scconn , Scmcon , Score , Scr1 , Scsfil ,   &
      & Sctoc , Step , Toler , Tran , Xxx , Z(1)
   LOGICAL Tdat(6)
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /cmb004/ Tdat
   COMMON /cmbfnd/ Inam , Ierr
   COMMON /output/ Ititl , Ihead
   COMMON /system/ Xxx , Iot , Junk , Ipage , Line , Itline , Maxlin , Idat
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , flag , i , ibas(2) , ibits(32) , ic , icc(6) , iccc , id(2) , ifile , ihd(96) , imsg , ip(6) , is , jbits(32) , &
         & k , kbits(32) , n , reles(2)
   INTEGER andf , rshift
   LOGICAL name , pager , print
   EXTERNAL andf , rshift
!
!     THIS SUBROUTINE PROCESSES THE RELES BULK DATA.
!
   DATA ihd/11*4H     , 4H  SU , 4HMMAR , 4HY OF , 4H PRO , 4HCESS , 4HED R , 4HELES , 4H BUL , 4HK DA , 4HTA   , 18*4H     ,       &
      & 4H   B , 4HASIC , 2*4H     , 4H GRI , 4HD    , 4H     , 4HREQU , 4HESTE , 4HD    , 4H  IN , 4HTERN , 4HAL   , 4H   C ,      &
       &4HURRE , 4HNT   , 4H  DO , 4HF TO , 4H BE  , 13*4H     , 4HSUBS , 4HTRUC , 4HTURE , 4H   P , 4HOINT , 4H ID  , 4H     ,     &
       &4H REL , 4HEASE , 4H     , 4H  PO , 4HINT  , 4HNO.  , 4H     , 4H DOF , 4H     , 4H   R , 4HELEA , 4HSED  , 6*4H    /
   DATA reles/410 , 4/ , aaa/4HBDAT , 4H04  /
!
   DO i = 1 , 96
      Ihead(i) = ihd(i)
   ENDDO
   pager = .TRUE.
   print = .FALSE.
   IF ( andf(rshift(Iprint,8),1)==1 ) print = .TRUE.
   ifile = Scbdat
   CALL open(*300,Scbdat,Z(Buf2),0)
   CALL skpfil(Scbdat,3)
   CALL close(Scbdat,2)
   CALL open(*300,Scbdat,Z(Buf2),3)
   ifile = Scr2
   CALL locate(*200,Z(Buf1),reles,flag)
   ifile = Geom4
   DO
      CALL read(*400,*200,Geom4,id,1,0,n)
      IF ( id(1)==Conset ) THEN
         name = .TRUE.
         IF ( pager .AND. print ) CALL page
         pager = .FALSE.
         Tdat(4) = .TRUE.
         DO
            CALL read(*400,*500,Geom4,id,2,0,n)
            IF ( id(1)+id(2)==-2 ) THEN
               CALL write(Scbdat,id,0,1)
               EXIT
            ELSEIF ( .NOT.name ) THEN
               CALL fndgrd(is,ic,id(1),ip,icc,n)
               IF ( Ierr/=1 ) THEN
                  CALL encode(id(2))
                  CALL bitpat(id(2),ibits)
                  DO i = 1 , n
                     iccc = andf(id(2),icc(i))
                     CALL bitpat(iccc,jbits)
                     icc(i) = andf(icc(i),63)
                     CALL bitpat(icc(i),kbits)
                     IF ( iccc/=0 ) THEN
                        IF ( print ) THEN
                           WRITE (Outt,99001) ibas , id(1) , ibits(1) , ibits(2) , ip(i) , kbits(1) , kbits(2) , jbits(1) , jbits(2)
99001                      FORMAT (35X,2A4,5X,I8,7X,A4,A2,6X,I8,6X,A4,A2,6X,A4,A2)
                        ENDIF
                        CALL write(Scbdat,ip(i),1,0)
                        CALL write(Scbdat,iccc,1,0)
                     ENDIF
                  ENDDO
               ELSE
                  WRITE (Outt,99002) Ufm , id(1) , Inam
99002             FORMAT (A23,' 6515, GRID POINT',I10,' BASIC SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
                  Idry = -2
               ENDIF
            ELSE
               CALL finder(id,is,ic)
               ibas(1) = id(1)
               ibas(2) = id(2)
               IF ( Ierr/=1 ) THEN
                  CALL write(Scbdat,is,1,0)
                  name = .NOT.name
               ELSE
                  WRITE (Outt,99003) Ufm , (id(k),k=1,2)
99003             FORMAT (A23,' 6517, THE BASIC SUBSTRUCTURE  ',2A4,/30X,'REFERED TO BY A RELES  BULK DATA CARD CAN NOT BE FOUND ', &
                         &'IN THE PROBLEM TABLE OF CONTENTS.')
                  Idry = -2
                  DO
                     CALL read(*400,*500,Geom4,id,2,0,n)
                     IF ( id(1)+id(2)==-2 ) GOTO 100
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ELSE
         DO
            CALL read(*400,*500,Geom4,id,2,0,n)
            IF ( id(1)+id(2)==-2 ) EXIT
         ENDDO
      ENDIF
 100  ENDDO
 200  CALL close(Scbdat,1)
   RETURN
!
 300  imsg = -1
   GOTO 600
 400  imsg = -2
   GOTO 600
 500  imsg = -3
 600  CALL mesage(imsg,ifile,aaa)
END SUBROUTINE bdat04
