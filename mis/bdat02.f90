
SUBROUTINE bdat02
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Combo(7,5) , Conset , Geom4 , Iauto , Idry , Ierr , Ihead(96) , Inam(2) , Inpt , Iprint , Isort , Ititl(96)&
         & , Lcore , Mcon , Npsub , Outt , Scr1
   REAL Buf3 , Buf4 , Buf5 , Casecc , Conect , Origin(7,3) , Restct(7,7) , Scbdat , Scconn , Scmcon , Score , Scr2 , Scsfil ,       &
      & Sctoc , Step , Toler , Tran , Z(1)
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
   INTEGER aaa(2) , comp , conct(2) , flag , i , ibits(32) , iblnk , ic1 , ic2 , id(2) , ifile , ihd(16) , imsg , io(9) , is1 ,     &
         & is2 , j , jbits(32) , kdh , kk , n , name(14) , nams(4) , np2 , nwd
   INTEGER andf , rshift
   LOGICAL print
   EXTERNAL andf , rshift
!
!     THIS SUBROUTINE PROCESSES CONCT BULK DATA AND WRITES CONNECTION
!     ENTRIES IN TERMS OF CODED GRID POINT ID NUMBERS ON SCR1
!
   DATA aaa/4HBDAT , 4H02  / , conct/210 , 2/
   DATA ihd/4H  SU , 4HMMAR , 4HY OF , 4H CON , 4HNECT , 4HION  , 4HENTR , 4HIES  , 4HSPEC , 4HIFIE , 4HD BY , 4H CON , 4HCT   ,    &
       &4HBULK , 4H DAT , 4HA   /
   DATA iblnk/4H    /
!
   DO i = 1 , 96
      Ihead(i) = iblnk
   ENDDO
   j = 1
   DO i = 73 , 88
      Ihead(i) = ihd(j)
      j = j + 1
   ENDDO
   print = .FALSE.
   IF ( andf(rshift(Iprint,3),1)==1 ) print = .TRUE.
   np2 = 2*Npsub
   DO i = 1 , np2 , 2
      j = i/2 + 1
      name(i) = Combo(j,1)
      name(i+1) = Combo(j,2)
   ENDDO
   ifile = Scr1
   CALL open(*400,Scr1,Z(Buf2),3)
   CALL locate(*100,Z(Buf1),conct,flag)
   ifile = Geom4
   DO
      CALL read(*200,*100,Geom4,id,1,0,n)
      IF ( id(1)==Conset ) THEN
         CALL read(*200,*300,Geom4,comp,1,0,n)
         IF ( print ) THEN
            CALL page
            CALL page2(6)
            WRITE (Outt,99001) (name(kdh),kdh=1,np2)
99001       FORMAT (/24X,74HNOTE  GRID POINT ID NUMBERS HAVE BEEN CODED TO THE COMPONENT SUBSTRUCTURE ,/30X,                        &
                   &75HWITHIN A GIVEN PSEUDOSTRUCTURE BY - 1000000*COMPONENT NO. + ACTUAL GRID ID.,//15X,22HCONNECTED   CONNECTION, &
                  & 23X,33HGRID POINT ID FOR PSEUDOSTRUCTURE/18X,3HDOF,9X,4HCODE,3X,7(3X,2A4)/)
         ENDIF
         Tdat(2) = .TRUE.
         CALL encode(comp)
         CALL read(*200,*300,Geom4,nams,4,0,n)
         CALL finder(nams(1),is1,ic1)
         IF ( Ierr==1 ) THEN
            WRITE (Outt,99004) Ufm , nams(1) , nams(2)
            Idry = -2
         ENDIF
         CALL finder(nams(3),is2,ic2)
         IF ( Ierr==1 ) THEN
            WRITE (Outt,99004) Ufm , nams(3) , nams(4)
            Idry = -2
         ENDIF
         DO
            CALL read(*200,*300,Geom4,id,2,0,n)
!
            IF ( id(1)+id(2)==-2 ) EXIT
            IF ( is1==is2 ) THEN
               kk = 2*is1 - 1
               WRITE (Outt,99002) Ufm , id(1) , id(2) , name(kk) , name(kk+1)
99002          FORMAT (A23,' 6536, MANUAL CONNECTION DATA IS ATTEMPTING TO ','CONNECT',/31X,'GRID POINTS',I9,5X,4HAND ,I8,/31X,     &
                      &'WHICH ARE BOTH CONTAINED IN PSEUDOSTRUCTURE ',2A4)
               Idry = -2
            ENDIF
            DO i = 1 , 9
               io(i) = 0
            ENDDO
            io(1) = comp
            io(2) = 2**(is1-1) + 2**(is2-1)
            io(2+is1) = ic1*1000000 + id(1)
            io(2+is2) = ic2*1000000 + id(2)
            nwd = 2 + Npsub
            CALL write(Scr1,io,nwd,1)
            IF ( .NOT.(.NOT.print .OR. Idry==-2) ) THEN
               CALL bitpat(io(1),ibits)
               CALL bitpat(io(2),jbits)
               CALL page2(1)
               WRITE (Outt,99003) (ibits(kdh),kdh=1,2) , (jbits(kdh),kdh=1,2) , (io(kdh+2),kdh=1,Npsub)
99003          FORMAT (16X,A4,A2,6X,A4,A3,2X,7(3X,I8))
            ENDIF
         ENDDO
      ELSE
         CALL read(*200,*300,Geom4,id,-5,0,n)
         DO
            CALL read(*200,*300,Geom4,id,2,0,n)
            IF ( id(1)+id(2)==-2 ) EXIT
         ENDDO
      ENDIF
   ENDDO
 100  CALL close(Scr1,1)
   RETURN
!
 200  imsg = -2
   GOTO 500
 300  imsg = -3
   GOTO 500
 400  imsg = -1
 500  CALL mesage(imsg,ifile,aaa)
99004 FORMAT (A23,' 6523, THE BASIC SUBSTRUCTURE ',2A4,/30X,'REFERED TO BY A CONCT  BULK DATA CARD CAN NOT BE FOUND ',              &
             &'IN THE PROBLEM TABLE OF CONTENTS.')
END SUBROUTINE bdat02