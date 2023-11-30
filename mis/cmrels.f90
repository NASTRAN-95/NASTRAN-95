
SUBROUTINE cmrels
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Inpt , Junk(38) , Lcore , Npsub , Scbdat , Scconn , Score , Z(1)
   REAL Buf3 , Buf4 , Buf5 , Outt , Scr1 , Scr2 , Scsfil
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Junk , Npsub
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , i , icode , id , ifile , ii , imsg , ist , ist1 , ist2 , iw , ix(7,3) , j , kid , kj , list(32) , n , nc , nce ,&
         & nnn , nw , nw1 , nw2 , nwrd , ps1 , ps2 , stce
   INTEGER andf
   LOGICAL first
   EXTERNAL andf
!
!     THIS SUBROUTINE ENFORCES THE RELES DATA SPECIFIED FOR THE
!     COMB1 MODULE.
!
   DATA aaa/4HCMRE , 4HLS  /
!
   ifile = Scbdat
   kj = 0
   DO i = 1 , 7
      DO j = 1 , 3
         ix(i,j) = 0
      ENDDO
   ENDDO
   DO i = 1 , Npsub
      first = .TRUE.
      CALL open(*600,Scbdat,Z(Buf1),0)
      CALL skpfil(Scbdat,3)
 50   DO
         CALL read(*150,*800,Scbdat,id,1,0,n)
         IF ( id==i ) THEN
            CALL read(*700,*100,Scbdat,Z(Score+kj),Lcore,1,nw)
            GOTO 900
         ELSE
            CALL fwdrec(*150,Scbdat)
         ENDIF
      ENDDO
 100  IF ( first ) ix(i,2) = Score + kj
      first = .FALSE.
      ix(i,3) = ix(i,3) + nw/2
      kj = kj + nw
      Lcore = Lcore - nw
      ix(i,1) = 1
      GOTO 50
 150  CALL close(Scbdat,1)
   ENDDO
   DO i = 1 , Npsub
      IF ( ix(i,1)/=0 ) THEN
         ist = ix(i,2)
         nw = ix(i,3)*2
         CALL sort(0,0,2,1,Z(ist),nw)
      ENDIF
   ENDDO
   ifile = Scconn
   CALL open(*600,Scconn,Z(Buf2),0)
   nwrd = 2 + Npsub
   nce = 0
   stce = Score + kj
 200  CALL read(*400,*300,Scconn,Z(Score+kj),Lcore,1,nnn)
   GOTO 900
 300  kj = kj + nwrd
   nce = nce + 1
   GOTO 200
 400  CALL close(Scconn,1)
   nce = nwrd*nce
   DO i = 1 , nce , nwrd
      ii = i - 1
      icode = Z(stce+ii+1)
      CALL decode(icode,list,nc)
      IF ( nc/=2 ) CYCLE
      ps1 = list(1) + 1
      ps2 = list(2) + 1
      ist1 = ix(ps1,2)
      ist2 = ix(ps2,2)
      nw1 = ix(ps1,3)
      nw2 = ix(ps2,3)
      IF ( ix(ps1,1)/=0 ) THEN
         kid = Z(stce+ii+1+ps1)
         CALL bisloc(*450,kid,Z(ist1),2,nw1,iw)
         Z(stce+ii) = Z(stce+ii) - andf(Z(stce+ii),Z(ist1+iw))
      ENDIF
 450  IF ( ix(ps2,1)/=0 ) THEN
         kid = Z(stce+ii+1+ps2)
         CALL bisloc(*500,kid,Z(ist2),2,nw2,iw)
         Z(stce+ii) = Z(stce+ii) - andf(Z(stce+ii),Z(ist2+iw))
      ENDIF
 500  ENDDO
   CALL open(*600,Scconn,Z(Buf1),1)
   DO i = 1 , nce , nwrd
      ii = i - 1
      IF ( Z(stce+ii)/=0 ) CALL write(Scconn,Z(stce+ii),nwrd,1)
   ENDDO
   CALL eof(Scconn)
   CALL close(Scconn,1)
   RETURN
!
 600  imsg = -1
   GOTO 1000
 700  imsg = -2
   GOTO 1000
 800  imsg = -3
   GOTO 1000
 900  imsg = -8
 1000 CALL mesage(imsg,ifile,aaa)
END SUBROUTINE cmrels
