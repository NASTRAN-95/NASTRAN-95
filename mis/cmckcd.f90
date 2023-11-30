
SUBROUTINE cmckcd
   IMPLICIT NONE
   REAL Buf1 , Buf3 , Buf4 , Buf5 , Casecc , Conect , Conset , Geom4 , Origin(7,3) , Restct(7,7) , Scbdat , Sccstm , Scmcon , Scr1 ,&
      & Scr2 , Scr3 , Sctoc , Step , Toler , Tran , Z(1)
   INTEGER Buf2 , Combo(7,5) , Iauto , Idry , Intp , Iprint , Isort , Lcore , Mcon , Npsub , Outt , Scconn , Score , Scsfil
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc , Sccstm , Scr3
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Intp , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , ce(9) , i , iadd , ierr , ifile , imsg , ipnum(7) , ist(7) , it , j , jj , kdh , kk , llco , mm , nnn , npt ,   &
         & nptm1 , nrec
   REAL coord(7,3) , diff2(3) , dist , sum
!
!     THIS SUBROUTINE DETERMINES WHETHER MANUALLY SPECIFIED CONNECTION
!     ENTRIES ARE ALLOWABLE BASED ON THE PRESCRIBED GEOMETRIC TOLERANCE.
!
   DATA aaa/4HCMCK , 4HCD  /
!
!     READ ALL BGSS INTO OPEN CORE
!
   it = 2
   ierr = 0
   llco = Lcore
   j = 0
   ifile = Scsfil
   CALL open(*400,Scsfil,Z(Buf2),0)
   DO i = 1 , Npsub
      nrec = Combo(i,5) + 1
      DO jj = 1 , nrec
         CALL fwdrec(*500,Scsfil)
      ENDDO
      CALL read(*500,*50,Scsfil,Z(Score+j),llco,1,nnn)
      GOTO 600
 50   ist(i) = Score + j
      j = j + nnn
      llco = llco - nnn
      CALL skpfil(Scsfil,1)
   ENDDO
   CALL close(Scsfil,1)
!
!     READ CONNECTION ENTRIES AND LOAD INTO COORD ARRAY
!
   ifile = Scconn
   CALL open(*400,Scconn,Z(Buf2),0)
 100  CALL read(*300,*200,Scconn,ce,10,1,nnn)
!
!     LOAD COORD ARRAY
!     CE(3)... UP TO CE(9) ARE INTERNAL POINT NO.
!     IZ(IADD) IS THE COORD (CSTM) ID OF THE INTERNAL PTS.
!     Z(IADD+1,+2,+3) ARE THE COORD. ORIGINS
!
 200  npt = 0
   DO i = 1 , Npsub
      IF ( ce(i+2)>0 ) THEN
         npt = npt + 1
         iadd = 4*(ce(i+2)-1) + ist(i)
         ipnum(npt) = ce(i+2)
         DO j = 1 , 3
            coord(npt,j) = Z(iadd+j)
         ENDDO
      ENDIF
   ENDDO
!
!     COMPARE ALL PAIRS OF COORDINATES AGAINST TOLER.
!
   nptm1 = npt - 1
   DO i = 1 , nptm1
      it = it - 1
      jj = i + 1
      DO j = jj , npt
         DO kk = 1 , 3
            diff2(kk) = (coord(j,kk)-coord(i,kk))**2
         ENDDO
         sum = 0.0
         DO kk = 1 , 3
            sum = sum + diff2(kk)
         ENDDO
         dist = sqrt(sum)
         IF ( dist>Toler ) THEN
            IF ( it<=1 ) THEN
               WRITE (Outt,99001) Ufm
99001          FORMAT (A23,' 6514, ERRORS HAVE BEEN FOUND IN MANUALLY SPECIFIED',' CONNECTION ENTRIES. SUMMARY FOLLOWS')
               ierr = 1
               Idry = -2
               it = 2
            ENDIF
            IF ( it<=2 ) THEN
               WRITE (Outt,99002) (ce(kdh),kdh=1,nnn)
99002          FORMAT ('0*** GEOMETRIC ERRORS HAVE BEEN FOUND IN THE FOLLOWING',' CONNECTION ENTRY',/5X,9I10)
               it = 3
            ENDIF
            WRITE (Outt,99003) ipnum(i) , (coord(i,mm),mm=1,3) , ipnum(j) , (coord(j,mm),mm=1,3)
99003       FORMAT ('0*** IP NUMBER',I10,13H COORDINATES ,3E16.6,4H AND,/,'     IP NUMBER',I10,13H COORDINATES ,3E16.6,             &
                   &' ARE NOT WITHIN TOLER UNITS.')
         ENDIF
      ENDDO
   ENDDO
   GOTO 100
!
 300  IF ( ierr==0 ) WRITE (Outt,99004) Uim
99004 FORMAT (A29,' 6516, ALL MANUAL CONNECTIONS SPECIFIED ARE ','ALLOWABLE WITH RESPECT TO TOLERANCE')
   CALL close(Scconn,1)
   GOTO 99999
!
 400  imsg = -1
   GOTO 700
 500  imsg = -2
   GOTO 700
 600  imsg = -8
 700  CALL mesage(imsg,ifile,aaa)
!
99999 RETURN
END SUBROUTINE cmckcd
