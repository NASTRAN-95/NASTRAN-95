
SUBROUTINE scalar
!
!     CONVERTS MATRIX ELEMENT TO PARAMETER
!
!     SCALAR   MTX//C,N,ROW/C,N,COL/V,N,RSP/V,N,RDP/V,N,SPLX/V,N,DPLX $
!
!     INPUT GINO FILE
!       MTX = ANY MATRIX, S.P. OR D.P.; REAL OR COMPLEX
!     OUTPUT GINO FILE
!       NONE
!     INPUT PARAMETERS
!       ROW, COL = ROW AND COLUMN OF MTX (DEFAULT ARE 1,1)
!     OUTPUT PARAMETERS
!       RSP  = VALUE OF MTX(ROW,COL), REAL SINGLE PRECISION
!       RDP  = VALUE OF MTX(ROW,COL), REAL DOUBLE PRECISION
!       SPLX = VALUE OF MTX(ROW,COL), S.P. COMPLEX
!       DPLX = VALUE OF MTX(ROW,COL), D.P. COMPLEX
!
!     ORIGINALY WRITTEN BY R. MITCHELL, GSFC, NOV. 1972
!
!     COMPLETELY REWRITTEN BY G.CHAN/UNISYS IN JUNE 1988, SUCH THAT THE
!     OUTPUT PARAMETERS ARE SAVED CORRECTLY ACCORDING TO THEIR PRECISION
!     TYPES. (THE PRTPARM MODULE WILL BE ABLE TO PRINT THEM OUT
!     CORRECTLY.) PLUS IMPROVED MESSAGES (WHICH CAN BE SUPPRESSED BY
!     DIAG 37)
!
   IMPLICIT NONE
   REAL A(4) , Rsp , Splx(2) , Vps(1)
   INTEGER Bk(1) , Col , Core(1) , D4(4) , Eol , Eor , Ii , Ivps(1) , Nout , R2(2) , Row , Sysbuf
   DOUBLE PRECISION Da(2) , Dplx(2) , Rdp
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Bk , Row , Col , Rsp , R2 , Splx , D4
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xvps  / Ivps
   COMMON /zntpkx/ A , Ii , Eol , Eor
   COMMON /zzzzzz/ Core
   DOUBLE PRECISION dp(2)
   INTEGER first , fnm(2) , form , i , ia(7) , ibuf , in1 , j , lcore , name(2) , ncol , nrow , pnm(2) , prec
   INTEGER korsz
   LOGICAL noprt
   REAL sp(4)
   CHARACTER*10 type(4)
   EQUIVALENCE (R2(1),Rdp) , (D4(1),Dplx(1)) , (ia(2),ncol) , (ia(3),nrow) , (ia(4),form) , (ia(5),prec) , (Da(1),A(1)) ,           &
    & (dp(1),sp(1)) , (Vps(1),Ivps(1))
   DATA in1 , name/101 , 4HSCAL , 4HAR  / , first/12/
   DATA type/'S.P. REAL ' , 'D.P. REAL ' , 'S.P. CMPLX' , 'D.P. CMPLX'/
!
!     SUPPRESS ALL SCALAR MESSAGES IF DIAG 37 IS ON
!
   CALL sswtch(37,i)
   noprt = i==1
!
!     MOVE VARIALBES IN /BLANK/ BY ONE WORD TO GET BY WORD BOUNDARY
!     ALIGNMENT SITUATION
!
   j = 12
   DO i = 1 , 11
      Bk(j) = Bk(j-1)
      j = j - 1
   ENDDO
!
!     INITIALIZATION
!
   lcore = korsz(Core)
   ibuf = lcore - Sysbuf + 1
   IF ( ibuf<1 ) THEN
!
!     ERROR MESSAGES, SET THEM ALL TO NON-FATAL
!
!     NOT ENOUGH CORE FOR GINO BUFFER
!
      j = 8
!
      CALL mesage(j,in1,name)
      GOTO 99999
   ELSE
      Rsp = 0.
      Splx(1) = 0.
      Splx(2) = 0.
      Rdp = 0.D0
      Dplx(1) = 0.D0
      Dplx(2) = 0.D0
      dp(1) = 0.D0
      dp(2) = 0.D0
      CALL fname(in1,fnm)
      CALL page2(first)
      first = 3
!
!     GET STATUS OF INPUT MATRIX
!     CHECK FOR PURGED INPUT OR OUT OF RANGE INPUT PARAMETERS
!
      ia(1) = in1
      CALL rdtrl(ia)
      IF ( ia(1)<0 ) GOTO 200
      IF ( Row>nrow ) THEN
!
!     INVALID ROW OR COLUMN NUMBER
!
         j = 7
         CALL mesage(j,in1,name)
         GOTO 99999
      ELSE
!
         IF ( form==3 ) THEN
!
!     DIAGONAL MATRIX
!
            IF ( Row/=Col ) GOTO 100
            IF ( Col>nrow ) THEN
               j = 7
               CALL mesage(j,in1,name)
               GOTO 99999
            ELSE
!     SET COL TO 1 FOR SPECIAL DIAGONAL FORMAT
               Col = 1
            ENDIF
         ELSEIF ( form==4 ) THEN
!
!     LOWER TRIANGULAR MATRIX (UPPER HALF= 0)
!
            IF ( Col>Row ) GOTO 100
         ELSEIF ( form==5 ) THEN
!
!     UPPER TRIANGULAR MATRIX (LOWER HALF= 0)
!
            IF ( Row>Col ) GOTO 100
         ELSEIF ( form==7 ) THEN
!
!     ROW VECTOR
!     SWITCH ROW AND COLUMN FOR PROPER INDEXING
!
            Row = Col
            Col = 1
         ELSEIF ( form==8 ) THEN
!
!     IDENTITY MATRIX
!
            IF ( Row==Col ) THEN
               Rsp = 1.0
               Rdp = 1.D0
               Splx(1) = 1.
               Dplx(1) = 1.D0
            ENDIF
            GOTO 100
!     SQUARE, RECTANGULAR OR SYMMETRIC MATRIX
!
         ELSEIF ( Col>ncol ) THEN
            j = 7
            CALL mesage(j,in1,name)
            GOTO 99999
         ENDIF
!
!     OPEN INPUT FILE AND SKIP HEADER RECORD AND UNINTERSTING COLUMNS
!
         CALL open(*200,in1,Core(ibuf),0)
         CALL skprec(in1,Col)
!
!     READ AND SEARCH COLUMN CONTAINING DESIRED ELEMENT.
!     RECALL THAT DEFAULT VALUE WAS SET TO ZERO
!
         CALL intpk(*100,in1,0,prec,0)
         DO
!
!     FETCH ONE ELEMENT
!     CHECK FOR DESIRED ELEMENT
!     IF INDEX HIGHER, IT MEANS ELEMENT WAS 0.
!
            CALL zntpki
            IF ( Ii<Row ) THEN
!
!     CHECK FOR LAST NON-ZERO ELEMENT IN COLUMN.
!
               IF ( Eol>0 ) GOTO 100
            ELSEIF ( Ii==Row ) THEN
!
!     MOVE VALUES TO OUTPUT PARAMETER AREA.
!     CHECK PRECISION OF INPUT VALUE.
!
               IF ( prec==2 ) THEN
!
                  Rdp = Da(1)
                  Rsp = sngl(Rdp)
               ELSEIF ( prec==3 ) THEN
!
                  Splx(1) = A(1)
                  Splx(2) = A(2)
                  Dplx(1) = dble(Splx(1))
                  Dplx(2) = dble(Splx(2))
                  EXIT
               ELSEIF ( prec==4 ) THEN
!
                  Dplx(1) = Da(1)
                  Dplx(2) = Da(2)
                  Splx(1) = sngl(Dplx(1))
                  Splx(2) = sngl(Dplx(2))
                  EXIT
               ELSE
!
                  Rsp = A(1)
                  Rdp = dble(Rsp)
               ENDIF
               Splx(1) = Rsp
               Dplx(1) = Rdp
               GOTO 100
            ELSE
               GOTO 100
            ENDIF
         ENDDO
         Rsp = 0.0
         Rdp = 0.D0
      ENDIF
   ENDIF
!
!     MOVE VALUES TO OUTPUT PARAMETERS AS REQUESTED BY USER, AND
!     SAVE PARAMETERS
!
 100  IF ( .NOT.(noprt) ) THEN
      CALL page2(3)
      WRITE (Nout,99001) Uim
99001 FORMAT (A29,' FROM SCALAR MODULE -',/5X,'(ALL SCALAR MESSAGES CAN BE SUPPRESSED BY DIAG 37)')
   ENDIF
   CALL fndpar(-3,j)
   IF ( j>0 ) THEN
      pnm(1) = Ivps(j-3)
      pnm(2) = Ivps(j-2)
      IF ( prec>=3 ) THEN
         WRITE (Nout,99006) Uwm , pnm
      ELSE
         Vps(j) = Rsp
         IF ( .NOT.(noprt) ) THEN
            WRITE (Nout,99002) Rsp , pnm
99002       FORMAT (73X,E15.8,4H  = ,2A4)
            WRITE (Nout,99007) Row , Col , type(prec) , fnm
         ENDIF
      ENDIF
   ENDIF
   CALL fndpar(-4,j)
   IF ( j>0 ) THEN
      pnm(1) = Ivps(j-3)
      pnm(2) = Ivps(j-2)
      IF ( prec>=3 ) THEN
         WRITE (Nout,99006) Uwm , pnm
      ELSE
         dp(1) = Rdp
         Vps(j) = sp(1)
         Vps(j+1) = sp(2)
         IF ( .NOT.(noprt) ) THEN
            WRITE (Nout,99003) Rdp , pnm
99003       FORMAT (73X,D15.8,4H  = ,2A4)
            WRITE (Nout,99007) Row , Col , type(prec) , fnm
         ENDIF
      ENDIF
   ENDIF
   CALL fndpar(-5,j)
   IF ( j>0 ) THEN
      Vps(j) = Splx(1)
      Vps(j+1) = Splx(2)
      pnm(1) = Ivps(j-3)
      pnm(2) = Ivps(j-2)
      IF ( .NOT.(noprt) ) THEN
         WRITE (Nout,99004) Splx , pnm
99004    FORMAT (73X,1H(,E15.8,1H,,E15.8,1H),4H  = ,2A4)
         WRITE (Nout,99007) Row , Col , type(prec) , fnm
      ENDIF
   ENDIF
   CALL fndpar(-6,j)
   IF ( j>0 ) THEN
      dp(1) = Dplx(1)
      dp(2) = Dplx(2)
      Vps(j) = sp(1)
      Vps(j+1) = sp(2)
      Vps(j+2) = sp(3)
      Vps(j+3) = sp(4)
      pnm(1) = Ivps(j-3)
      pnm(2) = Ivps(j-2)
      IF ( .NOT.(noprt) ) THEN
         WRITE (Nout,99005) Dplx , pnm
99005    FORMAT (73X,1H(,D15.8,1H,,D15.8,1H),4H  = ,2A4)
         WRITE (Nout,99007) Row , Col , type(prec) , fnm
      ENDIF
   ENDIF
!
!     CLOSE INPUT UNIT AND RETURN
!
   CALL close(in1,1)
   RETURN
!
!     INPUT FILE ERROR
!
 200  j = 1
   CALL mesage(j,in1,name)
99006 FORMAT (A25,' - INVALID OUTPUT REQUEST.',/5X,'ORIG. ELEM. IN ','COMPLEX FORM. OUTPUT PARAMETER ',2A4,' NOT SAVED)',/)
99007 FORMAT (1H+,4X,'ELEMENT (',I5,'-ROW,',I5,'-COL) OF ',A10,' INPUT',' FILE ',2A4,2H =)
!
99999 RETURN
END SUBROUTINE scalar
