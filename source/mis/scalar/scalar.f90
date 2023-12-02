!*==scalar.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
USE C_BLANK
USE C_SYSTEM
USE C_XMSSG
USE C_XVPS
USE C_ZNTPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: da , dp , dplx
   INTEGER , SAVE :: first , in1
   INTEGER , DIMENSION(2) :: fnm , pnm
   INTEGER :: form , i , ibuf , j , lcore , ncol , nrow , prec
   INTEGER , DIMENSION(7) :: ia
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: noprt
   REAL(REAL64) :: rdp
   REAL , DIMENSION(4) :: sp
   CHARACTER(10) , DIMENSION(4) , SAVE :: type
   REAL , DIMENSION(1) :: vps
   EXTERNAL close , fname , fndpar , intpk , korsz , mesage , open , page2 , rdtrl , skprec , sswtch , zntpki
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (R2(1),Rdp) , (D4(1),Dplx(1)) , (ia(2),ncol) , (ia(3),nrow) , (ia(4),form) , (ia(5),prec) , (Da(1),A(1)) ,           &
   !>>>> & (dp(1),sp(1)) , (Vps(1),Ivps(1))
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
      RETURN
   ELSE
      Rsp = 0.
      Splx(1) = 0.
      Splx(2) = 0.
      rdp = 0.D0
      dplx(1) = 0.D0
      dplx(2) = 0.D0
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
         RETURN
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
               RETURN
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
               rdp = 1.D0
               Splx(1) = 1.
               dplx(1) = 1.D0
            ENDIF
            GOTO 100
!     SQUARE, RECTANGULAR OR SYMMETRIC MATRIX
!
         ELSEIF ( Col>ncol ) THEN
            j = 7
            CALL mesage(j,in1,name)
            RETURN
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
         SPAG_Loop_1_1: DO
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
                  rdp = da(1)
                  Rsp = sngl(rdp)
               ELSEIF ( prec==3 ) THEN
!
                  Splx(1) = A(1)
                  Splx(2) = A(2)
                  dplx(1) = dble(Splx(1))
                  dplx(2) = dble(Splx(2))
                  EXIT SPAG_Loop_1_1
               ELSEIF ( prec==4 ) THEN
!
                  dplx(1) = da(1)
                  dplx(2) = da(2)
                  Splx(1) = sngl(dplx(1))
                  Splx(2) = sngl(dplx(2))
                  EXIT SPAG_Loop_1_1
               ELSE
!
                  Rsp = A(1)
                  rdp = dble(Rsp)
               ENDIF
               Splx(1) = Rsp
               dplx(1) = rdp
               GOTO 100
            ELSE
               GOTO 100
            ENDIF
         ENDDO SPAG_Loop_1_1
         Rsp = 0.0
         rdp = 0.D0
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
         vps(j) = Rsp
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
         dp(1) = rdp
         vps(j) = sp(1)
         vps(j+1) = sp(2)
         IF ( .NOT.(noprt) ) THEN
            WRITE (Nout,99003) rdp , pnm
99003       FORMAT (73X,D15.8,4H  = ,2A4)
            WRITE (Nout,99007) Row , Col , type(prec) , fnm
         ENDIF
      ENDIF
   ENDIF
   CALL fndpar(-5,j)
   IF ( j>0 ) THEN
      vps(j) = Splx(1)
      vps(j+1) = Splx(2)
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
      dp(1) = dplx(1)
      dp(2) = dplx(2)
      vps(j) = sp(1)
      vps(j+1) = sp(2)
      vps(j+2) = sp(3)
      vps(j+3) = sp(4)
      pnm(1) = Ivps(j-3)
      pnm(2) = Ivps(j-2)
      IF ( .NOT.(noprt) ) THEN
         WRITE (Nout,99005) dplx , pnm
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
END SUBROUTINE scalar
