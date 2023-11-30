
SUBROUTINE matdum(Ia,Iprc,Npl,Nout)
   IMPLICIT NONE
   REAL Col(20000) , Head1(96) , Head2(96)
   DOUBLE PRECISION Dcol(1)
   INTEGER Icol(1) , Incr , Inx(6) , Inx1(2) , Iout , It , K , L , Line , Nlpp , P12(2) , P3 , P4 , P5 , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / P12 , P3 , P4 , P5
   COMMON /output/ Head1 , Head2
   COMMON /system/ Sysbuf , Iout , Inx , Nlpp , Inx1 , Line
   COMMON /unpakx/ It , K , L , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Col
   INTEGER Iprc , Nout , Npl
   INTEGER Ia(7)
   REAL blank , cont , ddx , form(18) , type(10) , xinue , xix , xmatr
   CHARACTER*35 cfmt , fmtc(2,4)
   INTEGER file(14) , i , ia7a , ia7b , ia7c , ibegn , iblnk , if , ifin , ihop , inull , j , jj , jmp3 , jmp4 , kj , lcol , ln ,   &
         & mm , namea , ncol , nn , npl1 , nplp5 , nrow , px(5)
   CHARACTER*15 fmtr(2,7) , rfmt
   LOGICAL jump
   INTEGER korsz
!
!     THIS ROUTINE IS CALLED ONLY BY MATPRN TO PRINT UP TO 5 MATRICES
!
!     IF IPRC = 0, MATRICES ARE PRINTED IN THEIR ORIG. PRECISION FORMAT
!     IF IPRC = 1, MATRICES ARE PRINTED IN SINGLE PRECISION E FORMAT
!     IF IPRC = 2, MATRICES ARE PRINTED IN DOUBLE PRECISION D FORMAT
!     IF IPRC =-1, ONLY THE DIAGONAL ELEMENTS OF THE MATRICES ARE
!                  PRINTED IN THEIR ORIG. PRECISION FORMAT
!
!     INPUT MATRIX IA(1) CAN BE IN S.P., D.P., S.P.CMPLX OR D.P.CMPLX
!
!     NPL IS THE NO. OF DATA VALUES PRINTED PER OUTPUT LINE
!     FOR S.P. REAL  DEFAULT IS 8, MAX IS 14
!     FOR D.P. REAL  DEFAULT IS 6, MAX IS 12
!     EVEN NUMBER ONLY FOR COMPLEX
!
!     P3, P4, P5 ARE PRINTOUT CONTROLS
!     P3 = m, MATRIX COLUMNS, 1 THRU m, WILL BE PRINTED.
!             DEFAULT = 0, ALL MATRIX COLUMNS WILL BE PRINTED.
!        =-m, SEE P4 = -n
!     P4 = n, LAST n MATRIX COLUMNS ARE PRINTED. DEFAULT = 0
!        =-n, AND P3 = -m, EVERY OTHER n MATRIX COLUMNS WILL BE PRINTED,
!             STARTIN FROM COLUMN m.
!     P5 = k, EACH PRINTED COLUMN WILL NOT EXCEED k LINES LONG AND THE
!             REMAINING DATA WILL BE OMITTED.
!     NOUT = P6, FORTRAN UNIT (SEE MATPRN)
!
!ZZ   COMMON /ZZTBPR/  COL(1)
   !>>>>EQUIVALENCE (Col(1),Dcol(1),Icol(1)) , (iblnk,blank)
   DATA type/4HS.P. , 4HREAL , 4HD.P. , 4HREAL , 4HCOMP , 4HLEX  , 4HCMP  , 4HD.P. , 4HILL  , 4HDEFN/
   DATA form/4HSQUA , 4HRE   , 4HRECT , 4HANG  , 4HDIAG , 4HONAL , 4HLOW  , 4HTRI  , 4HUPP  , 4HTRI  , 4HSYMM , 4HETRC , 4HVECT ,   &
       &4HOR   , 4HIDEN , 4HTITY , 4HILL  , 4HDEFN/
   DATA blank , xmatr , xix , cont , xinue , ddx/4H     , 4HMATR , 4HIX   , 4HCONT , 4HINUE , 4HD   /
   DATA file/4HUT1  , 4HUT2  , 4HN/A  , 4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9 ,   &
       &4HINPT/
   DATA fmtr/'(1X,1P, 8E16.8)' , '(1X,1P,6D21.12)' , '(1X,1P, 9E14.6)' , '(1X,1P,7D18.10)' , '(1X,1P,10E13.5)' , '(1X,1P, 8D16.8)' ,&
       &'(1X,1P,11E11.3)' , '(1X,1P, 9D14.6)' , '(1X,1P,12E10.2)' , '(1X,1P,10D13.4)' , '(1X,1P,13E10.2)' , '(1X,1P,11D11.3)' ,     &
       &'(1X,1P,14E 9.1)' , '(1X,1P,12D10.2)'/
   DATA fmtc/'(4(1X,1P,E14.7,1HR,  1P,E15.7,1HI))' , '(3(1X,1P,D20.13,1HR,1P,D21.13,1HI))' , '(5(1X,1P,E11.4,1HR,  1P,E12.4,1HI))' ,&
       &'(4(1X,1P,D14.7,1HR,  1P,D15.7,1HI))' , '(6(1X,1P,E 9.2,1HR,  1P,E10.2,1HI))' , '(5(1X,1P,D11.4,1HR,  1P,D12.4,1HI))' ,     &
       &'(7(1X,1P,E 7.0,1HR,  1P,E 8.0,1HI))' , '(6(1X,1P,D 9.2,1HR,  1P,D10.2,1H)))'/
!
   namea = Ia(1)
   ncol = Ia(2)
   nrow = Ia(3)
   if = Ia(4)
   It = Ia(5)
   IF ( if==7 ) THEN
!
!     ROW VECTOR
!     A ROW OF MATRIX ELEMENTS STORED IN COLUMN FORMAT
!     INTERCHANGE ROW AND COLUMN FOR PRINTING
!
      j = ncol
      ncol = nrow
      nrow = j
   ENDIF
   IF ( It<=0 .OR. It>4 ) It = 5
   IF ( if<=0 .OR. if>8 ) if = 9
   IF ( Nout/=Iout ) WRITE (Iout,99001) Uim , file(Nout-10) , Nout
99001 FORMAT (A29,', MATRIX PRINTOUT SAVED IN ',A4,' (FORTRAN UNIT',I4,1H))
   IF ( Iprc/=-1 ) THEN
!
!     SET UP FORMAT FOR OUTPUT PRINT LINE
!
      j = Iprc
      IF ( It>=3 ) j = Iprc + 2
      IF ( j==2 ) THEN
         j = Npl - 5
         rfmt = fmtr(2,j)
      ELSEIF ( j==3 ) THEN
         j = (Npl/2) - 3
         cfmt = fmtc(1,j)
      ELSEIF ( j==4 ) THEN
         j = (Npl/2) - 2
         cfmt = fmtc(2,j)
      ELSE
         j = Npl - 7
         rfmt = fmtr(1,j)
      ENDIF
      npl1 = Npl - 1
!
!     SET UP P3 AND P4 PRINTOUT OPTIONS
!
      mm = P3
      nn = Ia(2)
      IF ( P3<=0 ) mm = Ia(2)
      IF ( P4<0 ) THEN
         jump = .TRUE.
         jmp4 = -P4
         jmp3 = iabs(P3)
         IF ( P3==0 ) jmp3 = 1
      ELSE
         jump = .FALSE.
         nn = Ia(2) - P4
      ENDIF
      nplp5 = Ia(3)
      IF ( P5/=0 ) nplp5 = Npl*P5
!WKBI SPR 93013
      IF ( It>2 ) nplp5 = 2*nplp5
   ENDIF
!
   DO i = 1 , 96
      Head2(i) = blank
   ENDDO
   Head2(1) = xmatr
   Head2(2) = xix
   Head2(6) = cont
   Head2(7) = xinue
   Head2(8) = ddx
   lcol = korsz(Col) - Sysbuf
   Incr = 1
   CALL gopen(namea,Col(lcol+1),0)
   CALL page1
   CALL fname(namea,Head2(3))
   WRITE (Nout,99002) Head2(3) , Head2(4) , namea , type(2*It-1) , type(2*It) , ncol , nrow , form(2*if-1) , form(2*if)
99002 FORMAT (1H0,6X,7HMATRIX ,2A4,11H (GINO NAME,I4,2H ),6H IS A ,2A4,1X,I6,10H COLUMN X ,I6,5H ROW ,2A4,8H MATRIX.)
   IF ( It==5 .OR. ncol==0 .OR. nrow==0 ) GOTO 1200
!
!     IF = 3, DIAGONAL MATRIX
!        = 7, ROW VECTOR
!        = 8, IDENTITY MATRIX
!
   IF ( if<8 ) THEN
      IF ( Iprc==-1 ) THEN
!
!     PRINT ONLY THE DIAGONAL ELEMENTS, IPRC = -1
!     TO CHECKOUT THE DIAGONALS FOR POSSIBLE MATRIX SINGULARITY
!
         WRITE (Nout,99003)
99003    FORMAT (/23X,'(ELEMENTS ON DIAGONAL ONLY)')
         IF ( ncol/=nrow ) WRITE (Nout,99004)
99004    FORMAT (23X,'*** MATRIX IS NOT SQUARE ***')
         WRITE (Nout,99005)
99005    FORMAT (1X)
         nn = min0(ncol,nrow)
         jj = 0
         DO i = 1 , nn
            K = i
            L = i
            CALL unpack(*10,namea,Col(jj+1))
            GOTO 20
 10         DO j = 1 , It
               Col(jj+j) = 0.0
            ENDDO
 20         jj = jj + It
         ENDDO
         CALL close(namea,1)
         IF ( It==2 ) THEN
            jj = jj/2
            WRITE (Nout,99006) (Dcol(j),j=1,jj)
99006       FORMAT (1X,1P,10D13.6)
         ELSEIF ( It==3 ) THEN
            WRITE (Nout,99007) (Col(j),j=1,jj)
99007       FORMAT ((1X,5(1P,E12.5,1HR,1P,E12.5,1HI)))
         ELSEIF ( It==4 ) THEN
            jj = jj/2
            WRITE (Nout,99008) (Dcol(j),j=1,jj)
99008       FORMAT ((1X,5(1P,D12.5,1HR,1P,D12.5,1HI)))
         ELSE
            WRITE (Nout,99009) (Col(j),j=1,jj)
99009       FORMAT (1X,1P,10E13.6)
         ENDIF
         kj = It
         IF ( It>=3 ) kj = It - 2
         nn = 0
         mm = 1
         DO j = 1 , jj
            ln = mm + It - 1
            DO i = mm , ln
               IF ( Col(i)/=0.0 ) GOTO 30
            ENDDO
            nn = nn + 1
            Icol(nn) = j
 30         mm = mm + kj
         ENDDO
         IF ( nn==0 ) THEN
            WRITE (Nout,99010)
99010       FORMAT ('0*** NO ZERO ON DIAGONALS')
         ELSE
            mm = min0(nn,200)
            WRITE (Nout,99011) (Icol(i),i=1,mm)
99011       FORMAT ('0*** ZERO DIAGONALS IN THE FOLLOWING COLUMNS -',/,(1X,20I6))
            IF ( nn>200 ) WRITE (Nout,99012)
99012       FORMAT (' ...AND MORE')
         ENDIF
         WRITE (Nout,99013) Ia
99013    FORMAT (/5X,'GINO FILE',I5,'   TRAILER =',6I7)
         Line = Line + Nlpp
         GOTO 99999
      ELSE
         IF ( if==3 .OR. if==7 ) THEN
            ncol = 1
            nrow = Ia(3)
         ENDIF
         inull = 0
         ASSIGN 200 TO ihop
         jj = 1
      ENDIF
   ELSEIF ( if==8 ) THEN
!
      WRITE (Nout,99014)
99014 FORMAT ('0IDENTITY MATRIX')
      GOTO 1200
   ELSE
      GOTO 1200
   ENDIF
 100  K = 0
   L = 0
   CALL unpack(*900,namea,Col)
   IF ( jj<=mm .OR. jj>=nn ) THEN
      IF ( jump ) THEN
         IF ( mod(jj,jmp4)/=jmp3 ) GOTO 1000
      ENDIF
      IF ( inull==1 ) GOTO 1300
   ELSE
      K = nn - mm - 1
      jj = jj + K
      IF ( jj<=ncol ) CALL skprec(namea,K)
      GOTO 1000
   ENDIF
 200  nrow = L - K + 1
   IF ( if==3 ) THEN
      WRITE (Nout,99015) K , L
99015 FORMAT ('0DIAGONAL ELEMENTS FOR COLUMNS',I6,4H TO ,I6,4H ARE,///)
      Line = Line + 2
   ELSEIF ( if==7 ) THEN
      WRITE (Nout,99016) K , L
99016 FORMAT ('0ROW ELEMENTS FOR COLUMNS',I6,4H TO ,I6,4H ARE,///)
      Line = Line + 2
   ELSE
      WRITE (Nout,99017) jj , K , L
99017 FORMAT (8H0COLUMN ,I6,5X,6H ROWS ,I6,6H THRU ,I6,5X,50(1H-),/,1H )
      Line = Line + 3
      IF ( Line>=Nlpp ) CALL page
      IF ( It>2 ) nrow = 2*nrow
   ENDIF
   K = 0
 300  j = K + 1
   IF ( j>nrow ) GOTO 1000
   K = j + npl1
   IF ( K>nrow ) K = nrow
   IF ( K>nplp5 ) GOTO 1000
   kj = K - j
   IF ( It==1 ) GOTO 500
   IF ( It==2 ) GOTO 600
   IF ( It==3 ) GOTO 700
   IF ( It==4 ) GOTO 800
!
 400  ln = (kj+npl1)/Npl
   Line = Line + ln
   IF ( Line>=Nlpp ) CALL page
   GOTO 300
!
!     REAL SINGLE PRECISION
!
 500  IF ( Iprc==2 ) THEN
      i = K
      DO ln = j , K
         Dcol(i) = Col(i)
         i = i - 1
      ENDDO
   ELSE
      WRITE (Nout,rfmt) (Col(i),i=j,K)
! 215 FORMAT (1X,1P,10E13.5)
!     LN = (KJ+10)/10
      GOTO 400
   ENDIF
!
!     REAL DOUBLE PRECISION
!
 600  IF ( Iprc==1 ) THEN
      DO i = j , K
         Col(i) = Dcol(i)
      ENDDO
      GOTO 500
   ELSE
      WRITE (Nout,rfmt) (Dcol(i),i=j,K)
! 245 FORMAT (1X,1P,8D16.8)
!     LN = (KJ+8)/8
      GOTO 400
   ENDIF
!
!     COMPLEX SINGLE
!
 700  IF ( Iprc==2 ) THEN
      i = K
      DO ln = j , K
         Dcol(i) = Col(i)
         i = i - 1
      ENDDO
   ELSE
      WRITE (Nout,cfmt) (Col(i),i=j,K)
! 275 FORMAT (1X,5(1P,E12.4,1HR,1P,E12.4,1HI))
!     LN = (KJ+10)/10
      GOTO 400
   ENDIF
!
!     COMPLEX DOUBLE
!
 800  IF ( Iprc==1 ) THEN
      DO i = j , K
         Col(i) = Dcol(i)
      ENDDO
      GOTO 700
   ELSE
      WRITE (Nout,cfmt) (Dcol(i),i=j,K)
! 305 FORMAT (1X,4(1P,D15.8,1HR,1P,D15.8,1HI))
!     LN = (KJ+8)/8
      GOTO 400
   ENDIF
!
 900  IF ( inull/=1 ) THEN
      inull = 1
      ibegn = jj
   ENDIF
 1000 jj = jj + 1
   IF ( jj<=ncol ) GOTO 100
   ASSIGN 1100 TO ihop
   IF ( inull==1 ) GOTO 1300
 1100 CALL close(namea,1)
   WRITE (Nout,99018) Ia(6)
99018 FORMAT ('0THE NUMBER OF NON-ZERO WORDS IN THE LONGEST RECORD =',I8)
   ia7a = Ia(7)/100
   ia7c = Ia(7) - 100*ia7a
   ia7b = ia7c/10
   ia7c = ia7c - 10*ia7b
   WRITE (Nout,99019) ia7a , ia7b , ia7c
99019 FORMAT ('0THE DENSITY OF THIS MATRIX IS ',I3,1H.,2I1,' PERCENT.')
   GOTO 99999
 1200 CALL close(namea,1)
!
!     FUNNY MATRIX - SAVE MODULE PARAMETERS AND TABLE PRINT IT
!
   DO i = 1 , 5
      px(i) = P12(i)
   ENDDO
   P12(1) = iblnk
   P12(2) = iblnk
   P3 = 3
   P4 = 3
   CALL tabprt(namea)
   DO i = 1 , 5
      P12(i) = px(i)
   ENDDO
   GOTO 99999
 1300 ifin = jj - 1
   WRITE (Nout,99020) ibegn , ifin
99020 FORMAT ('0COLUMNS ',I7,6H THRU ,I7,' ARE NULL.')
   inull = 0
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
   GOTO ihop
!
99999 RETURN
END SUBROUTINE matdum
