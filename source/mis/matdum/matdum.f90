!*==matdum.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE matdum(Ia,Iprc,Npl,Nout)
   IMPLICIT NONE
   USE c_blank
   USE c_output
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Ia
   INTEGER :: Iprc
   INTEGER :: Npl
   INTEGER :: Nout
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: blank , cont , ddx , xinue , xix , xmatr
   CHARACTER(35) :: cfmt
   REAL*8 , DIMENSION(1) :: dcol
   INTEGER , DIMENSION(14) , SAVE :: file
   CHARACTER(35) , DIMENSION(2,4) , SAVE :: fmtc
   CHARACTER(15) , DIMENSION(2,7) , SAVE :: fmtr
   REAL , DIMENSION(18) , SAVE :: form
   INTEGER :: i , ia7a , ia7b , ia7c , ibegn , iblnk , if , ifin , ihop , inull , j , jj , jmp3 , jmp4 , kj , lcol , ln , mm ,      &
            & namea , ncol , nn , npl1 , nplp5 , nrow
   INTEGER , DIMENSION(1) :: icol
   LOGICAL :: jump
   INTEGER , DIMENSION(5) :: px
   CHARACTER(15) :: rfmt
   REAL , DIMENSION(10) , SAVE :: type
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   it = Ia(5)
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
   IF ( it<=0 .OR. it>4 ) it = 5
   IF ( if<=0 .OR. if>8 ) if = 9
   IF ( Nout/=iout ) WRITE (iout,99001) uim , file(Nout-10) , Nout
99001 FORMAT (A29,', MATRIX PRINTOUT SAVED IN ',A4,' (FORTRAN UNIT',I4,1H))
   IF ( Iprc/=-1 ) THEN
!
!     SET UP FORMAT FOR OUTPUT PRINT LINE
!
      j = Iprc
      IF ( it>=3 ) j = Iprc + 2
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
      mm = p3
      nn = Ia(2)
      IF ( p3<=0 ) mm = Ia(2)
      IF ( p4<0 ) THEN
         jump = .TRUE.
         jmp4 = -p4
         jmp3 = iabs(p3)
         IF ( p3==0 ) jmp3 = 1
      ELSE
         jump = .FALSE.
         nn = Ia(2) - p4
      ENDIF
      nplp5 = Ia(3)
      IF ( p5/=0 ) nplp5 = Npl*p5
!WKBI SPR 93013
      IF ( it>2 ) nplp5 = 2*nplp5
   ENDIF
!
   DO i = 1 , 96
      head2(i) = blank
   ENDDO
   head2(1) = xmatr
   head2(2) = xix
   head2(6) = cont
   head2(7) = xinue
   head2(8) = ddx
   lcol = korsz(col) - sysbuf
   incr = 1
   CALL gopen(namea,col(lcol+1),0)
   CALL page1
   CALL fname(namea,head2(3))
   WRITE (Nout,99002) head2(3) , head2(4) , namea , type(2*it-1) , type(2*it) , ncol , nrow , form(2*if-1) , form(2*if)
99002 FORMAT (1H0,6X,7HMATRIX ,2A4,11H (GINO NAME,I4,2H ),6H IS A ,2A4,1X,I6,10H COLUMN X ,I6,5H ROW ,2A4,8H MATRIX.)
   IF ( it==5 .OR. ncol==0 .OR. nrow==0 ) GOTO 1200
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
            k = i
            l = i
            CALL unpack(*10,namea,col(jj+1))
            GOTO 20
 10         DO j = 1 , it
               col(jj+j) = 0.0
            ENDDO
 20         jj = jj + it
         ENDDO
         CALL close(namea,1)
         IF ( it==2 ) THEN
            jj = jj/2
            WRITE (Nout,99006) (dcol(j),j=1,jj)
99006       FORMAT (1X,1P,10D13.6)
         ELSEIF ( it==3 ) THEN
            WRITE (Nout,99007) (col(j),j=1,jj)
99007       FORMAT ((1X,5(1P,E12.5,1HR,1P,E12.5,1HI)))
         ELSEIF ( it==4 ) THEN
            jj = jj/2
            WRITE (Nout,99008) (dcol(j),j=1,jj)
99008       FORMAT ((1X,5(1P,D12.5,1HR,1P,D12.5,1HI)))
         ELSE
            WRITE (Nout,99009) (col(j),j=1,jj)
99009       FORMAT (1X,1P,10E13.6)
         ENDIF
         kj = it
         IF ( it>=3 ) kj = it - 2
         nn = 0
         mm = 1
         DO j = 1 , jj
            ln = mm + it - 1
            DO i = mm , ln
               IF ( col(i)/=0.0 ) GOTO 30
            ENDDO
            nn = nn + 1
            icol(nn) = j
 30         mm = mm + kj
         ENDDO
         IF ( nn==0 ) THEN
            WRITE (Nout,99010)
99010       FORMAT ('0*** NO ZERO ON DIAGONALS')
         ELSE
            mm = min0(nn,200)
            WRITE (Nout,99011) (icol(i),i=1,mm)
99011       FORMAT ('0*** ZERO DIAGONALS IN THE FOLLOWING COLUMNS -',/,(1X,20I6))
            IF ( nn>200 ) WRITE (Nout,99012)
99012       FORMAT (' ...AND MORE')
         ENDIF
         WRITE (Nout,99013) Ia
99013    FORMAT (/5X,'GINO FILE',I5,'   TRAILER =',6I7)
         line = line + nlpp
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
 100  k = 0
   l = 0
   CALL unpack(*900,namea,col)
   IF ( jj<=mm .OR. jj>=nn ) THEN
      IF ( jump ) THEN
         IF ( mod(jj,jmp4)/=jmp3 ) GOTO 1000
      ENDIF
      IF ( inull==1 ) GOTO 1300
   ELSE
      k = nn - mm - 1
      jj = jj + k
      IF ( jj<=ncol ) CALL skprec(namea,k)
      GOTO 1000
   ENDIF
 200  nrow = l - k + 1
   IF ( if==3 ) THEN
      WRITE (Nout,99015) k , l
99015 FORMAT ('0DIAGONAL ELEMENTS FOR COLUMNS',I6,4H TO ,I6,4H ARE,///)
      line = line + 2
   ELSEIF ( if==7 ) THEN
      WRITE (Nout,99016) k , l
99016 FORMAT ('0ROW ELEMENTS FOR COLUMNS',I6,4H TO ,I6,4H ARE,///)
      line = line + 2
   ELSE
      WRITE (Nout,99017) jj , k , l
99017 FORMAT (8H0COLUMN ,I6,5X,6H ROWS ,I6,6H THRU ,I6,5X,50(1H-),/,1H )
      line = line + 3
      IF ( line>=nlpp ) CALL page
      IF ( it>2 ) nrow = 2*nrow
   ENDIF
   k = 0
 300  j = k + 1
   IF ( j>nrow ) GOTO 1000
   k = j + npl1
   IF ( k>nrow ) k = nrow
   IF ( k>nplp5 ) GOTO 1000
   kj = k - j
   IF ( it==1 ) GOTO 500
   IF ( it==2 ) GOTO 600
   IF ( it==3 ) GOTO 700
   IF ( it==4 ) GOTO 800
!
 400  ln = (kj+npl1)/Npl
   line = line + ln
   IF ( line>=nlpp ) CALL page
   GOTO 300
!
!     REAL SINGLE PRECISION
!
 500  IF ( Iprc==2 ) THEN
      i = k
      DO ln = j , k
         dcol(i) = col(i)
         i = i - 1
      ENDDO
   ELSE
      WRITE (Nout,rfmt) (col(i),i=j,k)
! 215 FORMAT (1X,1P,10E13.5)
!     LN = (KJ+10)/10
      GOTO 400
   ENDIF
!
!     REAL DOUBLE PRECISION
!
 600  IF ( Iprc==1 ) THEN
      DO i = j , k
         col(i) = dcol(i)
      ENDDO
      GOTO 500
   ELSE
      WRITE (Nout,rfmt) (dcol(i),i=j,k)
! 245 FORMAT (1X,1P,8D16.8)
!     LN = (KJ+8)/8
      GOTO 400
   ENDIF
!
!     COMPLEX SINGLE
!
 700  IF ( Iprc==2 ) THEN
      i = k
      DO ln = j , k
         dcol(i) = col(i)
         i = i - 1
      ENDDO
   ELSE
      WRITE (Nout,cfmt) (col(i),i=j,k)
! 275 FORMAT (1X,5(1P,E12.4,1HR,1P,E12.4,1HI))
!     LN = (KJ+10)/10
      GOTO 400
   ENDIF
!
!     COMPLEX DOUBLE
!
 800  IF ( Iprc==1 ) THEN
      DO i = j , k
         col(i) = dcol(i)
      ENDDO
      GOTO 700
   ELSE
      WRITE (Nout,cfmt) (dcol(i),i=j,k)
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
      px(i) = p12(i)
   ENDDO
   p12(1) = iblnk
   p12(2) = iblnk
   p3 = 3
   p4 = 3
   CALL tabprt(namea)
   DO i = 1 , 5
      p12(i) = px(i)
   ENDDO
   GOTO 99999
 1300 ifin = jj - 1
   WRITE (Nout,99020) ibegn , ifin
99020 FORMAT ('0COLUMNS ',I7,6H THRU ,I7,' ARE NULL.')
   inull = 0
   line = line + 2
   IF ( line>=nlpp ) CALL page
   GOTO ihop
!
99999 END SUBROUTINE matdum
