
SUBROUTINE matwrt(Ifile,Xname,Xitem,Lcore)
   IMPLICIT NONE
   REAL Col(1) , Head1(96) , Head2(96)
   DOUBLE PRECISION Dcol(1)
   INTEGER Incr , Inx(6) , Inx1(2) , It , K , L , Line , Nlpp , Otpe , Sysbuf
   COMMON /output/ Head1 , Head2
   COMMON /system/ Sysbuf , Otpe , Inx , Nlpp , Inx1 , Line
   COMMON /unpakx/ It , K , L , Incr
   COMMON /zzzzzz/ Col
   INTEGER Ifile , Lcore
   REAL Xitem
   REAL Xname(2)
   REAL blank , bstr , cont , dx , em , form(18) , re , su , type(10) , uctu , xinue , xit
   INTEGER i , ia(7) , ia7a , ia7b , ia7c , ibegn , if , ifin , ihop , inull , it1 , itest , j , jj , lcol , namea , ncol , nrow
!
   EQUIVALENCE (Col(1),Dcol(1))
   DATA type/4HREAL , 4H     , 4HDB   , 4HPREC , 4HCOMP , 4HLEX  , 4HCMP  , 4HD.P. , 4HILL  , 4HDEFN/
   DATA form/4HSQUA , 4HRE   , 4HRECT , 4HANG  , 4HDIAG , 4HONAL , 4HLOW  , 4HTRI  , 4HUPP  , 4HTRI  , 4HSYME , 4HTRIC , 4HVECT ,   &
       &4HOR   , 4HIDEN , 4HITY  , 4HILL  , 4HDEFN/
   DATA blank , su , bstr , uctu , re , xit , em , cont/4H     , 4H  SU , 4HBSTR , 4HUCTU , 4HRE   , 4H  IT , 4HEM   , 4HCONT/
   DATA xinue , dx/4HINUE , 4HD   /
!
!
!     TRANSFER MATRIX FORM SOF TO GINO
!
   CALL mtrxi(Ifile,Xname,Xitem,0,itest)
   IF ( itest/=1 ) RETURN
   ia(1) = Ifile
   CALL rdtrl(ia(1))
!
   DO i = 1 , 96
      Head2(i) = blank
   ENDDO
   Head2(1) = su
   Head2(2) = bstr
   Head2(3) = uctu
   Head2(4) = re
   Head2(5) = Xname(1)
   Head2(6) = Xname(2)
   Head2(7) = xit
   Head2(8) = em
   Head2(9) = Xitem
   Head2(11) = cont
   Head2(12) = xinue
   Head2(13) = dx
   namea = Ifile
   lcol = Lcore - Sysbuf
   Incr = 1
   CALL gopen(namea,Col(lcol+1),0)
   It = ia(5)
   IF ( It<=0 .OR. It>4 ) It = 5
   if = ia(4)
   IF ( if<=0 .OR. if>8 ) if = 9
   ncol = ia(2)
   nrow = ia(3)
   IF ( if==7 ) ncol = ia(3)
   CALL page1
   WRITE (Otpe,99001) Xname , Xitem , type(2*It-1) , type(2*It) , ncol , nrow , form(2*if-1) , form(2*if)
99001 FORMAT (1H0,6X,13HSUBSTRUCTURE ,2A4,6H ITEM ,A4,6H IS A ,2A4,1X,I6,10H COLUMN X ,I6,5H ROW ,2A4,8H MATRIX.)
   IF ( It/=5 .AND. if/=9 .AND. ncol/=0 .AND. nrow/=0 ) THEN
      IF ( if<8 ) THEN
         IF ( if==3 .OR. if==7 ) THEN
            ncol = 1
            nrow = ia(3)
         ENDIF
         inull = 0
         it1 = 5
         IF ( It==1 .OR. It==3 ) it1 = 9
         ASSIGN 200 TO ihop
         jj = 1
         GOTO 100
      ELSEIF ( if==8 ) THEN
!
         WRITE (Otpe,99002)
99002    FORMAT (16H0IDENTITY MATRIX)
      ENDIF
   ENDIF
   CALL close(namea,1)
!
!     FUNNY MATRIX -- TABLE PRINT IT
!
   CALL tabprt(namea)
   GOTO 600
 100  K = 0
   L = 0
   CALL unpack(*300,namea,Col)
   IF ( inull==1 ) GOTO 700
 200  nrow = L - K + 1
   IF ( if==3 ) THEN
      WRITE (Otpe,99003) K , L
99003 FORMAT (30H0DIAGONAL ELEMENTS FOR COLUMNS,I6,3H TO,I7,4H ARE,/1H0)
      Line = Line + 2
   ELSEIF ( if==7 ) THEN
      WRITE (Otpe,99004) K , L
99004 FORMAT (25H0ROW ELEMENTS FOR COLUMNS,I6,4H TO ,I6,4H ARE,/1H0)
      Line = Line + 2
   ELSE
      WRITE (Otpe,99005) jj , K , L
99005 FORMAT (8H0COLUMN ,I6,5X,6H ROWS ,I6,6H THRU ,I6,5X,50(1H-),/1H )
      Line = Line + 3
      IF ( Line>=Nlpp ) CALL page
      IF ( It>2 ) nrow = 2*nrow
   ENDIF
   K = 0
   DO
      j = K + 1
      IF ( j>nrow ) GOTO 400
      K = j + it1
      IF ( K>nrow ) K = nrow
      IF ( It==2 ) THEN
!
!     REAL DOUBLE PRECISION
!
         WRITE (Otpe,99006) (Dcol(i),i=j,K)
99006    FORMAT (1P,6D22.14)
      ELSEIF ( It==3 ) THEN
!
!     COMPLEX SINGLE
!
         WRITE (Otpe,99007) (Col(i),i=j,K)
99007    FORMAT (5(1P,E12.4,1H+,1P,E12.4,1HI))
      ELSEIF ( It==4 ) THEN
!
!     COMPLEX DOUBLE
!
         WRITE (Otpe,99008) (Dcol(i),i=j,K)
99008    FORMAT (3(1P,D20.12,1H+,1P,D20.12,2HI ))
      ELSE
!
!     REAL SINGLE PRECISION
!
         WRITE (Otpe,99009) (Col(i),i=j,K)
99009    FORMAT (1X,1P,10E13.5)
      ENDIF
      Line = Line + 1
      IF ( Line>=Nlpp ) CALL page
   ENDDO
 300  IF ( inull/=1 ) THEN
      ibegn = jj
      inull = 1
   ENDIF
 400  jj = jj + 1
   IF ( jj<=ncol ) GOTO 100
   ASSIGN 500 TO ihop
   IF ( inull==1 ) GOTO 700
 500  CALL close(namea,1)
   WRITE (Otpe,99010) ia(6)
99010 FORMAT (53H0THE NUMBER OF NON-ZERO WORDS IN THE LONGEST RECORD =,I8)
   ia7a = ia(7)/100
   ia7c = ia(7) - 100*ia7a
   ia7b = ia7c/10
   ia7c = ia7c - 10*ia7b
   WRITE (Otpe,99011) ia7a , ia7b , ia7c
99011 FORMAT (31H0THE DENSITY OF THIS MATRIX IS ,I3,1H.,I1,I1,9H PERCENT.)
 600  RETURN
 700  ifin = jj - 1
   WRITE (Otpe,99012) ibegn , ifin
99012 FORMAT (9H0COLUMNS ,I7,6H THRU ,I7,10H ARE NULL.)
   inull = 0
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
   GOTO ihop
END SUBROUTINE matwrt
