
SUBROUTINE softoc
   IMPLICIT NONE
   INTEGER Avblks , Blksiz , Buf(1) , Dirsiz , Ditnsb , Ditsiz , Filsiz(10) , Hiblk , Ifrst , Ihalf , Item(7,1) , Line , Mach ,     &
         & Nbpc , Nbpw , Nfiles , Nitem , Nlpp , Nout
   REAL Dit , Ditbl , Ditlbn , Ditpbn , Filnam(10) , Supsiz , Sysbuf , Y(2) , Z1(6) , Z2(26)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /itemdt/ Nitem , Item
   COMMON /machin/ Mach , Ihalf
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks , Hiblk , Ifrst
   COMMON /system/ Sysbuf , Nout , Z1 , Nlpp , Y , Line , Z2 , Nbpc , Nbpw
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Buf
   INTEGER andf , klshft , lshift , rshift , sofsiz
   INTEGER blank , cs , hdr(27,4) , hl , i , ibit , im , image , imask , inum , iper , is , it , itm(27) , j , jj , jmkn , k , l ,  &
         & ll , m0009 , m1019 , m2029 , maxitm , nblk , nitm , nshft , ntype , num(10) , ps , ss , ssname(2) , type(5)
   EXTERNAL andf , lshift , rshift
!
!     SOF TABLE OF CONTENTS ROUTINE
!
!
!     THE CURRENT SUBSTRUCTURE TYPE BIT POSITIONS ARE -
!
!        NO BIT - BASIC SUBSTRUCTURE (EXCEPT IMAGE BIT)
!        BIT 30 - IMAGE SUBSTRUCTURE
!            29 - COMBINED SUBSTRUCTURE
!            28 - GUYAN REDUCTUION SUBSTRUCTURE
!            27 - MODAL REDUCTION SUBSTRUCTURE
!            26 - COMPLEX MODAL REDUCTION SUBSTRUCTURE
!
!     TO ADD A NEW SUBSTRUCTURE TYPE BIT THE FOLLOWING UPDATES ARE
!     REQUIRED.
!
!        1) INCREASE THE DEMENSION OF TYPE.
!        2) INCREASE THE VALUE OF NTYPE IN THE DATA STATEMENT.
!        3) ADD A NEW BCD TYPE VALUE TO THE DATA STATEMENT.
!
!
!     THIS ROUTINE IS CURRENTLY CODED TO HANDLE UP TO 27 SOF ITEMS
!     AUTOMATICALLY.
!     TO INCREASE THIS TO 40 ITEMS PERFORM THE FOLLOWING UPDATES.
!
!        1) CHANGE THE DIMENSION OF HDR TO (40,4)
!        2) CHANGE THE DIMENSION OF ITM TO (40)
!        3) CHANGE THE VALUE OF MAXITM IN THE DATA STATEMENT TO 40
!        4) CHANGE THE INNER GROUPS ON FORMAT 80 TO 39(A1,1X),A1
!        5) CHANGE THE INNER GROUP ON FORMAT 100 TO 39(A1,1X),A1
!
   DATA type/2HB  , 2HC  , 2HR  , 2HM  , 2HCM/
   DATA num/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA blank/4H    /
   DATA image/4HI   /
   DATA ntype/6/
   DATA maxitm/27/
!
   nitm = Nitem
   IF ( nitm>maxitm ) THEN
      nitm = maxitm
      WRITE (Nout,99001) Swm , maxitm
99001 FORMAT (A27,' 6237, THE SOFTOC ROUTINE CAN HANDLE ONLY',I4,' ITEMS.',/34X,'ADDITIONAL ITEMS WILL NOT BE SHOWN')
   ENDIF
!
!     SET UP HEADINGS AND MASKS
!
   nshft = 0
   DO i = 1 , 4
      DO j = 1 , nitm
         hdr(j,i) = klshft(Item(1,j),nshft/Nbpc)
      ENDDO
      k = nitm + 1
      IF ( k<=maxitm ) THEN
         DO j = k , maxitm
            hdr(j,i) = blank
         ENDDO
      ENDIF
      nshft = nshft + Nbpc
   ENDDO
!
   Line = Nlpp + 1
   m0009 = 1023
   m1019 = lshift(1023,10)
   m2029 = lshift(1023,20)
   imask = lshift(1,30)
!
!     LOOP THROUGH DIT
!
   DO jmkn = 1 , Ditsiz , 2
      i = (jmkn-1)/2 + 1
      CALL fdit(i,k)
      ssname(1) = Buf(k)
      ssname(2) = Buf(k+1)
      IF ( ssname(1)/=blank .OR. ssname(2)/=blank ) THEN
         CALL fmdi(i,k)
!
!     TEST TYPE BITS IN MDI
!
         DO it = 2 , ntype
            ibit = andf(Buf(k+1),lshift(1,31-it))
            IF ( ibit/=0 ) GOTO 20
         ENDDO
         it = 1
 20      is = andf(Buf(k+1),imask)
         im = blank
         IF ( is/=0 ) im = image
         ss = rshift(andf(Buf(k+1),m1019),10)
         ps = andf(Buf(k+1),m0009)
         ll = rshift(andf(Buf(k+2),m2029),20)
         cs = rshift(andf(Buf(k+2),m1019),10)
         hl = andf(Buf(k+2),m0009)
!
!     LOOP THROUGH MDI ENTRY FOR THIS SUBSTRUCTURE DETERMINING THE
!     SIZE OF EACH EXISTING ITEM.
!
         DO j = 1 , nitm
            jj = j + Ifrst - 1
            IF ( Buf(k+jj)==0 ) THEN
               itm(j) = blank
            ELSE
               inum = rshift(Buf(k+jj),Ihalf)*Blksiz
               inum = alog10(float(inum)) + .3
               itm(j) = num(inum)
               IF ( is/=0 .AND. Item(4,j)==0 ) itm(j) = num(10)
               IF ( ps/=0 .AND. is==0 .AND. Item(5,j)==0 ) itm(j) = num(10)
            ENDIF
         ENDDO
!
         Line = Line + 1
         IF ( Line>Nlpp ) THEN
            CALL page1
            Line = Line + 9 - 4
            WRITE (Nout,99002) hdr
99002       FORMAT (//,26X,90HS U B S T R U C T U R E   O P E R A T I N G   F I L E   T A B L E   O F   C O N T E N T S ,//,1H ,51X,&
                  & 26(A1,2X),A1,/1H ,51X,26(A1,2X),A1,/1H ,51X,26(A1,2X),A1,/,1H ,4X,12HSUBSTRUCTURE,35X,26(A1,2X),A1,/1H ,4X,     &
                  & 3HNO.,3X,4HNAME,4X,4HTYPE,3X,2HSS,3X,2HPS,3X,2HLL,3X,2HCS,3X,2HHL,4X,80(1H-)/)
         ENDIF
!
         WRITE (Nout,99003) i , ssname , im , type(it) , ss , ps , ll , cs , hl , (itm(l),l=1,nitm)
99003    FORMAT (2X,I6,2X,2A4,2X,A1,A2,5(1X,I4),4X,26(A1,2X),A1)
      ENDIF
   ENDDO
!
!     PRINT SOF SPACE UTILIZATION MESSAGE
!
   Line = Line + 8
   IF ( Line>Nlpp ) CALL page1
   k = sofsiz(k)
   nblk = 0
   DO i = 1 , Nfiles
      nblk = nblk + Filsiz(i)
   ENDDO
   iper = (Avblks*100)/nblk
   WRITE (Nout,99004) k , Avblks , iper , Hiblk
99004 FORMAT (//,51X,80HSIZE OF ITEM IS GIVEN IN POWERS OF TEN   (0 INDICATES DATA IS STORED IN PRIMARY),/,                         &
             &26H0*** UNUSED SPACE ON SOF =,I9,7H WORDS.,/,22X,4HOR =,I9,8H BLOCKS.,/,22X,4HOR =,I9,9H PERCENT.,/,                  &
             &26H0*** HIGHEST BLOCK USED  =,I9)
   Line = Nlpp
END SUBROUTINE softoc