
SUBROUTINE wrttrl(Filblk)
   IMPLICIT NONE
   INTEGER Fiat(3) , Fist(2) , Icfiat , Iout , Isav(6) , L15 , L8 , Lout , Mach , Nbpw
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   REAL System(175)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /l15l8 / L15 , L8
   COMMON /logout/ Lout
   COMMON /machin/ Mach
   COMMON /system/ System
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /xsortx/ Isav
   INTEGER Filbk(7) , Filblk(7)
   INTEGER andf , lshift , orf , rshift
   REAL count , fm , fn , words(4)
   INTEGER i , iendf , index , itucb , iucb , lb(2) , mask , mbit , n , name(2) , nout
   EXTERNAL andf , lshift , orf , rshift
!
   !>>>>EQUIVALENCE (System(2),Iout) , (System(24),Icfiat) , (System(40),Nbpw)
   DATA mbit/0/ , words/1.0 , 2.0 , 2.0 , 4.0/
   DATA name/4HWRTT , 4HRL  /
   DATA mask/65535/
!
!
!     IF ICFIAT= 8, WRTTRL WILL PACK SIX SIXTEEN BIT POSITIVE INTEGERS
!     INTO THREE THIRTY-TWO BIT WORDS AND STORE THEM IN THE FIAT
!     NO SUCH PACKING IF ICFIAT=11
!
!
!     SEARCH FIST FOR THE FILE
!
!     WRTTRL WILL NOT CHANGE TRAILER FOR 100 SERIES FILES
!
   IF ( Filblk(1)>100 .AND. Filblk(1)<199 ) CALL mesage(-40,Filblk(1),name)
!
!     ONLY MATGEN, OPTION 10, SENDS FILE 199 OVER HERE
!
   IF ( Filblk(1)==199 ) Filblk(1) = 101
!
!     THIS 'NEXT TO SIGN' MBIT IS SET BY SDCOMP AND SDCMPS
!
   mbit = lshift(1,Nbpw-2-(Nbpw-32))
   nout = Iout
   nout = Lout
!
!     VERIFY SQUARE AND SYMM. MATRICES
!
   IF ( L8/=0 .AND. L15/=0 ) THEN
      IF ( Filblk(7)>=mbit ) THEN
         IF ( Filblk(4)==1 .OR. Filblk(4)==6 ) THEN
            IF ( Filblk(2)/=Filblk(3) ) THEN
               CALL fname(Filblk(1),lb(1))
               WRITE (Iout,99001) Swm , lb(1) , lb(2) , Filblk(2) , Filblk(3) , Filblk(4)
99001          FORMAT (A27,', DATA BLOCK ',2A4,1H,,I9,3H BY,I8,', IS MIS-LABLED',' SQUARE OR SYMM.  (FORM=',I3,1H),/5X,             &
                      &'TURN DIAGS 1, 8 AND 15 ON FOR ERROR TRACEBACK')
               CALL sswtch(1,n)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!WKBD IF (N .NE. 0) CALL ERRTRC ('WRTTRL  ',10)
!
   n = Fist(2)*2 + 1
   DO i = 3 , n , 2
      IF ( Fist(i)==Filblk(1) ) THEN
         index = Fist(i+1) + 1
         GOTO 100
      ENDIF
   ENDDO
   CALL mesage(-11,Filblk(1),name)
!
!     IF (1) BIT 'NEXT TO SIGN BIT' IS ON IN FILBLK(7), (2) FILBLK(2)
!     AND FILBLK(3), WHICH ARE COLUMN AND ROW, ARE NON ZEROS, AND
!     FILBLK(5), WHICH IS TYPE, IS 1,2,3 OR 4, THE INCOMING TRAILER IS
!     A MATRIX TRAILER. IN THIS CASE FILBLK(7) IS CONVERTED TO A DENSITY
!     PERCENTAGE BEFORE STORING IN THE FIAT.
!
 100  IF ( Filblk(7)>=mbit ) THEN
      count = Filblk(7) - mbit
      i = Filblk(5)
      IF ( Filblk(2)/=0 .AND. Filblk(3)/=0 .AND. i>=1 .AND. i<=4 ) THEN
         fn = Filblk(2)
         fm = Filblk(3)
         Filblk(7) = (count/(fn*fm*words(i)))*1.E4 + 1.0E-3
         IF ( Filblk(7)==0 .AND. Filblk(6)/=0 ) Filblk(7) = 1
      ENDIF
   ENDIF
!
   IF ( L8/=0 ) THEN
      WRITE (nout,99002,ERR=200) Fiat(index+1) , Fiat(index+2) , (Filblk(i),i=2,7)
99002 FORMAT (' *** DIAG 8, MESSAGE -- TRAILER FOR DATA BLOCK ',2A4,2H =,6I10)
   ENDIF
   GOTO 300
 200  CALL sswtch(1,n)
   IF ( n/=0 ) THEN
      WRITE (nout,99003,ERR=300) (Filblk(i),i=2,7)
!IBMR 6/93 80 FORMAT (3H  (,6O20,1H))
99003 FORMAT (3H  (,6I8,1H))
   ENDIF
!WKBR   90 CALL ERRTRC ('WRTTRL  ',70)
!
!     IF ICFIAT IS 8, PACK THE TRAILER INFORMATION IN THE FIAT.
!     BEFORE PACKING MAKE SURE NUMBERS ARE POSITIVE AND .LE. 16 BITS.
!
!     IF ICFIAT IS 11, 6 TRAILER WORDS ARE STORED DIRECTLY INTO 4TH,
!     5TH, 6TH, 9TH, 10TH AND 11TH WORD OF A FIAT ENTRY
!
 300  IF ( Icfiat==11 ) THEN
      Fiat(index+3) = Filblk(2)
      Fiat(index+4) = Filblk(3)
      Fiat(index+5) = Filblk(4)
      Fiat(index+8) = Filblk(5)
      Fiat(index+9) = Filblk(6)
      Fiat(index+10) = Filblk(7)
   ELSE
      DO i = 2 , 7
         Filblk(i) = andf(mask,iabs(Filblk(i)))
      ENDDO
      Fiat(index+3) = orf(Filblk(3),lshift(Filblk(2),16))
      Fiat(index+4) = orf(Filblk(5),lshift(Filblk(4),16))
      Fiat(index+5) = orf(Filblk(7),lshift(Filblk(6),16))
   ENDIF
   IF ( Fiat(index)<0 ) THEN
!
!     FIND EQUIVALENCED FILES IN FIAT AND WRITE TRAILER ON THEM
!
      iucb = andf(Fiat(index),mask)
      iendf = Fiat(3)*Icfiat - 2
      DO i = 4 , iendf , Icfiat
         IF ( Fiat(i)<0 ) THEN
!
!     PICK UP UNIT CONTROL BLOCK
!
            itucb = andf(Fiat(i),mask)
            IF ( itucb==iucb ) THEN
!
!     FOUND FILE
!
               Fiat(i+3) = Fiat(index+3)
               Fiat(i+4) = Fiat(index+4)
               Fiat(i+5) = Fiat(index+5)
               IF ( Icfiat/=8 ) THEN
                  Fiat(i+8) = Fiat(index+8)
                  Fiat(i+9) = Fiat(index+9)
                  Fiat(i+10) = Fiat(index+10)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     SAVE THE TRAILER IN ISAV IF FILE IS SCRATCH 1
!     (SAVED FOR GINOFILE MODULE, SUBROUTINE GINOFL)
!
   IF ( Filblk(1)/=301 ) RETURN
   Isav(1) = Fiat(index+3)
   Isav(2) = Fiat(index+4)
   Isav(3) = Fiat(index+5)
   IF ( Icfiat/=8 ) THEN
      Isav(4) = Fiat(index+8)
      Isav(5) = Fiat(index+9)
      Isav(6) = Fiat(index+10)
   ENDIF
   RETURN
!
!
   ENTRY rdtrl(Filbk)
!     ===================
!
!     RDTRL WILL UNPACK THE THREE WORDS STORED IN THE FIAT AND RETURN
!     THE SIX WORDS OF TRAILER INFORMATION
!
!
!     SEARCH THE FIST FOR THE FILE
!
   n = Fist(2)*2 + 1
   DO i = 3 , n , 2
      IF ( Fist(i)==Filbk(1) ) THEN
         index = Fist(i+1) + 1
         GOTO 400
      ENDIF
   ENDDO
!
!     FILE WAS NOT FOUND, SET THE FILE NAME NEGATIVE
!
   Filbk(1) = -iabs(Filbk(1))
   RETURN
!
!     CHECK FIAT ENTRY 8 OR 11 WORDS PER ENTRY
!
 400  IF ( Icfiat==11 ) THEN
!
!     11 WORD ENTRY, TRAILER NOT PACKED
!
      Filbk(2) = Fiat(index+3)
      Filbk(3) = Fiat(index+4)
      Filbk(4) = Fiat(index+5)
      Filbk(5) = Fiat(index+8)
      Filbk(6) = Fiat(index+9)
      Filbk(7) = Fiat(index+10)
   ELSE
!
!     8 WORD ENTRY, UNPACK THE TRAILER INFORMATION
!
      Filbk(2) = rshift(Fiat(index+3),16)
      Filbk(3) = andf(Fiat(index+3),mask)
      Filbk(4) = rshift(Fiat(index+4),16)
      Filbk(5) = andf(Fiat(index+4),mask)
      Filbk(6) = rshift(Fiat(index+5),16)
      Filbk(7) = andf(Fiat(index+5),mask)
   ENDIF
!
END SUBROUTINE wrttrl