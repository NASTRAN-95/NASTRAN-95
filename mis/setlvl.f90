
SUBROUTINE setlvl(Newnm,Numb,Oldnms,Itest,Ibit)
!
!     CREATES A NEW SUBSTRUCTURE NEWNM WHERE
!     - NEWNM IS AN INDEPENDENT SUBSTRUCTURE IF NUMB = 0
!     - NEWNM IS REDUCED FROM THE FIRST SUBSTRUCTURE IN THE ARRAY OLDNMS
!     - NEWNM RESULTS FROM COMBINING THE FIRST I SUBSTRUCTURES IN THE
!       ARRAY OLDNMS IF NUMB = I
!
!     THE OUTPUT VARIABLE ITEST TAKES ON ONE OF THE FOLLOWING VALUES
!          4  IF ONE  OR MORE SUBSTRUCTURES IN OLDNMS DO NOT EXIST
!          7  IF NEWNM ALREADY EXISTS
!          8  IF ONE OF THE SUBSTRUCTURES IN OLDNMS HAS ALREADY
!             BEEN USED IN A REDUCTION OR COMBINATION
!          1  OTHERWISE
!
!     IF ITEST IS SET TO 4, NUMB WILL BE SET TO THE NUMBER OF
!     SUBSTRUCTURES IN OLDNMS THAT DO NOT EXIST AND THE FIRST NUMB NAMES
!     IN OLDNMS WILL BE SET TO THE NAMES OF THOSE SUBSTRUCTURES THAT DO
!     NOT EXIST.  BIT IBIT OF THE FIRST MDI WORD IS SET TO INDICATE THE
!     APPROPRIATE TYPE OF SUBSTRUCTURE. IF IBIT IS ZERO NO CHANGE IS
!     MADE TO THE MDI
!
   IMPLICIT NONE
   INTEGER Buf(1) , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Iodum(8) , Mdi , Mdibl , Mdilbn , Mdipbn , Nxtdum(15)
   LOGICAL Ditup , Mdiup
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum , Ditup , Mdiup
   COMMON /zzzzzz/ Buf
   INTEGER Ibit , Itest , Numb
   INTEGER Newnm(2) , Oldnms(14)
   INTEGER andf , complf , lshift , orf
   INTEGER cs , hl , i , ib , icount , idit , iempty , imdi , inew , iold(7) , k , kk , ll , llmask , maskcs , nmsbr(2)
   EXTERNAL andf , complf , lshift , orf
   DATA iempty/4H    / , nmsbr/4HSETL , 4HVL  /
   DATA ll , cs , hl/2 , 2 , 2/
   DATA ib/1/
!
   CALL chkopn(nmsbr(1))
   Itest = 1
   CALL fdsub(Newnm(1),i)
   IF ( i/=-1 ) THEN
!
!     NEWNM ALREADY EXISTS.
!
      Itest = 7
      RETURN
   ELSE
      IF ( Numb/=0 ) THEN
!
!     MAKE SURE THAT ALL THE SUBSTRUCTURES IN OLDNMS DO EXIST.
!
         icount = 0
         DO i = 1 , Numb
            k = 2*(i-1) + 1
            CALL fdsub(Oldnms(k),iold(i))
            IF ( iold(i)<=0 ) THEN
               icount = icount + 1
               kk = 2*(icount-1) + 1
               Oldnms(kk) = Oldnms(k)
               Oldnms(kk+1) = Oldnms(k+1)
            ENDIF
         ENDDO
         IF ( icount/=0 ) THEN
            Numb = icount
!
!     ONE OR MORE OF THE SUBSTRUCTURES IN OLDNMS DO NOT EXIST.
!
            Itest = 4
            RETURN
         ENDIF
      ENDIF
      CALL crsub(Newnm(1),inew)
      IF ( Numb==0 ) RETURN
!
!     NEWNM IS NOT A BASIC SUBSTRUCTURE (LEVEL 0).
!     UPDATE NEWNM S DIRECTORY IN THE MDI.
!
      CALL fmdi(inew,imdi)
      llmask = complf(lshift(1023,20))
      Buf(imdi+ll) = orf(andf(Buf(imdi+ll),llmask),lshift(iold(1),20))
      IF ( Ibit/=0 ) Buf(imdi+ib) = orf(Buf(imdi+ib),lshift(1,Ibit))
      Mdiup = .TRUE.
!
!     UPDATE IN THE MDI THE DIRECTORIES OF THE SUBSTRUCTURES IN OLDNMS.
!
      IF ( Numb>7 ) Numb = 7
      maskcs = complf(lshift(1023,10))
      DO i = 1 , Numb
         CALL fmdi(iold(i),imdi)
         IF ( andf(Buf(imdi+hl),1023)==0 ) THEN
            Buf(imdi+hl) = orf(Buf(imdi+hl),inew)
            Mdiup = .TRUE.
            IF ( Numb==1 ) RETURN
            IF ( i==Numb ) EXIT
            Buf(imdi+cs) = orf(andf(Buf(imdi+cs),maskcs),lshift(iold(i+1),10))
         ELSE
            icount = i
            GOTO 100
         ENDIF
      ENDDO
      Buf(imdi+cs) = orf(andf(Buf(imdi+cs),maskcs),lshift(iold(1),10))
      RETURN
   ENDIF
!
!     ONE OF THE SUBSTRUCTURES IN OLDNMS HAS ALREADY BEEN USED IN A
!     REDUCTION OR COMBINATION.  REMOVE ALL CHANGES THAT HAVE BEEN MADE.
!
 100  Itest = 8
   CALL fdit(inew,idit)
   Buf(idit) = iempty
   Buf(idit+1) = iempty
   Ditup = .TRUE.
   IF ( 2*inew==Ditsiz ) Ditsiz = Ditsiz - 2
   Ditnsb = Ditnsb - 1
   CALL fmdi(inew,imdi)
   Buf(imdi+ll) = andf(Buf(imdi+ll),llmask)
   Mdiup = .TRUE.
   icount = icount - 1
   IF ( icount<1 ) RETURN
   DO i = 1 , icount
      CALL fmdi(iold(i),imdi)
      Buf(imdi+hl) = andf(Buf(imdi+hl),complf(1023))
      Buf(imdi+cs) = andf(Buf(imdi+cs),maskcs)
      Mdiup = .TRUE.
   ENDDO
END SUBROUTINE setlvl