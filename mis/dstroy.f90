
SUBROUTINE dstroy(Name,Itest,Image,Imore,Lim)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Ifrst , Iodum(8) , Item(7,1) , Mdi , Mdibl ,&
         & Mdilbn , Mdipbn , Nitem , Nxtdum(15)
   LOGICAL Ditup , Mdiup
   REAL Sys(3)
   COMMON /itemdt/ Nitem , Item
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum , Ditup , Mdiup
   COMMON /sys   / Blksiz , Dirsiz , Sys , Ifrst
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Itest , Lim
   INTEGER Image(1) , Imore(1) , Name(2)
!
! Local variable declarations
!
   INTEGER andf , complf , lshift , orf , rshift
   INTEGER cs , hl , i , ibl , icheck , icount , ics , idit , iempty , ihere , iis , ill , imdi , imtop , indcs , index , indhl ,   &
         & indis , indll , indps , indsbr , indss , ips , iret1 , iret2 , is , isave , iss , isv , itm , itop , j , jdit , ll ,     &
         & maskl , maskm , nmsbr(2) , ps , ss
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
!
!     DESTROYS THE SUBSTRUCTURE NAME BY DELETING ITS DIRECTORY FROM THE
!     MDI AND ITS NAME FROM THE DIT.  NO OPERATION WILL TAKE PLACE IF
!     NAME IS AN IMAGE SUBSTRUCTURE.  IF NAME IS A SECONDARY SUBSTRUC-
!     TURE, IT IS DELETED FROM THE LIST OF SECONDARY SUBSTRUCTURES TO
!     WHICH IT BELONGS, AND ITS IMAGE CONTRIBUTING TREE IS DESTROYED.
!     IF NAME IS A PRIMARY SUBSTRUCTURE, ALL ITS SECONDARY SUBSTRUCTURES
!     ARE ALSO DESTROYED.  IN ALL CASES, ALL THE SUBSTRUCTURES DERIVED
!     FROM THE SUBSTRUCTURE BEING DESTROYED ARE ALSO DESTROYED, AND
!     CONNECTIONS WITH OTHER SUBSTRUCTURES ARE DELETED.
!
!     THE BLOCKS OCCUPIED BY THE ITEM ARE RETURNED TO THE LIST OF FREE
!     BLOCKS IF THEY BELONG TO THE SPECIFIED SUBSTRUCTURE
!
!     THE OUTPUT VARIABLE ITEST TAKES ONE OF THE FOLLOWING VALUES.
!        1  NORMAL RETURN
!        4  IF NAME DOES NOT EXIST
!        6  IF NAME IS AN IMAGE SUBSTRUCTURE
!
   DATA ps , ss , is , ll , cs , hl/1 , 1 , 1 , 2 , 2 , 2/
   DATA iempty/4H    /
   DATA indsbr/3/ , nmsbr/4HDSTR , 4HOY  /
!
   CALL chkopn(nmsbr(1))
   Itest = 1
   itop = 0
   imtop = 0
   CALL fdsub(Name(1),index)
   IF ( index==-1 ) THEN
!
!     NAME DOES NOT EXIST.
!
      Itest = 4
      RETURN
   ELSE
      maskm = complf(lshift(1023,10))
      maskl = complf(lshift(1023,20))
!                           1023 = 2**10 - 1
!
!     SAVE ALL CONNECTIONS WITH OTHER SUBSTRUCTURES.
!
      CALL fmdi(index,imdi)
   ENDIF
 100  i = Buf(imdi+ps)
   indps = andf(i,1023)
   indss = rshift(andf(i,1048575),10)
!                           1048575 = 2**20 - 1
   indis = andf(i,1073741824)
!                    1073741824 = 2**30
   i = Buf(imdi+ll)
   indhl = andf(i,1023)
   indcs = rshift(andf(i,1048575),10)
   indll = rshift(andf(i,1073741823),20)
!                           1073741823 = 2**30 - 1
   IF ( indis>0 ) GOTO 1500
   IF ( indps==0 ) THEN
!
!     PRIMARY SUBSTRUCTURE.
!     RETURN THE BLOCKS USED BY ALL ITEMS TO THE LIST OF FREE BLOCKS.
!
      DO j = Ifrst , Dirsiz
         ibl = andf(Buf(imdi+j),65535)
!                            65535 = 2**16 - 1
         IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
      ENDDO
      IF ( indss==0 ) GOTO 700
!
!     THE PRIMARY SUBSTRUCTURE BEING DESTROYED HAS SECONDARY EQUIVALENT
!     SUBSTRUCTURES.  MUST DESTROY ALL OF THEM.
!
      ASSIGN 1200 TO iret1
      ASSIGN 400 TO iret2
      isv = indss
      GOTO 300
   ELSE
      ASSIGN 200 TO iret1
      GOTO 1100
   ENDIF
!
!     REMOVE INDEX FROM THE LIST OF SUBSTRUCTURES THAT ARE SECONDARY TO
!     INDPS.
!
 200  isave = indps
   DO
      CALL fmdi(isave,imdi)
      isave = rshift(andf(Buf(imdi+ss),1048575),10)
      IF ( isave==0 ) THEN
         ASSIGN 600 TO iret2
         GOTO 1300
      ELSEIF ( isave==index ) THEN
         Buf(imdi+ss) = orf(andf(Buf(imdi+ss),maskm),lshift(indss,10))
         Mdiup = .TRUE.
         IF ( indll==0 ) GOTO 600
         ill = indll
         indll = 0
         isave = index
         ASSIGN 600 TO iret2
         GOTO 1300
      ENDIF
   ENDDO
 300  isave = isv
   CALL fmdi(isave,imdi)
   isv = rshift(andf(Buf(imdi+ss),1048575),10)
   iis = andf(Buf(imdi+is),1073741824)
   IF ( iis>0 ) GOTO 500
!
!     THE SECONDARY SUBSTRUCTURE IS NOT AN IMAGE SUBSTRUCTURE.  ADD ITS
!     INDEX TO THE LIST (IMORE) OF SUBSTRUCTURES TO BE DESTROYED LATER.
!
   itop = itop + 1
   IF ( itop>Lim ) THEN
      CALL mesage(-8,0,nmsbr)
      GOTO 99999
   ELSE
      Imore(itop) = isave
      GOTO 1100
   ENDIF
!
!     UPDATE THE MDI OF THE SECONDARY SUBSTRUCTURE WITH INDEX ISAVE.
!
 400  CALL fmdi(isave,imdi)
   Buf(imdi+ps) = 0
   Buf(imdi+ll) = andf(Buf(imdi+ll),maskl)
   DO j = Ifrst , Dirsiz
      Buf(imdi+j) = 0
   ENDDO
   Mdiup = .TRUE.
 500  IF ( isv/=0 ) GOTO 300
!
!     BACK TO THE SUBSTRUCTURE WITH INDEX  INDEX .
!     DELETE ITS DIRECTORY FROM THE MDI.
!
 600  CALL fmdi(index,imdi)
 700  DO j = 1 , Dirsiz
      Buf(imdi+j) = 0
   ENDDO
   Mdiup = .TRUE.
!
!     DELETE SUBSTRUCTURE NAME FROM THE DIT.
!
   CALL fdit(index,jdit)
   Buf(jdit) = iempty
   Buf(jdit+1) = iempty
   Ditup = .TRUE.
   IF ( index*2==Ditsiz ) Ditsiz = Ditsiz - 2
   Ditnsb = Ditnsb - 1
   IF ( indcs==0 ) GOTO 1000
!
!     DELETE LINK THROUGH COMBINED SUBSTRUCTURES, AND REMOVE ITEMS
!     CREATED AS A RESULTS OF THE COMBINE OR REDUCE.
!     THESE ITEMS WILL BE RETURNED TO THE LIST OF FREE BLOCKS.
!
 800  IF ( indcs==index ) GOTO 1000
   CALL fmdi(indcs,imdi)
   indcs = rshift(andf(Buf(imdi+cs),1048575),10)
 900  Buf(imdi+hl) = andf(Buf(imdi+hl),complf(1023))
   Buf(imdi+cs) = andf(Buf(imdi+cs),maskm)
   DO j = 1 , Nitem
      IF ( Item(6,j)/=0 ) THEN
         itm = j + Ifrst - 1
         ibl = andf(Buf(imdi+itm),65535)
         IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
         Buf(imdi+itm) = 0
      ENDIF
   ENDDO
   Mdiup = .TRUE.
   IF ( indcs/=0 ) GOTO 800
!
!     ERROR MESSAGES.
!
   CALL errmkn(indsbr,8)
   CALL mesage(-8,0,nmsbr)
   GOTO 99999
 1000 IF ( indll==0 ) THEN
      IF ( indhl==0 ) THEN
         IF ( itop==0 ) RETURN
!
!     MORE SUBSTRUCTURES TO DESTROY.
!
         index = Imore(itop)
         itop = itop - 1
         CALL fmdi(index,imdi)
      ELSE
!
!     A HIGHER LEVEL SUBSTRUCTURE WAS DERIVED FROM THE ONE BEING
!     DESTROYED. DESTROY THE HIGHER LEVEL SUBSTRUCTURE.
!
         index = indhl
         CALL fmdi(index,imdi)
         Buf(imdi+ll) = andf(Buf(imdi+ll),maskl)
         Mdiup = .TRUE.
      ENDIF
      GOTO 100
   ELSE
!
!     SUBSTRUCTURE WAS THE RESULT OF COMBINING LOWER LEVEL SUBSTRUCTURES
!     TOGETHER.  UPDATE THE MDI ACCORDINGLY.
!
      CALL fmdi(indll,imdi)
      indcs = rshift(andf(Buf(imdi+cs),1048575),10)
      index = indll
      indll = 0
      IF ( indcs==0 ) indcs = index
      GOTO 900
   ENDIF
!
!     INTERNAL SUBROUTINE.
!     RETURN TO THE LIST OF FREE BLOCKS THE BLOCKS USED BY A
!     SECONDARY SUBSTRUCTURE.
!     THESE BLOCKS INCLUDE THE FOLLOWING ITEMS
!
!     ITEMS COPIED DURING A EQUIV OPERATION
!     SOLUTION ITEMS
!     ITEMS PRODUCED BY A COMBINE OR REDUCE OPERATION
!
 1100 DO j = 1 , Nitem
      IF ( Item(5,j)/=0 ) THEN
         itm = j + Ifrst - 1
         ibl = andf(Buf(imdi+itm),65535)
         IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
         Buf(imdi+itm) = 0
      ENDIF
   ENDDO
   GOTO iret1
!
!     INTERNAL SUBROUTINE.
!     BUILD A LIST IMAGE OF ALL THE IMAGE SUBSTRUCTURES CONTRIBUTING TO
!     THE SECONDARY SUBSTRUCTURE WITH INDEX ISAVE, AND DELETE EACH IMAGE
!     SUBSTRUCTURE FROM THE LIST OF SECONDARY SUBSTRUCTURES TO WHICH IT
!     BELONGS.
!
 1200 CALL fmdi(isave,imdi)
   ill = rshift(andf(Buf(imdi+ll),1073741823),20)
   IF ( ill==0 ) GOTO iret2
 1300 imtop = 1
   Image(imtop) = ill
   icount = 1
   ihere = Image(icount)
   DO
      CALL fmdi(ihere,imdi)
      i = Buf(imdi+ps)
      ips = andf(i,1023)
      iss = rshift(andf(i,1048575),10)
      iis = andf(i,1073741824)
      i = Buf(imdi+ll)
      ill = rshift(andf(i,1073741823),20)
      ics = rshift(andf(i,1048575),10)
      IF ( iis==0 ) EXIT
!
!     DELETE THE SUBSTRUCTURE WITH INDEX IHERE FROM THE MDI AND THE DIT.
!     RETURN THE BLOCKS USED BY THE IMAGE SUBSTRUCTURE TO THE LIST OF
!     FREE BLOCKS.  THIS INCLUDES THE FOLLOWING ITEMS
!
!     ITEMS COPIED DURING A EQUIV OPERATION
!     SOLUTION ITEMS
!
      DO j = 1 , Nitem
         IF ( Item(4,j)/=0 ) THEN
            itm = j + Ifrst - 1
            ibl = andf(Buf(imdi+itm),65535)
            IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
            Buf(imdi+itm) = 0
         ENDIF
      ENDDO
      DO j = 1 , Dirsiz
         Buf(imdi+j) = 0
      ENDDO
      Mdiup = .TRUE.
      CALL fdit(ihere,idit)
      Buf(idit) = iempty
      Buf(idit+1) = iempty
      Ditup = .TRUE.
      IF ( ihere*2==Ditsiz ) Ditsiz = Ditsiz - 2
      Ditnsb = Ditnsb - 1
!
!     DELETE POINTERS TO IHERE.
!
      icheck = ips
      DO
         CALL fmdi(icheck,imdi)
         icheck = rshift(andf(Buf(imdi+ss),1048575),10)
         IF ( icheck==0 ) EXIT
         IF ( icheck==ihere ) THEN
            Buf(imdi+ss) = orf(andf(Buf(imdi+ss),maskm),lshift(iss,10))
            Mdiup = .TRUE.
            EXIT
         ENDIF
      ENDDO
!
!     ARE THERE MORE SUBSTRUCTURES TO ADD TO THE LIST IMAGE
!
      IF ( ill/=0 ) THEN
         DO j = 1 , imtop
            IF ( Image(j)==ill ) GOTO 1350
         ENDDO
         imtop = imtop + 1
         Image(imtop) = ill
      ENDIF
 1350 IF ( ics/=0 ) THEN
         DO j = 1 , imtop
            IF ( Image(j)==ics ) GOTO 1400
         ENDDO
         imtop = imtop + 1
         IF ( imtop>Lim ) THEN
            CALL mesage(-8,0,nmsbr)
            GOTO 99999
         ELSE
            Image(imtop) = ics
         ENDIF
      ENDIF
!
!     ARE THERE MORE SUBSTRUCTURES ON THE LIST IMAGE
!
 1400 IF ( icount==imtop ) GOTO iret2
      icount = icount + 1
      ihere = Image(icount)
   ENDDO
!
!     NAME IS AN IMAGE SUBSTRUCTURE.
!
 1500 Itest = 6
   RETURN
99999 RETURN
END SUBROUTINE dstroy
