
SUBROUTINE seteq(Name1,Name2,Prefx,Dry2,Itest,Imore,Lim)
!
!     SETS THE SUBSTRUCTURE NAME2 EQUIVALENT TO THE SUBSTRUCTURE NAME1.
!     THE OUTPUT VARIABLE ITEST TAKES ON ONE OF THE FOLLOWING VALUES
!
!         4  IF NAME1 DOES NOT EXIST
!         8  IF DRY DOES NOT EQUAL ZERO AND NAME2 OR ONE OF THE NEW
!            NAMES ALREADY EXISTS
!         9  IF DRY IS EQUAL TO ZERO AND NAME2 OR ONE OF THE NEW NAMES
!            DOES NOT EXIST
!         1  OTHERWISE
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Dum(36) , Ifrst , Ihalf , Io , Iodum(7) ,   &
         & Item(7,1) , Jhalf , Mach , Mdi , Mdibl , Mdilbn , Mdipbn , Nbpc , Nbpw , Nbuff , Ncpw , Nitem , Nout , Nxtdum(15) ,      &
         & Subtit(96) , Sys(3) , Title(96)
   LOGICAL Ditup , Mdiup
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /itemdt/ Nitem , Item
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /output/ Title , Subtit
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Io , Iodum , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum , Ditup ,  &
                 & Mdiup
   COMMON /sys   / Blksiz , Dirsiz , Sys , Ifrst
   COMMON /system/ Nbuff , Nout , Dum , Nbpc , Nbpw , Ncpw
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Dry2 , Itest , Lim , Prefx
   INTEGER Imore(1) , Name1(2) , Name2(2)
!
! Local variable declarations
!
   INTEGER andf , complf , khrfn1 , klshft , krshft , lshift , orf , rshift
   INTEGER bb , cs , dry , first , first2 , hl , i , ib , ibs , ics , idit , iempty , ifnd , ihl , ill , image , imdi , inc , ind1 ,&
         & ind2 , indcs , indll , indsbr , inxt , ipp , ips , iptr , ird , irdbl , iret , isave(50) , iss , itm , itop , iwant ,    &
         & iwrt , iwrtbl , j , k , kk , ll , mask , maskbb , maskll , maskss , max , min , namnew(2) , newblk , next , nmsbr(2) ,   &
         & numb , ps , rest , rest2 , ss
   LOGICAL more
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
   DATA ps , ss , ib , ll , cs , hl , bb , ird , iwrt , indsbr/1 , 1 , 1 , 2 , 2 , 2 , 1 , 1 , 2 , 15/
   DATA iempty , mask , nmsbr/4H     , 4HMASK , 4HSETE , 4HQ   /
!
   CALL chkopn(nmsbr(1))
   IF ( Nitem+Ifrst-1>50 ) THEN
      CALL errmkn(indsbr,10)
      GOTO 1700
   ELSE
      dry = Dry2
      Itest = 1
      CALL fdsub(Name1(1),ind1)
      IF ( ind1==-1 ) THEN
!
!     ERROR CONDITIONS
!
         Itest = 4
         GOTO 99999
      ELSE
         mask = andf(mask,2**(Nbpw-4*Nbpc)-1)
         maskss = complf(lshift(1023,10))
         maskll = complf(lshift(1023,20))
         maskbb = lshift(1023,20)
!
!     IF NAME2 EXISTS - VERIFY THAT IT IS MARKED EQUIVALENT TO NAME1.
!     NAME2 MAY ALREADY EXIST FOR RUN=GO OR OPTIONS=PA
!
         CALL fdsub(Name2(1),ind2)
         IF ( ind2/=-1 ) THEN
            dry = 0
!
            CALL fmdi(ind2,imdi)
            ips = andf(Buf(imdi+ps),1023)
            IF ( ips==0 ) THEN
               Itest = 8
               GOTO 99999
            ELSEIF ( ips/=ind1 ) THEN
               CALL fmdi(ind1,imdi)
               ipp = andf(Buf(imdi+ps),1023)
               IF ( ips/=ipp ) THEN
                  Itest = 8
                  GOTO 99999
               ENDIF
            ENDIF
         ENDIF
!
!     STEP 1.  MAKE A LIST OF ALL THE SUBSTRUCTURES CONTRIBUTING TO THE
!     SUBSTRUCTURE NAME1, AND STORE IT IN THE ARRAY IMORE
!
         itop = 1
         Imore(itop) = ind1
         iptr = 1
      ENDIF
   ENDIF
 100  CALL fmdi(ind1,imdi)
   i = Buf(imdi+ll)
   indll = rshift(andf(i,1073741823),20)
   indcs = rshift(andf(i,1048575),10)
   IF ( indll/=0 ) THEN
      DO j = 1 , itop
         IF ( Imore(j)==indll ) GOTO 200
      ENDDO
      itop = itop + 1
      IF ( itop>Lim ) THEN
         k = -8
         GOTO 1700
      ELSE
         Imore(itop) = indll
      ENDIF
   ENDIF
 200  IF ( indcs/=0 .AND. iptr/=1 ) THEN
      DO j = 1 , itop
         IF ( Imore(j)==indcs ) GOTO 300
      ENDDO
      itop = itop + 1
      IF ( itop>Lim ) THEN
         k = -8
         GOTO 1700
      ELSE
         Imore(itop) = indcs
      ENDIF
   ENDIF
 300  IF ( iptr/=itop ) THEN
      iptr = iptr + 1
      ind1 = Imore(iptr)
      GOTO 100
   ENDIF
!
!     STEP 2.  CREATE AN IMAGE SUBSTRUCTURE FOR EACH SUBSTRUCTURE IN THE
!     ARRAY IMORE, AND STORE ITS INDEX IN THE ARRAY IMAGE.  NOTE THAT
!     SINCE IMORE(1) CONTAINS THE INDEX OF NAME1, IMAGE(1) WILL CONTAIN
!     THE INDEX OF NAME2
!     FOR EACH NEW NAME CHECK THAT MAKING ROOM FOR THE PREFIX DOES NOT
!     TRUNCATE THE NAME
!
 400  IF ( iptr/=1 ) THEN
      CALL fdit(ind1,idit)
      first = klshft(krshft(Prefx,Ncpw-1),Ncpw-1)
      rest = klshft(krshft(Buf(idit),Ncpw-3),Ncpw-4)
      namnew(1) = orf(orf(first,rest),mask)
      first = klshft(krshft(Buf(idit),Ncpw-4),Ncpw-1)
      rest = klshft(krshft(Buf(idit+1),Ncpw-3),Ncpw-4)
      namnew(2) = orf(orf(first,rest),mask)
      IF ( khrfn1(iempty,4,Buf(idit+1),4)/=iempty ) WRITE (Nout,99001) Uwm , namnew , Buf(idit) , Buf(idit+1)
99001 FORMAT (A25,' 6236, DURING THE CREATION OF A NEW IMAGE SUBSTRUC','TURE NAMED ',2A4,' THE LAST CHARACTER ',/5X,                &
             &'OF SUBSTRUCTURE NAMED ',2A4,' WAS TRUNCATED TO MAKE ROOM',' FOR THE PREFIX.')
      CALL fdsub(namnew(1),i)
   ELSE
      CALL fdsub(Name2(1),i)
   ENDIF
   IF ( dry/=0 ) THEN
      IF ( i/=-1 ) THEN
         iptr = iptr + 1
         IF ( iptr<=itop ) THEN
            DO i = iptr , itop
               image = Imore(Lim+i)
               CALL fdit(image,idit)
               Buf(idit) = iempty
               Buf(idit+1) = iempty
               Ditup = .TRUE.
            ENDDO
         ENDIF
         Itest = 8
         GOTO 99999
      ELSEIF ( iptr/=1 ) THEN
         CALL crsub(namnew(1),i)
      ELSE
         CALL crsub(Name2(1),i)
      ENDIF
   ELSEIF ( i==-1 ) THEN
      Itest = 9
      GOTO 99999
   ENDIF
   Imore(iptr+Lim) = i
   IF ( iptr==1 ) THEN
!
!     STEP 3.  BUILD THE MDI OF NAME2, AND OF ALL IMAGE SUBSTRUCTURES
!
      ind2 = i
   ELSE
      iptr = iptr - 1
      ind1 = Imore(iptr)
      GOTO 400
   ENDIF
 500  CALL fmdi(ind1,imdi)
   DO j = 1 , Dirsiz
      isave(j) = Buf(imdi+j)
   ENDDO
!
!     SET THE SS ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND1
!
   IF ( dry/=0 ) THEN
      Buf(imdi+ss) = orf(andf(Buf(imdi+ss),maskss),lshift(ind2,10))
      Mdiup = .TRUE.
   ENDIF
   CALL fmdi(ind2,imdi)
   IF ( dry==0 ) GOTO 1300
   i = isave(ps)
!
!     SET THE PS ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
   ips = andf(i,1023)
   IF ( ips==0 ) THEN
      Buf(imdi+ps) = ind1
   ELSE
      Buf(imdi+ps) = ips
   ENDIF
!
!     SET THE SS ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
   iss = rshift(andf(i,1048575),10)
   IF ( iss/=0 ) Buf(imdi+ss) = orf(andf(Buf(imdi+ss),maskss),lshift(iss,10))
!
!     SET THE BB ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
   ibs = andf(i,maskbb)
   Buf(imdi+bb) = orf(andf(Buf(imdi+bb),maskll),ibs)
   i = isave(ll)
!
!     SET THE HL ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
   IF ( iptr==1 ) GOTO 900
   ihl = andf(i,1023)
   IF ( ihl==0 ) GOTO 700
   ASSIGN 600 TO iret
   iwant = ihl
   GOTO 1100
 600  Buf(imdi+hl) = ifnd
!
!     SET THE CS ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
 700  ics = rshift(andf(i,1048575),10)
   IF ( ics==0 ) GOTO 900
   ASSIGN 800 TO iret
   iwant = ics
   GOTO 1100
 800  Buf(imdi+cs) = orf(andf(Buf(imdi+cs),maskss),lshift(ifnd,10))
!
!     SET THE LL ENTRY FOR THE SUBSTRUCTURE WITH INDEX IND2
!
 900  ill = rshift(andf(i,1073741823),20)
   IF ( ill==0 ) GOTO 1200
   ASSIGN 1000 TO iret
   iwant = ill
   GOTO 1100
 1000 Buf(imdi+ll) = orf(andf(Buf(imdi+ll),maskll),lshift(ifnd,20))
   GOTO 1200
!
!     FIND THE INDEX OF THE IMAGE SUBSTRUCTURE TO THE SUBSTRUCTURE WITH
!     INDEX IWANT.  STORE THE FOUND INDEX IN IFND
!
 1100 DO k = 1 , itop
      IF ( Imore(k)==iwant ) THEN
         ifnd = Imore(Lim+k)
         GOTO iret
      ENDIF
   ENDDO
   CALL errmkn(indsbr,3)
   GOTO 1600
!
!     SET THE POINTERS OF THE ITEMS BELONGING TO THE SUBSTRUCTURE WITH
!     INDEX IND2
!
 1200 DO j = Ifrst , Dirsiz
      Buf(imdi+j) = 0
   ENDDO
 1300 IF ( iptr==1 ) THEN
!
!     SECONDARY SUBSTRUCTURE - SET POINTERS TO SHARED ITEMS
!
      DO j = 1 , Nitem
         IF ( Item(5,j)==0 ) THEN
            itm = j + Ifrst - 1
            IF ( Buf(imdi+itm)==0 ) Buf(imdi+itm) = isave(itm)
         ENDIF
      ENDDO
   ELSE
!
!     IMAGE SUBSTRUCTURE - SET POINTERS TO SHARED ITEMS AND SET IB BIT
!
      DO j = 1 , Nitem
         IF ( Item(4,j)==0 ) THEN
            itm = j + Ifrst - 1
            IF ( Buf(imdi+itm)==0 ) Buf(imdi+itm) = isave(itm)
         ENDIF
      ENDDO
      Buf(imdi+ib) = orf(Buf(imdi+ib),lshift(1,30))
   ENDIF
!
!     COPY APPROPRIATE ITEMS OF NAME1 AND WRITE THEM FOR
!     NAME2 AFTER CHANGING NAME1 TO NAME2 AND INSERTING THE NEW PREFIX
!     TO THE NAMES OF ALL CONTRIBUTING SUBSTRUCTURES
!
   DO j = 1 , Nitem
      IF ( Item(3,j)==0 ) CYCLE
      kk = j + Ifrst - 1
      IF ( Buf(imdi+kk)/=0 ) CYCLE
      irdbl = andf(isave(kk),Jhalf)
      IF ( irdbl/=0 .AND. irdbl/=Jhalf ) THEN
         CALL sofio(ird,irdbl,Buf(Io-2))
         CALL fdit(ind2,idit)
         Buf(Io+1) = Buf(idit)
         Buf(Io+2) = Buf(idit+1)
         CALL getblk(0,iwrtbl)
         IF ( iwrtbl==-1 ) GOTO 1600
         newblk = iwrtbl
         numb = Item(3,j)/1000000
         min = (Item(3,j)-numb*1000000)/1000
         inc = Item(3,j) - numb*1000000 - min*1000
         numb = Buf(Io+numb)
         IF ( numb<=1 .AND. ill==0 .AND. iptr==1 ) THEN
!
!     BASIC SUBSTRUCTURE
!
            Buf(Io+min) = Name2(1)
            Buf(Io+min+1) = Name2(2)
            more = .FALSE.
            GOTO 1400
         ENDIF
      ELSE
         Buf(imdi+kk) = isave(kk)
         CYCLE
      ENDIF
!
!     NOT A BASIC SUBSTRUCTURE
!
 1350 IF ( numb<=(Blksiz-min+1)/inc ) THEN
         max = min + inc*numb - 1
         more = .FALSE.
      ELSE
         numb = numb - (Blksiz-min+1)/inc
         max = Blksiz
         more = .TRUE.
      ENDIF
!
!     INSERT THE NEW PREFIX TO THE NAMES OF ALL CONTRIBUTING SUBSTRUC-
!     TURES
!     IF THE COMPONENT IS FOR MODAL DOF ON THE SECONDARY SUBSTRUCTURE,
!     USE THE ACTUAL NAME INSTEAD OF ADDING A PREFIX
!
      DO k = min , max , inc
         IF ( Buf(Io+k)==Name1(1) .AND. Buf(Io+k+1)==Name1(2) ) THEN
!
            Buf(Io+k) = Name2(1)
            Buf(Io+k+1) = Name2(2)
         ELSE
            first = klshft(krshft(Prefx,Ncpw-1),Ncpw-1)
            rest = klshft(krshft(Buf(Io+k),Ncpw-3),Ncpw-4)
            first2 = klshft(krshft(Buf(Io+k),Ncpw-4),Ncpw-1)
            rest2 = klshft(krshft(Buf(Io+k+1),Ncpw-3),Ncpw-4)
            Buf(Io+k) = orf(orf(first,rest),mask)
            Buf(Io+k+1) = orf(orf(first2,rest2),mask)
         ENDIF
      ENDDO
 1400 DO
!
!     WRITE OUT UPDATED DATA BLOCK
!
         CALL sofio(iwrt,iwrtbl,Buf(Io-2))
         CALL fnxt(irdbl,inxt)
         IF ( mod(irdbl,2)==1 ) THEN
            next = andf(Buf(inxt),Jhalf)
         ELSE
            next = andf(rshift(Buf(inxt),Ihalf),Jhalf)
         ENDIF
         IF ( next==0 ) THEN
!
!     NO MORE BLOCKS TO COPY.  UPDATE MDI OF NAME2
!
            Buf(imdi+kk) = orf(lshift(rshift(isave(kk),Ihalf),Ihalf),newblk)
            EXIT
         ELSE
!
!     MORE BLOCKS TO COPY
!
            irdbl = next
            CALL getblk(iwrtbl,next)
            IF ( next/=-1 ) THEN
               iwrtbl = next
               CALL sofio(ird,irdbl,Buf(Io-2))
               min = 1
               IF ( more ) GOTO 1350
            ELSE
               CALL retblk(newblk)
               GOTO 1600
            ENDIF
         ENDIF
      ENDDO
   ENDDO
!
   Mdiup = .TRUE.
   IF ( iptr/=itop ) THEN
      iptr = iptr + 1
      ind1 = Imore(iptr)
      ind2 = Imore(iptr+Lim)
      GOTO 500
!
!     WRITE USER MESSAGES
!
   ELSEIF ( dry==0 ) THEN
!
!     DRY RUN - PRINT MESSAGE INDICATING ONLY ADDITIONS MADE
!
      CALL page2(-3)
      WRITE (Nout,99002) Uim , Name2 , Name1 , Name2
99002 FORMAT (A29,' 6228, SUBSTRUCTURE ',2A4,' IS ALREADY AN EQUIVALENT',' SUBSTRUCTURE TO ',2A4,/36X,'ONLY ITEMS NOT PREVIOUSLY ', &
             &'EXISTING FOR ',2A4,' HAVE BEEN MADE EQUIVALENT.')
      GOTO 99999
   ELSE
      DO i = 1 , 96
         Subtit(i) = iempty
      ENDDO
      CALL page
      CALL page2(-4)
      WRITE (Nout,99003) Name2 , Name1
!
99003 FORMAT (32X,67HS U B S T R U C T U R E   E Q U I V A L E N C E   O P E R A T I O N,///23X,13HSUBSTRUCTURE ,2A4,               &
             &56H HAS BEEN CREATED AND MARKED EQUIVALENT TO SUBSTRUCTURE ,2A4)
      image = Imore(Lim+1)
      CALL fmdi(image,imdi)
      ips = andf(Buf(imdi+1),1023)
      CALL fdit(ips,i)
      CALL page2(-2)
      WRITE (Nout,99004) Name2 , Buf(i) , Buf(i+1)
99004 FORMAT (1H0,22X,28HTHE PRIMARY SUBSTRUCTURE OF ,2A4,4H IS ,2A4)
      iptr = 2
      IF ( iptr>itop ) GOTO 99999
      CALL page2(-2)
      WRITE (Nout,99005)
99005 FORMAT (1H0,22X,56HTHE FOLLOWING IMAGE SUBSTRUCTURES HAVE BEEN GENERATED --)
   ENDIF
 1500 DO i = 1 , 16
      Imore(i) = iempty
   ENDDO
   j = 1
   DO
      image = Imore(iptr+Lim)
      CALL fdit(image,i)
      Imore(j) = Buf(i)
      Imore(j+1) = Buf(i+1)
      iptr = iptr + 1
      IF ( iptr>itop ) EXIT
      j = j + 2
      IF ( j>=16 ) EXIT
   ENDDO
   CALL page2(-2)
   WRITE (Nout,99006) (Imore(j),j=1,16)
99006 FORMAT (1H0,22X,10(2A4,2X))
   IF ( iptr>itop ) GOTO 99999
   GOTO 1500
 1600 WRITE (Nout,99007) Ufm
99007 FORMAT (A23,' 6223, SUBROUTINE SETEQ - THERE ARE NO MORE FREE ','BLOCKS AVAILABLE ON THE SOF.')
   k = -37
 1700 CALL sofcls
   CALL mesage(k,0,nmsbr)
!
99999 RETURN
END SUBROUTINE seteq
