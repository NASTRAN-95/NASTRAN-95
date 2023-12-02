!*==dstroy.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dstroy(Name,Itest,Image,Imore,Lim)
   USE c_itemdt
   USE c_sof
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Itest
   INTEGER , DIMENSION(1) :: Image
   INTEGER , DIMENSION(1) :: Imore
   INTEGER :: Lim
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: cs , hl , iempty , indsbr , is , ll , ps , ss
   INTEGER :: i , ibl , icheck , icount , ics , idit , ihere , iis , ill , imdi , imtop , indcs , index , indhl , indis , indll ,   &
            & indps , indss , ips , iret1 , iret2 , isave , iss , isv , itm , itop , j , jdit , maskl , maskm
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , complf , errmkn , fdit , fdsub , fmdi , lshift , mesage , orf , retblk , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
         i = buf(imdi+ps)
         indps = andf(i,1023)
         indss = rshift(andf(i,1048575),10)
!                           1048575 = 2**20 - 1
         indis = andf(i,1073741824)
!                    1073741824 = 2**30
         i = buf(imdi+ll)
         indhl = andf(i,1023)
         indcs = rshift(andf(i,1048575),10)
         indll = rshift(andf(i,1073741823),20)
!                           1073741823 = 2**30 - 1
         IF ( indis>0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( indps==0 ) THEN
!
!     PRIMARY SUBSTRUCTURE.
!     RETURN THE BLOCKS USED BY ALL ITEMS TO THE LIST OF FREE BLOCKS.
!
            DO j = ifrst , dirsiz
               ibl = andf(buf(imdi+j),65535)
!                            65535 = 2**16 - 1
               IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
            ENDDO
            IF ( indss==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     THE PRIMARY SUBSTRUCTURE BEING DESTROYED HAS SECONDARY EQUIVALENT
!     SUBSTRUCTURES.  MUST DESTROY ALL OF THEM.
!
            ASSIGN 80 TO iret1
            ASSIGN 40 TO iret2
            isv = indss
            spag_nextblock_1 = 3
         ELSE
            ASSIGN 20 TO iret1
            spag_nextblock_1 = 9
         ENDIF
         CYCLE
!
!     REMOVE INDEX FROM THE LIST OF SUBSTRUCTURES THAT ARE SECONDARY TO
!     INDPS.
!
 20      isave = indps
         DO
            CALL fmdi(isave,imdi)
            isave = rshift(andf(buf(imdi+ss),1048575),10)
            IF ( isave==0 ) THEN
               ASSIGN 60 TO iret2
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( isave==index ) THEN
               buf(imdi+ss) = orf(andf(buf(imdi+ss),maskm),lshift(indss,10))
               mdiup = .TRUE.
               IF ( indll==0 ) GOTO 60
               ill = indll
               indll = 0
               isave = index
               ASSIGN 60 TO iret2
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         isave = isv
         CALL fmdi(isave,imdi)
         isv = rshift(andf(buf(imdi+ss),1048575),10)
         iis = andf(buf(imdi+is),1073741824)
         IF ( iis>0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THE SECONDARY SUBSTRUCTURE IS NOT AN IMAGE SUBSTRUCTURE.  ADD ITS
!     INDEX TO THE LIST (IMORE) OF SUBSTRUCTURES TO BE DESTROYED LATER.
!
         itop = itop + 1
         IF ( itop>Lim ) THEN
            CALL mesage(-8,0,nmsbr)
            RETURN
         ELSE
            Imore(itop) = isave
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     UPDATE THE MDI OF THE SECONDARY SUBSTRUCTURE WITH INDEX ISAVE.
!
 40      CALL fmdi(isave,imdi)
         buf(imdi+ps) = 0
         buf(imdi+ll) = andf(buf(imdi+ll),maskl)
         DO j = ifrst , dirsiz
            buf(imdi+j) = 0
         ENDDO
         mdiup = .TRUE.
         spag_nextblock_1 = 4
      CASE (4)
         IF ( isv/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BACK TO THE SUBSTRUCTURE WITH INDEX  INDEX .
!     DELETE ITS DIRECTORY FROM THE MDI.
!
 60      CALL fmdi(index,imdi)
         spag_nextblock_1 = 5
      CASE (5)
         DO j = 1 , dirsiz
            buf(imdi+j) = 0
         ENDDO
         mdiup = .TRUE.
!
!     DELETE SUBSTRUCTURE NAME FROM THE DIT.
!
         CALL fdit(index,jdit)
         buf(jdit) = iempty
         buf(jdit+1) = iempty
         ditup = .TRUE.
         IF ( index*2==ditsiz ) ditsiz = ditsiz - 2
         ditnsb = ditnsb - 1
         IF ( indcs==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     DELETE LINK THROUGH COMBINED SUBSTRUCTURES, AND REMOVE ITEMS
!     CREATED AS A RESULTS OF THE COMBINE OR REDUCE.
!     THESE ITEMS WILL BE RETURNED TO THE LIST OF FREE BLOCKS.
!
         IF ( indcs==index ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fmdi(indcs,imdi)
         indcs = rshift(andf(buf(imdi+cs),1048575),10)
         spag_nextblock_1 = 7
      CASE (7)
         buf(imdi+hl) = andf(buf(imdi+hl),complf(1023))
         buf(imdi+cs) = andf(buf(imdi+cs),maskm)
         DO j = 1 , nitem
            IF ( item(6,j)/=0 ) THEN
               itm = j + ifrst - 1
               ibl = andf(buf(imdi+itm),65535)
               IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
               buf(imdi+itm) = 0
            ENDIF
         ENDDO
         mdiup = .TRUE.
         IF ( indcs/=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ERROR MESSAGES.
!
         CALL errmkn(indsbr,8)
         CALL mesage(-8,0,nmsbr)
         RETURN
      CASE (8)
         IF ( indll==0 ) THEN
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
               buf(imdi+ll) = andf(buf(imdi+ll),maskl)
               mdiup = .TRUE.
            ENDIF
            spag_nextblock_1 = 2
         ELSE
!
!     SUBSTRUCTURE WAS THE RESULT OF COMBINING LOWER LEVEL SUBSTRUCTURES
!     TOGETHER.  UPDATE THE MDI ACCORDINGLY.
!
            CALL fmdi(indll,imdi)
            indcs = rshift(andf(buf(imdi+cs),1048575),10)
            index = indll
            indll = 0
            IF ( indcs==0 ) indcs = index
            spag_nextblock_1 = 7
         ENDIF
      CASE (9)
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
         DO j = 1 , nitem
            IF ( item(5,j)/=0 ) THEN
               itm = j + ifrst - 1
               ibl = andf(buf(imdi+itm),65535)
               IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
               buf(imdi+itm) = 0
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
 80      CALL fmdi(isave,imdi)
         ill = rshift(andf(buf(imdi+ll),1073741823),20)
         IF ( ill==0 ) GOTO iret2
         spag_nextblock_1 = 10
      CASE (10)
         imtop = 1
         Image(imtop) = ill
         icount = 1
         ihere = Image(icount)
         SPAG_Loop_1_1: DO
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL fmdi(ihere,imdi)
                  i = buf(imdi+ps)
                  ips = andf(i,1023)
                  iss = rshift(andf(i,1048575),10)
                  iis = andf(i,1073741824)
                  i = buf(imdi+ll)
                  ill = rshift(andf(i,1073741823),20)
                  ics = rshift(andf(i,1048575),10)
                  IF ( iis==0 ) EXIT SPAG_Loop_1_1
!
!     DELETE THE SUBSTRUCTURE WITH INDEX IHERE FROM THE MDI AND THE DIT.
!     RETURN THE BLOCKS USED BY THE IMAGE SUBSTRUCTURE TO THE LIST OF
!     FREE BLOCKS.  THIS INCLUDES THE FOLLOWING ITEMS
!
!     ITEMS COPIED DURING A EQUIV OPERATION
!     SOLUTION ITEMS
!
                  DO j = 1 , nitem
                     IF ( item(4,j)/=0 ) THEN
                        itm = j + ifrst - 1
                        ibl = andf(buf(imdi+itm),65535)
                        IF ( ibl>0 .AND. ibl/=65535 ) CALL retblk(ibl)
                        buf(imdi+itm) = 0
                     ENDIF
                  ENDDO
                  DO j = 1 , dirsiz
                     buf(imdi+j) = 0
                  ENDDO
                  mdiup = .TRUE.
                  CALL fdit(ihere,idit)
                  buf(idit) = iempty
                  buf(idit+1) = iempty
                  ditup = .TRUE.
                  IF ( ihere*2==ditsiz ) ditsiz = ditsiz - 2
                  ditnsb = ditnsb - 1
!
!     DELETE POINTERS TO IHERE.
!
                  icheck = ips
                  SPAG_Loop_2_2: DO
                     CALL fmdi(icheck,imdi)
                     icheck = rshift(andf(buf(imdi+ss),1048575),10)
                     IF ( icheck==0 ) EXIT SPAG_Loop_2_2
                     IF ( icheck==ihere ) THEN
                        buf(imdi+ss) = orf(andf(buf(imdi+ss),maskm),lshift(iss,10))
                        mdiup = .TRUE.
                        EXIT SPAG_Loop_2_2
                     ENDIF
                  ENDDO SPAG_Loop_2_2
!
!     ARE THERE MORE SUBSTRUCTURES TO ADD TO THE LIST IMAGE
!
                  IF ( ill/=0 ) THEN
                     DO j = 1 , imtop
                        IF ( Image(j)==ill ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     imtop = imtop + 1
                     Image(imtop) = ill
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  IF ( ics/=0 ) THEN
                     DO j = 1 , imtop
                        IF ( Image(j)==ics ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     imtop = imtop + 1
                     IF ( imtop>Lim ) THEN
                        CALL mesage(-8,0,nmsbr)
                        RETURN
                     ELSE
                        Image(imtop) = ics
                     ENDIF
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (3)
!
!     ARE THERE MORE SUBSTRUCTURES ON THE LIST IMAGE
!
                  IF ( icount==imtop ) GOTO iret2
                  icount = icount + 1
                  ihere = Image(icount)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 11
      CASE (11)
!
!     NAME IS AN IMAGE SUBSTRUCTURE.
!
         Itest = 6
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dstroy
