!*==rename.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rename(Name1,Name2,Z,Nz,Itest)
   USE c_itemdt
   USE c_sof
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name1
   INTEGER , DIMENSION(2) :: Name2
   INTEGER , DIMENSION(2) :: Z
   INTEGER :: Nz
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , eog , ps
   LOGICAL :: higher
   INTEGER :: i , icore , idit , iloc , iloc2 , imdi , incr , ind , inum , ips , irw , item , itm , ncomp , ncore , nwds
   INTEGER , DIMENSION(2) :: name , nameh
   INTEGER , DIMENSION(2) , SAVE :: namsub
   EXTERNAL andf , delete , errmkn , fdit , fdsub , fmdi , fndnxl , mesage , sfetch , sofcls , suread , suwrt
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE RENAMES SUBSTRUCTURE NAME1 TO NAME2. SOF ITEMS EQSS,
!     BGSS, CSTM, LODS, LOAP AND PLTS ARE REWRITTEN TO REFLECT THE NEW
!     NAME.  THESE ITEMS ARE CHANGED FOR NAME1 AND ANY HIGHER LEVEL
!     SUBSTRUCTURE FOR WHICH NAME1 IS A COMPONENT.  NO CHANGES ARE MADE
!     TO SECONDARY SUBSTRUCTURES OF NAME1 WHICH RETAIN THEIR ORIGINAL
!     NAMES.  ALSO NO CHANGES ARE MADE TO THE SOLUTION DATA (ITEM SOLN)
!     FOR SUBSTRUCTURE NAME1 OR ANY HIGHER LEVEL SUBSTRUCTURES.
!
!     VALUES RETURNED IN ITEST ARE
!        1 - NORMAL RETURN
!        4 - SUBSTRUCTURE NAME1 DOES NOT EXIST
!       10 - SUBSTRUCTURE NAME2 ALREADY EXISTS
!
   DATA ps/1/
   DATA eog/4H$EOG/ , blank/4H    /
   DATA namsub/4HRENA , 4HME  /
!
!
!     CHECK IF NAME2 ALREADY EXISTS
!
   CALL fdsub(Name2,ind)
   IF ( ind/=-1 ) THEN
!
!     ERROR RETURNS
!
!
!     SUBSTRUCTURE NAME2 ALREADY EXIST ON THE SOF
!
      WRITE (nout,99001) uwm , Name1 , Name2
99001 FORMAT (A25,' 6230, SUBSTRUCTURE ',2A4,' HAS NOT BEEN RENAMED ','BECAUSE ',2A4,' ALREADY EXISTS ON THE SOF.')
      Itest = 10
      RETURN
   ELSE
!
!     CHANGE DIT ENTRY FOR SUBSTRUCTURE NAME1
!
      CALL fdsub(Name1,ind)
      IF ( ind<0 ) THEN
!
!     SUBSTRUCTURURE NAME1 DOES NOT EXIST
!
         Itest = 4
         RETURN
      ELSE
         CALL fdit(ind,idit)
         IF ( Name1(1)/=buf(idit) .OR. Name1(2)/=buf(idit+1) ) THEN
!
!     DIT FORMAT ERROR
!
            CALL errmkn(21,5)
         ELSE
            buf(idit) = Name2(1)
            buf(idit+1) = Name2(2)
            ditup = .TRUE.
!
            name(1) = Name2(1)
            name(2) = Name2(2)
            higher = .FALSE.
            SPAG_Loop_1_3: DO
!
!     CHANGE TABLE ITEMS WHICH CONTAIN SUBSTRUCTRUE NAME
!     SUBSTRUCTURE NAME.
!     HIGHER = .FALSE. - WE ARE WORKING WITH SUBSTRUCTURE NAME1
!     HIGHER = .TRUE.  - WE ARE WORKING WITH A SUBSTRUCTURE FOR
!                        WHICH NAME1 IS A COMPONENT
!
               CALL fdsub(name,ind)
               CALL fmdi(ind,imdi)
               ips = andf(buf(imdi+ps),1023)
               DO itm = 1 , nitem
                  IF ( items(2,itm)<=0 ) THEN
                     item = items(1,itm)
                     inum = items(3,itm)/1000000
                     iloc = (items(3,itm)-inum*1000000)/1000
                     incr = items(3,itm) - inum*1000000 - iloc*1000
!
!     PROCESS THE FOLLOWING ITEMS
!
!     SUBSTRUCTRUE NAME1
!     DONT PROCESS THE ITEM IF THIS IS A SECONDARY SUBSTRUCTURE
!     AND THE ACTUAL ITEM IS STORED FOR THE PRIMARY (I.E. BGSS,CSTM(
!     HIGHER LEVEL SUBSTRUCTURE
!     DONT PROCESS THE ITEM IF IT IS ACTUALLY STORED FOR THE
!     PRIMARY SUBSTRUCTURE (I.E. BGSS,CSTM)
!
                     IF ( .NOT.(.NOT.higher .AND. iloc==0 .AND. ips/=0) ) THEN
                        IF ( .NOT.(higher .AND. iloc==0) ) THEN
!
!     READ ITEM INTO OPEN CORE
!
                           irw = 1
                           CALL sfetch(name,item,irw,Itest)
                           IF ( Itest==1 ) THEN
                              ncore = Nz
                              icore = 1
                              SPAG_Loop_3_1: DO
                                 CALL suread(Z(icore),ncore,nwds,Itest)
                                 IF ( Itest==3 ) THEN
                                    nwds = icore + nwds - 1
!
!     CHANGE ANY OCCURANCE OF NAME1 WITH NAME2
!
                                    IF ( .NOT.(higher) ) THEN
!
!     SUBSTRUCTURE NAME1 - NAME SHOULD BE IN WORDS 1 AND 2 OF GROUP 0
!
                                       IF ( Z(1)/=Name1(1) .OR. Z(2)/=Name1(2) ) EXIT SPAG_Loop_3_1
                                       Z(1) = Name2(1)
                                       Z(2) = Name2(2)
                                    ENDIF
!
!     SEARCH THE LIST OF COMPONENT SUBSTRUCTURES FOR NAME1
!
                                    ncomp = Z(inum)
                                    iloc2 = iloc + incr*ncomp - 1
                                    SPAG_Loop_4_2: DO i = iloc , iloc2 , incr
                                       IF ( Z(i)==Name1(1) .AND. Z(i+1)==Name1(2) ) THEN
                                         Z(i) = Name2(1)
                                         Z(i+1) = Name2(2)
                                         EXIT SPAG_Loop_4_2
                                       ENDIF
                                    ENDDO SPAG_Loop_4_2
!
!     DELETE OLD ITEM
!
                                    CALL delete(name,item,Itest)
!
!     WRITE NEW ITEM TO SOF
!
                                    Itest = 3
                                    irw = 2
                                    CALL sfetch(name,item,irw,Itest)
                                    Itest = 3
                                    CALL suwrt(Z(1),nwds,Itest)
                                    EXIT SPAG_Loop_3_1
                                 ELSE
                                    IF ( Itest==1 ) EXIT SPAG_Loop_1_3
                                    Z(icore+nwds) = eog
                                    icore = icore + nwds + 1
                                    ncore = ncore - nwds - 1
                                 ENDIF
                              ENDDO SPAG_Loop_3_1
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
!
               ENDDO
!
!     GET NEXT HIGHER LEVEL SUBSTRUCTURE FOR WHICH NAME1 IS A
!     COMPONENT AND PERFORM SAME PROCEDURE
!
               CALL fndnxl(name,nameh)
               IF ( nameh(1)==blank .OR. nameh(1)==name(1) .AND. nameh(2)==name(2) ) THEN
!
!     NO HIGHER LEVEL SUBSTRUCTURES LEFT - PRINT INFORMATION MESSAGE
!     AND RETURN
!
                  WRITE (nout,99002) uim , Name1 , Name2
99002             FORMAT (A29,' 6229, SUBSTRUCTURE ',2A4,' HAS BEEN RENAMED TO ',2A4)
                  Itest = 1
                  RETURN
               ELSE
                  name(1) = nameh(1)
                  name(2) = nameh(2)
                  higher = .TRUE.
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
!
!     INSUFFICIENT CORE TO HOLD ITEM
!
         CALL sofcls
         CALL mesage(-8,0,namsub)
      ENDIF
   ENDIF
END SUBROUTINE rename
