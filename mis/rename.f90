
SUBROUTINE rename(Name1,Name2,Z,Nz,Itest)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Buf(1) , Ditdum(6) , Sysbuf
   LOGICAL Ditup , Mdiup
   INTEGER Iodum(8) , Items(7,1) , Mdidum(4) , Nitem , Nout , Nxtdum(15)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /itemdt/ Nitem , Items
   COMMON /sof   / Ditdum , Iodum , Mdidum , Nxtdum , Ditup , Mdiup
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Itest , Nz
   INTEGER Name1(2) , Name2(2) , Z(2)
!
! Local variable declarations
!
   INTEGER andf
   INTEGER blank , eog , i , icore , idit , iloc , iloc2 , imdi , incr , ind , inum , ips , irw , item , itm , name(2) , nameh(2) , &
         & namsub(2) , ncomp , ncore , nwds , ps
   LOGICAL higher
   EXTERNAL andf
!
! End of declarations
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
      WRITE (Nout,99001) Uwm , Name1 , Name2
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
         IF ( Name1(1)/=Buf(idit) .OR. Name1(2)/=Buf(idit+1) ) THEN
!
!     DIT FORMAT ERROR
!
            CALL errmkn(21,5)
         ELSE
            Buf(idit) = Name2(1)
            Buf(idit+1) = Name2(2)
            Ditup = .TRUE.
!
            name(1) = Name2(1)
            name(2) = Name2(2)
            higher = .FALSE.
            DO
!
!     CHANGE TABLE ITEMS WHICH CONTAIN SUBSTRUCTRUE NAME
!     SUBSTRUCTURE NAME.
!     HIGHER = .FALSE. - WE ARE WORKING WITH SUBSTRUCTURE NAME1
!     HIGHER = .TRUE.  - WE ARE WORKING WITH A SUBSTRUCTURE FOR
!                        WHICH NAME1 IS A COMPONENT
!
               CALL fdsub(name,ind)
               CALL fmdi(ind,imdi)
               ips = andf(Buf(imdi+ps),1023)
               DO itm = 1 , Nitem
                  IF ( Items(2,itm)<=0 ) THEN
                     item = Items(1,itm)
                     inum = Items(3,itm)/1000000
                     iloc = (Items(3,itm)-inum*1000000)/1000
                     incr = Items(3,itm) - inum*1000000 - iloc*1000
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
                              DO
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
                                       IF ( Z(1)/=Name1(1) .OR. Z(2)/=Name1(2) ) EXIT
                                       Z(1) = Name2(1)
                                       Z(2) = Name2(2)
                                    ENDIF
!
!     SEARCH THE LIST OF COMPONENT SUBSTRUCTURES FOR NAME1
!
                                    ncomp = Z(inum)
                                    iloc2 = iloc + incr*ncomp - 1
                                    DO i = iloc , iloc2 , incr
                                       IF ( Z(i)==Name1(1) .AND. Z(i+1)==Name1(2) ) THEN
                                         Z(i) = Name2(1)
                                         Z(i+1) = Name2(2)
                                         EXIT
                                       ENDIF
                                    ENDDO
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
                                    EXIT
                                 ELSE
                                    IF ( Itest==1 ) GOTO 20
                                    Z(icore+nwds) = eog
                                    icore = icore + nwds + 1
                                    ncore = ncore - nwds - 1
                                 ENDIF
                              ENDDO
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
                  WRITE (Nout,99002) Uim , Name1 , Name2
99002             FORMAT (A29,' 6229, SUBSTRUCTURE ',2A4,' HAS BEEN RENAMED TO ',2A4)
                  Itest = 1
                  RETURN
               ELSE
                  name(1) = nameh(1)
                  name(2) = nameh(2)
                  higher = .TRUE.
               ENDIF
            ENDDO
         ENDIF
!
!     INSUFFICIENT CORE TO HOLD ITEM
!
 20      CALL sofcls
         CALL mesage(-8,0,namsub)
      ENDIF
   ENDIF
END SUBROUTINE rename
