!*==bdat04.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat04
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_cmbfnd
   USE c_output
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , reles
   INTEGER :: flag , i , ic , iccc , ifile , imsg , is , k , n
   INTEGER , DIMENSION(2) :: ibas , id
   INTEGER , DIMENSION(32) :: ibits , jbits , kbits
   INTEGER , DIMENSION(6) :: icc , ip
   INTEGER , DIMENSION(96) , SAVE :: ihd
   LOGICAL :: name , pager , print
   EXTERNAL andf , bitpat , close , encode , finder , fndgrd , locate , mesage , open , page , read , rshift , skpfil , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES THE RELES BULK DATA.
!
   DATA ihd/11*4H     , 4H  SU , 4HMMAR , 4HY OF , 4H PRO , 4HCESS , 4HED R , 4HELES , 4H BUL , 4HK DA , 4HTA   , 18*4H     ,       &
      & 4H   B , 4HASIC , 2*4H     , 4H GRI , 4HD    , 4H     , 4HREQU , 4HESTE , 4HD    , 4H  IN , 4HTERN , 4HAL   , 4H   C ,      &
       &4HURRE , 4HNT   , 4H  DO , 4HF TO , 4H BE  , 13*4H     , 4HSUBS , 4HTRUC , 4HTURE , 4H   P , 4HOINT , 4H ID  , 4H     ,     &
       &4H REL , 4HEASE , 4H     , 4H  PO , 4HINT  , 4HNO.  , 4H     , 4H DOF , 4H     , 4H   R , 4HELEA , 4HSED  , 6*4H    /
   DATA reles/410 , 4/ , aaa/4HBDAT , 4H04  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         DO i = 1 , 96
            ihead(i) = ihd(i)
         ENDDO
         pager = .TRUE.
         print = .FALSE.
         IF ( andf(rshift(iprint,8),1)==1 ) print = .TRUE.
         ifile = scbdat
         CALL open(*40,scbdat,z(buf2),0)
         CALL skpfil(scbdat,3)
         CALL close(scbdat,2)
         CALL open(*40,scbdat,z(buf2),3)
         ifile = scr2
         CALL locate(*20,z(buf1),reles,flag)
         ifile = geom4
         DO
            CALL read(*60,*20,geom4,id,1,0,n)
            IF ( id(1)==conset ) THEN
               name = .TRUE.
               IF ( pager .AND. print ) CALL page
               pager = .FALSE.
               tdat(4) = .TRUE.
               SPAG_Loop_2_1: DO
                  CALL read(*60,*80,geom4,id,2,0,n)
                  IF ( id(1)+id(2)==-2 ) THEN
                     CALL write(scbdat,id,0,1)
                     EXIT SPAG_Loop_2_1
                  ELSEIF ( .NOT.name ) THEN
                     CALL fndgrd(is,ic,id(1),ip,icc,n)
                     IF ( ierr/=1 ) THEN
                        CALL encode(id(2))
                        CALL bitpat(id(2),ibits)
                        DO i = 1 , n
                           iccc = andf(id(2),icc(i))
                           CALL bitpat(iccc,jbits)
                           icc(i) = andf(icc(i),63)
                           CALL bitpat(icc(i),kbits)
                           IF ( iccc/=0 ) THEN
                              IF ( print ) THEN
                                 WRITE (outt,99001) ibas , id(1) , ibits(1) , ibits(2) , ip(i) , kbits(1) , kbits(2) , jbits(1) ,   &
                                      & jbits(2)
99001                            FORMAT (35X,2A4,5X,I8,7X,A4,A2,6X,I8,6X,A4,A2,6X,A4,A2)
                              ENDIF
                              CALL write(scbdat,ip(i),1,0)
                              CALL write(scbdat,iccc,1,0)
                           ENDIF
                        ENDDO
                     ELSE
                        WRITE (outt,99002) ufm , id(1) , inam
99002                   FORMAT (A23,' 6515, GRID POINT',I10,' BASIC SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
                        idry = -2
                     ENDIF
                  ELSE
                     CALL finder(id,is,ic)
                     ibas(1) = id(1)
                     ibas(2) = id(2)
                     IF ( ierr/=1 ) THEN
                        CALL write(scbdat,is,1,0)
                        name = .NOT.name
                     ELSE
                        WRITE (outt,99003) ufm , (id(k),k=1,2)
99003                   FORMAT (A23,' 6517, THE BASIC SUBSTRUCTURE  ',2A4,/30X,                                                     &
                               &'REFERED TO BY A RELES  BULK DATA CARD CAN NOT BE FOUND ','IN THE PROBLEM TABLE OF CONTENTS.')
                        idry = -2
                        DO
                           CALL read(*60,*80,geom4,id,2,0,n)
                           IF ( id(1)+id(2)==-2 ) EXIT SPAG_Loop_2_1
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ELSE
               SPAG_Loop_2_2: DO
                  CALL read(*60,*80,geom4,id,2,0,n)
                  IF ( id(1)+id(2)==-2 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO
 20      CALL close(scbdat,1)
         RETURN
!
 40      imsg = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bdat04
