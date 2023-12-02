!*==bdat02.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat02
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_cmbfnd
   USE c_output
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , conct
   INTEGER :: comp , flag , i , ic1 , ic2 , ifile , imsg , is1 , is2 , j , kdh , kk , n , np2 , nwd
   INTEGER , DIMENSION(32) :: ibits , jbits
   INTEGER , SAVE :: iblnk
   INTEGER , DIMENSION(2) :: id
   INTEGER , DIMENSION(16) , SAVE :: ihd
   INTEGER , DIMENSION(9) :: io
   INTEGER , DIMENSION(14) :: name
   INTEGER , DIMENSION(4) :: nams
   LOGICAL :: print
   EXTERNAL andf , bitpat , close , encode , finder , locate , mesage , open , page , page2 , read , rshift , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES CONCT BULK DATA AND WRITES CONNECTION
!     ENTRIES IN TERMS OF CODED GRID POINT ID NUMBERS ON SCR1
!
   DATA aaa/4HBDAT , 4H02  / , conct/210 , 2/
   DATA ihd/4H  SU , 4HMMAR , 4HY OF , 4H CON , 4HNECT , 4HION  , 4HENTR , 4HIES  , 4HSPEC , 4HIFIE , 4HD BY , 4H CON , 4HCT   ,    &
       &4HBULK , 4H DAT , 4HA   /
   DATA iblnk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         DO i = 1 , 96
            ihead(i) = iblnk
         ENDDO
         j = 1
         DO i = 73 , 88
            ihead(i) = ihd(j)
            j = j + 1
         ENDDO
         print = .FALSE.
         IF ( andf(rshift(iprint,3),1)==1 ) print = .TRUE.
         np2 = 2*npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            name(i) = combo(j,1)
            name(i+1) = combo(j,2)
         ENDDO
         ifile = scr1
         CALL open(*80,scr1,z(buf2),3)
         CALL locate(*20,z(buf1),conct,flag)
         ifile = geom4
         DO
            CALL read(*40,*20,geom4,id,1,0,n)
            IF ( id(1)==conset ) THEN
               CALL read(*40,*60,geom4,comp,1,0,n)
               IF ( print ) THEN
                  CALL page
                  CALL page2(6)
                  WRITE (outt,99001) (name(kdh),kdh=1,np2)
99001             FORMAT (/24X,74HNOTE  GRID POINT ID NUMBERS HAVE BEEN CODED TO THE COMPONENT SUBSTRUCTURE ,/30X,                  &
                         &75HWITHIN A GIVEN PSEUDOSTRUCTURE BY - 1000000*COMPONENT NO. + ACTUAL GRID ID.,//15X,                     &
                         &22HCONNECTED   CONNECTION,23X,33HGRID POINT ID FOR PSEUDOSTRUCTURE/18X,3HDOF,9X,4HCODE,3X,7(3X,2A4)/)
               ENDIF
               tdat(2) = .TRUE.
               CALL encode(comp)
               CALL read(*40,*60,geom4,nams,4,0,n)
               CALL finder(nams(1),is1,ic1)
               IF ( ierr==1 ) THEN
                  WRITE (outt,99004) ufm , nams(1) , nams(2)
                  idry = -2
               ENDIF
               CALL finder(nams(3),is2,ic2)
               IF ( ierr==1 ) THEN
                  WRITE (outt,99004) ufm , nams(3) , nams(4)
                  idry = -2
               ENDIF
               SPAG_Loop_2_1: DO
                  CALL read(*40,*60,geom4,id,2,0,n)
!
                  IF ( id(1)+id(2)==-2 ) EXIT SPAG_Loop_2_1
                  IF ( is1==is2 ) THEN
                     kk = 2*is1 - 1
                     WRITE (outt,99002) ufm , id(1) , id(2) , name(kk) , name(kk+1)
99002                FORMAT (A23,' 6536, MANUAL CONNECTION DATA IS ATTEMPTING TO ','CONNECT',/31X,'GRID POINTS',I9,5X,4HAND ,I8,    &
                           & /31X,'WHICH ARE BOTH CONTAINED IN PSEUDOSTRUCTURE ',2A4)
                     idry = -2
                  ENDIF
                  DO i = 1 , 9
                     io(i) = 0
                  ENDDO
                  io(1) = comp
                  io(2) = 2**(is1-1) + 2**(is2-1)
                  io(2+is1) = ic1*1000000 + id(1)
                  io(2+is2) = ic2*1000000 + id(2)
                  nwd = 2 + npsub
                  CALL write(scr1,io,nwd,1)
                  IF ( .NOT.(.NOT.print .OR. idry==-2) ) THEN
                     CALL bitpat(io(1),ibits)
                     CALL bitpat(io(2),jbits)
                     CALL page2(1)
                     WRITE (outt,99003) (ibits(kdh),kdh=1,2) , (jbits(kdh),kdh=1,2) , (io(kdh+2),kdh=1,npsub)
99003                FORMAT (16X,A4,A2,6X,A4,A3,2X,7(3X,I8))
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ELSE
               CALL read(*40,*60,geom4,id,-5,0,n)
               SPAG_Loop_2_2: DO
                  CALL read(*40,*60,geom4,id,2,0,n)
                  IF ( id(1)+id(2)==-2 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO
 20      CALL close(scr1,1)
         RETURN
!
 40      imsg = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -1
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (A23,' 6523, THE BASIC SUBSTRUCTURE ',2A4,/30X,'REFERED TO BY A CONCT  BULK DATA CARD CAN NOT BE FOUND ',              &
             &'IN THE PROBLEM TABLE OF CONTENTS.')
END SUBROUTINE bdat02
