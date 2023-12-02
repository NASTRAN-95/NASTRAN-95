!*==bdat01.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat01
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_CMBFND
   USE C_OUTPUT
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , conct1
   INTEGER :: flag , i , ifile , imsg , isum , j , kdh , kk , m , n , nnn , np2 , nss , nssp1 , nwd
   INTEGER , DIMENSION(32) :: ibits , jbits
   INTEGER , SAVE :: iblnk
   INTEGER , DIMENSION(7) :: ic , is
   INTEGER , DIMENSION(14) :: id , name
   INTEGER , DIMENSION(16) , SAVE :: ihd
   INTEGER , DIMENSION(9) :: io
   LOGICAL :: print
   EXTERNAL andf , bitpat , close , encode , finder , locate , mesage , open , page , page2 , read , rshift , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES CONCT1 BULK DATA GENERATING
!     CONNECTION ENTRIES IN TERMS OF GRID POINT ID NUMBERS
!     CODED TO THE PSEUDO-STRUCTURE ID NUMBER.
!     THESE ARE THEN WRITTEN ON SCR1.
!
   DATA aaa/4HBDAT , 4H01  / , conct1/110 , 41/
   DATA ihd/4H  SU , 4HMMAR , 4HY OF , 4H CON , 4HNECT , 4HION  , 4HENTR , 4HIES  , 4HSPEC , 4HIFIE , 4HD BY , 4H CON , 4HCT1  ,    &
       &4HBULK , 4H DAT , 4HA   /
   DATA iblnk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         DO i = 1 , 96
            Ihead(i) = iblnk
         ENDDO
         j = 1
         DO i = 73 , 88
            Ihead(i) = ihd(j)
            j = j + 1
         ENDDO
         print = .FALSE.
         IF ( andf(rshift(Iprint,2),1)==1 ) print = .TRUE.
         np2 = 2*Npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            name(i) = Combo(j,1)
            name(i+1) = Combo(j,2)
         ENDDO
         ifile = Scr1
         CALL open(*60,Scr1,Z(Buf2),1)
         CALL locate(*80,Z(Buf1),conct1,flag)
         ifile = Geom4
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*20,*80,Geom4,id,2,0,n)
            nss = id(1)
            nssp1 = nss + 1
            IF ( id(2)==Conset ) THEN
               nwd = 2*nss
               IF ( print ) THEN
                  CALL page
                  CALL page2(6)
                  WRITE (Outt,99001) (name(kdh),kdh=1,np2)
99001             FORMAT (/24X,74HNOTE  GRID POINT ID NUMBERS HAVE BEEN CODED TO THE COMPONENT SUBSTRUCTURE,/30X,                   &
                         &75HWITHIN A GIVEN PSEUDOSTRUCTURE BY - 1000000*COMPONENT NO. + ACTUAL GRID ID.,//15X,                     &
                         &22HCONNECTED   CONNECTION,23X,33HGRID POINT ID FOR PSEUDOSTRUCTURE,/18X,3HDOF,9X,4HCODE,3X,7(3X,2A4)/)
               ENDIF
!
!     MAKING IT TO 50 IMPLIES THAT CONCT1 DATA EXISTS
!
               Tdat(1) = .TRUE.
               CALL read(*20,*40,Geom4,id,nwd,0,nnn)
               DO i = 1 , nss
                  j = 2*(i-1)
                  CALL finder(id(1+j),is(i),ic(i))
                  IF ( Ierr==1 ) THEN
                     WRITE (Outt,99002) Ufm , id(1+j) , id(2+j)
99002                FORMAT (A23,' 6522, THE BASIC SUBSTRUCTURE ',2A4,/30X,'REFERED TO BY A CONCT1 BULK DATA CARD CAN NOT BE FOUND '&
                           & ,'IN THE PROBLEM TABLE OF CONTENTS.')
                     Idry = -2
                  ENDIF
               ENDDO
               EXIT SPAG_Loop_1_1
            ELSE
               SPAG_Loop_2_2: DO
                  CALL read(*20,*40,Geom4,id,1,0,nnn)
                  IF ( id(1)==-1 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         DO i = 1 , 9
            io(i) = 0
         ENDDO
         DO i = 1 , nssp1
            CALL read(*20,*40,Geom4,id(i),1,0,nnn)
            IF ( id(i)==-1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         DO i = 1 , nss
            DO j = 1 , nss
               IF ( i/=j ) THEN
                  IF ( is(i)==is(j) .AND. id(i+1)/=0 .AND. id(j+1)/=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         kk = 2*is(i) - 1
         WRITE (Outt,99003) Ufm , id(i+1) , id(j+1) , name(kk) , name(kk+1)
99003    FORMAT (A23,' 6536, MANUAL CONNECTION DATA IS ATTEMPTING TO ','CONNECT',/31X,'GRID POINTS',I9,5X,4HAND ,I8,/31X,           &
                &'WHICH ARE BOTH CONTAINED IN PSEUDOSTRUCTURE ',2A4)
         Idry = -2
         spag_nextblock_1 = 5
      CASE (5)
         CALL encode(id(1))
         io(1) = id(1)
         isum = 0
         DO i = 1 , nss
            IF ( id(i+1)/=0 ) THEN
               IF ( id(i+1)/=0 ) isum = isum + 2**(is(i)-1)
               m = 2 + is(i)
               io(m) = ic(i)*1000000 + id(i+1)
            ENDIF
         ENDDO
         io(2) = -1*isum
         nwd = 2 + Npsub
         CALL write(Scr1,io,nwd,1)
         IF ( .NOT.(.NOT.print .OR. Idry==-2) ) THEN
            CALL bitpat(io(1),ibits)
            CALL bitpat(iabs(io(2)),jbits)
            CALL page2(1)
            WRITE (Outt,99004) (ibits(kdh),kdh=1,2) , (jbits(kdh),kdh=1,2) , (io(kdh+2),kdh=1,Npsub)
99004       FORMAT (16X,A4,A2,6X,A4,A3,2X,7(3X,I8))
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 20      imsg = -2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -3
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -1
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(imsg,ifile,aaa)
 80      CALL close(Scr1,2)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bdat01
