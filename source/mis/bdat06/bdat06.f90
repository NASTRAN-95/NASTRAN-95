!*==bdat06.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat06
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
   INTEGER , DIMENSION(2) , SAVE :: aaa , gtran
   INTEGER :: flag , i , ic , ifile , imsg , is , kk , n , nn
   INTEGER , DIMENSION(5) :: id
   INTEGER , DIMENSION(96) , SAVE :: ihd
   LOGICAL :: print
   EXTERNAL andf , close , eof , finder , locate , mesage , open , page , page2 , read , rshift , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES THE GTRAN BULK DATA
!
!WKBI 8/94 ALPHA-VMS
   DATA gtran/1510 , 15/ , aaa/4HBDAT , 4H06  /
   DATA ihd/11*4H     , 4H  SU , 4HMMAR , 4HY OF , 4H PRO , 4HCESS , 4HED G , 4HTRAN , 4H BUL , 4HK DA , 4HTA   , 19*4H     ,       &
      & 4H PSE , 4HUDO- , 4H     , 4H     , 4H COM , 4HPONE , 4HNT   , 4H     , 4H   T , 4HRANS , 2*4H     , 4HGRID , 4H     ,      &
       &4HREFE , 4HRENC , 4HE    , 14*4H     , 4H  ST , 4HRUCT , 4HURE  , 4HNO.  , 4H   S , 4HTRUC , 4HTURE , 4H NO. , 4H     ,     &
       &4H  SE , 4HT ID , 2*4H     , 4H ID  , 4H     , 4HTRAN , 4HS  I , 4HD    , 7*2H  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ifile = Scr1
         kk = 0
         print = .FALSE.
         IF ( andf(rshift(Iprint,5),1)==1 ) print = .TRUE.
         DO i = 1 , 96
            Ihead(i) = ihd(i)
         ENDDO
         CALL open(*80,Scr2,Z(Buf3),1)
         ifile = Scbdat
         CALL locate(*60,Z(Buf1),gtran,flag)
         IF ( print ) CALL page
         ifile = Geom4
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*100,*20,Geom4,id,1,0,n)
            DO i = 1 , Npsub
               IF ( id(1)==Combo(i,3) ) EXIT SPAG_Loop_1_1
            ENDDO
            CALL read(*100,*120,Geom4,id,-4,0,n)
         ENDDO SPAG_Loop_1_1
         Tdat(6) = .TRUE.
         kk = kk + 1
         CALL read(*100,*120,Geom4,id(2),4,0,n)
         CALL finder(id(2),is,ic)
         IF ( Ierr==1 ) THEN
            WRITE (Outt,99001) Ufm , id(2) , id(3)
99001       FORMAT (A23,' 6530, THE BASIC SUBSTRUCTURE ',2A4,/30X,'REFERED TO BY A GTRAN BULK DATA CARD WHICH CANNOT BE ',          &
                   &'FOUNDD IN THE PROBLEM TABLE OF CONTENTS.')
            Idry = -2
         ENDIF
         IF ( print ) CALL page2(1)
         IF ( print ) WRITE (Outt,99002) is , ic , id(1) , id(4) , id(5)
!
99002    FORMAT (36X,I1,14X,I5,8X,I8,4X,I8,4X,I8)
         id(3) = id(1)
         id(1) = is
         id(2) = ic
         id(4) = ic*1000000 + id(4)
         Z(Buf4+kk) = id(5)
         CALL write(Scr2,id,5,0)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL write(Scr2,id,0,1)
         CALL close(Scr2,1)
         IF ( .NOT.Tdat(6) ) GOTO 60
         ifile = Scr2
         CALL open(*80,Scr2,Z(Buf3),2)
         CALL read(*100,*40,Scr2,Z(Score),Lcore,0,nn)
         imsg = -8
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      CALL sort(0,0,5,1,Z(Score),nn)
         CALL write(Scbdat,Z(Score),nn,1)
 60      CALL eof(Scbdat)
         Z(Buf4) = kk
         CALL close(Scr2,1)
         IF ( print ) CALL page2(3)
         IF ( print ) WRITE (Outt,99003)
99003    FORMAT (/5X,'NOTE - THE PSEUDOSTRUCTURE AND COMPONENT NUMBERS RE',                                                         &
                &'FER TO THEIR POSITIONS IN THE PROBLEM TABLE OF CONTENTS.')
         RETURN
!
 80      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 100     imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 120     imsg = -3
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bdat06
