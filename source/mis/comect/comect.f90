!*==comect.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comect(Ele,Max)
   USE c_blank
   USE c_gpta1
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ele
   INTEGER :: Max
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b1 , b2 , ept , i , idx , j , jcomp , k , kcomp , lect , lele , m , n , ngpel , pid , type
   INTEGER , DIMENSION(2) :: elid
   INTEGER , DIMENSION(5) :: err
   INTEGER , DIMENSION(32) :: gp
   INTEGER , DIMENSION(3) :: idrec
   INTEGER , DIMENSION(20) , SAVE :: ihx2
   INTEGER , DIMENSION(32) , SAVE :: ihx3
   INTEGER , SAVE :: ilxx , inrew , nm1 , outrew , rew
   INTEGER , DIMENSION(1) :: ix
   INTEGER , DIMENSION(18) , SAVE :: m1
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: offset
   INTEGER , DIMENSION(12) , SAVE :: pcomp
   EXTERNAL close , clstab , fread , gopen , korsz , locate , mesage , open , preloc , read , skprec , write , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     REVISED  10/1990 BY G.CHAN/UNISYS
!              TO INCLUDE OFFSET DATA FOR CBAR, CTRIA3 AND CQUAD4 IN
!              THE ECT2 DATA BLOCK
!              (6 COORDINATE VALUES FOR THE BAR, AND 1 OFFSET VALUE
!              FOR EACH OF THE TWO PLATES, ARE ADDED AFTER THE GRID
!              DATA)
!
   !>>>>EQUIVALENCE (offset(1),gp(1)) , (Ix(1),X(1))
   DATA name/4H COM , 4HECT / , outrew , rew , inrew/1 , 1 , 0/
   DATA pcomp/5502 , 25 , 2 , 5602 , 14 , 2 , 5702 , 13 , 2 , 5802 , 17 , 17/
!                     PCOMP      PCOMP1     PCOMP2     PSHELL
   DATA nm1/18/ , m1/4H(33X , 4H,2A4 , 4H,18H , 4HIGNO , 4HRING , 4H ELE , 4HMENT , 4H (2A , 4H4,32 , 4HH) W , 4HITH  , 4HMORE ,    &
       &4H THA , 4HN 32 , 4H CON , 4HNECT , 4HIONS , 4H.)  / , ilxx/2HXX/
   DATA ihx2/1 , 1 , 3 , 3 , 5 , 5 , 7 , 7 , 1 , 3 , 5 , 7 , 13 , 13 , 15 , 15 , 17 , 17 , 19 , 19/
   DATA ihx3/1 , 1 , 4 , 4 , 4 , 7 , 7 , 7 , 10 , 10 , 10 , 1 , 1 , 4 , 7 , 10 , 21 , 24 , 27 , 30 , 21 , 21 , 24 , 24 , 24 , 27 ,  &
      & 27 , 27 , 30 , 30 , 30 , 21/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         b1 = korsz(x) - (3*bufsiz+2)
         b2 = b1 + bufsiz + 3
         err(1) = 4
         err(2) = name(1)
         err(3) = name(2)
!
!     IF EPT FILE IS PRESENT, AND ANY OF THE PSHELL, PCOMP, PCOMP1 AND
!     PCOMP2 CARDS IS ALSO PRESENT, CREATE A TABLE OF PROPERTY ID AND
!     OFFSET DATA, TO BE USE LATER BY CTRIA3 OR CQURD4 ELEMENTS
!
         jcomp = b1
         ept = 104
         CALL open(*40,ept,x(b1),inrew)
         CALL read(*20,*20,ept,ix,2,1,m)
         CALL close(ept,rew)
         CALL preloc(*40,x(b1),ept)
         n = 1
         DO i = 1 , 12 , 3
            idrec(1) = pcomp(i)
            idrec(2) = idrec(1)/100
            CALL locate(*10,x(b1),idrec,j)
            k = pcomp(i+1)
            j = pcomp(i+2)
            DO
               CALL read(*10,*10,ept,x,k,0,m)
               IF ( x(j)/=0.0 ) THEN
                  jcomp = jcomp - 2
                  ix(jcomp) = ix(1)
                  x(jcomp+1) = x(j)
               ENDIF
            ENDDO
 10         n = n + 1
         ENDDO
 20      CALL close(ept,rew)
         kcomp = b1 - 1
!
!     CONSTRUCT A LIST OF INDICES IN THE ECT FOR USE WITH GPECT IN THE
!     PLOT MODULE BY CONTOUR PLOTTING
!
 40      CALL gopen(ect1,x(b1),inrew)
         DO j = 1 , Max
            Ele(j) = 0
         ENDDO
         i = 1
 60      DO
            CALL read(*140,*80,ect1,idrec,3,0,m)
            DO j = 1 , nel
               idx = (j-1)*incr
               IF ( ne(idx+4)==idrec(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL skprec(ect1,1)
         ENDDO
 80      CALL mesage(-3,ect1,name)
 100     CALL mesage(-2,ect1,name)
         spag_nextblock_1 = 2
      CASE (2)
         lect = ne(idx+6) - 1
         DO
            CALL read(*100,*60,ect1,Ele(i),1,0,m)
            CALL fread(ect1,0,-lect,0)
            i = i + 1
            IF ( i>Max ) CALL mesage(-8,0,name)
         ENDDO
!
 120     CALL mesage(-1,ect1,name)
!
 140     lele = i - 1
         CALL close(ect1,rew)
!
         CALL preloc(*120,x(b1),ect1)
         CALL gopen(ect2,x(b2),outrew)
         DO n = 1 , nel
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  idx = (n-1)*incr
!
!     IF SCALAR CONNECTION POSSIBLE FOR ELEMENT THEN SKIP IT
!
                  IF ( ne(idx+11)/=0 ) CYCLE
!
!     SKIP DUMMY ELEMENTS AND POINT ELEMENTS
!
                  IF ( ne(idx+10)<=1 ) CYCLE
                  IF ( ne(idx+16)==ilxx ) CYCLE
                  CALL locate(*160,x(b1),ne(idx+4),i)
                  ngpel = ne(idx+10)
                  IF ( ngpel>32 ) THEN
!
!     ELEMENT TYPE WITH MORE THAN 32 CONNECTIONS
!
                     err(4) = ne(idx+1)
                     err(5) = ne(idx+2)
                     CALL wrtprt(merr,err,m1,nm1)
                     CALL skprec(ect1,1)
                     CYCLE
                  ELSE
!
                     CALL write(ect2,n,1,0)
                     CALL write(ect2,ngpel,1,0)
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL read(*142,*142,ect1,elid,1,0,i)
!
!     FIND THIS ELEMENTS POINTER IN THE ECT
!
                  DO i = 1 , lele
                     IF ( Ele(i)==elid(1) ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CALL mesage(-37,0,name)
                  spag_nextblock_2 = 3
               CASE (3)
                  elid(2) = i
!
!     DETERMINE NUMBER ENTRIES FOR SKIPPING TO GRID ENTRIES
!
                  i = ne(idx+13) - 2
                  IF ( n==52 ) THEN
!
!     SPECIAL HANDLING FOR CHBDY
!     IF TYPE IS NEGATIVE, SAVE TYPE FLAG AFTER GRIDS.
!
                     CALL fread(ect1,0,-1,0)
                     CALL fread(ect1,type,1,0)
                     CALL fread(ect1,gp,8,0)
                     CALL fread(ect1,0,-(ne(idx+6)-ngpel-i-1),0)
                     IF ( type<0 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     IF ( type==6 ) type = 3
                     gp(9) = type
!              CHBDY
!
                  ELSEIF ( n==64 .OR. n==83 ) THEN
!
!     (2) CTRIA3 AND CQUAD4 ELEMENTS, ONE OFFSET DATA NORMAL TO PLATE.
!         OFFSET DATA COULD BE ON ELEMENT CARD OR ON PSHELL OR PCOMPI
!         CARDS
!
                     CALL fread(ect1,pid,1,0)
                     CALL fread(ect1,gp,ngpel,0)
                     CALL write(ect2,elid,2,0)
                     CALL write(ect2,gp,ngpel,0)
                     j = 5
                     IF ( n==64 ) j = 6
                     CALL fread(ect1,0,-j,0)
                     CALL fread(ect1,offset,1,0)
                     IF ( offset(1)==0.0 ) THEN
                        IF ( jcomp/=b1 ) THEN
                           SPAG_Loop_2_1: DO i = jcomp , kcomp , 2
                              IF ( ix(i)==pid ) THEN
                                 offset(1) = x(i+1)
                                 EXIT SPAG_Loop_2_1
                              ENDIF
                           ENDDO SPAG_Loop_2_1
                        ENDIF
                     ENDIF
                     CALL write(ect2,offset,1,0)
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!           CQUAD4        CTRIA3
                     IF ( i<0 ) GOTO 120
!
                     IF ( i/=0 ) CALL fread(ect1,0,-i,0)
                     CALL fread(ect1,gp,ngpel,0)
                     IF ( n==34 ) THEN
!
!     SPECIAL HANDLING OF THOSE ELEMENTS HAVING GRID OFFSET.
!     ADD THESE OFFSET DATA AFTER THE GRID POINTS
!
!     (1) CBAR ELEMENT, 2 OFFSET VECTORS (6 VALUES)
!
                        CALL write(ect2,elid,2,0)
                        CALL write(ect2,gp,ngpel,0)
                        CALL fread(ect1,0,-6,0)
                        CALL fread(ect1,offset,6,0)
                        CALL write(ect2,offset,6,0)
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ELSE
!               CBAR
!
                        CALL fread(ect1,0,-(ne(idx+6)-ngpel-i-1),0)
!
!     SPCIAL HANDLING OF IHEX2 AND IHEX3 WITH ZERO GRIDS
!
                        IF ( n==66 .OR. n==67 ) THEN
                           DO j = 1 , ngpel
                              IF ( gp(j)==0 ) THEN
                                 k = ihx3(j)
                                 IF ( n==66 ) k = ihx2(j)
                                 gp(j) = gp(k)
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDIF
!
                  CALL write(ect2,elid,2,0)
                  CALL write(ect2,gp,ngpel,0)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
 142              CALL write(ect2,0,0,1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
 160     ENDDO
!
         CALL clstab(ect2,rew)
         CALL close(ect1,rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE comect
