!*==pltset.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pltset
   USE c_blank
   USE c_machin
   USE c_pltdat
   USE c_system
   USE c_xmssg
   USE c_xxparm
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: chrwrd , i , itrack , nout , pbfsiz , pltype
   REAL , DIMENSION(2) :: cntchr , xymax
   REAL :: cntsin
   INTEGER , SAVE :: plt1 , plt2
   REAL , DIMENSION(2) , SAVE :: xysize
   EXTERNAL tapbit
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COMMENTS FROM G.C. -
!     THE DRIVER FOR DMAP MODULE PLTSET IS DPLTST
!     THIS ROUTINE HAS NOTHING TO DO WITH DPLTST.  IT IS CALLED ONLY
!     BY PARAM (IN MODULE PLOT), XYPLOT, AND SEEMAT
!
!
   !>>>>EQUIVALENCE (Pdata(1),Xymax(1)) , (Pdata(3),Cntsin) , (Pdata(4),Cntchr(1)) , (Pdata(10),Pltype) , (Pdata(12),Pbfsiz) ,           &
!>>>>    & (Nout,Ksystm(2)) , (Chrwrd,Ksystm(41)) , (Itrack,Ksystm(59))
   DATA xysize/11.0 , 8.5/ , plt1 , plt2/4HPLT1 , 4HPLT2/
!
!     INITIALIZE -PDATA-
!
   DO i = 1 , 20
      pdata(i) = pltdat(i,ploter)
   ENDDO
!
!     PLT2 FILE WAS HARD CODED INTO THE 11TH WORD OF PLTDAT(11,PLOTER)
!     BY PLOTBD. IF USER REQUESTS PLT1 FILE, WE MUST MAKE A SWITCH HERE
!
   IF ( .NOT.tapbit(plt2) .AND. tapbit(plt1) ) pdata(11) = plt1
   IF ( pltnum==0 .AND. offscl==0 ) WRITE (nout,99001) uim , pdata(11)
99001 FORMAT (A29,', PLOT FILE GOES TO ',A4)
!
   IF ( offscl==0 ) offscl = 1
!
!     SCALE THE CHARACTURE SIZE BEFORE SETTING BORDERS
!
   cntchr(1) = chrscl*cntchr(1)
   cntchr(2) = chrscl*cntchr(2)
   pbufsz = pbfsiz/chrwrd
!
!     FOR UNIVAC 9 TRACK CALCOMP PLOT TAPES QUARTER WORD MODE WILL
!     BE USED LIMITING THE NUMBER OF CHARACTERS PER WORD TO 4
!     ITRACK = 2 FOR 9 TRACK TAPES - OTHERWISE 1 FOR 7 TRACK TAPES
!     THE DEFAULT IS FOR 7 TRACK TAPES
!
!     IF (MACH.EQ.3 .AND. ITRACK.EQ.2) PBUFSZ = PBFSIZ/4
!
!     SINCE GENERAL PLOTTER IS THE ONLY ONE SUPPORTED BY NASTRAN, THE
!     PLOT BUFFER FOR UNIVAC MUST BE 500 WORDS FOR BOTH FORTRAN V AND
!     ASCII FORTRAN. (SEE PROG. MANUAL PAGE 6.10-15)
!
   IF ( mach==3 ) pbufsz = pbfsiz/6
!
   pltype = model
!
!     INITIALIZE PAPER SIZE AND BORDERS
!
   DO i = 1 , 2
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( iabs(pltype)<2 ) THEN
               IF ( pltype<=0 ) THEN
!
!     CRT PLOTTERS
!
                  axymax(i) = xymax(i) - cntchr(i)
                  xyedge(i) = cntchr(i)*.5
               ELSE
                  axymax(i) = xymax(i)
                  xyedge(i) = 0.
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( iabs(pltype)==2 ) THEN
!
!     TABLE PLOTTERS
!
               IF ( papsiz(i)<=0.0 ) papsiz(i) = xysize(i)
!
               IF ( cntsin*papsiz(i)>xymax(i) ) papsiz(i) = xymax(i)/cntsin
            ELSE
!
!     DRUM PLOTTERS
!
               IF ( papsiz(i)<=0.0 ) papsiz(i) = xymax(i)/cntsin
               IF ( i==1 ) THEN
               ELSEIF ( i==2 ) THEN
                  IF ( cntsin*papsiz(i)>xymax(i) ) papsiz(i) = xymax(i)/cntsin
               ELSE
                  IF ( papsiz(i)<=0.0 ) papsiz(i) = xysize(i)
                  IF ( cntsin*papsiz(i)>xymax(i) ) papsiz(i) = xymax(i)/cntsin
               ENDIF
            ENDIF
            axymax(i) = cntsin*papsiz(i) - cntsin
            xyedge(i) = cntsin*.5
            spag_nextblock_1 = 2
         CASE (2)
            reg(i,1) = 0.
            reg(i,2) = axymax(i)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
END SUBROUTINE pltset
