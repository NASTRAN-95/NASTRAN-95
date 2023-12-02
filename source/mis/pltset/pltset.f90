!*==pltset.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pltset
   IMPLICIT NONE
   USE C_BLANK
   USE C_MACHIN
   USE C_PLTDAT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_XXPARM
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
      Pdata(i) = Pltdat(i,Ploter)
   ENDDO
!
!     PLT2 FILE WAS HARD CODED INTO THE 11TH WORD OF PLTDAT(11,PLOTER)
!     BY PLOTBD. IF USER REQUESTS PLT1 FILE, WE MUST MAKE A SWITCH HERE
!
   IF ( .NOT.tapbit(plt2) .AND. tapbit(plt1) ) Pdata(11) = plt1
   IF ( Pltnum==0 .AND. Offscl==0 ) WRITE (nout,99001) Uim , Pdata(11)
99001 FORMAT (A29,', PLOT FILE GOES TO ',A4)
!
   IF ( Offscl==0 ) Offscl = 1
!
!     SCALE THE CHARACTURE SIZE BEFORE SETTING BORDERS
!
   cntchr(1) = Chrscl*cntchr(1)
   cntchr(2) = Chrscl*cntchr(2)
   Pbufsz = pbfsiz/chrwrd
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
   IF ( Mach==3 ) Pbufsz = pbfsiz/6
!
   pltype = Model
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
                  Axymax(i) = xymax(i) - cntchr(i)
                  Xyedge(i) = cntchr(i)*.5
               ELSE
                  Axymax(i) = xymax(i)
                  Xyedge(i) = 0.
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( iabs(pltype)==2 ) THEN
!
!     TABLE PLOTTERS
!
               IF ( Papsiz(i)<=0.0 ) Papsiz(i) = xysize(i)
!
               IF ( cntsin*Papsiz(i)>xymax(i) ) Papsiz(i) = xymax(i)/cntsin
            ELSE
!
!     DRUM PLOTTERS
!
               IF ( Papsiz(i)<=0.0 ) Papsiz(i) = xymax(i)/cntsin
               IF ( i==1 ) THEN
               ELSEIF ( i==2 ) THEN
                  IF ( cntsin*Papsiz(i)>xymax(i) ) Papsiz(i) = xymax(i)/cntsin
               ELSE
                  IF ( Papsiz(i)<=0.0 ) Papsiz(i) = xysize(i)
                  IF ( cntsin*Papsiz(i)>xymax(i) ) Papsiz(i) = xymax(i)/cntsin
               ENDIF
            ENDIF
            Axymax(i) = cntsin*Papsiz(i) - cntsin
            Xyedge(i) = cntsin*.5
            spag_nextblock_1 = 2
         CASE (2)
            Reg(i,1) = 0.
            Reg(i,2) = Axymax(i)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
END SUBROUTINE pltset
