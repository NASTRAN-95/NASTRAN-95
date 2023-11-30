
SUBROUTINE pltset
   IMPLICIT NONE
   REAL Axymax(2) , Chrscl , Cntchr(2) , Cntsin , Papsiz(2) , Reg(2,2) , Skp235(226) , Skp4(4) , Skparm(6) , Xyedge(11) , Xymax(2)
   INTEGER Chrwrd , Itrack , Ksystm(65) , Mach , Model , Nout , Offscl , Pbfsiz , Pbufsz , Pdata(20) , Ploter , Pltdat(20,1) ,      &
         & Pltnum , Pltype
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Skp4 , Pltnum
   COMMON /machin/ Mach
   COMMON /pltdat/ Model , Ploter , Reg , Axymax , Xyedge , Chrscl , Pdata , Pltdat
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xxparm/ Pbufsz , Skparm , Papsiz , Skp235 , Offscl
   INTEGER i , plt1 , plt2
   LOGICAL tapbit
   REAL xysize(2)
!
!     COMMENTS FROM G.C. -
!     THE DRIVER FOR DMAP MODULE PLTSET IS DPLTST
!     THIS ROUTINE HAS NOTHING TO DO WITH DPLTST.  IT IS CALLED ONLY
!     BY PARAM (IN MODULE PLOT), XYPLOT, AND SEEMAT
!
!
   EQUIVALENCE (Pdata(1),Xymax(1)) , (Pdata(3),Cntsin) , (Pdata(4),Cntchr(1)) , (Pdata(10),Pltype) , (Pdata(12),Pbfsiz) ,           &
    & (Nout,Ksystm(2)) , (Chrwrd,Ksystm(41)) , (Itrack,Ksystm(59))
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
   IF ( Pltnum==0 .AND. Offscl==0 ) WRITE (Nout,99001) Uim , Pdata(11)
99001 FORMAT (A29,', PLOT FILE GOES TO ',A4)
!
   IF ( Offscl==0 ) Offscl = 1
!
!     SCALE THE CHARACTURE SIZE BEFORE SETTING BORDERS
!
   Cntchr(1) = Chrscl*Cntchr(1)
   Cntchr(2) = Chrscl*Cntchr(2)
   Pbufsz = Pbfsiz/Chrwrd
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
   IF ( Mach==3 ) Pbufsz = Pbfsiz/6
!
   Pltype = Model
!
!     INITIALIZE PAPER SIZE AND BORDERS
!
   DO i = 1 , 2
      IF ( iabs(Pltype)<2 ) THEN
         IF ( Pltype<=0 ) THEN
!
!     CRT PLOTTERS
!
            Axymax(i) = Xymax(i) - Cntchr(i)
            Xyedge(i) = Cntchr(i)*.5
         ELSE
            Axymax(i) = Xymax(i)
            Xyedge(i) = 0.
         ENDIF
         GOTO 50
      ELSEIF ( iabs(Pltype)==2 ) THEN
!
!     TABLE PLOTTERS
!
         IF ( Papsiz(i)<=0.0 ) Papsiz(i) = xysize(i)
!
         IF ( Cntsin*Papsiz(i)>Xymax(i) ) Papsiz(i) = Xymax(i)/Cntsin
      ELSE
!
!     DRUM PLOTTERS
!
         IF ( Papsiz(i)<=0.0 ) Papsiz(i) = Xymax(i)/Cntsin
         IF ( i==1 ) THEN
         ELSEIF ( i==2 ) THEN
            IF ( Cntsin*Papsiz(i)>Xymax(i) ) Papsiz(i) = Xymax(i)/Cntsin
         ELSE
            IF ( Papsiz(i)<=0.0 ) Papsiz(i) = xysize(i)
            IF ( Cntsin*Papsiz(i)>Xymax(i) ) Papsiz(i) = Xymax(i)/Cntsin
         ENDIF
      ENDIF
      Axymax(i) = Cntsin*Papsiz(i) - Cntsin
      Xyedge(i) = Cntsin*.5
 50   Reg(i,1) = 0.
      Reg(i,2) = Axymax(i)
   ENDDO
!
END SUBROUTINE pltset
