!*==suread.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE suread(Ia,Nd,Nout,Itest)
   USE c_machin
   USE c_sof
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ia
   INTEGER :: Nd
   INTEGER :: Nout
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icount , inxt , next
   INTEGER , SAVE :: idle , ieog , ieoi , indsbr , ird
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , errmkn , fnxt , rshift , sofio
!
! End of declarations rewritten by SPAG
!
!
!     READS DATA FROM THE SOF INTO THE ARRAY IA.  ND IS AN INPUT
!     PARAMETER INDICATING THE NUMBER OF WORDS DESIRED.  ND=-1 MEANS
!     READ UNTIL END OF GROUP. NOUT IS AN OUTPUT PARAMETER INDICATING
!     THE NUMBER OF WORD THAT HAVE BEEN READ.  ITEST IS AN OUTPUT
!     PARAMETER WHERE ITEST=3 MEANS END OF ITEM ENCOUNTERED, ITEST=2
!     MEANS END OF GROUP ENCOUNTERED, AND ITEST=1 OTHERWISE.
!
   DATA idle , ird/0 , 1/
   DATA ieog , ieoi/4H$EOG , 4H$EOI/
   DATA indsbr/19/ , nmsbr/4HSURE , 4HAD  /
!
   CALL chkopn(nmsbr(1))
   icount = 0
   IF ( iomode/=ird ) THEN
      Itest = 4
      Nout = 0
      RETURN
   ENDIF
   SPAG_Loop_1_1: DO
      IF ( ioptr>blksiz+io ) THEN
!
!     REACHED END OF BLOCK.  REPLACE THE BLOCK CURRENTLY IN CORE BY ITS
!     LINK BLOCK.
!
         CALL fnxt(iopbn,inxt)
         IF ( mod(iopbn,2)==1 ) THEN
            next = andf(buf(inxt),jhalf)
         ELSE
            next = andf(rshift(buf(inxt),ihalf),jhalf)
         ENDIF
         IF ( next==0 ) THEN
!
!     ERROR MESSAGES.
!
            CALL errmkn(indsbr,9)
            EXIT SPAG_Loop_1_1
         ELSE
            iopbn = next
            iolbn = iolbn + 1
            CALL sofio(ird,iopbn,buf(io-2))
            ioptr = io + 1
         ENDIF
      ENDIF
!
!     READ SOF INTO ARRAY IA, BUT WATCH FOR END OF GROUP AND END OF ITEM
!
      IF ( buf(ioptr)==ieoi ) THEN
!
!    REACHED END OF ITEM.
!
         Itest = 3
         iomode = idle
         ioptr = ioptr + 1
      ELSEIF ( buf(ioptr)==ieog .AND. Nd/=-2 ) THEN
!
!    REACHED END OF GROUP.
!
         Itest = 2
         ioptr = ioptr + 1
      ELSE
!
         icount = icount + 1
         Ia(icount) = buf(ioptr)
         ioptr = ioptr + 1
         IF ( icount/=Nd ) CYCLE
!
!     READ THE REQUIRED NUMBER OF WORDS.
!
         Itest = 1
      ENDIF
      Nout = icount
      RETURN
   ENDDO SPAG_Loop_1_1
END SUBROUTINE suread
