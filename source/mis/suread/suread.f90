!*==suread.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE suread(Ia,Nd,Nout,Itest)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SYS
   USE C_ZZZZZZ
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
   IF ( Iomode/=ird ) THEN
      Itest = 4
      Nout = 0
      RETURN
   ENDIF
   SPAG_Loop_1_1: DO
      IF ( Ioptr>Blksiz+Io ) THEN
!
!     REACHED END OF BLOCK.  REPLACE THE BLOCK CURRENTLY IN CORE BY ITS
!     LINK BLOCK.
!
         CALL fnxt(Iopbn,inxt)
         IF ( mod(Iopbn,2)==1 ) THEN
            next = andf(Buf(inxt),Jhalf)
         ELSE
            next = andf(rshift(Buf(inxt),Ihalf),Jhalf)
         ENDIF
         IF ( next==0 ) THEN
!
!     ERROR MESSAGES.
!
            CALL errmkn(indsbr,9)
            EXIT SPAG_Loop_1_1
         ELSE
            Iopbn = next
            Iolbn = Iolbn + 1
            CALL sofio(ird,Iopbn,Buf(Io-2))
            Ioptr = Io + 1
         ENDIF
      ENDIF
!
!     READ SOF INTO ARRAY IA, BUT WATCH FOR END OF GROUP AND END OF ITEM
!
      IF ( Buf(Ioptr)==ieoi ) THEN
!
!    REACHED END OF ITEM.
!
         Itest = 3
         Iomode = idle
         Ioptr = Ioptr + 1
      ELSEIF ( Buf(Ioptr)==ieog .AND. Nd/=-2 ) THEN
!
!    REACHED END OF GROUP.
!
         Itest = 2
         Ioptr = Ioptr + 1
      ELSE
!
         icount = icount + 1
         Ia(icount) = Buf(Ioptr)
         Ioptr = Ioptr + 1
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
