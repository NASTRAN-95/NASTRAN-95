!*==putstr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE putstr(Block)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER idiv(4) , iflg , lim , nelm
!*******************************************************
!
!       FORMAT OF THE I/O MATRIX CONTROL TABLE
!
!    WORD    QUARTER            DESCRIPTION
!       1       -       GINO FILE NAME
!       2       -       TYPE OF ELEMENTS (1,2,3,4) - REFERS TO TYPE
!                       BEING WRITTEN (BLDPK--) TO THE BUFFER OR
!                       TYPE OF ELEMENTS READ (INTPK--) FROM THE BUFFER
!       3       -       TRAILERS TO BE INCLUDED (0=NO,1=YES) ON WRITE
!                       TO BUFFER OR ARE INCLUDED ON READ FROM BUFFER
!       4       -       ROW NUMBER
!       5       -       INDEX TO STRING (RELATIVE TO /XNSTRN/)
!       6       -       NUMBER OF ELEMENTS AVAIL. OR  RESIDE IN STRING
!       7       -       NUMBER OF ELEMENTS WRITTEN TO STRING BY USER
!       8       -       BEGIN/END FLAG (-1, FIRST CALL FOR COLUMN,
!                       =0, INTERMEDIATE CALL; =1, LAST CALL)
!       9       -       INTERIM FLAG FOR COLUMN ('C','P','X')
!       10      -       COUNT OF NON-ZERO WORDS PER COLUMN
!       11      -       NUMBER OF WORDS PER ELEMENT (SEE WORD 2)
!       12      -       COLUMN NUMBER
!       13      -       TYPE OF INPUT (BLDPK) OR OUTPUT (INTPK)
!       14      -       DIVISOR FOR COMPUTING BLOCK(5)
!       15      -       ROW NUMBER ON INPUT (BLDPK)
!
!*********************************************************************
   DATA idiv/1 , 2 , 1 , 2/
   name = Block(1)
   CALL dsgefl
   lim = indbas + nbuff + 2
   IF ( Block(8)==-1 ) THEN
      nwords = nwrdel(Block(2))
      Block(14) = idiv(Block(2))
      Block(11) = nwords
      Block(8) = 0
      Block(9) = idsc
      iflg = idsc
      IF ( (lim-indcbp-6-Block(3)*2)<nwords ) THEN
         ibase(indcbp) = idseb
         CALL dswrnb
         lim = indbas + nbuff + 2
      ENDIF
      ibase(indcbp+1) = idsch + Block(3)*mulq3 + Block(2)
      ibase(indcbp+2) = Block(12)
      indcbp = indcbp + 2
   ELSE
      nwords = Block(11)
      iflg = Block(9)
   ENDIF
   SPAG_Loop_1_1: DO
      nlr = iabs(mod(indcbp+2,Block(14)))
      nelm = (lim-indcbp-nlr-6-Block(3)*2)/nwords
      IF ( nelm>=1 ) THEN
         Block(6) = nelm
         Block(7) = 0
         Block(5) = (indcbp+nlr+2)/Block(14) + 1
         IF ( nlr/=0 ) THEN
            ibase(indcbp+1) = idssd
            indcbp = indcbp + 1
         ENDIF
         CALL dssdcb
         EXIT SPAG_Loop_1_1
      ELSE
         iflg = Block(9)
         IF ( iflg/=idsx ) THEN
            iflg = idsp
            Block(9) = idsx
         ENDIF
         ibase(indclr) = idssb + iflg + (indcbp-indclr)
         ibase(indcbp+1) = idsrt + iflg + (indclr-indbas+1)
         ibase(indcbp+2) = idseb
         indclr = indcbp + 2
         CALL dswrnb
         lim = indbas + nbuff + 2
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE putstr
