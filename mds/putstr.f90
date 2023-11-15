
SUBROUTINE putstr(Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER Block(15)
!
! Local variable declarations
!
   INTEGER idiv(4) , iflg , lim , nelm
!
! End of declarations
!
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
   Name = Block(1)
   CALL dsgefl
   lim = Indbas + Nbuff + 2
   IF ( Block(8)==-1 ) THEN
      Nwords = Nwrdel(Block(2))
      Block(14) = idiv(Block(2))
      Block(11) = Nwords
      Block(8) = 0
      Block(9) = Idsc
      iflg = Idsc
      IF ( (lim-Indcbp-6-Block(3)*2)<Nwords ) THEN
         Ibase(Indcbp) = Idseb
         CALL dswrnb
         lim = Indbas + Nbuff + 2
      ENDIF
      Ibase(Indcbp+1) = Idsch + Block(3)*Mulq3 + Block(2)
      Ibase(Indcbp+2) = Block(12)
      Indcbp = Indcbp + 2
   ELSE
      Nwords = Block(11)
      iflg = Block(9)
   ENDIF
   DO
      Nlr = iabs(mod(Indcbp+2,Block(14)))
      nelm = (lim-Indcbp-Nlr-6-Block(3)*2)/Nwords
      IF ( nelm>=1 ) THEN
         Block(6) = nelm
         Block(7) = 0
         Block(5) = (Indcbp+Nlr+2)/Block(14) + 1
         IF ( Nlr/=0 ) THEN
            Ibase(Indcbp+1) = Idssd
            Indcbp = Indcbp + 1
         ENDIF
         CALL dssdcb
         EXIT
      ELSE
         iflg = Block(9)
         IF ( iflg/=Idsx ) THEN
            iflg = Idsp
            Block(9) = Idsx
         ENDIF
         Ibase(Indclr) = Idssb + iflg + (Indcbp-Indclr)
         Ibase(Indcbp+1) = Idsrt + iflg + (Indclr-Indbas+1)
         Ibase(Indcbp+2) = Idseb
         Indclr = Indcbp + 2
         CALL dswrnb
         lim = Indbas + Nbuff + 2
      ENDIF
   ENDDO
END SUBROUTINE putstr
