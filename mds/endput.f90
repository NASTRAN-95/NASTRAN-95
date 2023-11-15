
SUBROUTINE endput(Block)
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
   INTEGER iflg , lim
!
! End of declarations
!
   lim = Nbuff - Block(3)*2
   Name = Block(1)
   CALL dsgefl
   IF ( Block(7)>0 ) THEN
      IF ( Block(6)<Block(7) ) THEN
         CALL dsmsg1(Block)
         CALL dsmsg(109)
      ENDIF
      Ibase(Indcbp+1) = Idssh + Block(7)
      Ibase(Indcbp+2) = Block(4)
      Nwords = Block(11)
      Indcbp = Indcbp + (Block(7)*Nwords) + 2
      IF ( (Indcbp-Indbas)>lim ) CALL dsmsg(108)
      IF ( Block(3)/=0 ) THEN
         Ibase(Indcbp+1) = Idsst + Block(7)
         Ibase(Indcbp+2) = Block(4) + Block(7) - 1
         Indcbp = Indcbp + 2
      ENDIF
   ENDIF
   IF ( Block(8)==1 ) THEN
      Ibase(Indcbp+1) = Idsct + Block(3)*Mulq3 + Block(2)
      Ibase(Indcbp+2) = Block(12)
      Ibase(Indcbp+3) = Idsrt + Idsc + (Indclr-Indbas+1)
      Ibase(Indclr) = Idssb + Block(9) + Indcbp - Indclr + 2
      Indcbp = Indcbp + 4
      Indclr = Indcbp
   ENDIF
   IF ( Block(6)==Block(7) ) THEN
      IF ( Block(8)/=1 ) THEN
         iflg = Block(9)
         IF ( iflg/=Idsx ) THEN
            iflg = Idsp
            Block(9) = Idsx
         ENDIF
         Ibase(Indclr) = Idssb + iflg + (Indcbp-Indclr)
         Ibase(Indcbp+1) = Idsrt + iflg + (Indclr-Indbas+1)
         Ibase(Indcbp+2) = Idseb
         Indclr = Indcbp + 2
         Indcbp = Indclr
      ELSE
         Ibase(Indcbp) = Idseb
      ENDIF
      CALL dswrnb
   ENDIF
!WKBD  NCL93007 11/94
!  50 CALL DSSDCB
!WKBNB NCL93007 11/94
! ACCUMULATE THE TOTAL NUMBER OF TERMS AND STRINGS
   Fcb(16,Ifilex) = Fcb(16,Ifilex) + 1
   Fcb(17,Ifilex) = Fcb(17,Ifilex) + Block(7)
   CALL dssdcb
!WKBNE NCL93007 11/94
END SUBROUTINE endput
