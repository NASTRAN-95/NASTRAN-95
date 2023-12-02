!*==endput.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE endput(Block)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Block(15)
   INTEGER iflg , lim
   lim = nbuff - Block(3)*2
   name = Block(1)
   CALL dsgefl
   IF ( Block(7)>0 ) THEN
      IF ( Block(6)<Block(7) ) THEN
         CALL dsmsg1(Block)
         CALL dsmsg(109)
      ENDIF
      ibase(indcbp+1) = idssh + Block(7)
      ibase(indcbp+2) = Block(4)
      nwords = Block(11)
      indcbp = indcbp + (Block(7)*nwords) + 2
      IF ( (indcbp-indbas)>lim ) CALL dsmsg(108)
      IF ( Block(3)/=0 ) THEN
         ibase(indcbp+1) = idsst + Block(7)
         ibase(indcbp+2) = Block(4) + Block(7) - 1
         indcbp = indcbp + 2
      ENDIF
   ENDIF
   IF ( Block(8)==1 ) THEN
      ibase(indcbp+1) = idsct + Block(3)*mulq3 + Block(2)
      ibase(indcbp+2) = Block(12)
      ibase(indcbp+3) = idsrt + idsc + (indclr-indbas+1)
      ibase(indclr) = idssb + Block(9) + indcbp - indclr + 2
      indcbp = indcbp + 4
      indclr = indcbp
   ENDIF
   IF ( Block(6)==Block(7) ) THEN
      IF ( Block(8)/=1 ) THEN
         iflg = Block(9)
         IF ( iflg/=idsx ) THEN
            iflg = idsp
            Block(9) = idsx
         ENDIF
         ibase(indclr) = idssb + iflg + (indcbp-indclr)
         ibase(indcbp+1) = idsrt + iflg + (indclr-indbas+1)
         ibase(indcbp+2) = idseb
         indclr = indcbp + 2
         indcbp = indclr
      ELSE
         ibase(indcbp) = idseb
      ENDIF
      CALL dswrnb
   ENDIF
!WKBD  NCL93007 11/94
!  50 CALL DSSDCB
!WKBNB NCL93007 11/94
! ACCUMULATE THE TOTAL NUMBER OF TERMS AND STRINGS
   fcb(16,ifilex) = fcb(16,ifilex) + 1
   fcb(17,ifilex) = fcb(17,ifilex) + Block(7)
   CALL dssdcb
!WKBNE NCL93007 11/94
END SUBROUTINE endput
