!*==dswrt1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dswrt1(Idata)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Idata(2)
   INTEGER i , icount , ifin , iflg , inext , irwords
   inext = 0
   IF ( lwords<=-1 ) THEN
      ibase(indclr) = idseb
      lwords = 0
      iflg = idsx
      IF ( iblock==nblock ) iflg = idsc
   ELSE
      IF ( nblock==iblock ) THEN
         iflg = idsp
      ELSE
         iflg = idsx
      ENDIF
      IF ( lwords<=0 ) THEN
         ibase(indclr) = iand(ibase(indclr),not(maskq2))
         ibase(indclr) = ior(iflg,ibase(indclr))
         ibase(indcbp+1) = idsrt + iflg + (indclr-indbas+1)
         ibase(indcbp+2) = idseb
         lwords = 0
         indclr = indcbp + 2
         indcbp = indclr
         iflg = idsx
      ELSE
         icount = iand(ibase(indclr),maskh2)
         ibase(indclr) = idsrh + iflg + icount + lwords
         DO i = 1 , lwords
            ibase(indcbp+i) = Idata(i)
         ENDDO
         indcbp = indcbp + lwords + 1
         ibase(indcbp) = idsrt + iflg + (indclr-indbas+1)
         indclr = indcbp + 1
         ibase(indcbp+1) = idseb
         iflg = idsx
      ENDIF
   ENDIF
   CALL dswrnb
   irwords = nwords - lwords
   inext = inext + lwords
   SPAG_Loop_1_1: DO
      IF ( irwords>(nbuff-5) ) THEN
         ifin = 0
         IF ( iflg==idsc ) iflg = idsp
         nwords = nbuff - 5
      ELSE
         ifin = 1
         nwords = irwords
      ENDIF
      ibase(indclr) = idsrh + iflg + nwords
      DO i = 1 , nwords
         ibase(indcbp+i) = Idata(inext+i)
      ENDDO
      indcbp = indcbp + nwords
      IF ( ifin==1 ) THEN
         IF ( ieor/=0 ) THEN
            ibase(indcbp+1) = idsrt + idsc + (indclr-indbas+1)
            indclr = indcbp + 2
            indcbp = indclr
         ENDIF
         EXIT SPAG_Loop_1_1
      ELSE
         inext = inext + nwords
         ibase(indcbp+1) = idsrt + iflg + (indclr-indbas+1)
         ibase(indcbp+2) = idseb
         irwords = irwords - nwords
         iflg = idsx
         indclr = indclr + nwords + 2
         CALL dswrnb
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE dswrt1
