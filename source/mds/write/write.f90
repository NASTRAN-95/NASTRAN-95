!*==write.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE write(File,Idata,N,Eorflg)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_XNSTRN
   USE C_DDIOSV
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER , DIMENSION(2) :: Idata
   INTEGER :: N
   INTEGER :: Eorflg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
   ieor = Eorflg
   name = File
   nwords = N
   IF ( nwords<0 ) CALL dsmsg(6)
   CALL dsgefl
   IF ( nwords/=0 ) THEN
      IF ( iprvop==0 ) CALL dsmsg(7)
      IF ( indclr/=indcbp ) THEN
         iblock = ibase(indbas+nbuff+2)
      ELSE
         Iflpos(1,ifilex) = fcb(3,ifilex)
         Iflpos(2,ifilex) = fcb(4,ifilex)
         ibase(indclr) = idsrh + idsc
         iblock = nblock
         ibase(indbas+nbuff+2) = nblock
      ENDIF
      lwords = nbuff - (indcbp-indbas)
      IF ( lwords>=nwords ) THEN
         DO i = 1 , nwords
            ibase(indcbp+i) = Idata(i)
         ENDDO
         ibase(indclr) = ibase(indclr) + nwords
         indcbp = indcbp + nwords
      ELSE
         CALL dswrt1(Idata)
!WKBI SPR94013 11/94
         ibase(indbas+nbuff+2) = iblock
         CALL dssdcb
         GOTO 99999
      ENDIF
   ELSEIF ( indcbp==indclr ) THEN
      lwords = nbuff - (indclr-indbas) - 2
      IF ( lwords<=0 ) THEN
         ibase(indclr) = idseb
         CALL dswrnb
         ibase(indclr) = idsrh + idsc
         ibase(indclr+1) = idsrt + idsc + (indclr-indbas+1)
         indcbp = indcbp + 2
         indclr = indcbp
         CALL dssdcb
         GOTO 99999
      ENDIF
   ENDIF
   IF ( ieor/=0 ) THEN
      IF ( indcbp==indclr ) THEN
         ibase(indcbp) = idsrh + idsc
         Iflpos(1,ifilex) = fcb(3,ifilex)
         Iflpos(2,ifilex) = fcb(4,ifilex)
      ENDIF
      ibase(indcbp+1) = idsrt + idsc + (indclr-indbas+1)
      indcbp = indcbp + 2
      indclr = indcbp
      CALL dssdcb
      GOTO 99999
   ENDIF
   CALL dssdcb
99999 END SUBROUTINE write
