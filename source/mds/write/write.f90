!*==write.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE write(File,Idata,N,Eorflg)
   USE i_dsiof
   USE i_xnstrn
   USE c_ddiosv
   IMPLICIT NONE
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
         iflpos(1,ifilex) = fcb(3,ifilex)
         iflpos(2,ifilex) = fcb(4,ifilex)
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
         RETURN
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
         RETURN
      ENDIF
   ENDIF
   IF ( ieor/=0 ) THEN
      IF ( indcbp==indclr ) THEN
         ibase(indcbp) = idsrh + idsc
         iflpos(1,ifilex) = fcb(3,ifilex)
         iflpos(2,ifilex) = fcb(4,ifilex)
      ENDIF
      ibase(indcbp+1) = idsrt + idsc + (indclr-indbas+1)
      indcbp = indcbp + 2
      indclr = indcbp
      CALL dssdcb
      RETURN
   ENDIF
   CALL dssdcb
END SUBROUTINE write
