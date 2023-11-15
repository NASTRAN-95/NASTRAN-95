
SUBROUTINE dswrt1(Idata)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER Idata(2)
!
! Local variable declarations
!
   INTEGER i , icount , ifin , iflg , inext , irwords
!
! End of declarations
!
   inext = 0
   IF ( Lwords<=-1 ) THEN
      Ibase(Indclr) = Idseb
      Lwords = 0
      iflg = Idsx
      IF ( Iblock==Nblock ) iflg = Idsc
   ELSE
      IF ( Nblock==Iblock ) THEN
         iflg = Idsp
      ELSE
         iflg = Idsx
      ENDIF
      IF ( Lwords<=0 ) THEN
         Ibase(Indclr) = iand(Ibase(Indclr),not(Maskq2))
         Ibase(Indclr) = ior(iflg,Ibase(Indclr))
         Ibase(Indcbp+1) = Idsrt + iflg + (Indclr-Indbas+1)
         Ibase(Indcbp+2) = Idseb
         Lwords = 0
         Indclr = Indcbp + 2
         Indcbp = Indclr
         iflg = Idsx
      ELSE
         icount = iand(Ibase(Indclr),Maskh2)
         Ibase(Indclr) = Idsrh + iflg + icount + Lwords
         DO i = 1 , Lwords
            Ibase(Indcbp+i) = Idata(i)
         ENDDO
         Indcbp = Indcbp + Lwords + 1
         Ibase(Indcbp) = Idsrt + iflg + (Indclr-Indbas+1)
         Indclr = Indcbp + 1
         Ibase(Indcbp+1) = Idseb
         iflg = Idsx
      ENDIF
   ENDIF
   CALL dswrnb
   irwords = Nwords - Lwords
   inext = inext + Lwords
   DO
      IF ( irwords>(Nbuff-5) ) THEN
         ifin = 0
         IF ( iflg==Idsc ) iflg = Idsp
         Nwords = Nbuff - 5
      ELSE
         ifin = 1
         Nwords = irwords
      ENDIF
      Ibase(Indclr) = Idsrh + iflg + Nwords
      DO i = 1 , Nwords
         Ibase(Indcbp+i) = Idata(inext+i)
      ENDDO
      Indcbp = Indcbp + Nwords
      IF ( ifin==1 ) THEN
         IF ( Ieor/=0 ) THEN
            Ibase(Indcbp+1) = Idsrt + Idsc + (Indclr-Indbas+1)
            Indclr = Indcbp + 2
            Indcbp = Indclr
         ENDIF
         EXIT
      ELSE
         inext = inext + Nwords
         Ibase(Indcbp+1) = Idsrt + iflg + (Indclr-Indbas+1)
         Ibase(Indcbp+2) = Idseb
         irwords = irwords - Nwords
         iflg = Idsx
         Indclr = Indclr + Nwords + 2
         CALL dswrnb
      ENDIF
   ENDDO
END SUBROUTINE dswrt1
