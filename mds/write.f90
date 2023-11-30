
SUBROUTINE write(File,Idata,N,Eorflg)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Iflpos(2,80)
   COMMON /ddiosv/ Iflpos
   INTEGER Eorflg , File , N
   INTEGER Idata(2)
   INTEGER i
   Ieor = Eorflg
   Name = File
   Nwords = N
   IF ( Nwords<0 ) CALL dsmsg(6)
   CALL dsgefl
   IF ( Nwords/=0 ) THEN
      IF ( Iprvop==0 ) CALL dsmsg(7)
      IF ( Indclr/=Indcbp ) THEN
         Iblock = Ibase(Indbas+Nbuff+2)
      ELSE
         Iflpos(1,Ifilex) = Fcb(3,Ifilex)
         Iflpos(2,Ifilex) = Fcb(4,Ifilex)
         Ibase(Indclr) = Idsrh + Idsc
         Iblock = Nblock
         Ibase(Indbas+Nbuff+2) = Nblock
      ENDIF
      Lwords = Nbuff - (Indcbp-Indbas)
      IF ( Lwords>=Nwords ) THEN
         DO i = 1 , Nwords
            Ibase(Indcbp+i) = Idata(i)
         ENDDO
         Ibase(Indclr) = Ibase(Indclr) + Nwords
         Indcbp = Indcbp + Nwords
      ELSE
         CALL dswrt1(Idata)
!WKBI SPR94013 11/94
         Ibase(Indbas+Nbuff+2) = Iblock
         CALL dssdcb
         GOTO 99999
      ENDIF
   ELSEIF ( Indcbp==Indclr ) THEN
      Lwords = Nbuff - (Indclr-Indbas) - 2
      IF ( Lwords<=0 ) THEN
         Ibase(Indclr) = Idseb
         CALL dswrnb
         Ibase(Indclr) = Idsrh + Idsc
         Ibase(Indclr+1) = Idsrt + Idsc + (Indclr-Indbas+1)
         Indcbp = Indcbp + 2
         Indclr = Indcbp
         CALL dssdcb
         GOTO 99999
      ENDIF
   ENDIF
   IF ( Ieor/=0 ) THEN
      IF ( Indcbp==Indclr ) THEN
         Ibase(Indcbp) = Idsrh + Idsc
         Iflpos(1,Ifilex) = Fcb(3,Ifilex)
         Iflpos(2,Ifilex) = Fcb(4,Ifilex)
      ENDIF
      Ibase(Indcbp+1) = Idsrt + Idsc + (Indclr-Indbas+1)
      Indcbp = Indcbp + 2
      Indclr = Indcbp
      CALL dssdcb
      GOTO 99999
   ENDIF
   CALL dssdcb
99999 RETURN
END SUBROUTINE write
