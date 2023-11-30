
SUBROUTINE read(*,*,File,Idata,N,Ieorfl,M)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Ieorfl , M , N
   INTEGER Idata(2)
   INTEGER id , idiff , ilim , ireq , iwords , k , l , lasblk
   Name = File
   Nwords = N
   Ieor = Ieorfl
   Iretrn = 0
   CALL dsgefl
   IF ( Iprvop/=0 ) CALL dsmsg(4)
   id = iand(Ibase(Indclr),Maskq1)
   IF ( id==Idseb ) THEN
      CALL dbmlbk(lasblk)
      IF ( lasblk>Nblock ) THEN
         CALL dsrdnb
         id = iand(Ibase(Indclr),Maskq1)
      ELSE
         Iretrn = 1
         GOTO 100
      ENDIF
   ENDIF
   IF ( id==Idsrh ) THEN
      iwords = iand(Ibase(Indclr),Maskh2)
      idiff = Indcbp - Indclr
      iwords = iwords - idiff
      ireq = iabs(Nwords)
      IF ( ireq>iwords ) THEN
         CALL dsrdmb(Idata,M)
      ELSE
         IF ( Nwords>0 ) THEN
            l = 1
            ilim = Indcbp + Nwords - 1
            DO k = Indcbp , ilim
               Idata(l) = Ibase(k+1)
               l = l + 1
            ENDDO
         ENDIF
         Indcbp = Indcbp + ireq
      ENDIF
      IF ( Ieor/=0 ) CALL dsskrc
   ELSE
      IF ( id/=Idsef ) CALL dsmsg(105)
      Indclr = Indclr + 1
      Indcbp = Indclr
      Iretrn = 1
   ENDIF
 100  CALL dssdcb
   IF ( Iretrn==2 ) RETURN 2
   IF ( Iretrn==1 ) RETURN 1
END SUBROUTINE read
