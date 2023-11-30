
SUBROUTINE dsgnwr
   IMPLICIT NONE
   INCLUDE 'XNSTRN.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER Isybuf , Iwr
   COMMON /system/ Isybuf , Iwr
   CHARACTER*4 Cbuff(3)
   INTEGER i , ialloc , iavail , iccer , idevic , idsnr , ifirst , inext , iop , isave , istrb , itest , kk , lasblk , maxpr1
   !>>>>EQUIVALENCE (Cbuff,Ibase)
   Idsn = Mdsfcb(2,Ifilex)
   idsnr = Idsn
 100  istrb = Fcb(5,idsnr)
   IF ( Nblock>=istrb ) THEN
      ialloc = Fcb(7,idsnr)
      IF ( Nblock<=(ialloc+istrb-1) ) GOTO 200
      IF ( Idsn==8 ) CALL dsmsg(9)
      inext = iand(Mdsfcb(3,idsnr),Maskh2)
      IF ( inext==0 ) THEN
         maxpr1 = MAXPRI + 1
         DO i = maxpr1 , MAXFCB
            iavail = Mdsfcb(3,i)
            IF ( iavail==0 ) THEN
               ifirst = ialloc + istrb
               ialloc = 20000000
               Fcb(5,i) = ifirst
               Fcb(6,i) = ifirst - 1
               Mdsfcb(3,i) = idsnr*Mulq2
               inext = i
               Mdsfcb(3,idsnr) = ior(Mdsfcb(3,idsnr),i)
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ELSE
      inext = Mdsfcb(3,idsnr)/Mulq2
   ENDIF
   idsnr = inext
   IF ( idsnr>=1 .AND. idsnr<=Maxdsn ) GOTO 100
   CALL dsmsg(122)
 200  IF ( Idsn/=idsnr ) THEN
      CALL dsclos(Idsn)
      Mdsfcb(1,Idsn) = iand(Mdsfcb(1,Idsn),Maskh1)
      Idsn = idsnr
      Mdsfcb(1,Idsn) = ior(Mdsfcb(1,Idsn),Maskh2)
      Mdsfcb(2,Ifilex) = Idsn
      CALL dsmsg(8)
      idevic = 0
      DO kk = 1 , Numdev
         Mdsnam(Idsn)(1:2) = Dev(kk)
         isave = iop
         iop = 0
         CALL dsopen(Mdsnam(Idsn),Idsn,iop)
         iop = isave
         Cbuff(Indbas) = Mdsnam(Idsn)
         CALL dswrit(Idsn,Ibase(Indbas+3),Nbuff,Ioblk,iccer)
         IF ( iccer==0 ) GOTO 300
         CALL dsclos(Idsn)
      ENDDO
      WRITE (Iwr,99001)
99001 FORMAT (///,' NO MORE DISK SPACE AVAILABLE, JOB ABORTED.')
      CALL dsmsg(122)
   ENDIF
 300  Ioblk = Nblock - istrb + 1
   CALL dswrit(Idsn,Ibase(Indbas+3),Nbuff,Ioblk,iccer)
   IF ( iccer/=0 ) THEN
      IF ( iccer/=28 ) CALL dsmsg(101)
      IF ( Idsn>21 .OR. Idsn==8 .OR. Idsn==9 ) THEN
! ALLOW XPDT TO EXTEND (IDSN=9)---NOTE IDSN=8 IS THE NPTP
         itest = index(Mdsnam(8),'ZAP')
         IF ( Idsn/=8 .OR. itest/=0 ) THEN
            Fcb(7,Ifilex) = Fcb(6,Ifilex)
            idsnr = Idsn
            GOTO 100
         ENDIF
      ENDIF
      WRITE (Iwr,99002)
99002 FORMAT (///,' NO MORE DISK SPACE AVAILABLE IN DEFAULT DIRECTORY',' FOR PERMANENT FILES',/,' JOB ABORTED')
      CALL dsmsg(122)
   ELSE
      lasblk = Fcb(6,Idsn)
      IF ( lasblk<Nblock ) Fcb(6,Idsn) = Fcb(6,Idsn) + 1
   ENDIF
END SUBROUTINE dsgnwr