!*==pack.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE pack(A,File,Mcb)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_PAKBLK
   USE I_XNSTRN
   USE C_DDIOSV
   USE C_PACKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: A
   INTEGER :: File
   INTEGER , DIMENSION(7) :: Mcb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: incrr , indea1 , indexa , indexb , irow , k , kk , klast , klim , lasind , ncnt , nwdin
!
! End of declarations rewritten by SPAG
!
   name = File
   iblkc(1) = name
   iblkc(2) = Itypot
   iblkc(3) = 0
   iblkc(4) = 0
   iblkc(7) = 0
   iblkc(8) = -1
   iblkc(9) = Itypin
   iblkc(10) = 0
   IF ( Itypin>0 .AND. Itypin<=4 ) THEN
      IF ( Itypot>0 .AND. Itypot<=4 ) GOTO 100
   ENDIF
   CALL dsmsg1(iblkc)
   CALL dsmsg(118)
 100  nwdin = nwrdel(Itypin)
   iblkc(12) = Mcb(2) + 1
   CALL dsgefl
   Iflpos(1,ifilex) = fcb(3,ifilex)
   Iflpos(2,ifilex) = fcb(4,ifilex)
   CALL putstr(iblkc)
   ieor = 0
   indexa = 0
   irow = Irobgn
   indexb = (iblkc(5)-1)*iblkc(14) + 1
!DIR$ NOVECTOR
 200  DO k = 1 , nwdin
      IF ( A(indexa+k)/=0 ) GOTO 300
   ENDDO
!DIR$ VECTOR
   lasind = (Lasrow-irow+1)*Incr*nwdin
   klim = lasind + Incr
   klast = klim
   incrr = Incr*nwdin
   DO kk = 1 , nwdin
      indea1 = indexa - 1 + kk
      DO k = 1 , lasind , incrr
         IF ( A(indea1+k)/=0 ) THEN
            IF ( k<klast ) klast = k
            EXIT
         ENDIF
      ENDDO
   ENDDO
   ncnt = ((klast-1)/incrr) - 1
   IF ( klast==klim ) ncnt = Lasrow - irow
   irow = irow + ncnt
   indexa = indexa + ncnt*(nwdin*Incr)
   ieor = 1
   GOTO 400
 300  IF ( iblkc(7)==0 ) THEN
      iblkc(4) = irow
   ELSEIF ( ieor/=0 ) THEN
      CALL endput(iblkc)
      CALL putstr(iblkc)
      iblkc(7) = 0
      indexb = (iblkc(5)-1)*iblkc(14) + 1
      iblkc(4) = irow
   ENDIF
   IF ( Itypin/=Itypot ) THEN
      CALL dsupkc(Itypin,Itypot,A(indexa+1),ibase(indexb))
   ELSE
!DIR$ NOVECTOR
      DO k = 1 , nwdin
         ibase(indexb+k-1) = A(indexa+k)
!DIR$ VECTOR
      ENDDO
   ENDIF
   ieor = 0
   indexb = indexb + iblkc(11)
   iblkc(7) = iblkc(7) + 1
   iblkc(10) = iblkc(10) + iblkc(11)
   IF ( iblkc(7)>=iblkc(6) ) THEN
      CALL endput(iblkc)
      CALL putstr(iblkc)
      iblkc(7) = 0
      indexb = (iblkc(5)-1)*iblkc(14) + 1
   ENDIF
 400  indexa = indexa + (Incr*nwdin)
   irow = irow + 1
   IF ( irow<=Lasrow ) GOTO 200
   CALL dsbpnk(iblkc,Mcb)
END SUBROUTINE pack
