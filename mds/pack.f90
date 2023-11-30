
SUBROUTINE pack(A,File,Mcb)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Iflpos(2,80) , Incr , Irobgn , Itypin , Itypot , Lasrow
   COMMON /ddiosv/ Iflpos
   COMMON /packx / Itypin , Itypot , Irobgn , Lasrow , Incr
   INTEGER File
   INTEGER A(4) , Mcb(7)
   INTEGER incrr , indea1 , indexa , indexb , irow , k , kk , klast , klim , lasind , ncnt , nwdin
   Name = File
   Iblkc(1) = Name
   Iblkc(2) = Itypot
   Iblkc(3) = 0
   Iblkc(4) = 0
   Iblkc(7) = 0
   Iblkc(8) = -1
   Iblkc(9) = Itypin
   Iblkc(10) = 0
   IF ( Itypin>0 .AND. Itypin<=4 ) THEN
      IF ( Itypot>0 .AND. Itypot<=4 ) GOTO 100
   ENDIF
   CALL dsmsg1(Iblkc)
   CALL dsmsg(118)
 100  nwdin = Nwrdel(Itypin)
   Iblkc(12) = Mcb(2) + 1
   CALL dsgefl
   Iflpos(1,Ifilex) = Fcb(3,Ifilex)
   Iflpos(2,Ifilex) = Fcb(4,Ifilex)
   CALL putstr(Iblkc)
   Ieor = 0
   indexa = 0
   irow = Irobgn
   indexb = (Iblkc(5)-1)*Iblkc(14) + 1
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
   Ieor = 1
   GOTO 400
 300  IF ( Iblkc(7)==0 ) THEN
      Iblkc(4) = irow
   ELSEIF ( Ieor/=0 ) THEN
      CALL endput(Iblkc)
      CALL putstr(Iblkc)
      Iblkc(7) = 0
      indexb = (Iblkc(5)-1)*Iblkc(14) + 1
      Iblkc(4) = irow
   ENDIF
   IF ( Itypin/=Itypot ) THEN
      CALL dsupkc(Itypin,Itypot,A(indexa+1),Ibase(indexb))
   ELSE
!DIR$ NOVECTOR
      DO k = 1 , nwdin
         Ibase(indexb+k-1) = A(indexa+k)
!DIR$ VECTOR
      ENDDO
   ENDIF
   Ieor = 0
   indexb = indexb + Iblkc(11)
   Iblkc(7) = Iblkc(7) + 1
   Iblkc(10) = Iblkc(10) + Iblkc(11)
   IF ( Iblkc(7)>=Iblkc(6) ) THEN
      CALL endput(Iblkc)
      CALL putstr(Iblkc)
      Iblkc(7) = 0
      indexb = (Iblkc(5)-1)*Iblkc(14) + 1
   ENDIF
 400  indexa = indexa + (Incr*nwdin)
   irow = irow + 1
   IF ( irow<=Lasrow ) GOTO 200
   CALL dsbpnk(Iblkc,Mcb)
END SUBROUTINE pack
