!*==zblpki.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE zblpki
   IMPLICIT NONE
   USE I_DSIOF
   USE I_PAKBLK
   USE I_XNSTRN
   USE C_ZBLPKX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icrow , inccnt , index , itypin , k , kk , nexrow
!
! End of declarations rewritten by SPAG
!
   iblka(15) = I
   itypin = iblka(13)
   nwords = nwrdel(itypin)
   IF ( iblka(2)>=3 ) THEN
      inccnt = 2
   ELSE
      inccnt = 1
   ENDIF
   DO k = 1 , nwords
      IF ( A(k)/=0 ) GOTO 100
   ENDDO
   GOTO 99999
 100  IF ( iblka(4)/=0 ) THEN
      nexrow = iblka(4) + iblka(7)
      icrow = iblka(15)
      IF ( icrow<nexrow ) THEN
         CALL dsmsg1(iblka)
         CALL dsmsg(119)
      ENDIF
      IF ( icrow==nexrow ) GOTO 200
      CALL endput(iblka)
      CALL putstr(iblka)
      iblka(7) = 0
   ENDIF
   icrow = iblka(15)
   iblka(4) = icrow
 200  index = (iblka(5)-1)*iblka(14) + 1
   IF ( itypin/=iblka(2) ) THEN
      CALL dsupkc(itypin,iblka(2),A,ibase(index))
   ELSE
!DIR$ NOVECTOR
      DO kk = 1 , nwords
         ibase(index+kk-1) = A(kk)
!DIR$ VECTOR
      ENDDO
   ENDIF
   iblka(5) = iblka(5) + inccnt
   iblka(7) = iblka(7) + 1
   iblka(10) = iblka(10) + iblka(11)
   IF ( iblka(6)<=iblka(7) ) THEN
      CALL endput(iblka)
      CALL putstr(iblka)
      iblka(4) = 0
      iblka(7) = 0
   ENDIF
99999 END SUBROUTINE zblpki
