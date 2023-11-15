
SUBROUTINE zblpki
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'XNSTRN.COM'
!
! COMMON variable declarations
!
   INTEGER A(4) , I
   COMMON /zblpkx/ A , I
!
! Local variable declarations
!
   INTEGER icrow , inccnt , index , itypin , k , kk , nexrow
!
! End of declarations
!
   Iblka(15) = I
   itypin = Iblka(13)
   Nwords = Nwrdel(itypin)
   IF ( Iblka(2)>=3 ) THEN
      inccnt = 2
   ELSE
      inccnt = 1
   ENDIF
   DO k = 1 , Nwords
      IF ( A(k)/=0 ) GOTO 100
   ENDDO
   GOTO 99999
 100  IF ( Iblka(4)/=0 ) THEN
      nexrow = Iblka(4) + Iblka(7)
      icrow = Iblka(15)
      IF ( icrow<nexrow ) THEN
         CALL dsmsg1(Iblka)
         CALL dsmsg(119)
      ENDIF
      IF ( icrow==nexrow ) GOTO 200
      CALL endput(Iblka)
      CALL putstr(Iblka)
      Iblka(7) = 0
   ENDIF
   icrow = Iblka(15)
   Iblka(4) = icrow
 200  index = (Iblka(5)-1)*Iblka(14) + 1
   IF ( itypin/=Iblka(2) ) THEN
      CALL dsupkc(itypin,Iblka(2),A,Ibase(index))
   ELSE
!DIR$ NOVECTOR
      DO kk = 1 , Nwords
         Ibase(index+kk-1) = A(kk)
!DIR$ VECTOR
      ENDDO
   ENDIF
   Iblka(5) = Iblka(5) + inccnt
   Iblka(7) = Iblka(7) + 1
   Iblka(10) = Iblka(10) + Iblka(11)
   IF ( Iblka(6)<=Iblka(7) ) THEN
      CALL endput(Iblka)
      CALL putstr(Iblka)
      Iblka(4) = 0
      Iblka(7) = 0
   ENDIF
99999 END SUBROUTINE zblpki
