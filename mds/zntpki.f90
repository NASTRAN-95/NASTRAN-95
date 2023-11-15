
SUBROUTINE zntpki
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'XNSTRN.COM'
!
! COMMON variable declarations
!
   INTEGER A(4) , I , Iendrc , Ieol
   COMMON /zntpkx/ A , I , Ieol , Iendrc
!
! Local variable declarations
!
   INTEGER index , itypot , kk , num
!
! End of declarations
!
   Iretrn = 0
   I = Iblkb(4)
   index = (Iblkb(5)-1)*Iblkb(14) + 1 + Iblkb(7)*Iblkb(11)
   itypot = Iblkb(13)
!DIR$ NOVECTOR
   IF ( itypot/=Iblkb(2) ) THEN
      CALL dsupkc(Iblkb(2),itypot,Ibase(index),A)
   ELSE
      num = Nwrdel(itypot)
      DO kk = 1 , num
         A(kk) = Ibase(index+kk-1)
!DIR$ VECTOR
      ENDDO
   ENDIF
   Iblkb(4) = Iblkb(4) + 1
   Iblkb(7) = Iblkb(7) + 1
   Iblkb(10) = Iblkb(4)
   IF ( Iblkb(7)<Iblkb(6) ) GOTO 200
   CALL endget(Iblkb)
   CALL getstr(*100,Iblkb)
 100  Iblkb(7) = 0
 200  IF ( Iretrn/=0 ) THEN
      Ieol = 1
      Iendrc = 1
   ELSE
      Ieol = 0
      Iendrc = 0
   ENDIF
END SUBROUTINE zntpki
