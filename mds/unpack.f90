
SUBROUTINE unpack(*,File,A)
   IMPLICIT NONE
   INCLUDE 'PAKBLK.COM'
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Incr , Irobgn , Itypot , Lasrow
   COMMON /unpakx/ Itypot , Irobgn , Lasrow , Incr
   INTEGER File
   REAL A(4)
   INTEGER idiff , ilsrow , index1 , index2 , irow , itype , k , kk , kkk , large , num , numinc , numlef
   DATA large/65536/
   Name = File
   num = Nwrdel(iabs(Itypot))
   CALL dsipk1(Iblkd,Itypot)
   IF ( Iretrn==1 ) GOTO 800
   IF ( Irobgn<=0 .OR. Lasrow<=0 ) THEN
      Irobgn = Iblkd(4)
      irow = Irobgn
      ilsrow = large
   ELSE
      irow = Irobgn
      ilsrow = Lasrow
   ENDIF
   index2 = 1
   itype = Iblkd(13)
   index1 = (Iblkd(5)-1)*Iblkd(14) + 1
   numinc = num*Incr
 100  IF ( Iblkd(4)>ilsrow ) GOTO 500
   IF ( (Iblkd(4)+Iblkd(6)-1)<Irobgn ) GOTO 400
 200  idiff = (Iblkd(4)+Iblkd(7)) - irow
   Iblkd(7) = Iblkd(7) + 1
   IF ( idiff/=0 ) THEN
      IF ( idiff<0 ) GOTO 300
      DO k = 1 , num
         DO kkk = 1 , idiff
            A(index2+k-1+(kkk-1)*numinc) = 0.
         ENDDO
      ENDDO
      index2 = index2 + idiff*numinc
      irow = irow + idiff
   ENDIF
   IF ( Iblkd(2)/=itype ) THEN
      CALL dsupkc(Iblkd(2),itype,Ibase(index1),A(index2))
   ELSE
!DIR$ NOVECTOR
      DO k = 1 , num
         A(index2+k-1) = Ibase(index1+k-1)
!DIR$ VECTOR
      ENDDO
   ENDIF
   IF ( irow>=ilsrow ) GOTO 600
   irow = irow + 1
   index2 = index2 + numinc
 300  index1 = index1 + Iblkd(11)
   IF ( Iblkd(7)/=Iblkd(6) ) GOTO 200
 400  CALL endget(Iblkd)
   CALL getstr(*500,Iblkd)
   index1 = (Iblkd(5)-1)*Iblkd(14) + 1
   Iblkd(7) = 0
   IF ( Iblkd(8)<1 ) GOTO 100
 500  IF ( ilsrow==large ) THEN
      Lasrow = irow - 1
      GOTO 700
   ELSE
      numlef = Lasrow - irow + 1
      IF ( numlef>0 ) THEN
         DO kk = 1 , num
            DO k = 1 , numlef
               A(index2+kk-1+(k-1)*numinc) = 0.
            ENDDO
         ENDDO
      ENDIF
   ENDIF
 600  IF ( Iblkd(8)<1 ) THEN
      CALL dsskrc
      CALL dssdcb
   ENDIF
 700  RETURN
 800  RETURN 1
END SUBROUTINE unpack
