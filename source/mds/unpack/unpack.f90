!*==unpack.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE unpack(File,A) !HIDESTARS (*,File,A)
   IMPLICIT NONE
   USE I_PAKBLK
   USE I_DSIOF
   USE I_XNSTRN
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   REAL , DIMENSION(4) :: A
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: idiff , ilsrow , index1 , index2 , irow , itype , k , kk , kkk , num , numinc , numlef
   INTEGER , SAVE :: large
!
! End of declarations rewritten by SPAG
!
   DATA large/65536/
   name = File
   num = nwrdel(iabs(Itypot))
   CALL dsipk1(iblkd,Itypot)
   IF ( iretrn==1 ) GOTO 800
   IF ( Irobgn<=0 .OR. Lasrow<=0 ) THEN
      Irobgn = iblkd(4)
      irow = Irobgn
      ilsrow = large
   ELSE
      irow = Irobgn
      ilsrow = Lasrow
   ENDIF
   index2 = 1
   itype = iblkd(13)
   index1 = (iblkd(5)-1)*iblkd(14) + 1
   numinc = num*Incr
 100  IF ( iblkd(4)>ilsrow ) GOTO 500
   IF ( (iblkd(4)+iblkd(6)-1)<Irobgn ) GOTO 400
 200  idiff = (iblkd(4)+iblkd(7)) - irow
   iblkd(7) = iblkd(7) + 1
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
   IF ( iblkd(2)/=itype ) THEN
      CALL dsupkc(iblkd(2),itype,ibase(index1),A(index2))
   ELSE
!DIR$ NOVECTOR
      DO k = 1 , num
         A(index2+k-1) = ibase(index1+k-1)
!DIR$ VECTOR
      ENDDO
   ENDIF
   IF ( irow>=ilsrow ) GOTO 600
   irow = irow + 1
   index2 = index2 + numinc
 300  index1 = index1 + iblkd(11)
   IF ( iblkd(7)/=iblkd(6) ) GOTO 200
 400  CALL endget(iblkd)
   CALL getstr(*500,iblkd)
   index1 = (iblkd(5)-1)*iblkd(14) + 1
   iblkd(7) = 0
   IF ( iblkd(8)<1 ) GOTO 100
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
 600  IF ( iblkd(8)<1 ) THEN
      CALL dsskrc
      CALL dssdcb
   ENDIF
 700  RETURN
 800  RETURN 1
END SUBROUTINE unpack
