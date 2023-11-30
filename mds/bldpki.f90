
SUBROUTINE bldpki(A,I,File,Block)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , I
   INTEGER A(4) , Block(15)
   INTEGER icrow , inccnt , index , itypin , k , kk , nexrow
   Name = File
   Block(15) = I
   itypin = Block(13)
   Nwords = Nwrdel(itypin)
   IF ( Block(2)>=3 ) THEN
      inccnt = 2
   ELSE
      inccnt = 1
   ENDIF
   DO k = 1 , Nwords
      IF ( A(k)/=0 ) GOTO 100
   ENDDO
   GOTO 99999
 100  IF ( Block(4)/=0 ) THEN
      nexrow = Block(4) + Block(7)
      icrow = Block(15)
      IF ( icrow<nexrow ) THEN
         CALL dsmsg1(Block)
         CALL dsmsg(119)
      ENDIF
      IF ( icrow==nexrow ) GOTO 200
      CALL endput(Block)
      CALL putstr(Block)
      Block(7) = 0
   ENDIF
   icrow = Block(15)
   Block(4) = icrow
 200  index = (Block(5)-1)*Block(14) + 1
   IF ( itypin/=Block(2) ) THEN
      CALL dsupkc(itypin,Block(2),A,Ibase(index))
   ELSE
!DIR$ NOVECTOR
      DO kk = 1 , Nwords
         Ibase(index+kk-1) = A(kk)
!DIR$ VECTOR
      ENDDO
   ENDIF
   Block(5) = Block(5) + inccnt
   Block(7) = Block(7) + 1
   Block(10) = Block(10) + Block(11)
   IF ( Block(6)<=Block(7) ) THEN
      CALL endput(Block)
      CALL putstr(Block)
      Block(4) = 0
      Block(7) = 0
   ENDIF
99999 RETURN
END SUBROUTINE bldpki
