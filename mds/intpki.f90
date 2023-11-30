
SUBROUTINE intpki(A,I,File,Block,Ieol)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , I , Ieol
   INTEGER A(4) , Block(15)
   INTEGER index , itypot , kk , num
   Name = File
   Iretrn = 0
   I = Block(4)
   index = (Block(5)-1)*Block(14) + 1 + Block(7)*Block(11)
   itypot = Block(13)
   IF ( Block(2)/=itypot ) THEN
      CALL dsupkc(Block(2),itypot,Ibase(index),A)
   ELSE
      num = Nwrdel(itypot)
!DIR$ NOVECTOR
      DO kk = 1 , num
         A(kk) = Ibase(index+kk-1)
!DIR$ VECTOR
      ENDDO
   ENDIF
   Block(4) = Block(4) + 1
   Block(7) = Block(7) + 1
   Block(10) = Block(4)
   IF ( Block(7)<Block(6) ) GOTO 200
   CALL endget(Block)
   CALL getstr(*100,Block)
 100  Block(7) = 0
 200  IF ( Iretrn/=0 ) THEN
      Ieol = 1
   ELSE
      Ieol = 0
   ENDIF
END SUBROUTINE intpki