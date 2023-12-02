!*==bldpki.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bldpki(A,I,File,Block)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , I
   INTEGER A(4) , Block(15)
   INTEGER icrow , inccnt , index , itypin , k , kk , nexrow
   name = File
   Block(15) = I
   itypin = Block(13)
   nwords = nwrdel(itypin)
   IF ( Block(2)>=3 ) THEN
      inccnt = 2
   ELSE
      inccnt = 1
   ENDIF
   DO k = 1 , nwords
      IF ( A(k)/=0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      IF ( Block(4)/=0 ) THEN
         nexrow = Block(4) + Block(7)
         icrow = Block(15)
         IF ( icrow<nexrow ) THEN
            CALL dsmsg1(Block)
            CALL dsmsg(119)
         ENDIF
         IF ( icrow==nexrow ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL endput(Block)
         CALL putstr(Block)
         Block(7) = 0
      ENDIF
      icrow = Block(15)
      Block(4) = icrow
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      index = (Block(5)-1)*Block(14) + 1
      IF ( itypin/=Block(2) ) THEN
         CALL dsupkc(itypin,Block(2),A,ibase(index))
      ELSE
!DIR$ NOVECTOR
         DO kk = 1 , nwords
            ibase(index+kk-1) = A(kk)
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
   END SUBROUTINE spag_block_2
END SUBROUTINE bldpki
