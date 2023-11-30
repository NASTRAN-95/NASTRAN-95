
SUBROUTINE gfsh(Nuy,H)
   IMPLICIT NONE
   INTEGER I1 , Incr1 , N1 , Sysbuf , Ti1 , To1 , Z(1)
   REAL Rz(2)
   COMMON /packx / Ti1 , To1 , I1 , N1 , Incr1
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER H , Nuy
   INTEGER i , ibuf , mcb(7) , name(2) , nuy1 , nz
   INTEGER korsz
!
!     ROUTINE TO CALCULTE THE H TRANSFORMATION MATRIX USED WHEN NO
!     SPC'S ARE ON THE FLUID
!
!
!
!     OPEN CORE
!
!
!     SYSTEM COMMON
!
!
!     PACK - UNPACK COMMON BLOCKS
!
!
   EQUIVALENCE (Z(1),Rz(1))
!
   DATA name/4HGFSH , 4H    /
!
!     ALLOCATE CORE
!
   nz = korsz(Z(1))
   ibuf = nz - Sysbuf
   nz = ibuf - 1
   IF ( nz<Nuy ) THEN
!
!     ERRORS
!
      CALL mesage(-8,0,name)
      GOTO 99999
   ENDIF
   nuy1 = Nuy - 1
   CALL makmcb(mcb,H,nuy1,2,2)
   Ti1 = 1
   To1 = 2
   I1 = 1
   N1 = nuy1
   Incr1 = 1
!
   DO i = 1 , Nuy
      Rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL gopen(H,Z(ibuf),1)
   DO i = 1 , Nuy
      Rz(i) = float(nuy1)/float(Nuy)
      CALL pack(Rz(2),H,mcb)
      Rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL close(H,1)
   CALL wrttrl(mcb)
   RETURN
99999 RETURN
END SUBROUTINE gfsh
