!*==cfeer2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer2(Iret)
USE C_CDCMPX
USE C_FEERAA
USE C_FEERXC
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iret
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ibuf , ifilxx , iprec , itype , j , limit , nout
   EXTERNAL cdcomp , close , gopen , korsz , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CFEER2 INITIALIZES AND CALLS CDCOMP FOR CFCNTL
!
   !>>>>EQUIVALENCE (Z(1),Dz(1)) , (Nout,Ksystm(2)) , (Iprec,Ksystm(55))
!
         itype = iprec + 2
         Iret = 0
         Filea(1) = Scr1
         Filel(1) = Scr3
         Fileu(1) = Scr4
         Sr1fil = Scr5
         Sr2fil = Scr6
         Sr3fil = Scr7
         Filea(2) = Dumm(3)
         Filea(3) = Dumm(3)
         Filea(4) = Dumm(4)
         Filea(5) = itype
         Filea(6) = 0
         Filea(7) = 0
         Filel(5) = itype
         Nz = korsz(Z)
         Bbbbar(1) = 0
         CALL cdcomp(*20,Z,Z,Z)
!
!     ---------- SPECIAL PRINT -------------------------------
!
         IF ( Qpr ) THEN
            WRITE (nout,99001)
99001       FORMAT (//,7H CFEER2,//)
            WRITE (nout,99005)
            Typout = itype
            Irow = 1
            Nlast = Dumm(2)
            limit = 2*Nlast
            Incr = 1
            ibuf = Nz - Ksystm(1) - 2
            ifilxx = Scr3
            SPAG_Loop_1_1: DO
               CALL gopen(ifilxx,Z(ibuf),0)
               DO i = 1 , Nlast
                  WRITE (nout,99002) i
99002             FORMAT (1H ,6HCOLUMN,I4)
                  CALL unpack(*5,ifilxx,Z)
                  IF ( iprec==2 ) WRITE (nout,99003) (dz(j),j=1,limit)
99003             FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
                  IF ( iprec/=2 ) WRITE (nout,99004) (Z(j),j=1,limit)
99004             FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
 5             ENDDO
               CALL close(ifilxx,1)
               WRITE (nout,99005)
               IF ( ifilxx==Scr4 ) EXIT SPAG_Loop_1_1
               ifilxx = Scr4
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     --------------------------------------------------------
!
         DO i = 1 , 7
            Mcbut(i) = Fileu(i)
            Mcblt(i) = Filel(i)
         ENDDO
         RETURN
!
 20      Iret = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (1H ,13(10H----------))
END SUBROUTINE cfeer2
