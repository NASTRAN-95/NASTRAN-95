!*==cfeer2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer2(Iret)
   USE c_cdcmpx
   USE c_feeraa
   USE c_feerxc
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
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
         filea(1) = scr1
         filel(1) = scr3
         fileu(1) = scr4
         sr1fil = scr5
         sr2fil = scr6
         sr3fil = scr7
         filea(2) = dumm(3)
         filea(3) = dumm(3)
         filea(4) = dumm(4)
         filea(5) = itype
         filea(6) = 0
         filea(7) = 0
         filel(5) = itype
         nz = korsz(z)
         bbbbar(1) = 0
         CALL cdcomp(*20,z,z,z)
!
!     ---------- SPECIAL PRINT -------------------------------
!
         IF ( qpr ) THEN
            WRITE (nout,99001)
99001       FORMAT (//,7H CFEER2,//)
            WRITE (nout,99005)
            typout = itype
            irow = 1
            nlast = dumm(2)
            limit = 2*nlast
            incr = 1
            ibuf = nz - ksystm(1) - 2
            ifilxx = scr3
            SPAG_Loop_1_1: DO
               CALL gopen(ifilxx,z(ibuf),0)
               DO i = 1 , nlast
                  WRITE (nout,99002) i
99002             FORMAT (1H ,6HCOLUMN,I4)
                  CALL unpack(*5,ifilxx,z)
                  IF ( iprec==2 ) WRITE (nout,99003) (dz(j),j=1,limit)
99003             FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
                  IF ( iprec/=2 ) WRITE (nout,99004) (z(j),j=1,limit)
99004             FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
 5             ENDDO
               CALL close(ifilxx,1)
               WRITE (nout,99005)
               IF ( ifilxx==scr4 ) EXIT SPAG_Loop_1_1
               ifilxx = scr4
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     --------------------------------------------------------
!
         DO i = 1 , 7
            mcbut(i) = fileu(i)
            mcblt(i) = filel(i)
         ENDDO
         RETURN
!
 20      Iret = 1
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (1H ,13(10H----------))
END SUBROUTINE cfeer2
