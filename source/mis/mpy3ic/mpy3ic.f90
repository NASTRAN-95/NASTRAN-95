!*==mpy3ic.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3ic(Z,Iz,Dz)
USE C_MPY3CP
USE C_MPY3TL
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL(REAL64) , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dd , mm , nn , pp
   INTEGER :: file , ia , iacols , iakj , iantu , ib , ibc , ibcid , ibcols , ibntu , ic , ii , ik , iktbp , ipoint , isavp ,       &
            & itrans , ix , nacols , nakj , nantu , nbcid , nbcols , nbntu , nc , nerr , nktbp , npoint , nsavp , ntrans , precm
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fwdrec , gopen , mesage , mpy3a , mpy3b , mpy3c , mpy3nu , open , pack , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     IN-CORE PRODUCT.
!
   !>>>>EQUIVALENCE (Isavp,Zpntrs(1)) , (Nsavp,Zpntrs(2)) , (Ipoint,Zpntrs(3)) , (Npoint,Zpntrs(4)) , (Iacols,Zpntrs(5)) ,               &
!>>>>    & (Nacols,Zpntrs(6)) , (Itrans,Zpntrs(7)) , (Ntrans,Zpntrs(8)) , (Ic,Zpntrs(9)) , (Nc,Zpntrs(10)) , (Ibcols,Zpntrs(11)) ,       &
!>>>>    & (Nbcols,Zpntrs(12)) , (Ibcid,Zpntrs(13)) , (Nbcid,Zpntrs(14)) , (Ibntu,Zpntrs(15)) , (Nbntu,Zpntrs(16)) , (Iktbp,Zpntrs(17)) ,&
!>>>>    & (Nktbp,Zpntrs(18)) , (Iantu,Zpntrs(19)) , (Nantu,Zpntrs(20)) , (Iakj,Zpntrs(21)) , (Nakj,Zpntrs(22))
   DATA name/4HMPY3 , 4HIC  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     INITIALIZATION.
!
         First1 = .TRUE.
         First2 = .TRUE.
         dd = D
         nn = Ncb
         mm = M
         pp = Prec
!
!     OPEN CORE POINTERS
!
         isavp = 1
         nsavp = Ncb
         ipoint = nsavp + 1
         npoint = nsavp + Ncb
         iacols = npoint + 1
!     NACOLS = NPOINT + D*NCB*M/10000
         nacols = npoint + (dd*nn*mm/10000.D0+0.5D0)
         itrans = nacols + 1
         IF ( Prec/=1 .AND. mod(itrans,2)/=1 ) itrans = itrans + 1
!     NTRANS = ITRANS + PREC*D*NCB*M/10000 - 1
         ntrans = itrans + (pp*dd*nn*mm/10000.D0+0.5D0) - 1
         ic = ntrans + 1
         IF ( Prec/=1 .AND. mod(ic,2)/=1 ) ic = ic + 1
         nc = ic + Prec*M - 1
         ibcols = nc + 1
         nbcols = nc + Prec*N*Nk
         ibcid = nbcols + 1
         nbcid = nbcols + Nk
         ibntu = nbcid + 1
         nbntu = nbcid + Nk
         iktbp = nbntu + 1
         nktbp = nbntu + Maxa
         iantu = nktbp + 1
         nantu = nktbp + Maxa
         iakj = nantu + 1
         nakj = nantu + Prec*Maxa
!
!     PACK PARAMETERS
!
         Typin = Prec
         Typout = Prec
         Row1 = 1
         Incr = 1
!
!     UNPACK PARAMETERS
!
         Utyp = Prec
         Urow1 = 1
         Uincr = 1
!
!     PREPARE B AND A(T).
!
         CALL mpy3a(Z,Z,Z)
!
!     OPEN FILES AND CHECK EXISTENCE OF MATRIX E.
!
         IF ( E ) THEN
            file = Filee(1)
            CALL open(*20,Filee,Z(Buf4),2)
            CALL fwdrec(*40,Filee)
         ENDIF
         file = Filea(1)
         CALL open(*20,Filea,Z(Buf1),2)
         CALL fwdrec(*40,Filea)
         file = Scr1
         CALL open(*20,Scr1,Z(Buf2),0)
         file = Filec(1)
         CALL gopen(Filec,Z(Buf3),1)
         Rowm = Filec(3)
!
!     PROCESS COLUMNS OF C ONE BY ONE.
!
         DO J = 1 , M
!
!     INITIALIZE COLUMN OF C.
!
            DO ix = ic , nc
               Z(ix) = 0.
            ENDDO
            IF ( E ) THEN
               Urown = M
               CALL unpack(*10,Filee,Z(ic))
            ENDIF
 10         precm = Prec*M
!
!     PROCESS A AND PERFORM FIRST PART OF PRODUCT.
!
            CALL mpy3b(Z,Z,Z)
!
!     TEST IF PROCESSING IS COMPLETE
!
            IF ( Iflag/=0 ) THEN
               SPAG_Loop_2_2: DO
!
!     PROCESS REMAINING TERMS OF COLUMN J OF A.
!
!     TEST IF BCOLS IS FULL
!
                  IF ( K2>=Nk ) THEN
!
!     CALCULATE NEXT TIME USED FOR COLUMNS OF B AND/OR TERMS OF A
!
                     IF ( First2 ) THEN
                        First2 = .FALSE.
                        ibc = ibcid - 1
                        ib = ibntu - 1
                        DO ii = 1 , Nk
                           ibc = ibc + 1
                           I = Iz(ibc)
                           CALL mpy3nu(Z)
                           ib = ib + 1
                           Iz(ib) = Ntbu
                        ENDDO
                     ENDIF
                     ik = iktbp - 1
                     ia = iantu - 1
                     DO ii = 1 , K
                        ik = ik + 1
                        ia = ia + 1
                        IF ( Iz(ik)==0 ) THEN
                           Iz(ia) = 0
                        ELSE
                           I = Iz(ik)
                           CALL mpy3nu(Z)
                           Iz(ia) = Ntbu
                        ENDIF
                     ENDDO
                  ENDIF
                  SPAG_Loop_3_1: DO
!
!     ADD OR REPLACE COLUMN OF B INTO CORE AND PERFORM COMPUTATION
!
                     CALL mpy3c(Z,Z,Z)
                     IF ( Kcount==K ) EXIT SPAG_Loop_3_1
                     IF ( First2 ) CYCLE SPAG_Loop_2_2
                  ENDDO SPAG_Loop_3_1
                  EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
!
!     PACK COLUMN OF C.
!
            CALL pack(Z(ic),Filec,Filec)
         ENDDO
!
!     CLOSE FILES.
!
         CALL close(Filea,2)
         CALL close(Scr1,1)
         CALL close(Filec,1)
         IF ( E ) CALL close(Filee,2)
         RETURN
!
!     ERROR MESSAGES.
!
 20      nerr = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      nerr = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(nerr,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mpy3ic
