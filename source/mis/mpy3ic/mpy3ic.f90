!*==mpy3ic.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3ic(Z,Iz,Dz)
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zntpkx
   USE iso_fortran_env
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
         first1 = .TRUE.
         first2 = .TRUE.
         dd = d
         nn = ncb
         mm = m
         pp = prec
!
!     OPEN CORE POINTERS
!
         isavp = 1
         nsavp = ncb
         ipoint = nsavp + 1
         npoint = nsavp + ncb
         iacols = npoint + 1
!     NACOLS = NPOINT + D*NCB*M/10000
         nacols = npoint + (dd*nn*mm/10000.D0+0.5D0)
         itrans = nacols + 1
         IF ( prec/=1 .AND. mod(itrans,2)/=1 ) itrans = itrans + 1
!     NTRANS = ITRANS + PREC*D*NCB*M/10000 - 1
         ntrans = itrans + (pp*dd*nn*mm/10000.D0+0.5D0) - 1
         ic = ntrans + 1
         IF ( prec/=1 .AND. mod(ic,2)/=1 ) ic = ic + 1
         nc = ic + prec*m - 1
         ibcols = nc + 1
         nbcols = nc + prec*n*nk
         ibcid = nbcols + 1
         nbcid = nbcols + nk
         ibntu = nbcid + 1
         nbntu = nbcid + nk
         iktbp = nbntu + 1
         nktbp = nbntu + maxa
         iantu = nktbp + 1
         nantu = nktbp + maxa
         iakj = nantu + 1
         nakj = nantu + prec*maxa
!
!     PACK PARAMETERS
!
         typin = prec
         typout = prec
         row1 = 1
         incr = 1
!
!     UNPACK PARAMETERS
!
         utyp = prec
         urow1 = 1
         uincr = 1
!
!     PREPARE B AND A(T).
!
         CALL mpy3a(Z,Z,Z)
!
!     OPEN FILES AND CHECK EXISTENCE OF MATRIX E.
!
         IF ( e ) THEN
            file = filee(1)
            CALL open(*20,filee,Z(buf4),2)
            CALL fwdrec(*40,filee)
         ENDIF
         file = filea(1)
         CALL open(*20,filea,Z(buf1),2)
         CALL fwdrec(*40,filea)
         file = scr1
         CALL open(*20,scr1,Z(buf2),0)
         file = filec(1)
         CALL gopen(filec,Z(buf3),1)
         rowm = filec(3)
!
!     PROCESS COLUMNS OF C ONE BY ONE.
!
         DO j = 1 , m
!
!     INITIALIZE COLUMN OF C.
!
            DO ix = ic , nc
               Z(ix) = 0.
            ENDDO
            IF ( e ) THEN
               urown = m
               CALL unpack(*10,filee,Z(ic))
            ENDIF
 10         precm = prec*m
!
!     PROCESS A AND PERFORM FIRST PART OF PRODUCT.
!
            CALL mpy3b(Z,Z,Z)
!
!     TEST IF PROCESSING IS COMPLETE
!
            IF ( iflag/=0 ) THEN
               SPAG_Loop_2_2: DO
!
!     PROCESS REMAINING TERMS OF COLUMN J OF A.
!
!     TEST IF BCOLS IS FULL
!
                  IF ( k2>=nk ) THEN
!
!     CALCULATE NEXT TIME USED FOR COLUMNS OF B AND/OR TERMS OF A
!
                     IF ( first2 ) THEN
                        first2 = .FALSE.
                        ibc = ibcid - 1
                        ib = ibntu - 1
                        DO ii = 1 , nk
                           ibc = ibc + 1
                           i = Iz(ibc)
                           CALL mpy3nu(Z)
                           ib = ib + 1
                           Iz(ib) = ntbu
                        ENDDO
                     ENDIF
                     ik = iktbp - 1
                     ia = iantu - 1
                     DO ii = 1 , k
                        ik = ik + 1
                        ia = ia + 1
                        IF ( Iz(ik)==0 ) THEN
                           Iz(ia) = 0
                        ELSE
                           i = Iz(ik)
                           CALL mpy3nu(Z)
                           Iz(ia) = ntbu
                        ENDIF
                     ENDDO
                  ENDIF
                  SPAG_Loop_3_1: DO
!
!     ADD OR REPLACE COLUMN OF B INTO CORE AND PERFORM COMPUTATION
!
                     CALL mpy3c(Z,Z,Z)
                     IF ( kcount==k ) EXIT SPAG_Loop_3_1
                     IF ( first2 ) CYCLE SPAG_Loop_2_2
                  ENDDO SPAG_Loop_3_1
                  EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
!
!     PACK COLUMN OF C.
!
            CALL pack(Z(ic),filec,filec)
         ENDDO
!
!     CLOSE FILES.
!
         CALL close(filea,2)
         CALL close(scr1,1)
         CALL close(filec,1)
         IF ( e ) CALL close(filee,2)
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
