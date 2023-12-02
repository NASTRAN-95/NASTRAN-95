!*==mma102.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma102(Zi,Zd)
   USE i_mmacom
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL*8 , DIMENSION(2) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibrowi , idrow , idx2x , ii , indxa , indxb , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , j , k , nac ,   &
            & nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform ,     &
            & ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout , nwddnar
   REAL :: sysbuf
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MMA102 PERFORMS THE MATRIX OPERATION USING METHOD 10 AND IN
!       REAL DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA10- USES METHOD 10 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
!     MEMORY FOR EACH COLUMN OF "A" IS AS FOLLOWS:
!         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(3-N) = VALUES OF NON-ZERO ROWS
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX
!     Z( IDX      ) = ARRAY FOR ONE COLUMN OF "D" MATRIX
!     Z( IAX      ) = ARRAY FOR MULTIPLE COLUMNS OF "A" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
   DO ii = 1 , nbc
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            iurow1 = -1
            typeu = ndtype*signab
            CALL unpack(*10,fileb,Zd(1))
            irowb1 = iurow1
            irowbn = iurown
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
 10         irowb1 = 0
            irowbn = 0
            spag_nextblock_1 = 2
         CASE (2)
            IF ( ifile/=0 ) THEN
               iurow1 = 1
               iurown = ndr
               typeu = ndtype
               IF ( ipass==1 ) typeu = ndtype*signc
               CALL unpack(*20,ifile,Zd(idx2+1))
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 20         DO j = 1 , ndr
               Zd(idx2+j) = 0
            ENDDO
            spag_nextblock_1 = 3
         CASE (3)
            nwddnar = nwdd*nar
!
! CHECK IF COLUMN OF "B" IS NULL
!
            IF ( irowb1/=0 ) THEN
               IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
                  idrow = ibrow
! DOUBLE PRECISION
                  DO i = 1 , ncolpp
                     indxa = iax + 2*i + (i-1)*nwddnar
                     irowa1 = Zi(indxa-2)
                     IF ( irowa1/=0 ) THEN
                        irowan = Zi(indxa-1)
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        IF ( irown>=irow1 ) THEN
                           indxa = ((indxa+1)/2) - irowa1
                           idx2x = idx2 + idrow
                           indxb = 1 - irowb1
                           DO k = irow1 , irown
                              Zd(idx2x+i) = Zd(idx2x+i) + Zd(indxa+k)*Zd(indxb+k)
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDDO
               ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! DOUBLE PRECISION
                  DO i = 1 , ncolpp
                     ibrowi = ibrow + i
                     IF ( ibrowi>=irowb1 .AND. ibrowi<=irowbn ) THEN
                        ibrowi = ibrowi - irowb1 + 1
                        IF ( Zd(ibrowi)/=0.D0 ) THEN
                           indxa = iax + 2*i + (i-1)*nwddnar
                           irowa1 = Zi(indxa-2)
                           IF ( irowa1/=0 ) THEN
                              irowan = Zi(indxa-1)
                              indxa = ((indxa+1)/2) - irowa1
                              DO k = irowa1 , irowan
                                 Zd(idx2+k) = Zd(idx2+k) + Zd(indxa+k)*Zd(ibrowi)
                              ENDDO
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
            CALL pack(Zd(idx2+1),ofile,filed)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE mma102
