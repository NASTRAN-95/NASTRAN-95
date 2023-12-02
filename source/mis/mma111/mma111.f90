!*==mma111.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma111(Zi,Zr)
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
   REAL , DIMENSION(2) :: Zr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibrowi , idrow , idxx , ii , indx , indxa , indxav , indxb , indxv , irow1 , irowa1 , irowan , irowb1 , irowbn ,  &
            & irown , irows , j , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr ,        &
            & nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout ,       &
            & nwddnar
   REAL :: sysbuf
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MMA111 PERFORMS THE MATRIX OPERATION IN REAL SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA111 USES METHOD 11 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
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
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX IN COMPACT FORM
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
   DO ii = 1 , nbc
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
!
! READ A COLUMN FROM THE "B" MATRIX
!
            CALL mmarc1(Zi,Zr)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO.
!
            IF ( ifile/=0 ) THEN
               iurow1 = 1
               iurown = ndr
               typeu = ndtype
               IF ( ipass==1 ) typeu = ndtype*signc
               CALL unpack(*10,ifile,Zr(idx))
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 10         DO j = 1 , ndr
               Zr(idx+j-1) = 0
            ENDDO
            spag_nextblock_1 = 2
         CASE (2)
            nwddnar = nwdd*nar
!
! CHECK IF COLUMN OF "B" IS NULL
!
            irowb1 = Zi(1)
            irows = Zi(2)
            irowbn = irowb1 + irows - 1
            indx = 1
!
! CHECK FOR NULL COLUMN ON "B"
!
            IF ( irowb1/=0 ) THEN
               IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
                  idrow = ibrow
! SINGLE PRECISION
                  DO i = 1 , ncolpp
                     indx = 1
                     indxa = iax + 2*i + (i-1)*nar
                     irowa1 = Zi(indxa-2)
                     IF ( irowa1/=0 ) THEN
                        irowan = Zi(indxa-1)
                        indxav = indxa - irowa1
                        DO WHILE ( indx<lasind )
                           irowb1 = Zi(indx)
                           irows = Zi(indx+1)
                           irowbn = irowb1 + irows - 1
                           indxv = indx + 2
                           indx = indx + 2 + irows
                           irow1 = max0(irowa1,irowb1)
                           irown = min0(irowan,irowbn)
                           IF ( irown>=irow1 ) THEN
                              indxb = indxv - irowb1
                              idxx = idx + idrow - 1
                              DO k = irow1 , irown
                                 Zr(idxx+i) = Zr(idxx+i) + Zr(indxav+k)*Zr(indxb+k)
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! SINGLE PRECISION
                  SPAG_Loop_2_2: DO i = 1 , ncolpp
                     ibrowi = ibrow + i
                     indxa = iax + 2*i + (i-1)*nar
                     irowa1 = Zi(indxa-2)
                     IF ( irowa1/=0 ) THEN
                        irowan = Zi(indxa-1)
                        indxa = indxa - irowa1
                        SPAG_Loop_3_1: DO WHILE ( ibrowi>=irowb1 )
                           IF ( ibrowi<=irowbn ) THEN
                              indxv = ibrowi - irowb1 + indx + 2
                              IF ( Zr(indxv)/=0. ) THEN
                                 DO k = irowa1 , irowan
                                    Zr(idx+k-1) = Zr(idx+k-1) + Zr(indxa+k)*Zr(indxv)
                                 ENDDO
                              ENDIF
                              EXIT SPAG_Loop_3_1
                           ELSE
                              indx = indx + 2 + irows
                              IF ( indx>lasind ) EXIT SPAG_Loop_2_2
                              irowb1 = Zi(indx)
                              irows = Zi(indx+1)
                              irowbn = irowb1 + irows - 1
                           ENDIF
                        ENDDO SPAG_Loop_3_1
                     ENDIF
                  ENDDO SPAG_Loop_2_2
               ENDIF
            ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
            CALL pack(Zr(idx),ofile,filed)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE mma111
