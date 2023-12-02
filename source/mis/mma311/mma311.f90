!*==mma311.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma311(Zi,Zr)
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
   INTEGER :: i , icola , idrow , idxx , ii , indxa , indxal , indxav , indxb , indxbv , irow1 , irowa1 , irowan , irowb1 , irowbn ,&
            & irown , irows , iwr , j , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr ,  &
            & nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , ntms
   REAL :: sysbuf
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
!
!     MMA311 PERFORMS THE MATRIX OPERATION IN REAL SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA311 USES METHOD 31 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL MMARM1 TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
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
         irfile = fileb(1)
         DO ii = 1 , nbc
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!      PRINT *,' PROCESSING COLUMN=',II
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
                     CALL unpack(*2,ifile,Zr(idx))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 2                DO j = 1 , ndr
                     Zr(idx+j-1) = 0
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
!
! CHECK IF COLUMN OF "B" IS NULL
!
                  irowb1 = Zi(1)
                  irows = Zi(2)
                  irowbn = irowb1 + irows - 1
                  indxb = 1
                  indxa = iax
!
! CHECK FOR NULL COLUMN FROM "B" MATRIX
!
                  IF ( irowb1/=0 ) THEN
                     IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
                        idrow = ibrow
                        idxx = idx + idrow - 1
! SINGLE PRECISION
                        DO i = 1 , ncolpp
                           icola = ibrow + i
                           IF ( icola/=iabs(Zi(indxa)) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           indxal = Zi(indxa+1) + iax - 1
                           indxa = indxa + 2
                           indxb = 1
                           SPAG_Loop_3_1: DO WHILE ( indxb<lasind )
                              irowb1 = Zi(indxb)
                              irows = Zi(indxb+1)
                              irowbn = irowb1 + irows - 1
                              indxbv = indxb + 2 - irowb1
                              indxb = indxb + 2 + irows
                              DO WHILE ( indxa<indxal )
                                 irowa1 = Zi(indxa)
                                 ntms = Zi(indxa+1)
                                 irowan = irowa1 + ntms - 1
                                 IF ( irowbn<irowa1 ) CYCLE SPAG_Loop_3_1
                                 IF ( irowan<irowb1 ) THEN
                                    indxa = indxa + 2 + ntms
                                 ELSE
                                    irow1 = max0(irowa1,irowb1)
                                    irown = min0(irowan,irowbn)
                                    indxav = indxa + 2 - irowa1
                                    DO k = irow1 , irown
                                       Zr(idxx+i) = Zr(idxx+i) + Zr(indxav+k)*Zr(indxbv+k)
                                    ENDDO
                                    IF ( irowan>irowbn ) CYCLE SPAG_Loop_3_1
                                    indxa = indxa + 2 + ntms
                                 ENDIF
                              ENDDO
                              EXIT SPAG_Loop_3_1
                           ENDDO SPAG_Loop_3_1
                           indxa = indxal
                        ENDDO
                     ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! SINGLE PRECISION
                        SPAG_Loop_2_3: DO i = 1 , ncolpp
                           indxal = Zi(indxa+1) + iax - 1
                           icola = ibrow + i
                           IF ( icola/=iabs(Zi(indxa)) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           indxa = indxa + 2
                           SPAG_Loop_3_2: DO WHILE ( icola>=irowb1 )
                              IF ( icola<=irowbn ) THEN
                                 indxbv = icola - irowb1 + indxb + 2
                                 IF ( Zr(indxbv)==0. ) EXIT SPAG_Loop_3_2
                                 DO WHILE ( indxa<indxal )
                                    irowa1 = Zi(indxa)
                                    ntms = Zi(indxa+1)
                                    irowan = irowa1 + ntms - 1
                                    indxav = indxa + 2 - irowa1
                                    DO k = irowa1 , irowan
                                       Zr(idx+k-1) = Zr(idx+k-1) + Zr(indxav+k)*Zr(indxbv)
                                    ENDDO
                                    indxa = indxa + 2 + ntms
                                 ENDDO
                                 EXIT SPAG_Loop_3_2
                              ELSE
                                 indxb = indxb + 2 + irows
                                 IF ( indxb>lasind ) EXIT SPAG_Loop_2_3
                                 irowb1 = Zi(indxb)
                                 irows = Zi(indxb+1)
                                 irowbn = irowb1 + irows - 1
                              ENDIF
                           ENDDO SPAG_Loop_3_2
                           indxa = indxal
                        ENDDO SPAG_Loop_2_3
                     ENDIF
                  ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
                  CALL pack(Zr(idx),ofile,filed)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
      CASE (2)
         WRITE (iwr,99001) icola , Zi(indxa) , iax , indxa
99001    FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUND FOUND   :',I6,/,' IAX =',I7, &
                &' INDXA=',I7)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mma311
