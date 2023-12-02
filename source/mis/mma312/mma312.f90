!*==mma312.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mma312(Zi,Zd)
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
   REAL*8 :: dtemp
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
!     MMA312 PERFORMS THE MATRIX OPERATION IN REAL DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA312 USES METHOD 31 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL MMARM1 TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
!
!     MEMORY FOR EACH COLUMN OF "A" IS AS FOLLOWS:
!         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(3-N) = VALUES OF NON-ZERO ROWS
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
!      PRINT *,' PROCESSING B MATRIX COLUMN, II=',II
!
! READ A COLUMN FROM THE "B" MATRIX
!
                  CALL mmarc2(Zi,Zd)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO.
!
                  IF ( ifile/=0 ) THEN
                     iurow1 = 1
                     iurown = ndr
                     typeu = ndtype
                     IF ( ipass==1 ) typeu = ndtype*signc
                     CALL unpack(*2,ifile,Zd(idx2+1))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 2                IF ( Zi(1)/=0 ) THEN
                     DO j = 1 , ndr
                        Zd(idx2+j) = 0
                     ENDDO
                  ELSE
                     iprown = 1
                     CALL pack(0.0D0,ofile,filed)
                     iprown = ndr
                     CYCLE
                  ENDIF
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
! CHECK FOR NULL COLUMN FRO "B" MATRIX
!
                  IF ( irowb1/=0 ) THEN
                     IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
                        idrow = ibrow
                        idxx = idx2 + idrow
! DOUBLE PRECISION
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
                              indxbv = ((indxb+3)/2) - irowb1
                              indxb = indxb + 2 + irows*nwdd
                              DO WHILE ( indxa<indxal )
                                 irowa1 = Zi(indxa)
                                 ntms = Zi(indxa+1)
                                 irowan = irowa1 + ntms - 1
                                 IF ( irowbn<irowa1 ) CYCLE SPAG_Loop_3_1
                                 IF ( irowan<irowb1 ) THEN
                                    indxa = indxa + 2 + ntms*nwdd
                                 ELSE
                                    irow1 = max0(irowa1,irowb1)
                                    irown = min0(irowan,irowbn)
                                    indxav = ((indxa+3)/2) - irowa1
                                    dtemp = 0.0D0
                                    DO k = irow1 , irown
                                       dtemp = dtemp + Zd(indxav+k)*Zd(indxbv+k)
                                    ENDDO
                                    Zd(idxx+i) = Zd(idxx+i) + dtemp
                                    IF ( irowan>irowbn ) CYCLE SPAG_Loop_3_1
                                    indxa = indxa + 2 + ntms*nwdd
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
! DOUBLE PRECISION
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
                                 indxbv = icola - irowb1 + (indxb+3)/2
                                 dtemp = Zd(indxbv)
                                 IF ( dtemp==0.0D0 ) EXIT SPAG_Loop_3_2
                                 DO WHILE ( indxa<indxal )
                                    irowa1 = Zi(indxa)
                                    ntms = Zi(indxa+1)
                                    irowan = irowa1 + ntms - 1
                                    indxav = ((indxa+3)/2) - irowa1
                                    DO k = irowa1 , irowan
                                       Zd(idx2+k) = Zd(idx2+k) + Zd(indxav+k)*dtemp
                                    ENDDO
                                    indxa = indxa + 2 + ntms*nwdd
                                 ENDDO
                                 EXIT SPAG_Loop_3_2
                              ELSE
                                 indxb = indxb + 2 + irows*nwdd
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
                  CALL pack(Zd(idx2+1),ofile,filed)
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
END SUBROUTINE mma312
