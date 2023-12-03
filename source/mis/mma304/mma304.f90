!*==mma304.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma304(Zi,Zd,Zdc)
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
   COMPLEX*16 , DIMENSION(2) :: Zdc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibrow2 , icola , idxx , ii , indxa , indxal , indxav , indxb , irow1 , irowa1 , irowan , irowb1 , irowbn , irown ,&
            & iwr , j , k , kcnt , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype ,  &
            & ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , ntms
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
!     MMA304 PERFORMS THE MATRIX OPERATION USING METHOD 30 AND
!       COMPLEX DOUBLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA304 USES METHOD 30 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL 'MMARM1' TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
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
! PROCESS ALL OF THE COLUMNS OF "B";  ADD "C" DATA ON FIRST PASS
         DO ii = 1 , nbc
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  iurow1 = -1
                  typeu = ndtype
                  CALL unpack(*2,fileb,Zdc(1))
                  irowb1 = iurow1
                  irowbn = iurown
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 2                irowb1 = 0
                  irowbn = 0
                  spag_nextblock_2 = 2
               CASE (2)
                  IF ( ifile/=0 ) THEN
                     iurow1 = 1
                     iurown = ndr
                     typeu = ndtype
                     IF ( ipass==1 ) typeu = ndtype*signc
                     CALL unpack(*4,ifile,Zdc(idx4+1))
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 4                DO j = 1 , ndr
                     Zdc(idx4+j) = (0.0,0.0)
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (3)
!
! CHECK IF COLUMN OF "B" IS NULL
!
                  indxa = iax
                  IF ( irowb1/=0 ) THEN
                     IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMPLEX DOUBLE PRECISION
                        indxb = 1 - irowb1
                        idxx = idx4 + ibrow
                        DO i = 1 , ncolpp
                           icola = ibrow + i
                           IF ( icola/=iabs(Zi(indxa)) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           indxal = Zi(indxa+1) + iax - 1
                           indxa = indxa + 2
                           DO WHILE ( indxa<indxal )
                              irowa1 = Zi(indxa)
                              ntms = Zi(indxa+1)
                              irowan = irowa1 + ntms - 1
                              irow1 = max0(irowa1,irowb1)
                              irown = min0(irowan,irowbn)
                              IF ( irown>=irow1 ) THEN
                                 indxav = ((indxa+3)/2) + 2*(irow1-irowa1) - 1
                                 indxb = 2*(irow1-irowb1)
!
!         D = C + A*B
!
                                 kcnt = (irown-irow1)*2 + 1
                                 DO k = 1 , kcnt , 2
                                    Zdc(idxx+i) = Zdc(idxx+i) + dcmplx(Zd(indxav+k),Zd(indxav+k+1))                                 &
                                     & *dcmplx(Zd(indxb+k),Zd(indxb+k+1))
                                 ENDDO
                              ENDIF
                              indxa = indxa + 2 + ntms*4
                           ENDDO
                           indxa = indxal
                        ENDDO
                     ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! COMPLEX DOUBLE PRECISION
                        DO i = 1 , ncolpp
                           indxal = Zi(indxa+1) + iax - 1
                           icola = ibrow + i
                           IF ( icola>=irowb1 .AND. icola<=irowbn ) THEN
                              ibrow2 = 2*(ibrow+i-irowb1) + 1
                              IF ( Zd(ibrow2)/=0.D0 .OR. Zd(ibrow2+1)/=0.D0 ) THEN
                                 IF ( icola/=iabs(Zi(indxa)) ) THEN
                                    spag_nextblock_1 = 2
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 indxa = indxa + 2
                                 DO WHILE ( indxa<indxal )
                                    irowa1 = Zi(indxa)
                                    ntms = Zi(indxa+1)
                                    irowan = irowa1 + ntms - 1
                                    indxav = ((indxa+3)/2) - 2
                                    DO k = irowa1 , irowan
!
!         D = C + A*B
!
                                       indxav = indxav + 2
                                       Zdc(idx4+k) = Zdc(idx4+k) + dcmplx(Zd(indxav),Zd(indxav+1))*dcmplx(Zd(ibrow2),Zd(ibrow2+1))
                                    ENDDO
                                    indxa = indxa + 2 + ntms*4
                                 ENDDO
                              ENDIF
                           ENDIF
                           indxa = indxal
                        ENDDO
                     ENDIF
                  ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
                  CALL pack(Zdc(idx4+1),ofile,filed)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
      CASE (2)
         WRITE (iwr,99001) icola , Zi(indxa) , iax , indxa
99001    FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IAX =',I7, &
                &'  INDXA=',I7)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mma304