
SUBROUTINE mma323(Zi,Zc)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Iwr , Kdrow , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd ,    &
         & Nbr , Nbtype , Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nwords(4) &
         & , Nz , Rd , Rdrew , Signab , Signc , T , Typei , Typep , Typeu , Wrt , Wrtrew
   COMPLEX Cs(2)
   REAL D(4) , Prec1 , Scrtch , Sysbuf , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
   COMMON /zblpkx/ D , Kdrow
!
! Dummy argument declarations
!
   COMPLEX Zc(2)
   INTEGER Zi(2)
!
! Local variable declarations
!
   COMPLEX ctemp
   INTEGER i , icola , icrows , idrow , ii , indxa , indxal , indxav , indxb , indxbv , indxc , indxcv , irow1 , irowa1 , irowan ,  &
         & irowb1 , irowbn , irowc1 , irowcn , irown , irows , k , lasindb , lasindc , nrows , ntms
!
! End of declarations
!
!
!     MMA323 PERFORMS THE MATRIX OPERATION IN COMPLEX SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA323 USES METHOD 32 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL MMARM1 TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  ADD EACH ROW TERM OF "C" TO "D" MATRIX COLUMN
!       4.  CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" AND "C".
!
   EQUIVALENCE (D(1),Cs(1))
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
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
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
   idrow = Ibrow
   DO ii = 1 , Nbc
      CALL bldpk(Ndtype,Ndtype,Ofile,0,0)
!      PRINT *,' PROCESSING B MATRIX COLUMN, II=',II
!
! READ A COLUMN FROM THE "B" MATRIX
!
      Sign = 1
      Irfile = Fileb(1)
      CALL mmarc3(Zi,Zc)
      lasindb = Lasind
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO.
!
      IF ( Ifile/=0 ) THEN
         IF ( Ipass==1 ) Sign = Signc
         Irfile = Ifile
!
! READ A COLUMN FROM THE "C" MATRIX
!
         CALL mmarc3(Zi(Idx),Zc(Idx2+1))
         lasindc = Lasind + Idx - 1
      ENDIF
!
! CHECK IF COLUMN OF "B" IS NULL
!
      IF ( Zi(1)/=0 ) THEN
         irowb1 = Zi(1)
         irows = Zi(2)
         irowbn = irowb1 + irows - 1
         indxb = 1
         indxa = Iax
         indxc = Idx
         IF ( Ifile/=0 .AND. indxc<lasindc ) THEN
            irowc1 = Zi(indxc)
            icrows = Zi(indxc+1)
            irowcn = irowc1 + icrows - 1
!
! CHECK TO ADD TERMS FROM "C" OR INTERIM SCRATCH FILE BEFORE CURRENT ROW
!
            IF ( idrow/=0 .AND. irowc1<=idrow ) THEN
               DO
                  irown = idrow
                  IF ( irowcn<idrow ) irown = irowcn
                  indxcv = (indxc+3)/2
                  nrows = irown - irowc1 + 1
                  DO i = 1 , nrows
                     Kdrow = irowc1 + i - 1
                     Cs(1) = Zc(indxcv)
!      PRINT *,' AT 6000,ZC=',ZC(INDXCV)
                     indxcv = indxcv + 1
                     CALL zblpki
                  ENDDO
                  IF ( irowcn>=idrow ) EXIT
                  indxc = indxc + 2 + icrows*Nwdd
                  IF ( indxc>=lasindc ) EXIT
                  irowc1 = Zi(indxc)
                  icrows = Zi(indxc+1)
                  irowcn = irowc1 + icrows - 1
               ENDDO
            ENDIF
         ENDIF
!
! CHECK FOR NULL COLUMN FROM "B" MATRIX
!
         IF ( irowb1/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMPLEX SINGLE PRECISION
            DO i = 1 , Ncolpp
               Cs(1) = (0.0,0.0)
               Kdrow = idrow + i
               icola = Ibrow + i
               IF ( icola/=iabs(Zi(indxa)) ) GOTO 100
               indxal = Zi(indxa+1) + Iax - 1
               indxa = indxa + 2
               indxb = 1
               DO WHILE ( indxb<lasindb )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  indxbv = ((indxb+3)/2) - irowb1
                  indxb = indxb + 2 + irows*Nwdd
                  DO WHILE ( indxa<indxal )
                     irowa1 = Zi(indxa)
                     ntms = Zi(indxa+1)
                     irowan = irowa1 + ntms - 1
                     IF ( irowbn<irowa1 ) GOTO 5
                     IF ( irowan<irowb1 ) THEN
                        indxa = indxa + 2 + ntms*Nwdd
                     ELSE
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        indxav = ((indxa+3)/2) - irowa1
                        ctemp = (0.0,0.0)
!      PRINT *,' IROW1,N=',IROW1,IROWN
                        DO k = irow1 , irown
!      PRINT *,' K,D,A,B=',K,CTEMP,ZC(INDXAV+K),ZC(INDXBV+K)
                           ctemp = ctemp + Zc(indxav+k)*Zc(indxbv+k)
                        ENDDO
                        Cs(1) = Cs(1) + ctemp
                        IF ( irowan>irowbn ) GOTO 5
                        indxa = indxa + 2 + ntms*Nwdd
                     ENDIF
                  ENDDO
                  indxa = indxal
                  GOTO 10
 5             ENDDO
               indxa = indxal
 10            DO WHILE ( indxc<lasindc .AND. Ifile/=0 )
                  IF ( Kdrow<irowc1 ) EXIT
                  IF ( Kdrow>irowcn ) THEN
                     indxc = indxc + 2 + icrows*Nwdd
                     IF ( indxc>=lasindc ) EXIT
                     irowc1 = Zi(indxc)
                     icrows = Zi(indxc+1)
                     irowcn = irowc1 + icrows - 1
                  ELSE
                     indxcv = (indxc+3)/2 + Kdrow - irowc1
!      PRINT *,' ADDING C,DD,C=',CS(1),ZC(INDXCV)
                     Cs(1) = Cs(1) + Zc(indxcv)
!      PRINT *,' AT 14510,ZC=',ZC(INDXCV)
                     EXIT
                  ENDIF
               ENDDO
               CALL zblpki
            ENDDO
         ENDIF
         IF ( Kdrow/=Ndr .AND. Ifile/=0 .AND. indxc<lasindc ) THEN
!
! ADD REMAINING TERMS FROM EITHER THE "C" MATRIX OR INTERIM SCRATCH MATRIX
!
            irow1 = Kdrow + 1
            DO
               indxcv = (indxc+3)/2
               IF ( irow1<irowc1 ) EXIT
               IF ( irow1<=irowcn ) THEN
                  indxcv = (indxc+3)/2 + irow1 - irowc1
                  irowc1 = irow1
                  EXIT
               ELSE
                  indxc = indxc + 2 + icrows*Nwdd
                  IF ( indxc>=lasindc ) GOTO 50
                  irowc1 = Zi(indxc)
                  icrows = Zi(indxc+1)
                  irowcn = irowc1 + icrows - 1
               ENDIF
            ENDDO
            DO
               nrows = irowcn - irowc1 + 1
               DO k = 1 , nrows
                  Kdrow = irowc1 + k - 1
                  Cs(1) = Zc(indxcv)
!      PRINT *,' AT 51500,ZC=',ZC(INDXCV)
                  indxcv = indxcv + 1
                  CALL zblpki
               ENDDO
               indxc = indxc + 2 + icrows*Nwdd
               IF ( indxc>=lasindc ) EXIT
               irowc1 = Zi(indxc)
               icrows = Zi(indxc+1)
               irowcn = irowc1 + icrows - 1
               indxcv = (indxc+3)/2
            ENDDO
         ENDIF
      ELSE
         IF ( Ifile/=0 ) THEN
            IF ( Zi(Idx)/=0 ) THEN
               indxc = Idx
               DO WHILE ( indxc<lasindc )
                  irowc1 = Zi(indxc)
                  icrows = Zi(indxc+1)
                  irowcn = irowc1 + icrows - 1
                  indxcv = (indxc+3)/2
                  DO i = irowc1 , irowcn
                     Cs(1) = Zc(indxcv)
!      PRINT *,' A970,ZC=',ZC(INDXCV)
                     Kdrow = i
                     CALL zblpki
                     indxcv = indxcv + 1
                  ENDDO
                  indxc = indxc + 2 + icrows*Nwdd
               ENDDO
               GOTO 50
            ENDIF
         ENDIF
         Cs(1) = (0.0,0.0)
         Kdrow = 1
         CALL zblpki
      ENDIF
 50   CALL bldpkn(Ofile,0,Filed)
! END OF PROCESSING THIS COLUMN FOR THIS PASS
   ENDDO
   GOTO 99999
 100  WRITE (Iwr,99001) icola , Zi(indxa) , Iax , indxa
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUND FOUND   :',I6,/,' IAX =',I7,    &
             &' INDXA=',I7)
   CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE mma323
