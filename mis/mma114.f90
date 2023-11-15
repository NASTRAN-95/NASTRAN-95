
 
SUBROUTINE mma114(Zi,Zd,Zdc)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd , Nbr , Nbtype ,   &
         & Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nout , Nwords(4) , Nz ,  &
         & Rd , Rdrew , Signab , Signc , T , Typei , Typep , Typeu , Wrt , Wrtrew
   REAL Prec1 , Scrtch , Sysbuf , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
!
! Dummy argument declarations
!
   DOUBLE PRECISION Zd(2)
   DOUBLE COMPLEX Zdc(2)
   INTEGER Zi(2)
!
! Local variable declarations
!
   INTEGER i , ibrowi , idrow , idx4x , ii , indx , indxa , indxa1 , indxav , indxb , indxv , irow1 , irowa1 , irowan , irowb1 ,    &
         & irowbn , irown , irows , j , k , kcnt , nwddnar
!
! End of declarations
!
!
!     MMA114 PERFORMS THE MATRIX OPERATION IN COMPLEX DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA114 USES METHOD 10 WHICH IS AS FOLLOWS:
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
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
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
   DO ii = 1 , Nbc
!
! READ A COLUMN FROM THE "B" MATRIX
!
      CALL mmarc4(Zi,Zd)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO
!
      IF ( Ifile/=0 ) THEN
         Iurow1 = 1
         Iurown = Ndr
         Typeu = Ndtype
         IF ( Ipass==1 ) Typeu = Ndtype*Signc
         CALL unpack(*50,Ifile,Zdc(Idx4+1))
         GOTO 100
      ENDIF
 50   DO j = 1 , Ndr
         Zdc(Idx4+j) = (0.0,0.0)
      ENDDO
 100  nwddnar = Nwdd*Nar
!
! CHECK IF COLUMN OF "B" IS NULL
!
      irowb1 = Zi(1)
      irows = Zi(2)
      irowbn = irowb1 + irows - 1
      indx = 1
!
! CHECK FOR NULL COLUMN FROM THE "B" MATRIX
!
      IF ( irowb1/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
            idrow = Ibrow
! COMLEX DOUBLE PRECISION
            DO i = 1 , Ncolpp
               indx = 1
               indxa1 = Iax + 2*i + (i-1)*nwddnar
               irowa1 = Zi(indxa1-2)
               IF ( irowa1/=0 ) THEN
                  irowan = Zi(indxa1-1)
                  DO WHILE ( indx<Lasind )
                     irowb1 = Zi(indx)
                     irows = Zi(indx+1)
                     irowbn = irowb1 + irows - 1
                     indxv = (indx+3)/2
                     indx = indx + 2 + irows*Nwdd
                     irow1 = max0(irowa1,irowb1)
                     irown = min0(irowan,irowbn)
                     IF ( irown>=irow1 ) THEN
                        indxa = ((indxa1+1)/2) + 2*(irow1-irowa1) - 1
                        idx4x = Idx4 + idrow
                        indxb = indxv + 2*(irow1-irowb1) - 1
                        kcnt = (irown-irow1)*2 + 1
                        DO k = 1 , kcnt , 2
                           Zdc(idx4x+i) = Zdc(idx4x+i) + dcmplx(Zd(indxa+k),Zd(indxa+k+1))*dcmplx(Zd(indxb+k),Zd(indxb+k+1))
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! COMLEX DOUBLE PRECISION
            DO i = 1 , Ncolpp
               ibrowi = Ibrow + i
               indxa = Iax + 2*i + (i-1)*nwddnar
               irowa1 = Zi(indxa-2)
               IF ( irowa1/=0 ) THEN
                  irowan = Zi(indxa-1)
                  indxa1 = ((indxa+1)/2) - 2
                  DO WHILE ( ibrowi>=irowb1 )
                     IF ( ibrowi<=irowbn ) THEN
                        indxv = 2*(ibrowi-irowb1) + ((indx+3)/2)
                        IF ( Zd(indxv)/=0.D0 .OR. Zd(indxv+1)/=0.D0 ) THEN
                           indxav = indxa1
                           DO k = irowa1 , irowan
                              indxav = indxav + 2
                              Zdc(Idx4+k) = Zdc(Idx4+k) + dcmplx(Zd(indxav),Zd(indxav+1))*dcmplx(Zd(indxv),Zd(indxv+1))
                           ENDDO
                        ENDIF
                        EXIT
                     ELSE
                        indx = indx + 2 + irows*Nwdd
                        IF ( indx>=Lasind ) GOTO 150
                        irowb1 = Zi(indx)
                        irows = Zi(indx+1)
                        irowbn = irowb1 + irows - 1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
 150  CALL pack(Zdc(Idx4+1),Ofile,Filed)
   ENDDO
END SUBROUTINE mma114
