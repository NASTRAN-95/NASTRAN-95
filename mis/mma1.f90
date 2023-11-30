
SUBROUTINE mma1(Zi,Zr,Zd,Zc,Zdc)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Ksys58 , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd , Nbr ,   &
         & Nbtype , Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nout , Nwords(4)&
         & , Nz , Rd , Rdrew , Scrtch , Signab , Signc , Sysbuf , Typei , Typep , Typeu , Wrt , Wrtrew
   REAL Prec1 , T , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
   COMPLEX Zc(2)
   DOUBLE PRECISION Zd(2)
   DOUBLE COMPLEX Zdc(2)
   INTEGER Zi(2)
   REAL Zr(2)
   INTEGER i , iavail , ibuf1 , ibuf2 , ibuf3 , ibuf4 , indx , itest , jbegn , jend , kone , kzero , m , module(3) , npass
!
!     MMA1 PERFORMS THE MATRIX OPERATION USING METHODS 10 AND 11
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA1 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "A" AND "C".
!       5.  FOR METHOD 10, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
!       6.  FOR METHOD 11, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "B"
!           INTO MEMORY IN COMPACT FORM.
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA1 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   module(3) = jbegn
   IF ( Nbstor==1 ) module(2) = kzero
   IF ( Nbstor==2 ) module(2) = kone
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = Ndtype
   Typep = Ndtype
   Sign = Signab
   Nwdd = Nwords(Ndtype)
   Nwdb = Nwords(Nbtype)
   Irfile = Fileb(1)
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
   IF ( Nbstor==1 .OR. Ksys58==10 ) Idx = 1 + Nwdd*Nbr
   IF ( Nbstor==2 .OR. Ksys58==11 ) THEN
!
! REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
      Idx = 1 + Nwdd*Nbr + Nbr
      itest = mod(Idx,4)
      IF ( itest/=1 ) THEN
         IF ( itest==0 ) Idx = Idx + 1
         IF ( itest==2 ) Idx = Idx + 3
         IF ( itest==3 ) Idx = Idx + 2
      ENDIF
   ENDIF
   Idx2 = ((Idx+1)/2) - 1
   Idx4 = (Idx+1)/4
   Iax = Idx + Nwdd*Ndr
   ibuf1 = Nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = 0
   IF ( Filec(1)==0 .OR. Signc==0 ) THEN
      ibuf4 = ibuf2 - Sysbuf
   ELSE
      ibuf3 = ibuf2 - Sysbuf
      ibuf4 = ibuf3 - Sysbuf
   ENDIF
   Lasmem = ibuf4 - 1
   Iprow1 = 1
   Iprown = Ndr
   Incrp = 1
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   DO
!
!   DETERMINE HOW MANY COLUMNS OF A CAN BE READ INTO MEMORY IN ONE PASS
!
      iavail = Lasmem - Iax + 1
!
!   NCOLPP  -  NUMBER OF COLUMNS OF "A" THAT CAN BE READ IN ONE PASS
!   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "A" MATRIX
!
      Ncolpp = iavail/(2+Nwdd*Nar)
      IF ( Ncolpp>Nac ) Ncolpp = Nac
      IF ( Ncolpp<=0 ) CALL mesage(-8,iavail+Nwdd*Nar,module)
      npass = ((Nac-1)/Ncolpp) + 1
      IF ( npass==1 .OR. ibuf3/=0 ) THEN
         DO m = 1 , npass
            Ipass = m
            Ibrow = (m-1)*Ncolpp
            IF ( m==npass ) THEN
! LAST PASS, CREATE OUTPUT FILE
               Ncolpp = Nac - Ncolpp*(npass-1)
               Ofile = Filed(1)
               Ifile = Scrtch
               CALL rewind(Fileb)
               CALL skprec(Fileb,1)
               CALL gopen(Filed,Zr(ibuf4),Wrtrew)
               Filed(2) = 0
               Filed(6) = 0
               Filed(7) = 0
               IF ( m/=1 ) THEN
                  CALL gopen(Ifile,Zr(ibuf3),Rdrew)
                  GOTO 10
               ENDIF
            ELSE
!
! MULTIPLE PASSES REQUIRED, DETERMINE PROPER FILE FOR OUTPUT SO THAT
! REQUESTED OUTPUT FILE IS USED ON THE LAST PASS
!
               itest = npass - m
               itest = mod(itest,2)
               IF ( itest/=0 ) THEN
                  Ifile = Filed(1)
                  Ofile = Scrtch
               ELSE
                  Ifile = Scrtch
                  Ofile = Filed(1)
               ENDIF
               IF ( m==1 ) THEN
! FIRST PASS, OPEN "C" FILE IF IT EXISTS
                  CALL gopen(Ofile,Zr(ibuf4),Wrtrew)
               ELSE
                  CALL rewind(Fileb)
                  CALL skprec(Fileb,1)
                  CALL gopen(Ifile,Zr(ibuf3),Rdrew)
                  CALL gopen(Ofile,Zr(ibuf4),Wrtrew)
                  GOTO 10
               ENDIF
            ENDIF
            Ifile = Filec(1)
            IF ( Signc==0 ) Ifile = 0
            IF ( Ifile/=0 ) CALL gopen(Ifile,Zr(ibuf3),Rdrew)
 10         indx = Iax
            Typeu = Ndtype
            DO i = 1 , Ncolpp
               Iurow1 = -1
               CALL unpack(*15,Filea,Zr(indx+2))
               Zi(indx) = Iurow1
               Zi(indx+1) = Iurown
               indx = indx + 2 + Nwdd*Nar
               CYCLE
! NULL COLUMN READ
 15            Zi(indx) = 0
               Zi(indx+1) = 0
               indx = indx + 2 + Nwdd*Nar
            ENDDO
            IF ( Ksys58/=10 ) THEN
               IF ( Ksys58==11 ) GOTO 20
               IF ( Nbstor==2 ) GOTO 20
            ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
            IF ( Ndtype==1 ) CALL mma101(Zi,Zr)
            IF ( Ndtype==2 ) CALL mma102(Zi,Zd)
            IF ( Ndtype==3 ) CALL mma103(Zi,Zc)
            IF ( Ndtype==4 ) CALL mma104(Zi,Zd,Zdc)
            GOTO 30
 20         IF ( Ndtype==1 ) CALL mma111(Zi,Zr)
            IF ( Ndtype==2 ) CALL mma112(Zi,Zd)
            IF ( Ndtype==3 ) CALL mma113(Zi,Zc)
            IF ( Ndtype==4 ) CALL mma114(Zi,Zd,Zdc)
 30         CALL close(Ifile,Clsrew)
            CALL close(Ofile,Clsrew)
         ENDDO
         CALL close(Filea,Clsrew)
         CALL close(Fileb,Clsrew)
         module(3) = jend
         CALL conmsg(module,3,0)
         EXIT
      ELSE
!
! MUST ALLOCATE TWO BUFFERS FOR MULTIPLE PASSES
!
         ibuf3 = ibuf2 - Sysbuf
         ibuf4 = ibuf3 - Sysbuf
         Lasmem = ibuf4 - 1
      ENDIF
   ENDDO
END SUBROUTINE mma1