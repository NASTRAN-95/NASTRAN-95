
 
SUBROUTINE mma3(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER ibuf1 , ibuf2 , ibuf3 , ibuf4 , itest , jbegn , jend , kone , ktwo , kzero , module(3)
!
!     MMA3 PERFORMS THE MATRIX OPERATION USING METHODS 30, 31 AND 32
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA3 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  PACK (IN COMPACT FORM) AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!           SEE SUBROUTINES MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  FOR METHODS 30 AND 31, CALL UNPACK TO READ MATRIX "C".
!       5.  FOR METHOD 30, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
!       6.  FOR METHOD 31, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" INTO
!           MEMORY IN COMPACT FORM.
!       7.  FOR METHOD 32, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" AND
!           "C" INTO MEMORY IN COMPACT FORM.
!       8.  FOR METHODS 30 AND 31, CALL PACK TO WRITE "D" MATRIX.
!       9.  FOR METHOD 32, CALL BLDPK TO WRITE "D" MATRIX.
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   EQUIVALENCE (Ksystm(58),Ksys58)
   EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA3 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA ktwo/1H2/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   module(3) = jbegn
   IF ( Method==30 ) module(2) = kzero
   IF ( Method==31 ) module(2) = kone
   IF ( Method==32 ) module(2) = ktwo
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = Ndtype
   Typep = Ndtype
   Nwdd = Nwords(Ndtype)
   Nwdb = Nwords(Nbtype)
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
   Idx = 1 + Nwdd*Nbr
   IF ( Method==31 .OR. Method==32 ) THEN
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
   IF ( Method==32 ) THEN
!
! FOR METHOD 32, INSURE IAX IS ON QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
      Iax = Idx + Nwdd*Ndr + Ndr
      itest = mod(Iax,4)
      IF ( itest/=1 ) THEN
         IF ( itest==0 ) Iax = Iax + 1
         IF ( itest==2 ) Iax = Iax + 3
         IF ( itest==3 ) Iax = Iax + 2
      ENDIF
   ENDIF
   Iax2 = ((Iax+1)/2)
   ibuf1 = Nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   Lasmem = ibuf4 - 1
   Lasmem = Lasmem - Iax
   Iprow1 = 1
   Iprown = Ndr
   Incrp = 1
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   Ipass = 0
   Ircoln = 0
 100  Ipass = Ipass + 1
   Ircol1 = Ircoln + 1
   Ircoln = Nac
   Irfile = Filea(1)
   Sign = Signab
   IF ( Ipass/=1 ) CALL dsspos(Irfile,Irpos(1),Irpos(2),Irpos(3))
   IF ( Ndtype==1 ) CALL mmarm1(Zi(Iax),Zr(Iax),0)
   IF ( Ndtype==2 ) CALL mmarm2(Zi(Iax),Zd(Iax2),0)
   IF ( Ndtype==3 ) CALL mmarm3(Zi(Iax),Zc(Iax2),0)
   IF ( Ndtype==4 ) CALL mmarm4(Zi(Iax),Zd(Iax2),0)
   Ncolpp = Ircoln - Ircol1 + 1
   Ibrow = Ircol1 - 1
   IF ( Ircoln==Nac ) THEN
! LAST PASS, CREATE OUTPUT FILE
      IF ( Ifile==0 ) Ifile = Scrtch
      IF ( Ofile==Filed(1) .AND. Ipass/=1 ) CALL filswi(Ifile,Ofile)
      Ofile = Filed(1)
      Ifile = Scrtch
      CALL rewind(Fileb)
      CALL skprec(Fileb,1)
      CALL gopen(Filed,Zr(ibuf4),Wrtrew)
      Filed(2) = 0
      Filed(6) = 0
      Filed(7) = 0
      IF ( Ipass/=1 ) THEN
         CALL gopen(Ifile,Zr(ibuf3),Rdrew)
         GOTO 200
      ENDIF
   ELSE
      itest = mod(Ipass,2)
      IF ( itest==0 ) THEN
         Ifile = Filed(1)
         Ofile = Scrtch
      ELSE
         Ifile = Scrtch
         Ofile = Filed(1)
      ENDIF
      IF ( Ipass==1 ) THEN
! FIRST PASS, OPEN "C" FILE IF IT EXISTS
         CALL gopen(Ofile,Zr(ibuf4),Wrtrew)
      ELSE
         CALL rewind(Fileb)
         CALL skprec(Fileb,1)
         CALL gopen(Ifile,Zr(ibuf3),Rdrew)
         CALL gopen(Ofile,Zr(ibuf4),Wrtrew)
         GOTO 200
      ENDIF
   ENDIF
   Ifile = Filec(1)
   IF ( Signc==0 ) Ifile = 0
   IF ( Ifile/=0 ) CALL gopen(Ifile,Zr(ibuf3),Rdrew)
 200  Sign = 1
   IF ( Method/=30 ) THEN
      IF ( Method==31 ) THEN
         IF ( Ndtype==1 ) CALL mma311(Zi,Zr)
         IF ( Ndtype==2 ) CALL mma312(Zi,Zd)
         IF ( Ndtype==3 ) CALL mma313(Zi,Zc)
         IF ( Ndtype==4 ) CALL mma314(Zi,Zd,Zdc)
         GOTO 300
      ELSEIF ( Method==32 ) THEN
         IF ( Ndtype==1 ) CALL mma321(Zi,Zr)
         IF ( Ndtype==2 ) CALL mma322(Zi,Zd)
         IF ( Ndtype==3 ) CALL mma323(Zi,Zc)
         IF ( Ndtype==4 ) CALL mma324(Zi,Zd)
         GOTO 300
      ENDIF
   ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
   IF ( Ndtype==1 ) CALL mma301(Zi,Zr)
   IF ( Ndtype==2 ) CALL mma302(Zi,Zd)
   IF ( Ndtype==3 ) CALL mma303(Zi,Zc)
   IF ( Ndtype==4 ) CALL mma304(Zi,Zd,Zdc)
 300  CALL close(Ifile,Clsrew)
   CALL close(Ofile,Clsrew)
   IF ( Ircoln<Nac ) GOTO 100
!
! ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
!
   CALL close(Filea,Clsrew)
   CALL close(Fileb,Clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma3
