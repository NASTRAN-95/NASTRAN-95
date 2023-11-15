
SUBROUTINE cead
!
!     COMPLEX  EIGENVALUE EXTRACTION  MODULE
!
!     5  INPUT  FILES -  KDD,BDD,MDD,EED,CASECC
!     4  OUTPUT FILES -  PHID,LAMD,OCEIGS,PHIDL
!     12 SCRATCHES FILES
!     1  PARAMETER
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Eps
   INTEGER Ib(7) , Idmpfl , Ik(7) , Ilam(7) , Im(7) , Iphi(7) , Iscr(11) , Iz(1) , Kz(1) , Nfound , Noreg , Nout , Phidli ,         &
         & Reg(7,10) , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Nfound
   COMMON /cinvpx/ Ik , Im , Ib , Ilam , Iphi , Idmpfl , Iscr , Noreg , Eps , Reg , Phidli
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER bdd , capp , casecc , det , eed , eigc(2) , error(2) , feer , file , hes , ibuf , iflag , inv , ip1 , iz148 , iz2 , iz6 ,&
         & j , kdd , lamd , mcb(7) , mdd , method , name(2) , nrow , nvect , nz , oceigs , phid , phidl , scr1 , scr10 , scr11 ,    &
         & scr12 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9
   INTEGER korsz
!
! End of declarations
!
   EQUIVALENCE (Kz(1),Iz(1))
   DATA name/4HCEAD , 4H    /
   DATA hes/4HHESS/
   DATA feer/4HFEER/
   DATA error/4HEED  , 4HCEAD/
   DATA kdd , bdd , mdd , eed , casecc/101 , 102 , 103 , 104 , 105/
   DATA phid , lamd , oceigs , phidl/201 , 202 , 203 , 204/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308 , 309/
   DATA scr10 , scr11 , scr12/310 , 311 , 312/
   DATA det , inv , eigc(1) , eigc(2)/4HDET  , 4HINV  , 207 , 2/
   DATA iz2 , iz6 , iz148/2 , 6 , 148/
!
!     FIND SELECTED EIGC CARD IN CASECC
!
   ibuf = korsz(Iz) - Sysbuf
   CALL open(*100,casecc,Iz(ibuf),0)
   CALL skprec(casecc,1)
   CALL fread(casecc,Iz,166,1)
   CALL close(casecc,1)
   j = 148
   method = Iz(j)
   scr10 = 310
   GOTO 200
 100  method = -1
 200  file = eed
   CALL preloc(*300,Iz(ibuf),eed)
   CALL locate(*600,Iz(ibuf),eigc(1),iflag)
   DO
      CALL read(*500,*700,eed,Iz(1),10,0,iflag)
      IF ( method==Iz(1) .OR. method==-1 ) THEN
!
!     FOUND DESIRED  EIGC CARD
!
         CALL close(eed,1)
         j = 2
         capp = Iz(j)
         IF ( capp==det ) THEN
!
!     DETERMINANT
!
            CALL cdetm(method,eed,mdd,bdd,kdd,scr8,scr9,oceigs,Nfound,scr1,scr2,scr3,scr4,scr5,scr6,scr7,scr10)
            nvect = Nfound
         ELSE
            IF ( capp/=inv ) THEN
               IF ( capp==hes ) THEN
!
!     HESSENBURG METHOD
!
                  mcb(1) = kdd
                  CALL rdtrl(mcb)
                  nrow = mcb(2)
                  mcb(1) = bdd
                  CALL rdtrl(mcb)
                  IF ( mcb(1)>0 ) nrow = nrow*2
                  nz = korsz(Kz)
!
!     IF INSUFFICIENT CORE EXISTS FOR HESSENBURG METHOD.  DEFAULT TO
!     INVERSE POWER.
!
                  IF ( 6*nrow*nrow+nrow*8<=nz ) THEN
!
!     SUFFICIENT CORE.  PROCEED WITH HESSENBURG METHOD
!
                     CALL hess1(kdd,mdd,scr8,scr9,oceigs,Nfound,nvect,bdd,scr1,scr2,scr3,scr4,scr5,scr6,scr7,eed,method)
                     Nfound = nvect
                     GOTO 220
                  ELSE
                     WRITE (Nout,99001) Uim
99001                FORMAT (A29,' 2365, INSUFFICIENT CORE EXISTS FOR HESSENBURG ','METHOD.  CHANGING TO INVERSE POWER OR FEER.')
                  ENDIF
               ELSE
                  IF ( capp/=feer ) GOTO 600
!
!     FEER METHOD
!
                  CALL cfeer(eed,method,Nfound)
                  nvect = Nfound
                  GOTO 220
               ENDIF
            ENDIF
!
!     INVERSE POWER--
!
            Ik(1) = kdd
            CALL close(eed,1)
            CALL rdtrl(Ik)
            Im(1) = mdd
            CALL rdtrl(Im)
            Ib(1) = bdd
            CALL rdtrl(Ib)
            IF ( Ib(1)<0 ) Ib(1) = 0
            IF ( Ib(6)==0 ) Ib(1) = 0
            Ilam(1) = scr8
            Iphi(1) = scr9
            Idmpfl = oceigs
            Iscr(1) = scr1
            Iscr(2) = scr2
            Iscr(3) = scr3
            Iscr(4) = scr4
            Iscr(5) = scr5
            Iscr(6) = scr6
            Iscr(7) = scr7
            Iscr(8) = lamd
            Iscr(9) = phid
            Iscr(10) = scr10
            Iscr(11) = scr11
            Phidli = scr12
            Eps = .0001
            CALL cinvpr(eed,method,Nfound)
            nvect = Nfound
         ENDIF
!
!     LAMD ON SCR8, PHID ON SCR9
!
!     SORT EIGENVALUES AND PREPARE OUTPUT FILES
!
 220     IF ( Nfound/=0 ) THEN
            CALL cead1a(scr8,scr9,Phidli,lamd,phid,phidl,Nfound,nvect,capp)
         ELSE
            Nfound = -1
         ENDIF
         RETURN
      ELSE
         DO
            CALL fread(eed,Iz,7,0)
            j = 6
            IF ( Iz(j)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     ERROR MESAGES
!
 300  ip1 = -1
 400  CALL mesage(ip1,file,name)
 500  ip1 = -2
   GOTO 400
 600  ip1 = -7
   GOTO 400
 700  DO
      CALL mesage(-31,method,error(1))
   ENDDO
END SUBROUTINE cead
