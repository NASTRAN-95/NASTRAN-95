!*==cead.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cead
!
!     COMPLEX  EIGENVALUE EXTRACTION  MODULE
!
!     5  INPUT  FILES -  KDD,BDD,MDD,EED,CASECC
!     4  OUTPUT FILES -  PHID,LAMD,OCEIGS,PHIDL
!     12 SCRATCHES FILES
!     1  PARAMETER
!
   USE c_blank
   USE c_cinvpx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bdd , casecc , det , eed , feer , hes , inv , iz148 , iz2 , iz6 , kdd , lamd , mdd , oceigs , phid , phidl ,   &
                   & scr1 , scr10 , scr11 , scr12 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9
   INTEGER :: capp , file , ibuf , iflag , ip1 , j , method , nrow , nvect , nz
   INTEGER , DIMENSION(2) , SAVE :: eigc , error , name
   INTEGER , DIMENSION(1) :: kz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL cdetm , cead1a , cfeer , cinvpr , close , fread , hess1 , korsz , locate , mesage , open , preloc , rdtrl , read ,      &
          & skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Kz(1),Iz(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FIND SELECTED EIGC CARD IN CASECC
!
         ibuf = korsz(iz) - sysbuf
         CALL open(*20,casecc,iz(ibuf),0)
         CALL skprec(casecc,1)
         CALL fread(casecc,iz,166,1)
         CALL close(casecc,1)
         j = 148
         method = iz(j)
         scr10 = 310
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      method = -1
         spag_nextblock_1 = 2
      CASE (2)
         file = eed
         CALL preloc(*40,iz(ibuf),eed)
         CALL locate(*80,iz(ibuf),eigc(1),iflag)
         DO
            CALL read(*60,*100,eed,iz(1),10,0,iflag)
            IF ( method==iz(1) .OR. method==-1 ) THEN
!
!     FOUND DESIRED  EIGC CARD
!
               CALL close(eed,1)
               j = 2
               capp = iz(j)
               IF ( capp==det ) THEN
!
!     DETERMINANT
!
                  CALL cdetm(method,eed,mdd,bdd,kdd,scr8,scr9,oceigs,nfound,scr1,scr2,scr3,scr4,scr5,scr6,scr7,scr10)
                  nvect = nfound
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
                        nz = korsz(kz)
!
!     IF INSUFFICIENT CORE EXISTS FOR HESSENBURG METHOD.  DEFAULT TO
!     INVERSE POWER.
!
                        IF ( 6*nrow*nrow+nrow*8<=nz ) THEN
!
!     SUFFICIENT CORE.  PROCEED WITH HESSENBURG METHOD
!
                           CALL hess1(kdd,mdd,scr8,scr9,oceigs,nfound,nvect,bdd,scr1,scr2,scr3,scr4,scr5,scr6,scr7,eed,method)
                           nfound = nvect
                           GOTO 25
                        ELSE
                           WRITE (nout,99001) uim
99001                      FORMAT (A29,' 2365, INSUFFICIENT CORE EXISTS FOR HESSENBURG ',                                           &
                                  &'METHOD.  CHANGING TO INVERSE POWER OR FEER.')
                        ENDIF
                     ELSE
                        IF ( capp/=feer ) GOTO 80
!
!     FEER METHOD
!
                        CALL cfeer(eed,method,nfound)
                        nvect = nfound
                        GOTO 25
                     ENDIF
                  ENDIF
!
!     INVERSE POWER--
!
                  ik(1) = kdd
                  CALL close(eed,1)
                  CALL rdtrl(ik)
                  im(1) = mdd
                  CALL rdtrl(im)
                  ib(1) = bdd
                  CALL rdtrl(ib)
                  IF ( ib(1)<0 ) ib(1) = 0
                  IF ( ib(6)==0 ) ib(1) = 0
                  ilam(1) = scr8
                  iphi(1) = scr9
                  idmpfl = oceigs
                  iscr(1) = scr1
                  iscr(2) = scr2
                  iscr(3) = scr3
                  iscr(4) = scr4
                  iscr(5) = scr5
                  iscr(6) = scr6
                  iscr(7) = scr7
                  iscr(8) = lamd
                  iscr(9) = phid
                  iscr(10) = scr10
                  iscr(11) = scr11
                  phidli = scr12
                  eps = .0001
                  CALL cinvpr(eed,method,nfound)
                  nvect = nfound
               ENDIF
!
!     LAMD ON SCR8, PHID ON SCR9
!
!     SORT EIGENVALUES AND PREPARE OUTPUT FILES
!
 25            IF ( nfound/=0 ) THEN
                  CALL cead1a(scr8,scr9,phidli,lamd,phid,phidl,nfound,nvect,capp)
               ELSE
                  nfound = -1
               ENDIF
               RETURN
            ELSE
               SPAG_Loop_2_1: DO
                  CALL fread(eed,iz,7,0)
                  j = 6
                  IF ( iz(j)==-1 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
            ENDIF
         ENDDO
!
!     ERROR MESAGES
!
 40      ip1 = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,file,name)
 60      ip1 = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      ip1 = -7
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 100     DO
            CALL mesage(-31,method,error(1))
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cead
