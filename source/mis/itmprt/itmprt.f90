!*==itmprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE itmprt(Name,Item,Nz,Iopt)
   USE c_machin
   USE c_output
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: Name
   REAL :: Item
   INTEGER :: Nz
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: alphc , alphc1 , bgss , blank , cont , cparen , cstm , d , ec , ec1 , ec2 , eqss , itm , loap , lods , oparen ,   &
                & plts , uned
   CHARACTER(1) , DIMENSION(2000) :: ccore
   REAL :: flag
   INTEGER :: i , icode , icomp , inum , iout , irec , ist , iv , ix , left , llen , ngrd , nout , np , nred , ns , nsub , rc
   INTEGER , DIMENSION(4) :: icore
   INTEGER , SAVE :: intgc
   REAL , DIMENSION(3) , SAVE :: subs
   EXTERNAL cmiwrt , numtyp , page , sfetch , sjump , suread , wrtfmt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     WILL PRINT SOF ITEM - USING  E15.7,I10, OR ALPHA FORMAT
!
   !>>>>EQUIVALENCE (Ccore,Core)
   !>>>>EQUIVALENCE (Icore(1),Core(1))
   DATA oparen , cparen , ec , ec1 , ec2 , intgc , alphc , alphc1 , cont , uned , d/4H(1X  , 4H)    , 4H,1P, , 4HE13. , 4H6    ,    &
       &4H,I13 , 4H,9X, , 4HA4   , 4HCONT , 4HINUE , 4HD   /
   DATA blank , subs , itm/4H     , 4HSUBS , 4HTRUC , 4HTURE , 4HITEM/
   DATA eqss/4HEQSS/ , bgss/4HBGSS/ , cstm/4HCSTM/ , plts/4HPLTS/ , lods/4HLODS/ , loap/4HLOAP/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     TEST FOR FORMATED TABLE PRINT
!
         IF ( Iopt==2 ) THEN
            IF ( Item==eqss ) THEN
!
!     PERFORM FORMATED LISTING OF TABLE
!
!     EQSS TABLE
!
               CALL sfetch(Name,Item,1,rc)
               IF ( rc/=1 ) RETURN
               CALL suread(core(1),4,nout,rc)
               IF ( rc/=1 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nsub = icore(3)
               CALL suread(core(1),Nz,nout,rc)
               IF ( rc/=2 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ist = 1 + nout
               left = Nz - nout
               DO i = 1 , nsub
                  CALL suread(core(ist),left,nout,rc)
                  IF ( rc/=2 .AND. rc/=3 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  icomp = 1 + 2*(i-1)
                  CALL cmiwrt(1,Name,core(icomp),ist,nout,core,icore)
               ENDDO
               CALL suread(core(ist),left,nout,rc)
               IF ( rc/=2 .AND. rc/=3 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL cmiwrt(8,Name,0,ist,nout,core,icore)
               RETURN
            ELSEIF ( Item==bgss ) THEN
!
!     BGSS TABLE
!
               CALL sfetch(Name,Item,1,rc)
               IF ( rc/=1 ) RETURN
               ngrd = 1
               CALL sjump(ngrd)
               IF ( ngrd<0 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ist = 1
               CALL suread(core(ist),Nz,nout,rc)
               IF ( rc/=2 .AND. rc/=3 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL cmiwrt(2,Name,Name,ist,nout,core,icore)
               RETURN
            ELSEIF ( Item==cstm ) THEN
!
!     CSTM TABLE
!
               CALL sfetch(Name,Item,1,rc)
               IF ( rc/=1 ) RETURN
               ngrd = 1
               CALL sjump(ngrd)
               IF ( ngrd<0 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ist = 1
               CALL suread(core(ist),Nz,nout,rc)
               IF ( rc/=2 .OR. rc/=3 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL cmiwrt(3,Name,Name,ist,nout,core,icore)
               RETURN
            ELSEIF ( Item==plts ) THEN
!
!     PLTS TABLE
!
               CALL sfetch(Name,Item,1,rc)
               IF ( rc/=1 ) RETURN
               CALL suread(core(1),3,nout,rc)
               IF ( rc/=1 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ist = 1
               CALL suread(core(ist),Nz,nout,rc)
               IF ( rc/=2 .AND. rc/=3 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL cmiwrt(4,Name,Name,ist,nout,core,icore)
               RETURN
            ELSEIF ( Item==lods ) THEN
!
!     LODS TABLE
!
               icode = 5
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Item==loap ) THEN
!
!     LOAP TABLE
!
               icode = 7
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     PERFORM UNFORMATED DUMP OF TABLE
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , 96
            head2(i) = blank
         ENDDO
         DO i = 1 , 3
            head2(i) = subs(i)
         ENDDO
         head2(5) = Name(1)
         head2(6) = Name(2)
         head2(8) = itm
         head2(10) = Item
         CALL page
         head2(12) = cont
         head2(13) = uned
         head2(14) = d
         inum = Nz/2 - 1
         ns = inum + 1
         llen = 0
         core(1) = oparen
         irec = 0
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (otpe,99001) irec
99001    FORMAT ('0GROUP NO.',I4)
         irec = irec + 1
         line = line + 2
         IF ( line>=nlpp ) CALL page
         ix = inum
         nred = 0
         np = inum - 1
         iv = 4
         spag_nextblock_1 = 4
      CASE (4)
         ix = ix + 1
         iout = 4
         nred = nred + 1
         np = np + 1
         CALL suread(core(ix),1,flag,rc)
         IF ( rc<2 ) THEN
            i = numtyp(core(ix)) + 1
            IF ( i==1 .AND. iv/=4 ) i = iv
            iv = i
            IF ( i==1 .OR. i==2 ) THEN
!
!     INTEGER  (3)
!
               iout = 3
               IF ( llen+13>132 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               icore(nred+1) = intgc
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==4 ) THEN
!
!     ALPHA   (2)
!
               iout = 2
               IF ( llen+6>132 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     REAL NUMBER  (1)
!
               iout = 1
               IF ( llen+13>132 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ELSEIF ( rc==2 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         core(nred+1) = ec
         core(nred+2) = ec1
         core(nred+3) = ec2
         nred = nred + 2
         spag_nextblock_1 = 6
      CASE (6)
         llen = llen + 13
         spag_nextblock_1 = 4
      CASE (7)
         core(nred+1) = alphc
         core(nred+2) = alphc1
         nred = nred + 1
         spag_nextblock_1 = 6
      CASE (8)
!
!     BUFFER FULL - END RECORD   PRINT LINE
!
         core(nred+1) = cparen
         IF ( nred==1 ) WRITE (otpe,99002)
99002    FORMAT ('0END OF GROUP - NULL GROUP')
         IF ( nred/=1 ) THEN
            IF ( machx==2 .OR. machx==5 ) WRITE (otpe,core) (icore(i),i=ns,np)
            IF ( machx/=2 .AND. machx/=5 ) CALL wrtfmt(icore(ns),np-ns+1,ccore)
         ENDIF
         line = line + 1
         IF ( line>=nlpp ) CALL page
         llen = 0
         nred = 1
         np = inum
         core(inum+1) = core(ix)
         ix = inum + 1
         IF ( iout==1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iout==2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iout==3 ) THEN
            icore(nred+1) = intgc
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( iout==4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     END OF ITEM
!
         WRITE (otpe,99003)
99003    FORMAT ('0END OF ITEM')
         spag_nextblock_1 = 10
      CASE (10)
         RETURN
      CASE (11)
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) RETURN
         CALL suread(core(1),4,nout,rc)
         IF ( rc==1 ) THEN
            nsub = icore(4)
            CALL suread(core(1),Nz,nout,rc)
            IF ( rc==2 ) THEN
               ist = 1 + nout
               left = Nz - nout
               DO i = 1 , nsub
                  CALL suread(core(ist),left,nout,rc)
                  IF ( rc/=2 .AND. rc/=3 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  icomp = 1 + 2*(i-1)
                  CALL cmiwrt(icode,Name,core(icomp),ist,nout,core,icore)
                  icode = 6
               ENDDO
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     INSUFFICIENT CORE OR ILLEGAL ITEM FORMAT - FORCE PHYSICAL DUMP
!
         WRITE (otpe,99004) uwm , Item , Name
99004    FORMAT (A25,' 6231, INSUFFICIENT CORE AVAILABLE OR ILLEGAL ITEM ','FORMAT REQUIRES AN UNFORMATED',/31X,                    &
                &'DUMP TO BE PERFORM FOR ITEM ',A4,' OF SUBSTRUCTURE ',2A4)
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE itmprt