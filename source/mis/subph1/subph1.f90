!*==subph1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subph1
!
!     THIS MODULE PERFORMS THE PHASE 1 CONVERSION OF NASTRAN DATA BLOCK
!     TABLES TO THEIR EQUIVALENT SOF ITEMS
!
   USE c_blank
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b1 , b2 , b3 , buf1 , buf2 , c , file , i , icase , ipt , irec , is , isil , itest , iu , izp , j , k , kcode , litm ,&
            & mua , nc , nlod , nnew , npts , nsets , nu , nwds , nz , type
   INTEGER , SAVE :: bgpd , bgss , case , cstm , else , eqex , eqss , gpse , i0 , icstm , iua , loap , lods , papp , plts , scrt ,  &
                   & uset
   INTEGER , DIMENSION(10) :: buf , temp
   INTEGER , DIMENSION(32) :: icode
   LOGICAL :: last
   INTEGER , DIMENSION(5) , SAVE :: ltype1 , ltype2 , ltype3
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(12) :: rz
   INTEGER , DIMENSION(2) , SAVE :: sub1
   EXTERNAL andf , close , decode , fwdrec , gopen , korsz , lshift , mesage , open , orf , rdtrl , read , rewind , setlvl ,        &
          & sfetch , sofcls , sofopn , suwrt , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Rz(1),Z(1))
   DATA case , eqex , uset , bgpd , cstm , gpse , else , scrt/101 , 102 , 103 , 104 , 105 , 106 , 107 , 301/
   DATA eqss/4HEQSS/ , icstm/4HCSTM/ , lods/4HLODS/ , plts/4HPLTS/ , bgss/4HBGSS/
   DATA iua/25/ , sub1/4HSUBP , 4HH1  /
   DATA ltype1/4HEXTE , 4HRNAL , 4H STA , 4HTIC  , 4HLOAD/
   DATA ltype2/4H     , 4H     , 4HTHER , 4HMAL  , 4HLOAD/
   DATA ltype3/4H ELE , 4HMENT , 4H DEF , 4HORMA , 4HTION/
   DATA loap , papp/4HLOAP , 4HPAPP/ , i0/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         mua = two(iua)
!
!     INITIALLIZE CORE, ETC
!
         IF ( dry==0 ) RETURN
         nc = korsz(z(1))
         b1 = nc - bsize + 1
!
!     OPEN SCRATCH FILE TO WRITE CONVERTED DATA
!
         b2 = b1 - bsize
         b3 = b2 - bsize
         buf1 = b3 - bsize
         buf2 = buf1 - bsize
         nz = buf2 - 1
!
!     TEST FOR CORE
!
         IF ( nz>0 ) THEN
!
            CALL sofopn(z(b1),z(b2),z(b3))
!
!     EQSS GENERATION
!
            file = uset
            CALL open(*260,uset,z(buf1),0)
            CALL fwdrec(*260,uset)
!
!     READ USET INTO CORE
!
            CALL read(*260,*20,uset,z(1),nz,0,nu)
!
!     RAN OUT OF CORE
!
            CALL close(uset,1)
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
 20      CALL close(uset,1)
!
!     FLAG ELEMENTS IN UA SET  (SET OTHERS TO ZERO)
!
         DO i = 1 , nu
            IF ( andf(mua,z(i))==0 ) THEN
               z(i) = 0
            ELSE
               z(i) = 1
            ENDIF
         ENDDO
!
!     READ  SECOND RECORD OF EQEXIN - CONTAINS  G AND SIL PAIRS
!
         file = eqex
         CALL open(*260,eqex,z(buf1),0)
         CALL fwdrec(*260,eqex)
         CALL fwdrec(*260,eqex)
!
!     OPEN SCRATCH FILE TO WRITE CONVERTED DATA
!
         CALL open(*260,scrt,z(buf2),1)
!
!     LOOP ON GRID POINTS
!
         k = 0
         i = 0
         DO
!
            CALL read(*260,*40,eqex,buf,2,0,nwds)
            c = 0
            i = i + 1
            isil = buf(2)/10
            type = buf(2) - 10*isil
            IF ( type<2 ) THEN
!
!     GRID POINT, DETERMINE UA COMPONENTS, PUT IN BINARY FORM
!
               DO j = 1 , 6
                  iu = isil + j - 1
                  IF ( z(iu)/=0 ) c = orf(c,lshift(1,j-1))
               ENDDO
            ELSEIF ( type==2 ) THEN
!
!     SCALAR POINT
!
               IF ( z(isil)/=0 ) c = 1
            ELSE
!
!     BAD GRID POINT TYPE (IE AXISYMMETRIC OR)
!
               WRITE (out,99001) ufm , buf(1)
99001          FORMAT (A23,' 6013 , ILLEGAL TYPE OF POINT DEFINED FOR ','SUBSTRUCTURE ANALYSIS.',/5X,'POINT NUMBER =',I9)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     WRITE OUT G AND C
!
            IF ( c/=0 ) THEN
               buf(2) = c
               CALL write(scrt,buf,2,0)
               k = k + 1
            ENDIF
         ENDDO
!
 40      mcb(1) = eqex
         CALL rdtrl(mcb)
         npts = mcb(2)
         CALL rewind(eqex)
         CALL close(scrt,1)
         IF ( npts*2>nz ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ FIRST RECORD OF EQEXIN - GET G AND IOLD
!     READ SCRATCH - GET G AND C
!     BUILD TABLE IN CORE
!
         file = eqex
         CALL fwdrec(*260,eqex)
         file = scrt
         CALL open(*260,scrt,z(buf2),0)
!
!     SET CORE TO ZERO
!
         DO i = 1 , npts
            izp = 2*i
            z(izp) = 0
            z(izp-1) = 0
         ENDDO
         nnew = k
!
!     LOOP ON POINTS IN SCRATCH FILE, STORE C IN ITH WORD OF ENTRY
!     POSITION OF ENTRY IS THE INTERNAL SEQUENCE
!
         IF ( k>0 ) THEN
            DO i = 1 , k
               file = scrt
               CALL read(*260,*60,scrt,buf,2,0,nwds)
               file = eqex
               SPAG_Loop_2_1: DO
                  CALL read(*260,*60,eqex,temp,2,0,nwds)
                  IF ( buf(1)<temp(1) ) GOTO 260
                  IF ( buf(1)==temp(1) ) THEN
                     izp = 2*temp(2)
                     z(izp) = buf(2)
                     EXIT SPAG_Loop_2_1
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ENDDO
         ENDIF
!
!     CORE TABLE IS COMPLETE, FILL IN FIRST ENTRIES
!
 60      CALL close(scrt,1)
         CALL rewind(eqex)
         k = 0
         DO i = 1 , npts
            IF ( z(2*i)/=0 ) THEN
               k = k + 1
               z(2*i-1) = k
            ENDIF
         ENDDO
!
!     CORE NOW CONTAINS NEW IP VALUES AND C IN OLD IP POSITIONS
!
         file = eqss
!
!     CHECK IF SUBSTRUCTURE EXISTS ALREADY
!
         CALL fwdrec(*260,eqex)
         CALL setlvl(name,0,temp,itest,0)
         IF ( itest/=1 ) WRITE (out,99002) uwm , name
!
!
99002    FORMAT (A25,' 6325, SUBSTRUCTURE PHASE 1, BASIC SUBSTRUCTURE ',2A4,' ALREADY EXISTS ON SOF.',/32X,                         &
                &'ITEMS WHICH ALREADY EXIST WILL NOT BE REGENERATED.')
         itest = 3
         CALL sfetch(name,eqss,2,itest)
         IF ( itest==3 ) THEN
            buf(1) = name(1)
            buf(2) = name(2)
            buf(3) = 1
            buf(4) = nnew
            buf(5) = name(1)
            buf(6) = name(2)
!
            CALL suwrt(buf,6,2)
!
!     PROCESS EQSS OUTPUT-  G, IP, C - SORTED ON G
!
            DO i = 1 , npts
!
               CALL read(*260,*70,eqex,temp,2,0,nwds)
!
               ipt = temp(2)*2 - 1
               IF ( z(ipt)/=0 ) THEN
                  temp(2) = z(ipt)
                  temp(3) = z(ipt+1)
                  CALL suwrt(temp,3,1)
               ENDIF
 70         ENDDO
            CALL suwrt(temp,0,2)
!
!     BUILD SIL TABLE BY COUNTING C VALUES
!
            nc = 0
            is = 1
            DO i = 1 , npts
               ipt = 2*i - 1
!
               IF ( z(ipt)/=0 ) THEN
                  is = is + nc
                  z(ipt) = is
!
                  CALL suwrt(z(ipt),2,1)
!
!     CALCULATE NUMBER OF COMPONENTS FOR NEXT STEP
!
                  kcode = z(ipt+1)
                  CALL decode(kcode,icode,nc)
               ENDIF
            ENDDO
            CALL suwrt(0,0,2)
            CALL suwrt(temp,0,3)
         ELSE
            WRITE (out,99007) uwm , name , eqss
         ENDIF
         CALL close(eqex,1)
!
!     BGSS GENERATION
!
         file = bgpd
         CALL open(*260,bgpd,z(buf1),0)
         CALL fwdrec(*260,bgpd)
         itest = 3
         CALL sfetch(name,bgss,2,itest)
         IF ( itest==3 ) THEN
!
            buf(1) = name(1)
            buf(2) = name(2)
            buf(3) = nnew
            CALL suwrt(buf,3,2)
            DO i = 1 , npts
               CALL read(*260,*80,bgpd,buf,4,0,nwds)
!
!
               IF ( z(2*i-1)/=0 ) CALL suwrt(buf,4,1)
 80         ENDDO
            CALL suwrt(0,0,2)
            CALL suwrt(buf,0,3)
         ELSE
            WRITE (out,99007) uwm , name , bgss
         ENDIF
         CALL close(bgpd,1)
!
!
!     CSTM GENERATION
!
!
         CALL open(*120,cstm,z(buf1),0)
!
!     CSTM EXISTS
!
         CALL fwdrec(*260,cstm)
         itest = 3
         CALL sfetch(name,icstm,2,itest)
         IF ( itest==3 ) THEN
!
            buf(1) = name(1)
            buf(2) = name(2)
            CALL suwrt(buf,2,2)
!
!     BLAST COPY
!
            CALL read(*260,*100,cstm,z(1),nz,1,nwds)
            spag_nextblock_1 = 6
         ELSE
            WRITE (out,99007) uwm , name , icstm
            spag_nextblock_1 = 2
         ENDIF
         CYCLE
 100     CALL suwrt(z(1),nwds,2)
         CALL suwrt(0,0,3)
         spag_nextblock_1 = 2
      CASE (2)
         CALL close(cstm,1)
!
!     LODS GENERATION
!
 120     nlod = 0
!
         CALL gopen(case,z(buf1),0)
!
         icase = 0
         DO
!
            CALL read(*140,*140,case,z(1),9,1,nwds)
            icase = icase + 1
            IF ( z(i0+4)/=0 ) THEN
               WRITE (out,99008) uim , name , icase , ltype1 , z(i0+4)
               z(nlod+10) = z(i0+4)
            ELSEIF ( z(i0+7)/=0 ) THEN
               WRITE (out,99008) uim , name , icase , ltype2 , z(i0+7)
               z(nlod+10) = z(i0+7)
            ELSEIF ( z(i0+6)==0 ) THEN
               z(nlod+10) = 0
            ELSE
               WRITE (out,99008) uim , name , icase , ltype3 , z(i0+6)
               z(nlod+10) = z(i0+6)
            ENDIF
            nlod = nlod + 1
         ENDDO
 140     itest = 3
         litm = lods
         IF ( pitm==papp ) litm = loap
         CALL sfetch(name,litm,2,itest)
         IF ( itest==3 ) THEN
            z(1) = name(1)
            z(i0+2) = name(2)
            z(i0+3) = nlod
            z(i0+4) = 1
            z(i0+5) = name(1)
            z(i0+6) = name(2)
            CALL suwrt(z(1),6,2)
            CALL suwrt(nlod,1,1)
            CALL suwrt(z(i0+10),nlod,2)
            CALL suwrt(z(1),0,3)
         ELSE
            WRITE (out,99007) uwm , name , litm
         ENDIF
         CALL close(case,1)
!
!     PLOT SET DATA (PLTS) GENERATION
!
         IF ( pset<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = bgpd
         CALL gopen(bgpd,z(buf1),0)
!
         itest = 3
         CALL sfetch(name,plts,2,itest)
         IF ( itest==3 ) THEN
!
            buf(1) = name(1)
            buf(2) = name(2)
            buf(3) = 1
            buf(4) = name(1)
            buf(5) = name(2)
            CALL suwrt(buf,5,1)
            DO i = 1 , 11
               z(i) = 0
            ENDDO
            rz(4) = 1.0
            rz(8) = 1.0
            rz(12) = 1.0
            CALL suwrt(z,12,2)
!
            CALL read(*260,*160,bgpd,z(1),nz,0,nwds)
            spag_nextblock_1 = 6
         ELSE
            WRITE (out,99007) uwm , name , plts
            CALL close(bgpd,1)
            spag_nextblock_1 = 5
         ENDIF
         CYCLE
 160     CALL suwrt(z,nwds,2)
         CALL close(bgpd,1)
         file = eqex
         CALL gopen(eqex,z(buf1),0)
         CALL read(*260,*180,eqex,z,nz,1,nwds)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 180     CALL suwrt(z,nwds,2)
         CALL close(eqex,1)
         file = gpse
         last = .FALSE.
         CALL open(*240,gpse,z(buf1),0)
!
         CALL fwdrec(*240,gpse)
!
         CALL read(*260,*200,gpse,z(1),nz,0,nsets)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     FIND PLOT SET ID
!
 200     IF ( nsets/=0 ) THEN
!
            DO i = 1 , nsets
               IF ( z(i)==pset ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         GOTO 240
      CASE (3)
         irec = i - 1
         spag_nextblock_1 = 4
      CASE (4)
!
         IF ( irec/=0 ) THEN
!
!     POSITION FILE TO SELECTED SET
!
            DO i = 1 , irec
               CALL fwdrec(*240,file)
            ENDDO
         ENDIF
         CALL read(*240,*220,file,z(1),nz,0,nwds)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     CALL suwrt(z(1),nwds,2)
         CALL close(file,1)
         IF ( last ) THEN
!
!     FINISHED
!
            CALL suwrt(z(1),0,3)
            spag_nextblock_1 = 5
         ELSE
            last = .TRUE.
            file = else
            CALL open(*240,else,z(buf1),0)
            CALL fwdrec(*240,else)
            spag_nextblock_1 = 4
         ENDIF
         CYCLE
 240     CALL close(file,1)
         WRITE (out,99003) uwm , pset
99003    FORMAT (A25,' 6050, REQUESTED PLOT SET NO.',I8,' HAS NOT BEEN DEFINED')
         spag_nextblock_1 = 5
      CASE (5)
!
         CALL sofcls
         WRITE (out,99004) uim , name
99004    FORMAT (A29,' 6361, PHASE 1 SUCCESSFULLY EXECUTED FOR ','SUBSTRUCTURE ',2A4)
         RETURN
      CASE (6)
!
!     INSUFFICIENT CORE
!
         WRITE (out,99005) ufm , nz
99005    FORMAT (A23,' 6011, INSUFFICIENT CORE TO LOAD TABLES',/5X,'IN MODULE SUBPH1, CORE =',1I8)
         dry = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     BAD FILE
!
 260     WRITE (out,99006) sfm , file
99006    FORMAT (A25,' 6012, FILE =',I4,' IS PURGED OR NULL AND IS ','REQUIRED IN PHASE 1 SUBSTRUCTURE ANALYSIS.')
         spag_nextblock_1 = 7
      CASE (7)
!
         CALL sofcls
         CALL mesage(-61,0,sub1)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (A25,' 6326, SUBSTRUCTURE ',2A4,', ITEM ',A4,' ALREADY EXISTS ON SOF.')
99008 FORMAT (A29,' 6327, SUBSTRUCTURE ',2A4,' SUBCASE',I9,' IS IDENTIFIED BY',/36X,5A4,' SET',I9,' IN LODS ITEM.',/36X,            &
             &'REFER TO THIS NUMBER ON LOADC CARDS.')
END SUBROUTINE subph1
