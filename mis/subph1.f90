
SUBROUTINE subph1
!
!     THIS MODULE PERFORMS THE PHASE 1 CONVERSION OF NASTRAN DATA BLOCK
!     TABLES TO THEIR EQUIVALENT SOF ITEMS
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bsize , Dry , Name(2) , Out , Pitm , Pset , Two(32) , Z(1)
   REAL Rz(12)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Name , Pset , Pitm
   COMMON /system/ Bsize , Out
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER andf , korsz , lshift , orf
   INTEGER b1 , b2 , b3 , bgpd , bgss , buf(10) , buf1 , buf2 , c , case , cstm , else , eqex , eqss , file , gpse , i , i0 ,       &
         & icase , icode(32) , icstm , ipt , irec , is , isil , itest , iu , iua , izp , j , k , kcode , litm , loap , lods ,       &
         & ltype1(5) , ltype2(5) , ltype3(5) , mcb(7) , mua , nc , nlod , nnew , npts , nsets , nu , nwds , nz , papp , plts ,      &
         & scrt , sub1(2) , temp(10) , type , uset
   LOGICAL last
   EXTERNAL andf , lshift , orf
!
! End of declarations
!
   EQUIVALENCE (Rz(1),Z(1))
   DATA case , eqex , uset , bgpd , cstm , gpse , else , scrt/101 , 102 , 103 , 104 , 105 , 106 , 107 , 301/
   DATA eqss/4HEQSS/ , icstm/4HCSTM/ , lods/4HLODS/ , plts/4HPLTS/ , bgss/4HBGSS/
   DATA iua/25/ , sub1/4HSUBP , 4HH1  /
   DATA ltype1/4HEXTE , 4HRNAL , 4H STA , 4HTIC  , 4HLOAD/
   DATA ltype2/4H     , 4H     , 4HTHER , 4HMAL  , 4HLOAD/
   DATA ltype3/4H ELE , 4HMENT , 4H DEF , 4HORMA , 4HTION/
   DATA loap , papp/4HLOAP , 4HPAPP/ , i0/0/
!
   mua = Two(iua)
!
!     INITIALLIZE CORE, ETC
!
   IF ( Dry==0 ) RETURN
   nc = korsz(Z(1))
   b1 = nc - Bsize + 1
!
!     OPEN SCRATCH FILE TO WRITE CONVERTED DATA
!
   b2 = b1 - Bsize
   b3 = b2 - Bsize
   buf1 = b3 - Bsize
   buf2 = buf1 - Bsize
   nz = buf2 - 1
!
!     TEST FOR CORE
!
   IF ( nz>0 ) THEN
!
      CALL sofopn(Z(b1),Z(b2),Z(b3))
!
!     EQSS GENERATION
!
      file = uset
      CALL open(*1800,uset,Z(buf1),0)
      CALL fwdrec(*1800,uset)
!
!     READ USET INTO CORE
!
      CALL read(*1800,*100,uset,Z(1),nz,0,nu)
!
!     RAN OUT OF CORE
!
      CALL close(uset,1)
   ENDIF
   GOTO 1700
!
 100  CALL close(uset,1)
!
!     FLAG ELEMENTS IN UA SET  (SET OTHERS TO ZERO)
!
   DO i = 1 , nu
      IF ( andf(mua,Z(i))==0 ) THEN
         Z(i) = 0
      ELSE
         Z(i) = 1
      ENDIF
   ENDDO
!
!     READ  SECOND RECORD OF EQEXIN - CONTAINS  G AND SIL PAIRS
!
   file = eqex
   CALL open(*1800,eqex,Z(buf1),0)
   CALL fwdrec(*1800,eqex)
   CALL fwdrec(*1800,eqex)
!
!     OPEN SCRATCH FILE TO WRITE CONVERTED DATA
!
   CALL open(*1800,scrt,Z(buf2),1)
!
!     LOOP ON GRID POINTS
!
   k = 0
   i = 0
   DO
!
      CALL read(*1800,*200,eqex,buf,2,0,nwds)
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
            IF ( Z(iu)/=0 ) c = orf(c,lshift(1,j-1))
         ENDDO
      ELSEIF ( type==2 ) THEN
!
!     SCALAR POINT
!
         IF ( Z(isil)/=0 ) c = 1
      ELSE
!
!     BAD GRID POINT TYPE (IE AXISYMMETRIC OR)
!
         WRITE (Out,99001) Ufm , buf(1)
99001    FORMAT (A23,' 6013 , ILLEGAL TYPE OF POINT DEFINED FOR ','SUBSTRUCTURE ANALYSIS.',/5X,'POINT NUMBER =',I9)
         GOTO 1900
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
 200  mcb(1) = eqex
   CALL rdtrl(mcb)
   npts = mcb(2)
   CALL rewind(eqex)
   CALL close(scrt,1)
   IF ( npts*2>nz ) GOTO 1700
!
!     READ FIRST RECORD OF EQEXIN - GET G AND IOLD
!     READ SCRATCH - GET G AND C
!     BUILD TABLE IN CORE
!
   file = eqex
   CALL fwdrec(*1800,eqex)
   file = scrt
   CALL open(*1800,scrt,Z(buf2),0)
!
!     SET CORE TO ZERO
!
   DO i = 1 , npts
      izp = 2*i
      Z(izp) = 0
      Z(izp-1) = 0
   ENDDO
   nnew = k
!
!     LOOP ON POINTS IN SCRATCH FILE, STORE C IN ITH WORD OF ENTRY
!     POSITION OF ENTRY IS THE INTERNAL SEQUENCE
!
   IF ( k>0 ) THEN
      DO i = 1 , k
         file = scrt
         CALL read(*1800,*300,scrt,buf,2,0,nwds)
         file = eqex
         DO
            CALL read(*1800,*300,eqex,temp,2,0,nwds)
            IF ( buf(1)<temp(1) ) GOTO 1800
            IF ( buf(1)==temp(1) ) THEN
               izp = 2*temp(2)
               Z(izp) = buf(2)
               EXIT
            ENDIF
         ENDDO
      ENDDO
   ENDIF
!
!     CORE TABLE IS COMPLETE, FILL IN FIRST ENTRIES
!
 300  CALL close(scrt,1)
   CALL rewind(eqex)
   k = 0
   DO i = 1 , npts
      IF ( Z(2*i)/=0 ) THEN
         k = k + 1
         Z(2*i-1) = k
      ENDIF
   ENDDO
!
!     CORE NOW CONTAINS NEW IP VALUES AND C IN OLD IP POSITIONS
!
   file = eqss
!
!     CHECK IF SUBSTRUCTURE EXISTS ALREADY
!
   CALL fwdrec(*1800,eqex)
   CALL setlvl(Name,0,temp,itest,0)
   IF ( itest/=1 ) WRITE (Out,99002) Uwm , Name
!
!
99002 FORMAT (A25,' 6325, SUBSTRUCTURE PHASE 1, BASIC SUBSTRUCTURE ',2A4,' ALREADY EXISTS ON SOF.',/32X,                            &
             &'ITEMS WHICH ALREADY EXIST WILL NOT BE REGENERATED.')
   itest = 3
   CALL sfetch(Name,eqss,2,itest)
   IF ( itest==3 ) THEN
      buf(1) = Name(1)
      buf(2) = Name(2)
      buf(3) = 1
      buf(4) = nnew
      buf(5) = Name(1)
      buf(6) = Name(2)
!
      CALL suwrt(buf,6,2)
!
!     PROCESS EQSS OUTPUT-  G, IP, C - SORTED ON G
!
      DO i = 1 , npts
!
         CALL read(*1800,*350,eqex,temp,2,0,nwds)
!
         ipt = temp(2)*2 - 1
         IF ( Z(ipt)/=0 ) THEN
            temp(2) = Z(ipt)
            temp(3) = Z(ipt+1)
            CALL suwrt(temp,3,1)
         ENDIF
 350  ENDDO
      CALL suwrt(temp,0,2)
!
!     BUILD SIL TABLE BY COUNTING C VALUES
!
      nc = 0
      is = 1
      DO i = 1 , npts
         ipt = 2*i - 1
!
         IF ( Z(ipt)/=0 ) THEN
            is = is + nc
            Z(ipt) = is
!
            CALL suwrt(Z(ipt),2,1)
!
!     CALCULATE NUMBER OF COMPONENTS FOR NEXT STEP
!
            kcode = Z(ipt+1)
            CALL decode(kcode,icode,nc)
         ENDIF
      ENDDO
      CALL suwrt(0,0,2)
      CALL suwrt(temp,0,3)
   ELSE
      WRITE (Out,99007) Uwm , Name , eqss
   ENDIF
   CALL close(eqex,1)
!
!     BGSS GENERATION
!
   file = bgpd
   CALL open(*1800,bgpd,Z(buf1),0)
   CALL fwdrec(*1800,bgpd)
   itest = 3
   CALL sfetch(Name,bgss,2,itest)
   IF ( itest==3 ) THEN
!
      buf(1) = Name(1)
      buf(2) = Name(2)
      buf(3) = nnew
      CALL suwrt(buf,3,2)
      DO i = 1 , npts
         CALL read(*1800,*400,bgpd,buf,4,0,nwds)
!
!
         IF ( Z(2*i-1)/=0 ) CALL suwrt(buf,4,1)
 400  ENDDO
      CALL suwrt(0,0,2)
      CALL suwrt(buf,0,3)
   ELSE
      WRITE (Out,99007) Uwm , Name , bgss
   ENDIF
   CALL close(bgpd,1)
!
!
!     CSTM GENERATION
!
!
   CALL open(*700,cstm,Z(buf1),0)
!
!     CSTM EXISTS
!
   CALL fwdrec(*1800,cstm)
   itest = 3
   CALL sfetch(Name,icstm,2,itest)
   IF ( itest==3 ) THEN
!
      buf(1) = Name(1)
      buf(2) = Name(2)
      CALL suwrt(buf,2,2)
!
!     BLAST COPY
!
      CALL read(*1800,*500,cstm,Z(1),nz,1,nwds)
      GOTO 1700
   ELSE
      WRITE (Out,99007) Uwm , Name , icstm
      GOTO 600
   ENDIF
 500  CALL suwrt(Z(1),nwds,2)
   CALL suwrt(0,0,3)
 600  CALL close(cstm,1)
!
!     LODS GENERATION
!
 700  nlod = 0
!
   CALL gopen(case,Z(buf1),0)
!
   icase = 0
   DO
!
      CALL read(*800,*800,case,Z(1),9,1,nwds)
      icase = icase + 1
      IF ( Z(i0+4)/=0 ) THEN
         WRITE (Out,99008) Uim , Name , icase , ltype1 , Z(i0+4)
         Z(nlod+10) = Z(i0+4)
      ELSEIF ( Z(i0+7)/=0 ) THEN
         WRITE (Out,99008) Uim , Name , icase , ltype2 , Z(i0+7)
         Z(nlod+10) = Z(i0+7)
      ELSEIF ( Z(i0+6)==0 ) THEN
         Z(nlod+10) = 0
      ELSE
         WRITE (Out,99008) Uim , Name , icase , ltype3 , Z(i0+6)
         Z(nlod+10) = Z(i0+6)
      ENDIF
      nlod = nlod + 1
   ENDDO
 800  itest = 3
   litm = lods
   IF ( Pitm==papp ) litm = loap
   CALL sfetch(Name,litm,2,itest)
   IF ( itest==3 ) THEN
      Z(1) = Name(1)
      Z(i0+2) = Name(2)
      Z(i0+3) = nlod
      Z(i0+4) = 1
      Z(i0+5) = Name(1)
      Z(i0+6) = Name(2)
      CALL suwrt(Z(1),6,2)
      CALL suwrt(nlod,1,1)
      CALL suwrt(Z(i0+10),nlod,2)
      CALL suwrt(Z(1),0,3)
   ELSE
      WRITE (Out,99007) Uwm , Name , litm
   ENDIF
   CALL close(case,1)
!
!     PLOT SET DATA (PLTS) GENERATION
!
   IF ( Pset<=0 ) GOTO 1600
   file = bgpd
   CALL gopen(bgpd,Z(buf1),0)
!
   itest = 3
   CALL sfetch(Name,plts,2,itest)
   IF ( itest==3 ) THEN
!
      buf(1) = Name(1)
      buf(2) = Name(2)
      buf(3) = 1
      buf(4) = Name(1)
      buf(5) = Name(2)
      CALL suwrt(buf,5,1)
      DO i = 1 , 11
         Z(i) = 0
      ENDDO
      Rz(4) = 1.0
      Rz(8) = 1.0
      Rz(12) = 1.0
      CALL suwrt(Z,12,2)
!
      CALL read(*1800,*900,bgpd,Z(1),nz,0,nwds)
      GOTO 1700
   ELSE
      WRITE (Out,99007) Uwm , Name , plts
      CALL close(bgpd,1)
      GOTO 1600
   ENDIF
 900  CALL suwrt(Z,nwds,2)
   CALL close(bgpd,1)
   file = eqex
   CALL gopen(eqex,Z(buf1),0)
   CALL read(*1800,*1000,eqex,Z,nz,1,nwds)
   GOTO 1700
 1000 CALL suwrt(Z,nwds,2)
   CALL close(eqex,1)
   file = gpse
   last = .FALSE.
   CALL open(*1500,gpse,Z(buf1),0)
!
   CALL fwdrec(*1500,gpse)
!
   CALL read(*1800,*1100,gpse,Z(1),nz,0,nsets)
   GOTO 1700
!
!     FIND PLOT SET ID
!
 1100 IF ( nsets/=0 ) THEN
!
      DO i = 1 , nsets
         IF ( Z(i)==Pset ) GOTO 1200
      ENDDO
   ENDIF
   GOTO 1500
 1200 irec = i - 1
!
 1300 IF ( irec/=0 ) THEN
!
!     POSITION FILE TO SELECTED SET
!
      DO i = 1 , irec
         CALL fwdrec(*1500,file)
      ENDDO
   ENDIF
   CALL read(*1500,*1400,file,Z(1),nz,0,nwds)
   GOTO 1700
 1400 CALL suwrt(Z(1),nwds,2)
   CALL close(file,1)
   IF ( last ) THEN
!
!     FINISHED
!
      CALL suwrt(Z(1),0,3)
      GOTO 1600
   ELSE
      last = .TRUE.
      file = else
      CALL open(*1500,else,Z(buf1),0)
      CALL fwdrec(*1500,else)
      GOTO 1300
   ENDIF
 1500 CALL close(file,1)
   WRITE (Out,99003) Uwm , Pset
99003 FORMAT (A25,' 6050, REQUESTED PLOT SET NO.',I8,' HAS NOT BEEN DEFINED')
!
 1600 CALL sofcls
   WRITE (Out,99004) Uim , Name
99004 FORMAT (A29,' 6361, PHASE 1 SUCCESSFULLY EXECUTED FOR ','SUBSTRUCTURE ',2A4)
   RETURN
!
!     INSUFFICIENT CORE
!
 1700 WRITE (Out,99005) Ufm , nz
99005 FORMAT (A23,' 6011, INSUFFICIENT CORE TO LOAD TABLES',/5X,'IN MODULE SUBPH1, CORE =',1I8)
   Dry = -2
   GOTO 1900
!
!     BAD FILE
!
 1800 WRITE (Out,99006) Sfm , file
99006 FORMAT (A25,' 6012, FILE =',I4,' IS PURGED OR NULL AND IS ','REQUIRED IN PHASE 1 SUBSTRUCTURE ANALYSIS.')
!
 1900 CALL sofcls
   CALL mesage(-61,0,sub1)
   RETURN
99007 FORMAT (A25,' 6326, SUBSTRUCTURE ',2A4,', ITEM ',A4,' ALREADY EXISTS ON SOF.')
99008 FORMAT (A29,' 6327, SUBSTRUCTURE ',2A4,' SUBCASE',I9,' IS IDENTIFIED BY',/36X,5A4,' SET',I9,' IN LODS ITEM.',/36X,            &
             &'REFER TO THIS NUMBER ON LOADC CARDS.')
END SUBROUTINE subph1
