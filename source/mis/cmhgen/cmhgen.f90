!*==cmhgen.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmhgen
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_packx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , ihead
   INTEGER :: bufex , i , i1 , i2 , iaddh , ic , icode , idh , idhm1 , ifile , imsg , ioefil , ipnew , ipold , ir , ishptr , itest ,&
            & j , j1 , j2 , lhptr , locipn , locipo , ncn , nco , ncol , ncom , ncs , nnn , nrow , nsilnw , nslold , nsub , ssil
   INTEGER , DIMENSION(10) :: ce
   REAL , DIMENSION(6) :: colout
   LOGICAL :: frsfil
   INTEGER , DIMENSION(32) :: listn , listo
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) :: nam
   INTEGER , SAVE :: nheqss
   REAL , DIMENSION(6,6) :: t , tp , tpp , ttran
   REAL , DIMENSION(6,6) , SAVE :: tid
   REAL , SAVE :: zero
   EXTERNAL bckrec , close , decode , fwdrec , mesage , mtrxo , open , pack , read , sfetch , sjump , skpfil , suread , write ,     &
          & wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE GENERATES THE (H) TRANSFORMATION MATRICES FOR
!     COMPONENT SUBSTRUCTURES IN A COMBINE OPERATION AND WRITES THEM
!     ON THE SOF
!
   DATA zero/0.0/ , aaa/4HCMHG , 4HEN  / , ihead/4HHORG , 4H    /
   DATA tid/1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , &
      & 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1./
   DATA nheqss/4HEQSS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ SIL,C FROM SOF FOR COMBINED STRUCTURE
!
         incr = 1
         bufex = lcore - buf2 + buf3
         lcore = bufex - 1
         IF ( lcore<0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ioefil = 310
         CALL open(*20,ioefil,z(bufex),0)
         mcb(1) = scr1
         mcb(4) = 2
         mcb(5) = 1
         iin = 1
         iout = 1
         CALL sfetch(cnam,nheqss,1,itest)
         nsub = 0
         DO i = 1 , npsub
            nsub = nsub + combo(i,5)
         ENDDO
         CALL sjump(nsub+1)
         CALL suread(z(score),-1,nsilnw,itest)
!
!     LOOP ON NUMBER OF PSEUDO-STRUCTURES BEING COMBINED
!
         ssil = score + nsilnw
         lcore = lcore - nsilnw
         ifile = scr3
         CALL open(*20,scr3,z(buf1),0)
         ifile = scsfil
         CALL open(*20,scsfil,z(buf2),0)
!
         DO i = 1 , npsub
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  frsfil = .TRUE.
                  mcb(2) = 0
                  mcb(6) = 0
                  mcb(7) = 0
!
!     READ SIL,C FOR COMPONENT SUBSTRUCTURE
!
                  ncs = combo(i,5) + 2
                  DO j = 1 , ncs
                     CALL fwdrec(*40,scsfil)
                  ENDDO
                  ifile = ioefil
                  CALL read(*40,*2,ioefil,z(ssil),lcore,1,nslold)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 2                ishptr = ssil + nslold
                  ifile = scsfil
                  CALL read(*40,*4,scsfil,z(ishptr),lcore,1,lhptr)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 4                CALL skpfil(scsfil,1)
!
!     COMPUTE NUMBER OF ROWS IN MATRIX
!
                  icode = z(ssil+nslold-1)
                  CALL decode(icode,listo,ncom)
                  mcb(3) = z(ssil+nslold-2) + ncom - 1
!
!     READ CONNECTION ENTRIES
!
!     READ TRANSFORMATION MATRIX FOR PSEUDOSTRUCTURE
!
                  ifile = scr3
                  CALL read(*40,*6,scr3,ttran,37,1,nnn)
 6                CALL skpfil(scr3,-1)
                  IF ( i/=1 ) CALL skpfil(scr3,1)
                  ifile = scconn
                  CALL open(*20,scconn,z(buf3),0)
                  ifile = scr1
                  CALL open(*20,scr1,z(buf4),1)
                  CALL write(scr1,ihead,2,1)
                  ipnew = 0
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL read(*12,*8,scconn,ce,10,1,nnn)
 8                ipnew = ipnew + 1
                  locipn = score + 2*(ipnew-1) + 1
                  IF ( ce(i+2)==0 ) THEN
                     iiii = 1
                     nnnn = 1
                     icode = z(locipn)
                     CALL decode(icode,listn,ncn)
                     DO i1 = 1 , ncn
                        CALL pack(zero,scr1,mcb)
                     ENDDO
                     spag_nextblock_2 = 2
                  ELSE
                     ipold = ce(i+2)
                     locipo = ssil + 2*(ipold-1) + 1
                     icode = z(locipn)
                     CALL decode(icode,listn,ncn)
                     icode = z(locipo)
                     CALL decode(icode,listo,nco)
!
                     iaddh = ishptr + ipold - 1
                     idh = z(iaddh)
                     IF ( idh<1 ) THEN
!
!     IDENTITY MATRIX
!
                        DO i1 = 1 , 6
                           DO i2 = 1 , 6
                              t(i1,i2) = tid(i1,i2)
                           ENDDO
                        ENDDO
                     ELSEIF ( idh==1 ) THEN
!
!     TRANS MATRIX
!
                        DO i1 = 1 , 6
                           DO i2 = 1 , 6
                              t(i1,i2) = ttran(i1,i2)
                           ENDDO
                        ENDDO
                     ELSE
!
!     MATRIX DUE TO GTRAN
!
                        idhm1 = idh - 1
                        DO i1 = 1 , idhm1
                           CALL fwdrec(*40,scr3)
                        ENDDO
                        CALL read(*40,*10,scr3,t,37,1,nnn)
                        GOTO 10
                     ENDIF
                     spag_nextblock_2 = 3
                  ENDIF
                  CYCLE
 10               DO i1 = 1 , idh
                     CALL bckrec(scr3)
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (3)
!
!     DELETE ROWS OF (T) FOR EACH COLD EQUAL TO ZERO
!
                  DO j1 = 1 , nco
                     ir = listo(j1) + 1
                     DO j2 = 1 , 6
                        tp(j1,j2) = t(ir,j2)
                     ENDDO
                  ENDDO
                  nrow = nco
!
!     DELETE COLUMNS OF (T) FOR EACH CNEW EQUAL TO ZERO
!
                  DO j1 = 1 , ncn
                     ic = listn(j1) + 1
                     DO j2 = 1 , nrow
                        tpp(j2,j1) = tp(j2,ic)
                     ENDDO
                  ENDDO
                  ncol = ncn
                  DO i1 = 1 , ncol
                     DO i2 = 1 , nrow
                        colout(i2) = tpp(i2,i1)
                     ENDDO
                     iiii = z(locipo-1)
                     nnnn = iiii + nrow - 1
                     CALL pack(colout,scr1,mcb)
                  ENDDO
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 12               CALL close(scconn,1)
                  CALL wrttrl(mcb)
                  CALL close(scr1,1)
                  nam(1) = combo(i,1)
                  nam(2) = combo(i,2)
                  CALL mtrxo(scr1,nam,ihead(1),z(buf4),itest)
                  CALL skpfil(scr3,1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
         CALL close(scsfil,1)
         CALL close(scr3,1)
         CALL close(ioefil,1)
         lcore = bufex + buf2 - buf3
         RETURN
!
 20      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 3
      CASE (2)
         imsg = -8
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmhgen
