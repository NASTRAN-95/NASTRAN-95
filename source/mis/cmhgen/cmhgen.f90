!*==cmhgen.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmhgen
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_PACKX
   USE C_ZZZZZZ
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
         Incr = 1
         bufex = Lcore - Buf2 + Buf3
         Lcore = bufex - 1
         IF ( Lcore<0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ioefil = 310
         CALL open(*20,ioefil,Z(bufex),0)
         mcb(1) = Scr1
         mcb(4) = 2
         mcb(5) = 1
         Iin = 1
         Iout = 1
         CALL sfetch(Cnam,nheqss,1,itest)
         nsub = 0
         DO i = 1 , Npsub
            nsub = nsub + Combo(i,5)
         ENDDO
         CALL sjump(nsub+1)
         CALL suread(Z(Score),-1,nsilnw,itest)
!
!     LOOP ON NUMBER OF PSEUDO-STRUCTURES BEING COMBINED
!
         ssil = Score + nsilnw
         Lcore = Lcore - nsilnw
         ifile = Scr3
         CALL open(*20,Scr3,Z(Buf1),0)
         ifile = Scsfil
         CALL open(*20,Scsfil,Z(Buf2),0)
!
         DO i = 1 , Npsub
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
                  ncs = Combo(i,5) + 2
                  DO j = 1 , ncs
                     CALL fwdrec(*40,Scsfil)
                  ENDDO
                  ifile = ioefil
                  CALL read(*40,*2,ioefil,Z(ssil),Lcore,1,nslold)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 2                ishptr = ssil + nslold
                  ifile = Scsfil
                  CALL read(*40,*4,Scsfil,Z(ishptr),Lcore,1,lhptr)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 4                CALL skpfil(Scsfil,1)
!
!     COMPUTE NUMBER OF ROWS IN MATRIX
!
                  icode = Z(ssil+nslold-1)
                  CALL decode(icode,listo,ncom)
                  mcb(3) = Z(ssil+nslold-2) + ncom - 1
!
!     READ CONNECTION ENTRIES
!
!     READ TRANSFORMATION MATRIX FOR PSEUDOSTRUCTURE
!
                  ifile = Scr3
                  CALL read(*40,*6,Scr3,ttran,37,1,nnn)
 6                CALL skpfil(Scr3,-1)
                  IF ( i/=1 ) CALL skpfil(Scr3,1)
                  ifile = Scconn
                  CALL open(*20,Scconn,Z(Buf3),0)
                  ifile = Scr1
                  CALL open(*20,Scr1,Z(Buf4),1)
                  CALL write(Scr1,ihead,2,1)
                  ipnew = 0
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL read(*12,*8,Scconn,ce,10,1,nnn)
 8                ipnew = ipnew + 1
                  locipn = Score + 2*(ipnew-1) + 1
                  IF ( ce(i+2)==0 ) THEN
                     Iiii = 1
                     Nnnn = 1
                     icode = Z(locipn)
                     CALL decode(icode,listn,ncn)
                     DO i1 = 1 , ncn
                        CALL pack(zero,Scr1,mcb)
                     ENDDO
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     ipold = ce(i+2)
                     locipo = ssil + 2*(ipold-1) + 1
                     icode = Z(locipn)
                     CALL decode(icode,listn,ncn)
                     icode = Z(locipo)
                     CALL decode(icode,listo,nco)
!
                     iaddh = ishptr + ipold - 1
                     idh = Z(iaddh)
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
                           CALL fwdrec(*40,Scr3)
                        ENDDO
                        CALL read(*40,*10,Scr3,t,37,1,nnn)
                        GOTO 10
                     ENDIF
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 10               DO i1 = 1 , idh
                     CALL bckrec(Scr3)
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
                     Iiii = Z(locipo-1)
                     Nnnn = Iiii + nrow - 1
                     CALL pack(colout,Scr1,mcb)
                  ENDDO
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 12               CALL close(Scconn,1)
                  CALL wrttrl(mcb)
                  CALL close(Scr1,1)
                  nam(1) = Combo(i,1)
                  nam(2) = Combo(i,2)
                  CALL mtrxo(Scr1,nam,ihead(1),Z(Buf4),itest)
                  CALL skpfil(Scr3,1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
         CALL close(Scsfil,1)
         CALL close(Scr3,1)
         CALL close(ioefil,1)
         Lcore = bufex + Buf2 - Buf3
         RETURN
!
 20      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         imsg = -8
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmhgen
