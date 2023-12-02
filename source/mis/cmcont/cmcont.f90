!*==cmcont.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmcont
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmbfnd
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER , DIMENSION(6) :: dof , ip
   INTEGER :: i , iclen , icomp , icor , ifile , igrid , ii2 , imsg , isave , j , jj , mfile , ncsub , nip , nnn , nwd , ofile
   INTEGER , DIMENSION(9) :: ii , io
   INTEGER , DIMENSION(100) :: ilen , istrt
   LOGICAL :: odd
   EXTERNAL andf , close , gridip , lshift , mesage , open , read , rshift , skpfil , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE DEFINES THE CONNECTION ENTRIES IN TERMS OF IP
!     NUMBERS.
!
   DATA aaa/4HCMCO , 4HNT  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         icor = score
         iclen = lcore
         mfile = scsfil
         CALL open(*20,scsfil,z(buf3),0)
         ofile = scr2
         ifile = scr1
         nwd = 2 + npsub
         odd = .FALSE.
!
         DO i = 1 , npsub
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  odd = .NOT.odd
                  ncsub = combo(i,5)
!
!     READ IN EQSS FOR ITH PSEUDO-STRUCTURE
!
                  mfile = ifile
                  CALL open(*20,ifile,z(buf1),0)
                  mfile = ofile
                  CALL open(*20,ofile,z(buf2),1)
!
!     MOVE TO FIRST COMPONENT EQSS
!
                  DO j = 1 , ncsub
                     mfile = scsfil
                     CALL read(*40,*2,scsfil,z(score),lcore,1,nnn)
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
 2                   istrt(j) = score
                     ilen(j) = nnn
                     score = score + nnn
                     lcore = lcore - nnn
                  ENDDO
                  CALL skpfil(scsfil,1)
!
!     CONNECTION ENTRIES IN TERMS OF GRID POINT ID ARE ON SCR1
!     IN THE FORM...
!        C/CC/G1/G2/G3/G4/G5/G6/G7
!
!     READ CONNECTION ENTRY..
!
                  mfile = ifile
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL read(*6,*4,ifile,ii,10,1,nnn)
 4                icomp = ii(2+i)/1000000
                  igrid = ii(2+i) - 1000000*icomp
                  IF ( igrid==0 ) THEN
                     CALL write(ofile,ii,nwd,1)
                  ELSE
!
!     THE ABOVE RETRIEVED THE ORIGINAL GRID PT. NO., NOW FIND OUT
!     IF IT HAS SEVERAL IP NO.
!
                     IF ( ilen(icomp)/=0 ) THEN
                        CALL gridip(igrid,istrt(icomp),ilen(icomp),ip,dof,nip,z,nnn)
                        IF ( ierr/=1 ) THEN
                           DO j = 1 , nip
                              ii2 = rshift(dof(j),26)
                              ii2 = lshift(ii2,26)
                              dof(j) = dof(j) - ii2
                              io(1) = andf(ii(1),dof(j))
                              IF ( io(1)/=0 ) THEN
                                 io(2) = ii(2)
                                 DO jj = 1 , nwd
                                    io(2+jj) = ii(2+jj)
                                 ENDDO
                                 io(2+i) = ip(j)
                                 CALL write(ofile,io,nwd,1)
                              ENDIF
                           ENDDO
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     idry = -2
                     WRITE (outt,99001) ufm , igrid , combo(i,1) , combo(i,2) , icomp
99001                FORMAT (A23,' 6535, A MANUAL CONNECTION SPECIFIES GRID ID ',I8,' OF PSEUDOSTRUCTURE ',2A4,/30X,                &
                            &'COMPONENT STRUCTURE,I4,22H WHICH DOES NOT EXIST.')
                  ENDIF
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 6                CALL close(ifile,1)
                  IF ( i==npsub ) CALL close(ofile,2)
                  IF ( i<npsub ) CALL close(ofile,1)
                  isave = ifile
                  ifile = ofile
                  ofile = isave
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         scconn = scr1
         IF ( odd ) scconn = scr2
         IF ( scconn==scr1 ) scr1 = 305
         IF ( scconn==scr2 ) scr2 = 305
         score = icor
         lcore = iclen
         CALL close(scsfil,1)
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
         CALL mesage(imsg,mfile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmcont
