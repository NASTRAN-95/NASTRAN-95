!*==rcovim.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovim(Higher)
   USE c_blank
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_rcovcm
   USE c_rcovcr
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Higher
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , intyp , isk , item , ivec1 , ivec2 , j , k , lcorez , n , ncol , rc
   INTEGER , SAVE :: kmtx , mmtx , scr5 , scr6 , scr7 , scr8 , scr9 , uvec
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: rz
   REAL :: total
   EXTERNAL close , fwdrec , gopen , korsz , makmcb , mesage , mpyad , mtrxi , pack , rcovva , rdtrl , smsg , sofcls , sofopn ,     &
          & unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE CALCULATES THE ENERGIES ON THE MODAL COORDINATES
!     IN A SUBSTRUCTURE THAT WAS MODAL REDUCED.  IT WILL ALSO
!     CALCULATE THE TOTAL ENERGY FOR EACH COLUMN.
!
   !>>>>EQUIVALENCE (Z(1),Rz(1))
   DATA uvec , kmtx , mmtx/4HUVEC , 4HKMTX , 4HMMTX/
   DATA scr5 , scr6 , scr7 , scr8 , scr9/305 , 306 , 307 , 308 , 309/
   DATA name/4HRCOV , 4HIM  /
!
!     INITIALIZE
!
   lcorez = korsz(z)
   mpyz = lcorez
   tflag = 0
   signab = 1
   signc = 1
   mprec = 0
!
!     GET THE DISPLACEMENT VECTOR FOR THE HIGHER LEVEL REDUCED
!     SUBSTRUCTURE.
!
   item = uvec
   CALL mtrxi(scr5,Higher,uvec,0,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      CALL smsg(rc-2,item,Higher)
      iopt = -1
      RETURN
   ELSE
!
!     CALCULATE VELOCITIES IF NOT ALREADY DONE FOR THE OUTPUT PHASE.
!
      intyp = 1
      IF ( rfno==3 .OR. rfno==8 ) intyp = 0
      CALL rcovva(scr5,intyp,0,scr8,scr9,0,Higher,z(1),z(1),z(1))
      IF ( ua<=0 ) THEN
         iopt = -1
         RETURN
      ELSE
!
!     CALCULATE THE KENETIC ENERTY MULTIPLIER - M * V
!
         item = mmtx
         CALL mtrxi(scr5,Higher,mmtx,0,rc)
         IF ( rc/=1 ) THEN
            CALL smsg(rc-2,item,Higher)
            iopt = -1
            RETURN
         ELSE
            mcba(1) = scr5
            CALL rdtrl(mcba)
            mcbb(1) = scr9
            CALL rdtrl(mcbb)
            ncol = mcbb(2)
            mcbc(1) = 0
            CALL makmcb(mcbd,scr7,mcbb(3),rect,mcbb(5))
            scrm = scr6
            CALL sofcls
            CALL mpyad(z(1),z(1),z(1))
            CALL wrttrl(mcbd)
!
!     CALCULATE THE KENETIC ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL KINETIC ENERGY TO THE END OF EACH
!     COLUMN.
!
            itinu = rsp
            iru = 1
            nru = mcbd(3)
            incru = 1
            itinp = rsp
            itoutp = rsp
            irp = 1
            nrp = nru + 1
            incrp = 1
            ivec1 = 1
            ivec2 = ivec1 + nru + 1
            IF ( ivec2+nru+1>sof3 ) THEN
               n = 8
               CALL mesage(n,file,name)
               iopt = -1
               RETURN
            ELSE
!
               file = scr9
               CALL gopen(scr7,z(sof1),rdrew)
               CALL gopen(scr9,z(sof2),rdrew)
               CALL gopen(scr6,z(sof3),wrtrew)
               CALL makmcb(mcba,scr6,nrp,rect,rsp)
!
               DO i = 1 , ncol
                  spag_nextblock_1 = 1
                  SPAG_DispatchLoop_1: DO
                     SELECT CASE (spag_nextblock_1)
                     CASE (1)
                        isk = 1
                        CALL unpack(*2,scr7,rz(ivec1))
                        isk = 0
                        CALL unpack(*2,scr9,rz(ivec2))
!
                        total = 0.0
                        DO j = 1 , nru
                           k = j - 1
                           rz(ivec1+k) = rz(ivec1+k)*rz(ivec2+k)
                           total = total + rz(ivec1+k)
                        ENDDO
                        rz(ivec1+nru) = total
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
!
 2                      DO j = 1 , nrp
                           rz(ivec1+j-1) = 0.0
                        ENDDO
                        IF ( isk/=0 ) CALL fwdrec(*100,scr9)
                        spag_nextblock_1 = 2
                     CASE (2)
!
                        CALL pack(rz(ivec1),scr6,mcba)
                        EXIT SPAG_DispatchLoop_1
                     END SELECT
                  ENDDO SPAG_DispatchLoop_1
!
               ENDDO
!
               CALL close(scr7,rew)
               CALL close(scr9,rew)
               CALL close(scr6,rew)
               CALL wrttrl(mcba)
               CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     CALCULATE THE POTENTIAL ENERTY MULTPLYIER - K*U
!
               item = kmtx
               CALL mtrxi(scr5,Higher,kmtx,0,rc)
               IF ( rc/=1 ) THEN
                  CALL smsg(rc-2,item,Higher)
                  iopt = -1
                  RETURN
               ELSE
                  mcba(1) = scr5
                  CALL rdtrl(mcba)
                  mcbb(1) = scr8
                  CALL rdtrl(mcbb)
                  CALL makmcb(mcbd,scr9,mcbb(3),rect,mcbb(5))
                  scrm = scr7
                  CALL sofcls
                  CALL mpyad(z(1),z(1),z(1))
                  CALL wrttrl(mcbd)
!
!     CALCULATE THE POTENTIAL ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL POTENTIAL ENERGY TO THE END OF EACH
!     COLUMN.
!
                  itinu = rsp
                  iru = 1
                  nru = mcbd(3)
                  incru = 1
                  itinp = rsp
                  itoutp = rsp
                  irp = 1
                  nrp = nru + 1
                  incrp = 1
!
                  file = scr8
                  CALL gopen(scr9,z(sof1),rdrew)
                  CALL gopen(scr8,z(sof2),rdrew)
                  CALL gopen(scr7,z(sof3),wrtrew)
                  CALL makmcb(mcba,scr7,nrp,rect,rsp)
!
                  DO i = 1 , ncol
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           isk = 1
                           CALL unpack(*4,scr9,rz(ivec1))
                           isk = 0
                           CALL unpack(*4,scr8,rz(ivec2))
                           total = 0.0
                           DO j = 1 , nru
                              k = j - 1
                              rz(ivec1+k) = rz(ivec1+k)*rz(ivec2+k)
                              total = total + rz(ivec1+k)
                           ENDDO
                           rz(ivec1+nru) = total
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
!
 4                         DO j = 1 , nrp
                              rz(ivec1+j-1) = 0.0
                           ENDDO
                           IF ( isk/=0 ) CALL fwdrec(*100,scr8)
                           spag_nextblock_2 = 2
                        CASE (2)
!
                           CALL pack(rz(ivec1),scr7,mcba)
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
!
                  ENDDO
!
                  CALL close(scr9,rew)
                  CALL close(scr8,rew)
                  CALL close(scr7,rew)
                  CALL wrttrl(mcba)
!
!     NORMAL RETURN
!
                  CALL sofopn(z(sof1),z(sof2),z(sof3))
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  n = 2
   CALL mesage(n,file,name)
   iopt = -1
END SUBROUTINE rcovim
