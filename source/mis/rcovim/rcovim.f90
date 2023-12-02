!*==rcovim.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovim(Higher)
   IMPLICIT NONE
   USE C_BLANK
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_UNPAKX
   USE C_ZZZZZZ
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
   lcorez = korsz(Z)
   Mpyz = lcorez
   Tflag = 0
   Signab = 1
   Signc = 1
   Mprec = 0
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
      Iopt = -1
      RETURN
   ELSE
!
!     CALCULATE VELOCITIES IF NOT ALREADY DONE FOR THE OUTPUT PHASE.
!
      intyp = 1
      IF ( Rfno==3 .OR. Rfno==8 ) intyp = 0
      CALL rcovva(scr5,intyp,0,scr8,scr9,0,Higher,Z(1),Z(1),Z(1))
      IF ( Ua<=0 ) THEN
         Iopt = -1
         RETURN
      ELSE
!
!     CALCULATE THE KENETIC ENERTY MULTIPLIER - M * V
!
         item = mmtx
         CALL mtrxi(scr5,Higher,mmtx,0,rc)
         IF ( rc/=1 ) THEN
            CALL smsg(rc-2,item,Higher)
            Iopt = -1
            RETURN
         ELSE
            Mcba(1) = scr5
            CALL rdtrl(Mcba)
            Mcbb(1) = scr9
            CALL rdtrl(Mcbb)
            ncol = Mcbb(2)
            Mcbc(1) = 0
            CALL makmcb(Mcbd,scr7,Mcbb(3),Rect,Mcbb(5))
            Scrm = scr6
            CALL sofcls
            CALL mpyad(Z(1),Z(1),Z(1))
            CALL wrttrl(Mcbd)
!
!     CALCULATE THE KENETIC ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL KINETIC ENERGY TO THE END OF EACH
!     COLUMN.
!
            Itinu = Rsp
            Iru = 1
            Nru = Mcbd(3)
            Incru = 1
            Itinp = Rsp
            Itoutp = Rsp
            Irp = 1
            Nrp = Nru + 1
            Incrp = 1
            ivec1 = 1
            ivec2 = ivec1 + Nru + 1
            IF ( ivec2+Nru+1>Sof3 ) THEN
               n = 8
               CALL mesage(n,file,name)
               Iopt = -1
               RETURN
            ELSE
!
               file = scr9
               CALL gopen(scr7,Z(Sof1),Rdrew)
               CALL gopen(scr9,Z(Sof2),Rdrew)
               CALL gopen(scr6,Z(Sof3),Wrtrew)
               CALL makmcb(Mcba,scr6,Nrp,Rect,Rsp)
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
                        DO j = 1 , Nru
                           k = j - 1
                           rz(ivec1+k) = rz(ivec1+k)*rz(ivec2+k)
                           total = total + rz(ivec1+k)
                        ENDDO
                        rz(ivec1+Nru) = total
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
!
 2                      DO j = 1 , Nrp
                           rz(ivec1+j-1) = 0.0
                        ENDDO
                        IF ( isk/=0 ) CALL fwdrec(*100,scr9)
                        spag_nextblock_1 = 2
                     CASE (2)
!
                        CALL pack(rz(ivec1),scr6,Mcba)
                        EXIT SPAG_DispatchLoop_1
                     END SELECT
                  ENDDO SPAG_DispatchLoop_1
!
               ENDDO
!
               CALL close(scr7,Rew)
               CALL close(scr9,Rew)
               CALL close(scr6,Rew)
               CALL wrttrl(Mcba)
               CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     CALCULATE THE POTENTIAL ENERTY MULTPLYIER - K*U
!
               item = kmtx
               CALL mtrxi(scr5,Higher,kmtx,0,rc)
               IF ( rc/=1 ) THEN
                  CALL smsg(rc-2,item,Higher)
                  Iopt = -1
                  RETURN
               ELSE
                  Mcba(1) = scr5
                  CALL rdtrl(Mcba)
                  Mcbb(1) = scr8
                  CALL rdtrl(Mcbb)
                  CALL makmcb(Mcbd,scr9,Mcbb(3),Rect,Mcbb(5))
                  Scrm = scr7
                  CALL sofcls
                  CALL mpyad(Z(1),Z(1),Z(1))
                  CALL wrttrl(Mcbd)
!
!     CALCULATE THE POTENTIAL ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL POTENTIAL ENERGY TO THE END OF EACH
!     COLUMN.
!
                  Itinu = Rsp
                  Iru = 1
                  Nru = Mcbd(3)
                  Incru = 1
                  Itinp = Rsp
                  Itoutp = Rsp
                  Irp = 1
                  Nrp = Nru + 1
                  Incrp = 1
!
                  file = scr8
                  CALL gopen(scr9,Z(Sof1),Rdrew)
                  CALL gopen(scr8,Z(Sof2),Rdrew)
                  CALL gopen(scr7,Z(Sof3),Wrtrew)
                  CALL makmcb(Mcba,scr7,Nrp,Rect,Rsp)
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
                           DO j = 1 , Nru
                              k = j - 1
                              rz(ivec1+k) = rz(ivec1+k)*rz(ivec2+k)
                              total = total + rz(ivec1+k)
                           ENDDO
                           rz(ivec1+Nru) = total
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
!
 4                         DO j = 1 , Nrp
                              rz(ivec1+j-1) = 0.0
                           ENDDO
                           IF ( isk/=0 ) CALL fwdrec(*100,scr8)
                           spag_nextblock_2 = 2
                        CASE (2)
!
                           CALL pack(rz(ivec1),scr7,Mcba)
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
!
                  ENDDO
!
                  CALL close(scr9,Rew)
                  CALL close(scr8,Rew)
                  CALL close(scr7,Rew)
                  CALL wrttrl(Mcba)
!
!     NORMAL RETURN
!
                  CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  n = 2
   CALL mesage(n,file,name)
   Iopt = -1
END SUBROUTINE rcovim
