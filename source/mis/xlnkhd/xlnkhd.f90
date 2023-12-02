!*==xlnkhd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xlnkhd
!
!     THE PURPOSE OF XLNKHD IS TO GENERATE THE LINK HEADER SECTION FOR
!     AN OSCAR ENTRY
!
   IMPLICIT NONE
   USE C_AUTOHD
   USE C_SYSTEM
   USE C_XGPI2
   USE C_XGPI4
   USE C_XGPI5
   USE C_XGPI6
   USE C_XGPIC
   USE C_XGPID
   USE C_XMDMSK
   USE C_XOLDPT
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: and , or
   INTEGER :: i , j , k , loscar , mpler , osbot , ospnt , osprc
   INTEGER , DIMENSION(1) :: med , oscar
   INTEGER , DIMENSION(5) :: os
   INTEGER , SAVE :: xchk
   EXTERNAL andf , lshift , orf , rshift , xgpimw
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Oscar(1),Med(1),Os(5))
   DATA xchk/4HXCHK/
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   mpler = Mpl(Mplpnt+3)
   IF ( Ihead==1 ) mpler = 4
!
!     CHECK FOR DECLARATIVE INSTRUCTION
!
   IF ( Ihead/=1 ) THEN
      IF ( mpler/=5 ) THEN
!
!     UPDATE OSCAR PARAMETERS
!
         osprc = osbot
         osbot = oscar(osbot) + osbot
         ospnt = osbot
         Iseqn = oscar(osprc+1) + 1
!
!     LOAD LINK HEADER INFORMATION
!
         oscar(ospnt) = 6
         oscar(ospnt+1) = Iseqn
         oscar(ospnt+2) = mpler + lshift(Modidx,16)
         oscar(ospnt+3) = Dmap(Dmppnt)
         oscar(ospnt+4) = Dmap(Dmppnt+1)
         oscar(ospnt+5) = Dmpcnt
!
         Mplpnt = Mplpnt + 4
      ELSE
         ospnt = oscar(osbot) + osbot
      ENDIF
   ENDIF
   oscar(ospnt+5) = or(Isgnon,oscar(ospnt+5))
!
!     ALWAYS RAISE EXECUTE FLAG FOR COLD START RUNS
!
   IF ( Start/=Icst ) THEN
!
!     COMPARE SEQ NO. WITH REENTRY SEQ NO.
!
      IF ( Dmpcnt>=rshift(Seqno,16) ) THEN
!
!     WE ARE BEYOND REENTRY POINT - EXECUTE ALL MODULES HERE ON OUT.
!
         IF ( andf(Maskhi,Seqno)==0 .AND. mpler/=5 ) Seqno = or(Iseqn,and(Masklo,Seqno))
!
!     WE ARE BEFORE REENTRY POINT - CHECK APPROACH AND TYPE OF RESTART
!     ALWAYS RAISE EXECUTE FLAG FOR INSERT FOR MODIFIED RESTARTS.
!
      ELSEIF ( Insert==0 .OR. Start/=Imst ) THEN
         IF ( Start==Imst ) THEN
!
!     FOR RIGID FORMAT - CHECK DECISION TABLE FOR MODIFIED RESTART
!
            i = med(Medtp+1)
            DO j = 1 , i
               k = Medpnt + j - 1
               IF ( and(med(k),Medmsk(j))/=0 ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
            ENDDO
            oscar(ospnt+5) = and(Nosgn,oscar(ospnt+5))
         ELSE
!
!     LOWER EXECUTE FLAG FOR UNMODIFIED RESTART RUNS.
!
            oscar(ospnt+5) = and(Nosgn,oscar(ospnt+5))
            IF ( mpler==5 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            RETURN
         ENDIF
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      IF ( oscar(ospnt+3)==xchk .AND. Cpflg==0 ) oscar(ospnt+5) = and(Nosgn,oscar(ospnt+5))
      IF ( oscar(ospnt+5)>=0 .AND. mpler/=5 ) RETURN
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     PRINT COMPILE/EXECUTE FLAG FOR RESTART
!
      IF ( Start==Icst .OR. Ifirst==0 ) RETURN
      IF ( Dmpcnt==Iflag .AND. Insert==0 ) RETURN
      Iflag = Dmpcnt
      i = 7
      IF ( mpler==5 ) i = 10
      CALL xgpimw(i,0,0,0)
   END SUBROUTINE spag_block_2
END SUBROUTINE xlnkhd
