!*==xlnkhd.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xlnkhd
!
!     THE PURPOSE OF XLNKHD IS TO GENERATE THE LINK HEADER SECTION FOR
!     AN OSCAR ENTRY
!
   USE c_autohd
   USE c_system
   USE c_xgpi2
   USE c_xgpi4
   USE c_xgpi5
   USE c_xgpi6
   USE c_xgpic
   USE c_xgpid
   USE c_xmdmsk
   USE c_xoldpt
   USE c_zzzzzz
   IMPLICIT NONE
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
   mpler = mpl(mplpnt+3)
   IF ( ihead==1 ) mpler = 4
!
!     CHECK FOR DECLARATIVE INSTRUCTION
!
   IF ( ihead/=1 ) THEN
      IF ( mpler/=5 ) THEN
!
!     UPDATE OSCAR PARAMETERS
!
         osprc = osbot
         osbot = oscar(osbot) + osbot
         ospnt = osbot
         iseqn = oscar(osprc+1) + 1
!
!     LOAD LINK HEADER INFORMATION
!
         oscar(ospnt) = 6
         oscar(ospnt+1) = iseqn
         oscar(ospnt+2) = mpler + lshift(modidx,16)
         oscar(ospnt+3) = dmap(dmppnt)
         oscar(ospnt+4) = dmap(dmppnt+1)
         oscar(ospnt+5) = dmpcnt
!
         mplpnt = mplpnt + 4
      ELSE
         ospnt = oscar(osbot) + osbot
      ENDIF
   ENDIF
   oscar(ospnt+5) = or(isgnon,oscar(ospnt+5))
!
!     ALWAYS RAISE EXECUTE FLAG FOR COLD START RUNS
!
   IF ( start/=icst ) THEN
!
!     COMPARE SEQ NO. WITH REENTRY SEQ NO.
!
      IF ( dmpcnt>=rshift(seqno,16) ) THEN
!
!     WE ARE BEYOND REENTRY POINT - EXECUTE ALL MODULES HERE ON OUT.
!
         IF ( andf(maskhi,seqno)==0 .AND. mpler/=5 ) seqno = or(iseqn,and(masklo,seqno))
!
!     WE ARE BEFORE REENTRY POINT - CHECK APPROACH AND TYPE OF RESTART
!     ALWAYS RAISE EXECUTE FLAG FOR INSERT FOR MODIFIED RESTARTS.
!
      ELSEIF ( insert==0 .OR. start/=imst ) THEN
         IF ( start==imst ) THEN
!
!     FOR RIGID FORMAT - CHECK DECISION TABLE FOR MODIFIED RESTART
!
            i = med(medtp+1)
            DO j = 1 , i
               k = medpnt + j - 1
               IF ( and(med(k),medmsk(j))/=0 ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
            ENDDO
            oscar(ospnt+5) = and(nosgn,oscar(ospnt+5))
         ELSE
!
!     LOWER EXECUTE FLAG FOR UNMODIFIED RESTART RUNS.
!
            oscar(ospnt+5) = and(nosgn,oscar(ospnt+5))
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
      IF ( Oscar(Ospnt+3)==Xchk .AND. cpflg==0 ) Oscar(Ospnt+5) = and(Nosgn,Oscar(Ospnt+5))
      IF ( Oscar(Ospnt+5)>=0 .AND. Mpler/=5 ) RETURN
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     PRINT COMPILE/EXECUTE FLAG FOR RESTART
!
      IF ( Start==Icst .OR. ifirst==0 ) RETURN
      IF ( Dmpcnt==iflag .AND. Insert==0 ) RETURN
      iflag = Dmpcnt
      I = 7
      IF ( Mpler==5 ) I = 10
      CALL xgpimw(I,0,0,0)
   END SUBROUTINE spag_block_2
END SUBROUTINE xlnkhd
