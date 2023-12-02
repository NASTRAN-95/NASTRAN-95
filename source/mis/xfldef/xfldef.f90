!*==xfldef.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xfldef(Name1,Name2,Nofind)
   USE c_system
   USE c_two
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
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Name1
   INTEGER , DIMENSION(1) :: Name2
   INTEGER :: Nofind
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: and , or
   INTEGER , DIMENSION(1) :: fmed , fnm , oscar , ptdic
   INTEGER :: fmedtp , i , ii , index , iospnt , j , j1 , j2 , jj , k , k1 , l , loscar , n , nam1 , nam2 , osbot , ospnt , regen , &
            & reuse
   INTEGER , SAVE :: ifirst , nxchkp
   INTEGER , DIMENSION(5) :: os
   REAL :: osprc
   EXTERNAL andf , complf , orf , page1 , xgpidg , xgpimw
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF THIS ROUTINE IS TO TURN ON ALL OSCAR ENTRY EXECUTE
!     FLAGS NECESSARY TO DEFINE FILE .
!
!                 DESCRIPTION OF ARGUMENTS
!     NAM1,NAM2 = NAME OF FILE TO BE DEFINED.
!     NOFIND    = INDICATES TO CALLING PROGRAM WHETHER OR NOT FILE WAS
!                 FOUND.
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP CARD NAMES **
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Iospnt,Os(4)) , (Os(5),Oscar(1),Fnm(1),Fmed(1),Ptdic(1)) , &
!>>>>    & (Medtp,Fmedtp) , (Two(4),Reuse)
   DATA nxchkp/4HXCHK/ , ifirst/0/
!
   and(i,j) = andf(i,j)
   or(i,j) = orf(i,j)
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nam1 = Name1(1)
         nam2 = Name2(1)
!
!     SCAN OPTDIC FOR FILE NAME
!
         regen = Nofind
         Nofind = 1
         IF ( ptdbot>=ptdtp ) THEN
            DO ii = ptdtp , ptdbot , 3
               i = ptdbot + ptdtp - ii
               IF ( ptdic(i)==nam1 .AND. ptdic(i+1)==nam2 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     FILE NOT IN PTDIC - CHECK FNM TABLE IF RESTART IS MODIFIED AND
!     APPROACH IS NOT DMAP
!
         IF ( start/=icst .AND. iapp/=idmapp ) THEN
            IF ( regen>=0 ) THEN
               j = fnmtp + 1
               k = fnmtp + fnm(fnmtp)*3 - 2
               DO i = j , k , 3
                  IF ( nam1==fnm(i) .AND. nam2==fnm(i+1) ) GOTO 10
               ENDDO
            ENDIF
            RETURN
!
!     FILE IS IN FNM TABLE - CHECK FOR TABLE ERROR
!
 10         IF ( fnm(i+2)>0 ) THEN
!
!     CLEAR ALL THE MASK WORDS
!
               k = fmed(fmedtp+1)
               DO l = 1 , k
                  fmdmsk(l) = 0
               ENDDO
!
!     SET BIT IN FMDMSK FOR FILE REGENERATION
!
               l = ((fnm(i+2)-1)/31) + 1
               k = fnm(i+2) - 31*(l-1) + 1
               fmdmsk(l) = or(fmdmsk(l),two(k))
!
!     USE FMDMSK AND FMED TABLE TO TURN ON OSCAR EXECUTE FLAGS
!
               k = fmed(fmedtp+1)
               j1 = fmedtp + 2
               j2 = j1 + fmed(fmedtp)*fmed(fmedtp+1) - k
               index = 0
               ospnt = 1
               DO j = j1 , j2 , k
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        SPAG_Loop_4_1: DO k1 = 1 , k
                           jj = j + k1 - 1
                           IF ( and(fmed(jj),fmdmsk(k1))/=0 ) THEN
                              spag_nextblock_2 = 2
                              EXIT SPAG_Loop_4_1
                           ENDIF
                        ENDDO SPAG_Loop_4_1
                     CASE (2)
!
!     NON-ZERO ENTRY FOUND - COMPUTE DMAP SEQUENCE NUMBER FOR FMED ENTRY
!
                        n = ((j-j1)/k) + 1
                        IF ( and(oscar(iospnt+5),nosgn)<n ) RETURN
                        SPAG_Loop_2_1: DO
!
!     SET EXECUTINON FLAG FOR ALL OSCAR ENTRIES WITH SAME DMAP SEQ
!     NUMBER
!
                           IF ( and(oscar(ospnt+5),nosgn)<n ) THEN
                           ELSEIF ( and(oscar(ospnt+5),nosgn)==n ) THEN
                              IF ( .NOT.(oscar(ospnt+5)<0 .OR. (oscar(ospnt+3)==nxchkp .AND. icpflg==0)) ) THEN
                                 IF ( ifirst/=1 ) THEN
                                    ifirst = 1
                                    CALL page1
                                    CALL xgpimw(12,0,0,0)
                                 ENDIF
                                 IF ( index/=1 ) THEN
                                    index = 1
                                    CALL xgpimw(3,nam1,nam2,0)
                                 ENDIF
                                 CALL xgpimw(4,0,0,oscar(ospnt))
                                 Nofind = -1
                                 oscar(ospnt+5) = orf(oscar(ospnt+5),isgnon)
                              ENDIF
                           ELSE
                              EXIT SPAG_Loop_2_1
                           ENDIF
                           IF ( ospnt<osbot ) THEN
                              ospnt = ospnt + oscar(ospnt)
                              CYCLE
                           ENDIF
                           EXIT SPAG_Loop_2_1
                        ENDDO SPAG_Loop_2_1
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
!
!     MAKE SURE SOME MODULES WERE TURNED ON
!
               IF ( Nofind==-1 ) THEN
!
!     NEGATE FNM TABLE ENTRY FOR THIS FILE
!
                  fnm(i+2) = -fnm(i+2)
!
!     TURN OFF REUSE FLAGS IN PTDIC
!
                  IF ( ptdbot>ptdtp .AND. iflag==0 ) THEN
                     j = complf(reuse)
                     DO i = ptdtp , ptdbot , 3
                        ptdic(i+2) = andf(j,ptdic(i+2))
                     ENDDO
                  ENDIF
                  RETURN
               ENDIF
            ENDIF
!
!     D I A G N O S T I C    M E S S A G E S
!
!     MED OR FILE TABLE INCORRECT FOR REGENERATING FILE
!
            CALL xgpidg(41,nam1,nam2,fnm(i+2))
            Nofind = -1
            nogo = 2
         ENDIF
         RETURN
      CASE (2)
!
!     FILE IS IN PTDIC - SET REUSE FLAG FOR ALL EQUIVALENCED FILES
!
         IF ( ptdic(i+2)<0 ) THEN
            DO j = ptdtp , ptdbot , 3
               IF ( and(ptdic(j+2),noflgs)==and(ptdic(i+2),noflgs) ) ptdic(j+2) = or(ptdic(j+2),reuse)
            ENDDO
         ENDIF
         ptdic(i+2) = or(ptdic(i+2),reuse)
         Nofind = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xfldef
