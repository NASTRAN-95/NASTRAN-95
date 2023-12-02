!*==dplot.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dplot
!
   USE c_blank
   USE c_plothd
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf , i , i1 , i2 , oes1l , onrgy1
   INTEGER , SAVE :: inprew , rew
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(32) , SAVE :: tit
   EXTERNAL close , clstab , fread , gopen , korsz , mesage , param , read , totape , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     NOTE THAT NSETS IS DMAP PARAMETER JUMPPLOT
!     IUSED IS USED IN PLOT AND HDPLOT
!
   DATA inprew , rew/0 , 1/ , tit/12*1H  , 4HMESS , 4HAGES , 4H FRO , 4HM TH , 4HE PL , 4HOT M , 4HODUL , 1HE , 12*1H /
   DATA name/4HDPLO , 4HT   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FILE NAMES FOR UNDEFORMED PLOTS MAY BE
!     108  = USET (GPTLBL - SPC DEGREES OF FREEDOM)
!     109  = ECT  (ELELBL - PROPERTY IDS)
!     110  = ECPT
!          = EPT (UNDEFORMED PLOT ONLY, DMAP NUMBER 25 OR LESS)
!            EPT IS NEEDED FOR PSHELL CARDS IN ORDER TO PICK UP ANY
!            OFFSET FOR CTRIA3 AND CQUAD4 (IN COMECT)
!
         pltpar = 101
         gpsets = 102
         elsets = 103
         casecc = 104
         bgpdt = 105
         eqexin = 106
         sil = 107
         pdef1 = 108
         pdef2 = 109
         ecpt = 110
         oes1 = 111
         oes1l = 112
         onrgy1 = 113
         plotx = 201
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         nodef = 0
         IF ( ngp<=0 .OR. lsil<=0 ) RETURN
         CALL totape(2,x(1))
!
!     OUTPUT THE TITLE FOR MESSAGE FILE
!     THE LAST BUFFER IS BUFSIZ+1 FOR SUBROUTINE ELELBL
!
         buf = korsz(x) - 4*bufsiz
         IF ( buf-4*bufsiz<10 ) THEN
!
!     INSUFFICIENT CORE
!
            CALL mesage(-8,buf,name)
            nsets = -1
            pltflg = -1
            RETURN
         ELSEIF ( nsets>0 ) THEN
            CALL gopen(plotx,x(buf),rew)
!
!     COMMENTS FROM G.CHAN/UNISYS       11/90
!     NEXT 2 LINES ADD TIT HEADING TO THE 4TH LINE OF NASTRAN HEADERS
!     WHEN THE PLOTX FILE IS READ AND PRINTED BY PRTMSG MODULE.
!     THIS SHORTCUT TECHNIQUE IS NO WHERE DISCUSSED IN THE USER'S NOR
!     PROGRAMMER'S MAUNALS
!
            CALL write(plotx,-4,1,0)
            CALL write(plotx,tit,32,0)
!
!     READ THE SETID-S FROM -GPSETS- FILE.  SET NEGATIVE SETID-S THAT
!     HAVE NO ASSOCIATED GRIDS.  FIND FIRST DEFINED SET OR EXIT IF NONE
!
            buf = buf - bufsiz
            CALL gopen(gpsets,x(buf),inprew)
            CALL fread(gpsets,x,nsets,1)
            setd = 0
            x(nsets+1) = 1
!
            DO i = 1 , nsets
               CALL read(*5,*20,gpsets,x(nsets+2),1,1,i1)
               IF ( x(nsets+2)>0 ) THEN
                  IF ( setd==0 ) setd = i
                  CYCLE
               ENDIF
 5             WRITE (nout,99001) uwm , x(nsets+1)
99001          FORMAT (A25,' 697, SET',I9,' NOT DEFINED.  FIRST SET DEFINED WILL BE USED.')
               x(i) = -x(i)
            ENDDO
            CALL close(gpsets,rew)
            IF ( setd/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 20      WRITE (nout,99002) ufm
99002    FORMAT (A23,' 698, NO SETS DEFINED FOR PLOTS')
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     PROCESS PLOT REQUESTS
!
         CALL gopen(pltpar,x(buf),inprew)
         i1 = 1
         i2 = i1 + nsets
         buf = buf - bufsiz
         CALL param(x(i1),x(i2),buf-nsets)
         CALL close(pltpar,rew)
!
!     SET JUMPPLOT NEGATIVE IF NO FUTHER REQUESTS
!
         IF ( pltflg>=0 .AND. nodef==0 ) nsets = -1
         CALL clstab(plotx,rew)
         CALL close(gpsets,rew)
         pltflg = -1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dplot
