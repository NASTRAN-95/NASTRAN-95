!*==/home/marcusmae/nasa/nastran/SPAGged/C_NAMES.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_NAMES
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER :: Cdp, Csp, Diag, Eofnrw, Ident, Lower, Norew, Rd, Rdp, Rdrew, Rect, Rew, Row, Rsp, Square, Sym, Upper,   &
            & Wrt, Wrtrew
!
!     -------------------     / NAMES  /     ---------------------------
!
!     NAMES DEFINES VALUES FOR GINO FILE OPTIONS,ARITHMETIC TYPES
!     AND MATRIX FORMS.
!
   DATA rd/2/ , rdrew/0/ , wrt/3/ , wrtrew/1/ , rew/1/ , norew/2/ , eofnrw/3/ , rsp/1/ , rdp/2/ , csp/3/ , cdp/4/ , square/1/ ,     &
      & rect/2/ , diag/3/ , lower/4/ , upper/5/ , sym/6/ , row/7/ , ident/8/

END MODULE C_NAMES
