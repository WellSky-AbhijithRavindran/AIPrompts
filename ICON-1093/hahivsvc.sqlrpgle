      /copy copysrc,hxxcntrlws
      **************************************************************************
      **                                                                      **
      **   Web Service - Insurance Verification by Service                    **
      **                 CRUD actions from maintenance window                 **
      **                                                                      **
      **     Copyright - 2025 - WellSky                                       **
      **                                                                      **
      **************************************************************************
      **                                                                      **
      **  Web Service PCML Parameter Setup:                                   **
      **                                                                      **
      **    Parameter    Usage        Counter     Description                 **
      **    =========    ==========   ==========  ========================    **
      **    userid       input                    User ID                     **
      **    passwd       input                    Encrypted Password          **
      **    option       input                    Requested Option            **
      **                                          B = Build Grid              **
      **                                          A = Add Verification        **
      **                                          AA = Add Restriction        **
      **                                          C = Change Verification     **
      **                                          CC = Change Restriction     **
      **                                          Y = Copy Verification       **
      **                                          D = Delete Verification     **
      **                                          DD = Delete Restriction     **
      **                                          R = Reinstate Verification  **
      **                                          RR = Reinstate Restriction  **
      **    request      input                    Requested Parameters        **
      **    changeOK     input/output             Change OK flag              **
      **    gridCount    input/output             # of grid rows returned     **
      **    grid         input/output gridCount   Grid data structure         **
      **    restCount    input/output             # of grid rows returned     **
      **    restrictions input/output restCount   Grid data structure         **
      **    infoOnlyAuth output                   IVSVC Info Only Authority   **
      **    errorCount   output                   # of errors returned        **
      **    errors       output       errorCount  Return Errors DS            **
      **    warnMsg      output                   Warning message about update**
      **    WRMerror     output                   WRM error from API call     **
      **    rtrncd       output                   Return code                 **
      **                                                                      **
      **************************************************************************
      **                                                                      **
      **  change date  user/comments                    install date          **
      **                                                                      **
      **  09/26/2014   Mike Haston                                            **
      **              -New Program                                            **
      **                                                                      **
      **  02/11/2015   Chris Shull                                            **
      **              -allow end date to be passed in as 0 and treat as       **
      **               999999999                                              **
      **                                                                      **
      **  08/07/2015   Alex Hansen  GOL-629                                   **
      **              -allow verification by service to be set up by          **
      **               revenue code                                           **
      **              -check for valid rate types                             **
      **               - Installed by Michael Kundla                          **
      **  08/19/2015  -fix edit check fields                                  **
      **               - Installed by Michael Kundla                          **
      **                                                                      **
      **  01/14/2019   Tom Gorman  HDEV-28069                                 **
      **              - Change wscmnt from 50 to 250 characters               **
      **                                                                      **
      **  11/15/2019   Alex Price       HDEV-27742                            **
      **              - Added parms to xfxedits                               **
      **                                                                      **
      **  09/30/2020    Michelle Palmieri  HDEV-39247                         **
      **                - increase reimbursement type field size              **
      **                                                                      **
      **  10/14/2022    Ed Kidhardt  IBILL-882                                **
      **               -added logic to check for pending allowance postings   **
      **                                                                      **
      **  06/12/2024    Saikiran Parupalli  ICON-210                          **
      **               -added logic to check for INSVCAUT bill edit           **
      **                                                                      **
      **  06/27/2024    Abhijith Ravindran  ICON-315                          **
      **               -Added logic to include effective and end dates when   **
      **                adding or updating period restrictions.               **
      **                                                                      **
      **                                                                      **
      **  07/17/2024    Abhijith Ravindran ICON-349                           **
      **                - Add IPC Year (I) and Biweekly (B) to the period     **
      **                  restriction.                                        **
      **                                                                      **
      **  08/06/2024    Abhijith Ravindran ICON-375                           **
      **                - Removed validation for group name in verification   **
      **                  by service.                                         **
      **                                                                      **
      **  09/16/2024    Abhijith Ravindran ICON-386                           **
      **                - Added temporary auth code with Authorization levels **
      **                                                                      **
      **  10/17/2024    Abhijith Ravindran ICON-229                           **
      **                - Added logic to save and retrieve Supplemental units **
      **                  and amount.                                         **
      **                - Added leave to avoid duplicate errors on '00003830' **
      **                                                                      **
      **  11/06/2024    Abhijith Ravindran ICON-473                           **
      **                - Added 3 new disciplines as paraneters for the       **
      **                  service verifications.                              **
      **                                                                      **
      **  11/14/2024    SKosuri ICON-527                                      **
      **                - Fixed bug to check for overlaping period restriction**
      **                  dates                                               **
      **                - Fixed bug to check for overlaping service verification
      **                  dates                                               **
      **                                                                      **
      **  11/14/2024    Camila Barbini ICON-498                               **
      **                - Send insurance verification information to the      **
      **                  resource manager API integration                    **
      **                                                                      **
      **  11/22/2024    Abhijith Ravindran ICON-549                           **
      **                - Added authorization parameter for service           **
      **                  verifications.                                      **
      **                                                                      **
      **  12/06/2024    Saikiran Parupalli  ICON-560                          **
      **               - Added warning message for the INSVCAUT bill edit     **
      **                 is Optional                                          **
      **                                                                      **
      **  12/10/2024    SKosuri ICON-559                                      **
      **               - Opened reimbursement type validation to not check    **
      **                 for procedure                                        **
      **                                                                      **
      **  01/03/2025    Abhijith Ravindran ICON-510                           **
      **               - Dollars (Ivsvc) reimbursement type implementation    **
      **                                                                      **
      **  01/10/2025    SKosuri ICON-604                                      **
      **               - IVSVC quantities should not exceed group header qty  **
      **                                                                      **
      **  01/13/2025    Abhijith Ravindran ICON-543                           **
      **                - Added authorization parameter for new service       **
      **                  verifications.                                      **
      **                                                                      **
      **  01/21/2025    SKosuri ICON-606                                      **
      **               - fixed bug on delete/reinstate of Day Period          **
      **                 restriction                                          **
      **                                                                      **
      **  02/11/2025    Abhijith Ravindran ICON-660                           **
      **                - Save Conversion Factor Quantity for IVSVC Period    **
      **                  Restrictions.                                       **
      **                                                                      **
      **  02/18/2025  Saikiran Parupalli  ICON-684                            **
      **              - Updated Revenue code mappings for disciplines BH, CM, **
      **                SW and VC                                             **
      **                                                                      **
      **  02/19/2025  Alex Price          ICON-601                            **
      **              - Fixed issue with Fiscal Year period restriction       **
      **                error not returning the correct field name            **
      **                                                                      **
      **  02/21/2025    Camila Barbini ICON-498                               **
      **                - Send insurance verification information to the      **
      **                  resource manager API integration only for           **
      **                  type Visits or type Services with proc code (not rev)*
      **                - Send request to delete insurance verification info  **
      **                                                                      **
      **  05/19/2025    Alex Price     ICON-880                               **
      **                - Added Info Only field to HAPIVSVC                   **
      **                                                                      **
      **  05/23/2025    Saikiran Parupalli  ICON-829                          **
      **                - Fixed the error message for zero units on Dollar    **
      **                  Reimbursement type                                  **
      **                                                                      **
      **  06/05/2025    Abhijith Ravindran ICON-881                           **
      **                - Added "IVSVC Info Only Authorization"               **
      **                                                                      **
      **  08/01/2025    Camila Barbini ICEN-3149                              **
      **               - Increase authNum field size                          **
      **                                                                      **
      **  08/13/2025    Abhijith Ravindran ICON-1093                          **
      **               - Service Verification Maintenance changes to validate **
      **                 the group header type and modify the quantity        **
      **                 validations.                                         **
      **                                                                      **
      **************************************************************************
      ** File declarations                                                    **
      **************************************************************************
     fhmpmast   if   e           k disk    usropn
     fhxpbnfit  if   e           k disk    usropn
     fhalirnks  if   e           k disk    usropn
     fhapdemo   if   e           k disk    usropn
     fhapdemoi  if   e           k disk    usropn
     fhxlwinh   if   e           k disk    usropn
     fhxpprocm  if   e           k disk    usropn
     fhxpprocc  if   e           k disk    usropn
     fhxlprcca  if   e           k disk    rename(xffprocc:xffproccd) usropn
     fhbpbrev   if   e           k disk    usropn
     fhxpctg1   if   e           k disk    usropn
     fhbpcrate  if   e           k disk    usropn
     fhmpmams   if   e           k disk    usropn
     fhxpl6shf  if   e           k disk    usropn
     fhbppdiem  if   e           k disk    usropn
     fhapivsvc  uf a e           k disk    usropn
     fhalivsvr  if   e           k disk    rename(hafivsvc:hafivsvr)
     f                                     prefix(t:1) usropn
     fhalivsvrr if   e           k disk    rename(hafivsvc:hafivsvrr)
     f                                     prefix(t:1) usropn
     fhalivsvs  uf   e           k disk    rename(hafivsvc:hafivsvs)
     f                                     prefix(br:2) usropn
     fhalivsvu  if   e           k disk    rename(hafivsvc:hafivsvu)
     f                                     prefix(i:1) usropn
     fhxpinex   if   e           k disk    usropn
     fhxpinexr  if   e           k disk    usropn
     fhaltrno7  uf a e           k disk    rename(haftrno:haftrno7) usropn
     fhapivsp   uf a e           k disk    usropn
     fhalivsp   if   e           k disk    rename(hafivsp:hafivspd) usropn
     fhulapptz  uf   e           k disk    usropn
     fhupadtl   if   e           k disk    usropn
     fhblchrga  if   e           k disk    usropn
     fhbpchgtr  if   e           k disk    usropn
     fhalivstr  if   e           k disk    rename(hafivstr:hafivstx)
     f                                     prefix(bx:2) usropn
     fhapivstr  uf a e           k disk    usropn
     fhapivsgr  if   e           k disk    usropn
     fhalivsgrn if   e           k disk    rename(hafivsgr:hafivsgg)
     f                                     prefix(gg:2) usropn
     ferlerespr uf   e           k disk    usropn
     fhbpfschd  if   e           k disk    usropn

      **************************************************************************
      ** External Procedures - Global                                         **
      **************************************************************************
      *------------------------------------------------------------------------*
      *   Check authority                                                      *
      *------------------------------------------------------------------------*
     d xfxwsaut        pr                  extpgm('XFXWSAUT')
     d  userid                       10
     d  passwd                      100
     d  rtncod                        8
      *------------------------------------------------------------------------*
      *   User authorized to level                                             *
      *------------------------------------------------------------------------*
     d xfxauth         pr                  extpgm('XFXAUTH')
     d  chkType                       1
     d  chkNumber                     6  0
     d  ldaType                       1
     d  userMap                       6  0
     d  ldaUser                      10
     d  rtncod                        1
      *------------------------------------------------------------------------*
      *   User authorized to option                                            *
      *------------------------------------------------------------------------*
     d xfcnhkat        pr                  extpgm('XFCNHKAT')
     d  number                        7  0 const
     d  rtncod                        1
      *------------------------------------------------------------------------*
      *   Convert units                                                        *
      *------------------------------------------------------------------------*
     d xfxcvtim        pr                  extpgm('XFXCVTIM')
     d  quantity                      7  2
     d  convertCode                   2    const
     d  convertType                   2    const
     d  rtncod                        1
      *------------------------------------------------------------------------*
      *   Convert and validate mdate                                           *
      *------------------------------------------------------------------------*
     d xfxcmdy         pr                  extpgm('XFXCMDY')
     d  mdate                         8  0 const
     d  ydate                         8  0 const
      *------------------------------------------------------------------------*
      *   Convert and validate ydate                                           *
      *------------------------------------------------------------------------*
     d xfxcymd         pr                  extpgm('XFXCYMD')
     d  ydate                         8  0 const
     d  mdate                         8  0 const
      *------------------------------------------------------------------------*
     d xfxwsexit       pr                  extpgm('XFXWSEXIT')
      *------------------------------------------------------------------------*
      *   Get the Payor/Plan contract variables                                *
      *------------------------------------------------------------------------*
     d xfxppd          pr                  extpgm('XFXPPD')
     d  reqLvl6                       6  0 const
     d  reqAccount                   12  0 const
     d  reqFromDate                   8  0 const
     d  reqPayor                      6  0 const
     d  reqPlan                       5  0 const
     d  rtnppdDs                           like(ppdds)
      **************************************************************************
      ** Subprocedure Definitions - Local                                     **
      **************************************************************************
      *------------------------------------------------------------------------*
      *   Add error message to data structure array                            *
      *------------------------------------------------------------------------*
     d AddErrorMsg     pr
     d  fldname                      25    const
     d  errcode                       8    const
      *------------------------------------------------------------------------*
      *  Validate time field and add error message if invalid                  *
      *  Uses AddErrorMsg() procedure                                          *
      *------------------------------------------------------------------------*
     d ValidTime       pr              n
     d  name                         25    const
     d  value                         4  0 const
      *------------------------------------------------------------------------*
      *  Validate time range and add error message if invalid                  *
      *  Uses ValidTime() and AddErrorMsg() procedures                         *
      *------------------------------------------------------------------------*
     d ValidTimeRange...
     d                 pr              n
     d  nameFrom                     25    const
     d  valueFrom                     4  0 const
     d  nameTo                       25    const
     d  valueTo                       4  0 const
       //-----------------------------------------------------------------  hbbalwchb prototype
       dcl-pr HBBALWCHKI extpgm('HBBALWCHKI');
         iLevel6    packed( 6: 0) const;
         iAccount   packed(12: 0) const;
         iInsSeq    packed( 4: 0) const;
         oAlwPend   char  ( 1);
       end-pr;
       dcl-s alwPending char(1);

      **************************************************************************
      ** Parameters                                                           **
      **************************************************************************
     d  userid         s             10
     d*info|use:input|req:Y|
     d*cmnt|comment:This field is used to send in the users id|
     d  passwd         s            100
     d*info|use:input|req:Y|
     d*cmnt|comment:This field is used to send in the users password|
     d  option         s              2
     d*info|use:input|jType:S|req:Y|
     d*cmnt+|comment:Request option, 'B' to Build Grids, |
     d*cmnt+|comment:'A' to Add Procedure,|
     d*cmnt+|comment:'C' to Change Procdure,|
     d*cmnt+|comment:'Y' to Copy Procdure,|
     d*cmnt+|comment:'D' to Delete Procedure,|
     d*cmnt+|comment:'R' to Reinstate Procedure,|
     d*cmnt+|comment:'AA' to Add Period Restriction,|
     d*cmnt+|comment:'CC' to Change Period Restriction,|
     d*cmnt+|comment:'DD' to Delete Period Restriction,|
     d*cmnt|comment:'RR' to Reinstate Period Restriction|
     d  changeOk       s               n
     d*info|use:input/output|req:Y|
     d*cmnt+|comment:Proceed With Changes Flag, |
     d*cmnt+|comment:Input: Default to '0', pass '1' to proceed. |
     d*cmnt|comment:Output: Updates needs to be okay'd when warnMsg is not blank|
     d  gridCount      s             10i 0
     d*info|use:output|
     d*cmnt|comment:Return counter for procedures data structure|
     d  grid           ds                  likeds(gridDS) dim(999)
     d*info|use:input/output|counter:gridCount|req:Y|
     d*cmnt|comment: Data structure array of procedures|
     d  restCount      s             10i 0
     d*info|use:output|
     d*cmnt|comment:Return counter for procedures data structure|
     d  restrictions   ds                  likeds(restrictionsDS) dim(999)
     d*info|use:input/output|counter:restCount|req:Y|
     d*cmnt|comment: Data structure array of procedures|
     d infoOnlyAuth    s              1
     d*info|use:output|
     d*cmnt|comment: Info Only Checkbox Authority: Y=Default Info Only to Checked|
     d  warnMsg        s            150
     d*info|use:output|
     d*cmnt|comment:Warning message to update associated records.|
     d  WRMerror       s            200
     d*info|use:output|
     d*cmnt|comment:WRM error message from API Call.|
     d  rtrncd         s              8
     d*info|use:output|jType:S|
     d*cmnt|comment:This field is output and contains the response code (blank if no errors).|
     d  wsGroupType    s              1
     d*cmnt|comment:This field holds the group header type|

      **************************************************************************
      ** Data Structure Definitions //                                        **
      **************************************************************************
     d errorsds        ds                  qualified
     d                                     based(Template)
     d  fldname                      25
     d*info|use:|jType:S|req:N|
     d*cmnt|comment:This field is used to reference the field that caused an error|
     d  errcode                       8
     d*info|use:|jType:S|req:N|
     d*cmnt|comment:This field holds the error code that represents the error found|
     d errors          ds                  likeds(errorsds) dim(9999)
     d*info|use:output|counter:errorCount|jType:|
     d*cmnt|comment:Errors data structure|

     d request         ds
     d*info|use:input|req:Y|
     d*cmnt|comment:Request grid data structure|
     d lvl6                           6  0
     d*cmnt|comment:This field holds the level|
     d account                       12  0
     d*cmnt|comment:This field holds the account number|
     d medRcd                         9  0
     d*cmnt|comment:This field holds the medical record number|
     d payor                          6  0
     d*cmnt|comment:This field holds the payor|
     d plan                           5  0
     d*cmnt|comment:This field holds the plan|
     d policy                        20
     d*cmnt|comment:This field holds the policy|
     d fileSeq                        5  0
     d*cmnt|comment:This field holds the file sequence number|
     d rank                           4  0
     d*cmnt|comment:This field holds the rank sequence number|
     d proc                           8
     d*cmnt|comment:This field holds the procedure code|
     d maxUnits                       5  0
     d*cmnt|comment: Requested max units|
     d effDt                          8  0
     d*info|use:input|jType:Y|
     d*cmnt|comment: Requested effective date|
     d expDt                          8  0
     d*info|use:input|jType:Y|
     d*cmnt|comment: Requested expiration date|
     d showHistory                    1
     d*cmnt+|comment: Show History toggle checkbox|
     d*cmnt+|comment: set to '1' to show all procedures regardless end date|
     d*cmnt|comment: set to '0' to show only 'active' procedures whose end date >= today|

     d gridDS          ds
     d wsfseq                         5  0
     d*cmnt|comment:This field holds the file seq# which corresponds with the restriction seq#|
     d svcVerifyType                  1
     d*cmnt|comment:This field holds the service verification type code|
     d authType                       1
     d*cmnt|comment:This field holds the authorization type code|
     d authTypeDesc                  50
     d*cmnt|comment:This field holds the authorization type description|
     d wsproc                         8
     d*cmnt|comment:This field holds the name of procedure|
     d wsprocd                       30
     d*cmnt|comment:This field holds the procedure description|
     d wsrevc                         4  0
     d*cmnt|comment:This field holds the revenue code|
     d wsrevcd                       25
     d*cmnt|comment:This field holds the revenue code description|
     d ptDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for PT Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d otDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for OT Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d spDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for SP Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d swDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for SW Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d bhDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for BH Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d acDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for AC Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d trDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for TR Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d nrDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for NR Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d cmDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for CM Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d vcDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for VC Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d phDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for PH Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d tcDiscipline                   1
     d*cmnt+|comment:This field holds the selection values for TC Discipline|
     d*cmnt|comment: Default to 'N', pass 'Y' if the checkbox is selected. |
     d disciplineDesc                50
     d*cmnt|comment:This field holds the discipline description|
     d wsunit                         5  0
     d*cmnt|comment:This field holds the authorized units|
     d wsgrpn                        10  0
     d*cmnt|comment:This field holds the group number|
     d wsgrpm                        12
     d*cmnt|comment:This field holds the group name|
     d wsAuth                        25
     d*cmnt|comment:This field holds the authorization number|
     d wsaucn                         9  0
     d*cmnt|comment:This field holds the authorization contact number|
     d wslngt                         5  0
     d*cmnt|comment:This field holds the length of verification|
     d wsltyp                         1
     d*cmnt|comment:This field holds the length of verification type|
     d wsltypd                       50
     d*cmnt|comment:This field holds the length of verification description|
     d wseffd                         8  0
     d*info|jType:Y|
     d*cmnt|comment:This field holds the effective date|
     d wsendd                         8  0
     d*info|jType:Y|
     d*cmnt|comment:This field holds the expiration date|
     d wstype                         5
     d*cmnt|comment:This field holds the reimbursement rate type|
     d wstyped                       50
     d*cmnt|comment:This field holds the reimbursement rate description|
     d wsrate                         9  2
     d*cmnt|comment:This field holds the reimbursement rate amount|
     d supplUnits                     5  0
     d*cmnt|comment:This field holds the supplemental units|
     d supplAmount                    9  2
     d*cmnt|comment:This field holds the supplemental amount|
     d wsvsts                         2
     d*cmnt|comment:This field holds the verification status|
     d wsvstsd                       50
     d*cmnt|comment:This field holds the verification status description|
     d showWSLOC                      1
     d*cmnt|comment: Show/Hide the WSLOC field:  Y=Show, N=Hide|
     d wsloc                          2
     d*cmnt|comment:This field holds the location code|
     d showWSVBIL                     1
     d*cmnt|comment: Show/Hide the WSVBIL field:  Y=Show, N=Hide|
     d wsvbil                         1
     d*cmnt|comment:This field holds the voucher billing|
     d showWSADCD                     1
     d*cmnt|comment: Show/Hide the WSADCD field:  Y=Show, N=Hide|
     d wsadcd                         4
     d*cmnt|comment:This field holds the add-on code|
     d**** wscmnt                        50
     d wscmnt                       250
     d*cmnt|comment:This field holds the comments|
     d wsdlt                          1
     d*cmnt|comment:This field holds the procedure delete flag|
     d createdBy                     10
     d*cmnt|comment: Return id of user that created|
     d createdOnDate                  8  0
     d*info|jType:Y|
     d*cmnt|comment: Return date of creation|
     d changedBy                     10
     d*cmnt|comment: Return id of user that changed|
     d changedOnDate                  8  0
     d*info|jType:Y|
     d*cmnt|comment: Return date of change|
     d infoOnly                       1
     d*cmnt|comment: Info Only Checkbox:  Y=Checked, N=Not Checked|

     d restrictionsDS  ds                  qualified
     d  fileSeq                       5  0
     d*cmnt|comment:This field holds the file seq# which corresponds with the procedure seq#|
     d  period                        1
     d*info|use:output|jType:S|req:N|
     d*cmnt|comment: Return grid period code|
     d  periodD                      20
     d*info|use:output|jType:S|req:N|
     d*cmnt|comment: Return grid period code description|
     d  units                         5  0
     d*info|use:output|jType:I|req:N|
     d*cmnt|comment: Return grid number if units|
     d  sunday                        5
     d*cmnt+|comment:This field is the Sunday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  sundayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday start time. |
     d  sundayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday end time|
     d  monday                        5
     d*cmnt+|comment:This field is the Monday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  mondayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday start time|
     d  mondayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday end time|
     d  tuesday                       5
     d*cmnt+|comment:This field is the Tuesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  tuesdayFrom                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday start time|
     d  tuesdayTo                     4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday end time|
     d  wednesday                     5
     d*cmnt+|comment:This field is the Wednesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  wednesdayFrom                 4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday start time|
     d  wednesdayTo                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday end time|
     d  thursday                      5
     d*cmnt+|comment:This field is the Thursday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  thursdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday start time|
     d  thursdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday end time|
     d  friday                        5
     d*cmnt+|comment:This field is the Friday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  fridayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday start time|
     d  fridayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday end time|
     d  saturday                      5
     d*cmnt+|comment:This field is the Saturday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  saturdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday start time|
     d  saturdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday end time|
     d  deleteFlag                    1
     d*info|use:output|jType:S|req:N|
     d*cmnt|comment: Return grid Delete code column|
     d  createdBy                    10
     d*info|use:output|jType:S|req:N|
     d*cmnt|comment: Return grid created by user|
     d  createdDate                   8  0
     d*info|use:output|jType:Y|req:N|
     d*cmnt|comment: Return grid created on date|
     d  changedBy                    10
     d*info|use:output|jType:S|req:N|
     d*cmnt|comment: Return grid changed by user|
     d  changedDate                   8  0
     d*info|use:output|jType:Y|req:N|
     d*cmnt|comment: Return grid changed on date|
     d  oldEffFrom                    8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restriction original effective from date|
     d  oldEffTo                      8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restriction original effective to date|
     d  effectiveFrom                 8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restriction effective from date|
     d  effectiveTo                   8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restriction effective to date|

     d periodDS        ds                  inz
     d  period                        1
     d*cmnt|comment:This field is the period code|
     d  periodD                      20
     d*cmnt|comment: Period code description|
     d  units                         5  0
     d*info|use:Input/Output|jType:I|req:N|
     d*cmnt|comment: Number of units|
     d  sunday                        5
     d*cmnt+|comment:This field is the Sunday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  sundayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday start time. |
     d  sundayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday end time|
     d  monday                        5
     d*cmnt+|comment:This field is the Monday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  mondayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday start time|
     d  mondayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday end time|
     d  tuesday                       5
     d*cmnt+|comment:This field is the Tuesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  tuesdayFrom                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday start time|
     d  tuesdayTo                     4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday end time|
     d  wednesday                     5
     d*cmnt+|comment:This field is the Wednesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  wednesdayFrom                 4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday start time|
     d  wednesdayTo                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday end time|
     d  thursday                      5
     d*cmnt+|comment:This field is the Thursday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  thursdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday start time|
     d  thursdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday end time|
     d  friday                        5
     d*cmnt+|comment:This field is the Friday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  fridayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday start time|
     d  fridayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday end time|
     d  saturday                      5
     d*cmnt+|comment:This field is the Saturday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be integers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  saturdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday start time|
     d  saturdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday end time|
     d  rqsdsc                       20
     d*info|use:Input/Output|jType:S|req:N|
     d*cmnt|comment: Period short description|
     d  deleteFlag                    1
     d*info|use:Input/Output|jType:S|req:N|
     d*cmnt|comment: Delete code column|
     d  oldEffFrom                    8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restirction original effective from date|
     d  oldEffTo                      8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restriction original effective to date|
     d  effectiveFrom                 8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restirction effective from date|
     d  effectiveTo                   8  0
     d*info|use:Input/Output|jType:Y|req:N|
     d*cmnt|comment: Period restirction effective to date|

     d                 ds
     d tmstmp                        14  0
     d  stmpdt                       14  0 overlay(tmstmp)
     d   stmptm                       6  0 overlay(stmpdt)

     d                 ds
     d curtim                        14  0
     d  runtme                        6  0 overlay(curtim)
     d   runtim                       4  0 overlay(runtme)
     d  rundte                        8  0 overlay(curtim:7)

     d                 ds
     d wkunits                        6
     d  wknumb                        3    overlay(wkunits)
     d  wkconst                       1    overlay(wkunits:4)
     d  wkdec                         2    overlay(wkunits:5)
     d   hlddec1                      1    overlay(wkdec:1)
     d   hlddec2                      1    overlay(wkdec:2)
     d                 ds
     d wkqnty                         7  2
     d  wkqtyw                        5    overlay(wkqnty)
     d  wkqtyd                        2    overlay(wkqnty:6)
     d  wkqtydn                       2  0 overlay(wkqnty:6)
     d                 ds
     d workDate                       8  0
     d  workYear                      4  0 overlay(workDate)
     d  workMonthDay                  4  0 overlay(workDate:5)
     d   workMonth                    2  0 overlay(workMonthDay)
     d   workDay                      2  0 overlay(workMonthDay:3)

      **************************************************************************
      ** Misc. Variables                                                      **
      **************************************************************************
     d count           s              5i 0
     d dltFlag         s              1
     d enddat          s              8  0
     d enddte          s              8  0
     d errflg          s              1
     d errorCount      s             10i 0
     d*info|use:output|
     d*cmnt|comment:Counter for errors data structure|
     d effectDate      s              8  0
     d expireDate      s              8  0
     d groupOK         s              1
     d ivsFound        s               n
     d cnvFound        s               n
     d mdate           s              8  0
     d nextSeq         s              5i 0
     d otCount         s              5i 0
     d otRevCdCount    s              5i 0
     d procedure       s              8
     d prmlv6          s              6  0
     d prmacc          s             12  0
     d prmApp          s              1
     d prmval          s             25
     d prmnam          s             10
     d prmpyr          s              6  0
     d prmpln          s              5  0
     d prmpol          s             20
     d prmisq          s              4  0
     d prmdte          s              8  0
     d prmbyr          s              2  0
     d prmgdt          s              8  0
     d prmmsg          s             70
     d prmcod          s              1
     d prmResp         s              1
     d ptCount         s              5i 0
     d ptRevCdCount    s              5i 0
     d quantity        s              7  2
     d reprorateDate   s              8  0
     d reprorateFlag   s              1
     d rqrsid          s             30
     d rtncod          s              1
     d rtrnc2          s              1
     d savgrcnv        s              2
     d savgrunits      s              7  0
     d savunits        s              7  2
     d savyear         s              1
     d startd          s              8  0
     d spCount         s              5i 0
     d spRevCdCount    s              5i 0
     d usedQty         s              8  0
     d usedSup         s              8  0
     d***** units           s              5  0
     d verdte          s              8  0
     d********** workDate        s              8  0
     d wrkseq          s              5  0
     d x               s              5i 0
     d xx              s              5i 0
     d ydate           s              8  0
     d ydate1          s              8  0
     d ydate2          s              8  0
     d ydays           s              5  0
     d #date           s              8  0
     d #time           s              6  0
     d swDisCount      s              5i 0
     d bhDisCount      s              5i 0
     d acDisCount      s              5i 0
     d trDisCount      s              5i 0
     d nrDisCount      s              5i 0
     d cmDisCount      s              5i 0
     d vcDisCount      s              5i 0
     d phDisCount      s              5i 0
     d tcDisCount      s              5i 0
     d swRevCdCount    s              5i 0
     d bhRevCdCount    s              5i 0
     d acRevCdCount    s              5i 0
     d trRevCdCount    s              5i 0
     d nrRevCdCount    s              5i 0
     d cmRevCdCount    s              5i 0
     d vcRevCdCount    s              5i 0
     d phRevCdCount    s              5i 0
     d tcRevCdCount    s              5i 0

     d chkfrm          s              8  0
     d chkto           s              8  0
     d cnvtyp          s              1
     d dateerror       s              1
     d error           s              1
     d fcsstr          s              4  0
     d hldday          s              6
     d hlddec          s              2
     d hldpsq          s              2
     d mtmond          s              4  0
     d mtmon2          s              4  0
     d mtfrid          s              4  0
     d mtfri2          s              4  0
     d mtsatd          s              4  0
     d mtsat2          s              4  0
     d mtsund          s              4  0
     d mtsun2          s              4  0
     d mtthur          s              4  0
     d mtthu2          s              4  0
     d mttues          s              4  0
     d mttue2          s              4  0
     d mtweds          s              4  0
     d mtwed2          s              4  0
     d newdates        s              1
     d noupdate        s              1
     d qnty            s              7  2
     d qntySunday      s              7  2
     d qntyMonday      s              7  2
     d qntyTuesday     s              7  2
     d qntyWednesday   s              7  2
     d qntyThursday    s              7  2
     d qntyFriday      s              7  2
     d qntySaturday    s              7  2
     d repdte          s              8  0
     d repro           s              1
     d savend          s              8  0
     d savfrm          s              8  0
     d savpsq          s              4  0
     d wrkend          s              8  0
     d wrkfrm          s              8  0
     d wkwrkd          s              6
     d wOldEffFrom     s              8  0
     d wOldEffTo       s              8  0
     d xUnit           s              7  2
     d xSupUnit        s              7  2

      *-- data areas -----------------------------------------------------------
      /copy copysrc,hxxlda
      /copy copysrc,hxxlevel
      /copy copysrc,hxxtable
      /copy copysrc,hoxdefn
      /copy copysrc,cfxproc

      *-- sql options ----------------------------------------------------------
      /copy copysrc,hxxsqlopt

      *-- stand alone bottom----------------------------------------------------
      /copy copysrc,hxxppd

      **************************************************************************
      **                          Main Routine Code                           **
      **************************************************************************
     c     *entry        plist
     c                   parm                    userid
     c                   parm                    passwd
     c                   parm                    option
     c                   parm                    request
     c                   parm                    changeOK
     c                   parm                    gridCount
     c                   parm                    grid
     c                   parm                    restCount
     c                   parm                    restrictions
     c                   parm                    infoOnlyAuth
     c                   parm                    errorCount
     c                   parm                    errors
     c                   parm                    warnMsg
     c                   parm                    WRMerror
     c                   parm                    rtrncd

     c     *dtaara       define    hxalevel      level
     c     *dtaara       define    *lda          ldads
     c     *dtaara       define    haadate       today             8 0

      /free

         // Initializations //
         rtrncd = *blanks;
         warnMsg = *blanks;
         WRMerror = '';
         errorCount = 0;
         clear errors;
         xfpdsc = *blanks;
         xfpamt = *zeros;
         xfptxa = *zeros;
         xfprev = *zeros;
         xfpubc = *zeros;
         xfpnub = *zeros;
         xfpcpt = *blanks;
         xfpovr = *blanks;
         xfppfg = *blanks;
         xfpnch = *blanks;
         xfpbid = *blanks;
         xfpvrp = *blanks;
         xfpmod = *blanks;
         xfpmd2 = *blanks;
         xfpmd2 = *blanks;
         xfpmd4 = *blanks;
         xfpcnv = *blanks;
         xfpgfg = *blanks;
         xfptim = *zeros;
         xfpctx = *blanks;
         gridDS = grid(1);

         exsr CheckAuthorities;

         in *dtaara;
         #date = today;
         #time = %int(%char(%time():*ISO0));

         open hmpmast;
         open hxpbnfit;
         open halirnks;
         open hapdemo;
         open hapdemoi;
         open hxlwinh;
         open hxpprocm;
         open hxpprocc;
         open hxlprcca;
         open hbpbrev;
         open hxpctg1;
         open hbpcrate;
         open hmpmams;
         open hxpl6shf;
         open hbppdiem;
         open hapivsvc;
         open halivsvr;
         open halivsvrr;
         open halivsvs;
         open halivsvu;
         open hxpinex;
         open hxpinexr;
         open haltrno7;
         open hapivsp;
         open halivsp;
         open hulapptz;
         open hupadtl;
         open hblchrga;
         open hbpchgtr;
         open halivstr;
         open hapivstr;
         open hapivsgr;
         open halivsgrn;
         open erlerespr;
         open hbpfschd;

         chain (payor : plan ) xffbnfit;
         if not %found(hxpbnfit);
           rtrncd = '00000239';
         endif;

         chain (lvl6 : account) hmpmast;
         if not %found(hmpmast);    // Account not found //
           rtrncd = '00001516';
         endif;

         chain (lvl6 : account : rank) halirnks;
         if not %found(halirnks);    // Rank not found //
           rtrncd = '00001517';
         endif;

         xfxppd(lvl6: account: brkeff: payor: plan: ppdDS);
         infoOnlyAuth = sndiio;

         if (option <> 'B');
           if (option = 'A' or option = 'Y' or option = 'C' or option = 'D'
               or option = 'R');
             exsr ValidateProcedure;
           elseif (option = 'AA' or option = 'CC' or option = 'DD'
                   or option = 'RR');
             exsr ValidateRestriction;
           endif;
         endif;

         alwPending = ' ';
         if option <> 'B';
           callp HBBALWCHKI(lvl6:account:rank:alwPending);
           if alwPending = 'Y';
             rtrncd = '00003680';
           endif;
         endif;

         if (errorCount = 0) and (alwPending <> 'Y') and
            (rtrnCd = *blanks and warnMsg = *blanks);
           select;
             when (option = 'B');
               exsr BuildProceduresGrid;
               exsr BuildRestrictionsGrid;
             when (option = 'A' or option = 'Y');
               exsr AddCopyProcedure;
             when (option = 'C');
               exsr ChangeProcedure;
             when (option = 'D');
               exsr DeleteProcedure;
             when (option = 'R');
               exsr ReinstateProcedure;
             when (option = 'AA' or option = 'CC' or option = 'DD'
                   or option = 'R');
               //exsr DoAction;
           endsl;
           if (errorCount = 0 and reprorateFlag = 'Y' and
               rtrnCd = *blanks and warnMsg = *blanks);
             exsr Reprorate;
           endif;
         endif;

         close  *all;
         xfxwsexit();
         *inlr = *on;
         return;

        /////////////////////////////////////////////////////////////////
        // CheckAuthorities                                            //
        /////////////////////////////////////////////////////////////////
        begsr CheckAuthorities;

          // Check Authority to Web Services //
          xfxwsaut( userid : passwd : rtrncd );
          if rtrncd <> *blanks;
            return;
          endif;

          if (option = 'A');    // Check Authority to Add //
            xfcnhkat( 3552 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;
          if (option = 'AA');    // Check Authority to Add //
            xfcnhkat( 3557 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;

          if (option = 'Y');    // Check Authority to Copy //
            xfcnhkat( 3567 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;

          if (option = 'C');    // Check Authority to Change //
            xfcnhkat( 3553 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;
          if (option = 'CC');    // Check Authority to Change //
            xfcnhkat( 3558 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;

          if (option = 'D');    // Check Authority to Delete //
            xfcnhkat( 3554 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;
          if (option = 'DD');    // Check Authority to Delete //
            xfcnhkat( 3559 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;

          if (option = 'R');    // Check Authority to Reinstate //
            xfcnhkat( 3555 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;
          if (option = 'R');    // Check Authority to Reinstate //
            xfcnhkat( 3560 : rtrnc2);
            if rtrnc2 = 'E';
              rtrncd = '00000004';
              return;
            endif;
          endif;

        endsr;

        ////////////////////////////////////////////////////////////////////////
        begsr ValidateProcedure;
        ////////////////////////////////////////////////////////////////////////

           eval-corr gridDS = grid(1);

        //--------------------------------------------------------------------//
        // verify requested action can be performed                           //
        //--------------------------------------------------------------------//
          if (option = 'C' or option = 'D' or option = 'Y') and (wsdlt = 'D');
            // Action cannot be performed on deleted procedure //
            AddErrorMsg('option' : '00001514');
            leavesr;
          elseif (option = 'R' and wsdlt = ' ');
            // Action cannot be performed on active procedure //
            AddErrorMsg('option' : '00001515');
            leavesr;
          endif;

        //--------------------------------------------------------------------//
        // validate dates and range                                           //
        //--------------------------------------------------------------------//
          ydate = wseffd;
          xfxcymd(ydate : mdate);
          effectDate = ydate;
          if (mdate = 0);
            AddErrorMsg('WSEFFD' : '00001394');
          endif;

          // effective date is prior to policy effective date
          if (effectDate < brkeff);
            AddErrorMsg('WSEFFD' : '00001592');
          endif;

          if (wsendd <> 0);
            if (wsendd = 99999999);
              expireDate = 99999999;
            else;
              ydate = wsendd;
              xfxcymd(ydate : mdate);
              expireDate = ydate;
              if (mdate = 0);
                AddErrorMsg('WSENDD' : '00001394');
              endif;
            endif;
          else;
              expireDate = 99999999;
          endif;

          if ((effectDate <> 0 and expireDate <> 0)
           and (effectDate > expireDate));
            // Expirateion date must come after effective date //
            AddErrorMsg('WSENDD' : '00001392');
          endif;

        //--------------------------------------------------------------------//
        // validate procedure                                                 //
        //--------------------------------------------------------------------//
          if wsproc <> *blanks;

            chain wsproc hxpprocm;
            if not %found(hxpprocm);    // Procedure not valid //
              AddErrorMsg('WSPROC' : '00000503');
            else;
              xfxproc( lvl6   : wsProc : effectDate : xfpdsc : xfpamt : xfptxa :
                       xfprev : xfpubc : xfpnub : xfpcpt : xfpovr : xfppfg :
                       xfpnch : xfpbid : xfpvrp : xfpmod : xfpmd2 : xfpmd2 :
                       xfpmd4 : xfpcnv : xfpgfg : xfptim : xfpctx );
            endif;

            if (%trim(option) = 'A' or %trim(option) = 'Y' or
                 %trim(option) = 'C' or %trim(option) = 'R');

              // check for overlapping dates  //
              clear ivsFound;
              EXEC SQL
                SELECT '1' INTO :ivsFound
                FROM halivsvr
                WHERE bsvLv6 = :lvl6
                  AND bsvAcc = :account
                  AND bsvSeq = :rank
                  AND bsvPyr = :payor
                  AND bsvPln = :plan
                  AND bsvPol = :policy
                  AND bsvPrc = :wsProc
                  AND bsvDlt = ' '
                  AND (trim(:option) in ('A','Y','R')
                       OR (trim(:option) ='C' and bsvFsq <> :wsfSeq))
                  AND ((:effectDate < bsvEff AND bsvEnd < :expireDate)
                   OR (:effectDate < bsvEff
                       AND :expireDate BETWEEN bsvEff AND bsvEnd)
                   OR (:effectdate BETWEEN bsvEff AND bsvEnd
                       AND :expireDate BETWEEN bsvEff AND bsvEnd)
                   OR (:effectDate BETWEEN bsvEff AND bsvEnd
                       AND :expireDate > bsvEnd))
                FETCH FIRST 1 ROW ONLY;
            endif;

            if ivsFound and %trim(option) = 'R';
               rtrnCd = '00000263';
            elseif ivsFound;    // procedure with overlapping dates exist //
              AddErrorMsg('WSPROC' : '00000263');
              AddErrorMsg('WSEFFD' : '00000263');
              AddErrorMsg('WSENDD' : '00000263');
            endif;

          endif;

        //--------------------------------------------------------------------//
        // validate revenue code                                              //
        //--------------------------------------------------------------------//
          if wsrevc <> 0;

            chain wsrevc hbpbrev;
            if not %found(hbpbrev);    // Revenue code not valid //
              AddErrorMsg('WSREVC' : '00000007');
            endif;

            if (%trim(option) = 'A' or %trim(option) = 'Y'
                 or %trim(option) = 'C' or %trim(option) = 'R');
              // check for overlapping dates on request to add //
              clear ivsFound;
              exec sql
                select '1' into :ivsFound
                from halivsvrr
                where bsvlv6 = :lvl6
                  and bsvacc = :account
                  and bsvseq = :rank
                  and bsvpyr = :payor
                  and bsvpln = :plan
                  and bsvpol = :policy
                  and (bsvrvc = :wsrevc
                       or (bsvptd = 'Y' and :wsrevc >= 420 and :wsrevc <= 429)
                       or (bsvotd = 'Y' and :wsrevc >= 430 and :wsrevc <= 439)
                       or (bsvspd = 'Y' and :wsrevc >= 440 and :wsrevc <= 449)
                       or (bsvswd = 'Y' and :wsrevc >= 560 and :wsrevc <= 569)
                       or (bsvbhd = 'Y' and :wsrevc >= 900 and :wsrevc <= 918)
                       or (bsvacd = 'Y' and :wsrevc = 2101)
                       or (bsvtrd = 'Y' and :wsrevc = 941)
                       or (bsvnrd = 'Y' and :wsrevc = 230)
                       or (bsvcmd = 'Y' and :wsrevc >= 942 and :wsrevc <= 944)
                       or (bsvvcd = 'Y' and :wsrevc = 948)
                       or (bsvphd = 'Y' and :wsrevc = 960)
                       or (bsvtcd = 'Y' and :wsrevc = 940)
                      )
                  and bsvdlt = ' '
                  AND (TRIM(:option) IN ('A','Y','R')
                      OR (TRIM(:option) = 'C' AND bsvFsq <> :wsfSeq))
                  AND ((:effectDate < bsvEff AND bsvEnd < :expireDate)
                   OR (:effectDate < bsvEff
                       AND :expireDate BETWEEN bsvEff AND bsvEnd)
                   OR (:effectdate BETWEEN bsvEff AND bsvEnd
                       AND :expireDate BETWEEN bsvEff AND bsvEnd)
                   OR (:effectDate BETWEEN bsvEff AND bsvEnd
                       AND :expireDate > bsvEnd));
            endif;

            if ivsFound and %trim(option) = 'R';
               rtrnCd = '00000263';
            elseif ivsFound;    // revenue code with overlapping dates exist //
              AddErrorMsg('WSREVC' : '00000263');
              AddErrorMsg('WSEFFD' : '00000263');
              AddErrorMsg('WSENDD' : '00000263');
            endif;

          endif;

        //--------------------------------------------------------------------//
        // validate discipline checkboxes                                     //
        //--------------------------------------------------------------------//
          if ptDiscipline = 'Y' or otDiscipline = 'Y' or spDiscipline = 'Y' or
             swDiscipline = 'Y' or bhDiscipline = 'Y' or acDiscipline = 'Y' or
             trDiscipline = 'Y' or nrDiscipline = 'Y' or cmDiscipline = 'Y' or
             vcDiscipline = 'Y' or phDiscipline = 'Y' or tcDiscipline = 'Y';

            // check for overlapping dates on request to add //
            ptCount = 0;
            otCount = 0;
            spCount = 0;
            swDisCount = 0;
            bhDisCount = 0;
            acDisCount = 0;
            trDisCount = 0;
            nrDisCount = 0;
            cmDisCount = 0;
            vcDisCount = 0;
            phDisCount = 0;
            tcDisCount = 0;
            ptRevCdCount = 0;
            otRevCdCount = 0;
            spRevCdCount = 0;
            swRevCdCount = 0;
            bhRevCdCount = 0;
            acRevCdCount = 0;
            trRevCdCount = 0;
            nrRevCdCount = 0;
            cmRevCdCount = 0;
            vcRevCdCount = 0;
            phRevCdCount = 0;
            spRevCdCount = 0;

            if (%trim(option) = 'A' or %trim(option) = 'Y'
                 or %trim(option) = 'C' or %trim(option) = 'R');

              // check for overlapping disciplines
              exec sql
                with DisciplineRcds as (
                  select *
                  from hapivsvc
                  where bsvlv6 = :lvl6
                    and bsvacc = :account
                    and bsvseq = :rank
                    and bsvpyr = :payor
                    and bsvpln = :plan
                    and bsvpol = :policy
                    AND (bsvPtd = :ptDiscipline
                         OR bsvOtd = :otDiscipline
                         OR bsvSpd = :spDiscipline
                         OR bsvSwd = :swDiscipline
                         OR bsvBhd = :bhDiscipline
                         OR bsvAcd = :acDiscipline
                         OR bsvTrd = :trDiscipline
                         OR bsvNrd = :nrDiscipline
                         OR bsvCmd = :cmDiscipline
                         OR bsvVcd = :vcDiscipline
                         OR bsvPhd = :phDiscipline
                         OR bsvTcd = :tcDiscipline)
                    and bsvdlt = ' '
                    AND (TRIM(:option) in ('A','Y','R')
                         OR (TRIM(:option) = 'C' and bsvFsq <> :wsfSeq))
                    AND ((:effectDate < bsvEff AND bsvEnd < :expireDate)
                     OR (:effectDate < bsvEff
                         AND :expireDate BETWEEN bsvEff AND bsvEnd)
                     OR (:effectdate BETWEEN bsvEff AND bsvEnd
                         AND :expireDate BETWEEN bsvEff AND bsvEnd)
                     OR (:effectDate BETWEEN bsvEff AND bsvEnd
                         AND :expireDate > bsvEnd))
                )
                Select
                  sum(case when (bsvptd = 'Y' and :ptDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvotd = 'Y' and :otDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvspd = 'Y' and :spDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvswd = 'Y' and :swDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvbhd = 'Y' and :bhDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvacd = 'Y' and :acDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvtrd = 'Y' and :trDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvnrd = 'Y' and :nrDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvcmd = 'Y' and :cmDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvvcd = 'Y' and :vcDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvphd = 'Y' and :phDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvtcd = 'Y' and :tcDiscipline = 'Y')
                           then 1 else 0 end)
                into :ptCount, :otCount, :spCount, :swDisCount, :bhDisCount,
                     :acDisCount, :trDisCount, :nrDisCount, :cmDisCount,
                     :vcDisCount, :phDisCount, :tcDisCount
                from DisciplineRcds;

              // check for overlapping revenue codes for disciplines
              exec sql
                with DisciplineRevCdRcds as (
                  select *
                  from hapivsvc
                  where bsvlv6 = :lvl6
                    and bsvacc = :account
                    and bsvseq = :rank
                    and bsvpyr = :payor
                    and bsvpln = :plan
                    and bsvpol = :policy
                    and ((bsvrvc between 420 and 449) or
                         (bsvrvc between 560 and 569) or
                         (bsvrvc between 900 and 918) or
                         (bsvrvc between 942 and 944) or
                         (bsvrvc = 2101 or bsvrvc = 941 or bsvrvc = 230 or
                         bsvrvc = 948 or bsvrvc = 960 or bsvrvc = 940))
                    and bsvdlt = ' '
                    AND (TRIM(:option) in ('A','Y','R')
                         OR (TRIM(:option) = 'C' and bsvFsq <> :wsfSeq))
                    AND ((:effectDate < bsvEff AND bsvEnd < :expireDate)
                     OR (:effectDate < bsvEff
                         AND :expireDate BETWEEN bsvEff AND bsvEnd)
                     OR (:effectdate BETWEEN bsvEff AND bsvEnd
                         AND :expireDate BETWEEN bsvEff AND bsvEnd)
                     OR (:effectDate BETWEEN bsvEff AND bsvEnd
                         AND :expireDate > bsvEnd))
                )
                Select
                  sum(case when (bsvrvc between 420 and 429
                                 and :ptDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc between 430 and 439
                                 and :otDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc between 440 and 449
                                 and :spDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc between 560 and 569
                                 and :swDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc between 900 and 918
                                 and :bhDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 2101
                                 and :acDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 941
                                 and :trDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 230
                                 and :nrDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc between 942 and 944
                                 and :cmDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 948
                                 and :vcDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 960
                                 and :phDiscipline = 'Y')
                           then 1 else 0 end),
                  sum(case when (bsvrvc = 940
                                 and :tcDiscipline = 'Y')
                           then 1 else 0 end)
                into :ptRevCdCount, :otRevCdCount, :spRevCdCount,
                     :swRevCdCount, :bhRevCdCount, :acRevCdCount,
                     :trRevCdCount, :nrRevCdCount, :cmRevCdCount,
                     :vcRevCdCount, :phRevCdCount, :tcRevCdCount
                from DisciplineRevCdRcds;

            endif;

            if (ptCount > 0 or otCount > 0 or spCount > 0 or
                swDisCount > 0 or bhDisCount > 0 or acDisCount > 0 or
                trDisCount > 0 or nrDisCount > 0 or cmDisCount > 0 or
                vcDisCount > 0 or phDisCount > 0 or tcDisCount > 0 or
                ptRevCdCount > 0 or otRevCdCount > 0 or spRevCdCount > 0 or
                swRevCdCount > 0 or bhRevCdCount > 0 or acRevCdCount > 0 or
                trRevCdCount > 0 or nrRevCdCount > 0 or cmRevCdCount > 0 or
                vcRevCdCount > 0 or phRevCdCount > 0 or tcRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('WSEFFD' : '00000263');
                 AddErrorMsg('WSENDD' : '00000263');
               endif;
            endif;

            if (ptCount > 0 or ptRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                  AddErrorMsg('PTDISCIPLINE' : '00000263');
               endif;
            endif;

            if (otCount > 0 or otRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                  AddErrorMsg('OTDISCIPLINE' : '00000263');
               endif;
            endif;

            if (spCount > 0 or spRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('SPDISCIPLINE' : '00000263');
               endif;
            endif;

            if (swDisCount > 0 or swRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('SWDISCIPLINE' : '00000263');
               endif;
            endif;

            if (bhDisCount > 0 or bhRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('BHDISCIPLINE' : '00000263');
               endif;
            endif;

            if (acDisCount > 0 or acRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('ACDISCIPLINE' : '00000263');
               endif;
            endif;

            if (trDisCount > 0 or trRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('TRDISCIPLINE' : '00000263');
               endif;
            endif;

            if (nrDisCount > 0 or nrRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('NRDISCIPLINE' : '00000263');
               endif;
            endif;

            if (cmDisCount > 0 or cmRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('CMDISCIPLINE' : '00000263');
               endif;
            endif;

            if (vcDisCount > 0 or vcRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('VCDISCIPLINE' : '00000263');
               endif;
            endif;

            if (phDisCount > 0 or phRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('PHDISCIPLINE' : '00000263');
               endif;
            endif;

            if (tcDisCount > 0 or tcRevCdCount > 0);
               if %trim(option) = 'R';
                  rtrnCd = '00000263';
               else;
                 AddErrorMsg('TCDISCIPLINE' : '00000263');
               endif;
            endif;

          endif;

          exsr clrtbl;
          tcode = 'IVTP';
          ecode = svcVerifyType;
          exsr srtabl;
          if (tind = 'E');
            wsltyp = *blanks;    // Servive Verification type is not valid //
            AddErrorMsg('SVCVERIFYTYPE' : '00003865');
          endif;

          if (svcVerifyType = 'S' and
                ((wsproc = *blanks and wsrevc = 0) or
                 (wsproc <> *blanks and wsrevc <> 0)));
            AddErrorMsg('WSPROC' : '00002092');
            AddErrorMsg('WSREVC' : '00002092');
          endif;

          if (svcVerifyType = 'V' and
                (ptDiscipline <> 'Y' and otDiscipline <> 'Y' and
                  spDiscipline <> 'Y'and swDiscipline <> 'Y' and
                  bhDiscipline <> 'Y'and acDiscipline <> 'Y' and
                  trDiscipline <> 'Y'and nrDiscipline <> 'Y' and
                  cmDiscipline <> 'Y'and vcDiscipline <> 'Y' and
                  phDiscipline <> 'Y'and tcDiscipline <> 'Y'));
            AddErrorMsg('PTDISCIPLINE' : '00003864');
            AddErrorMsg('OTDISCIPLINE' : '00003864');
            AddErrorMsg('SPDISCIPLINE' : '00003864');
            AddErrorMsg('SWDISCIPLINE' : '00003864');
            AddErrorMsg('BHDISCIPLINE' : '00003864');
            AddErrorMsg('ACDISCIPLINE' : '00003864');
            AddErrorMsg('TRDISCIPLINE' : '00003864');
            AddErrorMsg('NRDISCIPLINE' : '00003864');
            AddErrorMsg('CMDISCIPLINE' : '00003864');
            AddErrorMsg('VCDISCIPLINE' : '00003864');
            AddErrorMsg('PHDISCIPLINE' : '00003864');
            AddErrorMsg('TCDISCIPLINE' : '00003864');
          endif;

        //--------------------------------------------------------------------//
        // validate group info                                                //
        //--------------------------------------------------------------------//
          groupOK = 'N';
          wsgrpm = *blanks;
          if (wsgrpn <> 0);
            chain wsgrpn halivsgrn;
            if (%found(halivsgrn) and ggrdlt = *blanks);
              groupOK = 'Y';
            else;    // Group number is not valid
              AddErrorMsg('WSGRPN' : '00001519');
            endif;
          endif;
          if (groupOK = 'Y' and wsgrpn <> 0);
            chain wsgrpn halivsgrn;
            if %found(halivsgrn);
              wsauth = ggraut;
              wsgrpm = ggrnam;

              ydate = ggreff;
              xfxcymd(ydate : mdate);
              effectDate = ydate;
              if (mdate = 0);
                AddErrorMsg('WSEFFD' : '00001394');
              endif;

              if (ggrend <> 99999999);
                ydate = ggrend;
                xfxcymd(ydate : mdate);
                expireDate = ydate;
                if (mdate = 0);
                  AddErrorMsg('WSENDD' : '00001394');
                endif;
              else;
                expireDate = ggrend;
                savgrunits = ggrcov;
                savgrcnv = ggrcnv;
              endif;
            else;    // Group number is not valid
              AddErrorMsg('WSGRPN' : '00001519');
            endif;
          endif;

        //--------------------------------------------------------------------//
        // validate group header type matches service verification type       //
        //--------------------------------------------------------------------//
          if wsgrpn > 0 and groupOK = 'Y';
            // Retrieve group header type from hapivsgr file
            exec sql
              SELECT bgrgtp
                INTO :wsGroupType
                FROM hapivsgr
                WHERE bgrlv6 = :lvl6
                  AND bgracc = :account
                  AND bgrnbr = :wsgrpn
                FETCH FIRST ROW ONLY;

            if sqlcode = 0;
              // Check if group header type matches service verification type
              if wsGroupType <> svcVerifyType;
                AddErrorMsg('SVCVERIFYTYPE' : '00003965');
              endif;
            endif;
          endif;

        //--------------------------------------------------------------------//
        // validate units                                                     //
        //--------------------------------------------------------------------//
          // Units must be between 1 and 99999 //
          if ((wsunit + supplUnits) = 0 or (wsunit + SupplUnits) > 99999);
            if (wsunit + supplUnits) <> 0 or wstype <> '$';
              AddErrorMsg('WSUNIT' : '00001221');
              AddErrorMsg('SUPPLUNITS' : '00001221');
            endif;
          endif;

          // Reimbursement type is not valid //
          if (((wsunit + supplUnits) <> 0 or (wsrate + supplAmount) <> 0)
              and wstype = '$' and sndf15 <> 'Y');
            AddErrorMsg('WSTYPE' : '00001520');
          endif;

          exsr CheckForYearly;

          if option <> 'D';
            exsr clrtbl;
            tcode = 'BDFT';
            ecode = wsauth;
            exsr srtabl;
            if (tind <> 'E');
              hldday = %trim(hmap);
              monitor;
                ydays = %int(hldday);
              on-error;
                ydays = 0;
              endmon;

              if ydays <> 0;
                setll (lvl6: account: rank: payor: plan: policy:
                        wsproc) hafivsvu;
                dou %eof(halivsvu);
                  reade (lvl6: account: rank: payor: plan: policy:
                         wsproc) hafivsvu;
                  if not %eof(halivsvu);
                    if isvfsq = wsfseq;
                      iter;
                    endif;
                    exsr clrtbl;
                    tcode = 'BDFT';
                    ecode = isvaut;
                    exsr srtabl;
                    if (tind <> 'E');
                      hldday = %trim(hmap);
                      monitor;
                        ydays = %int(hldday);
                      on-error;
                        ydays = 0;
                      endmon;
                      if ydays <> 0;
                        AddErrorMsg('WSAUTH' : '00003830');
                        leave;
                      endif;
                    endif;
                  endif;
                enddo;
              endif;
            endif;
          endif;

          if (savgrunits = 0);
            if ((wsunit + supplUnits) <= 0
                or (wsunit < 0) or (supplUnits < 0));
              if (wstype <> '$');    // Reimbursement type is not valid //
                AddErrorMsg('WSTYPE' : '00003925');
                AddErrorMsg('WSUNIT' : '00003925');
                AddErrorMsg('SUPPLUNITS' : '00003925');
              else;
              //if (sndf15 <> 'Y');    // ??????????????
              //  // *in23
              //  AddErrorMsg('SNDF15' : '00000000');
              //endif;
              endif;
            endif;
          endif;

          if (ggrcov <> 0 and option <> 'D');

            // Comment out existing logic of adding up quantities from other service verifications
            // Now validating individual service verification units against group header limits
            // clear usedQty;
            // clear usedSup;
            // EXEC SQL SELECT COALESCE(SUM(bsvCov - bsvSup),0),
            //                 COALESCE(SUM(bsvSup),0)
            //            INTO :usedQty, :usedSup
            //            FROM HAPIVSVC
            //           WHERE bsvLv6 = :lvl6
            //             AND bsvAcc = :account
            //             AND bsvGpn = :wsGrpn
            //             AND bsvDlt = ' '
            //             AND bsvFsq <> :wsfSeq;

            // convert units if conversion factor was used
            xUnit = wsUnit;
            xSupUnit = supplUnits;
            if xfppfg <> 'Y' and xfpcnv <> *blanks;
              xfxcvtim( xunit : xfpcnv : '1' : rtncod );
              xfxcvtim( xSupUnit : xfpcnv : '1' : rtncod );
            endif;

            // Modified validation to compare individual units to group header limits
            if xUnit > (ggrCov - ggrSup);
              AddErrorMsg('WSUNIT' : '00000371');
            endif;
            if xSupUnit > ggrSup;
              AddErrorMsg('SUPPLUNITS' : '00000371');
            endif;
            if (wstype = '$');    // Do not allow $ authorization on group procedures //
              AddErrorMsg('WSTYPE' : '00001521');
            endif;
          endif;

        //--------------------------------------------------------------------//
        // validate authorization number and contact number                   //
        //--------------------------------------------------------------------//

          // check for payor/plan edits on service authorization number
          if option <> 'D' and option <> 'R';
            prmnam = 'INSVCAUT';
            prmval = wsauth;
            exsr exedit;
            if prmcod <> *blanks and
               prmcod <> 'W';            // Authorization number is not valid/required  //
              AddErrorMsg('WSAUTH' : '00003807');
            elseif prmcod = 'W' and changeOk = *off;
          // Send warning message if the Auth Number is Optional
              warnMsg = 'Authorization number is missing or invalid. '
                        + 'Continuing will update them.';
            endif;
          endif;

          if (wsaucn <> 0);
            chain wsaucn hmpmams;
            if not %found(hmpmams);    // Authorization is not valid //
              AddErrorMsg('WSAUCN' : '00001523');
            else;
              if (hmdtyp = 'AC');
              else;
                AddErrorMsg('WSAUCN' : '00001523');
              endif;
            endif;
          endif;

        //--------------------------------------------------------------------//
        // validate the length and type of verification                       //
        //--------------------------------------------------------------------//
          if (wslngt <> 0);
            if (wsltyp = *blanks);    // Verification type is needed //
              AddErrorMsg('WSLTYP' : '00001524');
            endif;
          else;
            if (wsltyp <> *blanks);    // Verification length is needed //
              AddErrorMsg('WSLNGT' : '00001525');
            endif;
          endif;

          if (wsltyp <> *blanks);
            exsr clrtbl;
            tcode = 'BDWS';
            ecode = wsltyp;
            exsr srtabl;
            if (tind = 'E');
              wsltyp = *blanks;    // Verification type is not valid //
              AddErrorMsg('WSLTYP' : '00001526');
            endif;
          endif;

          // calculate the expiration date based on length of verification
          if (wslngt <> 0 and wsltyp <> *blanks and effectDate <> 0);
            exsr clrtbl;
            tcode = 'BDFT';
            ecode = wsauth;
            exsr srtabl;
            if (wsltyp = 'D' and (tind = 'E' or hmap = '00000'));
              ydate1 = wseffd;
              ydays = wslngt - 1;
              exsr sraddd;
              expireDate = ydate2;
            elseif (wsltyp = 'W' and (tind = 'E' or hmap = '00000'));
              ydate1 = wseffd;
              ydays = (wslngt * 7) - 1;
              exsr sraddd;
              expireDate = ydate2;
            endif;
          endif;

          if (wslngt = 0 and wsltyp = *blanks and effectDate <> 0
           and expireDate = 0);
            // Expiration date is needed
            AddErrorMsg('WSENDD' : '00001394');
          endif;

        //--------------------------------------------------------------------//
        // validate the rate type and amount                                  //
        //--------------------------------------------------------------------//
          // Supplemental amount not valid for Q & C
          if ((wstype = 'Q' or wstype = 'C') and supplAmount <> 0);
            AddErrorMsg('WSTYPE' : '00003842');
            AddErrorMsg('SUPPLAMOUNT' : '00003842');
          endif;

          if (wstype = 'C') and (wsrate < 1 or wsrate > 100);
            // Percentage entered is not valid must be between 1 and 100 //
            AddErrorMsg('WSRATE' : '00001527');
          endif;

          if (wstype = 'F' or wstype = 'U' or wstype = 'D' or wstype = 'H' or
              wstype = 'M' or wstype = '2' or wstype = '3' or wstype = '4' or
              wstype = '0' or wstype = '$')
           and ((wsrate + supplAmount) <= 0);
            // Reimbursement rate type and amount combination is not valid //
            AddErrorMsg('WSTYPE' : '00001528');
            AddErrorMsg('WSRATE' : '00001528');
            AddErrorMsg('SUPPLAMOUNT' : '00001528');
          endif;

          if (wstype = 'Z' and (wsrate <> 0 or supplAmount <> 0));
            // Reimbursement rate type and amount combination is not valid //
            AddErrorMsg('WSTYPE' : '00001528');
            AddErrorMsg('WSRATE' : '00001528');
            AddErrorMsg('SUPPLAMOUNT' : '00001528');
          endif;

          if (wstype = *blanks and (wsrate <> 0 or supplAmount <> 0));
            // Reimbursement amount cannot be greater than 0 if reimbursement type is blank //
            AddErrorMsg('WSTYPE' : '00001528');
            AddErrorMsg('WSRATE' : '00001528');
            AddErrorMsg('SUPPLAMOUNT' : '00001528');
          endif;

          if wstype = 'Q';
            // Reimbursement amount must be a valid fee schedule if reimbursement type is Q //
            chain (wsrate) hbffschd;
            if not %found(hbpfschd);
              AddErrorMsg('WSTYPE' : '00001528');
              AddErrorMsg('WSRATE' : '00001528');
            endif;
          endif;

          if (wstype <> *blanks);
            //exsr clrtbl;
            //tcode = 'BRTP';
            //ecode = wstype;
            //exsr srtabl;
            //if (tind = 'E');    // Rate type is not valid
            //  AddErrorMsg('WSTYPE' : '00001529');
            //endif;
            if wstype <> '$' and wstype <> 'C' and wstype <> 'D'
               and wstype <> 'F' and wstype <> 'H' and wstype <> 'M'
               and wstype <> 'U' and wstype <> 'Z' and wstype <> 'Q';
              AddErrorMsg('WSTYPE' : '00001529');
            endif;
          endif;

        //--------------------------------------------------------------------//
        // validate the verification status code                              //
        //--------------------------------------------------------------------//
          if (wsvsts <> *blanks);
            exsr clrtbl;
            tcode = 'BSTS';
            ecode = wsvsts;
            exsr srtabl;
            if (tind = 'E');    // Verification code is not valid //
              AddErrorMsg('WSVSTS' : '00001530');
            endif;
          else;    // Verification code is needed //
            AddErrorMsg('WSVSTS' : '00001565');
          endif;

          if (infoOnly <> *blanks);
            exsr clrtbl;
            tcode = 'XYON';
            ecode = infoOnly;
            exsr srtabl;
            if (tind = 'E');    // Checkbox is not valid //
              AddErrorMsg('INFOONLY' : '00001908');
            endif;
          else;    // Checkbox is not valid //
            AddErrorMsg('INFOONLY' : '00001908');
          endif;

        //--------------------------------------------------------------------//
        // check for payor/plan edits on authority number                     //
        // show/hide these fields                                             //
        //--------------------------------------------------------------------//
          if (showWSLOC = 'Y' and wsloc <> *blanks);
            exsr clrtbl;
            tcode = 'BILC';
            ecode = wsloc;
            exsr srtabl;
            if (tind = 'E');    // Verification code is not valid
              wsloc = *blanks;
              AddErrorMsg('WSLOC' : '00001530');
            endif;
          endif;

          if (showWSVBIL = 'Y' and wsvbil <> *blanks);
            exsr clrtbl;
            tcode = '****';
            ecode = wsvbil;
            exsr srtabl;
            if (tind = 'E');    // Voucher Billing code is not valid
              wsvbil = *blanks;
              AddErrorMsg('WSVBIL' : '00001531');
            endif;
          endif;

          if (showWSADCD = 'Y' and wsadcd <> *blanks);
            exsr clrtbl;
            tcode = 'BADC';
            ecode = wsadcd;
            exsr srtabl;
            if (tind = 'E');    // Add-on code is not valid
              wsadcd = *blanks;
              AddErrorMsg('WSADCD' : '00001532');
            endif;
          endif;

        endsr;

         //////////////////////////////////////////////////////////////////////
         begsr ValidateRestriction;
         //////////////////////////////////////////////////////////////////////

           eval-corr periodDS = restrictions(1);

        //--------------------------------------------------------------------//
        // verify requested action can be performed                           //
        //--------------------------------------------------------------------//
          if (option = 'CC' or option = 'DD') and (deleteFlag = 'Yes');
            // Action cannot be performed on deleted restriction //
            AddErrorMsg('OPTION' : '00001577');
            leavesr;
          elseif (option = 'RR' and deleteFlag = '   ');
            // Action cannot be performed on active restriction //
            AddErrorMsg('OPTION' : '00001578');
            leavesr;
          endif;

        //--------------------------------------------------------------------//
        // validate the period                                                //
        //--------------------------------------------------------------------//
          rqsdsc = *blanks;
          if (period <> *blanks);
            exsr clrtbl;
            tcode = 'BDWY';
            ecode = period;
            exsr srtabl;
            if (tind = 'E');    // Requested Period is missing or invalid.
              AddErrorMsg('PERIOD' : '00000755');
              leavesr;
            endif;
          else;    // Requested Period is missing or invalid.
            AddErrorMsg('PERIOD' : '00000755');
            leavesr;
          endif;
          rqsdsc = sdesc;

          hldpsq = %trim(hmap);
          monitor;
            savpsq = %int(hldpsq);
          on-error;
            savpsq = 0;
          endmon;

          select;

          when (option = 'AA' or option = 'CC' or option = 'DD'
                or option = 'RR');
            // check for duplicates at the hmap level for add
            //?if (option = 'C');
            //?  chain ivsvcky hafivsvs;
            //?endif;

            if (option = 'AA' or option ='CC' or option = 'RR');

              // Period restriction effective from cannot be zeros.
              ydate = effectiveFrom;
              xfxcymd(ydate : mdate);
              if (mdate = 0);
                AddErrorMsg('EFFECTIVEFROM' : '00003808');
              endif;

              // Period restriction effective to cannot be zeros.
              ydate = effectiveTo;
              xfxcymd(ydate : mdate);
              if (mdate = 0);
                AddErrorMsg('EFFECTIVETO' : '00003809');
              endif;

              // Period restriction effective from should be less than effective to.
              if (effectiveFrom > effectiveTo);
                  AddErrorMsg('EFFECTIVEFROM' : '00003810');
              endIf;

              // Period restriction effective from & to should not be earlier than group.
              if (effectiveFrom < effDt or effectiveTo < effDt);
                  AddErrorMsg('EFFECTIVEFROM' : '00003812');
              endIf;

              // Period restriction effective from & to should not be earlier than group.
              if (effectiveFrom > expDt or effectiveTo > expDt);
                  AddErrorMsg('EFFECTIVEFROM' : '00003813');
              endIf;

              if (option = 'AA');
                wOldEffFrom = effectiveFrom;
                wOldEffTo = effectiveTo;
              else;
                wOldEffFrom = oldEffFrom;
                wOldEffTo = oldEffTo;
              endIf;

              // period restriction found for given from date or to date
              clear ivsFound;
              if option = 'AA' or option = 'RR' or option = 'CC';
                // check for overlap
                EXEC SQL
                SELECT '1' INTO :ivsFound
                  FROM hapivstr
                 WHERE bstLv6 = :lvl6
                   AND bstAcc = :account
                   AND bstFsq = :fileSeq
                   AND bstDlt = ' '
                   AND (:option IN ('AA','RR')
                        OR (:option = 'CC' AND bstEff <> :wOldEffFrom
                                          AND bstEnd <> :wOldEffTo))
                   AND ((:effectiveFrom < bstEff AND bstEnd < :effectiveTo)
                    OR (:effectiveFrom < bstEff
                              AND :effectiveTo BETWEEN bstEff AND bstEnd)
                    OR (:effectiveFrom BETWEEN bstEff AND bstEnd
                              AND :effectiveTo BETWEEN bstEff AND bstEnd)
                    OR (:effectiveFrom BETWEEN bstEff AND bstEnd
                              AND :effectiveTo > bstEnd))
                 FETCH FIRST 1 ROW ONLY;

                 // if overlap found, throw error
                 if ivsFound and option = 'RR';
                    rtrnCd = '00003811';
                 elseif ivsFound;
                    AddErrorMsg('PERIOD' : '00003811');
                 endif;
              endif;

            endif;

            chain (lvl6: account: fileSeq) hafivsvs;
            if %found(halivsvs) and brvprc <> *blanks;
              xfxproc( lvl6   : brvprc : effectiveFrom   : xfpdsc : xfpamt :
                       xfptxa : xfprev : xfpubc : xfpnub : xfpcpt : xfpovr :
                       xfppfg : xfpnch : xfpbid : xfpvrp : xfpmod : xfpmd2 :
                       xfpmd2 : xfpmd4 : xfpcnv : xfpgfg : xfptim : xfpctx );
            endif;

            select;

        //--------------------------------------------------------------------//
        // checks for Yearly types (HMAP 10 in BDWY)                          //
        // checks for Monthly types (HMAP 20 in BDWY)                         //
        // checks for Weekly types (HMAP 30 in BDWY)                          //
        //--------------------------------------------------------------------//
            when (savpsq = 10 or savpsq = 20 or savpsq = 30);
              if (savpsq = 10);
                // calculate new dates for yearly types
                exsr calcyrdt;
                //if *in31;
                //  leavesr;
                //endif;

                if (dateerror <> *blanks);
                  // A verification already exists for this procedure/date range.
                  AddErrorMsg('RQEFFDT' : '00001567');
                  leavesr;
                endif;
              endif;

              if (units <= 0);
                // Units are either missing or invalid
                AddErrorMsg('UNITS' : '00001568');
                leavesr;
              endif;

              if (units > maxUnits);
                // Units for the period can not be greater than the total units.
                AddErrorMsg('UNITS' : '00001569');
                leavesr;
              endif;

              qnty = units;
              if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                cnvtyp = '1';
                exsr srcvtm;
              else;
                error = 'N';
              endif;
              if (error = 'Y');
                // The units/period entered are invalid.
                AddErrorMsg('UNITS' : '00001570');
                leavesr;
              endif;

              exsr wrttrail;

        //--------------------------------------------------------------------//
        // checks for Daily types (HMAP 40 in BDWY)                           //
        //--------------------------------------------------------------------//
              when (savpsq = 40);    // Check for Daily types (HMAP 40 in BDWY)

                if (period = 'D');    // Daily - number of units
                  if (units <= 0);    // Units are either missing or invalid.
                    AddErrorMsg('UNITS' : '00001568');
                    leavesr;
                  endif;
                  if (units > maxUnits);
                    // Units for the period can not be greater than the total units.
                    AddErrorMsg('UNITS' : '00001569');
                    leavesr;
                  endif;
                  if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                    qnty = units;
                    cnvtyp = '1';
                    exsr srcvtm;
                  else;
                    error = 'N';
                  endif;
                  if (error = 'Y');
                    // The units/period entered are invalid.
                    AddErrorMsg('UNITS' : '00001570');
                    leavesr;
                  endif;

                  exsr wrttrail;
                endif;

                if (period = 'X');    // only certain days

                  // For period type "X", enter an "X" for desired days.
                  if (sunday <> 'X' and sunday <> *blanks);
                    AddErrorMsg('SUNDAY' : '00001571');
                  endif;
                  if (monday <> 'X' and monday <> *blanks);
                    AddErrorMsg('MONDAY' : '00001571');
                  endif;
                  if (tuesday <> 'X' and tuesday <> *blanks);
                    AddErrorMsg('TUESDAY' : '00001571');
                  endif;
                  if (wednesday <> 'X' and wednesday <> *blanks);
                    AddErrorMsg('WEDNESDAY' : '00001571');
                  endif;
                  if (thursday <> 'X' and thursday <> *blanks);
                    AddErrorMsg('THURSDAY' : '00001571');
                  endif;
                  if (friday <> 'X' and friday <> *blanks);
                    AddErrorMsg('FRIDAY' : '00001571');
                  endif;
                  if (saturday <> 'X' and saturday <> *blanks);
                    AddErrorMsg('SATURDAY' : '00001571');
                  endif;
                  if (sunday = *blanks and monday = *blanks
                      and tuesday = *blanks and wednesday = *blanks
                      and thursday = *blanks and friday = *blanks
                      and saturday = *blanks);
                    // For period type "X", at least one day must be marked off.
                    AddErrorMsg('PERIOD' : '00001572');
                    leavesr;
                  endif;

                  exsr wrttrail;
                endif;


                // different units on different days
                if (period = 'U');

                  // Sunday
                  if (sunday <> *blanks);
                    evalr wkwrkd = %trim(sunday);
                    exsr srckperu;
                    evalr sunday = %trim(wkwrkd);
                    qntySunday = qnty;
                    if (error = 'Y');    // Units are either missing or invalid
                      AddErrorMsg('SUNDAY' : '00001568');
                    endif;
                  endif;

                  // Monday
                  if (monday <> *blanks);
                    evalr wkwrkd = %trim(monday);
                    exsr srckperu;
                    evalr monday = %trim(wkwrkd);
                    qntyMonday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('MONDAY' : '00001568');
                    endif;
                  endif;

                  // Tuesday
                  if (tuesday <> *blanks);
                    evalr wkwrkd = %trim(tuesday);
                    exsr srckperu;
                    evalr tuesday = %trim(wkwrkd);
                    qntyTuesday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('TUESDAY' : '00001568');
                    endif;
                  endif;

                  // Wednesday
                  if (wednesday <> *blanks);
                    evalr wkwrkd = %trim(wednesday);
                    exsr srckperu;
                    evalr wednesday = %trim(wkwrkd);
                    qntyWednesday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('WEDNESDAY' : '00001568');
                    endif;
                  endif;

                  // Thursday
                  if (thursday <> *blanks);
                    evalr wkwrkd = %trim(thursday);
                    exsr srckperu;
                    evalr thursday = %trim(wkwrkd);
                    qntyThursday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('THURSDAY' : '00001568');
                    endif;
                  endif;

                  // Friday
                  if (friday <> *blanks);
                    evalr wkwrkd = %trim(friday);
                    exsr srckperu;
                    evalr friday = %trim(wkwrkd);
                    qntyFriday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('FRIDAY' : '00001568');
                    endif;
                  endif;

                  // Saturday
                  if (saturday <> *blanks);
                    evalr wkwrkd = %trim(saturday);
                    exsr srckperu;
                    evalr saturday = %trim(wkwrkd);
                    qntySaturday = qnty;
                    if (error = 'Y');     // Units are either missing or invalid
                      AddErrorMsg('SATURDAY' : '00001568');
                    endif;
                  endif;

                  if (sunday = *blanks and monday = *blanks
                      and tuesday = *blanks and wednesday = *blanks
                      and thursday = *blanks and friday = *blanks
                      and saturday = *blanks);
                    // For period type "U", at least one day must have units.
                    AddErrorMsg('PERIOD' : '00001573');
                    leavesr;
                  endif;

                  exsr wrttrail;
                endif;

        //--------------------------------------------------------------------//
        // checks for Times (HMAP 50 in BDWY)                                 //
        //--------------------------------------------------------------------//
            when (savpsq = 50);

              if (sundayFrom = 0 and sundayTo = 0
                  and mondayFrom = 0 and mondayTo = 0
                  and tuesdayFrom = 0 and tuesdayTo = 0
                  and wednesdayFrom = 0 and wednesdayTo = 0
                  and thursdayFrom = 0 and thursdayTo = 0
                  and fridayFrom = 0 and fridayTo = 0
                  and saturdayFrom = 0 and saturdayTo = 0
              );
                // For period type "T", at least one day must have times.
                AddErrorMsg('PERIOD' : '00001574');
                leavesr;

              else;

                // Sunday
                if ValidTimeRange( 'SUNDAYFROM' : sundayFrom
                                 : 'SUNDAYTO' :sundayTo );
                  mtsund = sundayFrom;
                  mtsun2 = sundayTo;
                else;
                  mtsund = 0;
                  mtsun2 = 0;
                endif;

                // Monday
                if ValidTimeRange( 'MONDAYFROM' : mondayFrom
                                 : 'MONDAYTO' :mondayTo );
                  mtmond = mondayFrom;
                  mtmon2 = mondayTo;
                else;
                  mtmond = 0;
                  mtmon2 = 0;
                endif;

                // Tuesday
                if ValidTimeRange( 'TUESDAYFROM' : tuesdayFrom
                                 : 'TUESDAYTO' : tuesdayTo );
                  mttues = tuesdayFrom;
                  mttue2 = tuesdayTo;
                else;
                  mttues = 0;
                  mttue2 = 0;
                endif;

                // Wednesday
                if ValidTimeRange( 'WEDNESDAYFROM' : wednesdayFrom
                                 : 'WEDNESDAYTO' : wednesdayTo );
                  mtweds = wednesdayFrom;
                  mtwed2 = wednesdayTo;
                else;
                  mtweds = 0;
                  mtwed2 = 0;
                endif;

                // Thursday
                if ValidTimeRange( 'THURSDAYFROM' : thursdayFrom
                                 : 'THURSDAYTO' : thursdayTo );
                  mtthur = thursdayFrom;
                  mtthu2 = thursdayTo;
                else;
                  mtthur = 0;
                  mtthu2 = 0;
                endif;

                // Friday
                if ValidTimeRange( 'FRIDAYFROM' : fridayFrom
                                 : 'FRIDAYTO' :fridayTo );
                  mtfrid = fridayFrom;
                  mtfri2 = fridayTo;
                else;
                  mtfrid = 0;
                  mtfri2 = 0;
                endif;

                // Saturday
                if ValidTimeRange( 'SATURDAYFROM' : saturdayFrom
                                  : 'SATURDAYTO' : saturdayTo );
                  mtsatd = saturdayFrom;
                  mtsat2 = saturdayTo;
                else;
                  mtsatd = 0;
                  mtsat2 = 0;
                endif;

              endif;

              exsr wrttrail;

            endsl;
          endsl;

        endsr;
        /////////////////////////////////////////////////////////////////
        // Build Procedures Grid                                       //
        /////////////////////////////////////////////////////////////////
        begsr BuildProceduresGrid;

         if (showHistory = *blank);
           showHistory = '0';
         endif;
         gridCount = 0;
         clear grid;

         exec sql  declare aaa cursor for
           select
             bsvfsq,
             bsvsvt,
             case
               when bsvprc <> ''
                 then 'P'
               when bsvrvc <> 0
                 then 'R'
               when (bsvptd = 'Y' or bsvotd = 'Y' or bsvspd = 'Y' or
                     bsvswd = 'Y' or bsvbhd = 'Y' or bsvacd = 'Y' or
                     bsvtrd = 'Y' or bsvnrd = 'Y' or bsvcmd = 'Y' or
                     bsvvcd = 'Y' or bsvphd = 'Y' or bsvtcd = 'Y')
                 then 'D'
             end,
             case
               when bsvprc <> ''
                 then 'Procedure Code'
               when bsvrvc <> 0
                 then 'Revenue Code'
               when (bsvptd = 'Y' or bsvotd = 'Y' or bsvspd = 'Y' or
                     bsvswd = 'Y' or bsvbhd = 'Y' or bsvacd = 'Y' or
                     bsvtrd = 'Y' or bsvnrd = 'Y' or bsvcmd = 'Y' or
                     bsvvcd = 'Y' or bsvphd = 'Y' or bsvtcd = 'Y')
                 then 'Discipline'
             end,
             bsvprc,
             //xfpdsc,
             ' ',
             bsvrvc,
             ' ',
             bsvptd,
             bsvotd,
             bsvspd,
             bsvswd,
             bsvbhd,
             bsvacd,
             bsvtrd,
             bsvnrd,
             bsvcmd,
             bsvvcd,
             bsvphd,
             bsvtcd,
             ' ',
             bsvcov - bsvsup,
             bsvgpn,
             bsvnam,
             bsvaut,
             bsvac#,
             bsvlen,
             bsvltp,
             case when bdws.xfdlds is not null then bdws.xfdlds else ' ' end,
             bsveff,
             bsvend,
             bsvrtp,
             case when brtp.xfdlds is not null then brtp.xfdlds else ' ' end,
             bsvram - bsvsam,
             bsvsup,
             bsvsam,
             bsvsts,
             case when bsts.xfdlds is not null then bsts.xfdlds else ' ' end,
             'N',
             ' ',
             'N',
             ' ',
             'N',
             ' ',
             bsvcmt,
             bsvdlt,
             bsvcrb,
             bsvcrd,
             bsvchb,
             bsvchd,
             bsvioa

             from halivsvp
             //inner join hxpprocm on bsvprc=xfproc
             //left outer join halirnks on bsvlv6=brklv6 and bsvacc=brkacc
             //                       and bsvfsq=brkseq and bsveff >= brkeff
             left outer join hxptabld bsts
                                    on bsts.xfdtcd='BSTS' and bsts.xfdecd=bsvsts
             left outer join hxptabld brtp
                                    on brtp.xfdtcd='BRTP' and brtp.xfdecd=bsvrtp
             left outer join hxptabld bdws
                                    on bdws.xfdtcd='BDWS' and bdws.xfdecd=bsvltp

             where bsvlv6 = :lvl6
               and bsvacc = :account
               and bsvseq = :rank
               and bsvend >= case when :showHistory = '0' then :today
                                  else 00010101 end
             order by bsvfsq
             for read only
         ;

         x = 0;
         gridCount = 0;
         //clear procedures;
         exec sql  open aaa;
         exec sql  fetch from aaa into :gridDS;
         dow (sqlcod = 0);
           x += 1;
           gridCount = x;

           // convert units back to original
           xfxproc( lvl6   : wsProc : wseffd : xfpdsc : xfpamt : xfptxa :
                    xfprev : xfpubc : xfpnub : xfpcpt : xfpovr : xfppfg :
                    xfpnch : xfpbid : xfpvrp : xfpmod : xfpmd2 : xfpmd2 :
                    xfpmd4 : xfpcnv : xfpgfg : xfptim : xfpctx );
           if xfppfg <> 'Y' and xfpcnv <> *blanks;
             quantity = wsUnit;
             xfxcvtim( quantity : xfpcnv : '2' : rtncod );
             wsUnit = quantity;
             quantity = supplUnits;
             xfxcvtim( quantity : xfpcnv : '2' : rtncod );
             supplUnits = quantity;
           endif;

           eval-corr grid(x) = gridDS;
           if grid(x).wsproc <> *blanks;
             grid(x).wsprocd = xfpdsc;
           else;
             chain (grid(x).wsrevc) hbfbrev;
             if %found(hbpbrev);
               grid(x).wsrevcd = brvdsc;
             endif;
           endif;

           if grid(x).acDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'AC');
           endif;

           if grid(x).bhDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'BH');
           endif;

           if grid(x).cmDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'CM');
           endif;

           if grid(x).nrDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'NR');
           endif;

           if grid(x).otDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'OT');
           endif;

           if grid(x).phDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'PH');
           endif;

           if grid(x).ptDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'PT');
           endif;

           if grid(x).spDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'SP');
           endif;

           if grid(x).swDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'SW');
           endif;

           if grid(x).tcDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'TC');
           endif;

           if grid(x).trDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'TR');
           endif;

           if grid(x).vcDiscipline = 'Y';
             grid(x).disciplineDesc = getDiscplnDesc(grid(x).disciplineDesc
                                                     : 'VC');
           endif;

        //--------------------------------------------------------------------//
        // show/hide the following fields                                     //
        //--------------------------------------------------------------------//
           prmnam = 'IVSVCLOC';
           prmval = *blanks;
           exsr exedit;
           if (prmcod <> *blanks);
             showWSLOC = 'Y';
           endif;

           prmnam = 'IVSVCVB';
           prmval = *blanks;
           exsr exedit;
           if (prmcod <> *blanks);
             showWSVBIL = 'Y';
           endif;

           prmnam = 'IVSVCADCOD';
           prmval = *blanks;
           exsr exedit;
           if (prmcod <> *blanks);
             showWSADCD = 'Y';
           endif;

           exec sql  fetch next from aaa into :gridDS;
         enddo;
         exec sql  close aaa;


        endsr;

        //////////////////////////////////////////////////////////////////////
        begsr BuildRestrictionsGrid;
        //////////////////////////////////////////////////////////////////////

          xx = 0;
          restCount = 0;
          clear  restrictions;

          // grab period restrictions
          //setll (lvl6 : account : fileSeq) hapivstr;
          //reade (lvl6 : account : fileSeq) hapivstr;
          setll (lvl6 : account) hapivstr;
          reade (lvl6 : account) hapivstr;
          dow not %eof(hapivstr);
            xx += 1;
            restCount = xx;

            restrictions(xx).fileSeq = bstfsq;
            restrictions(xx).period = bstper;
            exsr clrtbl;
            tcode = 'BDWY';
            ecode = bstper;
            exsr srtabl;
            if (tind <> 'E');
              restrictions(xx).periodD = ldesc;
            endif;
            restrictions(xx).deleteFlag = bstdlt;

            chain (bstlv6: bstacc: bstfsq) hafivsvs;
            if %found(halivsvs) and brvprc <> *blanks;
              xfxproc( bstlv6 : brvprc : bsteff : xfpdsc : xfpamt : xfptxa :
                       xfprev : xfpubc : xfpnub : xfpcpt : xfpovr : xfppfg :
                       xfpnch : xfpbid : xfpvrp : xfpmod : xfpmd2 : xfpmd2 :
                       xfpmd4 : xfpcnv : xfpgfg : xfptim : xfpctx );
            endif;
            if (xfppfg <> 'Y' and xfpcnv <> *blanks);
              quantity = bstmax;
              xfxcvtim( quantity : xfpcnv : '2' : rtncod );
              restrictions(xx).units = quantity;
            else;
              restrictions(xx).units = bstmax;
            endif;

            if (restrictions(xx).period = 'X');
              restrictions(xx).sunday = btsunx;
              restrictions(xx).monday = btmonx;
              restrictions(xx).tuesday = bttuex;
              restrictions(xx).wednesday = btwedx;
              restrictions(xx).thursday = btthrx;
              restrictions(xx).friday = btfrix;
              restrictions(xx).saturday = btsatx;

            elseif (restrictions(xx).period ='U'
                or restrictions(xx).period = 'D');

              quantity = 0;
              if btsunu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btsunu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).sunday = %editc(quantity:'3');
                else;
                  restrictions(xx).sunday = %editc(btsunu:'3');
                endif;
              endif;
              if (restrictions(xx).period = 'D');
                restrictions(xx).units = quantity;
              endIf;

              quantity = 0;
              if btmonu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btmonu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).monday = %editc(quantity:'3');
                else;
                  restrictions(xx).monday = %editc(btmonu:'3');
                endif;
              endif;

              quantity = 0;
              if bttueu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = bttueu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).tuesday = %editc(quantity:'3');
                else;
                  restrictions(xx).tuesday = %editc(bttueu:'3');
                endif;
              endif;

              quantity = 0;
              if btwedu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btwedu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).wednesday = %editc(quantity:'3');
                else;
                  restrictions(xx).wednesday = %editc(btwedu:'3');
                endif;
              endif;

              quantity = 0;
              if btthru <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btthru;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).thursday = %editc(quantity:'3');
                else;
                  restrictions(xx).thursday = %editc(btthru:'3');
                endif;
              endif;

              quantity = 0;
              if btfriu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btfriu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).friday = %editc(quantity:'3');
                else;
                  restrictions(xx).friday = %editc(btfriu:'3');
                endif;
              endif;

              quantity = 0;
              if btsatu <> 0;
                if (xfppfg <> 'Y' and xfpcnv <> *blanks);
                  quantity = btsatu;
                  xfxcvtim( quantity : xfpcnv : '2' : rtncod );
                  restrictions(xx).saturday = %editc(quantity:'3');
                else;
                  restrictions(xx).saturday = %editc(btsatu:'3');
                endif;
              endif;

            elseif (restrictions(xx).period = 'T');
              restrictions(xx).sundayFrom = btsunf;
              restrictions(xx).sundayTo = btsunt;
              restrictions(xx).mondayFrom = btmonf;
              restrictions(xx).mondayTo = btmont;
              restrictions(xx).tuesdayFrom = bttuef;
              restrictions(xx).tuesdayTo = bttuet;
              restrictions(xx).wednesdayFrom = btwedf;
              restrictions(xx).wednesdayTo = btwedt;
              restrictions(xx).thursdayFrom = btthrf;
              restrictions(xx).thursdayTo = btthrt;
              restrictions(xx).fridayFrom = btfrif;
              restrictions(xx).fridayTo = btfrit;
              restrictions(xx).saturdayFrom = btsatf;
              restrictions(xx).saturdayTo = btsatt;
            endif;

            restrictions(xx).createdBy = bstcrb;
            restrictions(xx).createdDate = bstcrd;
            restrictions(xx).changedBy = bstchb;
            restrictions(xx).changedDate = bstchd;
            restrictions(xx).oldEffFrom = bsteff;
            restrictions(xx).oldEffTo = bstend;
            restrictions(xx).effectiveFrom = bsteff;
            restrictions(xx).effectiveTo = bstend;

          //reade (lvl6 : account : fileSeq) hapivstr;
            reade (lvl6 : account) hapivstr;
          enddo;

        endsr;
        /////////////////////////////////////////////////////////////////
        // Add or Copy a procedure                                     //
        /////////////////////////////////////////////////////////////////
        begsr AddCopyProcedure;

          nextSeq = 0;
          // get the next sequence number for procedure //
          exec sql
            select
              max(bsvfsq)
            into :nextSeq
            from hapivsvc
            where bsvlv6 = :lvl6
              and bsvacc = :account
          ;
          nextSeq += 1;

          if wsproc <> *blanks;
            // convert units if necessary
            if (xfpcnv <> *blanks and xfppfg <> 'Y' and
                (wsunit + supplUnits) < 99999);
              quantity = wsUnit;
              xfxcvtim( quantity : xfpcnv : '1' : rtncod );
              wsUnit = quantity;
              quantity = supplUnits;
              xfxcvtim( quantity : xfpcnv : '1' : rtncod );
              supplUnits = quantity;
            endif;
          endif;

          bsvlv6 = lvl6;
          bsvacc = account;
          bsvmrn = medRcd;
          bsvpyr = payor;
          bsvpln = plan;
          bsvpol = policy;
          bsvfsq = nextSeq;
          bsvseq = rank;
          bsveff = effectDate;
          bsvend = expireDate;
          bsvcov = wsunit + supplUnits;
          bsvsup = supplUnits;
          bsvsvt = svcVerifyType;
          if bsvsvt = 'S';
            bsvprc = wsproc;
            bsvrvc = wsrevc;
            bsvptd = 'N';
            bsvotd = 'N';
            bsvspd = 'N';
            bsvswd = 'N';
            bsvbhd = 'N';
            bsvacd = 'N';
            bsvtrd = 'N';
            bsvnrd = 'N';
            bsvcmd = 'N';
            bsvvcd = 'N';
            bsvphd = 'N';
            bsvtcd = 'N';
          elseif bsvsvt = 'V';
            bsvptd = ptDiscipline;
            bsvotd = otDiscipline;
            bsvspd = spDiscipline;
            bsvswd = swDiscipline;
            bsvbhd = bhDiscipline;
            bsvacd = acDiscipline;
            bsvtrd = trDiscipline;
            bsvnrd = nrDiscipline;
            bsvcmd = cmDiscipline;
            bsvvcd = vcDiscipline;
            bsvphd = phDiscipline;
            bsvtcd = tcDiscipline;
            bsvprc = *blanks;
            bsvrvc = 0;
          endif;
          bsvgpn = wsgrpn;
          bsvnam = wsgrpm;
          bsvaut = wsauth;
          bsvrtp = wstype;
          bsvram = wsrate + supplAmount;
          bsvsam = supplAmount;
          bsvac# = wsaucn;
          bsvova = *blanks;
          bsvlen = wslngt;
          bsvltp = wsltyp;
          bsvsts = wsvsts;
          bsvioa = infoOnly;
          bsvhst = *blanks;
          bsvcmt = wscmnt;
          bsvcrb = ldausr;
          bsvcrd = today;
          bsvcrt = #time;
          bsvchb = *blanks;
          bsvchd = 0;
          bsvcht = 0;
          bsvdlt = wsdlt;
          bsvdby = *blanks;
          bsvddt = 0;
          bsvdtm = 0;
          bsvftd = 0;
          bsvfam = 0;
          bsvfut = *blanks;
          bsvloc = wsloc;
          bsvvbl = wsvbil;
          bsvadc = wsadcd;

          write hafivsvc;
          reprorateFlag = 'Y';

          if ((svcVerifyType = 'S' and wsProc <> '') or
               svcVerifyType = 'V');
            sendInfoToWRM(lvl6 : account : nextSeq : WRMerror);
          endif;

          chain (rqrsid : wrkseq) erferesp;
          if (%found(erlerespr) and ersptp <> 'P');
            ersptp = 'P';
            erspby = ldausr;
            erspdt = #date;
            ersptm = #time;
            update erferesp;
          endif;

        endsr;
        /////////////////////////////////////////////////////////////////
        // Change Record                                               //
        /////////////////////////////////////////////////////////////////
        begsr ChangeProcedure;

          count = 0;

          chain(e) (lvl6 : account : fileSeq) halivsvs;
          if %status = 1218;   // record in use
            AddErrorMsg('RECORD' : '00000051');
          elseif %found(halivsvs);
            if wsproc <> *blanks;
              // convert units if necessary
              if (xfpcnv <> *blanks and xfppfg <> 'Y'
                  and (wsunit + supplUnits) < 99999);
                quantity = wsUnit;
                xfxcvtim( quantity : xfpcnv : '1' : rtncod );
                wsUnit = quantity;
                quantity = supplUnits;
                xfxcvtim( quantity : xfpcnv : '1' : rtncod );
                supplUnits = quantity;
              endif;
            endif;

            // check for yearly types in period restrictions file
            // and adjust effective and expiration dates accordingly
            chain (lvl6 : account : fileSeq : 10) halivstr;
            if %found(halivstr);
              if changeOK;
                effDt = grid(1).wseffd;
                expDt = grid(1).wsendd;
                period = bstper;
                exsr calcyrdt;
                wslngt = 0;
                wsltyp = *blanks;
                effectDate = wrkfrm;
                expireDate = wrkend;
              else;
                workDate = effectDate;
                warnMsg = 'Dates will be changed to match yearly period '
                        + 'restrictions of ' + %char(workMonth) + '/'
                        + %char(workDay) + '/' + %char(workYear)
                ;
                workDate = expireDate;
                warnMsg = %trim(warnMsg) + ' thru ' + %char(workMonth) + '/'
                        + %char(workDay) + '/' + %char(workYear) + '.  '
                        + 'Continue to confirm changes.'
                ;
              endif;
            endif;

            if (brvcov <> (wsunit + supplUnits));
              reprorateFlag = 'Y';
            endif;

            if (wsproc <> brvprc or wsgrpn <> brvgpn
             or effectDate <> brveff or expireDate <> brvend
             or (wsrate + supplAmount) <> brvram or wstype <> brvrtp
             or wsrevc <> brvrvc or ptDiscipline <> brvptd
             or otDiscipline <> brvotd or ptDiscipline <> brvspd
             or swDiscipline <> brvswd or bhDiscipline <> brvbhd
             or acDiscipline <> brvacd or trDiscipline <> brvtrd
             or nrDiscipline <> brvnrd or cmDiscipline <> brvcmd
             or vcDiscipline <> brvvcd or phDiscipline <> brvphd
             or tcDiscipline <> brvtcd or infoOnly <> brvIOA);
              reprorateFlag = 'Y';
            endif;

            if (option = 'R');
              reprorateFlag = 'Y';
            endif;

            if reprorateFlag <> 'Y' and wsauth <> brvaut;
              exsr clrtbl;
              tcode = 'BDFT';
              ecode = brvaut;
              exsr srtabl;
              if (tind <> 'E');
                exsr clrtbl;
                tcode = 'BDFT';
                ecode = wsauth;
                exsr srtabl;
                if tind = 'E';
                  reprorateFlag = 'Y';
                endif;
              endif;
            endif;

            brveff = effectDate;
            brvend = expireDate;
            brvcov = wsunit + supplUnits;
            brvsup = supplUnits;
            brvsvt = svcVerifyType;
            if brvsvt = 'S';
              brvprc = wsproc;
              brvrvc = wsrevc;
              brvptd = 'N';
              brvotd = 'N';
              brvspd = 'N';
              brvswd = 'N';
              brvbhd = 'N';
              brvacd = 'N';
              brvtrd = 'N';
              brvnrd = 'N';
              brvcmd = 'N';
              brvvcd = 'N';
              brvphd = 'N';
              brvtcd = 'N';
            elseif brvsvt = 'V';
              brvptd = ptDiscipline;
              brvotd = otDiscipline;
              brvspd = spDiscipline;
              brvswd = swDiscipline;
              brvbhd = bhDiscipline;
              brvacd = acDiscipline;
              brvtrd = trDiscipline;
              brvnrd = nrDiscipline;
              brvcmd = cmDiscipline;
              brvvcd = vcDiscipline;
              brvphd = phDiscipline;
              brvtcd = tcDiscipline;
              brvprc = *blanks;
              brvrvc = 0;
            endif;
            brvrtp = wstype;
            brvgpn = wsgrpn;
            brvnam = wsgrpm;
            brvaut = wsauth;
            brvram = wsrate + supplAmount;
            brvsam = supplAmount;
            brvac# = wsaucn;
            brvlen = wslngt;
            brvltp = wsltyp;
            brvsts = wsvsts;
            brvioa = infoOnly;
            brvcmt = wscmnt;
            brvchb = ldausr;
            brvchd = #date;
            brvcht = #time;
            brvdlt = ' ';
            brvloc = wsloc;
            brvvbl = wsvbil;
            brvadc = wsadcd;

            update hafivsvs;

            if ((svcVerifyType = 'S' and wsProc <> '') or
                 svcVerifyType = 'V');
              sendInfoToWRM(lvl6 : account : fileSeq : WRMerror);
            endif;
          endif;

          chain (rqrsid : wrkseq) erferesp;
          if (%found(erlerespr) and ersptp <> 'P');
            ersptp = 'P';
            erspby = ldausr;
            erspdt = #date;
            ersptm = #time;
            update erferesp;
          endif;

        endsr;
        /////////////////////////////////////////////////////////////////
        // Mark record as deleted                                      //
        /////////////////////////////////////////////////////////////////
        begsr DeleteProcedure;

          chain(e) (lvl6 : account : rank : effectDate : payor
                   : plan : policy : wsproc : wsrevc : fileSeq) hapivsvc;
          if %status = 1218;   // record in use
            AddErrorMsg('RECORD' : '00000051');
          elseif %found(hapivsvc);
            bsvdby = ldausr;
            bsvddt = #date;
            bsvdtm = #time;
            bsvdlt = 'D';

            update hafivsvc;
            reprorateFlag = 'Y';

            if ((svcVerifyType = 'S' and wsProc <> '') or
                 svcVerifyType = 'V');
              sendInfoToWRM(lvl6 : account : fileSeq : WRMerror);
            endif;
          endif;

        endsr;
        /////////////////////////////////////////////////////////////////
        // Reinstate Record                                            //
        /////////////////////////////////////////////////////////////////
        begsr ReinstateProcedure;

          exsr clrtbl;
          edate = today;
          tcode = 'BSAS';
          ecode = wsltyp;
          exsr srtabl;
          //if (tind = 'E');
          //  wsltyp = *blanks;
            // Verification type is not valid
          //  AddErrorMsg('WSLTYP' : '00001526');
          //endif;

      /end-free
     c**
     c     ivsvcky       klist
     c                   kfld                    lvl6
     c                   kfld                    account
     c                   kfld                    fileSeq

     c     ivstrky2      klist
     c                   kfld                    lvl6
     c                   kfld                    account
     c                   kfld                    fileSeq
     c                   kfld                    savpsq

     c     ivstrky3      klist
     c                   kfld                    lvl6
     c                   kfld                    account
     c                   kfld                    fileSeq
     c                   kfld                    savpsq
     c                   kfld                    period
     c                   kfld                    wOldEffFrom

     c     ivsvk3        klist
     c                   kfld                    mmplv6
     c                   kfld                    mmacct
     c                   kfld                    brkseq
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    brkply
     c                   kfld                    wsproc
     c                   kfld                    effectDate
     c**
     c     ivsvk4        klist
     c                   kfld                    mmplv6
     c                   kfld                    mmacct
     c                   kfld                    brkseq
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    brkply
     c                   kfld                    wsproc

     c     ivsvk5        klist
     c                   kfld                    lvl6
     c                   kfld                    account
     c                   kfld                    rank
     c                   kfld                    brvpyr
     c                   kfld                    brvpln
     c                   kfld                    brvpol
     c                   kfld                    brvprc

     c     ivsvrk3       klist
     c                   kfld                    mmplv6
     c                   kfld                    mmacct
     c                   kfld                    brkseq
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    brkply
     c                   kfld                    wsrevc
     c                   kfld                    effectDate
     c**
     c     ivsvrk4       klist
     c                   kfld                    mmplv6
     c                   kfld                    mmacct
     c                   kfld                    brkseq
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    brkply
     c                   kfld                    wsrevc

     c     ivsvrk5       klist
     c                   kfld                    lvl6
     c                   kfld                    account
     c                   kfld                    rank
     c                   kfld                    brvpyr
     c                   kfld                    brvpln
     c                   kfld                    brvpol
     c                   kfld                    brvrvc

     c     pdemk1        klist
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c**
     c     pdemk2        klist
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    effectDate
     c**
     c     inxrky        klist
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    xfpct1
     c                   kfld                    xfpct2
     c                   kfld                    xfpubc
     c                   kfld                    xfpfdt
     c**
     c     inexky        klist
     c                   kfld                    brkubc
     c                   kfld                    brkpln
     c                   kfld                    xfpct1
     c                   kfld                    xfpct2
     c                   kfld                    wsproc
     c                   kfld                    xfpfdt
     c**
     c                   if        wsproc <> *blanks
     c     ivsvk3        setgt     hafivsvr
     c     ivsvk4        reade     hafivsvr                               79    =check insurance
     c                   else
     c     ivsvrk3       setgt     hafivsvrr
     c     ivsvrk4       reade     hafivsvrr                              79    =check insurance
     c                   endif
1    c                   if        *in79 = *off                                  verification
     c                   eval      ydays = 1                                     for other files
     c                   eval      ydate1 = tsveff                               to determine
     c                   exsr      srmind                                        end date.
     c                   eval      enddte = ydate2
     c                   else
     c                   eval      enddte = brkedt
E1   c                   endif

     c     pdemk2        setll     hbfpdiem                                     =check all contracts
1    c                   dou       *in71 = *on                                   that are applicaale
     c     pdemk1        reade     hbfpdiem                               71
2    c                   if        *in71 = *off
     c                             and xfpdlt = *blanks
3    c                   if        xfpief = 'I'                                 =included
     c                             or xfpief = 'J'                               procedures
     c                             or xfpief = 'K'
     c                             or xfpief = 'L'
4    c                   if        xfpct1 = mmpct1                              =check categories
     c                             or xfpct1 = *blank
5    c                   if        xfpct2 = mmpct2
     c                             or xfpct2 = *blank
6    c                   if        effectDate <= xfptdt                         =if in date range
6    c                             and expireDate >= xfpfdt
     c     inexky        chain     hxfinex                            79        =check in/ex file
7    c                   if        *in79 = *on
      **
     c     mmpct1        chain     hxfctg1
     c                   if        %found(hxpctg1)
     c                             and (hxprty = 'O' or hxprty = 'E')
     c                             and xfpnub <> 0
     c                   eval      xfpubc = xfpnub
     c                   endif
      **
     c     inxrky        chain     hxfinexr                           79        =check in/ex rev
8    c                   if        *in79 = '1'                                   file
     c                   eval      *in16 = '1'                                  =error if not
     c                   eval      *in20 = '1'                                  =error if not
     c                   eval      errflg = 'Y'                                  there
E8   c                   endif
E7   c                   endif
E6   c                   endif
E5   c                   endif
E4   c                   endif
X3   c                   else
4    c                   if        xfpief = 'E'                                 =excluded
     c                             or xfpief = 'P'                               procedures
     c                             or xfpief = 'S'
     c                             or xfpief = 'X'
5    c                   if        xfpct1 = mmpct1                              =check categories
     c                             or xfpct1 = *blank
6    c                   if        xfpct2 = mmpct2
     c                             or xfpct2 = *blank
     c                   if        effectDate <= xfptdt                         =if in date range
     c                             and expireDate >= xfpfdt
     c     inexky        chain     hxfinex                            79        =check in/ex file
8    c                   if        *in79 = '0'
     c                   eval      *in16 = '1'                                  =error if it does
     c                   eval      *in21 = '1'
     c                   eval      errflg = 'Y'                                  exists
E8   c                   endif
      **
     c     mmpct1        chain     hxfctg1
     c                   if        %found(hxpctg1)
     c                             and (hxprty = 'O' or hxprty = 'E')
     c                             and xfpnub <> 0
     c                   eval      xfpubc = xfpnub
     c                   endif
      **
     c     inxrky        chain     hxfinexr                           79        =check in/ex rev
8    c                   if        *in79 = '0'                                   file
     c                   eval      *in16 = '1'                                  =error if it does
     c                   eval      *in22 = '1'                                  =error if it does
     c                   eval      errflg = 'Y'                                  exists
E8   c                   endif
E7   c                   endif
E6   c                   endif
E5   c                   endif
E4   c                   endif
E3   c                   endif
E2   c                   endif
E1   c                   enddo

      /free

          exsr ChangeProcedure;
          reprorateFlag = 'Y';

        endsr;
        /////////////////////////////////////////////////////////////////
        // Re Pro-rate                                                 //
        /////////////////////////////////////////////////////////////////
        begsr Reprorate;

          if (reprorateDate = 0);
            reprorateDate = effectDate;
          elseif (effectDate < reprorateDate);
            reprorateDate = effectDate;
          endif;

          atbtch = 500000 + ldabch;
          chain (lvl6 : account : atbtch) haltrno7;
          if %found(haltrno7);
            if (reprorateDate < atrprd and reprorateDate >= mmaddt);
              atrprd = reprorateDate;
              update haftrno7;
            endif;
          else;
            atrpro = 'Y';
            if (reprorateDate >= mmaddt);
              atrprd = reprorateDate;
            else;
              atrprd = mmaddt;
            endif;
            atcode = 07;
            atcomm = *blanks;
            atsdte = 0;
            ataamt = 0;
            atmsg = 0;
            atduno = 0;
            atagcy = 0;
            atrebl = *blanks;
            ataprv = *blanks;

            write haftrno7;
          endif;

        endsr;

        /////////////////////////////////////////////////////////////////
        // Check for yearly trailer file                               //
        /////////////////////////////////////////////////////////////////
        begsr CheckForYearly;

          // check for yearly in trailer file
          setll (lvl6 : account : fileSeq) halivstr;
          reade (lvl6 : account : fileSeq) halivstr;
          dow not %eof(halivstr);
            if (bxtpsq = 10);
              savyear = bxtper;
            endif;
            if (bxtmax > savunits);
              savunits = bxtmax;
            endif;
            if (bxsunu > savunits);
              savunits = bxsunu;
            endif;
            if (bxmonu > savunits);
              savunits = bxmonu;
            endif;
            if (bxtueu > savunits);
              savunits = bxtueu;
            endif;
            if (bxwedu > savunits);
              savunits = bxwedu;
            endif;
            if (bxthru > savunits);
              savunits = bxthru;
            endif;
            if (bxfriu > savunits);
              savunits = bxfriu;
            endif;
            if (bxsatu > savunits);
              savunits = bxsatu;
            endif;

            reade (lvl6 : account : fileSeq) halivstr;
          enddo;

          // convert units from trailer file
          if (savunits <> 0);
            if xfppfg <> 'Y' and xfpcnv <> *blanks;
              quantity = savUnits;
              xfxcvtim( savunits : xfpcnv : '2' : rtncod );
              savUnits = quantity;
            endif;
          endif;

          if (wsunit < savunits and savunits <> 0);
            AddErrorMsg('WSUNIT' : '00000371');
          endif;

        endsr;
      /end-free
     c*****************************************************************
     c** check for payor/plan edits on auth #
     c*****************************************************************
     c     exedit        begsr
     c**
     c                   call      'XFXEDITS'
     c                   parm      lvl6          prmlv6
     c                   parm      account       prmacc
     c                   parm                    prmval
     c                   parm                    prmnam
     c                   parm      payor         prmpyr
     c                   parm      plan          prmpln
     c                   parm      policy        prmpol
     c                   parm      rank          prmisq
     c                   parm      mmpct1        prmct1
     c                   parm      mmpct2        prmct2
     c                   parm      wseffd        prmdte
     c                   parm      0             prmbyr                         =Birth Year
     c                   parm      *blanks       prmResp
     c                   parm      *blanks       prmApp
     c                   parm      0             prmgdt
     c                   parm      *blanks       prmmsg
     c                   parm      *blank        prmcod
     c**
     c                   eval      prmnam = *blanks
     c**
     c                   endsr
     c*****************************************************************
     c     clrtbl        begsr
     c**
     c                   eval      tcode = *blanks                              =table code
     c                   eval      ecode = *blanks                              =table entry
     c                   eval      hmap = *blanks                               =hcs mapping
     c                   eval      edate = 0                                    =dlt date
     c                   eval      sdesc = *blanks                              =short desc
     c                   eval      ldesc = *blanks                              =long desc
     c                   eval      tind = *blanks                               =valid entry
     c**
     c                   endsr
     c*****************************************************************
     c     srtabl        begsr
     c**
     c                   call      'XFXTABL'
     c                   parm                    tcode                          =table code
     c                   parm                    ecode                          =entry code
     c                   parm                    hmap                           =hcs map cde
     c                   parm                    edate                          =date
     c                   parm                    sdesc                          =short desc
     c                   parm                    ldesc                          =long desc
     c                   parm                    tind                           =valid flag
     c**
     c                   endsr
     c*****************************************************************
     c     sraddd        begsr
     c**
     c                   call      'XFXADDD'                                    =minus days
     c                   parm                    ydate1
     c                   parm                    ydays
     c                   parm                    ydate2
     c**
     c                   endsr
     c*****************************************************************
     c     srmind        begsr
     c**
     c                   call      'XFXMIND'                                    =minus days
     c                   parm                    ydate1
     c                   parm                    ydays
     c                   parm                    ydate2
     c**
     c                   endsr
     c*****************************************************************
     c**********************************************************************
     c     calcyrdt      begsr                                                  =self pay verific

     c                   z-add     effDt         wrkfrm
     c                   z-add     expDt         wrkend
     c                   eval      *in30 = '0'

     c                   select
     c** Calendar Year
     c                   when      period = 'Y'
     c                   z-add     expDt         wrkend
     c                   move      '0101'        wrkfrm
     c                   move      '1231'        wrkend
     c** Fiscal Year
     c                   when      period = 'F'
     c                   if        xfbfcs = 0
     c** Fiscal year information is not set up for this payor/plan.
     c                   callp     AddErrorMsg('PERIOD' : '00001575')
     c                   leavesr
     c                   else
     c                   z-add     xfbfcs        fcsstr
     c                   move      fcsstr        wrkfrm
      /free
        monitor;
        wrkend = %int(%char(%date(wrkfrm:*iso) + %years(1) - %days(1) : *iso0));
        on-error;
        wrkend = 0;
        endmon;
      /end-free
     c                   endif
     c** Clinical Year
     c                   when      period = 'C'
      /free
        monitor;
        wrkend = %int(%char(%date(wrkfrm:*iso) + %years(1) - %days(1) : *iso0));
        on-error;
        wrkend = 0;
        endmon;
      /end-free
     c** IPC Year
     c                   when      period = 'I'
      /free
        monitor;
        wrkfrm = %int(%char(%date(wrkend:*iso) - %years(1) + %days(1) : *iso0));
        on-error;
        wrkfrm = 0;
        endmon;
      /end-free
     c                   endsl

     c                   if        brvprc <> *blanks
     c     ivsvk5        setll     hafivsvr
     c                   else
     c     ivsvrk5       setll     hafivsvrr
     c                   endif
     c                   dou       *in79 = *on
     c                   if        brvprc <> *blanks
     c     ivsvk5        reade     hafivsvr                               79     record
     c                   else
     c     ivsvrk5       reade     hafivsvrr                              79     record
     c                   endif
     c                   if        *in79 = *off
     c                             and brvfsq <> tsvfsq
     c                   eval      ydate = wrkfrm
     c                   callp     xfxcymd(ydate : mdate)
     c                   if        mdate = 0
     c                   eval      chkfrm = 0
     c                   else
     c                   eval      chkfrm = ydate
     c                   endif
     c                   eval      ydate = wrkend
     c                   callp     xfxcymd(ydate : mdate)
     c                   if        mdate = 0
     c                   eval      chkto = 0
     c                   else
     c                   eval      chkto = ydate
     c                   endif
     c                   if        chkfrm >= tsveff
     c                             and chkto <= tsvend
     c                             or chkfrm >= tsveff
     c                             and chkto <= tsvend
     c                             or chkfrm <= tsveff
     c                             and chkto >= tsvend
     c                   eval      dateerror = 'Y'
     c                   leavesr
     c                   endif
     c                   endif
     c                   enddo

     c                   if        wrkfrm <> effDt or wrkend <> expDt
     c                   eval      newdates = 'Y'
     c                   endif

     c     endyear       endsr
     c*****************************************************************
     c*****************************************************************
     c     srckperu      begsr

     c                   eval      error = *blanks

     c** decimal not found
     c                   if        %scan('.':wkwrkd) = 0
     c                   eval      hldday = %trim(wkwrkd) + '.' + '00'
     c                   evalr     wkwrkd = %trim(hldday)
     c                   evalr     wknumb = %subst(wkwrkd:1:%scan('.':wkwrkd)-1)
     c                   eval      wkdec = %subst(wkwrkd:%scan('.':wkwrkd)+ 1)
     c                   else
     c** decimal found
     c                   eval      wkconst = '.'
     c                   evalr     wknumb = %subst(wkwrkd:1:%scan('.':wkwrkd)-1)
     c                   eval      wkdec = %subst(wkwrkd:%scan('.':wkwrkd)+ 1)
     c                   if        hlddec1 = *blanks
     c                   eval      hlddec1 = '0'
     c                   endif
     c                   if        hlddec2 = *blanks
     c                   eval      hlddec2 = '0'
     c                   endif
     c                   evalr     wkwrkd = %trim(wkunits)
     c                   endif
     c** check conversion factor
     c                   evalr     wkqtyw = %subst(wkwrkd:1:%scan('.':wkwrkd)-1)
     c                   evalr     wkqtyd = %subst(wkwrkd:%scan('.':wkwrkd)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   else
     c                   if        wkqtydn <> 0
     c                   eval      error = 'Y'
     c                   endif
     c                   endif

     c                   if        wkqnty > maxUnits
     c                   eval      error = 'Y'
     c** Units for the period can not be greater than the total units.
     c                   callp     AddErrorMsg('UNITS' : '00001569')
     c                   endif

     c                   endsr
     c**********************************************************************
     c*****************************************************************
     c     srcvtm        begsr

     c                   call      'XFXCVTIM'
     c                   parm                    qnty                           =quantity
     c                   parm                    xfpcnv                         =conversion code
     c                   parm                    cnvtyp                         =conversion type
     c                   parm                    error                          =error flag

     c                   endsr
     c*****************************************************************
     c     wrttrail      begsr                                                  =self pay verific

     c                   if        errorCount > 0 or rtrnCd <> *blanks
     c                   leavesr
     c                   endif
      *
     c     noupdate      cabne     *blanks       endtrail
      *
     c** write file
     c                   eval      wOldEffFrom = oldEffFrom
     c                   eval      wOldEffTo = oldEffTo
      *
     c                   if        option <> 'AA'
     c     ivstrky3      setll     hafivstr
     c                   dou       *in79 = *on
     c     ivstrky3      reade     hafivstr                               79
     c** there could be duplicate record for given key. pull appropriate record
     c                   if        *in79 = '0'

     c                   if        (option = 'DD' and bstdlt = ' ')
     c                               and bstEnd = wOldEffTo
     c                               and (bstMax = qnty or (
     c                                   period = 'D' and btSunU = qnty) or
     c                                  (period = 'U' and btsunu = qntySunday
     c                                   and btmonu = qntyMonday
     c                                   and bttueu = qntyTuesday
     c                                   and btwedu = qntyWednesday
     c                                   and btthru = qntyThursday
     c                                   and btfriu = qntyFriday
     c                                   and btsatu = qntySaturday))
     c                   leave
     c                   endif

     c                   if        (option = 'RR' and bstdlt = 'D')
     c                               and bstEnd = wOldEffTo
     c                               and ((bstMax = qnty and period <> 'T'
     c                                     and period <> 'X' and period <> 'U')
     c                                    or (period = 'D' and btSunU = qnty))
     c                   leave
     c                   endif

     c                   if        option = 'RR' and
     c                             bstdlt = 'D'  and
     c                             bstEnd = wOldEffTo

     c                   if        period = 'T' and
     c                             btsunf = sundayFrom and
     c                             btsunt = sundayTo and
     c                             btmonf = mondayFrom and
     c                             btmont = mondayTo and
     c                             bttuef = tuesdayFrom and
     c                             bttuet = tuesdayTo and
     c                             btwedf = wednesdayFrom and
     c                             btwedt = wednesdayTo and
     c                             btthrf = thursdayFrom and
     c                             btthrt = thursdayTo and
     c                             btfrif = fridayFrom and
     c                             btfrit = fridayTo and
     c                             btsatf = saturdayFrom and
     c                             btsatt = saturdayTo
     c                   leave
     c                   endif

     c                   if        period = 'X' and
     c                             btsunx = sunday and
     c                             btmonx = monday and
     c                             bttuex = tuesday and
     c                             btwedx = wednesday and
     c                             btthrx = thursday and
     c                             btfrix = friday and
     c                             btsatx = saturday
     c                   leave
     c                   endif

     c                   if        period = 'U' and
     c                             btsunu = qntySunday and
     c                             btmonu = qntyMonday and
     c                             bttueu = qntyTuesday and
     c                             btwedu = qntyWednesday and
     c                             btthru = qntyThursday and
     c                             btfriu = qntyFriday and
     c                             btsatu = qntySaturday
     c                   leave
     c                   endif

     c                   endif

     c                   if        option = 'CC' and bstdlt = ' '
     c                             and bstEnd = wOldEffTo
     c                   leave
     c                   endif

     c                   endif
     c                   enddo
     c                   else
     c                   eval      *in79 = *on
     c                   endif
      *
     c                   select
     c** add or change
     c                   when      option = 'AA' or option = 'CC'

     c                   if        *in79 = '1'
     c                             or bstdlt <> *blanks
     c                   clear                   hafivstr
     c                   endif
     c                   eval      bstlv6 = lvl6
     c                   eval      bstacc = account
     c                   eval      bstfsq = fileSeq
     c                   eval      bstper = period
     c                   eval      bsteff = effectiveFrom
     c                   eval      bstend = effectiveTo
     c                   z-add     savpsq        bstpsq

     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     units         qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          bstmax
     c                   else
     c                   z-add     units         bstmax
     c                   endif

     c                   if        period = 'D'
     c                   z-add     bstmax        btsunu
     c                   z-add     bstmax        btmonu
     c                   z-add     bstmax        bttueu
     c                   z-add     bstmax        btwedu
     c                   z-add     bstmax        btthru
     c                   z-add     bstmax        btfriu
     c                   z-add     bstmax        btsatu
     c                   z-add     0             bstmax
     c                   endif

     c                   if        period = 'X'
     c                   eval      btsunx = %trim(sunday)
     c                   eval      btmonx = %trim(monday)
     c                   eval      bttuex = %trim(tuesday)
     c                   eval      btwedx = %trim(wednesday)
     c                   eval      btthrx = %trim(thursday)
     c                   eval      btfrix = %trim(friday)
     c                   eval      btsatx = %trim(saturday)
     c                   endif

     c                   if        period = 'U'

     c                   if        sunday <> *blanks
     c                   evalr     wkqtyw = %subst(sunday:1:%scan('.':sunday)-1)
     c                   eval      wkqtyd = %subst(sunday:%scan('.':sunday)+1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btsunu
     c                   else
     c                   z-add     wkqnty        btsunu
     c                   endif
     c                   else
     c                   z-add     0             btsunu
     c                   endif

     c                   if        monday <> *blanks
     c                   evalr     wkqtyw = %subst(monday:1:%scan('.':monday)-1)
     c                   eval      wkqtyd = %subst(monday:%scan('.':monday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btmonu
     c                   else
     c                   z-add     wkqnty        btmonu
     c                   endif
     c                   else
     c                   z-add     0             btmonu
     c                   endif

     c                   if        tuesday <> *blanks
     c                   evalr     wkqtyw = %subst(tuesday:1:
     c                                            %scan('.':tuesday)-1)
     c                   eval      wkqtyd = %subst(tuesday:
     c                                              %scan('.':tuesday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          bttueu
     c                   else
     c                   z-add     wkqnty        bttueu
     c                   endif
     c                   else
     c                   z-add     0             bttueu
     c                   endif

     c                   if        wednesday <> *blanks
     c                   evalr     wkqtyw = %subst(wednesday:1
     c                                            :%scan('.':wednesday)-1)
     c                   eval      wkqtyd = %subst(wednesday
     c                                            :%scan('.':wednesday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btwedu
     c                   else
     c                   z-add     wkqnty        btwedu
     c                   endif
     c                   else
     c                   z-add     0             btwedu
     c                   endif

     c                   if        thursday <> *blanks
     c                   evalr     wkqtyw = %subst(thursday:1
     c                                            :%scan('.':thursday)-1)
     c                   eval      wkqtyd = %subst(thursday
     c                                            :%scan('.':thursday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btthru
     c                   else
     c                   z-add     wkqnty        btthru
     c                   endif
     c                   else
     c                   z-add     0             btthru
     c                   endif

     c                   if        friday <> *blanks
     c                   evalr     wkqtyw = %subst(friday:1:%scan('.':friday)-1)
     c                   eval      wkqtyd = %subst(friday:%scan('.':friday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btfriu
     c                   else
     c                   z-add     wkqnty        btfriu
     c                   endif
     c                   else
     c                   z-add     0             btfriu
     c                   endif

     c                   if        saturday <> *blanks
     c                   evalr     wkqtyw = %subst(saturday:1
     c                                            :%scan('.':saturday)-1)
     c                   eval      wkqtyd = %subst(saturday
     c                                            :%scan('.':saturday)+ 1)
     c                   if        xfppfg <> 'Y' and xfpcnv <> *blanks
     c                   z-add     wkqnty        qnty
     c                   eval      cnvtyp = '1'
     c                   exsr      srcvtm
     c                   z-add     qnty          btsatu
     c                   else
     c                   z-add     wkqnty        btsatu
     c                   endif
     c                   else
     c                   z-add     0             btsatu
     c                   endif
     c                   endif

     c                   if        period = 'T'
     c                   eval      btsunf = mtsund
     c                   eval      btsunt = mtsun2
     c                   eval      btmonf = mtmond
     c                   eval      btmont = mtmon2
     c                   eval      bttuef = mttues
     c                   eval      bttuet = mttue2
     c                   eval      btwedf = mtweds
     c                   eval      btwedt = mtwed2
     c                   eval      btthrf = mtthur
     c                   eval      btthrt = mtthu2
     c                   eval      btfrif = mtfrid
     c                   eval      btfrit = mtfri2
     c                   eval      btsatf = mtsatd
     c                   eval      btsatt = mtsat2
     c                   endif

     c** new record
     c                   if        *in79 = '1'
     c                             or bstdlt = 'D'
     c                   time                    curtim
     c                   movel     ldausr        bstcrb
     c                   time                    tmstmp
     c                   eval      bstcrd = today
     c                   eval      bstcrt = stmptm
     c                   if        newDates = 'Y'
     c                   if        changeOK
     c                   write     hafivstr
     c                   endif
     c                   else
     c                   write     hafivstr
     c                   endif
     c                   eval      repro = 'Y'
     c                   else
     c** update existing
     c                   time                    curtim                          dropping tran
     c                   movel     ldausr        bstchb
     c                   time                    tmstmp
     c                   eval      bstchd = today
     c                   eval      bstcht = stmptm
     c                   update    hafivstr
     c                   eval      repro = 'Y'
     c                   endif

     c                   when      option = 'DD'
     c** flag record as deleted
     c                   if        *in79 = '0'
     c                   eval      bstdlt = 'D'
     c                   time                    curtim
     c                   movel     ldausr        bstdby
     c                   time                    tmstmp
     c                   eval      bstddt = today
     c                   eval      bstdtm = stmptm
     c                   update    hafivstr
     c                   eval      repro = 'Y'
     c                   endif

     c                   when      option = 'RR'
     c** reinstate record
     c                   if        *in79 = '0'
     c                   eval      bstdlt = ' '
     c                   eval      bstdby = *blanks
     c                   eval      bstddt = 0
     c                   eval      bstdtm = 0
     c                   time                    curtim                          dropping tran
     c                   movel     ldausr        bstchb
     c                   time                    tmstmp
     c                   eval      bstchd = today
     c                   eval      bstcht = stmptm
     c                   update    hafivstr
     c                   eval      repro = 'Y'
     c                   endif

     c                   endsl

     c                   if        newdates = 'Y'
     c                   z-add     wrkfrm        savfrm
     c                   z-add     wrkend        savend
     c     ivsvcky       chain     hafivsvs                           79
     c                   if        *in79 = '0'
     c                   z-add     wrkfrm        ydate
     c                   callp     xfxcymd(ydate : mdate)
     c                   if        mdate = 0
     c                   z-add     0             brveff
     c                   else
     c                   z-add     ydate         brveff
     c                   endif
     c                   z-add     wrkend        ydate
     c                   callp     xfxcymd(ydate : mdate)
     c                   if        mdate = 0
     c                   z-add     0             brvend
     c                   else
     c                   z-add     ydate         brvend
     c                   endif
     c                   eval      brvlen = 0
     c                   eval      brvltp = *blanks
     c                   if        changeOK
     c                   update    hafivsvs
     c                   else
     c                   eval      workDate = brveff
     c                   eval      warnMsg = 'Dates will be changed to '
     c                                     + %char(workMonth) + '/'
     c                                     + %char(workDay) + '/'
     c                                     + %char(workYear)
     c                   eval      workDate = brvend
     c                   eval      warnMsg = %trim(warnMsg) + ' to '
     c                                     + %char(workMonth) + '/'
     c                                     + %char(workDay) + '/'
     c                                     + %char(workYear)
     c                   eval      warnMsg = %trim(warnMsg) + '.  Continuing '
     c                                     + 'will update them.'
     c                   endif
     c                   endif
     c                   endif
     c                   unlock    halivsvs

     c                   if        repro = 'Y'
     c                   if        repdte = 0
     c                   eval      repdte = startd
     c                   elseif    startd < repdte
     c                   eval      repdte = startd
     c                   endif
     c                   endif

     c     endtrail      endsr
      **************************************************************************
      ** ValidTime                                                            **
      **************************************************************************
     p ValidTime       b
     d ValidTime       pi              n
     d  name                         25    const
     d  value                         4  0 const

      /free

         if (value < 0 or value > 2359);    // Invalid time entered
           AddErrorMsg(name : '00000915');
           return *off;
         else;
           return *on;
         endif;

      /end-free

     p ValidTime       e
      **************************************************************************
      ** ValidTimeRange                                                       **
      **************************************************************************
     p ValidTimeRange...
     p                 b
     d ValidTimeRange...
     d                 pi              n
     d  nameFrom                     25    const
     d  valueFrom                     4  0 const
     d  nameTo                       25    const
     d  valueTo                       4  0 const

      /free

        if ValidTime(nameFrom : valueFrom) and ValidTime(nameTo : valueTo);

          if (valueFrom = 0 and valueTo <> 0)
          or (valueFrom <> 0 and valueTo = 0)
          or (valueFrom > valueTo);
            AddErrorMsg(nameTo : '00001576');
            return *off;
          else;
            return *on;
          endif;

        else;
          AddErrorMsg(nameTo : '00001576');
          return *off;
        endif;

      /end-free

     p ValidTimeRange...
     p                 e
      **************************************************************************
      ** AddErrorMsg                                                          **
      **************************************************************************
     p AddErrorMsg     b
     d AddErrorMsg     pi
     d  fldname                      25    const
     d  errcode                       8    const

      /free

         errorCount += 1;
         errors(errorCount).fldname = %trim(fldname);
         errors(errorCount).errcode = %trim(errcode);

         return;

      /end-free

     p AddErrorMsg     e

     ***********************************************************************************************
     p sendInfoToWRM   b
     d sendInfoToWRM   pi
     d  level6                        6  0 const
     d  account                      12  0 const
     d  fileSeq                       5  0 const
     d  WRMerror                    200

       /copy copysrc,hxxappprfp
       dcl-s callApi ind;
       dcl-s insAuthInfoSent ind;
       dcl-s tranId int(10);

       //Run WSS API Requests
       HxxAppPrf('API' : '9' : prefDesc);
       if (prefDesc <> 'Y');
         return;
       endif;

       callApi = '0';
       exec sql
        select '1'
         into :callApi
        from VXWSSLVL6
        where hxulv6 = :lvl6
       fetch first row only;

       if (callApi);
         exec SQL set :tranId = WSS_sendInsAuthInfo(:level6,
                                                    :account,
                                                    :fileSeq);

         if (tranId <> 1);
           exec SQL set :WRMerror =
             WSS_read_error_response(:tranId);
         endif;
       endif;
     p                 e

      **************************************************************************
      ** Get the disciplines seperator                                        **
      **************************************************************************
     p getDiscplnDesc  b
     d getDiscplnDesc  pi            50
     d  iDescription                 50    const
     d  iDiscipline                   2    const

        if (iDescription = '');
          return iDiscipline;
        else;
          return %trim(iDescription) + '; ' + iDiscipline;
        endif;

     p getDiscplnDesc  e

