      /copy copysrc,hxxcntrlws
     H main(HAHIVSGR)
      *************************************************************************
      **                                                                     **
      **=  Web Service for Group Verification Maintenance/Listing Program    **
      **                                                                     **
      *************************************************************************
      **                                                                     **
      **  Web Service PCML Parameter Setup:                                  **
      **                                                                     **
      **    Parameter    Usage        Counter     Description                **
      **    =========    ==========   ==========  ================           **
      **    userid       input                    User ID                    **
      **    passwd       input                    Encrypted Password         **
      **    reqFlg       input                    Option Flag                **
      **                                          B = Build Grids            **
      **                                          A = Add Group Header       **
      **                                          U = Update Group Header    **
      **                                          D = Delete Group Header    **
      **                                          R = Reinstate Group Header **
      **                                          Z = Add Period Header      **
      **                                          C = Update Period Header   **
      **                                          X = Delete Period Header   **
      **                                          I = Reinstate Period Header**
      **    reqLv6       input                    Account Level6             **
      **    reqAcc       input                    Account Number             **
      **    reqISq       input                    Insurance Sequence         **
      **    changeOk     input/output             Proceed With Changes Flag  **
      **    grpCount     input/output             Group Header List Counter  **
      **    grpGrd       input/output grpCount    Group Header Listing Grid  **
      **    perCount     input/output             Header Period List Counter **
      **    perGrd       input/output perCount    Header Period Listing Grid **
      **    warnMsg      output                   Warning Message            **
      **    errorCount   output                   Field Error Counter        **
      **    errors       output       errorCount  Field Errors DS            **
      **    rtrncd       output                   Return Code                **
      **                                                                     **
      *************************************************************************
      **                                                                     **
      **     copyright - 2014 - Health Care Software, Inc                    **
      **                                                                     **
      *************************************************************************
      **                         Modification Summary                        **
      **                                                                     **
      ** Change Date  Programmer/comments                       Install Date **
      **                                                                     **
      ** 10/22/2014   Alex Price                                             **
      **             -Created                                                **
      **                                                                     **
      ** 08/07/2015   Alex Hansen  GOL-629                                   **
      **             -allow verification by service to be set up by          **
      **              revenue code                                           **
      **              - Installed by Michael Kundla                          **
      **                                                                     **
      ** 02/29/2016   Justin Sarnak                                          **
      **             -Fixed table names in WSI comments                      **
      **              - reInstalled by Brian Shore                           **
      **                                                                     **
      ** 11/15/2019   Alex Price       HDEV-27742                            **
      **             -Added parms to xfxedits                                **
      **                                                                     **
      ** 06/12/2024   Saikiran Parupalli  ICON-210                           **
      **             -Added logic to check for INSVCAUT bill edit            **
      **                                                                     **
      **  06/27/2024    Abhijith Ravindran  ICON-315                         **
      **               -Added logic to include effective and end dates when  **
      **                adding or updating period restrictions.              **
      **                                                                     **
      **  08/08/2024    Alex Price          ICON-375                         **
      **               -Fixed spelling mistake on warning message            **
      **                                                                     **
      **  10/23/2024    Abhijith Ravindran ICON-285                          **
      **                - Added logic to save and retrieve supplemental unit **
      **                  and amount.                                        **
      **                                                                     **
      **  11/12/2024    Abhijith Ravindran ICON-473                          **
      **                - Added logic for validating the overlap for         **
      **                  disciplines.                                       **
      **                                                                     **
      **  12/04/2024    Saikiran Parupalli  ICON-560                         **
      **                - Added warning message for the INSVCAUT bill edit   **
      **                  is Optional                                        **
      **                                                                     **
      **  01/06/2025    SKosuri ICON-604                                     **
      **               - IVSVC quantities should not exceed group header qty **
      **                                                                     **
      **  01/09/2025    Abhijith Ravindran ICON-543                          **
      **                - Added logic for validating the overlap for new     **
      **                  disciplines.                                       **
      **                                                                     **
      **  01/16/2025    SKosuri ICON-621                                     **
      **               - Added Biweekly(B) and IPC year(I) period restriction**
      **                                                                     **
      **  02/04/2025    Abhijith Ravindran ICON-658                          **
      **                - Deleting and Reinstating Group Header Period       **
      **                  Restrictions.                                      **
      **                                                                     **
      **  08/01/2025    Camila Barbini ICEN-3149                              **
      **               - Increase authNum field size                          **
      **                                                                     **
      **  08/14/2025    Abhijith Ravindran ICON-929                          **
      **               - Added new field for saving the Group Header Type    **
      **                 Modified the validations while saving the Group to  **
      **                 allow only non blank Group Header Type, allow user  **
      **                 to modify group header type for groups not already  **
      **                 linked.                                             **
      **                                                                      **
      *************************************************************************
     fHAPIVGTR  uf a e           k disk    usropn
     fHAPIVSGR  uf a e           k disk    usropn
     fHALTRNO7  uf a e           k disk    usropn
     f                                     rename(HAFTRNO:HAFTRNO7)
     fHALIVSVGR uf   e           k disk    usropn
     fHMPMAST   if   e           k disk    usropn
     fHXPBNFIT  if   e           k disk    usropn
     fHXPPROCM  if   e           k disk    usropn
     fHXPPROCC  if   e           k disk    usropn
     fHXPUSER   if   e           k disk    usropn
     fHALIRNKS  if   e           k disk    usropn
     fHALIVSGR  if   e           k disk    usropn
     f                                     rename(HAFIVSGR:HAFIVSGRB)
     fHALIVSVD  if   e           k disk    usropn
     f                                     rename(HAFIVSVC:HAFIVSVD)
     f                                     prefix(bx:2)
     fHALIVSVDR if   e           k disk    usropn
     f                                     rename(HAFIVSVC:HAFIVSVDR)
     f                                     prefix(bx:2)
     fHAPIRNK   if   e           k disk    usropn
     f                                     rename(HBFIRNK:HBFIRNKD)
     f                                     prefix(dte:3)
      *************************************************************************
      **=// Global Prototype Definitions //                                  **
      *************************************************************************
     d HAHIVSGR        pr                  extpgm('HAHIVSGR')
     d  userid                       10
     d*info|use:input|req:Y|
     d*cmnt|comment:This field is used to send in the users id|
     d  passwd                      100
     d*info|use:input|req:Y|
     d*cmnt|comment:This field is used to send in the users password|
     d  reqFlg                        1
     d*info|use:input|req:Y|
     d*cmnt+|comment:Request option, 'B' to Build Grids, |
     d*cmnt+|comment:'A' to Add Group Header,|
     d*cmnt+|comment:'U' to Update Group Header,|
     d*cmnt+|comment:'D' to Delete Group Header,|
     d*cmnt+|comment:'R' to Reinstate Group Header,|
     d*cmnt+|comment:'Z' to Add Period Header,|
     d*cmnt+|comment:'C' to Update Period Header,|
     d*cmnt+|comment:'X' to Delete Period Header,|
     d*cmnt|comment:'I' to Reinstate Period Header|
     d  reqLv6                        6  0
     d*info|use:input|req:Y|
     d*cmnt|comment:This is the requested level 6 number|
     d  reqAcc                       12  0
     d*info|use:input|req:Y|
     d*cmnt|comment:This is the requested account number|
     d  reqISq                        4  0
     d*info|use:input|req:Y|
     d*cmnt|comment:This field is the requested insurance sequence number|
     d  changeOk                       n
     d*info|use:input/output|req:Y|
     d*cmnt+|comment:Proceed With Changes Flag, |
     d*cmnt+|comment:Input: Default to '0', pass '1' to proceed. |
     d*cmnt|comment:Output: Updates needs to be okay'd is '1', no need for okay '0'|
     d  grpCount                     10i 0
     d*info|use:input/output|req:Y|
     d*cmnt|comment:Counter for Group Header Listing Grid|
     d  grpGrd                             likeds(grpDs) dim(9999)
     d*info|use:input/output|counter:grpCount|req:Y|
     d*cmnt|comment:Group Header Listing Grid data structure|
     d  perCount                     10i 0
     d*info|use:input/output|req:Y|
     d*cmnt|comment:Counter for Period Header Listing Grid|
     d  perGrd                             likeds(perDs) dim(9999)
     d*info|use:input/output|counter:perCount|req:Y|
     d*cmnt|comment:Period Header Listing Grid data structure|
     d  warnMsg                     100
     d*info|use:output|
     d*cmnt|comment:Warning message to update associated records.|
     d  errorCount                   10i 0
     d*info|use:output|
     d*cmnt|comment:Counter for errors data structure|
     d  errors                             likeds(errorsds) dim(9999)
     d*info|use:output|counter:errorCount|jType:|
     d*cmnt|comment:Errors data structure|
     d  rtrncd                        8
     d*info|use:output|
     d*cmnt|comment:This field is output and contains response code|
      *************************************************************************
      **=//Global Data Structure Definitions //                              **
      *************************************************************************
     d grpDs           ds                  qualified
     d  level6                        6  0
     d*cmnt|comment:This field is the group level 6 number|
     d  account                      12  0
     d*cmnt|comment:This field is the group account number|
     d  number                       10  0
     d*cmnt|comment:This field is the group number|
     d  name                         12
     d*cmnt|comment:This field is the group name|
     d  units                         7  2
     d*cmnt|comment:This field is the group units|
     d  supplUnits                    7  2
     d*cmnt|comment:This field is the supplemental units|
     d  strDate                       8  0
     d*info|jtype:Y|
     d*cmnt|comment:This field is the group effective date|
     d  endDate                       8  0
     d*info|jtype:Y|
     d*cmnt|comment:This field is the group end date|
     d  deleted                       3
     d*cmnt|comment:This field is the group deletion status|
     d  authNumber                   25
     d*cmnt|comment:This field is the group authority number|
     d  convFactor                    2
     d*info|table:XCNV|
     d*cmnt|comment:This field is the group conversion factor code|
     d  convFactorD                  50
     d*cmnt|comment:This field is the group conversion factor description|
     d  groupHdrTyp                   1
     d*info|table:IVTP|
     d*cmnt|comment:This field is the group header type|
      *-------------------------------------------------------------------------
     d perDs           ds                  qualified
     d  level6                        6  0
     d*cmnt|comment:This field is the period level 6 number|
     d  account                      12  0
     d*cmnt|comment:This field is the period account number|
     d  number                       10  0
     d*cmnt|comment:This field is the period group number|
     d  sequence                      4  0
     d*cmnt|comment:This field is the period sequence number|
     d  period                        1
     d*info|table:BDWY|
     d*cmnt|comment:This field is the period code|
     d  periodD                      50
     d*cmnt|comment:This field is the period description|
     d  units                         7  2
     d*cmnt|comment:This field is the period max units|
     d  sunday                        7
     d*cmnt+|comment:This field is the Sunday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  sundayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday start time. |
     d  sundayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Sunday end time|
     d  monday                        7
     d*cmnt+|comment:This field is the Monday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  mondayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday start time|
     d  mondayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Monday end time|
     d  tuesday                       7
     d*cmnt+|comment:This field is the Tuesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  tuesdayFrom                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday start time|
     d  tuesdayTo                     4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Tuesday end time|
     d  wednesday                     7
     d*cmnt+|comment:This field is the Wednesday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  wednesdayFrom                 4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday start time|
     d  wednesdayTo                   4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Wednesday end time|
     d  thursday                      7
     d*cmnt+|comment:This field is the Thursday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  thursdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday start time|
     d  thursdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Thursday end time|
     d  friday                        7
     d*cmnt+|comment:This field is the Friday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  fridayFrom                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday start time|
     d  fridayTo                      4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Friday end time|
     d  saturday                      7
     d*cmnt+|comment:This field is the Saturday units. |
     d*cmnt+|comment:When the period is 'X', |
     d*cmnt+|comment:this field can be 'X' or blank. |
     d*cmnt+|comment:When the period is 'D' or 'U', |
     d*cmnt+|comment:this field can only be float numbers. |
     d*cmnt|comment:When other periods are selected this should be blank.|
     d  saturdayFrom                  4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday start time|
     d  saturdayTo                    4  0
     d*info|jtype:H|
     d*cmnt|comment:This field is the Saturday end time|
     d  deleted                       3
     d*cmnt|comment:This field is the period deletion status|
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
      *-------------------------------------------------------------------------
     d errorsDs        ds                  qualified
     d                                     based(Template)
     d  fldname                      25
     d*cmnt|comment:This field is used to reference the field that caused|
     d*cmnt|comment: an error|
     d  errcode                       8
     d*cmnt+|comment:This field holds the error code that represents the|
     d*cmnt|comment: error found|
      *************************************************************************
      **=// Data Area Definitions //                                         **
      *************************************************************************
     d yudate          s              8  0 dtaara(haadate)
     d grpnbro         s             10  0 dtaara(haaivsgrn)
      *************************************************************************
      **= //*entry //                                                        **
      *************************************************************************
     p HAHIVSGR        b
     d                 pi
     d  userid                       10
     d  passwd                      100
     d  reqFlg                        1
     d  reqLv6                        6  0
     d  reqAcc                       12  0
     d  reqISq                        4  0
     d  changeOk                       n
     d  grpCount                     10i 0
     d  grpGrd                             likeds(grpDs) dim(9999)
     d  perCount                     10i 0
     d  perGrd                             likeds(perDs) dim(9999)
     d  warnMsg                     100
     d  errorCount                   10i 0
     d  errors                             likeds(errorsDs) dim(9999)
     d  rtrncd                        8
      *************************************************************************
      **=  //    Local Data Structure Definitions  //                        **
      *************************************************************************
     d                 ds
     d curtim                        14  0
     d  runtme                        6  0 overlay(curtim)
     d  rundte                        8  0 overlay(curtim:7)
     d   mmdd                         4  0 overlay(rundte)
     d   yyyy                         4  0 overlay(rundte:5)
      *-----------------------------------------------------------------------
     d                 ds
     d workDate                       8  0
     d  workYear                      4  0 overlay(workDate)
     d  workMonthDay                  4  0 overlay(workDate:5)
     d   workMonth                    2  0 overlay(workMonthDay)
     d   workDay                      2  0 overlay(workMonthDay:3)
      *************************************************************************
      **=// Local Misc. Variables //                                         **
      *************************************************************************
     d activeRcdFnd    s               n   inz('0')
     d cnvtyp          s              1
     d dayUnits        s              6
     d deletedRcdFnd   s               n   inz('0')
     d disCount        s              8  0
     d error           s              1
     d foundivsvc      s               n
     d fromDate        s              8  0
     d grpnbr          s             10  0
     d invalidTime     s               n
     d mdate           s              8  0
     d newDates        s               n   inz('0')
     d number          s              7  0
     d prmacc          s             12  0
     d prmApp          s              1
     d prmbyr          s              2  0
     d prmcod          s              1
     d prmdte          s              8  0
     d prmgdt          s              8  0
     d prmisq          s              4  0
     d prmlv6          s              6  0
     d prmmsg          s             70
     d prmnam          s             10
     d prmpln          s              5  0
     d prmpol          s             20
     d prmpyr          s              6  0
     d prmResp         s              1
     d prmval          s             25
     d qnty            s              7  2
     d qntySun         s              7  2
     d qntyMon         s              7  2
     d qntyTue         s              7  2
     d qntyWed         s              7  2
     d qntyThu         s              7  2
     d qntyFri         s              7  2
     d qntySat         s              7  2
     d repdte          s              8  0
     d repro           s               n   inz('0')
     d rtncod          s              1
     d saveQnty        s              7  2
     d saveSupplQnty   s              7  2
     d supplQnty       s              7  2
     d toDate          s              8  0
     d totalUnits      s              7  0
     d usedQty         s              8  0
     d usedSup         s              8  0
     d foundivsvccount s              6
     d viewPeriod      s               n
     d wkcnvf          s              2
     d workField       s             25
     d ydate           s              8  0
     d ydate1          s              8  0
     d ydate2          s              8  0
     d ydays           s              5  0
     d wOldEffFrom     s              8  0
     d wOldEffTo       s              8  0
      *************************************************************************
      **=// Local Prototype Definitions //                                   **
      *************************************************************************
     d XFXCVTIM        pr                  extpgm('XFXCVTIM')
     d  qnty_                              like(qnty)
     d  wkcnvf_                            like(wkcnvf)
     d  cnvtyp_                            like(cnvtyp)
     d  error_                             like(error)
      *************************************************************************
      /copy copysrc,hxxckaut
      /copy copysrc,hxxtable
      /copy copysrc,hxxproto
      /copy copysrc,hoxdefn

      *-- sql options ----------------------------------------------------------
      /copy copysrc,hxxsqlopt

      *************************************************************************
      **=                         Main Routine Code                          **
      *************************************************************************
      /free
         // Initializations //
         rtrncd = *blanks;
         warnMsg = *blanks;
         errorCount = 0;
         clear  errors;

         // Check Authority //
         XFXWSAUT( userid : passwd : rtrncd );
         if rtrncd <> *blanks;
           return;
         endif;

         open HAPIVGTR;
         open HAPIVSGR;
         open HALTRNO7;
         open HALIVSVGR;
         open HALIVSGR;
         open HALIVSVD;
         open HALIVSVDR;
         open HXPUSER;
         open HMPMAST;
         open HAPIRNK;
         open HALIRNKS;
         open HXPBNFIT;
         open HXPPROCM;
         open HXPPROCC;

         chain (userid) HXFUSER;
         if not%found(HXPUSER);
           rtrncd = '00000006';
           return;
         endif;

         chain (reqLv6 : reqAcc) HMFMAST;
         if not%found(HMPMAST);
           rtrncd = '00000006';
           return;
         endif;

         chain (MMPLV6 : MMACCT : reqISq) HBFIRNK;
         if not%found(HALIRNKS);
           rtrncd = '00000006';
           return;
         endif;

         chain (BRKUBC : BRKPLN) XFFBNFIT;
         if not%found(HXPBNFIT);
           rtrncd = '00000006';
           return;
         endif;

         exsr getTime;
         in yudate;

         rtncod = *blanks;
         if reqflg = 'B';
           exsr buildGrp;

         elseif reqflg = 'A';
           grpDs = grpGrd(1);
           exsr addGrp;
           grpGrd(1) = grpDs;

         elseif reqFlg = 'U';
           grpDs = grpGrd(1);
           exsr updateGrp;
           grpGrd(1) = grpDs;

         elseif reqFlg = 'D';
           grpDs = grpGrd(1);
           exsr deleteGrp;
           grpGrd(1) = grpDs;

         elseif reqFlg = 'R';
           grpDs = grpGrd(1);
           exsr reinstGrp;
           grpGrd(1) = grpDs;

         elseif reqflg = 'Z';
           perDs = perGrd(1);
           exsr addPer;
           perGrd(1) = perDs;

         elseif reqFlg = 'C';
           perDs = perGrd(1);
           exsr updatePer;
           perGrd(1) = perDs;

         elseif reqFlg = 'X';
           perDs = perGrd(1);
           exsr deletePer;
           perGrd(1) = perDs;

         elseif reqFlg = 'I';
           perDs = perGrd(1);
           exsr reinstPer;
           perGrd(1) = perDs;

         endif;

         // Exit //
         close  *all;
         xfxwsexit();
         *inlr = *on;
      /end-free
      **************************************************************************
      **=      Group Header Build                                             **
      **************************************************************************
     c     buildGrp      begsr
      /free
         changeOk = *off;
         grpCount = 0;
         clear  grpGrd;
         perCount = 0;
         clear  perGrd;

         // View Period Restrictions
         number = 3568;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           viewPeriod = *off;
         else;
           viewPeriod = *on;
         endif;

         setll (reqLv6 : reqAcc) HAFIVSGRB;
         dou %eof(HALIVSGR);
           reade (reqLv6 : reqAcc) HAFIVSGRB;
           if not %eof(HALIVSGR);
             clear grpDs;

             grpDs.level6     = BGRLV6;
             grpDs.account    = BGRACC;
             grpDs.number     = BGRNBR;
             grpDs.name       = BGRNAM;
             grpDs.strDate    = BGREFF;
             grpDs.endDate    = BGREND;
             grpDs.authNumber = BGRAUT;
             grpDs.groupHdrTyp = BGRGTP;

             if BGRDLT = 'D';
               grpDs.deleted = 'Yes';
             endif;

             if BGRCNV <> *blanks;
               qnty   = BGRCOV - BGRSUP;
               wkcnvf = BGRCNV;
               cnvtyp = '2';
               error  = *blanks;
               XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
               grpDs.units      = qnty;

               supplQnty   = BGRSUP;
               wkcnvf = BGRCNV;
               cnvtyp = '2';
               error  = *blanks;
               XFXCVTIM(supplQnty : wkcnvf : cnvtyp : error);
               grpDs.supplUnits = supplQnty;
             else;
               grpDs.units = BGRCOV - BGRSUP;
               grpDs.supplUnits = BGRSUP;
             endif;

             grpDs.convFactor = BGRCNV;
             clear tableDS;
             ecode = grpDs.convFactor;
             XFXTABL( 'XCNV' : ecode : hmap : edate : sdesc  : ldesc : tind );
             if tind <> 'E' and tind <> 'D';
               grpDs.convFactorD = ldesc;
             endif;

             if viewPeriod = *on;
               setll (BGRLV6 : BGRACC : BGRNBR) HAFIVGTR;
               dou %eof(HAPIVGTR);
                 reade (BGRLV6 : BGRACC : BGRNBR) HAFIVGTR;
                 if not %eof(HAPIVGTR);
                   exsr buildPer;
                 endif;
               enddo;
             endif;

             grpCount += 1;
             grpGrd(grpCount) = grpDs;
           endif;
         enddo;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Group  Period Build                                            **
      **************************************************************************
     c     buildPer      begsr
      /free
         clear perDs;

         perDs.level6     = BGTLV6;
         perDs.account    = BGTACC;
         perDs.number     = BGTNBR;
         perDs.sequence   = BGTPSQ;
         perDs.effectiveFrom = BGTEFF;
         perDs.effectiveTo = BGTEND;
         perDs.oldEffFrom = BGTEFF;
         perDs.oldEffTo = BGTEND;

         if grpDs.convFactor <> *blanks;
           qnty   = BGTMAX;
           wkcnvf = grpDs.convFactor;
           cnvtyp = '2';
           error  = *blanks;
           XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
           perDs.units      = qnty;
         else;
           perDs.units      = BGTMAX;
         endif;

         if BGTDLT = 'D';
           perDs.deleted = 'Yes';
         endif;

         perDs.period     = BGTPER;
         clear tableDS;
         ecode = perDs.period;
         XFXTABL( 'BDWY' : ecode : hmap : edate : sdesc  : ldesc : tind );
         if tind <> 'E' and tind <> 'D';
           perDs.periodD = ldesc;

           if perDs.period = 'X';
             perDs.sunday    = BGSUNX;
             perDs.monday    = BGMONX;
             perDs.tuesday   = BGTUEX;
             perDs.wednesday = BGWEDX;
             perDs.thursday  = BGTHRX;
             perDs.friday    = BGFRIX;
             perDs.saturday  = BGSATX;

           elseif perDs.period = 'U' or perDs.period = 'D';

             if BGSUNU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGSUNU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.sunday    = %editc(qnty:'3');
                 if perDs.period = 'D';
                   perDs.units = qnty;
                 endif;
               else;
                 if perDs.period = 'D';
                   perDs.units  = BGSUNU;
                 endif;
                 perDs.sunday    = %editc(BGSUNU:'3');
               endif;
             endif;

             if BGMONU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGMONU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.monday    = %editc(qnty:'3');
               else;
                 perDs.monday    = %editc(BGMONU:'3');
               endif;
             endif;

             if BGTUEU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGTUEU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.tuesday    = %editc(qnty:'3');
               else;
                 perDs.tuesday    = %editc(BGTUEU:'3');
               endif;
             endif;

             if BGWEDU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGWEDU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.wednesday    = %editc(qnty:'3');
               else;
                 perDs.wednesday    = %editc(BGWEDU:'3');
               endif;
             endif;

             if BGTHRU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGTHRU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.thursday    = %editc(qnty:'3');
               else;
                 perDs.thursday    = %editc(BGTHRU:'3');
               endif;
             endif;

             if BGFRIU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGFRIU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.friday    = %editc(qnty:'3');
               else;
                 perDs.friday    = %editc(BGFRIU:'3');
               endif;
             endif;

             if BGSATU <> 0;
               if grpDs.convFactor <> *blanks;
                 qnty   = BGSATU;
                 wkcnvf = grpDs.convFactor;
                 cnvtyp = '2';
                 error  = *blanks;
                 XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                 perDs.saturday    = %editc(qnty:'3');
               else;
                 perDs.saturday  = %editc(BGSATU:'3');
               endif;
             endif;


           elseif perDs.period = 'T';
             perDs.sundayFrom    = BGSUNF;
             perDs.sundayTo      = BGSUNT;
             perDs.mondayFrom    = BGMONF;
             perDs.mondayTo      = BGMONT;
             perDs.tuesdayFrom   = BGTUEF;
             perDs.tuesdayTo     = BGTUET;
             perDs.wednesdayFrom = BGWEDF;
             perDs.wednesdayTo   = BGWEDT;
             perDs.thursdayFrom  = BGTHRF;
             perDs.thursdayTo    = BGTHRT;
             perDs.fridayFrom    = BGFRIF;
             perDs.fridayTo      = BGFRIT;
             perDs.saturdayFrom  = BGSATF;
             perDs.saturdayTo    = BGSATT;
           endif;
         endif;

         perCount += 1;
         perGrd(perCount) = perDs;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Add Group Option                                               **
      **************************************************************************
     c     addGrp        begsr
      /free
         number = 3563;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         exsr checkGrpFlds;
         if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
           leavesr;
         endif;

         in *lock grpnbro;
         grpnbr = grpnbro;
         grpnbro += 1;
         out grpnbro;

         clear HAFIVSGR;

         if grpDs.convFactor <> *blanks;
           qnty = grpDs.units;
           wkcnvf = grpDs.convFactor;
           cnvtyp = '1';
           XFXCVTIM(qnty : wkcnvf : cnvtyp : error);

           supplQnty = grpDs.supplUnits;
           wkcnvf = grpDs.convFactor;
           cnvtyp = '1';
           XFXCVTIM(supplQnty : wkcnvf : cnvtyp : error);

           BGRCOV = qnty + supplQnty;
           BGRSUP = supplQnty;
         else;
           BGRCOV = grpDs.units + grpDs.supplUnits;
           BGRSUP = grpDs.supplUnits;
         endif;

         BGRLV6 = reqlv6;
         BGRACC = reqacc;
         BGRNBR = grpnbr;
         BGRNAM = grpDs.name;
         BGREFF = grpDs.strDate;
         BGREND = grpDs.endDate;
         BGRAUT = grpDs.authNumber;
         BGRGTP = grpDs.groupHdrTyp;
         BGRCNV = grpDs.convFactor;
         BGRCRB = userid;
         BGRCRD = yudate;
         BGRCRT = runtme;
         write HAFIVSGR;

      /end-free
     c                   endsr
      **************************************************************************
      **=      Update Group Option                                            **
      **************************************************************************
     c     updateGrp     begsr
      /free
         number = 3564;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         repro = *off;
         chain (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSGR;
         if %found(HAPIVSGR) and BGRDLT = *blanks;

           exsr checkGrpFlds;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           if grpDs.convFactor <> *blanks;
             qnty = grpDs.units;
             wkcnvf = grpDs.convFactor;
             cnvtyp = '1';
             XFXCVTIM(qnty : wkcnvf : cnvtyp : error);

             supplQnty = grpDs.supplUnits;
             wkcnvf = grpDs.convFactor;
             cnvtyp = '1';
             XFXCVTIM(supplQnty : wkcnvf : cnvtyp : error);

             if qnty + supplQnty <> BGRCOV;
               repro = *on;
             endif;
             BGRCOV = qnty + supplQnty;
             BGRSUP = supplQnty;
           else;
             if grpDs.units + grpDs.supplUnits <> BGRCOV;
               repro = *on;
             endif;
             BGRCOV = grpDs.units + grpDs.supplUnits;
             BGRSUP = grpDs.supplUnits;
           endif;

           if BGRCNV <> grpDs.convFactor;
             repro = *on;
           endif;
           BGRCNV = grpDs.convFactor;

           BGRNAM = grpDs.name;
           BGREFF = grpDs.strDate;
           BGREND = grpDs.endDate;
           BGRAUT = grpDs.authNumber;
           BGRGTP = grpDs.groupHdrTyp;
           BGRCHB = userid;
           BGRCHD = yudate;
           BGRCHT = runtme;
           update HAFIVSGR;
         endif;

         if repro = *on;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof();
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof();
               exsr gentrn;
             endif;
           enddo;
         elseif %found(HAPIVSGR) and BGRDLT = 'D';
           rtrncd = '00000786';
           leavesr;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Check Group                                                    **
      **************************************************************************
     c     checkGrpFlds  begsr
      /free
         saveQnty = grpDs.units;
         saveSupplQnty = grpDs.supplUnits;

         if grpDs.strDate <= 0;
           errorCount += 1;
           errors(errorCount).fldname = 'STRDATE';
           errors(errorCount).errcode = '00000406';
         else;
           ydate = grpDs.strDate;
           XFXCYMD(ydate : mdate);
           if mdate = 0;
             errorCount += 1;
             errors(errorCount).fldname = 'STRDATE';
             errors(errorCount).errcode = '00000406';
           endif;
         endif;

         if grpDs.endDate <= 0;
           errorCount += 1;
           errors(errorCount).fldname = 'ENDDATE';
           errors(errorCount).errcode = '00000865';
         elseif grpDs.endDate <> 99999999;
           ydate = grpDs.endDate;
           XFXCYMD(ydate : mdate);
           if mdate = 0;
             errorCount += 1;
             errors(errorCount).fldname = 'ENDDATE';
             errors(errorCount).errcode = '00000865';
           elseif grpDs.endDate <= grpDs.strDate;
             errorCount += 1;
             errors(errorCount).fldname = 'ENDDATE';
             errors(errorCount).errcode = '00000018';
           endif;
         endif;

         prmval = grpDs.authNumber;
         exsr autedt;
         if prmcod <> *blanks and prmcod <> 'W';
           errorCount += 1;
           errors(errorCount).fldname = 'AUTHNUMBER';
           errors(errorCount).errcode = '00003807';
         elseif prmcod = 'W' and changeOk = *off;
           warnMsg = 'Authorization number is missing or invalid. '
                     + 'Continuing will update them.';
         endif;
         if grpDs.authNumber <> *blanks;
           clear tableDS;
           ecode = grpDs.authNumber;
           XFXTABL( 'BDFT' : ecode : hmap : edate : sdesc  : ldesc : tind );
           if tind <> 'E' and tind <> 'D';
             ydays = %int(%trim(hmap));
             clear tableDS;
             ecode = grpDs.authNumber;
             XFXTABL( 'BDFA' : ecode : hmap : edate : sdesc  : ldesc : tind );
             if tind = 'E' or tind = 'D';
               if ydays <> 0;
                 ydate1 = grpDs.strDate;
                 XFXADDD(ydate1 : ydays : ydate2);
                 if grpDs.endDate = 0 or grpDs.endDate = 99999999;
                   grpDs.endDate = ydate2;
                 endif;
                 if grpDs.endDate > ydate2;
                   errorCount += 1;
                   errors(errorCount).fldname = 'ENDDATE';
                   errors(errorCount).errcode = '00001553';
                 endif;
               endif;
             else;
               number = 3573;
               rtncod = *blanks;
               XFCNHKAT(number : rtncod);
               if rtncod = 'E';
                 errorCount += 1;
                 errors(errorCount).fldname = 'AUTHNUMBER';
                 errors(errorCount).errcode = '00001554';
               elseif ydays <> 0;
                 ydate1 = grpDs.strDate;
                 XFXADDD(ydate1 : ydays : ydate2);
                 if grpDs.endDate = 0 or grpDs.endDate = 99999999;
                   grpDs.endDate = ydate2;
                 endif;
                 if grpDs.endDate > ydate2;
                   errorCount += 1;
                   errors(errorCount).fldname = 'ENDDATE';
                   errors(errorCount).errcode = '00001553';
                 endif;
               endif;
             endif;
           endif;
         endif;

         //if reqFlg = 'U' and errorCount <> 0;
         //  leaveSr;
         //elseif reqFlg = 'U'
         //      and grpDs.strDate <> BGREFF
         //      and grpDs.endDate <> BGREND;
         //  setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVGTR;
         //  dou %eof(HAPIVGTR);
         //    reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVGTR;
         //    if not %eof(HAPIVGTR)
         //        and BGTDLT = *blanks;
         //      if (BGTPER = 'C' or BGTPER = 'F' or BGTPER = 'Y');
         //        errorCount += 1;
         //        errors(errorCount).fldname = 'STRDATE';
         //        errors(errorCount).errcode = '00000000';
         //        errorCount += 1;
         //        errors(errorCount).fldname = 'ENDDATE';
         //        errors(errorCount).errcode = '00000000';
         //        leavesr;
         //      endif;
         //    endif;
         //  enddo;
         //endif;

         if grpDs.name = *blanks;
           errorCount += 1;
           errors(errorCount).fldname = 'NAME';
           errors(errorCount).errcode = '00001551';
         endif;

         // Validate Group Header Type
         if grpDs.groupHdrTyp <> *blanks;
           clear tableDS;
           ecode = grpDs.groupHdrTyp;
           XFXTABL( 'IVTP' : ecode : hmap : edate : sdesc  : ldesc : tind );
           if tind = 'E' or tind = 'D';
             errorCount += 1;
             errors(errorCount).fldname = 'GROUPHDRTYP';
             errors(errorCount).errcode = '00003962';
           else;
             // Check if group header type exists in HAPIVSVC table
             EXEC SQL SELECT bsvLv6
                        INTO :foundivsvccount
                        FROM HAPIVSVC
                       WHERE bsvLv6 = :grpDs.level6
                         AND bsvAcc = :grpDs.account
                         AND bsvGpn = :grpDs.number
                         AND bsvDlt = ' '
                       FETCH FIRST ROW ONLY;
             if SQLSTATE = '00000';
               errorCount += 1;
               errors(errorCount).fldname = 'GROUPHDRTYP';
               errors(errorCount).errcode = '00003962';
             endif;
           endif;
         endif;

         if grpDs.convFactor <> *blanks;
           clear tableDS;
           ecode = grpDs.convFactor;
           XFXTABL( 'XCNV' : ecode : hmap : edate : sdesc  : ldesc : tind );
           if tind = 'E' or tind = 'D';
             errorCount += 1;
             errors(errorCount).fldname = 'CONVFACTOR';
             errors(errorCount).errcode = '00001552';
           elseif (grpDs.units + grpDs.supplUnits <= 0);
             errorCount += 1;
             errors(errorCount).fldname = 'UNITS';
             errors(errorCount).errcode = '00001568';
             errorCount += 1;
             errors(errorCount).fldname = 'SUPPLUNITS';
             errors(errorCount).errcode = '00001568';
           else;
             qnty   = grpDs.units;
             wkcnvf = grpDs.convFactor;
             cnvtyp = '1';
             error  = *blanks;
             XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
             if error = 'Y';
               errorCount += 1;
               errors(errorCount).fldname = 'UNITS';
               errors(errorCount).errcode = '00001568';
             else;
               saveQnty = qnty;
             endif;
             supplQnty = grpDs.supplUnits;
             wkcnvf = grpDs.convFactor;
             cnvtyp = '1';
             error  = *blanks;
             XFXCVTIM(supplQnty : wkcnvf : cnvtyp : error);
             if error = 'Y';
               errorCount += 1;
               errors(errorCount).fldname = 'SUPPLUNITS';
               errors(errorCount).errcode = '00001568';
             else;
               saveSupplQnty = supplQnty;
             endif;
           endif;
         elseif (grpDs.units + grpDs.supplUnits <= 0);
           errorCount += 1;
           errors(errorCount).fldname = 'UNITS';
           errors(errorCount).errcode = '00001568';
           errorCount += 1;
           errors(errorCount).fldname = 'SUPPLUNITS';
           errors(errorCount).errcode = '00001568';
         endif;

         if errorCount <> 0 and rtrncd <> *blanks;
           leavesr;
         endif;

         if reqFlg = 'U';
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVGTR;
           dou %eof(HAPIVGTR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVGTR;
             if not %eof(HAPIVGTR)
                 and BGTDLT = *blanks;
               if BGTPER = 'U' or BGTPER = 'D';
                 if BGSUNU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGMONU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGTUEU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGWEDU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGTHRU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGFRIU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 elseif BGSATU > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 endif;
               elseif BGTPER <> 'T' and BGTPER <> 'X';
                 if BGTMAX > saveQnty + saveSupplQnty;
                   errorCount += 1;
                   errors(errorCount).fldname = 'UNITS';
                   errors(errorCount).errcode = '00000371';
                   errorCount += 1;
                   errors(errorCount).fldname = 'SUPPLUNITS';
                   errors(errorCount).errcode = '00000371';
                   leavesr;
                 endif;
               endif;
             endif;
           enddo;
         endif;

         foundivsvc = *off;
         clear usedQty;
         clear usedSup;
         EXEC SQL SELECT SUM(bsvCov-bsvSup), SUM(bsvSup)
                    INTO :usedQty, :usedSup
                    FROM HAPIVSVC
                   WHERE bsvLv6 = :grpDs.level6
                     AND bsvAcc = :grpDs.account
                     AND bsvGpn = :grpDs.number
                     AND bsvDlt = ' ';
         if (saveQnty <  usedQty);
          errorCount += 1;
          errors(errorCount).fldname = 'UNITS';
          errors(errorCount).errcode = '00001557';
         endif;
         if (saveSupplQnty  <  usedSup);
          errorCount += 1;
          errors(errorCount).fldname = 'SUPPLUNITS';
          errors(errorCount).errcode = '00001557';
         endif;

         if reqFlg = 'U' and changeOk = *off;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               chain (BXVLV6 : BXVPRC) XFFPROCC;
               if %found(HXPPROCC);
                 if grpDs.convFactor <> XFPCNV;
                   errorCount += 1;
                   errors(errorCount).fldname = 'CONVFACTOR';
                   errors(errorCount).errcode = '00001556';
                   leavesr;
                 endif;
               else;
                 chain (BXVPRC) XFFPROCM;
                 if grpDs.convFactor <> XFPCNV and %found(HXPPROCM);
                   errorCount += 1;
                   errors(errorCount).fldname = 'CONVFACTOR';
                   errors(errorCount).errcode = '00001556';
                   leavesr;
                 endif;
               endif;
               foundivsvc = *on;

               if bsvprc <> *blanks;
                 setll (BSVLV6 : BSVACC : BSVPRC) HAFIVSVD;
                 dou %eof(HALIVSVD);
                   reade (BSVLV6 : BSVACC : BSVPRC) HAFIVSVD;
                   if not %eof(HALIVSVD) and bsvGpn <> bxvGpn
                      and BSVDLT = *blanks
                      and BSVFSQ <> BXVFSQ;

                     if not (grpDs.strDate > BXVEND or grpDs.endDate < BXVEFF);

                       errorCount += 1;
                       errors(errorCount).fldname = 'STRDATE';
                       errors(errorCount).errcode = '00001558';
                       errorCount += 1;
                       errors(errorCount).fldname = 'ENDDATE';
                       errors(errorCount).errcode = '00001558';
                       leavesr;
                     endif;
                   endif;
                 enddo;
               elseif BSVRVC <> 0;
                 setll (BSVLV6 : BSVACC : BSVRVC) HAFIVSVDR;
                 dou %eof(HALIVSVDR);
                   reade (BSVLV6 : BSVACC : BSVRVC) HAFIVSVDR;
                   if not %eof(HALIVSVDR) and bsvGpn <> bxvGpn
                      and BSVDLT = *blanks
                      and BSVFSQ <> BXVFSQ;

                     if not (grpDs.strDate > BXVEND or grpDs.endDate < BXVEFF);

                       errorCount += 1;
                       errors(errorCount).fldname = 'STRDATE';
                       errors(errorCount).errcode = '00001558';
                       errorCount += 1;
                       errors(errorCount).fldname = 'ENDDATE';
                       errors(errorCount).errcode = '00001558';
                       leavesr;
                     endif;
                   endif;
                 enddo;
               else;
                 // check for overlapping disciplines
                 exec sql
                   select count(*)
                   into   :disCount
                   from   hapivsvc a
                   where  a.bsvlv6 = :bsvlv6
                     and  a.bsvacc = :bsvacc
                     and  a.bsvseq = :bsvseq
                     and  a.bsvpyr = :bsvpyr
                     and  a.bsvpln = :bsvpln
                     and  a.bsvpol = :bsvpol
                     and  a.bsvfsq <> :bsvfsq
                     and ((:bsvptd = 'Y' and a.bsvptd = :bsvptd)
                           or (:bsvotd = 'Y' and a.bsvotd = :bsvotd)
                           or (:bsvspd = 'Y' and a.bsvspd = :bsvspd)
                           or (:bsvswd = 'Y' and a.bsvswd = :bsvswd)
                           or (:bsvbhd = 'Y' and a.bsvbhd = :bsvbhd)
                           or (:bsvacd = 'Y' and a.bsvacd = :bsvacd)
                           or (:bsvtrd = 'Y' and a.bsvtrd = :bsvtrd)
                           or (:bsvnrd = 'Y' and a.bsvnrd = :bsvnrd)
                           or (:bsvcmd = 'Y' and a.bsvcmd = :bsvcmd)
                           or (:bsvvcd = 'Y' and a.bsvvcd = :bsvvcd)
                           or (:bsvphd = 'Y' and a.bsvphd = :bsvphd)
                           or (:bsvtcd = 'Y' and a.bsvtcd = :bsvtcd))
                     and  a.bsvdlt = ' '
                     and  (:grpDs.strDate between a.bsveff and a.bsvend
                           or :grpDs.endDate between a.bsveff and a.bsvend
                           or (:grpDs.strDate <= a.bsveff and
                                 :grpDs.endDate >= a.bsvend)
                           );

                 if disCount <> 0;
                   errorCount += 1;
                   errors(errorCount).fldname = 'STRDATE';
                   errors(errorCount).errcode = '00001558';
                   errorCount += 1;
                   errors(errorCount).fldname = 'ENDDATE';
                   errors(errorCount).errcode = '00001558';
                   leavesr;
                 endif;
               endif;
             endif;
             chain (BSVLV6 : BSVACC : BSVSEQ) HBFIRNKD;
             if %found(HAPIRNK) and grpDs.strDate < DTEEFF;
               errorCount += 1;
               errors(errorCount).fldname = 'STRDATE';
               errors(errorCount).errcode = '00001592';
               leavesr;
             endif;
           enddo;
           if errorCount = 0 and rtrncd = *blanks;
             changeOk = foundivsvc;
             if changeOk = *on;
               warnMsg = 'Individual IVSVCs exist for this group. '
                         + 'Continuing will update them.';
             endif;
           endif;
         elseif reqFlg = 'U' and changeOk = *on
                 and errorCount = 0 and rtrncd = *blanks;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               BSVNAM = grpDs.name;
               BSVEFF = grpDs.strDate;
               BSVEND = grpDs.endDate;
               update HAFIVSVC;
               exsr gentrn;            //Prorate
             endif;
           enddo;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Delete Group Option                                            **
      **************************************************************************
     c     deleteGrp     begsr
      /free
         number = 3565;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         chain (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSGR;
         if %found(HAPIVSGR) and BGRDLT = *blanks;

           exsr checkGrpDlt;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           BGRDLT = 'D';
           BGRDBY = userid;
           BGRDDT = yudate;
           BGRDTM = runtme;
           update HAFIVSGR;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof();
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof();
               exsr gentrn;
             endif;
           enddo;
         elseif %found(HAPIVSGR) and BGRDLT = 'D';
           rtrncd = '00000784';
           leavesr;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Check Group Delete                                             **
      **************************************************************************
     c     checkGrpDlt   begsr
      /free
         foundivsvc = *off;
         if changeOk = *off;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               foundivsvc = *on;
               leave;
             endif;
           enddo;
           if errorCount = 0 and rtrncd = *blanks;
             changeOk = foundivsvc;
             if changeOk = *on;
               warnMsg = 'Individual IVSVCs exist for this group. '
                         + 'Continuing will flag them as deleted.';
             endif;
           endif;
         elseif changeOk = *on and errorCount = 0 and rtrncd = *blanks;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               BSVDLT = 'D';
               BSVDBY = userid;
               BSVDDT = yudate;
               BSVDTM = runtme;
               update HAFIVSVC;
               exsr gentrn;            //Change?
             endif;
           enddo;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Reinstate Group Option                                         **
      **************************************************************************
     c     reinstGrp     begsr
      /free
         number = 3566;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         chain (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSGR;
         if %found(HAPIVSGR) and BGRDLT = 'D';

           exsr checkGrpRein;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           BGRDLT = *blanks;
           BGRDBY = *blanks;
           BGRDDT = 0;
           BGRDTM = 0;
           BGRCHB = userid;
           BGRCHD = yudate;
           BGRCHT = runtme;
           update HAFIVSGR;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR);
               exsr gentrn;
             endif;
           enddo;
         elseif %found(HAPIVSGR) and BGRDLT <> 'D';
           rtrncd = '00000785';
           leavesr;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Check Group Reinstate                                          **
      **************************************************************************
     c     checkGrpRein  begsr
      /free
         foundivsvc = *off;
         if changeOk = *off;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT <> *blanks;
               foundivsvc = *on;
               leave;
             endif;
           enddo;
           if errorCount = 0 and rtrncd = *blanks;
             changeOk = foundivsvc;
             if changeOk = *on;
               warnMsg = 'Individual IVSVCs exist for this group.'
                         + 'Continuing will reinstate them.';
             endif;
           endif;
         elseif changeOk = *on and errorCount = 0 and rtrncd = *blanks;
           setll (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (grpDs.level6 : grpDs.account : grpDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = 'D';
               BSVDLT = *blanks;
               BSVDBY = *blanks;
               BSVDDT = 0;
               BSVDTM = 0;
               BSVCHB = userid;
               BSVCHD = yudate;
               BSVCHT = runtme;
               update HAFIVSVC;
               exsr gentrn;            //Change?
             endif;
           enddo;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Add Period Option                                              **
      **************************************************************************
     c     addPer        begsr
      /free
         number = 3569;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         clear tableDS;
         ecode = perDs.period;
         XFXTABL( 'BDWY' : ecode : hmap : edate : sdesc  : ldesc : tind );
         if tind = 'E' or tind = 'D';
           errorCount += 1;
           errors(errorCount).fldname = 'PERIOD';
           errors(errorCount).errcode = '00001559';
         else;
           perDs.sequence = %int(%trim(hmap));
         endif;

         if errorCount <> 0 or rtrncd <> *blanks;
           leavesr;
         endif;

         // Validate effective from and to dates...
         exSr validateRestDates;
         if errorCount <> 0 or rtrnCd <> *blanks;
           leaveSr;
         endIf;

         chain (perDs.level6 : perDs.account : perDs.number ) HAFIVSGR;
         if not%found(HAPIVSGR);
           // No group header
           rtrncd = '00001560';
           leavesr;
         elseif %found(HAPIVSGR) and BGRDLT <> *blanks;
           // Group header deleted
           rtrncd = '00001561';
           leavesr;
         endif;

         exsr checkPeriod;
         if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
           leavesr;
         endif;

         clear HAFIVGTR;
         BGTLV6 = perDs.level6;
         BGTACC = perDs.account;
         BGTNBR = perDs.number;
         BGTPER = perDs.period;
         BGTPSQ = perDs.sequence;
         BGTEFF = perDs.effectiveFrom;
         BGTEND = perDs.effectiveTo;
         exsr loadPer;
         BGTCRB = userid;
         BGTCRD = yudate;
         BGTCRT = runtme;
         write HAFIVGTR;

         if newDates = *on;
           chain (perDs.level6 : perDs.account : perDs.number) HAFIVSGR;
           if %found(HAPIVSGR);
             BGREFF = fromDate;
             BGREND = toDate;
             update HAFIVSGR;
           endif;
         endif;

         setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
         dou %eof(HALIVSVGR);
           reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           if not %eof(HALIVSVGR);
             exsr gentrn;
           endif;
         enddo;
      /end-free
     c                   endsr
      **************************************************************************
      **      Validate Dates                                                 **
      **************************************************************************
      /free
        begSr validateRestDates;

          if (reqflg = 'Z' or reqflg = 'C');

            ydate = perDS.effectiveFrom;
            XFXCYMD(ydate : mdate);
            if mdate = 0;
              errorCount += 1;
              errors(errorCount).fldname = 'EFFECTIVEFROM';
              errors(errorCount).errcode = '00003808';
            endif;

            ydate = perDS.effectiveTo;
            XFXCYMD(ydate : mdate);
            if mdate = 0;
              errorCount += 1;
              errors(errorCount).fldname = 'EFFECTIVETO';
              errors(errorCount).errcode = '00003809';
            endif;

            if perDS.effectiveFrom > perDS.effectiveTo;
              errorCount += 1;
              errors(errorCount).fldname = 'EFFECTIVEFROM';
              errors(errorCount).errcode = '00003810';
            endif;

            chain (perDs.level6 : perDs.account : perDs.number) HAFIVSGR;
            if %found(HAPIVSGR);

              if perDS.effectiveFrom < bgreff
                  or perDs.effectiveTo < bgreff;
                errorCount += 1;
                errors(errorCount).fldname = 'EFFECTIVEFROM';
                errors(errorCount).errcode = '00003812';
              endIf;

              if perDS.effectiveFrom > bgrend
                  or perDs.effectiveTo > bgrend;
                errorCount += 1;
                errors(errorCount).fldname = 'EFFECTIVEFROM';
                errors(errorCount).errcode = '00003813';
              endIf;

            endIf;

            if (reqflg = 'Z');
              wOldEffFrom = perDS.effectiveFrom;
              wOldEffTo = perDs.effectiveTo;
            else;
              wOldEffFrom = perDS.oldEffFrom;
              wOldEffTo = perDS.oldEffTo;
            endIf;

            setll (perDs.level6 : perDs.account : perDs.number) HAFIVGTR;
            dou %eof(HAPIVGTR);
              reade (perDs.level6 : perDs.account : perDs.number) HAFIVGTR;
              if not %eof(HAPIVGTR);

                // A period restriction at this level already exists.
                if bgtDlt = *blanks;
                  if  perDs.effectiveTo >= bgteff
                      and perDs.effectiveFrom <= bgtend;

                    if (perDs.effectiveFrom <> bgteff
                          or perDs.effectiveTo <> bgtend)
                        and bgteff <> wOldEffFrom and bgtend <> wOldEffTo;
                      errorCount += 1;
                      errors(errorCount).fldname = 'PERIOD';
                      errors(errorCount).errcode = '00003811';
                    endIf;

                  endIf;

                  if (reqflg = 'Z');

                    if perDs.effectiveFrom = bgteff;
                      errorCount += 1;
                      errors(errorCount).fldname = 'EFFECTIVEFROM';
                      errors(errorCount).errcode = '00003814';
                    endIf;

                    if perDs.effectiveTo = bgtend;
                      errorCount += 1;
                      errors(errorCount).fldname = 'EFFECTIVETO';
                      errors(errorCount).errcode = '00003815';
                    endIf;

                  else;

                    if perDs.effectiveFrom <> wOldEffFrom
                        and perDs.effectiveFrom = bgteff;
                      errorCount += 1;
                      errors(errorCount).fldname = 'EFFECTIVEFROM';
                      errors(errorCount).errcode = '00003814';
                    endIf;

                    if perDs.effectiveTo <> wOldEffTo
                        and perDs.effectiveTo = bgtend;
                      errorCount += 1;
                      errors(errorCount).fldname = 'EFFECTIVETO';
                      errors(errorCount).errcode = '00003815';
                    endIf;

                  endIf;
                endIf;
              endIf;
            endDo;

          endIf;

          if reqflg = 'I';

            setll (perDs.level6 : perDs.account : perDs.number) HAFIVGTR;
            dou %eof(HAPIVGTR);
              reade (perDs.level6 : perDs.account : perDs.number) HAFIVGTR;
              if not %eof(HAPIVGTR) and bgtdlt = *blanks;

                // A period restriction at this level already exists.
                if  perDs.effectiveTo >= bgteff
                      and perDs.effectiveFrom <= bgtend
                      and (perDs.effectiveFrom <> bgteff
                            or perDs.effectiveTo <> bgtend);
                  rtrnCd = '00003811';
                  leave;
                endIf;

                if perDs.effectiveFrom = bgteff;
                  rtrnCd = '00003811';
                  leave;
                endIf;

                if perDs.effectiveTo = bgtend;
                  rtrnCd = '00003811';
                  leave;
                endIf;

              endIf;
            endDo;

          endIf;

        endSr;
      **************************************************************************
      **=      Check Period                                                   **
      **************************************************************************
     c     checkPeriod   begsr
      /free
         if perDs.period = 'C'
            or perDs.period = 'F'
            or perDs.period = 'Y'
            or perDs.period = 'I'
            or perDs.period = 'M'
            or perDs.period = 'B'
            or perDs.period = 'W'
            or perDs.period = 'D';

           if perDs.units <= 0;
             errorCount += 1;
             errors(errorCount).fldname = 'UNITS';
             errors(errorCount).errcode = '00001568';
           elseif perDs.units > BGRCOV;
             errorCount += 1;
             errors(errorCount).fldname = 'UNITS';
             errors(errorCount).errcode = '00001569';
           endif;

           if BGRCNV <> *blanks;
             qnty   = perDs.units;
             wkcnvf = BGRCNV;
             cnvtyp = '1';
             error  = *blanks;
             XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
             if error = 'Y';
               errorCount += 1;
               errors(errorCount).fldname = 'UNITS';
               errors(errorCount).errcode = '00001568';
             endif;
           endif;

           if perDs.period = 'C'
              or perDs.period = 'F'
              or perDs.period = 'I'
              or perDs.period = 'Y';
             exsr calcyrdt;
           endif;

         // Daily Checks
         elseif perDs.period = 'X'
              or perDs.period = 'U';

         /////////////////// Period X /////////////////////
           if perDs.period = 'X';
             if %trim(perDs.sunday) <> 'X'
                 and perDs.sunday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'SUNDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.monday) <> 'X'
                 and perDs.monday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'MONDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.tuesday) <> 'X'
                 and perDs.tuesday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'TUESDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.wednesday) <> 'X'
                 and perDs.wednesday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'WEDNESDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.thursday) <> 'X'
                 and perDs.thursday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'THURSDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.friday) <> 'X'
                 and perDs.friday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'FRIDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if %trim(perDs.saturday) <> 'X'
                 and perDs.saturday <> *blanks;
               errorCount += 1;
               errors(errorCount).fldname = 'SATURDAY';
               errors(errorCount).errcode = '00001571';
             endif;

             if perDs.sunday = *blanks
                 and perDs.monday = *blanks
                 and perDs.tuesday = *blanks
                 and perDs.wednesday = *blanks
                 and perDs.thursday = *blanks
                 and perDs.friday = *blanks
                 and perDs.saturday = *blanks
                 and errorCount = 0 and rtrncd = *blanks;
               rtrncd = '00001572';
               leavesr;
             endif;

         /////////////////// Period U /////////////////////
           elseif perDs.period = 'U';
             totalUnits = 0;

             if perDs.sunday <> *blanks;
               dayUnits = perDs.sunday;
               workField = 'SUNDAY';
               exsr checkDayUnits;
               qntySun = qnty;
             endif;

             if perDs.monday <> *blanks;
               dayUnits = perDs.monday;
               workField = 'MONDAY';
               exsr checkDayUnits;
               qntyMon = qnty;
             endif;

             if perDs.tuesday <> *blanks;
               dayUnits = perDs.tuesday;
               workField = 'TUESDAY';
               exsr checkDayUnits;
               qntyTue = qnty;
             endif;

             if perDs.wednesday <> *blanks;
               dayUnits = perDs.wednesday;
               workField = 'WEDNESDAY';
               exsr checkDayUnits;
               qntyWed = qnty;
             endif;

             if perDs.thursday <> *blanks;
               dayUnits = perDs.thursday;
               workField = 'THURSDAY';
               exsr checkDayUnits;
               qntyThu = qnty;
             endif;

             if perDs.friday <> *blanks;
               dayUnits = perDs.friday;
               workField = 'FRIDAY';
               exsr checkDayUnits;
               qntyFri = qnty;
             endif;

             if perDs.saturday <> *blanks;
               dayUnits = perDs.saturday;
               workField = 'SATURDAY';
               exsr checkDayUnits;
               qntySat = qnty;
             endif;

             if totalUnits = 0
                 and errorCount = 0 and rtrncd = *blanks;
               rtrncd = '00001573';
               leavesr;
             endif;
           endif;

         /////////////////// Period T /////////////////////
         elseif perDs.period = 'T';

           invalidTime = *off;
           if perDs.sundayFrom > 2359
               or perDs.sundayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'SUNDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.sundayTo > 2359
               or perDs.sundayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'SUNDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.sundayFrom > perDs.sundayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'SUNDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'SUNDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.mondayFrom > 2359
               or perDs.mondayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'MONDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.mondayTo > 2359
               or perDs.mondayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'MONDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.mondayFrom > perDs.mondayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'MONDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'MONDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.tuesdayFrom > 2359
               or perDs.tuesdayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'TUESDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.tuesdayTo > 2359
               or perDs.tuesdayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'TUESDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.tuesdayFrom > perDs.tuesdayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'TUESDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'TUESDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.wednesdayFrom > 2359
               or perDs.wednesdayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'WEDNESDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.wednesdayTo > 2359
               or perDs.wednesdayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'WEDNESDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.wednesdayFrom > perDs.wednesdayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'WEDNESDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'WEDNESDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.thursdayFrom > 2359
               or perDs.thursdayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'THURSDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.thursdayTo > 2359
               or perDs.thursdayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'THURSDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.thursdayFrom > perDs.thursdayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'THURSDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'THURSDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.fridayFrom > 2359
               or perDs.fridayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'FRIDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.fridayTo > 2359
               or perDs.fridayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'FRIDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.fridayFrom > perDs.fridayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'FRIDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'FRIDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           invalidTime = *off;
           if perDs.saturdayFrom > 2359
               or perDs.saturdayFrom < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'SATURDAYFROM';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.saturdayTo > 2359
               or perDs.saturdayTo < 0;
             invalidTime = *on;
             errorCount += 1;
             errors(errorCount).fldname = 'SATURDAYTO';
             errors(errorCount).errcode = '00001562';
           endif;
           if perDs.saturdayFrom > perDs.saturdayTo
               and invalidTime = *off;
             errorCount += 1;
             errors(errorCount).fldname = 'SATURDAYTO';
             errors(errorCount).errcode = '00001563';
             errorCount += 1;
             errors(errorCount).fldname = 'SATURDAYFROM';
             errors(errorCount).errcode = '00001563';
           endif;

           if perDs.sundayFrom = 0
               and perDs.sundayTo = 0
               and perDs.mondayFrom = 0
               and perDs.mondayTo = 0
               and perDs.tuesdayFrom = 0
               and perDs.tuesdayTo = 0
               and perDs.wednesdayFrom = 0
               and perDs.wednesdayTo = 0
               and perDs.thursdayFrom = 0
               and perDs.thursdayTo = 0
               and perDs.fridayFrom = 0
               and perDs.fridayTo = 0
               and perDs.saturdayFrom = 0
               and perDs.saturdayTo = 0
               and errorCount = 0 and rtrncd = *blanks;
              rtrncd = '00001574';
              leavesr;
           endif;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Update Period Option                                           **
      **************************************************************************
     c     updatePer     begsr
      /free
         number = 3570;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         clear tableDS;
         ecode = perDs.period;
         XFXTABL( 'BDWY' : ecode : hmap : edate : sdesc  : ldesc : tind );
         if tind = 'E' or tind = 'D';
           errorCount += 1;
           errors(errorCount).fldname = 'PERIOD';
           errors(errorCount).errcode = '00001559';
         elseif perDs.sequence <> %int(%trim(hmap));
           errorCount += 1;
           errors(errorCount).fldname = 'SEQUENCE';
           errors(errorCount).errcode = '00001564';
         endif;

         if errorCount <> 0 or rtrncd <> *blanks;
           leavesr;
         endif;

         chain (perDs.level6 : perDs.account : perDs.number ) HAFIVSGR;
         if not%found(HAPIVSGR);
           // No group header
           rtrncd = '00001560';
           leavesr;
         elseif %found(HAPIVSGR) and BGRDLT <> *blanks;
           // Group header deleted
           rtrncd = '00001561';
           leavesr;
         endif;

         // Validate effective from and to dates...
         exSr validateRestDates;
         if errorCount <> 0 or rtrnCd <> *blanks;
           leaveSr;
         endIf;

         setll (perDs.level6 : perDs.account : perDs.number
                : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;
         dou %eof(HAPIVGTR);
           reade (perDs.level6 : perDs.account : perDs.number
                  : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;

           if %eof(HAPIVGTR) or BGTDLT <> ' ';
             iter;
           endif;

           exsr checkPeriod;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           BGTEFF = perDs.effectiveFrom;
           BGTEND = perDs.effectiveTo;
           exsr loadPer;
           BGTCHB = userid;
           BGTCHD = yudate;
           BGTCHT = runtme;
           update HAFIVGTR;

           if newDates = *on;
             chain (perDs.level6 : perDs.account : perDs.number) HAFIVSGR;
             if %found(HAPIVSGR);
               BGREFF = fromDate;
               BGREND = toDate;
               update HAFIVSGR;
             endif;
           endif;

           setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR);
               exsr gentrn;
             endif;
           enddo;
         enddo;

      /end-free
     c                   endsr
      **************************************************************************
      **=      Load Period Fields                                             **
      **************************************************************************
     c     loadPer       begsr
      /free
          if perDs.period = 'C'
            or perDs.period = 'F'
            or perDs.period = 'Y'
            or perDs.period = 'M'
            or perDs.period = 'B'
            or perDs.period = 'I'
            or perDs.period = 'W';

            if BGRCNV <> *blanks;
              qnty = perDs.units;
              wkcnvf = BGRCNV;
              cnvtyp = '1';
              error  = *blanks;
              XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
              BGTMAX = qnty;
            else;
              BGTMAX = perDs.units;
            endif;

          elseif perDs.period = 'D';

            if BGRCNV <> *blanks;
              qnty = perDs.units;
              wkcnvf = BGRCNV;
              cnvtyp = '1';
              error  = *blanks;
              XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
              BGSUNU = qnty;
              BGMONU = qnty;
              BGTUEU = qnty;
              BGWEDU = qnty;
              BGTHRU = qnty;
              BGFRIU = qnty;
              BGSATU = qnty;
            else;
              BGSUNU = perDs.units;
              BGMONU = perDs.units;
              BGTUEU = perDs.units;
              BGWEDU = perDs.units;
              BGTHRU = perDs.units;
              BGFRIU = perDs.units;
              BGSATU = perDs.units;
            endif;


          elseif perDs.period = 'U';

            if BGRCNV <> *blanks;
              if perDs.sunday <> *blanks;
                qnty = %float(perDs.sunday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGSUNU = %int(qnty);
              else;
                BGSUNU = 0;
              endif;

              if perDs.monday <> *blanks;
                qnty = %float(perDs.monday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGMONU = %int(qnty);
              else;
                BGMONU = 0;
              endif;

              if perDs.tuesday <> *blanks;
                qnty = %float(perDs.tuesday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGTUEU = %int(qnty);
              else;
                BGTUEU = 0;
              endif;

              if perDs.wednesday <> *blanks;
                qnty = %float(perDs.wednesday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGWEDU = %int(qnty);
              else;
                BGWEDU = 0;
              endif;

              if perDs.thursday <> *blanks;
                qnty = %float(perDs.thursday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGTHRU = %int(qnty);
              else;
                BGTHRU = 0;
              endif;

              if perDs.friday <> *blanks;
                qnty = %float(perDs.friday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGFRIU = %int(qnty);
              else;
                BGFRIU = 0;
              endif;

              if perDs.saturday <> *blanks;
                qnty = %float(perDs.saturday);
                wkcnvf = BGRCNV;
                cnvtyp = '1';
                error  = *blanks;
                XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
                BGSATU = %int(qnty);
              else;
                BGSATU = 0;
              endif;

            else;      //   Conversion Factor is blank
              if perDs.sunday <> *blanks;
                BGSUNU = %int(perDs.sunday);
              else;
                BGSUNU = 0;
              endif;

              if perDs.monday <> *blanks;
                BGMONU = %int(perDs.monday);
              else;
                BGMONU = 0;
              endif;

              if perDs.tuesday <> *blanks;
                BGTUEU = %int(perDs.tuesday);
              else;
                BGTUEU = 0;
              endif;

              if perDs.wednesday <> *blanks;
                BGWEDU = %int(perDs.wednesday);
              else;
                BGWEDU = 0;
              endif;

              if perDs.thursday <> *blanks;
                BGTHRU = %int(perDs.thursday);
              else;
                BGTHRU = 0;
              endif;

              if perDs.friday <> *blanks;
                BGFRIU = %int(perDs.friday);
              else;
                BGFRIU = 0;
              endif;

              if perDs.saturday <> *blanks;
                BGSATU = %int(perDs.saturday);
              else;
                BGSATU = 0;
              endif;
            endif;

          elseif perDs.period = 'X';
            BGSUNX = %trim(perDs.sunday);
            BGMONX = %trim(perDs.monday);
            BGTUEX = %trim(perDs.tuesday);
            BGWEDX = %trim(perDs.wednesday);
            BGTHRX = %trim(perDs.thursday);
            BGFRIX = %trim(perDs.friday);
            BGSATX = %trim(perDs.saturday);

          elseif perDs.period = 'T';
            BGSUNF = perDs.sundayFrom;
            BGSUNT = perDs.sundayTo;
            BGMONF = perDs.mondayFrom;
            BGMONT = perDs.mondayTo;
            BGTUEF = perDs.tuesdayFrom;
            BGTUET = perDs.tuesdayTo;
            BGWEDF = perDs.wednesdayFrom;
            BGWEDT = perDs.wednesdayTo;
            BGTHRF = perDs.thursdayFrom;
            BGTHRT = perDs.thursdayTo;
            BGFRIF = perDs.fridayFrom;
            BGFRIT = perDs.fridayTo;
            BGSATF = perDs.saturdayFrom;
            BGSATT = perDs.saturdayTo;

          endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Delete Period Option                                           **
      **************************************************************************
     c     deletePer     begsr
      /free
         number = 3571;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         clear tableDS;
         ecode = perDs.period;
         XFXTABL( 'BDWY' : ecode : hmap : edate : sdesc  : ldesc : tind );
         if tind = 'E' or tind = 'D';
           errorCount += 1;
           errors(errorCount).fldname = 'PERIOD';
           errors(errorCount).errcode = '00001559';
         elseif perDs.sequence <> %int(%trim(hmap));
           errorCount += 1;
           errors(errorCount).fldname = 'SEQUENCE';
           errors(errorCount).errcode = '00001564';
         endif;

         if errorCount <> 0 or rtrncd <> *blanks;
           leavesr;
         endif;

         chain (perDs.level6 : perDs.account : perDs.number ) HAFIVSGR;
         if not%found(HAPIVSGR);
           // No group header
           rtrncd = '00001560';
           leavesr;
         elseif %found(HAPIVSGR) and BGRDLT <> *blanks;
           // Group header deleted
           rtrncd = '00001561';
           leavesr;
         endif;

         activeRcdFnd = *off;
         setll (perDs.level6 : perDs.account : perDs.number
                : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;
         dou %eof(HAPIVGTR);
           reade (perDs.level6 : perDs.account : perDs.number
                  : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;

           if %eof(HAPIVGTR) or BGTDLT <> ' ';
             iter;
           endif;

           exsr checkPeriod;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           activeRcdFnd = *on;
           BGTDLT = 'D';
           BGTDBY = userid;
           BGTDDT = yudate;
           BGTDTM = runtme;
           update HAFIVGTR;

           if newDates = *on;
             chain (perDs.level6 : perDs.account : perDs.number) HAFIVSGR;
             if %found(HAPIVSGR);
               BGREFF = fromDate;
               BGREND = toDate;
               update HAFIVSGR;
             endif;
           endif;

           setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR);
               exsr gentrn;
             endif;
           enddo;
         enddo;

         if not activeRcdFnd;
           //  Cant delete the deleted
           rtrncd = '00000784';
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      Reinstate Period Option                                        **
      **************************************************************************
     c     reinstPer     begsr
      /free
         number = 3572;
         rtncod = *blanks;
         XFCNHKAT(number : rtncod);
         if rtncod = 'E';
           rtrncd = '00000004';
           leavesr;
         endif;

         clear tableDS;
         ecode = perDs.period;
         XFXTABL( 'BDWY' : ecode : hmap : edate : sdesc  : ldesc : tind );
         if tind = 'E' or tind = 'D';
           errorCount += 1;
           errors(errorCount).fldname = 'PERIOD';
           errors(errorCount).errcode = '00001559';
         elseif perDs.sequence <> %int(%trim(hmap));
           errorCount += 1;
           errors(errorCount).fldname = 'SEQUENCE';
           errors(errorCount).errcode = '00001564';
         endif;

         if errorCount <> 0 or rtrncd <> *blanks;
           leavesr;
         endif;

         chain (perDs.level6 : perDs.account : perDs.number ) HAFIVSGR;
         if not%found(HAPIVSGR);
           // No group header
           rtrncd = '00001560';
           leavesr;
         elseif %found(HAPIVSGR) and BGRDLT <> *blanks;
           // Group header deleted
           rtrncd = '00001561';
           leavesr;
         endif;

         // Validate effective from and to dates...
         exSr validateRestDates;
         if errorCount <> 0 or rtrnCd <> *blanks;
           leaveSr;
         endIf;

         deletedRcdFnd = *off;
         setll (perDs.level6 : perDs.account : perDs.number
                : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;
         dou %eof(HAPIVGTR);
           reade (perDs.level6 : perDs.account : perDs.number
                  : perDs.sequence : perDs.period : perDs.oldEffFrom) HAFIVGTR;

           if %eof(HAPIVGTR) or BGTDLT = ' ';
             iter;
           endif;
           qnty = perDs.units;

           exsr checkPeriod;
           if errorCount <> 0 or rtrncd <> *blanks or warnMsg <> *blanks;
             leavesr;
           endif;

           if (BGTPER <> 'D' and BGTPER <> 'U' and BGTPER <> 'X'
               and BGTPER <> 'T' and BGTMAX = qnty) or
              (BGTPER = 'D' and BGSUNU = qnty) or
              (BGTPER = 'U' and
               BGSUNU = qntySun and
               BGMONU = qntyMon and
               BGTUEU = qntyTue and
               BGWEDU = qntyWed and
               BGTHRU = qntyThu and
               BGFRIU = qntyFri and
               BGSATU = qntySat) or
              (BGTPER = 'X' and
               BGSUNX = perDs.sunday and
               BGMONX = perDs.monday and
               BGTUEX = perDs.tuesday and
               BGWEDX = perDs.wednesday and
               BGTHRX = perDs.thursday and
               BGFRIX = perDs.friday and
               BGSATX = perDs.saturday) or
              (BGTPER = 'T' and
               BGSUNF = perDs.sundayFrom and
               BGSUNT = perDs.sundayTo and
               BGMONF = perDs.mondayFrom and
               BGMONT = perDs.mondayTo and
               BGTUEF = perDs.tuesdayFrom and
               BGTUET = perDs.tuesdayTo and
               BGWEDF = perDs.wednesdayFrom and
               BGWEDT = perDs.wednesdayTo and
               BGTHRF = perDs.thursdayFrom and
               BGTHRT = perDs.thursdayTo and
               BGFRIF = perDs.fridayFrom and
               BGFRIT = perDs.fridayTo and
               BGSATF = perDs.saturdayFrom and
               BGSATT = perDs.saturdayTo);

           deletedRcdFnd = *on;
           BGTDLT = *blanks;
           BGTDBY = *blanks;
           BGTDDT = 0;
           BGTDTM = 0;
           BGTCHB = userid;
           BGTCHD = yudate;
           BGTCHT = runtme;
           update HAFIVGTR;

           if newDates = *on;
             chain (perDs.level6 : perDs.account : perDs.number) HAFIVSGR;
             if %found(HAPIVSGR);
               BGREFF = fromDate;
               BGREND = toDate;
               update HAFIVSGR;
             endif;
           endif;

           setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR);
               exsr gentrn;
             endif;
           enddo;

           leave;
           endif;

         enddo;

         if not deletedRcdFnd;
           //  Cant can't reinstate the deleted
           rtrncd = '00000785';
           leavesr;
         endif;

      /end-free
     c                   endsr
      **************************************************************************
      **=      Calculate Year Date                                            **
      **************************************************************************
     c     calcyrdt      begsr
      /free
         fromDate = BGREFF;
         toDate = BGREND;

         if perDs.period = 'Y';
           workDate = fromDate;

           workMonth = 01;
           workDay = 01;
           fromDate = workDate;

           workMonth = 12;
           workDay = 31;
           toDate = workDate;

         elseif perDs.period = 'F';
           workDate = fromDate;
           if XFBFCS = 0;
             errorCount += 1;
             errors(errorCount).fldname = 'PERIOD';
             errors(errorCount).errcode = '00001575';
             leavesr;
           else;
             workMonthDay = XFBFCS;
             fromDate = workDate;

             workYear +=1;
             ydate1 = workDate;
             ydays = 1;
             ydate2 = 0;
             XFXMIND(ydate1 : ydays : ydate2);

             toDate = ydate2;
           endif;

         elseif perDs.period = 'C';
           workDate = fromDate;
           workYear += 1;

           ydate1 = workDate;
           ydays = 1;
           ydate2 = 0;
           XFXMIND(ydate1 : ydays : ydate2);
           toDate = ydate2;

         elseif perDs.period = 'I';
           monitor;
             fromDate = %int(%char(%date(toDate:*iso) - %years(1) + %days(1) :
                             *iso0));
           on-error;
             fromDate = 0;
           endmon;

         endif;

         //foundivsvc = *off;
         if changeOk = *off;
           setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               foundivsvc = *on;
               if bsvprc <> *blanks;
                 setll (BSVLV6 : BSVACC : BSVPRC) HAFIVSVD;
                 dou %eof(HALIVSVD);
                   reade (BSVLV6 : BSVACC : BSVPRC) HAFIVSVD;
                   if not %eof(HALIVSVD)
                       and BXVDLT = *blanks
                       and BSVFSQ <> BXVFSQ;
                     if fromDate >= BXVEFF
                         and toDate >= BXVEND
                         or toDate >= BXVEFF
                         and toDate <= BSVEFF
                         or fromDate <= BXVEFF
                         and toDate >= BXVEND;

                       errorCount += 1;
                       errors(errorCount).fldname = 'PERIOD';
                       errors(errorCount).errcode = '00001558';
                       leavesr;
                     endif;
                   endif;
                 enddo;
               elseif BSVRVC <> 0;
                 setll (BSVLV6 : BSVACC : BSVRVC) HAFIVSVDR;
                 dou %eof(HALIVSVDR);
                   reade (BSVLV6 : BSVACC : BSVRVC) HAFIVSVDR;
                   if not %eof(HALIVSVDR)
                       and BXVDLT = *blanks
                       and BSVFSQ <> BXVFSQ;
                     if fromDate >= BXVEFF
                         and toDate >= BXVEND
                         or toDate >= BXVEFF
                         and toDate <= BSVEFF
                         or fromDate <= BXVEFF
                         and toDate >= BXVEND;

                       errorCount += 1;
                       errors(errorCount).fldname = 'PERIOD';
                       errors(errorCount).errcode = '00001558';
                       leavesr;
                     endif;
                   endif;
                 enddo;
               else;
                 // check for overlapping disciplines
                 exec sql
                   select count(*)
                   into   :disCount
                   from   hapivsvc a
                   where  a.bsvlv6 = :bsvlv6
                     and  a.bsvacc = :bsvacc
                     and  a.bsvseq = :bsvseq
                     and  a.bsvpyr = :bsvpyr
                     and  a.bsvpln = :bsvpln
                     and  a.bsvpol = :bsvpol
                     and  a.bsvfsq <> :bsvfsq
                     and ((:bsvptd = 'Y' and a.bsvptd = :bsvptd)
                           or (:bsvotd = 'Y' and a.bsvotd = :bsvotd)
                           or (:bsvspd = 'Y' and a.bsvspd = :bsvspd)
                           or (:bsvswd = 'Y' and a.bsvswd = :bsvswd)
                           or (:bsvbhd = 'Y' and a.bsvbhd = :bsvbhd)
                           or (:bsvacd = 'Y' and a.bsvacd = :bsvacd)
                           or (:bsvtrd = 'Y' and a.bsvtrd = :bsvtrd)
                           or (:bsvnrd = 'Y' and a.bsvnrd = :bsvnrd)
                           or (:bsvcmd = 'Y' and a.bsvcmd = :bsvcmd)
                           or (:bsvvcd = 'Y' and a.bsvvcd = :bsvvcd)
                           or (:bsvphd = 'Y' and a.bsvphd = :bsvphd)
                           or (:bsvtcd = 'Y' and a.bsvtcd = :bsvtcd))
                     and  a.bsvdlt = ' '
                     and  (:fromDate between a.bsveff and a.bsvend
                           or :toDate between a.bsveff and a.bsvend
                           or (:fromDate <= a.bsveff and :toDate >= a.bsvend)
                          );

                 if disCount <> 0;
                   errorCount += 1;
                   errors(errorCount).fldname = 'PERIOD';
                   errors(errorCount).errcode = '00001558';
                   leavesr;
                 endif;
               endif;
             endif;
             chain (BSVLV6 : BSVACC : BSVSEQ) HBFIRNKD;
             if %found(HAPIRNK) and fromDate < DTEEFF;
               errorCount += 1;
               errors(errorCount).fldname = 'PERIOD';
               errors(errorCount).errcode = '00001592';
               leavesr;
             endif;
           enddo;
           if (toDate <> BGREND or fromDate <> BGREFF)
               and errorCount = 0 and rtrncd = *blanks;

             changeOk = *on;
             workDate = fromDate;
             warnMsg = 'Dates will be changed to ' + %char(workMonth)
                   + '/' + %char(workDay) + '/' + %char(workYear);
             workDate = toDate;
             warnMsg = %trim(warnMsg) + ' to ' + %char(workMonth)
                   + '/' + %char(workDay) + '/' + %char(workYear);
             warnMsg = %trim(warnMsg) + ' Continuing will update them.';
           endif;

         elseif changeOk = *on and errorCount = 0 and rtrncd = *blanks;
           setll (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
           dou %eof(HALIVSVGR);
             reade (perDs.level6 : perDs.account : perDs.number) HAFIVSVC;
             if not %eof(HALIVSVGR) and BSVDLT = *blanks;
               BSVEFF = fromDate;
               BSVEND = toDate;
               update HAFIVSVC;
               exsr gentrn;
             endif;
           enddo;
         endif;

         if toDate <> BGREND or fromDate <> BGREFF;
           newDates = *on;
         endif;
      /end-free
     c                   endsr
      **************************************************************************
      **=      SR gentrn                                                      **
      **************************************************************************
     c     gentrn        begsr
      /FREE
         atbtch = 500000;                               //=atd dem change
         atbtch += HXUBCH;
         repdte = bsveff;
         chain (BSVLV6 : BSVACC : ATBTCH) haftrno7; //=if duplicate
         if %found(HALTRNO7);                           // transaction
           if repdte < atrprd                           // use earlier
                 and repdte >= mmaddt;                  // not before admit
             atrprd = repdte;                           // repro date
             update haftrno7;
           endif;

         else;
           atrpro = 'Y';                                //=ver change -
           if repdte >= mmaddt;
             atrprd = repdte;                           // must reprorate
           else;
             atrprd = mmaddt;                           //=not before admit
           endif;
           atcode = 07;
           atcomm = *blanks;                            //=comments
           atsdte = 0;                                  //=svc date
           ataamt = 0;                                  //=agr amt
           atmsg = 0;                                   //=msg ind
           atduno = 0;                                  //=dun #
           atagcy = 0;                                  //=agcy #
           atrebl = *blanks;                            //=rebill ind
           ataprv = *blanks;                            //=apprv ind

           write haftrno7;                              //=write tran
         endif;
      /END-FREE
     c                   endsr
      **************************************************************************
      **=      Check Units Per Day                                            **
      **************************************************************************
     c     checkDayUnits begsr
      /free
         monitor;
           if %float(dayUnits) < 0;
             errorCount += 1;
             errors(errorCount).fldname = workField;
             errors(errorCount).errcode = '00001568';
           elseif BGRCNV <> *blanks and %float(dayUnits) <> 0;
             qnty   = %float(dayUnits);
             wkcnvf = BGRCNV;
             cnvtyp = '1';
             error  = *blanks;
             XFXCVTIM(qnty : wkcnvf : cnvtyp : error);
             if error = 'Y';
               errorCount += 1;
               errors(errorCount).fldname = workField;
               errors(errorCount).errcode = '00001568';
             elseif %int(qnty) > BGRCOV;
               errorCount += 1;
               errors(errorCount).fldname = workField;
               errors(errorCount).errcode = '00001568';
             else;
               totalUnits += %int(qnty);
             endif;
           elseif %int(dayUnits) > BGRCOV;
             errorCount += 1;
             errors(errorCount).fldname = workField;
             errors(errorCount).errcode = '00001568';
           else;
             totalUnits += %int(dayUnits);
           endif;
         on-error;
           errorCount += 1;
           errors(errorCount).fldname = workField;
           errors(errorCount).errcode = '00001568';
         endmon;
      /END-FREE
     c                   endsr
      **************************************************************************
      **=      Sr XFXEDITS                                                    **
      **************************************************************************
     c     autedt        begsr

     c                   eval      prmdte = grpDs.strDate

     c                   call      'XFXEDITS'
     c                   parm      mmplv6        prmlv6
     c                   parm      mmacct        prmacc
     c                   parm                    prmval
     c                   parm      'INSVCAUT'    prmnam
     c                   parm      brkubc        prmpyr
     c                   parm      brkpln        prmpln
     c                   parm      brkply        prmpol
     c                   parm      brkseq        prmisq
     c                   parm      mmpct1        prmct1
     c                   parm      mmpct2        prmct2
     c                   parm                    prmdte
     c                   parm      0             prmbyr                         =Birth Year
     c                   parm      *blanks       prmResp
     c                   parm      *blanks       prmApp
     c                   parm      0             prmgdt
     c                   parm      *blanks       prmmsg
     c                   parm      *blank        prmcod

     c                   endsr
      **************************************************************************
      **=      get Time                                                       **
      **************************************************************************
     c     getTime       begsr

     c                   time                    curtim

     c                   endsr
      **************************************************************************
     p HAHIVSGR        e
