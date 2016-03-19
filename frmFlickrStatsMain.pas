// Copyright (c) 2015-2016, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit frmFlickrStatsMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdIOHandler, IdIOHandlerStream, IdIOHandlerSocket, IdIOHandlerStack,
  IdSSL, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, Vcl.ComCtrls,
  flickr.repository, frmFlickrUserList,
  ExtCtrls, TeEngine, TeeProcs, Chart, Series, VclTee.TeeGDIPlus,
  System.UITypes, flickr.globals,
  Vcl.ImgList, Vcl.Buttons, System.Win.TaskbarCore, Vcl.Taskbar, System.Actions,
  Vcl.ActnList, IdHashMessageDigest, idHash, IdGlobal, Vcl.OleCtrls, SHDocVw,
  flickr.profiles, flickr.profile, flickr.filtered.list, Vcl.Menus,
  frmFlickrContextList, flickr.tendency, diagnostics, flickr.charts, flickr.organic,
  flickr.organic.stats, flickr.lib.options.agent, flickr.rejected, flickr.lib.utils,
  frmAuthentication, frmSetup, frmChart, flickr.pools.list, flickr.list.comparer,
  flickr.lib.options, flickr.albums.list, flickr.lib.folder, flickr.repository.rest,
  Vcl.Imaging.pngimage, System.zip, DateUtils, flickr.lib.rates,
  flickr.base, flickr.tracker, Vcl.DBCtrls, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.MSSQL, FireDAC.Comp.Client, FireDAC.Comp.DataSet, flickr.album.categoriser,
  System.ImageList, Vcl.WinXCtrls;

type
  TViewType = (TotalViews, TotalLikes, TotalComments, TotalViewsHistogram, TotalLikesHistogram);

  TfrmFlickrMain = class(TForm)
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    XMLDocument1: TXMLDocument;
    Panel1: TPanel;
    ImageList1: TImageList;
    Taskbar1: TTaskbar;
    ActionList1: TActionList;
    TotalViewsLabel: TLabel;
    PopupMenu1: TPopupMenu;
    MarkGroups1: TMenuItem;
    N1: TMenuItem;
    ShowListGroups1: TMenuItem;
    ShowListAlbums1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    StartMarking1: TMenuItem;
    EndMarking1: TMenuItem;
    N4: TMenuItem;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    Label19: TLabel;
    PredictionLabel: TLabel;
    ShowonFlickr1: TMenuItem;
    Label28: TLabel;
    UpdateTimeLabel: TLabel;
    TeeGDIPlus1: TTeeGDIPlus;
    Label32: TLabel;
    FollowingLabel: TLabel;
    N5: TMenuItem;
    Delete1: TMenuItem;
    PopupMenu2: TPopupMenu;
    ShowonFlickr2: TMenuItem;
    N6: TMenuItem;
    BanUnbanforgroupAddition1: TMenuItem;
    ClearSelection1: TMenuItem;
    ProgressBar1: TProgressBar;
    PopupMenu3: TPopupMenu;
    MenuItem1: TMenuItem;
    UncheckAll2: TMenuItem;
    CheckAll2: TMenuItem;
    UncheckAll3: TMenuItem;
    N7: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Authenticate1: TMenuItem;
    About1: TMenuItem;
    Help1: TMenuItem;
    AboutFlickrPhotoAnalytics1: TMenuItem;
    OnlineHelp1: TMenuItem;
    N8: TMenuItem;
    Exit1: TMenuItem;
    AuthenticateSession1: TMenuItem;
    BalloonHint1: TBalloonHint;
    Shape1: TShape;
    Shape2: TShape;
    totalLikesLabel: TLabel;
    TotalCommentsLabel: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    TotalGroupsLabel: TLabel;
    PhotosLabel: TLabel;
    Shape5: TShape;
    Shape6: TShape;
    TotalGroupsRateLabel: TLabel;
    imagephoto: TImage;
    Label43: TLabel;
    SaveTimeLabel: TLabel;
    Image4: TImage;
    Image5: TImage;
    Image10: TImage;
    backup1: TMenuItem;
    N9: TMenuItem;
    PopupMenu4: TPopupMenu;
    ShowGraph1: TMenuItem;
    ShowFavesList1: TMenuItem;
    Panel31: TPanel;
    Panel9: TPanel;
    Panel24: TPanel;
    Panel27: TPanel;
    Image1: TImage;
    Label13: TLabel;
    Label14: TLabel;
    LabelTodayViews: TLabel;
    LabelYesterdayViews: TLabel;
    upgreen1: TImage;
    labelArrow1: TLabel;
    LabelViewsPhoto: TLabel;
    Label16: TLabel;
    LabelViewsYesterdayRate: TLabel;
    LabelViewsTodayRate: TLabel;
    upgreen11: TImage;
    downred1: TImage;
    downred11: TImage;
    Panel25: TPanel;
    Panel28: TPanel;
    Image2: TImage;
    Label12: TLabel;
    LabelYesterdayLikes: TLabel;
    Label15: TLabel;
    LabelTodayLikes: TLabel;
    upgreen2: TImage;
    LabelArrow2: TLabel;
    LabelLikesPhoto: TLabel;
    Label46: TLabel;
    LabelLikesYesterdayRate: TLabel;
    LabelLikesTodayRate: TLabel;
    downred22: TImage;
    downred2: TImage;
    upgreen22: TImage;
    Panel26: TPanel;
    Panel29: TPanel;
    Image3: TImage;
    Label41: TLabel;
    LabelYesterdayComments: TLabel;
    Label42: TLabel;
    LabelTodayComments: TLabel;
    downred3: TImage;
    LabelArrow3: TLabel;
    LabelCommentsPhoto: TLabel;
    Label47: TLabel;
    LabelCommentsYesterdayRate: TLabel;
    LabelCommentsTodayRate: TLabel;
    downred33: TImage;
    upgreen3: TImage;
    upgreen33: TImage;
    PageControl1: TPageControl;
    Dashboard: TTabSheet;
    Splitter6: TSplitter;
    Splitter7: TSplitter;
    Panel14: TPanel;
    Splitter9: TSplitter;
    Splitter4: TSplitter;
    Panel17: TPanel;
    Splitter5: TSplitter;
    Splitter10: TSplitter;
    Splitter26: TSplitter;
    ChartViews: TChart;
    LineSeries4: TLineSeries;
    totalPhotos: TChart;
    LineSeries1: TLineSeries;
    ChartLikes: TChart;
    LineSeries7: TLineSeries;
    chartComments: TChart;
    LineSeries18: TLineSeries;
    Panel18: TPanel;
    Splitter11: TSplitter;
    Splitter12: TSplitter;
    Splitter20: TSplitter;
    mostviewschart: TChart;
    BarSeries3: TBarSeries;
    mostlikeschart: TChart;
    BarSeries4: TBarSeries;
    executionTime: TChart;
    LineSeries3: TAreaSeries;
    chartfollowing: TChart;
    BarSeries6: TBarSeries;
    Panel20: TPanel;
    Splitter15: TSplitter;
    Splitter16: TSplitter;
    Splitter19: TSplitter;
    organicViews: TChart;
    Series7: THorizBarSeries;
    organicLikes: TChart;
    LineSeries5: TLineSeries;
    groupspread: TChart;
    LineSeries6: TLineSeries;
    organicComments: TChart;
    LineSeries8: TLineSeries;
    Panel15: TPanel;
    Splitter8: TSplitter;
    Panel16: TPanel;
    Splitter13: TSplitter;
    Splitter14: TSplitter;
    dailyLikes: TChart;
    BarSeries1: TBarSeries;
    dailyComments: TChart;
    BarSeries5: TBarSeries;
    Panel30: TPanel;
    Splitter27: TSplitter;
    chartStreamViews: TChart;
    Series4: TBarSeries;
    ChartAlbumViews: TChart;
    BarSeries9: TBarSeries;
    Panel19: TPanel;
    Splitter17: TSplitter;
    Splitter18: TSplitter;
    ChartHallLikes: TChart;
    PieSeries1: TPieSeries;
    chartAlbum: TChart;
    Series5: TPieSeries;
    chartHallViews: TChart;
    PieSeries2: TPieSeries;
    dailyViews: TChart;
    LineSeries2: TBarSeries;
    TabSheet2: TTabSheet;
    Splitter1: TSplitter;
    Panel2: TPanel;
    listPhotos: TListView;
    Panel5: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label31: TLabel;
    Process: TLabel;
    Label36: TLabel;
    photoId: TEdit;
    btnAdd: TButton;
    batchUpdate: TButton;
    btnExcel: TButton;
    edtfilter: TEdit;
    btnAddFilter: TButton;
    btnResetFilter: TButton;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Button4: TButton;
    PageControl2: TPageControl;
    Statistics: TTabSheet;
    Panel4: TPanel;
    Splitter21: TSplitter;
    Splitter23: TSplitter;
    Splitter25: TSplitter;
    Panel11: TPanel;
    Splitter2: TSplitter;
    chartItemComments: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    chartItemCommentsH: TChart;
    BarSeries2: TBarSeries;
    Panel22: TPanel;
    Splitter22: TSplitter;
    chartItemViews: TChart;
    LineSeries9: TLineSeries;
    LineSeries10: TLineSeries;
    LineSeries11: TLineSeries;
    ChartItemViewsH: TChart;
    BarSeries7: TBarSeries;
    Panel23: TPanel;
    Splitter24: TSplitter;
    chartItemLikes: TChart;
    LineSeries12: TLineSeries;
    LineSeries13: TLineSeries;
    LineSeries14: TLineSeries;
    chartitemLikesH: TChart;
    BarSeries8: TBarSeries;
    totalGroups: TChart;
    LineSeries15: TLineSeries;
    LineSeries16: TLineSeries;
    LineSeries17: TLineSeries;
    TabSheet3: TTabSheet;
    Panel6: TPanel;
    Label34: TLabel;
    btnGetList: TButton;
    btnAddItems: TButton;
    listPhotosUser: TListView;
    TabSheet5: TTabSheet;
    Panel8: TPanel;
    Label5: TLabel;
    Label11: TLabel;
    Label37: TLabel;
    btnGetGroups: TButton;
    Button2: TButton;
    btnAddPhotos: TButton;
    edtFilterGroup: TEdit;
    Profiles: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    ComboBox1: TComboBox;
    btnLoadProfile: TButton;
    edtProfile: TEdit;
    btnSaveProfile: TButton;
    chkReplaceProfile: TCheckBox;
    chkDisplayOnly: TCheckBox;
    btnDeleteProfile: TButton;
    btnRemoveProfile: TButton;
    btnRemovePhoto: TButton;
    btnFilterOK: TButton;
    btnFilterCancel: TButton;
    btnBanGroups: TButton;
    Button5: TButton;
    btnUnbanGroups: TButton;
    PageControl3: TPageControl;
    tabList: TTabSheet;
    listGroups: TListView;
    tabStatus: TTabSheet;
    Panel10: TPanel;
    mStatus: TMemo;
    TabSheet1: TTabSheet;
    Panel12: TPanel;
    btnLoadHall: TButton;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Splitter3: TSplitter;
    Panel13: TPanel;
    lblTotalAlbum: TLabel;
    Button9: TButton;
    Button10: TButton;
    Memo2: TMemo;
    lvAlbums: TListView;
    TabSheet6: TTabSheet;
    mLogs: TMemo;
    TabSheet7: TTabSheet;
    Panel3: TPanel;
    btnLoadOptions: TButton;
    btnSaveOptions: TButton;
    Panel7: TPanel;
    Label9: TLabel;
    Label1: TLabel;
    Label30: TLabel;
    Label21: TLabel;
    Label8: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label25: TLabel;
    Label35: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    edtMax: TEdit;
    apikey: TEdit;
    edtEmail: TEdit;
    showMarks: TCheckBox;
    chkPending: TCheckBox;
    chkRealTime: TCheckBox;
    chksorting: TCheckBox;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton10: TRadioButton;
    edtMaxLog: TEdit;
    chkResponses: TCheckBox;
    chkRejected: TCheckBox;
    secret: TEdit;
    Button1: TButton;
    listValuesViewsAlbums: TMemo;
    listValuesViewsAlbumsID: TMemo;
    listValuesLikesAlbums: TMemo;
    listValuesLikesAlbumsID: TMemo;
    edtUrlName: TEdit;
    edtUserId: TEdit;
    chkUpdateCollections: TCheckBox;
    chkAddItem: TCheckBox;
    GroupBox2: TGroupBox;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton11: TRadioButton;
    edtWorkspace: TEdit;
    btnLoadDirectory: TButton;
    chkShowButtonHint: TCheckBox;
    edtBackup: TEdit;
    edtAppId: TEdit;
    TabSheet8: TTabSheet;
    Panel21: TPanel;
    btnShowReport: TButton;
    WebBrowser2: TWebBrowser;
    splitview: TSplitView;
    btnLoad: TButton;
    btnSave: TButton;
    btnBackup: TButton;
    Authenticate: TButton;
    btnAbout: TButton;
    Button3: TButton;
    Image6: TImage;
    procedure batchUpdateClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure listPhotosItemChecked(Sender: TObject; Item: TListItem);
    procedure FormDestroy(Sender: TObject);
    function isInSeries(id: string): Boolean;
    procedure listPhotosCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure btnGetListClick(Sender: TObject);
    procedure apikeyChange(Sender: TObject);
    procedure btnAddItemsClick(Sender: TObject);
    procedure btnGetGroupsClick(Sender: TObject);
    procedure AuthenticateClick(Sender: TObject);
    procedure btnExcelClick(Sender: TObject);
    procedure Label2DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnAddPhotosClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure btnSaveProfileClick(Sender: TObject);
    procedure btnLoadProfileClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure listGroupsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure btnFilterOKClick(Sender: TObject);
    procedure btnFilterCancelClick(Sender: TObject);
    procedure MarkGroups1Click(Sender: TObject);
    procedure ShowListGroups1Click(Sender: TObject);
    procedure ShowListAlbums1Click(Sender: TObject);
    procedure StartMarking1Click(Sender: TObject);
    procedure EndMarking1Click(Sender: TObject);
    procedure CheckAll1Click(Sender: TObject);
    procedure UncheckAll1Click(Sender: TObject);
    procedure showMarksClick(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure chartAlbumClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnSaveOptionsClick(Sender: TObject);
    procedure btnLoadOptionsClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure btnLoadHallClick(Sender: TObject);
    procedure ShowonFlickr1Click(Sender: TObject);
    procedure listGroupsItemChecked(Sender: TObject; Item: TListItem);
    procedure btnAddFilterClick(Sender: TObject);
    procedure btnResetFilterClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnRemovePhotoClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure btnShowReportClick(Sender: TObject);
    procedure Label12DblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure listPhotosUserItemChecked(Sender: TObject; Item: TListItem);
    procedure ChartViewsDblClick(Sender: TObject);
    procedure ShowonFlickr2Click(Sender: TObject);
    procedure BanUnbanforgroupAddition1Click(Sender: TObject);
    procedure ClearSelection1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter6Moved(Sender: TObject);
    procedure btnBanGroupsClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure UncheckAll2Click(Sender: TObject);
    procedure CheckAll2Click(Sender: TObject);
    procedure UncheckAll3Click(Sender: TObject);
    procedure photoIdChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure edtMaxChange(Sender: TObject);
    procedure edtMaxLogChange(Sender: TObject);
    procedure secretChange(Sender: TObject);
    procedure edtUrlNameChange(Sender: TObject);
    procedure edtUserIdChange(Sender: TObject);
    procedure edtEmailChange(Sender: TObject);
    procedure TabSheet7Exit(Sender: TObject);
    procedure chkPendingClick(Sender: TObject);
    procedure chkRealTimeClick(Sender: TObject);
    procedure chkRejectedClick(Sender: TObject);
    procedure chkResponsesClick(Sender: TObject);
    procedure chkUpdateCollectionsClick(Sender: TObject);
    procedure chkAddItemClick(Sender: TObject);
    procedure chksortingClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton8Click(Sender: TObject);
    procedure listValuesViewsAlbumsChange(Sender: TObject);
    procedure listValuesViewsAlbumsIDChange(Sender: TObject);
    procedure listValuesLikesAlbumsChange(Sender: TObject);
    procedure listValuesLikesAlbumsIDChange(Sender: TObject);
    procedure btnLoadDirectoryClick(Sender: TObject);
    procedure edtWorkspaceChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure edtProfileChange(Sender: TObject);
    procedure btnSaveProfileMouseEnter(Sender: TObject);
    procedure btnLoadMouseEnter(Sender: TObject);
    procedure btnSaveMouseEnter(Sender: TObject);
    procedure AuthenticateMouseEnter(Sender: TObject);
    procedure Button3MouseEnter(Sender: TObject);
    procedure btnAddMouseEnter(Sender: TObject);
    procedure batchUpdateMouseEnter(Sender: TObject);
    procedure Button4MouseEnter(Sender: TObject);
    procedure btnExcelMouseEnter(Sender: TObject);
    procedure btnGetListMouseEnter(Sender: TObject);
    procedure btnAddItemsMouseEnter(Sender: TObject);
    procedure btnGetGroupsMouseEnter(Sender: TObject);
    procedure Button5MouseEnter(Sender: TObject);
    procedure Button2MouseEnter(Sender: TObject);
    procedure btnAddPhotosMouseEnter(Sender: TObject);
    procedure btnRemovePhotoMouseEnter(Sender: TObject);
    procedure btnBanGroupsMouseEnter(Sender: TObject);
    procedure btnLoadProfileMouseEnter(Sender: TObject);
    procedure btnDeleteProfileMouseEnter(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure btnLoadHallMouseEnter(Sender: TObject);
    procedure Button9MouseEnter(Sender: TObject);
    procedure Button10MouseEnter(Sender: TObject);
    procedure btnLoadOptionsMouseEnter(Sender: TObject);
    procedure btnSaveOptionsMouseEnter(Sender: TObject);
    procedure Button1MouseEnter(Sender: TObject);
    procedure btnShowReportMouseEnter(Sender: TObject);
    procedure chkShowButtonHintClick(Sender: TObject);
    procedure backup1Click(Sender: TObject);
    procedure chartHallViewsClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartHallLikesClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chartAlbumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShowGraph1Click(Sender: TObject);
    procedure ShowFavesList1Click(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    procedure btnUnbanGroupsMouseEnter(Sender: TObject);
    procedure btnUnbanGroupsClick(Sender: TObject);
    procedure btnRemoveProfileMouseEnter(Sender: TObject);
    procedure btnRemoveProfileClick(Sender: TObject);
    procedure dailyViewsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dailyViewsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image6Click(Sender: TObject);
  private
    procedure LoadForms(repository: IFlickrRepository);
    function ExistPhotoInList(id: string; var Item: TListItem): Boolean;
    procedure RequestInformation_REST_Flickr(id: string; organicStat : IFlickrOrganicStats);
    procedure UpdateCounts;
    procedure UpdateTotals(onlyLabels : boolean);
    procedure UpdateChart(totalViews, totalLikes, totalComments, totalPhotos, totalSpreadGroups: Integer);
    procedure UpdateGlobals();
    procedure UpdateAnalytics();
    procedure LoadHallOfFame(repository: IFlickrRepository);
    function SaveToExcel(AView: TListView; ASheetName, AFileName: string): Boolean;
    function getTotalAlbumsCounts: Integer;
    procedure UpdateSingleStats(id: string);
    function SaveToExcelGroups(AView: TListView; ASheetName, AFileName: string): Boolean;
    function ExportGraphToExcel(viewsource : TViewType; ASheetName, AFileName: string): Boolean;
    procedure LoadProfiles;
    procedure UpdateDailyViewsChart;
    procedure UpdateDailyStreamViewsChart;
    procedure UpdateDailyAlbumViewsChart;
    procedure UpdateDailyLikesChart;
    procedure UpdateDailyCommentsChart;
    procedure UpdateMostViewedChart;
    procedure UpdateMostLikedChart;
    procedure UpdateLabels;
    procedure AlbumLog(s: string);
    procedure UpdateOrganics;
    procedure UpdateLabel;
    procedure UpdateLabelGroups;
    procedure UpdateLabelPhotos;
    procedure ResizeChartsDashBoard;
    procedure ClearAllCharts;
    procedure LoadOptions;
    procedure ShowHint(description : string; Sender: TObject);
    procedure InitialiseDashboardTrend;
    procedure UpdateArrows(valueYesterday, valueToday: integer; rateYesterday, rateToday: double; arrow1, arrow2, arrow3, arrow4: TImage; label1 : TLabel);
    procedure UpdateRate(valueBeforeYesterday, valueYesterday, valueToday: integer; out rateYesterday, rateToday: double); overload;
    procedure UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday : integer; out rateYesterday : double; out rateToday : double); overload;
    procedure UpdateListGroupsThrottleRemaining(base: IBase);
    procedure OpenPhotosAlbum(value: string);
    procedure OpenPhotosAlbumId(photoSetId : string; title : string; PhotosCount : string);
    procedure SendParseUpdate;
    procedure AutoZooming;
    function getTotalStreamViews: Integer;
  public
    repository: IFlickrRepository;
    globalsRepository: IFlickrGlobals;
    organic : IFlickrOrganic;
    CheckedSeries: TStringList;
    userToken: string;
    userTokenSecret: string;
    flickrProfiles: IProfiles;
    FilteredGroupList: IFilteredList;
    NavigationUrl : String;
    ListDisplay : TfrmFlickrContext;
    ListUsers : TfrmUserList;
    startMark : integer;
    endMark : integer;
    flickrChart : IFlickrChart;
    filterEnabled : boolean;
    optionsAgent : IoptionsAgent;
    options : IOptions;
    rejected: IRejected;
    FProcessingStop : boolean;
    FGroupStop : boolean;
    RepositoryLoaded : boolean;
    FDirtyOptions : boolean;
    FAvoidMessage : boolean;
    Fgreen : TColor;
    FRed : TColor;
    FYellow : TColor;
    FViolet : TColor;
    FHardBlue : TColor;
    FSoftBlue : TColor;
    FHardPink : TColor;
    FSoftPink : TColor;
    FHardGreen : TColor;
    FSoftGreen : TColor;
    FOrange : TColor;
    FColorViews : TColor;
    FColorLikes : TColor;
    FColorComments : TColor;
    FColorGroups : TColor;
    FColorPhotos : TColor;
    FColorSpread : TColor;
    FViewsP : TColor;
    FLikesP : TColor;
    FCommentsP : TColor;
    procedure Log(s: string);
  end;

const
  timeout = 200;

var
  frmFlickrMain: TfrmFlickrMain;

implementation

uses
  flickr.photos, flickr.stats, flickr.rest, flickr.top.stats, ComObj,
  flickr.oauth, StrUtils, flickr.access.token, flickr.lib.parallel, ActiveX,
  System.SyncObjs, generics.collections,
  flickr.pools, flickr.albums, System.inifiles, flickr.time, ShellApi,
  flickr.lib.response, flickr.lib.logging, frmSplash, flickr.lib.email.html,
  flickr.pools.histogram, flickr.lib.item, flickr.lib.item.list, flickr.photos.histogram,
  flickr.lib.email, flickr.lib.math, flickr.lib.backup, flickr.xml.helper, frmFlickrPhotoSetInfo,
  flickr.lib.parse, flickr.stats.global, flickr.users.info, flickr.http.lib, flickr.lib.parallel.albums,
  flickr.lib.parallel.groups.load;

{$R *.dfm}

procedure TfrmFlickrMain.apikeyChange(Sender: TObject);
begin
  btnSave.Enabled := true;
  btnBackup.Enabled := true;
  if optionsAgent.flickrApiKey <> apikey.text then
    FDirtyOptions := true;
end;

procedure TfrmFlickrMain.AuthenticateClick(Sender: TObject);
var
  OAuthUrl: string;
  response: string;
  oauth_callback_confirmed: string;
  oauth_token: string;
  oauth_token_secret: string;
  authenticationScreen: TfrmAuthenticate;
begin
  // apikey.text 0edf6f13dc6309c822b59ae8bb783df6
  // secret.text b9e217d1c0333300
  if apikey.text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  if secret.text = '' then
  begin
    showmessage('Secret key can''t be empty');
    exit;
  end;
  //btnGetToken.enabled := false;
  Log('authentication started');
  // oauthr authentication
  Log('Generating request token query for ' + apikey.text + ' ' + secret.text);
  OAuthUrl := TOAuth.New(apikey.text, secret.text).GenerateRequestTokenQuery();
  // Example successful response
  // oauth_callback_confirmed=true&
  // oauth_token=72157650113637896-43aba062d96def83&
  // oauth_token_secret=153e53c592649722
  Log('Calling OAuth URL ' + OAuthUrl);
  response := IdHTTP1.Get(OAuthUrl);
  Log('OAuth URL response ' + response);

  // Parsing response
  Log('Parsing response');
  // oauth_callback_confirmed=true&oauth_token=72157650113637896-43aba062d96def83&oauth_token_secret=153e53c592649722
  response := response.Replace('oauth_callback_confirmed', '');
  response := response.Replace('oauth_token', '');
  response := response.Replace('_secret', '');
  // =true&=72157650113637896-43aba062d96def83&=153e53c592649722
  oauth_callback_confirmed := AnsiLeftStr(response, AnsiPos('&', response));
  // =true&
  response := AnsiRightStr(response, length(response) - length(oauth_callback_confirmed));
  // =72157650113637896-43aba062d96def83&=153e53c592649722
  oauth_token := AnsiLeftStr(response, AnsiPos('&', response));
  // =72157650113637896-43aba062d96def83&
  response := AnsiRightStr(response, length(response) - length(oauth_token));
  // =153e53c592649722
  oauth_token_secret := response;

  // Clean the parameters
  oauth_callback_confirmed := oauth_callback_confirmed.Replace('=', '').Replace('&', '');
  oauth_token := oauth_token.Replace('=', '').Replace('&', '');
  oauth_token_secret := oauth_token_secret.Replace('=', '').Replace('&', '');
  Log('oauth_callback_confirmed= ' + oauth_callback_confirmed);
  Log('oauth_token= ' + oauth_token);
  Log('oauth_token_secret= ' + oauth_token_secret);

  userTokenSecret := oauth_token_secret;

  authenticationScreen := TfrmAuthenticate.Create(Application);
  try
    authenticationScreen.NavigationUrl := NavigationUrl;
    authenticationScreen.WebBrowser1.Navigate('https://www.flickr.com/services/oauth/authorize?oauth_token=' + oauth_token + '&perms=write');
    authenticationScreen.ShowModal;
  finally
    authenticationScreen.Free;
  end;

  Log('Navigating to ' + 'https://www.flickr.com/services/oauth/authorize?oauth_token=' + oauth_token + '&perms=write');
  NavigationUrl := 'https://www.flickr.com/services/oauth/authorize?oauth_token=' + oauth_token + '&perms=write';
  //WebBrowser1.Navigate('https://www.flickr.com/services/oauth/authorize?oauth_token=' + oauth_token + '&perms=write');
  //showmessage('Authorise the application in the browser and once you get the example page, press Get token button');
  //btnGetToken.enabled := true;
end;

procedure TfrmFlickrMain.AuthenticateMouseEnter(Sender: TObject);
begin
  ShowHint('Authorize your Flickr Account', Sender);
end;

procedure TfrmFlickrMain.backup1Click(Sender: TObject);
var
  date : TDate;
begin
  date := TBackup.Produce(options.Workspace);
  edtBackup.Text := DateToStr(date);
  ShowMessage('Backup has been completed!');
end;

procedure TfrmFlickrMain.BanUnbanforgroupAddition1Click(Sender: TObject);
var
  id : string;
  photo : IPhoto;
begin
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    photo := repository.GetPhoto(id);
    photo.banned := not photo.banned;
    listPhotos.Items[listPhotos.ItemIndex].SubItems[10] := photo.banned.ToString();
  end;
end;

procedure TfrmFlickrMain.Log(s: string);
var
  max : string;
begin
  max := edtMaxLog.text;
  if mLogs.Lines.Count > max.ToInteger then
    mLogs.Lines.Clear;
  mLogs.Lines.Add(DateTimeToStr(Now) + ' ' + s);
end;

procedure TfrmFlickrMain.AlbumLog(s: string);
var
  max : string;
begin
  max := edtMaxLog.text;
  if memo2.Lines.Count > max.ToInteger then
    memo2.Lines.Clear;
  memo2.Lines.Add(DateTimeToStr(Now) + ' ' + s);
end;

procedure TfrmFlickrMain.MarkGroups1Click(Sender: TObject);
var
  id : string;
  photo : IPhoto;
  j: Integer;
  pool : IPool;
begin
  //Mark the groups
  if listgroups.Items.Count > 0 then
  begin
    if listPhotos.ItemIndex <> -1 then
    begin
      btnFilterCancelClick(sender);
      listGroups.Visible := false;
      listGroups.OnItemChecked := nil;
      listgroups.OnCustomDrawItem := nil;
      id := listPhotos.Items[listPhotos.ItemIndex].Caption;
      photo := repository.GetPhoto(id);

      for j := 0 to listgroups.Items.count-1 do
      begin
        if photo.Groups.Exists(listgroups.Items[j].caption, pool) then
          listgroups.Items[j].Checked := true;
      end;

      listGroups.Visible := true;
      listGroups.OnItemChecked := listGroupsItemChecked;
      listgroups.OnCustomDrawItem := listGroupsCustomDrawItem;
    end;
  end;
end;

procedure TfrmFlickrMain.MenuItem1Click(Sender: TObject);
var
  i: Integer;
begin
  listPhotosUser.OnItemChecked := nil;
  for i := 0 to listPhotosUser.Items.Count - 1 do
  begin
    listPhotosUser.Items[i].Checked := true;
  end;
  Label34.Caption := 'Number of items: ' + InttoStr(listPhotosUser.Items.Count) + ' (' + InttoStr(listPhotosUser.Items.Count) + ') selected';
  listPhotosUser.OnItemChecked := listPhotosItemChecked;
end;

procedure TfrmFlickrMain.photoIdChange(Sender: TObject);
begin
  btnAdd.Enabled := photoId.Text <> '';
end;

procedure TfrmFlickrMain.batchUpdateClick(Sender: TObject);
var
  i: Integer;
  st : TStopWatch;
  stGeneral : TStopWatch;
  photos: TList<string>;
  generalUpdate : boolean;
  organicStat : IFlickrOrganicStats;
  totalContacts : integer;
begin
  if apikey.text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  photos := TList<string>.Create;
  try
    btnGetGroups.Enabled := false;
    btnAddPhotos.Enabled := false;
    btnRemovePhoto.Enabled := false;
    btnBanGroups.Enabled := false;
    btnUnBanGroups.Enabled := false;
    for i := 0 to listPhotos.Items.Count - 1 do
    begin
      if (listPhotos.Items[i].Checked) then
        photos.Add(listPhotos.Items[i].Caption);
    end;
    generalUpdate := (listPhotos.Items.Count = photos.Count) and (photos.Count > 0);
    batchUpdate.Enabled := false;
    FProcessingStop := false;
    btnLoad.Enabled := false;
    btnGetList.Enabled := false;
    ProgressBar1.Visible := true;
    photoId.Enabled := false;
    btnAdd.Enabled := false;
    Process.Visible := true;
    Taskbar1.ProgressState := TTaskBarProgressState.Normal;
    Taskbar1.ProgressMaxValue := listPhotos.Items.Count;
    listPhotos.OnItemChecked := nil;
    listPhotos.OnCustomDrawSubItem := nil;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := photos.Count;
    organicStat := nil;
    if generalUpdate then
    begin
      stGeneral := TStopWatch.Create;
      stGeneral.Start;
      organicStat := TFlickrOrganicStats.create();
    end;
    for i := 0 to photos.Count-1 do
    begin
      Process.Caption := 'Processing image: ' + photos[i] + ' ' + i.ToString + ' out of ' + photos.Count.ToString;
      ProgressBar1.position := i;
      Taskbar1.ProgressValue := i;
      Application.ProcessMessages;
      st := TStopWatch.Create;
      st.Start;
      RequestInformation_REST_Flickr(photos[i], organicStat);
      st.Stop;
      log('Getting history for ' + photos[i] + ': ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      if FProcessingStop then
        break;
    end;
    if generalUpdate then
    begin
      stGeneral.Stop;
      organicStat.executionTime := stGeneral.ElapsedMilliseconds;
      organicStat.date := Date;
      try
        totalContacts := 0;
        if userToken <> '' then
        begin
          try
            totalContacts := TRepositoryRest.getNumberOfContacts(optionsAgent);
          except on E: Exception do
            log('Exception reading contacts: ' + e.message);
          end;
          if (totalContacts < 0) and (organic.Globals.Count > 0) then
            totalContacts := organic.Globals[organic.Globals.Count-1].following;
        end;
      finally
        organicStat.Following := totalContacts;
      end;
      organic.AddGlobals(organicStat);

      TParseAnalyticsAPI.UpdateClient(edtAppId.Text, repository.photos.Count,
        globalsRepository.globals[globalsRepository.globals.Count-1].views,
        globalsRepository.globals[globalsRepository.globals.Count-1].likes,
        globalsRepository.globals[globalsRepository.globals.Count-1].Comments,
        TUtils.GetVersion);
    end;

    FProcessingStop := false;
    ProgressBar1.Visible := false;
    Process.Visible := false;
    if not filterEnabled then
    begin
      UpdateTotals(false);
      LoadHallOfFame(repository);
    end;
    listPhotos.OnItemChecked := listPhotosItemChecked;
    listPhotos.OnCustomDrawSubItem := listPhotosCustomDrawSubItem;
    btnSave.Enabled := true;
    btnBackup.Enabled := true;
    photoId.Enabled := true;
    btnLoad.Enabled := true;
    btnAdd.Enabled := true;
    Taskbar1.ProgressValue := 0;
    btnGetList.Enabled := true;
    batchUpdate.Enabled := true;
  finally
    btnGetGroups.Enabled := true;
    btnAddPhotos.Enabled := true;
    btnRemovePhoto.Enabled := true;
    btnBanGroups.Enabled := true;
    btnUnBanGroups.Enabled := true;
    photos.Free;
  end;
  showMessage('Update has been completed');
end;

procedure TfrmFlickrMain.batchUpdateMouseEnter(Sender: TObject);
begin
  ShowHint('Update the items selected. If all items are selected, '+sLineBreak+'organic views will be updated also.', Sender);
end;

procedure TfrmFlickrMain.UpdateTotals(onlyLabels : boolean);
var
  i: Integer;
  Item: TListItem;
  totalViews, totalViewsacc: Integer;
  totalLikes, totalLikesacc: Integer;
  totalComments, totalCommentsacc: Integer;
  statGlobal: IStatGlobal;
  totalAlbumViews : integer;
  totalStreamViews : integer;
begin
  totalViewsacc := 0;
  totalLikesacc := 0;
  totalCommentsacc := 0;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    Item := listPhotos.Items[i];
    totalViews := StrToInt(Item.SubItems.Strings[1]);
    totalViewsacc := totalViewsacc + totalViews;

    totalLikes := StrToInt(Item.SubItems.Strings[2]);;
    totalLikesacc := totalLikesacc + totalLikes;

    totalComments := StrToInt(Item.SubItems.Strings[3]);;
    totalCommentsacc := totalCommentsacc + totalComments;
  end;

  totalAlbumViews := getTotalAlbumsCounts();
  totalStreamViews := getTotalStreamViews();
  totalViewsacc := totalViewsacc + totalAlbumViews + totalStreamViews;
  statGlobal := TStatGlobal.Create(Date, totalViewsacc, totalLikesacc, totalCommentsacc, totalAlbumViews, totalStreamViews);
  globalsRepository.AddGlobals(statGlobal);

  if not onlyLabels then
  begin
    UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count, repository.getTotalSpreadGroups());
    UpdateGlobals();
    UpdateAnalytics();
    UpdateOrganics();
  end;
  UpdateLabels();
end;

procedure TfrmFlickrMain.UpdateLabels();
var
  viewsYesterday, viewsToday, commentsToday, viewsBeforeYesterday : integer;
  likesYesterday, likesToday, commentsYesterday : integer;
  totalViews, albumViews, streamViews, totalLikes, totalNegLikes, AccLikes : integer;
  totalViewsFormat, albumViewsFormat, streamViewsFormat : string;
  Rate1, Rate2 : double;
  i: Integer;
begin
  if globalsRepository.globals.Count > 3 then
  begin
    viewsBeforeYesterday := globalsRepository.globals[globalsRepository.globals.Count-2].views-globalsRepository.globals[globalsRepository.globals.Count-4].views;
  end
  else
  begin
    viewsBeforeYesterday := 0;
  end;
  if globalsRepository.globals.Count > 2 then
  begin
    viewsYesterday := globalsRepository.globals[globalsRepository.globals.Count-2].views-globalsRepository.globals[globalsRepository.globals.Count-3].views;
    likesYesterday := globalsRepository.globals[globalsRepository.globals.Count-2].likes-globalsRepository.globals[globalsRepository.globals.Count-3].likes;
    commentsYesterday := globalsRepository.globals[globalsRepository.globals.Count-2].Comments-globalsRepository.globals[globalsRepository.globals.Count-3].Comments;
  end
  else
  begin
    viewsYesterday := 0;
    likesYesterday := 0;
    commentsYesterday := 0;
  end;
  LabelYesterdayViews.Caption :=  Format('%n',[viewsYesterday.ToDouble]).Replace('.00','');
  LabelYesterdayLikes.Caption :=  Format('%n',[likesYesterday.ToDouble]).Replace('.00','');
  LabelYesterdayComments.Caption :=  Format('%n',[commentsYesterday.ToDouble]).Replace('.00','');
  if globalsRepository.globals.Count > 1 then
  begin
    viewsToday := globalsRepository.globals[globalsRepository.globals.Count-1].views-globalsRepository.globals[globalsRepository.globals.Count-2].views;
    likesToday := globalsRepository.globals[globalsRepository.globals.Count-1].likes-globalsRepository.globals[globalsRepository.globals.Count-2].likes;
    commentsToday := globalsRepository.globals[globalsRepository.globals.Count-1].Comments-globalsRepository.globals[globalsRepository.globals.Count-2].Comments;
  end
  else if globalsRepository.globals.Count = 1 then
  begin
    viewsToday := globalsRepository.globals[globalsRepository.globals.Count-1].views;
    likesToday := globalsRepository.globals[globalsRepository.globals.Count-1].likes;
    commentsToday := globalsRepository.globals[globalsRepository.globals.Count-1].Comments;
  end
  else
  begin
    viewsToday := 0;
    likesToday := 0;
    commentsToday := 0;
  end;
  LabelTodayViews.Caption :=  Format('%n',[viewsToday.ToDouble]).Replace('.00','');
  LabelTodayLikes.Caption :=  Format('%n',[likesToday.ToDouble]).Replace('.00','');
  LabelTodayComments.Caption :=  Format('%n',[commentsToday.ToDouble]).Replace('.00','');

  UpdateRate(viewsBeforeYesterday, viewsYesterday, viewsToday, Rate1, Rate2);
  LabelViewsYesterdayRate.caption := Format('%n',[Rate1]).Replace('.00','') + '%';
  LabelViewsTodayRate.caption := Format('%n',[Rate2]).Replace('.00','') + '%';
  UpdateArrows(viewsYesterday, viewsToday, Rate1, Rate2, upgreen1, downred1, upgreen11, downred11, LabelArrow1);

  UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday, Rate1, Rate2);
  LabelLikesYesterdayRate.caption := Format('%n',[Rate1]).Replace('.00','') + '%';
  LabelLikesTodayRate.caption := Format('%n',[Rate2]).Replace('.00','') + '%';
  UpdateArrows(likesYesterday, likesToday, Rate1, Rate2, upgreen2, downred2, upgreen22, downred22, LabelArrow2);

  UpdateRate(viewsYesterday, viewsToday, commentsYesterday, commentsToday, Rate1, Rate2);
  LabelCommentsYesterdayRate.caption := Format('%n',[Rate1]).Replace('.00','') + '%';
  LabelCommentsTodayRate.caption := Format('%n',[Rate2]).Replace('.00','') + '%';
  UpdateArrows(commentsYesterday, commentsToday, Rate1, Rate2, upgreen3, downred3, upgreen33, downred33, LabelArrow3);

  totalViews := globalsRepository.globals[globalsRepository.globals.Count-1].views;
  albumViews := globalsRepository.globals[globalsRepository.globals.Count-1].albumViews;
  streamViews := globalsRepository.globals[globalsRepository.globals.Count-1].streamViews;

  totalViewsFormat := Format('%n',[totalViews.ToDouble]).Replace('.00','');
  albumViewsFormat := Format('%n',[albumViews.ToDouble]).Replace('.00','');
  streamViewsFormat := Format('%n',[streamViews.ToDouble]).Replace('.00','');

  TotalViewsLabel.Caption :=  'Stream Views: '+streamViewsFormat+' Album Views: '+albumViewsFormat+' Total: '+totalViewsFormat;

  totalLikes := globalsRepository.globals[globalsRepository.globals.Count-1].likes;
  totalNegLikes := 0;
  for i := 0 to organic.globals.Count-1 do
  begin
    totalNegLikes := totalNegLikes + organic.Globals[i].lostLikes;
  end;
  AccLikes := totalLikes + totalNegLikes;
  TotalLikesLabel.Caption :=  'Likes '+Format('%n',[totalLikes.ToDouble]).Replace('.00','')+' Lost '+Format('%n',[totalNegLikes.ToDouble]).Replace('.00','')+' Total: '+ Format('%n',[AccLikes.ToDouble]).Replace('.00','');

  totalViews := globalsRepository.globals[globalsRepository.globals.Count-1].Comments;
  TotalCommentsLabel.Caption :=  Format('%n',[totalViews.ToDouble]).Replace('.00','');
  UpdateTimeLabel.Caption := DateToStr(globalsRepository.globals[globalsRepository.globals.Count-1].date);
end;

procedure TfrmFlickrMain.EndMarking1Click(Sender: TObject);
var
  i: Integer;
begin
  if (listPhotos.ItemIndex <> -1) and (startMark <> -1) then
  begin
    endMark :=  listPhotos.ItemIndex;
    for i := 0 to listPhotos.Items.Count - 1 do
    begin
      if (i >= startMark) and (i <= endMark) then
        listPhotos.Items[i].Checked := not listPhotos.Items[i].Checked;
    end;
  end;
  UpdateLabel();
end;

function TfrmFlickrMain.ExistPhotoInList(id: string; var Item: TListItem): Boolean;
var
  i: Integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < listPhotos.Items.Count) do
  begin
    found := listPhotos.Items[i].Caption = id;
    inc(i);
  end;
  if found then
    Item := listPhotos.Items[i - 1];
  Result := found;
end;

procedure TfrmFlickrMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmFlickrMain.RadioButton1Click(Sender: TObject);
var
  comparer : integer;
begin
  comparer := 0;
  if radioButton1.checked then
    comparer := 0;
  if radioButton2.checked then
    comparer := 2;
  if radioButton3.checked then
    comparer := 1;
  if radioButton4.checked then
    comparer := 3;
  if radioButton5.checked then
    comparer := 4;
  if radioButton6.checked then
    comparer := 5;
  if radioButton7.checked then
    comparer := 6;
  if radioButton10.Checked then
    comparer := 9;
  if options.SortedBy <> comparer then
    FDirtyOptions := true;
end;

procedure TfrmFlickrMain.RadioButton8Click(Sender: TObject);
var
  comparerGroups : integer;
begin
  comparerGroups := 0;
  if RadioButton8.Checked then
    comparerGroups := 7;
  if RadioButton9.Checked then
    comparerGroups := 8;
  if RadioButton11.Checked then
    comparerGroups := 9;
  if options.SortedByGropus <> comparerGroups then
    FDirtyOptions := true;
end;

procedure TfrmFlickrMain.RequestInformation_REST_Flickr(id: string; organicStat : IFlickrOrganicStats);
var
  Item, itemExisting: TListItem;
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  views, title, likes, comments, taken: string;
  stat: IStat;
  photo, existing: IPhoto;
  Albums: TAlbumList;
  Groups: TPoolList;
  tags : string;
  photoGroups : IPhoto;
begin
  CoInitialize(nil);
  try
    THttpRest.Post(TFlickrRest.New(optionsAgent).getInfo(id), procedure (iXMLRootNode : IXMLNode)
    begin
      views := iXMLRootNode.attributes['views'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <owner>
      tags := '';
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'title' then
          title := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'dates' then
          taken := iXMLRootNode4.attributes['taken'];
        if iXMLRootNode4.NodeName = 'comments' then
          comments := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'tags' then
        begin
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
          while iXMLRootNode5 <> nil do
          begin
            if (iXMLRootNode5.NodeName = 'tag') and (iXMLRootNode5.NodeValue <> 'jordicorbilla') and (iXMLRootNode5.NodeValue <> 'jordicorbillaphotography') then
              tags := tags + iXMLRootNode5.NodeValue + ',';
            iXMLRootNode5 := iXMLRootNode5.NextSibling;
          end;
        end;
        if iXMLRootNode4.NodeName = 'location' then
          break;

        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

    THttpRest.Post(TFlickrRest.New(optionsAgent).getFavorites(id), procedure (iXMLRootNode : IXMLNode)
    begin
      likes := iXMLRootNode.attributes['total'];
    end);

    stat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

    photoGroups := repository.GetPhoto(id);
    if photoGroups <> nil then
    begin
      photoGroups.LoadGroups;
      Groups := photoGroups.Groups;
      photoGroups.LoadAlbums;
      Albums := photoGroups.Albums;
    end
    else
    begin
      photo := TPhoto.Create(id, title, taken, tags);
      Groups := photo.Groups;
      Albums := photo.Albums;
    end;

    if chkUpdateCollections.checked then
    begin
      THttpRest.Post(TFlickrRest.New(optionsAgent).getAllContexts(id), procedure (iXMLRootNode : IXMLNode)
      begin
        while iXMLRootNode <> nil do
        begin
          if iXMLRootNode.NodeName = 'set' then
            Albums.AddItem(TAlbum.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title']));
          if iXMLRootNode.NodeName = 'pool' then
            Groups.AddItem(TPool.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title'], Date));
          iXMLRootNode := iXMLRootNode.NextSibling;
        end;
      end);
    end;

    TTracking.TrackPhoto(options.Workspace, id, '1', '50', optionsAgent);

    if repository.ExistPhoto(id, existing) then
    begin
      photo := existing;

      if organicStat <> nil then
      begin
        if photo.getTotalViewsDay() >= views.ToInteger() then
          organicStat.negativeViews := organicStat.negativeViews + 1
        else
          organicStat.positiveViews := organicStat.positiveViews + 1;

        if photo.getTotalLikesDay() > likes.ToInteger() then
          organicStat.lostLikes := organicStat.lostLikes + 1
        else if photo.getTotalLikesDay() = likes.ToInteger() then
          organicStat.negativeLikes := organicStat.negativeLikes + 1
        else
          organicStat.positiveLikes := organicStat.positiveLikes + 1;

        if photo.getTotalCommentsDay() > comments.ToInteger() then
          organicStat.lostComments := organicStat.lostComments + 1
        else if photo.getTotalCommentsDay() = comments.ToInteger() then
          organicStat.negativeComments := organicStat.negativeComments + 1
        else
          organicStat.positiveComments := organicStat.positiveComments + 1;

        organicStat.TotalGroups := repository.getTotalSpreadGroups();
      end;

      photo.Title := title; //replace the title as it changes
      photo.Taken := taken;
      photo.tags := tags;
      photo.AddStats(stat, albums, groups);
      photo.LastUpdate := Date;
    end
    else
    begin
      photo.AddStats(stat, albums, groups);
      photo.LastUpdate := Date;
      repository.AddPhoto(photo);
    end;

    if not ExistPhotoInList(id, itemExisting) then
    begin
      Item := frmFlickrMain.listPhotos.Items.Add;
      Item.Caption := frmFlickrMain.photoId.text;
      Item.SubItems.Add(title);
      Item.SubItems.Add(views);
      Item.SubItems.Add(likes);
      Item.SubItems.Add(comments);
      Item.SubItems.Add(DateToStr(Date));
      if views = '0' then
        views := '1';
      Item.SubItems.Add(taken);
      Item.SubItems.Add(photo.TotalAlbums.ToString());
      Item.SubItems.Add(photo.TotalGroups.ToString());
      Item.SubItems.Add(tags);
      Item.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      Item.SubItems.Add(photo.banned.ToString());
      Item.SubItems.Add(photo.getTrend.ToString());
      Item.SubItems.Add(photo.OmitGroups);
    end
    else
    begin
      itemExisting.Caption := id;
      itemExisting.SubItems.Clear;
      itemExisting.SubItems.Add(title);
      itemExisting.SubItems.Add(views);
      itemExisting.SubItems.Add(likes);
      itemExisting.SubItems.Add(comments);
      itemExisting.SubItems.Add(DateToStr(Date));
      if views = '0' then
        views := '1';
      itemExisting.SubItems.Add(taken);
      itemExisting.SubItems.Add(photo.TotalAlbums.ToString());
      itemExisting.SubItems.Add(photo.TotalGroups.ToString());
      itemExisting.SubItems.Add(tags);
      itemExisting.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      itemExisting.SubItems.Add(photo.banned.ToString());
      itemExisting.SubItems.Add(photo.getTrend.ToString());
      itemExisting.SubItems.Add(photo.OmitGroups);
    end;
    if chkRealTime.Checked then
      UpdateTotals(true);
  finally
    CoUninitialize;
  end;
end;

procedure TfrmFlickrMain.btnAboutClick(Sender: TObject);
var
  SplashScreen: TfrmFlickrSplash;
begin
  SplashScreen := TfrmFlickrSplash.Create(Application);
  try
    SplashScreen.label2.Visible := false;
    SplashScreen.label4.Visible := true;
    SplashScreen.btnClose.Visible := true;
    SplashScreen.ShowModal;
  finally
    SplashScreen.Free;
  end;
end;

procedure TfrmFlickrMain.btnAddClick(Sender: TObject);
var
  id : string;
begin
  if apikey.text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  listPhotos.OnItemChecked := nil;
  listPhotos.OnCustomDrawSubItem := nil;
  RequestInformation_REST_Flickr(photoId.text, nil);
  id := photoId.text;
  photoId.text := '';
  UpdateTotals(false);
  btnSave.Enabled := true;
  btnBackup.Enabled := true;
  listPhotos.OnItemChecked := listPhotosItemChecked;
  listPhotos.OnCustomDrawSubItem := listPhotosCustomDrawSubItem;
  UpdateLabel;
  if not FAvoidMessage then
    showMessage('Photo '+ id +' has been added');
end;

procedure TfrmFlickrMain.btnLoadClick(Sender: TObject);
var
  st : TStopWatch;
  comparer : TCompareType;
begin
  LoadOptions;
  if not fileExists(options.Workspace + '\flickrRepository.xml') then
  begin
    showMessage('There are no saved repositories, please save one first!');
    exit;
  end;
  if Assigned(repository) then
  begin
    repository := nil;
    if chksorting.Checked then
    begin
      comparer := tCompareViews;
      if radioButton1.checked then
        comparer := tCompareId;
      if radioButton2.checked then
        comparer := tCompareViews;
      if radioButton3.checked then
        comparer := tCompareLikes;
      if radioButton4.checked then
        comparer := tCompareComments;
      if radioButton5.checked then
        comparer := tCompareTaken;
      if radioButton6.checked then
        comparer := tCompareAlbums;
      if radioButton7.checked then
        comparer := tCompareGroups;
      if radioButton10.Checked then
        comparer := tCompareTrend;
      repository := TFlickrRepository.Create(chksorting.Checked, comparer)
    end
    else
      repository := TFlickrRepository.Create();
  end;
  st := TStopWatch.Create;
  st.Start;
  repository.sorted := chksorting.Checked;
  repository.version := TUtils.GetVersion;
  repository.load(options.Workspace + '\flickrRepository.xml');
  st.Stop;
  log('Loading repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  if Assigned(globalsRepository) then
  begin
    globalsRepository := nil;
    globalsRepository := TFlickrGlobals.Create();
  end;
  st := TStopWatch.Create;
  st.Start;
  globalsRepository.load(options.Workspace + '\flickrRepositoryGlobal.xml');
  st.Stop;
  log('Loading repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  if Assigned(organic) then
  begin
    organic := nil;
    organic := TFlickrOrganic.Create();
  end;
  st := TStopWatch.Create;
  st.Start;
  organic.load(options.Workspace + '\flickrOrganic.xml');
  st.Stop;
  log('Loading repository flickrOrganic: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  if Assigned(FilteredGroupList) then
  begin
    FilteredGroupList := nil;
    FilteredGroupList := TFilteredList.Create(tCompareRemaining);
  end;
  st := TStopWatch.Create;
  st.Start;
  FilteredGroupList.load(options.Workspace + '\flickrGroups.xml');
  FilteredGroupList.sort;
  btnFilterCancelClick(sender);
  st.Stop;
  log('Loading repository flickrGroups: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  st := TStopWatch.Create;
  st.Start;
  LoadForms(repository);
  st.Stop;
  log('Loading Forms for repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  st := TStopWatch.Create;
  st.Start;
  LoadHallOfFame(repository);
  st.Stop;
  log('Loading Hall of fame: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  st := TStopWatch.Create;
  st.Start;
  LoadProfiles();
  st.Stop;
  log('Loading Profiles: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  getTotalAlbumsCounts();
  RepositoryLoaded := true;
  btnShowReport.Enabled := true;
  Button9.Enabled := true;
  btnLoadHall.Enabled := true;
  AutoZooming();

  //Send that the app has been started
  SendParseUpdate();
  showMessage('Repository has been loaded');
end;

procedure TfrmFlickrMain.AutoZooming();
begin
  flickrChart.AutoZooming(2/3, dailyviews);
  flickrChart.AutoZooming(2/3, dailylikes);
  flickrChart.AutoZooming(2/3, dailycomments);
  flickrChart.AutoZooming(2/3, chartViews);
  flickrChart.AutoZooming(2/3, chartLikes);
  flickrChart.AutoZooming(2/3, chartComments);
  flickrChart.AutoZooming(2/3, ExecutionTime);
  flickrChart.AutoZooming(2/3, mostViewsChart);
  flickrChart.AutoZooming(2/3, mostLikesChart);
  flickrChart.AutoZooming(2/3, chartFollowing);
  flickrChart.AutoZooming(2/3, OrganicViews);
  flickrChart.AutoZooming(2/3, OrganicLikes);
  flickrChart.AutoZooming(2/3, OrganicComments);
  flickrChart.AutoZooming(2/3, groupspread);
end;

procedure TfrmFlickrMain.SendParseUpdate();
var
  success : boolean;
  count : integer;
begin
  success := false;
  count := 0;
  while not success do
  begin
    success := TParseAnalyticsAPI.Active();
    inc(count);
    if count > 5 then
    begin
      success := true;
      showMessage('Coudn''t connect with the server, please make sure your internet connection is on');
    end;
  end;
end;

procedure TfrmFlickrMain.btnLoadDirectoryClick(Sender: TObject);
var
  myFolder : string;
begin
  myFolder := TFolder.BrowseForFolder;
  edtWorkspace.Text := myFolder;
end;

procedure TfrmFlickrMain.btnLoadHallClick(Sender: TObject);
begin
  LoadHallOfFame(repository);
  showMessage('Hall of Fame has been loaded');
end;

procedure TfrmFlickrMain.btnLoadHallMouseEnter(Sender: TObject);
begin
  ShowHint('Load the Hall of Fame', Sender);
end;

procedure TfrmFlickrMain.btnLoadMouseEnter(Sender: TObject);
begin
  ShowHint('Load flickr Repository from XML file', Sender);
end;

procedure TfrmFlickrMain.LoadProfiles();
var
  i : integer;
begin
  ComboBox1.Clear;
  if Assigned(flickrProfiles) then
  begin
    flickrProfiles := nil;
    flickrProfiles := TProfiles.Create();
  end;
  flickrProfiles.load(options.workspace + '\flickrProfiles.xml');
  for i := 0 to flickrProfiles.list.Count - 1 do
  begin
    ComboBox1.AddItem(flickrProfiles.list[i].Name + ' (' + flickrProfiles.list[i].GroupId.Count.ToString + ')', nil);
  end;
end;

procedure TfrmFlickrMain.LoadHallOfFame(repository: IFlickrRepository);
var
  topStats: TTopStats;
  maxValues: string;
  SeriesV, SeriesL : TPieSeries;
begin
  topStats := TTopStats.Create(repository);
  Memo1.Lines.Clear;
  Memo1.Lines.Add('************************************');
  Memo1.Lines.Add('************ HALL OF FAME **********');
  Memo1.Lines.Add('************************************');

  if chartHallViews.SeriesList.Count > 0 then
    chartHallViews.RemoveAllSeries;
  if chartHallLikes.SeriesList.Count > 0 then
    chartHallLikes.RemoveAllSeries;

  SeriesV := flickrChart.GetNewPieSeries(chartHallViews, true);
  SeriesL := flickrChart.GetNewPieSeries(chartHallLikes, true);

  maxValues := edtMax.text;
  Memo1.Lines.Add(topStats.GetTopXNumberOfViews(maxValues.ToInteger(), SeriesV));
  Memo1.Lines.Add(topStats.GetTopXNumberOfLikes(maxValues.ToInteger(), SeriesL));
  Memo1.Lines.Add(topStats.GetTopXNumberOfComments(maxValues.ToInteger()));

  chartHallViews.AddSeries(SeriesV);
  chartHallLikes.AddSeries(SeriesL);
  topStats.Free;
end;

procedure TfrmFlickrMain.LoadForms(repository: IFlickrRepository);
begin
  apikey.text := repository.apikey;
  secret.text := repository.secret;
  edtUserId.text := repository.UserId;
  authenticate.Enabled := true;
  listPhotos.Clear;
  UpdateCounts();
  UpdateLabel;
end;

procedure TfrmFlickrMain.UpdateCounts();
var
  i: Integer;
  Item: TListItem;
  totalViews, totalViewsacc: Integer;
  totalLikes, totalLikesacc: Integer;
  totalComments, totalCommentsacc: Integer;
begin
  totalViewsacc := 0;
  totalLikesacc := 0;
  totalCommentsacc := 0;

  listPhotos.OnCustomDrawSubItem := nil;
  listPhotos.OnItemChecked := nil;

  for i := 0 to repository.photos.Count - 1 do
  begin
    Item := listPhotos.Items.Add;
    Item.Caption := repository.photos[i].id;
    Item.SubItems.Add(repository.photos[i].title);
    totalViews := repository.photos[i].TotalViews;
    totalViewsacc := totalViewsacc + totalViews;
    Item.SubItems.Add(IntToStr(totalViews));
    totalLikes := repository.photos[i].TotalLikes;
    totalLikesacc := totalLikesacc + totalLikes;
    Item.SubItems.Add(IntToStr(totalLikes));
    totalComments := repository.photos[i].TotalComments;
    totalCommentsacc := totalCommentsacc + totalComments;
    Item.SubItems.Add(IntToStr(totalComments));
    Item.SubItems.Add(DateToStr(repository.photos[i].LastUpdate));
    Item.SubItems.Add(repository.photos[i].taken); //taken
    Item.SubItems.Add(repository.photos[i].TotalAlbums.ToString()); //albums
    Item.SubItems.Add(repository.photos[i].TotalGroups.ToString()); //groups
    if totalViews = 0 then
      totalViews := 1;
    Item.SubItems.Add(repository.photos[i].tags);
    Item.SubItems.Add(FormatFloat('0.##%', (totalLikes / totalViews) * 100.0));
    Item.SubItems.Add(repository.photos[i].banned.ToString());
    Item.SubItems.Add(repository.photos[i].getTrend.ToString());
    Item.SubItems.Add(repository.photos[i].OmitGroups);
  end;

  listPhotos.OnCustomDrawSubItem := listPhotosCustomDrawSubItem;
  listPhotos.OnItemChecked := listPhotosItemChecked;

  UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count, repository.getTotalSpreadGroups());
  UpdateGlobals();
  UpdateLabels();
  UpdateOrganics();
  UpdateAnalytics();
end;

procedure TfrmFlickrMain.UpdateOrganics();
var
  SeriesPositive, SeriesNegative, SeriesLost  : TBarSeries;
  i: Integer;
  Series : TAreaSeries;
  SeriesTendency, SeriesTendency2: TLineSeries;
  chartTendency : ITendency;
  NegativeTendency : ITendency;
  viewsTendency : integer;
  color : TColor;
  min : double;
  value : double;
  max : double;
begin
  if organicViews.SeriesList.Count > 0 then
    organicViews.RemoveAllSeries;

  if organic.Globals.Count = 0 then
    exit;

  SeriesPositive := flickrChart.GetNewBarSeries(organicViews);
  OrganicViews.AddSeries(SeriesPositive);
  SeriesPositive.MultiBar := mbStacked;
  SeriesPositive.BarWidthPercent := 25;

  SeriesNegative := flickrChart.GetNewBarSeries(organicViews);
  OrganicViews.AddSeries(SeriesNegative);
  SeriesNegative.MultiBar := mbStacked;
  SeriesNegative.BarWidthPercent := 25;

  chartTendency := TTendency.Create;

  min := (organic.Globals[0].positiveViews * 100)/ (organic.Globals[0].positiveViews + organic.Globals[0].negativeViews);
  for i := 0 to organic.Globals.Count-1 do
  begin
    value := (organic.Globals[i].positiveViews * 100)/ (organic.Globals[i].positiveViews + organic.Globals[i].negativeViews);
    chartTendency.AddXY(i, Round(value));
    SeriesPositive.AddXY(organic.Globals[i].date, value, '', Fgreen);
    SeriesNegative.AddXY(organic.Globals[i].date, (organic.Globals[i].negativeViews * 100)/ (organic.Globals[i].positiveViews + organic.Globals[i].negativeViews), '', FHardBlue);
    if value < min then
      min := value;
  end;
  chartTendency.Calculate;
  SeriesTendency := flickrChart.GetNewLineSeries(organicViews);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  organicViews.AddSeries(SeriesTendency);
  organicViews.Axes.Left.SetMinMax(min, 100);

  if organicLikes.SeriesList.Count > 0 then
    organicLikes.RemoveAllSeries;

  SeriesPositive := flickrChart.GetNewBarSeries(organicLikes);
  //SeriesNegative := flickrChart.GetNewBarSeries(organicLikes);
  SeriesLost := flickrChart.GetNewBarSeries(organicLikes);
  organicLikes.AddSeries(SeriesPositive);
  //organicLikes.AddSeries(SeriesNegative);
  organicLikes.AddSeries(SeriesLost);
  SeriesPositive.MultiBar := mbStacked;
  SeriesPositive.BarWidthPercent := 25;
//  SeriesNegative.MultiBar := mbStacked;
//  SeriesNegative.BarWidthPercent := 25;
  SeriesLost.MultiBar := mbStacked;
  SeriesLost.BarWidthPercent := 25;

  chartTendency := TTendency.Create;
  NegativeTendency := TTendency.Create;

  for i := 0 to organic.Globals.Count-1 do
  begin
    chartTendency.AddXY(i, Round((organic.Globals[i].positiveLikes * 100)/ (organic.Globals[i].positiveLikes + organic.Globals[i].negativeLikes + organic.Globals[i].lostLikes)));
    SeriesPositive.AddXY(organic.Globals[i].date, (organic.Globals[i].positiveLikes * 100)/ (organic.Globals[i].positiveLikes + organic.Globals[i].negativeLikes + organic.Globals[i].lostLikes), '', Fgreen);
    //SeriesNegative.AddXY(organic.Globals[i].date, (organic.Globals[i].negativeLikes * 100)/ (organic.Globals[i].positiveLikes + organic.Globals[i].negativeLikes + organic.Globals[i].lostLikes), '', clred);
    NegativeTendency.AddXY(i, Round((organic.Globals[i]. lostLikes * 100)/ (organic.Globals[i].positiveLikes + organic.Globals[i].negativeLikes + organic.Globals[i].lostLikes)));
    SeriesLost.AddXY(organic.Globals[i].date, (organic.Globals[i]. lostLikes * 100)/ (organic.Globals[i].positiveLikes + organic.Globals[i].negativeLikes + organic.Globals[i].lostLikes), '', FOrange);
  end;

  chartTendency.Calculate;
  NegativeTendency.Calculate;
  SeriesTendency := flickrChart.GetNewLineSeries(organicLikes);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  organicLikes.AddSeries(SeriesTendency);

  SeriesTendency2 := flickrChart.GetNewLineSeries(organicLikes);

  //Adding only first and last item
  viewsTendency := NegativeTendency.tendencyResult(0);
  SeriesTendency2.AddXY(organic.Globals[0].date, viewsTendency, '', Fred);
  viewsTendency := NegativeTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency2.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', Fred);

  organicLikes.AddSeries(SeriesTendency2);

  if organicComments.SeriesList.Count > 0 then
    organicComments.RemoveAllSeries;

  SeriesPositive := flickrChart.GetNewBarSeries(organicComments);
  //SeriesNegative := flickrChart.GetNewBarSeries(organicComments);
  SeriesLost := flickrChart.GetNewBarSeries(organicComments);
  organicComments.AddSeries(SeriesPositive);
  //organicComments.AddSeries(SeriesNegative);
  organicComments.AddSeries(SeriesLost);
  SeriesPositive.MultiBar := mbStacked;
  SeriesPositive.BarWidthPercent := 25;
//  SeriesNegative.MultiBar := mbStacked;
//  SeriesNegative.BarWidthPercent := 25;
  SeriesLost.MultiBar := mbStacked;
  SeriesLost.BarWidthPercent := 25;

  chartTendency := TTendency.Create;

  for i := 0 to organic.Globals.Count-1 do
  begin
    chartTendency.AddXY(i, Round((organic.Globals[i].positiveComments * 100)/ (organic.Globals[i].positiveComments + organic.Globals[i].negativeComments + organic.Globals[i].lostComments)));
    SeriesPositive.AddXY(organic.Globals[i].date, (organic.Globals[i].positiveComments * 100)/ (organic.Globals[i].positiveComments + organic.Globals[i].negativeComments + organic.Globals[i].lostComments), '', Fgreen);
    //SeriesNegative.AddXY(organic.Globals[i].date, (organic.Globals[i].negativeComments * 100)/ (organic.Globals[i].positiveComments + organic.Globals[i].negativeComments + organic.Globals[i].lostComments), '', clred);
    SeriesLost.AddXY(organic.Globals[i].date, (organic.Globals[i]. lostComments * 100)/ (organic.Globals[i].positiveComments + organic.Globals[i].positiveComments + organic.Globals[i].lostComments), '', FOrange);
  end;

  chartTendency.Calculate;
  SeriesTendency := flickrChart.GetNewLineSeries(organicComments);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  organicComments.AddSeries(SeriesTendency);

  if executionTime.SeriesList.Count > 0 then
    executionTime.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(executionTime);

  chartTendency := TTendency.Create;

  for i := 0 to organic.Globals.Count-1 do
  begin
    chartTendency.AddXY(i, Round(TTime.GetAdjustedTimeValue(organic.Globals[i].executionTime)));
    Series.AddXY(organic.Globals[i].date, TTime.GetAdjustedTimeValue(organic.Globals[i].executionTime), '', FViolet);
  end;
  executionTime.AddSeries(Series);

  chartTendency.Calculate;

  SeriesTendency := flickrChart.GetNewLineSeries(executionTime);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  executionTime.AddSeries(SeriesTendency);

  //Group spread
  if groupspread.SeriesList.Count > 0 then
    groupspread.RemoveAllSeries;

  SeriesPositive := flickrChart.GetNewBarSeries(groupspread);

  chartTendency := TTendency.Create;
  color := RGB(Random(255), Random(255), Random(255));

  min := organic.Globals[0].TotalGroups;
  max := 0;
  for i := 0 to organic.Globals.Count-1 do
  begin
    value := organic.Globals[i].TotalGroups;
    chartTendency.AddXY(i, Round(value));
    SeriesPositive.AddXY(organic.Globals[i].date, value, '', color);
    if value < min then
      min := value;
    if value > max then
      max := value;
  end;
  min := min - 0.10 * (min);
  max := max + 0.10 * (max);
  groupspread.Axes.Left.SetMinMax(min, max);
  chartTendency.Calculate;
  SeriesTendency := flickrChart.GetNewLineSeries(groupspread);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  groupspread.AddSeries(SeriesTendency);


  //Following
  if chartFollowing.SeriesList.Count > 0 then
    chartFollowing.RemoveAllSeries;

  SeriesPositive := flickrChart.GetNewBarSeries(chartFollowing);

  chartTendency := TTendency.Create;
  color := RGB(Random(255), Random(255), Random(255));

  min := organic.Globals[0].Following;
  max := 0;
  for i := 0 to organic.Globals.Count-1 do
  begin
    value := organic.Globals[i].Following;
    chartTendency.AddXY(i, Round(value));
    SeriesPositive.AddXY(organic.Globals[i].date, value, '', color);
    if value < min then
      min := value;
    if value > max then
      max := value;
  end;
  min := min - 0.10 * (min);
  max := max + 0.10 * (max);
  chartFollowing.Axes.Left.SetMinMax(min, max);
  chartTendency.Calculate;
  SeriesTendency := flickrChart.GetNewLineSeries(chartFollowing);

  FollowingLabel.Caption := Format('%n',[organic.Globals[organic.Globals.Count-1].Following.ToDouble]).Replace('.00','');

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(organic.Globals[0].date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(organic.Globals.Count-1);
  SeriesTendency.AddXY(organic.Globals[organic.Globals.Count-1].date, viewsTendency, '', FYellow);

  chartFollowing.AddSeries(SeriesTendency);
end;

procedure TfrmFlickrMain.UpdateGlobals;
var
  Series, SeriesTendency: TLineSeries;
  color: TColor;
  i: Integer;
  chartTendency : ITendency;
  viewsTendency : integer;
  photoHistogram : TPhotoHistogram;
  histogram :  TList<IItem>;
  accumulated : integer;
begin
  if ChartViews.SeriesList.Count > 0 then
    ChartViews.RemoveAllSeries;

  chartTendency := TTendency.Create;

  Series := flickrChart.GetNewLineSeries(ChartViews);
  color := RGB(Random(255), Random(255), Random(255));

  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    chartTendency.AddXY(i, globalsRepository.Globals[i].views);
    Series.AddXY(globalsRepository.globals[i].Date, globalsRepository.globals[i].views, '', color);
  end;
  ChartViews.AddSeries(Series);
  chartTendency.Calculate;

  SeriesTendency := flickrChart.GetNewLineSeries(ChartViews);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(globalsRepository.globals[0].Date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(globalsRepository.globals.Count - 1);
  SeriesTendency.AddXY(globalsRepository.globals[globalsRepository.globals.Count - 1].Date, viewsTendency, '', FYellow);
//  for i := 0 to globalsRepository.globals.Count - 1 do
//  begin
//    viewsTendency := chartTendency.tendencyResult(i);
//    SeriesTendency.AddXY(globalsRepository.globals[i].Date, viewsTendency, '', color);
//  end;
  ChartViews.AddSeries(SeriesTendency);

  if ChartLikes.SeriesList.Count > 0 then
    ChartLikes.RemoveAllSeries;

  Series := flickrChart.GetNewLineSeries(ChartLikes);
  color := RGB(Random(255), Random(255), Random(255));
  chartTendency := TTendency.Create;
  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    chartTendency.AddXY(i, globalsRepository.globals[i].likes);
    Series.AddXY(globalsRepository.globals[i].Date, globalsRepository.globals[i].likes, '', color);
  end;
  ChartLikes.AddSeries(Series);

  chartTendency.Calculate;

  SeriesTendency := flickrChart.GetNewLineSeries(ChartLikes, false);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(globalsRepository.globals[0].Date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(globalsRepository.globals.Count - 1);
  SeriesTendency.AddXY(globalsRepository.globals[globalsRepository.globals.Count - 1].Date, viewsTendency, '', FYellow);

  if ChartComments.SeriesList.Count > 0 then
    ChartComments.RemoveAllSeries;

  Series := flickrChart.GetNewLineSeries(ChartComments);
  color := RGB(Random(255), Random(255), Random(255));
  chartTendency := TTendency.Create;
  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    chartTendency.AddXY(i, globalsRepository.globals[i].Comments);
    Series.AddXY(globalsRepository.globals[i].Date, globalsRepository.globals[i].Comments, '', color);
  end;
  ChartComments.AddSeries(Series);

  chartTendency.Calculate;

  SeriesTendency := flickrChart.GetNewLineSeries(ChartComments, false);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(globalsRepository.globals[0].Date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(globalsRepository.globals.Count - 1);
  SeriesTendency.AddXY(globalsRepository.globals[globalsRepository.globals.Count - 1].Date, viewsTendency, '', FYellow);
  //################################
  if totalPhotos.SeriesList.Count > 0 then
    totalPhotos.RemoveAllSeries;

  chartTendency := TTendency.Create;

  Series := flickrChart.GetNewLineSeries(totalPhotos);
  color := RGB(Random(255), Random(255), Random(255));

  photoHistogram := TPhotoHistogram.Create(repository.photos);
  histogram := photoHistogram.Histogram;
  accumulated := 0;
  for i := 0 to histogram.Count - 1 do
  begin
    accumulated := accumulated + histogram[i].count;
    chartTendency.AddXY(i, accumulated);
    Series.AddXY(histogram[i].date, accumulated, '', color);
    //log(DateToStr(histogram[i].date) + ' ' + accumulated.ToString());
  end;
  totalPhotos.AddSeries(Series);
  chartTendency.Calculate;

  SeriesTendency := flickrChart.GetNewLineSeries(totalPhotos);

  //Adding only first and last item
  viewsTendency := chartTendency.tendencyResult(0);
  SeriesTendency.AddXY(histogram[0].Date, viewsTendency, '', FYellow);
  viewsTendency := chartTendency.tendencyResult(histogram.Count - 1);
  SeriesTendency.AddXY(histogram[histogram.Count - 1].Date, viewsTendency, '', FYellow);
//  for i := 0 to globalsRepository.globals.Count - 1 do
//  begin
//    viewsTendency := chartTendency.tendencyResult(i);
//    SeriesTendency.AddXY(globalsRepository.globals[i].Date, viewsTendency, '', color);
//  end;
  totalPhotos.AddSeries(SeriesTendency);
  histogram.Free;
  photoHistogram.Free;
end;

procedure TfrmFlickrMain.UncheckAll1Click(Sender: TObject);
var
  i: Integer;
begin
  if chkAddItem.Checked then
    listPhotos.OnItemChecked := nil;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    listPhotos.Items[i].Checked := false;
  end;

  if chkAddItem.Checked then
    listPhotos.OnItemChecked := listPhotosItemChecked;

  Label31.Caption := 'Number of items: ' + InttoStr(listphotos.Items.Count) + ' (0) selected';
  chartItemViews.SeriesList.Clear;
  chartItemLikes.SeriesList.Clear;
  chartItemComments.SeriesList.Clear;
  chartItemViewsH.SeriesList.Clear;
  chartItemLikesH.SeriesList.Clear;
  chartItemCommentsH.SeriesList.Clear;
  totalGroups.SeriesList.Clear;
end;

procedure TfrmFlickrMain.UncheckAll2Click(Sender: TObject);
var
  i: Integer;
begin
  listPhotosUser.OnItemChecked := nil;
  for i := 0 to listPhotosUser.Items.Count - 1 do
  begin
    listPhotosUser.Items[i].Checked := false;
  end;
  listPhotosUser.OnItemChecked := listPhotosItemChecked;
  Label34.Caption := 'Number of items: ' + InttoStr(listPhotosUser.Items.Count) + ' (0) selected';
end;

procedure TfrmFlickrMain.UncheckAll3Click(Sender: TObject);
var
  i: Integer;
begin
  listgroups.OnItemChecked := nil;
  for i := 0 to listGroups.Items.Count - 1 do
  begin
    listGroups.Items[i].Checked := false;
  end;
  listgroups.OnItemChecked := listGroupsItemChecked;
  Label11.Caption := 'Number of items: ' + InttoStr(listgroups.Items.Count) + '(0) selected';
end;

procedure TfrmFlickrMain.UpdateDailyViewsChart();
var
  Series: TAreaSeries;
  i: Integer;
  theDate: TDateTime;
  views, viewsTotal, average: Integer;
  averageSeries, tendencySeries: TLineSeries;
  viewsTendency : ITendency;
begin
  if dailyViews.SeriesList.Count > 0 then
    dailyViews.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(dailyViews, false, FHardBlue);
  viewsTendency := TTendency.Create;

  if globalsRepository.globals.Count = 1 then
  begin
    theDate := globalsRepository.globals[0].Date;
    views := globalsRepository.globals[0].views;
    viewsTendency.AddXY(0, views);
    Series.AddXY(theDate, views, '', FSoftBlue);
  end
  else
  begin
    for i := 1 to globalsRepository.globals.Count - 1 do
    begin
      theDate := globalsRepository.globals[i].Date;
      views := globalsRepository.globals[i].views - globalsRepository.globals[i - 1].views;
      viewsTendency.AddXY(i, views);
      Series.AddXY(theDate, views, '', FSoftBlue);
    end;
  end;

  dailyViews.AddSeries(Series);

  viewsTendency.Calculate;

  // Add average views
  averageSeries := flickrChart.GetNewLineSeries(dailyViews);

  viewsTotal := 0;
  for i := 1 to globalsRepository.globals.Count - 1 do
    viewsTotal := viewsTotal + (globalsRepository.globals[i].views - globalsRepository.globals[i - 1].views);

  average := round(viewsTotal / globalsRepository.globals.Count);

  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    theDate := globalsRepository.globals[i].Date;
    averageSeries.AddXY(theDate, average, '', FRed);
  end;

  dailyViews.AddSeries(averageSeries);

  //Add tendency line
  if globalsRepository.globals.Count > 1 then
  begin
    tendencySeries := flickrChart.GetNewLineSeries(dailyViews);

    //Adding only first and last item
    theDate := globalsRepository.globals[0].Date;
    views := viewsTendency.tendencyResult(0);
    tendencySeries.AddXY(theDate, views, '', FYellow);
    theDate := globalsRepository.globals[globalsRepository.globals.Count - 1].Date;
    views := viewsTendency.tendencyResult(globalsRepository.globals.Count - 1);
    tendencySeries.AddXY(theDate, views, '', FYellow);

    dailyViews.AddSeries(tendencySeries);

    views := viewsTendency.tendencyResult(globalsRepository.globals.Count);
    PredictionLabel.Caption :=  Format('%n',[views.ToDouble]).Replace('.00','');
  end;
end;

procedure TfrmFlickrMain.UpdateDailyAlbumViewsChart;
var
  Series: TAreaSeries;
  i: Integer;
  theDate: TDateTime;
  views: Integer;
begin
  if chartAlbumViews.SeriesList.Count > 0 then
    chartAlbumViews.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(chartAlbumViews, false, FHardBlue);

  if globalsRepository.globals.Count = 1 then
  begin
    theDate := globalsRepository.globals[0].Date;
    views := globalsRepository.globals[0].albumViews;
    Series.AddXY(theDate, views, '', FSoftBlue);
  end
  else
  begin
    for i := 0 to globalsRepository.globals.Count - 1 do
    begin
      theDate := globalsRepository.globals[i].Date;
      views := globalsRepository.globals[i].albumViews;
      if views > 0 then
        Series.AddXY(theDate, views, '', FSoftBlue);
    end;
  end;

  chartAlbumViews.AddSeries(Series);
end;

procedure TfrmFlickrMain.UpdateDailyCommentsChart;
var
  Series: TAreaSeries;
  i: Integer;
  theDate: TDateTime;
  views, viewsTotal, average: Integer;
  averageLikes, tendencySeries: TLineSeries;
  viewsTendency : ITendency;
begin
  if dailyComments.SeriesList.Count > 0 then
    dailyComments.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(dailyComments, false, FHardGreen);
  viewsTendency := TTendency.Create;

  if globalsRepository.globals.Count = 1 then
  begin
      theDate := globalsRepository.globals[0].Date;
      views := globalsRepository.globals[0].Comments;
      viewsTendency.AddXY(0, views);
      Series.AddXY(theDate, views, '', FSoftGreen);
  end
  else
  begin
    for i := 1 to globalsRepository.globals.Count - 1 do
    begin
      theDate := globalsRepository.globals[i].Date;
      views := globalsRepository.globals[i].Comments - globalsRepository.globals[i - 1].Comments;
      viewsTendency.AddXY(i, views);
      Series.AddXY(theDate, views, '', FSoftGreen);
    end;
  end;

  dailyComments.AddSeries(Series);
  viewsTendency.Calculate;
  // Add average views
  averageLikes := flickrChart.GetNewLineSeries(dailyComments);

  viewsTotal := 0;
  for i := 1 to globalsRepository.globals.Count - 1 do
    viewsTotal := viewsTotal + (globalsRepository.globals[i].Comments - globalsRepository.globals[i - 1].Comments);

  average := round(viewsTotal / globalsRepository.globals.Count);

  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    theDate := globalsRepository.globals[i].Date;
    averageLikes.AddXY(theDate, average, '', FRed);
  end;

  dailyComments.AddSeries(averageLikes);

  //Add tendency line
  tendencySeries := flickrChart.GetNewLineSeries(dailyComments);

  if globalsRepository.globals.Count > 1 then
  begin
    //Adding only first and last item
    theDate := globalsRepository.globals[0].Date;
    views := viewsTendency.tendencyResult(0);
    tendencySeries.AddXY(theDate, views, '', FYellow);
    theDate := globalsRepository.globals[globalsRepository.globals.Count - 1].Date;
    views := viewsTendency.tendencyResult(globalsRepository.globals.Count - 1);
    tendencySeries.AddXY(theDate, views, '', FYellow);

    dailyComments.AddSeries(tendencySeries);
  end;
end;

procedure TfrmFlickrMain.UpdateDailyLikesChart();
var
  Series: TAreaSeries;
  i: Integer;
  theDate: TDateTime;
  views, viewsTotal, average: Integer;
  averageLikes, tendencySeries: TLineSeries;
  viewsTendency : ITendency;
begin
  if dailyLikes.SeriesList.Count > 0 then
    dailyLikes.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(dailyLikes, false, FHardPink);
  viewsTendency := TTendency.Create;

  if globalsRepository.globals.Count = 1 then
  begin
    theDate := globalsRepository.globals[0].Date;
    views := globalsRepository.globals[0].likes;
    viewsTendency.AddXY(0, views);
    Series.AddXY(theDate, views, '', FSoftPink);
  end
  else
  begin
  for i := 1 to globalsRepository.globals.Count - 1 do
  begin
    theDate := globalsRepository.globals[i].Date;
    views := globalsRepository.globals[i].likes - globalsRepository.globals[i - 1].likes;
    viewsTendency.AddXY(i, views);
    Series.AddXY(theDate, views, '', FSoftPink);
  end;
  end;

  dailyLikes.AddSeries(Series);
  viewsTendency.Calculate;
  // Add average views
  averageLikes := flickrChart.GetNewLineSeries(dailyLikes);
  color := clRed;

  viewsTotal := 0;
  for i := 1 to globalsRepository.globals.Count - 1 do
    viewsTotal := viewsTotal + (globalsRepository.globals[i].likes - globalsRepository.globals[i - 1].likes);

  average := round(viewsTotal / globalsRepository.globals.Count);

  for i := 0 to globalsRepository.globals.Count - 1 do
  begin
    theDate := globalsRepository.globals[i].Date;
    averageLikes.AddXY(theDate, average, '', FRed);
  end;

  dailyLikes.AddSeries(averageLikes);

  //Add tendency line
  if globalsRepository.globals.Count > 1 then
  begin
    tendencySeries := flickrChart.GetNewLineSeries(dailyLikes);

    //Adding only first and last item
    theDate := globalsRepository.globals[0].Date;
    views := viewsTendency.tendencyResult(0);
    tendencySeries.AddXY(theDate, views, '', FYellow);
    theDate := globalsRepository.globals[globalsRepository.globals.Count - 1].Date;
    views := viewsTendency.tendencyResult(globalsRepository.globals.Count - 1);
    tendencySeries.AddXY(theDate, views, '', FYellow);

    dailyLikes.AddSeries(tendencySeries);
  end;
end;

procedure TfrmFlickrMain.UpdateDailyStreamViewsChart;
var
  Series: TAreaSeries;
  i: Integer;
  theDate: TDateTime;
  views: Integer;
begin
  if chartStreamViews.SeriesList.Count > 0 then
    chartStreamViews.RemoveAllSeries;

  Series := flickrChart.GetNewAreaSeries(chartStreamViews, false, FHardBlue);

  if globalsRepository.globals.Count = 1 then
  begin
    theDate := globalsRepository.globals[0].Date;
    views := globalsRepository.globals[0].streamViews;
    Series.AddXY(theDate, views, '', FSoftBlue);
  end
  else
  begin
    for i := 0 to globalsRepository.globals.Count - 1 do
    begin
      theDate := globalsRepository.globals[i].Date;
      views := globalsRepository.globals[i].streamViews;
      if views > 0 then
        Series.AddXY(theDate, views, '', FSoftBlue);
    end;
  end;

  chartStreamViews.AddSeries(Series);
end;

procedure TfrmFlickrMain.UpdateAnalytics;
begin
  UpdateDailyViewsChart();
  UpdateDailyStreamViewsChart();
  UpdateDailyAlbumViewsChart();
  UpdateDailyLikesChart();
  UpdateDailyCommentsChart();
  UpdateMostViewedChart();
  UpdateMostLikedChart();
end;

procedure TfrmFlickrMain.UpdateMostLikedChart();
var
  Series: TBarSeries;
  color: TColor;
  i: Integer;
  PhotosSorted: TList<IPhoto>;
  topStats : TTopStats;
  maxValue : string;
begin
  if mostlikeschart.SeriesList.Count > 0 then
    mostlikeschart.RemoveAllSeries;

  Series := flickrChart.GetNewBarSeries(mostlikeschart);
  color := RGB(Random(255), Random(255), Random(255));

  topStats := TTopStats.Create(repository);
  PhotosSorted := nil;
  try
    PhotosSorted := topStats.GetTopXNumberofMostLiked();
    maxValue := edtMax.Text;
    for i := 0 to maxValue.ToInteger -1 do
    begin
      if i < PhotosSorted.count then
      begin
        Series.AddBar(PhotosSorted[i].getHighestLikes(), PhotosSorted[i].Id, color);
      end;
    end;
  finally
    topStats.Free;
    PhotosSorted.Free;
  end;
end;

procedure TfrmFlickrMain.UpdateMostViewedChart();
var
  Series: TBarSeries;
  color: TColor;
  i: Integer;
  PhotosSorted: TList<IPhoto>;
  topStats : TTopStats;
  maxValues : string;
begin
  if mostviewschart.SeriesList.Count > 0 then
    mostviewschart.RemoveAllSeries;

  Series := flickrChart.GetNewBarSeries(mostviewschart);
  color := RGB(Random(255), Random(255), Random(255));

  topStats := TTopStats.Create(repository);
  PhotosSorted := nil;
  try
    PhotosSorted := topStats.GetTopXNumberofMostViewed();
    maxValues := edtMax.text;
    for i := 0 to maxValues.ToInteger() - 1 do
    begin
      if i < PhotosSorted.count then
      begin
        Series.AddBar(PhotosSorted[i].getHighestViews(), PhotosSorted[i].Id, color);
      end;
    end;
  finally
    topStats.Free;
    PhotosSorted.Free;
  end;
end;

procedure TfrmFlickrMain.UpdateSingleStats(id: string);
var
  SeriesViews, SeriesLikes, SeriesComments: TBarSeries;
  color: TColor;
  i: Integer;
  theDate: TDateTime;
  views: Integer;
  photo: IPhoto;
begin
  SeriesViews := flickrChart.GetNewBarSeries(chartItemViewsH);
  SeriesLikes := flickrChart.GetNewBarSeries(chartItemLikesH);
  SeriesComments := flickrChart.GetNewBarSeries(chartItemCommentsH);
  SeriesViews.Title := id;
  SeriesLikes.Title := id;
  SeriesComments.Title := id;
  color := RGB(Random(255), Random(255), Random(255));

  photo := repository.GetPhoto(id);
  photo.LoadStats;
  for i := 1 to photo.stats.Count - 1 do
  begin
    theDate := photo.stats[i].Date;
    views := photo.stats[i].views - photo.stats[i - 1].views;
    SeriesViews.AddXY(theDate, views, '', color);

    views := photo.stats[i].likes - photo.stats[i - 1].likes;
    SeriesLikes.AddXY(theDate, views, '', color);

    views := photo.stats[i].Comments - photo.stats[i - 1].Comments;
    SeriesComments.AddXY(theDate, views, '', color);
  end;

  chartItemViewsH.AddSeries(SeriesViews);
  chartItemLikesH.AddSeries(SeriesLikes);
  chartItemCommentsH.AddSeries(SeriesComments);
  photo.stats.Clear;
end;

procedure TfrmFlickrMain.UpdateChart(totalViews, totalLikes, totalComments, totalPhotos, totalSpreadGroups: Integer);
var
  values : double;
begin
  Label16.Visible := true;
  imagephoto.Visible := true;

  values := totalPhotos.ToDouble;
  PhotosLabel.Caption :=  Format('%n',[values]).Replace('.00','');

  values := totalSpreadGroups.ToDouble;
  TotalGroupsLabel.Caption :=  Format('%n',[values]).Replace('.00','') + ' group spread';

  values := totalViews / totalPhotos;
  LabelViewsPhoto.Caption :=  Format('%n',[values]).Replace('.00','') + ' views/photo';

  values := totalLikes / totalPhotos;
  LabelLikesPhoto.Caption :=  Format('%n',[values]).Replace('.00','') + ' likes/photo';

  values := totalComments / totalPhotos;
  LabelCommentsPhoto.Caption :=  Format('%n',[values]).Replace('.00','') + ' comments/photo';

  values := totalSpreadGroups / totalPhotos;
  TotalGroupsRateLabel.Caption :=  Format('%n',[values]).Replace('.00','') + ' groups/photo';
end;

procedure TfrmFlickrMain.btnSaveClick(Sender: TObject);
var
  st : TSTopwatch;
  option : Integer;
  dateBackup : TDate;
  noBackup : boolean;
begin
  option := MessageDlg('Do you want to save the changes?',mtInformation, mbOKCancel, 0);
  if option = mrOK then
  begin
    if repository.photos.Count = 0 then
    begin
      showMessage('There is nothing to save!');
      exit;
    end;

    noBackup := false;
    try
      if edtBackup.Text = '' then
        noBackup := true;
      dateBackup := StrToDate(edtBackup.Text);
    except
      dateBackup := Date;
    end;

    if (DaysBetween(Date, dateBackup) > 5) or (noBackup) then
    begin
      option := MessageDlg('It''s been more than 5 days since your last backup, do you want to create it now?',mtInformation, mbOKCancel, 0);
      if option = mrOK then
      begin
        backup1Click(sender);
        btnSaveOptionsClick(sender);
      end;
    end;

    st := TStopWatch.Create;
    st.Start;
    repository.version := TUtils.GetVersion;
    repository.DateSaved := Now;
    repository.save(apikey.text, secret.text, edtUserId.text, options.Workspace + '\flickrRepository.xml');
    st.Stop;
    log('Saving repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

    st := TStopWatch.Create;
    st.Start;
    globalsRepository.save(options.Workspace + '\flickrRepositoryGlobal.xml');
    st.Stop;
    log('Saving repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

    st := TStopWatch.Create;
    st.Start;
    organic.save(options.Workspace + '\flickrOrganic.xml');
    st.Stop;
    log('Saving repository flickrOrganic: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

    btnSave.Enabled := false;
    btnBackup.Enabled := false;
    btnLoad.Enabled := true;
    SaveTimeLabel.Caption := DateTimeToStr(now);
  end;
  showMessage('Repository has been saved');
end;

procedure TfrmFlickrMain.btnSaveMouseEnter(Sender: TObject);
begin
  ShowHint('Save flickr Repository to XML file', Sender);
end;

procedure TfrmFlickrMain.Button10Click(Sender: TObject);
begin
  if (apikey.text = '') or (userToken = '') then
  begin
    showmessage('You are not authorized!');
    exit;
  end;
  TAlbumCategoriser.AutoAdd(repository, options, optionsAgent,
    procedure (value : string)
    begin
      AlbumLog(value);
    end);

  showMessage('Pictures have been added to the albums');
end;

procedure TfrmFlickrMain.Button10MouseEnter(Sender: TObject);
begin
  ShowHint('Automatically add pictures in your albums based on views/likes', Sender);
end;

procedure TfrmFlickrMain.btnSaveOptionsClick(Sender: TObject);
var
  comparer, comparerGroups : integer;
begin
  if listValuesViewsAlbums.Lines.Count <> listValuesViewsAlbumsID.Lines.count then
  begin
    showMessage('Album views list must contain the same amount of items');
    exit;
  end;
  if listValuesLikesAlbums.Lines.Count <> listValuesLikesAlbumsID.Lines.count then
  begin
    showMessage('Album likes list must contain the same amount of items');
    exit;
  end;
  if edtWorkspace.Text = '' then
  begin
    showMessage('Workspace needs to be defined');
    exit;
  end;
  if not DirectoryExists(edtWorkspace.Text) then
  begin
    showMessage('Workspace needs to be defined with a valid directory');
    exit;
  end;

  options.MaxItemsListGlobals := edtMax.Text;
  options.urlName := edtUrlName.Text;

  options.ShowMarksInGraphs := showMarks.Checked;
  options.ConsiderPendingQueueItems := chkPending.Checked;
  options.UpdateCountsRealTime := chkRealTime.Checked;
  options.KeepRejectedListAlive := chkRejected.Checked;
  options.DisplaySuccessfulResponses := chkResponses.Checked;
  options.UpdateCollections := chkUpdateCollections.Checked;
  options.DisableTrendDisplay := chkAddItem.Checked;
  options.sortingEnabled := chksorting.Checked;
  options.ShowHints := chkShowButtonHint.Checked;
  options.BackupDate := edtBackup.Text;

  options.workspace := edtWorkspace.Text;

  options.MaxNumberOfLinesLog := edtMaxLog.Text;
  options.eMailAddress := edtEmail.Text;

  comparer := 0;
  if radioButton1.checked then
    comparer := 0;
  if radioButton2.checked then
    comparer := 2;
  if radioButton3.checked then
    comparer := 1;
  if radioButton4.checked then
    comparer := 3;
  if radioButton5.checked then
    comparer := 4;
  if radioButton6.checked then
    comparer := 5;
  if radioButton7.checked then
    comparer := 6;
  if radioButton10.Checked then
    comparer := 9;

  options.SortedBy := comparer;

  comparerGroups := 0;
  if RadioButton8.Checked then
    comparerGroups := 7;
  if RadioButton9.Checked then
    comparerGroups := 8;
  if RadioButton11.Checked then
    comparerGroups := 9;

  options.SortedByGropus := comparerGroups;
  options.AlbumViews := TStringList(listValuesViewsAlbums.Lines);
  options.AlbumLikes := TStringList(listValuesLikesAlbums.Lines);
  options.AlbumViewsID := TStringList(listValuesViewsAlbumsID.Lines);
  options.AlbumLikesID := TStringList(listValuesLikesAlbumsID.Lines);
  options.Save;

  optionsAgent.flickrApiKey := apikey.Text;
  optionsAgent.secret := secret.Text;
  optionsAgent.userToken := userToken;
  optionsAgent.userTokenSecret := userTokenSecret;
  optionsAgent.flickrUserId := edtUserId.Text;
  optionsAgent.AppId := edtAppId.Text;
  optionsAgent.save;
  showmessage('Options Saved sucessfully');
  FDirtyOptions := false;
end;

procedure TfrmFlickrMain.btnSaveOptionsMouseEnter(Sender: TObject);
begin
  ShowHint('Save Options.', Sender);
end;

procedure TfrmFlickrMain.LoadOptions();
var
  i : integer;
  comparer, comparerGroups : integer;
begin
  options := TOptions.New().Load();

  edtMax.Text := options.MaxItemsListGlobals;
  edtUrlName.Text := options.urlName;
  edtWorkspace.Text := options.workspace;
  showMarks.Checked := options.ShowMarksInGraphs;
  chkPending.Checked := options.ConsiderPendingQueueItems;
  chkRealTime.Checked := options.UpdateCountsRealTime;
  chkRejected.Checked := options.KeepRejectedListAlive;
  chkResponses.Checked := options.DisplaySuccessfulResponses;
  chkUpdateCollections.Checked := options.UpdateCollections;
  chkAddItem.Checked := options.DisableTrendDisplay;
  chksorting.Checked := options.sortingEnabled;
  chkShowButtonHint.Checked := options.showHints;
  edtBackup.Text := options.BackupDate;

  edtMaxLog.Text := options.MaxNumberOfLinesLog;
  edtEmail.Text := options.eMailAddress;

  comparer := options.SortedBy;

  case comparer of
    0: radioButton1.checked := true;
    2: radioButton2.checked := true;
    1: radioButton3.checked := true;
    3: radioButton4.checked := true;
    4: radioButton5.checked := true;
    5: radioButton6.checked := true;
    6: radioButton7.checked := true;
    9: radioButton10.Checked := true;
  end;

  comparerGroups := options.SortedByGropus;

  case comparerGroups of
    7: RadioButton8.checked := true;
    8: RadioButton9.checked := true;
    9: RadioButton11.checked := true;
  end;

  listValuesViewsAlbums.Lines.Clear;
  listValuesViewsAlbumsID.Lines.Clear;
  listValuesViewsAlbums.OnChange := nil;
  listValuesViewsAlbumsID.OnChange := nil;
  for i := 0 to options.AlbumViews.count - 1 do
  begin
    listValuesViewsAlbums.Lines.Add(options.AlbumViews[i]);
    listValuesViewsAlbumsID.Lines.Add(options.AlbumViewsID[i]);
  end;
  listValuesViewsAlbums.OnChange := listValuesViewsAlbumsChange;
  listValuesViewsAlbumsID.OnChange := listValuesViewsAlbumsIDChange;

  listValuesLikesAlbums.Lines.Clear;
  listValuesLikesAlbumsID.Lines.Clear;
  listValuesLikesAlbums.OnChange := nil;
  listValuesLikesAlbumsID.OnChange := nil;
  for i := 0 to options.AlbumLikes.count - 1 do
  begin
    listValuesLikesAlbums.Lines.Add(options.AlbumLikes[i]);
    listValuesLikesAlbumsID.Lines.Add(options.AlbumLikesID[i]);
  end;
  listValuesLikesAlbums.OnChange := listValuesLikesAlbumsChange;
  listValuesLikesAlbumsID.OnChange := listValuesLikesAlbumsIDChange;

  optionsAgent := ToptionsAgent.New().load();
  apikey.Text := optionsAgent.flickrApiKey;
  secret.Text := optionsAgent.secret;
  edtUserId.Text := optionsAgent.flickrUserId;
  edtAppId.Text := optionsAgent.AppId;
end;

procedure TfrmFlickrMain.btnLoadOptionsClick(Sender: TObject);
begin
  LoadOptions();
  showmessage('Options Loaded sucessfully');
end;

procedure TfrmFlickrMain.btnLoadOptionsMouseEnter(Sender: TObject);
begin
  ShowHint('Load Options.', Sender);
end;

procedure TfrmFlickrMain.Button1Click(Sender: TObject);
var
  urlGroups: string;
  response: string;
begin
  urlGroups := TFlickrRest.New(optionsAgent).getTestLogin();
  response := IdHTTP1.Get(urlGroups);
  showmessage(response);
end;

procedure TfrmFlickrMain.Button1MouseEnter(Sender: TObject);
begin
  ShowHint('Test that your API key and secret are correct.', Sender);
end;

procedure TfrmFlickrMain.Button2Click(Sender: TObject);
begin
  if SaveToExcelGroups(listGroups, 'Flickr Analytics', options.Workspace + '\FlickrAnalyticsGroups.xls') then
    showmessage('Data saved successfully!');
end;

procedure TfrmFlickrMain.Button2MouseEnter(Sender: TObject);
begin
  ShowHint('Export list into excel.', Sender);
end;

procedure TfrmFlickrMain.Button3Click(Sender: TObject);
begin
  //Show help page in the blog
  ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('http://thundaxsoftware.blogspot.co.uk/p/flickr-photo-analytics-v45.html'), nil, SW_SHOW);
end;

procedure TfrmFlickrMain.Button3MouseEnter(Sender: TObject);
begin
  ShowHint('Go to online help', Sender);
end;

procedure TfrmFlickrMain.Button4Click(Sender: TObject);
begin
  FProcessingStop := true;
  showMessage('Process has been stopped');
end;

procedure TfrmFlickrMain.Button4MouseEnter(Sender: TObject);
begin
  ShowHint('Stop the update.', Sender);
end;

procedure TfrmFlickrMain.Button5Click(Sender: TObject);
begin
  FGroupStop := true;
  showMessage('Process has been stopped');
end;

procedure TfrmFlickrMain.Button5MouseEnter(Sender: TObject);
begin
  ShowHint('Stop processing of Adding, Removing or Banning.', Sender);
end;

procedure TfrmFlickrMain.btnAddFilterClick(Sender: TObject);
var
  i: Integer;
  add : boolean;
  value : string;
  Item : TListItem;
begin
  listPhotos.Items.Clear;
  filterEnabled := true;
  btnSave.Enabled := false;
  btnBackup.Enabled := false;
  btnLoad.Enabled := false;
  startMark := -1;
  endMark := -1;
  value := edtFilter.text;
  for i := 0 to repository.photos.count-1 do
  begin
    add := false;
    case Combobox2.ItemIndex of
          0: //ID
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].Id = value;
              1: add := repository.photos[i].Id.ToExtended < value.ToExtended;
              2: add := repository.photos[i].Id.ToExtended > value.ToExtended;
              3: add := repository.photos[i].Id.ToExtended <= value.ToExtended;
              4: add := repository.photos[i].Id.ToExtended >= value.ToExtended;
              5: add := repository.photos[i].Id <> value;
              6: add := repository.photos[i].Id.ToLower.Contains(value.ToLower);
            end;
          end;
          1: //Title
          begin
            case combobox3.ItemIndex of
              0,1,2,3,4: add := repository.photos[i].title = value;
              5: add := repository.photos[i].title <> edtfilter.Text;
              6: add := repository.photos[i].title.ToLower.Contains(value.ToLower);
            end;
          end;
          2: //Views
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views.tostring = value;
              1: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views < value.Tointeger;
              2: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views > value.Tointeger;
              3: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views <= value.Tointeger;
              4: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views >= value.Tointeger;
              5: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views <> value.ToInteger;
              6: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].views.tostring.ToLower.Contains(value.ToLower);
            end;
          end;
          3: //Likes
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes.tostring = value;
              1: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes < value.Tointeger;
              2: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes > value.Tointeger;
              3: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes <= value.Tointeger;
              4: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes >= value.Tointeger;
              5: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes <> value.ToInteger;
              6: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].likes.tostring.ToLower.Contains(value.ToLower);
            end;
          end;
          4: //Comments
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments.tostring = value;
              1: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments < value.Tointeger;
              2: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments > value.Tointeger;
              3: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments <= value.Tointeger;
              4: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments >= value.Tointeger;
              5: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments <> value.ToInteger;
              6: add := repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments.tostring.ToLower.Contains(value.ToLower);
            end;
          end;
          5: //Last Update
          begin
            case combobox3.ItemIndex of
              0,1,2,3,4,5,6: add := FormatDateTime('dd/mm/yyyy', repository.photos[i].LastUpdate).ToLower.Contains(value.ToLower);
            end;
          end;
          6: //Taken
          begin
            case combobox3.ItemIndex of
              0,1,2,3,4,5,6: add := repository.photos[i].Taken.ToLower.Contains(value.ToLower);
            end;
          end;
          7: //Albums
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].Albums.count.ToString = value;
              1: add := repository.photos[i].Albums.count < value.ToInteger;
              2: add := repository.photos[i].Albums.count > value.ToInteger;
              3: add := repository.photos[i].Albums.count <= value.ToInteger;
              4: add := repository.photos[i].Albums.count >= value.ToInteger;
              5: add := repository.photos[i].Albums.count <> value.ToInteger;
              6: add := repository.photos[i].Albums.count.ToString.ToLower.Contains(value.ToLower);
            end;
          end;
          8: //Groups
          begin
            case combobox3.ItemIndex of
              0: add := repository.photos[i].Groups.count.ToString = value;
              1: add := repository.photos[i].Groups.count < value.ToInteger;
              2: add := repository.photos[i].Groups.count > value.ToInteger;
              3: add := repository.photos[i].Groups.count <= value.ToInteger;
              4: add := repository.photos[i].Groups.count >= value.ToInteger;
              5: add := repository.photos[i].Groups.count <> value.ToInteger;
              6: add := repository.photos[i].Groups.count.ToString.ToLower.Contains(value.ToLower);
            end;
          end;
          9: //Tags
          begin
            case combobox3.ItemIndex of
              0,1,2,3,4,5,6: add := repository.photos[i].tags.ToLower.Contains(value.ToLower);
            end;
          end;
          10: //Affection
          begin

          end;
      end;
      if add then
      begin
        Item := frmFlickrMain.listPhotos.Items.Add;
        Item.Caption := repository.photos[i].Id;
        Item.SubItems.Add(repository.photos[i].Title);
        Item.SubItems.Add(repository.photos[i].stats[repository.photos[i].stats.Count-1].views.toString);
        Item.SubItems.Add(repository.photos[i].stats[repository.photos[i].stats.Count-1].likes.toString);
        Item.SubItems.Add(repository.photos[i].stats[repository.photos[i].stats.Count-1].Comments.toString);
        Item.SubItems.Add((DateToStr(repository.photos[i].stats[repository.photos[i].stats.Count-1].date)));
        Item.SubItems.Add(repository.photos[i].Taken);
        Item.SubItems.Add(repository.photos[i].Albums.Count.ToString());
        Item.SubItems.Add(repository.photos[i].Groups.Count.ToString());
        Item.SubItems.Add(repository.photos[i].Tags);
        Item.SubItems.Add(FormatFloat('0.##%', (repository.photos[i].stats[repository.photos[i].stats.Count-1].likes / repository.photos[i].stats[repository.photos[i].stats.Count-1].views) * 100.0));
        Item.SubItems.Add(repository.photos[i].banned.ToString());
        Item.SubItems.Add(repository.photos[i].getTrend.ToString());
        Item.SubItems.Add(repository.photos[i].OmitGroups);
      end;
      Label31.Caption := 'Number of items: ' + InttoStr(listphotos.Items.Count) + ' (0) selected';
  end;
  btnAddFilter.Enabled := false;
  label36.Visible := true;
end;

procedure TfrmFlickrMain.btnResetFilterClick(Sender: TObject);
begin
  //Restore everything like it was.
  startMark := -1;
  endMark := -1;
  edtFilter.Text := '';
  listPhotos.OnItemChecked := nil;
  listPhotos.OnCustomDrawSubItem := nil;
  filterEnabled := false;
  btnAddFilter.Enabled := true;
  btnSave.Enabled := true;
  btnBackup.Enabled := true;
  btnLoad.Enabled := true;
  listPhotos.Visible := false;
  LoadForms(repository);
  listPhotos.OnItemChecked := listPhotosItemChecked;
  listPhotos.OnCustomDrawSubItem := listPhotosCustomDrawSubItem;
  listPhotos.Visible := true;
  chartItemViews.SeriesList.Clear;
  chartItemLikes.SeriesList.Clear;
  totalGroups.SeriesList.Clear;
  chartItemComments.SeriesList.Clear;
  chartItemViewsH.SeriesList.Clear;
  chartItemLikesH.SeriesList.Clear;
  chartItemCommentsH.SeriesList.Clear;
  label36.Visible := false;
end;

procedure TfrmFlickrMain.btnFilterCancelClick(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  pagecontrol3.TabIndex := 0;
  listGroups.Visible := false;
  listGroups.OnItemChecked := nil;
  listgroups.OnCustomDrawItem := nil;

  listGroups.Items.Clear;
  FilteredGroupList.sort;
  for i := 0 to FilteredGroupList.list.Count - 1 do
  begin
    Item := listGroups.Items.Add;
    Item.Caption := FilteredGroupList.list[i].id;
    Item.SubItems.Add(FilteredGroupList.list[i].title);
    Item.SubItems.Add(FilteredGroupList.list[i].Photos.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].Members.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].Description);
    Item.SubItems.Add(FilteredGroupList.list[i].IsModerated.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleCount.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleMode);
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleRemaining.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].photos_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].videos_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].images_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].screens_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].art_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].safe_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].moderate_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].restricted_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].has_geo.ToString());
  end;

  listGroups.OnItemChecked := listGroupsItemChecked;
  listgroups.OnCustomDrawItem := listGroupsCustomDrawItem;
  edtFilterGroup.Text := '';
  label37.Visible := false;
  listGroups.Visible := true;
  Label11.Caption := 'Number of items: ' + InttoStr(listgroups.Items.Count) + ' (0) selected';
end;

procedure TfrmFlickrMain.btnFilterOKClick(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
  description: string;
begin
  pagecontrol3.TabIndex := 0;
  listGroups.Visible := false;
  listGroups.Items.Clear;
  FilteredGroupList.sort;
  description := edtFilterGroup.text;
  for i := 0 to FilteredGroupList.list.Count - 1 do
  begin
    if FilteredGroupList.list[i].title.ToUpper.Contains(description.ToUpper) then
    begin
      Item := listGroups.Items.Add;
      Item.Caption := FilteredGroupList.list[i].id;
      Item.SubItems.Add(FilteredGroupList.list[i].title);
      Item.SubItems.Add(FilteredGroupList.list[i].Photos.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].Members.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].Description);
      Item.SubItems.Add(FilteredGroupList.list[i].IsModerated.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].ThrottleCount.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].ThrottleMode);
      Item.SubItems.Add(FilteredGroupList.list[i].ThrottleRemaining.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].photos_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].videos_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].images_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].screens_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].art_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].safe_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].moderate_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].restricted_ok.ToString());
      Item.SubItems.Add(FilteredGroupList.list[i].has_geo.ToString());
    end;
  end;
  listGroups.Visible := true;
  label37.Visible := true;
  Label11.Caption := 'Number of items: ' + InttoStr(listgroups.Items.Count) + ' (0) selected';
end;

procedure TfrmFlickrMain.btnAddPhotosClick(Sender: TObject);
var
  i: Integer;
  j: Integer;
  k: Integer;
  photoId: string;
  urlAdd, response: string;
  photos: TList<string>;
  groups: TList<IBase>;
  timedout: Boolean;
  total : integer;
  max : string;
  success : integer;
  photoToCheck : IPhoto;
  option : integer;
  base : IBase;
begin
  if (apikey.text = '') or (userToken = '') then
  begin
    showmessage('You are not authorized!');
    exit;
  end;
  pagecontrol3.TabIndex := 0;
  photos := TList<string>.Create;
  groups := TList<IBase>.Create;
  FGroupStop := false;
  try
    batchUpdate.Enabled := false;
    PageControl3.ActivePage := tabStatus;
    for i := 0 to listPhotos.Items.Count - 1 do
    begin
      if listPhotos.Items[i].Checked then
        photos.Add(listPhotos.Items[i].Caption);
    end;
    for i := 0 to listGroups.Items.Count - 1 do
    begin
      if listGroups.Items[i].Checked then
      begin
        base := TBase.Create();
        base.Id := listGroups.Items[i].Caption;
        base.Title := listGroups.Items[i].SubItems[0];
        base.Photos := listGroups.Items[i].SubItems[1].ToInteger;
        base.Members := listGroups.Items[i].SubItems[2].ToInteger;
        base.Description := listGroups.Items[i].SubItems[3];
        base.IsModerated := listGroups.Items[i].SubItems[4].ToBoolean();
        base.ThrottleCount := listGroups.Items[i].SubItems[5].ToInteger;
        base.ThrottleMode := listGroups.Items[i].SubItems[6];
        base.ThrottleRemaining := listGroups.Items[i].SubItems[7].ToInteger;
        base.photos_ok := listGroups.Items[i].SubItems[8].ToBoolean();
        base.videos_ok := listGroups.Items[i].SubItems[9].ToBoolean();
        base.images_ok := listGroups.Items[i].SubItems[10].ToBoolean();
        base.screens_ok := listGroups.Items[i].SubItems[11].ToBoolean();
        base.art_ok := listGroups.Items[i].SubItems[12].ToBoolean();
        base.safe_ok := listGroups.Items[i].SubItems[13].ToBoolean();
        base.moderate_ok := listGroups.Items[i].SubItems[14].ToBoolean();
        base.restricted_ok := listGroups.Items[i].SubItems[15].ToBoolean();
        base.has_geo := listGroups.Items[i].SubItems[16].ToBoolean();
        groups.Add(base);
      end;
    end;

    if not chkRejected.Checked then
      rejected := TRejected.Create;
    // add photos to the groups
    progressbar1.Visible := true;
    progressbar1.Max := (photos.Count * groups.Count);
    progressbar1.Min := 0;
    Taskbar1.ProgressState := TTaskBarProgressState.Normal;
    Taskbar1.ProgressMaxValue := progressbar1.Max;
    k := 0;
    mStatus.Lines.Add('******************************************************************');
    mStatus.Lines.Add('Adding ' + photos.Count.ToString + ' photos into ' + groups.Count.ToString + ' groups each.');
    total := photos.Count * groups.Count;
    mStatus.Lines.Add('Total number of transactions: ' + total.ToString());
    success := 0;
    option := MessageDlg('Do you want to add ' + photos.Count.ToString + ' photos into ' + groups.Count.ToString + ' groups each?', mtInformation, mbOKCancel, 0);
    if option = mrOK then
    begin
      for i := 0 to photos.Count - 1 do
      begin
        for j := 0 to groups.Count - 1 do
        begin
          photoId := photos[i];
          base := groups[j];
          timedout := false;
          if not rejected.Exists(base.Id) and not (repository.isPhotoInGroup(photoId, base.Id, photoToCheck)) then
          begin
            if not (photoToCheck.banned) and not (photoToCheck.OmitGroups.Contains(base.Id)) then
            begin
              if ((base.ThrottleRemaining > 0) and (base.ThrottleMode <> 'none')) or ((base.ThrottleMode = 'none')) then
              begin
                urlAdd := TFlickrRest.New(optionsAgent).getPoolsAdd(photoId, base.Id);
                while (not timedout) do
                begin
                  try
                    response := IdHTTP1.Get(urlAdd);
                    if response.Contains('Photo limit reached') or response.Contains('Maximum') then
                    begin
                      mStatus.Lines.Add('Adding group : ' + base.Id + ' to the rejected list');
                      rejected.Add(base.Id);
                    end;
                    if not chkPending.Checked then
                    begin
                      if response.Contains('Pending Queue') then
                      begin
                        mStatus.Lines.Add('Adding group : ' + base.Id + ' to the rejected list');
                        rejected.Add(base.Id);
                      end;
                    end;
                    timedout := true;
                  except
                    on e: exception do
                    begin
                      sleep(timeout);
                      timedout := false;
                    end;
                  end;
                end;
              end;
            end
            else
              mStatus.Lines.Add('Photo '+ photoId +' is banned from group : ' + base.Id);
          end;
          inc(k);
          progressbar1.position := k;
          Taskbar1.ProgressValue := k;
          response := TResponse.filter(response);
          if chkResponses.Checked then
          begin
            if response.Contains('ok') then
            begin
              mStatus.Lines.Add('PhotoId: ' + photoId + ' GroupId: ' + base.Id + ' response: ' + response);
              inc(success);
              if ((base.ThrottleRemaining > 0) and (base.ThrottleMode <> 'none')) then
              begin
                base.ThrottleRemaining := base.ThrottleRemaining - 1;
                UpdateListGroupsThrottleRemaining(base);
                mStatus.Lines.Add('group : ' + base.Id + ' throttle count ' + base.ThrottleRemaining.ToString());
              end;
            end;
          end
          else
          begin
            mStatus.Lines.Add('PhotoId: ' + photoId + ' GroupId: ' + base.Id + ' response: ' + response);
            if response.Contains('ok') then
            begin
              inc(success);
              if ((base.ThrottleRemaining > 0) and (base.ThrottleMode <> 'none')) then
              begin
                base.ThrottleRemaining := base.ThrottleRemaining - 1;
                UpdateListGroupsThrottleRemaining(base);
                mStatus.Lines.Add('group : ' + base.Id + ' throttle count ' + base.ThrottleRemaining.ToString());
              end;
            end;
          end;

          max := edtMaxLog.text;
          if mStatus.Lines.Count > max.ToInteger then
            mStatus.Lines.Clear;
          Application.ProcessMessages;
          if FGroupStop then
            break;
          sleep(10);
        end;
        if FGroupStop then
          break;
     end;
    end;
  finally
    batchUpdate.Enabled := true;
    mStatus.Lines.Add('Total number of groups added: ' + success.ToString() + ' out of ' + total.ToString());
    mStatus.Lines.Add('******************************************************************');
    progressbar1.Visible := false;
    photos.Free;
    groups.Free;
  end;
  FGroupStop := false;
  showMessage('Photos have been added to the groups');
end;

procedure TfrmFlickrMain.UpdateListGroupsThrottleRemaining(base : IBase);
var
  i : integer;
begin
  for i := 0 to listGroups.Items.Count - 1 do
  begin
    if listGroups.Items[i].Caption = base.id then
    begin
      listGroups.Items[i].SubItems[7] := base.ThrottleRemaining.ToString;
      break;
    end;
  end;
  for i := 0 to FilteredGroupList.list.Count - 1 do
  begin
    if FilteredGroupList.list[i].id = base.id then
    begin
      FilteredGroupList.list[i].ThrottleRemaining := base.ThrottleRemaining;
      break;
    end;
  end;
end;

procedure TfrmFlickrMain.btnAddPhotosMouseEnter(Sender: TObject);
begin
  ShowHint('Add selected pictures to selected groups.', Sender);
end;

procedure TfrmFlickrMain.btnBanGroupsClick(Sender: TObject);
var
  photos: TList<string>;
  groups: TList<string>;
  i : integer;
  j : integer;
  photoId: string;
  groupId: string;
  p : IPhoto;
  OmitGroups : string;
  total : integer;
  option : integer;
begin
  //Ban these groups in the photos selected.
  PageControl3.ActivePage := tabStatus;
  photos := TList<string>.Create;
  groups := TList<string>.Create;
  try
    batchUpdate.Enabled := false;
    for i := 0 to listPhotos.Items.Count - 1 do
    begin
      if listPhotos.Items[i].Checked then
        photos.Add(listPhotos.Items[i].Caption);
    end;
    for i := 0 to listGroups.Items.Count - 1 do
    begin
      if listGroups.Items[i].Checked then
        groups.Add(listGroups.Items[i].Caption);
    end;
    mStatus.Lines.Add('******************************************************************');
    mStatus.Lines.Add('Banning ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each.');
    total := photos.Count * groups.Count;
    mStatus.Lines.Add('Total number of transactions: ' + total.ToString());
    option := MessageDlg('Do you want to ban ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each?', mtInformation, mbOKCancel, 0);
    if option = mrOK then
    begin
      for i := 0 to photos.Count - 1 do
      begin
        photoId := photos[i];
        p := repository.GetPhoto(photoId);
        OmitGroups := p.OmitGroups;
        for j := 0 to groups.Count - 1 do
        begin
          groupId := groups[j];
          if not OmitGroups.Contains(groupId) then
          begin
            OmitGroups := OmitGroups + groupId + ',';
            mStatus.Lines.Add('Adding photo to banned list for  : ' + photoId + ': ' + OmitGroups);
          end
          else
            mStatus.Lines.Add('Photo already in the banned list : ' + groupId);
        end;
        p.OmitGroups := OmitGroups;
      end;
    end;
  finally
    batchUpdate.Enabled := true;
    mStatus.Lines.Add('******************************************************************');
    photos.Free;
    groups.Free;
  end;
  showMessage('Photos have been banned from the groups, please refresh the main grid!');
end;

procedure TfrmFlickrMain.btnBanGroupsMouseEnter(Sender: TObject);
begin
  ShowHint('Ban selected pictures from selected groups.', Sender);
end;

procedure TfrmFlickrMain.btnDeleteProfileClick(Sender: TObject);
var
  profileName: string;
  option : integer;
begin
  if ComboBox1.ItemIndex = -1 then
    exit;
  option := mrOK;
  if chkReplaceProfile.Checked then
    option := MessageDlg('Are you sure you want to delete the profile?', mtInformation, mbOKCancel, 0);
  if option = mrOK then
  begin
    profileName := ComboBox1.Items[ComboBox1.ItemIndex];
    profileName := profileName.Remove(profileName.IndexOf('('), (profileName.IndexOf(')') - profileName.IndexOf('('))+1);
    profileName := profileName.Remove(profileName.Length-1, 1);
    // now delete the profile
    flickrProfiles.remove(profileName);
    ComboBox1.DeleteSelected;
  end;
end;

procedure TfrmFlickrMain.btnDeleteProfileMouseEnter(Sender: TObject);
begin
  ShowHint('Delete selected profile.', Sender);
end;

procedure TfrmFlickrMain.btnLoadProfileClick(Sender: TObject);
var
  profileName: string;
  profile: IProfile;
  i: Integer;
  j: Integer;
  Item: TListItem;
  groupId : string;
begin
  try
    if ComboBox1.ItemIndex = -1 then
      exit;
    chkReplaceProfile.Checked := false;
    pagecontrol3.TabIndex := 0;
    profileName := ComboBox1.Items[ComboBox1.ItemIndex];
    profileName := profileName.Remove(profileName.IndexOf('('), (profileName.IndexOf(')') - profileName.IndexOf('('))+1);
    profileName := profileName.Remove(profileName.Length-1, 1);
    // Now look for this profileName in the Main Object.
    profile := flickrProfiles.getProfile(profileName);
    listGroups.OnItemChecked := nil;
    listgroups.OnCustomDrawItem := nil;
    FilteredGroupList.sort;
    if chkDisplayOnly.Checked then
    begin
      listGroups.Visible := false;
      listGroups.Items.Clear;
      edtProfile.text := profileName;
      for i := 0 to FilteredGroupList.list.Count - 1 do
      begin
        if profile.groupId.TryGetValue(FilteredGroupList.list[i].id, groupId) then
        begin
            Item := listGroups.Items.Add;
            Item.Caption := FilteredGroupList.list[i].id;
            Item.SubItems.Add(FilteredGroupList.list[i].title);
            Item.SubItems.Add(FilteredGroupList.list[i].Photos.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].Members.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].Description);
            Item.SubItems.Add(FilteredGroupList.list[i].IsModerated.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].ThrottleCount.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].ThrottleMode);
            Item.SubItems.Add(FilteredGroupList.list[i].ThrottleRemaining.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].photos_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].videos_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].images_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].screens_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].art_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].safe_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].moderate_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].restricted_ok.ToString());
            Item.SubItems.Add(FilteredGroupList.list[i].has_geo.ToString());
            Item.checked := true;
        end;
      end;
      listGroups.Visible := true;
    end
    else
    begin
      if profile <> nil then
      begin
        listGroups.Visible := false;
        edtProfile.text := profileName;
        for j := 0 to listGroups.Items.Count - 1 do
        begin
          if profile.groupId.TryGetValue(listGroups.Items[j].Caption, groupId) then
            listGroups.Items[j].Checked := true;
        end;
        listGroups.Visible := true;
      end;
    end;
  finally
    listGroups.OnItemChecked := listGroupsItemChecked;
    listgroups.OnCustomDrawItem := listGroupsCustomDrawItem;
    UpdateLabelGroups();
  end;
end;

procedure TfrmFlickrMain.btnLoadProfileMouseEnter(Sender: TObject);
begin
  ShowHint('Load a profile from the list.', Sender);
end;

procedure TfrmFlickrMain.btnRemovePhotoClick(Sender: TObject);
var
  i: Integer;
  j: Integer;
  k: Integer;
  photoId: string;
  groupId: string;
  urlAdd, response: string;
  photos: TList<string>;
  groups: TList<string>;
  timedout: Boolean;
  total : integer;
  max : string;
  success : integer;
  resultPhoto : IPhoto;
  option : integer;
begin
  if (apikey.text = '') or (userToken = '') then
  begin
    showmessage('You are not authorized!');
    exit;
  end;
  photos := TList<string>.Create;
  groups := TList<string>.Create;
  FGroupStop := false;
  try
    batchUpdate.Enabled := false;
    PageControl3.ActivePage := tabStatus;
    for i := 0 to listPhotos.Items.Count - 1 do
      if listPhotos.Items[i].Checked then
        photos.Add(listPhotos.Items[i].Caption);
    for i := 0 to listGroups.Items.Count - 1 do
      if listGroups.Items[i].Checked then
        groups.Add(listGroups.Items[i].Caption);

    // add photos to the groups
    progressbar1.Visible := true;
    progressbar1.Max := (photos.Count * groups.Count);
    progressbar1.Min := 0;
    Taskbar1.ProgressState := TTaskBarProgressState.Normal;
    Taskbar1.ProgressMaxValue := progressbar1.Max;
    k := 0;
    mStatus.Lines.Add('******************************************************************');
    mStatus.Lines.Add('Removing ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each.');
    total := photos.Count * groups.Count;
    mStatus.Lines.Add('Total number of transactions: ' + total.ToString());
    success := 0;
    option := MessageDlg('Do you want to remove ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each?', mtInformation, mbOKCancel, 0);
    if option = mrOK then
    begin
      for i := 0 to photos.Count - 1 do
      begin
        for j := 0 to groups.Count - 1 do
        begin
          photoId := photos[i];
          groupId := groups[j];
          timedout := false;
          if (repository.isPhotoInGroup(photoId, groupId, resultPhoto)) then
          begin
            urlAdd := TFlickrRest.New(optionsAgent).getPoolsRemove(photoId, groupId);
            while (not timedout) do
            begin
              try
                response := IdHTTP1.Get(urlAdd);
                timedout := true;
              except
                on e: exception do
                begin
                  sleep(timeout);
                  timedout := false;
                end;
              end;
            end;
          end;
          inc(k);
          progressbar1.position := k;
          Taskbar1.ProgressValue := k;
          response := TResponse.filter(response);
          if chkResponses.Checked then
          begin
            if response.Contains('ok') then
            begin
              mStatus.Lines.Add('PhotoId: ' + photoId + ' GroupId: ' + groupId + ' response: ' + response);
              inc(success);
            end;
          end
          else
          begin
            mStatus.Lines.Add('PhotoId: ' + photoId + ' GroupId: ' + groupId + ' response: ' + response);
            if response.Contains('ok') then
            begin
              inc(success);
            end;
          end;

          max := edtMaxLog.text;
          if mStatus.Lines.Count > max.ToInteger then
            mStatus.Lines.Clear;
          Application.ProcessMessages;
          if FGroupStop then
            break;
          sleep(10);
        end;
        if FGroupStop then
          break;
      end;
    end;
  finally
    batchUpdate.Enabled := true;
    mStatus.Lines.Add('Total number of groups removed: ' + success.ToString() + ' out of ' + total.ToString());
    mStatus.Lines.Add('******************************************************************');
    progressbar1.Visible := false;
    photos.Free;
    groups.Free;
  end;
  FGroupStop := false;
  showMessage('Photos have been removed from the groups');
end;

procedure TfrmFlickrMain.btnRemovePhotoMouseEnter(Sender: TObject);
begin
  ShowHint('Remove selected pictures from selected groups.', Sender);
end;

procedure TfrmFlickrMain.btnRemoveProfileClick(Sender: TObject);
var
  profile: IProfile;
  i: Integer;
  index : integer;
begin
  if edtProfile.text = '' then
  begin
    showmessage('profile name can''t be empty');
    exit;
  end;

  // Give me the profile
  index := ComboBox1.ItemIndex;
  profile := flickrProfiles.getProfile(edtProfile.text);

  if profile = nil then
  begin
    showmessage('profile needs to be existing!');
    exit;
  end
  else
  begin
      for i := 0 to listGroups.Items.Count - 1 do
      begin
        if listGroups.Items[i].Checked then
        begin
          profile.RemoveId(listGroups.Items[i].Caption);
        end;
      end;
  end;
  flickrProfiles.save(options.workspace + '\flickrProfiles.xml');
  LoadProfiles();
  if index <> -1 then
    combobox1.ItemIndex := index;
end;

procedure TfrmFlickrMain.btnRemoveProfileMouseEnter(Sender: TObject);
begin
  ShowHint('Remove selected groups from the profile.', Sender);
end;

procedure TfrmFlickrMain.btnSaveProfileClick(Sender: TObject);
var
  profile: IProfile;
  i: Integer;
  option : integer;
  index : integer;
begin
  if edtProfile.text = '' then
  begin
    showmessage('profile name can''t be empty');
    exit;
  end;

  option := mrOK;
  if chkReplaceProfile.Checked then
  begin
    option := MessageDlg('Replace profile is ticked, do you really want to override the entire profile?',mtInformation, mbOKCancel, 0);
  end;

  if option = mrOK then
  begin
    // Give me the profile
    index := ComboBox1.ItemIndex;
    profile := flickrProfiles.getProfile(edtProfile.text);

    if profile = nil then
    begin
      // New Profile
      profile := TProfile.Create;
      profile.Name := edtProfile.text;
      for i := 0 to listGroups.Items.Count - 1 do
      begin
        if listGroups.Items[i].Checked then
        begin
          profile.AddId(listGroups.Items[i].Caption);
        end;
      end;
      flickrProfiles.Add(profile);
    end
    else
    begin
      if chkReplaceProfile.Checked then
      begin
        flickrProfiles.list.Remove(profile);
        profile := nil;

        // New Profile
        profile := TProfile.Create;
        profile.Name := edtProfile.text;
        for i := 0 to listGroups.Items.Count - 1 do
        begin
          if listGroups.Items[i].Checked then
          begin
            profile.AddId(listGroups.Items[i].Caption);
          end;
        end;
        flickrProfiles.Add(profile);
      end
      else
      begin
        for i := 0 to listGroups.Items.Count - 1 do
        begin
          if listGroups.Items[i].Checked then
          begin
            profile.AddId(listGroups.Items[i].Caption);
          end;
        end;
      end;
    end;
    flickrProfiles.save(options.workspace + '\flickrProfiles.xml');
    LoadProfiles();
    if index <> -1 then
      combobox1.ItemIndex := index;
  end;
end;

procedure TfrmFlickrMain.btnSaveProfileMouseEnter(Sender: TObject);
begin
  ShowHint('Save or update a profile when ''override profile'' is not ticked', Sender);
end;

procedure TfrmFlickrMain.ShowFavesList1Click(Sender: TObject);
var
  id : string;
begin
  ListUsers := TfrmUserList.Create(self);
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    ListUsers.PopulateList(options.Workspace, id);
  end;
  ListUsers.Show;
end;

procedure TfrmFlickrMain.ShowGraph1Click(Sender: TObject);
var
  id : string;
begin
  if lvAlbums.ItemIndex <> -1 then
  begin
    id := lvAlbums.Items[lvAlbums.ItemIndex].Caption;
    OpenPhotosAlbumId(id, lvAlbums.Items[lvAlbums.ItemIndex].SubItems[0], lvAlbums.Items[lvAlbums.ItemIndex].SubItems[2]);
  end;
end;

procedure TfrmFlickrMain.ShowHint(description : string; Sender: TObject);
var
  aPoint: TPoint;
begin
  if chkShowButtonHint.Checked then
  begin
    BalloonHint1.Description := description;
    aPoint.X := (Sender as TButton).width-5;
    aPoint.Y := (Sender as TButton).height-5;
    BalloonHint1.ShowHint((Sender as TButton).ClientToScreen(aPoint));
  end;
end;

procedure TfrmFlickrMain.btnShowReportClick(Sender: TObject);
var
  description : TStrings;
  option : integer;
begin
  //Show the html generated report.
  if RepositoryLoaded then
  begin
    description := THtmlComposer.getMessage(options, repository, globalsRepository, organic, true);
    try
      option := MessageDlg('Do you want to the report via email?',mtInformation, mbOKCancel, 0);
      if option = mrOK then
        TFlickrEmail.SendHTML(options.eMailAddress, description);
      description.SaveToFile(options.Workspace + '\flickrHtmlreport.htm');
      WebBrowser2.Navigate('file:///' + options.Workspace + '\flickrHtmlreport.htm');
    finally
      description.Free;
    end;
  end;
  showMessage('Report has been loaded successfully');
end;

procedure TfrmFlickrMain.btnShowReportMouseEnter(Sender: TObject);
begin
  ShowHint('Show HTML report.', Sender);
end;

procedure TfrmFlickrMain.btnUnbanGroupsClick(Sender: TObject);
var
  photos: TList<string>;
  groups: TList<string>;
  i : integer;
  j : integer;
  photoId: string;
  groupId: string;
  p : IPhoto;
  OmitGroups : string;
  total : integer;
  option : integer;
begin
  //Ban these groups in the photos selected.
  PageControl3.ActivePage := tabStatus;
  photos := TList<string>.Create;
  groups := TList<string>.Create;
  try
    batchUpdate.Enabled := false;
    for i := 0 to listPhotos.Items.Count - 1 do
    begin
      if listPhotos.Items[i].Checked then
        photos.Add(listPhotos.Items[i].Caption);
    end;
    for i := 0 to listGroups.Items.Count - 1 do
    begin
      if listGroups.Items[i].Checked then
        groups.Add(listGroups.Items[i].Caption);
    end;
    mStatus.Lines.Add('******************************************************************');
    mStatus.Lines.Add('Unbanning ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each.');
    total := photos.Count * groups.Count;
    mStatus.Lines.Add('Total number of transactions: ' + total.ToString());
    option := MessageDlg('Do you want to unban ' + photos.Count.ToString + ' photos from ' + groups.Count.ToString + ' groups each?', mtInformation, mbOKCancel, 0);
    if option = mrOK then
    begin
      for i := 0 to photos.Count - 1 do
      begin
        photoId := photos[i];
        p := repository.GetPhoto(photoId);
        OmitGroups := p.OmitGroups;
        for j := 0 to groups.Count - 1 do
        begin
          groupId := groups[j];
          if OmitGroups.Contains(groupId) then
          begin
            OmitGroups := OmitGroups.Replace(groupId + ',', '');
            mStatus.Lines.Add('Removing photo from banned list for  : ' + photoId + ': ' + OmitGroups);
          end
          else
            mStatus.Lines.Add('Photo not in the banned list : ' + groupId);
        end;
        p.OmitGroups := OmitGroups;
      end;
    end;
  finally
    batchUpdate.Enabled := true;
    mStatus.Lines.Add('******************************************************************');
    photos.Free;
    groups.Free;
  end;
  showMessage('Photos have been unbanned from the groups, please refresh the main grid!');
end;

procedure TfrmFlickrMain.btnUnbanGroupsMouseEnter(Sender: TObject);
begin
  ShowHint('Unban groups from photos.', Sender);
end;

procedure TfrmFlickrMain.Button8Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to listGroups.Items.Count - 1 do
  begin
    listGroups.Items[i].Checked := false;
  end;
end;

procedure TfrmFlickrMain.Button9Click(Sender: TObject);
begin
  getTotalAlbumsCounts();
  showMessage('Albums have been loaded');
end;

procedure TfrmFlickrMain.Button9MouseEnter(Sender: TObject);
begin
  ShowHint('Load the counts for your albums.', Sender);
end;

procedure TfrmFlickrMain.chartAlbumClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ValueIndex >= 0 then
    OpenPhotosAlbum(Series.ValueMarkText[ValueIndex]);
end;

procedure TfrmFlickrMain.chartAlbumMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SeriesIndex: Integer;
begin
  try
    SeriesIndex := chartAlbum.Series[0].Clicked(X, Y);
    chartAlbum.ShowHint := SeriesIndex <> -1;
    if chartAlbum.ShowHint then
    begin
      chartAlbum.Hint := chartAlbum.Series[0].ValueMarkText[SeriesIndex];
    end;
  except

  end;
end;

procedure TfrmFlickrMain.ChartHallLikesClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ValueIndex >= 0 then
    ShowMessage(Series.ValueMarkText[ValueIndex]);
end;

procedure TfrmFlickrMain.chartHallViewsClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ValueIndex >= 0 then
    ShowMessage(Series.ValueMarkText[ValueIndex]);
end;

procedure TfrmFlickrMain.OpenPhotosAlbum(value : string);
var
  photoSetId : string;
  PhotosCount : string;
  title : string;
  ViewCount : string;
  response : string;
begin
  //Get the values from the value, Id,
  response := value;
  photoSetId := AnsiLeftStr(response, AnsiPos('/', response));
  response := AnsiRightStr(response, length(response) - length(photoSetId));
  photoSetId := photoSetId.replace('/','');

  title := AnsiLeftStr(response, AnsiPos('/', response));
  response := AnsiRightStr(response, length(response) - length(title));
  title := title.replace('/','');

  PhotosCount := AnsiLeftStr(response, AnsiPos('/', response));
  response := AnsiRightStr(response, length(response) - length(PhotosCount));
  PhotosCount := PhotosCount.replace('/','');

  ViewCount := response;

  frmFlickrPhotoSet := TfrmFlickrPhotoSet.Create(nil);
  try
    frmFlickrPhotoSet.repository := repository;
    frmFlickrPhotoSet.optionsAgent := optionsAgent;
    frmFlickrPhotoSet.totalFromAlbum := PhotosCount;
    frmFlickrPhotoSet.Caption := 'PhotoSet info for ' + title;
    frmFlickrPhotoSet.LoadPhotos(photoSetId);
    frmFlickrPhotoSet.ShowModal;
  finally
    frmFlickrPhotoSet.Free;
  end;
end;

procedure TfrmFlickrMain.OpenPhotosAlbumId(photoSetId : string; title : string; PhotosCount : string);
begin
  frmFlickrPhotoSet := TfrmFlickrPhotoSet.Create(nil);
  try
    frmFlickrPhotoSet.repository := repository;
    frmFlickrPhotoSet.optionsAgent := optionsAgent;
    frmFlickrPhotoSet.totalFromAlbum := PhotosCount;
    frmFlickrPhotoSet.Caption := 'PhotoSet info for ' + title;
    frmFlickrPhotoSet.LoadPhotos(photoSetId);
    frmFlickrPhotoSet.ShowModal;
  finally
    frmFlickrPhotoSet.Free;
  end;
end;

procedure TfrmFlickrMain.ChartViewsDblClick(Sender: TObject);
begin
  frmChartViewer := TfrmChartViewer.Create(nil);
  frmChartViewer.CloneChart(TChart(Sender));
  frmChartViewer.Show;
end;

procedure TfrmFlickrMain.CheckAll1Click(Sender: TObject);
var
  i: Integer;
begin
  if chkAddItem.Checked then
    listPhotos.OnItemChecked := nil;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    listPhotos.Items[i].Checked := true;
  end;
  Label31.Caption := 'Number of items: ' + InttoStr(listphotos.Items.Count) + ' (' + InttoStr(listphotos.Items.Count) + ') selected';
  if chkAddItem.Checked then
    listPhotos.OnItemChecked := listPhotosItemChecked;
end;

procedure TfrmFlickrMain.CheckAll2Click(Sender: TObject);
var
  i: Integer;
begin
  listgroups.OnItemChecked := nil;
  for i := 0 to listGroups.Items.Count - 1 do
  begin
    listGroups.Items[i].Checked := true;
  end;
  listgroups.OnItemChecked := listGroupsItemChecked;
  Label11.Caption := 'Number of items: ' + InttoStr(listgroups.Items.Count) + '(' + InttoStr(listgroups.Items.Count) + ') selected';
end;

procedure TfrmFlickrMain.chkAddItemClick(Sender: TObject);
begin
  if options.DisableTrendDisplay <> chkAddItem.Checked then
    FDirtyOptions := true;
end;

procedure TfrmFlickrMain.chkPendingClick(Sender: TObject);
begin
  if options.ConsiderPendingQueueItems <> chkPending.Checked then
  begin
    FDirtyOptions := true;
    Log('chkPending has changed');
  end;
end;

procedure TfrmFlickrMain.chkRealTimeClick(Sender: TObject);
begin
  if options.UpdateCountsRealTime <> chkRealTime.Checked then
  begin
    FDirtyOptions := true;
    Log('chkRealTime has changed');
  end;
end;

procedure TfrmFlickrMain.chkRejectedClick(Sender: TObject);
begin
  if options.KeepRejectedListAlive <> chkRejected.Checked then
  begin
    FDirtyOptions := true;
    Log('chkRejected has changed');
  end;
end;

procedure TfrmFlickrMain.chkResponsesClick(Sender: TObject);
begin
  if options.DisplaySuccessfulResponses <> chkResponses.Checked then
  begin
    FDirtyOptions := true;
    Log('chkResponses has changed');
  end;
end;

procedure TfrmFlickrMain.chkShowButtonHintClick(Sender: TObject);
begin
  if options.showHints <> chkShowButtonHint.Checked then
  begin
    FDirtyOptions := true;
    Log('chkShowButtonHint has changed');
  end;
end;

procedure TfrmFlickrMain.chksortingClick(Sender: TObject);
begin
  if options.sortingEnabled <> chksorting.Checked then
  begin
    FDirtyOptions := true;
    Log('chksorting has changed');
  end;
end;

procedure TfrmFlickrMain.chkUpdateCollectionsClick(Sender: TObject);
begin
  if options.UpdateCollections <> chkUpdateCollections.Checked then
  begin
    FDirtyOptions := true;
    Log('chkUpdateCollections has changed');
  end;
end;

procedure TfrmFlickrMain.ClearSelection1Click(Sender: TObject);
var
  i: Integer;
begin
  if (startMark <> -1) and (endMark <> -1) then
  begin
    for i := startMark to endMark do
    begin
        listPhotos.Items[i].Checked := not listPhotos.Items[i].Checked;
    end;
    startMark := -1;
    endMark := -1;
  end;
  UpdateLabel();
end;

procedure TfrmFlickrMain.showMarksClick(Sender: TObject);
begin
  flickrChart.VisibleMarks(chartitemViews, showMarks.Checked);
  flickrChart.VisibleMarks(chartitemLikes, showMarks.Checked);
  flickrChart.VisibleMarks(totalGroups, showMarks.Checked);
  flickrChart.VisibleMarks(chartitemComments, showMarks.Checked);
  flickrChart.VisibleMarks(chartitemViewsH, showMarks.Checked);
  flickrChart.VisibleMarks(chartitemLikesH, showMarks.Checked);
  flickrChart.VisibleMarks(chartitemCommentsH, showMarks.Checked);
//  flickrChart.VisibleMarks(ChartGeneral, showMarks.Checked);
  flickrChart.VisibleMarks(ChartViews, showMarks.Checked);
  flickrChart.VisibleMarks(totalPhotos, showMarks.Checked);
  flickrChart.VisibleMarks(ChartLikes, showMarks.Checked);
  flickrChart.VisibleMarks(ChartComments, showMarks.Checked);
  flickrChart.VisibleMarks(dailyViews, showMarks.Checked);
  flickrChart.VisibleMarks(dailyLikes, showMarks.Checked);
  if options.ShowMarksInGraphs <> showMarks.Checked then
  begin
    FDirtyOptions := true;
    Log('showMarks has changed');
  end;
end;

procedure TfrmFlickrMain.ComboBox1Change(Sender: TObject);
begin
  btnLoadProfile.Enabled := true;
  btnDeleteProfile.Enabled := true;
  //edtProfile.Enabled := true;
  btnSaveProfile.Enabled := true;
  btnRemoveProfile.Enabled := true;
end;

procedure TfrmFlickrMain.dailyViewsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Log('mouse down for ' + x.ToString() + ' ' + y.ToString());
end;

procedure TfrmFlickrMain.dailyViewsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Log('mouse up for ' + x.ToString() + ' ' + y.ToString());
end;

procedure TfrmFlickrMain.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
begin
  case Button of
    nbFirst:
    begin
        dailyViews.Page := 0;
    end;
    nbPrior:
    begin
      dailyViews.Pages.PreviousPage;
    end;
    nbNext:
    begin
      dailyViews.Pages.NextPage;
    end;
    nbLast:
    begin
      dailyViews.Page := dailyViews.Pages.Count-1;
    end;
  end;
end;

procedure TfrmFlickrMain.Delete1Click(Sender: TObject);
var
  id : string;
  buttonSelected : integer;
begin
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    buttonSelected := MessageDlg('Are you sure you want to remove '+id+' from the list?',mtCustom,
                              [mbYes,mbCancel], 0);
    if buttonSelected = mrYes then
    begin
      listPhotos.Items[listPhotos.ItemIndex].Delete;
      repository.DeletePhoto(id);
      UpdateLabel();
    end;
  end;
end;

procedure TfrmFlickrMain.edtEmailChange(Sender: TObject);
begin
  if options.eMailAddress <> edtEmail.text then
  begin
    FDirtyOptions := true;
    Log('eMailAddress has changed');
  end;
end;

procedure TfrmFlickrMain.edtMaxChange(Sender: TObject);
begin
  if options.MaxItemsListGlobals <> edtMax.text then
  begin
    FDirtyOptions := true;
    Log('MaxItemsListGlobals has changed');
  end;
end;

procedure TfrmFlickrMain.edtMaxLogChange(Sender: TObject);
begin
  if options.MaxNumberOfLinesLog <> edtMaxLog.text then
  begin
    FDirtyOptions := true;
    Log('MaxNumberOfLinesLog has changed');
  end;
end;

procedure TfrmFlickrMain.edtProfileChange(Sender: TObject);
begin
  btnSaveProfile.Enabled := edtProfile.Text <> '';
  btnRemoveProfile.Enabled := btnSaveProfile.Enabled;
end;

procedure TfrmFlickrMain.edtUrlNameChange(Sender: TObject);
begin
  if options.urlName <> edtUrlName.text then
  begin
    FDirtyOptions := true;
    Log('urlName has changed');
  end;
end;

procedure TfrmFlickrMain.edtUserIdChange(Sender: TObject);
begin
  if optionsAgent.flickrUserId <> edtUserId.text then
  begin
    FDirtyOptions := true;
    Log('optionsAgent has changed');
  end;
end;

procedure TfrmFlickrMain.edtWorkspaceChange(Sender: TObject);
begin
  if options.Workspace <> edtWorkspace.text then
  begin
    FDirtyOptions := true;
    Log('Workspace has changed');
  end;
end;

procedure TfrmFlickrMain.btnGetGroupsClick(Sender: TObject);
var
  Item: TListItem;
  pages: string;
  numPages: Integer;
  i, j: Integer;
  st : TStopWatch;
  comparer : TCompareType;
  threadExec : TParallelGroupLoad;
  threads: array of TParallelGroupLoad;
begin
  if (apikey.text = '') or (userToken = '') then
  begin
    showmessage('You are not authorized!');
    exit;
  end;
  if (edtUserId.text = '') or (userTokenSecret = '') then
  begin
    showmessage('User ID key can''t be empty');
    exit;
  end;

  if Assigned(FilteredGroupList) then
  begin
    FilteredGroupList := nil;
    comparer := tCompareMembers;
    if RadioButton8.Checked then
      comparer := tCompareMembers;
    if RadioButton9.Checked then
      comparer := tComparePoolSize;
    if RadioButton11.Checked then
      comparer := tCompareRemaining;
    FilteredGroupList := TFilteredList.Create(comparer);
  end;

  btnFilterCancelClick(sender);
  batchUpdate.Enabled := false;
  pagecontrol3.TabIndex := 0;
  btnLoad.Enabled := false;
  btnAdd.Enabled := false;
  btnAddItems.Enabled := false;
  batchUpdate.Enabled := false;
  listGroups.Visible := false;
  progressbar1.Visible := true;
  Application.ProcessMessages;
  st := TStopWatch.Create;
  st.Start;
  threadExec := TParallelGroupLoad.create();
  try
    threadExec.restUrl := TFlickrRest.New(optionsAgent).getGroups('1', '100');
    threadExec.OptionsAgent := optionsAgent;
    threadExec.progressBar := progressbar1;
    threadExec.taskBar := taskbar1;
    threadExec.lvGroups := listGroups;
    threadExec.Initialize := true;
    threadExec.Start;
    threadExec.WaitFor;
    pages := threadExec.pages;
    for i := 0 to threadExec.FilteredGroupList.count-1 do
      FilteredGroupList.Add(threadExec.FilteredGroupList[i]);
  finally
    threadExec.Free;
  end;
  Application.ProcessMessages;
  // Load the remaining pages
  numPages := pages.ToInteger;
  SetLength(threads, numPages - 1);
  for i := 2 to numPages do
  begin
    threads[i-2] := TParallelGroupLoad.create();
    threads[i-2].restUrl := TFlickrRest.New(optionsAgent).getGroups(i.ToString, '100');
    threads[i-2].OptionsAgent := optionsAgent;
    threads[i-2].progressBar := progressbar1;
    threads[i-2].taskBar := taskbar1;
    threads[i-2].lvGroups := listGroups;
    threads[i-2].Initialize := false;
    threads[i-2].Start;
  end;
  Application.ProcessMessages;
  for i := 2 to numPages do
  begin
    threads[i-2].WaitFor;
    for j := 0 to threads[i-2].FilteredGroupList.count-1 do
      FilteredGroupList.Add(threads[i-2].FilteredGroupList[j]);
  end;
  Application.ProcessMessages;
  for i := 2 to numPages do
  begin
    threads[i-2].Free;
  end;
  st.Stop;
  log('loading groups in ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
  Application.ProcessMessages;
  // Add items to the listview
  st := TStopWatch.Create;
  FilteredGroupList.sort;
  st.Start;
  listGroups.OnItemChecked := nil;
  listgroups.OnCustomDrawItem := nil;
  for i := 0 to FilteredGroupList.list.Count - 1 do
  begin
    Item := listGroups.Items.Add;
    Item.Caption := FilteredGroupList.list[i].id;
    Item.SubItems.Add(FilteredGroupList.list[i].title);
    Item.SubItems.Add(FilteredGroupList.list[i].Photos.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].Members.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].Description);
    Item.SubItems.Add(FilteredGroupList.list[i].IsModerated.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleCount.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleMode);
    Item.SubItems.Add(FilteredGroupList.list[i].ThrottleRemaining.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].photos_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].videos_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].images_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].screens_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].art_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].safe_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].moderate_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].restricted_ok.ToString());
    Item.SubItems.Add(FilteredGroupList.list[i].has_geo.ToString());
  end;
  listGroups.OnItemChecked := listGroupsItemChecked;
  listgroups.OnCustomDrawItem := listGroupsCustomDrawItem;
  st.Stop;
  log('populating group list ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  st := TStopWatch.Create;
  st.Start;
  FilteredGroupList.save(options.Workspace + '\flickrGroups.xml');
  st.Stop;
  log('Saving repository flickrGroups: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  btnLoad.Enabled := true;
  btnAdd.Enabled := true;
  batchUpdate.Enabled := true;
  btnAddPhotos.Enabled := true;
  btnRemovePhoto.Enabled := true;
  btnBanGroups.Enabled := true;
  btnUnBanGroups.Enabled := true;
  btnAddItems.Enabled := true;
  progressbar1.Visible := false;
  Taskbar1.ProgressValue := 0;
  listGroups.Visible := true;
  UpdateLabelGroups();
  showMessage('Flickr groups have been loaded');
end;

procedure TfrmFlickrMain.btnGetGroupsMouseEnter(Sender: TObject);
begin
  ShowHint('Reload the groups you belong from Flickr.', Sender);
end;

function TfrmFlickrMain.getTotalStreamViews() : Integer;
begin
  result := TUserInfo.getStreamViews(edtUserId.text, optionsAgent);
end;

function TfrmFlickrMain.getTotalAlbumsCounts(): Integer;
var
  pages: string;
  total : integer;
  numPages: Integer;
  i: Integer;
  Series : TPieSeries;
  threadExec : TParallelAlbum;
  threads: array of TParallelAlbum;
  results : array of integer;
  st : TStopWatch;
const
  maxAlbumsLoad = '30';
begin
  st := TStopWatch.Create;
  st.Start;
  if chartAlbum.SeriesList.Count > 0 then
    chartAlbum.RemoveAllSeries;

  Series := flickrChart.GetNewPieSeries(chartAlbum, true);

  listPhotosUser.Visible := false;
  progressbar1.Visible := true;
  lvAlbums.Clear;
  application.ProcessMessages;
  threadExec := TParallelAlbum.create();
  try
    threadExec.restUrl := TFlickrRest.New(optionsAgent).getPhotoSets('1', maxAlbumsLoad);
    threadExec.progressBar := progressbar1;
    threadExec.taskBar := taskbar1;
    threadExec.series := Series;
    threadExec.lvAlbums := lvAlbums;
    threadExec.Initialize := true;
    threadExec.Start;
    threadExec.WaitFor;
    pages := threadExec.pages;
    total := threadExec.TotalViews;
  finally
    threadExec.Free;
  end;

  application.ProcessMessages;
  // Load the remaining pages
  numPages := pages.ToInteger;
  SetLength(threads, numPages - 1);
  SetLength(results, numPages - 1);
  for i := 2 to numPages do
  begin
    threads[i-2] := TParallelAlbum.create();
    threads[i-2].restUrl := TFlickrRest.New(optionsAgent).getPhotoSets(i.toString(), maxAlbumsLoad);
    threads[i-2].progressBar := progressbar1;
    threads[i-2].taskBar := taskbar1;
    threads[i-2].series := Series;
    threads[i-2].lvAlbums := lvAlbums;
    threads[i-2].Initialize := false;
    threads[i-2].Start;
  end;

  for i := 2 to numPages do
  begin
    threads[i-2].WaitFor;
    application.ProcessMessages;
    total := total + threads[i-2].TotalViews;
  end;

  for i := 2 to numPages do
  begin
    threads[i-2].Free;
  end;
  application.ProcessMessages;
  chartAlbum.AddSeries(Series);

  progressbar1.Visible := false;
  Taskbar1.ProgressValue := 0;
  listPhotosUser.Visible := true;
  LblTotalAlbum.Caption := 'Total views: ' + total.ToString() + ' in ' + lvAlbums.Items.Count.ToString + ' albums';
  st.Stop;
  log('Loading Albums: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
  Result := total;
end;

procedure TfrmFlickrMain.btnGetListClick(Sender: TObject);
var
  Item: TListItem;
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, title, id, ispublic, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  xmlDocument: IXMLDocument;
  timedout: Boolean;
begin
  if apikey.text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  if edtUserId.text = '' then
  begin
    showmessage('UserId can''t be empty');
    exit;
  end;
  btnLoad.Enabled := false;
  btnAdd.Enabled := false;
  btnAddItems.Enabled := false;
  batchUpdate.Enabled := false;
  listPhotosUser.Visible := false;
  progressbar1.Visible := true;
  Application.ProcessMessages;

  IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdIOHandler.ReadTimeout := IdTimeoutInfinite;
  IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
  xmlDocument := TXMLDocument.Create(nil);
  IdHTTP := TIdHTTP.Create(nil);
  try
    IdHTTP.IOHandler := IdIOHandler;
    timedout := false;
    while (not timedout) do
    begin
      try
        response := IdHTTP.Get(TFlickrRest.New(optionsAgent).getPhotos('1', '500'));
        timedout := true;
      except
        on e: exception do
        begin
          sleep(timeout);
          timedout := false;
        end;
      end;
    end;
      xmlDocument.LoadFromXML(response);
      iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
      iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
      iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photos>
      pages := iXMLRootNode3.attributes['pages'];
      total := iXMLRootNode3.attributes['total'];
      iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photo>
      listPhotosUser.Clear;
      numTotal := total.ToInteger();
      progressbar1.Max := numTotal;
      Taskbar1.ProgressState := TTaskBarProgressState.Normal;
      Taskbar1.ProgressMaxValue := numTotal;
      progressbar1.position := 0;
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ispublic := iXMLRootNode4.attributes['ispublic'];
          title := iXMLRootNode4.attributes['title'];
          if ispublic = '1' then
          begin
            Item := listPhotosUser.Items.Add;
            Item.Caption := id;
            Item.SubItems.Add(title);
          end;
        end;
        progressbar1.position := progressbar1.position + 1;
        Taskbar1.ProgressValue := progressbar1.position;
        Application.ProcessMessages;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
  finally
    IdIOHandler.Free;
    IdHTTP.Free;
    xmlDocument := nil;
  end;

  // Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numPages do
  begin
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    xmlDocument := TXMLDocument.Create(nil);
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      timedout := false;
      while (not timedout) do
      begin
        try
          response := IdHTTP.Get(TFlickrRest.New(optionsAgent).getPhotos(i.ToString, '500'));
          timedout := true;
        except
          on e: exception do
          begin
            sleep(timeout);
            timedout := false;
          end;
        end;
      end;
        xmlDocument.LoadFromXML(response);
        iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
        iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
        iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photos>
        pages := iXMLRootNode3.attributes['pages'];
        iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photo>
        while iXMLRootNode4 <> nil do
        begin
          if iXMLRootNode4.NodeName = 'photo' then
          begin
            id := iXMLRootNode4.attributes['id'];
            ispublic := iXMLRootNode4.attributes['ispublic'];
            title := iXMLRootNode4.attributes['title'];
            if ispublic = '1' then
            begin
              Item := listPhotosUser.Items.Add;
              Item.Caption := id;
              Item.SubItems.Add(title);
            end;
          end;
          progressbar1.position := progressbar1.position + 1;
          Taskbar1.ProgressValue := progressbar1.position;
          Application.ProcessMessages;
          iXMLRootNode4 := iXMLRootNode4.NextSibling;
        end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;
  end;
  btnLoad.Enabled := true;
  btnAdd.Enabled := true;
  batchUpdate.Enabled := true;
  btnAddItems.Enabled := true;
  progressbar1.Visible := false;
  Taskbar1.ProgressValue := 0;
  listPhotosUser.Visible := true;
  showMessage('Flickr photo list has been loaded');
end;

procedure TfrmFlickrMain.btnGetListMouseEnter(Sender: TObject);
begin
  ShowHint('Get the list of photos from your Flickr stream.', Sender);
end;

procedure TfrmFlickrMain.btnAddItemsClick(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  btnAddItems.Enabled := false;
  ProgressBar1.Visible := true;
  photoId.Enabled := false;
  btnAdd.Enabled := false;
  batchUpdate.Enabled := false;
  Process.Visible := true;
  ProgressBar1.Min := 0;
  ProgressBar1.Max := listPhotos.Items.Count;
  for i := 0 to listPhotosUser.Items.Count - 1 do
  begin
    if listPhotosUser.Items[i].Checked then
    begin
      Process.Caption := 'Processing image: ' + listPhotosUser.Items[i].Caption + ' ' + i.ToString + ' out of ' + listPhotosUser.Items.Count.ToString;
      ProgressBar1.position := i;
      Application.ProcessMessages;

      if not ExistPhotoInList(listPhotosUser.Items[i].Caption, Item) then
      begin
        photoId.text := listPhotosUser.Items[i].Caption;
        FAvoidMessage := true;
        btnAddClick(Sender);
        FAvoidMessage := false;
      end;
    end;
  end;
  photoId.text := '';
  ProgressBar1.Visible := false;
  Process.Visible := false;
  UpdateTotals(false);
  LoadHallOfFame(repository);
  btnSave.Enabled := true;
  btnBackup.Enabled := true;
  batchUpdate.Enabled := true;
  photoId.Enabled := true;
  btnAdd.Enabled := true;
  btnAddItems.Enabled := true;
  showMessage('Photos have been added to the main list');
end;

procedure TfrmFlickrMain.btnAddItemsMouseEnter(Sender: TObject);
begin
  ShowHint('Import your photo information into the tool.', Sender);
end;

procedure TfrmFlickrMain.btnAddMouseEnter(Sender: TObject);
begin
  ShowHint('Add pictures manually using the PhotoID', Sender);
end;

function TfrmFlickrMain.SaveToExcel(AView: TListView; ASheetName, AFileName: string): Boolean;
const
  xlWBATWorksheet = -4167;
var
  Row: Integer;
  ExcelOLE, Sheet: OLEVariant;
  i: Integer;
begin
  // Create Excel-OLE Object
  Result := false;
  ExcelOLE := CreateOleObject('Excel.Application');
  try
    // Hide Excel
    ExcelOLE.Visible := false;

    ExcelOLE.Workbooks.Add(xlWBATWorksheet);
    Sheet := ExcelOLE.Workbooks[1].WorkSheets[1];
    Sheet.Name := ASheetName;

    Sheet.Cells[1, 1] := 'Id';
    Sheet.Cells[1, 2] := 'Title';
    Sheet.Cells[1, 3] := 'Views';
    Sheet.Cells[1, 4] := 'Likes';
    Sheet.Cells[1, 5] := 'Comments';
    Sheet.Cells[1, 6] := 'Last Update';
    Sheet.Cells[1, 7] := 'Taken';
    Sheet.Cells[1, 8] := 'Albums';
    Sheet.Cells[1, 9] := 'Groups';
    Sheet.Cells[1, 10] := 'Tags';
    Sheet.Cells[1, 11] := 'Affection';
    Sheet.Cells[1, 12] := 'Banned';
    Sheet.Cells[1, 13] := 'Trend';
    Sheet.Cells[1, 14] := 'Omit Groups';

    Row := 2;
    for i := 0 to AView.Items.Count - 1 do
    begin
      Sheet.Cells[Row, 1] := AView.Items.Item[i].Caption;
      Sheet.Cells[Row, 2] := AView.Items.Item[i].SubItems[0];
      Sheet.Cells[Row, 3] := AView.Items.Item[i].SubItems[1];
      Sheet.Cells[Row, 4] := AView.Items.Item[i].SubItems[2];
      Sheet.Cells[Row, 5] := AView.Items.Item[i].SubItems[3];
      Sheet.Cells[Row, 6] := AView.Items.Item[i].SubItems[4];
      Sheet.Cells[Row, 7] := AView.Items.Item[i].SubItems[5];
      Sheet.Cells[Row, 8] := AView.Items.Item[i].SubItems[6];
      Sheet.Cells[Row, 9] := AView.Items.Item[i].SubItems[7];
      Sheet.Cells[Row, 10] := AView.Items.Item[i].SubItems[8];
      Sheet.Cells[Row, 11] := AView.Items.Item[i].SubItems[9];
      Sheet.Cells[Row, 12] := AView.Items.Item[i].SubItems[10];
      Sheet.Cells[Row, 13] := AView.Items.Item[i].SubItems[11];
      Sheet.Cells[Row, 14] := AView.Items.Item[i].SubItems[12];
      inc(Row);
    end;

    try
      ExcelOLE.Workbooks[1].SaveAs(AFileName);
      Result := true;
    except

    end;
  finally
    if not VarIsEmpty(ExcelOLE) then
    begin
      ExcelOLE.DisplayAlerts := false;
      ExcelOLE.Quit;
      ExcelOLE := Unassigned;
      Sheet := Unassigned;
    end;
  end;
end;

function TfrmFlickrMain.ExportGraphToExcel(viewsource : TViewType; ASheetName, AFileName: string): Boolean;
const
  xlWBATWorksheet = -4167;
var
  Row: Integer;
  ExcelOLE, Sheet: OLEVariant;
  i: Integer;
begin
  // Create Excel-OLE Object
  Result := false;
  ExcelOLE := CreateOleObject('Excel.Application');
  try
    // Hide Excel
    ExcelOLE.Visible := false;

    ExcelOLE.Workbooks.Add(xlWBATWorksheet);
    Sheet := ExcelOLE.Workbooks[1].WorkSheets[1];
    Sheet.Name := ASheetName;

    Sheet.Cells[1, 1] := 'Date';


    case viewsource of
      TotalViews:
      begin
        Sheet.Cells[1, 2] := 'Views';
        Row := 2;
        for i := 0 to globalsRepository.globals.Count - 1 do
        begin
          Sheet.Cells[Row, 1] := '''' + FormatDateTime('dd/mm/yyyy', globalsRepository.globals[i].Date);
          Sheet.Cells[Row, 2] := globalsRepository.globals[i].views.ToString;
          inc(Row);
        end;
        Row := 2;
      end;
      TotalLikes:
      begin
        Sheet.Cells[1, 2] := 'Likes';
        Row := 2;
      end;
      TotalComments:
      begin
        Sheet.Cells[1, 2] := 'Comments';
        Row := 2;
      end;
      TotalViewsHistogram:
      begin
        Sheet.Cells[1, 2] := 'Views';
        Row := 2;
        for i := 1 to globalsRepository.globals.Count - 1 do
        begin
          Sheet.Cells[Row, 1] := '''' + FormatDateTime('dd/mm/yyyy', globalsRepository.globals[i].Date);
          Sheet.Cells[Row, 2] := InttoStr(globalsRepository.globals[i].views - globalsRepository.globals[i - 1].views);
          inc(Row);
        end;
      end;
      TotalLikesHistogram:
      begin
        Sheet.Cells[1, 2] := 'Likes';
        Row := 2;
      end;
    end;

    try
      ExcelOLE.Workbooks[1].SaveAs(AFileName);
      Result := true;
    except

    end;
  finally
    if not VarIsEmpty(ExcelOLE) then
    begin
      ExcelOLE.DisplayAlerts := false;
      ExcelOLE.Quit;
      ExcelOLE := Unassigned;
      Sheet := Unassigned;
    end;
  end;
end;

function TfrmFlickrMain.SaveToExcelGroups(AView: TListView; ASheetName, AFileName: string): Boolean;
const
  xlWBATWorksheet = -4167;
var
  Row: Integer;
  ExcelOLE, Sheet: OLEVariant;
  i: Integer;
begin
  // Create Excel-OLE Object
  Result := false;
  ExcelOLE := CreateOleObject('Excel.Application');
  try
    // Hide Excel
    ExcelOLE.Visible := false;

    ExcelOLE.Workbooks.Add(xlWBATWorksheet);
    Sheet := ExcelOLE.Workbooks[1].WorkSheets[1];
    Sheet.Name := ASheetName;

    Sheet.Cells[1, 1] := 'Id';
    Sheet.Cells[1, 2] := 'Title';
    Sheet.Cells[1, 3] := 'Photos';
    Sheet.Cells[1, 4] := 'Members';
    Sheet.Cells[1, 5] := 'Description';
    Sheet.Cells[1, 6] := 'Moderated';
    Sheet.Cells[1, 7] := 'Count';
    Sheet.Cells[1, 8] := 'Mode';
    Sheet.Cells[1, 9] := 'Remaining';
    Sheet.Cells[1, 10] := 'PhotosOK';
    Sheet.Cells[1, 11] := 'VideosOK';
    Sheet.Cells[1, 12] := 'ImgOK';
    Sheet.Cells[1, 13] := 'ScreenOK';
    Sheet.Cells[1, 14] := 'ArtOK';
    Sheet.Cells[1, 15] := 'SafeOK';
    Sheet.Cells[1, 16] := 'ModerateOK';
    Sheet.Cells[1, 17] := 'RestrictedOK';
    Sheet.Cells[1, 18] := 'Geo';

    Row := 2;
    for i := 0 to AView.Items.Count - 1 do
    begin
      Sheet.Cells[Row, 1] := AView.Items.Item[i].Caption;
      Sheet.Cells[Row, 2] := AView.Items.Item[i].SubItems[0];
      Sheet.Cells[Row, 3] := AView.Items.Item[i].SubItems[1];
      Sheet.Cells[Row, 4] := AView.Items.Item[i].SubItems[2];
      Sheet.Cells[Row, 5] := AView.Items.Item[i].SubItems[3];
      Sheet.Cells[Row, 6] := AView.Items.Item[i].SubItems[4];
      Sheet.Cells[Row, 7] := AView.Items.Item[i].SubItems[5];
      Sheet.Cells[Row, 8] := AView.Items.Item[i].SubItems[6];
      Sheet.Cells[Row, 9] := AView.Items.Item[i].SubItems[7];
      Sheet.Cells[Row, 10] := AView.Items.Item[i].SubItems[8];
      Sheet.Cells[Row, 11] := AView.Items.Item[i].SubItems[9];
      Sheet.Cells[Row, 12] := AView.Items.Item[i].SubItems[10];
      Sheet.Cells[Row, 13] := AView.Items.Item[i].SubItems[11];
      Sheet.Cells[Row, 14] := AView.Items.Item[i].SubItems[12];
      Sheet.Cells[Row, 15] := AView.Items.Item[i].SubItems[13];
      Sheet.Cells[Row, 16] := AView.Items.Item[i].SubItems[14];
      Sheet.Cells[Row, 17] := AView.Items.Item[i].SubItems[15];
      Sheet.Cells[Row, 18] := AView.Items.Item[i].SubItems[16];
      inc(Row);
    end;

    try
      ExcelOLE.Workbooks[1].SaveAs(AFileName);
      Result := true;
    except

    end;
  finally
    if not VarIsEmpty(ExcelOLE) then
    begin
      ExcelOLE.DisplayAlerts := false;
      ExcelOLE.Quit;
      ExcelOLE := Unassigned;
      Sheet := Unassigned;
    end;
  end;
end;

procedure TfrmFlickrMain.secretChange(Sender: TObject);
begin
  if optionsAgent.secret <> secret.text then
    FDirtyOptions := true;
end;

procedure TfrmFlickrMain.ShowListAlbums1Click(Sender: TObject);
var
  id : string;
  photo : IPhoto;
  album : TPair<string, IAlbum>;
begin
  ListDisplay := TfrmFlickrContext.Create(self);
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    photo := repository.GetPhoto(id);
    for album in photo.Albums do
    begin
      ListDisplay.AddItem(album.value.id, album.value.title);
    end;
  end;
  ListDisplay.Show;
end;

procedure TfrmFlickrMain.ShowListGroups1Click(Sender: TObject);
var
  id : string;
  photo : IPhoto;
  Item: TPair<string, IPool>;
begin
  ListDisplay := TfrmFlickrContext.Create(self);
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    photo := repository.GetPhoto(id);
    for Item in photo.Groups do
    begin
      ListDisplay.AddItem(Item.Value.id, Item.Value.title, DateToStr(Item.Value.Added));
    end;
  end;
  ListDisplay.Show;
end;

procedure TfrmFlickrMain.ShowonFlickr1Click(Sender: TObject);
var
  id : string;
begin
  if edtUrlName.text = '' then
    ShowMessage('Url Name needs to be populated.');
  //Show item in the URL view
  //https://www.flickr.com/photos/jordicorbillaphotography/
  if listPhotos.ItemIndex <> -1 then
  begin
    id := listPhotos.Items[listPhotos.ItemIndex].Caption;
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://www.flickr.com/photos/'+edtUrlName.text+'/' + id + '/in/photostream/lightbox/'), nil, SW_SHOW);
  end;
end;

procedure TfrmFlickrMain.ShowonFlickr2Click(Sender: TObject);
var
  id : string;
begin
  //Show item in the URL view
  //https://www.flickr.com/photos/jordicorbillaphotography/
  if listGroups.ItemIndex <> -1 then
  begin
    id := listGroups.Items[listGroups.ItemIndex].Caption;
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://www.flickr.com/groups/' + id), nil, SW_SHOW);
  end;
end;

procedure TfrmFlickrMain.Splitter1Moved(Sender: TObject);
begin
  chartItemViews.Height := round(panel4.Height / 2);
  chartItemLikes.Height := round(panel4.Height / 2);
  chartItemComments.Height := round(panel4.Height / 2);
  panel22.Width := round(panel4.Width / 4);
  panel23.Width := round(panel4.Width / 4);
  panel11.Width := round(panel4.Width / 4);
  totalGroups.Width := round(panel4.Width / 4);
end;

procedure TfrmFlickrMain.Splitter6Moved(Sender: TObject);
begin
  ResizeChartsDashBoard();
end;

procedure TfrmFlickrMain.StartMarking1Click(Sender: TObject);
begin
  if listPhotos.ItemIndex <> -1 then
  begin
    startMark :=  listPhotos.ItemIndex;
    endMark := -1;
  end;
end;

procedure TfrmFlickrMain.TabSheet7Exit(Sender: TObject);
var
  option : Integer;
begin
  if FDirtyOptions then
  begin
    option := MessageDlg('There are changes pending to be saved, do you want to save them?',mtInformation, mbOKCancel, 0);
    if option = mrOK then
      btnSaveOptionsClick(sender)
    else
      LoadOptions;
    FDirtyOptions := false;
  end;
end;

procedure TfrmFlickrMain.btnExcelClick(Sender: TObject);
begin
  if SaveToExcel(listPhotos, 'Flickr Analytics', options.Workspace + '\FlickrAnalytics.xls') then
  begin
    ExportGraphToExcel(TotalViews, 'Total Views History',options.Workspace + '\FlickrAnalyticsTotalViews.xls');
    ExportGraphToExcel(TotalViewsHistogram, 'Total Views History',options.Workspace + '\FlickrAnalyticsTotalViewsHistogram.xls');
    showmessage('Data saved successfully!');
  end;
end;

procedure TfrmFlickrMain.btnExcelMouseEnter(Sender: TObject);
begin
  ShowHint('Export the main list into Excel.', Sender);
end;

procedure TfrmFlickrMain.FormCreate(Sender: TObject);
var
  comparer : TCompareType;
begin
  SplitView.Close;
  if chksorting.Checked then
  begin
    comparer := tCompareViews;
    if radioButton1.checked then
      comparer := tCompareId;
    if radioButton2.checked then
      comparer := tCompareViews;
    if radioButton3.checked then
      comparer := tCompareLikes;
    if radioButton4.checked then
      comparer := tCompareComments;
    if radioButton5.checked then
      comparer := tCompareTaken;
    if radioButton6.checked then
      comparer := tCompareAlbums;
    if radioButton7.checked then
      comparer := tCompareGroups;
    repository := TFlickrRepository.Create(chksorting.Checked, comparer);
  end
  else
    repository := TFlickrRepository.Create();
  organic := TFlickrOrganic.Create();
  rejected := TRejected.Create;
  flickrProfiles := TProfiles.Create();
  FilteredGroupList := TFilteredList.Create(tCompareMembers);

  comparer := tCompareMembers;
  if RadioButton8.Checked then
    comparer := tCompareMembers;
  if RadioButton9.Checked then
    comparer := tComparePoolSize;
  if RadioButton11.Checked then
    comparer := tCompareRemaining;
  FilteredGroupList := TFilteredList.Create(comparer);

  globalsRepository := TFlickrGlobals.Create();
  CheckedSeries := TStringList.Create;
  Process.Visible := false;
  PageControl1.ActivePage := Dashboard;
  PageControl2.ActivePage := statistics;
  startMark := -1;
  endMark := -1;
  flickrChart := TFlickrChart.create;
  frmFlickrMain.Caption := 'Flickr Photo Analytics ' + TUtils.GetVersion + ' - Free (non-commercial use only)';
  RepositoryLoaded := false;
  ClearAllCharts();
  FDirtyOptions := false;
  FAvoidMessage := false;

  Fgreen := RGB(126, 203, 52);
  FRed := RGB(231, 75, 61);
  FYellow := RGB(240, 195, 29);
  FHardBlue := RGB(93, 173, 225);
  FSoftBlue := RGB(214, 233, 247);
  FOrange := RGB(242, 155, 25);
  FViolet := RGB(74, 16, 165);
  FHardPink := RGB(74, 36, 57);
  FSoftPink := RGB(173, 93, 140);
  FHardGreen := RGB(16, 101, 90);
  FSoftGreen := RGB(66, 199, 181);
  FColorViews := RGB(107, 60, 82);
  FColorLikes := RGB(57, 65, 173);
  FColorComments := RGB(148, 101, 24);
  FColorGroups := RGB(107, 117, 99);
  FColorPhotos := RGB(41, 93, 206);
  FColorSpread := RGB(90, 36, 181);
  FViewsP := RGB(198, 113, 41);
  FLikesP := RGB(165, 125, 173);
  FCommentsP := RGB(214, 113, 140);

  InitialiseDashboardTrend()
end;

procedure TfrmFlickrMain.Image6Click(Sender: TObject);
begin
  if splitview.Opened then
    splitview.Close
  else
    splitview.Open;
end;

procedure TfrmFlickrMain.InitialiseDashboardTrend();
var
  totals : double;
begin
  Shape1.Brush.Color := FColorViews;
  Shape2.Brush.Color := FColorLikes;
  Shape3.Brush.Color := FColorComments;
  Shape4.brush.Color := FColorGroups;
  Shape5.brush.Color := FColorPhotos;
  Shape6.brush.Color := FColorSpread;
  totals := 0.0;
  TotalViewsLabel.Caption :=  'Stream Views: '+Format('%n',[totals]).Replace('.00','')+' Album Views: '+Format('%n',[totals]).Replace('.00','')+' Total: '+Format('%n',[totals]).Replace('.00','');
  TotalLikesLabel.Caption :=  'Likes '+Format('%n',[totals]).Replace('.00','')+' Lost '+Format('%n',[totals]).Replace('.00','')+' Total: '+ Format('%n',[totals]).Replace('.00','');
  TotalCommentsLabel.Caption :=  Format('%n',[totals]).Replace('.00','');
  totalGroupsLabel.caption :=  Format('%n',[totals]).Replace('.00','') + ' group spread';
  totalGroupsRateLabel.caption :=  Format('%n',[totals]).Replace('.00','') + ' groups/photo';
  updateTimeLabel.Caption := '00/00/0000';
  SaveTimeLabel.Caption := '';
  PredictionLabel.Caption := Format('%n',[totals]).Replace('.00','');
  PhotosLabel.Caption := '0';
  FollowingLabel.Caption := '0';

  LabelYesterdayViews.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelTodayViews.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelArrow1.Caption := '+' + Format('%n',[totals]).Replace('.00','') + '%';
  LabelViewsPhoto.Caption := Format('%n',[totals]).Replace('.00','') + ' views/photo';
  LabelViewsYesterdayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  LabelViewsTodayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  UpdateArrows(0,0,0.0,0.0, upgreen1, downred1, upgreen11, downred11, LabelArrow1);

  LabelYesterdayLikes.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelTodayLikes.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelArrow2.Caption := '+' + Format('%n',[totals]).Replace('.00','') + '%';
  LabelLikesPhoto.Caption := Format('%n',[totals]).Replace('.00','') + ' likes/photo';
  LabelLikesYesterdayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  LabelLikesTodayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  UpdateArrows(0,0,0.0,0.0, upgreen2, downred2, upgreen22, downred22, LabelArrow2);

  LabelYesterdayComments.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelTodayComments.Caption :=  Format('%n',[totals]).Replace('.00','');
  LabelArrow3.Caption := '+' + Format('%n',[totals]).Replace('.00','') + '%';
  LabelCommentsPhoto.Caption := Format('%n',[totals]).Replace('.00','') + ' comments/photo';
  LabelCommentsYesterdayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  LabelCommentsTodayRate.Caption := Format('%n',[totals]).Replace('.00','') + '%';
  UpdateArrows(0,0,0.0,0.0, upgreen3, downred3, upgreen33, downred33, LabelArrow3);
end;

procedure TfrmFlickrMain.UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday : integer; out rateYesterday : double; out rateToday : double);
begin
  TRate.UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday, rateYesterday, rateToday);
end;

procedure TfrmFlickrMain.UpdateRate(valueBeforeYesterday, valueYesterday, valueToday : integer; out rateYesterday : double; out rateToday : double);
begin
  TRate.UpdateRate(valueBeforeYesterday, valueYesterday, valueToday, rateYesterday, rateToday);
end;

procedure TfrmFlickrMain.UpdateArrows(valueYesterday, valueToday : integer; rateYesterday, rateToday : double; arrow1, arrow2, arrow3, arrow4 : TImage; label1 : TLabel);
var
  value : double;
begin
  arrow1.Visible := false;
  arrow2.Visible := false;
  arrow3.Visible := false;
  arrow4.Visible := false;
  if valueYesterday = 0 then
    valueYesterday := valueToday;
  if valueYesterday < valueToday then //green
  begin
    arrow1.visible := true;
    value := ((valueToday / valueYesterday) -1 )* 100.0;
    if (valueToday > 0) and (value < 0) then
      value := value * -1;
    if value > 0 then
      label1.caption := '+' + Format('%n',[value]).Replace('.00','') + '%'
    else
      label1.caption := Format('%n',[value]).Replace('.00','') + '%'
  end;
  if valueYesterday = valueToday then //red
  begin
      arrow2.visible := true;
      label1.caption := '+' + Format('%n',[0.00]).Replace('.00','') + '%';
  end;
  if valueYesterday > valueToday then //red
  begin
    arrow2.visible := true;
    value := ((valueToday / valueYesterday) -1 )* 100.0;
    if value > 0 then
      label1.caption := '+' + Format('%n',[value]).Replace('.00','') + '%'
    else
      label1.caption := Format('%n',[value]).Replace('.00','') + '%'
  end;

  if TMathHelper.Compare(rateYesterday, rateToday, '<') then //green
  begin
    arrow3.visible := true;
  end;
  if TMathHelper.Compare(rateYesterday, rateToday, '=') then //red
  begin
    arrow4.visible := true;
  end;
  if TMathHelper.Compare(rateYesterday, rateToday, '>') then //red
  begin
    arrow4.visible := true;
  end;
end;


procedure TfrmFlickrMain.ClearAllCharts();
begin
  ChartStreamViews.RemoveAllSeries;
  ChartAlbumViews.RemoveAllSeries;
  ChartViews.RemoveAllSeries;
  totalPhotos.RemoveAllSeries;
  ChartLikes.RemoveAllSeries;
  ChartComments.RemoveAllSeries;
  executionTime.RemoveAllSeries;
  mostviewschart.RemoveAllSeries;
  mostlikeschart.RemoveAllSeries;
  chartfollowing.RemoveAllSeries;
  organicViews.RemoveAllSeries;
  organicLikes.RemoveAllSeries;
  organicComments.RemoveAllSeries;
  groupspread.RemoveAllSeries;
  dailyViews.RemoveAllSeries;
  dailyLikes.RemoveAllSeries;
  dailyComments.RemoveAllSeries;
  chartAlbum.RemoveAllSeries;
  chartHallViews.RemoveAllSeries;
  ChartHallLikes.RemoveAllSeries;
  chartItemViews.RemoveAllSeries;
  chartItemLikes.RemoveAllSeries;
  totalGroups.RemoveAllSeries;
  chartItemComments.RemoveAllSeries;
  chartItemViewsH.RemoveAllSeries;
  chartItemLikesH.RemoveAllSeries;
  chartItemCommentsH.RemoveAllSeries
end;

procedure TfrmFlickrMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CheckedSeries);
  repository := nil;
  organic := nil;
  flickrProfiles := nil;
  FilteredGroupList := nil;
  globalsRepository := nil;
  flickrChart := nil;
  rejected := nil;
  options := nil;
  optionsAgent := nil;
end;

procedure TfrmFlickrMain.ResizeChartsDashBoard();
begin
  //First Page
  panel14.Width := round(dashboard.Width * 0.55);
  panel17.width := round(panel14.Width * 0.33);
  panel18.width := round(panel14.Width * 0.33);

  chartViews.Height := round(panel17.Height * 0.40);
  chartLikes.Height := round(panel17.Height * 0.20);
  chartComments.height := round(panel17.Height * 0.20);

  executionTime.Height := round(panel18.Height * 0.25);
  mostViewsChart.Height := round(panel18.Height * 0.25);
  mostLikeschart.Height:= round(panel18.Height * 0.25);

  OrganicViews.Height := round(panel20.Height * 0.25);
  OrganicLikes.Height := round(panel20.Height * 0.25);
  OrganicComments.Height:= round(panel20.Height * 0.25);

  panel30.Height := round(panel16.Height * 0.50);
  chartStreamViews.Width := panel30.Width div 2;
  dailyLikes.Height := round(panel16.Height * 0.25);

  chartAlbum.Height := round(panel19.Height * 0.33);
  chartHallViews.Height := round(panel19.Height * 0.33);
  panel16.Width := round(panel15.Width * 0.7);
end;

procedure TfrmFlickrMain.FormResize(Sender: TObject);
begin
  //Resize additional components
  ResizeChartsDashBoard();

  panel24.Width := round(panel9.Width / 3);
  panel25.Width := round(panel9.Width / 3);

  panel27.Left := (panel24.Width-panel27.Width) div 2;
  panel28.Left := (panel25.Width-panel28.Width) div 2;
  panel29.Left := (panel26.Width-panel29.Width) div 2;
  //Second page
  panel2.Height := round (tabsheet2.Height * 0.6);
  if (tabsheet2.Height - panel2.Height - splitter1.Height) > 0 then
    pagecontrol2.Height := tabsheet2.Height - panel2.Height - splitter1.Height;
  chartItemViews.Height := round(panel4.Height / 2);
  chartItemLikes.Height := round(panel4.Height / 2);
  chartItemComments.Height := round(panel4.Height / 2);
  panel22.Width := round(panel4.Width / 4);
  panel23.Width := round(panel4.Width / 4);
  panel11.Width := round(panel4.Width / 4);
  totalGroups.Width := round(panel4.Width / 4);
end;

procedure TfrmFlickrMain.FormShow(Sender: TObject);
var
  setup: TfrmSetupApp;
  MyGuid1 : TGUID;
begin
  LoadOptions;
  if (Apikey.Text = '') or (secret.Text = '') or (edtUserId.Text = '')  or (edtWorkspace.Text = '') or (edtAppId.Text = '') then
  begin
    setup := TfrmSetupApp.Create(Application);
    try
      if Apikey.Text <> '' then
        setup.apikey.Text := Apikey.Text;
      if secret.Text <> '' then
        setup.secret.Text := secret.Text;
      if edtUserId.Text <> '' then
        setup.edtUserId.Text := edtUserId.Text;
      if edtWorkspace.Text <> '' then
        setup.edtWorkspace.Text := edtWorkspace.Text;
      if edtAppId.Text <> '' then
        setup.edtAppId.Text := edtAppId.Text
      else
      begin
        CreateGUID(MyGuid1);
        setup.edtAppId.Text := GUIDToString(MyGuid1);
      end;
      setup.ShowModal;
    finally
      setup.Free;
    end;
  end;
end;

function TfrmFlickrMain.isInSeries(id: string): Boolean;
var
  i: Integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < CheckedSeries.Count) do
  begin
    found := CheckedSeries[i] = id;
    inc(i);
  end;
  Result := found;
end;

procedure TfrmFlickrMain.Label12DblClick(Sender: TObject);
var
  Bitmap: TBitMap;
  prefix : string;
begin
  Bitmap := Self.GetFormImage;
  try
    prefix := DateTimeToStr(Date).Replace('\','').Replace('/','').Replace(' ','').Replace(':','');
    Bitmap.SaveToFile( prefix + 'Image.bmp' );
    ShowMessage('Screen capture saved!');
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmFlickrMain.Label2DblClick(Sender: TObject);
begin
  batchUpdate.Enabled := true;
end;

procedure TfrmFlickrMain.listGroupsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  color, Color2: TColor;
begin
  color := Sender.Canvas.Font.color;
  Color2 := Sender.Canvas.Brush.color;
  if Item.Checked then
  begin
    Sender.Canvas.Font.color := clBlue;
    Sender.Canvas.Brush.color := Color2;
  end
  else
  begin
    Sender.Canvas.Font.color := color;
    Sender.Canvas.Brush.color := Color2;
  end;
end;

procedure TfrmFlickrMain.listGroupsItemChecked(Sender: TObject; Item: TListItem);
begin
  UpdateLabelGroups();
end;

procedure TfrmFlickrMain.listPhotosCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  color, Color2: TColor;
begin
  color := Sender.Canvas.Font.color;
  Color2 := Sender.Canvas.Brush.color;
  if SubItem = 1 then
  begin
    if ((Item.SubItems.Strings[1].ToInteger >= 1000) and (Item.SubItems.Strings[1].ToInteger < 3000)) then
    begin
      Sender.Canvas.Font.color := clBlue;
      Sender.Canvas.Brush.color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 3000) and (Item.SubItems.Strings[1].ToInteger < 5000)) then
    begin
      Sender.Canvas.Font.color := clGreen;
      Sender.Canvas.Brush.color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 5000) and (Item.SubItems.Strings[1].ToInteger < 8000)) then
    begin
      Sender.Canvas.Font.color := clOlive;
      Sender.Canvas.Brush.color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 8000) and (Item.SubItems.Strings[1].ToInteger < 10000)) then
    begin
      Sender.Canvas.Font.color := clFuchsia;
      Sender.Canvas.Brush.color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 10000)) then
    begin
      Sender.Canvas.Font.color := clRed;
      Sender.Canvas.Brush.color := Color2;
    end;
    if ((Item.SubItems.Strings[10].ToBoolean)) then
    begin
      Sender.Canvas.Font.color := clNavy;
      Sender.Canvas.Brush.color := Color2;
    end;
  end
  else
  begin
    Sender.Canvas.Font.color := color;
    Sender.Canvas.Brush.color := Color2;
  end;
end;

procedure TfrmFlickrMain.listPhotosItemChecked(Sender: TObject; Item: TListItem);
var
  id, title, views, likes, comments, LastUpdate: string;
  photo: IPhoto;
  stat: IStat;
  i: Integer;
  Series, SeriesViews, SeriesLikes, SeriesComments: TLineSeries;
  Series2 : TAreaSeries;
  SeriesPool : TAreaSeries;
  barSeries: TBarSeries;
  colour: TColor;
  viewsTendency, likesTendency, commentsTendency : Itendency;
  SeriesTendencyViews, SeriesTendencyLikes, SeriesTendencyComments : TLineSeries;
  theDate : TDateTime;
  vTendency : integer;
  poolHistogram : TPoolHistogram;
  histogram :  TList<IItem>;
  acc : integer;
  accDate : TDateTime;
begin
  if (Item.Checked) and (not chkAddItem.Checked) then
  begin
    id := Item.Caption;
    title := Item.SubItems.Strings[0];
    views := Item.SubItems.Strings[1];
    likes := Item.SubItems.Strings[2];
    comments := Item.SubItems.Strings[3];
    LastUpdate := Item.SubItems.Strings[4];

    photo := repository.GetPhoto(id);
    if photo <> nil then
    begin
      photo.LoadStats;
      SeriesViews := flickrChart.GetNewLineSeries(chartItemViews);
      SeriesPool := flickrChart.GetNewAreaSeries(totalGroups);
      SeriesLikes := flickrChart.GetNewLineSeries(chartItemLikes);
      SeriesComments := flickrChart.GetNewLineSeries(chartItemComments);
      SeriesPool.Title := id + 'pool';
      SeriesViews.title := id;
      SeriesLikes.title := id;
      SeriesComments.title := id;
      CheckedSeries.Add(id);
      viewsTendency := TTendency.Create;
      likesTendency := TTendency.Create;
      commentsTendency := TTendency.Create;
      colour := RGB(Random(255), Random(255), Random(255));
      for i := 0 to photo.stats.Count - 1 do
      begin
        stat := photo.stats[i];
        viewsTendency.AddXY(i, stat.views);
        SeriesViews.AddXY(stat.Date, stat.views, '', colour);

        likesTendency.AddXY(i, stat.likes);
        SeriesLikes.AddXY(stat.Date, stat.likes, '', colour);

        commentsTendency.AddXY(i, stat.Comments);
        SeriesComments.AddXY(stat.Date, stat.Comments, '', colour);
      end;

      photo.LoadGroups;
      poolHistogram := TPoolHistogram.Create(photo.Groups);
      colour := RGB(Random(255), Random(255), Random(255));
      histogram := poolHistogram.Histogram;
      acc := 0;
      for i := 0 to histogram.Count-1 do
      begin
        acc := acc + histogram[i].count;
        accDate := histogram[i].date;
        SeriesPool.AddXY(accDate, acc, '', colour);
      end;
      histogram.Free;
      poolHistogram.Free;

      chartItemViews.AddSeries(SeriesViews);
      totalGroups.AddSeries(SeriesPool);
      chartItemLikes.AddSeries(SeriesLikes);
      chartItemComments.AddSeries(SeriesComments);
      viewsTendency.Calculate;
      likesTendency.Calculate;
      commentsTendency.Calculate;

      SeriesTendencyViews := flickrChart.GetNewLineSeries(chartItemViews);
      SeriesTendencyViews.title := id + 'tendency';

      //Adding only first and last item
      theDate := photo.stats[0].Date;
      vTendency := viewsTendency.tendencyResult(0);
      SeriesTendencyViews.AddXY(theDate, vTendency, '', FYellow);
      theDate := photo.stats[photo.stats.Count - 1].Date;
      vTendency := viewsTendency.tendencyResult(photo.stats.Count - 1);
      SeriesTendencyViews.AddXY(theDate, vTendency, '', FYellow);

      chartItemViews.AddSeries(SeriesTendencyViews);

      SeriesTendencyLikes := flickrChart.GetNewLineSeries(chartItemLikes);
      SeriesTendencyLikes.title := id + 'tendency';

      //Adding only first and last item
      theDate := photo.stats[0].Date;
      vTendency := likesTendency.tendencyResult(0);
      SeriesTendencyLikes.AddXY(theDate, vTendency, '', FYellow);
      theDate := photo.stats[photo.stats.Count - 1].Date;
      vTendency := likesTendency.tendencyResult(photo.stats.Count - 1);
      SeriesTendencyLikes.AddXY(theDate, vTendency, '', FYellow);

      chartItemLikes.AddSeries(SeriesTendencyLikes);

      SeriesTendencyComments := flickrChart.GetNewLineSeries(chartItemComments);
      SeriesTendencyComments.title := id + 'tendency';

      //Adding only first and last item
      theDate := photo.stats[0].Date;
      vTendency := commentsTendency.tendencyResult(0);
      SeriesTendencyComments.AddXY(theDate, vTendency, '', FYellow);
      theDate := photo.stats[photo.stats.Count - 1].Date;
      vTendency := commentsTendency.tendencyResult(photo.stats.Count - 1);
      SeriesTendencyComments.AddXY(theDate, vTendency, '', FYellow);

      chartItemComments.AddSeries(SeriesTendencyComments);

      UpdateSingleStats(id);
      photo.Stats.Clear;
      photo.Groups.Clear;
    end;
  end
  else
  begin
    id := Item.Caption;
    if isInSeries(id) then
    begin
      Series := nil;
      for i := 0 to chartItemViews.SeriesList.Count - 1 do
      begin
        if chartItemViews.SeriesList[i].title = id then
        begin
          Series := TLineSeries(chartItemViews.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemViews.RemoveSeries(Series);

      Series := nil;
      for i := 0 to chartItemLikes.SeriesList.Count - 1 do
      begin
        if chartItemLikes.SeriesList[i].title = id then
        begin
          Series := TLineSeries(chartItemLikes.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemLikes.RemoveSeries(Series);

      Series2 := nil;
      for i := 0 to totalGroups.SeriesList.Count - 1 do
      begin
        if totalGroups.SeriesList[i].title = id + 'pool' then
        begin
          Series2 := TAreaSeries(totalGroups.SeriesList[i]);
          Break;
        end;
      end;
      if Series2 <> nil then
        totalGroups.RemoveSeries(Series2);

      Series := nil;
      for i := 0 to chartItemComments.SeriesList.Count - 1 do
      begin
        if chartItemComments.SeriesList[i].title = id then
        begin
          Series := TLineSeries(chartItemComments.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemComments.RemoveSeries(Series);

      Series := nil;
      for i := 0 to chartItemViews.SeriesList.Count - 1 do
      begin
        if chartItemViews.SeriesList[i].title = id + 'tendency' then
        begin
          Series := TLineSeries(chartItemViews.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemViews.RemoveSeries(Series);

      Series := nil;
      for i := 0 to chartItemLikes.SeriesList.Count - 1 do
      begin
        if chartItemLikes.SeriesList[i].title = id + 'tendency' then
        begin
          Series := TLineSeries(chartItemLikes.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemLikes.RemoveSeries(Series);

      Series := nil;
      for i := 0 to chartItemComments.SeriesList.Count - 1 do
      begin
        if chartItemComments.SeriesList[i].title = id + 'tendency' then
        begin
          Series := TLineSeries(chartItemComments.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        chartItemComments.RemoveSeries(Series);

      barSeries := nil;
      for i := 0 to chartItemViewsH.SeriesList.Count - 1 do
      begin
        if chartItemViewsH.SeriesList[i].title = id then
        begin
          barSeries := TBarSeries(chartItemViewsH.SeriesList[i]);
          Break;
        end;
      end;
      if barSeries <> nil then
        chartItemViewsH.RemoveSeries(barSeries);

      barSeries := nil;
      for i := 0 to chartItemLikesH.SeriesList.Count - 1 do
      begin
        if chartItemLikesH.SeriesList[i].title = id then
        begin
          barSeries := TBarSeries(chartItemLikesH.SeriesList[i]);
          Break;
        end;
      end;
      if barSeries <> nil then
        chartItemLikesH.RemoveSeries(barSeries);

      barSeries := nil;
      for i := 0 to chartItemCommentsH.SeriesList.Count - 1 do
      begin
        if chartItemCommentsH.SeriesList[i].title = id then
        begin
          barSeries := TBarSeries(chartItemCommentsH.SeriesList[i]);
          Break;
        end;
      end;
      if barSeries <> nil then
        chartItemCommentsH.RemoveSeries(barSeries);
    end;
  end;

  UpdateLabel();
end;

procedure TfrmFlickrMain.listPhotosUserItemChecked(Sender: TObject; Item: TListItem);
begin
  UpdateLabelPhotos();
end;

procedure TfrmFlickrMain.listValuesLikesAlbumsChange(Sender: TObject);
begin
  if not assigned(options) then exit;
  if TUtils.StringListToString(options.AlbumLikes) <> TUtils.StringListToString(TStringList(listValuesLikesAlbums.Lines)) then
  begin
    FDirtyOptions := true;
    Log('listValuesLikesAlbums has changed');
  end;
end;

procedure TfrmFlickrMain.listValuesLikesAlbumsIDChange(Sender: TObject);
begin
  if not assigned(options) then exit;
  if TUtils.StringListToString(options.AlbumLikesID) <> TUtils.StringListToString(TStringList(listValuesLikesAlbumsID.Lines)) then
  begin
    FDirtyOptions := true;
    Log('listValuesLikesAlbumsID has changed');
  end;
end;

procedure TfrmFlickrMain.listValuesViewsAlbumsChange(Sender: TObject);
begin
  if not assigned(options) then exit;
  if TUtils.StringListToString(options.AlbumViews) <> TUtils.StringListToString(TStringList(listValuesViewsAlbums.Lines)) then
  begin
    FDirtyOptions := true;
    Log('listValuesViewsAlbums has changed');
  end;
end;

procedure TfrmFlickrMain.listValuesViewsAlbumsIDChange(Sender: TObject);
begin
  if not assigned(options) then exit;
  if TUtils.StringListToString(options.AlbumViewsID) <> TUtils.StringListToString(TStringList(listValuesViewsAlbumsID.Lines)) then
  begin
    FDirtyOptions := true;
    Log('listValuesViewsAlbumsID has changed');
  end;
end;

procedure TfrmFlickrMain.UpdateLabel();
var
  i : integer;
  count : integer;
begin
  count := 0;
  for i := 0 to listphotos.Items.Count-1 do
  begin
    if listphotos.Items[i].Checked then
      inc(count);
  end;
  Label31.Caption := 'Number of items: ' + InttoStr(listphotos.Items.Count) + ' (' + count.ToString + ') selected';
end;

procedure TfrmFlickrMain.UpdateLabelGroups();
var
  i: Integer;
  count : integer;
begin
  count := 0;
  for i := 0 to listgroups.Items.Count-1 do
  begin
    if listgroups.Items[i].Checked then
      inc(count);
  end;
  Label11.Caption := 'Number of items: ' + InttoStr(listgroups.Items.Count) + ' (' + count.ToString + ') selected';
end;

procedure TfrmFlickrMain.UpdateLabelPhotos();
var
  i: Integer;
  count : integer;
begin
  count := 0;
  for i := 0 to listPhotosUser.Items.Count-1 do
  begin
    if listPhotosUser.Items[i].Checked then
      inc(count);
  end;
  Label34.Caption := 'Number of items: ' + InttoStr(listPhotosUser.Items.Count) + ' (' + count.ToString + ') selected';
end;

end.
