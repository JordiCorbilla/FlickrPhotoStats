unit flickr.photo.trend.info;

interface

uses
  Generics.Collections, Generics.defaults;

type
  IPhotoTrend = interface
    function getId(): string;
    function getTitle(): string;
    function getValue(): integer;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetValue(value: integer);
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property Value: integer read getValue write SetValue;
  end;

  TPhotoTrend = class(TInterfacedObject, IPhotoTrend)
  private
    FId : string;
    FTitle : string;
    FValue : integer;
    function getId(): string;
    function getTitle(): string;
    function getValue(): integer;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetValue(value: integer);
  public
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property Value: integer read getValue write SetValue;
  end;

  TIPhotoTrendComparer = class(TComparer<IPhotoTrend>)
  public
    function Compare(const Left, Right: IPhotoTrend): Integer; override;
  end;

implementation

{ TPhotoTrend }

function TPhotoTrend.getId: string;
begin
  result := FId;
end;

function TPhotoTrend.getTitle: string;
begin
  result := FTitle;
end;

function TPhotoTrend.getValue: integer;
begin
  result := FValue;
end;

procedure TPhotoTrend.SetId(value: string);
begin
  FId := value;
end;

procedure TPhotoTrend.SetTitle(value: string);
begin
  FTitle := value;
end;

procedure TPhotoTrend.SetValue(value: integer);
begin
  FValue := value;
end;

{ TIPhotoTrendComparerViews }

function TIPhotoTrendComparer.Compare(const Left, Right: IPhotoTrend): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.Value;
  RightTerm := Right.Value;
  Result := RightTerm - LeftTerm;
end;

end.
