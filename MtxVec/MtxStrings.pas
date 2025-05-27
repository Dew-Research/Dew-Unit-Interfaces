











(*<summary>TStrings and TStringList classes.</summary>
  
<remarks>Note:
    Code taken from CodeGear RAD.
</remarks>
*)
unit MtxStrings;

interface

{$I BdsppDefs.inc}

uses System.IO, System.Text, Math387, System.Globalization, System.Collections, System.ComponentModel
     ,System.Diagnostics
     
     ,Classes
     
     
     ;

const sdDelimiter = 1; 
const sdQuoteChar = 2;
const sdNameValueSeparator = 4;
const sdLineBreak = 8;
const sdStrictDelimiter = 16;

type

 TStringsDefined = cardinal; 

  TStrings = class;

  TStringsEnumerator = class
  strict private
    FIndex: Integer;
    FStrings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    function GetCurrent: string;
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;

  TNameItem = class
  strict private
    fOwner: TStrings;
    function GetItem(Index: Integer): string;
  public
    property Items[Index: Integer]: string read GetItem; default;
    constructor Create(aOwner: TStrings);
  end;

  TValueFromIndexItem = class
  strict private
    fOwner: TStrings;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
  public
    property Items[Index: Integer]: string read GetItem write SetItem; default;
    constructor Create(aOwner: TStrings);
  end;

  TValueItem = class
  strict private
    fOwner: TStrings;
    function GetItem(const Name: string): string;
    procedure SetItem(const Name, Value: string);
  public
    property Items[const Name: string]: string read GetItem write SetItem; default;
    constructor Create(aOwner: TStrings);
  end;

  TObjectItem = class
  strict private
    fOwner: TStrings;
    function GetItem(Index: Integer): TObject;
    procedure PutItem(Index: Integer; const Value: TObject);
  public
     property Items[Index: Integer]: TObject read GetItem write PutItem; default;
    constructor Create(aOwner: TStrings);
  end;

  [DebuggerDisplay('{DelimitedText, nq}')]
  TStrings = class(TPersistent)
  strict private
    FDefined: TStringsDefined;
    FDelimiter: Char;
    FLineBreak: string;
    FQuoteChar: Char;
    FNameValueSeparator: Char;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FTrailingLineBreak: boolean;
    function GetCommaText: string;
    function GetDelimitedText: string;
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetLineBreak: string;
    procedure SetLineBreak(const Value: string);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const Value: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
  strict protected
    procedure SetTrailingLineBreak(const Value: boolean);
    procedure Error(const Msg: string; Data: Integer); overload;
    function ExtractName(const S: string): string;
    function GetString(Index: Integer): string; virtual;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    function GetTextStr: string; virtual;
    procedure PutString(Index: Integer; const S: string); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    Names: TNameItem;
    ValueFromIndex: TValueFromIndexItem;
    Values: TValueItem;
    Objects: TObjectItem;


    function GetName(Index: Integer): string;

    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);

    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;

    function GetObject(Index: Integer): TObject; virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;

    function CompareStrings(const S1, S2: string): Integer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); virtual;
    procedure Assign(Source: TPersistent); virtual;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumerator;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); virtual;

    procedure LoadFromFile(const FileName: string); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: System.Text.Encoding); overload; virtual;
    procedure LoadFromStream(Stream: Stream); overload; virtual;
    procedure LoadFromStream(Stream: Stream; Encoding: System.Text.Encoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: System.Text.Encoding); overload; virtual;
    procedure SaveToStream(Stream: Stream); overload; virtual;
    procedure SaveToStream(Stream: Stream; Encoding: System.Text.Encoding); overload; virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property TrailingLineBreak: boolean read FTrailingLineBreak write SetTrailingLineBreak;


    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;


    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: string read GetString write PutString; default;
    property Text: string read GetTextStr write SetTextStr;
  end;



  TStringList = class;

  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  TStringItemArray = array of TStringItem;

  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

  TDuplicates = (dupIgnore, dupAccept, dupError);

  TStringList = class(TStrings)
  strict private
    FList: TStringItemArray;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TMtxNotifyEvent;
    FOnChanging: TMtxNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  strict protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function GetString(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    procedure PutString(Index: Integer; const S: string); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
    procedure SetOnChange(const aValue: TMtxNotifyEvent);
    procedure SetOnChanging(const aValue: TMtxNotifyEvent);
  public
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
  public
    function CompareStrings(const S1, S2: string): Integer; override;
    function Add(const S: string): Integer; overload; override;
    function Add(const C: char): Integer;  reintroduce;  overload;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    constructor Create; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TMtxNotifyEvent read FOnChange write SetOnChange;
    property OnChanging: TMtxNotifyEvent read FOnChanging write SetOnChanging;
  end;


  TListSortCompare = function (Item1, Item2: System.Object): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);

  TList = class;

  TListEnumerator = class
  strict private
    FIndex: Integer;
    FList: TList;
  public
    constructor Create(aList: TList);
    function GetCurrent: System.Object;
    function MoveNext: Boolean;
    property Current: System.Object read GetCurrent;
  end;

  TList = class(TObject)
  strict private
    FList: ArrayList;
  strict protected
    function GetItem(Index: Integer): System.Object;
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure Grow; virtual;
    procedure SetItem(Index: Integer; Item: System.Object);
    procedure Notify(Instance: System.Object; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function getFirst: System.Object;
    function getLast: System.Object;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: System.Object): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);

    class procedure Error(const Msg: string; Data: Integer); overload; 
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(Item: System.Object): System.Object;
    function GetEnumerator: TListEnumerator;
    function IndexOf(Item: System.Object): Integer;
    procedure Insert(Index: Integer; Item: System.Object);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: System.Object): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TList; AOperator: TListAssignOp = laCopy; ListB: TList = nil);
    
    property First: System.Object read getFirst;
    
    property Last: System.Object read getLast;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: System.Object read GetItem write SetItem; default;
    property List: ArrayList read FList;
  end;



  TObjectList = class(TList)
  strict private
    FOwnsObjects: Boolean;
  strict protected
    procedure Notify(Instance: System.Object; Action: TListNotification); override;
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;


    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  procedure aSetLength(var a: TStringItemArray; Len: integer); overload;
  function aGetLength(const a: TStringItemArray): integer; overload;

