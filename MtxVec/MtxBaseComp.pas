










(*<summary>Defines the common ancestor of all components using MtxVec.</summary>*)
unit MtxBaseComp;


interface

{$I BdsppDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}

Uses Math387, MtxVecBase, MtxVecUtils
     

     

     
     ,Classes
     ,SysUtils
     
     

     
       
       ,Contnrs
       
     
     ;

Type

       (*<summary>Exception type raised when operation is not supported.</summary>
          
<remarks>Exception meant to be be raised, when an operation is not supported
          by the component.
</remarks>
*)
       
       ENoOperation = class(Exception)
       
       constructor Create;
       
       end;

       
       TMtxComponent = class;
       TReferenceList = class;
       

       

       TReferenceListItem = class
       strict private
          
          Owner: TObject;
       public
          Item: TReferenceList;
          Event: TNotifyEvent;
          Component: TObject;
          destructor Destroy; override;
          constructor Create(aOwner: TObject);
       end;

       TReferenceList = class
       strict private
          List: TObjectsList;
          function GetReferenceListItem(aIndex: integer): TReferenceListItem;
          procedure SetReferenceListItem(aIndex: integer; Item: TReferenceListItem);
       strict protected
          
          Owner: TObject;
          FNilEvent: TNotifyEvent;
          procedure Delete(i: integer);
          function IndexOf(Src: TReferenceList): integer;
       public
          
          procedure NotifyAll;
          procedure RemoveWithoutNotify(Src: TReferenceList); overload;
          procedure RemoveWithoutNotify(Src: TMtxComponent); overload;
          procedure Remove(Src: TReferenceList); overload;
          procedure Remove(Src: TMtxComponent); overload;
          procedure UpdateListReferences;
          function get_Count: integer;          
          

          (*<summary>Add a reference of Src to Self and also a reference of Self to Src.</summary>
            
<remarks>Event that will be called, when the connected component
            is destroyed is the method TMtxComponent.RemoveReference. The parameter
            passed to the method will be the pointer of the component (and not of its TReferenceList)
            that is being destroyed.
</remarks>
*)
          procedure Add(Src: TMtxComponent); overload;
          (*<summary>Add a reference to Src to Self and also a reference of Self to Src.</summary>
            
<remarks>Event is the procedure to be called, when the connected component
            is destroyed.
</remarks>
*)
          procedure Add(Src: TReferenceList; Event: TNotifyEvent; CompSrc: TObject = nil); overload;
          destructor Destroy; override;
          constructor Create(aOwner: TObject); virtual;


          property Items[aIndex: integer]: TReferenceListItem read GetReferenceListItem write SetReferenceListItem; default;
          property Count: integer read get_Count;
       end;

  (*<summary>Common ancestor of all components using MtxVec.</summary>
    
<remarks>Most Delphi/C++Builder developers are familiar with the following methods:
    * SaveToStream
    * LoadFromStream
    * LoadFromFile
    * SaveToFile
    * Assign

    TMtxComponent declares generic versions of these
    routines so that all components derived from it have all their
    published properties streamed, without the need to make
    any changes to the streaming routines. Therefore, all
    components derived form TMtxComponent have the capability
    to store their states (properties) to the stream,
    file or to assign from another object of the same type.
    The default component streaming mechanism has a bug when
    it comes to streaming properties with a declared
    default value. This has been fixed for TMtxComponent.

    The component also declares a more advanced set of streaming methods:
    * LoadTemplateFromStream
    * SaveTemplateToStream
    * LoadTemplateFromFile
    * SaveTemplateToFile
    * AssignTemplate

    These methods set a special protected property named BlockAssign to True. This is very useful
    when there is a need to save only "parameters" and not the "data" of the component. The
    parameters will remain the same, while the "data" will be different next time
    and there is no point in wasting disk space by saving it.

    The BlockAssign property can be used in two ways. It can be used as a storage specifier for
    a published property:

    <c>property Data: TVec read fData write SetData stored AllowStreaming;</c>

    In this case when the BlockAssign property is True the AllowStreaming protected function will return false.
    The property will not be saved and consequently when the stream is loaded this property will not
    be set. This will affect only the SaveTemplateToStream/SaveTemplateToFile/Assign methods.

    Another approach is to allow storing the property to the stream and then explicitely check the value
    BlockAssign property to prevent assignment of the value. The first approach is much more
    elegant because it reduces the size of the "template" file.

    The value of the protected BlockAssign property is set automatically by the component.
    All the user is required to do is use the storage specifiers (the "stored" keyword with
    AllowStreaming function) for "template like" properties.
</remarks>
*)
  
  
    TMtxComponent = class(TComponent)
    strict private
      FReference: TReferenceList;
      FEditorActive: boolean;
      FBlockAssign: boolean;
      
      procedure SetEditorActive(const Value: boolean);
    strict protected
      (*<summary>Returns True, if the component is used at design time within IDE.</summary>*)
      function IsDesignTime: boolean;
      function IsLoading: boolean;
      (*<summary>Setter for the BlockAssign property.</summary>*)
      procedure SetBlockAssign(const Value: boolean); virtual;
      (*<summary>Getter for the BlockAssign property.</summary>*)
      function  GetBlockAssign: boolean; virtual;
      
    
    protected
      
      procedure DefineProperties(Filer: TFiler); override;
      
    

    strict protected
      (*<summary>Generic method to be overriden in descendants to request recalculation of the data.</summary>*)
      function Update: boolean; virtual; 
      
      (*<summary>Generic method to be overriden for streaming custom data with the component.</summary>*)
      procedure LoadBinaryStreams(Src: TStream); overload; virtual;
      (*<summary>Generic method to be overriden for streaming custom data with the component.</summary>*)
      procedure SaveBinaryStreams(Dst: TStream); overload; virtual;
      
    
    protected
    
      (*<summary>Block streaming of specific properties when storing only a "template".</summary>
         
<remarks>If true, AllowStreaming will be returning false. See the description of this component for more info.
</remarks>
*)
      property BlockAssign: boolean read GetBlockAssign write SetBlockAssign;
      (*<summary>To be overriden in descendanr classses.</summary>
        
<remarks>Should be overriden in descendant classes to return the name of the TForm descendant, dialog
        used to edit the properties of the component. This property has meaning only if used in
        conjunction with TMtxDialog.
</remarks>
*)
      classfunction  EditorClass: string; virtual;
      
      procedure ReferenceRemoved(Sender: TObject); virtual;
      
      
      
      (*<summary>Save the component to Dst stream.</summary>*)
      procedure SaveToStream(Dst: TStream); overload; virtual;
      (*<summary>Load the component from Src stream.</summary>*)
      procedure LoadFromStream(Src: TStream); overload; virtual;
      
    strict protected
      (*<summary>Save the component to file.</summary>*)
      procedure SaveToFile(FileName: string); virtual;
      (*<summary>Load the component from file.</summary>*)
      procedure LoadFromFile(FileName: string); virtual;
    public
      
      constructor Create(aOwner: TComponent); overload; override;
      destructor Destroy; override;
      

      (*<summary>Reset is called after loading the data from stream or file. </summary>
        
<remarks>Override this procedure to implement component specific processing.
</remarks>
*)
      procedure Reset; virtual;

      (*<summary>Returns the negatated value of BlockAssign.</summary>
         
<remarks>Should be used as a storage specifier. See the description of this component for more info.
</remarks>
*)
      function AllowStreaming: boolean; virtual;
      (*<summary>Assign values of all published properties from Source.</summary>
         
<remarks>Assign will raise an exception, if the types of the source
         and destination will not be an exact match.
</remarks>
*)
      procedure Assign(Source: TPersistent);  override; 
      (*<summary>Assign values of "template-like" published properties from Source.</summary>
         
<remarks>Assign will raise an exception, if the types of the source
         and destination will not be an exact match.
         See the description of this component for more info.
</remarks>
*)
      procedure AssignTemplate(Source: TPersistent); virtual;

      

      
      (*<summary>Load a template from stream.</summary>
         
<remarks>See the description of this component for more info.
</remarks>
*)
      procedure LoadTemplateFromStream(Stream: TStream); overload; virtual;
      (*<summary>Save template to stream.</summary>
         
<remarks>See the description of this component for more info.
</remarks>
*)
      procedure SaveTemplateToStream(Stream: TStream); overload; virtual;
      (*<summary>Copies object to an array of bytes.</summary>*)
      
      


      (*<summary>Load a template from file.</summary>
         
<remarks>See the description of this component for more info.
</remarks>
*)
      procedure LoadTemplateFromFile(FileName: string); virtual;
      (*<summary>Save template to file.</summary>
         
<remarks>See the description of this component for more info.
</remarks>
*)
      procedure SaveTemplateToFile(FileName: string); virtual;

      (*<summary>Returns True, if the component editor is displayed.</summary>*)
      property EditorActive: boolean read FEditorActive write SetEditorActive;
      (*<summary>Stores a list of components that have to be notified, when this component is destroyed.</summary>*)
      property Reference: TReferenceList read FReference;
      
    end;

    TMtxComponentList = class;
    (*<summary>Event for TMtxComponentList.</summary>
      
<remarks>Event triggered after a new item has been added to the
      component list.
</remarks>
*)
    TMtxComponentAddEvent = procedure (Sender: TMtxComponentList; Item: TMtxComponent) of object;
    (*<summary>Event for TMtxComponentList.</summary>
      
<remarks>Event triggered before the Item has been deleted from the
      component list.
</remarks>
*)
    TMtxComponentDeleteEvent = procedure (Sender: TMtxComponentList; Item: TMtxComponent) of object;


    (*<summary>Manages a list of TMtxComponents.</summary>
      
<remarks>Manages a list of TMtxComponent. The class can be quickly customized to manage a list
      of components derived from TMtxComponent. The components in the list are owned by
      the object and are created and destroyed automatically when setting the Count property.
</remarks>
*)
    TMtxComponentList = class(TMtxComponent)
    strict private
      FOnItemAdd: TMtxComponentAddEvent;
      FOnItemDelete: TMtxComponentDeleteEvent;
      FPropagateTemplate: boolean;
      FManaged: boolean;
      function GetCount: integer;
      procedure SetOnItemAdd(const Value: TMtxComponentAddEvent);
      procedure SetOnItemDelete(const Value: TMtxComponentDeleteEvent);
      procedure SetPropagateTemplate(const Value: boolean);
      procedure SetManaged(const Value: boolean);
    strict protected
      aList: TObjectsList;
      procedure SetCount(const Value: integer); virtual;
      procedure SetBlockAssign(const Value: boolean); override;
      procedure Add(MtxComponent: TMtxComponent); virtual;
      (*<summary>Called every time the Count property has changed or an item has been added or removed.</summary>*)
      function  AddItem: TMtxComponent; virtual;
      function getFirst: TMtxComponent;
      function getLast: TMtxComponent;

     
     (*<summary><remarks>
      Generic method to be overriden for streaming custom data with the component.
      Read data from the Stream that has been previously written with (overriden) <see cre="SaveBinaryStream"/>.
      </remarks></summary>*)
      procedure LoadBinaryStreams(Src: TStream); override;
     (*<summary><remarks>
      Generic method to be overriden for streaming custom data with the component.
      Write data to be stored with the component to the Stream. Read the data by overriding <see cref="LoadBinaryStream"/>.
      </remarks></summary>*)
      procedure SaveBinaryStreams(Dst: TStream); override;
      
     
      function GetItems(Index: integer): TMtxComponent;
      procedure SetItems(Index: integer; const Value: TMtxComponent);
     
    public
      
      property Managed: boolean read FManaged write SetManaged;

      (*<summary>Returns the pointer to Items[0].</summary>*)
      property First: TMtxComponent read getFirst;

      (*<summary>Returns Items[Count - 1].</summary>
        
<remarks>Call Last to retrieve the last pointer in the Items array.
</remarks>
*)
      property Last: TMtxComponent read getLast;

      (*<summary>Swaps the position of two items in the Items array.</summary>
        
<remarks>Call Exchange to swap the positions of the items at positions Index1
        and Index2 of the Items array. The indexes are zero-based.
</remarks>
*)
      procedure Exchange(Index1, Index2: Integer);

      (*<summary>Deletes the first reference to the Item parameter from the Items array.</summary>
        
<remarks>Call Remove to remove a specific item from the Items array when its index is unknown.
</remarks>
*)
      function Remove(Item: TMtxComponent): Integer;

      (*<summary>Changes the position of an item in the Items array.</summary>
        
<remarks>Call Move to move the item at the position CurIndex so
        that it occupies the position NewIndex. Indexes are zero based.
</remarks>
*)
      procedure Move(CurIndex, NewIndex: Integer);

      (*<summary>Deletes all items from the list.</summary>
        
<remarks>Clear also frees the memory used to store the Items array.
        All list items are destroyed.
</remarks>
*)
      procedure Clear;
      (*<summary>Deletes the item from the list.</summary>
        
<remarks>Deletes the item at position Index from the List and frees the object.
</remarks>
*)
      procedure Delete(Index: integer);

      (*<summary>Extracts the item from the list.</summary>
        
<remarks>Deletes the item at position Index from the List, but does not free the object.
</remarks>
*)
      function Extract(Index: integer): TMtxComponent;

      (*<summary>Inserts an item at position Index</summary>*)
      procedure Insert(Index: integer);
      (*<summary>Rotate the list up or down.</summary>
        
<remarks>Very fast way to rotate the items in the list. If the Step is positive
        the items are shifted up and those that exit at the top are inserted
        at the begining of the List. If the Step is negative the items are shifted down and inserted at the end.
</remarks>
*)
      procedure Rotate(Offset: integer);
      (*<summary> Default property to access elements from the stored list. </summary>*)
      property Items[Index: integer]: TMtxComponent read GetItems write SetItems; default;

     
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
     

      (*<summary><remarks>
        Assign all property values of the defined item to the items
        specified with their indexes in the DstIndex array.
        </remarks></summary>*)
      procedure Propagate(SrcIndex: integer; const DstIndex: array of integer); overload;
    published
     
      (*<summary>Defines how <see cref="Propagate"/> will proparage properties.</summary>
      
<remarks>If true, the Propagate routine, will propagate only the "template" of the
      selected source and not all the published properties of the source.
</remarks>
*)
      property PropagateTemplate: boolean read FPropagateTemplate write SetPropagateTemplate;

     
      (*<summary>Defines the size of the list.</summary>
        
<remarks>Setting the property creates or destroys appropriate number of list items.
        In derived components the AddItem function should be overriden to
        return the custom type object.
</remarks>
*)
      property Count: integer read GetCount write SetCount;

      
      (*<summary>Event triggered after a new item has been added.</summary>*)
      property OnItemAdd: TMtxComponentAddEvent read FOnItemAdd write SetOnItemAdd;

      
      (*<summary>Event triggered before the Item will be deleted.</summary>*)
      property OnItemDelete: TMtxComponentDeleteEvent read FOnItemDelete write SetOnItemDelete;
    end;

      (*<summary>Defines a collectionItem, which allows customevent defintion.</summary>*)
     
     TMtxCollectionItem = class(TCollectionItem)
     public
        procedure CustomEvent(Sender: TObject); virtual;
        procedure UpdateReference; virtual;
     end;
     


     

     TMtxCollection = class(TCollection)
     strict private
        FBlockAssign: boolean;
        function GetCount: integer;
        procedure SetCount(const Value: integer);
        procedure SetBlockAssign(const Value: boolean); virtual;
     
     protected
     
        property BlockAssign: boolean read FBlockAssign write SetBlockAssign;
     public
        property Count: integer read GetCount write SetCount;
     end;

     



  (*<summary>Assign a component to a component.</summary>
    
<remarks>Generic component assign. The source and destination
    must be of the same type. The routine will copy
    values of all published properties from source
    to destination component.
</remarks>
*)
  
  procedure AssignComponent(Source, Destination: TMtxComponent);

  
  (*<summary>Reset the values of published properties.</summary>
    
<remarks>Resets the values of all published properties to their default values. (if defined).
</remarks>
*)
  
  procedure ResetProperties(Instance: TPersistent);
  



