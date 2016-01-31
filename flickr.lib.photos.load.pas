unit flickr.lib.photos.load;

interface

uses
  Generics.collections, XMLIntf,
  flickr.rest, System.SysUtils, flickr.lib.options.agent;

type
  TPhotoLoader = class(TObject)
    class function load(optionsAgent : IOptionsAgent): TList<String>;
  end;

implementation

uses
  flickr.http.lib;

{ TPhotoLoader }

class function TPhotoLoader.load(optionsAgent : IOptionsAgent): TList<String>;
var
  iXMLRootNode4: IXMLNode;
  pages, title, id, ispublic, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  PhotoList: TList<String>;
begin
  PhotoList := TList<string>.Create();
  THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotos('1', '500'), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['pages'];
      total := iXMLRootNode.attributes['total'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photo>
      numTotal := total.ToInteger();
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ispublic := iXMLRootNode4.attributes['ispublic'];
          title := iXMLRootNode4.attributes['title'];
          if ispublic = '1' then
          begin
            PhotoList.Add(id);
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

  // Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numPages do
  begin
    THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotos(i.ToString, '500'), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['pages'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photo>
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ispublic := iXMLRootNode4.attributes['ispublic'];
          title := iXMLRootNode4.attributes['title'];
          if ispublic = '1' then
          begin
            PhotoList.Add(id);
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
  end;

  result := PhotoList;
end;

end.
