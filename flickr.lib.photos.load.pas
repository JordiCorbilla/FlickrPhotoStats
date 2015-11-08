unit flickr.lib.photos.load;

interface

uses
  Generics.collections, IdHTTP, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, ActiveX, IdGlobal,
  flickr.rest, System.SysUtils;

type
  TPhotoLoader = class(TObject)
    class function load(apikey, userId: string): TList<String>;
  end;

implementation

{ TPhotoLoader }

class function TPhotoLoader.load(apikey, userId: string): TList<String>;
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, title, id, ispublic, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  xmlDocument: IXMLDocument;
  timedout: Boolean;
  PhotoList: TList<String>;
begin
  IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdIOHandler.ReadTimeout := IdTimeoutInfinite;
  IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
  xmlDocument := TXMLDocument.Create(nil);
  IdHTTP := TIdHTTP.Create(nil);
  PhotoList := TList<string>.Create();
  try
    IdHTTP.IOHandler := IdIOHandler;
    timedout := false;
    while (not timedout) do
    begin
      try
        response := IdHTTP.Get(TFlickrRest.New().getPhotos(apikey, userId, '1', '500'));
        timedout := true;
      except
        on e: exception do
        begin
          sleep(200);
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
          response := IdHTTP.Get(TFlickrRest.New().getPhotos(apikey, userId, i.ToString, '500'));
          timedout := true;
        except
          on e: exception do
          begin
            sleep(200);
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
            PhotoList.Add(id);
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;
  end;
  result := PhotoList;
end;

end.
