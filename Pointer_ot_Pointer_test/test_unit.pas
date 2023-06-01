unit test_unit;

{$mode ObjFPC}{$H+}

interface

uses
ctypes,  Classes, SysUtils;

type
   PFL_FORM = ^TFL_FORM;
   PFL_OBJECT = ^TFL_OBJECT;

  TFL_OBJECT = record
      form : PFL_FORM;
      prev : PFL_OBJECT;
      next : PFL_OBJECT;
      parent : PFL_OBJECT;
      child : PFL_OBJECT;
      nc : PFL_OBJECT;
      want_update : cint;
    end;
type

  TFL_FORMCALLBACKPTR = procedure (para1:PFL_OBJECT; para2:pointer);cdecl;

  TFL_FORM = record
      first : PFL_OBJECT;
      last : PFL_OBJECT;
      focusobj : PFL_OBJECT;
      parent : PFL_FORM;
      child : PFL_FORM;
      parent_obj : PFL_OBJECT;
      in_redraw : cint;
    end;




implementation

end.

