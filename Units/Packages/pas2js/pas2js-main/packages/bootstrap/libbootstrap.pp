{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    Bootstrap 4 mappings for pas2js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit libbootstrap;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, web, libjquery;

Type
  TBootstrapVersion = (bv4,bv5);
  { TBootstrap }

  TBootstrapModalOptions = Class external name 'Object' (TJSObject)
    show : boolean;
    focus : boolean;
    keyboard : boolean;
    backdrop : boolean;
  end;

  TBootstrapToastOptions = Class external name 'Object' (TJSObject)
    animation : boolean;
    autohide : boolean;
    delay : NativeInt;
  end;

  TSanitizeFunction = reference to function(arg: jsValue): jsvalue;

  TBootstrapTooltipOptions = Class external name 'Object' (TJSObject)
    animation : boolean;
    container : jsValue;
    delay : jsValue;
    html : boolean;
    placement : jsValue;
    selector : jsValue;
    template : string;
    title : jsValue;
    trigger: string;
    offset : jsvalue;
    fallbackPlacement : JSValue;
    boundary : JSValue;
    sanitize : boolean;
    whiteList: TJSObject;
    sanitizeFn : TSanitizeFunction;
  end;

  TBootstrapCarouselOptions = Class external name 'Object' (TJSObject)
    interval : nativeint;
    keyboard : boolean;
    pause : boolean;
    pauseStr: string; external name 'pause';
    ride: string;
    wrap : boolean;
    touch : boolean;
  end;

  TBootstrapDropDownOptions = Class external name 'Object' (TJSObject)
    offset : JSValue;
    flip : boolean;
    boundary : string;
    boundaryEl : TJSHTMLElement; external name 'boundary';
    reference : string;
    referenceEl : TJSHTMLElement; external name 'reference';
    display : string;
  end;

  TBootstrapPopoverDelayOptions = Class external name 'Object' (TJSObject)
    show : NativeInt;
    hide : NativeInt;
  end;

  TPopoverContentFunction = reference to function(arg: jsValue): jsvalue;
  TPopoverPlacementFunction = reference to function(popoverNode, triggeringNode: jsValue): jsvalue;
  
  TBootstrapPopoverOptions = Class external name 'Object' (TJSObject)
    animation : boolean;
    container : string;
    containerEl : TJSHTMLElement; external name 'container';
    content : string;
    contentEl : TJSHTMLElement; external name 'content';
    contentFn : TPopoverContentFunction; external name 'content';
    delay : NativeInt;
    delayObj : TBootstrapPopoverDelayOptions; external name 'delay';
    html : boolean;
    placement : string;
    placementFn : TPopoverPlacementFunction; external name 'placement';
    selector : string;
    selectorBool: Boolean; external name 'selector';
    template : string; 
    title: string;
    titleEl : TJSHTMLElement; external name 'title';
    trigger: string;
    offset : string;
    offsetInt : Integer;  external name 'offset';
    fallbackPlacement : string;
    boundary: string;
    boundaryEl : TJSHTMLElement; external name 'boundary';
  end;

  TBootstrap = Class helper for TJQuery
    Procedure modal; external name 'modal';
    Procedure modal(aCommand : string); external name 'modal';
    Procedure modal(aOptions : TJSObject); external name 'modal';
    Procedure modal(aOptions : TBootstrapModalOptions); external name 'modal';
    Procedure ModalToggle;
    Procedure ModalShow;
    Procedure ModalHide;
    Procedure ModalDispose;
    Procedure ModalHandleUpdate;
    Procedure Toast; external name 'toast';
    Procedure Toast(aCommand : string); external name 'toast';
    Procedure Toast(aOptions : TJSObject); external name 'toast';
    Procedure ToastShow;
    Procedure ToastHide;
    Procedure ToastDispose;
    Procedure Tab;external name 'tab';
    Procedure Tab(aCommand : String) ;external name 'tab';
    Procedure TabShow;
    Procedure TabDispose;
    Procedure ToolTip; external name 'tooltip';
    Procedure ToolTip(aOptions : TJSObject); external name 'tooltip';
    Procedure ToolTip(aOptions : TBootstrapTooltipOptions); external name 'tooltip';
    Procedure ToolTip(aCommand : string); external name 'tooltip';
    Procedure ToolTipShow;
    Procedure ToolTipHide;
    Procedure ToolTipToggle;
    Procedure ToolTipDispose;
    Procedure ToolTipEnable;
    Procedure ToolTipDisable;
    Procedure ToolTipToggleEnabled;
    Procedure ToolTipUpdate;
    Procedure Alert; external name 'alert';
    Procedure Alert(aCommand : string); external name 'alert';
    Procedure AlertClose;
    Procedure AlertDispose;
    Procedure Button(aCommand : string); external name 'button';
    Procedure ButtonDispose;
    Procedure ButtonToggle;
    Procedure Carousel; external name 'carousel';
    Procedure Carousel(aCommand : string); external name 'carousel';
    Procedure Carousel(options : TJSObject); external name 'carousel';
    Procedure Carousel(options : TBootstrapCarouselOptions); external name 'carousel';
    Procedure Carousel(aIndex : Integer); external name 'carousel';
    Procedure CarouselCycle;
    Procedure CarouselPause;
    Procedure CarouselPrev;
    Procedure CarouselNext;
    Procedure CarouselDispose;
    Procedure DropDown; external name 'dropdown';
    Procedure DropDown(aCommand : string); external name 'dropdown';
    Procedure DropDown(options : TJSObject); external name 'dropdown';
    Procedure DropDown(options : TBootstrapDropDownOptions); external name 'dropdown';
    Procedure DropDownToggle;
    Procedure DropDownShow;
    Procedure DropDownHide;
    Procedure DropDownUpdate;
    Procedure DropDownDispose;
    Procedure PopOver; external name 'popover';
    Procedure PopOver(aCommand : string); external name 'popover';
    Procedure PopOver(options : TJSObject); external name 'popover';
    Procedure PopOver(options : TBootstrapPopoverOptions); external name 'popover';
    Procedure PopOverToggle;
    Procedure PopOverShow;
    Procedure PopOverHide;
    Procedure PopOverDispose;
    Procedure PopOverEnable;
    Procedure PopOverDisable;
    Procedure PopOverToggleEnabled;
    Procedure PopOverUpdate;
  end;

Var
  BootstrapVersion : TBootstrapVersion = bv4;

implementation

{ TBootstrap }

procedure TBootstrap.ModalToggle;
begin
  Modal('toggle');
end;

procedure TBootstrap.ModalShow;
begin
  Modal('show');
end;

procedure TBootstrap.ModalHide;
begin
  Modal('hide');
end;

procedure TBootstrap.ModalDispose;
begin
  Modal('dispose');
end;

procedure TBootstrap.ModalHandleUpdate;
begin
  Modal('handleupdate');
end;

procedure TBootstrap.ToastShow;
begin
  Toast('show');
end;

procedure TBootstrap.ToastHide;
begin
  Toast('hide');
end;

procedure TBootstrap.ToastDispose;
begin
  Toast('dispose');
end;

procedure TBootstrap.TabShow;
begin
  Tab('show');
end;

procedure TBootstrap.TabDispose;
begin
  Tab('dispose');
end;

procedure TBootstrap.ToolTipShow;
begin
  ToolTip('show');
end;

procedure TBootstrap.ToolTipHide;
begin
  ToolTip('hide');
end;

procedure TBootstrap.ToolTipToggle;
begin
  ToolTip('toggle');
end;

procedure TBootstrap.ToolTipDispose;
begin
  ToolTip('dispose');
end;

procedure TBootstrap.ToolTipEnable;
begin
  ToolTip('enable');
end;

procedure TBootstrap.ToolTipDisable;
begin
  ToolTip('disable');
end;

procedure TBootstrap.ToolTipToggleEnabled;
begin
  ToolTip('toggleEnabled');
end;

procedure TBootstrap.ToolTipUpdate;
begin
  ToolTip('update');
end;

procedure TBootstrap.AlertClose;
begin
  Alert('close');
end;

procedure TBootstrap.AlertDispose;
begin
  Alert('dispose');
end;

procedure TBootstrap.ButtonDispose;
begin
  Button('dispose');
end;

procedure TBootstrap.ButtonToggle;
begin
  Button('toggle');
end;

procedure TBootstrap.CarouselCycle;
begin
  Carousel('cycle');
end;

procedure TBootstrap.CarouselPause;
begin
  Carousel('pause');
end;

procedure TBootstrap.CarouselPrev;
begin
  Carousel('prev');
end;

procedure TBootstrap.CarouselNext;
begin
  Carousel('next');
end;

procedure TBootstrap.CarouselDispose;
begin
  Carousel('dispose');
end;

procedure TBootstrap.DropDownToggle;
begin
  dropdown('toggle');
end;

procedure TBootstrap.DropDownShow;
begin
  dropdown('show');
end;

procedure TBootstrap.DropDownHide;
begin
  dropdown('hide');
end;

procedure TBootstrap.DropDownUpdate;
begin
  dropdown('update');
end;

procedure TBootstrap.DropDownDispose;
begin
  dropdown('dispose');
end;

procedure TBootstrap.PopOverDisable;
begin
  popover('disable');
end;

procedure TBootstrap.PopOverDispose;
begin
  popover('dispose');
end;

procedure TBootstrap.PopOverEnable;
begin
  popover('enable');
end;

procedure TBootstrap.PopOverHide;
begin
  popover('hide');
end;

procedure TBootstrap.PopOverShow;
begin
  popover('show');
end;

procedure TBootstrap.PopOverToggle;
begin
  popover('toggle');
end;

procedure TBootstrap.PopOverToggleEnabled;
begin
  popover('toggleEnabled');
end;

procedure TBootstrap.PopOverUpdate;
begin
  popover('enabled');
end;


end.

