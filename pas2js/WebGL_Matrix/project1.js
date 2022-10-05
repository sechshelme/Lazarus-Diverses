var pas = { $libimports: {}};

var rtl = {

  version: 20301,

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  $res : {},

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  checkVersion: function(v){
    if (rtl.version != v) throw "expected rtl version "+v+", but found "+rtl.version;
  },

  hiInt: Math.pow(2,53),

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isTRecord: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$new') && (typeof(type.$new)==='function'));
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist);
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var r = Object.create(rtl.tSectionRTTI);
    var module = r.$module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: null,
      $impl: null,
      $rtti: r
    };
    if (impluseslist) module.$impl = {
          $module: module,
          $rtti: r
        };
  },

  exitcode: 0,

  run: function(module_name){
    try {
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if ((module_name=='program') || (module_name=='library')){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas[module_name].$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    } catch(re) {
      if (!rtl.showUncaughtExceptions) {
        throw re
      } else {  
        if (!rtl.handleUncaughtException(re)) {
          rtl.showException(re);
          rtl.exitcode = 216;
        }  
      }
    } 
    return rtl.exitcode;
  },
  
  showException : function (re) {
    var errMsg = rtl.hasString(re.$classname) ? re.$classname : '';
    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
    alert('Uncaught Exception : '+errMsg);
  },

  handleUncaughtException: function (e) {
    if (rtl.onUncaughtException) {
      try {
        rtl.onUncaughtException(e);
        return true;
      } catch (ee) {
        return false; 
      }
    } else {
      return false;
    }
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    var len = useslist.length;
    for (var i = 0; i<len; i++) {
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  createSafeCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        try{
          return scope[fn].apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    } else {
      cb = function(){
        try{
          return fn.apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a===b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn===b.fn);
    }
  },

  initStruct: function(c,parent,name){
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.$name+'.'+name;
    };
    return parent;
  },

  initClass: function(c,parent,name,initfn,rttiname){
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$classname,{ "class": c });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn,rttiname){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = { $ancestor: null };
      c.$create = function(fn,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$init();
        try{
          if (typeof(fn)==="string"){
            o[fn].apply(o,args);
          } else {
            fn.apply(o,args);
          };
          o.AfterConstruction();
        } catch($e){
          // do not call BeforeDestruction
          if (o.Destroy) o.Destroy();
          o.$final();
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        if (this[fnname]) this[fnname]();
        this.$final();
      };
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn,rttiname){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var isFunc = rtl.isFunction(ancestor);
    var c = null;
    if (isFunc){
      // create pascal class descendent from JS function
      c = Object.create(ancestor.prototype);
      c.$ancestorfunc = ancestor;
      c.$ancestor = null; // no pascal ancestor
    } else if (ancestor.$func){
      // create pascal class descendent from a pascal class descendent of a JS function
      isFunc = true;
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
    } else {
      c = Object.create(ancestor);
      c.$ancestor = null; // no pascal ancestor
    }
    c.$create = function(fn,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fn,args);
      } else if(isFunc) {
        o = new this.$func(args);
      } else {
        o = Object.create(c);
      }
      if (o.$init) o.$init();
      try{
        if (typeof(fn)==="string"){
          this[fn].apply(o,args);
        } else {
          fn.apply(o,args);
        };
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        // do not call BeforeDestruction
        if (o.Destroy) o.Destroy();
        if (o.$final) o.$final();
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      if (this[fnname]) this[fnname]();
      if (this.$final) this.$final();
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
    if (isFunc){
      function f(){}
      f.prototype = c;
      c.$func = f;
    }
  },

  createHelper: function(parent,name,ancestor,initfn,rttiname){
    // create a helper,
    // ancestor must be null or a helper,
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // c.$ancestor === Object.getPrototypeOf(c)
    } else {
      c = { $ancestor: null };
    };
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    var t = c.$module.$rtti.$Helper(c.$classname,{ "helper": c });
    c.$rtti = t;
    if (rtl.isObject(ancestor)) t.ancestor = ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return null;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return null;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  hideProp: function(o,p,v){
    Object.defineProperty(o,p, {
      enumerable: false,
      configurable: true,
      writable: true
    });
    if(arguments.length>2){ o[p]=v; }
  },

  recNewT: function(parent,name,initfn,full){
    // create new record type
    var t = {};
    if (parent) parent[name] = t;
    var h = rtl.hideProp;
    if (full){
      rtl.initStruct(t,parent,name);
      t.$record = t;
      h(t,'$record');
      h(t,'$name');
      h(t,'$parent');
      h(t,'$module');
      h(t,'$initSpec');
    }
    initfn.call(t);
    if (!t.$new){
      t.$new = function(){ return Object.create(t); };
    }
    t.$clone = function(r){ return t.$new().$assign(r); };
    h(t,'$new');
    h(t,'$clone');
    h(t,'$eq');
    h(t,'$assign');
    return t;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,
  EIntOverflow: null,
  EPropWriteOnly: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn, rttiname){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = rttiname?rttiname:name;
    i.$fullname = module.$name+'.'+i.$name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(i.$name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,TGuid.$new());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    if (!aclass.hasOwnProperty('$intfmaps')) aclass.$intfmaps = {};
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsIntfT: function(intf,intftype){
    return (intf!==null) && rtl.queryIntfIsT(intf.$o,intftype);
  },

  intfAsIntfT: function (intf,intftype){
    if (!intf) return null;
    var i = rtl.getIntfG(intf.$o,intftype.$guid);
    if (i) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      if(intf) {
        this[id]=intf;
      }
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          var intf = this[id];
          if (intf){
            //console.log('rtl.intfRefs.free: id='+id+' '+intf.$name+' $o='+intf.$o.$classname);
            intf._Release();
          }
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  _ReleaseArray: function(a,dim){
    if (!a) return null;
    for (var i=0; i<a.length; i++){
      if (dim<=1){
        if (a[i]) a[i]._Release();
      } else {
        rtl._ReleaseArray(a[i],dim-1);
      }
    }
    return null;
  },

  trunc: function(a){
    return a<0 ? Math.ceil(a) : Math.floor(a);
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  oc: function(i){
    // overflow check integer
    if ((Math.floor(i)===i) && (i>=-0x1fffffffffffff) && (i<=0x1fffffffffffff)) return i;
    rtl.raiseE('EIntOverflow');
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arrayRef: function(a){
    if (a!=null) rtl.hideProp(a,'$pas2jsrefcnt',1);
    return a;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    var stack = [];
    var s = 9999;
    for (var i=2; i<arguments.length; i++){
      var j = arguments[i];
      if (j==='s'){ s = i-2; }
      else {
        stack.push({ dim:j+0, a:null, i:0, src:null });
      }
    }
    var dimmax = stack.length-1;
    var depth = 0;
    var lastlen = 0;
    var item = null;
    var a = null;
    var src = arr;
    var srclen = 0, oldlen = 0;
    do{
      if (depth>0){
        item=stack[depth-1];
        src = (item.src && item.src.length>item.i)?item.src[item.i]:null;
      }
      if (!src){
        a = [];
        srclen = 0;
        oldlen = 0;
      } else if (src.$pas2jsrefcnt>0 || depth>=s){
        a = [];
        srclen = src.length;
        oldlen = srclen;
      } else {
        a = src;
        srclen = 0;
        oldlen = a.length;
      }
      lastlen = stack[depth].dim;
      a.length = lastlen;
      if (depth>0){
        item.a[item.i]=a;
        item.i++;
        if ((lastlen===0) && (item.i<item.a.length)) continue;
      }
      if (lastlen>0){
        if (depth<dimmax){
          item = stack[depth];
          item.a = a;
          item.i = 0;
          item.src = src;
          depth++;
          continue;
        } else {
          if (srclen>lastlen) srclen=lastlen;
          if (rtl.isArray(defaultvalue)){
            // array of dyn array
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=[];
          } else if (rtl.isObject(defaultvalue)) {
            if (rtl.isTRecord(defaultvalue)){
              // array of record
              for (var i=0; i<srclen; i++) a[i]=defaultvalue.$clone(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue.$new();
            } else {
              // array of set
              for (var i=0; i<srclen; i++) a[i]=rtl.refSet(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]={};
            }
          } else {
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue;
          }
        }
      }
      // backtrack
      while ((depth>0) && (stack[depth-1].i>=stack[depth-1].dim)){
        depth--;
      };
      if (depth===0){
        if (dimmax===0) return a;
        return stack[0].a;
      }
    }while (true);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    } else if (type === 'slice'){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos].slice(0); // clone static array of simple types
    } else if (typeof(type)==='function'){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = type(src[srcpos]); // clone function
    } else if (rtl.isTRecord(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = type.$clone(src[srcpos]); // clone record
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=0; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=rtl.arrayRef(src); // Note: concat(a) does not clone
      } else if (a['$pas2jsrefcnt']){
        a=a.concat(src); // clone a and append src
      } else {
        for (var i=0; i<src.length; i++){
          a.push(src[i]);
        }
      }
    };
    return a;
  },

  arrayPush: function(type,a){
    if(a===null){
      a=[];
    } else if (a['$pas2jsrefcnt']){
      a=rtl.arrayCopy(type,a,0,a.length);
    }
    rtl.arrayClone(type,arguments,2,arguments.length,a,a.length);
    return a;
  },

  arrayPushN: function(a){
    if(a===null){
      a=[];
    } else if (a['$pas2jsrefcnt']){
      a=a.concat();
    }
    for (var i=1; i<arguments.length; i++){
      a.push(arguments[i]);
    }
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  arrayInsert: function(item, arr, index){
    if (arr){
      arr.splice(index,0,item);
      return arr;
    } else {
      return [item];
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    rtl.hideProp(s,'$shared',true);
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
      return s;
    };
  },

  floatToStr: function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (((ad>1) && (ad<1.0e+10)) ||  ((ad>1.e-10) && (ad<1))) {
		pad='00';
	  } else if ((ad>1) && (ad<1.0e+100) || (ad<1.e-10)) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=24;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  valEnum: function(s, enumType, setCodeFn){
    s = s.toLowerCase();
    for (var key in enumType){
      if((typeof(key)==='string') && (key.toLowerCase()===s)){
        setCodeFn(0);
        return enumType[key];
      }
    }
    setCodeFn(1);
    return 0;
  },

  lw: function(l){
    // fix longword bitwise operation
    return l<0?l+0x100000000:l;
  },

  and: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) & (b / hi);
    var l = (a & low) & (b & low);
    return h*hi + l;
  },

  or: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) | (b / hi);
    var l = (a & low) | (b & low);
    return h*hi + l;
  },

  xor: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) ^ (b / hi);
    var l = (a & low) ^ (b & low);
    return h*hi + l;
  },

  shr: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (a<0x80000000) return a >> b;
    if (b<=0) return a;
    if (b>54) return 0;
    return Math.floor(a / Math.pow(2,b));
  },

  shl: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (b<=0) return a;
    if (b>54) return 0;
    var r = a * Math.pow(2,b);
    if (r <= rtl.hiInt) return r;
    return r % rtl.hiInt;
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo", kind: 0, $module: null, attr: null };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = { attr: null };
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,flags,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params,result,flags);
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      t.params = rtl.isArray(t.params) ? rtl.newTIParams(t.params) : null;
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",18 /* tkInterface */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoHelper",19 /* tkHelper */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoExtClass",20 /* tkExtClass */,rtl.tTypeInfoClass);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); },
    $Helper: function(name,o){ return this.$Scope(name,rtl.tTypeInfoHelper,o); },
    $ExtClass: function(name,o){ return this.$Scope(name,rtl.tTypeInfoExtClass,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result?result:null,
      flags: flags?flags:0
    };
    return s;
  },

  addResource: function(aRes){
    rtl.$res[aRes.name]=aRes;
  },

  getResource: function(aName){
    var res = rtl.$res[aName];
    if (res !== undefined) {
      return res;
    } else {
      return null;
    }
  },

  getResourceList: function(){
    return Object.keys(rtl.$res);
  }
}

rtl.addResource({name: "unit1", unit: "unit1", format: "application/octet-stream", encoding: "base64", data: "b2JqZWN0IFdGb3JtMTogVFdGb3JtMQogIExlZnQgPSAyMTkzCiAgSGVpZ2h0ID0gMzM3CiAgVG9wID0gNDkKICBXaWR0aCA9IDU5MgogIEFscGhhQmxlbmQgPSBGYWxzZQogIEFscGhhQmxlbmRWYWx1ZSA9IDI1NQogIENhcHRpb24gPSAnV0Zvcm0xJwogIENsaWVudEhlaWdodCA9IDMzNwogIENsaWVudFdpZHRoID0gNTkyCiAgT25DcmVhdGUgPSBGb3JtQ3JlYXRlCiAgb2JqZWN0IFdCdXR0b24xOiBUV0J1dHRvbgogICAgTGVmdCA9IDEzMgogICAgSGVpZ2h0ID0gMjUKICAgIFRvcCA9IDgzCiAgICBXaWR0aCA9IDc1CiAgICBDYXB0aW9uID0gJ1dCdXR0b24xJwogICAgVGFiT3JkZXIgPSAwCiAgICBPbkNsaWNrID0gV0J1dHRvbjFDbGljawogIGVuZAogIG9iamVjdCBXVGltZXIxOiBUV1RpbWVyCiAgICBJbnRlcnZhbCA9IDEwMAogICAgT25UaW1lciA9IFdUaW1lcjFUaW1lcgogICAgTGVmdCA9IDU3CiAgICBUb3AgPSA5MwogIGVuZAplbmQK"});
rtl.addResource({name: "project1", unit: "project1", format: "application/octet-stream", encoding: "base64", data: "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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAACAAAAAwAAAAQAAAAGAAAACQAAAA0AAAAQAAAAEwAAABYAAAAXAAAAGQAAABoAAAAbAAAAHAAAABsAAAAaAAAAGQAAABcAAAAWAAAAEwAAAA8AAAALAAAACAAAAAUAAAAEAAAAAwAAAAIAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAgAAAAMAAAAFAAAACQAAAA8AAAAVAAAAGwAAACEAAAAoAAAALwAAADYAAAA8AAAAQAAAAEQAAABHAAAASQAAAEsAAABLAAAASwAAAEkAAABHAAAARAAAAEAAAAA7AAAANQAAAC0AAAAmAAAAHwAAABoAAAAVAAAADwAAAAkAAAAFAAAAAwAAAAIAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/////////gAAP////////////////8AAAAH///////////////wAAAAAH//////////////wAAAAAAP/////////////gAAAAAAA/////////////gAAAAAAAD////////////gAAAAAAAAP///////////gAAAAAAAAA///////////gAAAAAAAAAD//////////gAAAAAAAAAAP/////////wAAAAAAAAAAB/////////wAAAAAAAAAAAH////////4AAAAAAAAAAAA////////8AAAAAAAAAAAAH///////+AAAAAAAAAAAAA///////+AAAAAAAAAAAAAD///////AAAAAAAAAAAAAAf//////gAAAAAAAAAAAAAD//////wAAAAAAAAAAAAAAf/////4AAAAAAAAAAAAAAD/////8AAAAAAAAAAAAAAAf/////AAAAAAAAAAAAAAAH/////gAAAAAAAAAAAAAAA/////wAAAAAAAAAAAAAAAH////4AAAAAAAAAAAAAAAA////8AAAAAAAAAAAAAAAAH////AAAAAAAAAAAAAAAAB////gAAAAAAAAAAAAAAAAP///wAAAAAAAAAAAAAAAAB///8AAAAAAAAAAAAAAAAAf//+AAAAAAAAAAAAAAAAAD///gAAAAAAAAAAAAAAAAA///wAAAAAAAAAAAAAAAAAH//8AAAAAAAAAAAAAAAAAB//+AAAAAAAAAAAAAAAAAAP//gAAAAAAAAAAAAAAAAAD//wAAAAAAAAAAAAAAAAAAf/8AAAAAAAAAAAAAAAAAAH/+AAAAAAAAAAAAAAAAAAA//gAAAAAAAAAAAAAAAAAAP/4AAAAAAAAAAAAAAAAAAD/8AAAAAAAAAAAAAAAAAAAf/AAAAAAAAAAAAAAAAAAAH/gAAAAAAAAAAAAAAAAAAA/4AAAAAAAAAAAAAAAAAAAP+AAAAAAAAAAAAAAAAAAAD/gAAAAAAAAAAAAAAAAAAA/wAAAAAAAAAAAAAAAAAAAH8AAAAAAAAAAAAAAAAAAAB/AAAAAAAAAAAAAAAAAAAAfwAAAAAAAAAAAAAAAAAAAH8AAAAAAAAAAAAAAAAAAAB/AAAAAAAAAAAAAAAAAAAAfgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAD4AAAAAAAAAAAAAAAAAAAA+AAAAAAAAAAAAAAAAAAAAPgAAAAAAAAAAAAAAAAAAAH4AAAAAAAAAAAAAAAAAAAB+AAAAAAAAAAAAAAAAAAAAfgAAAAAAAAAAAAAAAAAAAH4AAAAAAAAAAAAAAAAAAAB/AAAAAAAAAAAAAAAAAAAA/wAAAAAAAAAAAAAAAAAAAP8AAAAAAAAAAAAAAAAAAAD/AAAAAAAAAAAAAAAAAAAA/4AAAAAAAAAAAAAAAAAAAf+AAAAAAAAAAAAAAAAAAAH/gAAAAAAAAAAAAAAAAAAD/4AAAAAAAAAAAAAAAAAAA//AAAAAAAAAAAAAAAAAAAP/wAAAAAAAAAAAAAAAAAAH/+AAAAAAAAAAAAAAAAAAB//gAAAAAAAAAAAAAAAAAA//4AAAAAAAAAAAAAAAAAAP//AAAAAAAAAAAAAAAAAAH//wAAAAAAAAAAAAAAAAAB//+AAAAAAAAAAAAAAAAAA///gAAAAAAAAAAAAAAAAAP//8AAAAAAAAAAAAAAAAAH///AAAAAAAAAAAAAAAAAB///4AAAAAAAAAAAAAAAAA////AAAAAAAAAAAAAAAAAf///wAAAAAAAAAAAAAAAAH///+AAAAAAAAAAAAAAAAD////wAAAAAAAAAAAAAAAB////8AAAAAAAAAAAAAAAA/////gAAAAAAAAAAAAAAAf////8AAAAAAAAAAAAAAAH/////gAAAAAAAAAAAAAAD/////4AAAAAAAAAAAAAAB//////AAAAAAAAAAAAAAA//////4AAAAAAAAAAAAAAf//////AAAAAAAAAAAAAAP//////4AAAAAAAAAAAAAP///////gAAAAAAAAAAAAH///////8AAAAAAAAAAAAD////////gAAAAAAAAAAAB////////8AAAAAAAAAAAB/////////wAAAAAAAAAAA/////////+AAAAAAAAAAA//////////4AAAAAAAAAA///////////AAAAAAAAAA///////////8AAAAAAAAAf///////////wAAAAAAAAf////////////AAAAAAAA/////////////+AAAAAAA//////////////4AAAAAB///////////////4AAAAD////////////////4AAAf/////////////////////////////////////////////////////////////////////////////////////////////6glAAAgAAAA//8DAP//AwAAAAAAEBAAAAAAAAAAAAAAKAAAADAAAABgAAAAAQAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAQAAAAEAAAAHAAAAEAAAABYAAAASAAAADAAAAAQAAAABAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAABQAAACEAAAA/AAAAVwAAAGkAAAByAAAAcgAAAHIAAAByAAAAcgAAAG8AAABhAAAATAAAADAAAAARAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA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"});
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.LineEnding = "\n";
  this.MaxLongint = 0x7fffffff;
  this.Maxint = 2147483647;
  rtl.recNewT(this,"TGuid",function () {
    this.D1 = 0;
    this.D2 = 0;
    this.D3 = 0;
    this.$new = function () {
      var r = Object.create(this);
      r.D4 = rtl.arraySetLength(null,0,8);
      return r;
    };
    this.$eq = function (b) {
      return (this.D1 === b.D1) && (this.D2 === b.D2) && (this.D3 === b.D3) && rtl.arrayEq(this.D4,b.D4);
    };
    this.$assign = function (s) {
      this.D1 = s.D1;
      this.D2 = s.D2;
      this.D3 = s.D3;
      this.D4 = s.D4.slice(0);
      return this;
    };
  });
  rtl.recNewT(this,"TMethod",function () {
    this.Code = null;
    this.Data = null;
    this.$eq = function (b) {
      return (this.Code === b.Code) && (this.Data === b.Data);
    };
    this.$assign = function (s) {
      this.Code = s.Code;
      this.Data = s.Data;
      return this;
    };
  });
  this.$rtti.$Class("TObject");
  this.$rtti.$ClassRef("TClass",{instancetype: this.$rtti["TObject"]});
  rtl.createClass(this,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.MethodAddress = function (aName) {
      var Result = null;
      Result = null;
      if (aName === "") return Result;
      var i = 0;
        var TI = this.$rtti;
        var N = "";
        var MN = "";
        N = aName.toLowerCase();
        while ((MN === "") && (TI != null)) {
          i = 0;
          while ((MN === "") && (i < TI.methods.length)) {
            if (TI.getMethod(i).name.toLowerCase() === N) MN = TI.getMethod(i).name;
            i += 1;
          };
          if (MN === "") TI = TI.ancestor;
        };
        if (MN !== "") Result = this[MN];
      //  return Result;
      return Result;
    };
    this.FieldAddress = function (aName) {
      var Result = null;
      Result = null;
      if (aName === "") return Result;
      var aClass = this.$class;
      var ClassTI = null;
      var myName = aName.toLowerCase();
      var MemberTI = null;
      while (aClass !== null) {
        ClassTI = aClass.$rtti;
        for (var i = 0, $end2 = ClassTI.fields.length - 1; i <= $end2; i++) {
          MemberTI = ClassTI.getField(i);
          if (MemberTI.name.toLowerCase() === myName) {
             return MemberTI;
          };
        };
        aClass = aClass.$ancestor ? aClass.$ancestor : null;
      };
      return Result;
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
    this.GetInterface = function (iid, obj) {
      var Result = false;
      var i = iid.$intf;
      if (i){
        // iid is the private TGuid of an interface
        i = rtl.getIntfG(this,i.$guid,2);
        if (i){
          obj.set(i);
          return true;
        }
      };
      Result = this.GetInterfaceByStr(rtl.guidrToStr(iid),obj);
      return Result;
    };
    this.GetInterfaceByStr = function (iidstr, obj) {
      var Result = false;
      Result = false;
      if (!$mod.IObjectInstance["$str"]) $mod.IObjectInstance["$str"] = rtl.guidrToStr($mod.IObjectInstance);
      if (iidstr == $mod.IObjectInstance["$str"]) {
        obj.set(this);
        return true;
      };
      var i = rtl.getIntfG(this,iidstr,2);
      obj.set(i);
      Result=(i!==null);
      return Result;
    };
  });
  this.S_OK = 0;
  this.E_NOINTERFACE = -2147467262;
  rtl.createInterface(this,"IUnknown","{00000000-0000-0000-C000-000000000046}",["QueryInterface","_AddRef","_Release"],null,function () {
    this.$kind = "com";
  });
  this.IObjectInstance = this.TGuid.$clone({D1: 0xD91C9AF4, D2: 0x3C93, D3: 0x420F, D4: [0xA3,0x03,0xBF,0x5B,0xA8,0x2B,0xFD,0x23]});
  this.TTypeKind = {"0": "tkUnknown", tkUnknown: 0, "1": "tkInteger", tkInteger: 1, "2": "tkChar", tkChar: 2, "3": "tkString", tkString: 3, "4": "tkEnumeration", tkEnumeration: 4, "5": "tkSet", tkSet: 5, "6": "tkDouble", tkDouble: 6, "7": "tkBool", tkBool: 7, "8": "tkProcVar", tkProcVar: 8, "9": "tkMethod", tkMethod: 9, "10": "tkArray", tkArray: 10, "11": "tkDynArray", tkDynArray: 11, "12": "tkRecord", tkRecord: 12, "13": "tkClass", tkClass: 13, "14": "tkClassRef", tkClassRef: 14, "15": "tkPointer", tkPointer: 15, "16": "tkJSValue", tkJSValue: 16, "17": "tkRefToProcVar", tkRefToProcVar: 17, "18": "tkInterface", tkInterface: 18, "19": "tkHelper", tkHelper: 19, "20": "tkExtClass", tkExtClass: 20};
  this.tkFloat = 6;
  this.vtInteger = 0;
  this.vtExtended = 3;
  this.vtWideChar = 9;
  this.vtCurrency = 12;
  this.vtUnicodeString = 18;
  this.vtNativeInt = 19;
  rtl.recNewT(this,"TVarRec",function () {
    this.VType = 0;
    this.VJSValue = undefined;
    this.$eq = function (b) {
      return (this.VType === b.VType) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue);
    };
    this.$assign = function (s) {
      this.VType = s.VType;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      return this;
    };
  });
  this.VarRecs = function () {
    var Result = [];
    var i = 0;
    var v = null;
    Result = [];
    while (i < arguments.length) {
      v = $mod.TVarRec.$new();
      v.VType = rtl.trunc(arguments[i]);
      i += 1;
      v.VJSValue = arguments[i];
      i += 1;
      Result.push($mod.TVarRec.$clone(v));
    };
    return Result;
  };
  this.IsConsole = false;
  this.OnParamCount = null;
  this.OnParamStr = null;
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.Int = function (A) {
    var Result = 0.0;
    Result = $mod.Trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if ((Index < 1) || (Index > S.get().length) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set($mod.Copy(t,1,Index - 1) + Insertion + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    NI.set($impl.valint(S,-9007199254740991,9007199254740991,Code));
  };
  this.val$8 = function (S, d, Code) {
    var x = 0.0;
    if (S === "") {
      Code.set(1);
      return;
    };
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else {
      Code.set(0);
      d.set(x);
    };
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
    Result = "";
    for (var $l = 1, $end = l; $l <= $end; $l++) {
      i = $l;
      Result = Result + c;
    };
    return Result;
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = arguments.length - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l = 0, $end = l; $l <= $end; $l++) {
        i = $l;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l1 = 0, $end1 = l; $l1 <= $end1; $l1++) {
        i = $l1;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  this.SetWriteCallBack = function (H) {
    var Result = null;
    Result = $impl.WriteCallBack;
    $impl.WriteCallBack = H;
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  $mod.$implcode = function () {
    $impl.WriteBuf = "";
    $impl.WriteCallBack = null;
    $impl.valint = function (S, MinVal, MaxVal, Code) {
      var Result = 0;
      var x = 0.0;
      if (S === "") {
        Code.set(1);
        return Result;
      };
      x = Number(S);
      if (isNaN(x)) {
        var $tmp = $mod.Copy(S,1,1);
        if ($tmp === "$") {
          x = Number("0x" + $mod.Copy$1(S,2))}
         else if ($tmp === "&") {
          x = Number("0o" + $mod.Copy$1(S,2))}
         else if ($tmp === "%") {
          x = Number("0b" + $mod.Copy$1(S,2))}
         else {
          Code.set(1);
          return Result;
        };
      };
      if (isNaN(x) || (x !== $mod.Int(x))) {
        Code.set(1)}
       else if ((x < MinVal) || (x > MaxVal)) {
        Code.set(2)}
       else {
        Result = $mod.Trunc(x);
        Code.set(0);
      };
      return Result;
    };
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},[]);
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SArgumentMissing: {org: 'Missing argument in format "%s"'}, SInvalidFormat: {org: 'Invalid format specifier : "%s"'}, SInvalidArgIndex: {org: 'Invalid argument index in format: "%s"'}, SListCapacityError: {org: "List capacity (%s) exceeded."}, SListCountError: {org: "List count (%s) out of bounds."}, SListIndexError: {org: "List index (%s) out of bounds"}, SInvalidName: {org: 'Invalid component name: "%s"'}, SDuplicateName: {org: 'Duplicate component name: "%s"'}, SCantReadPropertyS: {org: 'Cannot read property "%s"'}, SCantWritePropertyS: {org: 'Cannot write property "%s"'}, SIndexedPropertyNeedsParams: {org: 'Indexed property "%s" needs parameters'}, SErrInvalidInteger: {org: 'Invalid integer value: "%s"'}, SEmptyStreamIllegalReader: {org: "Illegal Nil stream for TReader constructor"}, SInvalidPropertyValue: {org: "Invalid value for property"}, SInvalidImage: {org: "Invalid stream format"}, SUnknownProperty: {org: 'Unknown property: "%s"'}, SUnknownPropertyType: {org: "Unknown property type %s"}, SAncestorNotFound: {org: 'Ancestor class for "%s" not found.'}, SUnsupportedPropertyVariantType: {org: "Unsupported property variant type %d"}, SPropertyException: {org: "Error reading %s%s%s: %s"}, SInvalidPropertyPath: {org: "Invalid property path"}, SReadOnlyProperty: {org: "Property is read-only"}, SClassNotFound: {org: 'Class "%s" not found'}, SParserExpected: {org: "Wrong token type: %s expected"}, SParserInvalidFloat: {org: "Invalid floating point number: %s"}, SParserInvalidInteger: {org: "Invalid integer number: %s"}, SParserUnterminatedString: {org: "Unterminated string"}, SParserWrongTokenType: {org: "Wrong token type: %s expected but %s found"}, SParserWrongTokenSymbol: {org: "Wrong token symbol: %s expected but %s found"}, SParserLocInfo: {org: " (at %d,%d, stream offset %.8x)"}, SParserUnterminatedBinValue: {org: "Unterminated byte value"}, SParserInvalidProperty: {org: "Invalid property"}};
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  rtl.recNewT(this,"TSize",function () {
    this.cx = 0;
    this.cy = 0;
    this.$eq = function (b) {
      return (this.cx === b.cx) && (this.cy === b.cy);
    };
    this.$assign = function (s) {
      this.cx = s.cx;
      this.cy = s.cy;
      return this;
    };
  });
  rtl.recNewT(this,"TPoint",function () {
    this.x = 0;
    this.y = 0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      return this;
    };
    var $r = $mod.$rtti.$Record("TPoint",{});
    $r.addField("x",rtl.longint);
    $r.addField("y",rtl.longint);
  });
  rtl.recNewT(this,"TRect",function () {
    this.Left = 0;
    this.Top = 0;
    this.Right = 0;
    this.Bottom = 0;
    this.$eq = function (b) {
      return (this.Left === b.Left) && (this.Top === b.Top) && (this.Right === b.Right) && (this.Bottom === b.Bottom);
    };
    this.$assign = function (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
      return this;
    };
  });
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = $mod.TRect.$new();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = $mod.TPoint.$new();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.Size = function (AWidth, AHeight) {
    var Result = $mod.TSize.$new();
    Result.cx = AWidth;
    Result.cy = AHeight;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  this.isClassInstance = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == Object.getPrototypeOf(v));
  };
  this.isDefined = function (v) {
    return !(v == undefined);
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  this.TEndian = {"0": "Little", Little: 0, "1": "Big", Big: 1};
  rtl.recNewT(this,"TFormatSettings",function () {
    this.CurrencyDecimals = 0;
    this.CurrencyFormat = 0;
    this.CurrencyString = "";
    this.DateSeparator = "";
    this.DecimalSeparator = "";
    this.LongDateFormat = "";
    this.LongTimeFormat = "";
    this.NegCurrFormat = 0;
    this.ShortDateFormat = "";
    this.ShortTimeFormat = "";
    this.ThousandSeparator = "";
    this.TimeAMString = "";
    this.TimePMString = "";
    this.TimeSeparator = "";
    this.TwoDigitYearCenturyWindow = 0;
    this.InitLocaleHandler = null;
    this.$new = function () {
      var r = Object.create(this);
      r.DateTimeToStrFormat = rtl.arraySetLength(null,"",2);
      r.LongDayNames = rtl.arraySetLength(null,"",7);
      r.LongMonthNames = rtl.arraySetLength(null,"",12);
      r.ShortDayNames = rtl.arraySetLength(null,"",7);
      r.ShortMonthNames = rtl.arraySetLength(null,"",12);
      return r;
    };
    this.$eq = function (b) {
      return (this.CurrencyDecimals === b.CurrencyDecimals) && (this.CurrencyFormat === b.CurrencyFormat) && (this.CurrencyString === b.CurrencyString) && (this.DateSeparator === b.DateSeparator) && rtl.arrayEq(this.DateTimeToStrFormat,b.DateTimeToStrFormat) && (this.DecimalSeparator === b.DecimalSeparator) && (this.LongDateFormat === b.LongDateFormat) && rtl.arrayEq(this.LongDayNames,b.LongDayNames) && rtl.arrayEq(this.LongMonthNames,b.LongMonthNames) && (this.LongTimeFormat === b.LongTimeFormat) && (this.NegCurrFormat === b.NegCurrFormat) && (this.ShortDateFormat === b.ShortDateFormat) && rtl.arrayEq(this.ShortDayNames,b.ShortDayNames) && rtl.arrayEq(this.ShortMonthNames,b.ShortMonthNames) && (this.ShortTimeFormat === b.ShortTimeFormat) && (this.ThousandSeparator === b.ThousandSeparator) && (this.TimeAMString === b.TimeAMString) && (this.TimePMString === b.TimePMString) && (this.TimeSeparator === b.TimeSeparator) && (this.TwoDigitYearCenturyWindow === b.TwoDigitYearCenturyWindow);
    };
    this.$assign = function (s) {
      this.CurrencyDecimals = s.CurrencyDecimals;
      this.CurrencyFormat = s.CurrencyFormat;
      this.CurrencyString = s.CurrencyString;
      this.DateSeparator = s.DateSeparator;
      this.DateTimeToStrFormat = s.DateTimeToStrFormat.slice(0);
      this.DecimalSeparator = s.DecimalSeparator;
      this.LongDateFormat = s.LongDateFormat;
      this.LongDayNames = s.LongDayNames.slice(0);
      this.LongMonthNames = s.LongMonthNames.slice(0);
      this.LongTimeFormat = s.LongTimeFormat;
      this.NegCurrFormat = s.NegCurrFormat;
      this.ShortDateFormat = s.ShortDateFormat;
      this.ShortDayNames = s.ShortDayNames.slice(0);
      this.ShortMonthNames = s.ShortMonthNames.slice(0);
      this.ShortTimeFormat = s.ShortTimeFormat;
      this.ThousandSeparator = s.ThousandSeparator;
      this.TimeAMString = s.TimeAMString;
      this.TimePMString = s.TimePMString;
      this.TimeSeparator = s.TimeSeparator;
      this.TwoDigitYearCenturyWindow = s.TwoDigitYearCenturyWindow;
      return this;
    };
    this.GetJSLocale = function () {
      return Intl.DateTimeFormat().resolvedOptions().locale;
    };
    this.Create = function () {
      var Result = $mod.TFormatSettings.$new();
      Result.$assign($mod.TFormatSettings.Create$1($mod.TFormatSettings.GetJSLocale()));
      return Result;
    };
    this.Create$1 = function (ALocale) {
      var Result = $mod.TFormatSettings.$new();
      Result.LongDayNames = $impl.DefaultLongDayNames.slice(0);
      Result.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
      Result.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
      Result.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
      Result.DateTimeToStrFormat[0] = "c";
      Result.DateTimeToStrFormat[1] = "f";
      Result.DateSeparator = "-";
      Result.TimeSeparator = ":";
      Result.ShortDateFormat = "yyyy-mm-dd";
      Result.LongDateFormat = "ddd, yyyy-mm-dd";
      Result.ShortTimeFormat = "hh:nn";
      Result.LongTimeFormat = "hh:nn:ss";
      Result.DecimalSeparator = ".";
      Result.ThousandSeparator = ",";
      Result.TimeAMString = "AM";
      Result.TimePMString = "PM";
      Result.TwoDigitYearCenturyWindow = 50;
      Result.CurrencyFormat = 0;
      Result.NegCurrFormat = 0;
      Result.CurrencyDecimals = 2;
      Result.CurrencyString = "$";
      if ($mod.TFormatSettings.InitLocaleHandler != null) $mod.TFormatSettings.InitLocaleHandler($mod.UpperCase(ALocale),$mod.TFormatSettings.$clone(Result));
      return Result;
    };
  },true);
  rtl.createClass(this,"Exception",pas.System.TObject,function () {
    this.LogMessageOnCreate = false;
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      if (this.LogMessageOnCreate) pas.System.Writeln("Created exception ",this.$classname," with message: ",Msg);
      return this;
    };
    this.CreateFmt = function (Msg, Args) {
      this.Create$1($mod.Format(Msg,Args));
      return this;
    };
  });
  rtl.createClass(this,"EConvertError",this.Exception,function () {
  });
  rtl.createClass(this,"EHeapMemoryError",this.Exception,function () {
  });
  rtl.createClass(this,"EOutOfMemory",this.EHeapMemoryError,function () {
  });
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    Result = $mod.Format$1(Fmt,Args,$mod.FormatSettings);
    return Result;
  };
  this.Format$1 = function (Fmt, Args, aSettings) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9") && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === 255) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          var $tmp = Args[ArgN].VType;
          if ($tmp === 0) {
            Value = Args[ArgN].VJSValue}
           else if ($tmp === 19) {
            Value = Args[ArgN].VJSValue}
           else {
            $impl.DoFormatError(1,Fmt);
          };
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = 255;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === 255) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (Args[DoArg].VType !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp = Fchar;
        if ($tmp === "D") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp === "U") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue >>> 0)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp === "E") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,2,3,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,2,3,Prec,aSettings);
        } else if ($tmp === "F") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,0,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,0,9999,Prec,aSettings);
        } else if ($tmp === "G") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,1,Prec,3,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,1,Prec,3,aSettings);
        } else if ($tmp === "N") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,3,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,3,9999,Prec,aSettings);
        } else if ($tmp === "M") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,4,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,4,9999,Prec,aSettings);
        } else if ($tmp === "S") {
          if (Checkarg(18,false)) {
            Hs = Args[DoArg].VJSValue}
           else if (Checkarg(9,true)) Hs = Args[DoArg].VJSValue;
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp === "P") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,8)}
           else if (Checkarg(0,true)) ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,16);
        } else if ($tmp === "X") {
          if (Checkarg(0,false)) {
            vq = Args[DoArg].VJSValue;
            Index = 16;
          } else if (Checkarg(19,true)) {
            vq = Args[DoArg].VJSValue;
            Index = 31;
          };
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while ((rtl.shl(1,Index * 4) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if (0 in Flags) REFlags = "g";
    if (1 in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    Result = $impl.IntTryStrToInt(S,res,$mod.FormatSettings.DecimalSeparator);
    return Result;
  };
  this.TryStrToInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }});
    if (Result) res.set(R);
    return Result;
  };
  this.StrToQWord = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SErrInvalidInteger"),pas.System.VarRecs(18,S)]);
    Result = N;
    return Result;
  };
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    Result = "";
    if (Value < 0) if (Value<0) Value = 0xFFFFFFFF + Value + 1;
    Result=Value.toString(16);
    Result = $mod.UpperCase(Result);
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStr$1(Value,$mod.FormatSettings);
    return Result;
  };
  this.FloatToStr$1 = function (Value, aSettings) {
    var Result = "";
    Result = $mod.FloatToStrF$1(Value,1,15,0,aSettings);
    return Result;
  };
  this.FloatToStrF$1 = function (Value, format, Precision, Digits, aSettings) {
    var Result = "";
    var TS = "";
    var DS = "";
    DS = aSettings.DecimalSeparator;
    TS = aSettings.ThousandSeparator;
    var $tmp = format;
    if ($tmp === 1) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp === 2) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp === 0) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp === 3) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,TS)}
     else if ($tmp === 4) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,aSettings);
    if ((format !== 4) && (Result.length > 1) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS,TS);
    return Result;
  };
  this.OnGetEnvironmentVariable = null;
  this.OnGetEnvironmentString = null;
  this.OnGetEnvironmentVariableCount = null;
  this.OnShowException = null;
  this.SetOnUnCaughtExceptionHandler = function (aValue) {
    var Result = null;
    Result = $impl.OnPascalException;
    $impl.OnPascalException = aValue;
    $mod.HookUncaughtExceptions();
    return Result;
  };
  this.HookUncaughtExceptions = function () {
    rtl.onUncaughtException = $impl.RTLExceptionHook;
    rtl.showUncaughtExceptions = true;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = rtl.getResStr($mod,"SApplicationException") + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = S + " : " + ExceptObject.fMessage;
    $impl.DoShowException(S);
    if (ExceptAddr === null) ;
  };
  this.TimeSeparator = "";
  this.DateSeparator = "";
  this.ShortDateFormat = "";
  this.LongDateFormat = "";
  this.ShortTimeFormat = "";
  this.LongTimeFormat = "";
  this.DecimalSeparator = "";
  this.ThousandSeparator = "";
  this.TimeAMString = "";
  this.TimePMString = "";
  this.ShortMonthNames = rtl.arraySetLength(null,"",12);
  this.LongMonthNames = rtl.arraySetLength(null,"",12);
  this.ShortDayNames = rtl.arraySetLength(null,"",7);
  this.LongDayNames = rtl.arraySetLength(null,"",7);
  this.FormatSettings = this.TFormatSettings.$new();
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 0;
  this.CurrencyString = "";
  $mod.$implcode = function () {
    $impl.DefaultShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
    $impl.DefaultLongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
    $impl.DefaultShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
    $impl.DefaultLongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
    $impl.DoShowException = function (S) {
      if ($mod.OnShowException != null) {
        $mod.OnShowException(S)}
       else {
        window.alert(S);
      };
    };
    $impl.OnPascalException = null;
    $impl.OnJSException = null;
    $impl.RTLExceptionHook = function (aError) {
      var S = "";
      if (pas.JS.isClassInstance(aError)) {
        if ($impl.OnPascalException != null) {
          $impl.OnPascalException(rtl.getObject(aError))}
         else $mod.ShowException(rtl.getObject(aError),null);
      } else if (rtl.isObject(aError)) {
        if ($impl.OnJSException != null) {
          $impl.OnJSException(aError)}
         else {
          if (aError.hasOwnProperty("message")) {
            S = rtl.getResStr($mod,"SErrUnknownExceptionType") + ("" + aError["message"])}
           else S = rtl.getResStr($mod,"SErrUnknownExceptionType") + aError.toString();
          $impl.DoShowException(S);
        };
      } else {
        S = rtl.getResStr($mod,"SErrUnknownExceptionType") + ("" + aError);
        $impl.DoShowException(S);
      };
    };
    $impl.feInvalidFormat = 1;
    $impl.feMissingArgument = 2;
    $impl.feInvalidArgIndex = 3;
    $impl.DoFormatError = function (ErrCode, fmt) {
      var $tmp = ErrCode;
      if ($tmp === 1) {
        throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidFormat"),pas.System.VarRecs(18,fmt)])}
       else if ($tmp === 2) {
        throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SArgumentMissing"),pas.System.VarRecs(18,fmt)])}
       else if ($tmp === 3) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidArgIndex"),pas.System.VarRecs(18,fmt)]);
    };
    $impl.maxdigits = 15;
    $impl.ReplaceDecimalSep = function (S, DS) {
      var Result = "";
      var P = 0;
      P = pas.System.Pos(".",S);
      if (P > 0) {
        Result = pas.System.Copy(S,1,P - 1) + DS + pas.System.Copy(S,P + 1,S.length - P)}
       else Result = S;
      return Result;
    };
    $impl.FormatGeneralFloat = function (Value, Precision, DS) {
      var Result = "";
      var P = 0;
      var PE = 0;
      var Q = 0;
      var Exponent = 0;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      Result = $mod.TrimLeft(Result);
      P = pas.System.Pos(".",Result);
      if (P === 0) return Result;
      PE = pas.System.Pos("E",Result);
      if (PE === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      Q = PE + 2;
      Exponent = 0;
      while (Q <= Result.length) {
        Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - 48;
        Q += 1;
      };
      if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
      if (((P + Exponent) < PE) && (Exponent > -6)) {
        Result = rtl.strSetLength(Result,PE - 1);
        if (Exponent >= 0) {
          for (var $l = 0, $end = Exponent - 1; $l <= $end; $l++) {
            Q = $l;
            Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
            P += 1;
          };
          Result = rtl.setCharAt(Result,P - 1,".");
          P = 1;
          if (Result.charAt(P - 1) === "-") P += 1;
          while ((Result.charAt(P - 1) === "0") && (P < Result.length) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P,1);
        } else {
          pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 1);
          Result = rtl.setCharAt(Result,P - Exponent - 1,Result.charAt(P - Exponent - 1 - 1));
          Result = rtl.setCharAt(Result,P - 1,".");
          if (Exponent !== -1) Result = rtl.setCharAt(Result,P - Exponent - 1 - 1,"0");
        };
        Q = Result.length;
        while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
        if (Result.charAt(Q - 1) === ".") Q -= 1;
        if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
          Result = "0"}
         else Result = rtl.strSetLength(Result,Q);
      } else {
        while (Result.charAt(PE - 1 - 1) === "0") {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt(PE - 1 - 1) === DS) {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt((PE + 1) - 1) === "+") {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE + 1,1)}
         else PE += 1;
        while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1);
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
      var Result = "";
      var P = 0;
      DS = $mod.FormatSettings.DecimalSeparator;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos("E",Result);
      if (P === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      P += 2;
      if (Digits > 4) Digits = 4;
      Digits = (Result.length - P - Digits) + 1;
      if (Digits < 0) {
        pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P)}
       else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
        if (P > Result.length) {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 2,2);
          break;
        };
        Digits -= 1;
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatFixedFloat = function (Value, Digits, DS) {
      var Result = "";
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
      var Result = "";
      var P = 0;
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 15) Digits = 15;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos(".",Result);
      if (P <= 0) P = Result.length + 1;
      Result = $impl.ReplaceDecimalSep(Result,DS);
      P -= 3;
      if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
        if (Result.charAt(P - 1 - 1) !== "-") pas.System.Insert(TS,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P);
        P -= 3;
      };
      return Result;
    };
    $impl.RemoveLeadingNegativeSign = function (AValue, DS, aThousandSeparator) {
      var Result = false;
      var i = 0;
      var TS = "";
      var StartPos = 0;
      Result = false;
      StartPos = 2;
      TS = aThousandSeparator;
      for (var $l = StartPos, $end = AValue.get().length; $l <= $end; $l++) {
        i = $l;
        Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get().charAt(i - 1) === TS);
        if (!Result) break;
      };
      if (Result && (AValue.get().charAt(0) === "-")) pas.System.Delete(AValue,1,1);
      return Result;
    };
    $impl.FormatNumberCurrency = function (Value, Digits, aSettings) {
      var Result = "";
      var Negative = false;
      var P = 0;
      var CS = "";
      var DS = "";
      var TS = "";
      DS = aSettings.DecimalSeparator;
      TS = aSettings.ThousandSeparator;
      CS = aSettings.CurrencyString;
      if (Digits === -1) {
        Digits = aSettings.CurrencyDecimals}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value / 10000,0,Digits);
      Negative = Result.charAt(0) === "-";
      if (Negative) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos(".",Result);
      if (TS !== "") {
        if (P !== 0) {
          Result = $impl.ReplaceDecimalSep(Result,DS)}
         else P = Result.length + 1;
        P -= 3;
        while (P > 1) {
          pas.System.Insert(TS,{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P);
          P -= 3;
        };
      };
      if (Negative) $impl.RemoveLeadingNegativeSign({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},DS,TS);
      if (!Negative) {
        var $tmp = aSettings.CurrencyFormat;
        if ($tmp === 0) {
          Result = CS + Result}
         else if ($tmp === 1) {
          Result = Result + CS}
         else if ($tmp === 2) {
          Result = CS + " " + Result}
         else if ($tmp === 3) Result = Result + " " + CS;
      } else {
        var $tmp1 = aSettings.NegCurrFormat;
        if ($tmp1 === 0) {
          Result = "(" + CS + Result + ")"}
         else if ($tmp1 === 1) {
          Result = "-" + CS + Result}
         else if ($tmp1 === 2) {
          Result = CS + "-" + Result}
         else if ($tmp1 === 3) {
          Result = CS + Result + "-"}
         else if ($tmp1 === 4) {
          Result = "(" + Result + CS + ")"}
         else if ($tmp1 === 5) {
          Result = "-" + Result + CS}
         else if ($tmp1 === 6) {
          Result = Result + "-" + CS}
         else if ($tmp1 === 7) {
          Result = Result + CS + "-"}
         else if ($tmp1 === 8) {
          Result = "-" + Result + " " + CS}
         else if ($tmp1 === 9) {
          Result = "-" + CS + " " + Result}
         else if ($tmp1 === 10) {
          Result = Result + " " + CS + "-"}
         else if ($tmp1 === 11) {
          Result = CS + " " + Result + "-"}
         else if ($tmp1 === 12) {
          Result = CS + " " + "-" + Result}
         else if ($tmp1 === 13) {
          Result = Result + "-" + " " + CS}
         else if ($tmp1 === 14) {
          Result = "(" + CS + " " + Result + ")"}
         else if ($tmp1 === 15) Result = "(" + Result + " " + CS + ")";
      };
      return Result;
    };
    $impl.RESpecials = "([\\$\\+\\[\\]\\(\\)\\\\\\.\\*\\^\\?\\|])";
    $impl.IntTryStrToInt = function (S, res, aSep) {
      var Result = false;
      var Radix = 10;
      var N = "";
      var J = undefined;
      N = S;
      if ((pas.System.Pos(aSep,N) !== 0) || (pas.System.Pos(".",N) !== 0)) return false;
      var $tmp = pas.System.Copy(N,1,1);
      if ($tmp === "$") {
        Radix = 16}
       else if ($tmp === "&") {
        Radix = 8}
       else if ($tmp === "%") Radix = 2;
      if ((Radix !== 16) && (pas.System.Pos("e",$mod.LowerCase(N)) !== 0)) return false;
      if (Radix !== 10) pas.System.Delete({get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},1,1);
      J = parseInt(N,Radix);
      Result = !isNaN(J);
      if (Result) res.set(rtl.trunc(J));
      return Result;
    };
    $impl.InitGlobalFormatSettings = function () {
      $mod.FormatSettings.$assign($mod.TFormatSettings.Create());
      $mod.TimeSeparator = $mod.FormatSettings.TimeSeparator;
      $mod.DateSeparator = $mod.FormatSettings.DateSeparator;
      $mod.ShortDateFormat = $mod.FormatSettings.ShortDateFormat;
      $mod.LongDateFormat = $mod.FormatSettings.LongDateFormat;
      $mod.ShortTimeFormat = $mod.FormatSettings.ShortTimeFormat;
      $mod.LongTimeFormat = $mod.FormatSettings.LongTimeFormat;
      $mod.DecimalSeparator = $mod.FormatSettings.DecimalSeparator;
      $mod.ThousandSeparator = $mod.FormatSettings.ThousandSeparator;
      $mod.TimeAMString = $mod.FormatSettings.TimeAMString;
      $mod.TimePMString = $mod.FormatSettings.TimePMString;
      $mod.CurrencyFormat = $mod.FormatSettings.CurrencyFormat;
      $mod.NegCurrFormat = $mod.FormatSettings.NegCurrFormat;
      $mod.CurrencyDecimals = $mod.FormatSettings.CurrencyDecimals;
      $mod.CurrencyString = $mod.FormatSettings.CurrencyString;
    };
    $mod.$resourcestrings = {SApplicationException: {org: "Application raised an exception: "}, SErrUnknownExceptionType: {org: "Caught unknown exception type : "}};
  };
  $mod.$init = function () {
    (function () {
      $impl.InitGlobalFormatSettings();
    })();
    $mod.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
    $mod.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
    $mod.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
    $mod.LongDayNames = $impl.DefaultLongDayNames.slice(0);
  };
},[]);
rtl.module("TypInfo",["System","SysUtils","Types","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.pfGetFunction = 1;
  this.pfSetProcedure = 2;
  this.pfHasIndex = 16;
  rtl.createClass(this,"EPropertyError",pas.SysUtils.Exception,function () {
  });
  this.GetPropInfo = function (TI, PropName) {
    var Result = null;
    var m = null;
    var i = 0;
    var C = null;
    C = TI;
    while (C !== null) {
      m = C.members[PropName];
      if (rtl.isExt(m,rtl.tTypeMemberProperty)) return m;
      if (!rtl.isExt(C,rtl.tTypeInfoClass)) break;
      C = C.ancestor;
    };
    Result = null;
    do {
      for (var $l = 0, $end = TI.properties.length - 1; $l <= $end; $l++) {
        i = $l;
        if (pas.SysUtils.CompareText(PropName,TI.properties[i]) === 0) {
          m = TI.members[TI.properties[i]];
          if (rtl.isExt(m,rtl.tTypeMemberProperty)) Result = m;
          return Result;
        };
      };
      if (!rtl.isExt(TI,rtl.tTypeInfoClass)) break;
      TI = TI.ancestor;
    } while (!(TI === null));
    return Result;
  };
  this.GetPropInfo$1 = function (TI, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo(TI,PropName);
    if (rtl.neSet(Kinds,{}) && (Result !== null) && !(Result.typeinfo.kind in Kinds)) Result = null;
    return Result;
  };
  this.GetPropInfo$4 = function (aClass, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo$1(aClass.$rtti,PropName,{});
    return Result;
  };
  this.GetJSValueProp$1 = function (Instance, PropInfo) {
    var Result = undefined;
    var gk = 0;
    gk = $impl.GetPropGetterKind(PropInfo);
    var $tmp = gk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantReadPropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      Result = Instance[PropInfo.getter]}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Result = Instance[PropInfo.getter](PropInfo.index)}
       else Result = Instance[PropInfo.getter]()}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
    return Result;
  };
  this.GetJSValueProp$3 = function (Instance, PropInfo) {
    var Result = undefined;
    Result = $mod.GetJSValueProp$1(Instance,PropInfo);
    return Result;
  };
  this.SetJSValueProp$1 = function (Instance, PropInfo, Value) {
    var sk = 0;
    sk = $impl.GetPropSetterKind(PropInfo);
    var $tmp = sk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantWritePropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      Instance[PropInfo.setter] = Value}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Instance[PropInfo.setter](PropInfo.index,Value)}
       else Instance[PropInfo.setter](Value)}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
  };
  this.SetJSValueProp$3 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.SetOrdProp$1 = function (Instance, PropInfo, Value) {
    var o = null;
    var i = 0;
    if (PropInfo.typeinfo.kind === 5) {
      o = new Object();
      for (i = 0; i <= 31; i++) if (((1 << i) & Value) > 0) o["" + i] = true;
      $mod.SetJSValueProp$3(Instance,PropInfo,o);
    } else if (PropInfo.typeinfo.kind === 2) {
      $mod.SetJSValueProp$3(Instance,PropInfo,String.fromCharCode(Value))}
     else $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.GetEnumValue = function (TypeInfo, Name) {
    var Result = 0;
    Result = TypeInfo.enumtype[Name];
    return Result;
  };
  this.SetBoolProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.SetStrProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.SetFloatProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.GetObjectProp$2 = function (Instance, PropInfo) {
    var Result = null;
    Result = $mod.GetObjectProp$3(Instance,PropInfo,null);
    return Result;
  };
  this.GetObjectProp$3 = function (Instance, PropInfo, MinClass) {
    var Result = null;
    var O = null;
    O = rtl.getObject($mod.GetJSValueProp$3(Instance,PropInfo));
    if ((MinClass !== null) && !O.$class.InheritsFrom(MinClass)) {
      Result = null}
     else Result = O;
    return Result;
  };
  this.SetObjectProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.SetMethodProp = function (Instance, PropInfo, Value) {
    var cb = null;
    var Code = null;
    Code = Value.Code;
    if (Code === null) {
      cb = null}
     else if (rtl.isFunction(Code)) {
      if ((Code["scope"] === Value.Data) && (rtl.isFunction(Code["fn"]) || rtl.isString(Code["fn"]))) {
        cb = Code;
      } else if (rtl.isString(Code["fn"])) {
        cb = rtl.createCallback(Value.Data,"" + Code["fn"])}
       else cb = rtl.createCallback(Value.Data,Code);
    } else cb = rtl.createCallback(Value.Data,Code);
    $mod.SetJSValueProp$3(Instance,PropInfo,cb);
  };
  this.SetInterfaceProp$1 = function (Instance, PropInfo, Value) {
    var sk = 0;
    var Setter = "";
    if (PropInfo.typeinfo.kind !== 18) throw pas.SysUtils.Exception.$create("Create$1",["Cannot set RAW interface from IInterface interface"]);
    sk = $impl.GetPropSetterKind(PropInfo);
    Setter = PropInfo.setter;
    var $tmp = sk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantWritePropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      rtl.setIntfP(Instance,Setter,Value)}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Instance[Setter](PropInfo.index,Value)}
       else Instance[Setter](Value)}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
  };
  $mod.$implcode = function () {
    $impl.TGetterKind = {"0": "gkNone", gkNone: 0, "1": "gkField", gkField: 1, "2": "gkFunction", gkFunction: 2, "3": "gkFunctionWithParams", gkFunctionWithParams: 3};
    $impl.GetPropGetterKind = function (PropInfo) {
      var Result = 0;
      if (PropInfo.getter === "") {
        Result = 0}
       else if ((1 & PropInfo.flags) > 0) {
        if (rtl.length(PropInfo.params) > 0) {
          Result = 3}
         else Result = 2;
      } else Result = 1;
      return Result;
    };
    $impl.TSetterKind = {"0": "skNone", skNone: 0, "1": "skField", skField: 1, "2": "skProcedure", skProcedure: 2, "3": "skProcedureWithParams", skProcedureWithParams: 3};
    $impl.GetPropSetterKind = function (PropInfo) {
      var Result = 0;
      if (PropInfo.setter === "") {
        Result = 0}
       else if ((2 & PropInfo.flags) > 0) {
        if (rtl.length(PropInfo.params) > 0) {
          Result = 3}
         else Result = 2;
      } else Result = 1;
      return Result;
    };
  };
},[]);
rtl.module("weborworker",["System","JS","Types"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Web",["System","Types","JS","weborworker"],function () {
  "use strict";
  var $mod = this;
  rtl.createClassExt(this,"TJSFocusEvent",Event,"",function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
  });
});
rtl.module("p2jsres",["System","Types"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TResourceSource = {"0": "rsJS", rsJS: 0, "1": "rsHTML", rsHTML: 1};
  rtl.recNewT(this,"TResourceInfo",function () {
    this.name = "";
    this.encoding = "";
    this.resourceunit = "";
    this.format = "";
    this.data = "";
    this.$eq = function (b) {
      return (this.name === b.name) && (this.encoding === b.encoding) && (this.resourceunit === b.resourceunit) && (this.format === b.format) && (this.data === b.data);
    };
    this.$assign = function (s) {
      this.name = s.name;
      this.encoding = s.encoding;
      this.resourceunit = s.resourceunit;
      this.format = s.format;
      this.data = s.data;
      return this;
    };
  });
  this.SetResourceSource = function (aSource) {
    var Result = 0;
    Result = $impl.gMode;
    $impl.gMode = aSource;
    return Result;
  };
  this.GetResourceInfo = function (aName, aInfo) {
    var Result = false;
    Result = $mod.GetResourceInfo$1($impl.gMode,aName,aInfo);
    return Result;
  };
  this.GetResourceInfo$1 = function (aSource, aName, aInfo) {
    var Result = false;
    var $tmp = aSource;
    if ($tmp === 0) {
      Result = $impl.GetRTLResourceInfo(aName,aInfo)}
     else if ($tmp === 1) Result = $impl.GetHTMLResourceInfo(aName,aInfo);
    return Result;
  };
  $mod.$implcode = function () {
    $impl.gMode = 0;
    $impl.GetRTLResourceInfo = function (aName, aInfo) {
      var Result = false;
      var RTLInfo = null;
      RTLInfo = rtl.getResource(pas.SysUtils.LowerCase(aName));
      Result = RTLInfo != null;
      if (Result) {
        aInfo.name = RTLInfo.name;
        aInfo.encoding = RTLInfo.encoding;
        aInfo.format = RTLInfo.format;
        aInfo.resourceunit = RTLInfo.unit;
        aInfo.data = RTLInfo.data;
      };
      return Result;
    };
    $impl.IDPrefix = "resource-";
    $impl.GetHTMLResourceInfo = function (aName, aInfo) {
      var Result = false;
      var el = null;
      var S = "";
      var I = 0;
      Result = false;
      if (!pas.JS.isDefined(document)) return Result;
      el = document.getElementById($impl.IDPrefix + pas.SysUtils.LowerCase(aName));
      Result = (el != null) && pas.SysUtils.SameText(el.tagName,"link");
      if (!Result) return Result;
      aInfo.name = pas.SysUtils.LowerCase(aName);
      aInfo.resourceunit = el.dataset["unit"];
      S = el.href;
      S = pas.System.Copy(S,6,S.length - 5);
      I = pas.System.Pos(",",S);
      aInfo.data = pas.System.Copy(S,I + 1,S.length - 1);
      S = pas.System.Copy(S,1,I - 1);
      I = pas.System.Pos(";",S);
      if (I === 0) {
        aInfo.encoding = ""}
       else {
        aInfo.encoding = pas.System.Copy(S,I + 1,S.length - 1);
        S = pas.System.Copy(S,1,I - 1);
      };
      aInfo.format = S;
      return Result;
    };
  };
},["SysUtils","JS","Web"]);
rtl.module("simplelinkedlist",["System"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TLinkedListItem",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.Next = null;
    };
    this.$final = function () {
      this.Next = undefined;
      pas.System.TObject.$final.call(this);
    };
  });
  rtl.createClass(this,"TLinkedListVisitor",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TLinkedList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FItemClass = null;
      this.FRoot = null;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FRoot = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AnItemClass) {
      this.FItemClass = AnItemClass;
      return this;
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      var I = null;
      I = this.FRoot;
      while (I !== null) {
        this.FRoot = I;
        I = I.Next;
        this.FRoot.Next = null;
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FRoot;
          }, set: function (v) {
            this.p.FRoot = v;
          }});
      };
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create");
      Result.Next = this.FRoot;
      this.FRoot = Result;
      return Result;
    };
    this.ForEach = function (Visitor) {
      var I = null;
      I = this.FRoot;
      while ((I !== null) && Visitor.Visit(I)) I = I.Next;
    };
    this.RemoveItem = function (Item, FreeItem) {
      var I = null;
      if ((Item !== null) && (this.FRoot !== null)) {
        if (Item === this.FRoot) {
          this.FRoot = Item.Next}
         else {
          I = this.FRoot;
          while ((I.Next !== null) && (I.Next !== Item)) I = I.Next;
          if (I.Next === Item) I.Next = Item.Next;
        };
        if (FreeItem) Item = rtl.freeLoc(Item);
      };
    };
  });
},["SysUtils"]);
rtl.module("Classes",["System","RTLConsts","Types","SysUtils","JS","TypInfo","p2jsres"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass(this,"EStreamError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EFilerError",this.EStreamError,function () {
  });
  rtl.createClass(this,"EReadError",this.EFilerError,function () {
  });
  rtl.createClass(this,"EClassNotFound",this.EFilerError,function () {
  });
  rtl.createClass(this,"EResNotFound",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EComponentError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EParserError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EOutOfResources",pas.SysUtils.EOutOfMemory,function () {
  });
  this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  this.$rtti.$Enum("TAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass(this,"TFPList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListCapacityError"),"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListCountError"),"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.Error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,pas.System.VarRecs(18,Data)]);
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === 0) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass(this,"TList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Notify = function (aValue, Action) {
      if (pas.System.Assigned(aValue)) ;
      if (Action === 1) ;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FList = $mod.TFPList.$create("Create");
      return this;
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,0);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if (pas.System.Assigned(V)) this.Notify(V,2);
    };
    this.Insert = function (Index, Item) {
      this.FList.Insert(Index,Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,0);
    };
  });
  rtl.createClass(this,"TPersistent",pas.System.TObject,function () {
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw pas.SysUtils.EConvertError.$create("Create$1",["Cannot assign a " + SourceName + " to a " + this.$classname + "."]);
    };
    this.DefineProperties = function (Filer) {
      if (Filer === null) return;
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass(this,"TCollectionItem",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection != null) this.FCollection.RemoveItem(this);
        if (Value != null) Value.InsertItem(this);
      };
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.SetCollection(ACollection);
      return this;
    };
    this.Destroy = function () {
      this.SetCollection(null);
      pas.System.TObject.Destroy.call(this);
    };
  });
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass(this,"TCollection",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,0);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,1);
      I = this.FItems.IndexOfItem(Item,1);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === 0) ;
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($mod.TCollection.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l = 0, $end = Source.GetCount() - 1; $l <= $end; $l++) {
          I = $l;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
  });
  this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass(this,"TComponent",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FDesignInfo = 0;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetComponent = function (AIndex) {
      var Result = null;
      if (!(this.FComponents != null)) {
        Result = null}
       else Result = rtl.getObject(this.FComponents.Get(AIndex));
      return Result;
    };
    this.GetComponentCount = function () {
      var Result = 0;
      if (!(this.FComponents != null)) {
        Result = 0}
       else Result = this.FComponents.FCount;
      return Result;
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $mod.TFPList.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.ReadLeft = function (AReader) {
      this.FDesignInfo = (this.FDesignInfo & 0xffff0000) | (AReader.ReadInteger() & 0xffff);
    };
    this.ReadTop = function (AReader) {
      this.FDesignInfo = ((AReader.ReadInteger() & 0xffff) << 16) | (this.FDesignInfo & 0xffff);
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,8);
        };
      };
    };
    this.SetReference = function (Enable) {
      var aField = null;
      var aValue = null;
      var aOwner = null;
      if (this.FName === "") return;
      if (this.FOwner != null) {
        aOwner = this.FOwner;
        aField = this.FOwner.$class.FieldAddress(this.FName);
        if (aField != null) {
          if (Enable) {
            aValue = this}
           else aValue = null;
          aOwner["" + aField["name"]] = aValue;
        };
      };
    };
    this.WriteLeft = function (AWriter) {
      AWriter.WriteInteger(this.FDesignInfo & 0xffff);
    };
    this.WriteTop = function (AWriter) {
      AWriter.WriteInteger((this.FDesignInfo >>> 16) & 0xffff);
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.DefineProperties = function (Filer) {
      var Temp = 0;
      var Ancestor = null;
      Ancestor = Filer.FAncestor;
      if (Ancestor != null) {
        Temp = Ancestor.FDesignInfo}
       else Temp = 0;
      Filer.DefineProperty("Left",rtl.createCallback(this,"ReadLeft"),rtl.createCallback(this,"WriteLeft"),(this.FDesignInfo & 0xffff) !== (Temp & 0xffff));
      Filer.DefineProperty("Top",rtl.createCallback(this,"ReadTop"),rtl.createCallback(this,"WriteTop"),(this.FDesignInfo & 0xffff0000) !== (Temp & 0xffff0000));
    };
    this.GetChildOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.GetChildParent = function () {
      var Result = null;
      Result = this;
      return Result;
    };
    this.Loaded = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,0);
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === 1) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.ReadState = function (Reader) {
      Reader.ReadData(this);
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,4)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,4);
      if ((this.FComponents != null) && SetChildren) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !pas.SysUtils.IsValidIdent(NewName,false,false)) throw $mod.EComponentError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidName"),pas.System.VarRecs(18,NewName)]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.SetReference(false);
      this.ChangeName(NewName);
      this.SetReference(true);
    };
    this.SetChildOrder = function (Child, Order) {
      if (Child === null) ;
      if (Order === 0) ;
    };
    this.SetParentComponent = function (Value) {
      if (Value === null) ;
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((AComponent !== null) && (pas.SysUtils.CompareText(CurName,NewName) !== 0) && (AComponent.FOwner === this) && (this.FindComponent(NewName) !== null)) throw $mod.EComponentError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SDuplicateName"),pas.System.VarRecs(18,NewName)]);
      if ((4 in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this._AddRef = function () {
      var Result = 0;
      Result = -1;
      return Result;
    };
    this._Release = function () {
      var Result = 0;
      Result = -1;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet(0);
      if (AOwner != null) AOwner.InsertComponent(this);
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,1);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      pas.System.TObject.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!(3 in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if (3 in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,3);
      if (this.FComponents != null) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.QueryInterface = function (IID, Obj) {
      var Result = 0;
      if (this.GetInterface(IID,Obj)) {
        Result = 0}
       else Result = -2147467262;
      return Result;
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        I = $l;
        if (pas.SysUtils.CompareText(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.GetParentComponent = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      if (AComponent.FOwner !== null) AComponent.FOwner.RemoveComponent(AComponent);
      this.Insert(AComponent);
      if (4 in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,0);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,1);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag",{Default: 0});
  });
  this.$rtti.$ClassRef("TComponentClass",{instancetype: this.$rtti["TComponent"]});
  this.TSeekOrigin = {"0": "soBeginning", soBeginning: 0, "1": "soCurrent", soCurrent: 1, "2": "soEnd", soEnd: 2};
  rtl.createClass(this,"TStream",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FEndian = 0;
    };
    this.MakeInt = function (B, aSize, Signed) {
      var Result = 0;
      var Mem = null;
      var A = null;
      var D = null;
      var isLittle = false;
      isLittle = this.FEndian === 0;
      Mem = new ArrayBuffer(rtl.length(B));
      A = new Uint8Array(Mem);
      A.set(B);
      D = new DataView(Mem);
      if (Signed) {
        var $tmp = aSize;
        if ($tmp === 1) {
          Result = D.getInt8(0)}
         else if ($tmp === 2) {
          Result = D.getInt16(0,isLittle)}
         else if ($tmp === 4) {
          Result = D.getInt32(0,isLittle)}
         else if ($tmp === 8) Result = Math.round(D.getFloat64(0,isLittle));
      } else {
        var $tmp1 = aSize;
        if ($tmp1 === 1) {
          Result = D.getUint8(0)}
         else if ($tmp1 === 2) {
          Result = D.getUint16(0,isLittle)}
         else if ($tmp1 === 4) {
          Result = D.getUint32(0,isLittle)}
         else if ($tmp1 === 8) Result = Math.round(D.getFloat64(0,isLittle));
      };
      return Result;
    };
    this.MakeBytes = function (B, aSize, Signed) {
      var Result = [];
      var Mem = null;
      var A = null;
      var D = null;
      var isLittle = false;
      isLittle = this.FEndian === 0;
      Mem = new ArrayBuffer(aSize);
      D = new DataView(Mem);
      if (Signed) {
        var $tmp = aSize;
        if ($tmp === 1) {
          D.setInt8(0,B)}
         else if ($tmp === 2) {
          D.setInt16(0,B,isLittle)}
         else if ($tmp === 4) {
          D.setInt32(0,B,isLittle)}
         else if ($tmp === 8) D.setFloat64(0,B,isLittle);
      } else {
        var $tmp1 = aSize;
        if ($tmp1 === 1) {
          D.setUint8(0,B)}
         else if ($tmp1 === 2) {
          D.setUint16(0,B,isLittle)}
         else if ($tmp1 === 4) {
          D.setUint32(0,B,isLittle)}
         else if ($tmp1 === 8) D.setFloat64(0,B,isLittle);
      };
      Result = rtl.arraySetLength(Result,0,aSize);
      A = new Uint8Array(Mem);
      Result = $mod.TMemoryStream.MemoryToBytes$1(A);
      return Result;
    };
    this.GetPosition = function () {
      var Result = 0;
      Result = this.Seek(0,1);
      return Result;
    };
    this.SetPosition = function (Pos) {
      this.Seek(Pos,0);
    };
    this.GetSize = function () {
      var Result = 0;
      var p = 0;
      p = this.Seek(0,1);
      Result = this.Seek(0,2);
      this.Seek(p,0);
      return Result;
    };
    this.ReadMaxSizeData = function (Buffer, aSize, aCount) {
      var Result = 0;
      var CP = 0;
      if (aCount <= aSize) {
        Result = this.Read({get: function () {
            return Buffer;
          }, set: function (v) {
            Buffer = v;
          }},aCount)}
       else {
        Result = this.Read({get: function () {
            return Buffer;
          }, set: function (v) {
            Buffer = v;
          }},aSize);
        CP = this.GetPosition();
        Result = (Result + this.Seek(aCount - aSize,1)) - CP;
      };
      return Result;
    };
    this.WriteMaxSizeData = function (Buffer, aSize, aCount) {
      var Result = 0;
      var CP = 0;
      if (aCount <= aSize) {
        Result = this.Write(Buffer,aCount)}
       else {
        Result = this.Write(Buffer,aSize);
        CP = this.GetPosition();
        Result = (Result + this.Seek(aCount - aSize,1)) - CP;
      };
      return Result;
    };
    this.Read = function (Buffer, Count) {
      var Result = 0;
      Result = this.Read$1(rtl.arrayRef(Buffer.get()),0,Count);
      return Result;
    };
    this.Write = function (Buffer, Count) {
      var Result = 0;
      Result = this.Write$1(Buffer,0,Count);
      return Result;
    };
    this.ReadData$3 = function (Buffer) {
      var Result = 0;
      Result = this.ReadData$4(Buffer,2);
      return Result;
    };
    this.ReadData$4 = function (Buffer, Count) {
      var Result = 0;
      var W = 0;
      Result = this.ReadData$12({get: function () {
          return W;
        }, set: function (v) {
          W = v;
        }},Count);
      if (Result === 2) Buffer.set(String.fromCharCode(W));
      return Result;
    };
    this.ReadData$6 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),1,Count);
      if (Result >= 1) Buffer.set(this.MakeInt(rtl.arrayRef(B),1,true));
      return Result;
    };
    this.ReadData$8 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),1,Count);
      if (Result >= 1) Buffer.set(this.MakeInt(rtl.arrayRef(B),1,false));
      return Result;
    };
    this.ReadData$10 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),2,Count);
      if (Result >= 2) Buffer.set(this.MakeInt(rtl.arrayRef(B),2,true));
      return Result;
    };
    this.ReadData$12 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),2,Count);
      if (Result >= 2) Buffer.set(this.MakeInt(rtl.arrayRef(B),2,false));
      return Result;
    };
    this.ReadData$14 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),4,Count);
      if (Result >= 4) Buffer.set(this.MakeInt(rtl.arrayRef(B),4,true));
      return Result;
    };
    this.ReadData$16 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),4,Count);
      if (Result >= 4) Buffer.set(this.MakeInt(rtl.arrayRef(B),4,false));
      return Result;
    };
    this.ReadData$18 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),8,8);
      if (Result >= 8) Buffer.set(this.MakeInt(rtl.arrayRef(B),8,true));
      return Result;
    };
    this.ReadData$22 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      var Mem = null;
      var A = null;
      var D = null;
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),8,Count);
      if (Result >= 8) {
        Mem = new ArrayBuffer(8);
        A = new Uint8Array(Mem);
        A.set(B);
        D = new DataView(Mem);
        Buffer.set(D.getFloat64(0));
      };
      return Result;
    };
    this.ReadBufferData$2 = function (Buffer) {
      this.ReadBufferData$3(Buffer,2);
    };
    this.ReadBufferData$3 = function (Buffer, Count) {
      if (this.ReadData$4(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$4 = function (Buffer) {
      this.ReadBufferData$5(Buffer,1);
    };
    this.ReadBufferData$5 = function (Buffer, Count) {
      if (this.ReadData$6(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$6 = function (Buffer) {
      this.ReadBufferData$7(Buffer,1);
    };
    this.ReadBufferData$7 = function (Buffer, Count) {
      if (this.ReadData$8(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$8 = function (Buffer) {
      this.ReadBufferData$9(Buffer,2);
    };
    this.ReadBufferData$9 = function (Buffer, Count) {
      if (this.ReadData$10(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$12 = function (Buffer) {
      this.ReadBufferData$13(Buffer,4);
    };
    this.ReadBufferData$13 = function (Buffer, Count) {
      if (this.ReadData$14(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$14 = function (Buffer) {
      this.ReadBufferData$15(Buffer,4);
    };
    this.ReadBufferData$15 = function (Buffer, Count) {
      if (this.ReadData$16(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$16 = function (Buffer) {
      this.ReadBufferData$17(Buffer,8);
    };
    this.ReadBufferData$17 = function (Buffer, Count) {
      if (this.ReadData$18(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$20 = function (Buffer) {
      this.ReadBufferData$21(Buffer,8);
    };
    this.ReadBufferData$21 = function (Buffer, Count) {
      if (this.ReadData$22(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.WriteBuffer = function (Buffer, Count) {
      this.WriteBuffer$1(Buffer,0,Count);
    };
    this.WriteBuffer$1 = function (Buffer, Offset, Count) {
      if (this.Write$1(Buffer,Offset,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteData$4 = function (Buffer, Count) {
      var Result = 0;
      var U = 0;
      U = Buffer.charCodeAt();
      Result = this.WriteData$12(U,Count);
      return Result;
    };
    this.WriteData$8 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,1,false),1,Count);
      return Result;
    };
    this.WriteData$12 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,2,true),2,Count);
      return Result;
    };
    this.WriteData$16 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,4,false),4,Count);
      return Result;
    };
    this.WriteData$18 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,8,true),8,Count);
      return Result;
    };
    this.WriteData$22 = function (Buffer, Count) {
      var Result = 0;
      var Mem = null;
      var A = null;
      var D = null;
      var B = [];
      var I = 0;
      Mem = new ArrayBuffer(8);
      D = new DataView(Mem);
      D.setFloat64(0,Buffer);
      B = rtl.arraySetLength(B,0,8);
      A = new Uint8Array(Mem);
      for (I = 0; I <= 7; I++) B[I] = A[I];
      Result = this.WriteMaxSizeData(B,8,Count);
      return Result;
    };
    this.WriteBufferData$4 = function (Buffer) {
      this.WriteBufferData$5(Buffer,2);
    };
    this.WriteBufferData$5 = function (Buffer, Count) {
      if (this.WriteData$4(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$9 = function (Buffer, Count) {
      if (this.WriteData$8(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$12 = function (Buffer) {
      this.WriteBufferData$13(Buffer,2);
    };
    this.WriteBufferData$13 = function (Buffer, Count) {
      if (this.WriteData$12(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$14 = function (Buffer) {
      this.WriteBufferData$15(Buffer,4);
    };
    this.WriteBufferData$15 = function (Buffer, Count) {
      if (this.WriteData$16(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$16 = function (Buffer) {
      this.WriteBufferData$17(Buffer,8);
    };
    this.WriteBufferData$17 = function (Buffer, Count) {
      if (this.WriteData$18(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$20 = function (Buffer) {
      this.WriteBufferData$21(Buffer,8);
    };
    this.WriteBufferData$21 = function (Buffer, Count) {
      if (this.WriteData$22(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteByte = function (b) {
      this.WriteBufferData$9(b,1);
    };
  });
  rtl.createClass(this,"TCustomMemoryStream",this.TStream,function () {
    this.$init = function () {
      $mod.TStream.$init.call(this);
      this.FMemory = null;
      this.FDataView = null;
      this.FDataArray = null;
      this.FSize = 0;
      this.FPosition = 0;
      this.FSizeBoundsSeek = false;
    };
    this.$final = function () {
      this.FMemory = undefined;
      this.FDataView = undefined;
      this.FDataArray = undefined;
      $mod.TStream.$final.call(this);
    };
    this.GetDataArray = function () {
      var Result = null;
      if (this.FDataArray === null) this.FDataArray = new Uint8Array(this.FMemory);
      Result = this.FDataArray;
      return Result;
    };
    this.GetDataView = function () {
      var Result = null;
      if (this.FDataView === null) this.FDataView = new DataView(this.FMemory);
      Result = this.FDataView;
      return Result;
    };
    this.GetSize = function () {
      var Result = 0;
      Result = this.FSize;
      return Result;
    };
    this.GetPosition = function () {
      var Result = 0;
      Result = this.FPosition;
      return Result;
    };
    this.SetPointer = function (Ptr, ASize) {
      this.FMemory = Ptr;
      this.FSize = ASize;
      this.FDataView = null;
      this.FDataArray = null;
    };
    this.MemoryToBytes = function (Mem) {
      var Result = [];
      Result = this.MemoryToBytes$1(new Uint8Array(Mem));
      return Result;
    };
    this.MemoryToBytes$1 = function (Mem) {
      var Result = [];
      var I = 0;
      for (var $l = 0, $end = Mem.length - 1; $l <= $end; $l++) {
        I = $l;
        Result[I] = Mem[I];
      };
      return Result;
    };
    this.Read$1 = function (Buffer, Offset, Count) {
      var Result = 0;
      var I = 0;
      var Src = 0;
      var Dest = 0;
      Result = 0;
      if ((this.FSize > 0) && (this.FPosition < this.FSize) && (this.FPosition >= 0)) {
        Result = Count;
        if (Result > (this.FSize - this.FPosition)) Result = this.FSize - this.FPosition;
        Src = this.FPosition;
        Dest = Offset;
        I = 0;
        while (I < Result) {
          Buffer[Dest] = this.GetDataView().getUint8(Src);
          Src += 1;
          Dest += 1;
          I += 1;
        };
        this.FPosition = this.FPosition + Result;
      };
      return Result;
    };
    this.Seek = function (Offset, Origin) {
      var Result = 0;
      var $tmp = Origin;
      if ($tmp === 0) {
        this.FPosition = Offset}
       else if ($tmp === 2) {
        this.FPosition = this.FSize + Offset}
       else if ($tmp === 1) this.FPosition = this.FPosition + Offset;
      if (this.FSizeBoundsSeek && (this.FPosition > this.FSize)) this.FPosition = this.FSize;
      Result = this.FPosition;
      return Result;
    };
  });
  rtl.createClass(this,"TMemoryStream",this.TCustomMemoryStream,function () {
    this.$init = function () {
      $mod.TCustomMemoryStream.$init.call(this);
      this.FCapacity = 0;
    };
    this.SetCapacity = function (NewCapacity) {
      this.SetPointer(this.Realloc({get: function () {
          return NewCapacity;
        }, set: function (v) {
          NewCapacity = v;
        }}),this.FSize);
      this.FCapacity = NewCapacity;
    };
    this.Realloc = function (NewCapacity) {
      var Result = null;
      var GC = 0;
      var DestView = null;
      if (NewCapacity.get() < 0) {
        NewCapacity.set(0)}
       else {
        GC = this.FCapacity + rtl.trunc(this.FCapacity / 4);
        if ((NewCapacity.get() > this.FCapacity) && (NewCapacity.get() < GC)) NewCapacity.set(GC);
        NewCapacity.set((NewCapacity.get() + (4096 - 1)) & ~(4096 - 1));
      };
      if (NewCapacity.get() === this.FCapacity) {
        Result = this.FMemory}
       else if (NewCapacity.get() === 0) {
        Result = null}
       else {
        Result = new ArrayBuffer(NewCapacity.get());
        if (Result === null) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SMemoryStreamError")]);
        DestView = new Uint8Array(Result);
        DestView.set(this.GetDataArray());
      };
      return Result;
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      this.FSize = 0;
      this.FPosition = 0;
      this.SetCapacity(0);
    };
    this.Write$1 = function (Buffer, Offset, Count) {
      var Result = 0;
      var NewPos = 0;
      if ((Count === 0) || (this.FPosition < 0)) return 0;
      NewPos = this.FPosition + Count;
      if (NewPos > this.FSize) {
        if (NewPos > this.FCapacity) this.SetCapacity(NewPos);
        this.FSize = NewPos;
      };
      this.GetDataArray().set(rtl.arrayCopy(0,Buffer,Offset,Count),this.FPosition);
      this.FPosition = NewPos;
      Result = Count;
      return Result;
    };
  });
  rtl.createClass(this,"TBytesStream",this.TMemoryStream,function () {
    this.GetBytes = function () {
      var Result = [];
      Result = $mod.TMemoryStream.MemoryToBytes(this.FMemory);
      return Result;
    };
  });
  rtl.createClass(this,"TStringStream",this.TMemoryStream,function () {
    this.Create$2 = function (aString) {
      var Len = 0;
      pas.System.TObject.Create.call(this);
      Len = aString.length;
      this.SetPointer($mod.StringToBuffer(aString,Len),Len * 2);
      this.FCapacity = Len * 2;
      return this;
    };
  });
  this.TFilerFlag = {"0": "ffInherited", ffInherited: 0, "1": "ffChildPos", ffChildPos: 1, "2": "ffInline", ffInline: 2};
  rtl.createClass(this,"TFiler",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FRoot = null;
      this.FLookupRoot = null;
      this.FAncestor = null;
    };
    this.$final = function () {
      this.FRoot = undefined;
      this.FLookupRoot = undefined;
      this.FAncestor = undefined;
      pas.System.TObject.$final.call(this);
    };
  });
  this.TValueType = {"0": "vaNull", vaNull: 0, "1": "vaList", vaList: 1, "2": "vaInt8", vaInt8: 2, "3": "vaInt16", vaInt16: 3, "4": "vaInt32", vaInt32: 4, "5": "vaDouble", vaDouble: 5, "6": "vaString", vaString: 6, "7": "vaIdent", vaIdent: 7, "8": "vaFalse", vaFalse: 8, "9": "vaTrue", vaTrue: 9, "10": "vaBinary", vaBinary: 10, "11": "vaSet", vaSet: 11, "12": "vaNil", vaNil: 12, "13": "vaCollection", vaCollection: 13, "14": "vaCurrency", vaCurrency: 14, "15": "vaDate", vaDate: 15, "16": "vaNativeInt", vaNativeInt: 16};
  rtl.createClass(this,"TAbstractObjectReader",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TBinaryObjectReader",this.TAbstractObjectReader,function () {
    this.$init = function () {
      $mod.TAbstractObjectReader.$init.call(this);
      this.FStream = null;
    };
    this.$final = function () {
      this.FStream = undefined;
      $mod.TAbstractObjectReader.$final.call(this);
    };
    this.ReadDWord = function () {
      var Result = 0;
      this.FStream.ReadBufferData$14({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.SkipProperty = function () {
      this.ReadStr();
      this.SkipValue();
    };
    this.SkipSetBody = function () {
      while (this.ReadStr().length > 0) {
      };
    };
    this.Create$1 = function (Stream) {
      pas.System.TObject.Create.call(this);
      if (Stream === null) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SEmptyStreamIllegalReader")]);
      this.FStream = Stream;
      return this;
    };
    this.NextValue = function () {
      var Result = 0;
      Result = this.ReadValue();
      this.FStream.Seek(-1,1);
      return Result;
    };
    this.ReadValue = function () {
      var Result = 0;
      var b = 0;
      this.FStream.ReadBufferData$6({get: function () {
          return b;
        }, set: function (v) {
          b = v;
        }});
      Result = b;
      return Result;
    };
    this.BeginRootComponent = function () {
      this.ReadSignature();
    };
    this.BeginComponent = function (Flags, AChildPos, CompClassName, CompName) {
      var Prefix = 0;
      var ValueType = 0;
      Flags.set({});
      if ((this.NextValue() & 0xf0) === 0xf0) {
        Prefix = this.ReadValue();
        Flags.set({});
        if ((Prefix & 0x1) !== 0) Flags.set(rtl.includeSet(Flags.get(),0));
        if ((Prefix & 0x2) !== 0) Flags.set(rtl.includeSet(Flags.get(),1));
        if ((Prefix & 0x4) !== 0) Flags.set(rtl.includeSet(Flags.get(),2));
        if (1 in Flags.get()) {
          ValueType = this.ReadValue();
          var $tmp = ValueType;
          if ($tmp === 2) {
            AChildPos.set(this.ReadInt8())}
           else if ($tmp === 3) {
            AChildPos.set(this.ReadInt16())}
           else if ($tmp === 4) {
            AChildPos.set(this.ReadInt32())}
           else if ($tmp === 16) {
            AChildPos.set(this.ReadNativeInt())}
           else {
            throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
          };
        };
      };
      CompClassName.set(this.ReadStr());
      CompName.set(this.ReadStr());
    };
    this.BeginProperty = function () {
      var Result = "";
      Result = this.ReadStr();
      return Result;
    };
    this.Read = function (Buffer, Count) {
      this.FStream.Read(Buffer,Count);
    };
    this.ReadFloat = function () {
      var Result = 0.0;
      this.FStream.ReadBufferData$20({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadCurrency = function () {
      var Result = 0;
      Result = rtl.trunc(this.ReadFloat() * 10000);
      return Result;
    };
    this.ReadIdent = function (ValueType) {
      var Result = "";
      var i = 0;
      var c = "";
      var $tmp = ValueType;
      if ($tmp === 7) {
        this.FStream.ReadBufferData$6({get: function () {
            return i;
          }, set: function (v) {
            i = v;
          }});
        Result = rtl.strSetLength(Result,i);
        for (var $l = 1, $end = Result.length; $l <= $end; $l++) {
          i = $l;
          this.FStream.ReadBufferData$2({get: function () {
              return c;
            }, set: function (v) {
              c = v;
            }});
          Result = rtl.setCharAt(Result,i - 1,c);
        };
      } else if ($tmp === 12) {
        Result = "nil"}
       else if ($tmp === 8) {
        Result = "False"}
       else if ($tmp === 9) {
        Result = "True"}
       else if ($tmp === 0) Result = "Null";
      return Result;
    };
    this.ReadInt8 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$4({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadInt16 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$8({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadInt32 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadNativeInt = function () {
      var Result = 0;
      this.FStream.ReadBufferData$16({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadSet = function (EnumType) {
      var Result = 0;
      var Name = "";
      var Value = 0;
      try {
        Result = 0;
        while (true) {
          Name = this.ReadStr();
          if (Name.length === 0) break;
          Value = EnumType.enumtype[Name];
          if (Value === -1) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
          Result = Result | (1 << Value);
        };
      } catch ($e) {
        this.SkipSetBody();
        throw $e;
      };
      return Result;
    };
    this.ReadSignature = function () {
      var Signature = 0;
      this.FStream.ReadBufferData$12({get: function () {
          return Signature;
        }, set: function (v) {
          Signature = v;
        }});
      if (Signature !== 809914452) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidImage")]);
    };
    this.ReadStr = function () {
      var Result = "";
      var l = 0;
      var i = 0;
      var c = "";
      this.FStream.ReadBufferData$6({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }});
      Result = rtl.strSetLength(Result,l);
      for (var $l = 1, $end = l; $l <= $end; $l++) {
        i = $l;
        this.FStream.ReadBufferData$2({get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        Result = rtl.setCharAt(Result,i - 1,c);
      };
      return Result;
    };
    this.ReadString = function (StringType) {
      var Result = "";
      var i = 0;
      var C = "";
      Result = "";
      if (StringType !== 6) throw $mod.EFilerError.$create("Create$1",["Invalid string type passed to ReadString"]);
      i = this.ReadDWord();
      Result = rtl.strSetLength(Result,i);
      for (var $l = 1, $end = Result.length; $l <= $end; $l++) {
        i = $l;
        this.FStream.ReadBufferData$2({get: function () {
            return C;
          }, set: function (v) {
            C = v;
          }});
        Result = rtl.setCharAt(Result,i - 1,C);
      };
      return Result;
    };
    this.SkipComponent = function (SkipComponentInfos) {
      var Flags = {};
      var Dummy = 0;
      var CompClassName = "";
      var CompName = "";
      if (SkipComponentInfos) this.BeginComponent({get: function () {
          return Flags;
        }, set: function (v) {
          Flags = v;
        }},{get: function () {
          return Dummy;
        }, set: function (v) {
          Dummy = v;
        }},{get: function () {
          return CompClassName;
        }, set: function (v) {
          CompClassName = v;
        }},{get: function () {
          return CompName;
        }, set: function (v) {
          CompName = v;
        }});
      while (this.NextValue() !== 0) this.SkipProperty();
      this.ReadValue();
      while (this.NextValue() !== 0) this.SkipComponent(true);
      this.ReadValue();
    };
    this.SkipValue = function () {
      var $Self = this;
      function SkipBytes(Count) {
        var Dummy = [];
        var SkipNow = 0;
        while (Count > 0) {
          if (Count > 1024) {
            SkipNow = 1024}
           else SkipNow = Count;
          Dummy = rtl.arraySetLength(Dummy,0,SkipNow);
          $Self.Read({get: function () {
              return Dummy;
            }, set: function (v) {
              Dummy = v;
            }},SkipNow);
          Count -= SkipNow;
        };
      };
      var Count = 0;
      var $tmp = this.ReadValue();
      if (($tmp === 0) || ($tmp === 8) || ($tmp === 9) || ($tmp === 12)) {}
      else if ($tmp === 1) {
        while (this.NextValue() !== 0) this.SkipValue();
        this.ReadValue();
      } else if ($tmp === 2) {
        SkipBytes(1)}
       else if ($tmp === 3) {
        SkipBytes(2)}
       else if ($tmp === 4) {
        SkipBytes(4)}
       else if (($tmp === $mod.TValueType.vaNativeInt) || ($tmp === 5)) {
        SkipBytes(8)}
       else if ($tmp === 7) {
        this.ReadStr()}
       else if ($tmp === 6) {
        this.ReadString(6)}
       else if ($tmp === 10) {
        Count = this.ReadDWord() & 0xFFFFFFFF;
        SkipBytes(Count);
      } else if ($tmp === 11) {
        this.SkipSetBody()}
       else if ($tmp === 13) {
        while (this.NextValue() !== 0) {
          if (this.NextValue() in rtl.createSet(2,3,4)) this.SkipValue();
          SkipBytes(1);
          while (this.NextValue() !== 0) this.SkipProperty();
          this.ReadValue();
        };
        this.ReadValue();
      };
    };
  });
  rtl.createClass(this,"TReader",this.TFiler,function () {
    this.$init = function () {
      $mod.TFiler.$init.call(this);
      this.FDriver = null;
      this.FOwner = null;
      this.FParent = null;
      this.FFixups = null;
      this.FLoaded = null;
      this.FOnFindMethod = null;
      this.FOnSetMethodProperty = null;
      this.FOnSetName = null;
      this.FOnReferenceName = null;
      this.FOnAncestorNotFound = null;
      this.FOnError = null;
      this.FOnPropertyNotFound = null;
      this.FOnFindComponentClass = null;
      this.FOnCreateComponent = null;
      this.FPropName = "";
      this.FCanHandleExcepts = false;
      this.FOnReadStringProperty = null;
    };
    this.$final = function () {
      this.FDriver = undefined;
      this.FOwner = undefined;
      this.FParent = undefined;
      this.FFixups = undefined;
      this.FLoaded = undefined;
      this.FOnFindMethod = undefined;
      this.FOnSetMethodProperty = undefined;
      this.FOnSetName = undefined;
      this.FOnReferenceName = undefined;
      this.FOnAncestorNotFound = undefined;
      this.FOnError = undefined;
      this.FOnPropertyNotFound = undefined;
      this.FOnFindComponentClass = undefined;
      this.FOnCreateComponent = undefined;
      this.FOnReadStringProperty = undefined;
      $mod.TFiler.$final.call(this);
    };
    this.DoFixupReferences = function () {
      var R = null;
      var RN = null;
      var G = null;
      var Ref = "";
      var C = null;
      var P = 0;
      var L = null;
      var $ir = rtl.createIntfRefs();
      try {
        if (this.FFixups != null) {
          L = this.FFixups;
          R = L.FRoot;
          while (R !== null) {
            RN = R.Next;
            Ref = R.FRelative;
            if (this.FOnReferenceName != null) this.FOnReferenceName(this,{get: function () {
                return Ref;
              }, set: function (v) {
                Ref = v;
              }});
            C = $mod.FindNestedComponent(R.FRoot,Ref,true);
            if (C != null) {
              if (R.FPropInfo.typeinfo.kind === 18) {
                pas.TypInfo.SetInterfaceProp$1(R.Finstance,R.FPropInfo,$ir.ref(1,rtl.queryIntfT(C,pas.System.IUnknown)))}
               else pas.TypInfo.SetObjectProp$1(R.Finstance,R.FPropInfo,C)}
             else {
              P = pas.System.Pos(".",R.FRelative);
              if (P !== 0) {
                G = $impl.AddtoResolveList(R.Finstance);
                G.AddReference(R.FRoot,R.FPropInfo,pas.System.Copy(R.FRelative,1,P - 1),pas.System.Copy(R.FRelative,P + 1,R.FRelative.length - P));
              };
            };
            L.RemoveItem(R,true);
            R = RN;
          };
          pas.SysUtils.FreeAndNil({p: this, get: function () {
              return this.p.FFixups;
            }, set: function (v) {
              this.p.FFixups = v;
            }});
        };
      } finally {
        $ir.free();
      };
    };
    this.FindComponentClass = function (AClassName) {
      var $Self = this;
      var Result = null;
      var PersistentClass = null;
      function FindClassInFieldTable(Instance) {
        var Result = null;
        var aClass = null;
        var i = 0;
        var ClassTI = null;
        var MemberClassTI = null;
        var MemberTI = null;
        aClass = Instance.$class.ClassType();
        while (aClass !== null) {
          ClassTI = aClass.$rtti;
          for (var $l = 0, $end = ClassTI.fields.length - 1; $l <= $end; $l++) {
            i = $l;
            MemberTI = ClassTI.getField(i).typeinfo;
            if (MemberTI.kind === 13) {
              MemberClassTI = MemberTI;
              if (pas.SysUtils.SameText(MemberClassTI.name,AClassName) && rtl.is(MemberClassTI.class,$mod.TComponent)) return MemberClassTI.class;
            };
          };
          aClass = aClass.$ancestor;
        };
        return Result;
      };
      Result = null;
      Result = FindClassInFieldTable(this.FRoot);
      if ((Result === null) && (this.FLookupRoot != null) && (this.FLookupRoot !== this.FRoot)) Result = FindClassInFieldTable(this.FLookupRoot);
      if (Result === null) {
        PersistentClass = $mod.GetClass(AClassName);
        if ((PersistentClass != null) && PersistentClass.InheritsFrom($mod.TComponent)) Result = PersistentClass;
      };
      if ((Result === null) && (this.FOnFindComponentClass != null)) this.FOnFindComponentClass($Self,AClassName,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      if ((Result === null) || !Result.InheritsFrom($mod.TComponent)) throw $mod.EClassNotFound.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SClassNotFound"),pas.System.VarRecs(18,AClassName)]);
      return Result;
    };
    this.Error = function (Message) {
      var Result = false;
      Result = false;
      if (this.FOnError != null) this.FOnError(this,Message,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.FindMethod = function (ARoot, AMethodName) {
      var Result = null;
      var ErrorResult = false;
      Result = null;
      if ((ARoot === null) || (AMethodName === "")) return Result;
      Result = ARoot.$class.MethodAddress(AMethodName);
      ErrorResult = Result === null;
      if (this.FOnFindMethod != null) this.FOnFindMethod(this,AMethodName,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return ErrorResult;
        }, set: function (v) {
          ErrorResult = v;
        }});
      if (ErrorResult) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadProperty = function (AInstance) {
      var $Self = this;
      var Path = "";
      var Instance = null;
      var PropInfo = null;
      var Obj = null;
      var Name = "";
      var Skip = false;
      var Handled = false;
      var OldPropName = "";
      var DotPos = "";
      var NextPos = 0;
      function HandleMissingProperty(IsPath) {
        var Result = false;
        Result = true;
        if ($Self.FOnPropertyNotFound != null) {
          OldPropName = $Self.FPropName;
          Handled = false;
          Skip = false;
          $Self.FOnPropertyNotFound($Self,Instance,{p: $Self, get: function () {
              return this.p.FPropName;
            }, set: function (v) {
              this.p.FPropName = v;
            }},IsPath,{get: function () {
              return Handled;
            }, set: function (v) {
              Handled = v;
            }},{get: function () {
              return Skip;
            }, set: function (v) {
              Skip = v;
            }});
          if (Handled && !Skip && (OldPropName !== $Self.FPropName)) PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),$Self.FPropName);
          if (Skip) {
            $Self.FDriver.SkipValue();
            Result = false;
            return Result;
          };
        };
        return Result;
      };
      try {
        Path = this.FDriver.BeginProperty();
        try {
          Instance = AInstance;
          this.FCanHandleExcepts = true;
          DotPos = Path;
          while (true) {
            NextPos = pas.System.Pos(".",DotPos);
            if (NextPos > 0) {
              this.FPropName = pas.System.Copy(DotPos,1,NextPos - 1)}
             else {
              this.FPropName = DotPos;
              break;
            };
            pas.System.Delete({get: function () {
                return DotPos;
              }, set: function (v) {
                DotPos = v;
              }},1,NextPos);
            PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),this.FPropName);
            if (!(PropInfo != null)) {
              if (!HandleMissingProperty(true)) return;
              if (!(PropInfo != null)) this.PropertyError();
            };
            if (PropInfo.typeinfo.kind === 13) {
              Obj = pas.TypInfo.GetObjectProp$2(Instance,PropInfo)}
             else Obj = null;
            if (!$mod.TPersistent.isPrototypeOf(Obj)) {
              this.FDriver.SkipValue();
              throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyPath")]);
            };
            Instance = Obj;
          };
          PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),this.FPropName);
          if (PropInfo != null) {
            this.ReadPropValue(Instance,PropInfo)}
           else {
            this.FCanHandleExcepts = false;
            Instance.DefineProperties($Self);
            this.FCanHandleExcepts = true;
            if (this.FPropName.length > 0) {
              if (!HandleMissingProperty(false)) return;
              if (!(PropInfo != null)) this.PropertyError();
            };
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var e = $e;
            Name = rtl.strSetLength(Name,0);
            if (AInstance.$class.InheritsFrom($mod.TComponent)) Name = AInstance.FName;
            if (Name.length === 0) Name = AInstance.$classname;
            throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SPropertyException"),pas.System.VarRecs(18,Name,9,".",18,Path,18,e.fMessage)]);
          } else throw $e
        };
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var e = $e;
          if (!this.FCanHandleExcepts || !this.Error(e.fMessage)) throw $e;
        } else throw $e
      };
    };
    var NullMethod = pas.System.TMethod.$clone({Code: null, Data: null});
    this.ReadPropValue = function (Instance, PropInfo) {
      var PropType = null;
      var Value = 0;
      var Ident = "";
      var Method = pas.System.TMethod.$new();
      var Handled = false;
      var TmpStr = "";
      if (PropInfo.setter === "") throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SReadOnlyProperty")]);
      PropType = PropInfo.typeinfo;
      var $tmp = PropType.kind;
      if ($tmp === 1) {
        var $tmp1 = this.FDriver.NextValue();
        if ($tmp1 === 7) {
          Ident = this.ReadIdent();
          if ($impl.GlobalIdentToInt(Ident,{get: function () {
              return Value;
            }, set: function (v) {
              Value = v;
            }})) {
            pas.TypInfo.SetOrdProp$1(Instance,PropInfo,Value)}
           else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
        } else if ($tmp1 === 16) {
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadNativeInt())}
         else if ($tmp1 === 14) {
          pas.TypInfo.SetFloatProp$1(Instance,PropInfo,this.ReadCurrency() / 10000)}
         else {
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadInteger());
        };
      } else if ($tmp === 7) {
        pas.TypInfo.SetBoolProp$1(Instance,PropInfo,this.ReadBoolean())}
       else if ($tmp === 2) {
        pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadChar().charCodeAt())}
       else if ($tmp === 4) {
        Value = pas.TypInfo.GetEnumValue(PropType,this.ReadIdent());
        if (Value === -1) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
        pas.TypInfo.SetOrdProp$1(Instance,PropInfo,Value);
      } else if ($tmp === pas.System.TTypeKind.tkDouble) {
        pas.TypInfo.SetFloatProp$1(Instance,PropInfo,this.ReadFloat())}
       else if ($tmp === 5) {
        this.CheckValue(11);
        if (PropType.comptype.kind === 4) pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.FDriver.ReadSet(PropType.comptype));
      } else if (($tmp === 9) || ($tmp === 17)) {
        if (this.FDriver.NextValue() === 12) {
          this.FDriver.ReadValue();
          pas.TypInfo.SetMethodProp(Instance,PropInfo,NullMethod);
        } else {
          Handled = false;
          Ident = this.ReadIdent();
          if (this.FOnSetMethodProperty != null) this.FOnSetMethodProperty(this,Instance,PropInfo,Ident,{get: function () {
              return Handled;
            }, set: function (v) {
              Handled = v;
            }});
          if (!Handled) {
            Method.Code = this.FindMethod(this.FRoot,Ident);
            Method.Data = this.FRoot;
            if (Method.Code != null) pas.TypInfo.SetMethodProp(Instance,PropInfo,Method);
          };
        }}
       else if ($tmp === 3) {
        TmpStr = this.ReadString();
        if (this.FOnReadStringProperty != null) this.FOnReadStringProperty(this,Instance,PropInfo,{get: function () {
            return TmpStr;
          }, set: function (v) {
            TmpStr = v;
          }});
        pas.TypInfo.SetStrProp$1(Instance,PropInfo,TmpStr);
      } else if ($tmp === 16) {
        pas.TypInfo.SetJSValueProp$3(Instance,PropInfo,this.ReadVariant());
      } else if (($tmp === 13) || ($tmp === 18)) {
        var $tmp2 = this.FDriver.NextValue();
        if ($tmp2 === 12) {
          this.FDriver.ReadValue();
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,0);
        } else if ($tmp2 === 13) {
          this.FDriver.ReadValue();
          this.ReadCollection(pas.TypInfo.GetObjectProp$2(Instance,PropInfo));
        } else {
          if (!(this.FFixups != null)) this.FFixups = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TLocalUnResolvedReference]);
          var $with = this.FFixups.Add();
          $with.Finstance = Instance;
          $with.FRoot = this.FRoot;
          $with.FPropInfo = PropInfo;
          $with.FRelative = this.ReadIdent();
        };
      } else {
        throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnknownPropertyType"),pas.System.VarRecs(18,pas.System.TTypeKind[PropType.kind])]);
      };
    };
    this.PropertyError = function () {
      this.FDriver.SkipValue();
      throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnknownProperty"),pas.System.VarRecs(18,this.FPropName)]);
    };
    this.ReadData = function (Instance) {
      var SavedOwner = null;
      var SavedParent = null;
      while (!this.EndOfList()) this.ReadProperty(Instance);
      this.ReadListEnd();
      SavedOwner = this.FOwner;
      SavedParent = this.FParent;
      try {
        this.FOwner = Instance.GetChildOwner();
        if (!(this.FOwner != null)) this.FOwner = this.FRoot;
        this.FParent = Instance.GetChildParent();
        while (!this.EndOfList()) this.ReadComponent(null);
        this.ReadListEnd();
      } finally {
        this.FOwner = SavedOwner;
        this.FParent = SavedParent;
      };
      if (Instance === this.FRoot) this.DoFixupReferences();
    };
    this.CreateDriver = function (Stream) {
      var Result = null;
      Result = $mod.TBinaryObjectReader.$create("Create$1",[Stream]);
      return Result;
    };
    this.Create$1 = function (Stream) {
      pas.System.TObject.Create.call(this);
      if (Stream === null) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SEmptyStreamIllegalReader")]);
      this.FDriver = this.CreateDriver(Stream);
      return this;
    };
    this.Destroy = function () {
      rtl.free(this,"FDriver");
      pas.System.TObject.Destroy.call(this);
    };
    this.CheckValue = function (Value) {
      if (this.FDriver.NextValue() !== Value) {
        throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")])}
       else this.FDriver.ReadValue();
    };
    this.DefineProperty = function (Name, AReadData, WriteData, HasData) {
      if ((AReadData != null) && pas.SysUtils.SameText(Name,this.FPropName)) {
        AReadData(this);
        this.FPropName = rtl.strSetLength(this.FPropName,0);
      } else if ((WriteData != null) && HasData) ;
    };
    this.EndOfList = function () {
      var Result = false;
      Result = this.FDriver.NextValue() === 0;
      return Result;
    };
    this.NextValue = function () {
      var Result = 0;
      Result = this.FDriver.NextValue();
      return Result;
    };
    this.ReadBoolean = function () {
      var Result = false;
      var ValueType = 0;
      ValueType = this.FDriver.ReadValue();
      if (ValueType === 9) {
        Result = true}
       else if (ValueType === 8) {
        Result = false}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadChar = function () {
      var Result = "";
      var s = "";
      s = this.ReadString();
      if (s.length === 1) {
        Result = s.charAt(0)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadCollection = function (Collection) {
      var Item = null;
      Collection.BeginUpdate();
      if (!this.EndOfList()) Collection.Clear();
      while (!this.EndOfList()) {
        this.ReadListBegin();
        Item = Collection.Add();
        while (this.NextValue() !== 0) this.ReadProperty(Item);
        this.ReadListEnd();
      };
      Collection.EndUpdate();
      this.ReadListEnd();
    };
    this.ReadComponent = function (Component) {
      var $Self = this;
      var Result = null;
      var Flags = {};
      function Recover(E, aComponent) {
        var Result = false;
        Result = false;
        if (!((0 in Flags) || (Component != null))) aComponent.set(rtl.freeLoc(aComponent.get()));
        aComponent.set(null);
        $Self.FDriver.SkipComponent(false);
        Result = $Self.Error(E.fMessage);
        return Result;
      };
      var CompClassName = "";
      var Name = "";
      var n = 0;
      var ChildPos = 0;
      var SavedParent = null;
      var SavedLookupRoot = null;
      var ComponentClass = null;
      var C = null;
      var NewComponent = null;
      var SubComponents = null;
      this.FDriver.BeginComponent({get: function () {
          return Flags;
        }, set: function (v) {
          Flags = v;
        }},{get: function () {
          return ChildPos;
        }, set: function (v) {
          ChildPos = v;
        }},{get: function () {
          return CompClassName;
        }, set: function (v) {
          CompClassName = v;
        }},{get: function () {
          return Name;
        }, set: function (v) {
          Name = v;
        }});
      SavedParent = this.FParent;
      SavedLookupRoot = this.FLookupRoot;
      SubComponents = null;
      try {
        Result = Component;
        if (!(Result != null)) try {
          if (0 in Flags) {
            if (this.FLookupRoot != null) {
              Result = this.FLookupRoot.FindComponent(Name)}
             else Result = null;
            if (!(Result != null)) {
              if (this.FOnAncestorNotFound != null) this.FOnAncestorNotFound($Self,Name,this.FindComponentClass(CompClassName),{get: function () {
                  return Result;
                }, set: function (v) {
                  Result = v;
                }});
              if (!(Result != null)) throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SAncestorNotFound"),pas.System.VarRecs(18,Name)]);
            };
            this.FParent = Result.GetParentComponent();
            if (!(this.FParent != null)) this.FParent = this.FRoot;
          } else {
            Result = null;
            ComponentClass = this.FindComponentClass(CompClassName);
            if (this.FOnCreateComponent != null) this.FOnCreateComponent($Self,ComponentClass,{get: function () {
                return Result;
              }, set: function (v) {
                Result = v;
              }});
            if (!(Result != null)) {
              NewComponent = Object.create(ComponentClass);
              NewComponent.$init();
              if (2 in Flags) NewComponent.FComponentState = rtl.unionSet(NewComponent.FComponentState,rtl.createSet(0,9));
              NewComponent.Create$1(this.FOwner);
              NewComponent.AfterConstruction();
              Result = NewComponent;
            };
            Result.FComponentState = rtl.includeSet(Result.FComponentState,0);
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var E = $e;
            if (!Recover(E,{get: function () {
                return Result;
              }, set: function (v) {
                Result = v;
              }})) throw $e;
          } else throw $e
        };
        if (Result != null) try {
          Result.FComponentState = rtl.includeSet(Result.FComponentState,0);
          SubComponents = $mod.TList.$create("Create$1");
          for (var $l = 0, $end = Result.GetComponentCount() - 1; $l <= $end; $l++) {
            n = $l;
            C = Result.GetComponent(n);
            if (2 in C.FComponentStyle) {
              SubComponents.Add(C);
              C.FComponentState = rtl.includeSet(C.FComponentState,0);
            };
          };
          if (!(0 in Flags)) try {
            Result.SetParentComponent(this.FParent);
            if (this.FOnSetName != null) this.FOnSetName($Self,Result,{get: function () {
                return Name;
              }, set: function (v) {
                Name = v;
              }});
            Result.SetName(Name);
            if ($mod.FindGlobalComponent(Name) === Result) Result.FComponentState = rtl.includeSet(Result.FComponentState,9);
          } catch ($e) {
            if (pas.SysUtils.Exception.isPrototypeOf($e)) {
              var E = $e;
              if (!Recover(E,{get: function () {
                  return Result;
                }, set: function (v) {
                  Result = v;
                }})) throw $e;
            } else throw $e
          };
          if (!(Result != null)) return Result;
          if (9 in Result.FComponentState) this.FLookupRoot = Result;
          Result.FComponentState = rtl.includeSet(Result.FComponentState,1);
          for (var $l1 = 0, $end1 = SubComponents.GetCount() - 1; $l1 <= $end1; $l1++) {
            n = $l1;
            rtl.getObject(SubComponents.Get(n)).FComponentState = rtl.includeSet(rtl.getObject(SubComponents.Get(n)).FComponentState,1);
          };
          Result.ReadState($Self);
          Result.FComponentState = rtl.excludeSet(Result.FComponentState,1);
          for (var $l2 = 0, $end2 = SubComponents.GetCount() - 1; $l2 <= $end2; $l2++) {
            n = $l2;
            rtl.getObject(SubComponents.Get(n)).FComponentState = rtl.excludeSet(rtl.getObject(SubComponents.Get(n)).FComponentState,1);
          };
          if (1 in Flags) this.FParent.SetChildOrder(Result,ChildPos);
          if (!((0 in Flags) || (9 in Result.FComponentState)) || (this.FLoaded.IndexOf(Result) < 0)) {
            for (var $l3 = 0, $end3 = SubComponents.GetCount() - 1; $l3 <= $end3; $l3++) {
              n = $l3;
              this.FLoaded.Add(SubComponents.Get(n));
            };
            this.FLoaded.Add(Result);
          };
        } catch ($e) {
          if ((0 in Flags) || (Component != null)) Result = rtl.freeLoc(Result);
          throw $e;
        };
      } finally {
        this.FParent = SavedParent;
        this.FLookupRoot = SavedLookupRoot;
        SubComponents = rtl.freeLoc(SubComponents);
      };
      return Result;
    };
    this.ReadFloat = function () {
      var Result = 0.0;
      if (this.FDriver.NextValue() === $mod.TValueType.vaDouble) {
        this.ReadValue();
        Result = this.FDriver.ReadFloat();
      } else Result = this.ReadNativeInt();
      return Result;
    };
    this.ReadCurrency = function () {
      var Result = 0;
      if (this.FDriver.NextValue() === 14) {
        this.FDriver.ReadValue();
        Result = this.FDriver.ReadCurrency();
      } else Result = this.ReadInteger() * 10000;
      return Result;
    };
    this.ReadIdent = function () {
      var Result = "";
      var ValueType = 0;
      ValueType = this.FDriver.ReadValue();
      if (ValueType in rtl.createSet(7,12,8,9,0)) {
        Result = this.FDriver.ReadIdent(ValueType)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadInteger = function () {
      var Result = 0;
      var $tmp = this.FDriver.ReadValue();
      if ($tmp === 2) {
        Result = this.FDriver.ReadInt8()}
       else if ($tmp === 3) {
        Result = this.FDriver.ReadInt16()}
       else if ($tmp === 4) {
        Result = this.FDriver.ReadInt32()}
       else {
        throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      };
      return Result;
    };
    this.ReadNativeInt = function () {
      var Result = 0;
      if (this.FDriver.NextValue() === $mod.TValueType.vaNativeInt) {
        this.FDriver.ReadValue();
        Result = this.FDriver.ReadNativeInt();
      } else Result = this.ReadInteger();
      return Result;
    };
    this.ReadListBegin = function () {
      this.CheckValue(1);
    };
    this.ReadListEnd = function () {
      this.CheckValue(0);
    };
    this.ReadRootComponent = function (ARoot) {
      var Result = null;
      var Dummy = 0;
      var i = 0;
      var Flags = {};
      var CompClassName = "";
      var CompName = "";
      var ResultName = "";
      this.FDriver.BeginRootComponent();
      Result = null;
      try {
        this.FDriver.BeginComponent({get: function () {
            return Flags;
          }, set: function (v) {
            Flags = v;
          }},{get: function () {
            return Dummy;
          }, set: function (v) {
            Dummy = v;
          }},{get: function () {
            return CompClassName;
          }, set: function (v) {
            CompClassName = v;
          }},{get: function () {
            return CompName;
          }, set: function (v) {
            CompName = v;
          }});
        if (!(ARoot != null)) {
          Result = $mod.FindClass(CompClassName).$create("Create$1",[null]);
          Result.SetName(CompName);
        } else {
          Result = ARoot;
          if (!(4 in Result.FComponentState)) {
            Result.FComponentState = rtl.unionSet(Result.FComponentState,rtl.createSet(0,1));
            i = 0;
            ResultName = CompName;
            while ($mod.FindGlobalComponent(ResultName) != null) {
              i += 1;
              ResultName = CompName + "_" + pas.SysUtils.IntToStr(i);
            };
            Result.SetName(ResultName);
          };
        };
        this.FRoot = Result;
        this.FLookupRoot = Result;
        if ($impl.GlobalLoaded != null) {
          this.FLoaded = $impl.GlobalLoaded}
         else this.FLoaded = $mod.TFPList.$create("Create");
        try {
          if (this.FLoaded.IndexOf(this.FRoot) < 0) this.FLoaded.Add(this.FRoot);
          this.FOwner = this.FRoot;
          this.FRoot.FComponentState = rtl.unionSet(this.FRoot.FComponentState,rtl.createSet(0,1));
          this.FRoot.ReadState(this);
          this.FRoot.FComponentState = rtl.excludeSet(this.FRoot.FComponentState,1);
          if (!($impl.GlobalLoaded != null)) for (var $l = 0, $end = this.FLoaded.FCount - 1; $l <= $end; $l++) {
            i = $l;
            rtl.getObject(this.FLoaded.Get(i)).Loaded();
          };
        } finally {
          if (!($impl.GlobalLoaded != null)) rtl.free(this,"FLoaded");
          this.FLoaded = null;
        };
        $impl.GlobalFixupReferences();
      } catch ($e) {
        $mod.RemoveFixupReferences(ARoot,"");
        if (!(ARoot != null)) Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.ReadVariant = function () {
      var Result = undefined;
      var nv = 0;
      nv = this.NextValue();
      var $tmp = nv;
      if ($tmp === 12) {
        Result = undefined;
        this.ReadValue();
      } else if ($tmp === 0) {
        Result = null;
        this.ReadValue();
      } else if (($tmp === 2) || ($tmp === 3) || ($tmp === 4)) {
        Result = this.ReadInteger();
      } else if ($tmp === $mod.TValueType.vaNativeInt) {
        Result = this.ReadNativeInt();
      } else if (($tmp === 8) || ($tmp === 9)) {
        Result = nv !== 8;
        this.ReadValue();
      } else if ($tmp === 14) {
        Result = this.ReadCurrency() / 10000;
      } else if ($tmp === 5) {
        Result = this.ReadFloat();
      } else if ($tmp === 6) {
        Result = this.ReadString();
      } else {
        throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnsupportedPropertyVariantType"),pas.System.VarRecs(0,nv)]);
      };
      return Result;
    };
    this.ReadString = function () {
      var Result = "";
      var StringType = 0;
      StringType = this.FDriver.ReadValue();
      if (StringType === 6) {
        Result = this.FDriver.ReadString(StringType)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadValue = function () {
      var Result = 0;
      Result = this.FDriver.ReadValue();
      return Result;
    };
  });
  rtl.createClass(this,"TAbstractObjectWriter",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TWriter",this.TFiler,function () {
    this.$init = function () {
      $mod.TFiler.$init.call(this);
      this.FDriver = null;
      this.FDestroyDriver = false;
      this.FPropPath = "";
    };
    this.$final = function () {
      this.FDriver = undefined;
      $mod.TFiler.$final.call(this);
    };
    this.Destroy = function () {
      if (this.FDestroyDriver) rtl.free(this,"FDriver");
      pas.System.TObject.Destroy.call(this);
    };
    this.DefineProperty = function (Name, ReadData, AWriteData, HasData) {
      if (HasData && (AWriteData != null)) {
        this.FDriver.BeginProperty(this.FPropPath + Name);
        AWriteData(this);
        this.FDriver.EndProperty();
      } else if (ReadData != null) ;
    };
    this.WriteInteger = function (Value) {
      this.FDriver.WriteInteger(Value);
    };
  });
  this.TParserToken = {"0": "toUnknown", toUnknown: 0, "1": "toEOF", toEOF: 1, "2": "toSymbol", toSymbol: 2, "3": "ToString", ToString: 3, "4": "toInteger", toInteger: 4, "5": "toFloat", toFloat: 5, "6": "toMinus", toMinus: 6, "7": "toSetStart", toSetStart: 7, "8": "toListStart", toListStart: 8, "9": "toCollectionStart", toCollectionStart: 9, "10": "toBinaryStart", toBinaryStart: 10, "11": "toSetEnd", toSetEnd: 11, "12": "toListEnd", toListEnd: 12, "13": "toCollectionEnd", toCollectionEnd: 13, "14": "toBinaryEnd", toBinaryEnd: 14, "15": "toComma", toComma: 15, "16": "toDot", toDot: 16, "17": "toEqual", toEqual: 17, "18": "toColon", toColon: 18, "19": "toPlus", toPlus: 19};
  rtl.createClass(this,"TParser",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fStream = null;
      this.fBuf = [];
      this.FBufLen = 0;
      this.fPos = 0;
      this.fDeltaPos = 0;
      this.fFloatType = "";
      this.fSourceLine = 0;
      this.fToken = 0;
      this.fEofReached = false;
      this.fLastTokenStr = "";
    };
    this.$final = function () {
      this.fStream = undefined;
      this.fBuf = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.GetTokenName = function (aTok) {
      var Result = "";
      Result = $impl.TokNames[aTok];
      return Result;
    };
    this.LoadBuffer = function () {
      var CharsRead = 0;
      var i = 0;
      CharsRead = 0;
      for (i = 0; i <= 4095; i++) {
        if (this.fStream.ReadData$3({a: i, p: this.fBuf, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }}) !== 2) break;
        CharsRead += 1;
      };
      this.fDeltaPos += CharsRead;
      this.fPos = 0;
      this.FBufLen = CharsRead;
      this.fEofReached = CharsRead === 0;
    };
    this.CheckLoadBuffer = function () {
      if (this.fPos >= this.FBufLen) this.LoadBuffer();
    };
    this.ProcessChar = function () {
      this.fLastTokenStr = this.fLastTokenStr + this.fBuf[this.fPos];
      this.GotoToNextChar();
    };
    this.IsNumber = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57);
      return Result;
    };
    this.IsHexNum = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57,null,65,70,null,97,102);
      return Result;
    };
    this.IsAlpha = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(95,null,65,90,null,97,122);
      return Result;
    };
    this.IsAlphaNum = function () {
      var Result = false;
      Result = this.IsAlpha() || this.IsNumber();
      return Result;
    };
    this.GetHexValue = function (c) {
      var Result = 0;
      var $tmp = c;
      if (($tmp >= "0") && ($tmp <= "9")) {
        Result = c.charCodeAt() - 0x30}
       else if (($tmp >= "A") && ($tmp <= "F")) {
        Result = c.charCodeAt() - 0x37}
       else if (($tmp >= "a") && ($tmp <= "f")) Result = c.charCodeAt() - 0x57;
      return Result;
    };
    this.GetAlphaNum = function () {
      var Result = "";
      if (!this.IsAlpha()) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserExpected"),pas.System.VarRecs(18,this.GetTokenName(2)));
      Result = "";
      while (!this.fEofReached && this.IsAlphaNum()) {
        Result = Result + this.fBuf[this.fPos];
        this.GotoToNextChar();
      };
      return Result;
    };
    this.HandleNewLine = function () {
      if (this.fBuf[this.fPos] === "\r") this.GotoToNextChar();
      if (!this.fEofReached && (this.fBuf[this.fPos] === "\n")) this.GotoToNextChar();
      this.fSourceLine += 1;
      this.fDeltaPos = -(this.fPos - 1);
    };
    this.SkipBOM = function () {
    };
    this.SkipSpaces = function () {
      while (!this.fEofReached && (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(32,9))) this.GotoToNextChar();
    };
    this.SkipWhitespace = function () {
      while (!this.fEofReached) {
        var $tmp = this.fBuf[this.fPos];
        if (($tmp === " ") || ($tmp === "\t")) {
          this.SkipSpaces()}
         else if (($tmp === "\n") || ($tmp === "\r")) {
          this.HandleNewLine()}
         else {
          break;
        };
      };
    };
    this.HandleEof = function () {
      this.fToken = 1;
      this.fLastTokenStr = "";
    };
    this.HandleAlphaNum = function () {
      this.fLastTokenStr = this.GetAlphaNum();
      this.fToken = 2;
    };
    var floatPunct = {"0": "fpDot", fpDot: 0, "1": "fpE", fpE: 1};
    this.HandleNumber = function () {
      var allowed = {};
      this.fLastTokenStr = "";
      while (this.IsNumber()) this.ProcessChar();
      this.fToken = 4;
      if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(46,101,69)) {
        this.fToken = 5;
        allowed = rtl.createSet(0,1);
        while (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(46,101,69,null,48,57)) {
          var $tmp = this.fBuf[this.fPos];
          if ($tmp === ".") {
            if (0 in allowed) {
              allowed = rtl.excludeSet(allowed,0)}
             else break}
           else if (($tmp === "E") || ($tmp === "e")) if (1 in allowed) {
            allowed = {};
            this.ProcessChar();
            if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(43,45)) this.ProcessChar();
            if (!(this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57))) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidFloat"),pas.System.VarRecs(18,this.fLastTokenStr + this.fBuf[this.fPos]));
          } else break;
          this.ProcessChar();
        };
      };
      if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(115,83,100,68,99,67)) {
        this.fFloatType = this.fBuf[this.fPos];
        this.GotoToNextChar();
        this.fToken = 5;
      } else this.fFloatType = "\x00";
    };
    this.HandleHexNumber = function () {
      var valid = false;
      this.fLastTokenStr = "$";
      this.GotoToNextChar();
      valid = false;
      while (this.IsHexNum()) {
        valid = true;
        this.ProcessChar();
      };
      if (!valid) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidInteger"),pas.System.VarRecs(18,this.fLastTokenStr));
      this.fToken = 4;
    };
    this.HandleQuotedString = function () {
      var Result = "";
      Result = "";
      this.GotoToNextChar();
      while (true) {
        var $tmp = this.fBuf[this.fPos];
        if ($tmp === "\x00") {
          this.ErrorStr(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedString"))}
         else if (($tmp === "\r") || ($tmp === "\n")) {
          this.ErrorStr(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedString"))}
         else if ($tmp === "'") {
          this.GotoToNextChar();
          if (this.fBuf[this.fPos] !== "'") return Result;
        };
        Result = Result + this.fBuf[this.fPos];
        this.GotoToNextChar();
      };
      return Result;
    };
    this.HandleDecimalCharacter = function () {
      var Result = "";
      var i = 0;
      this.GotoToNextChar();
      i = 0;
      while (this.IsNumber() && (i < 65535)) {
        i = ((i * 10) + this.fBuf[this.fPos].charCodeAt()) - 48;
        this.GotoToNextChar();
      };
      if (i > 65535) i = 0;
      Result = String.fromCharCode(i);
      return Result;
    };
    this.HandleString = function () {
      var s = "";
      this.fLastTokenStr = "";
      while (true) {
        var $tmp = this.fBuf[this.fPos];
        if ($tmp === "'") {
          s = this.HandleQuotedString();
          this.fLastTokenStr = this.fLastTokenStr + s;
        } else if ($tmp === "#") {
          this.fLastTokenStr = this.fLastTokenStr + this.HandleDecimalCharacter();
        } else {
          break;
        };
      };
      this.fToken = 3;
    };
    this.HandleMinus = function () {
      this.GotoToNextChar();
      if (this.IsNumber()) {
        this.HandleNumber();
        this.fLastTokenStr = "-" + this.fLastTokenStr;
      } else {
        this.fToken = 6;
        this.fLastTokenStr = "-";
      };
    };
    this.HandleUnknown = function () {
      this.fToken = 0;
      this.fLastTokenStr = this.fBuf[this.fPos];
      this.GotoToNextChar();
    };
    this.GotoToNextChar = function () {
      this.fPos += 1;
      this.CheckLoadBuffer();
    };
    this.Create$1 = function (Stream) {
      this.fStream = Stream;
      this.fBuf = rtl.arraySetLength(this.fBuf,"",4096);
      this.FBufLen = 0;
      this.fPos = 0;
      this.fDeltaPos = 1;
      this.fSourceLine = 1;
      this.fEofReached = false;
      this.fLastTokenStr = "";
      this.fFloatType = "\x00";
      this.fToken = 1;
      this.LoadBuffer();
      this.SkipBOM();
      this.NextToken();
      return this;
    };
    this.Destroy = function () {
      var aCount = 0;
      aCount = this.fLastTokenStr.length * 2;
      this.fStream.SetPosition(this.SourcePos() - aCount);
    };
    this.CheckToken = function (T) {
      if (this.fToken !== T) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserWrongTokenType"),pas.System.VarRecs(18,this.GetTokenName(T),18,this.GetTokenName(this.fToken)));
    };
    this.CheckTokenSymbol = function (S) {
      this.CheckToken(2);
      if (pas.SysUtils.CompareText(this.fLastTokenStr,S) !== 0) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserWrongTokenSymbol"),pas.System.VarRecs(18,S,18,this.fLastTokenStr));
    };
    this.Error = function (Ident) {
      this.ErrorStr(Ident);
    };
    this.ErrorFmt = function (Ident, Args) {
      this.ErrorStr(pas.SysUtils.Format(Ident,Args));
    };
    this.ErrorStr = function (Message) {
      throw $mod.EParserError.$create("CreateFmt",[Message + rtl.getResStr(pas.RTLConsts,"SParserLocInfo"),pas.System.VarRecs(0,this.fSourceLine,0,this.fPos + this.fDeltaPos,0,this.SourcePos())]);
    };
    this.HexToBinary = function (Stream) {
      var outbuf = [];
      var b = 0;
      var i = 0;
      outbuf = rtl.arraySetLength(outbuf,0,4096);
      i = 0;
      this.SkipWhitespace();
      while (this.IsHexNum()) {
        b = this.GetHexValue(this.fBuf[this.fPos]) << 4;
        this.GotoToNextChar();
        if (!this.IsHexNum()) this.Error(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedBinValue"));
        b = b | this.GetHexValue(this.fBuf[this.fPos]);
        this.GotoToNextChar();
        outbuf[i] = b;
        i += 1;
        if (i >= 4096) {
          Stream.WriteBuffer(outbuf,i);
          i = 0;
        };
        this.SkipWhitespace();
      };
      if (i > 0) Stream.WriteBuffer(outbuf,i);
      this.NextToken();
    };
    this.NextToken = function () {
      var $Self = this;
      var Result = 0;
      function SetToken(aToken) {
        $Self.fToken = aToken;
        $Self.GotoToNextChar();
      };
      this.SkipWhitespace();
      if (this.fEofReached) {
        this.HandleEof()}
       else {
        var $tmp = this.fBuf[this.fPos];
        if (($tmp === "_") || (($tmp >= "A") && ($tmp <= "Z")) || (($tmp >= "a") && ($tmp <= "z"))) {
          this.HandleAlphaNum()}
         else if ($tmp === "$") {
          this.HandleHexNumber()}
         else if ($tmp === "-") {
          this.HandleMinus()}
         else if (($tmp >= "0") && ($tmp <= "9")) {
          this.HandleNumber()}
         else if (($tmp === "'") || ($tmp === "#")) {
          this.HandleString()}
         else if ($tmp === "[") {
          SetToken(7)}
         else if ($tmp === "(") {
          SetToken(8)}
         else if ($tmp === "<") {
          SetToken(9)}
         else if ($tmp === "{") {
          SetToken(10)}
         else if ($tmp === "]") {
          SetToken(11)}
         else if ($tmp === ")") {
          SetToken(12)}
         else if ($tmp === ">") {
          SetToken(13)}
         else if ($tmp === "}") {
          SetToken(14)}
         else if ($tmp === ",") {
          SetToken(15)}
         else if ($tmp === ".") {
          SetToken(16)}
         else if ($tmp === "=") {
          SetToken(17)}
         else if ($tmp === ":") {
          SetToken(18)}
         else if ($tmp === "+") {
          SetToken(19)}
         else {
          this.HandleUnknown();
        };
      };
      Result = this.fToken;
      return Result;
    };
    this.SourcePos = function () {
      var Result = 0;
      Result = (this.fStream.GetPosition() - this.FBufLen) + this.fPos;
      return Result;
    };
    this.TokenComponentIdent = function () {
      var Result = "";
      if (this.fToken !== 2) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserExpected"),pas.System.VarRecs(18,this.GetTokenName(2)));
      this.CheckLoadBuffer();
      while (this.fBuf[this.fPos] === ".") {
        this.ProcessChar();
        this.fLastTokenStr = this.fLastTokenStr + this.GetAlphaNum();
      };
      Result = this.fLastTokenStr;
      return Result;
    };
    this.TokenFloat = function () {
      var Result = 0.0;
      var errcode = 0;
      pas.System.val$8(this.fLastTokenStr,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return errcode;
        }, set: function (v) {
          errcode = v;
        }});
      if (errcode !== 0) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidFloat"),pas.System.VarRecs(18,this.fLastTokenStr));
      return Result;
    };
    this.TokenInt = function () {
      var Result = 0;
      if (!pas.SysUtils.TryStrToInt64(this.fLastTokenStr,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = pas.SysUtils.StrToQWord(this.fLastTokenStr);
      return Result;
    };
    this.TokenString = function () {
      var Result = "";
      var $tmp = this.fToken;
      if ($tmp === 5) {
        if (this.fFloatType !== "\x00") {
          Result = this.fLastTokenStr + this.fFloatType}
         else Result = this.fLastTokenStr}
       else {
        Result = this.fLastTokenStr;
      };
      return Result;
    };
    this.TokenSymbolIs = function (S) {
      var Result = false;
      Result = (this.fToken === 2) && (pas.SysUtils.CompareText(this.fLastTokenStr,S) === 0);
      return Result;
    };
  });
  rtl.createClass(this,"TObjectTextConverter",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FParser = null;
      this.FInput = null;
      this.Foutput = null;
    };
    this.$final = function () {
      this.FParser = undefined;
      this.FInput = undefined;
      this.Foutput = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.WriteDouble = function (e) {
      this.Foutput.WriteBufferData$20(e);
    };
    this.WriteDWord = function (lw) {
      this.Foutput.WriteBufferData$14(lw);
    };
    this.WriteInteger = function (value) {
      if ((value >= -128) && (value <= 127)) {
        this.Foutput.WriteByte(2);
        this.Foutput.WriteByte(value & 255);
      } else if ((value >= -32768) && (value <= 32767)) {
        this.Foutput.WriteByte(3);
        this.WriteWord(value & 65535);
      } else if ((value >= -2147483648) && (value <= 2147483647)) {
        this.Foutput.WriteByte(4);
        this.WriteDWord(value >>> 0);
      } else {
        this.Foutput.WriteByte($mod.TValueType.vaNativeInt);
        this.WriteQWord(value);
      };
    };
    this.WriteQWord = function (q) {
      this.Foutput.WriteBufferData$16(q);
    };
    this.WriteString = function (s) {
      var i = 0;
      var size = 0;
      if (s.length > 255) {
        size = 255}
       else size = s.length;
      this.Foutput.WriteByte(size);
      for (var $l = 1, $end = s.length; $l <= $end; $l++) {
        i = $l;
        this.Foutput.WriteBufferData$4(s.charAt(i - 1));
      };
    };
    this.WriteWord = function (w) {
      this.Foutput.WriteBufferData$12(w);
    };
    this.WriteWString = function (s) {
      var i = 0;
      this.WriteDWord(s.length);
      for (var $l = 1, $end = s.length; $l <= $end; $l++) {
        i = $l;
        this.Foutput.WriteBufferData$4(s.charAt(i - 1));
      };
    };
    this.ProcessObject = function () {
      var Flags = 0;
      var ObjectName = "";
      var ObjectType = "";
      var ChildPos = 0;
      if (this.FParser.TokenSymbolIs("OBJECT")) {
        Flags = 0}
       else {
        if (this.FParser.TokenSymbolIs("INHERITED")) {
          Flags = 1}
         else {
          this.FParser.CheckTokenSymbol("INLINE");
          Flags = 4;
        };
      };
      this.FParser.NextToken();
      this.FParser.CheckToken(2);
      ObjectName = "";
      ObjectType = this.FParser.TokenString();
      this.FParser.NextToken();
      if (this.FParser.fToken === 18) {
        this.FParser.NextToken();
        this.FParser.CheckToken(2);
        ObjectName = ObjectType;
        ObjectType = this.FParser.TokenString();
        this.FParser.NextToken();
        if (this.FParser.fToken === 7) {
          this.FParser.NextToken();
          ChildPos = this.FParser.TokenInt();
          this.FParser.NextToken();
          this.FParser.CheckToken(11);
          this.FParser.NextToken();
          Flags = Flags | 2;
        };
      };
      if (Flags !== 0) {
        this.Foutput.WriteByte(0xf0 | Flags);
        if ((Flags & 2) !== 0) this.WriteInteger(ChildPos);
      };
      this.WriteString(ObjectType);
      this.WriteString(ObjectName);
      while (!(this.FParser.TokenSymbolIs("END") || this.FParser.TokenSymbolIs("OBJECT") || this.FParser.TokenSymbolIs("INHERITED") || this.FParser.TokenSymbolIs("INLINE"))) this.ProcessProperty();
      this.Foutput.WriteByte(0);
      while (!this.FParser.TokenSymbolIs("END")) this.ProcessObject();
      this.FParser.NextToken();
      this.Foutput.WriteByte(0);
    };
    this.ProcessProperty = function () {
      var name = "";
      this.FParser.CheckToken(2);
      name = this.FParser.TokenString();
      while (true) {
        this.FParser.NextToken();
        if (this.FParser.fToken !== 16) break;
        this.FParser.NextToken();
        this.FParser.CheckToken(2);
        name = name + "." + this.FParser.TokenString();
      };
      this.WriteString(name);
      this.FParser.CheckToken(17);
      this.FParser.NextToken();
      this.ProcessValue();
    };
    this.ProcessValue = function () {
      var flt = 0.0;
      var stream = null;
      var $tmp = this.FParser.fToken;
      if ($tmp === 4) {
        this.WriteInteger(this.FParser.TokenInt());
        this.FParser.NextToken();
      } else if ($tmp === 5) {
        this.Foutput.WriteByte($mod.TValueType.vaDouble);
        flt = this.FParser.TokenFloat();
        this.WriteDouble(flt);
        this.FParser.NextToken();
      } else if ($tmp === 3) {
        this.ProcessWideString("")}
       else if ($tmp === 2) {
        if (pas.SysUtils.CompareText(this.FParser.TokenString(),"True") === 0) {
          this.Foutput.WriteByte(9)}
         else if (pas.SysUtils.CompareText(this.FParser.TokenString(),"False") === 0) {
          this.Foutput.WriteByte(8)}
         else if (pas.SysUtils.CompareText(this.FParser.TokenString(),"nil") === 0) {
          this.Foutput.WriteByte(12)}
         else {
          this.Foutput.WriteByte(7);
          this.WriteString(this.FParser.TokenComponentIdent());
        };
        this.FParser.NextToken();
      } else if ($tmp === 7) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(11);
        if (this.FParser.fToken !== 11) while (true) {
          this.FParser.CheckToken(2);
          this.WriteString(this.FParser.TokenString());
          this.FParser.NextToken();
          if (this.FParser.fToken === 11) break;
          this.FParser.CheckToken(15);
          this.FParser.NextToken();
        };
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 8) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(1);
        while (this.FParser.fToken !== 12) this.ProcessValue();
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 9) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(13);
        while (this.FParser.fToken !== 13) {
          this.FParser.CheckTokenSymbol("item");
          this.FParser.NextToken();
          this.Foutput.WriteByte(1);
          while (!this.FParser.TokenSymbolIs("end")) this.ProcessProperty();
          this.FParser.NextToken();
          this.Foutput.WriteByte(0);
        };
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 10) {
        this.Foutput.WriteByte(10);
        stream = $mod.TBytesStream.$create("Create");
        try {
          this.FParser.HexToBinary(stream);
          this.WriteDWord(stream.GetSize());
          this.Foutput.WriteBuffer(stream.GetBytes(),stream.GetSize());
        } finally {
          stream = rtl.freeLoc(stream);
        };
        this.FParser.NextToken();
      } else {
        this.FParser.Error(rtl.getResStr(pas.RTLConsts,"SParserInvalidProperty"));
      };
    };
    this.ProcessWideString = function (left) {
      var ws = "";
      ws = left + this.FParser.TokenString();
      while (this.FParser.NextToken() === 19) {
        this.FParser.NextToken();
        if (!(this.FParser.fToken === 3)) this.FParser.CheckToken(3);
        ws = ws + this.FParser.TokenString();
      };
      this.Foutput.WriteByte($mod.TValueType.vaString);
      this.WriteWString(ws);
    };
    this.ObjectTextToBinary = function (aInput, aOutput) {
      this.FInput = aInput;
      this.Foutput = aOutput;
      this.Execute();
    };
    this.Execute = function () {
      if (!(this.FInput != null)) throw $mod.EReadError.$create("Create$1",["Missing input stream"]);
      if (!(this.Foutput != null)) throw $mod.EReadError.$create("Create$1",["Missing output stream"]);
      this.FParser = $mod.TParser.$create("Create$1",[this.FInput]);
      try {
        this.Foutput.WriteBufferData$14(809914452);
        this.ProcessObject();
      } finally {
        rtl.free(this,"FParser");
      };
    };
  });
  rtl.createClass(this,"TLoadHelper",pas.System.TObject,function () {
  });
  rtl.recNewT(this,"TIdentMapEntry",function () {
    this.Value = 0;
    this.Name = "";
    this.$eq = function (b) {
      return (this.Value === b.Value) && (this.Name === b.Name);
    };
    this.$assign = function (s) {
      this.Value = s.Value;
      this.Name = s.Name;
      return this;
    };
  });
  this.$rtti.$ProcVar("TInitComponentHandler",{procsig: rtl.newTIProcSig([["Instance",this.$rtti["TComponent"]],["RootAncestor",pas.System.$rtti["TClass"]]],rtl.boolean)});
  this.RegisterInitComponentHandler = function (ComponentClass, Handler) {
    var I = 0;
    var H = null;
    if ($impl.InitHandlerList === null) $impl.InitHandlerList = $mod.TList.$create("Create$1");
    H = $impl.TInitHandler.$create("Create");
    H.AClass = ComponentClass;
    H.AHandler = Handler;
    try {
      var $with = $impl.InitHandlerList;
      I = 0;
      while ((I < $with.GetCount()) && !H.AClass.InheritsFrom(rtl.getObject($with.Get(I)).AClass)) I += 1;
      if ((I < $with.GetCount()) && (rtl.getObject($with.Get(I)).AClass === H.AClass)) {
        rtl.getObject($with.Get(I)).AHandler = Handler;
        H = rtl.freeLoc(H);
      } else $impl.InitHandlerList.Insert(I,H);
    } catch ($e) {
      H = rtl.freeLoc(H);
      throw $e;
    };
  };
  this.GetClass = function (AClassName) {
    var Result = null;
    Result = null;
    if (AClassName === "") return Result;
    if (!$impl.ClassList.hasOwnProperty(AClassName)) return Result;
    Result = rtl.getObject($impl.ClassList[AClassName]);
    return Result;
  };
  this.FindGlobalComponent = function (Name) {
    var Result = null;
    var i = 0;
    Result = null;
    if ($impl.FindGlobalComponentList != null) {
      for (var $l = $impl.FindGlobalComponentList.FCount - 1; $l >= 0; $l--) {
        i = $l;
        Result = $impl.FindGlobalComponentList.Get(i)(Name);
        if (Result != null) break;
      };
    };
    return Result;
  };
  this.FindNestedComponent = function (Root, APath, CStyle) {
    var Result = null;
    function GetNextName() {
      var Result = "";
      var P = 0;
      var CM = false;
      P = pas.System.Pos(".",APath);
      CM = false;
      if (P === 0) {
        if (CStyle) {
          P = pas.System.Pos("->",APath);
          CM = P !== 0;
        };
        if (P === 0) P = APath.length + 1;
      };
      Result = pas.System.Copy(APath,1,P - 1);
      pas.System.Delete({get: function () {
          return APath;
        }, set: function (v) {
          APath = v;
        }},1,P + (CM + 0));
      return Result;
    };
    var C = null;
    var S = "";
    if (APath === "") {
      Result = null}
     else {
      Result = Root;
      while ((APath !== "") && (Result !== null)) {
        C = Result;
        S = pas.SysUtils.UpperCase(GetNextName());
        Result = C.FindComponent(S);
        if ((Result === null) && (S === "OWNER")) Result = C;
      };
    };
    return Result;
  };
  this.RemoveFixupReferences = function (Root, RootName) {
    if ($impl.NeedResolving === null) return;
    $impl.VisitResolveList($impl.TRemoveReferenceVisitor.$create("Create$1",[Root,RootName]));
  };
  this.RegisterIntegerConsts = function (IntegerType, IdentToIntFn, IntToIdentFn) {
    if (!($impl.IntConstList != null)) $impl.IntConstList = $mod.TFPList.$create("Create");
    $impl.IntConstList.Add($impl.TIntConst.$create("Create$1",[IntegerType,IdentToIntFn,IntToIdentFn]));
  };
  this.IdentToInt = function (Ident, Int, map) {
    var Result = false;
    var i = 0;
    for (var $l = 0, $end = rtl.length(map) - 1; $l <= $end; $l++) {
      i = $l;
      if (pas.SysUtils.CompareText(map[i].Name,Ident) === 0) {
        Int.set(map[i].Value);
        return true;
      };
    };
    Result = false;
    return Result;
  };
  this.IntToIdent = function (Int, Ident, map) {
    var Result = false;
    var i = 0;
    for (var $l = 0, $end = rtl.length(map) - 1; $l <= $end; $l++) {
      i = $l;
      if (map[i].Value === Int) {
        Ident.set(map[i].Name);
        return true;
      };
    };
    Result = false;
    return Result;
  };
  this.FindClass = function (AClassName) {
    var Result = null;
    Result = $mod.GetClass(AClassName);
    if (!(Result != null)) throw $mod.EClassNotFound.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SClassNotFound"),pas.System.VarRecs(18,AClassName)]);
    return Result;
  };
  this.ObjectTextToBinary = function (aInput, aOutput) {
    var Conv = null;
    Conv = $mod.TObjectTextConverter.$create("Create");
    try {
      Conv.ObjectTextToBinary(aInput,aOutput);
    } finally {
      Conv = rtl.freeLoc(Conv);
    };
  };
  this.SetLoadHelperClass = function (aClass) {
    var Result = null;
    Result = $impl.GlobalLoadHelper;
    $impl.GlobalLoadHelper = aClass;
    return Result;
  };
  this.StringToBuffer = function (aString, aLen) {
    var Result = null;
    var I = 0;
    Result = new ArrayBuffer(aLen * 2);
    var $with = new Uint16Array(Result);
    for (var $l = 0, $end = aLen - 1; $l <= $end; $l++) {
      I = $l;
      $with[I] = aString.charCodeAt(I);
    };
    return Result;
  };
  this.vaExtended = 5;
  this.vaWString = 6;
  this.vaInt64 = 16;
  $mod.$implcode = function () {
    $impl.GlobalLoaded = null;
    $impl.IntConstList = null;
    $impl.GlobalLoadHelper = null;
    rtl.createClass($impl,"TIntConst",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.IntegerType = null;
        this.IdentToIntFn = null;
        this.IntToIdentFn = null;
      };
      this.$final = function () {
        this.IdentToIntFn = undefined;
        this.IntToIdentFn = undefined;
        pas.System.TObject.$final.call(this);
      };
      this.Create$1 = function (AIntegerType, AIdentToInt, AIntToIdent) {
        this.IntegerType = AIntegerType;
        this.IdentToIntFn = AIdentToInt;
        this.IntToIdentFn = AIntToIdent;
        return this;
      };
    });
    $impl.GlobalIdentToInt = function (Ident, Int) {
      var Result = false;
      var i = 0;
      Result = false;
      if (!($impl.IntConstList != null)) return Result;
      var $with = $impl.IntConstList;
      for (var $l = 0, $end = $with.FCount - 1; $l <= $end; $l++) {
        i = $l;
        if (rtl.getObject($with.Get(i)).IdentToIntFn(Ident,Int)) return true;
      };
      return Result;
    };
    $impl.TMSGrow = 4096;
    $impl.FilerSignatureInt = 809914452;
    rtl.createClass($impl,"TUnresolvedReference",pas.simplelinkedlist.TLinkedListItem,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListItem.$init.call(this);
        this.FRoot = null;
        this.FPropInfo = null;
        this.FGlobal = "";
        this.FRelative = "";
      };
      this.$final = function () {
        this.FRoot = undefined;
        this.FPropInfo = undefined;
        pas.simplelinkedlist.TLinkedListItem.$final.call(this);
      };
      this.Resolve = function (Instance) {
        var Result = false;
        var C = null;
        C = $mod.FindGlobalComponent(this.FGlobal);
        Result = C !== null;
        if (Result) {
          C = $mod.FindNestedComponent(C,this.FRelative,true);
          Result = C !== null;
          if (Result) pas.TypInfo.SetObjectProp$1(Instance,this.FPropInfo,C);
        };
        return Result;
      };
      this.RootMatches = function (ARoot) {
        var Result = false;
        Result = (ARoot === null) || (ARoot === this.FRoot);
        return Result;
      };
      this.NextRef = function () {
        var Result = null;
        Result = this.Next;
        return Result;
      };
    });
    rtl.createClass($impl,"TLocalUnResolvedReference",$impl.TUnresolvedReference,function () {
      this.$init = function () {
        $impl.TUnresolvedReference.$init.call(this);
        this.Finstance = null;
      };
      this.$final = function () {
        this.Finstance = undefined;
        $impl.TUnresolvedReference.$final.call(this);
      };
      var $r = this.$rtti;
      $r.addField("Finstance",$mod.$rtti["TPersistent"]);
    });
    rtl.createClass($impl,"TUnResolvedInstance",pas.simplelinkedlist.TLinkedListItem,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListItem.$init.call(this);
        this.Instance = null;
        this.FUnresolved = null;
      };
      this.$final = function () {
        this.Instance = undefined;
        this.FUnresolved = undefined;
        pas.simplelinkedlist.TLinkedListItem.$final.call(this);
      };
      this.Destroy = function () {
        rtl.free(this,"FUnresolved");
        pas.System.TObject.Destroy.call(this);
      };
      this.AddReference = function (ARoot, APropInfo, AGlobal, ARelative) {
        var Result = null;
        if (this.FUnresolved === null) this.FUnresolved = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TUnresolvedReference]);
        Result = rtl.as(this.FUnresolved.Add(),$impl.TUnresolvedReference);
        Result.FGlobal = AGlobal;
        Result.FRelative = ARelative;
        Result.FPropInfo = APropInfo;
        Result.FRoot = ARoot;
        return Result;
      };
      this.RootUnresolved = function () {
        var Result = null;
        Result = null;
        if (this.FUnresolved != null) Result = this.FUnresolved.FRoot;
        return Result;
      };
      this.ResolveReferences = function () {
        var Result = false;
        var R = null;
        var RN = null;
        R = this.RootUnresolved();
        while (R !== null) {
          RN = R.NextRef();
          if (R.Resolve(this.Instance)) this.FUnresolved.RemoveItem(R,true);
          R = RN;
        };
        Result = this.RootUnresolved() === null;
        return Result;
      };
    });
    rtl.createClass($impl,"TBuildListVisitor",pas.simplelinkedlist.TLinkedListVisitor,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListVisitor.$init.call(this);
        this.List = null;
      };
      this.$final = function () {
        this.List = undefined;
        pas.simplelinkedlist.TLinkedListVisitor.$final.call(this);
      };
      this.Add = function (Item) {
        if (this.List === null) this.List = $mod.TFPList.$create("Create");
        this.List.Add(Item);
      };
      this.Destroy = function () {
        var I = 0;
        if (this.List != null) for (var $l = 0, $end = this.List.FCount - 1; $l <= $end; $l++) {
          I = $l;
          $impl.NeedResolving.RemoveItem(rtl.getObject(this.List.Get(I)),true);
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.List;
          }, set: function (v) {
            this.p.List = v;
          }});
        pas.System.TObject.Destroy.call(this);
      };
    });
    rtl.createClass($impl,"TResolveReferenceVisitor",$impl.TBuildListVisitor,function () {
      this.Visit = function (Item) {
        var Result = false;
        if (Item.ResolveReferences()) this.Add(Item);
        Result = true;
        return Result;
      };
      var $r = this.$rtti;
      $r.addMethod("Visit",1,[["Item",pas.simplelinkedlist.$rtti["TLinkedListItem"]]],rtl.boolean);
    });
    rtl.createClass($impl,"TRemoveReferenceVisitor",$impl.TBuildListVisitor,function () {
      this.$init = function () {
        $impl.TBuildListVisitor.$init.call(this);
        this.FRef = "";
        this.FRoot = null;
      };
      this.$final = function () {
        this.FRoot = undefined;
        $impl.TBuildListVisitor.$final.call(this);
      };
      this.Create$1 = function (ARoot, ARef) {
        this.FRoot = ARoot;
        this.FRef = pas.SysUtils.UpperCase(ARef);
        return this;
      };
      this.Visit = function (Item) {
        var Result = false;
        var I = 0;
        var UI = null;
        var R = null;
        var L = null;
        UI = Item;
        R = UI.RootUnresolved();
        L = null;
        try {
          while (R !== null) {
            if (R.RootMatches(this.FRoot) && ((this.FRef === "") || (this.FRef === pas.SysUtils.UpperCase(R.FGlobal)))) {
              if (!(L != null)) L = $mod.TFPList.$create("Create");
              L.Add(R);
            };
            R = R.NextRef();
          };
          if (L != null) {
            for (var $l = 0, $end = L.FCount - 1; $l <= $end; $l++) {
              I = $l;
              UI.FUnresolved.RemoveItem(rtl.getObject(L.Get(I)),true);
            };
          };
          if (UI.FUnresolved.FRoot === null) {
            if (this.List === null) this.List = $mod.TFPList.$create("Create");
            this.List.Add(UI);
          };
        } finally {
          L = rtl.freeLoc(L);
        };
        Result = true;
        return Result;
      };
    });
    $impl.NeedResolving = null;
    $impl.FindUnresolvedInstance = function (AInstance) {
      var Result = null;
      Result = null;
      if ($impl.NeedResolving != null) {
        Result = $impl.NeedResolving.FRoot;
        while ((Result !== null) && (Result.Instance !== AInstance)) Result = Result.Next;
      };
      return Result;
    };
    $impl.AddtoResolveList = function (AInstance) {
      var Result = null;
      Result = $impl.FindUnresolvedInstance(AInstance);
      if (Result === null) {
        if (!($impl.NeedResolving != null)) $impl.NeedResolving = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TUnResolvedInstance]);
        Result = rtl.as($impl.NeedResolving.Add(),$impl.TUnResolvedInstance);
        Result.Instance = AInstance;
      };
      return Result;
    };
    $impl.VisitResolveList = function (V) {
      try {
        $impl.NeedResolving.ForEach(V);
      } finally {
        pas.SysUtils.FreeAndNil({get: function () {
            return V;
          }, set: function (v) {
            V = v;
          }});
      };
    };
    $impl.GlobalFixupReferences = function () {
      if ($impl.NeedResolving === null) return;
      $impl.VisitResolveList($impl.TResolveReferenceVisitor.$create("Create"));
    };
    rtl.createClass($impl,"TInitHandler",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.AHandler = null;
        this.AClass = null;
      };
      this.$final = function () {
        this.AHandler = undefined;
        this.AClass = undefined;
        pas.System.TObject.$final.call(this);
      };
      var $r = this.$rtti;
      $r.addField("AHandler",$mod.$rtti["TInitComponentHandler"]);
      $r.addField("AClass",$mod.$rtti["TComponentClass"]);
    });
    $impl.ClassList = null;
    $impl.InitHandlerList = null;
    $impl.FindGlobalComponentList = null;
    $impl.ParseBufSize = 4096;
    $impl.TokNames = ["?","EOF","Symbol","String","Integer","Float","-","[","(","<","{","]",")",">","}",",",".","=",":","+"];
    $mod.$resourcestrings = {SReadError: {org: "Could not read data from stream"}, SWriteError: {org: "Could not write data to stream"}, SMemoryStreamError: {org: "Could not allocate memory"}};
  };
  $mod.$init = function () {
    $impl.ClassList = new Object();
  };
},["simplelinkedlist"]);
rtl.module("Graphics",["System","Classes","SysUtils","Types","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.$rtti.$Int("TFontCharSet",{minvalue: 0, maxvalue: 255, ordtype: 3});
  this.TFontStyle = {"0": "fsBold", fsBold: 0, "1": "fsItalic", fsItalic: 1, "2": "fsUnderline", fsUnderline: 2, "3": "fsStrikeOut", fsStrikeOut: 3};
  this.$rtti.$Enum("TFontStyle",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFontStyle});
  this.$rtti.$Set("TFontStyles",{comptype: this.$rtti["TFontStyle"]});
  this.TTextLayout = {"0": "tlTop", tlTop: 0, "1": "tlCenter", tlCenter: 1, "2": "tlBottom", tlBottom: 2};
  this.$rtti.$Enum("TTextLayout",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TTextLayout});
  this.TPenStyle = {"0": "psSolid", psSolid: 0, "1": "psDash", psDash: 1, "2": "psDot", psDot: 2, "3": "psDashDot", psDashDot: 3, "4": "psDashDotDot", psDashDotDot: 4, "5": "psInsideFrame", psInsideFrame: 5, "6": "psPattern", psPattern: 6, "7": "psClear", psClear: 7};
  this.$rtti.$Enum("TPenStyle",{minvalue: 0, maxvalue: 7, ordtype: 1, enumtype: this.TPenStyle});
  this.TBrushStyle = {"0": "bsSolid", bsSolid: 0, "1": "bsClear", bsClear: 1, "2": "bsHorizontal", bsHorizontal: 2, "3": "bsVertical", bsVertical: 3, "4": "bsFDiagonal", bsFDiagonal: 4, "5": "bsBDiagonal", bsBDiagonal: 5, "6": "bsCross", bsCross: 6, "7": "bsDiagCross", bsDiagCross: 7, "8": "bsImage", bsImage: 8, "9": "bsPattern", bsPattern: 9};
  this.$rtti.$Enum("TBrushStyle",{minvalue: 0, maxvalue: 9, ordtype: 1, enumtype: this.TBrushStyle});
  rtl.createClass(this,"TFont",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FCharSet = 0;
      this.FColor = 0;
      this.FName = "";
      this.FSize = 0;
      this.FStyle = {};
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FStyle = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = Math.round((-this.FSize * 72) / 96);
      return Result;
    };
    this.SetCharSet = function (AValue) {
      if (this.FCharSet !== AValue) {
        this.FCharSet = AValue;
        this.Changed();
      };
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetSize(Math.round((-AValue * 96) / 72));
    };
    this.SetName = function (AValue) {
      if (this.FName !== AValue) {
        this.FName = AValue;
        this.Changed();
      };
    };
    this.SetSize = function (AValue) {
      if (this.FSize !== AValue) {
        this.FSize = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (rtl.neSet(this.FStyle,AValue)) {
        this.FStyle = rtl.refSet(AValue);
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FColor = 0;
      this.FName = $mod.ffSans;
      this.FSize = 10;
      this.FStyle = {};
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VFont = null;
      if ((Source != null) && $mod.TFont.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VFont = Source;
          this.FCharSet = VFont.FCharSet;
          this.FColor = VFont.FColor;
          this.FName = VFont.FName;
          this.FSize = VFont.FSize;
          this.FStyle = rtl.refSet(VFont.FStyle);
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.IsEqual = function (AFont) {
      var Result = false;
      if (AFont != null) {
        if ((this.FCharSet !== AFont.FCharSet) || (this.FColor !== AFont.FColor) || (this.FName !== AFont.FName) || (this.FSize !== AFont.FSize) || rtl.neSet(this.FStyle,AFont.FStyle)) {
          Result = false;
        } else {
          Result = true;
        };
      } else {
        Result = false;
      };
      return Result;
    };
    this.TextExtent = function (AText) {
      var Result = pas.Types.TSize.$new();
      Result.$assign($mod.JSMeasureText(AText,this.FName,this.FSize,0));
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("CharSet",2,$mod.$rtti["TFontCharSet"],"FCharSet","SetCharSet");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Height",3,rtl.nativeint,"GetHeight","SetHeight");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Size",2,rtl.nativeint,"FSize","SetSize");
    $r.addProperty("Style",2,$mod.$rtti["TFontStyles"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TPen",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FWidth = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle !== AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.SetWidth = function (AValue) {
      if (this.FWidth !== AValue) {
        this.FWidth = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FWidth = 1;
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VPen = null;
      if ((Source != null) && $mod.TPen.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VPen = Source;
          this.FColor = VPen.FColor;
          this.FStyle = VPen.FStyle;
          this.FWidth = VPen.FWidth;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TPenStyle"],"FStyle","SetStyle");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TBrush",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle === AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FColor = 16777215;
      this.FStyle = 0;
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VBrush = null;
      if ((Source != null) && $mod.TBrush.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VBrush = Source;
          this.FColor = VBrush.FColor;
          this.FStyle = VBrush.FStyle;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TBrushStyle"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TPicture",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FData = "";
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetData = function (AValue) {
      if (this.FData !== AValue) {
        this.FData = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      this.FData = "";
      this.FUpdateCount = 0;
      this.FOnChange = null;
      return this;
    };
    this.Assign = function (Source) {
      var VPicture = null;
      if ((Source != null) && $mod.TPicture.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VPicture = Source;
          this.FData = VPicture.FData;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Data",2,rtl.string,"FData","SetData");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TCanvas",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      this.FUpdateCount = 0;
      this.FOnChange = null;
      this.FCanvasElement = null;
      this.FContextElement = null;
    };
    this.$final = function () {
      this.FBrush = undefined;
      this.FFont = undefined;
      this.FPen = undefined;
      this.FOnChange = undefined;
      this.FCanvasElement = undefined;
      this.FContextElement = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FCanvasElement = document.createElement("canvas");
      this.FContextElement = this.FCanvasElement.getContext("2d");
      this.FBrush = $mod.TBrush.$create("Create$1");
      this.FFont = $mod.TFont.$create("Create$1");
      this.FPen = $mod.TPen.$create("Create$1");
      this.FUpdateCount = 0;
      return this;
    };
    this.Destroy = function () {
      this.FBrush.$destroy("Destroy");
      this.FFont.$destroy("Destroy");
      this.FPen.$destroy("Destroy");
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      pas.System.TObject.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Brush",0,$mod.$rtti["TBrush"],"FBrush","FBrush");
    $r.addProperty("Font",0,$mod.$rtti["TFont"],"FFont","FFont");
    $r.addProperty("Pen",0,$mod.$rtti["TPen"],"FPen","FPen");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  this.clBlack = 0x0;
  this.clMaroon = 0x80;
  this.clGreen = 0x8000;
  this.clOlive = 0x8080;
  this.clNavy = 0x800000;
  this.clPurple = 0x800080;
  this.clTeal = 0x808000;
  this.clGray = 0x808080;
  this.clSilver = 0xC0C0C0;
  this.clRed = 0xFF;
  this.clLime = 0xFF00;
  this.clYellow = 0xFFFF;
  this.clBlue = 0xFF0000;
  this.clFuchsia = 0xFF00FF;
  this.clAqua = 0xFFFF00;
  this.clWhite = 0xFFFFFF;
  this.clMoneyGreen = 0xC0DCC0;
  this.clSkyBlue = 0xF0CAA6;
  this.clCream = 0xF0FBFF;
  this.clMedGray = 0xA4A0A0;
  this.clNone = 0x1FFFFFFF;
  this.clDefault = 0x20000000;
  this.clBase = 0x80000000;
  this.clScrollBar = -2147483648 + 0;
  this.clBackground = -2147483648 + 1;
  this.clActiveCaption = -2147483648 + 2;
  this.clInactiveCaption = -2147483648 + 3;
  this.clMenu = -2147483648 + 4;
  this.clWindow = -2147483648 + 5;
  this.clWindowFrame = -2147483648 + 6;
  this.clMenuText = -2147483648 + 7;
  this.clWindowText = -2147483648 + 8;
  this.clCaptionText = -2147483648 + 9;
  this.clActiveBorder = -2147483648 + 10;
  this.clInactiveBorder = -2147483648 + 11;
  this.clAppWorkspace = -2147483648 + 12;
  this.clHighlight = -2147483648 + 13;
  this.clHighlightText = -2147483648 + 14;
  this.clBtnFace = -2147483648 + 15;
  this.clBtnShadow = -2147483648 + 16;
  this.clGrayText = -2147483648 + 17;
  this.clBtnText = -2147483648 + 18;
  this.clInactiveCaptionText = -2147483648 + 19;
  this.clBtnHighlight = -2147483648 + 20;
  this.cl3DDkShadow = -2147483648 + 21;
  this.cl3DLight = -2147483648 + 22;
  this.clInfoText = -2147483648 + 23;
  this.clInfoBk = -2147483648 + 24;
  this.ffSans = '"Arial Narrow", Arial, "Helvetica Condensed", Helvetica, sans-serif';
  this.JSColor = function (AColor) {
    var Result = "";
    var R = 0;
    var G = 0;
    var B = 0;
    var $tmp = AColor;
    if ($tmp === -2147483648) {
      Result = "Scrollbar"}
     else if ($tmp === -2147483647) {
      Result = "Background"}
     else if ($tmp === -2147483646) {
      Result = "ActiveCaption"}
     else if ($tmp === -2147483645) {
      Result = "InactiveCaption"}
     else if ($tmp === -2147483644) {
      Result = "Menu"}
     else if ($tmp === -2147483643) {
      Result = "Window"}
     else if ($tmp === -2147483642) {
      Result = "WindowFrame"}
     else if ($tmp === -2147483641) {
      Result = "MenuText"}
     else if ($tmp === -2147483640) {
      Result = "WindowText"}
     else if ($tmp === -2147483639) {
      Result = "CaptionText"}
     else if ($tmp === -2147483638) {
      Result = "ActiveBorder"}
     else if ($tmp === -2147483637) {
      Result = "InactiveBorder"}
     else if ($tmp === -2147483636) {
      Result = "AppWorkspace"}
     else if ($tmp === -2147483635) {
      Result = "Highlight"}
     else if ($tmp === -2147483634) {
      Result = "HighlightText"}
     else if ($tmp === -2147483633) {
      Result = "ButtonFace"}
     else if ($tmp === -2147483632) {
      Result = "ButtonShadow"}
     else if ($tmp === -2147483631) {
      Result = "GrayText"}
     else if ($tmp === -2147483630) {
      Result = "ButtonText"}
     else if ($tmp === -2147483629) {
      Result = "InactiveCaptionText"}
     else if ($tmp === -2147483628) {
      Result = "ButtonHighlight"}
     else if ($tmp === -2147483627) {
      Result = "ThreeDDarkShadow"}
     else if ($tmp === -2147483626) {
      Result = "ThreeDHighlight"}
     else if ($tmp === -2147483625) {
      Result = "InfoText"}
     else if ($tmp === -2147483624) {
      Result = "InfoBackground"}
     else {
      R = AColor & 0xFF;
      G = (AColor >>> 8) & 0xFF;
      B = (AColor >>> 16) & 0xFF;
      Result = "#" + pas.SysUtils.IntToHex(R,2) + pas.SysUtils.IntToHex(G,2) + pas.SysUtils.IntToHex(B,2);
    };
    return Result;
  };
  this.JSMeasureText = function (AText, AFontName, AFontSize, AFixedWidth) {
    var Result = pas.Types.TSize.$new();
    var VDiv = null;
    Result.$assign(pas.Types.Size(0,0));
    if (AText !== "") {
      VDiv = document.createElement("div");
      VDiv.style.setProperty("font-family",AFontName);
      VDiv.style.setProperty("font-size",pas.SysUtils.IntToStr(AFontSize) + "px");
      VDiv.style.setProperty("overflow","scroll");
      if (AFixedWidth === 0) {
        VDiv.style.setProperty("display","inline-block");
        VDiv.style.setProperty("white-space","nowrap");
      } else {
        VDiv.style.setProperty("max-width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
        VDiv.style.setProperty("width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
      };
      VDiv.innerHTML = AText;
      document.body.appendChild(VDiv);
      Result.$assign(pas.Types.Size(VDiv.scrollWidth,VDiv.scrollHeight));
      document.body.removeChild(VDiv);
    };
    return Result;
  };
  this.ColorToIdent = function (Color, Ident) {
    var Result = false;
    Result = pas.Classes.IntToIdent(Color,Ident,$impl.Colors);
    return Result;
  };
  this.IdentToColor = function (Ident, Color) {
    var Result = false;
    Result = pas.Classes.IdentToInt(Ident,Color,$impl.Colors);
    return Result;
  };
  $mod.$implcode = function () {
    $impl.Colors$a$clone = function (a) {
      var b = [];
      b.length = 47;
      for (var c = 0; c < 47; c++) b[c] = pas.Classes.TIdentMapEntry.$clone(a[c]);
      return b;
    };
    $impl.Colors = [pas.Classes.TIdentMapEntry.$clone({Value: 0, Name: "clBlack"}),pas.Classes.TIdentMapEntry.$clone({Value: 128, Name: "clMaroon"}),pas.Classes.TIdentMapEntry.$clone({Value: 32768, Name: "clGreen"}),pas.Classes.TIdentMapEntry.$clone({Value: 32896, Name: "clOlive"}),pas.Classes.TIdentMapEntry.$clone({Value: 8388608, Name: "clNavy"}),pas.Classes.TIdentMapEntry.$clone({Value: 8388736, Name: "clPurple"}),pas.Classes.TIdentMapEntry.$clone({Value: 8421376, Name: "clTeal"}),pas.Classes.TIdentMapEntry.$clone({Value: 8421504, Name: "clGray"}),pas.Classes.TIdentMapEntry.$clone({Value: 12632256, Name: "clSilver"}),pas.Classes.TIdentMapEntry.$clone({Value: 255, Name: "clRed"}),pas.Classes.TIdentMapEntry.$clone({Value: 65280, Name: "clLime"}),pas.Classes.TIdentMapEntry.$clone({Value: 65535, Name: "clYellow"}),pas.Classes.TIdentMapEntry.$clone({Value: 16711680, Name: "clBlue"}),pas.Classes.TIdentMapEntry.$clone({Value: 16711935, Name: "clFuchsia"}),pas.Classes.TIdentMapEntry.$clone({Value: 16776960, Name: "clAqua"}),pas.Classes.TIdentMapEntry.$clone({Value: 16777215, Name: "clWhite"}),pas.Classes.TIdentMapEntry.$clone({Value: 12639424, Name: "clMoneyGreen"}),pas.Classes.TIdentMapEntry.$clone({Value: 15780518, Name: "clSkyBlue"}),pas.Classes.TIdentMapEntry.$clone({Value: 15793151, Name: "clCream"}),pas.Classes.TIdentMapEntry.$clone({Value: 10789024, Name: "clMedGray"}),pas.Classes.TIdentMapEntry.$clone({Value: 536870911, Name: "clNone"}),pas.Classes.TIdentMapEntry.$clone({Value: 536870912, Name: "clDefault"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483648, Name: "clScrollBar"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483647, Name: "clBackground"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483646, Name: "clActiveCaption"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483645, Name: "clInactiveCaption"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483644, Name: "clMenu"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483641, Name: "clMenuText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483643, Name: "clWindow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483642, Name: "clWindowFrame"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483640, Name: "clWindowText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483639, Name: "clCaptionText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483638, Name: "clActiveBorder"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483637, Name: "clInactiveBorder"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483636, Name: "clAppWorkspace"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483635, Name: "clHighlight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483634, Name: "clHighlightText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483633, Name: "clBtnFace"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483632, Name: "clBtnShadow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483631, Name: "clGrayText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483630, Name: "clBtnText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483629, Name: "clInactiveCaptionText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483628, Name: "clBtnHighlight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483627, Name: "cl3DDkShadow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483626, Name: "cl3DLight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483625, Name: "clInfoText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483624, Name: "clInfoBk"})];
  };
  $mod.$init = function () {
    pas.Classes.RegisterIntegerConsts(rtl.longint,$mod.IdentToColor,$mod.ColorToIdent);
  };
},[]);
rtl.module("Controls",["System","Classes","SysUtils","Types","JS","Web","Graphics"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.mrNone = 0;
  this.mrOk = 0 + 1;
  this.mrCancel = 0 + 2;
  this.mrAbort = 0 + 3;
  this.mrRetry = 0 + 4;
  this.mrIgnore = 0 + 5;
  this.mrYes = 0 + 6;
  this.mrNo = 0 + 7;
  this.mrAll = 0 + 8;
  this.mrNoToAll = 0 + 9;
  this.mrYesToAll = 0 + 10;
  this.mrClose = 0 + 11;
  this.crDefault = 0;
  this.crNone = -1;
  this.crArrow = -2;
  this.crCross = -3;
  this.crIBeam = -4;
  this.crSize = -22;
  this.crSizeAll = -22;
  this.crSizeNESW = -6;
  this.crSizeNS = -7;
  this.crSizeNWSE = -8;
  this.crSizeWE = -9;
  this.crSizeNW = -23;
  this.crSizeN = -24;
  this.crSizeNE = -25;
  this.crSizeW = -26;
  this.crSizeE = -27;
  this.crSizeSW = -28;
  this.crSizeS = -29;
  this.crSizeSE = -30;
  this.crUpArrow = -10;
  this.crHourGlass = -11;
  this.crDrag = -12;
  this.crNoDrop = -13;
  this.crHSplit = -14;
  this.crVSplit = -15;
  this.crMultiDrag = -16;
  this.crSQLWait = -17;
  this.crNo = -18;
  this.crAppStart = -19;
  this.crHelp = -20;
  this.crHandPoint = -21;
  this.$rtti.$Class("TWinControl");
  this.$rtti.$Class("TControl");
  this.TAlign = {"0": "alNone", alNone: 0, "1": "alTop", alTop: 1, "2": "alBottom", alBottom: 2, "3": "alLeft", alLeft: 3, "4": "alRight", alRight: 4, "5": "alClient", alClient: 5, "6": "alCustom", alCustom: 6};
  this.$rtti.$Enum("TAlign",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TAlign});
  this.TAnchorKind = {"0": "akTop", akTop: 0, "1": "akLeft", akLeft: 1, "2": "akRight", akRight: 2, "3": "akBottom", akBottom: 3};
  this.$rtti.$Enum("TAnchorKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TAnchorKind});
  this.$rtti.$Set("TAnchors",{comptype: this.$rtti["TAnchorKind"]});
  this.TBevelCut = {"0": "bvNone", bvNone: 0, "1": "bvLowered", bvLowered: 1, "2": "bvRaised", bvRaised: 2, "3": "bvSpace", bvSpace: 3};
  this.$rtti.$Enum("TBevelCut",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TBevelCut});
  this.TFormBorderStyle = {"0": "bsNone", bsNone: 0, "1": "bsSingle", bsSingle: 1, "2": "bsSizeable", bsSizeable: 2, "3": "bsDialog", bsDialog: 3, "4": "bsToolWindow", bsToolWindow: 4, "5": "bsSizeToolWin", bsSizeToolWin: 5};
  this.$rtti.$inherited("TCaption",rtl.string,{});
  this.$rtti.$Int("TCursor",{minvalue: -32768, maxvalue: 32767, ordtype: 2});
  rtl.createClass(this,"TControlCanvas",pas.Graphics.TCanvas,function () {
    this.$init = function () {
      pas.Graphics.TCanvas.$init.call(this);
      this.FControl = null;
      this.FHeight = 0;
      this.FWidth = 0;
    };
    this.$final = function () {
      this.FControl = undefined;
      pas.Graphics.TCanvas.$final.call(this);
    };
    this.SetHeight = function (AValue) {
      if (this.FHeight !== AValue) {
        this.FHeight = AValue;
        this.FCanvasElement.height = this.FHeight;
      };
    };
    this.SetWidth = function (AValue) {
      if (this.FWidth !== AValue) {
        this.FWidth = AValue;
        this.FCanvasElement.width = this.FWidth;
      };
    };
    this.Create$2 = function (AControl) {
      pas.Graphics.TCanvas.Create$1.call(this);
      if (AControl != null) {
        this.SetHeight(AControl.FHeight);
        this.SetWidth(AControl.FWidth);
        this.FFont.Assign(AControl.FFont);
        this.FBrush.SetColor(AControl.FColor);
        this.FPen.SetColor(AControl.FFont.FColor);
        this.FControl = AControl;
        this.FControl.FHandleElement.insertBefore(this.FCanvasElement,AControl.FHandleElement.firstChild);
        this.FControl.Invalidate();
      };
      return this;
    };
  });
  this.TShiftStateEnum = {"0": "ssShift", ssShift: 0, "1": "ssAlt", ssAlt: 1, "2": "ssCtrl", ssCtrl: 2, "3": "ssLeft", ssLeft: 3, "4": "ssRight", ssRight: 4, "5": "ssMIDdle", ssMIDdle: 5, "6": "ssDouble", ssDouble: 6};
  this.$rtti.$Enum("TShiftStateEnum",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TShiftStateEnum});
  this.$rtti.$Set("TShiftState",{comptype: this.$rtti["TShiftStateEnum"]});
  this.$rtti.$MethodVar("TKeyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.nativeint,1],["Shift",this.$rtti["TShiftState"]]]), methodkind: 0});
  this.$rtti.$MethodVar("TKeyPressEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.char,1]]), methodkind: 0});
  this.TMouseButton = {"0": "mbLeft", mbLeft: 0, "1": "mbRight", mbRight: 1, "2": "mbMiddle", mbMiddle: 2};
  this.$rtti.$Enum("TMouseButton",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TMouseButton});
  this.$rtti.$MethodVar("TMouseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",this.$rtti["TMouseButton"]],["Shift",this.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  this.$rtti.$MethodVar("TMouseMoveEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",this.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  this.$rtti.$MethodVar("TMouseWheelEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",this.$rtti["TShiftState"]],["WheelDelta",rtl.nativeint],["MousePos",pas.Types.$rtti["TPoint"]],["Handled",rtl.boolean,1]]), methodkind: 0});
  this.TFocusSearchDirection = {"0": "fsdFirst", fsdFirst: 0, "1": "fsdLast", fsdLast: 1, "2": "fsdNext", fsdNext: 2, "3": "fsdPrev", fsdPrev: 3};
  this.TControlFlag = {"0": "cfInAlignControls", cfInAlignControls: 0};
  rtl.createClass(this,"TControlBorderSpacing",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FAround = 0;
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetAround = function (AValue) {
      if (this.FAround !== AValue) {
        this.FAround = AValue;
        this.Changed();
      };
    };
    this.SetBottom = function (AValue) {
      if (this.FBottom !== AValue) {
        this.FBottom = AValue;
        this.Changed();
      };
    };
    this.SetLeft = function (AValue) {
      if (this.FLeft !== AValue) {
        this.FLeft = AValue;
        this.Changed();
      };
    };
    this.SetRight = function (AValue) {
      if (this.FRight !== AValue) {
        this.FRight = AValue;
        this.Changed();
      };
    };
    this.SetTop = function (AValue) {
      if (this.FTop !== AValue) {
        this.FTop = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VSpacing = null;
      if ((Source != null) && $mod.TControlBorderSpacing.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VSpacing = Source;
          this.FAround = VSpacing.FAround;
          this.FBottom = VSpacing.FBottom;
          this.FLeft = VSpacing.FLeft;
          this.FRight = VSpacing.FRight;
          this.FTop = VSpacing.FTop;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Around",2,rtl.nativeint,"FAround","SetAround");
    $r.addProperty("Bottom",2,rtl.nativeint,"FBottom","SetBottom");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("Right",2,rtl.nativeint,"FRight","SetRight");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TControl",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAlign = 0;
      this.FAnchors = {};
      this.FAutoSize = false;
      this.FBorderSpacing = null;
      this.FBorderStyle = $mod.TFormBorderStyle.bsNone;
      this.FCaption = "";
      this.FColor = 0;
      this.FControlFlags = {};
      this.FControls = null;
      this.FCursor = 0;
      this.FDesignRect = pas.Types.TRect.$new();
      this.FEnabled = false;
      this.FFont = null;
      this.FHandleClass = "";
      this.FHandleElement = null;
      this.FHandleId = "";
      this.FHeight = 0;
      this.FHint = "";
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = false;
      this.FParentShowHint = false;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = false;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = false;
      this.FWidth = 0;
      this.FOnClick = null;
      this.FOnDblClick = null;
      this.FOnMouseDown = null;
      this.FOnMouseEnter = null;
      this.FOnMouseLeave = null;
      this.FOnMouseMove = null;
      this.FOnMouseUp = null;
      this.FOnMouseWheel = null;
      this.FOnResize = null;
      this.FOnScroll = null;
    };
    this.$final = function () {
      this.FAnchors = undefined;
      this.FBorderSpacing = undefined;
      this.FControlFlags = undefined;
      this.FControls = undefined;
      this.FDesignRect = undefined;
      this.FFont = undefined;
      this.FHandleElement = undefined;
      this.FParent = undefined;
      this.FOnClick = undefined;
      this.FOnDblClick = undefined;
      this.FOnMouseDown = undefined;
      this.FOnMouseEnter = undefined;
      this.FOnMouseLeave = undefined;
      this.FOnMouseMove = undefined;
      this.FOnMouseUp = undefined;
      this.FOnMouseWheel = undefined;
      this.FOnResize = undefined;
      this.FOnScroll = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetClientHeight = function () {
      var Result = 0;
      Result = this.GetClientRect().Bottom;
      return Result;
    };
    this.GetClientRect = function () {
      var Result = pas.Types.TRect.$new();
      Result.$assign(pas.Types.Rect(0,0,this.FWidth - 1,this.FHeight - 1));
      return Result;
    };
    this.GetClientWidth = function () {
      var Result = 0;
      Result = this.GetClientRect().Right;
      return Result;
    };
    this.GetText = function () {
      var Result = "";
      Result = this.RealGetText();
      return Result;
    };
    this.IsAnchorsStored = function () {
      var Result = false;
      Result = rtl.neSet(this.FAnchors,$mod.AnchorAlign[this.FAlign]);
      return Result;
    };
    this.SetAlign = function (AValue) {
      var oldalign = 0;
      if (this.FAlign !== AValue) {
        oldalign = this.FAlign;
        this.FAlign = AValue;
        if (rtl.eqSet(this.FAnchors,$mod.AnchorAlign[oldalign]) && rtl.neSet(this.FAnchors,$mod.AnchorAlign[this.FAlign])) this.SetAnchors(rtl.refSet($mod.AnchorAlign[this.FAlign]));
        if (this.FParent != null) {
          this.FParent.ReAlign()}
         else this.ReAlign();
      };
    };
    this.SetAnchors = function (AValue) {
      if (rtl.eqSet(this.FAnchors,AValue)) return;
      this.FAnchors = rtl.refSet(AValue);
    };
    this.SetAutoSize = function (AValue) {
      if (this.FAutoSize !== AValue) {
        this.FAutoSize = AValue;
        if (this.FAutoSize) {
          this.AdjustSize();
        };
      };
    };
    this.SetBorderSpacing = function (AValue) {
      this.FBorderSpacing.Assign(AValue);
    };
    this.SetClientSize = function (AValue) {
      var VClient = pas.Types.TRect.$new();
      VClient.$assign(this.GetClientRect());
      this.SetBounds(this.FLeft,this.FTop,(this.FWidth - VClient.Right) + AValue.x,(this.FHeight - VClient.Bottom) + AValue.y);
    };
    this.SetClientHeight = function (AValue) {
      this.SetClientSize(pas.Types.TPoint.$clone(pas.Types.Point(this.GetClientWidth(),AValue)));
    };
    this.SetClientWidth = function (AValue) {
      this.SetClientSize(pas.Types.TPoint.$clone(pas.Types.Point(AValue,this.GetClientHeight())));
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.FParentColor = false;
        this.ColorChanged(this);
      };
    };
    this.SetCursor = function (AValue) {
      if (this.FCursor !== AValue) {
        this.FCursor = AValue;
        this.Changed();
      };
    };
    this.SetEnabled = function (AValue) {
      if (this.FEnabled !== AValue) {
        this.FEnabled = AValue;
        this.Changed();
      };
    };
    this.SetFont = function (AValue) {
      if (!this.FFont.IsEqual(AValue)) {
        this.FFont.Assign(AValue);
      };
    };
    this.SetHandleClass = function (AValue) {
      if (this.FHandleClass !== AValue) {
        this.FHandleClass = AValue;
        this.Changed();
      };
    };
    this.SetHandleId = function (AValue) {
      if (this.FHandleId !== AValue) {
        this.FHandleId = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,this.FWidth,AValue);
    };
    this.SetHint = function (AValue) {
      if (this.FHint !== AValue) {
        this.FHint = AValue;
        this.Changed();
      };
    };
    this.SetLeft = function (AValue) {
      this.SetBounds(AValue,this.FTop,this.FWidth,this.FHeight);
    };
    this.SetParent = function (AValue) {
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      this.CheckNewParent(AValue);
      this.FParent = AValue;
      if (this.FParent != null) {
        this.FParent.RegisterChild(this);
        this.BeginUpdate();
        try {
          if (this.FParentColor) {
            this.FColor = this.FParent.FColor;
          };
          if (this.FParentFont) {
            this.FFont.Assign(this.FParent.FFont);
          };
          if (this.FParentShowHint) {
            this.FShowHint = this.FParent.FShowHint;
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetParentColor = function (AValue) {
      if (this.FParentColor !== AValue) {
        this.FParentColor = AValue;
        if (this.FParentColor && (this.FParent != null)) {
          this.FColor = this.FParent.FColor;
          this.Changed();
        };
      };
    };
    this.SetParentFont = function (AValue) {
      if (this.FParentFont !== AValue) {
        this.FParentFont = AValue;
        if (this.FParentFont && (this.FParent != null) && !this.FFont.IsEqual(this.FParent.FFont)) {
          this.FFont.Assign(this.FParent.FFont);
        };
      };
    };
    this.SetParentShowHint = function (AValue) {
      if (this.FParentShowHint !== AValue) {
        this.FParentShowHint = AValue;
        if (this.FParentShowHint && (this.FParent != null)) {
          this.FShowHint = this.FParent.FShowHint;
          this.Changed();
        };
      };
    };
    this.SetShowHint = function (AValue) {
      if (this.FShowHint !== AValue) {
        this.FShowHint = AValue;
        this.FParentShowHint = false;
        this.Changed();
      };
    };
    this.SetTabOrder = function (AValue) {
      if (this.FTabOrder !== AValue) {
        this.FTabOrder = AValue;
        if (this.FParent != null) {
          this.FParent.UpdateTabOrder(this);
        };
      };
    };
    this.SetTabStop = function (AValue) {
      if (this.FTabStop !== AValue) {
        this.FTabStop = AValue;
        this.Changed();
      };
    };
    this.SetText = function (AValue) {
      this.RealSetText(AValue);
    };
    this.SetTop = function (AValue) {
      this.SetBounds(this.FLeft,AValue,this.FWidth,this.FHeight);
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.ReAlign();
      };
    };
    this.SetWidth = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,AValue,this.FHeight);
    };
    this.SetBorderStyle = function (AValue) {
      if (this.FBorderStyle !== AValue) {
        this.FBorderStyle = AValue;
        this.Changed();
      };
    };
    this.Click = function () {
      if (this.FOnClick != null) {
        this.FOnClick(this);
      };
    };
    this.DblClick = function () {
      if (this.FOnDblClick != null) {
        this.FOnDblClick(this);
      };
    };
    this.DoResize = function () {
      if (this.FOnResize != null) {
        this.FOnResize(this);
      };
    };
    this.DoScroll = function () {
      if (this.FOnScroll != null) {
        this.FOnScroll(this);
      };
    };
    this.MouseDown = function (Button, Shift, X, Y) {
      if (this.FOnMouseDown != null) {
        this.FOnMouseDown(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseEnter = function () {
      if (this.FOnMouseEnter != null) {
        this.FOnMouseEnter(this);
      };
    };
    this.MouseLeave = function () {
      if (this.FOnMouseLeave != null) {
        this.FOnMouseLeave(this);
      };
    };
    this.MouseMove = function (Shift, X, Y) {
      if (this.FOnMouseMove != null) {
        this.FOnMouseMove(this,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseUp = function (Button, Shift, X, Y) {
      if (this.FOnMouseUp != null) {
        this.FOnMouseUp(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseWeel = function (Shift, WheelDelta, MousePos, Handled) {
      if (this.FOnMouseWheel != null) {
        this.FOnMouseWheel(this,rtl.refSet(Shift),WheelDelta,pas.Types.TPoint.$clone(MousePos),Handled);
      };
    };
    this.HandleClick = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.Click();
      Result = true;
      return Result;
    };
    this.HandleDblClick = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DblClick();
      Result = true;
      return Result;
    };
    this.HandleMouseDown = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      AEvent.stopPropagation();
      this.MouseDown(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseEnter = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.MouseEnter();
      Result = true;
      return Result;
    };
    this.HandleMouseLeave = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.MouseLeave();
      Result = true;
      return Result;
    };
    this.HandleMouseMove = function (AEvent) {
      var Result = false;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      AEvent.stopPropagation();
      this.MouseMove(rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseUp = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      AEvent.stopPropagation();
      this.MouseUp(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseWheel = function (AEvent) {
      var Result = false;
      var VDelta = 0;
      var VHandled = false;
      var VMousePos = pas.Types.TPoint.$new();
      var VShift = {};
      var VOffSets = pas.Types.TRect.$new();
      VDelta = pas.System.Trunc(-AEvent.deltaY);
      VHandled = false;
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VMousePos.$assign(pas.Types.Point(VOffSets.Left,VOffSets.Top));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      AEvent.stopPropagation();
      this.MouseWeel(rtl.refSet(VShift),VDelta,pas.Types.TPoint.$clone(VMousePos),{get: function () {
          return VHandled;
        }, set: function (v) {
          VHandled = v;
        }});
      Result = true;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoResize();
      Result = true;
      return Result;
    };
    this.HandleScroll = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoScroll();
      Result = true;
      return Result;
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.call(this);
      this.FDesignRect.$assign(pas.Types.Rect(this.FLeft,this.FTop,(this.FLeft + this.FWidth) - 1,(this.FTop + this.FHeight) - 1));
      this.Changed();
    };
    this.Changed = function () {
      var $Self = this;
      var form = null;
      function AdjustWithPPI(aValue) {
        var Result = 0;
        if (form != null) {
          Result = pas.System.Trunc((96 * aValue) / form.FDesignTimePPI)}
         else Result = aValue;
        return Result;
      };
      function FindParentForm() {
        var Result = null;
        var p = null;
        p = $Self.FParent;
        while ((p != null) && !pas.Forms.TCustomForm.isPrototypeOf(p)) p = p.FParent;
        if (pas.Forms.TCustomForm.isPrototypeOf(p)) {
          Result = p}
         else Result = null;
        return Result;
      };
      if (!this.IsUpdating() && rtl.eqSet(rtl.intersectSet(rtl.createSet(0,3),this.FComponentState),{})) {
        form = FindParentForm();
        var $with = this.FHandleElement;
        if (this.FHandleId !== "") {
          $with.setAttribute("id",this.FHandleId);
        } else {
          $with.removeAttribute("id");
        };
        if (this.FHandleClass !== "") {
          $with.setAttribute("class",this.FHandleClass);
        } else {
          $with.removeAttribute("class");
        };
        if ((this.FHandleClass === "") && (this.FHandleId === "")) {
          $with.style.setProperty("color",pas.Graphics.JSColor(this.FFont.FColor));
          $mod.UpdateHtmlElementFont(this.FHandleElement,this.FFont,false);
          if (this.FColor in rtl.createSet(536870912,536870911)) {
            $with.style.removeProperty("background-color");
          } else {
            $with.style.setProperty("background-color",pas.Graphics.JSColor(this.FColor));
          };
        };
        $with.style.setProperty("left",pas.SysUtils.IntToStr(AdjustWithPPI(this.FLeft)) + "px");
        $with.style.setProperty("top",pas.SysUtils.IntToStr(AdjustWithPPI(this.FTop)) + "px");
        $with.style.setProperty("width",pas.SysUtils.IntToStr(AdjustWithPPI(this.FWidth)) + "px");
        $with.style.setProperty("height",pas.SysUtils.IntToStr(AdjustWithPPI(this.FHeight)) + "px");
        $with.style.setProperty("cursor",$mod.JSCursor(this.FCursor));
        if (this.FEnabled) {
          $with.removeAttribute("disabled");
          $with.style.removeProperty("opacity");
        } else {
          $with.setAttribute("disabled","true");
          $with.style.setProperty("opacity","0.5");
        };
        if (this.FVisible) {
          $with.style.setProperty("visibility","visible");
          $with.style.setProperty("display","block");
        } else {
          $with.style.setProperty("visibility","hidden");
          $with.style.setProperty("display","none");
        };
        if ((this.FHint !== "") && this.FShowHint) {
          $with.setAttribute("title",this.FHint);
        } else {
          $with.removeAttribute("title");
        };
        if (this.FBorderStyle === 0) {
          $with.style.setProperty("border-style","none");
        } else {
          $with.style.removeProperty("border-style");
        };
        $with.setAttribute("tabindex",$mod.IfThen$3(this.FTabStop,"1","-1"));
        $with.style.setProperty("position","absolute");
        $with.style.setProperty("overflow","hidden");
        $with.style.setProperty("-webkit-box-sizing","border-box");
        $with.style.setProperty("-moz-box-sizing","border-box");
        $with.style.setProperty("box-sizing","border-box");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      throw new Error(pas.SysUtils.Format("%s.CreateHandleElement=nil",pas.System.VarRecs(18,this.$classname)));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      var $with = this.FHandleElement;
      $with.addEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with.addEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with.addEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with.addEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with.addEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with.addEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with.addEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with.addEventListener("scroll",rtl.createSafeCallback(this,"HandleScroll"));
      $with.addEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      $with.addEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
    };
    this.UnRegisterHandleEvents = function () {
      var $with = this.FHandleElement;
      $with.removeEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with.removeEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with.removeEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with.removeEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with.removeEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with.removeEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with.removeEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with.removeEventListener("scroll",rtl.createSafeCallback(this,"HandleScroll"));
      $with.removeEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      $with.removeEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
    };
    this.CheckNewParent = function (AParent) {
      if ((AParent != null) && !AParent.CheckChildClassAllowed(this.$class.ClassType())) {
        throw new Error(pas.SysUtils.Format("Control of class '%s' can't have control of class '%s' as a child",pas.System.VarRecs(8,AParent.$class.ClassType(),18,this.$classname)));
      };
      if (pas.Forms.TCustomForm.isPrototypeOf(this) && pas.Forms.TCustomForm.isPrototypeOf(AParent)) {
        throw new Error('A "Form" can\'t have another "Form" as parent');
      };
      if (this === AParent) {
        throw new Error('A "Control" can\'t have itself as a Parent');
      };
    };
    this.RegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex < 0) {
          this.FControls.push(AControl);
          if (!this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.appendChild(AControl.FHandleElement);
          };
          this.ReAlign();
          AControl.SetTabOrder(this.FControls.length);
        };
      };
    };
    this.UnRegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex >= 0) {
          this.FControls.splice(VIndex,1);
          if (this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.removeChild(AControl.FHandleElement);
          };
          this.ReAlign();
          this.UpdateTabOrder(null);
        };
      };
    };
    this.AlignControls = function () {
      var $Self = this;
      var VControl = null;
      var VSpacing = null;
      var VIndex = 0;
      var VLeft = 0;
      var VTop = 0;
      var VRight = 0;
      var VBotton = 0;
      var VWidth = 0;
      var newleft = 0;
      var newtop = 0;
      var newright = 0;
      var newbottom = 0;
      if (0 in this.FControlFlags) return;
      this.FControlFlags = rtl.includeSet(this.FControlFlags,0);
      this.BeginUpdate();
      try {
        VLeft = 0;
        VTop = 0;
        VRight = this.FWidth;
        VBotton = this.FHeight;
        VWidth = this.FWidth;
        for (var $l = 0, $end = this.FControls.length - 1; $l <= $end; $l++) {
          VIndex = $l;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 1) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              VControl.SetHeight(VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
            VTop = VTop + VControl.FHeight + VSpacing.FTop + VSpacing.FBottom + (VSpacing.FAround * 2);
          };
        };
        if (VTop < 0) {
          VTop = 0;
        };
        for (var $l1 = 0, $end1 = this.FControls.length - 1; $l1 <= $end1; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 2) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              if (!(0 in VControl.FAnchors)) {
                VControl.SetTop(VBotton - VControl.FHeight - VSpacing.FBottom - VSpacing.FAround)}
               else VControl.SetTop(VControl.FTop);
              VControl.SetWidth(VWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              if (!(0 in VControl.FAnchors)) {
                VControl.SetHeight(VControl.FHeight)}
               else VControl.SetHeight(VBotton - VControl.FTop);
            } finally {
              VControl.EndUpdate();
            };
            VBotton = VBotton - VControl.FHeight - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2);
          };
        };
        if (VBotton < 0) {
          VBotton = 0;
        };
        for (var $l2 = 0, $end2 = this.FControls.length - 1; $l2 <= $end2; $l2++) {
          VIndex = $l2;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 3) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VControl.FWidth);
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VLeft = VLeft + VControl.FWidth + VSpacing.FLeft + VSpacing.FRight + (VSpacing.FAround * 2);
          };
        };
        if (VLeft < 0) {
          VLeft = 0;
        };
        for (var $l3 = 0, $end3 = this.FControls.length - 1; $l3 <= $end3; $l3++) {
          VIndex = $l3;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 4) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              if (!(1 in VControl.FAnchors)) {
                VControl.SetLeft(VRight - VControl.FWidth - VSpacing.FRight - VSpacing.FAround)}
               else VControl.SetLeft(VControl.FLeft);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              if (!(1 in VControl.FAnchors)) {
                VControl.SetWidth(VControl.FWidth)}
               else VControl.SetWidth(VRight - VControl.FLeft);
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VRight = VRight - VControl.FWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2);
          };
        };
        if (VRight < 0) {
          VRight = 0;
        };
        for (var $l4 = 0, $end4 = this.FControls.length - 1; $l4 <= $end4; $l4++) {
          VIndex = $l4;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 5) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VRight - VLeft - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
          };
        };
        for (var $l5 = 0, $end5 = this.FControls.length - 1; $l5 <= $end5; $l5++) {
          VIndex = $l5;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 0) && VControl.FVisible && rtl.neSet(VControl.FAnchors,{})) {
            VControl.BeginUpdate();
            try {
              if (1 in VControl.FAnchors) newleft = VControl.FLeft;
              if (0 in VControl.FAnchors) newtop = VControl.FTop;
              if (3 in VControl.FAnchors) newbottom = this.FHeight - (this.FDesignRect.Bottom - VControl.FDesignRect.Bottom);
              if (2 in VControl.FAnchors) newright = this.FWidth - (this.FDesignRect.Right - VControl.FDesignRect.Right);
              if (rtl.leSet(rtl.createSet(1,2),VControl.FAnchors)) {
                VControl.SetLeft(newleft);
                VControl.SetWidth((newright - newleft) + 1);
              } else if (1 in VControl.FAnchors) {
                VControl.SetLeft(newleft)}
               else if (2 in VControl.FAnchors) VControl.SetLeft(newright - VControl.FWidth);
              if (rtl.leSet(rtl.createSet(0,3),VControl.FAnchors)) {
                VControl.SetTop(newtop);
                VControl.SetHeight((newbottom - newtop) + 1);
              } else if (0 in VControl.FAnchors) {
                VControl.SetTop(newtop)}
               else if (3 in VControl.FAnchors) VControl.SetTop(newbottom - VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        this.FControlFlags = rtl.excludeSet(this.FControlFlags,0);
        this.EndUpdate();
      };
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FCaption;
      return Result;
    };
    this.RealSetText = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.Changed();
      };
    };
    this.BorderSpacingChanged = function (Sender) {
      if (this.FParent != null) {
        this.FParent.AlignControls();
      };
    };
    this.ColorChanged = function (Sender) {
      this.Changed();
    };
    this.FontChanged = function (Sender) {
      this.Changed();
    };
    this.TabOrderArray = function () {
      var Result = null;
      Result = this.FControls.slice(0).sort(rtl.createCallback(this,"CompareTabOrder"));
      return Result;
    };
    this.CompareTabOrder = function (A, B) {
      var Result = 0;
      if (pas.System.Assigned(A) && pas.System.Assigned(B) && rtl.isExt(A,$mod.TControl,1) && rtl.isExt(B,$mod.TControl,1)) {
        Result = rtl.getObject(A).FTabOrder - rtl.getObject(B).FTabOrder;
      } else {
        Result = 0;
      };
      return Result;
    };
    this.UpdateTabOrder = function (AValue) {
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      if (AValue != null) {
        for (var $l = 0, $end = this.FControls.length - 1; $l <= $end; $l++) {
          VIndex = $l;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl !== AValue) && (VControl.FTabOrder >= AValue.FTabOrder)) {
            VControl.FTabOrder += 1;
          };
        };
      };
      VArray = this.TabOrderArray();
      try {
        for (var $l1 = 0, $end1 = VArray.length - 1; $l1 <= $end1; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(VArray[VIndex]);
          if (VControl != null) {
            VControl.BeginUpdate();
            try {
              VControl.FTabOrder = VIndex;
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
    };
    this.SetParentComponent = function (AValue) {
      if ($mod.TWinControl.isPrototypeOf(AValue)) this.SetParent(AValue);
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 75;
      Result.cy = 50;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      var sz = pas.Types.TSize.$new();
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FHandleElement = this.CreateHandleElement();
      this.FHandleClass = "";
      this.FHandleId = "";
      this.RegisterHandleEvents();
      this.FControls = new Array();
      this.FBorderSpacing = $mod.TControlBorderSpacing.$create("Create$1");
      this.FBorderSpacing.FOnChange = rtl.createCallback(this,"BorderSpacingChanged");
      this.FBorderStyle = 1;
      this.FFont = pas.Graphics.TFont.$create("Create$1");
      this.FFont.FOnChange = rtl.createCallback(this,"FontChanged");
      this.FAlign = 0;
      this.FAnchors = rtl.createSet(1,0);
      this.FAutoSize = false;
      this.FCaption = "";
      this.FColor = 536870912;
      this.FCursor = 0;
      sz.$assign(this.$class.GetControlClassDefaultSize());
      this.FDesignRect.$assign(pas.Types.Rect(0,0,sz.cx - 1,sz.cy - 1));
      this.FEnabled = true;
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = true;
      this.FParentShowHint = true;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = true;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = true;
      return this;
    };
    this.Destroy = function () {
      if (this.FHandleElement != null) this.UnRegisterHandleEvents();
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      if (this.FControls != null) this.FControls.length = 0;
      rtl.free(this,"FBorderSpacing");
      this.FBorderSpacing = null;
      rtl.free(this,"FFont");
      this.FFont = null;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.AdjustSize = function () {
    };
    this.IsUpdating = function () {
      var Result = false;
      Result = this.FUpdateCount > 0;
      return Result;
    };
    this.Invalidate = function () {
    };
    this.ReAlign = function () {
      this.AlignControls();
      if (this.FParent != null) {
        this.FParent.ReAlign();
      };
      this.Invalidate();
    };
    this.BringToFront = function () {
      var VParentElement = null;
      VParentElement = this.FHandleElement.parentElement;
      if (VParentElement != null) {
        VParentElement.removeChild(this.FHandleElement);
        VParentElement.appendChild(this.FHandleElement);
      };
    };
    this.SetBounds = function (ALeft, ATop, AWidth, AHeight) {
      if ((this.FLeft !== ALeft) || (this.FTop !== ATop) || (this.FWidth !== AWidth) || (this.FHeight !== AHeight)) {
        this.FLeft = ALeft;
        this.FTop = ATop;
        if (AWidth > 0) {
          this.FWidth = AWidth;
        } else {
          this.FWidth = 0;
        };
        if (AHeight > 0) {
          this.FHeight = AHeight;
        } else {
          this.FHeight = 0;
        };
        this.Changed();
        this.ReAlign();
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Cursor",2,$mod.$rtti["TCursor"],"FCursor","SetCursor");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("Height",2,rtl.nativeint,"FHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
  });
  rtl.createClass(this,"TWinControl",this.TControl,function () {
    this.$init = function () {
      $mod.TControl.$init.call(this);
      this.FOnEnter = null;
      this.FOnExit = null;
      this.FOnKeyDown = null;
      this.FOnKeyPress = null;
      this.FOnKeyUp = null;
    };
    this.$final = function () {
      this.FOnEnter = undefined;
      this.FOnExit = undefined;
      this.FOnKeyDown = undefined;
      this.FOnKeyPress = undefined;
      this.FOnKeyUp = undefined;
      $mod.TControl.$final.call(this);
    };
    this.DoEnter = function () {
      if (this.FOnEnter != null) {
        this.FOnEnter(this);
      };
    };
    this.DoExit = function () {
      if (this.FOnExit != null) {
        this.FOnExit(this);
      };
    };
    this.KeyDown = function (Key, Shift) {
      if (this.FOnKeyDown != null) {
        this.FOnKeyDown(this,Key,rtl.refSet(Shift));
      };
    };
    this.KeyPress = function (Key) {
      if (this.FOnKeyPress != null) {
        this.FOnKeyPress(this,Key);
      };
    };
    this.KeyUp = function (Key, Shift) {
      if (this.FOnKeyUp != null) {
        this.FOnKeyUp(this,Key,rtl.refSet(Shift));
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VParent.SetActiveControl(this);
          break;
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      this.DoEnter();
      Result = true;
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoExit();
      Result = true;
      return Result;
    };
    this.HandleKeyDown = function (AEvent) {
      var Result = false;
      var VControl = null;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyDown(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyDown({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      } else {
        var $tmp = VKey;
        if ($tmp === 9) {
          if (this.FParent != null) {
            if (0 in VShift) {
              VControl = this.FParent.FindFocusControl(this,3);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,1);
              };
            } else {
              VControl = this.FParent.FindFocusControl(this,2);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,0);
              };
            };
            if ((VControl != null) && VControl.CanSetFocus()) {
              VControl.SetFocus();
            };
            AEvent.preventDefault();
          };
        };
      };
      Result = true;
      return Result;
    };
    this.HandleKeyUp = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyUp(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyUp({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      };
      Result = true;
      return Result;
    };
    this.HandleKeyPress = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = "";
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyPress(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      VKey = $mod.ExtractKeyChar(AEvent);
      if (VKey === "\x00") {
        AEvent.preventDefault();
      } else {
        this.KeyPress({get: function () {
            return VKey;
          }, set: function (v) {
            VKey = v;
          }});
        if (VKey === "\x00") {
          AEvent.preventDefault();
        };
      };
      Result = true;
      return Result;
    };
    this.RegisterHandleEvents = function () {
      $mod.TControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("focus",rtl.createCallback(this,"HandleEnter"));
      $with.addEventListener("blur",rtl.createSafeCallback(this,"HandleExit"));
      $with.addEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with.addEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with.addEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.UnRegisterHandleEvents = function () {
      $mod.TControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("focus",rtl.createCallback(this,"HandleEnter"));
      $with.removeEventListener("blur",rtl.createSafeCallback(this,"HandleExit"));
      $with.removeEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with.removeEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with.removeEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = (AChildClass != null) && AChildClass.InheritsFrom($mod.TControl);
      return Result;
    };
    this.FindFocusControl = function (AStartControl, ADirection) {
      var Result = null;
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      var VTabOrder = 0;
      Result = null;
      VArray = this.TabOrderArray();
      if (VArray.length === 0) {
        return Result;
      };
      try {
        VTabOrder = VArray.indexOf(AStartControl);
        if (VTabOrder < 0) {
          if (ADirection in rtl.createSet(0)) {
            VTabOrder = VArray.length - 1;
          } else {
            VTabOrder = 0;
          };
        };
        var $tmp = ADirection;
        if ($tmp === 0) {
          VControl = rtl.getObject(VArray[0]);
          if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp === 1) {
          VControl = rtl.getObject(VArray[VArray.length - 1]);
          if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp === 2) {
          if (VTabOrder < (VArray.length - 1)) {
            for (var $l = VTabOrder + 1, $end = VArray.length - 1; $l <= $end; $l++) {
              VIndex = $l;
              VControl = rtl.getObject(VArray[VIndex]);
              if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        } else if ($tmp === 3) {
          if (VTabOrder > 0) {
            for (var $l1 = VTabOrder - 1; $l1 >= 0; $l1--) {
              VIndex = $l1;
              VControl = rtl.getObject(VArray[VIndex]);
              if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
      return Result;
    };
    this.CanSetFocus = function () {
      var Result = false;
      var VControl = null;
      VControl = this;
      while (true) {
        if (!VControl.FVisible && VControl.FEnabled) {
          Result = false;
          return Result;
        };
        if (VControl.FParent != null) {
          VControl = VControl.FParent;
        } else {
          break;
        };
      };
      Result = (VControl != null) && pas.Forms.TCustomForm.isPrototypeOf(VControl);
      return Result;
    };
    this.SetFocus = function () {
      this.FHandleElement.focus();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomControl",this.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FCanvas = null;
      this.FOnPaint = null;
    };
    this.$final = function () {
      this.FCanvas = undefined;
      this.FOnPaint = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.GetCanvas = function () {
      var Result = null;
      if (!(this.FCanvas != null)) {
        this.FCanvas = $mod.TControlCanvas.$create("Create$2",[this]);
      };
      Result = this.FCanvas;
      return Result;
    };
    this.ColorChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FBrush.SetColor(this.FColor);
      };
      $mod.TControl.ColorChanged.call(this,Sender);
    };
    this.FontChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FFont.Assign(this.FFont);
      };
      $mod.TControl.FontChanged.call(this,Sender);
    };
    this.Paint = function () {
      if (this.FOnPaint != null) {
        this.FOnPaint(this);
      };
    };
    this.Destroy = function () {
      if (this.FCanvas != null) {
        this.FCanvas.$destroy("Destroy");
        this.FCanvas = null;
      };
      $mod.TControl.Destroy.call(this);
    };
    this.Invalidate = function () {
      $mod.TControl.Invalidate.call(this);
      this.Paint();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.AnchorAlign$a$clone = function (a) {
    var b = [];
    b.length = 7;
    for (var c = 0; c < 7; c++) b[c] = rtl.refSet(a[c]);
    return b;
  };
  this.AnchorAlign = [rtl.createSet(1,0),rtl.createSet(1,0,2),rtl.createSet(1,2,3),rtl.createSet(1,0,3),rtl.createSet(2,0,3),rtl.createSet(1,0,2,3),rtl.createSet(1,0)];
  this.IfThen$3 = function (AExpression, AConsequence, AAlternative) {
    var Result = "";
    if (AExpression) {
      Result = AConsequence;
    } else {
      Result = AAlternative;
    };
    return Result;
  };
  this.OffSets = function (AElement) {
    var Result = pas.Types.TRect.$new();
    Result.$assign(pas.Types.Rect(0,0,0,0));
    if (AElement != null) {
      var $with = AElement.getBoundingClientRect();
      Result.Left = pas.System.Trunc($with.left + window.scrollX);
      Result.Top = pas.System.Trunc($with.top + window.scrollY);
    };
    return Result;
  };
  this.UpdateHtmlElementFont = function (AElement, AFont, AClear) {
    var s = "";
    var $with = AElement.style;
    if (AClear) {
      $with.removeProperty("font-family");
      $with.removeProperty("font-size");
      $with.removeProperty("font-weight");
      $with.removeProperty("font-style");
      $with.removeProperty("text-decoration");
    } else {
      $with.setProperty("font-family",AFont.FName);
      $with.setProperty("font-size",pas.SysUtils.IntToStr(AFont.FSize) + "pt");
      if (0 in AFont.FStyle) {
        $with.setProperty("font-weight","bold")}
       else $with.setProperty("font-weight","");
      $with.setProperty("font-style","normal");
      s = "";
      if (1 in AFont.FStyle) s = "italic";
      if (2 in AFont.FStyle) {
        if (s !== "") s = s + " ";
        s = s + "underline";
      };
      if (3 in AFont.FStyle) {
        if (s !== "") s = s + " ";
        s = s + "line-through";
      };
      if (s !== "") {
        $with.setProperty("text-decoration",s)}
       else $with.removeProperty("text-decoration");
    };
  };
  this.ExtractKeyCode = function (AEvent) {
    var Result = 0;
    var VLocation = 0;
    var VKey = "";
    VLocation = AEvent.location;
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = -1;
    var $tmp = VKey;
    if ($tmp === "backspace") {
      Result = 8}
     else if ($tmp === "tab") {
      Result = 9}
     else if ($tmp === "enter") {
      Result = 13}
     else if ($tmp === "shift") {
      Result = 16}
     else if ($tmp === "control") {
      Result = 17}
     else if ($tmp === "alt") {
      Result = 18}
     else if ($tmp === "altgraph") {
      Result = 18}
     else if ($tmp === "pause") {
      Result = 19}
     else if ($tmp === "capslock") {
      Result = 20}
     else if ($tmp === "escape") {
      Result = 27}
     else if ($tmp === "pageup") {
      Result = 33}
     else if ($tmp === "pagedown") {
      Result = 34}
     else if ($tmp === "end") {
      Result = 35}
     else if ($tmp === "home") {
      Result = 36}
     else if ($tmp === "arrowleft") {
      Result = 37}
     else if ($tmp === "arrowup") {
      Result = 38}
     else if ($tmp === "arrowright") {
      Result = 39}
     else if ($tmp === "arrowdown") {
      Result = 40}
     else if ($tmp === "insert") {
      Result = 45}
     else if ($tmp === "delete") {
      Result = 46}
     else if ($tmp === "f1") {
      Result = 112}
     else if ($tmp === "f2") {
      Result = 113}
     else if ($tmp === "f3") {
      Result = 114}
     else if ($tmp === "f4") {
      Result = 115}
     else if ($tmp === "f5") {
      Result = 116}
     else if ($tmp === "f6") {
      Result = 117}
     else if ($tmp === "f7") {
      Result = 118}
     else if ($tmp === "f8") {
      Result = 119}
     else if ($tmp === "f9") {
      Result = 120}
     else if ($tmp === "f10") {
      Result = 121}
     else if ($tmp === "f11") {
      Result = 122}
     else if ($tmp === "f12") {
      Result = 123}
     else if ($tmp === "f13") {
      Result = 124}
     else if ($tmp === "f14") {
      Result = 125}
     else if ($tmp === "f15") {
      Result = 126}
     else if ($tmp === "f16") {
      Result = 127}
     else if ($tmp === "f17") {
      Result = 128}
     else if ($tmp === "f18") {
      Result = 129}
     else if ($tmp === "f19") {
      Result = 130}
     else if ($tmp === "f20") {
      Result = 131}
     else if ($tmp === "numlock") {
      Result = 144}
     else if ($tmp === "scrolllock") Result = 145;
    if (VLocation === 3) {
      var $tmp1 = VKey;
      if ($tmp1 === "0") {
        Result = 96}
       else if ($tmp1 === "1") {
        Result = 97}
       else if ($tmp1 === "2") {
        Result = 98}
       else if ($tmp1 === "3") {
        Result = 99}
       else if ($tmp1 === "4") {
        Result = 100}
       else if ($tmp1 === "5") {
        Result = 101}
       else if ($tmp1 === "6") {
        Result = 102}
       else if ($tmp1 === "7") {
        Result = 103}
       else if ($tmp1 === "8") {
        Result = 104}
       else if ($tmp1 === "9") {
        Result = 105}
       else if ($tmp1 === "*") {
        Result = 106}
       else if ($tmp1 === "+") {
        Result = 107}
       else if ($tmp1 === "-") {
        Result = 109}
       else if ($tmp1 === ",") {
        Result = 110}
       else if ($tmp1 === "/") {
        Result = 111}
       else if ($tmp1 === ".") Result = 194;
    } else {
      var $tmp2 = VKey;
      if ($tmp2 === "0") {
        Result = 48}
       else if ($tmp2 === "1") {
        Result = 49}
       else if ($tmp2 === "2") {
        Result = 50}
       else if ($tmp2 === "3") {
        Result = 51}
       else if ($tmp2 === "4") {
        Result = 52}
       else if ($tmp2 === "5") {
        Result = 53}
       else if ($tmp2 === "6") {
        Result = 54}
       else if ($tmp2 === "7") {
        Result = 55}
       else if ($tmp2 === "8") {
        Result = 56}
       else if ($tmp2 === "9") {
        Result = 57}
       else if ($tmp2 === "ç") {
        Result = 63}
       else if ($tmp2 === "a") {
        Result = 65}
       else if ($tmp2 === "b") {
        Result = 66}
       else if ($tmp2 === "c") {
        Result = 67}
       else if ($tmp2 === "d") {
        Result = 68}
       else if ($tmp2 === "e") {
        Result = 69}
       else if ($tmp2 === "f") {
        Result = 70}
       else if ($tmp2 === "g") {
        Result = 71}
       else if ($tmp2 === "h") {
        Result = 72}
       else if ($tmp2 === "i") {
        Result = 73}
       else if ($tmp2 === "j") {
        Result = 74}
       else if ($tmp2 === "k") {
        Result = 75}
       else if ($tmp2 === "l") {
        Result = 76}
       else if ($tmp2 === "m") {
        Result = 77}
       else if ($tmp2 === "n") {
        Result = 78}
       else if ($tmp2 === "o") {
        Result = 79}
       else if ($tmp2 === "p") {
        Result = 80}
       else if ($tmp2 === "q") {
        Result = 81}
       else if ($tmp2 === "r") {
        Result = 82}
       else if ($tmp2 === "s") {
        Result = 83}
       else if ($tmp2 === "t") {
        Result = 84}
       else if ($tmp2 === "u") {
        Result = 85}
       else if ($tmp2 === "v") {
        Result = 86}
       else if ($tmp2 === "w") {
        Result = 87}
       else if ($tmp2 === "x") {
        Result = 88}
       else if ($tmp2 === "y") {
        Result = 89}
       else if ($tmp2 === "z") {
        Result = 90}
       else if ($tmp2 === "=") {
        Result = 187}
       else if ($tmp2 === ",") {
        Result = 188}
       else if ($tmp2 === "-") {
        Result = 189}
       else if ($tmp2 === ".") {
        Result = 190}
       else if ($tmp2 === "'") {
        Result = 192}
       else if ($tmp2 === "/") {
        Result = 193}
       else if ($tmp2 === "]") {
        Result = 220}
       else if ($tmp2 === "[") Result = 221;
    };
    return Result;
  };
  this.ExtractKeyChar = function (AEvent) {
    var Result = "";
    var VKey = "";
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = "\x00";
    if (VKey.length === 1) {
      Result = VKey.charAt(0);
    } else {
      var $tmp = VKey;
      if ($tmp === "backspace") {
        Result = "\b"}
       else if ($tmp === "tab") {
        Result = "\t"}
       else if ($tmp === "enter") {
        Result = "\r"}
       else if ($tmp === "escape") Result = "\x1B";
    };
    return Result;
  };
  this.ExtractShiftState = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractShiftState$1 = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractMouseButton = function (AEvent) {
    var Result = 0;
    var $tmp = AEvent.button;
    if ($tmp === 0) {
      Result = 0}
     else if ($tmp === 1) {
      Result = 2}
     else if ($tmp === 2) {
      Result = 1}
     else {
      Result = 2;
    };
    return Result;
  };
  this.JSCursor = function (ACursor) {
    var Result = "";
    var $tmp = ACursor;
    if ($tmp === -1) {
      Result = "none"}
     else if ($tmp === -3) {
      Result = "crosshair"}
     else if ($tmp === -4) {
      Result = "text"}
     else if ($tmp === -22) {
      Result = "move"}
     else if ($tmp === -6) {
      Result = "nesw-resize"}
     else if ($tmp === -7) {
      Result = "ns-resize"}
     else if ($tmp === -8) {
      Result = "nwse-resize"}
     else if ($tmp === -9) {
      Result = "ew-resize"}
     else if ($tmp === -23) {
      Result = "nwse-resize"}
     else if ($tmp === -24) {
      Result = "ns-resize"}
     else if ($tmp === -25) {
      Result = "nesw-resize"}
     else if ($tmp === -26) {
      Result = "col-resize"}
     else if ($tmp === -27) {
      Result = "col-resize"}
     else if ($tmp === -28) {
      Result = "nesw-resize"}
     else if ($tmp === -29) {
      Result = "ns-resize"}
     else if ($tmp === -30) {
      Result = "nwse-resize"}
     else if ($tmp === -11) {
      Result = "wait"}
     else if ($tmp === -13) {
      Result = "no-drop"}
     else if ($tmp === -14) {
      Result = "col-resize"}
     else if ($tmp === -15) {
      Result = "row-resize"}
     else if ($tmp === -17) {
      Result = "progress"}
     else if ($tmp === -18) {
      Result = "not-allowed"}
     else if ($tmp === -19) {
      Result = "wait"}
     else if ($tmp === -20) {
      Result = "help"}
     else if ($tmp === -21) {
      Result = "pointer"}
     else {
      Result = "";
    };
    return Result;
  };
  $mod.$implcode = function () {
    $impl.CursorIdents$a$clone = function (a) {
      var b = [];
      b.length = 30;
      for (var c = 0; c < 30; c++) b[c] = pas.Classes.TIdentMapEntry.$clone(a[c]);
      return b;
    };
    $impl.CursorIdents = [pas.Classes.TIdentMapEntry.$clone({Value: 0, Name: "crDefault"}),pas.Classes.TIdentMapEntry.$clone({Value: -1, Name: "crNone"}),pas.Classes.TIdentMapEntry.$clone({Value: -2, Name: "crArrow"}),pas.Classes.TIdentMapEntry.$clone({Value: -3, Name: "crCross"}),pas.Classes.TIdentMapEntry.$clone({Value: -4, Name: "crIBeam"}),pas.Classes.TIdentMapEntry.$clone({Value: -6, Name: "crSizeNESW"}),pas.Classes.TIdentMapEntry.$clone({Value: -7, Name: "crSizeNS"}),pas.Classes.TIdentMapEntry.$clone({Value: -8, Name: "crSizeNWSE"}),pas.Classes.TIdentMapEntry.$clone({Value: -9, Name: "crSizeWE"}),pas.Classes.TIdentMapEntry.$clone({Value: -23, Name: "crSizeNW"}),pas.Classes.TIdentMapEntry.$clone({Value: -24, Name: "crSizeN"}),pas.Classes.TIdentMapEntry.$clone({Value: -25, Name: "crSizeNE"}),pas.Classes.TIdentMapEntry.$clone({Value: -26, Name: "crSizeW"}),pas.Classes.TIdentMapEntry.$clone({Value: -27, Name: "crSizeE"}),pas.Classes.TIdentMapEntry.$clone({Value: -28, Name: "crSizeSW"}),pas.Classes.TIdentMapEntry.$clone({Value: -29, Name: "crSizeS"}),pas.Classes.TIdentMapEntry.$clone({Value: -30, Name: "crSizeSE"}),pas.Classes.TIdentMapEntry.$clone({Value: -10, Name: "crUpArrow"}),pas.Classes.TIdentMapEntry.$clone({Value: -11, Name: "crHourGlass"}),pas.Classes.TIdentMapEntry.$clone({Value: -12, Name: "crDrag"}),pas.Classes.TIdentMapEntry.$clone({Value: -13, Name: "crNoDrop"}),pas.Classes.TIdentMapEntry.$clone({Value: -14, Name: "crHSplit"}),pas.Classes.TIdentMapEntry.$clone({Value: -15, Name: "crVSplit"}),pas.Classes.TIdentMapEntry.$clone({Value: -16, Name: "crMultiDrag"}),pas.Classes.TIdentMapEntry.$clone({Value: -17, Name: "crSQLWait"}),pas.Classes.TIdentMapEntry.$clone({Value: -18, Name: "crNo"}),pas.Classes.TIdentMapEntry.$clone({Value: -19, Name: "crAppStart"}),pas.Classes.TIdentMapEntry.$clone({Value: -20, Name: "crHelp"}),pas.Classes.TIdentMapEntry.$clone({Value: -21, Name: "crHandPoint"}),pas.Classes.TIdentMapEntry.$clone({Value: -22, Name: "crSizeAll"})];
    $impl.CursorToIdent = function (aCursor, aIdent) {
      var Result = false;
      Result = pas.Classes.IntToIdent(aCursor,aIdent,$impl.CursorIdents);
      return Result;
    };
    $impl.IdentToCursor = function (aIdent, aCursor) {
      var Result = false;
      Result = pas.Classes.IdentToInt(aIdent,aCursor,$impl.CursorIdents);
      return Result;
    };
  };
  $mod.$init = function () {
    pas.Classes.RegisterIntegerConsts($mod.$rtti["TCursor"],$impl.IdentToCursor,$impl.CursorToIdent);
  };
},["Forms"]);
rtl.module("WCLStrConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {rsFormResourceSNotFoundForResourcelessFormsCreateNew: {org: "Form resource %s not found. For resourceless forms CreateNew constructor must be used."}, rsFormStreamingError: {org: 'Form streaming "%s" error: %s'}, rsErrUncaughtException: {org: "Uncaught exception of type %s: \n\n%s"}, rsErrUncaughtObject: {org: "Uncaught exception of type %s."}, rsNoTimers: {org: "No more timers available."}};
});
rtl.module("WResources",["System","Classes"],function () {
  "use strict";
  var $mod = this;
  this.InitResourceComponent = function (Instance, RootAncestor) {
    var Result = false;
    function InitComponent(ClassType) {
      var Result = false;
      var data = "";
      var ResName = "";
      var Stream = null;
      var BinStream = null;
      var Reader = null;
      var info = pas.p2jsres.TResourceInfo.$new();
      Result = false;
      if ((ClassType === pas.Classes.TComponent) || (ClassType === RootAncestor)) return Result;
      if (ClassType.$ancestor != null) Result = InitComponent(ClassType.$ancestor);
      Stream = null;
      ResName = ClassType.$module.$name;
      if (!pas.p2jsres.GetResourceInfo(ResName,info)) return Result;
      data = window.atob(info.data);
      if (data !== "") Stream = pas.Classes.TStringStream.$create("Create$2",[data]);
      if (!(Stream != null)) return Result;
      try {
        try {
          BinStream = pas.Classes.TMemoryStream.$create("Create");
          try {
            pas.Classes.ObjectTextToBinary(Stream,BinStream);
            BinStream.SetPosition(0);
            Reader = pas.Classes.TReader.$create("Create$1",[BinStream]);
            try {
              Reader.ReadRootComponent(Instance);
            } finally {
              Reader = rtl.freeLoc(Reader);
            };
          } finally {
            BinStream = rtl.freeLoc(BinStream);
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var E = $e;
            pas.System.Writeln(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsFormStreamingError"),pas.System.VarRecs(18,ClassType.$classname,18,E.fMessage)));
            throw $e;
          } else throw $e
        };
      } finally {
        Stream = rtl.freeLoc(Stream);
      };
      Result = true;
      return Result;
    };
    if (rtl.neSet(rtl.intersectSet(Instance.FComponentState,rtl.createSet(0,9)),{})) {
      Result = InitComponent(Instance.$class.ClassType());
    } else try {
      Result = InitComponent(Instance.$class.ClassType());
    } finally {
    };
    return Result;
  };
  $mod.$init = function () {
    pas.Classes.RegisterInitComponentHandler(pas.Classes.TComponent,$mod.InitResourceComponent);
  };
},["Web","SysUtils","p2jsres","WCLStrConsts"]);
rtl.module("Forms",["System","Classes","SysUtils","Types","JS","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TFormType = {"0": "ftModalForm", ftModalForm: 0, "1": "ftWindow", ftWindow: 1};
  this.TCloseAction = {"0": "caNone", caNone: 0, "1": "caHide", caHide: 1, "2": "caFree", caFree: 2};
  this.$rtti.$Enum("TCloseAction",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TCloseAction});
  this.$rtti.$MethodVar("TCloseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CloseAction",this.$rtti["TCloseAction"],1]]), methodkind: 0});
  this.$rtti.$MethodVar("TCloseQueryEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CanClose",rtl.boolean,1]]), methodkind: 0});
  this.$rtti.$Int("TModalResult",{minvalue: -2147483648, maxvalue: 2147483647, ordtype: 4});
  rtl.createClass(this,"TCustomForm",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 0;
      this.FChildForm = null;
      this.FDesignTimePPI = 0;
      this.FFormType = 0;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.FOnActivate = null;
      this.FOnClose = null;
      this.FOnCloseQuery = null;
      this.FOnCreate = null;
      this.FOnDeactivate = null;
      this.FOnDestroy = null;
      this.FOnHide = null;
      this.FOnResize$1 = null;
      this.FOnScroll$1 = null;
      this.FOnShow = null;
      this.fFormBorderStyle = 0;
    };
    this.$final = function () {
      this.FActiveControl = undefined;
      this.FChildForm = undefined;
      this.FModalResultProc = undefined;
      this.FOverlay = undefined;
      this.FOnActivate = undefined;
      this.FOnClose = undefined;
      this.FOnCloseQuery = undefined;
      this.FOnCreate = undefined;
      this.FOnDeactivate = undefined;
      this.FOnDestroy = undefined;
      this.FOnHide = undefined;
      this.FOnResize$1 = undefined;
      this.FOnScroll$1 = undefined;
      this.FOnShow = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.SetActiveControl = function (AValue) {
      if (this.FActiveControl !== AValue) {
        this.FActiveControl = AValue;
      };
    };
    this.SetAlphaBlend = function (AValue) {
      if (this.FAlphaBlend !== AValue) {
        this.FAlphaBlend = AValue;
        this.Changed();
      };
    };
    this.SetAlphaBlendValue = function (AValue) {
      if (this.FAlphaBlendValue !== AValue) {
        this.FAlphaBlendValue = AValue;
        this.Changed();
      };
    };
    this.SetFormBorderStyle = function (AValue) {
      var bs = pas.Controls.TFormBorderStyle.bsNone;
      if (this.fFormBorderStyle === AValue) return;
      this.fFormBorderStyle = AValue;
      if (AValue in rtl.createSet(null,0,1)) {
        bs = AValue}
       else bs = 0;
      pas.Controls.TControl.SetBorderStyle.call(this,bs);
    };
    this.SetModalResult = function (AValue) {
      if (this.FModalResult !== AValue) {
        this.FModalResult = AValue;
        if ((this.FModalResult !== 0) && (this.FModalResultProc != null)) {
          this.Close();
        };
      };
    };
    this.Activate = function () {
      if (this.FOnActivate != null) {
        this.FOnActivate(this);
      };
    };
    this.Deactivate = function () {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoClose = function (CloseAction) {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoCreate = function () {
      if (this.FOnCreate != null) {
        this.FOnCreate(this);
      };
    };
    this.DoDestroy = function () {
      if (this.FOnDestroy != null) {
        this.FOnDestroy(this);
      };
    };
    this.DoHide = function () {
      if (this.FOnHide != null) {
        this.FOnHide(this);
      };
    };
    this.DoResize = function () {
      pas.Controls.TControl.DoResize.call(this);
      if (this.FOnResize$1 != null) {
        this.FOnResize$1(this);
      };
    };
    this.DoShow = function () {
      if (this.FOnShow != null) {
        this.FOnShow(this);
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VControl = null;
      Result = pas.Controls.TWinControl.HandleEnter.call(this,AEvent);
      if ((this.FChildForm != null) && (this.FChildForm.FFormType === 0)) {
        this.FChildForm.Show();
      } else {
        if (this.FActiveControl != null) {
          VControl = this.FActiveControl;
        } else {
          VControl = this.FindFocusControl(null,0);
        };
        this.FocusControl(VControl);
        this.Activate();
      };
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      Result = pas.Controls.TWinControl.HandleExit.call(this,AEvent);
      this.Deactivate();
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("outline","none");
        if (this.FAlphaBlend) {
          $with.style.setProperty("opacity",pas.SysUtils.FloatToStr(rtl.trunc(this.FAlphaBlendValue / 255)));
        } else {
          $with.style.removeProperty("opacity");
        };
        $with.style.setProperty("overflow","auto");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.ProcessResource = function () {
      if (!pas.WResources.InitResourceComponent(this,$mod.TWForm)) throw pas.Classes.EResNotFound.$create("CreateFmt",[rtl.getResStr(pas.WCLStrConsts,"rsFormResourceSNotFoundForResourcelessFormsCreateNew"),pas.System.VarRecs(18,this.$classname)]);
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 320;
      Result.cy = 240;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      this.CreateNew(AOwner,1);
      if ((this.$class.ClassType() !== $mod.TWForm) && !(4 in this.FComponentState)) {
        this.ProcessResource();
      };
      return this;
    };
    this.CreateNew = function (AOwner, Num) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 255;
      this.FDesignTimePPI = 96;
      this.FChildForm = null;
      this.FFormType = 1;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.SetFormBorderStyle(2);
      this.BeginUpdate();
      try {
        this.SetColor(16777215);
        this.SetParentFont(false);
        this.SetParentShowHint(false);
        this.SetVisible(false);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      this.FActiveControl = null;
      this.FChildForm = null;
      pas.Controls.TCustomControl.Destroy.call(this);
    };
    this.AfterConstruction = function () {
      pas.System.TObject.AfterConstruction.call(this);
      $mod.Application().UpdateMainForm(this);
      $mod.Application().RegisterModule(this);
      this.Loaded();
      this.DoCreate();
    };
    this.BeforeDestruction = function () {
      pas.Classes.TComponent.BeforeDestruction.call(this);
      $mod.Application().UnRegisterModule(this);
      this.DoDestroy();
    };
    this.Close = function () {
      var VAction = 0;
      var VIndex = 0;
      var VOwnerForm = null;
      var VModule = null;
      if (this.CloseQuery()) {
        VAction = 1;
        this.DoClose({get: function () {
            return VAction;
          }, set: function (v) {
            VAction = v;
          }});
        if (VAction !== 0) {
          if ($mod.Application().FMainForm === this) {
            $mod.Application().Terminate();
          } else {
            this.Hide();
            if (this.FFormType === 0) {
              if ((this.FOwner != null) && $mod.TCustomForm.isPrototypeOf(this.FOwner)) {
                VOwnerForm = this.FOwner;
                VOwnerForm.FChildForm = null;
                if (VOwnerForm.FOverlay != null) {
                  VOwnerForm.FOverlay.$destroy("Destroy");
                  VOwnerForm.FOverlay = null;
                };
                VOwnerForm.Show();
              };
              if (this.FModalResultProc != null) {
                this.FModalResultProc(this,this.FModalResult);
              };
            } else {
              for (var $l = $mod.Application().GetModuleCount() - 1; $l >= 0; $l--) {
                VIndex = $l;
                VModule = $mod.Application().GetModule(VIndex);
                if ((VModule != null) && VModule.FVisible && (VModule !== this) && VModule.$class.InheritsFrom($mod.TCustomForm)) {
                  VModule.Show();
                  return;
                };
              };
              if ($mod.Application().FMainForm != null) {
                $mod.Application().FMainForm.Show();
              };
            };
          };
        };
      };
    };
    this.CloseQuery = function () {
      var Result = false;
      Result = true;
      if (this.FOnCloseQuery != null) {
        this.FOnCloseQuery(this,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }});
      };
      return Result;
    };
    this.FocusControl = function (AControl) {
      if ((AControl != null) && AControl.CanSetFocus()) {
        AControl.SetFocus();
      };
    };
    this.Hide = function () {
      this.SetVisible(false);
      this.DoHide();
    };
    this.Loaded = function () {
      pas.Controls.TControl.Loaded.call(this);
    };
    this.Resize = function () {
      var VHeight = 0;
      var VLeft = 0;
      var VTop = 0;
      var VWidth = 0;
      var VWindowHeight = 0;
      var VWindowWidth = 0;
      VWindowWidth = window.innerWidth;
      VWindowHeight = window.innerHeight;
      var $tmp = this.FFormType;
      if ($tmp === 0) {
        VWidth = this.FWidth;
        VHeight = this.FHeight;
        VLeft = rtl.trunc((VWindowWidth - VWidth) / 2);
        VTop = rtl.trunc((VWindowHeight - VHeight) / 2);
        this.SetBounds(VLeft,VTop,VWidth,VHeight);
      } else if ($tmp === 1) {
        this.SetBounds(0,0,VWindowWidth,VWindowHeight);
      };
      this.DoResize();
    };
    this.Show = function () {
      $mod.Application().FActiveForm = this;
      $mod.Application().SetTitle(this.GetText());
      this.BeginUpdate();
      try {
        this.SetVisible(true);
        this.Resize();
      } finally {
        this.EndUpdate();
      };
      this.BringToFront();
      this.SetFocus();
      this.DoShow();
    };
    this.ShowModal = function (AModalResultProc) {
      var VForm = null;
      if (!(this.FOwner != null)) {
        throw new Error("Owner not found.");
      };
      if (!$mod.TCustomForm.isPrototypeOf(this.FOwner)) {
        throw new Error("Invalid owner.");
      };
      VForm = this.FOwner;
      if (VForm.FChildForm != null) {
        throw new Error("Modal form already exists.");
      };
      VForm.FChildForm = this;
      VForm.FOverlay = $impl.TOverlay.$create("Create$1",[VForm]);
      this.FFormType = 0;
      this.FModalResult = 0;
      if (AModalResultProc != null) {
        this.FModalResultProc = AModalResultProc;
      } else {
        this.FModalResultProc = $impl.DefaultModalProc;
      };
      this.Show();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TApplication",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FModules = null;
      this.FActiveForm = null;
      this.FMainForm = null;
      this.FStopOnException = false;
      this.FTerminated = false;
      this.FTitle = "";
      this.FOnResize = null;
      this.FOnUnload = null;
    };
    this.$final = function () {
      this.FModules = undefined;
      this.FActiveForm = undefined;
      this.FMainForm = undefined;
      this.FOnResize = undefined;
      this.FOnUnload = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetApplicatioName = function () {
      var Result = "";
      Result = window.location.pathname;
      return Result;
    };
    this.GetModule = function (AIndex) {
      var Result = null;
      Result = rtl.getObject(this.FModules[AIndex]);
      return Result;
    };
    this.GetModuleCount = function () {
      var Result = 0;
      Result = this.FModules.length;
      return Result;
    };
    this.SetTitle = function (AValue) {
      if (this.FTitle !== AValue) {
        this.FTitle = AValue;
        document.title = this.FTitle;
      };
    };
    this.DoResize = function () {
      if (this.FOnResize != null) {
        this.FOnResize(this);
      };
    };
    this.DoUnload = function () {
      if (this.FOnUnload != null) {
        this.FOnUnload(this);
      };
    };
    this.LoadIcon = function () {
      var $with = document.head.appendChild(document.createElement("link"));
      $with.setAttribute("rel","icon");
      $with.setAttribute("type","image/icon");
      $with.setAttribute("href",this.GetApplicatioName().replace("html","ico"));
    };
    this.RegisterHandleEvents = function () {
      window.addEventListener("error",rtl.createCallback(this,"HandleError"));
      window.addEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      window.addEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    this.UnRegisterHandleEvents = function () {
      window.removeEventListener("error",rtl.createCallback(this,"HandleError"));
      window.removeEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      window.removeEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    var CLE = pas.System.LineEnding;
    var CError = "Error Message: %s " + CLE + "Line Nro: %d " + CLE + "Column Nro: %d " + CLE;
    this.HandleError = function (AEvent) {
      var Result = false;
      if (AEvent.message.toLowerCase().indexOf("script error",0) > -1) {
        window.alert("Script Error: See Browser Console for Detail");
      } else {
        window.alert(pas.SysUtils.Format(CError,pas.System.VarRecs(18,AEvent.message,0,AEvent.lineno,0,AEvent.colno)));
      };
      if (this.FStopOnException) {
        this.Terminate();
      };
      AEvent.stopPropagation();
      Result = false;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      var VControl = null;
      var VIndex = 0;
      AEvent.stopPropagation();
      this.DoResize();
      Result = true;
      for (var $l = 0, $end = this.FModules.length - 1; $l <= $end; $l++) {
        VIndex = $l;
        VControl = rtl.getObject(this.FModules[VIndex]);
        if ((VControl != null) && VControl.FVisible && VControl.$class.InheritsFrom($mod.TCustomForm)) {
          VControl.Resize();
        };
      };
      return Result;
    };
    this.HandleUnload = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      Result = true;
      try {
        this.DoUnload();
      } finally {
        this.Terminate();
      };
      return Result;
    };
    this.HandleException = function (AException) {
      if (pas.SysUtils.Exception.isPrototypeOf(AException)) {
        window.alert(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsErrUncaughtException"),pas.System.VarRecs(18,AException.$classname,18,AException.fMessage)));
      } else {
        window.alert(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsErrUncaughtObject"),pas.System.VarRecs(18,AException.$classname)));
      };
      if (this.FStopOnException) this.Terminate();
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      pas.p2jsres.SetResourceSource(0);
      pas.SysUtils.SetOnUnCaughtExceptionHandler($impl.DoUncaughtPascalException);
      rtl.showUncaughtExceptions=true;
      this.FModules = new Array();
      this.FMainForm = null;
      this.FStopOnException = true;
      this.FTerminated = false;
      this.FTitle = "";
      return this;
    };
    this.Destroy = function () {
      this.FModules.length = 0;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.CreateForm = function (AInstanceClass, AReference) {
      try {
        AReference.set(AInstanceClass.$create("Create$1",[this]));
      } catch ($e) {
        AReference.set(null);
        throw $e;
      };
    };
    this.Initialize = function () {
    };
    this.Run = function () {
      this.RegisterHandleEvents();
      this.LoadIcon();
      if (this.FMainForm != null) {
        this.FMainForm.Show();
      };
    };
    this.Terminate = function () {
      var VModule = null;
      var VIndex = 0;
      if (!this.FTerminated) {
        this.UnRegisterHandleEvents();
        this.FTerminated = true;
        for (var $l = this.FModules.length - 1; $l >= 0; $l--) {
          VIndex = $l;
          VModule = rtl.getObject(this.FModules[VIndex]);
          if (VModule != null) {
            VModule.$destroy("Destroy");
            VModule = null;
          };
        };
      };
    };
    this.UpdateMainForm = function (AForm) {
      if (!(this.FMainForm != null)) {
        this.FMainForm = AForm;
        this.FActiveForm = AForm;
      };
    };
    this.RegisterModule = function (AModule) {
      if (AModule != null) {
        if (this.FModules.indexOf(AModule) === -1) {
          this.FModules.push(AModule);
          if (!document.body.contains(AModule.FHandleElement)) {
            document.body.appendChild(AModule.FHandleElement);
          };
        };
      };
    };
    this.UnRegisterModule = function (AModule) {
      var VIndex = 0;
      if (AModule != null) {
        VIndex = this.FModules.indexOf(AModule);
        if (VIndex >= 0) {
          this.FModules.splice(VIndex,1);
          if (document.body.contains(AModule.FHandleElement)) {
            document.body.removeChild(AModule.FHandleElement);
          };
        };
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TWForm",this.TCustomForm,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("ActiveControl",2,pas.Controls.$rtti["TWinControl"],"FActiveControl","SetActiveControl");
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlphaBlend",2,rtl.boolean,"FAlphaBlend","SetAlphaBlend");
    $r.addProperty("AlphaBlendValue",2,rtl.byte,"FAlphaBlendValue","SetAlphaBlendValue");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("DesignTimePPI",0,rtl.longint,"FDesignTimePPI","FDesignTimePPI");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("KeyPreview",0,rtl.boolean,"FKeyPreview","FKeyPreview");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnActivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnActivate","FOnActivate");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnClose",0,$mod.$rtti["TCloseEvent"],"FOnClose","FOnClose");
    $r.addProperty("OnCloseQuery",0,$mod.$rtti["TCloseQueryEvent"],"FOnCloseQuery","FOnCloseQuery");
    $r.addProperty("OnCreate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnCreate","FOnCreate");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnDeactivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDeactivate","FOnDeactivate");
    $r.addProperty("OnDestroy",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDestroy","FOnDestroy");
    $r.addProperty("OnHide",0,pas.Classes.$rtti["TNotifyEvent"],"FOnHide","FOnHide");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize$1","FOnResize$1");
    $r.addProperty("OnScroll",0,pas.Classes.$rtti["TNotifyEvent"],"FOnScroll$1","FOnScroll$1");
    $r.addProperty("OnShow",0,pas.Classes.$rtti["TNotifyEvent"],"FOnShow","FOnShow");
  });
  this.Application = function () {
    var Result = null;
    if (!($impl.VAppInstance != null)) {
      $impl.VAppInstance = $mod.TApplication.$create("Create$1",[null]);
    };
    Result = $impl.VAppInstance;
    return Result;
  };
  $mod.$implcode = function () {
    $impl.DefaultModalProc = function (Sender, ModalResult) {
      if (Sender != null) {
        Sender.$destroy("Destroy");
        Sender = null;
      };
    };
    $impl.VAppInstance = null;
    rtl.createClass($impl,"TOverlay",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.FForm = null;
        this.FHandleElement = null;
      };
      this.$final = function () {
        this.FForm = undefined;
        this.FHandleElement = undefined;
        pas.System.TObject.$final.call(this);
      };
      this.Create$1 = function (AForm) {
        this.FForm = AForm;
        if (this.FForm != null) {
          this.FHandleElement = document.createElement("div");
          var $with = this.FHandleElement;
          $with.style.setProperty("left","0px");
          $with.style.setProperty("top","0px");
          $with.style.setProperty("height","100%");
          $with.style.setProperty("width","100%");
          $with.style.setProperty("background","rgba(0, 0, 0, 0.6)");
          $with.style.setProperty("position","absolute");
          $with.style.setProperty("overflow","hidden");
          this.FForm.FHandleElement.appendChild(this.FHandleElement);
        };
        return this;
      };
      this.Destroy = function () {
        if (this.FForm != null) {
          this.FForm.FHandleElement.removeChild(this.FHandleElement);
        };
        pas.System.TObject.Destroy.call(this);
      };
    });
    $impl.DoUncaughtPascalException = function (E) {
      $mod.Application().HandleException(E);
    };
  };
},["WResources","WCLStrConsts","p2jsres"]);
rtl.module("Rtl.BrowserLoadHelper",["System","Classes","SysUtils","JS","Web"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TBrowserLoadHelper",pas.Classes.TLoadHelper,function () {
  });
  $mod.$init = function () {
    pas.Classes.SetLoadHelperClass($mod.TBrowserLoadHelper);
  };
});
rtl.module("browserconsole",["System","JS","Web","Rtl.BrowserLoadHelper","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.BrowserLineBreak = "\n";
  this.DefaultMaxConsoleLines = 25;
  this.DefaultConsoleStyle = ".pasconsole { " + this.BrowserLineBreak + "font-family: courier;" + this.BrowserLineBreak + "font-size: 14px;" + this.BrowserLineBreak + "background: #FFFFFF;" + this.BrowserLineBreak + "color: #000000;" + this.BrowserLineBreak + "display: block;" + this.BrowserLineBreak + "}";
  this.ConsoleElementID = "";
  this.ConsoleStyle = "";
  this.MaxConsoleLines = 0;
  this.ConsoleLinesToBrowserLog = false;
  this.ResetConsole = function () {
    if ($impl.LinesParent === null) return;
    while ($impl.LinesParent.firstElementChild !== null) $impl.LinesParent.removeChild($impl.LinesParent.firstElementChild);
    $impl.AppendLine();
  };
  this.InitConsole = function () {
    if ($impl.ConsoleElement === null) return;
    if ($impl.ConsoleElement.nodeName.toLowerCase() !== "body") {
      while ($impl.ConsoleElement.firstElementChild !== null) $impl.ConsoleElement.removeChild($impl.ConsoleElement.firstElementChild);
    };
    $impl.StyleElement = document.createElement("style");
    $impl.StyleElement.innerText = $mod.ConsoleStyle;
    $impl.ConsoleElement.appendChild($impl.StyleElement);
    $impl.LinesParent = document.createElement("div");
    $impl.ConsoleElement.appendChild($impl.LinesParent);
  };
  this.HookConsole = function () {
    $impl.ConsoleElement = null;
    if ($mod.ConsoleElementID !== "") $impl.ConsoleElement = document.getElementById($mod.ConsoleElementID);
    if ($impl.ConsoleElement === null) $impl.ConsoleElement = document.body;
    if ($impl.ConsoleElement === null) return;
    $mod.InitConsole();
    $mod.ResetConsole();
    pas.System.SetWriteCallBack($impl.WriteConsole);
  };
  $mod.$implcode = function () {
    $impl.LastLine = null;
    $impl.StyleElement = null;
    $impl.LinesParent = null;
    $impl.ConsoleElement = null;
    $impl.AppendLine = function () {
      var CurrentCount = 0;
      var S = null;
      CurrentCount = 0;
      S = $impl.LinesParent.firstChild;
      while (S != null) {
        CurrentCount += 1;
        S = S.nextSibling;
      };
      while (CurrentCount > $mod.MaxConsoleLines) {
        CurrentCount -= 1;
        $impl.LinesParent.removeChild($impl.LinesParent.firstChild);
      };
      $impl.LastLine = document.createElement("div");
      $impl.LastLine.className = "pasconsole";
      $impl.LinesParent.appendChild($impl.LastLine);
    };
    $impl.EscapeString = function (S) {
      var Result = "";
      var CL = "";
      CL = pas.SysUtils.StringReplace(S,"<","&lt;",rtl.createSet(0));
      CL = pas.SysUtils.StringReplace(CL,">","&gt;",rtl.createSet(0));
      CL = pas.SysUtils.StringReplace(CL," ","&nbsp;",rtl.createSet(0));
      CL = pas.SysUtils.StringReplace(CL,"\r\n","<br>",rtl.createSet(0));
      CL = pas.SysUtils.StringReplace(CL,"\n","<br>",rtl.createSet(0));
      CL = pas.SysUtils.StringReplace(CL,"\r","<br>",rtl.createSet(0));
      Result = CL;
      return Result;
    };
    $impl.WriteConsole = function (S, NewLine) {
      var CL = "";
      CL = $impl.LastLine.innerHTML;
      CL = CL + $impl.EscapeString("" + S);
      $impl.LastLine.innerHTML = CL;
      if (NewLine) {
        if ($mod.ConsoleLinesToBrowserLog) window.console.log($impl.LastLine.innerText);
        $impl.AppendLine();
      };
    };
  };
  $mod.$init = function () {
    $mod.ConsoleLinesToBrowserLog = true;
    $mod.ConsoleElementID = "pasjsconsole";
    $mod.ConsoleStyle = $mod.DefaultConsoleStyle;
    $mod.MaxConsoleLines = 25;
    $mod.HookConsole();
  };
},[]);
rtl.module("BrowserApp",["System","Classes","SysUtils","Types","JS","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.ReloadEnvironmentStrings = function () {
    var I = 0;
    var S = "";
    var N = "";
    var A = [];
    var P = [];
    if ($impl.EnvNames != null) pas.SysUtils.FreeAndNil({p: $impl, get: function () {
        return this.p.EnvNames;
      }, set: function (v) {
        this.p.EnvNames = v;
      }});
    $impl.EnvNames = new Object();
    S = window.location.search;
    S = pas.System.Copy(S,2,S.length - 1);
    A = S.split("&");
    for (var $l = 0, $end = rtl.length(A) - 1; $l <= $end; $l++) {
      I = $l;
      P = A[I].split("=");
      N = pas.SysUtils.LowerCase(decodeURIComponent(P[0]));
      if (rtl.length(P) === 2) {
        $impl.EnvNames[N] = decodeURIComponent(P[1])}
       else if (rtl.length(P) === 1) $impl.EnvNames[N] = "";
    };
  };
  $mod.$implcode = function () {
    $impl.EnvNames = null;
    $impl.Params = [];
    $impl.ReloadParamStrings = function () {
      $impl.Params = rtl.arraySetLength($impl.Params,"",1);
      $impl.Params[0] = window.location.pathname;
    };
    $impl.GetParamCount = function () {
      var Result = 0;
      Result = rtl.length($impl.Params) - 1;
      return Result;
    };
    $impl.GetParamStr = function (Index) {
      var Result = "";
      Result = $impl.Params[Index];
      return Result;
    };
    $impl.MyGetEnvironmentVariable = function (EnvVar) {
      var Result = "";
      var aName = "";
      aName = pas.SysUtils.LowerCase(EnvVar);
      if ($impl.EnvNames.hasOwnProperty(aName)) {
        Result = "" + $impl.EnvNames[aName]}
       else Result = "";
      return Result;
    };
    $impl.MyGetEnvironmentVariableCount = function () {
      var Result = 0;
      Result = rtl.length(Object.getOwnPropertyNames($impl.EnvNames));
      return Result;
    };
    $impl.MyGetEnvironmentString = function (Index) {
      var Result = "";
      Result = "" + $impl.EnvNames[Object.getOwnPropertyNames($impl.EnvNames)[Index]];
      return Result;
    };
  };
  $mod.$init = function () {
    pas.System.IsConsole = true;
    pas.System.OnParamCount = $impl.GetParamCount;
    pas.System.OnParamStr = $impl.GetParamStr;
    $mod.ReloadEnvironmentStrings();
    $impl.ReloadParamStrings();
    pas.SysUtils.OnGetEnvironmentVariable = $impl.MyGetEnvironmentVariable;
    pas.SysUtils.OnGetEnvironmentVariableCount = $impl.MyGetEnvironmentVariableCount;
    pas.SysUtils.OnGetEnvironmentString = $impl.MyGetEnvironmentString;
  };
},[]);
rtl.module("StdCtrls",["System","Classes","SysUtils","Types","Web","Graphics","Controls","Forms"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomButton",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FModalResult = 0;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("padding","0");
        $with.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("button");
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 80;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FModalResult = 0;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.AdjustSize = function () {
      var VSize = pas.Types.TSize.$new();
      pas.Controls.TControl.AdjustSize.call(this);
      VSize.$assign(this.FFont.TextExtent(this.GetText()));
      this.SetBounds(this.FLeft,this.FTop,VSize.cx,VSize.cy);
    };
    this.Click = function () {
      var VParent = null;
      if (this.FModalResult !== 0) {
        VParent = this.FParent;
        while (VParent != null) {
          if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
            VParent.SetModalResult(this.FModalResult);
            break;
          };
          VParent = VParent.FParent;
        };
      };
      pas.Controls.TControl.Click.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomLabel",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = 0;
      this.FContentElement = null;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = false;
      this.FWordWrap = false;
    };
    this.$final = function () {
      this.FContentElement = undefined;
      this.FFocusControl = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetLayout = function (AValue) {
      if (this.FLayout !== AValue) {
        this.FLayout = AValue;
        this.Changed();
      };
    };
    this.SetTransparent = function (AValue) {
      if (this.FTransparent !== AValue) {
        this.FTransparent = AValue;
        this.BeginUpdate();
        try {
          if (this.FTransparent) {
            this.SetColor(536870911);
          } else if (this.FColor === 536870911) {
            this.SetColor(-2147483647);
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.DoEnter = function () {
      pas.Controls.TWinControl.DoEnter.call(this);
      if ((this.FFocusControl != null) && this.FFocusControl.CanSetFocus()) {
        this.FFocusControl.SetFocus();
      };
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        if (this.FTransparent) {
          $with.style.setProperty("background-color","transparent");
        };
        $with.style.setProperty("outline","none");
        $with.style.setProperty("user-select","none");
        $with.style.setProperty("-moz-user-select","none");
        $with.style.setProperty("-ms-user-select","none");
        $with.style.setProperty("-khtml-user-select","none");
        $with.style.setProperty("-webkit-user-select","none");
        if (this.FAutoSize) {
          $with.style.removeProperty("height");
          $with.style.removeProperty("width");
        };
        var $tmp = this.FAlignment;
        if ($tmp === 2) {
          $with.style.setProperty("text-align","center")}
         else if ($tmp === 0) {
          $with.style.setProperty("text-align","left")}
         else if ($tmp === 1) $with.style.setProperty("text-align","right");
        var $with1 = this.FContentElement;
        $with1.innerHTML = "";
        $with1.style.setProperty("display","table-cell");
        $with1.style.setProperty("width",pas.SysUtils.IntToStr(this.FWidth) + "px");
        $with1.style.setProperty("height",pas.SysUtils.IntToStr(this.FHeight) + "px");
        var $tmp1 = this.FLayout;
        if ($tmp1 === 2) {
          $with1.style.setProperty("vertical-align","bottom")}
         else if ($tmp1 === 1) {
          $with1.style.setProperty("vertical-align","middle")}
         else if ($tmp1 === 0) $with1.style.setProperty("vertical-align","top");
        if (this.FWordWrap) {
          $with1.style.setProperty("word-wrap","break-word");
        } else {
          $with1.style.removeProperty("word-wrap");
        };
        $with1.style.setProperty("overflow","hidden");
        $with1.style.setProperty("text-overflow","ellipsis");
        $with1.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateContentElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("label"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 65;
      Result.cy = 17;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FContentElement = this.CreateContentElement();
      this.FAlignment = 0;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = true;
      this.FWordWrap = false;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        this.SetAutoSize(true);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.AdjustSize = function () {
      pas.Controls.TControl.AdjustSize.call(this);
      this.Changed();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
},["RTLConsts"]);
rtl.module("ExtCtrls",["System","JS","Classes","SysUtils","Types","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomImage",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FCenter = false;
      this.FPicture = null;
      this.FProportional = false;
      this.FStretch = false;
      this.FOnPictureChanged = null;
      this.FStretchInEnabled = false;
      this.FStretchOutEnabled = false;
      this.FTransparent = false;
      this.FURL = "";
    };
    this.$final = function () {
      this.FPicture = undefined;
      this.FOnPictureChanged = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.SetCenter = function (AValue) {
      if (this.FCenter !== AValue) {
        this.FCenter = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetProportional = function (AValue) {
      if (this.FProportional !== AValue) {
        this.FProportional = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetStretch = function (AValue) {
      if (this.FStretch !== AValue) {
        this.FStretch = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetStretchInEnabled = function (AValue) {
      if (this.FStretchInEnabled !== AValue) ;
      this.FStretchInEnabled = AValue;
      this.PictureChanged(this);
    };
    this.SetStretchOutEnabled = function (AValue) {
      if (this.FStretchOutEnabled !== AValue) {
        this.FStretchOutEnabled = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetTransparent = function (AValue) {
      if (this.FTransparent === AValue) {
        this.FTransparent = AValue;
      };
    };
    this.SetURL = function (AValue) {
      if (this.FURL === AValue) return;
      this.FURL = AValue;
      this.PictureChanged(this);
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("outline","none");
        $with.style.setProperty("background-image",pas.SysUtils.Format("url('%s')",pas.System.VarRecs(18,this.FURL)));
        $with.style.setProperty("background-repeat","no-repeat");
        if (this.FCenter) {
          $with.style.setProperty("background-position","center  center");
        } else {
          $with.style.removeProperty("background-position");
        };
        if (this.FProportional) {
          $with.style.setProperty("background-size","contain");
        } else if (this.FStretch) {
          if (this.FStretchInEnabled && this.FStretchOutEnabled) {
            $with.style.setProperty("background-size","100% 100%");
          } else if (this.FStretchInEnabled) {
            $with.style.setProperty("background-size","auto 100%");
          } else if (this.FStretchOutEnabled) {
            $with.style.setProperty("background-size","100% auto");
          };
        } else {
          $with.style.setProperty("background-size","auto");
        };
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.PictureChanged = function (Sender) {
      this.Changed();
      if (this.FOnPictureChanged != null) {
        this.FOnPictureChanged(this);
      };
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 90;
      Result.cy = 90;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FPicture = pas.Graphics.TPicture.$create("Create$1");
      this.FPicture.FOnChange = rtl.createCallback(this,"PictureChanged");
      this.FCenter = false;
      this.FProportional = false;
      this.FStretch = false;
      this.FStretchOutEnabled = true;
      this.FStretchInEnabled = true;
      this.FTransparent = false;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.$rtti.$Int("TBevelWidth",{minvalue: 1, maxvalue: 2147483647, ordtype: 5});
  rtl.createClass(this,"TCustomPanel",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FAlignment = 0;
      this.FBevelColor = 0;
      this.FBevelInner = 0;
      this.FBevelOuter = 0;
      this.FBevelWidth = 0;
      this.FLayout = 0;
      this.FWordWrap = false;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetBevelColor = function (AValue) {
      if (this.FBevelColor !== AValue) {
        this.FBevelColor = AValue;
        this.Changed();
      };
    };
    this.SetBevelInner = function (AValue) {
      if (this.FBevelInner !== AValue) {
        this.FBevelInner = AValue;
        this.Changed();
      };
    };
    this.SetBevelOuter = function (AValue) {
      if (this.FBevelOuter !== AValue) {
        this.FBevelOuter = AValue;
        this.Changed();
      };
    };
    this.SetBevelWidth = function (AValue) {
      if (this.FBevelWidth !== AValue) {
        this.FBevelWidth = AValue;
        this.Changed();
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      var VTopColor = 0;
      var VBottomColor = 0;
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        if (this.FBevelOuter === 0) {
          $with.style.removeProperty("border-width");
          $with.style.removeProperty("border-left-color");
          $with.style.removeProperty("border-left-style");
          $with.style.removeProperty("border-top-color");
          $with.style.removeProperty("border-top-style");
          $with.style.removeProperty("border-right-color");
          $with.style.removeProperty("border-right-style");
          $with.style.removeProperty("border-bottom-color");
          $with.style.removeProperty("border-bottom-style");
        } else {
          if (this.FBevelColor === 536870912) {
            var $tmp = this.FBevelOuter;
            if ($tmp === 1) {
              VTopColor = 8421504;
              VBottomColor = 16777215;
            } else if ($tmp === 2) {
              VTopColor = 16777215;
              VBottomColor = 8421504;
            } else {
              VTopColor = this.FColor;
              VBottomColor = this.FColor;
            };
          } else {
            VTopColor = this.FBevelColor;
            VBottomColor = this.FBevelColor;
          };
          $with.style.setProperty("border-width",pas.SysUtils.IntToStr(this.FBevelWidth) + "px");
          $with.style.setProperty("border-style","solid");
          $with.style.setProperty("border-left-color",pas.Graphics.JSColor(VTopColor));
          $with.style.setProperty("border-top-color",pas.Graphics.JSColor(VTopColor));
          $with.style.setProperty("border-right-color",pas.Graphics.JSColor(VBottomColor));
          $with.style.setProperty("border-bottom-color",pas.Graphics.JSColor(VBottomColor));
        };
        $with.style.setProperty("outline","none");
        $with.style.setProperty("user-select","none");
        $with.style.setProperty("-moz-user-select","none");
        $with.style.setProperty("-ms-user-select","none");
        $with.style.setProperty("-khtml-user-select","none");
        $with.style.setProperty("-webkit-user-select","none");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 170;
      Result.cy = 50;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FAlignment = 2;
      this.FBevelColor = 536870912;
      this.FBevelOuter = 2;
      this.FBevelInner = 0;
      this.FBevelWidth = 1;
      this.FLayout = 1;
      this.FWordWrap = false;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomTimer",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FEnabled = false;
      this.FInterval = 0;
      this.FTimerHandle = 0;
      this.FOnStartTimer = null;
      this.FOnStopTimer = null;
      this.FOnTimer = null;
    };
    this.$final = function () {
      this.FOnStartTimer = undefined;
      this.FOnStopTimer = undefined;
      this.FOnTimer = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetEnabled = function (AValue) {
      if (this.FEnabled === AValue) return;
      this.FEnabled = AValue;
      this.UpdateTimer();
    };
    this.SetInterval = function (AValue) {
      if (this.FInterval === AValue) return;
      this.FInterval = AValue;
      this.UpdateTimer();
    };
    this.SetOnTimer = function (AValue) {
      if (rtl.eqCallback(this.FOnTimer,AValue)) return;
      this.FOnTimer = AValue;
      this.UpdateTimer();
    };
    this.UpdateTimer = function () {
      var $Self = this;
      this.KillTimer();
      if (this.FEnabled && (this.FInterval > 0) && rtl.eqSet(rtl.intersectSet(rtl.createSet(0,3),this.FComponentState),{}) && (this.FOnTimer != null)) {
        this.FTimerHandle = window.setInterval(function () {
          $Self.FOnTimer($Self);
        },this.FInterval);
        if (this.FTimerHandle === 0) throw pas.Classes.EOutOfResources.$create("Create$1",[rtl.getResStr(pas.WCLStrConsts,"rsNoTimers")]);
        if (this.FOnStartTimer != null) this.FOnStartTimer($Self);
      };
    };
    this.KillTimer = function () {
      if (this.FTimerHandle !== 0) {
        window.clearInterval(this.FTimerHandle);
        if (this.FOnStopTimer != null) this.FOnStopTimer(this);
      };
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.call(this);
      this.UpdateTimer();
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FEnabled = true;
      this.FInterval = 1000;
      this.FTimerHandle = 0;
      return this;
    };
    this.Destroy = function () {
      this.KillTimer();
      pas.Classes.TComponent.Destroy.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
},["WCLStrConsts"]);
rtl.module("WebCtrls",["System","Classes","SysUtils","Types","Graphics","Controls","Forms","StdCtrls","ExtCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TWButton",pas.StdCtrls.TCustomButton,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("ModalResult",0,pas.Forms.$rtti["TModalResult"],"FModalResult","FModalResult");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWLabel",pas.StdCtrls.TCustomLabel,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: true});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("FocusControl",0,pas.Controls.$rtti["TWinControl"],"FFocusControl","FFocusControl");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Layout",2,pas.Graphics.$rtti["TTextLayout"],"FLayout","SetLayout");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Transparent",2,rtl.boolean,"FTransparent","SetTransparent");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWImage",pas.ExtCtrls.TCustomImage,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Center",2,rtl.boolean,"FCenter","SetCenter",{Default: false});
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("Proportional",2,rtl.boolean,"FProportional","SetProportional",{Default: false});
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Stretch",2,rtl.boolean,"FStretch","SetStretch",{Default: false});
    $r.addProperty("StretchOutEnabled",2,rtl.boolean,"FStretchOutEnabled","SetStretchOutEnabled",{Default: true});
    $r.addProperty("StretchInEnabled",2,rtl.boolean,"FStretchInEnabled","SetStretchInEnabled",{Default: true});
    $r.addProperty("Transparent",2,rtl.boolean,"FTransparent","SetTransparent",{Default: false});
    $r.addProperty("URL",2,rtl.string,"FURL","SetURL");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnPaint",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPaint","FOnPaint");
    $r.addProperty("OnPictureChanged",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPictureChanged","FOnPictureChanged");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWPanel",pas.ExtCtrls.TCustomPanel,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taCenter});
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BevelColor",2,rtl.longint,"FBevelColor","SetBevelColor",{Default: 536870912});
    $r.addProperty("BevelInner",2,pas.Controls.$rtti["TBevelCut"],"FBevelInner","SetBevelInner",{Default: pas.Controls.TBevelCut.bvNone});
    $r.addProperty("BevelOuter",2,pas.Controls.$rtti["TBevelCut"],"FBevelOuter","SetBevelOuter",{Default: pas.Controls.TBevelCut.bvRaised});
    $r.addProperty("BevelWidth",2,pas.ExtCtrls.$rtti["TBevelWidth"],"FBevelWidth","SetBevelWidth",{Default: 1});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnPaint",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPaint","FOnPaint");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWTimer",pas.ExtCtrls.TCustomTimer,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("Interval",2,rtl.longword,"FInterval","SetInterval",{Default: 1000});
    $r.addProperty("OnTimer",2,pas.Classes.$rtti["TNotifyEvent"],"FOnTimer","SetOnTimer");
    $r.addProperty("OnStartTimer",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStartTimer","FOnStartTimer");
    $r.addProperty("OnStopTimer",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStopTimer","FOnStopTimer");
  });
});
rtl.module("Dialogs",["System","Classes","SysUtils","Types","Graphics","Controls","StdCtrls","ExtCtrls","Forms","WebCtrls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TMsgDlgType = {"0": "mtWarning", mtWarning: 0, "1": "mtError", mtError: 1, "2": "mtInformation", mtInformation: 2, "3": "mtConfirmation", mtConfirmation: 3, "4": "mtCustom", mtCustom: 4};
  this.$rtti.$Enum("TMsgDlgType",{minvalue: 0, maxvalue: 4, ordtype: 1, enumtype: this.TMsgDlgType});
  this.TMsgDlgBtn = {"0": "mbYes", mbYes: 0, "1": "mbNo", mbNo: 1, "2": "mbOK", mbOK: 2, "3": "mbCancel", mbCancel: 3, "4": "mbAbort", mbAbort: 4, "5": "mbRetry", mbRetry: 5, "6": "mbIgnore", mbIgnore: 6, "7": "mbAll", mbAll: 7, "8": "mbNoToAll", mbNoToAll: 8, "9": "mbYesToAll", mbYesToAll: 9, "10": "mbHelp", mbHelp: 10, "11": "mbClose", mbClose: 11};
  this.$rtti.$Enum("TMsgDlgBtn",{minvalue: 0, maxvalue: 11, ordtype: 1, enumtype: this.TMsgDlgBtn});
  this.$rtti.$Set("TMsgDlgButtons",{comptype: this.$rtti["TMsgDlgBtn"]});
  this.MessageDlg = function (AOwner, ACaption, AMessage, ADlgType, AButtons, ADefaultButton, AModalResultProc) {
    var VMessageDialog = null;
    if (!(AOwner != null)) AOwner = pas.Forms.Application().FActiveForm;
    VMessageDialog = $impl.TMessageDialog.$create("Create$1",[AOwner]);
    VMessageDialog.FButtons = rtl.refSet(AButtons);
    VMessageDialog.SetText(ACaption);
    VMessageDialog.FDefaultButton = ADefaultButton;
    VMessageDialog.FDialogType = ADlgType;
    VMessageDialog.FMessage = AMessage;
    VMessageDialog.PrepareLayout();
    VMessageDialog.ShowModal(AModalResultProc);
  };
  this.MessageDlg$2 = function (AOwner, ACaption, AMessage, ADlgType, AButtons, AModalResultProc) {
    $mod.MessageDlg(AOwner,ACaption,AMessage,ADlgType,rtl.refSet(AButtons),$impl.ModalDefaultButton(AButtons),AModalResultProc);
  };
  this.ShowMessage$1 = function (AMessage) {
    $mod.MessageDlg$2(pas.Forms.Application().FActiveForm,"",AMessage,2,rtl.createSet(2),null);
  };
  $mod.$implcode = function () {
    $impl.DialogCaption = ["Warning","Error","Information","Confirmation",""];
    $impl.ButtonCaption = ["Yes","No","Ok","Cancel","Abort","Retry","Ignore","All","No to All","Yes to All","Help","Close"];
    $impl.DialogIcon = ["data:image/png;base64,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","data:image/png;base64,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","data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAANXElEQVR42u2bBVQcSbfH572N+7cR4p51jUAWXdxdgkSQbOSTyPrG3XF3d4cYsnF3NEIcm2EGhxnkVs+rS2bPi8wK0P15nfM76XTfuvf+b1dVyzQ8jtt/23/bhymDePPD5g74PEJDwSxz1fQlJz1nLiuIp2TNXJqfh+A27ptmd8IDbdCWtyB4DmXgv6ho/xEDFoapTLU7fuC9lWd+tna/9+ibNEHnvsJmJvh6B4kt6SYp9wjJqJD2gNu4L4geQ5tv0gUdVh73Hr3reqpwqu2x/QMWRSijz39+4Z8Fzpxglv3dx2sunF0VXdnofUlMUu4zJLJMStxvSsnOy1Ly/TkprDslhbWFUlhVgLzYxn14DG3QFvsk32OI92UxWRX9vPEj6nOCec63vM/DZvzzCf8kdNZEi+wdGltuFu3KayYJ5Qzxu8OQTReksLqAAbe8voF90Yc/9YU+d+U1Ec2tt8umWB/dg8X+xwtXCR/5J4PU1ao/XS/ZQ4duDD1re64wZA1N3PUku6DPvdR3DC0ExlL+/lrx23rpqzCHf4z4hWEfzVtemLs+WSCJLCZk5yWGrKSJupzgFoyBsTDmumS+eN6KwhzeopAP/37Cv9wxYJRekr36plvl3pclxPM6IWvyCbgc//uylsbE2J6XOojapptlo7VTlmBu3Io39Bk83iTzJ8eAJ6Kg20A2nWXA+Rj5h7LpHANBt4As8XskHGuU9SNvrs9gjha6mOEKFpn73CKrWvxvEfLnfIAVx/45+EsBgD89IW4RVa0TrbIPYq6sn/mJljkH1sTXSLxvEPLVCYDlR/+5WHUSAHNbHVstUbDI3sfaSMB5NdYo86cV4ZUtHteAuB7rhmW57PPtz12w7VwXcTvedx+Ym8d1IMvDnrWMNcr4kWeb8la/9Y/STrK38q4QuV99IX5pDrt8dawTSvjdjFTWGsREuu2MhPTVnxvN0f1aN7HwrBCO0Ey26/elTvG7ayWHLnWSVce7YGk2uzhlSuBqZSeKf6XVtnRLnTLa++x3FR1FmLPSd1fLeUoRH/T5Jmf2soLcbfltZEN+NzhldbHOX3ObgZHKb+uPNffLN+a8Ja+VzHLKy+rTc8SfdFNXL4+oEm8+1U0cM7uAbRwyOmD/qUYi/ZX2lywR2vQrxpYz3WRZWFX72wbpbr2+t5+/8XLxrnOdZEVOJzhkso99Wjt8n8uXW4CnIjFjE1ML9hmSfsVYTnNHDQvWX7rdq4coehnZvTZZRP56sosm0ckJS1LbwSqyCkqqWl6ZBW0dIF2XcI/YJNSzEmddXhesoVom0Ie1P/xIu2DjldKtZzqIIx2C9ukckSYBmzgRWPoVQdylKubKw0Ym40YtszzkDlhGVIJdcgsrcVADalm48WrRHxoF44yyv3OOrYM/H6dnKa2DU2yTW8EqWgAm/vfA0OsOGPuVg2VkDdgkNLIaB7U4xwpgnHH6N797uzvHufDc93kS4kDPkF0q99gmtYJ1fANY0dFgHVcPNkktrMdwSJcAaprrXHj6N68I+BrL8PAD0V9pxWxTJP9WoCb9gw9EAxTDv/jVAky0PnrAJaEBltN5Y5Ms+bdiRWYHONOFVcH86B654vEN7AyngsK/HRODXYqYdvr74JDSDlsKxcQlk9s4qAm1UY0Fct8246trpR9uP1iTKwHrJDHn2NKkfC6JiaAVXlz+Ool0XU4LpzFRm+IPN+/xPgqe84Z+fBev7/5YsiJDDFaJ7Zyy/2w7qWzslr7e8u+3MZZxzZzFdaba9I88lAz4PEz9zVtfg/TVFoHVxIEOFcuEdk5Ykd4GF5/iw4/8dut5G2MWJeIsvmOqGMwDqsloPTm3xgoWRz3to0X0stQGFvHss6OglTRJiPS3WtatOsY0UshJfMSOakONEy1yj7xRgMnWJxKWJTaDNRrHsYd1XCsk3m5nGEb6u21r1lNiGiHEfpyA2pYlNMEkq+NxbxbA9nj2iuTWHkPzWHZwTWuFcn5Xj/SnQrF0e9p9si66RO4wILRCNqFPwDSqEftygiXVhhonWx/PlFOAEwUuaWjYCmYsYBJZDw/qupiOLiINKHhKtA9eBV3v+2AX/gTkFeB+bRtjGFwFpjHN2J8TUJtLGi2A7cl8uQVwpgUwi2kFUxYwDOHD2uhy4hpyh2h7lIFBcDUYhQlh50mR3BGQfLWWMQoVYF/OQG0rUn6lABNtjmYvTWyhhi1gEt1/UKyu7yPQ9XsMhlSYcVQTGEfUQ9qtRrmrwY9pTwj2wb5cgdqc6Do3Sd4UmGR9LME+ls4/LEAUO1BBFNH//z9UCPf54jcK0NlNpCb+j2iBGtCOM1CbfUyD/EVQwSzH0zq0Dsyjm8E4khssw/kA5M0BcP1RI6MXUEkL0MRZbMQihuYQJoAJ8i6Do+mvvAZez4klrZJRRDP7hDfBphyh3PnvX1hJ9ANr0Y5TrKg2A89nZLR2upvcW2GNvRVim9hmMKTJso1BiBDirtbLnf+ukRVEP1iAdpyC2tT3PhDz5oeqyX0Y+mTD9ft28c1gQI3ZRi9IAMVV7W8UQNTSKdX2egT6oSK045QlVNvH66/f5X0cMFvu4/AUu7wC+zjZGQtjF6OgWsDF7vV24k4do+NXBQahjWjHGajJPrap5xL4qx9fjTfP2m/sVwOmEU30jDSyR0gDrE8VyJ3/u7OfEB3/GrTjFLNIusD6VcNYk8zdv/5K7LMwFcVNpUKraDpkQxpZQzdICCHn6+UWwDqwAnQCBGjHKahJ8adi0UClyMW/+VJ0qn3eGevIBtqpAXSD2UHbnw+XHra8Mf/5TR1STY/HoBMoRDvOQC2oaeqSvFO/+zPZaL2077QOPQHT8EbQCWpgBS2famgSv/kC5OdSIaPp9fwVW+MwWVwWQS2aBx93jzFI+/oP/TAyx/VssXU0rV5QPeiwgHNsjdwHIJ+850TTp6rHxiKiHk6UtTHPG7qkOgF8VuIiqAG1zFt5/jZd/Kbz/kgbY5S+XefwM3o26PANrO8XWv51cDivTu78P3iskmj71sDBgibSKH5Rowv3GhgDv0rQDhD1OzbSo+HwE0J/IN3eqy8+ZzmfvW0ZTquIIgL6zpe+tXCyRP4DUDdhpM2yt0TiTpDuz7hPFm+9DBruT2jhRP2Ki2DuFlTDTOczN3vOfm/aSM2UVYt33GszCa0HTZpMX9HwqoLnIon0t1ppZQtj5XEVlPfcBnWPZ7RofOzbbzB3pW13W0dqJrr26YNnBcvj2fo+9BodRM+kn6hPqHk8h4radrkjoFXSLT2U/YAobb0AKgfvU/GVVLwQ+/UbzFmPLr5UQ0afvxrDLzBnOZ8pMgkSgra/EJPrNWruz2BfVgV5WXh7B0gjTz9ldHdfgC923QK1w4/pSKlBe1bAXDHn2c5nSmWfyPS9DVVPXPLxhlt1piF0WFHnGj69AQtQBYt33YYfYktI5tVqxvf4I6K58zwdmtdA5UAFPV4JGt4CtGUFzBFz/WjDLf5Q5QRrXn8bfmpG14Pv5v9Q3GASIqIVrgN1n15AxakcfgJKtAiLtlwGxR3XQfnAA1A98gzUvGrQhjUwN8xx/g9FTSN107/H3HlsNPzokL4v2Ke4tawdq6tBg6l59wIvWgQ6ElSOPKf/VoKqZy3uZxUUj7kpbiltx1zxL1VY/1R2pH7q3gU/FDeah+JQqwNVKuyfAcwFc1rwY3HzaIOM/fSSN4zHQftfnsKy4SM14zd9suFmnUVIHej50yJ4Cv6hYA4WwQKgOQlGaib8xJu8ahjmyrp4ygDKIJ7CJ8MHKQY7zlr+c5GRTzWYBglBHYe4h3yU3fmkP/yaXw0a0yxYCEbeVTB7xamyIYsDnTA3zBFzZbMI/0N5izKQMpgylDKC9+FmxfGmWUcXby1tt6ajwQBHgwcfBcMXR2oJF6BvjIGxMKbSlrK2caaZubx3flyEOclyGyzL9S3MnbMCUEbxxmpNGaocvG6GU36JzoHHxDZcCIZ+AqJMk118mF3QJ/rGGNr7H5KZTvnFQ5WD/sYbpzsZc+GmAHKmAGUIZdgvRaC8zZvqqDhcLdBjhsPxEq19D7rtQgXEPEhAtL3xzNUQpcN9A/uiD/SFPrX2VpAZS/MqRqgHe/OmLVmEsV8SPwxzkz8FuCvCcMpoigJlBm+iwZfDFQ+6K5in31jw7Y0mI/enxD6sjtiE1hGTAAHR8+ETTc9aoupOC0MFLj70AtzGfXgMbdAW+2Bf9DH/mxtNCuapN4YrHTmCMTCWLCbGHt4H8awWYuhLRZhOmccbNnshb9oy55EqnlFjDeNvvLfyVKXy1uJOnf0VxMz7GbEJrCb2oXziFCFAerZxHx5DG7R9d+Wp52MNE66PVPaKQF/oE31jjJfED2VBOCtrwy/FGEOZQJlKmU15jzdwzOe8cXomvOnLvhr6+bYDo9R9k0ZrhheM0Yk6P0Y35noPOtHncd8odZ+kIZ9u34+22Af7og+Zr6ky32N+Ec3OXGe/IANkCY6ijKUoyJKfLhMyl/IuCnuNd2XHZstsp8r6jpX5Qp8DuBfMRWFk0+alq8jwlxZRBLeHv7SKy4Yz90L/D1wmRcCSWFt6AAAAAElFTkSuQmCC","data:image/png;base64,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","data:image/png;base64,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"];
    $impl.ButtonModalResult = [6,7,1,2,3,4,5,8,9,10,0,11];
    rtl.createClass($impl,"TMessageDialog",pas.Forms.TWForm,function () {
      this.CControlsSpacing = 2;
      this.CMinDialogHeight = 150;
      this.CMinDialogWidth = 300;
      this.CMinButtonHeight = 25;
      this.CMinButtonWidth = 100;
      this.CMinImageHeight = 70;
      this.CMinImageWidth = 70;
      this.CTitleHeight = 24;
      this.$init = function () {
        pas.Forms.TWForm.$init.call(this);
        this.FButtons = {};
        this.FDefaultButton = 0;
        this.FDialogType = 0;
        this.FMessage = "";
        this.FButtonPanel = null;
        this.FInfoImage = null;
        this.fMessagePanel = null;
        this.FMessageText = null;
        this.fTitlePanel = null;
        this.fTitleText = null;
      };
      this.$final = function () {
        this.FButtons = undefined;
        this.FButtonPanel = undefined;
        this.FInfoImage = undefined;
        this.fMessagePanel = undefined;
        this.FMessageText = undefined;
        this.fTitlePanel = undefined;
        this.fTitleText = undefined;
        pas.Forms.TWForm.$final.call(this);
      };
      this.PrepareButtons = function () {
        var VMsgDlgBtn = 0;
        var VButton = null;
        var VButtonCount = 0;
        var VButtonHeight = 0;
        var VButtonWidth = 0;
        var VFormWidth = 0;
        var VSize = pas.Types.TSize.$new();
        var buttonofs = 0;
        VButtonCount = 0;
        buttonofs = 0;
        VButtonHeight = 25;
        VButtonWidth = 100;
        this.BeginUpdate();
        try {
          for (var $l = $mod.TMsgDlgBtn.mbYes, $end = $mod.TMsgDlgBtn.mbClose; $l <= $end; $l++) {
            VMsgDlgBtn = $l;
            if (VMsgDlgBtn in this.FButtons) {
              VButtonCount += 1;
              VSize.$assign(pas.Graphics.JSMeasureText($impl.ButtonCaption[VMsgDlgBtn],this.FFont.FName,this.FFont.FSize,0));
              if (VSize.cy > VButtonHeight) {
                VButtonHeight = VSize.cy;
              };
              if (VSize.cx > VButtonWidth) {
                VButtonWidth = VSize.cx;
              };
            };
          };
          for (var $l1 = $mod.TMsgDlgBtn.mbYes, $end1 = $mod.TMsgDlgBtn.mbClose; $l1 <= $end1; $l1++) {
            VMsgDlgBtn = $l1;
            if (VMsgDlgBtn in this.FButtons) {
              VButton = pas.WebCtrls.TWButton.$create("Create$1",[this.FButtonPanel]);
              VButton.BeginUpdate();
              try {
                VButton.SetParent(this.FButtonPanel);
                VButton.FBorderSpacing.SetAround(2);
                VButton.SetBounds(buttonofs,0,VButtonWidth,VButtonHeight);
                VButton.FModalResult = $impl.ButtonModalResult[VMsgDlgBtn];
                VButton.SetText($impl.ButtonCaption[VMsgDlgBtn]);
                VButton.SetAlign(4);
              } finally {
                VButton.EndUpdate();
              };
              if (VMsgDlgBtn === this.FDefaultButton) {
                this.SetActiveControl(VButton);
              };
            };
            buttonofs = buttonofs + VButtonWidth;
          };
          this.FButtonPanel.SetHeight(VButtonHeight + (2 * 2));
          VFormWidth = ((VButtonWidth + (2 * 2)) * VButtonCount) + (2 * 2);
          if (VFormWidth < 300) {
            VFormWidth = 300;
          };
          this.SetWidth(VFormWidth);
        } finally {
          this.EndUpdate();
        };
      };
      this.PrepareImage = function () {
        this.FInfoImage.SetURL($impl.DialogIcon[this.FDialogType]);
      };
      this.PrepareText = function () {
        this.FMessageText.SetText(this.FMessage);
      };
      this.PrepareTitle = function () {
        this.SetText(pas.Controls.IfThen$3(this.GetText() !== "",this.GetText(),$impl.DialogCaption[this.FDialogType]));
        this.fTitleText.SetText(this.GetText());
      };
      this.PrepareLayout = function () {
        this.PrepareTitle();
        this.PrepareText();
        this.PrepareImage();
        this.PrepareButtons();
      };
      this.KeyDown = function (Key, Shift) {
        pas.Controls.TWinControl.KeyDown.call(this,Key,rtl.refSet(Shift));
        var $tmp = Key.get();
        if ($tmp === 27) {
          this.SetModalResult(2);
          this.Close();
        };
      };
      this.Show = function () {
        var ownForm = null;
        var curHeight = 0;
        pas.Forms.TCustomForm.Show.call(this);
        if (!pas.Forms.TWForm.isPrototypeOf(this.FOwner)) return;
        ownForm = this.FOwner;
        this.BeginUpdate();
        try {
          curHeight = this.FMessageText.FHandleElement.scrollHeight + 25 + (2 * 8) + 24;
          if ((curHeight - 70) > ownForm.FHeight) curHeight = ownForm.FHeight - 70;
          if (curHeight < 150) curHeight = 150;
          this.SetHeight(curHeight);
          this.fMessagePanel.SetHeight(this.FMessageText.FHandleElement.scrollHeight);
          this.SetTop(Math.round((ownForm.FHeight / 2) - (this.FHeight / 2)));
        } finally {
          this.EndUpdate();
        };
      };
      this.Create$1 = function (AOwner) {
        pas.Forms.TCustomForm.CreateNew.call(this,AOwner,1);
        this.BeginUpdate();
        try {
          this.FKeyPreview = true;
          this.SetBounds(0,0,300,150);
          this.FButtonPanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.FButtonPanel.BeginUpdate();
          try {
            this.FButtonPanel.SetParent(this);
            this.FButtonPanel.FBorderSpacing.SetAround(2);
            this.FButtonPanel.SetBevelOuter(0);
            this.FButtonPanel.SetBounds(0,0,300,25);
            this.FButtonPanel.SetAlign(2);
          } finally {
            this.FButtonPanel.EndUpdate();
          };
          this.fTitlePanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.fTitlePanel.BeginUpdate();
          try {
            this.fTitlePanel.SetParent(this);
            this.fTitlePanel.FBorderSpacing.SetAround(2);
            this.fTitlePanel.SetHeight(24);
            this.fTitlePanel.SetBevelOuter(0);
            this.fTitlePanel.SetAlign(1);
            this.fTitlePanel.SetColor(15780518);
          } finally {
            this.fTitlePanel.EndUpdate();
          };
          this.fTitleText = pas.WebCtrls.TWLabel.$create("Create$1",[this.fTitlePanel]);
          this.fTitleText.BeginUpdate();
          try {
            this.fTitleText.SetParent(this.fTitlePanel);
            this.fTitleText.FBorderSpacing.SetAround(2);
            this.fTitleText.SetAlign(5);
            this.fTitleText.SetAlignment(2);
          } finally {
            this.fTitleText.EndUpdate();
          };
          this.FInfoImage = pas.WebCtrls.TWImage.$create("Create$1",[this]);
          this.FInfoImage.BeginUpdate();
          try {
            this.FInfoImage.SetParent(this);
            this.FInfoImage.FBorderSpacing.SetAround(2);
            this.FInfoImage.SetBounds(0,0,70,70);
            this.FInfoImage.SetCenter(true);
            this.FInfoImage.SetAlign(3);
          } finally {
            this.FInfoImage.EndUpdate();
          };
          this.fMessagePanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.fMessagePanel.BeginUpdate();
          try {
            this.fMessagePanel.SetParent(this);
            this.fMessagePanel.FBorderSpacing.SetAround(2);
            this.fMessagePanel.SetBevelOuter(0);
            this.fMessagePanel.SetAlign(5);
          } finally {
            this.fMessagePanel.EndUpdate();
          };
          this.FMessageText = pas.WebCtrls.TWLabel.$create("Create$1",[this.fMessagePanel]);
          this.FMessageText.BeginUpdate();
          try {
            this.FMessageText.SetParent(this.fMessagePanel);
            this.FMessageText.FBorderSpacing.SetAround(2);
            this.FMessageText.SetWordWrap(true);
            this.FMessageText.SetAlign(5);
          } finally {
            this.FMessageText.EndUpdate();
          };
        } finally {
          this.EndUpdate();
        };
        return this;
      };
      rtl.addIntf(this,pas.System.IUnknown);
      var $r = this.$rtti;
      $r.addProperty("Buttons",0,$mod.$rtti["TMsgDlgButtons"],"FButtons","FButtons");
      $r.addProperty("DefaultButton",0,$mod.$rtti["TMsgDlgBtn"],"FDefaultButton","FDefaultButton");
      $r.addProperty("DialogType",0,$mod.$rtti["TMsgDlgType"],"FDialogType","FDialogType");
      $r.addProperty("Message",0,rtl.string,"FMessage","FMessage");
    });
    $impl.ModalDefaultButton = function (AButtons) {
      var Result = 0;
      if (0 in AButtons) {
        Result = 0;
      } else if (2 in AButtons) {
        Result = 2;
      } else if (9 in AButtons) {
        Result = 9;
      } else if (7 in AButtons) {
        Result = 7;
      } else if (5 in AButtons) {
        Result = 5;
      } else if (10 in AButtons) {
        Result = 10;
      } else if (3 in AButtons) {
        Result = 3;
      } else if (1 in AButtons) {
        Result = 1;
      } else if (8 in AButtons) {
        Result = 8;
      } else if (4 in AButtons) {
        Result = 4;
      } else if (6 in AButtons) {
        Result = 6;
      } else if (11 in AButtons) {
        Result = 11;
      } else {
        Result = 2;
      };
      return Result;
    };
  };
},[]);
rtl.module("Mat4",["System","browserconsole","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass(this,"TMat4",pas.System.TObject,function () {
    this.RawComponents$a$clone = function (a) {
      var b = [];
      b.length = 4;
      for (var c = 0; c < 4; c++) b[c] = a[c].slice(0);
      return b;
    };
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.RawComponents = rtl.arraySetLength(null,0.0,4,4);
    };
    this.$final = function () {
      this.RawComponents = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Identity = function () {
      this.RawComponents[0][0] = 1.0;
      this.RawComponents[0][1] = 0.0;
      this.RawComponents[0][2] = 0.0;
      this.RawComponents[0][3] = 0.0;
      this.RawComponents[1][0] = 0.0;
      this.RawComponents[1][1] = 1.0;
      this.RawComponents[1][2] = 0.0;
      this.RawComponents[1][3] = 0.0;
      this.RawComponents[2][0] = 0.0;
      this.RawComponents[2][1] = 0.0;
      this.RawComponents[2][2] = 1.0;
      this.RawComponents[2][3] = 0.0;
      this.RawComponents[3][0] = 0.0;
      this.RawComponents[3][1] = 0.0;
      this.RawComponents[3][2] = 0.0;
      this.RawComponents[3][3] = 1.0;
      return this;
    };
    this.CopyList = function () {
      var Result = [];
      var x = 0;
      var y = 0;
      var list = null;
      list = new Array();
      for (x = 0; x <= 3; x++) for (y = 0; y <= 3; y++) list.push(this.RawComponents[x][y]);
      Result = list;
      return Result;
    };
  });
  $mod.$implcode = function () {
    $impl.Matrix4x4Identity = null;
  };
  $mod.$init = function () {
    $impl.Matrix4x4Identity = $mod.TMat4.$create("Identity");
  };
},[]);
rtl.module("MemoryBuffer",["System","JS"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TMemoryBuffer",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.byteBuffer = null;
      this.byteOffset = 0;
      this.floatBuffer = null;
    };
    this.$final = function () {
      this.byteBuffer = undefined;
      this.floatBuffer = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (size) {
      this.byteBuffer = new Uint8Array(size);
      return this;
    };
    this.AddBytes = function (count, data) {
      this.byteBuffer.set(data,this.byteOffset);
      this.byteOffset = this.byteOffset + (count * 1);
    };
    var kElementSize = 4;
    this.AddFloats = function (count, data) {
      var floatOffset = 0;
      floatOffset = rtl.trunc(this.byteOffset / 4);
      if (this.floatBuffer === null) this.floatBuffer = new Float32Array(this.byteBuffer.buffer,0,rtl.trunc(this.byteBuffer.byteLength / 4));
      this.floatBuffer.set(data,floatOffset);
      this.byteOffset = this.byteOffset + (count * 4);
    };
  });
});
rtl.module("webgl",["System","JS","Web"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("GLTypes",["System","webgl","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.recNewT(this,"TVec2",function () {
    this.x = 0.0;
    this.y = 0.0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      return this;
    };
  });
  this.V2 = function (x, y) {
    var Result = $mod.TVec2.$new();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.ToFloats$1 = function (v) {
    var Result = [];
    Result = rtl.arraySetLength(Result,0.0,2);
    Result[0] = v.x;
    Result[1] = v.y;
    return Result;
  };
  this.RGBAb = function (r, g, b, a) {
    var Result = [];
    Result[0] = r;
    Result[1] = g;
    Result[2] = b;
    Result[3] = a;
    return Result;
  };
});
rtl.module("GLUtils",["System","MemoryBuffer","Mat4","GLTypes","browserconsole","webgl","JS","Types","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass(this,"TShader",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.gl = null;
      this.vertexShader = null;
      this.fragmentShader = null;
      this.programID = null;
    };
    this.$final = function () {
      this.gl = undefined;
      this.vertexShader = undefined;
      this.fragmentShader = undefined;
      this.programID = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (context, vertexShaderSource, fragmentShaderSource) {
      this.gl = context;
      this.vertexShader = this.CreateShader(35633,vertexShaderSource);
      this.fragmentShader = this.CreateShader(35632,fragmentShaderSource);
      return this;
    };
    this.Compile = function () {
      this.programID = this.gl.createProgram();
      this.gl.attachShader(this.programID,this.vertexShader);
      this.gl.attachShader(this.programID,this.fragmentShader);
    };
    this.Link = function () {
      this.gl.linkProgram(this.programID);
      if (!this.gl.getProgramParameter(this.programID,35714)) {
        $impl.Fatal(this.gl.getProgramInfoLog(this.programID));
      };
    };
    this.Use = function () {
      this.gl.useProgram(this.programID);
    };
    this.BindAttribLocation = function (index, name) {
      this.gl.bindAttribLocation(this.programID,index,name);
    };
    this.SetUniformMat4 = function (name, value) {
      var list = [];
      list = value.CopyList();
      this.gl.uniformMatrix4fv(this.GetUniformLocation(name),false,list);
      $mod.GLFatal(this.gl,"gl.uniformMatrix4fv");
    };
    this.GetUniformLocation = function (name) {
      var Result = null;
      Result = this.gl.getUniformLocation(this.programID,name);
      $mod.GLFatal(this.gl,"gl.getUniformLocation");
      return Result;
    };
    this.CreateShader = function (theType, source) {
      var Result = null;
      Result = this.gl.createShader(theType);
      if (Result === null) $impl.Fatal("create shader failed");
      this.gl.shaderSource(Result,source);
      this.gl.compileShader(Result);
      if (this.gl.getShaderParameter(Result,35713)) {
        return Result;
      } else {
        $impl.Fatal(this.gl.getShaderInfoLog(Result));
      };
      return Result;
    };
  });
  this.GLSizeof = function (glType) {
    var Result = 0;
    var $tmp = glType;
    if (($tmp === 5121) || ($tmp === 5120)) {
      Result = 1}
     else if (($tmp === 5122) || ($tmp === 5123)) {
      Result = 2}
     else if (($tmp === 5124) || ($tmp === 5125)) {
      Result = 4}
     else if ($tmp === 5126) {
      Result = 4}
     else {
      $impl.Fatal("GLSizeof type is invalid.");
    };
    return Result;
  };
  this.GLFatal = function (gl, messageString) {
    var error = 0;
    error = gl.getError();
    if (error !== 0) {
      var $tmp = error;
      if ($tmp === 1281) {
        messageString = messageString + " (GL_INVALID_VALUE)"}
       else if ($tmp === 1282) {
        messageString = messageString + " (GL_INVALID_OPERATION)"}
       else if ($tmp === 1280) {
        messageString = messageString + " (GL_INVALID_ENUM)"}
       else {
        messageString = messageString + " " + pas.SysUtils.IntToStr(error);
      };
      $impl.Fatal(messageString);
    };
  };
  $mod.$implcode = function () {
    $impl.Fatal = function (messageString) {
      pas.System.Writeln("*** FATAL: ",messageString);
      throw pas.SysUtils.Exception.$create("Create$1",["FATAL"]);
    };
  };
},[]);
rtl.module("Unit1",["System","browserconsole","BrowserApp","JS","Classes","SysUtils","Graphics","Controls","Forms","Dialogs","WebCtrls","ExtCtrls","Web","Mat4","MemoryBuffer","GLUtils","GLTypes","webgl"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass(this,"TWForm1",pas.Forms.TWForm,function () {
    this.$init = function () {
      pas.Forms.TWForm.$init.call(this);
      this.WButton1 = null;
      this.WTimer1 = null;
    };
    this.$final = function () {
      this.WButton1 = undefined;
      this.WTimer1 = undefined;
      pas.Forms.TWForm.$final.call(this);
    };
    this.FormCreate = function (Sender) {
      this.WTimer1.SetEnabled(false);
      this.SetColor(32768);
      this.GetCanvas().SetWidth(500);
      this.GetCanvas().SetHeight(500);
      $impl.gl = this.GetCanvas().FCanvasElement.getContext("webgl");
      if ($impl.gl === null) {
        pas.Dialogs.ShowMessage$1("failed to load webgl!");
        this.SetColor(255);
        return;
      };
      pas.System.Writeln("fdgfgfdggfd");
      $impl.vertexShaderSource = "" + pas.System.LineEnding + "attribute vec2 inPos; varying vec2 pos;  void main(){ gl_Position = vec4(inPos, 1.0, 1.0);   pos = inPos.xy;}";
      $impl.fragmentShaderSource = "" + pas.System.LineEnding + "precision highp float; varying vec2 pos;  void main(void){  gl_FragColor = vec4(pos, 0.0, 1.0); }";
      $impl.shader = pas.GLUtils.TShader.$create("Create$1",[$impl.gl,$impl.vertexShaderSource,$impl.fragmentShaderSource]);
      $impl.shader.Compile();
      $impl.shader.BindAttribLocation(0,"inPos");
      $impl.shader.Link();
      $impl.shader.Use();
      this.WTimer1.SetEnabled(true);
    };
    this.WButton1Click = function (Sender) {
      pas.System.Writeln("button");
    };
    this.WTimer1Timer = function (Sender) {
      $impl.gl.clearColor(0.3,0.0,0.3,1);
      $impl.gl.viewport(0,0,this.GetCanvas().FWidth,this.GetCanvas().FHeight);
      $impl.gl.clear(16384);
      $impl.viewTransform = pas.Mat4.TMat4.$create("Identity");
      $impl.buffer = $impl.gl.createBuffer();
      $impl.gl.bindBuffer(34962,$impl.buffer);
      $impl.gl.bufferData(34962,$impl.GetVertexData(),35044);
      $impl.offset = 0;
      $impl.stride = 12;
      $impl.gl.enableVertexAttribArray(0);
      $impl.gl.vertexAttribPointer(0,2,5126,false,$impl.stride,$impl.offset);
      $impl.offset += pas.GLUtils.GLSizeof(5126) * 2;
      $impl.gl.enableVertexAttribArray(1);
      $impl.gl.vertexAttribPointer(1,4,5121,true,$impl.stride,$impl.offset);
      $impl.offset += pas.GLUtils.GLSizeof(5121) * 4;
      window.requestAnimationFrame($impl.UpdateCanvas);
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addField("WButton1",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WTimer1",pas.WebCtrls.$rtti["TWTimer"]);
    $r.addMethod("FormCreate",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WButton1Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WTimer1Timer",0,[["Sender",pas.System.$rtti["TObject"]]]);
  });
  this.WForm1 = null;
  $mod.$implcode = function () {
    rtl.recNewT($impl,"GLVertex2",function () {
      this.$new = function () {
        var r = Object.create(this);
        r.pos = pas.GLTypes.TVec2.$new();
        r.color = [];
        return r;
      };
      this.$eq = function (b) {
        return this.pos.$eq(b.pos) && (this.color === b.color);
      };
      this.$assign = function (s) {
        this.pos.$assign(s.pos);
        this.color = rtl.arrayRef(s.color);
        return this;
      };
    });
    $impl.kSIZEOF_VERTEX = 12;
    $impl.GetVertexData = function () {
      var Result = null;
      var buffer = null;
      var verts = null;
      var v = $impl.GLVertex2.$new();
      var i = 0;
      verts = new Array();
      v.pos.$assign(pas.GLTypes.V2(0,0));
      v.color = pas.GLTypes.RGBAb(255,0,0,255);
      verts.push($impl.GLVertex2.$clone(v));
      v.pos.$assign(pas.GLTypes.V2(0,100));
      v.color = pas.GLTypes.RGBAb(0,255,0,255);
      verts.push($impl.GLVertex2.$clone(v));
      v.pos.$assign(pas.GLTypes.V2(100,100));
      v.color = pas.GLTypes.RGBAb(0,0,255,255);
      verts.push($impl.GLVertex2.$clone(v));
      buffer = pas.MemoryBuffer.TMemoryBuffer.$create("Create$1",[12 * verts.length]);
      for (var $l = 0, $end = verts.length - 1; $l <= $end; $l++) {
        i = $l;
        v.$assign(rtl.getObject(verts[i]));
        buffer.AddFloats(2,pas.GLTypes.ToFloats$1(pas.GLTypes.TVec2.$clone(v.pos)));
        buffer.AddBytes(4,v.color);
      };
      Result = buffer.byteBuffer;
      return Result;
    };
    $impl.nextTime = 0;
    $impl.deltaTime = 0;
    $impl.gl = null;
    $impl.shader = null;
    $impl.viewTransform = null;
    $impl.rotateAngle = 0;
    $impl.UpdateCanvas = function (time) {
      var now = 0.0;
      now = time * 0.001;
      $impl.deltaTime = now - $impl.nextTime;
      $impl.nextTime = now;
      $impl.rotateAngle = $impl.rotateAngle + (20 * $impl.deltaTime);
      $impl.shader.SetUniformMat4("modelTransform",$impl.viewTransform);
      $impl.gl.clear(16384);
      $impl.gl.drawArrays(4,0,3);
      window.requestAnimationFrame($impl.UpdateCanvas);
    };
    $impl.stride = 0;
    $impl.offset = 0;
    $impl.vertexShaderSource = "";
    $impl.fragmentShaderSource = "";
    $impl.buffer = null;
  };
},[]);
rtl.module("program",["System","Forms","Unit1"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    pas.Forms.Application().Initialize();
    pas.Forms.Application().CreateForm(pas.Unit1.TWForm1,{p: pas.Unit1, get: function () {
        return this.p.WForm1;
      }, set: function (v) {
        this.p.WForm1 = v;
      }});
    pas.Forms.Application().Run();
  };
});
//# sourceMappingURL=project1.js.map
