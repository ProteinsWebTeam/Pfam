
// an add-on for prototype 1.6.03 that allows storage of arbitrary data
// on an element. Taken from: 
//   http://prototypejs.org/2009/2/16/pimp-my-code-1-element-storage

Element.Storage = {
  UID: 1
};
 
Element.addMethods({
  getStorage: function(element) {
    if (!(element = $(element))) return;
    
    if (Object.isUndefined(element._prototypeUID))
      element._prototypeUID = Element.Storage.UID++;
      
    var uid = element._prototypeUID;
    
    if (!Element.Storage[uid])
      Element.Storage[uid] = $H();
      
    return Element.Storage[uid];
  },
  
  store: function(element, key, value) {
    if (!(element = $(element))) return;
    element.getStorage().set(key, value);
    return element;
  },
  
  retrieve: function(element, key, defaultValue) {
    if (!(element = $(element))) return;
    
    var hash = element.getStorage(), value = hash.get(key);
    
    if (Object.isUndefined(value)) {
      hash.set(key, defaultValue);
      value = defaultValue;
    }
    
    return value;
  }
});
