//  htmx extension which encodes requests
//  by serializing the subtree of the requesting
//  element.
htmx.defineExtension('dom-enc', {
  onEvent: function (name, evt) {
    if (name === "htmx:configRequest") {
      evt.detail.headers['Content-Type'] = "text/html";
    }
  },
  encodeParameters : function(xhr, parameters, elt) {
    xhr.overrideMimeType('text/html');
    let xmls = new XMLSerializer();
    return (xmls.serializeToString(elt));
  }
});
