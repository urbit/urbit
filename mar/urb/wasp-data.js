urb.waspElem = function(ele){
  url = ele.src || ele.href
  if(!url || (new URL(url)).host != document.location.host)
    return;
  urb.waspUrl(url)
}
urb.waspUrl = function(url){
  var xhr = new XMLHttpRequest()
  xhr.open("HEAD", url)
  xhr.send()
  xhr.onload = urb.waspLoadedXHR
}
urb.waspLoadedXHR = function(){urb.wasp(urb.getXHRWasp(this))}
urb.getXHRWasp = function(xhr){
  var dep = xhr.getResponseHeader("etag")
  if(dep) return JSON.parse(dep.substr(2))
}

urb.datadeps = {}
urb.waspData = function(dep){
  urb.datadeps[dep] = true
  urb.wasp(dep)
}
urb.ondataupdate = urb.onupdate  // overridable

var _onupdate = urb.onupdate
urb.onupdate = function(dep){
  if(urb.datadeps[dep]) urb.ondataupdate(dep)
  else _onupdate(dep)
}
