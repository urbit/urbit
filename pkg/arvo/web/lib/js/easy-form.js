//REVIEW this feels too complicated
let match_url_end = (pattern,out={})=> {
  if(!pattern) return out
  let here = document.location.pathname.split("/").reverse()
  while(!here[0]) here.shift()
  for(let segment of pattern.split("/").reverse()){
    let val = here.shift()
    if(segment[0] != ":") continue //REVIEW check for match?
    out[segment.slice(1)] = val
  }
  return out
}
//
window.easy_form = {
  submit: (form)=> {
    const param = (key)=> {
      var x = form.elements[`easy_form:${key}`]
      return x && x.value
    }
    let mark = param("mark")
    if(!mark) throw new TypeError("Need a mark")
    let appl = param("appl") || mark.match(/^[^-]*/)[0]
    let tag = param("tag")
    //
    if(param("confirm") != null && !confirm("Are you sure?"))
      return false
    //
    let req = {}
    req = match_url_end(param("url_end"),req)
    //
    for (let [k,v] of new FormData(form)){
      if(!/^easy_form:/.test(k)) {
          req[k] = v
      }
    }
    if(tag) req = {[tag]:req}

    fetch("/~/auth.json", {credentials: "same-origin"})
    .then((res)=>res.json())
    .then(({oryx})=> fetch(`/~/to/${appl}/${mark}`,{
      method: "POST",
      body:JSON.stringify({oryx,wire:"/",xyro:req}),
      credentials: "same-origin"
    }))
    return false
  }
}
