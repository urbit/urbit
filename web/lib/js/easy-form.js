window.easy_form = {
  submit: (form,req={})=> {
    var mark, appl, tag;
    var req = {}
    for (var [k,v] of new FormData(form)){
      switch(k){
               case "easy_form:tag":
          tag = v
        break; case "easy_form:appl":
          appl = v
        break; case "easy_form:mark":
          mark = v
          appl = appl || v.match(/^[^-]*/)[0]
        break; default:
          req[k] = v
      }
    }
    if(!mark) throw new TypeError("Need a mark")
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
