window.easy_form = {
  submit: (form)=> {
    const param = (key)=> {
      var x = form.elements[`easy_form:${key}`]
      return x && x.value
    }
    var mark = param("mark")
    if(!mark) throw new TypeError("Need a mark")
    var appl = param("appl") || mark.match(/^[^-]*/)[0]
    var tag = param("tag")
    //
    if(param("confirm") != null && !confirm("Are you sure?"))
      return false
    //
    var req = {}
    for (var [k,v] of new FormData(form)){
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
