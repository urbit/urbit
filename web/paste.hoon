/=  all  /;  flop  /^  (list (pair time ,*))  /&  /mime/
;html
  ;head:title:"Pastebin"
  ;body
    ;link(rel "stylesheet", href "/home/lib/base.css");
    ;link(rel "stylesheet", href "/home/pub/paste/main.css");
    ;script@"//code.jquery.com/jquery-2.1.4.min.js";
    ;script@"/~/as/own/~/at/home/lib/urb.js";
    ;script:'''
            document.title = 'pastebin - urbit'
            urb.appl = 'hood'
            urb.send.mark = 'write-paste'
            submit = function(){
              if($("select :selected").attr('value')===undefined) {
                $("select").addClass('err')
                return false
              }
              $("select").removeClass('err')
              $("textarea,button").attr('disabled', true)
              urb.send({
                txt:$("textarea").val(),
                typ:$("select :selected").val()
              }, function(){
                $.getJSON('paste.paste-new',null,function(resp){
                  if(!resp) throw "No paste"
                  window.location = "paste/"+resp.u
              })})
            }
            '''
  ::
    ;h1: New
    ;p:textarea;
    ;select
      ;option(): Type
      ;option(value "md"): Markdown
      ;option(value "txt"): Text
      ;option(value "hoon"): Hoon
    ==
    ;button(onclick "submit()"):"Submit"
    ;hr;
    ;h1: Recent
    ;*  (turn all |=([a=time *] ;p:a/"paste/{<a>}":"{<a>}"))
  ==
==
