TreeStore   = require '../stores/TreeStore.coffee'

recl = React.createClass
{div} = React.DOM

module.exports = recl
  hash:null
  displayName: "TableOfContents"

  _onChangeStore: -> @setState tocs: @compute()
  _click: (e) ->
    console.log 'click'
    document.location.hash = @urlsafe $(e.target).text()

  urlsafe: (str) ->
    str.toLowerCase().replace(/\ /g, "-").replace(/[^a-z0-9~_.-]/g,"")
    
  componentDidMount: ->
    TreeStore.addChangeListener @_onChangeStore
    @int = setInterval @checkHash,100
    @setState tocs: @compute()

  checkHash: ->
    if document.location.hash? and document.location.hash isnt @hash
      hash = document.location.hash.slice(1)
      for k,v of @state.tocs
        if hash is @urlsafe v.t
          @hash = document.location.hash
          $(window).scrollTop v.e.offset().top
          break

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore
    clearInterval @int

  getInitialState: -> tocs: @compute()

  compute: ->
    $headers = $('#toc h1, #toc h2, #toc h3, #toc h4')
    for h in $headers
      $h = $(h)
      {h:h.tagName.toLowerCase(),t:$h.text(),e:$h}

  render: -> 
    onClick = @_click
    (div {className:'toc'}, @state.tocs.map ({h,t},key) ->
      (React.DOM[h] {onClick,key}, t)
    )
