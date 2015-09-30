query       = require './Async.coffee'
reactify    = require './Reactify.coffee'

recl = React.createClass
{div} = React.DOM


module.exports = query {body:'r'}, recl
  hash:null
  displayName: "TableOfContents"

  _click: (id)->
    -> if id then document.location.hash = id
    
  componentDidMount: ->
    @int = setInterval @checkHash,100
    @st = $(window).scrollTop()
    # $(window).on 'scroll',@checkScroll
    @$headers = $('#toc').children('h1,h2,h3,h4').filter('[id]')

  checkScroll: ->
    st = $(window).scrollTop()
    if Math.abs(@st-st) > 10
      hash = null
      @st = st
      for v in @$headers 
        continue if v.tagName is undefined
        $h = $ v
        hst = $h.offset().top-$h.outerHeight(true)+10
        if hst < st
          hash = $h.attr('id')
        if hst > st and hash isnt @hash and hash isnt null
          @hash = "#"+hash
          document.location.hash = hash
          break

  checkHash: ->
    if document.location.hash?.length > 0 and document.location.hash isnt @hash
      hash = document.location.hash.slice(1)
      for v in @$headers 
        $h = $ v
        if hash is $h.attr('id')
          @hash = document.location.hash
          offset = $h.offset().top - $h.outerHeight(true)
          setTimeout -> $(window).scrollTop offset
            , 10
          break

  componentWillUnmount: ->
    clearInterval @int

  collectHeader: ({gn,ga,c})->
    if gn and gn[0] is 'h' and parseInt(gn[1]) isnt NaN
      ga = _.clone ga
      ga.onClick = @_click ga.id
      delete ga.id
      {gn,ga,c}

  parseHeaders: ->
    if @props.body.c
      for v in @props.body.c
        if v.gn is 'div' and v.ga?.id is "toc"
          return {
            gn:"div"
            ga:{className:"toc"}
            c:[
              {gn:"h1", ga:{className:"t"}, c:["Table of contents"]}
              (_.filter v.c.map @collectHeader)...
          ]}

  render: -> reactify @parseHeaders()
