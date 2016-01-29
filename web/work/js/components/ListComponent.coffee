recl = React.createClass
rece = React.createElement
{div,h1,input,textarea} = React.DOM

WorkStore     = require '../stores/WorkStore.coffee'
WorkActions   = require '../actions/WorkActions.coffee'
ItemComponent = require './ItemComponent.coffee'

ListeningComponent = require './ListeningComponent.coffee'
FilterComponent = require './FilterComponent.coffee'
SortComponent = require './SortComponent.coffee'

module.exports = recl
  displayName: 'List'
  stateFromStore: -> window.canSort = WorkStore.canSort(); {
    list:WorkStore.getList()
    noNew:WorkStore.noNew()
    canSort:WorkStore.canSort()
    fulllist:WorkStore.getFullList()
    sorts:WorkStore.getSorts()
    filters:WorkStore.getFilters()
    expand:false
    updated:WorkStore.getUpdated()
  }

  getInitialState: -> @stateFromStore()
  _onChangeStore: -> @setState @stateFromStore()

  alias: ->
    @$el = $ @getDOMNode()
    @$items = @$el.find('.items').children()

  _focus: (e,i) -> @setState {selected:i.props.index}

  _dragStart: (e,i) -> @dragged = i.dragged

  _dragEnd: (e,i) -> 
    from = Number @dragged.closest('.item-wrap').attr('data-index')
    to = Number @over.closest('.item-wrap').attr('data-index')
    if from<to then to--
    if @drop is 'after' then to++
    sort = _.clone @state.list
    sort.splice to, 0, sort.splice(from,1)[0]
    list = (id for {id,version} in sort when (version ? -1) >= 0)
    WorkActions.moveItem list, to, from
    @dragged.removeClass 'hidden'
    @placeholder.remove()

  _dragOver: (e,i) ->
    e.preventDefault()
    $t = $(e.target).closest('.item')
    if $t.hasClass 'placeholder' then return
    if $t.length is 0 then return
    @over = $t
    if not @dragged.hasClass('hidden') then @dragged.addClass 'hidden'
    if (e.clientY - $t[0].offsetTop) < ($t[0].offsetHeight / 2)
      @drop = 'before'
      @placeholder.insertBefore $t
    else
      @drop = 'after'
      @placeholder.insertAfter $t

  title_keyDown: (e,i) -> switch e.keyCode
    # enter - add new
    when 13
      e.preventDefault()
      return if @state.noNew
      {item} = i.props
      after = null; before = null
      if window.getSelection().getRangeAt(0).endOffset is 0
        ins = @state.selected
        before = item.id
      else
        after = item.id
        ins = @state.selected+1 # XX consolidate
        @setState {selected:ins,select:true}
      unless item.ghost
        {tags,audience} = item
        item = {tags,audience}
      WorkActions.newItem {before,after}, item
    # backspace - remove if at 0
    when 8
      if  (window.getSelection().getRangeAt(0).endOffset is 0)
        e.preventDefault()
        if (e.target.innerText.length is 0)
          if @state.selected isnt 0
            @setState {selected:@state.selected-1,select:"end"}
          WorkActions.removeItem i.props.item
        else if ({index} = i.props; index > 0) and
                (prev = @state.list[i.props.index - 1]; prev.version < 0)
          WorkActions.removeItem prev
    # up
    when 38
      e.preventDefault()
      last = @state.selected-1
      if last<0 then last = @state.list.length-1
      @$items.eq(last).find('.title .input').focus()
      @setState {select:"end"}
    # down
    when 40
      e.preventDefault()
      next = @state.selected+1
      if next is @state.list.length then next = 0
      @$items.eq(next).find('.title .input').focus()
      @setState {select:"end"}

  _changeListening: ->

  _changeFilter: (key,val) -> WorkActions.setFilter key,val,@state.filters

  _changeSort: (key,val) -> WorkActions.setSort key,val,@state.sorts

  componentDidMount: -> 
    @placeholder = $ "<div class='item placeholder'><div class='sort'>x</div></div>"
    WorkStore.addChangeListener @_onChangeStore
    WorkActions.listenList @props.list
    WorkActions.getLocal 'filters'
    WorkActions.getLocal 'sorts'
    @alias()

  componentDidUpdate: (_props,_state)-> 
    @alias()
    if @state.select
      $title = @$items.eq(@state.selected).find('.title .input')
      if @state.selected?
        $title.focus()
      if @state.select is "end"
        r = window.getSelection().getRangeAt(0)
        r.setStart $title[0],0
        r.setEnd $title[0],0
        s = window.getSelection()
        s.removeAllRanges()
        s.addRange r
      @setState {select:false}

  render: ->
    (div {},
      (div {className:'ctrl'},
        rece(ListeningComponent, {
          listening:@state.listening
          onChange:@_changeListening
        })
        (div {className:'transforms'},
          rece(FilterComponent, {
            filters:@state.filters
            onChange:@_changeFilter
          })
          rece(SortComponent, {
            sorts:@state.sorts
            onChange:@_changeSort
          })
        )
      )
      (div {
        className:'items'
        onDragOver:@_dragOver
        }, _.map @state.list,(item,index) => 
            className = "item-wrap"
            key = item.id
            draggable = @state.canSort
            if item.ghost
              className += " ghost"
              draggable = false
            (div {className,key,'data-index':index},
              rece(ItemComponent,{
                item
                index
                draggable
                @_focus
                @title_keyDown
                @_dragStart
                @_dragEnd})
             )
      )
    )
