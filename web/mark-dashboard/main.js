TreeActions = window.tree.actions
d = React.DOM

TreeActions.registerComponent("mark-dashboard", React.createClass({
  render: function(){ 
    return d.ul({},
      !this.state.data ? "loading..." :
      _(this.state.data)
        .map(function(result,mark){return {result:result, mark:mark}})
        .sortBy('mark')
        .map(function(x){
           return d.li({key:x.mark},"%"+x.mark, "  ", 
              (!/\n/.test(x.result) ? d.code({},x.result) : d.pre({},d.code({},x.result)))
          )})
        .value()
  )},
  getInitialState: function(){ return {data:null}},
  componentDidMount: function(){
    $this = this
    
    urb.bind("/scry/x/main",
        {appl:"mark-dashboard"},
        function(err, dat){
          urb.drop("/scry/x/main", {appl:"mark-dashboard"})
          $this.setState({data:dat.data})
        }
    )
  }
}))
