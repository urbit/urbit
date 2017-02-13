TreeActions = window.tree.actions
d = React.DOM

TreeActions.registerComponent("mark-dashboard", React.createClass({
  render: function(){ 
    return d.ul({},
      !this.state.data ? "loading..." :
      _.map(this.state.data,
         function(result,mark){
           return d.li({key:mark},"%"+mark, "  ", 
              (!/\n/.test(result) ? d.code({},result) : d.pre({},d.code({},result)))
      )})
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
