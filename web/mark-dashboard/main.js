TreeActions = window.tree.actions

TreeActions.registerComponent("mark-dashboard", React.createClass({
  render: function(){ return React.DOM.div({},""+this.state.data)},
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
