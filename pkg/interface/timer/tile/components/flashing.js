import React, { Component } from 'react';

export default class Flashing extends Component {
  constructor(props) {
    super(props);
    this.alive = true;
    this.state = { color: "black" };
  }

 //memory cleanup 
  componentWillUnmount() {
    this.alive = false;
  }

  render() {
      setTimeout(()=>{
        if(this.alive) {
          if(this.state.color == "black") { this.setState({color: "white"}) }
          else if(this.state.color == "white") { this.setState({color: "black"}) }
        }
      },400);
    
    return <div style={{ color: this.state.color }}>
      {this.props.children}
    </div>
  }
}