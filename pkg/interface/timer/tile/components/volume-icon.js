import React, { Component } from 'react';

export default class VolumeIcon extends Component {

  constructor(props) {
    super(props);
    this.toggleSound = this.toggleSound.bind(this);
  }


  toggleSound() {
    this.props.parent.setState( {playSound: !this.props.parent.state.playSound} )
  }

  render() {
    let on = this.props.parent.state.playSound;
    return <div> <a onClick={()=> {
      this.toggleSound()}}>
        <img src=
        {
          on 
          ?
          "~timer/img/volume-high.png"
          :
          "~timer/img/volume-mute.png"
        }
        style={{
          width:"30px",
          height:"30px"
        }}
      ></img>
      </a>
    </div>

  }

}