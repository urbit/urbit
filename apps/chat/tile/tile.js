import React, { Component } from 'react';
import classnames from 'classnames';


export default class ChatTile extends Component {

  constructor(props) {
    super(props);

    this.state = {
      configs: null,
      messages: null,
      message: null
    };
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (prevProps !== props) {

      // TODO: reducer logic :(
     
      this.setState(props);
    }
  }

  render() {
    console.log(this.state);
    return (
      <div className="bg-dark-gray w-100 h-100">
        <a className="w-100 h-100 db" style={{
          paddingTop: 68,
          paddingBottom: 68,
          paddingLeft: 64,
          paddingRight: 64
        }}
           href="/~chat">
           <img src="/~chat/img/Tile.png" width={106} height={98} />
        </a>
      </div>
    );
  }

}

window.chatTile = ChatTile;
