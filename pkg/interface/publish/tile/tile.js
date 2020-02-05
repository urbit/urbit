import React, { Component } from 'react'
import classnames from 'classnames';


export default class PublishTile extends Component {
  constructor(props){
    super(props);
    console.log("publish-tile", this.props);
  }


  render(){
    return (
      <div className="w-100 h-100 relative bg-white ba b--black">
        <a className="w-100 h-100 db no-underline" href="/~publish">
          <p className="black f9 absolute"
            style={{left: 8, top: 8}}>
            Publishing
          </p>
          <img
            className="absolute"
            style={{left: 39, top: 39}}
            src="/~publish/tile.png"
            width={48}
            height={48} />
          <div className="absolute w-100 flex-col f9"
            style={{verticalAlign: "bottom", bottom: 8, left: 8}}>
            <span className="green2">{this.props.data.notifications}</span>
          </div>
        </a>
      </div>
    );
  }
}

window.publishTile = PublishTile;
