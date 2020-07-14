import React, { Component } from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faHome } from "@fortawesome/free-solid-svg-icons";
import { BrowserRouter, Route, Link, Switch } from "react-router-dom";




export class Tile extends Component {

  constructor(props) {
    super(props);
    

  }



  render() {
    return (
      <div className="fl ma2 bg-white bg-gray0-d overflow-hidden"
           style={{ height: '126px', width: '126px' }}>
               <div className="w-100 h-100 relative bg-white bg-gray0-d ba b--black b--gray1-d">
                   <Link className="w-100 h-100 db pa2 no-underline" to={this.props.linkPath}>
                       <p className="black white-d absolute f9" style={{left: '8px', top: '8px'}}>{this.props.pageName}</p>
                       <div className="absolute invert-d" style={{top: '45px', left: '45px'}}>
                       <FontAwesomeIcon icon={this.props.tileIcon} size="3x" />
                       </div>
                       
                    </Link>
                    
                </div>
                
      </div>
    );
  }

}