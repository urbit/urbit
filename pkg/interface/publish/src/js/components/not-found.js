import React, { Component } from 'react';
import classnames from 'classnames';
import { PathControl } from "/components/lib/path-control";

export class NotFound extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let pathData = [{text: "Home", url: "/~publish/recent"}];
    let backText = "<- Back";

    let back = (this.props.history)
      ?  <p className="body-regular pointer" style={{marginTop: 22}}
           onClick={() => {this.props.history.goBack()}}>
            {backText}
         </p>
      :  null;

    return (
      <div>
        <PathControl pathData={pathData} create={false}/>
        <div className="absolute w-100" style={{top:124}}>
          <div className="mw-688 center w-100">
            {back}
            <h2>Page Not Found</h2> 
          </div>
        </div>
      </div>
    );
  }
}
